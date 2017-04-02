extern crate parser;
use parser::*;

extern crate llvm_sys;
use llvm_sys::prelude::LLVMValueRef;
use llvm_sys::prelude::LLVMTypeRef;
use llvm_sys::LLVMIntPredicate;
use llvm_sys::LLVMRealPredicate;
use llvm_sys::LLVMTypeKind;
mod llvm;

use std::ptr;
use std::collections::HashMap;

pub fn codegen(module: &parser::Module, source_name: &String) {
    let mut llvm = LLVMCodeGen::new(source_name);
    //create main block
    llvm.init();
    let res = Some(module.codegen(&mut llvm));
    if let Some(v) = res {
        llvm.sys.build_ret(v);
    } else {
        let zero = llvm.sys.const_int(0);
        llvm.sys.build_ret(zero);
    }
    llvm.sys.print_module();
}

pub struct LLVMCodeGen {
    pub sys: llvm::LLVMSys,
    pub values: HashMap<String, LLVMValueRef>,
    pub types: HashMap<String, LLVMTypeRef>,
    pub cur_func: Option<LLVMValueRef>,
}

impl LLVMCodeGen {
    pub fn new(name: &str) -> LLVMCodeGen {
        LLVMCodeGen {
            sys: llvm::LLVMSys::new(name),
            values: HashMap::new(),
            types: HashMap::new(),
            cur_func: None,
        }
    }

    fn init(&mut self) {
        //define built-in types
        let int = self.sys.int64_type();
        self.set_type(&"Int".to_string(), int);
        let float = self.sys.float_type();
        self.set_type(&"Float".to_string(), float);
        let name_str = "main";
        let main = self.sys.add_func(int, name_str, ptr::null_mut(), 0);
        self.set_value(&name_str.to_string(), main);
        self.cur_func = Some(main);
        let block = self.sys.add_block("entry", self.get_value(&name_str.to_string()).unwrap());
        self.sys.position_builder_at_end(block);
    }

    fn set_value(&mut self, name: &String, value: LLVMValueRef) {
        let ref mut values = self.values;
        values.insert(name.clone(), value);
    }

    fn get_value(&self, name: &String) -> Option<LLVMValueRef> {
        self.values.get(name).map(|x| *x)
    }

    fn set_type(&mut self, name: &String, atype: LLVMTypeRef) {
        let ref mut types = self.types;
        types.insert(name.clone(), atype);
    }

    fn get_type(&self, name: &String) -> LLVMTypeRef {
        self.types[name]
    }
}

trait CodeGen {
    fn codegen(&self, llvm: &mut LLVMCodeGen) -> LLVMValueRef;
}

impl CodeGen for Module {
    fn codegen(&self, llvm: &mut LLVMCodeGen) -> LLVMValueRef {
        let mut ret = None;
        for expdecl in &self.expdecl {
            match expdecl {
                &ExpDecl::Exp(ref e) => ret = Some(e.codegen(llvm)),
                &ExpDecl::Decl(ref d) => {
                    match d {
                        &Decl::Typedecl(ref t) => t.codegen(llvm),
                    };
                }
            };
        }
        if let Some(v) = ret { v } else { ptr::null_mut() }
    }
}

impl CodeGen for Typedecl {
    fn codegen(&self, llvm: &mut LLVMCodeGen) -> LLVMValueRef {
        //self.class.argsから型取得;
        let mut fields = vec![llvm.sys.int64_type()];
        let atype = llvm.sys.build_struct_named(&mut fields, self.id.as_str());
        llvm.set_type(&self.id, atype);
        ptr::null_mut()
    }
}

/*impl CodeGen for Classdecl {
    fn codegen(&self, llvm: &mut LLVMCodeGen) -> LLVMValueRef {
    }
}*/

impl CodeGen for Expression {
    fn codegen(&self, llvm: &mut LLVMCodeGen) -> LLVMValueRef {
        self.assign.codegen(llvm)
    }
}

impl CodeGen for Assign {
    fn codegen(&self, llvm: &mut LLVMCodeGen) -> LLVMValueRef {
        if let Some(ref i) = self.id {
            let v = self.comp.codegen(llvm);
            let t = llvm.sys.type_of(v);
            let a = if let Some(var) = llvm.get_value(i) {
                var
            } else {
                llvm.sys.build_alloca(t, i.as_str())
            };
            llvm.set_value(&i, a);
            llvm.sys.build_store(v, a);
            v
        } else {
            self.comp.codegen(llvm)
        }
    }
}

fn is_int(kind: &LLVMTypeKind) -> bool {
    match *kind {
        LLVMTypeKind::LLVMIntegerTypeKind => true,
        _ => false,
    }
}

impl CodeGen for Comparative {
    fn codegen(&self, llvm: &mut LLVMCodeGen) -> LLVMValueRef {
        let l = self.lhs.codegen(llvm);

        if let Some(ref rhs) = self.rhs {
            let r = rhs.add.codegen(llvm);
            let lkind = llvm.sys.get_type_kind(llvm.sys.type_of(l));
            let rkind = llvm.sys.get_type_kind(llvm.sys.type_of(r));
            //lとrが共にintなら，そのままicmp
            if is_int(&lkind) && is_int(&rkind) {
                let cmp = match rhs.op {
                    CompOp::Lt => LLVMIntPredicate::LLVMIntSLT,
                    CompOp::Le => LLVMIntPredicate::LLVMIntSLE,
                    CompOp::Eq => LLVMIntPredicate::LLVMIntEQ,
                    CompOp::Ne => LLVMIntPredicate::LLVMIntNE,
                    CompOp::Gt => LLVMIntPredicate::LLVMIntSGT,
                    CompOp::Ge => LLVMIntPredicate::LLVMIntSGE,

                };
                llvm.sys.build_icmp(cmp, l, r, "cmp")
            } else {
                //lとrが共にfloatなら，そのままfcmp
                //どちらか片方がfloatなら，intの方をfloatに変換してからfcmp
                let lval = if is_int(&lkind) {
                    llvm.sys.ui_to_fp(l, llvm.sys.float_type(), "fp")
                } else {
                    l
                };
                let rval = if is_int(&rkind) {
                    llvm.sys.ui_to_fp(r, llvm.sys.float_type(), "fp")
                } else {
                    r
                };
                let cmp = match rhs.op {
                    CompOp::Lt => LLVMRealPredicate::LLVMRealOLT,
                    CompOp::Le => LLVMRealPredicate::LLVMRealOLE,
                    CompOp::Eq => LLVMRealPredicate::LLVMRealOEQ,
                    CompOp::Ne => LLVMRealPredicate::LLVMRealONE,
                    CompOp::Gt => LLVMRealPredicate::LLVMRealOGT,
                    CompOp::Ge => LLVMRealPredicate::LLVMRealOGE,

                };
                llvm.sys.build_fcmp(cmp, lval, rval, "cmp")
            }
        } else {
            return l;
        }
    }
}

impl CodeGen for Additive {
    fn codegen(&self, llvm: &mut LLVMCodeGen) -> LLVMValueRef {
        let l = self.lhs.codegen(llvm);
        if let Some(ref rhs) = self.rhs {
            let r = rhs.add.codegen(llvm);
            let lkind = llvm.sys.get_type_kind(llvm.sys.type_of(l));
            let rkind = llvm.sys.get_type_kind(llvm.sys.type_of(r));
            //lとrが共にintなら，そのままadd
            if is_int(&lkind) && is_int(&rkind) {
                match rhs.op {
                    AddOp::Add => llvm.sys.build_add(l, r, "add"),
                    AddOp::Sub => llvm.sys.build_sub(l, r, "sub"),
                }
            } else {

                let lval = if is_int(&lkind) {
                    llvm.sys.ui_to_fp(l, llvm.sys.float_type(), "fp")
                } else {
                    l
                };
                let rval = if is_int(&rkind) {
                    llvm.sys.ui_to_fp(r, llvm.sys.float_type(), "fp")
                } else {
                    r
                };
                match rhs.op {
                    AddOp::Add => llvm.sys.build_fadd(lval, rval, "add"),
                    AddOp::Sub => llvm.sys.build_fsub(lval, rval, "sub"),
                }
            }
        } else {
            l
        }
    }
}

impl CodeGen for Multitive {
    fn codegen(&self, llvm: &mut LLVMCodeGen) -> LLVMValueRef {
        let l = self.lhs.codegen(llvm);
        if let Some(ref rhs) = self.rhs {
            let r = rhs.mul.codegen(llvm);
            let lkind = llvm.sys.get_type_kind(llvm.sys.type_of(l));
            let rkind = llvm.sys.get_type_kind(llvm.sys.type_of(r));
            //lとrが共にintなら，そのままadd
            if is_int(&lkind) && is_int(&rkind) {
                match rhs.op {
                    MulOp::Mul => llvm.sys.build_mul(l, r, "mul"),
                    MulOp::Div => llvm.sys.build_sdiv(l, r, "div"),
                }
            } else {
                let lval = if is_int(&lkind) {
                    llvm.sys.ui_to_fp(l, llvm.sys.float_type(), "fp")
                } else {
                    l
                };
                let rval = if is_int(&rkind) {
                    llvm.sys.ui_to_fp(r, llvm.sys.float_type(), "fp")
                } else {
                    r
                };
                match rhs.op {
                    MulOp::Mul => llvm.sys.build_fmul(lval, rval, "mul"),
                    MulOp::Div => llvm.sys.build_fdiv(lval, rval, "div"),
                }
            }
        } else {
            l
        }
    }
}

impl CodeGen for Unary {
    fn codegen(&self, llvm: &mut LLVMCodeGen) -> LLVMValueRef {
        if let Some(ref u) = self.unaryop {
            if u.as_str() == "-" {
                let zero = llvm.sys.const_int(0);
                let prim = self.primary.codegen(llvm);
                llvm.sys.build_sub(zero, prim, "neg")
            } else {
                self.primary.codegen(llvm)

            }
        } else {
            self.primary.codegen(llvm)
        }
    }
}

impl CodeGen for If {
    fn codegen(&self, llvm: &mut LLVMCodeGen) -> LLVMValueRef {
        //cmp
        let cmp = self.condition.codegen(llvm);
        //then, else, merge block
        let then_block = llvm.sys.add_block("then", llvm.cur_func.unwrap());
        let else_block = llvm.sys.add_block("else", llvm.cur_func.unwrap());
        let merge_block = llvm.sys.add_block("merge", llvm.cur_func.unwrap());
        let result = llvm.sys.build_branch(cmp, then_block, else_block);

        llvm.sys.position_builder_at_end(then_block);
        let mut then_value = None;
        for e in &self.then_blk {
            then_value = Some(e.codegen(llvm));
        }

        // goto merge
        llvm.sys.build_br(merge_block);

        llvm.sys.position_builder_at_end(else_block);
        let mut else_value = None;
        if let Some(ref else_blk) = self.else_blk {
            for e in else_blk {
                else_value = Some(e.codegen(llvm));
            }
        }

        //goto merge
        llvm.sys.build_br(merge_block);

        //merge block
        llvm.sys.position_builder_at_end(merge_block);

        //make phi
        let phi = llvm.sys.build_phi(llvm.sys.int64_type(), "phi");
        let mut values = [if let Some(t) = then_value {
                              t
                          } else {
                              ptr::null_mut()
                          },
                          if let Some(e) = else_value {
                              e
                          } else {
                              ptr::null_mut()
                          }];
        let mut blocks = [then_block, else_block];
        llvm.sys.add_incoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);
        phi
    }
}

impl CodeGen for While {
    fn codegen(&self, llvm: &mut LLVMCodeGen) -> LLVMValueRef {
        //cmp
        let cond_block = llvm.sys.add_block("cond", llvm.cur_func.unwrap());
        llvm.sys.build_br(cond_block);
        llvm.sys.position_builder_at_end(cond_block);
        let cond = self.condition.codegen(llvm);
        let loop_block = llvm.sys.add_block("loop", llvm.cur_func.unwrap());
        let end_block = llvm.sys.add_block("end", llvm.cur_func.unwrap());
        let result = llvm.sys.build_branch(cond, loop_block, end_block);

        llvm.sys.position_builder_at_end(loop_block);
        let mut loop_value = None;
        for e in &self.exps {
            loop_value = Some(e.codegen(llvm));
        }

        llvm.sys.build_br(cond_block);
        llvm.sys.position_builder_at_end(end_block);
        ptr::null_mut()
    }
}

impl CodeGen for Classinst {
    fn codegen(&self, llvm: &mut LLVMCodeGen) -> LLVMValueRef {
        let t = llvm.get_type(&self.id);
        let mut fields = vec![llvm.sys.const_int(3)];
        llvm.sys.const_named_struct(t, &mut fields)
    }
}

impl CodeGen for Funccall {
    fn codegen(&self, llvm: &mut LLVMCodeGen) -> LLVMValueRef {
        let func = match &self.f_id {
            &FuncdefID::Funcdef(ref f) => f.codegen(llvm),
            &FuncdefID::ID(ref i) => llvm.get_value(i).unwrap(),
        };
        let mut p = Vec::new();
        for e in &self.params {
            p.push(e.codegen(llvm));
        }
        let (params, count) = if p.len() == 0 {
            (ptr::null_mut(), 0)
        } else {
            (p.as_mut_ptr(), p.len())
        };
        llvm.sys.add_call(func, params, count as u32, "call")
    }
}

impl CodeGen for Funcdef {
    fn codegen(&self, llvm: &mut LLVMCodeGen) -> LLVMValueRef {
        let name_str = if let Some(ref s) = self.id {
            s.as_str().clone()
        } else {
            "anon_func"
        };
        let (args, count) = if self.args.len() == 0 {
            (ptr::null_mut(), 0)
        } else {
            let mut types = vec![];
            for typearg in &self.args {
                types.push(llvm.get_type(&typearg.atype));
            }
            (types.as_mut_ptr(), types.len())
        };
        let ret = if let Some(ref atype) = self.ret {
            llvm.get_type(atype)
        } else {
            llvm.sys.void_type()
        };
        let func = llvm.sys.add_func(ret, name_str, args, count as u32);
        llvm.cur_func = Some(func);
        let block = llvm.sys.add_block("entry", func);
        llvm.sys.position_builder_at_end(block);
        for (i, a) in self.args.iter().enumerate() {
            let arg = llvm.sys.get_param(func, i as u32);
            let v = llvm.sys.build_alloca(llvm.get_type(&a.atype), &a.arg.as_str());
            llvm.sys.build_store(arg, v);
            llvm.set_value(&a.arg, v);
        }
        llvm.set_value(&name_str.to_string(), func);
        let mut ret = None;
        for e in &self.exps {
            ret = Some(e.codegen(llvm));
        }
        if let Some(r) = ret {
            llvm.sys.build_ret(r)
        } else {
            ptr::null_mut()
        }
    }
}

impl CodeGen for Primary {
    fn codegen(&self, llvm: &mut LLVMCodeGen) -> LLVMValueRef {
        match self {
            &Primary::Expression(ref e) => e.codegen(llvm),
            &Primary::If(ref i) => i.codegen(llvm),
            &Primary::While(ref w) => w.codegen(llvm),
            &Primary::Funccall(ref f) => f.codegen(llvm),
            &Primary::Classinst(ref c) => c.codegen(llvm),
            &Primary::Literal(ref l) => {
                match l {
                    &Literal::Int(ref i) => llvm.sys.const_int(*i as u64),
                    &Literal::Float(ref f) => llvm.sys.const_float(*f),
                    &Literal::Funcdef(ref f) => {
                        let fdef = f.codegen(llvm);
                        let func = llvm.get_value(&"main".to_string()).unwrap();
                        let block = llvm.sys
                            .get_last_basic_block(func);
                        llvm.sys.position_builder_at_end(block);
                        fdef
                    }
                    &Literal::Tuple(ref t) => t.codegen(llvm),
                    &Literal::Array(ref a) => a.codegen(llvm),
                }
            }
            &Primary::Varacc(ref v) => {
                let ptr = llvm.get_value(&v.id).unwrap();
                if let Some(ref m) = v.memb {
                    let mut memb_value: Vec<LLVMValueRef> =
                        m.into_iter().map(|x| llvm.sys.const_int32(*x as u64)).collect();
                    memb_value.insert(0, llvm.sys.const_int32(0));
                    let target = llvm.sys.build_gep(ptr, &mut memb_value, "memb");
                    llvm.sys.build_load(target, "load")
                    //llvm.sys.build_extract_value(load, m[0], "memb")
                } else {
                    llvm.sys.build_load(ptr, &v.id.as_str())
                }
            }
        }
    }
}

impl CodeGen for Tuple {
    fn codegen(&self, llvm: &mut LLVMCodeGen) -> LLVMValueRef {

        let mut values: Vec<LLVMValueRef> = vec![];
        for e in &self.params {
            values.push(e.codegen(llvm));
        }
        llvm.sys.const_struct(&mut values)
    }
}

impl CodeGen for Array {
    fn codegen(&self, llvm: &mut LLVMCodeGen) -> LLVMValueRef {
        let mut values: Vec<LLVMValueRef> = vec![];
        for e in &self.params {
            values.push(e.codegen(llvm));
        }
        let atype = llvm.sys.array_type(llvm.sys.int64_type(), values.len() as u32);
        llvm.sys.variable_struct(atype, &mut values)
        //いずれ各要素が定数式であると判断できる様になったら
        //llvm.sys.const_array(llvm.sys.int64_type(), &mut values)
    }
}