use std::ffi::CString;
use std::ptr;

extern crate llvm_sys as llvm;
use llvm::llvm::*;
use llvm::llvm::prelude::*;
use llvm::llvm::core::*;

pub struct LLVMSys {
    context: *mut LLVMContext,
    module: *mut LLVMModule,
    builder: *mut LLVMBuilder,
    name: String,
}

impl LLVMSys {
    pub fn new(name: &str) -> LLVMSys {
        unsafe {
            let context = LLVMContextCreate();
            let cname = CString::new(name).unwrap();
            let module = LLVMModuleCreateWithNameInContext(cname.as_ptr(), context);
            let builder = LLVMCreateBuilderInContext(context);
            return LLVMSys {
                context: context,
                module: module,
                builder: builder,
                name: name.to_string(),
            };
        }
    }

    pub fn add_func(&self,
                    ret_type: LLVMTypeRef,
                    name: &str,
                    args: *mut LLVMTypeRef,
                    count: u32)
                    -> LLVMValueRef {
        unsafe {
            let fun_type = LLVMFunctionType(ret_type, args, count, 0);
            let cname = CString::new(name).unwrap();
            LLVMAddFunction(self.module, cname.as_ptr(), fun_type)
        }
    }

    pub fn get_param(&self, func: LLVMValueRef, i: u32) -> LLVMValueRef {
        unsafe { LLVMGetParam(func, i) }
    }

    pub fn add_call(&self,
                    fun: LLVMValueRef,
                    args: *mut LLVMValueRef,
                    count: u32,
                    name: &str)
                    -> LLVMValueRef {
        unsafe {
            let cname = CString::new(name).unwrap();
            LLVMBuildCall(self.builder, fun, args, count, cname.as_ptr())
        }
    }

    pub fn add_block(&self, name: &str, fun: LLVMValueRef) -> LLVMBasicBlockRef {
        unsafe {
            let cname = CString::new(name).unwrap();
            LLVMAppendBasicBlockInContext(self.context, fun, cname.as_ptr())
        }
    }

    pub fn get_last_basic_block(&self, fun: LLVMValueRef) -> LLVMBasicBlockRef {
        unsafe { LLVMGetLastBasicBlock(fun) }
    }

    pub fn position_builder_at_end(&self, block: LLVMBasicBlockRef) {
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder, block);
        }
    }

    pub fn build_ret(&self, value: LLVMValueRef) -> LLVMValueRef {
        unsafe { LLVMBuildRet(self.builder, value) }
    }

    pub fn build_add(&self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        unsafe {
            let cname = CString::new(name).unwrap();
            LLVMBuildAdd(self.builder, lhs, rhs, cname.as_ptr())
        }
    }

    pub fn build_fadd(&self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        unsafe {
            let cname = CString::new(name).unwrap();
            LLVMBuildFAdd(self.builder, lhs, rhs, cname.as_ptr())
        }
    }

    pub fn build_sub(&self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        unsafe {
            let cname = CString::new(name).unwrap();
            LLVMBuildSub(self.builder, lhs, rhs, cname.as_ptr())
        }
    }

    pub fn build_fsub(&self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        unsafe {
            let cname = CString::new(name).unwrap();
            LLVMBuildFSub(self.builder, lhs, rhs, cname.as_ptr())
        }
    }

    pub fn build_mul(&self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        unsafe {
            let cname = CString::new(name).unwrap();
            LLVMBuildMul(self.builder, lhs, rhs, cname.as_ptr())
        }
    }

    pub fn build_fmul(&self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        unsafe {
            let cname = CString::new(name).unwrap();
            LLVMBuildFMul(self.builder, lhs, rhs, cname.as_ptr())
        }
    }

    pub fn build_sdiv(&self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        unsafe {
            let cname = CString::new(name).unwrap();
            LLVMBuildSDiv(self.builder, lhs, rhs, cname.as_ptr())
        }
    }

    pub fn build_fdiv(&self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        unsafe {
            let cname = CString::new(name).unwrap();
            LLVMBuildFDiv(self.builder, lhs, rhs, cname.as_ptr())
        }
    }

    pub fn build_alloca(&self, llvm_type: LLVMTypeRef, name: &str) -> LLVMValueRef {
        unsafe {
            let cname = CString::new(name).unwrap();
            LLVMBuildAlloca(self.builder, llvm_type, cname.as_ptr())
        }
    }

    pub fn build_store(&self, value: LLVMValueRef, ptr: LLVMValueRef) {
        unsafe {
            LLVMBuildStore(self.builder, value, ptr);
        }
    }

    pub fn build_load(&self, ptr: LLVMValueRef, name: &str) -> LLVMValueRef {
        unsafe {
            let cname = CString::new(name).unwrap();
            LLVMBuildLoad(self.builder, ptr, cname.as_ptr())
        }
    }

    pub fn build_gep(&self,
                     ptr: LLVMValueRef,
                     indices: &mut Vec<LLVMValueRef>,
                     name: &str)
                     -> LLVMValueRef {
        unsafe {
            let cname = CString::new(name).unwrap();
            let c_indices = indices.as_mut_ptr();
            let num = indices.len();
            LLVMBuildGEP(self.builder, ptr, c_indices, num as u32, cname.as_ptr())
        }
    }

    pub fn variable_struct(&self,
                           atype: LLVMTypeRef,
                           values: &mut Vec<LLVMValueRef>)
                           -> LLVMValueRef {

        let ptr = self.build_alloca(atype, "struct");
        for (i, v) in values.into_iter().enumerate() {
            let elem = self.build_gep(ptr, &mut vec![self.const_int32(0), self.const_int32(i as u64)], "var");
            self.build_store(*v, elem);
        }
        self.build_load(ptr, "load")
    }

    pub fn build_extract_value(&self, ptr: LLVMValueRef, index: i64, name: &str) -> LLVMValueRef {
        unsafe {
            let cname = CString::new(name).unwrap();
            LLVMBuildExtractValue(self.builder, ptr, index as u32, cname.as_ptr())
        }
    }

    pub fn build_icmp(&self,
                      op: LLVMIntPredicate,
                      lhs: LLVMValueRef,
                      rhs: LLVMValueRef,
                      name: &str)
                      -> LLVMValueRef {
        unsafe {
            let cname = CString::new(name).unwrap();
            LLVMBuildICmp(self.builder, op, lhs, rhs, cname.as_ptr())
        }
    }

    pub fn build_fcmp(&self,
                      op: LLVMRealPredicate,
                      lhs: LLVMValueRef,
                      rhs: LLVMValueRef,
                      name: &str)
                      -> LLVMValueRef {
        unsafe {
            let cname = CString::new(name).unwrap();
            LLVMBuildFCmp(self.builder, op, lhs, rhs, cname.as_ptr())
        }
    }

    pub fn build_branch(&self,
                        if_value: LLVMValueRef,
                        then_block: LLVMBasicBlockRef,
                        else_block: LLVMBasicBlockRef)
                        -> LLVMValueRef {
        unsafe { LLVMBuildCondBr(self.builder, if_value, then_block, else_block) }
    }

    pub fn build_br(&self, block: LLVMBasicBlockRef) {
        unsafe {
            LLVMBuildBr(self.builder, block);
        }
    }

    pub fn build_phi(&self, llvm_type: LLVMTypeRef, name: &str) -> LLVMValueRef {
        unsafe {
            let cname = CString::new(name).unwrap();
            LLVMBuildPhi(self.builder, llvm_type, cname.as_ptr())
        }
    }

    pub fn add_incoming(&self,
                        phi: LLVMValueRef,
                        values: *mut LLVMValueRef,
                        blocks: *mut LLVMBasicBlockRef,
                        count: u32) {
        unsafe { LLVMAddIncoming(phi, values, blocks, count) }
    }

    pub fn const_int(&self, value: u64) -> LLVMValueRef {
        unsafe {
            let llvm_type = self.int64_type();
            LLVMConstInt(llvm_type, value, 0)
        }
    }

    pub fn const_int32(&self, value: u64) -> LLVMValueRef {
        unsafe {
            let llvm_type = self.int32_type();
            LLVMConstInt(llvm_type, value, 0)
        }
    }

    pub fn const_float(&self, value: f64) -> LLVMValueRef {
        unsafe {
            let llvm_type = self.float_type();
            LLVMConstReal(llvm_type, value)
        }
    }

    pub fn const_named_struct(&self,
                              t: LLVMTypeRef,
                              values: &mut Vec<LLVMValueRef>)
                              -> LLVMValueRef {
        unsafe { LLVMConstNamedStruct(t, values.as_mut_ptr(), values.len() as u32) }
    }

    pub fn const_struct(&self, values: &mut Vec<LLVMValueRef>) -> LLVMValueRef {
        unsafe { LLVMConstStruct(values.as_mut_ptr(), values.len() as u32, 0) }
    }

    pub fn const_array(&self, atype: LLVMTypeRef, values: &mut Vec<LLVMValueRef>) -> LLVMValueRef {
        unsafe { LLVMConstArray(atype, values.as_mut_ptr(), values.len() as u32) }
    }

    pub fn ui_to_fp(&self, value: LLVMValueRef, typeref: LLVMTypeRef, name: &str) -> LLVMValueRef {
        unsafe {
            let cname = CString::new(name).unwrap();
            LLVMBuildSIToFP(self.builder, value, typeref, cname.as_ptr())
        }

    }

    pub fn type_of(&self, value: LLVMValueRef) -> LLVMTypeRef {
        unsafe { LLVMTypeOf(value) }
    }

    pub fn get_type_kind(&self, typeref: LLVMTypeRef) -> LLVMTypeKind {
        unsafe { LLVMGetTypeKind(typeref) }
    }

    pub fn void_type(&self) -> LLVMTypeRef {
        unsafe { LLVMVoidType() }
    }

    pub fn int32_type(&self) -> LLVMTypeRef {
        unsafe { LLVMInt32TypeInContext(self.context) }
    }

    pub fn int64_type(&self) -> LLVMTypeRef {
        unsafe { LLVMInt64TypeInContext(self.context) }
    }

    pub fn float_type(&self) -> LLVMTypeRef {
        unsafe { LLVMFloatTypeInContext(self.context) }
    }

    pub fn array_type(&self, atype: LLVMTypeRef, count: u32) -> LLVMTypeRef {
        unsafe { LLVMArrayType(atype, count) }
    }

    pub fn build_struct_named(&self, args: &mut Vec<LLVMTypeRef>, name: &str) -> LLVMTypeRef {
        unsafe {
            let cname = CString::new(name).unwrap();
            let t = LLVMStructCreateNamed(self.context, cname.as_ptr());
            LLVMStructSetBody(t, args.as_mut_ptr(), args.len() as u32, 0);
            t
        }
    }

    pub fn build_struct(&self, args: &mut Vec<LLVMTypeRef>) -> LLVMTypeRef {
        unsafe {
            let t = LLVMStructType(args.as_mut_ptr(), args.len() as u32, 0);
            //LLVMStructSetBody(t, args.as_mut_ptr(), args.len() as u32, 0);
            t
        }
    }

    pub fn print_module(&self) {
        unsafe {
            let ll_name = CString::new(self.name.clone() + ".ll").unwrap();
            LLVMPrintModuleToFile(self.module, ll_name.as_ptr(), ptr::null_mut());
        }
    }
}

impl Drop for LLVMSys {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}

#[test]
fn fib() {
    unsafe {
        let mut llvm = LLVMSys::new("main");
        let int64 = llvm.int64_type();
        //func fib(n)
        let fib_fun = llvm.add_func(int64, "fib", [int64].as_mut_ptr(), 1);
        let block = llvm.add_block("entry", fib_fun);
        llvm.position_builder_at_end(block);

        //n = args[0]
        let a0 = LLVMGetParam(fib_fun, 0);
        let n = llvm.build_alloca(int64, "n");
        llvm.build_store(a0, n);

        //load n
        let n1 = llvm.build_load(n, "n1");

        //cmp
        let two = llvm.const_int(2);
        let cmp = llvm.build_icmp(LLVMIntPredicate::LLVMIntSLT, n1, two, "cmp");

        //then, else, merge block
        let then_block = llvm.add_block("then", fib_fun);
        let else_block = llvm.add_block("else", fib_fun);
        let merge_block = llvm.add_block("merge", fib_fun);
        let result = llvm.build_branch(cmp, then_block, else_block);

        // then return n
        llvm.position_builder_at_end(then_block);
        let then_value = n1.clone();

        // goto merge
        llvm.build_br(merge_block);

        // else return fib(n-1) + fib(n-2)
        llvm.position_builder_at_end(else_block);
        //fib(n-1)
        let one = llvm.const_int(1);
        let n_1 = llvm.build_sub(n1.clone(), one, "n_1");
        let fibn_1 = llvm.add_call(fib_fun, [n_1].as_mut_ptr(), 1, "fib");

        //fib(n-2)
        let two = llvm.const_int(2);
        let n_2 = llvm.build_sub(n1.clone(), two, "n_2");
        let fibn_2 = llvm.add_call(fib_fun, [n_2].as_mut_ptr(), 1, "fib");

        let else_value = llvm.build_add(fibn_1, fibn_2, "add_else");

        //goto merge
        llvm.build_br(merge_block);

        //merge block
        llvm.position_builder_at_end(merge_block);

        //make phi
        let phi = llvm.build_phi(int64, "phi");
        let mut values = [then_value, else_value];
        let mut blocks = [then_block, else_block];
        LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);
        llvm.build_ret(phi);

        let main_fun = llvm.add_func(int64, "main", ptr::null_mut(), 0);
        let entry_block = llvm.add_block("entry", main_fun);
        llvm.position_builder_at_end(entry_block);
        let v = llvm.const_int(30);
        let val = llvm.add_call(fib_fun, [v].as_mut_ptr(), 1, "fib");
        llvm.build_ret(val);
        llvm.print_module();

    }
}

#[test]
fn p2() {
    unsafe {
        let mut llvm = LLVMSys::new("m2");
        let int64 = llvm.int64_type();
        let fun = llvm.add_func(int64, "main", ptr::null_mut(), 0);

        let entry_block = llvm.add_block("entry", fun);
        llvm.position_builder_at_end(entry_block);

        // a = 42;
        let a = llvm.build_alloca(int64, "a");
        let v = llvm.const_int(0);
        llvm.build_store(v, a);

        //if a == 0
        let new_a = llvm.build_load(a, "new_a");
        let zero = llvm.const_int(0);
        let cmp = llvm.build_icmp(LLVMIntPredicate::LLVMIntEQ, new_a, zero, "cmp");

        //then, else, merge block
        let then_block = llvm.add_block("then", fun);
        let else_block = llvm.add_block("else", fun);
        let merge_block = llvm.add_block("merge", fun);
        let result = llvm.build_branch(cmp, then_block, else_block);

        // then a = a + 52
        llvm.position_builder_at_end(then_block);
        let rhs = llvm.const_int(52);
        let then_value = llvm.build_add(new_a, rhs, "add_then");

        // goto merge
        llvm.build_br(merge_block);

        // else a = a + 20
        llvm.position_builder_at_end(else_block);
        let rhs = llvm.const_int(20);
        let else_value = llvm.build_add(new_a, rhs, "add_else");

        //goto merge
        llvm.build_br(merge_block);

        //merge block
        llvm.position_builder_at_end(merge_block);

        //make phi
        let phi = llvm.build_phi(int64, "phi");
        let mut values = [then_value, else_value];
        let mut blocks = [then_block, else_block];
        LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);
        llvm.build_ret(phi);
        llvm.print_module();

    }
}