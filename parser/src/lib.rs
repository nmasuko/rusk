#![feature(box_syntax)]
#![feature(plugin)]
#![plugin(peg_syntax_ext)]
peg_file! rusk("rusk.rustpeg");
pub use rusk::module;

#[derive(Debug)]
pub struct Module {
    pub expdecl: Vec<ExpDecl>,
}

#[derive(Debug)]
pub enum ExpDecl {
    Exp(Expression),
    Decl(Decl),
}

#[derive(Debug)]
pub enum Decl {
    Typedecl(Typedecl),
}

#[derive(Debug)]
pub struct Typedecl {
    pub id: String,
    pub class: Classdecl,
}

/*#[derive(Debug)]
pub enum ClassTuple {
    Classdecl(Classdecl),
    Tupledecl(Tupledecl),
}*/

#[derive(Debug)]
pub struct Classdecl {
    pub args: Vec<TypeArg>,
}

#[derive(Debug)]
pub struct Tupledecl {
    pub args: Vec<String>,
}

#[derive(Debug)]
pub struct Expression {
    pub assign: Assign,
}

#[derive(Debug)]
pub struct Assign {
    pub id: Option<String>,
    pub comp: Comparative,
}

#[derive(Debug)]
pub struct Comparative {
    pub lhs: Additive,
    pub rhs: Option<Box<Comparer>>,
}

#[derive(Debug)]
pub struct Comparer {
    pub op: CompOp,
    pub add: Additive,
}

#[derive(Debug, Clone)]
pub enum CompOp {
    Lt,
    Le,
    Eq,
    Ne,
    Gt,
    Ge,
}

#[derive(Debug)]
pub struct Additive {
    pub lhs: Multitive,
    pub rhs: Option<Box<Adder>>,
}

#[derive(Debug)]
pub struct Adder {
    pub op: AddOp,
    pub add: Additive,
}

#[derive(Debug)]
pub enum AddOp {
    Add,
    Sub,
}

#[derive(Debug)]
pub struct Multitive {
    pub lhs: Unary,
    pub rhs: Option<Box<Multiplier>>,
}

#[derive(Debug)]
pub struct Multiplier {
    pub op: MulOp,
    pub mul: Multitive,
}

#[derive(Debug)]
pub enum MulOp {
    Mul,
    Div,
}

#[derive(Debug)]
pub struct Unary {
    pub unaryop: Option<String>,
    pub primary: Primary,
}

#[derive(Debug)]
pub struct If {
    pub condition: Box<Expression>,
    pub then_blk: Vec<Expression>,
    pub else_blk: Option<Vec<Expression>>,
}

#[derive(Debug)]
pub struct While {
    pub condition: Box<Expression>,
    pub exps: Vec<Expression>,
}

#[derive(Debug)]
pub struct Funccall {
    pub f_id: FuncdefID,
    pub params: Vec<Expression>,
}

#[derive(Debug)]
pub enum FuncdefID {
    Funcdef(Funcdef),
    ID(String),
}

#[derive(Debug)]
pub struct Funcdef {
    pub id: Option<String>,
    pub args: Vec<TypeArg>,
    pub ret: Option<String>,
    pub exps: Vec<Expression>,
}

#[derive(Debug)]
pub struct TypeArg {
    pub arg: String,
    pub atype: String,
}

#[derive(Debug)]
pub struct Classinst {
    pub id: String,
    pub params: Vec<Expression>,
}

/*#[derive(Debug)]
pub enum ClassdeclID {
    Classdecl(Classdecl),
    ID(String),
}*/

#[derive(Debug)]
pub enum Primary {
    Expression(Box<Expression>),
    If(If),
    While(While),
    Funccall(Funccall),
    Classinst(Classinst),
    Literal(Literal),
    Varacc(Varacc),
}

#[derive(Debug)]
pub struct Varacc {
    pub id: String,
    pub memb: Option<Vec<i64>>,
}

#[derive(Debug)]
pub struct Tuple {
    pub params: Vec<Expression>,
}

#[derive(Debug)]
pub struct Array {
    pub params: Vec<Expression>,
}

#[derive(Debug)]
pub enum Literal {
    Array(Array),
    Tuple(Tuple),
    Funcdef(Funcdef),
    Int(i64),
    Float(f64),
}
