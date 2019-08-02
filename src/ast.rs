use std::rc::Rc;

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Int32,
    Bool,
    Unit,
}

pub type RcType = Rc<Type>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Identifier {
    Identifier(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Bop {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Num(i32),
    Bool(bool),
    Unit,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Bop {
        bop: Bop,
        e1: TypedExpr,
        e2: TypedExpr,
    },
    Literal(Literal),
    If {
        condition: TypedExpr,
        then: TypedExpr,
        otherwise: TypedExpr,
    },
    Print(TypedExpr),
    Block(Vec<Stmt>, TypedExpr),
    Var(Identifier),
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedExpr {
    pub ty: Option<RcType>,
    pub expr: Rc<Expr>,
}

impl From<Expr> for TypedExpr {
    fn from(expr: Expr) -> TypedExpr {
        TypedExpr {
            ty: None,
            expr: expr.into(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Let(Option<Identifier>, TypedExpr, bool),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}
