use std::rc::Rc;

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Int32,
    Bool
}

pub type RcType = Rc<Type>;

#[derive(Debug)]
pub enum Bop {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub enum Literal {
    Num(i32),
    Bool(bool)
}

#[derive(Debug)]
pub enum Expr {
    Bop {
        bop: Bop,
        e1: TypedExpr,
        e2: TypedExpr
    },
    Literal(Literal),
    If {
        condition: TypedExpr,
        then: TypedExpr,
        otherwise: TypedExpr
    }
}

#[derive(Debug)]
pub struct TypedExpr {
    pub ty: Option<RcType>,
    pub expr: Rc<Expr>
}

impl From<Expr> for TypedExpr {
    fn from(expr: Expr) -> TypedExpr {
        TypedExpr { ty:None, expr: expr.into() }
    }
}

#[derive(Debug)]
pub enum Stmt {
    Print(TypedExpr)
}

#[derive(Debug)]
pub struct Program {
    pub stmts: Vec<Stmt>
}
