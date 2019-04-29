use std::rc::Rc;

pub type RcExpr = Rc<Expr>;

pub enum Bop {
    Add,
    Sub,
    Mul,
    Div,
}

pub enum Expr {
    Bop {
        bop: Bop,
        e1: RcExpr,
        e2: RcExpr
    },
    Num(i32)
}

pub enum Stmt {
    Print(RcExpr)
}

pub struct Program {
    pub stmts: Vec<Stmt>
}
