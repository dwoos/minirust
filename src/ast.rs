use std::rc::Rc;

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Int32,
    Bool,
    Unit,
    Function(Vec<RcType>, RcType),
    Borrow(RcType, bool),
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
pub enum Cmp {
    Eq,
    Neq,
    Le,
    Lt,
    Ge,
    Gt,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Num(i32),
    Bool(bool),
    Unit,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Literal(Literal),

    Bop {
        bop: Bop,
        e1: TypedExpr,
        e2: TypedExpr,
    },
    Cmp {
        cmp: Cmp,
        e1: TypedExpr,
        e2: TypedExpr,
    },
    Not(TypedExpr),
    Borrow(TypedExpr, bool),
    Deref(TypedExpr),
    Or(TypedExpr, TypedExpr),
    And(TypedExpr, TypedExpr),
    If {
        condition: TypedExpr,
        then: TypedExpr,
        otherwise: TypedExpr,
    },
    While {
        condition: TypedExpr,
        body: TypedExpr,
    },

    Var(Identifier),
    Assign(TypedExpr, TypedExpr),

    FunCall(TypedExpr, Vec<TypedExpr>),
    Print(TypedExpr),
    Block(Vec<Stmt>, TypedExpr),
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
pub enum Item {
    Function(Identifier, Vec<(Identifier, RcType)>, RcType, TypedExpr),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub items: Vec<Item>,
}
