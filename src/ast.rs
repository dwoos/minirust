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
pub enum Expr<Annot> {
    Literal(Literal),

    Bop {
        bop: Bop,
        e1: AnnotExpr<Annot>,
        e2: AnnotExpr<Annot>,
    },
    Cmp {
        cmp: Cmp,
        e1: AnnotExpr<Annot>,
        e2: AnnotExpr<Annot>,
    },
    Not(AnnotExpr<Annot>),
    Borrow(AnnotExpr<Annot>, bool),
    Deref(AnnotExpr<Annot>),
    Or(AnnotExpr<Annot>, AnnotExpr<Annot>),
    And(AnnotExpr<Annot>, AnnotExpr<Annot>),
    If {
        condition: AnnotExpr<Annot>,
        then: AnnotExpr<Annot>,
        otherwise: AnnotExpr<Annot>,
    },
    While {
        condition: AnnotExpr<Annot>,
        body: AnnotExpr<Annot>,
    },

    Var(Identifier),
    Assign(AnnotExpr<Annot>, AnnotExpr<Annot>),

    FunCall(AnnotExpr<Annot>, Vec<AnnotExpr<Annot>>),
    Print(AnnotExpr<Annot>),
    Block(Vec<Stmt<AnnotExpr<Annot>>>, AnnotExpr<Annot>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct AnnotExpr<Annot> {
    pub expr: Rc<Expr<Annot>>,
    pub annot: Annot,
}

pub type BaseExpr = AnnotExpr<()>;

impl From<Expr<()>> for BaseExpr {
    fn from(expr: Expr<()>) -> BaseExpr {
        BaseExpr {
            expr: expr.into(),
            annot: (),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeAnnot {
    pub ty: RcType,
}

impl From<Type> for TypeAnnot {
    fn from(ty: Type) -> TypeAnnot {
        TypeAnnot { ty: ty.into() }
    }
}

impl From<RcType> for TypeAnnot {
    fn from(ty: RcType) -> TypeAnnot {
        TypeAnnot { ty: ty }
    }
}

pub type TypedExpr = AnnotExpr<TypeAnnot>;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt<E> {
    Let(Option<Identifier>, E, bool),
}

pub type BaseStmt = Stmt<BaseExpr>;

pub type TypedStmt = Stmt<TypedExpr>;

#[derive(Debug, PartialEq, Clone)]
pub enum Item<E> {
    Function(Identifier, Vec<(Identifier, RcType)>, RcType, E),
}

pub type BaseItem = Item<BaseExpr>;

pub type TypedItem = Item<TypedExpr>;

#[derive(Debug, PartialEq, Clone)]
pub struct Program<E> {
    pub items: Vec<Item<E>>,
}

pub type BaseProgram = Program<BaseExpr>;

pub type TypedProgram = Program<TypedExpr>;
