use crate::ast::*;
use crate::grammar;
use failure::{bail, Error};

pub fn parse_program(text: &str) -> Result<Program, Error> {
    let parsed = grammar::ProgramParser::new().parse(&text);
    match parsed {
        Ok(parsed) => Ok(parsed),
        Err(s) => bail!(s.to_string()),
    }
}

#[cfg(test)]
#[macro_export]
macro_rules! program {
    ($($prog:tt)*) => {
        crate::parse::parse_program(&stringify!($($prog)*)).unwrap()
    };
}

#[cfg(test)]
pub fn parse_expr(text: &str) -> Result<TypedExpr, Error> {
    let parsed = grammar::ExprParser::new().parse(&text);
    match parsed {
        Ok(parsed) => Ok(parsed),
        Err(e) => bail!(e.to_string()),
    }
}

#[cfg(test)]
pub fn untyped(e: Expr) -> TypedExpr {
    TypedExpr {
        ty: None,
        expr: std::rc::Rc::new(e),
    }
}

#[cfg(test)]
#[macro_export]
macro_rules! expr {
    ($e:expr) => {
        crate::parse::parse_expr(&stringify!($e)).unwrap()
    };
}

#[cfg(test)]
pub fn parse_type(text: &str) -> Result<Type, Error> {
    let parsed = grammar::TypeParser::new().parse(&text);
    match parsed {
        Ok(parsed) => Ok(parsed),
        Err(_) => bail!("parse error"),
    }
}

#[cfg(test)]
#[macro_export]
macro_rules! ty {
    ($t:ty) => {
        crate::parse::parse_type(&stringify!($t)).unwrap()
    };
}

#[cfg(test)]
pub(crate) use {expr, program, ty};

#[cfg(test)]
mod tests {
    use crate::parse::*;

    // expr tests

    #[test]
    fn test_basic_expr() {
        assert_eq!(expr!(3), untyped(Expr::Literal(Literal::Num(3))));
    }

    #[test]
    fn test_associativity() {
        assert_eq!(expr!(3 + 4 + 5), expr!(3 + (4 + 5)));
    }

    #[test]
    fn test_cmp_logic() {
        let three = untyped(Expr::Literal(Literal::Num(3)));
        let four = untyped(Expr::Literal(Literal::Num(4)));
        assert_eq!(
            expr!((3 == 4 || 3 != 4) && (3 < 4 || 3 <= 4 || 3 > 4 || !(3 >= 4))),
            untyped(Expr::And(
                untyped(Expr::Or(
                    untyped(Expr::Cmp {
                        cmp: Cmp::Eq,
                        e1: three.clone(),
                        e2: four.clone()
                    }),
                    untyped(Expr::Cmp {
                        cmp: Cmp::Neq,
                        e1: three.clone(),
                        e2: four.clone()
                    })
                )),
                untyped(Expr::Or(
                    untyped(Expr::Cmp {
                        cmp: Cmp::Lt,
                        e1: three.clone(),
                        e2: four.clone()
                    }),
                    untyped(Expr::Or(
                        untyped(Expr::Cmp {
                            cmp: Cmp::Le,
                            e1: three.clone(),
                            e2: four.clone()
                        }),
                        untyped(Expr::Or(
                            untyped(Expr::Cmp {
                                cmp: Cmp::Gt,
                                e1: three.clone(),
                                e2: four.clone()
                            }),
                            untyped(Expr::Not(untyped(Expr::Cmp {
                                cmp: Cmp::Ge,
                                e1: three.clone(),
                                e2: four.clone()
                            })))
                        ))
                    ))
                ))
            ))
        );
    }

    // program tests

    fn main_fn(stmts: Vec<Stmt>) -> Item {
        Item::Function(
            Identifier::Identifier("main".to_string()),
            vec![],
            Type::Unit.into(),
            untyped(Expr::Block(stmts, untyped(Expr::Literal(Literal::Unit)))),
        )
    }

    #[test]
    fn test_basic_program() {
        let x = Identifier::Identifier("x".to_string());
        let y = Identifier::Identifier("y".to_string());
        let three = Expr::Literal(Literal::Num(3));
        assert_eq!(
            program!(
                fn main() {
                    let x = 3;
                    let mut y = 3;
                    print(x);
                }
            ),
            Program {
                items: vec![main_fn(vec![
                    Stmt::Let(Some(x.clone()), untyped(three.clone()), false),
                    Stmt::Let(Some(y.clone()), untyped(three.clone()), true),
                    Stmt::Let(
                        None,
                        untyped(Expr::Print(untyped(Expr::Var(x.clone())))),
                        false
                    )
                ])]
            }
        );
    }

    #[test]
    fn test_block_program() {
        let three = Expr::Literal(Literal::Num(3));
        let unit = Expr::Literal(Literal::Unit);
        assert_eq!(
            program!(
                fn main() {
                    {
                        print(3);
                    };
                }
            ),
            Program {
                items: vec![main_fn(vec![Stmt::Let(
                    None,
                    untyped(Expr::Block(
                        vec![Stmt::Let(None, untyped(Expr::Print(untyped(three))), false)],
                        untyped(unit)
                    )),
                    false
                )])]
            }
        );
    }

    // Need to skip formatting here since rustfmt doesn't like the semicolon
    // after the while loop
    #[test]
    #[rustfmt::skip]
    fn test_while() {
        let three = Expr::Literal(Literal::Num(3));
        let unit = Expr::Literal(Literal::Unit);
        let true_ = Expr::Literal(Literal::Bool(true));
        assert_eq!(
            program!(
                fn main() {
                    while true {
                        print(3);
                    };
                }
            ),
            Program {
                items: vec![main_fn(vec![Stmt::Let(
                    None,
                    untyped(Expr::While {
                        condition: untyped(true_),
                        body: untyped(Expr::Block(
                            vec![Stmt::Let(None, untyped(Expr::Print(untyped(three))), false)],
                            untyped(unit)
                        ))
                    }),
                    false
                )])]
            }
        );
    }

    #[test]
    fn test_complex_function() {
        assert_eq!(
            program!(
                fn foobar(x: i32, silly: bool) -> bool {
                    print(x);
                    silly
                }
            ),
            Program {
                items: vec![Item::Function(
                    Identifier::Identifier("foobar".to_string()),
                    vec![
                        (Identifier::Identifier("x".to_string()), Type::Int32.into()),
                        (
                            Identifier::Identifier("silly".to_string()),
                            Type::Bool.into()
                        )
                    ],
                    Type::Bool.into(),
                    expr!({
                        print(x);
                        silly
                    })
                )]
            }
        );
    }

    #[test]
    fn test_borrows() {
        assert_eq!(
            program!(
                fn main(a: &mut i32) -> () {
                    let x = 3;
                    let y = &x;
                    let z = &mut y;
                }
            ),
            Program {
                items: vec![Item::Function(
                    Identifier::Identifier("main".to_string()),
                    vec![(
                        Identifier::Identifier("a".to_string()),
                        Type::Borrow(Type::Int32.into(), true).into()
                    )],
                    Type::Unit.into(),
                    untyped(Expr::Block(
                        vec![
                            Stmt::Let(
                                Some(Identifier::Identifier("x".to_string())),
                                untyped(Expr::Literal(Literal::Num(3))),
                                false
                            ),
                            Stmt::Let(
                                Some(Identifier::Identifier("y".to_string())),
                                untyped(Expr::Borrow(
                                    untyped(Expr::Var(Identifier::Identifier("x".to_string()))),
                                    false
                                )),
                                false
                            ),
                            Stmt::Let(
                                Some(Identifier::Identifier("z".to_string())),
                                untyped(Expr::Borrow(
                                    untyped(Expr::Var(Identifier::Identifier("y".to_string()))),
                                    true
                                )),
                                false
                            )
                        ],
                        untyped(Expr::Literal(Literal::Unit))
                    ))
                )]
            }
        );
    }
}
