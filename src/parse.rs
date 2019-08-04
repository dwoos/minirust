use crate::ast::*;
use crate::grammar;
use failure::{bail, Error};
use std::rc::Rc;

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

pub fn parse_expr(text: &str) -> Result<TypedExpr, Error> {
    let parsed = grammar::ExprParser::new().parse(&text);
    match parsed {
        Ok(parsed) => Ok(parsed),
        Err(_) => bail!("parse error"),
    }
}

pub fn untyped(e: Expr) -> TypedExpr {
    TypedExpr {
        ty: None,
        expr: Rc::new(e),
    }
}

#[cfg(test)]
#[macro_export]
macro_rules! expr {
    ($e:expr) => {
        crate::parse::parse_expr(&stringify!($e)).unwrap()
    };
}

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

    #[test]
    fn test_basic_expr_parse() {
        assert_eq!(expr!(3), untyped(Expr::Literal(Literal::Num(3))));
    }

    #[test]
    fn test_basic_program_parse() {
        let x = Identifier::Identifier("x".to_string());
        let y = Identifier::Identifier("y".to_string());
        let three = Expr::Literal(Literal::Num(3));
        assert_eq!(
            program!(let x = 3; let mut y = 3; print(x);),
            Program {
                stmts: vec![
                    Stmt::Let(Some(x.clone()), untyped(three.clone()), false),
                    Stmt::Let(Some(y.clone()), untyped(three.clone()), true),
                    Stmt::Let(
                        None,
                        untyped(Expr::Print(untyped(Expr::Var(x.clone())))),
                        false
                    )
                ]
            }
        );
    }

    #[test]
    fn test_block_program_parse() {
        let three = Expr::Literal(Literal::Num(3));
        let unit = Expr::Literal(Literal::Unit);
        assert_eq!(
            program!({
                print(3);
            };),
            Program {
                stmts: vec![Stmt::Let(
                    None,
                    untyped(Expr::Block(
                        vec![Stmt::Let(None, untyped(Expr::Print(untyped(three))), false)],
                        untyped(unit)
                    )),
                    false
                )]
            }
        );
    }

    #[test]
    fn test_while_parse() {
        let three = Expr::Literal(Literal::Num(3));
        let unit = Expr::Literal(Literal::Unit);
        let true_ = Expr::Literal(Literal::Bool(true));
        assert_eq!(
            program!(
                while true {
                    print(3);
                };),
            Program {
                stmts: vec![Stmt::Let(
                    None,
                    untyped(Expr::While {
                        condition: untyped(true_),
                        body: untyped(Expr::Block(
                            vec![Stmt::Let(None, untyped(Expr::Print(untyped(three))), false)],
                            untyped(unit)
                        ))
                    }),
                    false
                )]
            }
        );
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

}
