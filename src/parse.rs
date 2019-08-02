use crate::ast::*;
use crate::grammar;
use failure::{bail, Error};
use std::rc::Rc;

pub fn parse_program(text: &str) -> Result<Program, Error> {
    let parsed = grammar::ProgramParser::new().parse(&text);
    match parsed {
        Ok(parsed) => Ok(parsed),
        Err(_) => bail!("parse error"),
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

}
