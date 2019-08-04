#![allow(dead_code)]

use crate::ast::*;
use crate::context;
use failure::{bail, Error};
use std::rc::Rc;

struct Context {
    types: context::Context<(RcType, bool)>,
}

impl Context {
    fn new() -> Context {
        Context {
            types: context::Context::new(),
        }
    }
}

fn check_lhs(context: &mut Context, expr: &mut TypedExpr) -> Result<(), Error> {
    match Rc::get_mut(&mut expr.expr).unwrap() {
        Expr::Var(ref id) => {
            let ty = context.types.lookup(id);
            if let Some(ty) = ty {
                if ty.1 {
                    return Ok(());
                } else {
                    bail!("{:?} is not mutable", expr);
                }
            } else {
                bail!("Variable {:?} used before being defined");
            }
        }
        _ => bail!("Invalid lhs {:?}", expr),
    }
}

fn infer_expr(context: &mut Context, expr: &mut TypedExpr) -> Result<(), Error> {
    #[allow(unreachable_patterns)]
    let ty = match Rc::get_mut(&mut expr.expr).unwrap() {
        Expr::Literal(ref l) => match l {
            Literal::Num(_) => Type::Int32.into(),
            Literal::Bool(_) => Type::Bool.into(),
            Literal::Unit => Type::Unit.into(),
        },
        Expr::Bop {
            bop: _,
            ref mut e1,
            ref mut e2,
        } => {
            check_expr(context, e1, &Type::Int32.into())?;
            check_expr(context, e2, &Type::Int32.into())?;
            Type::Int32.into()
        }
        Expr::Cmp {
            ref cmp,
            ref mut e1,
            ref mut e2,
        } => {
            match cmp {
                Cmp::Eq | Cmp::Neq => {
                    // e1 and e2 have to be the same type
                    infer_expr(context, e1)?;
                    check_expr(context, e2, &e1.ty.clone().unwrap())?;
                    Type::Bool.into()
                }
                Cmp::Lt | Cmp::Le | Cmp::Gt | Cmp::Ge => {
                    // both args have to be numbers
                    check_expr(context, e1, &Type::Int32.into())?;
                    check_expr(context, e2, &Type::Int32.into())?;
                    Type::Bool.into()
                }
            }
        }
        Expr::Or(ref mut e1, ref mut e2) | Expr::And(ref mut e1, ref mut e2) => {
            check_expr(context, e1, &Type::Bool.into())?;
            check_expr(context, e2, &Type::Bool.into())?;
            Type::Bool.into()
        }
        Expr::Not(ref mut e) => {
            check_expr(context, e, &Type::Bool.into())?;
            Type::Bool.into()
        }
        Expr::If {
            ref mut condition,
            ref mut then,
            ref mut otherwise,
        } => {
            check_expr(context, condition, &Type::Bool.into())?;
            infer_expr(context, then)?;
            let ty = then.ty.clone().unwrap();
            check_expr(context, otherwise, &ty)?;
            ty
        }
        Expr::While {
            ref mut condition,
            ref mut body,
        } => {
            check_expr(context, condition, &Type::Bool.into())?;
            infer_expr(context, body)?;
            body.ty.clone().unwrap()
        }
        Expr::Print(ref mut e) => {
            check_expr(context, e, &Type::Int32.into())?;
            Type::Unit.into()
        }
        Expr::Block(ref mut stmts, ref mut e) => {
            context.types.push();
            for stmt in stmts.iter_mut() {
                check_stmt(context, stmt)?;
            }
            infer_expr(context, e)?;
            let ty = e.ty.clone().unwrap();
            context.types.pop();
            ty
        }
        Expr::FunCall(ref mut f, ref mut args) => {
            infer_expr(context, f)?;
            if let &Type::Function(ref targs, ref ret) = &*f.ty.clone().unwrap() {
                if args.len() != targs.len() {
                    bail!("Wrong number of arguments supplied to {:?}", f);
                }
                for i in 0..args.len() {
                    check_expr(context, &mut args[i], &targs[i])?;
                }
                ret.clone()
            } else {
                bail!("{:?} is not a function type", f.ty.clone())
            }
        }
        Expr::Var(ref mut id) => {
            let ty = context.types.lookup(id);
            if let Some(ty) = ty {
                ty.0.clone()
            } else {
                bail!("bad identifier {:?}", id);
            }
        }
        Expr::Assign(ref mut lhs, ref mut rhs) => {
            // lhs needs to be a valid left-hand side. for now, that means it
            // has to be a variable.
            infer_expr(context, lhs)?;
            check_lhs(context, lhs)?;
            check_expr(context, rhs, &lhs.ty.clone().unwrap())?;
            Type::Unit.into()
        }
        _ => unimplemented!(),
    };
    expr.ty = Some(ty);
    Ok(())
}

fn check_expr(context: &mut Context, expr: &mut TypedExpr, ty: &RcType) -> Result<(), Error> {
    infer_expr(context, expr)?;
    if &expr.ty.clone().unwrap() == ty {
        Ok(())
    } else {
        bail!("types don't match");
    }
}

fn check_stmt(context: &mut Context, stmt: &mut Stmt) -> Result<(), Error> {
    match stmt {
        Stmt::Let(None, ref mut e, false) => {
            infer_expr(context, e)?;
        }
        Stmt::Let(Some(id), ref mut e, is_mut) => {
            infer_expr(context, e)?;
            context
                .types
                .set(id.clone(), (e.ty.clone().unwrap(), *is_mut));
        }
        _ => unimplemented!(),
    }
    Ok(())
}

fn check_item(context: &mut Context, item: &mut Item) -> Result<(), Error> {
    #[allow(unreachable_patterns)]
    match item {
        Item::Function(_, ref args, ref ret, ref mut body) => {
            context.types.push();
            for (name, ty) in args {
                context.types.set(name.clone(), (ty.clone(), false));
            }
            check_expr(context, body, &ret.clone())?;
            context.types.pop();
            // this fn's type should already be in the context, no need to add it
            Ok(())
        }
        _ => unimplemented!(),
    }
}

pub fn check_program(program: &mut Program) -> Result<(), Error> {
    let mut context = Context::new();
    // add declarations to top-level context
    for item in program.items.iter() {
        #[allow(unreachable_patterns)]
        match item {
            Item::Function(name, ref args, ref ret, _) => {
                context.types.set(
                    name.clone(),
                    (
                        Type::Function(
                            args.iter().map(|(_, ty)| ty.clone()).collect(),
                            ret.clone(),
                        )
                        .into(),
                        false,
                    ),
                );
            }
            _ => unimplemented!(),
        }
    }
    for item in program.items.iter_mut() {
        check_item(&mut context, item)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::parse::*;
    use crate::typecheck::*;
    use std::rc::Rc;

    macro_rules! assert_expr_infers {
        ($e:expr, $t: ty) => {{
            let mut e = expr!($e);
            let ty = Rc::new(ty!($t));
            let mut context = Context::new();
            infer_expr(&mut context, &mut e).expect("unexpected type error");
            assert_eq!(e.ty.unwrap(), ty);
        }};
    }

    macro_rules! assert_expr_ill_typed {
        ($e:expr) => {{
            let mut e = expr!($e);
            let mut context = Context::new();
            assert!(infer_expr(&mut context, &mut e).is_err());
        }};
    }

    macro_rules! assert_program_well_typed {
        ($($p:tt)*) => {{
            let mut p = program!($($p)*);
            assert!(check_program(&mut p).is_ok());
        }};
    }

    macro_rules! assert_program_ill_typed {
        ($($p:tt)*) => {{
            let mut p = program!($($p)*);
            assert!(check_program(&mut p).is_err());
        }};
    }

    macro_rules! assert_main_program_well_typed {
        ($($p:tt)*) => {{
            assert_program_well_typed!(fn main() { $($p)* });
        }};
    }

    macro_rules! assert_main_program_ill_typed {
        ($($p:tt)*) => {{
            assert_program_ill_typed!(fn main() { $($p)* });
        }};
    }

    #[test]
    fn test_basic_expr() {
        assert_expr_infers!(3, i32);
        assert_expr_ill_typed!(3 + true);
        assert_expr_infers!(
            {
                print(2);
                3
            },
            i32
        );
        assert_expr_infers!(
            {
                print(2);
                3;
            },
            ()
        );
        assert_expr_infers!(if (true) { 3 } else { 4 }, i32);
        assert_expr_ill_typed!(if (true) { 3 } else { false });
    }

    #[test]
    fn test_basic_program() {
        assert_main_program_well_typed!(
            let x = 3;
            print(x);
        );
        assert_main_program_ill_typed!(
            let x = 3;
            print(y);
        );
        assert_main_program_ill_typed!(
            let x = 3 + true;
            print(x);
        );
    }

    #[test]
    fn test_block_program() {
        assert_main_program_well_typed!(
            let x = 3;
            {
                print(x);
            };
        );
        assert_main_program_ill_typed!(
            {
                let x = 3;
            };
            print(x);
        );
    }

    #[test]
    fn test_assignment() {
        assert_main_program_well_typed!(
            let mut x = 3;
            x = 4;
            print(x);
        );

        assert_main_program_ill_typed!(
            let x = 3;
            x = 4;
            print(x);
        );

        assert_main_program_well_typed!(
            let mut x = false;
            x = true;
        );

        assert_main_program_ill_typed!(
            let mut x = 4;
            x = true;
        );
    }

    #[test]
    fn test_while() {
        assert_main_program_well_typed!(while true {};);
        assert_main_program_ill_typed!(while 3 {};);
    }

    #[test]
    fn test_while_cmp() {
        assert_main_program_well_typed!(
            let mut x = 10;
            while x > 0 {
                print(x);
                x = x - 1;
            };);
    }

    #[test]
    fn test_logic_cmp() {
        assert_expr_infers!(!(3 == 4) && (true == false) || 3 < 4, bool);
        assert_expr_ill_typed!(true < false);
        assert_expr_ill_typed!(!3);
    }

    #[test]
    fn test_complex_functions() {
        assert_program_well_typed!(
            fn foobar(x: i32, silly: bool) -> bool {
                print(x);
                silly
            }
        );
        assert_program_ill_typed!(
            fn foobar(x: i32, silly: bool) -> bool {
                print(x);
                silly;
            }
        );
        assert_program_ill_typed!(
            fn foobar(x: i32, silly: bool) -> bool {
                print(x);
                x
            }
        );
    }

    #[test]
    fn test_multiple_functions() {
        assert_program_well_typed!(
            fn foobar(x: i32, silly: bool) -> bool {
                print(x);
                silly
            }

            fn foobar_prime(y: i32, z: i32) {
                print(y);
                print(z);
            }
        );
        assert_program_ill_typed!(
            fn foobar(x: i32, silly: bool) -> bool {
                print(x);
                silly
            }

            fn foobar_prime(y: i32, z: i32) {
                print(y);
                print(x);
            }
        );
    }

    #[test]
    fn test_funcall() {
        assert_program_well_typed!(
            fn foo(x: i32) -> i32 {
                if x > 3 {
                    3
                } else {
                    x
                }
            }

            fn main() {
                print(foo(4));
            }
        );

        assert_program_ill_typed!(
            fn foo(x: i32) -> i32 {
                if x > 3 {
                    3
                } else {
                    x
                }
            }

            fn main() {
                print(foo(true));
            }
        );

        assert_program_ill_typed!(
            fn foo(x: i32) -> bool {
                if x > 3 {
                    true
                } else {
                    false
                }
            }

            fn main() {
                print(foo(3));
            }
        );
    }

    #[test]
    fn test_recursion() {
        assert_program_well_typed!(
            fn fact(x: i32) -> i32 {
                if (x <= 1) {
                    1
                } else {
                    x * fact(x - 1)
                }
            }
        );

        assert_program_well_typed!(
            fn even(x: i32) -> bool {
                if (x == 0) {
                    true
                } else {
                    !odd(x)
                }
            }

            fn odd(x: i32) -> bool {
                if (x == 1) {
                    true
                } else {
                    even(x - 1)
                }
            }
        );
    }
}
