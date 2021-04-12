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

fn check_place_expr(_context: &mut Context, expr: &TypedExpr) -> Result<(), Error> {
    match &*expr.expr {
        Expr::Var(_) => Ok(()),
        Expr::Deref(_) => Ok(()),
        _ => bail!("Invalid lhs {:?}", expr),
    }
}

fn typed(expr: impl Into<Rc<Expr<TypeAnnot>>>, ty: impl Into<TypeAnnot>) -> TypedExpr {
    TypedExpr {
        expr: expr.into(),
        annot: ty.into(),
    }
}

fn infer_expr(context: &mut Context, expr: &BaseExpr) -> Result<TypedExpr, Error> {
    #[allow(unreachable_patterns)]
    match &*expr.expr {
        Expr::Literal(ref l) => match l {
            Literal::Num(n) => Ok(typed(Expr::Literal(Literal::Num(*n)), Type::Int32)),
            Literal::Bool(b) => Ok(typed(Expr::Literal(Literal::Bool(*b)), Type::Bool)),
            Literal::Unit => Ok(typed(Expr::Literal(Literal::Unit), Type::Unit)),
        },
        Expr::Bop {
            bop,
            ref e1,
            ref e2,
        } => {
            let e1 = check_expr(context, e1, &Type::Int32.into())?;
            let e2 = check_expr(context, e2, &Type::Int32.into())?;
            Ok(typed(
                Expr::Bop {
                    bop: bop.clone(),
                    e1,
                    e2,
                },
                Type::Int32,
            ))
        }
        Expr::Cmp {
            ref cmp,
            ref e1,
            ref e2,
        } => {
            match cmp {
                Cmp::Eq | Cmp::Neq => {
                    // e1 and e2 have to be the same type
                    let e1 = infer_expr(context, e1)?;
                    let e2 = check_expr(context, e2, &e1.annot.ty.clone())?;
                    Ok(typed(
                        Expr::Cmp {
                            cmp: cmp.clone(),
                            e1,
                            e2,
                        },
                        Type::Bool,
                    ))
                }
                Cmp::Lt | Cmp::Le | Cmp::Gt | Cmp::Ge => {
                    // both args have to be numbers
                    let e1 = check_expr(context, e1, &Type::Int32.into())?;
                    let e2 = check_expr(context, e2, &Type::Int32.into())?;
                    Ok(typed(
                        Expr::Cmp {
                            cmp: cmp.clone(),
                            e1,
                            e2,
                        },
                        Type::Bool,
                    ))
                }
            }
        }
        Expr::Or(ref e1, ref e2) => {
            let e1 = check_expr(context, e1, &Type::Bool.into())?;
            let e2 = check_expr(context, e2, &Type::Bool.into())?;
            Ok(typed(Expr::Or(e1, e2), Type::Bool))
        }
        Expr::And(ref e1, ref e2) => {
            let e1 = check_expr(context, e1, &Type::Bool.into())?;
            let e2 = check_expr(context, e2, &Type::Bool.into())?;
            Ok(typed(Expr::Or(e1, e2), Type::Bool))
        }
        Expr::Not(ref e) => {
            let e = check_expr(context, e, &Type::Bool.into())?;
            Ok(typed(Expr::Not(e), Type::Bool))
        }
        Expr::If {
            ref condition,
            ref then,
            ref otherwise,
        } => {
            let condition = check_expr(context, condition, &Type::Bool.into())?;
            let then = infer_expr(context, then)?;
            let ty = then.annot.ty.clone();
            let otherwise = check_expr(context, otherwise, &ty)?;
            Ok(typed(
                Expr::If {
                    condition,
                    then,
                    otherwise,
                },
                ty,
            ))
        }
        Expr::While {
            ref condition,
            ref body,
        } => {
            let condition = check_expr(context, condition, &Type::Bool.into())?;
            let body = infer_expr(context, body)?;
            let ty = body.annot.ty.clone();
            Ok(typed(Expr::While { condition, body }, ty))
        }
        Expr::Print(ref e) => {
            let e = check_expr(context, e, &Type::Int32.into())?;
            Ok(typed(Expr::Print(e), Type::Unit))
        }
        Expr::Block(ref stmts, ref e) => {
            context.types.push();
            let mut checked_stmts = vec![];
            // TODO: should be a way to do this as a map() but the Result makes it inconvenient
            for stmt in stmts.iter() {
                checked_stmts.push(check_stmt(context, stmt)?);
            }
            let e = infer_expr(context, e)?;
            let ty = e.annot.ty.clone();
            context.types.pop();
            Ok(typed(Expr::Block(checked_stmts, e), ty))
        }
        Expr::FunCall(ref f, ref args) => {
            let f = infer_expr(context, f)?;
            if let &Type::Function(ref targs, ref ret) = &*f.annot.ty.clone() {
                if args.len() != targs.len() {
                    bail!("Wrong number of arguments supplied to {:?}", f);
                }
                let mut checked_args = vec![];
                for i in 0..args.len() {
                    checked_args.push(check_expr(context, &args[i], &targs[i])?);
                }
                Ok(typed(Expr::FunCall(f, checked_args), ret.clone()))
            } else {
                bail!("{:?} is not a function type", f.annot.ty.clone())
            }
        }
        Expr::Var(ref id) => {
            let ty = context.types.lookup(id);
            if let Some(ty) = ty {
                Ok(typed(Expr::Var(id.clone()), ty.0.clone()))
            } else {
                bail!("bad identifier {:?}", id);
            }
        }
        Expr::Assign(ref lhs, ref rhs) => {
            // lhs needs to be a valid left-hand side
            let lhs = infer_expr(context, lhs)?;
            check_place_expr(context, &lhs)?;
            let rhs = check_expr(context, rhs, &lhs.annot.ty.clone())?;
            Ok(typed(Expr::Assign(lhs, rhs), Type::Unit))
        }
        Expr::Borrow(ref e, is_mut) => {
            let e = infer_expr(context, e)?;
            let ty = e.annot.ty.clone();
            Ok(typed(Expr::Borrow(e, *is_mut), Type::Borrow(ty, *is_mut)))
        }
        Expr::Deref(ref e) => {
            let e = infer_expr(context, e)?;
            if let Type::Borrow(ref ty, _) = *e.annot.ty.clone() {
                Ok(typed(Expr::Deref(e), ty.clone()))
            } else {
                bail!("{:?} is not a borrow", e.annot.ty.clone());
            }
        }
        _ => unimplemented!(),
    }
}

fn check_expr(context: &mut Context, expr: &BaseExpr, ty: &RcType) -> Result<TypedExpr, Error> {
    let expr = infer_expr(context, expr)?;
    if &expr.annot.ty.clone() == ty {
        Ok(expr)
    } else {
        bail!("types don't match");
    }
}

fn check_stmt(context: &mut Context, stmt: &BaseStmt) -> Result<TypedStmt, Error> {
    match stmt {
        Stmt::Let(None, ref e, false) => {
            let e = infer_expr(context, e)?;
            Ok(TypedStmt::Let(None, e, false))
        }
        Stmt::Let(Some(id), ref e, is_mut) => {
            let e = infer_expr(context, e)?;
            context.types.set(id.clone(), (e.annot.ty.clone(), *is_mut));
            Ok(TypedStmt::Let(Some(id.clone()), e, *is_mut))
        }
        _ => unimplemented!(),
    }
}

fn check_item(context: &mut Context, item: &BaseItem) -> Result<TypedItem, Error> {
    #[allow(unreachable_patterns)]
    match item {
        Item::Function(ref name, ref args, ref ret, ref body) => {
            context.types.push();
            for (name, ty) in args {
                context.types.set(name.clone(), (ty.clone(), false));
            }
            let body = check_expr(context, body, &ret.clone())?;
            context.types.pop();
            // this fn's type should already be in the context, no need to add it
            Ok(TypedItem::Function(
                name.clone(),
                args.clone(),
                ret.clone(),
                body,
            ))
        }
        _ => unimplemented!(),
    }
}

pub fn check_program(program: &BaseProgram) -> Result<TypedProgram, Error> {
    let mut context = Context::new();
    let mut items = vec![];
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
    for item in program.items.iter() {
        items.push(check_item(&mut context, item)?);
    }
    Ok(TypedProgram { items })
}

#[cfg(test)]
mod tests {
    use crate::parse::*;
    use crate::typecheck::*;
    use std::rc::Rc;

    macro_rules! assert_expr_infers {
        ($e:expr, $t: ty) => {{
            let e = expr!($e);
            let ty = Rc::new(ty!($t));
            let mut context = Context::new();
            let e = infer_expr(&mut context, &e).expect("unexpected type error");
            assert_eq!(e.annot.ty, ty);
        }};
    }

    macro_rules! assert_expr_ill_typed {
        ($e:expr) => {{
            let e = expr!($e);
            let mut context = Context::new();
            assert!(infer_expr(&mut context, &e).is_err());
        }};
    }

    macro_rules! assert_program_well_typed {
        ($($p:tt)*) => {{
            let  p = program!($($p)*);
            assert!(check_program(& p).is_ok());
        }};
    }

    macro_rules! assert_program_ill_typed {
        ($($p:tt)*) => {{
            let  p = program!($($p)*);
            assert!(check_program(& p).is_err());
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

        // We don't check mutability in the typechecker
        assert_main_program_well_typed!(
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

    #[test]
    fn test_borrows_derefs() {
        assert_program_well_typed!(
            fn main(x: &i32) -> i32 {
                let y = *x;
                y
            }
        );
        assert_program_ill_typed!(
            fn main(x: &i32) -> i32 {
                let y = **x;
                y
            }
        );
        assert_program_well_typed!(
            fn main(x: &i32) -> i32 {
                *x = 4;
                *x
            }
        );
        assert_program_ill_typed!(
            fn main(x: &i32) -> () {
                **x = 4;
            }
        );

        assert_program_ill_typed!(
            fn main(x: &mut &i32) -> () {
                *x = 4;
            }
        );

        assert_program_well_typed!(
            fn main() -> () {
                *&5 = 4;
            }
        );

        assert_program_ill_typed!(
            fn main() -> () {
                *&5 = true;
            }
        );
    }
}
