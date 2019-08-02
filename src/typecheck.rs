#![allow(dead_code)]

use crate::ast::*;
use crate::context;
use failure::{bail, Error};
use std::rc::Rc;

type Context = context::Context<RcType>;

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
        Expr::Print(ref mut e) => {
            check_expr(context, e, &Type::Int32.into())?;
            Type::Unit.into()
        }
        Expr::Block(ref mut stmts, ref mut e) => {
            context.push();
            for stmt in stmts.iter_mut() {
                check_stmt(context, stmt)?;
            }
            infer_expr(context, e)?;
            let ty = e.ty.clone().unwrap();
            context.pop();
            ty
        }
        Expr::Var(ref mut id) => {
            let ty = context.lookup(id);
            if let Some(ty) = ty {
                ty.clone()
            } else {
                bail!("bad identifier {:?}", id);
            }
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
        Stmt::Let(Some(id), ref mut e, false) => {
            infer_expr(context, e)?;
            context.set(id.clone(), e.ty.clone().unwrap());
        }
        _ => unimplemented!(),
    }
    Ok(())
}

pub fn check_program(program: &mut Program) -> Result<(), Error> {
    let mut context = Context::new();
    for stmt in program.stmts.iter_mut() {
        check_stmt(&mut context, stmt)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::parse::*;
    use crate::typecheck::{infer_expr, Context};
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

    #[test]
    fn test_basic_expr_typechecking() {
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

}
