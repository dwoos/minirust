#![allow(dead_code)]

use crate::ast::*;
use failure::{bail, Error};
use std::collections::HashMap;
use std::rc::Rc;

struct Context {
    envs: Vec<HashMap<Identifier, RcType>>,
}

impl Context {
    fn new() -> Self {
        Self {
            envs: vec![HashMap::new()],
        }
    }

    fn lookup(&self, id: &Identifier) -> Option<RcType> {
        for env in self.envs.iter().rev() {
            if let Some(ty) = env.get(&id) {
                return Some(ty.clone());
            }
        }
        None
    }

    fn set(&mut self, id: Identifier, ty: RcType) {
        let last_index = self.envs.len() - 1;
        self.envs[last_index].insert(id, ty);
    }

    fn push(&mut self) {
        self.envs.push(HashMap::new());
    }

    fn pop(&mut self) {
        if self.envs.len() == 1 {
            panic!("Attempt to pop last frame");
        }
        self.envs.pop();
    }
}

fn infer_expr(context: &mut Context, expr: &mut TypedExpr) -> Result<(), Error> {
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
