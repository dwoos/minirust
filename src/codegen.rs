use crate::ast::*;
use crate::llvm;
use failure::Error;

use llvm_sys::prelude::*;

struct CodeGenContext {
    int_format_string: LLVMValueRef,
    unit: LLVMValueRef
}

fn size(cgc: &mut CodeGenContext, ty: RcType) -> LLVMTypeRef {
    match *ty {
        Type::Int32 => llvm::int32_type(),
        Type::Bool => llvm::int1_type(),
        Type::Unit => llvm::int1_type()
    }
}

// Returns a value and an unterminated basic block
fn compile_expr(cgc: &mut CodeGenContext,
                module: &mut llvm::Module,
                f: LLVMValueRef,
                bb: LLVMBasicBlockRef,
                expr: &TypedExpr) -> (LLVMValueRef, LLVMBasicBlockRef) {
    match *expr.expr {
        Expr::Literal(ref l) => {
            (match l {
                Literal::Num(x) => llvm::int32(*x),
                Literal::Bool(b) => llvm::int1(*b),
                Literal::Unit => cgc.unit
            }, bb)
        }
        Expr::Bop {ref bop, ref e1, ref e2} => {
            let (arg1, bb) = compile_expr(cgc, module, f, bb, e1);
            let (arg2, bb) = compile_expr(cgc, module, f, bb, e2);
            (match bop {
                Bop::Add => llvm::add_add(module, bb, arg1, arg2),
                Bop::Sub => llvm::add_sub(module, bb, arg1, arg2),
                Bop::Mul => llvm::add_mul(module, bb, arg1, arg2),
                Bop::Div => llvm::add_sdiv(module, bb, arg1, arg2),
            }, bb)
        }
        Expr::If {ref condition, ref then, ref otherwise} => {
            let llvm_type = size(cgc, expr.ty.clone().unwrap());
            let result = llvm::add_alloca(module, bb, llvm_type);
            let (condition_val, bb) = compile_expr(cgc, module, f, bb, condition);
            let then_bb = llvm::add_basic_block(module, f, "");
            let otherwise_bb = llvm::add_basic_block(module, f, "");
            let exit_bb = llvm::add_basic_block(module, f, "");
            llvm::add_conditional_br(module, bb, condition_val, then_bb, otherwise_bb);
            let (then_value, then_bb) = compile_expr(cgc, module, f, then_bb, then);
            llvm::add_store(module, then_bb, then_value, result);
            llvm::add_br(module, then_bb, exit_bb);
            let (otherwise_value, otherwise_bb) = compile_expr(cgc, module, f, otherwise_bb, otherwise);
            llvm::add_store(module, otherwise_bb, otherwise_value, result);
            llvm::add_br(module, otherwise_bb, exit_bb);
            (llvm::add_load(module, exit_bb, result), exit_bb)
        }
        Expr::Print(ref e) => {
            let arg1 = llvm::add_pointer_cast(module, bb, cgc.int_format_string,
                                              llvm::int8_ptr_type(), "");
            let (arg2, bb) = compile_expr(cgc, module, f, bb, e);
            llvm::add_function_call(module, bb, "printf", &mut [arg1, arg2], "");
            (cgc.unit, bb)
        }
        Expr::Block(ref stmts, ref e) => {
            let mut bb = bb;
            for stmt in stmts.iter() {
                bb = compile_statement(cgc, module, f, bb, stmt);
            }
            compile_expr(cgc, module, f, bb, e)
        }
        _ => unimplemented!()
    }
}

// Returns an unterminated basic block
fn compile_statement(cgc: &mut CodeGenContext,
                     module: &mut llvm::Module,
                     f: LLVMValueRef,
                     bb: LLVMBasicBlockRef,
                     stmt: &Stmt) -> LLVMBasicBlockRef {
    match stmt {
        Stmt::Let(None, ref e, false) => {
            let (_, bb) = compile_expr(cgc, module, f, bb, e);
            bb
        }
        _ => unimplemented!()
    }
}


pub fn compile_program(prog: Program, output_file: &str) -> Result<(), Error> {
    let mut module = llvm::create_module("main");
    let int_format_string = llvm::add_static_string(&mut module, "%d\n", "int_format_string");
    let unit = llvm::add_static_int1(&mut module, false);
    let main = llvm::add_main_fn(&mut module);
    let entry_block = llvm::add_basic_block(&mut module, main, "entry");
    let exit_block = llvm::add_basic_block(&mut module, main, "exit");
    llvm::add_ret(&mut module, exit_block, llvm::int32(0));
    let mut cgc = CodeGenContext {int_format_string, unit};
    let mut last_block = entry_block;
    for stmt in prog.stmts.iter() {
        last_block = compile_statement(&mut cgc, &mut module, main, last_block, stmt);
    }
    llvm::add_br(&mut module, last_block, exit_block);
    llvm::write_module(&mut module, output_file);
    Ok(())
}
