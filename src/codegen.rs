use crate::ast::{Program,Stmt,RcExpr,Expr, Bop};
use crate::llvm;
use failure::Error;

use llvm_sys::prelude::*;

struct CodeGenContext {
    int_format_string: LLVMValueRef
}

fn compile_expr(cgc: &mut CodeGenContext, module: &mut llvm::Module, bb: LLVMBasicBlockRef,
                expr: &RcExpr) -> LLVMValueRef {
    match **expr {
        Expr::Num(x) => llvm::int32(x),
        Expr::Bop {ref bop, ref e1, ref e2} => {
            let arg1 = compile_expr(cgc, module, bb, e1);
            let arg2 = compile_expr(cgc, module, bb, e2);
            match bop {
                Bop::Add => llvm::add_add(module, bb, arg1, arg2),
                Bop::Sub => llvm::add_sub(module, bb, arg1, arg2),
                Bop::Mul => llvm::add_mul(module, bb, arg1, arg2),
                Bop::Div => llvm::add_sdiv(module, bb, arg1, arg2),
            }
        }
    }
}

fn compile_statement(cgc: &mut CodeGenContext,
                     module: &mut llvm::Module,
                     bb: LLVMBasicBlockRef,
                     next_bb: LLVMBasicBlockRef,
                     stmt: &Stmt) {
    match stmt {
        Stmt::Print(e) => {
            let arg1 = llvm::add_pointer_cast(module, bb, cgc.int_format_string,
                                          llvm::int8_ptr_type(), "");
            let arg2 = compile_expr(cgc, module, bb, e);
            llvm::add_function_call(module, bb, "printf", &mut [arg1, arg2], "");
            llvm::add_br(module, bb, next_bb);
        }
    }
}


pub fn compile_program(prog: Program, output_file: &str) -> Result<(), Error> {
    let mut module = llvm::create_module("main");
    let int_format_string = llvm::add_static_string(&mut module, "%d\n", "int_format_string");
    let main = llvm::add_main_fn(&mut module);
    let entry_block = llvm::add_basic_block(&mut module, main, "entry");
    let exit_block = llvm::add_basic_block(&mut module, main, "exit");
    llvm::add_ret(&mut module, exit_block, llvm::int32(0));
    let mut cgc = CodeGenContext {int_format_string};
    let mut last_block = entry_block;
    for stmt in prog.stmts.iter() {
        let mut next_block = llvm::add_basic_block(&mut module, main, "");
        compile_statement(&mut cgc, &mut module, last_block, next_block, stmt);
        last_block = next_block;
    }
    llvm::add_br(&mut module, last_block, exit_block);
    llvm::write_module(&mut module, output_file);
    Ok(())
}
