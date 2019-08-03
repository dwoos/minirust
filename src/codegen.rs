use crate::ast::*;
use crate::context::Context;
use failure::Error;

use hlllvm::{LLVMBasicBlock, LLVMFunction, LLVMName::Name, LLVMType, LLVMValue, Module};

type SymbolTable = Context<LLVMValue>;

struct CodeGenContext {
    int_format_string: LLVMValue,
    unit: LLVMValue,
    bool_false: LLVMValue,
    bool_true: LLVMValue,
    printf_fn: LLVMFunction,
    module: Module,
    symtab: SymbolTable,
}

fn size(_cgc: &mut CodeGenContext, ty: RcType) -> LLVMType {
    match *ty {
        Type::Int32 => LLVMType::Int32,
        Type::Bool => LLVMType::Int1,
        Type::Unit => LLVMType::Int1,
    }
}

// Returns a value and an unterminated basic block
fn compile_expr(
    cgc: &mut CodeGenContext,
    f: LLVMFunction,
    bb: LLVMBasicBlock,
    expr: &TypedExpr,
) -> (LLVMValue, LLVMBasicBlock) {
    #[allow(unreachable_patterns)]
    match *expr.expr {
        Expr::Literal(ref l) => (
            match l {
                Literal::Num(x) => cgc.module.const_int32(*x),
                Literal::Bool(b) => {
                    if *b {
                        cgc.bool_true
                    } else {
                        cgc.bool_false
                    }
                }
                Literal::Unit => cgc.unit,
            },
            bb,
        ),
        Expr::Bop {
            ref bop,
            ref e1,
            ref e2,
        } => {
            let (arg1, bb) = compile_expr(cgc, f, bb, e1);
            let (arg2, bb) = compile_expr(cgc, f, bb, e2);
            (
                match bop {
                    Bop::Add => cgc.module.add(bb, arg1, arg2),
                    Bop::Sub => cgc.module.sub(bb, arg1, arg2),
                    Bop::Mul => cgc.module.mul(bb, arg1, arg2),
                    Bop::Div => cgc.module.sdiv(bb, arg1, arg2),
                },
                bb,
            )
        }
        Expr::If {
            ref condition,
            ref then,
            ref otherwise,
        } => {
            let llvm_type = size(cgc, expr.ty.clone().unwrap());
            let result = cgc.module.alloca(bb, llvm_type);
            let (condition_val, bb) = compile_expr(cgc, f, bb, condition);
            let then_bb = cgc.module.add_block(f);
            let otherwise_bb = cgc.module.add_block(f);
            let exit_bb = cgc.module.add_block(f);
            cgc.module
                .conditional_br(bb, condition_val, then_bb, otherwise_bb);
            let (then_value, then_bb) = compile_expr(cgc, f, then_bb, then);
            cgc.module.store(then_bb, then_value, result);
            cgc.module.br(then_bb, exit_bb);
            let (otherwise_value, otherwise_bb) = compile_expr(cgc, f, otherwise_bb, otherwise);
            cgc.module.store(otherwise_bb, otherwise_value, result);
            cgc.module.br(otherwise_bb, exit_bb);
            (cgc.module.load(exit_bb, result), exit_bb)
        }
        Expr::Print(ref e) => {
            let arg1 = cgc.module.pointer_cast(
                bb,
                cgc.int_format_string,
                LLVMType::pointer(LLVMType::Int8),
            );
            let (arg2, bb) = compile_expr(cgc, f, bb, e);
            cgc.module.call(bb, cgc.printf_fn, &[arg1, arg2]);
            (cgc.unit, bb)
        }
        Expr::Block(ref stmts, ref e) => {
            cgc.symtab.push();
            let mut bb = bb;
            for stmt in stmts.iter() {
                bb = compile_statement(cgc, f, bb, stmt);
            }
            let (val, bb) = compile_expr(cgc, f, bb, e);
            cgc.symtab.pop();
            (val, bb)
        }
        Expr::Var(ref id) => {
            let storage = cgc.symtab.lookup(id).unwrap();
            let val = cgc.module.load(bb, storage);
            (val, bb)
        }
        _ => unimplemented!(),
    }
}

// Returns an unterminated basic block
fn compile_statement(
    cgc: &mut CodeGenContext,
    f: LLVMFunction,
    bb: LLVMBasicBlock,
    stmt: &Stmt,
) -> LLVMBasicBlock {
    #[allow(unreachable_patterns)]
    match stmt {
        Stmt::Let(ref id, ref e, false) => {
            let (val, bb) = compile_expr(cgc, f, bb, e);
            if let Some(id) = id {
                let llvm_type = size(cgc, e.ty.clone().unwrap());
                let storage = cgc.module.alloca(bb, llvm_type);
                cgc.module.store(bb, val, storage);
                cgc.symtab.set(id.clone(), storage);
            }
            bb
        }
        _ => unimplemented!(),
    }
}

pub fn compile_program(prog: Program, output_file: &str) -> Result<(), Error> {
    let mut module = Module::new("main");
    let int_format_string = module.static_string("%d\n");
    let unit = module.static_bool(false);
    let bool_false = module.static_bool(false);
    let bool_true = module.static_bool(true);
    let printf_fn = module.declare_function("printf", LLVMType::Int32, &[], true);
    let symtab = SymbolTable::new();
    let mut cgc = CodeGenContext {
        int_format_string,
        unit,
        bool_false,
        bool_true,
        printf_fn,
        module,
        symtab,
    };
    let main = cgc
        .module
        .declare_function("main", LLVMType::Int32, &[], false);
    let entry_block = cgc.module.add_block_named(main, Name("entry"));
    let exit_block = cgc.module.add_block_named(main, Name("exit"));
    let zero = cgc.module.const_int32(0);
    cgc.module.ret(exit_block, zero);
    let mut last_block = entry_block;
    for stmt in prog.stmts.iter() {
        last_block = compile_statement(&mut cgc, main, last_block, stmt);
    }
    cgc.module.br(last_block, exit_block);
    cgc.module.write(output_file);
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::ast::*;
    use crate::codegen::*;
    use crate::parse::*;
    use crate::typecheck::check_program;
    use std::process;
    use tempfile::NamedTempFile;

    fn execute_program(mut prog: Program) -> String {
        check_program(&mut prog);
        let out_file = NamedTempFile::new().expect("temp file creation failed");
        let out_file_path = out_file.path().to_str().unwrap();
        compile_program(prog, out_file_path);
        let out = process::Command::new("lli")
            .arg(out_file_path)
            .output()
            .expect("process failed to execute");
        String::from_utf8_lossy(&out.stdout).to_string()
    }

    #[test]
    fn test_basic() {
        assert_eq!(execute_program(program!(print(3);)), "3\n");
        assert_eq!(
            execute_program(program!(let x = 4;
                                            { let x = 3;
                                              print(x);
                                            };
                                            print(x);)),
            "3\n4\n"
        );
    }
}
