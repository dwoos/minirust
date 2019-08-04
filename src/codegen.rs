use crate::ast::*;
use crate::context::Context;
use failure::Error;

use hlllvm::{LLVMBasicBlock, LLVMFunction, LLVMIBinop, LLVMType, LLVMValue, Module};

type SymbolTable = Context<LLVMValue>;
type FunctionTable = Context<LLVMFunction>;

struct CodeGenContext {
    int_format_string: LLVMValue,
    unit: LLVMValue,
    bool_false: LLVMValue,
    bool_true: LLVMValue,
    printf_fn: LLVMFunction,
    module: Module,
    symtab: SymbolTable,
    functions: FunctionTable,
}

fn size(_cgc: &CodeGenContext, ty: RcType) -> LLVMType {
    match *ty {
        Type::Int32 => LLVMType::Int32,
        Type::Bool => LLVMType::Int1,
        Type::Unit => LLVMType::Int1,
        _ => unimplemented!(),
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
            let llvm_bop = match bop {
                Bop::Add => LLVMIBinop::Add,
                Bop::Sub => LLVMIBinop::Sub,
                Bop::Mul => LLVMIBinop::Mul,
                Bop::Div => LLVMIBinop::SDiv,
            };
            (cgc.module.ibinop(bb, llvm_bop, arg1, arg2), bb)
        }
        Expr::And(ref e1, ref e2) => {
            let (arg1, bb) = compile_expr(cgc, f, bb, e1);
            let (arg2, bb) = compile_expr(cgc, f, bb, e2);
            let llvm_bop = LLVMIBinop::And;
            (cgc.module.ibinop(bb, llvm_bop, arg1, arg2), bb)
        }
        Expr::Or(ref e1, ref e2) => {
            let (arg1, bb) = compile_expr(cgc, f, bb, e1);
            let (arg2, bb) = compile_expr(cgc, f, bb, e2);
            let llvm_bop = LLVMIBinop::Or;
            (cgc.module.ibinop(bb, llvm_bop, arg1, arg2), bb)
        }
        Expr::Not(ref e) => {
            let (arg, bb) = compile_expr(cgc, f, bb, e);
            (cgc.module.not(bb, arg), bb)
        }
        Expr::Cmp {
            ref cmp,
            ref e1,
            ref e2,
        } => {
            let (arg1, bb) = compile_expr(cgc, f, bb, e1);
            let (arg2, bb) = compile_expr(cgc, f, bb, e2);
            let llvm_bop = match cmp {
                Cmp::Eq => LLVMIBinop::Eq,
                Cmp::Neq => LLVMIBinop::Neq,
                Cmp::Lt => LLVMIBinop::SLt,
                Cmp::Le => LLVMIBinop::SLe,
                Cmp::Gt => LLVMIBinop::SGt,
                Cmp::Ge => LLVMIBinop::SGe,
            };
            (cgc.module.ibinop(bb, llvm_bop, arg1, arg2), bb)
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
        Expr::While {
            ref condition,
            ref body,
        } => {
            let llvm_type = size(cgc, expr.ty.clone().unwrap());
            let result = cgc.module.alloca(bb, llvm_type);
            let test_bb = cgc.module.add_block(f);
            let body_bb = cgc.module.add_block(f);
            let exit_bb = cgc.module.add_block(f);
            cgc.module.br(bb, test_bb);
            let (condition_val, test_bb) = compile_expr(cgc, f, test_bb, condition);
            cgc.module
                .conditional_br(test_bb, condition_val, body_bb, exit_bb);
            let (body_value, body_bb) = compile_expr(cgc, f, body_bb, body);
            cgc.module.store(body_bb, body_value, result);
            cgc.module.br(body_bb, test_bb);
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
        Expr::FunCall(ref callee, ref args) => {
            // right now, f has to be an identifier--there's no other way to get
            // something of function type. eventually we will have closures and
            // will need to make functions values
            match *callee.expr {
                Expr::Var(ref id) => {
                    let llvm_function = cgc.functions.lookup(id).unwrap();
                    let mut bb = bb;
                    let mut llvm_args = vec![];
                    for arg in args.iter() {
                        let (val, newbb) = compile_expr(cgc, f, bb, arg);
                        bb = newbb;
                        llvm_args.push(val);
                    }
                    let res = cgc.module.call(bb, llvm_function, &llvm_args);
                    (res, bb)
                }
                _ => panic!("Bad function"),
            }
        }
        Expr::Var(ref id) => {
            let storage = cgc.symtab.lookup(id).unwrap();
            let val = cgc.module.load(bb, storage);
            (val, bb)
        }
        Expr::Assign(ref lhs, ref rhs) => {
            match *lhs.expr {
                Expr::Var(ref id) => {
                    let storage = cgc.symtab.lookup(id).unwrap();
                    let (val, bb) = compile_expr(cgc, f, bb, rhs);
                    cgc.module.store(bb, val, storage);
                    // assignments return the unit value
                    (cgc.unit, bb)
                }
                _ => panic!("Bad assignment"),
            }
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
        // we can ignore mutability here--it only matters for the
        // typechecker. If a program typechecks, it isn't mutating anything it
        // isn't supposed to.
        Stmt::Let(ref id, ref e, _) => {
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

fn compile_item(cgc: &mut CodeGenContext, item: &Item) -> Result<(), Error> {
    #[allow(unreachable_patterns)]
    match item {
        Item::Function(ref name, ref args, _, ref body) => {
            let f = cgc.functions.lookup(name).unwrap();
            cgc.symtab.push();
            let bb = cgc.module.add_block(f);
            for (i, (name, ty)) in args.iter().enumerate() {
                // llvm parameters are values, not locations. note that if we
                // try to use the value directly it segfaults--this is a bug in
                // hlllvm. Regardless, we need to allocate a variable to store
                // the incoming value.
                let arg_llvm_type = size(cgc, ty.clone());
                let param = cgc.module.alloca(bb, arg_llvm_type);
                let param_val = cgc.module.param(f, i);
                cgc.module.store(bb, param_val, param);
                cgc.symtab.set(name.clone(), param);
            }
            let (ret, bb) = compile_expr(cgc, f, bb, body);
            cgc.module.ret(bb, ret);
            Ok(())
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
    let functions = FunctionTable::new();
    let mut cgc = CodeGenContext {
        int_format_string,
        unit,
        bool_false,
        bool_true,
        printf_fn,
        module,
        symtab,
        functions,
    };
    // add all function declarations to global context
    for item in prog.items.iter() {
        #[allow(unreachable_patterns)]
        match item {
            Item::Function(ref name, ref args, ref ret, _) => {
                let args: Vec<_> = args.iter().map(|(_, ty)| size(&cgc, ty.clone())).collect();
                let Identifier::Identifier(name_s) = name;
                let ret_size = size(&cgc, ret.clone());
                let llvm_function = cgc.module.declare_function(name_s, ret_size, &args, false);
                cgc.functions.set(name.clone(), llvm_function);
            }
            _ => unimplemented!(),
        }
    }

    for item in prog.items.iter() {
        compile_item(&mut cgc, item)?;
    }
    cgc.module.write(output_file);
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::codegen::*;
    use crate::parse::*;
    use crate::typecheck::check_program;
    use std::process;
    use tempfile::NamedTempFile;

    fn execute_program(mut prog: Program) -> String {
        check_program(&mut prog).expect("typechecking failed");
        let out_file = NamedTempFile::new().expect("temp file creation failed");
        let out_file_path = out_file.path().to_str().unwrap();
        compile_program(prog, out_file_path).expect("compilation failed");
        let out = process::Command::new("lli")
            .arg(out_file_path)
            .output()
            .expect("process failed to execute");
        String::from_utf8_lossy(&out.stdout).to_string()
    }

    #[test]
    fn test_basic() {
        assert_eq!(
            execute_program(program!(
                fn main() {
                    print(3);
                }
            )),
            "3\n"
        );
        assert_eq!(
            execute_program(program!(
                fn main() {
                    let x = 4;
                    {
                        let x = 3;
                        print(x);
                    };
                    print(x);
                }
            )),
            "3\n4\n"
        );
    }

    #[test]
    fn test_assignment() {
        assert_eq!(
            execute_program(program!(
                fn main() {
                    let mut x = 3;
                    print(x);
                    x = 4;
                    print(x);
                }
            )),
            "3\n4\n"
        );

        assert_eq!(
            execute_program(program!(
                fn main() {
                    let mut x = 3;
                    print(x);
                    x = x + 1;
                    print(x);
                    x = x + 2;
                    print(x);
                }
            )),
            "3\n4\n6\n"
        );
    }

    #[test]
    fn test_while_cmp() {
        assert_eq!(
            execute_program(program!(
                fn main() {
                    let mut x = 3;
                    while x > 0 {
                        print(x);
                        x = x - 1;
                    }
                }
            )),
            "3\n2\n1\n"
        );
    }

    #[test]
    fn test_funcall() {
        assert_eq!(
            execute_program(program!(
                fn main() {
                    let x = 3;
                    let y = 4;
                    print(add(x, y));
                    print(x);
                    print(y);
                }

                fn add(x: i32, y: i32) -> i32 {
                    x + y
                }
            )),
            "7\n3\n4\n"
        );
    }

    #[test]
    #[rustfmt::skip]
    fn test_recursion() {
        assert_eq!(
            execute_program(program!(
                fn main() {
                    let mut x = 0;
                    while x < 6 {
                        print(fac(x));
                        x = x + 1;
                    };
                }

                fn fac(x: i32) -> i32 {
                    if x <= 1 {
                        1
                    } else {
                        x * fac(x - 1)
                    }
                }
            )),
            "1\n1\n2\n6\n24\n120\n"
        );
    }
}
