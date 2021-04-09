use crate::ast::*;
use crate::context::Context;
use failure::Error;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context as LLVMContext;
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};

type SymbolTable<'a> = Context<PointerValue<'a>>;
type FunctionTable<'a> = Context<FunctionValue<'a>>;

struct CodeGenContext<'a, 'ctx> {
    int_format_string: BasicValueEnum<'ctx>,
    unit: BasicValueEnum<'ctx>,
    bool_false: BasicValueEnum<'ctx>,
    bool_true: BasicValueEnum<'ctx>,
    printf_fn: FunctionValue<'ctx>,
    module: &'a Module<'ctx>,
    symtab: SymbolTable<'ctx>,
    functions: FunctionTable<'ctx>,
    context: &'ctx LLVMContext,
    builder: &'a Builder<'ctx>,
}

fn size<'a, 'ctx>(cgc: &'a CodeGenContext<'a, 'ctx>, ty: RcType) -> BasicTypeEnum<'ctx> {
    match *ty {
        Type::Int32 => cgc.context.i32_type().into(),
        Type::Bool => cgc.context.bool_type().into(),
        Type::Unit => cgc.context.bool_type().into(),
        _ => unimplemented!(),
    }
}

// Returns a value and an unterminated basic block
fn compile_expr<'a, 'ctx>(
    cgc: &mut CodeGenContext<'a, 'ctx>,
    f: FunctionValue<'ctx>,
    bb: BasicBlock<'ctx>,
    expr: &TypedExpr,
) -> (BasicValueEnum<'ctx>, BasicBlock<'ctx>) {
    #[allow(unreachable_patterns)]
    match *expr.expr {
        Expr::Literal(ref l) => (
            match l {
                Literal::Num(x) => cgc.context.i32_type().const_int(*x as u64, true).into(),
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
            cgc.builder.position_at_end(bb);
            let ivalue = match bop {
                Bop::Add => cgc
                    .builder
                    .build_int_add(arg1.into_int_value(), arg2.into_int_value(), "")
                    .into(),
                Bop::Sub => cgc
                    .builder
                    .build_int_sub(arg1.into_int_value(), arg2.into_int_value(), "")
                    .into(),
                Bop::Mul => cgc
                    .builder
                    .build_int_mul(arg1.into_int_value(), arg2.into_int_value(), "")
                    .into(),
                Bop::Div => cgc
                    .builder
                    .build_int_signed_div(arg1.into_int_value(), arg2.into_int_value(), "")
                    .into(),
            };
            (ivalue, bb)
        }
        Expr::And(ref e1, ref e2) => {
            let (arg1, bb) = compile_expr(cgc, f, bb, e1);
            let (arg2, bb) = compile_expr(cgc, f, bb, e2);
            cgc.builder.position_at_end(bb);
            (
                cgc.builder
                    .build_and(arg1.into_int_value(), arg2.into_int_value(), "")
                    .into(),
                bb,
            )
        }
        Expr::Or(ref e1, ref e2) => {
            let (arg1, bb) = compile_expr(cgc, f, bb, e1);
            let (arg2, bb) = compile_expr(cgc, f, bb, e2);
            cgc.builder.position_at_end(bb);
            (
                cgc.builder
                    .build_or(arg1.into_int_value(), arg2.into_int_value(), "")
                    .into(),
                bb,
            )
        }
        Expr::Not(ref e) => {
            let (arg, bb) = compile_expr(cgc, f, bb, e);
            cgc.builder.position_at_end(bb);
            (cgc.builder.build_not(arg.into_int_value(), "").into(), bb)
        }
        Expr::Cmp {
            ref cmp,
            ref e1,
            ref e2,
        } => {
            let (arg1, bb) = compile_expr(cgc, f, bb, e1);
            let (arg2, bb) = compile_expr(cgc, f, bb, e2);
            let pred = match cmp {
                Cmp::Eq => inkwell::IntPredicate::EQ,
                Cmp::Neq => inkwell::IntPredicate::NE,
                Cmp::Lt => inkwell::IntPredicate::SLT,
                Cmp::Le => inkwell::IntPredicate::SLE,
                Cmp::Gt => inkwell::IntPredicate::SGT,
                Cmp::Ge => inkwell::IntPredicate::SGE,
            };
            cgc.builder.position_at_end(bb);
            (
                cgc.builder
                    .build_int_compare(pred, arg1.into_int_value(), arg2.into_int_value(), "")
                    .into(),
                bb,
            )
        }
        Expr::If {
            ref condition,
            ref then,
            ref otherwise,
        } => {
            let llvm_type = size(cgc, expr.ty.clone().unwrap());
            cgc.builder.position_at_end(bb);
            let result = cgc.builder.build_alloca(llvm_type, "");
            let (condition_val, bb) = compile_expr(cgc, f, bb, condition);
            let then_bb = cgc.context.append_basic_block(f, "then");
            let otherwise_bb = cgc.context.append_basic_block(f, "else");
            let exit_bb = cgc.context.append_basic_block(f, "exit");
            cgc.builder.position_at_end(bb);
            cgc.builder.build_conditional_branch(
                condition_val.into_int_value(),
                then_bb,
                otherwise_bb,
            );
            let (then_value, then_bb) = compile_expr(cgc, f, then_bb, then);
            cgc.builder.position_at_end(then_bb);
            cgc.builder.build_store(result, then_value);
            cgc.builder.build_unconditional_branch(exit_bb);
            let (otherwise_value, otherwise_bb) = compile_expr(cgc, f, otherwise_bb, otherwise);
            cgc.builder.position_at_end(otherwise_bb);
            cgc.builder.build_store(result, otherwise_value);
            cgc.builder.build_unconditional_branch(exit_bb);
            cgc.builder.position_at_end(exit_bb);
            (cgc.builder.build_load(result, ""), exit_bb)
        }
        Expr::While {
            ref condition,
            ref body,
        } => {
            let llvm_type = size(cgc, expr.ty.clone().unwrap());
            cgc.builder.position_at_end(bb);
            let result = cgc.builder.build_alloca(llvm_type, "");
            let test_bb = cgc.context.append_basic_block(f, "test");
            let body_bb = cgc.context.append_basic_block(f, "body");
            let exit_bb = cgc.context.append_basic_block(f, "exit");
            cgc.builder.build_unconditional_branch(test_bb);
            let (condition_val, test_bb) = compile_expr(cgc, f, test_bb, condition);
            cgc.builder.position_at_end(test_bb);
            cgc.builder
                .build_conditional_branch(condition_val.into_int_value(), body_bb, exit_bb);
            let (body_value, body_bb) = compile_expr(cgc, f, body_bb, body);
            cgc.builder.position_at_end(body_bb);
            cgc.builder.build_store(result, body_value);
            cgc.builder.build_unconditional_branch(test_bb);
            cgc.builder.position_at_end(exit_bb);
            (cgc.builder.build_load(result, ""), exit_bb)
        }
        Expr::Print(ref e) => {
            cgc.builder.position_at_end(bb);
            let arg1 = unsafe {
                cgc.builder
                    .build_in_bounds_gep(
                        cgc.int_format_string.into_pointer_value(),
                        &[
                            cgc.context.i32_type().const_int(0, false),
                            cgc.context.i32_type().const_int(0, false),
                        ],
                        "",
                    )
                    .into()
            };
            let (arg2, bb) = compile_expr(cgc, f, bb, e);
            cgc.builder.position_at_end(bb);
            cgc.builder.build_call(cgc.printf_fn, &[arg1, arg2], "");
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
                    cgc.builder.position_at_end(bb);
                    let res = cgc
                        .builder
                        .build_call(llvm_function, &llvm_args, "")
                        .try_as_basic_value()
                        .left()
                        .unwrap();
                    (res, bb)
                }
                _ => panic!("Bad function"),
            }
        }
        Expr::Var(ref id) => {
            let storage = cgc.symtab.lookup(id).unwrap();
            cgc.builder.position_at_end(bb);
            let val = cgc.builder.build_load(storage, "");
            (val, bb)
        }
        Expr::Assign(ref lhs, ref rhs) => {
            match *lhs.expr {
                Expr::Var(ref id) => {
                    let storage = cgc.symtab.lookup(id).unwrap();
                    let (val, bb) = compile_expr(cgc, f, bb, rhs);
                    cgc.builder.position_at_end(bb);
                    cgc.builder.build_store(storage, val);
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
fn compile_statement<'a, 'ctx>(
    cgc: &mut CodeGenContext<'a, 'ctx>,
    f: FunctionValue<'ctx>,
    bb: BasicBlock<'ctx>,
    stmt: &Stmt,
) -> BasicBlock<'ctx> {
    #[allow(unreachable_patterns)]
    match stmt {
        // we can ignore mutability here--it only matters for the
        // typechecker. If a program typechecks, it isn't mutating anything it
        // isn't supposed to.
        Stmt::Let(ref id, ref e, _) => {
            let (val, bb) = compile_expr(cgc, f, bb, e);
            if let Some(id) = id {
                let llvm_type = size(cgc, e.ty.clone().unwrap());
                cgc.builder.position_at_end(bb);
                let storage = cgc.builder.build_alloca(llvm_type, "");
                cgc.builder.build_store(storage, val);
                cgc.symtab.set(id.clone(), storage);
            }
            bb
        }
        _ => unimplemented!(),
    }
}

fn compile_item<'a, 'ctx>(cgc: &mut CodeGenContext<'a, 'ctx>, item: &Item) -> Result<(), Error> {
    #[allow(unreachable_patterns)]
    match item {
        Item::Function(ref name, ref args, _, ref body) => {
            let f = cgc.functions.lookup(name).unwrap();
            cgc.symtab.push();
            let bb = cgc.context.append_basic_block(f, "entry");
            cgc.builder.position_at_end(bb);
            for (i, (name, ty)) in args.iter().enumerate() {
                /*
                llvm parameters are values, not locations. note that if we
                try to use the value directly it segfaults--this is a bug in
                hlllvm. Regardless, we need to allocate a variable to store
                the incoming value.
                */
                let arg_llvm_type = size(cgc, ty.clone());
                let param = cgc.builder.build_alloca(arg_llvm_type, "param");
                let param_val = f.get_nth_param(i as u32).unwrap();
                cgc.builder.build_store(param, param_val);
                cgc.symtab.set(name.clone(), param);
            }
            let (ret, bb) = compile_expr(cgc, f, bb, body);
            cgc.builder.position_at_end(bb);
            cgc.builder.build_return(Some(&ret));
            Ok(())
        }
        _ => unimplemented!(),
    }
}

pub fn compile_program(prog: Program, output_file: &std::path::Path) -> Result<(), Error> {
    let context = LLVMContext::create();
    let module = context.create_module("main");
    let mut charcodes: Vec<_> = "%d\n".chars().map(|c| c as u8).collect();
    charcodes.push(0);

    let array_ty = context.i8_type().array_type(charcodes.len() as u32);
    let array_vals: Vec<_> = charcodes
        .iter()
        .map(|v| context.i8_type().const_int((*v).into(), false))
        .collect();

    let int_format_string = module.add_global(array_ty, None, "int_format_string");
    int_format_string.set_initializer(&context.i8_type().const_array(array_vals.as_slice()));

    let unit = context.bool_type().const_int(1, false).into();
    let bool_false = context.bool_type().const_int(0, false).into();
    let bool_true = context.bool_type().const_int(1, false).into();
    let printf_fn = module.add_function("printf", context.i32_type().fn_type(&[], true), None);
    let symtab = SymbolTable::new();
    let functions = FunctionTable::new();
    let builder = context.create_builder();
    let mut cgc = CodeGenContext {
        int_format_string: int_format_string.as_pointer_value().into(),
        unit,
        bool_false,
        bool_true,
        printf_fn,
        module: &module,
        symtab,
        functions,
        context: &context,
        builder: &builder,
    };
    // add all function declarations to global context
    for item in prog.items.iter() {
        #[allow(unreachable_patterns)]
        match item {
            Item::Function(ref name, ref args, ref ret, _) => {
                let args: Vec<_> = args.iter().map(|(_, ty)| size(&cgc, ty.clone())).collect();
                let Identifier::Identifier(name_s) = name;
                let ret_size = size(&cgc, ret.clone()).into_int_type();
                let llvm_function =
                    cgc.module
                        .add_function(name_s, ret_size.fn_type(&args, false), None);
                cgc.functions.set(name.clone(), llvm_function);
            }
            _ => unimplemented!(),
        }
    }

    for item in prog.items.iter() {
        compile_item(&mut cgc, item)?;
    }
    cgc.module.print_to_file(output_file);
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
        let out_file_path = out_file.path();
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
