/*
Much of the below was copied from https://github.com/Wilfred/bfc/blob/master/src/llvm.rs
*/

#![allow(dead_code)]


use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMBuilder, LLVMModule};
use llvm_sys::target_machine::LLVMGetDefaultTargetTriple;
use std::ffi::{CStr, CString};
use std::os::raw::{c_uint, c_ulonglong};
use std::ptr::null_mut;
use std::str;


/// A struct that keeps ownership of all the strings we've passed to
/// the LLVM API until we destroy the `LLVMModule`.
const LLVM_FALSE: LLVMBool = 0;
const LLVM_TRUE: LLVMBool = 1;

pub struct Module {
    module: *mut LLVMModule,
    strings: Vec<CString>,
    empty_name: *const i8
}

impl Module {
    /// Create a new CString associated with this LLVMModule,
    /// and return a pointer that can be passed to LLVM APIs.
    /// Assumes s is pure-ASCII.
    fn new_string_ptr(&mut self, s: &str) -> *const i8 {
        self.new_mut_string_ptr(s)
    }

    // TODO: ideally our pointers wouldn't be mutable.
    fn new_mut_string_ptr(&mut self, s: &str) -> *mut i8 {
        let cstring = CString::new(s).unwrap();
        let ptr = cstring.as_ptr() as *mut _;
        self.strings.push(cstring);
        ptr
    }

    pub fn to_cstring(&self) -> CString {
        unsafe {
            // LLVM gives us a *char pointer, so wrap it in a CStr to mark it
            // as borrowed.
            let llvm_ir_ptr = LLVMPrintModuleToString(self.module);
            let llvm_ir = CStr::from_ptr(llvm_ir_ptr as *const _);

            // Make an owned copy of the string in our memory space.
            let module_string = CString::new(llvm_ir.to_bytes()).unwrap();

            // Cleanup borrowed string.
            LLVMDisposeMessage(llvm_ir_ptr);

            module_string
        }
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        // Rust requires that drop() is a safe function.
        unsafe {
            LLVMDisposeModule(self.module);
        }
    }
}

/// Wraps LLVM's builder class to provide a nicer API and ensure we
/// always dispose correctly.
struct Builder {
    builder: *mut LLVMBuilder,
}

impl Builder {
    /// Create a new Builder in LLVM's global context.
    fn new() -> Self {
        unsafe {
            Builder {
                builder: LLVMCreateBuilder(),
            }
        }
    }

    fn position_at_end(&self, bb: LLVMBasicBlockRef) {
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder, bb);
        }
    }
}

impl Drop for Builder {
    fn drop(&mut self) {
        // Rust requires that drop() is a safe function.
        unsafe {
            LLVMDisposeBuilder(self.builder);
        }
    }
}

/// Convert this integer to LLVM's representation of a constant
/// integer.
fn int8(val: i8) -> LLVMValueRef {
    unsafe {LLVMConstInt(LLVMInt8Type(), val as c_ulonglong, LLVM_FALSE)}
}
/// Convert this integer to LLVM's representation of a constant
/// integer.
// TODO: this should be a machine word size rather than hard-coding 32-bits.
pub fn int32(val: i32) -> LLVMValueRef {
    unsafe { LLVMConstInt(LLVMInt32Type(), val as c_ulonglong, LLVM_FALSE) }
}

pub fn int1_type() -> LLVMTypeRef {
    unsafe { LLVMInt1Type() }
}

pub fn int1(val: bool) -> LLVMValueRef {
    unsafe {LLVMConstInt(LLVMInt1Type(), (if val {1} else {0}) as c_ulonglong, LLVM_FALSE)}
}

pub fn int8_type() -> LLVMTypeRef {
    unsafe { LLVMInt8Type() }
}

pub fn int32_type() -> LLVMTypeRef {
    unsafe { LLVMInt32Type() }
}

pub fn int8_ptr_type() -> LLVMTypeRef {
    unsafe { LLVMPointerType(LLVMInt8Type(), 0) }
}

fn add_function(
    module: &mut Module,
    fn_name: &str,
    args: &mut [LLVMTypeRef],
    ret_type: LLVMTypeRef
) {
    unsafe {
        let fn_type = LLVMFunctionType(ret_type, args.as_mut_ptr(), args.len() as u32, LLVM_FALSE);
        LLVMAddFunction(module.module, module.new_string_ptr(fn_name), fn_type);
    }
}

fn add_varargs_function(
    module: &mut Module,
    fn_name: &str,
    args: &mut [LLVMTypeRef],
    ret_type: LLVMTypeRef
) -> LLVMValueRef {
    unsafe {
        let fn_type = LLVMFunctionType(ret_type, args.as_mut_ptr(), args.len() as u32, LLVM_TRUE);
        LLVMAddFunction(module.module, module.new_string_ptr(fn_name), fn_type)
    }
}

pub fn add_static_string(module: &mut Module, value: &str, name: &str) -> LLVMValueRef {
    unsafe {
        let llvm_str = LLVMConstString(
            module.new_string_ptr(value),
            value.len() as c_uint,
            LLVM_FALSE
        );
        let llvm_array = LLVMAddGlobal(
            module.module,
            LLVMArrayType(int8_type(),
                          (value.len() + 1) as c_uint),
            module.new_string_ptr(""));
        LLVMSetInitializer(llvm_array, llvm_str);
        LLVMSetGlobalConstant(llvm_array, LLVM_TRUE);
        llvm_array
    }
}

fn add_stdlib_declarations(module: &mut Module) {
    let void;
    unsafe {
        void = LLVMVoidType();
    }

    add_varargs_function(
        module,
        "printf",
        &mut [int8_ptr_type()],
        void
    );
}


pub fn add_main_fn(module: &mut Module) -> LLVMValueRef {
    let mut main_args = vec![];
    unsafe {
        let main_type = LLVMFunctionType(int32_type(), main_args.as_mut_ptr(), 0, LLVM_FALSE);
        // TODO: use add_function() here instead.
        LLVMAddFunction(module.module, module.new_string_ptr("main"), main_type)
    }
}


fn get_default_target_triple() -> CString {
    let target_triple;
    unsafe {
        let target_triple_ptr = LLVMGetDefaultTargetTriple();
        target_triple = CStr::from_ptr(target_triple_ptr as *const _).to_owned();
        LLVMDisposeMessage(target_triple_ptr);
    }

    target_triple
}

pub fn create_module(module_name: &str) -> Module {
    let c_module_name = CString::new(module_name).unwrap();
    let module_name_char_ptr = c_module_name.to_bytes_with_nul().as_ptr() as *const _;

    let llvm_module;
    unsafe {
        llvm_module = LLVMModuleCreateWithName(module_name_char_ptr);
    }
    let empty_name = CString::new("").unwrap();
    let empty_name_ptr = empty_name.to_bytes_with_nul().as_ptr() as *const _;
    let mut module = Module {
        module: llvm_module,
        strings: vec![c_module_name, empty_name],
        empty_name: empty_name_ptr
    };

    let target_triple_cstring = get_default_target_triple();

    // This is necessary for maximum LLVM performance, see
    // http://llvm.org/docs/Frontend/PerformanceTips.html
    unsafe {
        LLVMSetTarget(llvm_module, target_triple_cstring.as_ptr() as *const _);
    }
    // TODO: add a function to the LLVM C API that gives us the
    // data layout from the target machine.

    add_stdlib_declarations(&mut module);
    module
}


pub fn write_module(module: &mut Module, filename: &str) {
    unsafe {
        LLVMPrintModuleToFile(module.module, module.new_string_ptr(filename), null_mut());
    }
}


pub fn add_basic_block(module: &mut Module,
                          f: LLVMValueRef,
                          name: &str)
                       -> LLVMBasicBlockRef {
    unsafe {
        LLVMAppendBasicBlock(f, module.new_string_ptr(name))
    }
}

pub fn add_function_call(
    module: &mut Module,
    bb: LLVMBasicBlockRef,
    fn_name: &str,
    args: &mut [LLVMValueRef],
    name: &str,
) -> LLVMValueRef {
    let builder = Builder::new();
    builder.position_at_end(bb);
    unsafe {
        let function = LLVMGetNamedFunction(module.module, module.new_string_ptr(fn_name));

        LLVMBuildCall(
            builder.builder,
            function,
            args.as_mut_ptr(),
            args.len() as c_uint,
            module.new_string_ptr(name),
        )
    }
}

pub fn add_br(_: &mut Module, bb: LLVMBasicBlockRef, next: LLVMBasicBlockRef) {
    let builder = Builder::new();
    builder.position_at_end(bb);
    unsafe {
        LLVMBuildBr(builder.builder, next);
    }
}

pub fn add_conditional_br(_: &mut Module,
                          bb: LLVMBasicBlockRef,
                          condition: LLVMValueRef,
                          then: LLVMBasicBlockRef,
                          otherwise: LLVMBasicBlockRef) {
    let builder = Builder::new();
    builder.position_at_end(bb);
    unsafe {
        LLVMBuildCondBr(builder.builder, condition, then, otherwise);
    }
}


pub fn add_pointer_cast(module: &mut Module,
                           bb: LLVMBasicBlockRef,
                           pointer: LLVMValueRef,
                           ty: LLVMTypeRef,
                           name: &str) -> LLVMValueRef {
    let builder = Builder::new();
    builder.position_at_end(bb);
    unsafe {
        LLVMBuildPointerCast(builder.builder,
                             pointer,
                             ty,
                             module.new_string_ptr(name))
    }
}

pub fn add_ret(_module: &mut Module,
               bb: LLVMBasicBlockRef,
               value: LLVMValueRef) {
    let builder = Builder::new();
    builder.position_at_end(bb);
    unsafe {
        LLVMBuildRet(builder.builder, value);
    }
}

pub fn add_add(module: &mut Module, bb: LLVMBasicBlockRef,
               value1: LLVMValueRef, value2: LLVMValueRef)
                -> LLVMValueRef {
    let builder = Builder::new();
    builder.position_at_end(bb);
    unsafe {
        LLVMBuildAdd(builder.builder, value1, value2, module.empty_name)
    }
}


pub fn add_sub(module: &mut Module, bb: LLVMBasicBlockRef,
               value1: LLVMValueRef, value2: LLVMValueRef)
                -> LLVMValueRef {
    let builder = Builder::new();
    builder.position_at_end(bb);
    unsafe {
        LLVMBuildSub(builder.builder, value1, value2, module.empty_name)
    }
}


pub fn add_mul(module: &mut Module, bb: LLVMBasicBlockRef,
               value1: LLVMValueRef, value2: LLVMValueRef)
                -> LLVMValueRef {
    let builder = Builder::new();
    builder.position_at_end(bb);
    unsafe {
        LLVMBuildMul(builder.builder, value1, value2, module.empty_name)
    }
}


pub fn add_sdiv(module: &mut Module, bb: LLVMBasicBlockRef,
                value1: LLVMValueRef, value2: LLVMValueRef)
                -> LLVMValueRef {
    let builder = Builder::new();
    builder.position_at_end(bb);
    unsafe {
        LLVMBuildSDiv(builder.builder, value1, value2, module.empty_name)
    }
}

pub fn add_alloca(module: &mut Module, bb: LLVMBasicBlockRef,
                  ty: LLVMTypeRef) -> LLVMValueRef {
    let builder = Builder::new();
    builder.position_at_end(bb);
    unsafe {
        LLVMBuildAlloca(builder.builder, ty, module.empty_name)
    }
}

pub fn add_load(module: &mut Module, bb: LLVMBasicBlockRef,
                ptr: LLVMValueRef) -> LLVMValueRef {
    let builder = Builder::new();
    builder.position_at_end(bb);
    unsafe {
        LLVMBuildLoad(builder.builder, ptr, module.empty_name)
    }
}

pub fn add_store(_module: &mut Module, bb: LLVMBasicBlockRef,
                 value: LLVMValueRef,
                 ptr: LLVMValueRef) {
    let builder = Builder::new();
    builder.position_at_end(bb);
    unsafe {
        LLVMBuildStore(builder.builder, value, ptr);
    }
}
