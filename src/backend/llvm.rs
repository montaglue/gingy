use llvm_sys::{prelude::LLVMModuleRef, LLVMContext};

use crate::{config::BackendStats, ir};

pub struct Module(LLVMModuleRef);

pub unsafe fn module(
    context: *mut LLVMContext,
    ir: &ir::Module,
    show_backend_ir: bool,
) -> (Module, BackendStats) {
    todo!()
}

pub unsafe fn run_jit(module: Module) -> i32 {
    todo!()
}

pub mod output {
    use std::ffi::CStr;

    pub unsafe fn emit_bitcoe(target: Option<&CStr>, module: super::Module, out: &str) {
        todo!()
    }
}
