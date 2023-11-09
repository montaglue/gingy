use std::{collections::HashMap, path::Path};

use crate::{
    ast::FunctionId,
    config::{Debug, Stats},
    dependency::Dependency,
    error::{CompilerResult, Errors},
    irgen::Functions,
    resolve::types::SymbolTable,
};

pub struct CompiledProject {
    pub symbols: SymbolTable,
    pub ir: Functions,
    pub main: Option<FunctionId>,
}

pub fn project(
    path: &Path,
    debug: Debug,
    dependencies: HashMap<String, Dependency>,
    require_main_func: bool,
    stats: &mut Stats,
    is_std: bool,
) -> (CompilerResult<CompiledProject>, crate::ast::Ast, Errors) {
    todo!()
}
