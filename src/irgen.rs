use crate::{
    ast::{self, Ast, FunctionId},
    ir,
    resolve::types::SymbolTable,
};

pub struct Functions {}

impl Functions {
    pub fn finish_module(
        mut self,
        symbols: SymbolTable,
        ast: &Ast,
        main: Option<FunctionId>,
    ) -> ir::Module {
        todo!()
    }
}
