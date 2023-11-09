use crate::{
    ast::ModuleId,
    span::Span,
    token::{ExpectedTokens, TokenType},
};

#[derive(Debug)]
pub enum Error {
    Text(String),
    Errors(Vec<CompilerError>),
    UnexpectedToken {
        expected: ExpectedTokens,
        found: TokenType,
    },
    InvalidGlobalVarPattern,
    InvalidTopLevelBlockItem,
    DuplicateDefinition,
    UnexpectedEndOfFile,
    CantUseRootPath,
}

impl Error {
    pub fn at_span(self, span: Span) -> CompilerError {
        CompilerError { error: self, span }
    }
    pub fn at(self, start: u32, end: u32, module: ModuleId) -> CompilerError {
        CompilerError {
            error: self,
            span: Span { start, end, module },
        }
    }
}

#[derive(Debug)]
pub struct CompilerError {
    pub error: Error,
    pub span: Span,
}

pub struct Warning {
    pub message: String,
}

pub struct Errors {
    pub errors: Vec<CompilerError>,
    pub warnings: Vec<Warning>,
}

impl Errors {
    pub fn emit(&mut self, err: Error, start: u32, end: u32, module: ModuleId) {
        self.emit_span(err, Span { start, end, module });
    }

    pub fn emit_span(&mut self, err: Error, span: Span) {
        self.emit_err(CompilerError { error: err, span });
    }

    pub fn emit_err(&mut self, err: CompilerError) {
        self.errors.push(err);
    }
}

impl From<Errors> for Error {
    fn from(error: Errors) -> Self {
        Error::Errors(error.errors)
    }
}

pub type CompilerResult<T> = Result<T, Error>;
