use crate::{
    ast::ModuleId,
    error::{CompilerError, Error},
    span::Span,
    token::{Token, TokenType},
};

pub struct TokenReader {
    tokens: Vec<Token>,
    index: usize,
    len: usize,
    pub module: ModuleId,
}

impl TokenReader {
    pub fn new(tokens: Vec<Token>, module: ModuleId) -> Self {
        let len = tokens.len();
        Self {
            tokens,
            index: 0,
            len,
            module,
        }
    }

    pub fn current(&self) -> Result<&Token, CompilerError> {
        if self.index < self.len {
            Ok(&self.tokens[self.index])
        } else {
            let end = self.last_src_pos();
            Err(CompilerError {
                error: Error::Text("Unexpected end of file".to_string()),
                span: Span {
                    start: end,
                    end,
                    module: self.module,
                },
            })
        }
    }

    pub fn previouse(&self) -> Option<&Token> {
        if self.index > 0 {
            Some(&self.tokens[self.index - 1])
        } else {
            None
        }
    }

    pub fn last_src_pos(&self) -> u32 {
        self.tokens.last().map_or(0, |tok| tok.span.end)
    }

    pub fn current_end_pos(&self) -> u32 {
        self.current()
            .map(|tok| tok.span.end)
            .ok()
            .or_else(|| self.tokens.last().map(|tok| tok.span.end))
            .unwrap_or(0)
    }

    pub fn step(&mut self) -> Result<Token, CompilerError> {
        self.index += 1;
        if self.index <= self.len {
            Ok(self.tokens[self.index - 1])
        } else {
            let end = self.last_src_pos();
            Err(CompilerError {
                error: Error::Text("Unexpected end of file".to_string()),
                span: Span {
                    start: end,
                    end,
                    module: self.module,
                },
            })
        }
    }

    pub fn step_back(&mut self) {
        self.index -= 1;
    }

    pub fn step_assert(&mut self, ty: impl Into<TokenType>) -> Token {
        let tok = self.step().unwrap();

        debug_assert_eq!(tok.ty, ty.into());
        tok
    }

    pub fn step_expect<const N: usize, T: Into<[TokenType; N]>>(
        &mut self,
        expected: T,
    ) -> Result<Token, CompilerError> {
        let expected: [TokenType; N] = expected.into();
        let module = self.module;
        let tok = self.step()?;

        if !expected.iter().any(|expected_tok| *expected_tok == tok.ty) {
            return Err(CompilerError {
                error: Error::Text(format!(
                    "Unexpected token, expected: {:?}, found: {:?}",
                    expected, tok.ty,
                )),
                span: tok.span.in_mod(module),
            });
        }

        Ok(tok)
    }

    pub fn peek(&self) -> Option<Token> {
        if self.index < self.tokens.len() {
            Some(self.tokens[self.index])
        } else {
            None
        }
    }

    pub fn step_if<const N: usize, T: Into<[TokenType; N]>>(
        &mut self,
        expected: T,
    ) -> Option<Token> {
        if let Some(next) = self.peek() {
            next.is(expected).then(|| self.step().unwrap())
        } else {
            None
        }
    }

    pub fn more_on_line(&self) -> bool {
        self.peek().map_or(false, |tok| !tok.new_line)
    }

    pub fn is_at_end(&self) -> bool {
        self.index > self.len
    }
}

pub enum Delimit {
    Yes,
    No,
    Optional,
    OptionalIfNewLine,
}
