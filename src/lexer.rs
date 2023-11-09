use crate::{
    ast::ModuleId,
    error::{Error, Errors},
    span::TSpan,
    token::{Bracket, Token, TokenType},
};

pub fn parse(src: &str, errors: &mut Errors, module: ModuleId) -> Vec<Token> {
    let chars = src
        .char_indices()
        .map(|(i, c)| (i as u32, c))
        .collect::<Vec<_>>();
    Lexer {
        src,
        chars,
        index: 0,
        tokens: Vec::new(),
        module,
        newline: true,
    }
    .parse(errors)
}

struct Lexer<'a> {
    src: &'a str,
    chars: Vec<(u32, char)>,
    index: usize,
    tokens: Vec<Token>,
    module: ModuleId,
    newline: bool,
}

fn is_ident_char(c: Option<char>) -> bool {
    matches!(c, Some('A'..='Z' | 'a'..='z' | '0'..='9' | '_'))
}

impl<'a> Lexer<'a> {
    pub fn parse(mut self, errors: &mut Errors) -> Vec<Token> {
        while self.index < self.chars.len() {
            self.newline |= self.skip();
            if self.is_at_end() {
                break;
            }

            if let Some(token) = self.parse_token(errors) {
                self.tokens.push(token);
            }
        }
        self.tokens
    }

    pub fn is_at_end(&self) -> bool {
        self.index >= self.chars.len()
    }

    pub fn skip(&mut self) -> bool {
        let mut new_line = false;
        if self.is_at_end() {
            return new_line;
        }

        while let ' ' | '\r' | '\n' = self.current() {
            if self.current() == '\n' {
                new_line = true;
            }

            if self.step().is_none() {
                return new_line;
            }

            if self.is_at_end() {
                return new_line;
            }
        }
        new_line
    }

    pub fn pos(&self) -> u32 {
        if self.chars.is_empty() {
            return 0;
        }
        if let Some((x, _)) = self.chars.get(self.index) {
            return *x;
        }
        let (pos, c) = self.chars.last().unwrap();
        *pos + c.len_utf8() as u32
    }

    pub fn current(&self) -> char {
        self.chars[self.index].1
    }

    pub fn peek(&self) -> Option<char> {
        if self.index + 1 < self.chars.len() {
            Some(self.chars[self.index + 1].1)
        } else {
            None
        }
    }

    pub fn step(&mut self) -> Option<char> {
        self.index += 1;
        if self.index < self.chars.len() {
            Some(self.current())
        } else {
            None
        }
    }

    pub fn parse_miltiline_comment(&mut self, errors: &mut Errors) -> usize {
        let start = self.pos() - 1;
        let mut new_lines = 0;

        loop {
            match self.step() {
                Some('-') if self.peek() == Some('#') => {
                    self.step();
                    break;
                }
                Some('#') if self.peek() == Some('-') => {
                    self.step();
                    new_lines += self.parse_miltiline_comment(errors);
                }
                Some('\n') => {
                    new_lines += 1;
                }
                None => {
                    errors.emit(
                        Error::Text("Unexpected end of file".to_owned()),
                        start,
                        self.pos(),
                        self.module,
                    );
                    break;
                }
                _ => {}
            }
        }
        new_lines
    }

    pub fn unstep(&mut self) {
        self.index -= 1;
    }

    fn parse_token(&mut self, errors: &mut Errors) -> Option<Token> {
        fn emit_invalid(invalid: &mut Option<(u32, u32)>, errors: &mut Errors, module: ModuleId) {
            if let Some((start, end)) = invalid.take() {
                errors.emit(
                    Error::Text("UnexpectedCharacters".to_owned()),
                    start,
                    end,
                    module,
                );
            }
        }

        let mut start;
        let mut invalid_chars = None;

        let ty = loop {
            start = self.pos();
            if self.is_at_end() {
                emit_invalid(&mut invalid_chars, errors, self.module);
                return None;
            }

            break match self.current() {
                '#' => {
                    emit_invalid(&mut invalid_chars, errors, self.module);
                    if let Some('-') = self.peek() {
                        self.step();
                        if self.parse_miltiline_comment(errors) > 0 {
                            self.newline = true;
                        }
                    } else {
                        while let Some(c) = self.step() {
                            match c {
                                '#' if matches!(self.peek(), Some('-')) => {
                                    self.step();
                                    if self.parse_miltiline_comment(errors) > 0 {
                                        self.newline = true;
                                        break;
                                    }
                                }
                                '\n' => {
                                    self.newline = true;
                                    break;
                                }
                                _ => {}
                            }
                        }
                    }
                    self.step();
                    self.newline |= self.skip();
                    continue;
                }
                ':' => match self.peek() {
                    Some(':') => {
                        self.step();
                        TokenType::DoubleColon
                    }
                    Some('=') => {
                        self.step();
                        TokenType::Declare
                    }
                    _ => TokenType::Colon,
                },
                ',' => TokenType::Comma,
                ';' => TokenType::Semicolon,
                '.' => match self.peek() {
                    Some('.') => {
                        self.step();
                        match self.peek() {
                            Some('.') => {
                                self.step();
                                TokenType::TripleDot
                            }
                            Some('<') => {
                                self.step();
                                TokenType::DotDotLessThan
                            }
                            _ => TokenType::DotDot,
                        }
                    }
                    _ => TokenType::Dot,
                },
                '(' => TokenType::Left(Bracket::Paren),
                ')' => TokenType::Right(Bracket::Paren),
                '[' => TokenType::Left(Bracket::Square),
                ']' => TokenType::Right(Bracket::Square),
                '{' => TokenType::Left(Bracket::Curly),
                '}' => TokenType::Right(Bracket::Curly),

                '=' => {
                    if let Some('=') = self.peek() {
                        self.step();
                        TokenType::DoubleEquals
                    } else {
                        TokenType::Equals
                    }
                }

                '+' => {
                    if let Some('=') = self.peek() {
                        self.step();
                        TokenType::PlusEquals
                    } else {
                        TokenType::Plus
                    }
                }

                '-' => match self.peek() {
                    Some('=') => {
                        self.step();
                        TokenType::MinusEquals
                    }
                    Some('>') => {
                        self.step();
                        TokenType::Arrow
                    }
                    _ => TokenType::Minus,
                },

                '*' => {
                    if let Some('=') = self.peek() {
                        self.step();
                        TokenType::StarEquals
                    } else {
                        TokenType::Star
                    }
                }

                '/' => {
                    if let Some('=') = self.peek() {
                        self.step();
                        TokenType::SlashEquals
                    } else {
                        TokenType::Slash
                    }
                }

                '%' => {
                    if let Some('=') = self.peek() {
                        self.step();
                        TokenType::PercentEquals
                    } else {
                        TokenType::Percent
                    }
                }

                '&' => TokenType::Ampersand,
                '~' => TokenType::SnackWave,
                '^' => TokenType::Caret,

                '<' => {
                    if let Some('=') = self.peek() {
                        self.step();
                        TokenType::LessEquals
                    } else {
                        TokenType::LessThan
                    }
                }

                '>' => {
                    if let Some('=') = self.peek() {
                        self.step();
                        TokenType::GreaterEquals
                    } else {
                        TokenType::GreaterThan
                    }
                }

                '!' => {
                    if let Some('=') = self.peek() {
                        self.step();
                        TokenType::BangEquals
                    } else {
                        TokenType::Bang
                    }
                }

                '0'..='9' => {
                    let mut is_float = false;
                    while matches!(self.peek(), Some('0'..='9' | '.')) {
                        if self.step().unwrap() == '.' {
                            if let Some('.') = self.peek() {
                                self.unstep();
                                break;
                            }
                            if is_float {
                                errors.emit(
                                    Error::Text("Multiple dots in float literal".to_owned()),
                                    self.pos(),
                                    self.pos(),
                                    self.module,
                                );
                            }
                            is_float = true;
                        }
                    }

                    if is_float {
                        TokenType::FloatLiteral
                    } else {
                        TokenType::IntLiteral
                    }
                }

                '"' => {
                    while self.peek() != Some('"') {
                        if self.step().is_none() {
                            errors.emit(
                                Error::Text("Unexpected en of file".to_owned()),
                                start,
                                self.pos() - 1,
                                self.module,
                            )
                        }
                    }
                    self.step();
                    TokenType::StringLiteral
                }

                c @ ('A'..='Z' | 'a'..='z' | '_') => {
                    if c == '_' && !is_ident_char(self.peek()) {
                        break TokenType::Underscore;
                    }
                    while is_ident_char(self.peek()) {
                        self.step().unwrap();
                    }
                    if let Ok(keyword) = self.src[start as usize..=self.pos() as usize].parse() {
                        TokenType::Keyword(keyword)
                    } else {
                        TokenType::Ident
                    }
                }

                _ => {
                    let start = self.pos();
                    self.step();
                    let end = self.pos() - 1;
                    if let Some((_, chars_end)) = &mut invalid_chars {
                        *chars_end = end;
                    } else {
                        invalid_chars = Some((start, end));
                    }
                    self.newline |= self.skip();
                    continue;
                }
            };
        };

        emit_invalid(&mut invalid_chars, errors, self.module);
        let end = self.pos();
        self.step();
        let new_line = self.newline;
        self.newline = false;

        Some(Token {
            span: TSpan { start, end },
            ty,
            new_line,
        })
    }
}
