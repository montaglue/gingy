use std::str::FromStr;

use crate::{
    span::TSpan,
    types::{FloatType, IntType, Primitive},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token {
    pub span: TSpan,
    pub ty: TokenType,
    pub new_line: bool,
}

impl Token {
    pub fn get_val<'a>(&self, src: &'a str) -> &'a str {
        &src[self.span.start as usize..=self.span.end as usize]
    }

    pub fn is<const N: usize>(&self, types: impl Into<[TokenType; N]>) -> bool {
        types.into().contains(&self.ty)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Bracket {
    Paren,
    Square,
    Curly,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    Colon,
    DoubleColon,
    Comma,
    Semicolon,
    Dot,
    DotDot,
    DotDotLessThan,
    TripleDot,

    Left(Bracket),
    Right(Bracket),

    Bang,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    Ampersand,
    SnackWave,
    Caret,

    Underscore,

    Equals,
    DoubleEquals,
    BangEquals,

    PlusEquals,
    MinusEquals,
    StarEquals,
    SlashEquals,
    PercentEquals,

    LessThan,
    GreaterThan,
    LessEquals,
    GreaterEquals,

    Declare,

    Arrow,

    StringLiteral,
    IntLiteral,
    FloatLiteral,

    Keyword(Keyword),
    Ident,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    Primitive(Primitive),
    Fn,
    Ret,
    True,
    False,
    And,
    Or,
    As,
    Struct,
    Enum,
    Trait,
    Impl,
    If,
    Else,
    Match,
    While,
    For,
    Extern,
    Root,
    Use,
    Asm,
}

impl FromStr for Keyword {
    type Err = ();

    fn from_str(s: &str) -> Result<Keyword, ()> {
        use Keyword::Primitive as P;
        use Primitive::*;

        Ok(match s {
            "i8" => P(Int(IntType::I8)),
            "i16" => P(Int(IntType::I16)),
            "i32" => P(Int(IntType::I32)),
            "i64" => P(Int(IntType::I64)),
            "i128" => P(Int(IntType::I128)),
            "u8" => P(Int(IntType::U8)),
            "u16" => P(Int(IntType::U16)),
            "u32" => P(Int(IntType::U32)),
            "u32" => P(Int(IntType::u32)),
            "u128" => P(Int(IntType::U128)),
            "f32" => P(Float(FloatType::F32)),
            "f64" => P(Float(FloatType::F64)),
            "bool" => P(Primitive::Bool),
            "type" => P(Primitive::Type),
            "fn" => Keyword::Fn,
            "ret" => Keyword::Ret,
            "true" => Keyword::True,
            "false" => Keyword::False,
            "and" => Keyword::And,
            "or" => Keyword::Or,
            "as" => Keyword::As,
            "struct" => Keyword::Struct,
            "enum" => Keyword::Enum,
            "trait" => Keyword::Trait,
            "impl" => Keyword::Impl,
            "if" => Keyword::If,
            "else" => Keyword::Else,
            "match" => Keyword::Match,
            "while" => Keyword::While,
            "for" => Keyword::For,
            "extern" => Keyword::Extern,
            "root" => Keyword::Root,
            "use" => Keyword::Use,
            "asm" => Keyword::Asm,
            _ => return Err(()),
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Assignment(AssignmentType),

    Or,
    And,
    Equals,
    NotEquals,

    LessThan,
    GreaterThan,
    LessEquals,
    GreaterEquals,

    Range,
    RangeExclusive,
}

#[derive(Debug, Clone, Copy)]
pub enum AssignmentType {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
}

impl Operator {
    pub fn precedence(self) -> u8 {
        use Operator::*;
        match self {
            Assignment(_) => 10,
            Range | RangeExclusive => 20,
            Or => 30,
            And => 40,
            Equals | NotEquals => 50,
            LessThan | GreaterThan | LessEquals | GreaterEquals => 60,
            Add | Sub => 70,
            Mul | Div | Mod => 80,
        }
    }

    pub fn is_boolean(self) -> bool {
        use Operator::*;
        match self {
            Or | And => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum ExpectedTokens {
    Specific(TokenType),
    AnyOf(Vec<TokenType>),
    Expr,
    Type,
}
