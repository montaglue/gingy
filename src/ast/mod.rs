use std::{
    collections::HashMap,
    hash::Hash,
    ops::{Index, IndexMut},
    path::PathBuf,
};

use crate::{
    parser::{Counts, IdentId},
    span::{self, Span, TSpan},
    token::Operator,
    types::Primitive,
};

pub struct Ast {
    pub modules: Vec<Module>,
    pub sources: Vec<(String, PathBuf)>,
    exprs: Vec<Expr>,
    extra: Vec<ExprRef>,
    defs: Vec<HashMap<String, Definition>>,
    pub functions: Vec<Function>,
    pub types: Vec<TypeDef>,
    pub traits: Vec<TraitDefinition>,
    pub globals: Vec<GlobalDefinition>,
    pub calls: Vec<Call>,
    pub member_access_count: u32,
}

impl Ast {
    pub fn new() -> Self {
        Self {
            modules: Vec::new(),
            sources: Vec::new(),
            exprs: Vec::new(),
            extra: Vec::new(),
            defs: Vec::new(),
            functions: Vec::new(),
            types: Vec::new(),
            traits: Vec::new(),
            globals: Vec::new(),
            calls: Vec::new(),
            member_access_count: 0,
        }
    }

    pub fn add_global(&mut self, global: GlobalDefinition) -> GlobalId {
        self.globals.push(global);
        GlobalId((self.globals.len() - 1) as _)
    }

    pub fn add_defs(&mut self, defs: HashMap<String, Definition>) -> Definitions {
        self.defs.push(defs);
        Definitions((self.defs.len() - 1) as _)
    }

    pub fn add_extra(&mut self, extra: &[ExprRef]) -> ExprExtra {
        let idx = ExprExtra {
            count: extra.len() as _,
            index: self.extra.len() as _,
        };
        self.extra.extend(extra);
        idx
    }

    pub fn add_expr(&mut self, expr: Expr) -> ExprRef {
        self.exprs.push(expr);
        ExprRef((self.exprs.len() - 1) as _)
    }

    pub fn add_type(&mut self, type_: TypeDef) -> TypeId {
        self.types.push(type_);
        TypeId((self.types.len() - 1) as _)
    }

    pub fn add_trait(&mut self, trait_: TraitDefinition) -> TraitId {
        self.traits.push(trait_);
        TraitId((self.traits.len() - 1) as _)
    }

    pub fn add_func(&mut self, func: Function) -> FunctionId {
        self.functions.push(func);
        FunctionId((self.functions.len() - 1) as _)
    }
}

impl Index<ExprRef> for Ast {
    type Output = Expr;

    fn index(&self, index: ExprRef) -> &Self::Output {
        &self.exprs[index.0 as usize]
    }
}

impl IndexMut<ExprRef> for Ast {
    fn index_mut(&mut self, index: ExprRef) -> &mut Self::Output {
        &mut self.exprs[index.0 as usize]
    }
}

pub enum Expr {
    Block {
        span: TSpan,
        items: ExprExtra,
        defs: Definitions,
    },

    Declare {
        pat: ExprRef,
        annotated_ty: UnresolvedType,
    },
    DeclareWithVal {
        pat: ExprRef,
        annotated_ty: UnresolvedType,
        val: ExprRef,
    },
    Return {
        start: u32,
        val: ExprRef,
    },
    ReturnUnit {
        start: u32,
    },
    IntLiteral(TSpan),
    FloatLiteral(TSpan),
    StringLiteral(TSpan),
    BoolLiteral {
        start: u32,
        val: bool,
    },
    EnumLiteral {
        span: TSpan,
        ident: TSpan,
        args: ExprExtra,
    },
    Record {
        span: TSpan,
        names: Vec<TSpan>,
        values: u32,
    },
    Nested(TSpan, ExprRef),
    Unit(TSpan),
    Variable {
        span: TSpan,
        id: IdentId,
    },
    Hole(u32),
    Array(TSpan, ExprExtra),
    Tuple(TSpan, ExprExtra),
    If {
        start: u32,
        cond: ExprRef,
        then: ExprRef,
    },
    IfElse {
        start: u32,
        cond: ExprRef,
        then: ExprRef,
        else_: ExprRef,
    },
    IfPat {
        start: u32,
        pat: ExprRef,
        value: ExprRef,
        then: ExprRef,
        else_: ExprRef,
    },

    Match {
        span: TSpan,
        val: ExprRef,
        extra_branches: u32,
        branch_count: u32,
    },
    While {
        start: u32,
        cond: ExprRef,
        body: ExprRef,
    },
    WhilePat {
        start: u32,
        pat: ExprRef,
        val: ExprRef,
        body: ExprRef,
    },
    FunctionCall(CallId),
    UnOp(u32, UnOp, ExprRef),
    BinOp(Operator, ExprRef, ExprRef),
    MemberAccess {
        left: ExprRef,
        name: TSpan,
        id: MemberAccessId,
    },
    Index {
        expr: ExprRef,
        idx: ExprRef,
        end: u32,
    },
    TupleIndex {
        expr: ExprRef,
        idx: u32,
        end: u32,
    },
    Cast(TSpan, UnresolvedType, ExprRef),
    Root(u32),
    Asm {
        span: TSpan,
        asm_str_span: TSpan,
        args: ExprExtra,
    },
}

impl Expr {
    pub fn start(&self, ast: &Ast) -> u32 {
        self.span(ast).start
    }

    pub fn end(&self, ast: &Ast) -> u32 {
        self.span(ast).end
    }

    pub fn span(&self, ast: &Ast) -> TSpan {
        let s = |r: &ExprRef| ast[*r].start(ast);
        let e = |r: &ExprRef| ast[*r].end(ast);

        match self {
            Expr::Block { span, .. }
            | Expr::StringLiteral(span)
            | Expr::IntLiteral(span)
            | Expr::FloatLiteral(span)
            | Expr::Record { span, .. }
            | Expr::Nested(span, ..)
            | Expr::Unit(span)
            | Expr::Variable { span, .. }
            | Expr::Array(span, ..)
            | Expr::Tuple(span, ..)
            | Expr::Cast(span, ..)
            | Expr::Match { span, .. }
            | Expr::EnumLiteral { span, .. } => *span,
            Expr::Declare { pat, annotated_ty } => TSpan {
                start: s(pat),
                end: annotated_ty.span().end,
            },
            Expr::DeclareWithVal { pat, val, .. } => TSpan {
                start: s(pat),
                end: e(val),
            },
            Expr::Return { start, val } => todo!(),
            Expr::ReturnUnit { start } => todo!(),
            Expr::BoolLiteral { start, val } => todo!(),
            Expr::Hole(_) => todo!(),
            Expr::If { start, cond, then } => todo!(),
            Expr::IfElse {
                start,
                cond,
                then,
                else_,
            } => todo!(),
            Expr::IfPat {
                start,
                pat,
                value,
                then,
                else_,
            } => todo!(),
            Expr::While { start, cond, body } => todo!(),
            Expr::WhilePat {
                start,
                pat,
                val,
                body,
            } => todo!(),
            Expr::FunctionCall(_) => todo!(),
            Expr::UnOp(_, _, _) => todo!(),
            Expr::BinOp(_, _, _) => todo!(),
            Expr::MemberAccess { left, name, id } => todo!(),
            Expr::Index { expr, idx, end } => todo!(),
            Expr::TupleIndex { expr, idx, end } => todo!(),
            Expr::Root(_) => todo!(),
            Expr::Asm {
                span,
                asm_str_span,
                args,
            } => todo!(),
        }
    }

    pub fn span_in(&self, ast: &Ast, module: ModuleId) -> Span {
        todo!()
    }
}

pub struct MemberAccessId(u32);

pub enum UnOp {
    Neg,
    Not,
    Ref,
    Deref,
}

pub struct CallId(u32);

pub struct Call {
    pub called_expr: ExprRef,
    args: ExprExtra,
    pub end: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnresolvedType {
    Primitive(Primitive, TSpan),
    Unresolved(IdentPath, Option<(Vec<UnresolvedType>, TSpan)>),
    Pointer(Box<(UnresolvedType, u32)>),
    Array(Box<(UnresolvedType, TSpan, Option<u32>)>),
    Tuple(Vec<UnresolvedType>, TSpan),
    Infer(u32),
}

impl UnresolvedType {
    pub fn span(&self) -> TSpan {
        match self {
            UnresolvedType::Primitive(_, span) | UnresolvedType::Tuple(_, span) => *span,
            UnresolvedType::Unresolved(path, generics) => generics
                .as_ref()
                .map_or_else(|| path.0, |generics| generics.1),
            UnresolvedType::Pointer(ptr) => {
                let (inner, start) = &**ptr;
                TSpan {
                    start: *start,
                    end: inner.span().end,
                }
            }
            UnresolvedType::Array(array) => array.1,
            UnresolvedType::Infer(s) => TSpan { start: *s, end: *s },
        }
    }
}

pub struct ExprExtra {
    pub index: u32,
    pub count: u32,
}

pub struct ExprExtraSpans(u32, u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprRef(u32);

pub struct Function {
    pub params: Vec<(String, UnresolvedType, u32, u32)>,
    pub generics: Vec<GenericDef>,
    pub varargs: bool,
    pub return_type: UnresolvedType,
    pub body: Option<ExprRef>,
    pub counts: Counts,
    pub span: TSpan,
}

pub struct StructDefinition {
    pub name: String,
    pub generics: Vec<GenericDef>,
    pub members: Vec<(String, UnresolvedType, u32, u32)>,
    pub methods: HashMap<String, FunctionId>,
}

pub struct EnumDefinition {
    pub name: String,
    pub generics: Vec<GenericDef>,
    pub variants: Vec<(TSpan, String, Vec<UnresolvedType>)>,
    pub methods: HashMap<String, FunctionId>,
}

pub enum TypeDef {
    Struct(StructDefinition),
    Enum(EnumDefinition),
}

pub struct TraitDefinition {
    pub generics: Vec<GenericDef>,
    pub functions: HashMap<String, (TSpan, Function)>,
}

pub struct GlobalDefinition {
    pub ty: UnresolvedType,
    pub val: Option<(ExprRef, Counts)>,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionId(u32);

#[derive(Debug, Clone, Copy)]
pub struct ModuleId(u32);

pub struct TypeId(u32);
pub struct TraitId(u32);

pub struct GlobalId(u32);

pub enum Definition {
    Function(FunctionId),
    Type(TypeId),
    Trait(TraitId),
    Module(ModuleId),
    Use(IdentPath),
    Const {
        ty: UnresolvedType,
        val: ExprRef,
        counts: Counts,
    },
    Global(GlobalId),
}

pub struct Definitions(u32);

pub enum Item {
    Definition {
        name: String,
        name_span: TSpan,
        def: Definition,
    },
    Impl(TraitImpl),
    Expr(ExprRef),
}

pub struct TraitImpl {
    pub impl_generics: Vec<GenericDef>,
    pub trait_path: IdentPath,
    pub trait_generics: Option<(Vec<UnresolvedType>, TSpan)>,
    pub ty: UnresolvedType,
    pub function: HashMap<String, FunctionId>,
    pub impl_keyword_start: u32,
}

pub struct GenericDef {
    pub name: TSpan,
    pub requirements: Vec<IdentPath>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IdentPath(TSpan);

impl IdentPath {
    pub fn segments<'a>(
        &'a self,
        src: &'a str,
    ) -> (
        Option<TSpan>,
        impl Iterator<Item = (&str, TSpan)>,
        Option<(&str, TSpan)>,
    ) {
        let start_addr = src.as_ptr() as usize;

        let s = &src[self.0.range()];

        let mut split = s
            .split('.')
            .map(move |segment| {
                let trimmed = segment.trim();
                let idx = (trimmed.as_ptr() as usize - start_addr) as u32;
                (
                    trimmed,
                    TSpan {
                        start: idx,
                        end: idx + trimmed.len() as u32 - 1,
                    },
                )
            })
            .peekable();

        let first = split.peek().copied();
        let last = split.next_back().unwrap();

        if let Some(("root", first_span)) = first {
            split.next();
            let last = if last.0 == "root" { None } else { Some(last) };
            (Some(first_span), split, last)
        } else {
            (None, split, Some(last))
        }
    }
}

pub struct Module {
    pub definitions: Definitions,
    pub impls: Vec<TraitImpl>,
    pub uses: Vec<IdentPath>,
    pub root: ModuleId,
}
