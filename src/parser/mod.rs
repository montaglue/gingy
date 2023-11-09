use std::collections::HashMap;

use crate::{
    ast::{
        Ast, Definition, EnumDefinition, Expr, ExprRef, Function, FunctionId, GenericDef,
        GlobalDefinition, IdentPath, Item, Module, ModuleId, StructDefinition, TraitDefinition,
        TraitImpl, TypeDef, UnresolvedType,
    },
    error::{CompilerError, CompilerResult, Error},
    resolve::types::TraitDef,
    span::{Span, TSpan},
    token::{Bracket, ExpectedTokens, Keyword, Token, TokenType},
    types::Primitive,
};

use self::reader::{Delimit, TokenReader};

pub mod reader;

pub struct IdentId(u32);

pub struct Counts {
    pub idents: u32,
}

impl Counts {
    pub fn new() -> Self {
        Self { idents: 0 }
    }

    fn ident(&mut self) -> IdentId {
        self.idents += 1;
        IdentId(self.idents as u32)
    }
}

pub struct Parser<'a> {
    src: &'a str,
    tokens: TokenReader,
    ast: &'a mut Ast,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, src: &'a str, ast: &'a mut Ast, module: ModuleId) -> Self {
        Self {
            src,
            tokens: TokenReader::new(tokens, module),
            ast,
        }
    }

    pub fn parse(&mut self, root_module: ModuleId) -> Result<Module, CompilerError> {
        todo!()
    }

    fn parse_delimited<F, D>(
        &mut self,
        delim: TokenType,
        end: TokenType,
        mut item: F,
    ) -> Result<Token, CompilerError>
    where
        F: FnMut(&mut Self) -> Result<D, CompilerError>,
        D: Into<Delimit>,
    {
        loop {
            if let Some(end_tok) = self.tokens.step_if([end]) {
                return Ok(end_tok);
            }
            let delimit = item(self)?.into();

            match delimit {
                Delimit::Yes => {
                    let delim_or_end = self.tokens.step()?;

                    if delim_or_end.ty == delim {
                        continue;
                    }

                    if delim_or_end.ty == end {
                        return Ok(delim_or_end);
                    }

                    return Err(Error::UnexpectedToken {
                        expected: ExpectedTokens::AnyOf(vec![delim, end]),
                        found: delim_or_end.ty,
                    }
                    .at_span(delim_or_end.span.in_mod(self.tokens.module)));
                }
                Delimit::No => {}
                Delimit::Optional => {
                    self.tokens.step_if([delim]);
                }
                Delimit::OptionalIfNewLine => {
                    if self.tokens.step_if([delim]).is_none() {
                        if let Some(after) = self.tokens.peek() {
                            if !after.new_line && after.ty != end {
                                dbg!(after);
                                return Err(Error::UnexpectedToken {
                                    expected: ExpectedTokens::AnyOf(vec![delim, end]),
                                    found: after.ty,
                                }
                                .at_span(after.span.in_mod(self.tokens.module)));
                            }
                        }
                    }
                }
            }
        }
    }

    fn parse_module(&mut self, root: ModuleId) -> Result<Module, CompilerError> {
        let mut definitions: HashMap<String, Definition> = HashMap::new();
        let mut impls: Vec<TraitImpl> = Vec::new();
        let mut uses: Vec<IdentPath> = Vec::new();

        while !self.tokens.is_at_end() {
            let start = self.tokens.current().unwrap().span.start;
            let name_pat =
                |pat: ExprRef, s: &Self| match &s.ast[pat] {
                    Expr::Variable { span, .. } => Ok(*span),
                    expr => Err(Error::InvalidGlobalVarPattern
                        .at_span(expr.span_in(s.ast, s.tokens.module))),
                };

            let mut item_counts = Counts::new();

            let def = match self.parse_item(&mut item_counts)? {
                Item::Definition {
                    name,
                    name_span,
                    def,
                } => Some((name, name_span, def)),
                Item::Impl(impl_) => {
                    impls.push(impl_);
                    None
                }
                Item::Expr(expr) => match &self.ast[expr] {
                    Expr::Declare { pat, annotated_ty } => {
                        let name = name_pat(*pat, self)?;
                        let id = self.ast.add_global(GlobalDefinition {
                            ty: annotated_ty.clone(),
                            val: None,
                        });
                        Some((
                            self.src[name.range()].to_owned(),
                            name,
                            Definition::Global(id),
                        ))
                    }
                    Expr::DeclareWithVal {
                        pat,
                        annotated_ty,
                        val,
                    } => {
                        let name = name_pat(*pat, self)?;
                        let id = self.ast.add_global(GlobalDefinition {
                            ty: annotated_ty.clone(),
                            val: Some((*val, item_counts)),
                        });
                        Some((
                            self.src[name.range()].to_owned(),
                            name,
                            Definition::Global(id),
                        ))
                    }
                    _ => {
                        return Err(CompilerError {
                            error: Error::InvalidTopLevelBlockItem,
                            span: Span {
                                start,
                                end: self.tokens.current_end_pos(),
                                module: self.tokens.module,
                            },
                        })
                    }
                },
            };

            if let Some((name, name_span, def)) = def {
                if let Some(_existiong) = definitions.insert(name, def) {
                    return Err(
                        Error::DuplicateDefinition.at_span(name_span.in_mod(self.tokens.module))
                    );
                }
            }
        }

        uses.shrink_to_fit();

        Ok(Module {
            definitions: self.ast.add_defs(definitions),
            impls,
            uses,
            root,
        })
    }

    fn parse_block_or_expr(&mut self, counts: &mut Counts) -> Result<ExprRef, CompilerError> {
        let token = self.tokens.peek().ok_or_else(|| {
            Error::UnexpectedEndOfFile.at(
                self.tokens.last_src_pos(),
                self.tokens.last_src_pos(),
                self.tokens.module,
            )
        })?;

        match token.ty {
            TokenType::Left(Bracket::Curly) => self.parse_block(counts),
            TokenType::Colon => {
                self.tokens.step_expect([TokenType::Colon])?;
                self.parse_expr(counts)
            }
            _ => {
                return Err(Error::UnexpectedToken {
                    expected: ExpectedTokens::AnyOf(vec![
                        TokenType::Left(Bracket::Curly),
                        TokenType::Colon,
                    ]),
                    found: token.ty,
                }
                .at_span(Span {
                    start: self.tokens.last_src_pos(),
                    end: self.tokens.last_src_pos(),
                    module: self.tokens.module,
                }))
            }
        }
    }

    fn parse_block(&mut self, counts: &mut Counts) -> Result<ExprRef, CompilerError> {
        let lbrace = self.tokens.step_expect([TokenType::Left(Bracket::Curly)])?;
        self.parse_block_from_lbrace(lbrace, counts)
    }

    fn parse_block_from_lbrace(
        &mut self,
        lbrace: Token,
        counts: &mut Counts,
    ) -> Result<ExprRef, CompilerError> {
        let mut defs = HashMap::new();
        let mut impls = Vec::new();
        let mut items = Vec::new();

        while self.tokens.current()?.ty != TokenType::Right(Bracket::Curly) {
            let start = self.tokens.current().unwrap().span.start;

            match self.parse_item(counts)? {
                Item::Definition {
                    name,
                    name_span,
                    def,
                } => {
                    if let Some(_existing) = defs.insert(name, def) {
                        return Err(Error::DuplicateDefinition
                            .at_span(name_span.in_mod(self.tokens.module)));
                    }
                }

                Item::Impl(impl_) => impls.push(impl_),
                Item::Expr(item) => items.push(item),
            }
        }
        let rbrace = self
            .tokens
            .step_expect([TokenType::Right(Bracket::Curly)])?;
        let items = self.ast.add_extra(&items);
        let defs = self.ast.add_defs(defs);

        Ok(self.ast.add_expr(Expr::Block {
            span: TSpan {
                start: lbrace.span.start,
                end: rbrace.span.end,
            },
            items,
            defs,
        }))
    }

    fn parse_item(&mut self, counts: &mut Counts) -> Result<Item, CompilerError> {
        let current = self.tokens.current()?;
        Ok(match &current.ty {
            TokenType::Keyword(Keyword::Use) => {
                self.tokens.step_assert(TokenType::Keyword(Keyword::Use));
                let use_token = self.tokens.current().unwrap();
                let use_start = use_token.span.start;
                let use_end = use_token.span.end;

                let path = self.parse_path()?;
                let last = path.segments(self.src).2.ok_or(CompilerError {
                    error: Error::CantUseRootPath,
                    span: Span {
                        start: use_start,
                        end: use_end,
                        module: self.tokens.module,
                    },
                })?;

                let name = if self
                    .tokens
                    .step_if([TokenType::Keyword(Keyword::As)])
                    .is_some()
                {
                    self.src[self.tokens.step_expect([TokenType::Ident])?.span.range()].to_owned()
                } else {
                    last.0.to_owned()
                };

                Item::Definition {
                    name,
                    name_span: last.1,
                    def: Definition::Use(path),
                }
            }
            TokenType::Keyword(Keyword::Impl) => {
                let impl_token = self.tokens.step_assert(TokenType::Keyword(Keyword::Impl));
                Item::Impl(self.parse_trait_impl(impl_token)?)
            }
            TokenType::Ident => {
                let ident = self.tokens.step_assert(TokenType::Ident);
                let ident_span = ident.span;

                let name = ident.get_val(self.src);

                match self.tokens.peek().map(|t| t.ty) {
                    Some(TokenType::DoubleColon) => {
                        self.tokens.step_assert(TokenType::DoubleColon);

                        let def = if let Some(fn_token) =
                            self.tokens.step_if([TokenType::Keyword(Keyword::Fn)])
                        {
                            let func = self.parse_function_def(fn_token)?;
                            Definition::Function(self.ast.add_func(func))
                        } else if let Some(struct_token) =
                            self.tokens.step_if([TokenType::Keyword(Keyword::Struct)])
                        {
                            let def = self.parse_struct_def(name, struct_token)?;
                            Definition::Type(self.ast.add_type(TypeDef::Struct(def)))
                        } else if self
                            .tokens
                            .step_if([TokenType::Keyword(Keyword::Enum)])
                            .is_some()
                        {
                            let def = self.parse_enum_def(name.to_owned())?;
                            Definition::Type(self.ast.add_type(TypeDef::Enum(def)))
                        } else if let Some(trait_token) =
                            self.tokens.step_if([TokenType::Keyword(Keyword::Trait)])
                        {
                            let def = self.parse_trait_def(trait_token)?;
                            Definition::Trait(self.ast.add_trait(def))
                        } else {
                            let mut counts = Counts::new();
                            let val = self.parse_expr(&mut counts)?;

                            Definition::Const {
                                ty: UnresolvedType::Infer(0),
                                val,
                                counts,
                            }
                        };
                        Item::Definition {
                            name: name.to_owned(),
                            name_span: ident_span,
                            def,
                        }
                    }
                    Some(TokenType::Colon) => {
                        self.tokens.step_assert(TokenType::Colon);
                        let ty = self.parse_type()?;
                        if self.tokens.step_if([TokenType::Equals]).is_some() {
                            let pat = self.ast.add_expr(Expr::Variable {
                                span: ident_span,
                                id: counts.ident(),
                            });
                            let val = self.parse_expr(counts)?;

                            Item::Expr(self.ast.add_expr(Expr::DeclareWithVal {
                                pat,
                                annotated_ty: ty,
                                val,
                            }))
                        } else if self.tokens.step_if([TokenType::Colon]).is_some() {
                            let mut counts = Counts::new();

                            Item::Definition {
                                name: name.to_owned(),
                                name_span: ident_span,
                                def: Definition::Const {
                                    ty,
                                    val: self.parse_expr(&mut counts)?,
                                    counts,
                                },
                            }
                        } else {
                            let pat = self.ast.add_expr(Expr::Variable {
                                span: ident_span,
                                id: counts.ident(),
                            });
                            Item::Expr(self.ast.add_expr(Expr::Declare {
                                pat,
                                annotated_ty: ty,
                            }))
                        }
                    }
                    Some(TokenType::Declare) => {
                        let decl_start = self.tokens.step_assert(TokenType::Declare).span.start;
                        let pat = self.ast.add_expr(Expr::Variable {
                            span: ident_span,
                            id: counts.ident(),
                        });
                        let val = self.parse_expr(counts)?;

                        Item::Expr(self.ast.add_expr(Expr::DeclareWithVal {
                            pat,
                            annotated_ty: UnresolvedType::Infer(decl_start),
                            val,
                        }))
                    }
                    _ => {
                        let var = self.ast.add_expr(Expr::Variable {
                            span: ident_span,
                            id: counts.ident(),
                        });
                        let expr = self.parse_stmt_starting_with(var, counts)?;
                        Item::Expr(expr)
                    }
                }
            }
            _ => Item::Expr(self.parse_stmt(counts)?),
        })
    }

    fn parse_type(&mut self) -> Result<UnresolvedType, CompilerError> {
        let type_token = self.tokens.step()?;
        match type_token.ty {
            TokenType::Keyword(Keyword::Root) => Ok(UnresolvedType::Unresolved(
                self.parse_rest_of_path(type_token.span),
                self.parse_optional_generic_instance()?,
            )),
            TokenType::Ident => Ok(UnresolvedType::Unresolved(
                self.parse_rest_of_path(type_token.span),
                self.parse_optional_generic_instance()?,
            )),
            TokenType::Keyword(Keyword::Primitive(primitive)) => {
                Ok(UnresolvedType::Primitive(primitive, type_token.span))
            }
            _ => todo!(),
        }
    }

    fn parse_rest_of_path(&mut self, span: TSpan) -> IdentPath {
        todo!()
    }

    fn parse_optional_generic_instance(
        &mut self,
    ) -> Result<Option<(Vec<UnresolvedType>, TSpan)>, CompilerError> {
        todo!()
    }

    fn parse_stmt_starting_with(
        &mut self,
        expr: ExprRef,
        counts: &mut Counts,
    ) -> Result<ExprRef, CompilerError> {
        todo!()
    }

    fn parse_trait_def(&mut self, trait_token: Token) -> Result<TraitDefinition, CompilerError> {
        todo!()
    }

    fn parse_struct_def(
        &mut self,
        name: &str,
        struct_token: Token,
    ) -> Result<StructDefinition, CompilerError> {
        todo!()
    }

    fn parse_enum_def(&mut self, name: String) -> Result<EnumDefinition, CompilerError> {
        todo!()
    }

    fn parse_trait_impl(&mut self, impl_token: Token) -> Result<TraitImpl, CompilerError> {
        todo!()
    }

    fn parse_function_def(&mut self, fn_token: Token) -> Result<Function, CompilerError> {
        let mut func = self.parse_function_header(fn_token)?;
        func.body = self.parse_function_body(&mut func.counts)?;
        Ok(func)
    }

    fn parse_function_header(&mut self, fn_token: Token) -> Result<Function, CompilerError> {
        debug_assert_eq!(fn_token.ty, TokenType::Keyword(Keyword::Fn));
        let start = fn_token.span.start;
        let generics = self.parse_optional_generics()?.map_or(Vec::new(), |g| g.1);

        let mut params = Vec::new();
        let mut varargs = false;

        if self
            .tokens
            .step_if([TokenType::Left(Bracket::Paren)])
            .is_some()
        {}

        let return_type = if self.tokens.step_if([TokenType::Arrow]).is_some() {
            self.parse_type()?
        } else {
            UnresolvedType::Primitive(Primitive::Unit, self.tokens.previouse().unwrap().span)
        };

        let end = self.tokens.previouse().unwrap().span.end;

        let idents = params.len() as _;

        Ok(Function {
            params,
            generics,
            varargs,
            return_type,
            body: None,
            counts: Counts { idents },
            span: TSpan { start, end },
        })
    }

    fn parse_optional_generics(
        &mut self,
    ) -> Result<Option<(TSpan, Vec<GenericDef>)>, CompilerError> {
        self.tokens
            .step_if([TokenType::Left(Bracket::Square)])
            .map(|l| {
                let mut generics = Vec::new();
                let r = self.parse_delimited(
                    TokenType::Comma,
                    TokenType::Right(Bracket::Square),
                    |p| {
                        let name = p.tokens.step_expect([TokenType::Ident])?.span;
                        let mut requirements = Vec::new();
                        if p.tokens.step_if([TokenType::Colon]).is_some() {
                            loop {
                                requirements.push(p.parse_path()?);
                                if p.tokens.step_if([TokenType::Plus]).is_none() {
                                    break;
                                }
                            }
                            p.tokens.step_if([TokenType::Ident]);
                        }
                        generics.push(GenericDef { name, requirements });
                        Ok(Delimit::OptionalIfNewLine)
                    },
                )?;
                Ok((l.span, generics))
            })
            .transpose()
    }

    fn parse_function_body(
        &mut self,
        counts: &mut Counts,
    ) -> Result<Option<ExprRef>, CompilerError> {
        todo!()
    }

    fn parse_expr(&mut self, counts: &mut Counts) -> Result<ExprRef, CompilerError> {
        todo!()
    }

    fn parse_stmt(&mut self, counts: &mut Counts) -> Result<ExprRef, CompilerError> {
        todo!()
    }

    fn parse_path(&mut self) -> Result<IdentPath, CompilerError> {
        todo!()
    }
}
