use super::{
    const_val::ConstVal, std_builtins::Buildins, trait_impls::TraitImpls, MemberAccess,
    ResolvedCall,
};

pub struct MaybeTypeDef;

pub struct FunctionHeader;

pub struct TraitDef;

pub struct TypeRef(u32);

pub enum Type {}

pub struct SymbolTable {
    pub builtins: Buildins,
    pub funcs: Vec<Option<FunctionHeader>>,
    pub types: Vec<(String, MaybeTypeDef)>,
    pub traits: Vec<Option<TraitDef>>,
    pub consts: Vec<ConstVal>,
    pub globas: Vec<Option<(String, Type, Option<ConstVal>)>>,
    pub trait_impls: TraitImpls,
    pub expr_types: Vec<TypeRef>,
    pub calls: Vec<Option<ResolvedCall>>,
    pub member_accesses: Vec<MemberAccess>,
}
