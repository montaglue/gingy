use std::ops::RangeInclusive;

use crate::ast::ModuleId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TSpan {
    pub start: u32,
    pub end: u32,
}

impl TSpan {
    pub fn in_mod(self, module: ModuleId) -> Span {
        Span {
            start: self.start,
            end: self.end,
            module,
        }
    }

    pub fn range(self) -> RangeInclusive<usize> {
        self.start as usize..=self.end as usize
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: u32,
    pub end: u32,
    pub module: ModuleId,
}
