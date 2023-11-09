use std::path::{Path, PathBuf};

pub struct Dependency {
    pub path: PathBuf,
    pub is_std: bool,
}
