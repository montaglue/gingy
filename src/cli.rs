use clap;

use crate::config::Backend;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, clap::ValueEnum, Debug, Default)]
pub enum Command {
    #[default]
    Check,
    Run,
    Build,
    Jit,
    Lsp,
}

#[derive(clap::Parser, Default)]
#[clap(
    version,
    about,
    long_about = "A compiler for the gingy (not that gingy) programming language."
)]
pub struct Args {
    #[clap(value_enum)]
    pub command: Command,

    pub file: Option<String>,

    #[clap(long)]
    pub log: bool,

    #[clap(long)]
    pub link_command: Option<String>,

    #[clap(long)]
    pub nostd: bool,

    #[clap(long)]
    pub emit_obj: bool,

    #[clap(long)]
    pub lib: bool,

    #[clap(long)]
    pub benchmarks: bool,

    #[clap(short, long)]
    pub tokens: bool,

    #[clap(short, long)]
    pub reconstruct_src: bool,

    #[clap(long)]
    pub debug_infer: bool,

    #[clap(long)]
    pub ir: bool,

    #[clap(long)]
    pub backend_ir: bool,

    #[clap(long)]
    pub crash_on_error: bool,

    #[clap(short, long)]
    pub link: Vec<String>,

    #[clap(short, long, value_enum, default_value_t = Backend::LLVM)]
    pub backend: Backend,
}
