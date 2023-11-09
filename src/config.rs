use std::{env::current_exe, fmt::Display, fs::try_exists, path::PathBuf, time::Duration};

const SEPARATOR_LINE: &str = "------------------------------\n";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, clap::ValueEnum)]
pub enum Backend {
    C,
    #[default]
    LLVM,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Stats {
    pub file_times: Vec<FileStats>,
    pub resolve: Duration,
    pub irgen: Duration,
}

impl Display for Stats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{SEPARATOR_LINE}\
            Timings of {} files:",
            self.file_times.len()
        )?;

        let mut overall_lex = Duration::ZERO;
        let mut overall_parse = Duration::ZERO;

        for file in &self.file_times {
            writeln!(
                f,
                "\t{}: {:?} (lex: {:?}, parse: {:?})",
                file.name,
                file.lex + file.parse,
                file.lex,
                file.parse,
            )?;
            overall_lex += file.lex;
            overall_parse += file.parse;
        }

        write!(
            f,
            "\nOverall: {:?} \n\tlex: {:?}\n\tparse: {:?}\n\tresolve: {:?}\n\tirgen: {:?}\n\
            {SEPARATOR_LINE}",
            overall_lex + overall_parse + self.resolve + self.irgen,
            overall_lex,
            overall_parse,
            self.resolve,
            self.irgen,
        )?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FileStats {
    name: String,
    lex: Duration,
    parse: Duration,
}

pub struct BackendStats {
    name: &'static str,
    init: Duration,
    type_creation: Duration,
    func_header_creation: Duration,
    emit: Duration,
}

impl Display for BackendStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{SEPARATOR_LINE}Backend Imings: {}", self.name)?;
        writeln!(f, "\tInit: {:?}", self.init)?;
        writeln!(f, "\tType creation: {:?}", self.type_creation)?;
        writeln!(
            f,
            "\tFunction header creation: {:?}",
            self.func_header_creation
        )?;
        writeln!(f, "\tEmit: {:?}", self.emit)?;
        write!(
            f,
            "Overall: {:?}\n{SEPARATOR_LINE}",
            self.init + self.type_creation + self.func_header_creation + self.emit
        )?;
        Ok(())
    }
}

pub struct BackendParams<'a> {
    pub benchmarks: bool,
    pub show_backend_ir: bool,
    pub jit: bool,
    pub obj_file: &'a str,
    pub project_name: &'a str,
}

pub struct Debug {
    pub tokens: bool,
    pub reconstruct_src: bool,
}

pub fn std_path() -> PathBuf {
    match current_exe()
        .ok()
        .and_then(|path| path.parent().map(|p| p.join("std")))
    {
        Some(path) => match try_exists(&path) {
            Ok(true) => path,
            _ => "std".into(),
        },
        _ => "std".into(),
    }
}
