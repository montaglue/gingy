#![feature(fs_try_exists)]
use std::{
    collections::HashMap,
    fs::{create_dir, try_exists, File},
    io::ErrorKind,
    path::{Path, PathBuf},
    time::Instant,
};

use cli::Command;
use config::{std_path, Backend, BackendParams, Stats};
use dependency::Dependency;
use error::{CompilerResult, Error};
use ir::Module;

use crate::cli::Args;

extern crate llvm_sys as llvm;

mod ast;
mod backend;
mod cli;
mod compile;
mod config;
mod dependency;
mod error;
mod ir;
mod irgen;
mod lexer;
mod link;
mod lsp;
mod parser;
mod resolve;
mod span;
mod token;
mod types;

fn main() {
    let args: Args = clap::Parser::parse();
    if let Err(e) = run(args) {
        eprintln!("{:?}", e);
        std::process::exit(1);
    }
}

fn run(args: Args) -> CompilerResult<()> {
    if args.command == Command::Lsp {
        return lsp::lsp(args);
    }

    let path = Path::new(args.file.as_deref().unwrap_or("./"));

    let no_extnsion = path.with_extension("");
    let project_name = no_extnsion.file_name().unwrap().to_str().unwrap();

    let mut stats = Stats::default();

    let (project, ast) = {
        let is_std = path.file_name().map(|s| s.to_str().unwrap()) == Some("std");

        let mut dependencies = HashMap::new();
        if !args.nostd && !is_std {
            dependencies.insert(
                "std".to_owned(),
                Dependency {
                    path: std_path(),
                    is_std: true,
                },
            );
        }

        let (result, ast, errors) = compile::project(
            path,
            config::Debug {
                tokens: args.tokens,
                reconstruct_src: args.reconstruct_src,
            },
            dependencies,
            !args.emit_obj && !args.lib,
            &mut stats,
            is_std,
        );

        if errors.errors.len() > 0 {
            return Err(errors.into());
        }

        if errors.warnings.len() > 0 {
            eprintln!("{} warnings", errors.warnings.len());
            for warning in errors.warnings {
                eprintln!("{:?}", warning.message);
            }
        }

        (result?, ast)
    };

    match args.command {
        Command::Check => {
            println!("Check successful!");
        }
        Command::Build | Command::Run | Command::Jit => {
            let reduce_start_time = Instant::now();

            let ir = project
                .ir
                .finish_module(project.symbols, &ast, project.main);

            stats.irgen += reduce_start_time.elapsed();

            if args.ir {
                println!("{:?}", ir);
            }

            build_project(&args, &ir, project_name)?;
        }
        Command::Lsp => unreachable!(),
    }

    if args.benchmarks {
        println!("{}", stats);
    }
    Ok(())
}

pub fn build_project(args: &Args, ir: &ir::Module, project_name: &str) -> CompilerResult<()> {
    let ojb_file = format!("gingybuild/{project_name}.o");
    let exe_file = format!("gingybuild/{project_name}");

    let jit = args.command == Command::Jit;

    if !jit
        && !try_exists("gingybuild").expect("Faild to check availability of gingytbould directory")
    {
        match create_dir("gingybuild") {
            Ok(_) => (),
            Err(err) if err.kind() == ErrorKind::AlreadyExists => (),
            Err(_) => {
                return Err(Error::Text(
                    "Failed to create gingybuild directory".to_owned(),
                ))
            }
        }
    }

    if jit && args.lib {
        return Err(Error::Text("Cannot build a library in jit mode".to_owned()));
    }

    if jit && args.backend != config::Backend::LLVM {
        return Err(Error::Text(
            "Cannot use jit mode with backend other than llvm".to_owned(),
        ));
    }

    let params = BackendParams {
        benchmarks: args.benchmarks,
        show_backend_ir: args.backend_ir,
        jit,
        obj_file: &ojb_file,
        project_name,
    };

    run_backend(ir, args.backend, params)?;

    if args.command == Command::Jit {
        return Ok(());
    }

    if args.emit_obj {
        println!("Object successfully emitted to {}", ojb_file);
        return Ok(());
    }

    link::link(&ojb_file, &exe_file, args)?;

    if args.command == Command::Run {
        if args.lib {
            return Err(Error::Text("Cannot run a library".to_owned()));
        }
        println!("Running {}", project_name);
        execute_file(&exe_file);
    } else {
        println!("Build successful!");
    }

    Ok(())
}

pub fn execute_file(file: impl AsRef<Path>) -> ! {
    let mut command = std::process::Command::new(file.as_ref());

    #[cfg(unix)]
    {
        let error = std::os::unix::process::CommandExt::exec(&mut command);
        panic!("Failed to execute file: {}", error)
    }

    #[cfg(not(unix))]
    {
        let status = command
            .spawn()
            .expect("Failed to execute file")
            .wait()
            .expect("Failed to wait for process");
        std::process::exit(status.code().unwrap_or(0));
    }
}

pub fn run_backend<'a>(
    ir: &Module,
    backend: Backend,
    params: BackendParams<'a>,
) -> CompilerResult<()> {
    match backend {
        Backend::LLVM => unsafe {
            let context = llvm::core::LLVMContextCreate();
            let (llvm_module, stats) = backend::llvm::module(context, ir, params.show_backend_ir);
            if params.benchmarks {
                println!("{stats}");
            }

            if params.jit {
                println!("JITing {}", params.project_name);
                let ret_val = backend::llvm::run_jit(llvm_module);
                llvm::core::LLVMContextDispose(context);

                std::process::exit(ret_val);
            } else {
                let bitcode_emit_start_time = Instant::now();
                backend::llvm::output::emit_bitcoe(None, llvm_module, params.obj_file);
                if params.benchmarks {
                    println!(
                        "LLVM backend bitcode emit time: {:?}",
                        bitcode_emit_start_time.elapsed()
                    );
                }

                llvm::core::LLVMContextDispose(context);
            }
        },

        Backend::C => {
            let c_path = PathBuf::from(format!("./gingybuild/{}.c", params.project_name));
            let file = File::create(&c_path).unwrap();
            backend::c::emit(ir, file)?;
            let status = std::process::Command::new("gcc")
                .arg(c_path)
                .arg("-c")
                .args(["-o", params.obj_file])
                .status()
                .expect("Failed to execute gcc");

            if !status.success() {
                return Err(Error::Text(format!(
                    "Failed to compile C file: {}",
                    status.code().unwrap_or(0)
                )));
            }
        }
    }

    Ok(())
}
