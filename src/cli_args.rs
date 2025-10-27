use std::path::PathBuf;

use clap::{Parser, ValueEnum};

#[derive(Parser, Debug)]
#[command(version, about, long_about=None)]
pub struct CliArgs {
    /// The path to the source file to be compiled.
    #[arg(value_name = "FILE")]
    pub src_file: PathBuf,

    /// The target architecture in which assembly will be produced.
    #[arg(short, long, default_value = "lark")]
    pub target_arch: crate::backends::BackendArch,
}
