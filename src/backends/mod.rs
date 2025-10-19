use std::path::Path;

use clap::ValueEnum;

use crate::{instr_sel::Select, Compiler};

pub mod avr;
pub mod i8086;
pub mod hobby;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum BackendArch {
    Lark,
    Sprind,
}

impl BackendArch {
    pub fn instantiate<'out>(self) -> Box<dyn Backend + 'out> {
        match self {
            BackendArch::Lark => Box::new(hobby::lark::LarkInstrSel::new()),
            BackendArch::Sprind => Box::new(hobby::sprind::SprindInstrSel::new()),
        }
    }
}

pub trait Backend {
    fn compile(&mut self, fname: &Path, src: &str);
}

impl Backend for hobby::lark::LarkInstrSel {
    fn compile(&mut self, fname: &Path, src: &str) {
        Compiler::new(self).compile(fname, src);
    }
}

impl Backend for hobby::sprind::SprindInstrSel {
    fn compile(&mut self, fname: &Path, src: &str) {
        Compiler::new(self).compile(fname, src);
    }
}



