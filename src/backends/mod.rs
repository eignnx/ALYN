use std::path::Path;

use clap::ValueEnum;

use crate::{Compiler, instr_sel::Select};

pub mod avr;
pub mod hobby;
pub mod i8086;

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
