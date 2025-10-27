#![allow(unused)]

//! Stack frame interface

use alyn_common::names::{Lbl, Tmp};

pub enum Escape {
    Escapes,
    Contained,
}

pub trait Frame {
    type Access;
    fn new_frame(name: Lbl, escape_params: &[Escape]) -> Self;
    fn name(&self) -> Lbl;
    /// AKA: `formals`
    fn params(&self) -> impl Iterator<Item = &Self::Access>;
    fn alloc_local(&mut self, escapes: Escape) -> Self::Access;
    // fn view_shift() -> Vec<Instr>;
}

#[cfg(test)]
mod test_frame {
    use super::*;

    #[derive(Debug)]
    struct MipsFrame {
        next_local_idx: u16,
        name: Lbl,
        params: Vec<MipsAccess>,
        locals: Vec<MipsAccess>,
    }

    #[derive(Debug, Clone)]
    enum MipsAccess {
        InFrame(i32),
        InReg(Tmp),
    }

    impl Frame for MipsFrame {
        type Access = MipsAccess;

        fn new_frame(name: Lbl, escape_params: &[Escape]) -> Self {
            let mut params = vec![];
            let mut i = -1;
            for esc in escape_params {
                match esc {
                    Escape::Escapes => {
                        params.push(MipsAccess::InFrame(i));
                        i -= 1;
                    }
                    Escape::Contained => {
                        params.push(MipsAccess::InReg(Tmp::fresh("contained_param")))
                    }
                }
            }

            MipsFrame {
                next_local_idx: 0,
                name,
                params,
                locals: vec![],
            }
        }

        fn name(&self) -> Lbl {
            self.name
        }

        fn params(&self) -> impl Iterator<Item = &Self::Access> {
            self.params.iter()
        }

        fn alloc_local(&mut self, escapes: Escape) -> Self::Access {
            match escapes {
                Escape::Escapes => {
                    let access = MipsAccess::InFrame(self.next_local_idx as i32);
                    self.locals.push(access.clone());
                    self.next_local_idx += 1;
                    access
                }
                Escape::Contained => {
                    let access = MipsAccess::InReg(Tmp::fresh("local"));
                    self.locals.push(access.clone());
                    access
                }
            }
        }
    }
}
