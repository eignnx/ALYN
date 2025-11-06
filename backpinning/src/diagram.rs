use core::fmt;
use std::{collections::HashMap, marker::PhantomData};

use alyn_common::names::Tmp;
use regalloc_common::{ctrl_flow::{CtrlFlow, GetCtrlFlow}, stmt::Stmt};

use crate::{InstrExePhase, LiveRange, PrgPt};

pub struct DisplayLiveRanges<'a, I, CharSet> {
    stmts: &'a [Stmt<I>],
    live_ranges: &'a HashMap<Tmp, Vec<LiveRange>>,
    _char_set: PhantomData<CharSet>,
}

impl<'a, I, CharSet: DiagramCharSet> DisplayLiveRanges<'a, I, CharSet> {
    pub fn new(stmts: &'a [Stmt<I>], live_ranges: &'a HashMap<Tmp, Vec<LiveRange>>) -> Self {
        Self { stmts, live_ranges, _char_set: PhantomData }
    }
}

pub trait DiagramCharSet {
    const LR_BEGIN: char;
    const LR_END: char;
    const LR_LIVE: char;
    const LR_DEAD: char;
    const DRAW_X_GUIDE: char;
    const LR_LIVE_CROSSES_X_GUIDE: char;
    const LR_DEAD_CROSSES_X_GUIDE: char;
}

pub struct AsciiCharSet;

impl DiagramCharSet for AsciiCharSet {
    const LR_BEGIN: char = '#';
    const LR_END: char = '#';
    const LR_LIVE: char = '#';
    const LR_DEAD: char = '\'';
    const DRAW_X_GUIDE: char = '.';
    const LR_LIVE_CROSSES_X_GUIDE: char = '#';
    const LR_DEAD_CROSSES_X_GUIDE: char = '!';
}

impl<'a, I: fmt::Debug + GetCtrlFlow, CharSet: DiagramCharSet> fmt::Display for DisplayLiveRanges<'a, I, CharSet> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut tmps = self.live_ranges.keys()
            .map(|t| (*t, format!("{t:?}").len()))
            .collect::<Vec<_>>();
        tmps.sort_unstable_by_key(|(tmp, _)| {
            self.live_ranges[tmp]
                .iter()
                .map(|range| range.begin)
                .min()
                .unwrap()
        });

        let numcol_width = self.stmts.len().ilog10() as usize + 1;

        write!(f, "  ")?;
        for (iter, (tmp, len)) in tmps.iter().enumerate() {
            if iter != 0 {
                write!(f, " ")?;
            }
            write!(f, "{:<width$}", format!("{tmp:?}"), width=len)?;
        }
        writeln!(f, "   ")?;

        write!(f, ".-")?;
        for (iter, (_, len)) in tmps.iter().enumerate() {
            if iter != 0 {
                write!(f, "-")?;
            }
            write!(f, "{:-<width$}", "+", width=len)?;
        }
        writeln!(f, "--.")?;

        for (i, stmt) in self.stmts.iter().enumerate() {
            for phase in InstrExePhase::PHASES {
                let pt = PrgPt::new(i, phase);

                if phase != InstrExePhase::ReadArgs && tmps.iter().all(|(tmp, _)| {
                    let ranges = &self.live_ranges[tmp];
                    ranges.iter().all(|r| r.begin != pt && r.end != pt)
                }) {
                    continue;
                }

                write!(f, "| ")?;

                let mut draw_x_guide = false;

                // NOTE: can't use "dynamic" fill character, so gotta replace all `.`s with
                // your chosen character.
                let x = CharSet::DRAW_X_GUIDE;

                for (tmp, len) in &tmps {
                    let ranges = &self.live_ranges[tmp];
                    let contained = ranges.iter().any(|r| r.contains(pt));

                    if ranges.iter().any(|r| r.begin == pt) {
                        draw_x_guide = true;
                        write!(f, "{:.<width$}{x}", CharSet::LR_BEGIN, width=len)?;
                    } else if ranges.iter().any(|r| r.end == pt) {
                        draw_x_guide = true;
                        write!(f, "{:.<width$}{x}", CharSet::LR_END, width=len)?;
                    } else if draw_x_guide {
                        if contained {
                            write!(f, "{:.<width$}{x}", CharSet::LR_LIVE_CROSSES_X_GUIDE, width=len)?;
                        } else {
                            write!(f, "{:.<width$}{x}", CharSet::LR_DEAD_CROSSES_X_GUIDE, width=len)?;
                        }
                    } else {
                        if contained {
                            write!(f, "{: <width$} ", CharSet::LR_LIVE, width=len)?;
                        } else {
                            write!(f, "{: <width$} ", CharSet::LR_DEAD, width=len)?;
                        }
                    }
                }

                if draw_x_guide {
                    write!(f, "{x}")?;
                } else {
                    write!(f, " ")?;
                }

                match phase {
                    InstrExePhase::ReadArgs if draw_x_guide => {
                        write!(f, "|─(r)─{i:0width$}: {stmt:?}", width=numcol_width)?;
                    }
                    InstrExePhase::ReadArgs => {
                        write!(f, "|     {i:0width$}: {stmt:?}", width=numcol_width)?;
                    }
                    InstrExePhase::WriteBack => {
                        write!(f, "|─(w)─┘")?;
                    }
                }

                writeln!(f)?;
            }
            if !matches!(stmt.ctrl_flow(), CtrlFlow::Advance) && i != self.stmts.len() - 1 {
                write!(f, "+=")?;
                for (iter, (_, len)) in tmps.iter().enumerate() {
                    if iter != 0 {
                        write!(f, "=")?;
                    }
                    write!(f, "{:=<width$}", "+", width=len)?;
                }
                writeln!(f, "==+")?;
            }
        }

        write!(f, "'-")?;
        for (iter, (_, len)) in tmps.iter().enumerate() {
            if iter != 0 {
                write!(f, "-")?;
            }
            write!(f, "{:-<width$}", "+", width=len)?;
        }
        writeln!(f, "--'")?;

        write!(f, "  ")?;
        for (iter, (tmp, len)) in tmps.iter().enumerate() {
            if iter != 0 {
                write!(f, " ")?;
            }
            write!(f, "{:<width$}", format!("{tmp:?}"), width=len)?;
        }
        writeln!(f, "   ")?;

        Ok(())
    }
}
