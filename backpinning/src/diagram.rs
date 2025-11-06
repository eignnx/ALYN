use core::fmt;
use std::{collections::HashMap, fmt::Write, marker::PhantomData};

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
    const BB_BOUNDARY: char;
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
    const BB_BOUNDARY: char = '-';
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

        let total_len = tmps.iter().map(|(_, len)| *len + 1).sum::<usize>() + 2;
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

        let mut stmts_iter = self.stmts.iter().enumerate().peekable();
        while let Some((i, stmt)) = stmts_iter.next() {

            if let Stmt::Label(lbl) = stmt {
                let mut lbls = vec![lbl.to_string()];
                while let Some((_, Stmt::Label(lbl))) = stmts_iter.peek() {
                    lbls.push(lbl.to_string());
                    let _ = stmts_iter.next();
                }
                write!(f, ":{:-^width$}:", format!("[ {}: ]", lbls.join("; ")), width=total_len)?;
                writeln!(f, "     {i:0width$}", width=numcol_width)?;
                continue;
            }

            let x = CharSet::DRAW_X_GUIDE;

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

                for (tmp, len) in &tmps {
                    let ranges = &self.live_ranges[tmp];
                    let live = ranges.iter().any(|r| r.contains(pt));

                    let mark = if ranges.iter().any(|r| r.begin == pt) {
                        draw_x_guide = true;
                        CharSet::LR_BEGIN
                    } else if ranges.iter().any(|r| r.end == pt) {
                        draw_x_guide = true;
                        CharSet::LR_END
                    } else if draw_x_guide {
                        if live {
                            CharSet::LR_LIVE_CROSSES_X_GUIDE
                        } else {
                            CharSet::LR_DEAD_CROSSES_X_GUIDE
                        }
                    } else {
                        if live {
                            CharSet::LR_LIVE
                        } else {
                            CharSet::LR_DEAD
                        }
                    };

                    let pad = if draw_x_guide { x } else { ' ' }
                        .pad()
                        .align('<')
                        .width(*len as u16 + 1)
                        .value(mark);

                    write!(f, "{pad}")?;
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

                if !matches!(stmt.ctrl_flow(), CtrlFlow::Advance) && i != self.stmts.len() - 1 && !matches!(stmts_iter.peek(), Some((_, Stmt::Label(_)))) {
                    // end of basic block
                    writeln!(f, ":{}:", CharSet::BB_BOUNDARY.pad().width(total_len as u16))?;
                }
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

pub trait PadWith: Sized {
    fn pad(self) -> PadOpts;
}

impl PadWith for char {
    fn pad(self) -> PadOpts {
        PadOpts::default()
            .alignment(fmt::Alignment::Center)
            .fill(self)
    }
}

#[derive(Clone, Copy, Default)]
pub struct PadOpts {
    options: fmt::FormattingOptions,
}

impl fmt::Display for PadOpts {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value(""))
    }
}

impl PadOpts {
    pub fn fill(mut self, fill: char) -> Self {
        self.options.fill(fill);
        self
    }

    pub fn width(mut self, width: u16) -> Self {
        self.options.width(Some(width));
        self
    }
    pub fn alignment(mut self, alignment: fmt::Alignment) -> Self {
        self.options.align(Some(alignment));
        self
    }

    pub fn align(mut self, align: char) -> Self {
        self.options.align(match align {
            '<' => Some(fmt::Alignment::Left),
            '>' => Some(fmt::Alignment::Right),
            '^' => Some(fmt::Alignment::Center),
            ' ' => None,
            _ => panic!("invalid alignment character '{align}'"),
        });
        self
    }

    pub fn value<T>(self, value: T) -> Pad<T> {
        Pad {
            value,
            options: self.options,
        }
    }
}

pub struct Pad<T> {
    value: T,
    options: fmt::FormattingOptions,
}

impl<T: fmt::Debug> fmt::Debug for Pad<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let rendered = format!("{:?}", self.value);
        let mut f = self.options.create_formatter(f);
        f.pad(rendered.as_ref())?;
        Ok(())
    }
}

impl<T: fmt::Display> fmt::Display for Pad<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let rendered = format!("{}", self.value);
        let mut f = self.options.create_formatter(f);
        f.pad(rendered.as_ref())?;
        Ok(())
    }
}
