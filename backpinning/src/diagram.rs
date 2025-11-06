use core::fmt;
use std::{cell::LazyCell, collections::HashMap, fmt::Write, marker::PhantomData, sync::LazyLock};

use alyn_common::names::Tmp;
use regalloc_common::{
    ctrl_flow::{CtrlFlow, GetCtrlFlow},
    stmt::Stmt,
};

use crate::{InstrExePhase, LiveRange, PrgPt};

pub struct DisplayLiveRanges<'a, I> {
    stmts: &'a [Stmt<I>],
    live_ranges: &'a HashMap<Tmp, Vec<LiveRange>>,
}

impl<'a, I> DisplayLiveRanges<'a, I> {
    pub fn new(stmts: &'a [Stmt<I>], live_ranges: &'a HashMap<Tmp, Vec<LiveRange>>) -> Self {
        Self { stmts, live_ranges }
    }
}

pub struct DiagramCharSet {
    live_begin: char,
    live_end: char,
    live: char,
    dead: char,
    x_guide: char,
    live_crossing_x_guide: char,
    dead_crossing_x_guide: char,
    bb_boundary: (char, char, char),
    border_top: (char, char, char),
    border_bottom: (char, char, char),
    border_crossing_lifeline: char,
    border_side: char,
    border_side_crossing_x_guide: char,
}

static CHAR_SET: LazyLock<DiagramCharSet> = LazyLock::new(|| {
    if let Ok(val) = std::env::var("CHARSET")
        && val == "ascii"
    {
        DiagramCharSet {
            live_begin: '#',
            live_end: '#',
            live: '#',
            dead: '\'',
            x_guide: '.',
            live_crossing_x_guide: '#',
            dead_crossing_x_guide: '!',
            bb_boundary: (':', '-', ':'),
            border_top: ('.', '-', '.'),
            border_bottom: ('\'', '-', '\''),
            border_crossing_lifeline: '+',
            border_side: '|',
            border_side_crossing_x_guide: '|',
        }
    } else {
        DiagramCharSet {
            live_begin: '▄',
            live_end: '▀',
            live: '█',
            dead: '╵',
            //dead: '┊',
            //dead: '·',
            x_guide: '╴',
            //x_guide: '┄',
            live_crossing_x_guide: '█',
            dead_crossing_x_guide: '┼',
            //dead_crossing_x_guide: '·',
            //dead_crossing_x_guide: '┄',
            bb_boundary: ('╞', '═', '╡'),
            border_top: ('╒', '═', '╕'),
            border_bottom: ('╘', '═', '╛'),
            border_crossing_lifeline: '╪',
            border_side: '│',
            border_side_crossing_x_guide: '┼',
        }
    }
});

impl<'a, I: fmt::Debug + GetCtrlFlow> fmt::Display for DisplayLiveRanges<'a, I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut tmps = self
            .live_ranges
            .keys()
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
            write!(f, "{:<width$}", format!("{tmp:?}"), width = len)?;
        }
        writeln!(f, "   ")?;

        let (left, fill, right) = CHAR_SET.border_top;
        write!(f, "{left}{fill}")?;
        for (iter, (_, len)) in tmps.iter().enumerate() {
            if iter != 0 {
                write!(f, "{fill}")?;
            }
            let fill = fill
                .pad()
                .align('<')
                .width(*len as u16)
                .value(CHAR_SET.border_crossing_lifeline);
            write!(f, "{fill}")?;
        }
        writeln!(f, "{fill}{fill}{right}")?;

        let mut stmts_iter = self.stmts.iter().enumerate().peekable();
        while let Some((i, stmt)) = stmts_iter.next() {
            if let Stmt::Label(lbl) = stmt {
                let mut lbls = vec![lbl.to_string()];
                while let Some((_, Stmt::Label(lbl))) = stmts_iter.peek() {
                    lbls.push(lbl.to_string());
                    let _ = stmts_iter.next();
                }
                let lbls_joined = format!("[{}]", lbls.join("; "));

                let (left, fill, right) = CHAR_SET.bb_boundary;
                let fill = fill.pad().width(total_len as u16).value(lbls_joined);

                writeln!(
                    f,
                    "{left}{fill}{right}     {i:0width$}",
                    width = numcol_width
                )?;
                continue;
            }

            let x = CHAR_SET.x_guide;

            for phase in InstrExePhase::PHASES {
                let pt = PrgPt::new(i, phase);

                if phase != InstrExePhase::ReadArgs
                    && tmps.iter().all(|(tmp, _)| {
                        let ranges = &self.live_ranges[tmp];
                        ranges.iter().all(|r| r.begin != pt && r.end != pt)
                    })
                {
                    continue;
                }

                write!(f, "{} ", CHAR_SET.border_side)?;

                let mut draw_x_guide = false;

                for (tmp, len) in &tmps {
                    let ranges = &self.live_ranges[tmp];
                    let live = ranges.iter().any(|r| r.contains(pt));

                    let mark = if ranges.iter().any(|r| r.begin == pt) {
                        draw_x_guide = true;
                        CHAR_SET.live_begin
                    } else if ranges.iter().any(|r| r.end == pt) {
                        draw_x_guide = true;
                        CHAR_SET.live_end
                    } else {
                        match (draw_x_guide, live) {
                            (true, true) => CHAR_SET.live_crossing_x_guide,
                            (true, false) => CHAR_SET.dead_crossing_x_guide,
                            (false, true) => CHAR_SET.live,
                            (false, false) => CHAR_SET.dead,
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

                let side = CHAR_SET.border_side;
                let side_crossing = CHAR_SET.border_side_crossing_x_guide;
                match phase {
                    InstrExePhase::ReadArgs if draw_x_guide => {
                        write!(
                            f,
                            "{side_crossing}─(r)─{i:0width$}: {stmt:?}",
                            width = numcol_width
                        )?;
                    }
                    InstrExePhase::ReadArgs => {
                        write!(f, "{side}     {i:0width$}: {stmt:?}", width = numcol_width)?;
                    }
                    InstrExePhase::WriteBack => {
                        write!(f, "{side_crossing}─(w)─┘")?;
                    }
                }

                writeln!(f)?;

                if !matches!(stmt.ctrl_flow(), CtrlFlow::Advance)
                    && i != self.stmts.len() - 1
                    && !matches!(stmts_iter.peek(), Some((_, Stmt::Label(_))))
                {
                    // end of basic block
                    let (left, fill, right) = CHAR_SET.bb_boundary;
                    let fill = fill.pad().width(total_len as u16);
                    writeln!(f, "{left}{fill}{right}")?;
                }
            }
        }

        let (left, fill, right) = CHAR_SET.border_bottom;
        write!(f, "{left}{fill}")?;
        for (iter, (_, len)) in tmps.iter().enumerate() {
            if iter != 0 {
                write!(f, "{fill}")?;
            }
            let fill = fill
                .pad()
                .width(*len as u16)
                .align('<')
                .value(CHAR_SET.border_crossing_lifeline);
            write!(f, "{fill}")?;
        }
        writeln!(f, "{fill}{fill}{right}")?;

        write!(f, "  ")?;
        for (iter, (tmp, len)) in tmps.iter().enumerate() {
            if iter != 0 {
                write!(f, " ")?;
            }
            write!(f, "{:<width$}", format!("{tmp:?}"), width = len)?;
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
