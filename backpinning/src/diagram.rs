use core::fmt;
use std::{collections::HashMap, sync::LazyLock};

use regalloc_common::{
    Instruction, Register,
    cfg::StmtIdx,
    ctrl_flow::{CtrlFlow, GetCtrlFlow},
    stg::Stg,
    stmt::Stmt,
};

use crate::{InstrExePhase, LiveRange, PrgPt, pad::PadWith};

pub struct DisplayLiveRanges<'a, R, I> {
    stmts: &'a [Stmt<I>],
    live_ranges: &'a HashMap<Stg<R>, Vec<LiveRange>>,

    /// The usize represents the length of the string representation of the Stg<R>.
    columns: Vec<Column<Stg<R>>>,

    diagram_width: u16,
    numcol_width: usize,
}

struct Column<T> {
    value: T,
    rendered: String,
}

impl<T: fmt::Debug> Column<T> {
    fn new(value: T) -> Self {
        let rendered = format!("{value:?}");
        Self { value, rendered }
    }

    fn width(&self) -> usize {
        self.rendered.len()
    }
}

impl<'a, R: Register, I: Instruction<Reg = R>> DisplayLiveRanges<'a, R, I> {
    pub fn new(stmts: &'a [Stmt<I>], live_ranges: &'a HashMap<Stg<R>, Vec<LiveRange>>) -> Self {
        let mut columns = live_ranges
            .keys()
            .map(|stg| Column::new(*stg))
            .collect::<Vec<_>>();

        columns.sort_unstable_by_key(|col| {
            live_ranges[&col.value]
                .iter()
                .map(|range| range.begin)
                .min()
                .unwrap()
        });

        let diagram_width = columns
            .iter()
            .map(|col| col.width() as u16 + 1)
            .sum::<u16>()
            + 2;
        let numcol_width = stmts.len().ilog10() as usize + 1;

        Self {
            stmts,
            live_ranges,
            columns,
            diagram_width,
            numcol_width,
        }
    }
}

impl<'a, R: Register, I: fmt::Debug + GetCtrlFlow> DisplayLiveRanges<'a, R, I> {
    fn draw_top_header(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "  ")?;
        for (iter, col) in self.columns.iter().enumerate() {
            if iter != 0 {
                write!(f, " ")?;
            }
            write!(f, "{:<width$}", col.rendered, width = col.width())?;
        }
        writeln!(f, "   ")?;

        let (left, fill, right) = CHAR_SET.border_top;
        write!(f, "{left}{fill}")?;
        for (iter, col) in self.columns.iter().enumerate() {
            if iter != 0 {
                write!(f, "{fill}")?;
            }
            let fill = fill
                .pad()
                .align('<')
                .width(col.width() as u16)
                .value(CHAR_SET.border_crossing_lifeline);
            write!(f, "{fill}")?;
        }
        writeln!(f, "{fill}{fill}{right}")?;
        Ok(())
    }

    fn draw_bottom_header(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (left, fill, right) = CHAR_SET.border_bottom;
        write!(f, "{left}{fill}")?;
        for (iter, col) in self.columns.iter().enumerate() {
            if iter != 0 {
                write!(f, "{fill}")?;
            }
            let fill = fill
                .pad()
                .width(col.width() as u16)
                .align('<')
                .value(CHAR_SET.border_crossing_lifeline);
            write!(f, "{fill}")?;
        }
        writeln!(f, "{fill}{fill}{right}")?;

        write!(f, "  ")?;
        for (iter, col) in self.columns.iter().enumerate() {
            if iter != 0 {
                write!(f, " ")?;
            }
            write!(f, "{:<width$}", col.rendered, width = col.width())?;
        }
        writeln!(f, "   ")?;
        Ok(())
    }

    fn draw_stmt_sidebar(
        &self,
        f: &mut fmt::Formatter,
        phase: InstrExePhase,
        i: StmtIdx,
        stmt: &Stmt<I>,
        draw_x_guide: bool,
    ) -> fmt::Result {
        let side = CHAR_SET.border_side;
        let side_crossing = CHAR_SET.border_side_crossing_x_guide;
        match phase {
            InstrExePhase::ReadArgs if draw_x_guide => {
                write!(
                    f,
                    "{side_crossing}─(r)─{i:0width$}: {stmt:?}",
                    width = self.numcol_width
                )?;
            }
            InstrExePhase::ReadArgs => {
                write!(
                    f,
                    "{side}     {i:0width$}: {stmt:?}",
                    width = self.numcol_width
                )?;
            }
            InstrExePhase::WriteBack => {
                write!(f, "{side_crossing}─(w)─┘")?;
            }
        }
        Ok(())
    }
}

impl<'a, R: Register, I: fmt::Debug + GetCtrlFlow> fmt::Display for DisplayLiveRanges<'a, R, I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.draw_top_header(f)?;

        let mut stmts_iter = self.stmts.iter().enumerate().peekable();
        while let Some((i, stmt)) = stmts_iter.next() {
            let i = StmtIdx::from(i);

            if let Stmt::Label(lbl) = stmt {
                let mut lbls = vec![lbl.to_string()];
                while let Some((_, Stmt::Label(lbl))) = stmts_iter.peek() {
                    lbls.push(lbl.to_string());
                    let _ = stmts_iter.next();
                }
                let lbls_joined = format!("[{}]", lbls.join("; "));

                let (left, fill, right) = CHAR_SET.bb_boundary;
                let fill = fill.pad().width(self.diagram_width).value(lbls_joined);

                writeln!(
                    f,
                    "{left}{fill}{right}     {i:0width$}",
                    width = self.numcol_width
                )?;
                continue;
            }

            for phase in InstrExePhase::PHASES {
                let pt = PrgPt::new(i, phase);

                if phase != InstrExePhase::ReadArgs
                    && self.columns.iter().all(|col| {
                        let ranges = &self.live_ranges[&col.value];
                        ranges.iter().all(|r| r.begin != pt && r.end != pt)
                    })
                {
                    continue;
                }

                write!(f, "{} ", CHAR_SET.border_side)?;

                let mut draw_x_guide = false;

                for col in &self.columns {
                    let ranges = &self.live_ranges[&col.value];
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

                    let pad = if draw_x_guide { CHAR_SET.x_guide } else { ' ' }
                        .pad()
                        .align('<')
                        .width(col.width() as u16 + 1)
                        .value(mark);

                    write!(f, "{pad}")?;
                }

                if draw_x_guide {
                    write!(f, "{}", CHAR_SET.x_guide)?;
                } else {
                    write!(f, " ")?;
                }

                self.draw_stmt_sidebar(f, phase, i, stmt, draw_x_guide)?;

                writeln!(f)?;

                // Draw basic block boundary
                if !matches!(stmt.ctrl_flow(), CtrlFlow::Advance)
                    && i != StmtIdx::from(self.stmts.len() - 1)
                    && !matches!(stmts_iter.peek(), Some((_, Stmt::Label(_))))
                {
                    // end of basic block
                    let (left, fill, right) = CHAR_SET.bb_boundary;
                    let fill = fill.pad().width(self.diagram_width);
                    writeln!(f, "{left}{fill}{right}")?;
                }
            }
        }

        self.draw_bottom_header(f)?;

        Ok(())
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
    if let Ok(val) = std::env::var("ALYN_LIVENESS_DIAGRAM_CHARSET")
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
