use core::fmt;
use std::{collections::{BTreeSet, HashMap}, sync::LazyLock};

use regalloc_common::{
    cfg::{Cfg, StmtIdx}, ctrl_flow::GetCtrlFlow, stg::Stg, Instruction, Register
};

use crate::{InstrExePhase, LiveRange, PrgPt, pad::PadWith};

pub struct DisplayLiveRanges<'a, R, I> {
    cfg: &'a Cfg<'a, R, I>,
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
    pub fn new(cfg: &'a Cfg<'a, R, I>, live_ranges: &'a HashMap<Stg<R>, Vec<LiveRange>>) -> Self {
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
        let numcol_width = cfg.stmts().len().ilog10() as usize + 1;

        Self {
            cfg,
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
        phases_mentioned: &BTreeSet<InstrExePhase>,
        i: StmtIdx,
        instr: &I,
        draw_x_guide: bool,
    ) -> fmt::Result {
        let side = CHAR_SET.border_side;
        let side_crossing = CHAR_SET.border_side_crossing_x_guide;
        let ncolwidth = self.numcol_width;

        let anchored_phase_index = InstrExePhase::PHASES
            .iter()
            .position(|p| *p == ANCHORED_PHASE)
            .unwrap();

        let opt_offset = phases_mentioned
            .iter()
            .position(|p| *p == phase)
            .map(|idx| idx.abs_diff(anchored_phase_index));

        let Some(offset) = opt_offset else {
            match phase {
                InstrExePhase::JustBefore => {
                    write!(f, "(b)")?;
                }
                InstrExePhase::ReadArgs => {
                    write!(f, "(R)─{i:0ncolwidth$}: {instr:?}")?;
                }
                InstrExePhase::WriteBack => {
                    write!(f, "(W)")?;
                }
                InstrExePhase::JustAfter => {
                    write!(f, "(a)")?;
                }
            }
            //write!(f, "{side}     {i:0ncolwidth$}: {stmt:?}")?;
            return Ok(());
        };


        if draw_x_guide {
            write!(f, "{side_crossing}─")?;
            match phase {
                InstrExePhase::JustBefore => {
                    write!(f, "{:─<offset$}(b)─┐", "")?;
                }
                InstrExePhase::ReadArgs => {
                    write!(f, "{:─<offset$}(R)─{i:0ncolwidth$}: {instr:?}", "")?;
                }
                InstrExePhase::WriteBack => {
                    write!(f, "{:─<offset$}(W)─┘", "")?;
                }
                InstrExePhase::JustAfter => {
                    write!(f, "{:─<offset$}(a)─┘", "")?;
                }
            }
        } else {
            if phase == ANCHORED_PHASE {
                write!(f, "{side}     {i:0ncolwidth$}: {instr:?}")?;
            }
        }
        Ok(())
    }

    fn is_endpoint_on_row(&self, pt: PrgPt) -> bool {
        self.columns.iter().any(|col| {
            let ranges = &self.live_ranges[&col.value];
            ranges.iter().any(|r| r.begin == pt) || ranges.iter().any(|r| r.end == pt)
        })
    }

    fn cols_with_endpoint_on_row(&self, pt: PrgPt) -> impl Iterator<Item = &Column<Stg<R>>> {
        self.columns.iter().filter(move |col| {
            let ranges = &self.live_ranges[&col.value];
            ranges.iter().any(|r| r.begin == pt) || ranges.iter().any(|r| r.end == pt)
        })
    }

    fn endpoints_at_stmt_idx(&self, stmt_idx: StmtIdx) -> impl Iterator<Item = (&Column<Stg<R>>, InstrExePhase)> {
        self.columns.iter().flat_map(move |col| {
            InstrExePhase::PHASES.iter().filter_map(move |phase| {
                let ranges = &self.live_ranges[&col.value];
                let pt = PrgPt::new(stmt_idx, *phase);
                if ranges.iter().any(|r| r.begin == pt) || ranges.iter().any(|r| r.end == pt) {
                    Some((col, *phase))
                } else {
                    None
                }
            })
        })
    }

    /// Which phases have some endpoint on a given statement?
    fn marked_phases_at_stmt_idx(&self, stmt_idx: StmtIdx) -> impl Iterator<Item = InstrExePhase> {
        InstrExePhase::PHASES.iter().copied().filter(move |&phase| {
            self.columns.iter().any(move |col| {
                let ranges = &self.live_ranges[&col.value];
                let pt = PrgPt::new(stmt_idx, phase);
                ranges.iter().any(|r| r.begin == pt) || ranges.iter().any(|r| r.end == pt)
            })
        })
    }
}

const ANCHORED_PHASE: InstrExePhase = InstrExePhase::ReadArgs;

impl<'a, R: Register, I: fmt::Debug + GetCtrlFlow> fmt::Display for DisplayLiveRanges<'a, R, I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.draw_top_header(f)?;

        let mut first_iter = true;
        for bb_idx in self.cfg.bbs() {

            let lbls = self.cfg.bb_labels(bb_idx).map(|l| l.to_string()).collect::<Vec<_>>();
            if lbls.is_empty() {
                if !first_iter {
                    // Draw basic block boundary
                    let (left, fill, right) = CHAR_SET.bb_boundary;
                    let fill = fill.pad().width(self.diagram_width);
                    writeln!(f, "{left}{fill}{right} {bb_idx}")?;
                }
            } else {
                let lbls_joined = format!("[{}]", lbls.join("; "));
                let (left, fill, right) = CHAR_SET.bb_boundary;
                let padded = fill.pad().width(self.diagram_width).value(lbls_joined);
                writeln!(f, "{left}{padded}{right} {bb_idx}")?;
            }

            for (i, stmt) in self.cfg.bb_instrs_indexed(bb_idx) {
                let phases_mentioned = self.marked_phases_at_stmt_idx(i).collect::<BTreeSet<_>>();

                for phase in InstrExePhase::PHASES {
                    if !phases_mentioned.contains(&phase) && phase != ANCHORED_PHASE {
                        // Nothing interesting happens in this phase; skip.
                        continue;
                    }
                    let pt = PrgPt::new(i, phase);

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


                    self.draw_stmt_sidebar(f, phase, &phases_mentioned, i, stmt, draw_x_guide)?;

                    writeln!(f)?;

                }
            }
            first_iter = false;
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
