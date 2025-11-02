use alyn_common::names::Lbl;

#[derive(Debug, Clone)]
pub enum CtrlFlow {
    /// Exit the current subroutine. Either a return statement, a tail-call, or maybe
    /// `system_exit`.
    Exit,

    /// Just advance to the next instruction: `$PC <- $PC + 1`
    Advance,

    /// Unconditional jump to given label.
    Jump(Lbl),

    /// Jump to one of the given labels, like in a switch statement.
    Switch(Vec<Lbl>),

    /// Either fallthrough (which would be the same as `Advance`) or branch to the given label.
    Branch(Lbl),
}

pub trait GetCtrlFlow {
    fn ctrl_flow(&self) -> CtrlFlow;
}
