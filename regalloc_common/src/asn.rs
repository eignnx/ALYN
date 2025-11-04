#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SlotId(pub usize);

/// "Assignment"
/// The thing to which a temporary is assigned by the end of regalloc.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Asn<R> {
    /// The temporary has been allocated to a specific register.
    Reg(R),
    /// A location on the stack relative to the base pointer (aka frame pointer, activation record
    /// pointer).
    Slot(SlotId),
}

