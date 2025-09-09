#[derive(Debug, Clone, Copy)]
pub enum IdentKind {
    /// The name of a statically-known subroutine.
    Subr,
    /// Represents subroutine parameters and local variables.
    Local,
    /// A statically-known global variable.
    Global,
    /// Fictitious ident kind for type checking return statements.
    SubrRet,
}
