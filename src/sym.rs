#[derive(Debug, Clone, Copy)]
pub enum IdentKind {
    Subr,
    Param(u8),
    Local,
    Global,
    SubrRet,
}
