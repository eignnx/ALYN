use std::sync::atomic::{AtomicUsize, Ordering};

use derive_more::From;
use internment::Intern;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lbl {
    SubrStart(Intern<String>),
    Global(Intern<String>),
    ControlFlow(Intern<String>),
}

impl Lbl {
    pub fn render(&self) -> String {
        match self {
            Lbl::SubrStart(name) => format!("subr__{name}"),
            Lbl::Global(name) => format!("glbl__{name}"),
            Lbl::ControlFlow(name) => format!("local__{name}"),
        }
    }

    pub fn inner_ident(&self) -> Intern<String> {
        match self {
            Lbl::SubrStart(name) | Lbl::Global(name) | Lbl::ControlFlow(name) => name.clone(),
        }
    }
}

static LBL_ID: AtomicUsize = AtomicUsize::new(0);

pub fn reset_lbl_id() {
    LBL_ID.store(0, Ordering::SeqCst);
}

impl Lbl {
    pub fn fresh(base_name: impl AsRef<str>) -> Self {
        let id = LBL_ID.fetch_add(1, Ordering::SeqCst);
        Self::ControlFlow(Intern::new(format!("{}#{id}", base_name.as_ref())))
    }
}

impl std::fmt::Debug for Lbl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SubrStart(name) => write!(f, "subr<{name}>"),
            Self::Global(name) => write!(f, "glbl<{name}>"),
            Self::ControlFlow(name) => write!(f, "local<{name}>"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tmp(pub Intern<String>);

impl From<&str> for Tmp {
    fn from(name: &str) -> Self {
        Self(Intern::new(name.into()))
    }
}

static TMP_ID: AtomicUsize = AtomicUsize::new(0);

pub fn reset_tmp_id() {
    TMP_ID.store(0, Ordering::SeqCst);
}

impl Tmp {
    pub fn fresh(base_name: &str) -> Self {
        let id = TMP_ID.fetch_add(1, Ordering::SeqCst);
        Self(Intern::new(format!("{base_name}#{id}")))
    }
}

pub fn reset_name_ids() {
    reset_lbl_id();
    reset_tmp_id();
}
