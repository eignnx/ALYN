use std::sync::atomic::{AtomicUsize, Ordering};

use derive_more::From;
use internment::Intern;

#[derive(Debug, Clone, Copy)]
pub struct Lbl(Intern<String>);

impl From<&str> for Lbl {
    fn from(name: &str) -> Self {
        Self(Intern::new(name.into()))
    }
}

pub static LBL_ID: AtomicUsize = AtomicUsize::new(0);

impl Lbl {
    pub fn fresh(base_name: &str) -> Self {
        let id = LBL_ID.fetch_add(1, Ordering::SeqCst);
        Self(Intern::new(format!("{base_name}#{id}")))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Tmp(Intern<String>);

impl From<&str> for Tmp {
    fn from(name: &str) -> Self {
        Self(Intern::new(name.into()))
    }
}

pub static TMP_ID: AtomicUsize = AtomicUsize::new(0);

impl Tmp {
    pub fn fresh(base_name: &str) -> Self {
        let id = TMP_ID.fetch_add(1, Ordering::SeqCst);
        Self(Intern::new(format!("{base_name}#{id}")))
    }
}
