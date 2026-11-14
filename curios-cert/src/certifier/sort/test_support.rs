//! What the sort suites share: a certifier, a name and a binder.

use {
    crate::*,
    curios_analysis::test_support::*,
    curios_common::Qualifier,
    curios_core::{Free, Global},
};

pub fn certifier() -> Certifier<'static> {
    Certifier::new(100_000, SYNTAX)
}

pub fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

pub fn binder(index: u32, hint: &str) -> Free {
    Free::local(u64::from(index), Some(hint))
}
