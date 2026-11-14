//! What the sort suites share: a certifier and a name.

use {crate::*, curios_analysis::test_support::*, curios_common::Qualifier, curios_core::Global};

pub fn certifier() -> Certifier<'static> {
    Certifier::new(100_000, SYNTAX)
}

pub fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}
