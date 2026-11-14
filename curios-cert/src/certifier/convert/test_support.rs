//! Fixtures the certifier's conversion suites share: the certifier they start from, and the families they compare at.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across `convert`, and nothing outside it.

use {
    crate::{test_support::*, *},
    curios_analysis::test_support::*,
    curios_common::fill,
};

pub fn certifier() -> Certifier<'static> {
    Certifier::new(100_000, SYNTAX)
}

/// A nominal family `path` with no constructor at `sort` — the only way to obtain a base proposition, since the registry is what says a nominal type is one — declared in `certifier`, and the text of its type.
pub fn declare(certifier: &mut Certifier, path: &str, sort: &str) -> String {
    declared(
        certifier,
        &fill!("induct /${path}: pub ${sort}\nend", path, sort),
    );

    fill!("#{node = InductType} /${path}()", path)
}
