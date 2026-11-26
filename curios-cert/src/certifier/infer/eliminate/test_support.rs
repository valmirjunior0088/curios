//! The families, goals and opaque type formers the elimination suites eliminate over, each written in Curios/Core.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across `eliminate`, and nothing outside it.

use {
    crate::{test_support::*, *},
    curios_analysis::test_support::*,
    curios_common::{Qualifier, fill},
    curios_core::{Global, UniverseContext},
};

pub fn certifier() -> Certifier<'static> {
    Certifier::new(100_000, SYNTAX)
}

/// `z() : (0)`, the case at zero.
pub const ZERO: &str = "z(): (0)";

/// `s(p : Nat) : (p + 1)`, the case at a successor, in the successor-floor form reduction keeps.
pub const SUCCESSOR: &str = "s(p: #NatType): (#Nat(Succ(1, p)))";

/// The family `path` at `sort` over the indices `indices`, a telescope (`i: #NatType`) or none, with `cases`, each written `tag(payload): (targets)`, declared in `certifier`. A binder named in more than one target is the non-linear target `Eq`'s `refl(@z) : (z, z)` has.
pub fn declare(certifier: &mut Certifier, path: &str, indices: &str, sort: &str, cases: &[&str]) {
    let arity = match indices.is_empty() {
        true => String::new(),
        false => fill!("(${indices}) -> ", indices),
    };
    let cases = cases
        .iter()
        .map(|case| fill!("\n| ${case}", case))
        .collect::<String>();

    declared(
        certifier,
        &fill!(
            "induct /${path}: ${arity}pub ${sort}${cases}\nend",
            path,
            arity,
            sort,
            cases
        ),
    );
}

/// `path`'s type at `indices`, a family with no parameter: `#{node = InductType} /path()(indices)`, or `/path()` where it takes no index.
pub fn family_at(path: &str, indices: &str) -> String {
    match indices.is_empty() {
        true => fill!("#{node = InductType} /${path}()", path),
        false => fill!("#{node = InductType} /${path}()(${indices})", path, indices),
    }
}

/// `#{goal = ${goal}} match subject | arms end`, the elimination of a scrutinee `subject`: its goal stated at the scrutinee's own indices, each arm `tag(binders) => body`.
pub fn eliminating(goal: &str, arms: &[&str]) -> String {
    let arms = arms
        .iter()
        .map(|arm| fill!(" | ${arm}", arm))
        .collect::<String>();

    fill!("#{goal = ${goal}} match subject${arms} end", goal, arms)
}

/// A name `path` at `type_` with no body, declared in `certifier` as a judgment declares one before its body: an opaque type former, for observing which instance a term checks at.
pub fn opaque(certifier: &mut Certifier, path: &str, type_: &str) {
    certifier
        .declare(
            &Global::Authored(Qualifier::from([path])),
            &term(type_),
            &UniverseContext::default(),
        )
        .expect("a name is declared once");
}

/// An opaque `P : (n : Nat, x : ${family}(n)) -> Type`, for observing which *scrutinee* a term checks at as well as which index: a catch-all refines no index, so a former over the index alone cannot tell the scrutinee instances apart.
pub fn scrutinee_family(certifier: &mut Certifier, family: &str) {
    opaque(
        certifier,
        "P",
        &fill!(
            "(n: #NatType, x: ${family}) -> Type",
            family = family_at(family, "n")
        ),
    );
}

/// The name a declaration under `path` has.
pub fn named(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}
