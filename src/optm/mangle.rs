//! Name mangling for the optimization passes.
//!
//! Every name a pass mints follows one grammar:
//!
//! ```text
//! <base> "@" <tag> ( "#" <item> )*
//! ```
//!
//! - `@` marks a pass-minted name. It cannot appear in a lowerer-minted name
//!   (`v0`, `b1`, `c2`, `main`), so any `@` says "minted by the pass named by
//!   `<tag>`" — and a mangled name can never collide with a lowerer one.
//! - `#` introduces a disambiguating item: a counter (`v@eval#3`, `v0@f#2`,
//!   `lit@bin#0`, `p@cap#1`) or a specialization key entry (`f@spec#0=c`).
//!
//! Passes never `format!` a name inline; every constructor lives here so the
//! namespace is auditable in one place.

use {super::*, std::fmt::Display};

/// The function a closure is lifted to ([`closure_lifting`](super::closure_lifting)).
pub fn lifted(clsr: &ClsrName) -> FuncName {
    FuncName::from(format!("{clsr}@lifted"))
}

/// The freshening suffix for a single-site inline
/// ([`function_inlining`](super::function_inlining) Tier 1).
pub fn inline_suffix(callee: &FuncName) -> String {
    format!("@{callee}")
}

/// The per-site freshening suffix for a multi-site inline
/// ([`function_inlining`](super::function_inlining) Tier 2).
pub fn inline_site_suffix(callee: &FuncName, site: usize) -> String {
    format!("@{callee}#{site}")
}

/// The label of a specialized clone: its base plus one `#position=shape` entry
/// per resolved candidate ([`specialize_calls`](super::specialize_calls)). A pure
/// function of the key, so equal keys map to one clone.
pub fn specialized_label<S: Display>(base: impl Display, resolved: &[(usize, S)]) -> String {
    let mut label = format!("{base}@spec");

    for (position, shape) in resolved {
        label.push_str(&format!("#{position}={shape}"));
    }

    label
}

/// A callee-bound value name freshened with an inline suffix
/// ([`function_inlining`](super::function_inlining)).
pub fn suffixed_value(name: &ValueName, suffix: &str) -> ValueName {
    ValueName::from(format!("{name}{suffix}"))
}

/// The block-name mirror of [`suffixed_value`].
pub fn suffixed_block(name: &BlockName, suffix: &str) -> BlockName {
    BlockName::from(format!("{name}{suffix}"))
}

/// A specialized clone's threaded capture parameter
/// ([`specialize_calls`](super::specialize_calls)).
pub fn capture_param(param: &ValueName, index: usize) -> ValueName {
    ValueName::from(format!("{param}@cap#{index}"))
}

/// An interned module const ([`hoist_literals`](super::hoist_literals)).
pub fn hoisted_const(kind: &str, index: usize) -> ValueName {
    ValueName::from(format!("lit@{kind}#{index}"))
}

/// A materialised interpretation result
/// ([`evaluate_pure_calls`](super::evaluate_pure_calls)).
pub fn eval_result(index: usize) -> ValueName {
    ValueName::from(format!("v@eval#{index}"))
}

