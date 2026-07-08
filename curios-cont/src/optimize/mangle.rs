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

/// The function a closure is lifted to ([`closure_lifting`](super::closure_lifting)):
/// a pure function of the closure's name, so every call site and every lifting
/// pass agree on one target without sharing state.
pub(super) fn lifted(clsr: &ClsrName) -> FuncName {
    FuncName::from(format!("{clsr}@lifted"))
}

/// The per-site freshening suffix for an inline splice
/// ([`function_inlining`](super::function_inlining)): a per-callee site number
/// keeps every splice's bound names distinct across all inline passes.
pub(super) fn inline_site_suffix(callee: &FuncName, site: usize) -> String {
    format!("@{callee}#{site}")
}

/// The label of a specialized clone: its base plus one `#position=shape` entry
/// per resolved candidate ([`specialize_calls`](super::specialize_calls)). A pure
/// function of the key, so equal keys map to one clone.
pub(super) fn specialized_label<S: Display>(base: impl Display, resolved: &[(usize, S)]) -> String {
    let mut label = format!("{base}@spec");

    for (position, shape) in resolved {
        label.push_str(&format!("#{position}={shape}"));
    }

    label
}

/// A callee-bound value name freshened with an inline suffix
/// ([`function_inlining`](super::function_inlining)).
pub(super) fn suffixed_value(name: &ValueName, suffix: &str) -> ValueName {
    ValueName::from(format!("{name}{suffix}"))
}

/// The block-name mirror of [`suffixed_value`].
pub(super) fn suffixed_block(name: &BlockName, suffix: &str) -> BlockName {
    BlockName::from(format!("{name}{suffix}"))
}

/// A specialized clone's threaded capture parameter
/// ([`specialize_calls`](super::specialize_calls)).
pub(super) fn capture_param(param: &ValueName, index: usize) -> ValueName {
    ValueName::from(format!("{param}@cap#{index}"))
}

/// A specialized clone's rebuilt tuple element, when the baked shape is a witness
/// dictionary ([`specialize_calls`](super::specialize_calls)). Distinct from
/// [`capture_param`]'s `@cap#`, so a tuple element and a closure capture minted from
/// the same base never collide.
pub(super) fn element_param(param: &ValueName, index: usize) -> ValueName {
    ValueName::from(format!("{param}@elem#{index}"))
}

/// The loop header block a converted self-tail-recursive function jumps to
/// ([`tail_recursion`](super::tail_recursion)). One per body: conversion
/// consumes every self-call, so a body is never converted twice.
pub(super) fn loop_header() -> BlockName {
    BlockName::from("b@loop")
}

/// The fresh outer parameter standing in for a converted function's original
/// parameter, which the loop header block takes over
/// ([`tail_recursion`](super::tail_recursion)).
pub(super) fn loop_param(param: &ValueName) -> ValueName {
    ValueName::from(format!("{param}@loop"))
}

/// The freshening suffix for a decided join-block clone
/// ([`tag_threading`](super::tag_threading)). `index` is a per-body counter,
/// so two splices in the same body can never collide.
pub(super) fn thread_suffix(index: usize) -> String {
    format!("@thread#{index}")
}

/// An interned module const ([`hoist_literals`](super::hoist_literals)): `kind`
/// groups related literals under one counter, and `index` is probed upward
/// until the candidate doesn't collide with an already-taken name.
pub(super) fn hoisted_const(kind: &str, index: usize) -> ValueName {
    ValueName::from(format!("lit@{kind}#{index}"))
}

/// A materialised interpretation result
/// ([`evaluate_pure_calls`](super::evaluate_pure_calls)): named from a fresh
/// per-pass counter, so each constant folded out of a pure call gets its own
/// binding even when several fold in the same pass.
pub(super) fn eval_result(index: usize) -> ValueName {
    ValueName::from(format!("v@eval#{index}"))
}

/// An offset adjustment minted when forwarding an aggregate access through a
/// slice ([`slice_forwarding`](super::slice_forwarding)): the `start + index`
/// re-basing that lets a `get`/`slice` read the underlying buffer directly.
pub(super) fn slice_offset(index: usize) -> ValueName {
    ValueName::from(format!("v@slice#{index}"))
}
