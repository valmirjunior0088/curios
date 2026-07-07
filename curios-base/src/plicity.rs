/// Whether a binder/argument participates in implicit-argument insertion.
/// An elaboration directive only: conversion ignores plicity entirely, so
/// erasing the marks yields exactly the unmarked system.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Plicity {
    Explicit,
    Implicit,
    /// A `use` binder/argument: filled by witness resolution (concept lookup)
    /// rather than unification when omitted at a call site. Like `Implicit`,
    /// purely an elaboration directive — conversion and erasure never read it.
    Witness,
}
