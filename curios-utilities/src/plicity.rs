/// Whether a binder/argument participates in implicit-argument insertion, and how an omitted one is filled. The marks are part of a function type's identity: conversion refuses two function types whose plicity vectors differ before comparing a single domain, which is what lets a witness key read them off a type. Erasure never reads them — its keep/drop decisions are sort-driven.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived]
pub enum Plicity {
    Explicit,
    Implicit,
    /// A `use` binder/argument: filled by witness resolution (concept lookup) rather than unification when omitted at a call site.
    Witness,
}
