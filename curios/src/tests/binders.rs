//! Binder scoping and plicity: a lambda's parameter list is a dependent telescope, and each of its binders is explicit, implicit or a witness slot.
//!
//! Scoping is that a later binder's annotation sees the earlier binders, exactly as a Π type's later domains do. Plicity is which of those binders a call site must supply, which elaboration inserts, and which a constructor pattern may mark. Irrefutable pattern shapes are `aggregates.rs`.

mod lambda_tests;
mod pattern_tests;
mod scope_tests;
