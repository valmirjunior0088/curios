//! Compile-time partial evaluation — the transformation only this representation can perform.
//!
//! A fueled big-step, call-by-value interpreter runs every closed call the arena can finish and replaces it with its reified result — data as construction right-hand sides and interned constants, a reached tail-position effect as a residual call. This is the term-level shadow of type-level computation: a dependently-typed API's static argument (a format string, a literal certificate) was already evaluated by the elaborator, so the evaluator's success envelope is defined, not opportunistic — and Cont provably cannot recover it (closed recursion to a constant is not expressible over CPS and a physical alphabet).
//!
//! The engine is one shared core — the runtime [`value`] domain, the deterministic [`budget`], reification [`reify`], and the region [`copy`] — under two drivers: closed-term evaluation ([`closed`]) and literal-spine specialization ([`spine`]). A third consumer once ran the same interpreter to prove a combinator web's eager initialization inert so pruning could drop it; a recursive group is forced by need now and performs nothing when bound, so pruning decides by reachability alone and the proof is gone.

mod budget;
mod closed;
mod copy;
mod interpret;
mod reify;
mod spine;
mod value;

pub(crate) use closed::evaluate_closed_terms;
pub(crate) use spine::specialize_literal_spines;
