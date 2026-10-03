//! Compile-time partial evaluation — the transformation only this representation can perform.
//!
//! A fueled big-step, call-by-value interpreter runs every closed call the arena can finish and replaces it with its reified result — data as construction right-hand sides and interned constants, a reached tail-position effect as a residual call. This is the term-level shadow of type-level computation: a dependently-typed API's static argument (a format string, a literal certificate) was already evaluated by the elaborator, so the evaluator's success envelope is defined, not opportunistic — and Cont provably cannot recover it (closed recursion to a constant is not expressible over CPS and a physical alphabet).
//!
//! The engine is one shared core — the runtime [`value`] domain, the deterministic [`budget`], reification [`reify`](mod@reify), and the region [`copy`] — under two drivers: closed-term evaluation ([`closed`]) and literal-spine specialization ([`spine`]).

mod budget;
use budget::*;

mod closed;
pub(crate) use closed::*;

mod copy;
use copy::*;

mod interpret;
use interpret::*;

mod reify;
use reify::*;

mod spine;
pub(crate) use spine::*;

mod value;
use value::*;
