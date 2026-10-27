//! The shared analyses, exercised through a real checker.
//!
//! These are an integration test rather than unit tests beside the analyses for a structural reason, not a filing one. Every analysis on that seam is written against `Env`/`Judge`, and `Judge`'s one method is `convert_at` — so testing one needs a *real* implementation of conversion. The only ones in the workspace are `curios-cert`'s `Kernel` and the elaborator's `Context`, and both sit above `curios-analysis`. A test-only `Env` would be a third checker, which is exactly what nobody should write.
//!
//! A dev-dependency cycle does not solve it either: a `#[cfg(test)]` module inside `curios-analysis` compiles that crate a second time, so `Kernel`'s `Judge` implementation would be against the *other* copy and the trait bound would not hold. An integration test links the real libraries, which is why these suites are here and not there.
//!
//! What stays beside the analyses is everything that needs no checker at all — the polarity lattice's own laws, the size-change matrix algebra, the readings through local definitions. Those are unit tests of pure functions and belong beside them.

mod test_support;

mod classification_tests;
mod forcing_tests;
mod positivity_tests;
mod walk_tests;
