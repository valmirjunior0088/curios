//! The size-change engine's own algebra: composition, join, idempotence, and the shape order.
//!
//! Pure functions of matrices and shapes, needing no checker. The probes that drive `group_totality` through a real `Driver` — including the `Walk::walk` position differential — live under `curios-analysis/tests/driven/`; its `main.rs` says why they are an integration test rather than a module here.

mod test_support;

mod algebra;
mod closure;
mod group;
