# Findings

- **Three instrumented measurements are to be retaken under Rust 1.99.1.** `curios/src/tests/instrumented/coordination.rs`, `fixpoint.rs` and `host_boundary.rs` state figures taken under Rust 1.95.0, which `rust-toolchain.toml` has moved past. Fix: each retaken on a quiet machine by the protocol it states, its figures and conditions rewritten. Check: the measurement's own command. Size: one commit.
