//! What a contestant's build weighs: the one figure a reading can take without running anything.
//!
//! It is deterministic, which is what separates it from a [`crate::Timed`] row. The pins fix the compilers and the sources are the corpus's, so two readings of one commit weigh the same; a difference between two readings is therefore real, with no span to clear and no control to hold.
//!
//! It is **not** comparable between contestants, and the report never puts two side by side. The Curios contestant is a self-contained executable with the engine compiled into it, so its weight is the engine's with the program's on top, while `rust` is an ordinary native binary and `rust-wasm` a module. What the figure answers is whether one contestant's build grew between two readings.

/// What one contestant's build of one workload weighs, in bytes.
#[derive(Debug)]
pub struct Built {
    pub workload: &'static str,
    pub contestant: &'static str,
    pub bytes: u64,
}

/// A build's weight, written on one line where a reading records it.
pub const fn weighed(workload: &'static str, contestant: &'static str, bytes: u64) -> Built {
    Built {
        workload,
        contestant,
        bytes,
    }
}
