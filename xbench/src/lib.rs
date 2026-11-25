//! The bench's library: what a record is, the lines a measurement prints for one, and the harness that prints them — what a measurement beside the code it measures takes as a dev-dependency, and what the bench's runner and report read back.
//!
//! - [`Record`] is one sitting of one measurement, every figure's readings by group and everything the sitting was taken under, with [`Platform`] the host's account of where.
//! - [`Line`] is one reading as a measurement prints it, under [`PREFIX`].
//! - [`read`], [`read_elapsed`], [`turn`] and [`phase`] are the harness: a reading named, a time among them, variants interleaved, and a phase run under the recorder with its instructions counted.

mod platform;
pub use platform::{Platform, capture};

mod record;
pub use record::{Figure, Gated, Measure, Pin, Record};

mod lines;
pub use lines::{Line, PREFIX};

mod harness;
pub use harness::{Phase, phase, read, read_elapsed, read_sample, read_span, turn};
