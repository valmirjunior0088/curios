//! The `/std` effect vocabularies and how a `!` region sequences each: `Io` descriptions, `State` threading, `Result`'s early return, and the `Lift` edges between them.
//!
//! What each vocabulary *is* stays with its own section in these suites. What they share is the shape these tests pin: a region's monad is read from its expected type, never inferred from the action, and an embedding across monads exists only where a `/std/Monad/Lift` witness declares one.

mod lift_tests;
mod region_tests;
mod vocabulary_tests;
