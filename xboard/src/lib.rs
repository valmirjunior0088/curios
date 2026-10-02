//! The soundness board: every proof of `False` found in the Curios checkers, kept as something that is run. Why it is built this way is the README's.
//!
//! The board itself is the binary's, under `src/board/`. This library is what it is written in and run by, and reads in the order a run uses it:
//!
//! - [`Part`], [`Ticket`] and [`Witness`] are what is written on the board, a witness carrying the [`Proof`] of `False` it alleges.
//! - [`Expected`], written by [`refused!`], is the refusal a witness expects once its flaw is closed.
//! - [`Proof::answer`] puts a witness to the checkers at `False`, and an [`Answer`] is what they said, each [`Refusal`] the checker's own error.
//! - [`Report`] runs every ticket and says where each one stands, a [`Standing`].

mod date;
pub use date::*;

mod ticket;
pub use ticket::*;

mod expect;
pub use expect::*;

mod put;
pub use put::*;

mod report;
pub use report::*;
