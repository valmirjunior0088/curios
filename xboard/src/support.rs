//! What the soundness board is written in and run by. The board itself is the binary's, one part of the judgment a module beside `main.rs`, and why it is built this way is the README's.
//!
//! This module reads in the order a run uses it:
//!
//! - [`Part`], [`Ticket`] and [`Witness`] are what is written on the board, a witness carrying the [`Proof`] of `False` it alleges.
//! - [`Expected`], written by [`refused!`](crate::refused), is the refusal a witness expects once its flaw is closed.
//! - [`Proof::answer`] puts a witness to the checkers at `False`, and an [`Answer`] is what they said, each [`Refusal`] the checker's own error.
//! - [`Report`] runs every ticket and says where each one stands, a [`Standing`].
//! - [`Probe`] puts one program that is on no ticket, and says what the checkers made of it.

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

mod probe;
pub use probe::*;
