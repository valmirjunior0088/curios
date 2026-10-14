//! The engine as it stood, a parser a boxed closure that a grammar function builds wherever it is tried: what a grammar not yet written over [`Parser`](crate::Parser) is written over, and imports whole from here. Scaffolding, taken down once no grammar is.
//!
//! **The two meet at two points.** A boxed parser is a [`Parser`](crate::Parser), and a combinator here takes the crate's [`IntoParser`](crate::IntoParser), so a nonterminal written either way is an operand of the other; and [`Parser::of`] boxes one written the new way, for a function of the old shape to hand on.

mod parser;
pub use parser::*;

mod primitive;
pub use primitive::*;

mod span;
pub use span::*;

mod repeat;
pub use repeat::*;

mod memo;
pub use memo::*;

pub use crate::{Mark, ParserError, Recovered, is_blank, obtained, run_parser, shown, unseen};
