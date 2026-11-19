//! What a function of a compiled program was made of: the function of a declaration's text its code came from.
//!
//! **A fact of where code came from, kept by every copy.** Erasure stamps it where it mints a function inside a declaration's binding, counting the declaration's functions in the order it mints them; every stage after it keeps it on each copy it makes, as it keeps a function's role and its termination verdict, and moves it with the code a pass moves: the loop contification makes of a function, the function a frame bound lifts a continuation into, the continuations a lowering makes while lowering a function's body. Code made of no declaration's text — the entry's, the runtime's, a continuation a pass makes — carries none. Nothing prints it and nothing emits it: a function's hint is what a dump shows of it, and display only, so what a compiled program holds of a declaration is read off the lineage and never off a spelling.
//!
//! **Why a fact of its own.** A hint cannot carry it: a function a local `let` binds takes its binder's bare name, a derived suffix (`owner/n`, a crossing's `/step`) reads as a path segment, and a heap twin is hinted afresh, so a reading of hints makes a behaviour of a spelling. The linker's map of each function to the names its item declares holds an item, not the declaration of a group a function was written in, and only the functions of an item's evaluation. A source location on every node, LLVM's `inlinedAt`, would also place straight-line code inlined into a caller, but every rewrite of every pass would carry it and nothing asks it yet; a trap naming its source or a profile attributing inlined code would be a capability of its own.

use crate::Qualifier;

/// The function of a declaration's text a piece of compiled code was made of: the declaration, by the path it is declared at, and which of the functions its erasure minted, from the first.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived]
pub struct Lineage {
    pub declaration: Qualifier,
    pub function: u32,
}
