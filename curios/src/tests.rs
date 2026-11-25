mod aggregates;
mod algebra;
mod alignment;
mod big_num;
mod binders;
mod bounds;
mod characters;
mod cli;

mod codegen;
// Only the fixpoint probe reads the driver the census keeps.
use codegen::*;

mod concepts;
mod core_text;
mod derive;
mod document;
mod effects;
mod erasure;
mod fmt;

mod growth;
// Only the growth measurement writes the programs at the sizes it counts.
use growth::*;

mod harness;
mod host;
mod inference;
mod instrumented;
mod laws;
mod map;
mod matching;
mod numeric;
mod operators;
mod packages;
mod readme;
mod recovery;
mod recursion;
mod reduction;
mod reference;
mod reports;
mod respelling;
mod rules;
mod runaways;
// Only the runaway measurement builds the programs the fixtures answer.
use runaways::*;

mod concurrent;
mod runtime;
mod strings;
mod structs;
mod towers;
// Only the tower measurement builds the shapes the fixtures compile.
use towers::*;

mod tui;
// Only the two profile suites replay the listing.
use tui::*;

mod unfolding;
use unfolding::*;

mod universes;
mod wasm_conformance;
