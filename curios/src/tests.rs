mod aggregates;
mod arena;
mod big_int;
mod big_nat;
mod binaryen;
mod binders;
mod codegen;
mod concepts;
mod erasure;
mod fmt;
mod foreign;
mod inference;
mod io;
mod map;
mod matching;
mod network;
mod numeric;
mod operators;
mod plicity;
mod positivity;
mod runtime;
mod scheduler;
mod soundness;
mod strings;
mod structs;
mod toml;
mod universes;

use curios_runtime::MockHost;

fn run(source: &str) -> Vec<u8> {
    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    io.output().to_vec()
}
