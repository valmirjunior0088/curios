mod aggregates;
mod arena;
mod big_int;
mod big_nat;
mod binaryen;
mod codegen;
mod concepts;
mod erasure;
mod foreign;
mod inference;
mod io;
mod map;
mod matching;
mod network;
mod numeric;
mod operators;
mod runtime;
mod scheduler;
mod strings;
mod structs;
mod toml;

use {curios_runtime::MockHost, std::time::Duration};

fn run(source: &str) -> Vec<u8> {
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    io.output().to_vec()
}
