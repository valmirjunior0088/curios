mod aggregates;
// PT2: BigInt tests disabled pending its own packed-Bits port.
// mod bigint;
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
mod operators;
mod runtime;
mod scheduler;
mod strings;
mod structs;

use {curios_rt::MockHost, std::time::Duration};

fn run(source: &str) -> Vec<u8> {
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    io.output().to_vec()
}
