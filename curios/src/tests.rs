use {curios_rt::MockHost, std::time::Duration};

fn run(source: &str) -> Vec<u8> {
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    io.output().to_vec()
}

mod aggregates;
mod bignat;
mod binaryen;
mod bridge;
mod codegen;
mod concepts;
mod dependencies;
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
