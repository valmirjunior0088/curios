//! A program compiled as the product compiles one, and a run of it at a size: what the codegen suites and the bench's measurements of a workload run.

use {
    super::*,
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_native::{Compiled, ForeignBindings, test_support::*},
    curios_pipeline::compile_with_prelude,
    curios_surface::Entrypoint,
};

/// `source` compiled against the prelude and through Binaryen, as the product compiles a program.
pub fn compiled_of(source: &str) -> Compiled {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())
        .expect("the workload parses");
    let (module, foreigns) =
        compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |_| {})
            .expect("the workload compiles");

    to_compiled(&module, foreigns).expect("the workload compiles")
}

/// One run, its size handed as the program's one argument: what the program printed.
pub fn run_at(compiled: &Compiled, n: u64) -> Vec<u8> {
    let (host, io) = MockHost::builder()
        .args(["program".to_string(), n.to_string()])
        .build();
    let code = compiled
        .run(host, ForeignBindings::empty())
        .expect("the workload runs");
    assert_eq!(code, 0, "the workload exits cleanly");

    io.output()
}
