//! What this crate's suites, `curios-prelude`'s and the bench's measurements share, a module by subject: here, a module compiled and run in this process, where a shipped build runs one from its `.cwasm` alone; in `programs`, a program compiled and run as a fixture states it; in `structural`, the structural fixtures, compiled to raw pre-Binaryen wasm and read as text; and in a module each, the programs a suite and a measurement both build.

mod growth;
pub use growth::{Growth, grown, joined, program, repeated};

mod towers;
pub use towers::{Shape, Stands, chain, tower};

mod runaways;
pub use runaways::{Runaway, runaway};

mod listing;
pub use listing::LISTING_PROGRAM;

mod runs;
pub use runs::{compiled_of, run_at};

mod churn;
pub use churn::ROUNDS;

mod alternation;
pub use alternation::{SIZES, alternation_programs};

mod reduction;
pub use reduction::{bytes_growing, declaration_cost, list_growing, str_literal};

mod unfolding;
pub use unfolding::{
    Consumed, ENTRY, Inner, SEQUENCED, TAIL, compile_only, consumer, copies, emitted, grammar,
    numeric_web, numerics, predicates,
};

mod laws;
pub use laws::{nested, summed};

mod ladder;
pub use ladder::mirror_counts;

mod programs;
pub use programs::{
    Lying, anf_optm, anf_optm_tests, census_settles, compile, core, core_elab, cps_operations,
    cps_optm, cps_optm_module, emits, error, fenced, host_writes, programs, reads_nat, run,
    run_entrypoint, run_tests_program, run_tests_under, run_text, test_program, tests_error,
    typecheck, typecheck_within, unlaid, workspace,
};

mod structural;
pub use structural::{
    CALL_RESULT_SEED, CAPTURED_FLOAT, COMPUTED_TYPE, CONJUNCTION_CALL, DIRECT_ESCAPING,
    FLOAT_LITERAL_LOOP, FUNCTION_ONLY, Function, HIGHER_ORDER, KNOWN_PARAMETER_SEED, LCG,
    LIST_EQUALITY, LOOPED_PICK, MUTUAL_RECURSION, PAIR_STEP, PARSED_FLOAT, SHARED_PAYLOAD,
    SPLIT_RETURN, STATED_SEEDS, STRING_WALK, TREES, UNCURRY, UNCURRY_CAPTURED,
    UNCURRY_THROUGH_A_CALL, VARIANT_FILLER, compile_raw, compile_raw_program, function_with,
    functions, in_heap_function, indices, loops_containing, row_declaration, run_binaryen, run_raw,
    user_allocations, user_functions_with, wat,
};

use {
    crate::compile::optimize,
    curios_abi::{ForeignStore, HostOps},
    curios_native::{Compiled, ForeignBindings, GuestMap, start_engine},
};

/// Optimize (Binaryen) and compile (Cranelift) a module to the program this process runs, on `curios-native`'s shared engine: [`to_cwasm`](crate::to_cwasm)'s machine code, held rather than written out, so running it reads nothing back. `foreigns` is the rows the compilation handed back beside `module`, which the program carries to its link.
pub fn to_compiled(
    module: &curios_wasm::Module,
    foreigns: ForeignStore,
) -> Result<Compiled, String> {
    // A suite runs no `main`, so it starts the engine as every test does, with no map for a sampler, before the optimizer validates against it.
    start_engine(GuestMap::Unwritten)?;
    let bytes = optimize(module);

    let code = curios_cranelift::compile(&bytes)
        .map_err(|error| format!("failed to compile module: {error}"))?;

    Ok(Compiled::of(code, foreigns))
}

/// Compile a module and run it in-process on the shared runtime engine. `foreigns` is the rows the compilation handed back beside it, and `bindings` answers rows ahead of the host, the program's own `foreign` declarations among them (pass [`ForeignBindings::empty`] where the host answers every row). Returns the process exit code.
pub fn run_wasm<H: HostOps + Send + Sync + 'static>(
    module: &curios_wasm::Module,
    foreigns: ForeignStore,
    host: H,
    bindings: ForeignBindings,
) -> Result<u8, String> {
    to_compiled(module, foreigns)?
        .run(host, bindings)
        .map_err(|stop| stop.to_string())
}
