//! The closure ABI: table indices, interned capture-free closures, and the escaping uses that coexist with direct ones.

//! Structural acceptance fixtures. Each test compiles a small `.crs` fixture to the raw, pre-Binaryen wasm module and asserts a structural property of the emitted code — a clean natural loop for a hot kernel, direct recursion, the closure ABI only where a call is genuinely unknown — and that the raw module validates and executes without Binaryen repairing control flow.
//!
//! Emitted function names are `$func/<N>` ids — a module-wide monotonic index over every reachable function, prelude included — optionally suffixed with the source hint as `$func/<N>$hint`. The index carries identity; the hint is only origin annotation. Hot kernels are still located by a distinctive literal constant baked into their arithmetic (`65537` for LCG, `1000003` for trees) or by name-independent structure (self-recursion, the shared `$func/<N>`/`$clsr/<N>` index of a function used both directly and as a closure), never by a source name. A genuine irreducible-cycle dispatcher is the `loop $$dispatch/<anchor>` the emitter names in `into_wasm::expr_emitter`; an ordinary constructor-tag `switch` is not a dispatcher whatever shape it takes — a `br_table` over `$case$N`/`$tail` labels for three or more cases, a plain `if` for the two-way and one-way shapes.

use super::test_support::*;

// -- general corpus ---------------------------------------------------------

/// G1: a genuinely unknown higher-order call retains the closure ABI and dispatches through its arity's typed table. `f` is selected at runtime, so it cannot be devirtualized: the module declares `$clsr/…` closure types, materializes the branches as environments carrying their body's `i32` table index, and dispatches through `call_indirect`.
#[test]
fn unknown_higher_order_call_uses_closure_abi_and_call_indirect() {
    let wat = wat(HIGHER_ORDER);
    assert!(
        wat.contains("call_indirect $clsr/"),
        "the unknown call dispatches through its arity's closure table"
    );
    assert!(wat.contains("$clsr/"), "the closure ABI is retained");
}

/// The closure ABI's code field is an `i32` table index, and nothing in *function code* touches a funcref. Construction writes `i32.const`, dispatch reads the field into `call_indirect`, and one typed table per arity — `(ref null $clsr/N)`, filled by an active element segment at offset 1, slot 0 null so a zeroed code field traps — carries that arity's bodies. The element type is what satisfies the `call_indirect` signature check statically, so the per-dispatch runtime subtype check has no site left to fire on; the only `ref.func`s in a module are the segments' items, and `call_ref` is absent everywhere.
#[test]
fn closures_carry_their_code_as_a_table_index() {
    for (label, source) in [
        ("lcg", LCG),
        ("trees", TREES),
        ("higher-order", HIGHER_ORDER),
        ("direct/escaping", DIRECT_ESCAPING),
        ("function-only", FUNCTION_ONLY),
        ("mutual-recursion", MUTUAL_RECURSION),
        ("split-return", SPLIT_RETURN),
        ("uncurry", UNCURRY),
        ("string-walk", STRING_WALK),
        ("looped-pick", LOOPED_PICK),
    ] {
        let wat = wat(source);
        assert_eq!(
            wat.matches("ref.func").count(),
            wat.matches("(item ref.func").count(),
            "{label}: every funcref materialization is an element-segment item",
        );
        assert!(
            !wat.contains("call_ref"),
            "{label}: no dispatch reads a funcref back",
        );
    }

    let wat = wat(HIGHER_ORDER);
    assert!(
        wat.contains("(field $! i32)"),
        "the environment's code field is an ordinary i32",
    );
    assert!(
        wat.contains("(table $clsr/") && wat.contains("(ref null $clsr/"),
        "each arity's typed table holds its closure bodies",
    );
    assert!(
        wat.contains("(offset i32.const 1) (ref $clsr/"),
        "an active typed segment fills each table from slot 1, leaving slot 0 null",
    );
}

/// A capture-free closure constructed in a loop pins as a module const: the constant hoister interns it like any constant aggregate — the swap made its code field an `i32`, dissolving the exclusion that kept closures inline to keep `ref.func` out of the start function — so the loop's arms reference globals and no per-iteration environment construction survives in function code. The environments are built exactly once, in `$start`.
#[test]
fn a_capture_free_closure_in_a_loop_interns_as_a_const() {
    let wat = wat(LOOPED_PICK);

    // `spin` owns the 40009 modulus and is contified into the entry, so the claim is made of the loop itself; the two lambdas own 30011 and are lifted to their own functions.
    let kernel = loop_containing(&wat, "40009");
    assert!(
        !kernel.contains("struct.new $envr/"),
        "the loop constructs no environment per iteration: {kernel}",
    );
    assert!(
        kernel.contains("global.get $const/"),
        "the arms reference the interned consts instead: {kernel}",
    );

    let functions = functions(&wat);
    let start = functions
        .iter()
        .find(|function| function.name == "$start")
        .expect("the module has a start function");
    assert!(
        start.body.matches("struct.new $envr/").count() >= 2,
        "both lambdas materialize once, at instantiation: {}",
        start.body,
    );
}

/// G2: direct and escaping uses of the same function coexist. A function used both directly and as a first-class value is emitted once as `$func/<N>` (the direct callee) and once as `$clsr/<N>` (the escaping wrapper) under the same index, so the set of directly-called `$func/<N>` indices and the set of allocated `$envr/<N>` environments overlap — the environment carries its wrapper's index, and its allocation is what materializing the closure is now.
#[test]
fn direct_and_escaping_uses_coexist() {
    let wat = wat(DIRECT_ESCAPING);
    let called_directly = indices(&wat, "call $func/");
    let escaped = indices(&wat, "struct.new $envr/");

    assert!(
        called_directly.intersection(&escaped).next().is_some(),
        "some function must be both directly called and materialized as a closure\n\
         direct: {called_directly:?}  escaping: {escaped:?}",
    );
}

/// A returned closure that every caller applies is absorbed into the callee, so nothing allocates it and nothing calls through it.
///
/// All three are asserted because each alone is satisfiable the wrong way. A module that allocated nothing but still dispatched indirectly would have moved the cost rather than removed it; one that dispatched directly while still allocating would pay for a closure nothing reaches; and both hold vacuously of a module where the recursion was simply peeled away, which is what a fixture inside the inline budget produces.
///
/// The `call_indirect` exemption is `main`'s and the `$io/` thunks', following [`trees_hot_arithmetic_has_no_indirect_calls`]: a program *is* a description now, so forcing one is structurally an indirect call. It goes through [`user_functions_with`] rather than [`user_allocations`] because the instruction names the table and the closure *type* it calls through and never the callee, leaving the enclosing function as the only thing that says whose call it is.
///
/// **The environment goes with the closure, and that is lowering's doing rather than this transform's.** A free value reaches a directly-called function as a lifted parameter and an escaping one as an environment field — one decision, taken in `machine::lower` — so absorbing the application moves `walk`'s captured `n` from the second case to the first for free. The emitted pair takes it as a parameter and allocates nothing.
/// A returned closure the caller also captures keeps being a closure. Absorbing the application handed the capturing lambda the applied answer in the closure's place: this program trapped inside the lambda with no argument and inside `mk` with three, until the admission walk entered the lambda.
#[test]
fn a_returned_closure_the_caller_also_captures_is_not_absorbed() {
    for args in [&[][..], &["a", "b", "c"][..]] {
        assert_eq!(run_raw(UNCURRY_CAPTURED, args), b"2 10", "raw, {args:?}");
        assert_eq!(
            run_binaryen(UNCURRY_CAPTURED, args),
            b"2 10",
            "optimized, {args:?}"
        );
    }
}

#[test]
fn a_returned_closure_every_caller_applies_is_absorbed() {
    let wat = wat(UNCURRY);
    let functions = functions(&wat);

    let closures = user_allocations(&wat, "struct.new $clsr/");
    assert!(
        closures.is_empty(),
        "an absorbed closure is never built: {closures:?}"
    );

    let indirect = user_functions_with(&functions, "call_indirect");
    assert!(
        indirect.is_empty(),
        "nor does the application it received stay indirect: {indirect:?}"
    );

    // Located by the walk's own arithmetic, since neither surviving name is load-bearing: the absorbed step becomes a parameter of the function that used to return it, so the pair is identified by what it computes.
    let stepping = functions
        .iter()
        .filter(|function| function.body.contains("30011"))
        .collect::<Vec<_>>();
    assert_eq!(
        stepping.len(),
        2,
        "the walk survives as the two functions it was written as, rather than being peeled",
    );
    for function in stepping {
        assert!(
            function.body.contains("return_call $func/"),
            "and each hands on directly and in tail position, keeping the loop flat: {}",
            function.name,
        );
    }
}
