//! Hot loops and recursion: single-entry continuations, natural loops, scalars in registers, and no irreducible fallback.

//! Structural acceptance fixtures. Each test compiles a small `.crs` fixture to the raw, pre-Binaryen wasm module and asserts a structural property of the emitted code — a clean natural loop for a hot kernel, direct recursion, the closure ABI only where a call is genuinely unknown — and that the raw module validates and executes without Binaryen repairing control flow.
//!
//! Emitted function names are `$func/<N>` ids — a module-wide monotonic index over every reachable function, prelude included — optionally suffixed with the source hint as `$func/<N>$hint`. The index carries identity; the hint is only origin annotation. Hot kernels are still located by a distinctive literal constant baked into their arithmetic (`65537` for LCG, `1000003` for trees) or by name-independent structure (self-recursion, the shared `$func/<N>`/`$clsr/<N>` index of a function used both directly and as a closure), never by a source name. A genuine irreducible-cycle dispatcher is the `loop $$dispatch/<anchor>` the emitter names in `into_wasm::expr_emitter`; an ordinary constructor-tag `switch` is not a dispatcher whatever shape it takes — a `br_table` over `$case$N`/`$tail` labels for three or more cases, a plain `if` for the two-way and one-way shapes.

use crate::tests::cont_optm;

use super::test_support::*;

// -- LCG --------------------------------------------------------------------

/// L1: the LCG kernel reaches closure conversion as a single-entry recursive continuation. Proxy: the user `loop` is contified — the optimized high-CPS module keeps only `main`, prelude helpers, the `io/…` description thunks every effect boundary erases to, and the lambdas those lift, so the recursive kernel survives as a local continuation (a recursive `cont` with a single external entry and its own backedge), not a function. The contification mechanism is owned by `curios-cont`'s `contify_calls` tests; this pins the end-to-end result.
#[test]
fn lcg_kernel_is_single_entry_recursive_continuation() {
    let cont = cont_optm(LCG);
    assert!(
        cont.contains("NatRem") && cont.contains("65537"),
        "the kernel arithmetic must survive into the optimized cont module",
    );

    for line in cont
        .lines()
        .map(str::trim_start)
        .filter(|l| l.starts_with("fun ~f"))
    {
        // A named function prints `fun ~fN$hint(...)`: the source hint is the run after the first `$` and before the parameter list.
        let provenance = line
            .split_once('$')
            .and_then(|(_, rest)| rest.split_once('('))
            .map(|(name, _)| name)
            .unwrap_or_default();
        // An allowlist rather than a check on the loop's own name, which is only possible because every emitted function carries a hint: a prelude helper its `/std/` path, the description machinery its `io/` tag, and a lifted lambda its owner's name qualified — `/std/Handle/write/1`. A leaked `loop` matches none of them.
        assert!(
            provenance == "main"
                || provenance.starts_with("/std/")
                || provenance.starts_with("io/"),
            "the recursive loop must be contified, not left a top-level function: {line}",
        );
    }
}

/// L2/L3: the hot kernel is exactly one natural loop with a clean backedge — no nested loop, and no `$dispatch/` selector driving the iteration.
#[test]
fn lcg_hot_kernel_is_one_natural_loop() {
    let wat = wat(LCG);
    let kernel = loop_containing(&wat, "65537");

    assert_eq!(
        kernel.matches("loop ").count(),
        1,
        "the kernel must be a single natural loop, not nested loops",
    );
    assert!(
        !kernel.contains("$dispatch/"),
        "the backedge must be an ordinary loop branch, not a dispatcher selector",
    );
}

/// L4: the loop body is direct scalar arithmetic — Nat multiply (`i64.mul`, widened for its overflow check) and unsigned remainder (`i32.rem_u`) — with no closure allocation and no indirect (`call_ref`) dispatch.
#[test]
fn lcg_loop_is_scalar_no_closure_no_indirect() {
    let wat = wat(LCG);
    let kernel = loop_containing(&wat, "65537");

    assert!(
        kernel.contains("i64.mul"),
        "Nat multiply is direct scalar arithmetic"
    );
    assert!(
        kernel.contains("i32.rem_u"),
        "the modulo is direct scalar arithmetic"
    );
    assert!(
        !kernel.contains("call_indirect"),
        "no indirect call in the hot loop"
    );
    assert!(
        !kernel.contains("struct.new $clsr/"),
        "no closure allocation in the hot loop"
    );
    assert!(
        !kernel.contains("struct.new $envr/"),
        "no environment allocation in the hot loop"
    );
}

/// L5: the loop carries its scalars in registers, so a back edge moves a register to a register. `ref.as_non_null` is the tell: every edge argument used to be loaded with it, and a parameter the representation analysis holds raw is loaded at its carrier instead — a bare `local.get`. Zero of them in the kernel is the loop-carried decision the `cps::represent` fixpoint exists to produce, and it is the one count that went to zero.
///
/// The casts do *not* go to zero and asserting that they do would be wrong: 4 `ref.cast`/`i31.get_u` pairs survive on values the loop reads from outside itself, where the coercion is correct and is the cheaper side of the trade. Nor does the `i64` widening go away — see `i64.mul` in [`lcg_loop_is_scalar_no_closure_no_indirect`] — because a `Nat` product leaving the i31 envelope must trap and `i32.mul` wraps rather than trapping, which no storage decision changes.
#[test]
fn lcg_loop_carries_its_scalars_in_registers() {
    let wat = wat(LCG);
    let kernel = loop_containing(&wat, "65537");

    assert!(
        !kernel.contains("ref.as_non_null"),
        "no edge argument in the hot loop is reboxed to cross it: {kernel}"
    );
}

// -- trees ------------------------------------------------------------------

/// T1: build and sum retain direct recursive code. `sum` is the function carrying the `1000003` modulus; `build` is the other user function with two direct self calls (the recursive `to_str` prelude helper has one). Both recurse through direct `call`/`return_call`, and — since the whole module emits no `call_ref` (see [`trees_hot_arithmetic_has_no_indirect_calls`]) — that recursion is direct.
#[test]
fn trees_build_and_sum_stay_direct_recursive() {
    let wat = wat(TREES);
    let functions = functions(&wat);

    let sum = function_with(&functions, "1000003");
    assert!(sum.self_calls() >= 1, "sum must recurse directly");

    let build = functions
        .iter()
        .filter(|f| f.name.starts_with("$func/") && !f.body.contains("1000003"))
        .find(|f| f.self_calls() >= 2)
        .expect("build recurses directly on both subtrees");
    assert!(
        build.name != sum.name,
        "build and sum are distinct direct-recursive functions",
    );
}

/// T2: the recursive arithmetic is folded to bare intrinsic instructions rather than dispatched through a witness — the invariant `Nat` operation implementations propagate through the recursive SCC and collapse to `i32` instructions, with no `call_ref` witness projection left behind. The SCC known-argument propagation that enables this is owned by `curios-cont`'s specialization tests; this pins its emitted consequence.
#[test]
fn trees_invariant_arithmetic_propagates_through_scc() {
    let wat = wat(TREES);
    let functions = functions(&wat);
    let sum = function_with(&functions, "1000003");

    assert!(
        sum.body.contains("i32.rem_u"),
        "the modulus folded to a bare instruction"
    );
    assert!(
        sum.body.contains("i32.add"),
        "the summation folded to bare instructions"
    );
    assert!(
        !sum.body.contains("call_indirect"),
        "no witness dispatch survives in the recursive arithmetic",
    );
}

/// T3: the hot recursive code performs no indirect calls. Every call in the trees module is direct except at the effect boundary, where forcing a description *is* an indirect call — `main` forces the program's own description, and `io/bind` forces each of the two it sequences. The tree recursion is not among them.
///
/// Stated as "the module contains no indirect dispatch" this held only while programs were direct-style; a program is a description now, so two forces are structural. Pinning `main`'s count keeps that from being a licence: an indirect call anywhere in user code, or a second one in `main`, still fails.
#[test]
fn trees_hot_arithmetic_has_no_indirect_calls() {
    let wat = wat(TREES);
    let functions = functions(&wat);

    let stray = user_functions_with(&functions, "call_indirect");
    assert!(
        stray.is_empty(),
        "trees calls indirectly outside the effect boundary: {stray:?}"
    );

    let main = functions
        .iter()
        .find(|function| function.name == "$func/main")
        .expect("the module has an entry");
    assert_eq!(
        main.body.matches("call_indirect").count(),
        1,
        "main forces the program's description once and calls nothing else indirectly",
    );
}

/// T4: ordinary recursive functions allocate no closures. The trees module allocates only data tuples (`$tuple/…` for the `Tree` nodes) — no closure (`$clsr/`) or environment (`$envr/`) structs.
#[test]
fn trees_ordinary_recursion_allocates_no_closures() {
    let wat = wat(TREES);
    let closures = user_allocations(&wat, "struct.new $clsr/");
    assert!(closures.is_empty(), "no closure allocation: {closures:?}");
    let envs = user_allocations(&wat, "struct.new $envr/");
    assert!(envs.is_empty(), "no environment allocation: {envs:?}");
}

/// A string walk allocates nothing per character.
///
/// `/std/Str/fold` used to be an induction over the bytes whose motive was a *function* of the scan state and the accumulator, because a right fold cannot carry a value leftwards any other way. Every step therefore returned a closure: the walk built `step₀ ∘ … ∘ base` and applied it once, so N characters cost N environment allocations and N indirect calls before any of the user's own work ran. It is now a `rec` whose parameters carry the scan state and the accumulator, and whose tail call advances them.
///
/// **What this asserts is the property, not the spelling.** Any encoding that captures per character reintroduces an environment allocation here, whatever it is named — which is what makes this survive the next person to reach for the induction form.
///
/// Measured when it landed, at N = 1 000 000 on `programs/parse_digits.crs` and `programs/parse_bindless.crs`: 2.31 s to 1.07 s and 2.23 s to 1.01 s, with the emitted `$envr/…$/std/Str/fold/…` sites going from two to none. The figures live beside the probe that reproduces them, in [`super::ladder`].
#[test]
fn a_string_walk_allocates_no_closure_per_character() {
    let wat = wat(STRING_WALK);
    let envs = user_allocations(&wat, "struct.new $envr/");
    assert!(
        envs.is_empty(),
        "the walk carries its state in parameters, so nothing is captured per character: {envs:?}"
    );
}

/// G3: function-only recursion allocates no closure. `down` is a plain recursive function; the module allocates no closure (`$clsr/`) or environment (`$envr/`) for it.
#[test]
fn function_only_recursion_allocates_no_closures() {
    let wat = wat(FUNCTION_ONLY);
    // Allocation, not mention: a module that forces a description at all declares the closure *type* for the arity it forces at, and names it in the `call_ref`. What `down` must not do is allocate one.
    let closures = user_allocations(&wat, "struct.new $clsr/");
    assert!(
        closures.is_empty(),
        "function-only recursion needs no closures: {closures:?}"
    );
    let envs = user_allocations(&wat, "struct.new $envr/");
    assert!(
        envs.is_empty(),
        "function-only recursion needs no environments: {envs:?}"
    );
}

/// G4: ordinary corpus cases use no irreducible fallback. None of the ordinary fixtures — including mutual recursion — emit a `loop $$dispatch/` localized dispatcher; their constructor-tag matches lower to ordinary data switches — a `br_table` over `$case$N` labels where the family is wide enough to want one, an `if` where it is not — and neither is a dispatcher.
#[test]
fn ordinary_corpus_uses_no_irreducible_fallback() {
    for (label, source) in [
        ("lcg", LCG),
        ("trees", TREES),
        ("function-only", FUNCTION_ONLY),
        ("higher-order", HIGHER_ORDER),
        ("direct/escaping", DIRECT_ESCAPING),
    ] {
        assert!(
            !wat(source).contains("$dispatch/"),
            "{label} must not need an irreducible dispatcher",
        );
    }
}

/// G5: the one-localized-dispatcher guarantee. Curios surface syntax has no unstructured jump, so even mutual recursion entered from two arms is structured reducibly (no `$dispatch/`) — there is no `.crs` program that produces a genuine irreducible cycle. The dispatcher path (exactly one `loop $$dispatch/` per irreducible component) is therefore owned and asserted at the backend-unit level by `curios-cont`'s `an_irreducible_component_uses_exactly_one_localized_dispatcher` in `into_wasm::emit_tests`; this test pins the surface-level fact that motivates that ownership boundary.
#[test]
fn mutual_recursion_stays_reducible() {
    assert!(
        !wat(MUTUAL_RECURSION).contains("$dispatch/"),
        "mutual recursion must structure reducibly, without a localized dispatcher",
    );
}
