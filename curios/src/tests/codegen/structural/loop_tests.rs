//! Hot loops and recursion: single-entry continuations, natural loops, scalars in registers, and no irreducible fallback.

use {
    super::test_support::*,
    crate::tests::{cont_optm_module, emits, reads_nat},
};

// -- LCG --------------------------------------------------------------------

/// The LCG kernel reaches closure conversion as a single-entry recursive continuation. Proxy: the user `loop` is contified — the optimized high-CPS module keeps only `main`, prelude helpers, the `io/…` description thunks every effect boundary erases to, and the lambdas those lift, so the recursive kernel survives as a local continuation (a recursive `cont` with a single external entry and its own backedge), not a function. The contification mechanism is owned by `curios-cont`'s `contify_calls` tests; this pins the end-to-end result.
#[test]
fn lcg_kernel_is_single_entry_recursive_continuation() {
    let module = cont_optm_module(LCG);
    assert!(
        emits(&module, |op| *op == curios_cont::Intrinsic::NatRem) && reads_nat(&module, 65537),
        "the kernel arithmetic must survive into the optimized cont module:\n{module}",
    );

    for function in module.functions().iter().flatten() {
        let provenance = function.debug_name.as_deref().unwrap_or_default();
        // An allowlist rather than a check on the loop's own name, which is only possible because every emitted function carries a hint: a prelude helper its `/std/` path, the description machinery its `io/` tag, the steps a suspension takes among them, and a lifted lambda its owner's name qualified — `/sys/Handle/write/1`, or `main/1`, the program's continuation past the host call that reads its arguments, which a suspension carries. A leaked `loop` matches none of them.
        assert!(
            provenance == "main"
                || provenance.starts_with("main/")
                || provenance.starts_with("/entry/")
                || provenance.starts_with("/std/")
                || provenance.starts_with("io/"),
            "the recursive loop must be contified, not left a top-level function: {provenance}",
        );
    }
}

/// The hot kernel is exactly one natural loop with a clean backedge — no nested loop, and no `$dispatch/` selector driving the iteration.
#[test]
fn lcg_hot_kernel_is_one_natural_loop() {
    let wat = wat(LCG);
    for kernel in loops_containing(&wat, "65537") {
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
}

/// The loop body is direct scalar arithmetic — Nat multiply (`i64.mul`, widened for its overflow check) and unsigned remainder (`i32.rem_u`) — with no closure allocation and no indirect (`call_indirect`) dispatch.
#[test]
fn lcg_loop_is_scalar_no_closure_no_indirect() {
    let wat = wat(LCG);
    for kernel in loops_containing(&wat, "65537") {
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
}

/// The loop carries its scalars in registers, so a back edge moves a register to a register. `ref.as_non_null` is the tell: a boxed edge argument is loaded with it, and a parameter the representation analysis holds raw is loaded at its carrier instead — a bare `local.get`. Zero of them in the kernel is the loop-carried decision `curios-cont`'s `represent` fixpoint exists to produce.
///
/// The casts do *not* go to zero and asserting that they do would be wrong: 4 `ref.cast`/`i31.get_u` pairs survive on values the loop reads from outside itself, where the coercion is correct and is the cheaper side of the trade. Nor does the `i64` widening go away — see `i64.mul` in [`lcg_loop_is_scalar_no_closure_no_indirect`] — because a `Nat` product leaving the i31 has to be detected and grown into a boxed magnitude, and `i32.mul` wraps silently, which no storage decision changes.
#[test]
fn lcg_loop_carries_its_scalars_in_registers() {
    let wat = wat(LCG);
    for kernel in loops_containing(&wat, "65537") {
        assert!(
            !kernel.contains("ref.as_non_null"),
            "no edge argument in the hot loop is reboxed to cross it: {kernel}"
        );
    }
}

/// A float literal the loop reads in a register is its constant. Hoisting renames every literal to a module const, whose global holds a box for the reads that want a reference, and a read of it in a register took the global back out of that box: a global load, a cast and a field load every round, which Binaryen leaves where they are.
#[test]
fn a_float_literal_read_in_a_register_is_its_constant() {
    let wat = wat(FLOAT_LITERAL_LOOP);
    let globals = ["1.25", "0.375"].map(|literal| float_global(&wat, literal));
    for kernel in loops_containing(&wat, "i32.const 7919") {
        assert!(
            kernel.contains("f64.const 1.25") && kernel.contains("f64.const 0.375"),
            "both literals are read where the loop reads them: {kernel}"
        );
        for global in &globals {
            assert!(
                !kernel
                    .lines()
                    .any(|line| line.trim() == format!("global.get {global}")),
                "no literal is read back out of its global's box, `{global}`: {kernel}"
            );
        }
    }
}

// -- floats across known calls ----------------------------------------------

/// A loop seeded by its function's own float parameter holds the float in a register. `spin` does not escape, so every call to it is one the representation reads, and its parameter is a join over those calls, each handing it a float.
#[test]
fn a_loop_a_known_parameter_seeds_allocates_no_float() {
    let wat = wat(KNOWN_PARAMETER_SEED);
    let functions = functions(&wat);
    let spin = function_with(&functions, "i32.const 7919");
    assert!(
        spin.name.ends_with("$/spin"),
        "the premise: the loop stays in `spin`, seeded by its parameter, rather than being copied into a caller: {}",
        spin.name
    );
    for kernel in loops_containing(spin.body, "i32.const 7919") {
        assert!(
            !kernel.contains("struct.new $flt"),
            "no round boxes the float its parameter seeded: {kernel}"
        );
    }
}

/// A loop seeded by a call's result holds the float in a register: `base`'s class hands its result back from return edges that each carry a float, so the slot, and the resume parameter it feeds, are registers.
#[test]
fn a_loop_a_call_result_seeds_allocates_no_float() {
    let wat = wat(CALL_RESULT_SEED);
    assert!(
        functions(&wat)
            .iter()
            .any(|function| function.name.ends_with("$/base")),
        "the premise: `base` stays a function whose result seeds the loop"
    );
    for kernel in loops_containing(&wat, "i32.const 7919") {
        assert!(
            !kernel.contains("struct.new $flt"),
            "no round boxes the float a call's result seeded: {kernel}"
        );
    }
}

/// A step over a two-float structure, called once a round, takes its fields and hands back its answer in registers: the worker split and the return protocol already cross its boundary as fields, and the representation now holds each field at its carrier, so neither the step nor the loop allocates.
#[test]
fn a_step_over_floats_called_from_a_loop_allocates_no_float() {
    let wat = wat(PAIR_STEP);
    let functions = functions(&wat);
    let step = functions
        .iter()
        .find(|function| function.name.ends_with("$/step"))
        .expect("the premise: `step` stays a function the loop calls");
    assert!(
        !step.body.contains("struct.new $flt"),
        "the step hands its answer back without boxing it: {}",
        step.body
    );
    for kernel in loops_containing(&wat, "i32.const 7919") {
        assert!(
            kernel.contains(&format!("call {}", step.name)),
            "the premise: the loop calls the step: {kernel}"
        );
        assert!(
            !kernel.contains("struct.new $flt"),
            "no round boxes what crosses the step's boundary: {kernel}"
        );
    }
}

/// A float a closure captures stays a float on both sides of the capture: the closure's environment holds it in a field declared `f64`, the closure's body reads it there, and the loop that defines it holds it in a register too, where a capture once withdrew it everywhere.
#[test]
fn a_captured_float_seeds_loops_on_both_sides_of_the_capture_unboxed() {
    let wat = wat(CAPTURED_FLOAT);
    assert!(
        wat.lines()
            .any(|line| line.contains("(type $envr/") && line.contains("f64")),
        "the premise: the escaping closure's environment declares the captured float a field of its own"
    );
    // Each loop's exit boxes its answer once, where it leaves: the closure hands it back at its arity's uniform signature, and the entry's loop into a join a reference reads. What must not happen is a box on the way round.
    for needle in ["i32.const 7919", "i32.const 6007"] {
        for kernel in loops_containing(&wat, needle) {
            assert!(
                !boxes_a_round(kernel),
                "no round boxes the float the capture seeded: {kernel}"
            );
        }
    }
}

/// A loop seeded by an escaping function's parameter, and one seeded by a closure call's result: each seed is a reference whose type erasure read as a float, so each loop holds its float in a register and unboxes the seed once, where each once boxed every round.
#[test]
fn a_loop_a_stated_float_seeds_boxes_no_round() {
    let wat = wat(STATED_SEEDS);
    assert!(
        wat.lines()
            .any(|line| line.contains("(func $clsr/") && line.contains("$/warp ")),
        "the premise: warp escapes, entered through its closure wrapper"
    );
    for needle in ["i32.const 7919", "i32.const 6007"] {
        for kernel in loops_containing(&wat, needle) {
            assert!(
                !boxes_a_round(kernel),
                "no round boxes the float a stated reference seeded: {kernel}"
            );
        }
    }
}

/// Whether some float box in `kernel` is on the way round: its straight run of instructions ends at the loop's back edge rather than leaving it.
fn boxes_a_round(kernel: &str) -> bool {
    let lines = kernel.lines().map(str::trim).collect::<Vec<_>>();
    lines.iter().enumerate().any(|(index, line)| {
        *line == "struct.new $flt"
            && lines[index + 1..]
                .iter()
                .find(|next| {
                    next.starts_with("br ")
                        || next.starts_with("return")
                        || next.starts_with("else")
                        || next.starts_with("end")
                })
                .is_some_and(|transfer| transfer.starts_with("br $$loop"))
    })
}

/// What the programs whose floats now cross a function boundary in registers print, worked independently in IEEE doubles with ties to even.
#[test]
fn floats_across_function_boundaries_answer_as_computed() {
    let args = ["prog", "a", "b", "c"];
    for (source, expected) in [
        (
            KNOWN_PARAMETER_SEED,
            "+384.7028433303833 | +41956.94057294268",
        ),
        (CALL_RESULT_SEED, "+2498.0003496329787 | +3.0526796875"),
        (
            PAIR_STEP,
            "+0.00175099278948419 | +0.00175099290668634 | +0.01425",
        ),
        (CAPTURED_FLOAT, "+1608.2786484375 | +8057.660031863108"),
        (STATED_SEEDS, "+3355645.0365754715 | +710593533.1304455"),
    ] {
        assert_eq!(String::from_utf8(run_raw(source, &args)).unwrap(), expected);
    }
}

/// The global a float literal is hoisted to: the one whose initializer boxes that literal, read off its declaration whether the printer kept it on one line or broke it before the initializer.
fn float_global<'a>(wat: &'a str, literal: &str) -> &'a str {
    let lines = wat.lines().collect::<Vec<_>>();
    (0..lines.len())
        .find_map(|index| {
            let name = lines[index]
                .trim()
                .strip_prefix("(global ")?
                .split_whitespace()
                .next()?;
            let continued = lines
                .get(index + 1)
                .filter(|next| !next.trim_start().starts_with('('));
            let tokens = lines[index]
                .split_whitespace()
                .chain(
                    continued
                        .into_iter()
                        .flat_map(|next| next.split_whitespace()),
                )
                .collect::<Vec<_>>();
            tokens
                .windows(2)
                .any(|pair| pair[0] == "f64.const" && pair[1].trim_end_matches(')') == literal)
                .then_some(name)
        })
        .unwrap_or_else(|| panic!("the literal `{literal}` is hoisted to a global"))
}

// -- trees ------------------------------------------------------------------

/// Build and sum retain direct recursive code. `sum` is the function carrying the `1000003` modulus; `build` is the other user function with two direct self calls (the recursive `to_str` prelude helper has one). Both recurse through direct `call`/`return_call`, and — since no user function emits a `call_indirect` (see [`trees_hot_arithmetic_has_no_indirect_calls`]) — that recursion is direct.
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

/// The recursive arithmetic is folded to bare intrinsic instructions rather than dispatched through a witness — the invariant `Nat` operation implementations propagate through the recursive SCC and collapse to `i32` instructions, with no `call_indirect` witness dispatch left behind. The SCC known-argument propagation that enables this is owned by `curios-cont`'s specialization tests; this pins its emitted consequence.
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

/// The hot recursive code performs no indirect calls. Every call in the trees module is direct except at the effect boundary, where forcing a description *is* an indirect call — `main` forces the program's own description, and `io/bind` forces each of the two it sequences. The tree recursion is not among them.
///
/// A program is a description, so two forces are structural and "the module contains no indirect dispatch" would be false. Pinning `main`'s count keeps that from being a licence: an indirect call anywhere in user code, or a second one in `main`, still fails.
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

/// Ordinary recursive functions allocate no closures. The trees module allocates only data tuples (`$tuple/…` for the `Tree` nodes) — no closure (`$clsr/`) or environment (`$envr/`) structs.
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
/// `/std/Str/fold` is a `rec` whose parameters carry the scan state and the accumulator, and whose tail call advances them. An induction over the bytes would need a motive that is a *function* of the scan state and the accumulator, since a right fold cannot carry a value leftwards any other way: every step would return a closure, the walk would build `step₀ ∘ … ∘ base` and apply it once, and N characters would cost N environment allocations and N indirect calls before any of the user's own work ran.
///
/// **What this asserts is the property, not the spelling.** Any encoding that captures per character reintroduces an environment allocation here, whatever it is named — which is what makes this survive the next person to reach for the induction form.
///
/// The walk's timings live beside the probe that retakes them, in `ladder`.
#[test]
fn a_string_walk_allocates_no_closure_per_character() {
    let wat = wat(STRING_WALK);
    let envs = user_allocations(&wat, "struct.new $envr/");
    assert!(
        envs.is_empty(),
        "the walk carries its state in parameters, so nothing is captured per character: {envs:?}"
    );
}

/// Function-only recursion allocates no closure. `down` is a plain recursive function; the module allocates no closure (`$clsr/`) or environment (`$envr/`) for it.
#[test]
fn function_only_recursion_allocates_no_closures() {
    let wat = wat(FUNCTION_ONLY);
    // Allocation, not mention: a module that forces a description at all declares the closure *type* for the arity it forces at, and names it in the `call_indirect`. What `down` must not do is allocate one.
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

/// Ordinary corpus cases use no irreducible fallback. None of the ordinary fixtures — including mutual recursion — emit a `loop $$dispatch/` localized dispatcher; their constructor-tag matches lower to ordinary data switches — a `br_table` over `$case$N` labels where the family is wide enough to want one, an `if` where it is not — and neither is a dispatcher.
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

/// The one-localized-dispatcher guarantee. Curios surface syntax has no unstructured jump, so even mutual recursion entered from two arms is structured reducibly (no `$dispatch/`) — there is no `.crs` program that produces a genuine irreducible cycle. The dispatcher path (exactly one `loop $$dispatch/` per irreducible component) is therefore owned and asserted at the backend-unit level by `curios-emit`'s `an_irreducible_component_uses_exactly_one_localized_dispatcher` in `curios-emit`'s `module_tests`; this test pins the surface-level fact that motivates that ownership boundary.
#[test]
fn mutual_recursion_stays_reducible() {
    assert!(
        !wat(MUTUAL_RECURSION).contains("$dispatch/"),
        "mutual recursion must structure reducibly, without a localized dispatcher",
    );
}
