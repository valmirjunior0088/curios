//! Codegen parity for concept-dispatched operators (the witness-projection folding gate). No dedicated folding rule exists in core — none is needed: after erasure a witness is a bakeable value (a one-method dictionary collapses to its bare method field, a many-method one is a tuple of methods), so its resolved instance argument is a specialization candidate. Closure specialization bakes it into the caller, the `Tpl.get`s (for a tuple witness) fold to the known methods, and the cont inliner beta-reduces the chain down to the bare intrinsic instruction. These tests pin that pipeline behavior: a concept method call at a concrete intrinsic type must emit the *same operations* as calling the intrinsic wrapper directly. The generated names differ — the concept path threads the witness through an extra specialization, decorating clone names with its key — but that provenance names no instruction, so the emitted operations match exactly. These gate (and then guard) the infix rewrite that routes every operator through the concepts.

use {
    curios_pipeline::{Stage, compile_entrypoint},
    curios_text::{Entrypoint, RootSource},
};

/// The optimized cont-stage dump for `source`.
///
/// Handed to [`operations`] verbatim rather than digit-normalized first. Entropy-derived name counters (`~v37`, `~f26`) do differ between the compared programs, but [`operations`] discards names entirely, and the digits that survive into an operation token — a projection's index, a concatenation's arity — are its semantics, so collapsing them would make `TplGet(0)` and `TplGet(1)` compare equal.
fn cont_optm(source: &str) -> String {
    let entrypoint = source.parse::<Entrypoint>().expect("parity source parses");

    let mut dump = String::new();
    compile_entrypoint(
        crate::DEFAULT_STEP_BUDGET,
        &entrypoint,
        RootSource::none(),
        |stage| {
            if let Stage::ContOptm(module) = stage {
                dump = module.to_string();
            }
        },
    )
    .expect("parity source compiles");

    dump
}

/// `Add/add(x, 1)` at `Nat` — dictionary resolved from the sys witness — emits the same operations as the direct intrinsic wrapper call, over a runtime (non-constant-foldable) operand. `Add` is a one-method concept, so its witness erases to the bare `Nat/add` closure; the resolved instance is baked in by specialization and inlined to `Nat.add`. The clone names carry the extra witness key, so the dumps are not byte-identical — but the emitted operations are.
#[test]
fn concept_method_call_matches_direct_intrinsic_codegen() {
    let through_concept = r#"
        use /std/{Nat, Lst, Handle, Str, Add, proc};
        pub let bump(x : Nat) -> Nat = Add/add(x, 1);
        let taint = Lst/len(proc/args!);
        let n : Nat = taint;
        /std/print(Nat/to_str(bump(n)))
        "#;
    let direct = r#"
        use /std/{Nat, Lst, Handle, Str, proc};
        pub let bump(x : Nat) -> Nat = Nat/add(x, 1);
        let taint = Lst/len(proc/args!);
        let n : Nat = taint;
        /std/print(Nat/to_str(bump(n)))
        "#;

    assert_eq!(
        operations(&cont_optm(through_concept)),
        operations(&cont_optm(direct)),
    );
}

/// `choose` desugars to exactly the nested boolean matches a user would hand-write: `choose | c0 => b0 | c1 => b1 | _ => d end` is `match c0 | true => b0 | false => match c1 | true => b1 | false => d end end`. Both lower through the same core `bool_match` nesting, so they emit the same intrinsic operations — the two forms mint metavars in a slightly different order, which only permutes the emission order of the top-level specialized closures (their bodies are identical), so `operations()` is the exact comparison. A runtime operand (`Lst/len(proc/args!)`) keeps the ladder from folding to a constant.
#[test]
fn choose_matches_hand_nested_bool_codegen() {
    let ladder = r#"
        use /std/{Nat, Lst, Handle, Str, proc};
        let taint = Lst/len(proc/args!);
        let n : Nat = taint;
        let result =
            choose
            | n <= 0 => Nat/add(n, 100)
            | n <= 1 => Nat/add(n, 200)
            | _ => Nat/add(n, 300)
            end;
        /std/print(Nat/to_str(result))
        "#;
    let nested = r#"
        use /std/{Nat, Lst, Handle, Str, proc};
        let taint = Lst/len(proc/args!);
        let n : Nat = taint;
        let result =
            match n <= 0
            | true => Nat/add(n, 100)
            | false =>
                match n <= 1
                | true => Nat/add(n, 200)
                | false => Nat/add(n, 300)
                end
            end;
        /std/print(Nat/to_str(result))
        "#;

    assert_eq!(
        operations(&cont_optm(ladder)),
        operations(&cont_optm(nested)),
    );
}

/// A single-refutation bind arm `| some(x) = o => …` desugars to exactly the headed catch-all `match o | some(x) => … | _ => …` — both a single-row inductive match with the same default — so they emit identical operations.
#[test]
fn choose_bind_arm_matches_headed_catch_all_codegen() {
    let bind = r#"
        use /std/{Option, Nat, Lst, Handle, Str, proc};
        let f(o : Option(Nat)) -> Nat =
            choose
            | some(x) = o => x + 10
            | _ => 99
            end;
        let taint = Lst/len(proc/args!);
        let n : Nat = taint;
        /std/print(Nat/to_str(f(Option/some(n))))
        "#;
    let headed = r#"
        use /std/{Option, Nat, Lst, Handle, Str, proc};
        let f(o : Option(Nat)) -> Nat =
            match o
            | some(x) => x + 10
            | _ => 99
            end;
        let taint = Lst/len(proc/args!);
        let n : Nat = taint;
        /std/print(Nat/to_str(f(Option/some(n))))
        "#;

    assert_eq!(operations(&cont_optm(bind)), operations(&cont_optm(headed)),);
}

/// The instructions a cont dump emits, sorted — `NatAdd`, `TplGet(0)`, `BinLen(X)`, and the qualified `cell.Set` / `intrinsic.LstMap` forms — with the generated names that wire them together and the operands they read left out. Sorted because the two programs may emit their top-level closures in a different order while their bodies agree, which is the whole point of comparing operations rather than dumps.
///
/// `curios_cont`'s `Display` prints an instruction as its `Debug` spelling immediately followed by its bracketed operand list, so each `[` is a candidate — but the shape alone is not enough to identify one, which is why [`operation_ending_at`] decides on position instead.
///
/// The previous spelling looked for `Kind.op` tokens (`Nat.add`, `Tpl.get`) that the printer has not emitted for some time, so it matched nothing at all and every comparison below was `vec![] == vec![]`.
fn operations(dump: &str) -> Vec<String> {
    let mut ops: Vec<String> = dump
        .match_indices('[')
        .filter_map(|(at, _)| operation_ending_at(dump, at))
        .collect();
    ops.sort();

    // Every program compared here reaches the host and does arithmetic on a runtime operand, so an empty result means this function stopped recognizing the printer's spelling rather than that the program emits nothing. Asserting it is what keeps a comparison of two empty vectors from passing as parity — the exact way these tests went quiet before.
    assert!(
        !ops.is_empty(),
        "no instruction recognized in the cont dump: {dump}"
    );

    ops
}

/// The instruction whose operand list opens at `at`, when the text before it is in instruction position.
///
/// Position is what decides it, not the bracket: `apply Known(CpsFunId(13))[…]` and `apply Closure(CpsValueId(95))[…]` end in a bracketed operand list too, and their heads are entropy-derived ids that differ between any two compilations. Accepting those would compare the numbering rather than the code. So the head must sit where `curios_cont::CpsNode`'s `Display` puts an operation: after a `let` binding's `= `, or behind a `cell.`/`intrinsic.` qualifier.
///
/// The qualifier is kept because it selects a different operation enum — `CpsIntrinsicOp::LstMap` and an intrinsic of the same name would otherwise read alike.
fn operation_ending_at(dump: &str, at: usize) -> Option<String> {
    let head = &dump[..at];
    // `TplGet(0)`, `BinLen(X)`, `BinConcat(X, 2)`: step over the parenthesized argument to reach the name.
    let name_end = match head.ends_with(')') {
        true => head.rfind('(')?,
        false => at,
    };
    let name = &head[..name_end];
    let start = name
        .rfind(|c: char| !c.is_ascii_alphanumeric() && c != '_')
        .map_or(0, |boundary| boundary + 1);

    if !name[start..].starts_with(|c: char| c.is_ascii_uppercase()) {
        return None;
    }

    let before = &name[..start];
    ["= ", "cell.", "intrinsic."]
        .into_iter()
        .find(|position| before.ends_with(position))
        .map(|position| match position {
            "= " => head[start..].to_string(),
            qualifier => format!("{qualifier}{}", &head[start..]),
        })
}

/// The comparison concept folds the same way: `Cmp/lt` at `Nat` is the bare `Nat.lt` instruction. Unlike the single-method operators, `Cmp` is a many-method concept whose witness is a *tuple* of methods, so its resolved instance does not newtype-collapse to a bare field — it is baked in by closure specialization (`specialize_calls`), whose `Tpl.get`s then fold to the same intrinsic. The specialized-clone names therefore differ from the direct wrapper's, so the dumps are no longer byte-identical; what must still match is the emitted instructions — the concept path lowers `Cmp/lt` to the bare `Nat.lt` with no witness dispatch left behind, so it emits exactly the direct intrinsic's operations.
#[test]
fn concept_comparison_matches_direct_intrinsic_codegen() {
    let through_concept = r#"
        use /std/{Nat, Bool, Lst, Handle, Str, Cmp, proc};
        pub let small(x : Nat) -> Bool = Cmp/lt(x, 10);
        let taint = Lst/len(proc/args!);
        let n : Nat = taint;
        /std/print(Bool/to_str(small(n)))
        "#;
    let direct = r#"
        use /std/{Nat, Bool, Lst, Handle, Str, proc};
        pub let small(x : Nat) -> Bool = Nat/lt(x, 10);
        let taint = Lst/len(proc/args!);
        let n : Nat = taint;
        /std/print(Bool/to_str(small(n)))
        "#;

    assert_eq!(
        operations(&cont_optm(through_concept)),
        operations(&cont_optm(direct)),
    );
}
