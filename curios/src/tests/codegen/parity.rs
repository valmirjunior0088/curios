//! Codegen parity for concept-dispatched operators (the witness-projection folding gate). No dedicated folding rule exists in core — none is needed: after erasure a witness is a bakeable value (a one-method dictionary collapses to its bare method field, a many-method one is a tuple of methods), so its resolved instance argument is a specialization candidate. Closure specialization bakes it into the caller, the `Tuple.get`s (for a tuple witness) fold to the known methods, and the cont inliner beta-reduces the chain down to the bare intrinsic instruction. These tests pin that pipeline behavior: a concept method call at a concrete intrinsic type must emit the *same operations* as calling the intrinsic wrapper directly. The generated names differ — the concept path threads the witness through an extra specialization, decorating clone names with its key — but that provenance names no instruction, so the emitted operations match exactly. These gate (and then guard) the infix rewrite that routes every operator through the concepts.

use crate::tests::{cont_operations, cont_optm_module};

/// The operations `source` compiles to, refusing an empty answer.
///
/// Every program compared here reaches the host and does arithmetic on a runtime operand, so emptiness means the extraction stopped seeing what the stage emits rather than that the program emits nothing. The guard exists because these comparisons went quiet exactly that way once, when the extraction read the printed dump and a printer change left it matching nothing: `vec![] == vec![]` passed for a stretch. Reading the graph removes that failure mode; the guard stays because a vacuous pass is worth refusing whatever causes it.
fn operations(source: &str) -> Vec<String> {
    let operations = cont_operations(&cont_optm_module(source));
    assert!(
        !operations.is_empty(),
        "no operation in the optimized module for:\n{source}"
    );
    operations
}

/// `Add/add(x, 1)` at `Nat` — dictionary resolved from the sys witness — emits the same operations as the direct intrinsic wrapper call, over a runtime (non-constant-foldable) operand. `Add` is a one-method concept, so its witness erases to the bare `Nat/add` closure; the resolved instance is baked in by specialization and inlined to `Nat.add`. The clone names carry the extra witness key, so the dumps are not byte-identical — but the emitted operations are.
#[test]
fn concept_method_call_matches_direct_intrinsic_codegen() {
    let through_concept = r#"
        use /std/{Nat, List, Str, proc};
        use /std/ops/{Add};
        pub let bump(x : Nat) -> Nat = Add/add(x, 1);
        let taint = List/len(proc/args!);
        let n : Nat = taint;
        /std/print(Nat/to_str(bump(n)))
        "#;
    let direct = r#"
        use /std/{Nat, List, Str, proc};
        pub let bump(x : Nat) -> Nat = Nat/add(x, 1);
        let taint = List/len(proc/args!);
        let n : Nat = taint;
        /std/print(Nat/to_str(bump(n)))
        "#;

    assert_eq!(operations(through_concept), operations(direct),);
}

/// `choose` desugars to exactly the nested boolean matches a user would hand-write: `choose | c0 => b0 | c1 => b1 | _ => d end` is `match c0 | true => b0 | false => match c1 | true => b1 | false => d end end`. Both lower through the same core `bool_match` nesting, so they emit the same intrinsic operations — the two forms mint metavars in a slightly different order, which only permutes the emission order of the top-level specialized closures (their bodies are identical), so `operations()` is the exact comparison. A runtime operand (`List/len(proc/args!)`) keeps the ladder from folding to a constant.
#[test]
fn choose_matches_hand_nested_bool_codegen() {
    let ladder = r#"
        use /std/{Nat, List, Str, proc};
        let taint = List/len(proc/args!);
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
        use /std/{Nat, List, Str, proc};
        let taint = List/len(proc/args!);
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

    assert_eq!(operations(ladder), operations(nested),);
}

/// A single-refutation bind arm `| some(x) = o => …` desugars to exactly the headed catch-all `match o | some(x) => … | _ => …` — both a single-row inductive match with the same default — so they emit identical operations.
#[test]
fn choose_bind_arm_matches_headed_catch_all_codegen() {
    let bind = r#"
        use /std/{Option, Nat, List, Str, proc};
        let f(o : Option(Nat)) -> Nat =
            choose
            | some(x) = o => x + 10
            | _ => 99
            end;
        let taint = List/len(proc/args!);
        let n : Nat = taint;
        /std/print(Nat/to_str(f(Option/some(n))))
        "#;
    let headed = r#"
        use /std/{Option, Nat, List, Str, proc};
        let f(o : Option(Nat)) -> Nat =
            match o
            | some(x) => x + 10
            | _ => 99
            end;
        let taint = List/len(proc/args!);
        let n : Nat = taint;
        /std/print(Nat/to_str(f(Option/some(n))))
        "#;

    assert_eq!(operations(bind), operations(headed),);
}

/// The comparison concept folds the same way: `Cmp/lt` at `Nat` is the bare `Nat.lt` instruction. Unlike the single-method operators, `Cmp` is a many-method concept whose witness is a *tuple* of methods, so its resolved instance does not newtype-collapse to a bare field — it is baked in by closure specialization, whose `Tuple.get`s then fold to the same intrinsic. The specialized-clone names therefore differ from the direct wrapper's, so the dumps are no longer byte-identical; what must still match is the emitted instructions — the concept path lowers `Cmp/lt` to the bare `Nat.lt` with no witness dispatch left behind, so it emits exactly the direct intrinsic's operations.
#[test]
fn concept_comparison_matches_direct_intrinsic_codegen() {
    let through_concept = r#"
        use /std/{Nat, Bool, List, Str, proc};
        use /std/ops/{Cmp};
        pub let small(x : Nat) -> Bool = Cmp/lt(x, 10);
        let taint = List/len(proc/args!);
        let n : Nat = taint;
        /std/print(Bool/to_str(small(n)))
        "#;
    let direct = r#"
        use /std/{Nat, Bool, List, Str, proc};
        pub let small(x : Nat) -> Bool = Nat/lt(x, 10);
        let taint = List/len(proc/args!);
        let n : Nat = taint;
        /std/print(Bool/to_str(small(n)))
        "#;

    assert_eq!(operations(through_concept), operations(direct),);
}
