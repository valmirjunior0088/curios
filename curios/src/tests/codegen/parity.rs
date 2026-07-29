//! Codegen parity for concept-dispatched operators (the witness-projection
//! folding gate). No dedicated folding rule exists in core — none is needed:
//! after erasure a witness is a bakeable value (a one-method dictionary collapses
//! to its bare method field, a many-method one is a tuple of methods), so its
//! resolved instance argument is a specialization candidate. Closure specialization
//! bakes it into the caller, the `Tpl.get`s (for a tuple witness) fold to the known
//! methods, and the cont inliner beta-reduces the chain down to the bare primitive
//! instruction. These tests pin that pipeline behavior: a concept method call at a
//! concrete primitive type must emit the *same operations* as calling the primitive
//! wrapper directly. The generated names differ — the concept path threads the
//! witness through an extra specialization, decorating clone names with its key —
//! but that provenance names no instruction, so the emitted operations match
//! exactly. These gate (and then guard) the infix rewrite that routes every operator
//! through the concepts.

use {
    curios_pipeline::{Stage, compile_entrypoint},
    curios_text::{Entrypoint, RootSource},
};

/// The optimized cont-stage dump for `source`, with every digit run replaced
/// by `#`: entropy-derived name counters (`~v37`, `~f26`) are the
/// only expected difference between the compared programs, and both sources
/// use identical numeric literals, so digit-blind comparison is exact for
/// everything that matters.
fn normalized_cont_optm(source: &str) -> String {
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

    // Collapse every digit run to one `#`, so counter-length differences
    // (`~v9` vs `~v100`) normalize away.
    let mut normalized = String::with_capacity(dump.len());
    let mut in_digits = false;
    for c in dump.chars() {
        if c.is_ascii_digit() {
            if !in_digits {
                normalized.push('#');
            }
            in_digits = true;
        } else {
            normalized.push(c);
            in_digits = false;
        }
    }
    normalized
}

/// `Add/add(x, 1)` at `Nat` — dictionary resolved from the sys witness — emits the
/// same operations as the direct primitive wrapper call, over a runtime
/// (non-constant-foldable) operand. `Add` is a one-method concept, so its witness
/// erases to the bare `Nat/add` closure; the resolved instance is baked in by
/// specialization and inlined to `Nat.add`. The clone names carry the extra witness
/// key, so the dumps are not byte-identical — but the emitted operations are.
#[test]
fn concept_method_call_matches_direct_primitive_codegen() {
    let through_concept = r#"
        use /std/{Nat, Lst, Handle, Str, Add, proc};
        pub let bump(x : Nat) -> Nat = Add/add(x, 1);
        let n : Nat = Lst/len(proc/args());
        /std/print(Nat/to_str(bump(n)))
        "#;
    let direct = r#"
        use /std/{Nat, Lst, Handle, Str, proc};
        pub let bump(x : Nat) -> Nat = Nat/add(x, 1);
        let n : Nat = Lst/len(proc/args());
        /std/print(Nat/to_str(bump(n)))
        "#;

    assert_eq!(
        operations(&normalized_cont_optm(through_concept)),
        operations(&normalized_cont_optm(direct)),
    );
}

/// `choose` desugars to exactly the nested boolean matches a user would
/// hand-write: `choose | c0 => b0 | c1 => b1 | _ => d end` is
/// `match c0 | true => b0 | false => match c1 | true => b1 | false => d end end`.
/// Both lower through the same core `bool_match` nesting, so they emit the same
/// primitive operations — the two forms mint metavars in a slightly different
/// order, which only permutes the emission order of the top-level specialized
/// closures (their bodies are identical), so `operations()` is the exact
/// comparison. A runtime operand (`Lst/len(proc/args())`) keeps the ladder from
/// folding to a constant.
#[test]
fn choose_matches_hand_nested_bool_codegen() {
    let ladder = r#"
        use /std/{Nat, Lst, Handle, Str, proc};
        let n : Nat = Lst/len(proc/args());
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
        let n : Nat = Lst/len(proc/args());
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
        operations(&normalized_cont_optm(ladder)),
        operations(&normalized_cont_optm(nested)),
    );
}

/// A single-refutation bind arm `| some(x) = o => …` desugars to exactly the
/// headed catch-all `match o | some(x) => … | _ => …` — both a single-row
/// inductive match with the same default — so they emit identical operations.
#[test]
fn choose_bind_arm_matches_headed_catch_all_codegen() {
    let bind = r#"
        use /std/{Option, Nat, Lst, Handle, Str, proc};
        let f(o : Option(Nat)) -> Nat =
            choose
            | some(x) = o => x + 10
            | _ => 99
            end;
        let n : Nat = Lst/len(proc/args());
        /std/print(Nat/to_str(f(Option/some(n))))
        "#;
    let headed = r#"
        use /std/{Option, Nat, Lst, Handle, Str, proc};
        let f(o : Option(Nat)) -> Nat =
            match o
            | some(x) => x + 10
            | _ => 99
            end;
        let n : Nat = Lst/len(proc/args());
        /std/print(Nat/to_str(f(Option/some(n))))
        "#;

    assert_eq!(
        operations(&normalized_cont_optm(bind)),
        operations(&normalized_cont_optm(headed)),
    );
}

/// The primitive operations emitted by a cont dump, sorted — the `Kind.op` tokens
/// (`Nat.lt`, `Tpl.get`, `Lst.len`, …) that are the actual instructions, ignoring
/// the generated names that wire them together. A generated name is never
/// `Uppercase.lowercase`, so this picks out exactly the primitive ops.
fn operations(dump: &str) -> Vec<String> {
    let mut ops: Vec<String> = dump
        .split(|c: char| !(c.is_ascii_alphanumeric() || c == '.' || c == '_'))
        .filter(|token| {
            let mut parts = token.splitn(2, '.');
            matches!(
                (parts.next(), parts.next()),
                (Some(kind), Some(op))
                    if kind.chars().next().is_some_and(|c| c.is_ascii_uppercase())
                        && !op.is_empty()
                        && op.chars().all(|c| c.is_ascii_lowercase() || c == '_')
            )
        })
        .map(str::to_string)
        .collect();
    ops.sort();
    ops
}

/// The comparison concept folds the same way: `Cmp/lt` at `Nat` is the bare
/// `Nat.lt` instruction. Unlike the single-method operators, `Cmp` is a
/// many-method concept whose witness is a *tuple* of methods, so its resolved
/// instance does not newtype-collapse to a bare field — it is baked in by closure
/// specialization (`specialize_calls`), whose `Tpl.get`s then
/// fold to the same primitive. The specialized-clone names therefore differ from
/// the direct wrapper's, so the dumps are no longer byte-identical; what must still
/// match is the emitted instructions — the concept path lowers `Cmp/lt` to the bare
/// `Nat.lt` with no witness dispatch left behind, so it emits exactly the direct
/// primitive's operations.
#[test]
fn concept_comparison_matches_direct_primitive_codegen() {
    let through_concept = r#"
        use /std/{Nat, Bool, Lst, Handle, Str, Cmp, proc};
        pub let small(x : Nat) -> Bool = Cmp/lt(x, 10);
        let n : Nat = Lst/len(proc/args());
        /std/print(Bool/to_str(small(n)))
        "#;
    let direct = r#"
        use /std/{Nat, Bool, Lst, Handle, Str, proc};
        pub let small(x : Nat) -> Bool = Nat/lt(x, 10);
        let n : Nat = Lst/len(proc/args());
        /std/print(Bool/to_str(small(n)))
        "#;

    assert_eq!(
        operations(&normalized_cont_optm(through_concept)),
        operations(&normalized_cont_optm(direct)),
    );
}
