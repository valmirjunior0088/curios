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
    crate::{Stage, compile_entrypoint, text},
    std::time::Duration,
};

/// The optimized cont-stage dump for `source`, with every digit run replaced
/// by `#`: entropy-derived name counters (`%v37`, `b@c26@lifted#7`) are the
/// only expected difference between the compared programs, and both sources
/// use identical numeric literals, so digit-blind comparison is exact for
/// everything that matters.
fn normalized_cont_optm(source: &str) -> String {
    let entrypoint = source
        .parse::<text::Entrypoint>()
        .expect("parity source parses");

    let mut dump = String::new();
    compile_entrypoint(
        Duration::from_secs(10),
        &entrypoint,
        text::RootSource::None,
        |stage| {
            if let Stage::ContOptm(module) = stage {
                dump = module.to_string();
            }
        },
    )
    .expect("parity source compiles");

    // Collapse every digit run to one `#`, so counter-length differences
    // (`%v9` vs `%v100`) normalize away.
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
        use /std/{Nat, Lst, Io, Str, Add, Proc};
        pub let bump(x : Nat) -> Nat = Add/add(x, 1);
        let n : Nat = Lst/len(Proc/args());
        Io/print(Nat/to_str(bump(n)))
        "#;
    let direct = r#"
        use /std/{Nat, Lst, Io, Str, Proc};
        pub let bump(x : Nat) -> Nat = Nat/add(x, 1);
        let n : Nat = Lst/len(Proc/args());
        Io/print(Nat/to_str(bump(n)))
        "#;

    assert_eq!(
        operations(&normalized_cont_optm(through_concept)),
        operations(&normalized_cont_optm(direct)),
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
/// specialization ([`specialize_calls`](crate::cont::optm)), whose `Tpl.get`s then
/// fold to the same primitive. The specialized-clone names therefore differ from
/// the direct wrapper's, so the dumps are no longer byte-identical; what must still
/// match is the emitted instructions — the concept path lowers `Cmp/lt` to the bare
/// `Nat.lt` with no witness dispatch left behind, so it emits exactly the direct
/// primitive's operations.
#[test]
fn concept_comparison_matches_direct_primitive_codegen() {
    let through_concept = r#"
        use /std/{Nat, Bln, Lst, Io, Str, Cmp, Proc};
        pub let small(x : Nat) -> Bln = Cmp/lt(x, 10);
        let n : Nat = Lst/len(Proc/args());
        Io/print(Bln/to_str(small(n)))
        "#;
    let direct = r#"
        use /std/{Nat, Bln, Lst, Io, Str, Proc};
        pub let small(x : Nat) -> Bln = Nat/lt(x, 10);
        let n : Nat = Lst/len(Proc/args());
        Io/print(Bln/to_str(small(n)))
        "#;

    assert_eq!(
        operations(&normalized_cont_optm(through_concept)),
        operations(&normalized_cont_optm(direct)),
    );
}
