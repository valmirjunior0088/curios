//! Codegen parity for concept-dispatched operators (the witness-projection
//! folding gate). No dedicated folding rule exists in core — none is needed:
//! after erasure the single-method dictionary collapses to its bare field
//! (the method wrapper erases to the identity), `add_nat` delta-reduces to
//! the named `Nat/add` wrapper, and the cont inliner beta-reduces the chain
//! down to the bare primitive instruction. These tests pin that pipeline
//! behavior: a concept method call at a concrete primitive type must compile
//! to the *same* cont code as calling the primitive wrapper directly, modulo
//! generated-name counters. They gate (and then guard) the infix rewrite that
//! routes every operator through the concepts.

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
        &text::NullLoader,
        |stage| {
            if let Stage::ContOptm(module) = stage {
                dump = module.to_string();
            }
        },
    )
    .expect("parity source compiles");

    dump.chars()
        .map(|c| if c.is_ascii_digit() { '#' } else { c })
        .collect::<String>()
        // Collapse counter-length differences (`%v9` vs `%v10`).
        .replace("##", "#")
        .replace("##", "#")
}

/// `Add/add(x, 1)` at `Nat` — dictionary resolved from the sys witness —
/// compiles to the same optimized cont code as the direct primitive wrapper
/// call, over a runtime (non-constant-foldable) operand.
#[test]
fn concept_method_call_matches_direct_primitive_codegen() {
    let through_concept = r#"
        use /std/{Nat, Arr, Io, Str, Add, Proc};
        pub let bump(x : Nat) -> Nat = Add/add(x, 1);
        let n : Nat = Arr/len(Proc/args());
        Io/print(Nat/to_str(bump(n)))
        "#;
    let direct = r#"
        use /std/{Nat, Arr, Io, Str, Proc};
        pub let bump(x : Nat) -> Nat = Nat/add(x, 1);
        let n : Nat = Arr/len(Proc/args());
        Io/print(Nat/to_str(bump(n)))
        "#;

    assert_eq!(
        normalized_cont_optm(through_concept),
        normalized_cont_optm(direct)
    );
}

/// The comparison concept folds the same way: `Cmp/lt` at `Nat` is the bare
/// `Nat.lt` instruction.
#[test]
fn concept_comparison_matches_direct_primitive_codegen() {
    let through_concept = r#"
        use /std/{Nat, Bln, Arr, Io, Str, Cmp, Proc};
        pub let small(x : Nat) -> Bln = Cmp/lt(x, 10);
        let n : Nat = Arr/len(Proc/args());
        Io/print(Bln/to_str(small(n)))
        "#;
    let direct = r#"
        use /std/{Nat, Bln, Arr, Io, Str, Proc};
        pub let small(x : Nat) -> Bln = Nat/lt(x, 10);
        let n : Nat = Arr/len(Proc/args());
        Io/print(Bln/to_str(small(n)))
        "#;

    assert_eq!(
        normalized_cont_optm(through_concept),
        normalized_cont_optm(direct)
    );
}
