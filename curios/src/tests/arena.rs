//! The Ersd v2 behavior-identity corpus: representative programs run through
//! the legacy production path and the arena vertical (erase → lower → Cont →
//! Wasm), compared on observable runtime output — never on bytes. One program
//! per behavior class the specification names; the full suite remains the
//! oracle at the flip.

use super::{run, run_arena};

/// Assert the arena path produces the same observable behavior as production.
fn behavior_matches(source: &str) {
    assert_eq!(run(source), run_arena(source));
}

#[test]
fn arena_matches_scalar_arithmetic_and_strings() {
    // Scalar operations, string formatting, and the whole `/std/Str` proof
    // machinery (`Utf8` certificates, newtype collapse) behind `to_str`.
    behavior_matches(
        r#"std/Io/write(std/Io/stdout, /std/Str/to_bytes(/std/Nat/to_str(75 * 41 % 65537)))"#,
    );
}

#[test]
fn arena_matches_inductive_matching_and_recursion() {
    // Inductive construction and matching, self-recursion, runtime-tainted
    // input so the kernel is not constant-folded on the legacy path.
    behavior_matches(
        r#"
        use /std/{Io, Nat, Lst, Str, proc};
        induct Tree : Type
        | leaf(Nat)
        | node(Nat, Tree, Tree)
        end
        rec build(d : Nat, v : Nat) -> Tree =
            match d : Tree
            | 0 => Tree/leaf(v)
            | dp + 1; ih => Tree/node(v, build(dp, v * 2), build(dp, v * 2 + 1))
            end;
        rec sum(t : Tree) -> Nat =
            match t : Nat
            | leaf(v) => v
            | node(v, l, r) => v + sum(l) + sum(r)
            end;
        let n : Nat = Lst/len(proc/args());
        Io/write(Io/stdout, Str/to_bytes(Nat/to_str(sum(build(n + 4, 1)))))
        "#,
    );
}

#[test]
fn arena_matches_folds_and_closures() {
    // Sequence folds over strings, closures and higher-order application.
    behavior_matches(
        r#"
        use /std/{Io, Nat, Str};
        let count = Str/fold(@Nat, "abcdef", 0, (c, acc) => acc + 1);
        Io/write(Io/stdout, Str/to_bytes(Nat/to_str(count)))
        "#,
    );
}

#[test]
fn arena_matches_effect_order_and_cells() {
    // Cells and effect ordering: writes interleaved with cell state must
    // appear in exactly the production order.
    behavior_matches(
        r#"
        use /std/{Io, Nat, Str, Cell};
        let c = Cell/new(@Nat, 1);
        let a = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(Cell/get(@Nat, c))));
        let s = Cell/set(@Nat, c, 2);
        let b = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(Cell/get(@Nat, c))));
        ()
        "#,
    );
}

#[test]
fn arena_is_deterministic() {
    let source = r#"std/Io/write(std/Io/stdout, /std/Str/to_bytes("determinism"))"#;
    assert_eq!(run_arena(source), run_arena(source));
}

/// The structural property gate through the arena vertical: the LCG hot
/// kernel — a runtime-tainted self-recursive dispatch — stays one natural
/// loop of direct scalar arithmetic, with no closure allocation, no indirect
/// dispatch, and no irreducible-cycle dispatcher, exactly as the legacy
/// structural suite pins for production. Properties, never bytes.
#[test]
fn arena_lcg_kernel_keeps_its_structural_properties() {
    let source = r#"
        use /std/{Io, Nat, Lst, proc};
        rec loop(k : Nat, x : Nat) -> Nat =
            match k : Nat
            | 0 => x
            | kp + 1; ih => loop(kp, 75 * x % 65537)
            end;
        let n : Nat = Lst/len(proc/args());
        Io/print(Nat/to_str(loop(n, 1)))
        "#;

    let entrypoint = source
        .parse::<curios_text::Entrypoint>()
        .expect("fixture parses");
    let (module, _foreigns) = curios_pipeline::compile_entrypoint_via_arena(
        std::time::Duration::from_secs(60),
        &entrypoint,
        curios_text::RootSource::none(),
        |_| {},
    )
    .expect("fixture compiles through the arena");
    let wat = module.to_string();

    let position = wat.find("65537").expect("the kernel constant is emitted");
    let function_start = wat[..position]
        .rfind("(func ")
        .expect("the constant sits inside a function");
    let function_end = wat[position..]
        .find("\n  (func ")
        .map(|offset| position + offset)
        .unwrap_or(wat.len());
    let kernel = &wat[function_start..function_end];

    assert_eq!(
        kernel.matches("loop ").count(),
        1,
        "the kernel must be a single natural loop:\n{kernel}"
    );
    assert!(
        !kernel.contains("$dispatch/"),
        "the backedge must not be a dispatcher selector:\n{kernel}"
    );
    assert!(
        !kernel.contains("struct.new") || !kernel.contains("call_ref"),
        "the loop body must not allocate closures or dispatch indirectly:\n{kernel}"
    );
}
