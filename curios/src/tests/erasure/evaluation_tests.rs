//! A proof gives a program no behaviour: an erased proof is never computed, wherever it is bound, while a kept value still is — and a program pays for nothing it does not name.

use {
    crate::tests::{cont_optm, run, run_text},
    curios_runtime::MockHost,
};

// Regression: a `Prop` family is proof-irrelevant, so erasure drops its inhabitants wholesale. Classifying `Eq`'s `refl(@z : A)` payload on its own abstract `A` used to keep it, so rebuilding the constructor computed the field from binders the same erasure had dropped — `Eq/cong` erased to `apply f(unit)`, and a proof bound as a top-level item, which then ran at initialization, fed that unit to a `Bits` fold and trapped. Such an item is no longer computed at all (see `a_top_level_proof_does_not_run_before_the_program`), so what this still holds is the classification.
#[test]
fn proof_bound_as_a_statement_does_not_run_its_certificate() {
    let source = r#"
        use /std/{BigNat, Nat, Str, Eq, Io};
        let a : BigNat = BigNat/of_nat(6);
        let b : BigNat = BigNat/of_nat(7);
        let p : Eq(BigNat/add(a, b), BigNat/add(b, a)) = BigNat/add/comm(a, b);
        /std/print("ok")
        "#;
    assert_eq!(run(source), b"ok");
}

// A proof bound by a local `let` is a kept slot holding nothing: the name is bound and the proof is not computed. The recursion that builds it would otherwise survive to the optimized program as a loop whose result is dropped, since nothing below Core knows a call is total.
#[test]
fn a_let_bound_proof_leaves_no_computation_behind() {
    let source = r#"
        use /std/{Nat, List, Io, proc};
        use /std/Nat/{Le};
        let use_it(a: Nat, b: Nat, _p: Nat/Le(a, b)) -> Nat = a + b;
        Io/bind(proc/args, (args) =>
            let n = List/len(args);
            let p = Le/succ_r(n, n, Le/refl(n));
            proc/exit(use_it(n, n + 1, p)))
        "#;
    let optimized = cont_optm(source);
    assert!(
        !optimized.contains("succ_r"),
        "the proof's recursion reached the optimized program:\n{optimized}"
    );
}

// What a computed proof could still do is refuse: the lemma's first argument leaves the `Nat` carrier at run time. Bound by `let` or written where it is used, the proof is the same proof, so the program runs alike — a proof gives a program no behaviour.
#[test]
fn a_let_bound_proof_cannot_refuse_the_program() {
    let bound = r#"
        use /std/{Nat, List, Io, proc, print};
        use /std/Nat/{Le};
        let lemma(k: Nat, a: Nat) -> Nat/Le(a, a) = Le/refl(a);
        let keep(a: Nat, _p: Nat/Le(a, a)) -> Nat = a;
        Io/bind(proc/args, (args) =>
            let n = List/len(args);
            let p = lemma(n * 1000000 * 1000000, n);
            print(Nat/to_str(keep(n, p))))
        "#;
    let inline = r#"
        use /std/{Nat, List, Io, proc, print};
        use /std/Nat/{Le};
        let lemma(k: Nat, a: Nat) -> Nat/Le(a, a) = Le/refl(a);
        let keep(a: Nat, _p: Nat/Le(a, a)) -> Nat = a;
        Io/bind(proc/args, (args) =>
            let n = List/len(args);
            print(Nat/to_str(keep(n, lemma(n * 1000000 * 1000000, n)))))
        "#;
    for source in [bound, inline] {
        let (system, io) = MockHost::builder().args(["prog", "x"]).build();
        run_text(source, system).expect("a proof gives a program no way to refuse");
        assert_eq!(io.output(), b"2");
    }
}

// A top-level item that is not a function is a value computed at initialization, and a proof there is a kept slot like a local one. Pruning already drops an unused item whose evaluation the erased program calls pure; a lemma that recurses is one it has to keep, since a recursive call may diverge for all it knows, and that lemma ran — here three hundred steps — before the program's first instruction.
#[test]
fn a_top_level_proof_does_not_run_before_the_program() {
    let source = r#"
        use /std/{Nat, print};
        use /std/Nat/{Le};
        let p: Nat/Le(300, 301) = Le/succ_r(300, 300, Le/refl(300));
        print("ok")
        "#;
    let optimized = cont_optm(source);
    assert!(
        !optimized.contains("succ_r"),
        "the proof's recursion reached the optimized program:\n{optimized}"
    );
    assert_eq!(run(source), b"ok");
}

// The control: a binding that is a value is still computed where it is written, used or not, so the same product bound as a `Nat` refuses.
#[test]
fn a_let_bound_value_is_still_computed() {
    let source = r#"
        use /std/{Nat, List, Io, proc, print};
        Io/bind(proc/args, (args) =>
            let n = List/len(args);
            let _unused = n * 1000000 * 1000000;
            print(Nat/to_str(n)))
        "#;
    let (system, _io) = MockHost::builder().args(["prog", "x"]).build();
    let refusal = run_text(source, system).expect_err("the product leaves the carrier");
    assert!(
        refusal.contains("left its carrier"),
        "stopped, but not on the carrier:\n{refusal}"
    );
}

// The same law in an erased position stays non-strict: it is never evaluated, and the consumer still typechecks against it.
#[test]
fn proof_in_an_erased_position_is_not_evaluated() {
    let source = r#"
        use /std/{BigNat, Nat, Str, Eq, Io};
        let a : BigNat = BigNat/of_nat(6);
        let b : BigNat = BigNat/of_nat(7);
        let consume(x : BigNat, y : BigNat, p : Eq(BigNat/add(x, y), BigNat/add(y, x))) -> Nat = 42;
        /std/print(Nat/to_str(consume(a, b, BigNat/add/comm(a, b))))
        "#;
    assert_eq!(run(source), b"42");
}

/// A program pays only for what it names: the standard library's parser web reaches a trivial entry not at all.
///
/// **Pruning is what stands between a program and the whole prelude**, and it was defeated by one spelling. `/std/Json/decode/decode` is a top-level `apply` whose callee is an *alias* of `/std/Parse/bind` rather than a bare function atom, so the effect summary took its conservative top, pruning read the item as observably effectful and kept it — and with it the recursive parser group it names and the entire `Json`/`Parse`/`Flt/of_str` web. Every program carried it: this entry optimized to 3723 lines of Cont, and to 80 once the alias resolves.
///
/// Asserted on the *optimized* Cont, which is the last place anything could still drop it, and by name rather than by size, so the reason a regression fails here is legible.
#[test]
fn a_trivial_program_retains_none_of_the_parser_web() {
    let cont = cont_optm(r#"/std/print("hi\n")"#);

    for absent in [
        "/std/Json/",
        "/std/Parse/",
        "/std/Toml/",
        "/std/Fmt/",
        "/std/Flt/of_str",
    ] {
        assert!(
            !cont.contains(absent),
            "{absent} reaches an entry that names nothing of it:\n{}",
            &cont[..cont.len().min(4000)]
        );
    }
}
