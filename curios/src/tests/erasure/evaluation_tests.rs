//! Erasure removes proofs; optimization removes unused total values, but preserves calls that may diverge — and a program pays for nothing it does not name.

use crate::tests::{cont_optm, run};

// A `Prop` family is proof-irrelevant, so erasure drops its inhabitants wholesale. Classifying `Eq`'s `refl(@z : A)` payload on its own abstract `A` would keep it, and rebuilding the constructor would compute the field from binders the same erasure had dropped — `Eq/cong` erasing to `apply f(unit)`. A proof bound as a top-level item is not computed at all (see `a_top_level_proof_does_not_run_before_the_program`), so what this holds is the classification.
#[test]
fn proof_bound_as_a_statement_does_not_run_its_certificate() {
    let source = r#"
        use /std/{Nat, Eq};
        use /std/Nat/{le};
        let a : Nat = 6;
        let b : Nat = 7;
        let p : Eq()(a + (b - a), b) = le/add_sub_cancel(a, b, le/add_r(a, 1));
        /std/print("ok")
        "#;
    assert_eq!(run(source), b"ok");
}

// A proof bound by a local `let` is a kept slot holding nothing: the name is bound and the proof is not computed. Its certificate contributes no runtime recursion.
#[test]
fn a_let_bound_proof_leaves_no_computation_behind() {
    let source = r#"
        use /std/{Nat, List, Io, proc};
        use /std/Bool/{True};
        use /std/Nat/{le};
        use /std/Bool/{Holds};
        let use_it(a: Nat, b: Nat, _p: Holds(a <= b)) -> Nat = a + b;
        Io/bind(proc/args, (args) =>
            let n = List/len(args);
            let p = le/trans(@n, @n, @n + 1, le/refl(n), True/qed());
            proc/exit(Nat/to_byte(use_it(n, n + 1, p) % 256)))
        "#;
    let optimized = cont_optm(source);
    assert!(
        !optimized.contains("trans"),
        "the proof's recursion reached the optimized program:\n{optimized}"
    );
}

// A proof bound as a top-level item contributes no initialization work. Its certificate is erased before the program begins, wherever the proof is bound.
#[test]
fn a_top_level_proof_does_not_run_before_the_program() {
    let source = r#"
        use /std/{Nat, print};
        use /std/Bool/{True};
        use /std/Nat/{le};
        use /std/Bool/{Holds};
        let p: Holds(300 <= 301) = le/trans(@300, @300, @301, le/refl(300), True/qed());
        print("ok")
        "#;
    let optimized = cont_optm(source);
    assert!(
        !optimized.contains("trans"),
        "the proof's recursion reached the optimized program:\n{optimized}"
    );
    assert_eq!(run(source), b"ok");
}

// The termination verdict reaches the backend through `Function::droppable`: a total call with no observable effects and an unread result may be omitted even when its result is a value rather than a proof.
#[test]
fn an_unused_total_value_leaves_no_computation_behind() {
    let source = r#"
        use /std/{Nat, List, Io, proc};
        let count(a: Nat, b: Nat) -> Nat =
            match a | 0 => b | ap + 1 => count(ap, b + 1) end;
        Io/bind(proc/args, (args) =>
            let n = List/len(args);
            let _unused = count(n, n);
            proc/exit(Nat/to_byte(n % 256)))
        "#;
    let optimized = cont_optm(source);
    assert!(
        !optimized.contains("count"),
        "the unused total value's recursion reached the optimized program:\n{optimized}"
    );
}

// An unread result alone does not license omission: retaining a call that may diverge preserves whether the program reaches its exit.
#[test]
fn an_unused_partial_value_is_still_computed() {
    let source = r#"
        use /std/{Nat, List, Io, proc};
        let count(a: Nat, b: Nat) -> Nat =
            match a | 0 => b | _ + 1 => count(a, b + 1) end;
        Io/bind(proc/args, (args) =>
            let n = List/len(args);
            let _unused = count(n, n);
            proc/exit(Nat/to_byte(n % 256)))
        "#;
    let optimized = cont_optm(source);
    assert!(
        optimized.contains("count"),
        "the partial value's recursion was dropped from the optimized program:\n{optimized}"
    );
}

// The same law in an erased position stays non-strict: it is never evaluated, and the consumer still typechecks against it.
#[test]
fn proof_in_an_erased_position_is_not_evaluated() {
    let source = r#"
        use /std/{Nat, Eq};
        use /std/Nat/{le};
        let a : Nat = 6;
        let b : Nat = 7;
        let consume(x : Nat, y : Nat, p : Eq()(x + (y - x), y)) -> Nat = 42;
        /std/print(Nat/to_str(consume(a, b, le/add_sub_cancel(a, b, le/add_r(a, 1)))))
        "#;
    assert_eq!(run(source), b"42");
}

/// A program pays only for what it names: the standard library's parser web reaches a trivial entry not at all.
///
/// **Pruning is what stands between a program and everything the library computes as it is initialized**, and one spelling can defeat it: `/std/Json/decode/decode` is a top-level `apply` whose callee is an *alias* of `/std/Parse/bind` rather than a bare function atom, and an effect summary taking its conservative top there would read the item as observably effectful and keep it — and with it the recursive parser group it names and the entire `Json`/`Parse`/`Flt/of_str` web, in every program.
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

// A recursion on a bound runs as its step and nothing else: the proofs that keep the measure under the bound are erased, and `descend` is a wrapper around a local loop, so inlining the wrapper fuses the step into the loop. A `descend` passing `step` to itself would survive to the optimized program as a function of its own, calling the step through a closure at every level.
#[test]
fn a_recursion_on_a_bound_is_fused_with_its_step() {
    let source = r#"
        use /std/{Nat, List, Io, proc};
        use /std/Bool/{True};
        let sum_to(n: Nat) -> Nat =
            Nat/descend(
                (k: Nat) => k,
                (_) => Nat,
                (k, ih) => match k | 0 => 0 | kp + 1 => k + ih(kp, True/qed()) end,
                n);
        Io/bind(proc/args, (args) => proc/exit(Nat/to_byte(sum_to(List/len(args)) % 256)))
        "#;
    let optimized = cont_optm(source);
    assert!(
        !optimized.contains("Nat/descend"),
        "the fixpoint survived the step it should have fused:\n{optimized}"
    );
}
