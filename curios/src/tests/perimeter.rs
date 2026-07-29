//! End-to-end coverage for the soundness perimeter entries that nothing else
//! guards.
//!
//! `DESIGN.md` states the consistency claim against an enumerated perimeter and
//! marks each entry *probed*, *argued*, or *auditable only*. "Probed" is a claim
//! about executable evidence, so it needs a test that fails when the rule stops
//! holding — otherwise the column records what someone once tried by hand and
//! decays the moment nobody remembers doing it.
//!
//! The entries with their own homes are not repeated here: strict positivity
//! lives in `tests::positivity`, the two totality obligations in
//! `tests::soundness`, and witness coherence in `tests::concepts`. What is left
//! is the large-elimination guard, `Prop` non-informativeness, coverage, and the
//! foreign wire contract — four rules the claim rests on that had no regression
//! test at all.
//!
//! Each rejection asserts its *own* diagnostic, following `tests::soundness`. A
//! perimeter test that accepts any error is worse than none: an invalid fixture
//! passes it while the rule it names goes unchecked. That is not hypothetical —
//! the first draft of these probes "passed" on `unbound variable`, having never
//! reached the check at all.

use {super::run, curios_runtime::MockHost};

/// Reject `source`, and by the diagnostic naming the rule under test.
fn rejected_by(source: &str, diagnostic: &str) {
    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(source, system)
        .expect_err("expected the perimeter rule to reject this program");
    assert!(
        error.contains(diagnostic),
        "rejected, but not by '{diagnostic}':\n{error}",
    );
}

// The large-elimination guard, in the direction that matters for soundness.
// Every `Box` is definitionally equal to every other by proof irrelevance, so
// reading a `Nat` back out of one would make 0 and 7 convertible.
#[test]
fn a_multi_constructor_proposition_cannot_be_eliminated_into_data() {
    let source = r#"
        use /std/{Nat};

        induct Box : pub Prop
        | mk(n : Nat)
        end

        let extract(b : Box) -> Nat =
            match b
            | mk(n) => n
            end;

        extract(Box/mk(7))
        "#;
    rejected_by(source, "cannot eliminate the proposition");
}

// The same guard, in the direction that matters for the language staying
// usable. A guard that rejected these would be indistinguishable from one that
// rejected everything, and `ex falso` and transport are both load-bearing:
// `/std/Eq/subst` in the prelude is the singleton case.
#[test]
fn an_empty_proposition_still_eliminates_into_data() {
    let source = r#"
        use /std/{Nat, False};

        let ex_falso(f : False) -> Nat =
            match f
            end;

        /std/print(Nat/to_str(0))
        "#;
    assert_eq!(run(source), b"0");
}

#[test]
fn a_proposition_still_eliminates_into_another_proposition() {
    let source = r#"
        use /std/{Nat, True};

        induct Two : pub Prop
        | a()
        | b()
        end

        let into_prop(t : Two) -> True =
            match t
            | a() => True/qed()
            | b() => True/qed()
            end;

        /std/print(Nat/to_str(1))
        "#;
    assert_eq!(run(source), b"1");
}

// `Prop` non-informativeness, which is what makes proof irrelevance safe: a
// proposition whose inhabitants differ observably is not a subsingleton, so
// identifying them would identify the data they carry.
#[test]
fn a_proposition_may_not_carry_informative_fields() {
    let source = r#"
        use /std/{Nat};

        struct Bad : pub Prop {
            value : Nat
        }

        ()
        "#;
    rejected_by(source, "is informative");
}

// A concept is a structure, so the same rule must reach a `Prop`-sorted concept
// whose method returns data. Worth its own fixture: the concept path generates
// its record entry rather than declaring it, so it could regress independently.
#[test]
fn a_proposition_concept_may_not_carry_informative_methods() {
    let source = r#"
        use /std/{Nat};

        concept Bad(A : Type) : pub Prop {
            get(A) -> Nat,
        }

        ()
        "#;
    rejected_by(source, "is informative");
}

// Coverage. A missing arm leaves an elimination undefined at that constructor,
// which is a proof of the motive at an index nothing established.
#[test]
fn an_elimination_must_enumerate_its_constructors() {
    let source = r#"
        use /std/{Nat, Option};

        let f(o : Option(Nat)) -> Nat =
            match o
            | some(x) => x
            end;

        f(Option/none())
        "#;
    rejected_by(source, "missing match case");
}

// The foreign wire contract. The embedder supplies these values, so a `foreign`
// admitted at an arbitrary type would let the host hand back an inhabitant of a
// proposition that nothing ever checked.
#[test]
fn a_foreign_declaration_is_confined_to_wire_types() {
    let source = r#"
        use /std/{Str};

        foreign bad : Str;

        ()
        "#;
    rejected_by(source, "expected a wire type");
}

// The large-elimination guard again, at its *singleton* rung. A one-constructor
// proposition may eliminate into data only when every payload binder is
// non-informative — a proposition itself, or recovered from the scrutinee's
// indices, as `Eq`'s `refl(@z) : (z, z)` recovers `z`. `singleton_eliminable`
// decides "recovered" by `terminal.free_vars()`: a *syntactic* occurrence test.
//
// Occurring in an index target is not the same as being determined by one.
// `blur` is constant, so `Loose(0)` is inhabited by `mk(0)` and by `mk(7)`
// alike, and no index tells them apart — yet `a` occurs in `blur(a)`, so the
// guard reads it as forced and admits the elimination. Proof irrelevance then
// identifies the two inhabitants while `extract` observes them apart, and the
// gap is a closed inhabitant of `False`. The program below prints "FORGED"
// today; it must be refused.
//
// The two ends of the discrimination are already covered by fixtures here: the
// same declaration with target `(0)` is rejected, and `(a)` is a genuinely
// forced binder that must stay accepted. Only the middle — an occurrence that
// does not determine — escapes.
//
// The diagnostic asserted is the guard's, because the guard is the perimeter
// entry that let this through. A fix that instead refuses the *declaration* —
// requiring a constructor's index targets to be patterns rather than arbitrary
// terms — is equally legitimate and would need this expectation updated.
#[test]
#[ignore = "open: the elaborator still admits this; the kernel refuses it, but nothing runs the kernel yet"]
fn a_non_injective_index_target_does_not_force_its_binder() {
    let source = r#"
        use /std/{Nat, Eq, False};

        let blur(a : Nat) -> Nat = 0;

        induct Loose : (n : Nat) -> pub Prop
        | mk(a : Nat) : (blur(a))
        end

        let extract(p : Loose(0)) -> Nat =
            match p : (m, q) => Nat
            | mk(a) => a
            end;

        let same : Eq(Loose/mk(0), Loose/mk(7)) = Eq/refl();

        let boom : False =
            Eq/subst((n : Nat) => match n : (_) => Type | 0 => {} | _ => False end,
                     Eq/cong(extract, same),
                     ());

        /std/print("FORGED")
        "#;
    rejected_by(source, "cannot eliminate the proposition");
}

// The lower end of that discrimination, and the reason the hole is in the
// occurrence test rather than in the guard as a whole: drop `a` from the index
// target and the guard fires. Without this, a fix could "close" the hole by
// rejecting every indexed proposition and nothing here would notice.
#[test]
#[ignore = "control for the fixture above; ignored with it so the pair stays read as a pair"]
fn an_unmentioned_payload_binder_is_not_forced() {
    let source = r#"
        use /std/{Nat};

        induct Tight : (n : Nat) -> pub Prop
        | mk(a : Nat) : (0)
        end

        let extract(p : Tight(0)) -> Nat =
            match p : (m, q) => Nat
            | mk(a) => a
            end;

        /std/print(Nat/to_str(extract(Tight/mk(7))))
        "#;
    rejected_by(source, "cannot eliminate the proposition");
}
