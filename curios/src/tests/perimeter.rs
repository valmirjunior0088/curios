//! End-to-end coverage for the soundness perimeter entries that nothing else guards.
//!
//! `DESIGN.md` states the consistency claim against an enumerated perimeter and marks each entry *probed*, *argued*, or *auditable only*. "Probed" is a claim about executable evidence, so it needs a test that fails when the rule stops holding — otherwise the column records what someone once tried by hand and decays the moment nobody remembers doing it.
//!
//! The entries with their own homes are not repeated here: strict positivity lives in `tests::positivity`, the two totality obligations in `tests::soundness`, and witness coherence in `tests::concepts`. What is left is the large-elimination guard, `Prop` non-informativeness, coverage, and the foreign wire contract — four rules the claim rests on that had no regression test at all.
//!
//! Each rejection asserts its *own* diagnostic, following `tests::soundness`. A perimeter test that accepts any error is worse than none: an invalid fixture passes it while the rule it names goes unchecked. That is not hypothetical — the first draft of these probes "passed" on `unbound variable`, having never reached the check at all.

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

// The large-elimination guard, in the direction that matters for soundness. Every `Box` is definitionally equal to every other by proof irrelevance, so reading a `Nat` back out of one would make 0 and 7 convertible.
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

// The same guard, in the direction that matters for the language staying usable. A guard that rejected these would be indistinguishable from one that rejected everything, and `ex falso` and transport are both load-bearing: `/std/Eq/subst` in the prelude is the singleton case.
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

// `Prop` non-informativeness, which is what makes proof irrelevance safe: a proposition whose inhabitants differ observably is not a subsingleton, so identifying them would identify the data they carry.
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

// A concept is a structure, so the same rule must reach a `Prop`-sorted concept whose method returns data. Worth its own fixture: the concept path generates its record entry rather than declaring it, so it could regress independently.
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

// Coverage. A missing arm leaves an elimination undefined at that constructor, which is a proof of the motive at an index nothing established.
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

// The foreign wire contract. The embedder supplies these values, so a `foreign` admitted at an arbitrary type would let the host hand back an inhabitant of a proposition that nothing ever checked.
#[test]
fn a_foreign_declaration_is_confined_to_wire_types() {
    let source = r#"
        use /std/{Str};

        foreign bad : Str;

        ()
        "#;
    rejected_by(source, "expected a wire type");
}

// The large-elimination guard again, at its *singleton* rung. A one-constructor proposition may eliminate into data only when every payload binder is non-informative — a proposition itself, or *pinned* by the constructor's index targets, as `Eq`'s `refl(@z) : (z, z)` recovers `z`.
//
// Occurring in an index target is not the same as being determined by one. `blur` is constant, so `Loose(0)` is inhabited by `mk(0)` and by `mk(7)` alike, and no index tells them apart — proof irrelevance identifies the two inhabitants while `extract` would observe them apart, and the gap is a closed inhabitant of `False`. `singleton_eliminable` once read `a` as forced because it *occurs* in `blur(a)` — a syntactic occurrence test — and this program printed "FORGED". Both checkers now decide the condition by the shared `pinned_by_targets` walk: a binder counts only when matching a value against the target recovers it, which `blur(a)` never does.
//
// The two ends of the discrimination are covered alongside: the same declaration with target `(0)` is rejected below, and `(a)` is a genuinely forced binder that must stay accepted.
#[test]
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

// Proof irrelevance and index inversion disagree about a `Prop`-valued index, and the disagreement is a closed inhabitant of `False`. Conversion identifies `Two/a()` with `Two/b()` — any two inhabitants of a proposition are equal — so `Ind(Two/a())` and `Ind(Two/b())` are the same type and `coerce` is well typed. Inversion decides a case is impossible by *syntactic* constructor clash (`invert_indices` decomposes constructor forms and clashes on distinct tags, with no sort condition), so it reads `only`'s target `Two/a()` against the actual index `Two/b()` as disjoint and accepts the arm-less elimination as vacuous — at a type conversion just proved inhabited.
//
// Verified against the built compiler: this source compiles (`curios compile` exits 0, and `recheck_module_suffix` on the compile path certifies `let /bad : False = boom(coerce(only()))`), and running it traps at the `unreachable` the vacuous elimination emitted, which is the runtime witness that the impossibility claim was false. Ignored because the rule that refuses it does not exist yet: a clash may only be concluded at a position whose type distinguishes its inhabitants, and both checkers reach the shared walk through a `Judge` seam that today exposes no sort.
//
// The refusal is the coverage rule's: with the clash retracted, `only` is an ordinary reachable constructor, and an elimination with no arm for it is missing one it cannot prove absent.
#[test]
fn a_proposition_valued_index_cannot_make_an_elimination_vacuous() {
    let source = r#"
        use /std/{False};

        induct Two : pub Prop
        | a()
        | b()
        end

        induct Ind : (x : Two) -> pub Type
        | only() : (Two/a())
        end

        let coerce(w : Ind(Two/a())) -> Ind(Two/b()) = w;

        let boom(w : Ind(Two/b())) -> False =
            match w : (x, q) => False
            end;

        let bad : False = boom(coerce(Ind/only()));

        /std/print("FORGED")
        "#;
    rejected_by(
        source,
        "is not provably impossible at the scrutinee's indices",
    );
}

// The same disagreement through coverage rather than vacuity, which is why the fix belongs in the shared walk and not at one of its callers: here the elimination has an arm, and it is the *omitted* one that inversion wrongly excuses. `Ind/right()` inhabits `Ind(Two/a())` by the same conversion, so the match falls through every arm it enumerated.
#[test]
fn a_proposition_valued_index_cannot_excuse_an_omitted_arm() {
    let source = r#"
        use /std/{Nat};

        induct Two : pub Prop
        | a()
        | b()
        end

        induct Ind : (x : Two) -> pub Type
        | left() : (Two/a())
        | right() : (Two/b())
        end

        let coerce(w : Ind(Two/b())) -> Ind(Two/a()) = w;

        let f(w : Ind(Two/a())) -> Nat =
            match w : (x, q) => Nat
            | left() => 0
            end;

        /std/print(Nat/to_str(f(coerce(Ind/right()))))
        "#;
    rejected_by(
        source,
        "is not provably impossible at the scrutinee's indices",
    );
}

// The lower end of that discrimination: drop `a` from the index target and the guard fires. Without this, a fix could "close" the hole above by rejecting every indexed proposition and nothing here would notice.
#[test]
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
