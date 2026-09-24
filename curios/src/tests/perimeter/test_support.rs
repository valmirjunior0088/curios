//! Programs the perimeter suites compile and run.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

//! End-to-end coverage for the soundness perimeter entries that nothing else guards.
//!
//! The soundness perimeter is `documentation/soundness/`, one entry per rule, each graded *probed*, *argued*, or *auditable only* (see `documentation/design/language/the-soundness-perimeter.md`). "Probed" is a claim about executable evidence, so it needs a test that fails when the rule stops holding — otherwise the grade records what someone once tried by hand and decays the moment nobody remembers doing it.
//!
//! The entries with their own homes are not repeated here: strict positivity lives in `tests::positivity`, the two totality obligations in `tests::soundness`, and witness coherence in `tests::concepts`. What is left is the large-elimination guard, `Prop` non-informativeness, coverage, and the foreign wire contract — four rules the claim rests on that had no regression test at all.
//!
//! Each rejection asserts its *own* diagnostic, following `tests::soundness`. A perimeter test that accepts any error is worse than none: an invalid fixture passes it while the rule it names goes unchecked. That is not hypothetical — the first draft of these probes "passed" on `unbound variable`, having never reached the check at all.

use {
    crate::tests::run_text,
    curios_pipeline::recheck_with_prelude as recheck,
    curios_runtime::MockHost,
    curios_text::{Entrypoint, RootSource},
};

/// Reject `source`, and by the diagnostic naming the rule under test.
pub(super) fn rejected_by(source: &str, diagnostic: &str) {
    let (system, _io) = MockHost::builder().build();
    let error =
        run_text(source, system).expect_err("expected the perimeter rule to reject this program");
    assert!(
        error.contains(diagnostic),
        "rejected, but not by '{diagnostic}':\n{error}",
    );
}

pub(super) const A_MULTI_CONSTRUCTOR_PROPOSITION_CANNOT_BE_ELIMINATED_INTO_DATA: &str = r#"
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

pub(super) const AN_EMPTY_PROPOSITION_STILL_ELIMINATES_INTO_DATA: &str = r#"
        use /std/{Nat, Bool};

        let ex_falso(f : Bool/False) -> Nat =
            match f
            end;

        /std/print(Nat/to_str(0))
        "#;

pub(super) const A_PROPOSITION_STILL_ELIMINATES_INTO_ANOTHER_PROPOSITION: &str = r#"
        use /std/{Nat, Bool};

        induct Two : pub Prop
        | a()
        | b()
        end

        let into_prop(t : Two) -> Bool/True =
            match t
            | a() => Bool/True/qed()
            | b() => Bool/True/qed()
            end;

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_PROPOSITION_MAY_NOT_CARRY_INFORMATIVE_FIELDS: &str = r#"
        use /std/{Nat};

        struct Bad : pub Prop {
            value : Nat
        }

        /std/Io/pure(())
        "#;

pub(super) const A_PROPOSITION_CONCEPT_MAY_NOT_CARRY_INFORMATIVE_METHODS: &str = r#"
        use /std/{Nat};

        concept Bad(A : Type) : pub Prop {
            get(A) -> Nat,
        }

        /std/Io/pure(())
        "#;

pub(super) const AN_ELIMINATION_MUST_ENUMERATE_ITS_CONSTRUCTORS: &str = r#"
        use /std/{Nat, Option};

        let f(o : Option(Nat)) -> Nat =
            match o
            | some(x) => x
            end;

        f(Option/none())
        "#;

pub(super) const A_FOREIGN_DECLARATION_IS_CONFINED_TO_WIRE_TYPES: &str = r#"
        use /std/{Str};

        foreign bad : Str;

        /std/Io/pure(())
        "#;

pub(super) const A_NON_INJECTIVE_INDEX_TARGET_DOES_NOT_FORCE_ITS_BINDER: &str = r#"
        use /std/{Nat, Eq, Bool};

        let blur(a : Nat) -> Nat = 0;

        induct Loose : (n : Nat) -> pub Prop
        | mk(a : Nat) : (blur(a))
        end

        let extract(p : Loose(0)) -> Nat =
            match p : (m, q) => Nat
            | mk(a) => a
            end;

        let same : Eq(Loose/mk(0), Loose/mk(7)) = Eq/refl();

        let boom : Bool/False =
            Eq/subst((n : Nat) => match n : (_) => Type | 0 => {} | _ => Bool/False end,
                     Eq/cong(extract, same),
                     ());

        /std/print("FORGED")
        "#;

pub(super) const A_PROPOSITION_VALUED_INDEX_CANNOT_MAKE_AN_ELIMINATION_VACUOUS: &str = r#"
        use /std/{Bool};

        induct Two : pub Prop
        | a()
        | b()
        end

        induct Ind : (x : Two) -> pub Type
        | only() : (Two/a())
        end

        let coerce(w : Ind(Two/a())) -> Ind(Two/b()) = w;

        let boom(w : Ind(Two/b())) -> Bool/False =
            match w : (x, q) => Bool/False
            end;

        let bad : Bool/False = boom(coerce(Ind/only()));

        /std/print("FORGED")
        "#;

pub(super) const A_PROPOSITION_VALUED_INDEX_CANNOT_EXCUSE_AN_OMITTED_ARM: &str = r#"
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

/// `drop`'s `@A` is constrained by nothing — no argument mentions it and no result determines it — so every use below leaves one unsolved term metavariable exactly where the fixture plants it.
pub(super) const AN_UNCONSTRAINED_IMPLICIT: &str = r#"
        use /std/{Nat};

        let drop(@A : Type, n : Nat) -> Nat = n;
"#;

pub(super) const A_METAVARIABLE_IN_AN_INDUCT_TELESCOPE: &str = r#"
        induct Bad : pub Type
        | c(x : /std/Eq(drop(0), 0))
        end

        /std/Io/pure(())
        "#;

pub(super) const A_METAVARIABLE_IN_A_STRUCT_FIELD: &str = r#"
        struct Bad : pub Type {
            x : /std/Eq(drop(0), 0)
        }

        /std/Io/pure(())
        "#;

pub(super) const A_METAVARIABLE_IN_A_DEFINITIONS_TYPE: &str = r#"
        let f(x : /std/Eq(drop(0), 0)) -> Nat = 0;

        /std/Io/pure(())
        "#;

pub(super) const A_METAVARIABLE_IN_THE_ENTRYPOINT_BODY: &str = r#"
        /std/print(Nat/to_str(drop(0)))
        "#;

/// The same argument supplied in all four positions at once, so the refusals above cannot be passing for "a declaration may not mention an implicit".
pub(super) const A_SOLVED_METAVARIABLE_IN_EVERY_POSITION: &str = r#"
        induct Fine : pub Type
        | c(x : /std/Eq(drop(@Nat, 0), 0))
        end

        struct Also : pub Type {
            x : /std/Eq(drop(@Nat, 0), 0)
        }

        let f(x : /std/Eq(drop(@Nat, 0), 0)) -> Nat = 0;

        /std/print(Nat/to_str(drop(@Nat, 0)))
        "#;

pub(super) const A_NESTED_PROPOSITION_VALUED_INDEX_CANNOT_MAKE_AN_ELIMINATION_VACUOUS: &str = r#"
        use /std/{Nat, Bool};

        induct Two : pub Prop
        | a()
        | b()
        end

        induct Pair : pub Type
        | mk(n : Nat, p : Two)
        end

        induct Ind : (x : Pair) -> pub Type
        | only() : (Pair/mk(0, Two/a()))
        end

        let coerce(w : Ind(Pair/mk(0, Two/a()))) -> Ind(Pair/mk(0, Two/b())) = w;

        let boom(w : Ind(Pair/mk(0, Two/b()))) -> Bool/False =
            match w : (x, q) => Bool/False
            end;

        let bad : Bool/False = boom(coerce(Ind/only()));

        /std/print("FORGED")
        "#;

pub(super) const A_NESTED_PROPOSITION_VALUED_INDEX_CANNOT_EXCUSE_AN_OMITTED_ARM: &str = r#"
        use /std/{Nat};

        induct Two : pub Prop
        | a()
        | b()
        end

        induct Pair : pub Type
        | mk(n : Nat, p : Two)
        end

        induct Ind : (x : Pair) -> pub Type
        | left()  : (Pair/mk(0, Two/a()))
        | right() : (Pair/mk(0, Two/b()))
        end

        let coerce(w : Ind(Pair/mk(0, Two/b()))) -> Ind(Pair/mk(0, Two/a())) = w;

        let f(w : Ind(Pair/mk(0, Two/a()))) -> Nat =
            match w : (x, q) => Nat
            | left() => 0
            end;

        /std/print(Nat/to_str(f(coerce(Ind/right()))))
        "#;

pub(super) const A_NESTED_RELEVANT_CLASH_STILL_EXCUSES_AN_OMITTED_ARM: &str = r#"
        use /std/{Nat};

        induct Pair : pub Type
        | mk(n : Nat)
        end

        induct Ind : (x : Pair) -> pub Type
        | left()  : (Pair/mk(0))
        | right() : (Pair/mk(1))
        end

        let f(w : Ind(Pair/mk(0))) -> Nat =
            match w : (x, q) => Nat
            | left() => 0
            end;

        /std/print(Nat/to_str(f(Ind/left())))
        "#;

pub(super) const A_CLASH_BETWEEN_TWO_FORCINGS_OF_ONE_BINDER_EXCUSES_THE_ARM: &str = r#"
        use /std/{Eq, Bool, Nat, Option};

        induct Color : pub Type
        | red()
        | green()
        end

        induct Same(@A : Type) : (A, A) -> pub Prop
        | same(@z : A) : (z, z)
        end

        let absurd_bool(h : Eq(false, true)) -> Bool/False =
            match h end;

        let absurd_color(h : Eq(Color/green(), Color/red())) -> Bool/False =
            match h end;

        let absurd_nat(h : Eq(0, 1)) -> Bool/False =
            match h end;

        let absurd_successor(n : Nat, h : Eq(0, n + 1)) -> Bool/False =
            match h end;

        let absurd_option(h : Eq(Option/some(1), Option/none())) -> Bool/False =
            match h end;

        let absurd_same(h : Same(Color/green(), Color/red())) -> Bool/False =
            match h end;

        /std/print("ok")
        "#;

pub(super) const TWO_PROOFS_FORCED_ON_ONE_BINDER_DO_NOT_CLASH: &str = r#"
        use /std/{Eq, Bool};

        induct Two : pub Prop
        | a()
        | b()
        end

        let absurd(h : Eq(Two/a(), Two/b())) -> Bool/False =
            match h end;

        let forged : Bool/False = absurd(Eq/refl());

        /std/print("FORGED")
        "#;

pub(super) const AN_OPEN_FORCING_DOES_NOT_CLASH: &str = r#"
        use /std/{Eq, Bool};

        induct Color : pub Type
        | red()
        | green()
        end

        let absurd(c : Color, h : Eq(c, Color/red())) -> Bool/False =
            match h end;

        /std/print("FORGED")
        "#;

pub(super) const TWO_APPLICATIONS_OF_ONE_OPAQUE_FUNCTION_DO_NOT_CLASH: &str = r#"
        use /std/{Eq, Bool, Nat};

        let absurd(f : (Nat) -> Nat, h : Eq(f(0), f(1))) -> Bool/False =
            match h end;

        /std/print("FORGED")
        "#;

pub(super) const A_PARITY_DISAGREEMENT_IS_NOT_A_CLASH: &str = r#"
        use /std/{Eq, Bool, Nat};

        let absurd(x : Nat, y : Nat, h : Eq(x * 2 + 1, y * 2)) -> Bool/False =
            match h end;

        /std/print("FORGED")
        "#;

pub(super) const AN_UNMENTIONED_PAYLOAD_BINDER_IS_NOT_FORCED: &str = r#"
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

pub(super) const A_SINGLETON_CARRYING_A_TYPE_DOES_NOT_ELIMINATE: &str = r#"
        use /std/{Eq, Bool, Nat};

        induct Box : pub Prop
        | mk(A : Type)
        end

        let unbox(b : Box) -> Type =
            match b : (_) => Type
            | mk(A) => A
            end;

        let boxes_equal(A : Type, B : Type) -> Eq(Box/mk(A), Box/mk(B)) = Eq/refl();

        let types_equal(A : Type, B : Type) -> Eq(A, B) =
            Eq/cong(unbox, boxes_equal(A, B));

        let bad : Bool/False =
            Eq/subst((t : Type) => t, types_equal(Nat, Bool/False), 0);

        /std/print("FORGED")
        "#;

pub(super) const A_PROPOSITION_MAY_NOT_CARRY_A_TYPE_FIELD: &str = r#"
        struct Bad : pub Prop {
            carried : Type
        }

        /std/Io/pure(())
        "#;

pub(super) const A_LIST_OF_PROOFS_IS_NOT_A_PROPOSITION: &str = r#"
        use /std/{Eq, List, Bool};

        let all_equal(@X : Prop, x : X, y : X) -> Eq(x, y) =
            Eq/refl();

        let one : List(Bool/True) = [Bool/True/qed()];
        let none : List(Bool/True) = [];

        let bad : Eq(one, none) =
            all_equal(one, none);

        /std/print("FORGED")
        "#;

/// The witness's lemma at a *genuine* proposition, which must stay accepted: `all_equal` is sound, and a fix that closed the hole by refusing `Prop`-abstracted binders would take this with it. Type-checked rather than run — it is the shape the erase boundary cannot lower.
pub(super) const IRRELEVANCE_STILL_IDENTIFIES_A_PROPOSITIONS_INHABITANTS: &str = r#"
        use /std/{Nat, Eq};

        induct Two : pub Prop
        | a()
        | b()
        end

        let all_equal(@X : Prop, x : X, y : X) -> Eq(x, y) =
            Eq/refl();

        let same : Eq(Two/a(), Two/b()) =
            all_equal(Two/a(), Two/b());

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_LIST_OF_PROOFS_IS_STILL_A_LIST: &str = r#"
        use /std/{Nat, List, Bool};

        let one : List(Bool/True) = [Bool/True/qed()];

        /std/print(Nat/to_str(List/len(one)))
        "#;

pub(super) const A_CATCH_ALL_IS_CHECKED_AT_ITS_SCRUTINEE: &str = r#"
        use /std/{Nat, Eq};

        induct Three : pub Type
        | a()
        | b()
        | c()
        end

        let same(t : Three) -> Eq(t, t) =
            match t : (q) => Eq(q, q)
            | a() => Eq/refl()
            | _ => Eq/refl()
            end;

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_RECORD_OF_PROPOSITIONS_IS_A_PROPOSITION: &str = r#"
        use /std/{Nat, Eq};

        struct Holder : pub Prop {
            field : {Eq(0, 0), Eq(1, 1)}
        }

        /std/print(Nat/to_str(1))
        "#;

pub(super) const THE_EMPTY_RECORD_IS_NOT_A_PROPOSITION: &str = r#"
        struct Holder : pub Prop {
            field : {}
        }

        /std/Io/pure(())
        "#;

pub(super) const A_FUNCTION_INTO_A_PROPOSITION_IS_A_PROPOSITION: &str = r#"
        use /std/{Nat, Eq};

        struct Holder : pub Prop {
            field : (A : Type) -> Eq(0, 0)
        }

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_FUNCTION_INTO_A_TYPE_IS_NOT_A_PROPOSITION: &str = r#"
        struct Holder : pub Prop {
            field : (A : Type) -> A
        }

        /std/Io/pure(())
        "#;

pub(super) const A_PROPOSITION_STILL_ELIMINATES_INTO_A_FORMED_PROPOSITION: &str = r#"
        use /std/{Nat, Eq};

        induct Two : pub Prop
        | a()
        | b()
        end

        let into_record(t : Two) -> {Eq(0, 0), Eq(1, 1)} =
            match t
            | a() => (Eq/refl(), Eq/refl())
            | b() => (Eq/refl(), Eq/refl())
            end;

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_PROPOSITION_MAY_NOT_BE_ELIMINATED_INTO_A_FORMED_TYPE: &str = r#"
        use /std/{Nat};

        induct Two : pub Prop
        | a()
        | b()
        end

        let into_record(t : Two) -> {Nat, Nat} =
            match t
            | a() => (0, 0)
            | b() => (1, 1)
            end;

        /std/Io/pure(())
        "#;

pub(super) const A_NON_STRICT_OCCURRENCE_BEHIND_A_RECORD_IS_STILL_REFUSED: &str = r#"
        induct Bad : pub Type
        | mk(f : {((Bad) -> Prop) -> Prop})
        end

        /std/Io/pure(())
        "#;

pub(super) const ETA_CONVERTS_A_FUNCTION_AND_A_RECORD_WITH_THEIR_EXPANSIONS: &str = r#"
        use /std/{Eq, Nat, Bool};

        let function(g : (Nat) -> Nat) -> Eq((x : Nat) => g(x), g) = Eq/refl();

        let record(p : {Nat, Bool}) -> Eq((p.0, p.1), p) = Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

pub(super) const AN_EXPANSION_THAT_DROPS_ITS_BINDER_IS_NOT_ETA: &str = r#"
        use /std/{Eq, Nat};

        let dropped(g : (Nat) -> Nat) -> Eq((x : Nat) => g(0), g) = Eq/refl();

        /std/Io/pure(())
        "#;

pub(super) const AN_EXPANSION_THAT_SWAPS_ITS_COMPONENTS_IS_NOT_ETA: &str = r#"
        use /std/{Eq, Nat};

        let swapped(p : {Nat, Nat}) -> Eq((p.1, p.0), p) = Eq/refl();

        /std/Io/pure(())
        "#;

pub(super) const A_FUNCTION_INTO_A_PROPOSITION_IS_DISCHARGED_BEFORE_ETA: &str = r#"
        use /std/{Eq, Nat};

        let same(g : (Nat) -> Eq(0, 0), h : (Nat) -> Eq(0, 0)) -> Eq(g, h) = Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_FUNCTION_INTO_A_TYPE_IS_NOT_DISCHARGED_UNCOMPARED: &str = r#"
        use /std/{Eq, Nat};

        let same(g : (Nat) -> Nat, h : (Nat) -> Nat) -> Eq(g, h) = Eq/refl();

        /std/Io/pure(())
        "#;

pub(super) const ETA_HANDS_A_RECORDS_PROOF_COMPONENT_TO_IRRELEVANCE: &str = r#"
        use /std/{Eq, Nat};

        let same(g : (Nat) -> {Nat, Eq(0, 0)}, p : Eq(0, 0))
            -> Eq(g, (x : Nat) => (g(x).0, p)) = Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

pub(super) const ETA_STILL_COMPARES_A_RECORDS_RELEVANT_COMPONENT: &str = r#"
        use /std/{Eq, Nat};

        let same(p : {Nat, Eq(0, 0)}) -> Eq(p, (0, p.1)) = Eq/refl();

        /std/Io/pure(())
        "#;

pub(super) const A_SPINE_ARGUMENT_COMPARES_AT_THE_HEADS_DOMAIN: &str = r#"
        use /std/{Eq, Nat};

        let ground(f : (Eq(0, 0)) -> Nat, p : Eq(0, 0), q : Eq(0, 0)) -> Eq(f(p), f(q)) =
            Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_DEFINITION_APPLIED_TO_TWO_PROOFS_CONVERTS_BEFORE_UNFOLDING: &str = r#"
        use /std/{Eq, Nat};

        induct Z: (Nat) -> pub Prop
        | mk(): (0)
        end

        let h(n : Nat, e : Z(n)) -> Nat = match e | mk() => 1 end;

        let same(n : Nat, p : Z(n), q : Z(n), x : Eq(h(n, p), 0)) -> Eq(h(n, q), 0) = x;

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_POLYMORPHIC_DEFINITION_APPLIED_TO_TWO_PROOFS_CONVERTS: &str = r#"
        use /std/{Eq, Nat};

        let h(n : Nat, e : Eq(n, n)) -> Nat = match e | refl(@_) => 1 end;

        let same(n : Nat, p : Eq(n, n), q : Eq(n, n), x : Eq(h(n, p), 0)) -> Eq(h(n, q), 0) = x;

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_RECURSIVE_FUNCTION_CARRYING_A_PROOF_CONVERTS_WITHOUT_UNFOLDING: &str = r#"
        use /std/{Eq, Nat};

        induct T: pub Prop
        | t()
        end

        let g(n : Nat, p : T) -> Nat = match n | 0 => 0 | k + 1; _ => g(k, p) + 1 end;

        let same(n : Nat, p : T, q : T, x : Eq(g(n, p), 0)) -> Eq(g(n, q), 0) = x;

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_RECURSIVE_FUNCTION_MATCHING_ITS_PROOF_CONVERTS: &str = r#"
        use /std/{Eq, Nat};

        induct T: pub Prop
        | t()
        end

        let g(n : Nat, p : T) -> Nat = match n | 0 => (match p | t() => 0 end) | k + 1; _ => g(k, p) + 1 end;

        let same(n : Nat, p : T, q : T, x : Eq(g(n, p), 0)) -> Eq(g(n, q), 0) = x;

        /std/print(Nat/to_str(1))
        "#;

pub(super) const TWO_ACCESSIBILITY_PROOFS_AT_ONE_RECURSIVE_CALL_CONVERT: &str = r#"
        use /std/{Eq, Nat};
        use /std/WellFounded/{Accessible};

        let R(y : Nat, x : Nat) -> Prop = Nat/Lt(x, y);

        let f(n : Nat, lt : (k : Nat) -> Nat/Lt(k, k + 1), a : Accessible(R, n)) -> Nat =
            match a | intro(@_, below) => f(n + 1, lt, below(n + 1, lt(n))) end;

        let inv(n : Nat, a : Accessible(R, n)) -> (y : Nat, r : R(y, n)) -> Accessible(R, y) =
            (y, r) => match a | intro(@_, below) => below(y, r) end;

        let same(lt : (k : Nat) -> Nat/Lt(k, k + 1), a : Accessible(R, 0), x : Eq(f(0, lt, a), 0))
            -> Eq(f(0, lt, Accessible/intro(inv(0, a))), 0) = x;

        /std/print(Nat/to_str(1))
        "#;

pub(super) const AN_INFERRED_VALUE_UNDER_A_REFINED_PROOF_KEEPS_ITS_UNIVERSE_INSTANCE: &str = r#"
        use /std/{Eq, Nat};

        let f(h : (A : Prop) -> A) -> Eq(h(Eq(0, 0)), h(Eq(0, 0))) =
            match h(Eq(0, 0)) | refl(@_) => Eq/refl() end;

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_PROOF_IS_NOT_REDUCED_TO_COMPARE_IT_WITH_ANOTHER: &str = r#"
        use /std/{Eq, Nat};

        let Bot : Prop = (A : Prop) -> A;
        let Top : Prop = (Bot) -> Bot;
        let cast(A : Prop, B : Prop, e : Eq(A, B), x : A) -> B = match e | refl(@_) => x end;
        let delta : Top = (z) => z(Top)(z);
        let omega(h : (A : Prop, B : Prop) -> Eq(A, B)) -> Bot = (A) => cast(Top, A, h(Top, A), delta);
        let Omega(h : (A : Prop, B : Prop) -> Eq(A, B)) -> Bot = delta(omega(h));

        induct T: pub Prop
        | t()
        end

        let within(h : (A : Prop, B : Prop) -> Eq(A, B), y : T, w : Eq(@T, y, y)) -> Nat =
            match h(Top, Top)
            | refl(@_) =>
                let _v : Eq(@T, Omega(h)(T), y) = w;
                0
            end;

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_STRUCTS_FUNCTION_FIELD_MEETS_A_NEUTRAL_APPLICATION: &str = r#"
        use /std/{Eq, Nat, State};

        let left(a: Nat, f: (Nat) -> State(Nat, Nat)) -> Eq(State/bind(State/pure(a), f), f(a)) =
            Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_STRUCTS_FUNCTION_FIELD_MEETS_A_NEUTRAL_VARIABLE: &str = r#"
        use /std/{Eq, Nat, State};

        let right(m: State(Nat, Nat)) -> Eq(State/bind(m, (v) => State/pure(v)), m) =
            Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

pub(super) const TWO_STRUCT_LITERALS_COMPARE_THEIR_FUNCTION_FIELDS: &str = r#"
        use /std/{Eq, Nat, State};

        let assoc(m: State(Nat, Nat), f: (Nat) -> State(Nat, Nat), g: (Nat) -> State(Nat, Nat))
            -> Eq(State/bind(State/bind(m, f), g), State/bind(m, (v) => State/bind(f(v), g))) =
            Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_GROUP_MEMBER_IS_USED_A_LEVEL_UP_BY_ITS_SIBLING: &str = r#"
        use /std/{Nat};

        let pick(@A: Type, x: A) -> Nat = 0
        and other(n: Nat) -> Nat = pick(Nat) + n;

        /std/print(Nat/to_str(other(1)))
        "#;

pub(super) const THE_SAME_PAIR_DECLARED_APART_CERTIFIES: &str = r#"
        use /std/{Nat};

        let pick(@A: Type, x: A) -> Nat = 0;
        let other(n: Nat) -> Nat = pick(Nat) + n;

        /std/print(Nat/to_str(other(1)))
        "#;

pub(super) const A_PROOF_FIELD_DOES_NOT_DISTINGUISH_TWO_LITERALS: &str = r#"
        use /std/{Eq, Nat, Option, Str};

        induct P : pub Prop | mk() end
        struct S : pub Type { n : Nat, p : P }
        induct W : pub Type | wrap(Nat, P) end

        let field(n : Nat, p : P, q : P) -> Eq(S { n = n, p = p }, S { n = n, p = q }) = Eq/refl();
        let payload(n : Nat, p : P, q : P) -> Eq(W/wrap(n, p), W/wrap(n, q)) = Eq/refl();
        let some(p : P, q : P) -> Eq(Option/some(p), Option/some(q)) = Eq/refl();
        let assoc(a : Str, b : Str, c : Str)
            -> Eq(Str/concat(Str/concat(a, b), c), Str/concat(a, Str/concat(b, c))) = Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_NOMINAL_STRUCTS_ETA_IS_NOT_FORFEITED_THERE: &str = r#"
        use /std/{Eq, Nat};

        struct Sealed : pub Type {
            one : Eq(0, 0),
            two : Eq(1, 1)
        }

        let same(f : (Sealed) -> Nat, b : Sealed, p : Eq(0, 0), q : Eq(1, 1))
            -> Eq(f(Sealed { one = p, two = q }), f(b)) = Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

/// The premise every rule above is stated over and no entry under `documentation/soundness/` names: a type is a *pure* term. It used to be enforced by `reduce_intrinsic`, whose `Cell`, `CellGet`, `CellSet`, `Foreign` and `ProcExit` arms each refused type-level reduction, and this derivation was refused as `CellGet cannot appear at the type level` on that account alone, with no refinement in play. Those arms are gone — a description sitting at the type level is a value, not an error — and what refuses the program now is the scrutinee's own type. `Cell/fill(c, true) : Io(Bool)` describes an attempt instead of performing it, so it is not a `Bool`, not something `match` can eliminate, and not something `Eq` can be stated over. The fixture uses the current write-once operation, whose repeated attempts can still yield different Booleans; its refusal must land on the unforced description.
pub(super) const AN_EFFECTFUL_SCRUTINEE_IS_NOT_A_VALUE: &str = r#"
    use /std/{Cell, Eq, Bool, Str};

    let c = Cell/new(@Bool)!;

    let forged : Str =
        match Cell/fill(c, true)
        | true =>
            let p : Eq(Cell/fill(c, true), true) = Eq/refl();
            let done = Cell/fill(c, false);
            match Cell/fill(c, true)
            | true => "second read true"
            | false => match Bool/false_neq_true(p) end
            end
        | false => "first read false"
        end;

    /std/print(forged)
    "#;

/// The control, and it has more to guard than it used to. Only the refinement's escape into a *type* was ever at issue, so a fix that refused the elimination outright would be a brick — and now that the cure is a typing rule rather than a reduction guard, a rule that refused `Cell/poll` in every position would be exactly that brick. Forcing the poll yields an ordinary `Option(Bool)`, from which the fixture reads its Boolean.
pub(super) const A_MATCH_ON_A_FORCED_CELL_READ_STILL_COMPILES: &str = r#"
    use /std/{Cell, Bool, Option, Str};

    let c = Cell/new(@Bool)!;
    let _ = Cell/fill(c, true)!;
    let v = Option/unwrap_or(Cell/poll(c)!, false);

    /std/print(
        match v
        | true => "t"
        | false => "f"
        end
    )
    "#;

// **The four paragraphs below are the history of a hole that is now closed by typing rather than by any of the guards they describe.** They are kept because this row's grade rests on what was actually attacked, and because a reader who meets `Cell/get` in a scrutinee should find out why it was hard before it was impossible. The program is still a regression fixture; what refuses it changed.
//
// The same premise, past the guard that closed the entry above, because that guard asked a question weak-head reduction cannot answer. `refuses_type_level_reduction` reduced the scrutinee and read the refusal, and `reduce` stops at a *stuck head* handing the application back with its arguments untouched — so `f(Cell/get(c))`, a variable-headed application carrying the effect in an argument, never reaches `reduce_intrinsic`, answers `Ok`, and is registered as a spelling that fixes a value. The effect is then inside a type: `Eq(g(Cell/get(c)), true)` is admitted on those terms exactly as `Eq(Cell/get(c), true)` was.
//
// `curios-cert` has the same hole for the same reason and is not the backstop here: `assume_case_value` records at its own `whnf`, whose `step_apply` likewise stops at a stuck head without visiting an argument. Both checkers register the equation, which is what makes this agreement on a wrong rule rather than a disagreement — and why, unlike the entry above, it compiled.
//
// Two heads rather than one because a nested refinement of a *single* key is dropped by the kernel by accident: `assume_case_value` reduces the inner scrutinee under the outer arm's equation, gets the literal `true` back, and `Scope::refine` skips a local-free-less key. Refining `f(...)` outside and `g(...)` inside sidesteps that, and `h` carries the outer arm's knowledge across — so in `| true =>` the outer equation reads `h(Cell/get(c))` at `Eq(g(Cell/get(c)), true)`, which is `step`'s parameter type as written. After `Cell/set(c, false)` the inner `match g(Cell/get(c))` refines that same spelling to `false`, `p` re-reads at `Eq(false, true)`, and `/std/Bool/false_neq_true` turns it into `/std/Bool/False`.
//
// Verified while the hole was open: the program **compiled**, the compile-path recheck raised nothing, and running it trapped in the Wasm — `False/absurd` on the forged proof erasing to the `unreachable` the arm reaches. The arm is reachable rather than merely well-typed: with the derivation replaced by a string the same program printed `REACHED: second read false`. And the acceptance was the refinement's doing rather than a fixture that never reached the check — the identical program with the derivation moved to the inner `| true =>` arm, where the spelling refines to `true`, was refused with `type mismatch`, `inferred Eq(true, true)` against `expected Eq(false, true)`.
pub(super) const AN_EFFECT_BEHIND_A_STUCK_HEAD_IS_NOT_AN_ARGUMENT: &str = r#"
    use /std/{Cell, Eq, Bool, Str};

    /std/print(
        ((f : (Bool) -> Bool,
          g : (Bool) -> Bool,
          h : (x : Bool) -> Eq(g(x), f(x)),
          c : Cell(Bool)) =>
            match f(Cell/fill(@Bool, c, true))
            | true =>
                let step(p : Eq(g(Cell/fill(@Bool, c, true)), true)) -> Str =
                    let done = Cell/fill(c, false);
                    match g(Cell/fill(@Bool, c, true))
                    | true => "second read true"
                    | false => match Bool/false_neq_true(p) end
                    end;
                step(h(Cell/fill(@Bool, c, true)))
            | false => "first read false"
            end
        )((b) => b, (b) => b, (x) => Eq/refl(), Cell/new(@Bool)!)
    )
    "#;

/// The control, and it guards the brick this fix could have been: a scrutinee whose head is stuck is the *ordinary* case — `flip(b)` for a `b` nothing can instantiate — and refining it is what lets a hypothesis stated over the scrutinee re-read at the arm's value. So a guard that refused every stuck application, or every application it could not fully reduce, would still reject this.
///
/// The head is a *definition* here for historical reasons only. `fixes_no_value` could read a definition's body and so let the application fix whatever the callee fixed, while a parameter's body does not exist yet — so the parameter spelling had to move out of this control and into the derivation below. Nothing is walked now, and [`a_parameter_headed_scrutinee_refines_again`] is that spelling brought back.
pub(super) const A_STUCK_APPLICATION_SCRUTINEE_STILL_REFINES: &str = r#"
    use /std/{Eq, Bool, Str};

    let flip(b : Bool) -> Bool = Bool/not(b);

    let refined(b : Bool, p : Eq(flip(b), true)) -> Str =
        match flip(b)
        | true => "t"
        | false => match Bool/false_neq_true(p) end
        end;

    /std/print(refined(false, Eq/refl()))
    "#;

// The route no search over the term could have closed, and the one this whole discipline exists for. The scrutinee is `f(true)` for a *parameter* `f`: no `Intrinsic::CellGet` in it, none in anything it names, so every walk answered *pure* and both checkers recorded the equation. The caller then bound `f := (b) => Cell/get(c)`, and one spelling read `true` before the `Cell/set` and `false` after. Effectfulness of `f(true)` is not a property of `f(true)` — it is a property of the environment, and at the moment an arm records its equation the binder has no value to inspect, so asking the term was never sufficient.
//
// `fixes_no_value`'s cure was to ask a second question — does the walk read the body of every function the term would call — and refuse the equation when it does not. It worked and it was expensive in exactly the direction that matters: a *pure* opaque head stopped refining too, because nothing distinguished it. `(Bool) -> Bool` said nothing about purity, since the function space admitted `Cell/get`.
//
// Nothing is asked now, and the sentence that made the walk necessary is false. the current fixture's `(b) => Cell/fill(c, true)` has type `(Bool) -> Io(Bool)`; it does not inhabit `(Bool) -> Bool`, so the *caller's argument* is refused and the derivation never reaches an arm, a refinement, or an equation. What removes the class is an effect discipline on the arrow rather than another clause in the walk (see `documentation/soundness/per-term-rules/a-term-outside-io-performs-no-effect.md`) — and [`a_parameter_headed_scrutinee_refines_again`] is what the walk was costing.
pub(super) const AN_EFFECT_CANNOT_INHABIT_A_PURE_ARROW: &str = r#"
    use /std/{Cell, Eq, Bool, Str};

    let forge(f : (Bool) -> Bool, c : Cell(Bool), p : Eq(f(true), true)) -> Str =
        let done = Cell/fill(c, false);
        match f(true)
        | false =>
            let contradiction : Bool/False = Bool/false_neq_true(p);
            match contradiction end
        | true => "no contradiction"
        end;

    /std/print(
        ((c : Cell(Bool)) =>
            ((f : (Bool) -> Bool) =>
                match f(true)
                | true => forge(f, c, Eq/refl())
                | false => "first read false"
                end
            )((b) => Cell/fill(c, true))
        )(Cell/new(@Bool)!)
    )
    "#;

/// What the deleted walk was costing, and the reason this campaign is refinement-*restoring* rather than merely analysis-deleting. A parameter-headed scrutinee is the shape `fixes_no_value` had to refuse — it could not read a binder's body, so it could not tell a pure `f` from an effectful one — and refusing it withheld a refinement from every program that stated a hypothesis over an opaque head. Purity is a typing fact now, so the equation is licensed and `p` re-reads at the arm's value.
pub(super) const A_PARAMETER_HEADED_SCRUTINEE_REFINES_AGAIN: &str = r#"
    use /std/{Eq, Bool, Str};

    let refined(f : (Bool) -> Bool, b : Bool, p : Eq(f(b), true)) -> Str =
        match f(b)
        | true => "t"
        | false => match Bool/false_neq_true(p) end
        end;

    /std/print(refined((x) => x, true, Eq/refl()))
    "#;

/// A partial definition behind a `Type`-sorted carrier, reached four ways. The kernel's local gate does not fire — `Box` is neither a proposition nor a sort — so before the erasure obligations moved into `curios-cert` these were the class the trusted base took entirely on the elaborator's word.
pub(super) const PARTIAL_DIRECT: &str = r#"
    use /std/{Nat, Bool};
    struct Box : pub Type { p : Bool/False }
    let loop(n : Nat) -> Box = loop(n);
    let bad : Bool/False = loop(0).p;
    /std/print("FORGED")
    "#;

pub(super) const PARTIAL_THROUGH_WITNESS: &str = r#"
    use /std/{Nat, Bool};
    struct Box : pub Type { p : Bool/False }
    concept Make(A : Type) : pub Type { make(A) -> Box, }
    let loop(n : Nat) -> Box = loop(n);
    satisfy Make(Nat) { make(n) = loop(n), }
    let bad : Bool/False = Make/make(0).p;
    /std/print("FORGED")
    "#;

pub(super) const PARTIAL_HIGHER_ORDER: &str = r#"
    use /std/{Nat, Bool};
    struct Box : pub Type { p : Bool/False }
    let loop(n : Nat) -> Box = loop(n);
    let apply(f : (Nat) -> Box, n : Nat) -> Box = f(n);
    let bad : Bool/False = apply(loop, 0).p;
    /std/print("FORGED")
    "#;

pub(super) const PARTIAL_IN_FIELD: &str = r#"
    use /std/{Nat, Bool};
    struct Box : pub Type { p : Bool/False }
    struct Holder : pub Type { run : (Nat) -> Box }
    let loop(n : Nat) -> Box = loop(n);
    let holder : Holder = Holder { run = loop };
    let bad : Bool/False = holder.run(0).p;
    /std/print("FORGED")
    "#;

/// A diverging proof in a position the judgment *infers* rather than checks — a match scrutinee — inside a definition whose own type is relevant, so the body is not a proof position either.
///
/// The elaborator records every settled node with the type it settled at, checked or inferred alike, so it has always caught this. The kernel recorded only checked positions, and nothing here is one: the elimination conjured a `Nat` from a proof that never terminates. This row read `elab=refuses, cert=accepts` until the kernel began seeding inferred positions too — the quadrant this matrix exists to make visible.
pub(super) const INFERRED_PROOF_POSITION: &str = r#"
    use /std/{Nat, Bool};
    struct Box : pub Type { p : Bool/False }
    let loop(n : Nat) -> Box = loop(n);
    let conjured : Nat =
        match loop(0).p : (_) => Nat
        end;
    /std/print(Nat/to_str(conjured))
    "#;

/// A non-descending `rec` written *inline*, at a `Type`-sorted type so the kernel's local descent gate does not apply, inside a definition that is not itself a proof position.
///
/// Nothing but the classification walk can see this one: `make` has no name-level partiality to inherit and no proof-typed member to gate, so it is partial only if `locally_partial` descends into the `rec` group's member scopes rather than stopping at the node. The proof position is `bad`, which merely mentions `make`.
pub(super) const INLINE_REC_UNDER_CARRIER: &str = r#"
    use /std/{Nat, Bool};
    struct Box : pub Type { p : Bool/False }
    let make : Box =
        let r : Box = r;
        r;
    let bad : Bool/False = make.p;
    /std/print("FORGED")
    "#;

/// Obligation **(T)** rather than (V): a *type* that reaches a definition which is not known to terminate.
///
/// `spin` is legal — general recursion at a relevant type is the language's design — but a type mentioning it is not, because erasure deletes types too, and a type-level loop reties the negative knot strict positivity exists to forbid. The elaborator seeds this syntactically from written type positions; the kernel seeds it from its own typing, where the body of a definition whose type is a sort is checked against that sort. This row is the only coverage either seeding has for (T).
pub(super) const TYPE_REACHING_PARTIAL: &str = r#"
    use /std/{Nat, Vec};
    let spin(n : Nat) -> Nat = spin(n);
    let Sized(n : Nat) -> Type = Vec(Nat, spin(n));
    /std/print("FORGED")
    "#;

/// The induction hypothesis of a `Nat` fold, at the wrong instance.
///
/// In the successor arm the hypothesis is the motive at the *predecessor*, and the goal is the motive at the successor. Handing the hypothesis back directly proves `Eq(k + 1, 0)` from `Eq(k, 0)`, so a rule that typed the hypothesis at the scrutinee rather than at the peeled index would make every predicate provable by induction.
pub(super) const INDUCTION_HYPOTHESIS_AT_THE_SCRUTINEE: &str = r#"
    use /std/{Nat, Eq};
    let bogus(n : Nat) -> Eq(n, 0) =
        match n : (m) => Eq(m, 0)
        | 0 => Eq/refl()
        | k + 1; ih => ih
        end;
    /std/print("FORGED")
    "#;

/// A natural-number dispatch whose default is checked at a case's instance rather than the scrutinee's.
///
/// The default binds nothing and refines no index, so it must be checked at the scrutinee's own value: its goal here is `Eq(n, 0)` for an arbitrary `n`, which `Eq/refl()` cannot inhabit. Were it checked at the `0` arm's instance — the shape a refinement leak would produce — reflexivity would discharge it and every natural would equal zero.
pub(super) const DISPATCH_DEFAULT_AT_A_CASE: &str = r#"
    use /std/{Nat, Eq};
    let bogus(n : Nat) -> Eq(n, 0) =
        match n : (m) => Eq(m, 0)
        | 0 => Eq/refl()
        | _ => Eq/refl()
        end;
    /std/print("FORGED")
    "#;

/// Saturating subtraction is not a descent.
///
/// Note which rule fires on each side: the elaborator refuses through the reach obligation — the definition is a proof position reaching something not known to terminate — while the kernel refuses through its own local gate, a recursive member at a proposition whose group does not descend. Two rules, two crates, one program; that is what the second opinion is supposed to look like.
///
/// `n - 1` is `0` at `0`, so `bogus(0)` calls itself forever. The declared result is a proposition, which obliges the group to descend, and the size-change engine decides that — an engine crediting `n - 1` as strictly smaller would certify a recursion that does not terminate, and since erasure deletes the proof, `False` follows immediately.
pub(super) const SATURATING_SUBTRACTION_IS_NOT_DESCENT: &str = r#"
    use /std/{Nat, Bool};
    let bogus(n : Nat) -> Bool/False = bogus(n - 1);
    let bad : Bool/False = bogus(0);
    /std/print("FORGED")
    "#;

/// Permuting the arguments is not a descent either.
///
/// The classic size-change subtlety: every call maps a parameter to a parameter, so each call matrix is full of `Same` entries and none is `Less`. Composing a swap with itself returns the identity, so no cycle carries a strict decrease and the group cannot be total — an engine reading "the argument came from a parameter" as progress would certify a recursion that runs forever.
pub(super) const PERMUTING_ARGUMENTS_IS_NOT_DESCENT: &str = r#"
    use /std/{Nat, Bool};
    let bogus(a : Nat, b : Nat) -> Bool/False = bogus(b, a);
    let bad : Bool/False = bogus(0, 1);
    /std/print("FORGED")
    "#;

/// Two distinct recursions are not the same function.
///
/// Neither descends, so both are legal values, and both fold to themselves — which is the shape that puts conversion's recurrence rule to work: comparing them unfolds each once, arrives at the same goal, and a history that treated "already assumed" as "proved" would equate two definitions that differ. `f` is constantly zero and `g` constantly one, so equating them and transporting along the equality gives `Eq(0, 1)`.
pub(super) const DISTINCT_RECURSIONS_ARE_NOT_EQUAL: &str = r#"
    use /std/{Nat, Eq};
    let f(n : Nat) -> Nat =
        match n
        | 0 => 0
        | k + 1; _ => f(k)
        end;
    let g(n : Nat) -> Nat =
        match n
        | 0 => 1
        | k + 1; _ => g(k)
        end;
    let same : Eq(f, g) = Eq/refl();
    /std/print("FORGED")
    "#;

/// A `Bool` arm is checked at *its own* case value.
///
/// The `false` arm's goal is the motive at `false`, so `Eq/refl()` would have to inhabit `Eq(false, true)`. An arm rule that refined the scrutinee to the wrong case — or to none at all — would let reflexivity discharge it, and every boolean would equal `true`.
pub(super) const BOOL_ARM_AT_THE_WRONG_CASE: &str = r#"
    use /std/{Bool, Eq};
    let bogus(b : Bool) -> Eq(b, true) =
        match b : (c) => Eq(c, true)
        | true => Eq/refl()
        | false => Eq/refl()
        end;
    /std/print("FORGED")
    "#;

/// A natural-number dispatch's *literal* arm is checked at that literal.
///
/// The `1` arm's goal is the motive at `1`, which `Eq/refl()` cannot inhabit for `Eq(1, 0)`. This is the companion to the default-arm fixture: there the danger is refining an arm that binds nothing, here it is refining a literal arm to the wrong literal.
pub(super) const DISPATCH_LITERAL_AT_THE_WRONG_VALUE: &str = r#"
    use /std/{Nat, Eq};
    let bogus(n : Nat) -> Eq(n, 0) =
        match n : (m) => Eq(m, 0)
        | 0 => Eq/refl()
        | 1 => Eq/refl()
        | _ => Eq/refl()
        end;
    /std/print("FORGED")
    "#;

/// A program both checkers must accept, so a harness that refused everything would fail here.
pub(super) const A_SOUND_PROGRAM: &str = r#"
    use /std/{Nat};
    let double(n : Nat) -> Nat = n + n;
    /std/print(Nat/to_str(double(21)))
    "#;

/// What one checker did with a fixture.
#[derive(Debug)]
pub(super) enum Verdict {
    Accepts,
    Refuses(String),
    /// Never asked. Elaboration produced no module, so this checker never saw the program at all — which is a fact about coverage, not a pass.
    NotAsked,
}

/// Put `source` to each checker independently, and report what each said.
///
/// The elaborator's erasure obligations are *reported* rather than raised (`typecheck_reporting`), so a program only it refuses still yields a module for the kernel to judge. Without that the kernel's column would read whatever the elaborator's short circuit left behind, which is exactly the disagreement this exists to expose.
pub(super) fn both_checkers(source: &str) -> (Verdict, Verdict) {
    // A rule enforced by the grammar refuses here, before either checker exists. `foreign`'s wire contract is the standing example, and recording it is more honest than asserting the fixture parses: it says plainly that the rule is the parser's.
    //
    // What it does not mean is that the contract rests on the parser alone, and this comment used to read as though it did. A host call's type is not something the kernel takes on trust: `Intrinsic::Foreign` carries a wire signature over `WireType`, a closed six-variant enum with no case for a nominal type, and `infer` *constructs* the result from it rather than reading one off the term. So the boundary holds from Core as well, where no surface program can reach — `curios-cert`'s `recheck::tests::a_forged_foreign_row_cannot_inhabit_a_proposition` forges a row and pins it. `NotAsked` below records that neither checker is *asked*, not that neither would refuse.
    let entrypoint = match source.parse::<Entrypoint>() {
        Ok(entrypoint) => entrypoint,
        Err(error) => return (Verdict::Refuses(format!("{error:?}")), Verdict::NotAsked),
    };

    match curios_pipeline::typecheck_with_prelude(
        curios_pipeline::DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
    ) {
        // Refused before a module existed: type-checking proper, not an erasure obligation.
        Err(error) => (Verdict::Refuses(error.into()), Verdict::NotAsked),
        Ok((module, obligations)) => {
            let elaborator = match obligations.into_iter().next() {
                Some(error) => Verdict::Refuses(error),
                None => Verdict::Accepts,
            };
            // Exactly as the compile path judges it: the archived prelude arrives as scope on the archive's word rather than being re-walked, which is both what production does and what keeps a fixture cheap.
            let module =
                curios_core::Zonked::project(&module).expect("the checked module is zonked");
            let kernel = match recheck(&module, curios_pipeline::DEFAULT_STEP_BUDGET)
                .into_iter()
                .next()
            {
                None => Verdict::Accepts,
                Some(verdict) => Verdict::Refuses(verdict.error.to_string()),
            };
            (elaborator, kernel)
        }
    }
}

pub(super) const A_FOLD_MOTIVE_MAY_NOT_CAPTURE_ITS_SCRUTINEE: &str = r#"
        use /std/{Nat, Eq};

        let all_zero(n : Nat) -> Eq(n, 0) =
            match n : (_) => Eq(n, 0)
            | 0 => Eq/refl()
            | k + 1; ih => ih
            end;

        let boom : Eq(1, 0) = all_zero(1);

        /std/print("unreachable")
        "#;

pub(super) const A_LIST_FOLD_MOTIVE_MAY_NOT_CAPTURE_ITS_SCRUTINEE: &str = r#"
        use /std/{Nat, Eq, List};

        let all_empty(l : List(Nat)) -> Eq(List/len(l), 0) =
            match l : (_) => Eq(List/len(l), 0)
            | [] => Eq/refl()
            | [h, ..t]; ih => ih
            end;

        let boom : Eq(1, 0) = all_empty([7]);

        /std/print("unreachable")
        "#;

pub(super) const A_FOLD_MOTIVE_MAY_NOT_CAPTURE_ITS_SCRUTINEE_EXPRESSION: &str = r#"
        use /std/{Nat, Eq};

        let twice(n : Nat) -> Nat = n + n;

        let all_zero(n : Nat) -> Eq(twice(n), 0) =
            match twice(n) : (_) => Eq(twice(n), 0)
            | 0 => Eq/refl()
            | k + 1; ih => ih
            end;

        let boom : Eq(2, 0) = all_zero(1);

        /std/print("unreachable")
        "#;

pub(super) const A_FOLD_MOTIVE_MAY_NOT_CAPTURE_ITS_SCRUTINEE_THROUGH_AN_ALIAS: &str = r#"
        use /std/{Nat, Eq};

        let all_zero(n : Nat) -> Eq(n, 0) =
            let y = n;
            match n : (_) => Eq(y, 0)
            | 0 => Eq/refl()
            | k + 1; ih => ih
            end;

        let boom : Eq(1, 0) = all_zero(1);

        /std/print("unreachable")
        "#;

pub(super) const A_FOLD_MOTIVE_THAT_BINDS_ITS_SCRUTINEE_STILL_FOLDS: &str = r#"
        use /std/{Nat, Eq};

        let plus_zero(n : Nat) -> Eq(n + 0, n) =
            match n : (m) => Eq(m + 0, m)
            | 0 => Eq/refl()
            | k + 1; ih => Eq/cong((w : Nat) => w + 1, ih)
            end;

        /std/print(Nat/to_str(3))
        "#;

/// What a fixture's row claims a checker does.
pub(super) enum Expect {
    Accepts,
    /// Refused, by a diagnostic containing this fragment — the rule must be named, or a fixture broken in some unrelated way would pass.
    Refuses(&'static str),
    /// Never reached this checker. Not a pass: it records that the rule is enforced earlier, so whether this checker also enforces it is *unverified here*. Only the erasure obligations can be deferred far enough for the kernel to see a program the elaborator rejects — every other refusal happens while the module is still being built, so there is nothing to hand over.
    ///
    /// What it carries is where the kernel *is* asked: the fixture in `curios-cert` that puts the same rule to it from a module built by hand, as `<file under curios-cert/src>::<test>`, which `every_named_kernel_twin_exists` holds to the tree. `None` says no such fixture was found, so the kernel's half of that row stands verified nowhere — a rung of the same rule held by a neighbouring row's twin is not counted as this row's.
    NotAsked(Option<&'static str>),
}

/// Every fixture above, with what each checker is expected to say about it.
///
/// The pair is the point. Both refusing is a rule covered twice; both accepting is a program that must compile. The kernel refusing what the elaborator accepts would be recorded conversion incompleteness, the safe direction. The elaborator refusing what the kernel *accepts* would be the trusted base resting on an elaborator-only analysis — the shape that made a whole class of `False` certifiable — and the four `partial_*` rows read that way until the erasure obligations moved into `curios-cert`.
pub(super) const CORPUS: &[(&str, &str, Expect, Expect)] = &[
    (
        "multi_constructor_prop",
        A_MULTI_CONSTRUCTOR_PROPOSITION_CANNOT_BE_ELIMINATED_INTO_DATA,
        Expect::Refuses("cannot eliminate the proposition"),
        Expect::NotAsked(Some(
            "kernel/infer/eliminate/tests.rs::a_proposition_with_two_constructors_does_not_eliminate_into_a_type",
        )),
    ),
    (
        "empty_prop_eliminates",
        AN_EMPTY_PROPOSITION_STILL_ELIMINATES_INTO_DATA,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "prop_into_prop",
        A_PROPOSITION_STILL_ELIMINATES_INTO_ANOTHER_PROPOSITION,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "informative_prop_field",
        A_PROPOSITION_MAY_NOT_CARRY_INFORMATIVE_FIELDS,
        Expect::Refuses("is informative"),
        Expect::NotAsked(Some(
            "kernel/module/tests.rs::a_proposition_may_not_carry_an_informative_field",
        )),
    ),
    (
        "informative_prop_method",
        A_PROPOSITION_CONCEPT_MAY_NOT_CARRY_INFORMATIVE_METHODS,
        Expect::Refuses("is informative"),
        Expect::NotAsked(None),
    ),
    (
        "coverage",
        AN_ELIMINATION_MUST_ENUMERATE_ITS_CONSTRUCTORS,
        Expect::Refuses("missing match case"),
        Expect::NotAsked(Some(
            "kernel/infer/eliminate/tests.rs::an_undecided_absent_arm_is_refused",
        )),
    ),
    // Enforced by the grammar, before either checker exists.
    (
        "wire_types",
        A_FOREIGN_DECLARATION_IS_CONFINED_TO_WIRE_TYPES,
        Expect::Refuses("expected a wire type"),
        Expect::NotAsked(Some(
            "recheck/foreign_tests.rs::a_forged_foreign_row_cannot_inhabit_a_proposition",
        )),
    ),
    (
        "prop_index_vacuous",
        A_PROPOSITION_VALUED_INDEX_CANNOT_MAKE_AN_ELIMINATION_VACUOUS,
        Expect::Refuses("not provably impossible"),
        Expect::NotAsked(None),
    ),
    (
        "prop_index_omitted_arm",
        A_PROPOSITION_VALUED_INDEX_CANNOT_EXCUSE_AN_OMITTED_ARM,
        Expect::Refuses("not provably impossible"),
        Expect::NotAsked(None),
    ),
    // Both must accept: the clash is the shared unifier's, so a checker that excused these arms alone would be the trusted base resting on the other's analysis.
    (
        "clash_between_two_forcings",
        A_CLASH_BETWEEN_TWO_FORCINGS_OF_ONE_BINDER_EXCUSES_THE_ARM,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "two_proofs_forced_on_one_binder",
        TWO_PROOFS_FORCED_ON_ONE_BINDER_DO_NOT_CLASH,
        Expect::Refuses("not provably impossible"),
        Expect::NotAsked(Some(
            "kernel/infer/eliminate/tests.rs::a_clash_between_two_forcings_of_one_binder_excuses_the_arm",
        )),
    ),
    (
        "open_forcing",
        AN_OPEN_FORCING_DOES_NOT_CLASH,
        Expect::Refuses("not provably impossible"),
        Expect::NotAsked(Some(
            "kernel/infer/eliminate/tests.rs::a_clash_between_two_forcings_of_one_binder_excuses_the_arm",
        )),
    ),
    (
        "two_applications_of_one_opaque_function",
        TWO_APPLICATIONS_OF_ONE_OPAQUE_FUNCTION_DO_NOT_CLASH,
        Expect::Refuses("not provably impossible"),
        Expect::NotAsked(Some(
            "kernel/infer/eliminate/tests.rs::a_clash_between_two_forcings_of_one_binder_excuses_the_arm",
        )),
    ),
    (
        "parity_disagreement",
        A_PARITY_DISAGREEMENT_IS_NOT_A_CLASH,
        Expect::Refuses("not provably impossible"),
        Expect::NotAsked(Some(
            "kernel/infer/eliminate/tests.rs::a_clash_between_two_forcings_of_one_binder_excuses_the_arm",
        )),
    ),
    (
        "non_injective_target",
        A_NON_INJECTIVE_INDEX_TARGET_DOES_NOT_FORCE_ITS_BINDER,
        Expect::Refuses("cannot eliminate the proposition"),
        Expect::NotAsked(Some(
            "kernel/infer/eliminate/tests.rs::a_singleton_whose_index_merely_mentions_its_payload_does_not",
        )),
    ),
    (
        "unmentioned_binder",
        AN_UNMENTIONED_PAYLOAD_BINDER_IS_NOT_FORCED,
        Expect::Refuses("cannot eliminate the proposition"),
        Expect::NotAsked(None),
    ),
    // `NotAsked` here is the elaborator gating first, not the kernel being silent: its half of both rules is guarded in `curios-cert`'s own tests, which reach it by building the module directly.
    (
        "singleton_carrying_a_type",
        A_SINGLETON_CARRYING_A_TYPE_DOES_NOT_ELIMINATE,
        Expect::Refuses("cannot eliminate the proposition"),
        Expect::NotAsked(Some(
            "kernel/infer/eliminate/tests.rs::a_singleton_carrying_a_type_does_not_eliminate_into_a_type",
        )),
    ),
    (
        "type_valued_prop_field",
        A_PROPOSITION_MAY_NOT_CARRY_A_TYPE_FIELD,
        Expect::Refuses("is informative"),
        Expect::NotAsked(Some(
            "kernel/module/tests.rs::a_proposition_may_not_carry_a_type",
        )),
    ),
    // Both accepted this one: the sort of a parameterized intrinsic former was implemented twice on each side, and the typing rule's copy disagreed with `Sort::of`'s. Its kernel half is guarded where the rule lives, in `curios_cert::kernel::infer::tests`.
    (
        "list_of_proofs_is_not_a_prop",
        A_LIST_OF_PROOFS_IS_NOT_A_PROPOSITION,
        Expect::Refuses("type mismatch"),
        Expect::NotAsked(Some(
            "kernel/sort/tests.rs::a_list_of_proofs_is_not_a_proposition",
        )),
    ),
    (
        "irrelevance_still_identifies",
        IRRELEVANCE_STILL_IDENTIFIES_A_PROPOSITIONS_INHABITANTS,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "list_of_proofs_is_still_a_list",
        A_LIST_OF_PROOFS_IS_STILL_A_LIST,
        Expect::Accepts,
        Expect::Accepts,
    ),
    // Both must accept: the catch-all's instance is a rule they are supposed to decide the same way, and this row is where they were caught deciding it differently.
    (
        "catch_all_at_its_scrutinee",
        A_CATCH_ALL_IS_CHECKED_AT_ITS_SCRUTINEE,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "partial_direct",
        PARTIAL_DIRECT,
        Expect::Refuses("not known to terminate"),
        Expect::Refuses("not known to terminate"),
    ),
    (
        "partial_through_witness",
        PARTIAL_THROUGH_WITNESS,
        Expect::Refuses("not known to terminate"),
        Expect::Refuses("not known to terminate"),
    ),
    (
        "partial_higher_order",
        PARTIAL_HIGHER_ORDER,
        Expect::Refuses("not known to terminate"),
        Expect::Refuses("not known to terminate"),
    ),
    (
        "partial_in_field",
        PARTIAL_IN_FIELD,
        Expect::Refuses("not known to terminate"),
        Expect::Refuses("not known to terminate"),
    ),
    (
        "inferred_proof_position",
        INFERRED_PROOF_POSITION,
        Expect::Refuses("not known to terminate"),
        Expect::Refuses("not known to terminate"),
    ),
    (
        "inline_rec_under_carrier",
        INLINE_REC_UNDER_CARRIER,
        Expect::Refuses("not known to terminate"),
        Expect::Refuses("not known to terminate"),
    ),
    (
        "type_reaching_partial",
        TYPE_REACHING_PARTIAL,
        Expect::Refuses("not known to terminate"),
        Expect::Refuses("not known to terminate"),
    ),
    (
        "induction_hypothesis_at_the_scrutinee",
        INDUCTION_HYPOTHESIS_AT_THE_SCRUTINEE,
        Expect::Refuses("type mismatch"),
        Expect::NotAsked(Some(
            "kernel/infer/intrinsic_tests.rs::a_free_monoid_arm_must_inhabit_the_motive_at_its_case",
        )),
    ),
    (
        "dispatch_default_at_a_case",
        DISPATCH_DEFAULT_AT_A_CASE,
        Expect::Refuses("type mismatch"),
        Expect::NotAsked(None),
    ),
    (
        "saturating_subtraction_is_not_descent",
        SATURATING_SUBTRACTION_IS_NOT_DESCENT,
        Expect::Refuses("not known to terminate"),
        Expect::Refuses("does not descend"),
    ),
    (
        "permuting_arguments_is_not_descent",
        PERMUTING_ARGUMENTS_IS_NOT_DESCENT,
        Expect::Refuses("not known to terminate"),
        Expect::Refuses("does not descend"),
    ),
    (
        "distinct_recursions_are_not_equal",
        DISTINCT_RECURSIONS_ARE_NOT_EQUAL,
        Expect::Refuses("type mismatch"),
        Expect::NotAsked(Some(
            "kernel/convert/recursion_tests.rs::a_recurrence_does_not_excuse_a_finite_disagreement",
        )),
    ),
    (
        "bool_arm_at_the_wrong_case",
        BOOL_ARM_AT_THE_WRONG_CASE,
        Expect::Refuses("type mismatch"),
        Expect::NotAsked(None),
    ),
    (
        "dispatch_literal_at_the_wrong_value",
        DISPATCH_LITERAL_AT_THE_WRONG_VALUE,
        Expect::Refuses("type mismatch"),
        Expect::NotAsked(None),
    ),
    (
        "sound_program",
        A_SOUND_PROGRAM,
        Expect::Accepts,
        Expect::Accepts,
    ),
    // Sort formation. Both accepting rungs reach the kernel, which is unusual on this map: most rows below refuse during elaboration and leave the certifier nothing to judge.
    (
        "record_of_propositions",
        A_RECORD_OF_PROPOSITIONS_IS_A_PROPOSITION,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "empty_record_is_unit",
        THE_EMPTY_RECORD_IS_NOT_A_PROPOSITION,
        Expect::Refuses("is informative"),
        Expect::NotAsked(Some(
            "kernel/sort/tests.rs::a_record_of_propositions_is_a_proposition_but_the_empty_one_is_unit",
        )),
    ),
    (
        "function_into_proposition",
        A_FUNCTION_INTO_A_PROPOSITION_IS_A_PROPOSITION,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "function_into_type",
        A_FUNCTION_INTO_A_TYPE_IS_NOT_A_PROPOSITION,
        Expect::Refuses("is informative"),
        Expect::NotAsked(None),
    ),
    (
        "prop_into_formed_prop",
        A_PROPOSITION_STILL_ELIMINATES_INTO_A_FORMED_PROPOSITION,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "prop_into_formed_type",
        A_PROPOSITION_MAY_NOT_BE_ELIMINATED_INTO_A_FORMED_TYPE,
        Expect::Refuses("cannot eliminate the proposition"),
        Expect::NotAsked(None),
    ),
    (
        "non_strict_behind_record",
        A_NON_STRICT_OCCURRENCE_BEHIND_A_RECORD_IS_STILL_REFUSED,
        Expect::Refuses("positively, but not strictly"),
        Expect::NotAsked(None),
    ),
    // Eta, whose accepting rungs all reach the kernel because they compile — so this row's second opinion is one of the few on this map that says anything.
    (
        "eta_expansions",
        ETA_CONVERTS_A_FUNCTION_AND_A_RECORD_WITH_THEIR_EXPANSIONS,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "eta_drops_its_binder",
        AN_EXPANSION_THAT_DROPS_ITS_BINDER_IS_NOT_ETA,
        Expect::Refuses("type mismatch"),
        Expect::NotAsked(None),
    ),
    (
        "eta_swaps_its_components",
        AN_EXPANSION_THAT_SWAPS_ITS_COMPONENTS_IS_NOT_ETA,
        Expect::Refuses("type mismatch"),
        Expect::NotAsked(None),
    ),
    (
        "function_into_proposition_needs_no_eta",
        A_FUNCTION_INTO_A_PROPOSITION_IS_DISCHARGED_BEFORE_ETA,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "function_into_type_needs_comparing",
        A_FUNCTION_INTO_A_TYPE_IS_NOT_DISCHARGED_UNCOMPARED,
        Expect::Refuses("type mismatch"),
        Expect::NotAsked(None),
    ),
    (
        "eta_reaches_irrelevance",
        ETA_HANDS_A_RECORDS_PROOF_COMPONENT_TO_IRRELEVANCE,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "eta_compares_the_relevant_component",
        ETA_STILL_COMPARES_A_RECORDS_RELEVANT_COMPONENT,
        Expect::Refuses("type mismatch"),
        Expect::NotAsked(None),
    ),
    // **The quadrant this table had one instance of, and now has none.** Two proofs of one proposition passed to an opaque head were compared at `Type`, where irrelevance is never asked, so the kernel refused what the elaborator accepted. A variable head carries the telescope its arguments inhabit, and reading it is a lookup rather than an inference, so the arguments compare at their domains and the proofs discharge without being read.
    (
        "a_spine_argument_compares_at_the_heads_domain",
        A_SPINE_ARGUMENT_COMPARES_AT_THE_HEADS_DOMAIN,
        Expect::Accepts,
        Expect::Accepts,
    ),
    // **Four more sat in that quadrant, found by putting the checkers to programs rather than by reading this table.** The elaborator compares two applications of one definition by their spines before it unfolds either, typing each argument at the head's telescope, so two proofs meet irrelevance; the kernel forced both sides first, and the proof landed where nothing typed it — a stuck match's scrutinee, or a folded recursive call it then unfolded under fresh binders until the budget ran out. The kernel now compares the spines first too, and the elaborator's own rule reaches the universe-polymorphic head a definition mentioning `Eq` has, which it had skipped.
    (
        "a_definition_applied_to_two_proofs_converts_before_unfolding",
        A_DEFINITION_APPLIED_TO_TWO_PROOFS_CONVERTS_BEFORE_UNFOLDING,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "a_polymorphic_definition_applied_to_two_proofs_converts",
        A_POLYMORPHIC_DEFINITION_APPLIED_TO_TWO_PROOFS_CONVERTS,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "a_recursive_function_carrying_a_proof_converts_without_unfolding",
        A_RECURSIVE_FUNCTION_CARRYING_A_PROOF_CONVERTS_WITHOUT_UNFOLDING,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "a_recursive_function_matching_its_proof_converts",
        A_RECURSIVE_FUNCTION_MATCHING_ITS_PROOF_CONVERTS,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "two_accessibility_proofs_at_one_recursive_call_convert",
        TWO_ACCESSIBILITY_PROOFS_AT_ONE_RECURSIVE_CALL_CONVERT,
        Expect::Accepts,
        Expect::Accepts,
    ),
    // The elaborator reduced both sides of a goal before asking whether its type was a proposition, and under an absurd arm's case equation Abel and Coquand's `Omega` reduces forever — so it spent its budget on two proofs irrelevance equates outright, a refusal the kernel, asking irrelevance first, never makes. Both now ask it first wherever nothing is flexible.
    (
        "a_proof_is_not_reduced_to_compare_it_with_another",
        A_PROOF_IS_NOT_REDUCED_TO_COMPARE_IT_WITH_ANOTHER,
        Expect::Accepts,
        Expect::Accepts,
    ),
    // The elaborator solved `refl`'s value with the arm's refinements suppressed, and its reducer handed back the refinement's universe-erased key as the reduct, so the solution held a bare `/std/Eq/Eq` the kernel refused as an occurrence stating no universe instance. The reducer now hands back the probe as spelled.
    (
        "an_inferred_value_under_a_refined_proof_keeps_its_universe_instance",
        AN_INFERRED_VALUE_UNDER_A_REFINED_PROOF_KEEPS_ITS_UNIVERSE_INSTANCE,
        Expect::Accepts,
        Expect::Accepts,
    ),
    // The same closure reached from `/std`'s own vocabulary, and the reason it is worth a row: `State`'s identity laws are a library's own equations, not adversarial programs. Each sets `State/bind`'s literal against a neutral — `f(a)` on the left, `m` on the right — and both now converge, the field comparing at the function type the declaration gives it rather than at `Type`, where a lambda never meets a projection. Associativity is the control: both sides are literals there, and it converged all along.
    (
        "state_left_identity_converges",
        A_STRUCTS_FUNCTION_FIELD_MEETS_A_NEUTRAL_APPLICATION,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "state_right_identity_converges",
        A_STRUCTS_FUNCTION_FIELD_MEETS_A_NEUTRAL_VARIABLE,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "state_associativity_is_two_literals",
        TWO_STRUCT_LITERALS_COMPARE_THEIR_FUNCTION_FIELDS,
        Expect::Accepts,
        Expect::Accepts,
    ),
    // The third disagreement was not conversion's at all. A member of an `and` group used at a type one level up needs `1 ≤ u` of the group's own instance, a group being monomorphic in its universes, and the elaborator records exactly that in the scheme it generalizes — so the kernel had the premise and refused anyway, because its entailment decided a level's *constant* part structurally before the hypotheses were reached. A parameter ranges over every natural when nothing is assumed; a hypothesis is what puts a floor under it.
    (
        "group_member_used_a_level_up_by_its_sibling",
        A_GROUP_MEMBER_IS_USED_A_LEVEL_UP_BY_ITS_SIBLING,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "the_same_pair_declared_apart",
        THE_SAME_PAIR_DECLARED_APART_CERTIFIES,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "nominal_struct_eta_survives_grounding",
        A_NOMINAL_STRUCTS_ETA_IS_NOT_FORFEITED_THERE,
        Expect::Accepts,
        Expect::Accepts,
    ),
    // The forfeiture stops at the opaque head: a struct's fields and a constructor's payload compare at their declaration's telescope in both checkers, so a proof there distinguishes nothing.
    (
        "proof_field_does_not_distinguish_two_literals",
        A_PROOF_FIELD_DOES_NOT_DISTINGUISH_TWO_LITERALS,
        Expect::Accepts,
        Expect::Accepts,
    ),
    (
        "fold_motive_captures_scrutinee",
        A_FOLD_MOTIVE_MAY_NOT_CAPTURE_ITS_SCRUTINEE,
        Expect::Refuses("through the binder it declares"),
        Expect::NotAsked(Some(
            "recheck/elimination_tests.rs::a_fold_motive_that_captures_its_scrutinee_is_refused",
        )),
    ),
    (
        "list_fold_motive_captures_scrutinee",
        A_LIST_FOLD_MOTIVE_MAY_NOT_CAPTURE_ITS_SCRUTINEE,
        Expect::Refuses("through the binder it declares"),
        Expect::NotAsked(Some(
            "recheck/elimination_tests.rs::a_fold_motive_that_captures_its_scrutinee_is_refused",
        )),
    ),
    (
        "fold_motive_captures_scrutinee_expression",
        A_FOLD_MOTIVE_MAY_NOT_CAPTURE_ITS_SCRUTINEE_EXPRESSION,
        Expect::Refuses("through the binder it declares"),
        Expect::NotAsked(Some(
            "recheck/elimination_tests.rs::a_fold_motive_that_captures_its_scrutinee_is_refused",
        )),
    ),
    (
        "fold_motive_captures_scrutinee_through_an_alias",
        A_FOLD_MOTIVE_MAY_NOT_CAPTURE_ITS_SCRUTINEE_THROUGH_AN_ALIAS,
        Expect::Refuses("through the binder it declares"),
        Expect::NotAsked(Some(
            "recheck/elimination_tests.rs::a_fold_motive_that_captures_its_scrutinee_is_refused",
        )),
    ),
    (
        "fold_motive_binds_scrutinee",
        A_FOLD_MOTIVE_THAT_BINDS_ITS_SCRUTINEE_STILL_FOLDS,
        Expect::Accepts,
        Expect::Accepts,
    ),
];

pub(super) fn agrees(name: &str, checker: &str, expected: &Expect, actual: &Verdict) {
    match (expected, actual) {
        (Expect::Accepts, Verdict::Accepts) => {}
        (Expect::NotAsked(_), Verdict::NotAsked) => {}
        (Expect::Refuses(fragment), Verdict::Refuses(error)) => assert!(
            error.contains(fragment),
            "{name}: {checker} refused, but not by '{fragment}':\n{error}",
        ),
        _ => panic!("{name}: {checker} expected {expected:?}, got {actual:?}"),
    }
}

impl std::fmt::Debug for Expect {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expect::Accepts => write!(formatter, "accepts"),
            Expect::NotAsked(_) => write!(formatter, "not-asked"),
            Expect::Refuses(fragment) => write!(formatter, "refuses({fragment})"),
        }
    }
}
