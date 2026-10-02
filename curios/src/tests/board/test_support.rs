//! Programs the board suites compile and run.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use {crate::tests::run_text, curios_runtime::MockHost};

/// Reject `source`, and by the diagnostic naming the rule under test.
pub(super) fn rejected_by(source: &str, diagnostic: &str) {
    let (system, _io) = MockHost::builder().build();
    let error =
        run_text(source, system).expect_err("expected the board rule to reject this program");
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
        use /std/{Nat};
        use /std/Bool/{False};

        let ex_falso(f : False) -> Nat =
            match f
            end;

        /std/print(Nat/to_str(0))
        "#;

pub(super) const A_PROPOSITION_STILL_ELIMINATES_INTO_ANOTHER_PROPOSITION: &str = r#"
        use /std/{Nat};
        use /std/Bool/{True};

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
        use /std/{Nat, Eq};
        use /std/Bool/{False};

        let blur(a : Nat) -> Nat = 0;

        induct Loose : (n : Nat) -> pub Prop
        | mk(a : Nat) : (blur(a))
        end

        let extract(p : Loose(0)) -> Nat =
            match p : (m, q) => Nat
            | mk(a) => a
            end;

        let same : Eq()(Loose/mk(0), Loose/mk(7)) = Eq/refl();

        let boom : False =
            Eq/subst((n : Nat) => match n : (_) => Type | 0 => {} | _ => False end,
                     Eq/cong(extract, same),
                     ());

        /std/print("FORGED")
        "#;

pub(super) const A_PROPOSITION_VALUED_INDEX_CANNOT_MAKE_AN_ELIMINATION_VACUOUS: &str = r#"
        use /std/Bool/{False};

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
        | c(x : /std/Eq()(drop(0), 0))
        end

        /std/Io/pure(())
        "#;

pub(super) const A_METAVARIABLE_IN_A_STRUCT_FIELD: &str = r#"
        struct Bad : pub Type {
            x : /std/Eq()(drop(0), 0)
        }

        /std/Io/pure(())
        "#;

pub(super) const A_METAVARIABLE_IN_A_DEFINITIONS_TYPE: &str = r#"
        let f(x : /std/Eq()(drop(0), 0)) -> Nat = 0;

        /std/Io/pure(())
        "#;

pub(super) const A_METAVARIABLE_IN_THE_ENTRYPOINT_BODY: &str = r#"
        /std/print(Nat/to_str(drop(0)))
        "#;

/// The same argument supplied in all four positions at once, so the refusals above cannot be passing for "a declaration may not mention an implicit".
pub(super) const A_SOLVED_METAVARIABLE_IN_EVERY_POSITION: &str = r#"
        induct Fine : pub Type
        | c(x : /std/Eq()(drop(@Nat, 0), 0))
        end

        struct Also : pub Type {
            x : /std/Eq()(drop(@Nat, 0), 0)
        }

        let f(x : /std/Eq()(drop(@Nat, 0), 0)) -> Nat = 0;

        /std/print(Nat/to_str(drop(@Nat, 0)))
        "#;

pub(super) const A_NESTED_PROPOSITION_VALUED_INDEX_CANNOT_MAKE_AN_ELIMINATION_VACUOUS: &str = r#"
        use /std/{Nat};
        use /std/Bool/{False};

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

        let boom(w : Ind(Pair/mk(0, Two/b()))) -> False =
            match w : (x, q) => False
            end;

        let bad : False = boom(coerce(Ind/only()));

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
        use /std/{Eq, Nat, Option};
        use /std/Bool/{False};

        induct Color : pub Type
        | red()
        | green()
        end

        induct Same(@A : Type) : (A, A) -> pub Prop
        | same(@z : A) : (z, z)
        end

        let absurd_bool(h : Eq()(false, true)) -> False =
            match h end;

        let absurd_color(h : Eq()(Color/green(), Color/red())) -> False =
            match h end;

        let absurd_nat(h : Eq()(0, 1)) -> False =
            match h end;

        let absurd_successor(n : Nat, h : Eq()(0, n + 1)) -> False =
            match h end;

        let absurd_option(h : Eq()(Option/some(1), Option/none())) -> False =
            match h end;

        let absurd_same(h : Same()(Color/green(), Color/red())) -> False =
            match h end;

        /std/print("ok")
        "#;

pub(super) const TWO_PROOFS_FORCED_ON_ONE_BINDER_DO_NOT_CLASH: &str = r#"
        use /std/{Eq};
        use /std/Bool/{False};

        induct Two : pub Prop
        | a()
        | b()
        end

        let absurd(h : Eq()(Two/a(), Two/b())) -> False =
            match h end;

        let forged : False = absurd(Eq/refl());

        /std/print("FORGED")
        "#;

pub(super) const AN_OPEN_FORCING_DOES_NOT_CLASH: &str = r#"
        use /std/{Eq};
        use /std/Bool/{False};

        induct Color : pub Type
        | red()
        | green()
        end

        let absurd(c : Color, h : Eq()(c, Color/red())) -> False =
            match h end;

        /std/print("FORGED")
        "#;

pub(super) const TWO_APPLICATIONS_OF_ONE_OPAQUE_FUNCTION_DO_NOT_CLASH: &str = r#"
        use /std/{Eq, Nat};
        use /std/Bool/{False};

        let absurd(f : (Nat) -> Nat, h : Eq()(f(0), f(1))) -> False =
            match h end;

        /std/print("FORGED")
        "#;

pub(super) const A_PARITY_DISAGREEMENT_IS_NOT_A_CLASH: &str = r#"
        use /std/{Eq, Nat};
        use /std/Bool/{False};

        let absurd(x : Nat, y : Nat, h : Eq()(x * 2 + 1, y * 2)) -> False =
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
        use /std/{Eq, Nat};
        use /std/Bool/{False};

        induct Box : pub Prop
        | mk(A : Type)
        end

        let unbox(b : Box) -> Type =
            match b : (_) => Type
            | mk(A) => A
            end;

        let boxes_equal(A : Type, B : Type) -> Eq()(Box/mk(A), Box/mk(B)) = Eq/refl();

        let types_equal(A : Type, B : Type) -> Eq()(A, B) =
            Eq/cong(unbox, boxes_equal(A, B));

        let bad : False =
            Eq/subst((t : Type) => t, types_equal(Nat, False), 0);

        /std/print("FORGED")
        "#;

pub(super) const A_PROPOSITION_MAY_NOT_CARRY_A_TYPE_FIELD: &str = r#"
        struct Bad : pub Prop {
            carried : Type
        }

        /std/Io/pure(())
        "#;

pub(super) const A_LIST_OF_PROOFS_IS_NOT_A_PROPOSITION: &str = r#"
        use /std/{Eq, List};
        use /std/Bool/{True};

        let all_equal(@X : Prop, x : X, y : X) -> Eq()(x, y) =
            Eq/refl();

        let one : List(True) = [True/qed()];
        let none : List(True) = [];

        let bad : Eq()(one, none) =
            all_equal(one, none);

        /std/print("FORGED")
        "#;

/// The witness's lemma at a *genuine* proposition, which must stay accepted: `all_equal` is sound, and a fix that closed the hole by refusing `Prop`-abstracted binders would take this with it.
pub(super) const IRRELEVANCE_STILL_IDENTIFIES_A_PROPOSITIONS_INHABITANTS: &str = r#"
        use /std/{Nat, Eq};

        induct Two : pub Prop
        | a()
        | b()
        end

        let all_equal(@X : Prop, x : X, y : X) -> Eq()(x, y) =
            Eq/refl();

        let same : Eq()(Two/a(), Two/b()) =
            all_equal(Two/a(), Two/b());

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_LIST_OF_PROOFS_IS_STILL_A_LIST: &str = r#"
        use /std/{Nat, List};
        use /std/Bool/{True};

        let one : List(True) = [True/qed()];

        /std/print(Nat/to_str(List/len(one)))
        "#;

pub(super) const A_CATCH_ALL_IS_CHECKED_AT_ITS_SCRUTINEE: &str = r#"
        use /std/{Nat, Eq};

        induct Three : pub Type
        | a()
        | b()
        | c()
        end

        let same(t : Three) -> Eq()(t, t) =
            match t : (q) => Eq()(q, q)
            | a() => Eq/refl()
            | _ => Eq/refl()
            end;

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_RECORD_OF_PROPOSITIONS_IS_A_PROPOSITION: &str = r#"
        use /std/{Nat, Eq};

        struct Holder : pub Prop {
            field : {Eq()(0, 0), Eq()(1, 1)}
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
            field : (A : Type) -> Eq()(0, 0)
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

        let into_record(t : Two) -> {Eq()(0, 0), Eq()(1, 1)} =
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

        let function(g : (Nat) -> Nat) -> Eq()((x : Nat) => g(x), g) = Eq/refl();

        let record(p : {Nat, Bool}) -> Eq()((p.0, p.1), p) = Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

pub(super) const AN_EXPANSION_THAT_DROPS_ITS_BINDER_IS_NOT_ETA: &str = r#"
        use /std/{Eq, Nat};

        let dropped(g : (Nat) -> Nat) -> Eq()((x : Nat) => g(0), g) = Eq/refl();

        /std/Io/pure(())
        "#;

pub(super) const AN_EXPANSION_THAT_SWAPS_ITS_COMPONENTS_IS_NOT_ETA: &str = r#"
        use /std/{Eq, Nat};

        let swapped(p : {Nat, Nat}) -> Eq()((p.1, p.0), p) = Eq/refl();

        /std/Io/pure(())
        "#;

pub(super) const A_FUNCTION_INTO_A_PROPOSITION_IS_DISCHARGED_BEFORE_ETA: &str = r#"
        use /std/{Eq, Nat};

        let same(g : (Nat) -> Eq()(0, 0), h : (Nat) -> Eq()(0, 0)) -> Eq()(g, h) = Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_FUNCTION_INTO_A_TYPE_IS_NOT_DISCHARGED_UNCOMPARED: &str = r#"
        use /std/{Eq, Nat};

        let same(g : (Nat) -> Nat, h : (Nat) -> Nat) -> Eq()(g, h) = Eq/refl();

        /std/Io/pure(())
        "#;

pub(super) const ETA_HANDS_A_RECORDS_PROOF_COMPONENT_TO_IRRELEVANCE: &str = r#"
        use /std/{Eq, Nat};

        let same(g : (Nat) -> {Nat, Eq()(0, 0)}, p : Eq()(0, 0))
            -> Eq()(g, (x : Nat) => (g(x).0, p)) = Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

pub(super) const ETA_STILL_COMPARES_A_RECORDS_RELEVANT_COMPONENT: &str = r#"
        use /std/{Eq, Nat};

        let same(p : {Nat, Eq()(0, 0)}) -> Eq()(p, (0, p.1)) = Eq/refl();

        /std/Io/pure(())
        "#;

pub(super) const A_SPINE_ARGUMENT_COMPARES_AT_THE_HEADS_DOMAIN: &str = r#"
        use /std/{Eq, Nat};

        let ground(f : (Eq()(0, 0)) -> Nat, p : Eq()(0, 0), q : Eq()(0, 0)) -> Eq()(f(p), f(q)) =
            Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_DEFINITION_APPLIED_TO_TWO_PROOFS_CONVERTS_BEFORE_UNFOLDING: &str = r#"
        use /std/{Eq, Nat};

        induct Z: (Nat) -> pub Prop
        | mk(): (0)
        end

        let h(n : Nat, e : Z(n)) -> Nat = match e | mk() => 1 end;

        let same(n : Nat, p : Z(n), q : Z(n), x : Eq()(h(n, p), 0)) -> Eq()(h(n, q), 0) = x;

        /std/print(Nat/to_str(1))
        "#;

pub(super) const AN_INTRINSIC_APPLIED_TO_TWO_PROOFS_CONVERTS_AT_THEIR_PROPOSITION: &str = r#"
        use /std/{Eq, Nat};

        let halve(a : Nat, b : Nat, @p : Nat/Lt(0, b)) -> Nat = Nat/div(a, b, @p);

        let same(a : Nat, b : Nat, p : Nat/Lt(0, b), q : Nat/Lt(0, b)) -> Eq()(halve(a, b, @p), Nat/div(a, b, @q)) = Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_POLYMORPHIC_DEFINITION_APPLIED_TO_TWO_PROOFS_CONVERTS: &str = r#"
        use /std/{Eq, Nat};

        let h(n : Nat, e : Eq()(n, n)) -> Nat = match e | refl(@_) => 1 end;

        let same(n : Nat, p : Eq()(n, n), q : Eq()(n, n), x : Eq()(h(n, p), 0)) -> Eq()(h(n, q), 0) = x;

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_RECURSIVE_FUNCTION_CARRYING_A_PROOF_CONVERTS_WITHOUT_UNFOLDING: &str = r#"
        use /std/{Eq, Nat};

        induct T: pub Prop
        | t()
        end

        let g(n : Nat, p : T) -> Nat = match n | 0 => 0 | k + 1; _ => g(k, p) + 1 end;

        let same(n : Nat, p : T, q : T, x : Eq()(g(n, p), 0)) -> Eq()(g(n, q), 0) = x;

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_RECURSIVE_FUNCTION_MATCHING_ITS_PROOF_CONVERTS: &str = r#"
        use /std/{Eq, Nat};

        induct T: pub Prop
        | t()
        end

        let g(n : Nat, p : T) -> Nat = match n | 0 => (match p | t() => 0 end) | k + 1; _ => g(k, p) + 1 end;

        let same(n : Nat, p : T, q : T, x : Eq()(g(n, p), 0)) -> Eq()(g(n, q), 0) = x;

        /std/print(Nat/to_str(1))
        "#;

pub(super) const TWO_ACCESSIBILITY_PROOFS_AT_ONE_RECURSIVE_CALL_CONVERT: &str = r#"
        use /std/{Eq, Nat};
        use /std/WellFounded/{Accessible};

        let R(y : Nat, x : Nat) -> Prop = Nat/Lt(x, y);

        let f(n : Nat, lt : (k : Nat) -> Nat/Lt(k, k + 1), a : Accessible(R)(n)) -> Nat =
            match a | intro(@_, below) => f(n + 1, lt, below(n + 1, lt(n))) end;

        let inv(n : Nat, a : Accessible(R)(n)) -> (y : Nat, r : R(y, n)) -> Accessible(R)(y) =
            (y, r) => match a | intro(@_, below) => below(y, r) end;

        let same(lt : (k : Nat) -> Nat/Lt(k, k + 1), a : Accessible(R)(0), x : Eq()(f(0, lt, a), 0))
            -> Eq()(f(0, lt, Accessible/intro(inv(0, a))), 0) = x;

        /std/print(Nat/to_str(1))
        "#;

pub(super) const AN_INFERRED_VALUE_UNDER_A_REFINED_PROOF_KEEPS_ITS_UNIVERSE_INSTANCE: &str = r#"
        use /std/{Eq, Nat};

        let k(@x: Eq()(0, 0), _: Eq()(x, x)) -> Nat = 1;

        let f(h : (A : Prop) -> A, w : Eq()(h(Eq()(0, 0)), h(Eq()(0, 0)))) -> Nat =
            k(match h(Eq()(0, 0)) | refl(@_) => w end);

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_PROOF_IS_NOT_REDUCED_TO_COMPARE_IT_WITH_ANOTHER: &str = r#"
        use /std/{Eq, Nat};

        let Bot : Prop = (A : Prop) -> A;
        let Top : Prop = (Bot) -> Bot;
        let cast(A : Prop, B : Prop, e : Eq()(A, B), x : A) -> B = match e | refl(@_) => x end;
        let delta : Top = (z) => z(Top)(z);
        let omega(h : (A : Prop, B : Prop) -> Eq()(A, B)) -> Bot = (A) => cast(Top, A, h(Top, A), delta);
        let Omega(h : (A : Prop, B : Prop) -> Eq()(A, B)) -> Bot = delta(omega(h));

        induct T: pub Prop
        | t()
        end

        let within(h : (A : Prop, B : Prop) -> Eq()(A, B), y : T, w : Eq(@T)(y, y)) -> Nat =
            match h(Top, Top)
            | refl(@_) =>
                let _v : Eq(@T)(Omega(h)(T), y) = w;
                0
            end;

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_STRUCTS_FUNCTION_FIELD_MEETS_A_NEUTRAL_APPLICATION: &str = r#"
        use /std/{Eq, Nat, State};

        let left(a: Nat, f: (Nat) -> State(Nat, Nat)) -> Eq()(State/bind(State/pure(a), f), f(a)) =
            Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_STRUCTS_FUNCTION_FIELD_MEETS_A_NEUTRAL_VARIABLE: &str = r#"
        use /std/{Eq, Nat, State};

        let right(m: State(Nat, Nat)) -> Eq()(State/bind(m, (v) => State/pure(v)), m) =
            Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

pub(super) const TWO_STRUCT_LITERALS_COMPARE_THEIR_FUNCTION_FIELDS: &str = r#"
        use /std/{Eq, Nat, State};

        let assoc(m: State(Nat, Nat), f: (Nat) -> State(Nat, Nat), g: (Nat) -> State(Nat, Nat))
            -> Eq()(State/bind(State/bind(m, f), g), State/bind(m, (v) => State/bind(f(v), g))) =
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

        let field(n : Nat, p : P, q : P) -> Eq()(S { n = n, p = p }, S { n = n, p = q }) = Eq/refl();
        let payload(n : Nat, p : P, q : P) -> Eq()(W/wrap(n, p), W/wrap(n, q)) = Eq/refl();
        let some(p : P, q : P) -> Eq()(Option/some(p), Option/some(q)) = Eq/refl();
        let assoc(a : Str, b : Str, c : Str)
            -> Eq()(Str/concat(Str/concat(a, b), c), Str/concat(a, Str/concat(b, c))) = Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

pub(super) const A_NOMINAL_STRUCTS_ETA_IS_NOT_FORFEITED_THERE: &str = r#"
        use /std/{Eq, Nat};

        struct Sealed : pub Type {
            one : Eq()(0, 0),
            two : Eq()(1, 1)
        }

        let same(f : (Sealed) -> Nat, b : Sealed, p : Eq()(0, 0), q : Eq()(1, 1))
            -> Eq()(f(Sealed { one = p, two = q }), f(b)) = Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

/// The premise every rule above is stated over and no entry under `documentation/design/soundness/` names: a type is a *pure* term. A description sitting at the type level is a value, not an error, so what refuses the program is the scrutinee's own type. `Cell/fill(c, true) : Io(Bool)` describes an attempt instead of performing it, so it is not a `Bool`, not something `match` can eliminate, and not something `Eq` can be stated over. The fixture uses the write-once operation, whose repeated attempts can still yield different Booleans; its refusal must land on the unforced description.
pub(super) const AN_EFFECTFUL_SCRUTINEE_IS_NOT_A_VALUE: &str = r#"
    use /std/{Cell, Eq, Bool, Str};

    let c = Cell/new(@Bool)!;

    let forged : Str =
        match Cell/fill(c, true)
        | true =>
            let p : Eq()(Cell/fill(c, true), true) = Eq/refl();
            let done = Cell/fill(c, false);
            match Cell/fill(c, true)
            | true => "second read true"
            | false => match Bool/false_neq_true(p) end
            end
        | false => "first read false"
        end;

    /std/print(forged)
    "#;

/// The control. Only the refinement's escape into a *type* is at issue, so a rule that refused the elimination outright, or refused `Cell/poll` in every position, would be a brick. Forcing the poll yields an ordinary `Option(Bool)`, from which the fixture reads its Boolean.
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

// The same premise behind a stuck head. With the read an argument of the parameters `f`, `g` and `h`, weak-head reduction stops at the head without visiting it, so one spelling of the read would be refined to `true` in the outer arm and, after `Cell/fill(c, false)`, to `false` in the inner one — `h` carrying the outer arm's knowledge across, so `p` re-reads at `Eq()(false, true)` and `/std/Bool/false_neq_true` turns it into `/std/Bool/False`. Two heads rather than one because the kernel drops a nested refinement of a *single* key: `assume_case_value` reduces the inner scrutinee under the outer arm's equation, gets the literal `true` back, and `Scope::refine` skips a key with no local free. The read's type is `Io(Bool)`, which `f : (Bool) -> Bool` does not take, so the argument is refused before any arm records an equation.
pub(super) const AN_EFFECT_BEHIND_A_STUCK_HEAD_IS_NOT_AN_ARGUMENT: &str = r#"
    use /std/{Cell, Eq, Bool, Str};

    /std/print(
        ((f : (Bool) -> Bool,
          g : (Bool) -> Bool,
          h : (x : Bool) -> Eq()(g(x), f(x)),
          c : Cell(Bool)) =>
            match f(Cell/fill(@Bool, c, true))
            | true =>
                let step(p : Eq()(g(Cell/fill(@Bool, c, true)), true)) -> Str =
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

/// The control, and it guards against a brick: a scrutinee whose head is stuck is the *ordinary* case — `flip(b)` for a `b` nothing can instantiate — and refining it is what lets a hypothesis stated over the scrutinee re-read at the arm's value. So a guard that refused every stuck application, or every application it could not fully reduce, would still reject this.
///
/// The head is a *definition* here; [`a_parameter_headed_scrutinee_refines_again`] is the same control over a parameter.
pub(super) const A_STUCK_APPLICATION_SCRUTINEE_STILL_REFINES: &str = r#"
    use /std/{Eq, Bool, Str};

    let flip(b : Bool) -> Bool = Bool/not(b);

    let refined(b : Bool, p : Eq()(flip(b), true)) -> Str =
        match flip(b)
        | true => "t"
        | false => match Bool/false_neq_true(p) end
        end;

    /std/print(refined(false, Eq/refl()))
    "#;

// The route no search over the term could close. The scrutinee is `f(true)` for a *parameter* `f`: nothing in it names an effect, and at the moment an arm records its equation the binder has no value to inspect, so whether `f(true)` performs one is a property of the environment rather than of the term. The caller's `(b) => Cell/fill(c, true)` has type `(Bool) -> Io(Bool)` and does not inhabit `(Bool) -> Bool`, so the *caller's argument* is refused and the derivation never reaches an arm, a refinement, or an equation. What removes the class is an effect discipline on the arrow rather than a walk over the term (see `documentation/design/soundness/effects/a-term-outside-io-performs-no-effect.md`), and [`a_parameter_headed_scrutinee_refines_again`] is what such a walk would cost.
pub(super) const AN_EFFECT_CANNOT_INHABIT_A_PURE_ARROW: &str = r#"
    use /std/{Cell, Eq, Bool, Str};
    use /std/Bool/{False};

    let forge(f : (Bool) -> Bool, c : Cell(Bool), p : Eq()(f(true), true)) -> Str =
        let done = Cell/fill(c, false);
        match f(true)
        | false =>
            let contradiction : False = Bool/false_neq_true(p);
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

/// A parameter-headed scrutinee refines. A walk over the term cannot read a binder's body, so it cannot tell a pure `f` from an effectful one, and refusing the equation would withhold a refinement from every program stating a hypothesis over an opaque head; purity is a typing fact, so the equation is licensed and `p` re-reads at the arm's value.
pub(super) const A_PARAMETER_HEADED_SCRUTINEE_REFINES_AGAIN: &str = r#"
    use /std/{Eq, Bool, Str};

    let refined(f : (Bool) -> Bool, b : Bool, p : Eq()(f(b), true)) -> Str =
        match f(b)
        | true => "t"
        | false => match Bool/false_neq_true(p) end
        end;

    /std/print(refined((x) => x, true, Eq/refl()))
    "#;

/// A partial definition behind a `Type`-sorted carrier, reached four ways. The kernel's local gate does not fire — `Box` is neither a proposition nor a sort — so these are the class the erasure obligations in `curios-cert` exist for.
pub(super) const PARTIAL_DIRECT: &str = r#"
    use /std/{Nat};
    use /std/Bool/{False};
    struct Box : pub Type { p : False }
    let loop(n : Nat) -> Box = loop(n);
    let bad : False = loop(0).p;
    /std/print("FORGED")
    "#;

pub(super) const PARTIAL_THROUGH_WITNESS: &str = r#"
    use /std/{Nat};
    use /std/Bool/{False};
    struct Box : pub Type { p : False }
    concept Make(A : Type) : pub Type { make(A) -> Box, }
    let loop(n : Nat) -> Box = loop(n);
    satisfy Make(Nat) { make(n) = loop(n), }
    let bad : False = Make/make(0).p;
    /std/print("FORGED")
    "#;

pub(super) const PARTIAL_HIGHER_ORDER: &str = r#"
    use /std/{Nat};
    use /std/Bool/{False};
    struct Box : pub Type { p : False }
    let loop(n : Nat) -> Box = loop(n);
    let apply(f : (Nat) -> Box, n : Nat) -> Box = f(n);
    let bad : False = apply(loop, 0).p;
    /std/print("FORGED")
    "#;

pub(super) const PARTIAL_IN_FIELD: &str = r#"
    use /std/{Nat};
    use /std/Bool/{False};
    struct Box : pub Type { p : False }
    struct Holder : pub Type { run : (Nat) -> Box }
    let loop(n : Nat) -> Box = loop(n);
    let holder : Holder = Holder { run = loop };
    let bad : False = holder.run(0).p;
    /std/print("FORGED")
    "#;

/// A diverging proof in a position the judgment *infers* rather than checks — a match scrutinee — inside a definition whose own type is relevant, so the body is not a proof position either.
///
/// Both checkers record every settled node with the type it settled at, checked or inferred alike. Nothing here is a checked position, and the elimination conjures a `Nat` from a proof that never terminates, so a checker seeding only checked positions would accept it — the quadrant this matrix exists to make visible.
pub(super) const INFERRED_PROOF_POSITION: &str = r#"
    use /std/{Nat};
    use /std/Bool/{False};
    struct Box : pub Type { p : False }
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
    use /std/{Nat};
    use /std/Bool/{False};
    struct Box : pub Type { p : False }
    let make : Box =
        let r : Box = r;
        r;
    let bad : False = make.p;
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
/// In the successor arm the hypothesis is the motive at the *predecessor*, and the goal is the motive at the successor. Handing the hypothesis back directly proves `Eq()(k + 1, 0)` from `Eq()(k, 0)`, so a rule that typed the hypothesis at the scrutinee rather than at the peeled index would make every predicate provable by induction.
pub(super) const INDUCTION_HYPOTHESIS_AT_THE_SCRUTINEE: &str = r#"
    use /std/{Nat, Eq};
    let bogus(n : Nat) -> Eq()(n, 0) =
        match n : (m) => Eq()(m, 0)
        | 0 => Eq/refl()
        | k + 1; ih => ih
        end;
    /std/print("FORGED")
    "#;

/// A natural-number dispatch whose default is checked at a case's instance rather than the scrutinee's.
///
/// The default binds nothing and refines no index, so it must be checked at the scrutinee's own value: its goal here is `Eq()(n, 0)` for an arbitrary `n`, which `Eq/refl()` cannot inhabit. Were it checked at the `0` arm's instance — the shape a refinement leak would produce — reflexivity would discharge it and every natural would equal zero.
pub(super) const DISPATCH_DEFAULT_AT_A_CASE: &str = r#"
    use /std/{Nat, Eq};
    let bogus(n : Nat) -> Eq()(n, 0) =
        match n : (m) => Eq()(m, 0)
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
    use /std/{Nat};
    use /std/Bool/{False};
    let bogus(n : Nat) -> False = bogus(n - 1);
    let bad : False = bogus(0);
    /std/print("FORGED")
    "#;

/// Permuting the arguments is not a descent either.
///
/// The classic size-change subtlety: every call maps a parameter to a parameter, so each call matrix is full of `Same` entries and none is `Less`. Composing a swap with itself returns the identity, so no cycle carries a strict decrease and the group cannot be total — an engine reading "the argument came from a parameter" as progress would certify a recursion that runs forever.
pub(super) const PERMUTING_ARGUMENTS_IS_NOT_DESCENT: &str = r#"
    use /std/{Nat};
    use /std/Bool/{False};
    let bogus(a : Nat, b : Nat) -> False = bogus(b, a);
    let bad : False = bogus(0, 1);
    /std/print("FORGED")
    "#;

/// Two distinct recursions are not the same function.
///
/// Neither descends, so both are legal values, and both fold to themselves — which is the shape that puts conversion's recurrence rule to work: comparing them unfolds each once, arrives at the same goal, and a history that treated "already assumed" as "proved" would equate two definitions that differ. `f` is constantly zero and `g` constantly one, so equating them and transporting along the equality gives `Eq()(0, 1)`.
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
    let same : Eq()(f, g) = Eq/refl();
    /std/print("FORGED")
    "#;

/// A `Bool` arm is checked at *its own* case value.
///
/// The `false` arm's goal is the motive at `false`, so `Eq/refl()` would have to inhabit `Eq()(false, true)`. An arm rule that refined the scrutinee to the wrong case — or to none at all — would let reflexivity discharge it, and every boolean would equal `true`.
pub(super) const BOOL_ARM_AT_THE_WRONG_CASE: &str = r#"
    use /std/{Bool, Eq};
    let bogus(b : Bool) -> Eq()(b, true) =
        match b : (c) => Eq()(c, true)
        | true => Eq/refl()
        | false => Eq/refl()
        end;
    /std/print("FORGED")
    "#;

/// A natural-number dispatch's *literal* arm is checked at that literal.
///
/// The `1` arm's goal is the motive at `1`, which `Eq/refl()` cannot inhabit for `Eq()(1, 0)`. This is the companion to the default-arm fixture: there the danger is refining an arm that binds nothing, here it is refining a literal arm to the wrong literal.
pub(super) const DISPATCH_LITERAL_AT_THE_WRONG_VALUE: &str = r#"
    use /std/{Nat, Eq};
    let bogus(n : Nat) -> Eq()(n, 0) =
        match n : (m) => Eq()(m, 0)
        | 0 => Eq/refl()
        | 1 => Eq/refl()
        | _ => Eq/refl()
        end;
    /std/print("FORGED")
    "#;

pub(super) const A_FOLD_MOTIVE_MAY_NOT_CAPTURE_ITS_SCRUTINEE: &str = r#"
        use /std/{Nat, Eq};

        let all_zero(n : Nat) -> Eq()(n, 0) =
            match n : (_) => Eq()(n, 0)
            | 0 => Eq/refl()
            | k + 1; ih => ih
            end;

        let boom : Eq()(1, 0) = all_zero(1);

        /std/print("unreachable")
        "#;

pub(super) const A_LIST_FOLD_MOTIVE_MAY_NOT_CAPTURE_ITS_SCRUTINEE: &str = r#"
        use /std/{Nat, Eq, List};

        let all_empty(l : List(Nat)) -> Eq()(List/len(l), 0) =
            match l : (_) => Eq()(List/len(l), 0)
            | [] => Eq/refl()
            | [h, ..t]; ih => ih
            end;

        let boom : Eq()(1, 0) = all_empty([7]);

        /std/print("unreachable")
        "#;

pub(super) const A_FOLD_MOTIVE_MAY_NOT_CAPTURE_ITS_SCRUTINEE_EXPRESSION: &str = r#"
        use /std/{Nat, Eq};

        let twice(n : Nat) -> Nat = n + n;

        let all_zero(n : Nat) -> Eq()(twice(n), 0) =
            match twice(n) : (_) => Eq()(twice(n), 0)
            | 0 => Eq/refl()
            | k + 1; ih => ih
            end;

        let boom : Eq()(2, 0) = all_zero(1);

        /std/print("unreachable")
        "#;

pub(super) const A_FOLD_MOTIVE_MAY_NOT_CAPTURE_ITS_SCRUTINEE_THROUGH_AN_ALIAS: &str = r#"
        use /std/{Nat, Eq};

        let all_zero(n : Nat) -> Eq()(n, 0) =
            let y = n;
            match n : (_) => Eq()(y, 0)
            | 0 => Eq/refl()
            | k + 1; ih => ih
            end;

        let boom : Eq()(1, 0) = all_zero(1);

        /std/print("unreachable")
        "#;

pub(super) const A_FOLD_MOTIVE_THAT_BINDS_ITS_SCRUTINEE_STILL_FOLDS: &str = r#"
        use /std/{Nat, Eq};

        let plus_zero(n : Nat) -> Eq()(n + 0, n) =
            match n : (m) => Eq()(m + 0, m)
            | 0 => Eq/refl()
            | k + 1; ih => Eq/cong((w : Nat) => w + 1, ih)
            end;

        /std/print(Nat/to_str(3))
        "#;
