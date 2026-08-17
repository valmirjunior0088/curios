//! End-to-end coverage for the soundness perimeter entries that nothing else guards.
//!
//! `design.md` states the consistency claim against an enumerated perimeter, and `soundness.md` is that perimeter: one entry per rule, each graded *probed*, *argued*, or *auditable only*. "Probed" is a claim about executable evidence, so it needs a test that fails when the rule stops holding — otherwise the grade records what someone once tried by hand and decays the moment nobody remembers doing it.
//!
//! The entries with their own homes are not repeated here: strict positivity lives in `tests::positivity`, the two totality obligations in `tests::soundness`, and witness coherence in `tests::concepts`. What is left is the large-elimination guard, `Prop` non-informativeness, coverage, and the foreign wire contract — four rules the claim rests on that had no regression test at all.
//!
//! Each rejection asserts its *own* diagnostic, following `tests::soundness`. A perimeter test that accepts any error is worse than none: an invalid fixture passes it while the rule it names goes unchecked. That is not hypothetical — the first draft of these probes "passed" on `unbound variable`, having never reached the check at all.

use {
    super::{run, run_text},
    curios_pipeline::recheck_with_prelude as recheck,
    curios_runtime::MockHost,
    curios_text::{Entrypoint, RootSource},
};

/// Reject `source`, and by the diagnostic naming the rule under test.
fn rejected_by(source: &str, diagnostic: &str) {
    let (system, _io) = MockHost::builder().build();
    let error =
        run_text(source, system).expect_err("expected the perimeter rule to reject this program");
    assert!(
        error.contains(diagnostic),
        "rejected, but not by '{diagnostic}':\n{error}",
    );
}

const A_MULTI_CONSTRUCTOR_PROPOSITION_CANNOT_BE_ELIMINATED_INTO_DATA: &str = r#"
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

const AN_EMPTY_PROPOSITION_STILL_ELIMINATES_INTO_DATA: &str = r#"
        use /std/{Nat, False};

        let ex_falso(f : False) -> Nat =
            match f
            end;

        /std/print(Nat/to_str(0))
        "#;

const A_PROPOSITION_STILL_ELIMINATES_INTO_ANOTHER_PROPOSITION: &str = r#"
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

const A_PROPOSITION_MAY_NOT_CARRY_INFORMATIVE_FIELDS: &str = r#"
        use /std/{Nat};

        struct Bad : pub Prop {
            value : Nat
        }

        /std/Io/pure(())
        "#;

const A_PROPOSITION_CONCEPT_MAY_NOT_CARRY_INFORMATIVE_METHODS: &str = r#"
        use /std/{Nat};

        concept Bad(A : Type) : pub Prop {
            get(A) -> Nat,
        }

        /std/Io/pure(())
        "#;

const AN_ELIMINATION_MUST_ENUMERATE_ITS_CONSTRUCTORS: &str = r#"
        use /std/{Nat, Option};

        let f(o : Option(Nat)) -> Nat =
            match o
            | some(x) => x
            end;

        f(Option/none())
        "#;

const A_FOREIGN_DECLARATION_IS_CONFINED_TO_WIRE_TYPES: &str = r#"
        use /std/{Str};

        foreign bad : Str;

        /std/Io/pure(())
        "#;

const A_NON_INJECTIVE_INDEX_TARGET_DOES_NOT_FORCE_ITS_BINDER: &str = r#"
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

const A_PROPOSITION_VALUED_INDEX_CANNOT_MAKE_AN_ELIMINATION_VACUOUS: &str = r#"
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

const A_PROPOSITION_VALUED_INDEX_CANNOT_EXCUSE_AN_OMITTED_ARM: &str = r#"
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
const AN_UNCONSTRAINED_IMPLICIT: &str = r#"
        use /std/{Nat};

        let drop(@A : Type, n : Nat) -> Nat = n;
"#;

const A_METAVARIABLE_IN_AN_INDUCT_TELESCOPE: &str = r#"
        induct Bad : pub Type
        | c(x : /std/Eq(drop(0), 0))
        end

        /std/Io/pure(())
        "#;

const A_METAVARIABLE_IN_A_STRUCT_FIELD: &str = r#"
        struct Bad : pub Type {
            x : /std/Eq(drop(0), 0)
        }

        /std/Io/pure(())
        "#;

const A_METAVARIABLE_IN_A_DEFINITIONS_TYPE: &str = r#"
        let f(x : /std/Eq(drop(0), 0)) -> Nat = 0;

        /std/Io/pure(())
        "#;

const A_METAVARIABLE_IN_THE_ENTRYPOINT_BODY: &str = r#"
        /std/print(Nat/to_str(drop(0)))
        "#;

/// The same argument supplied in all four positions at once, so the refusals above cannot be passing for "a declaration may not mention an implicit".
const A_SOLVED_METAVARIABLE_IN_EVERY_POSITION: &str = r#"
        induct Fine : pub Type
        | c(x : /std/Eq(drop(@Nat, 0), 0))
        end

        struct Also : pub Type {
            x : /std/Eq(drop(@Nat, 0), 0)
        }

        let f(x : /std/Eq(drop(@Nat, 0), 0)) -> Nat = 0;

        /std/print(Nat/to_str(drop(@Nat, 0)))
        "#;

const A_NESTED_PROPOSITION_VALUED_INDEX_CANNOT_MAKE_AN_ELIMINATION_VACUOUS: &str = r#"
        use /std/{Nat, False};

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

const A_NESTED_PROPOSITION_VALUED_INDEX_CANNOT_EXCUSE_AN_OMITTED_ARM: &str = r#"
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

const A_NESTED_RELEVANT_CLASH_STILL_EXCUSES_AN_OMITTED_ARM: &str = r#"
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

const AN_UNMENTIONED_PAYLOAD_BINDER_IS_NOT_FORCED: &str = r#"
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

const A_SINGLETON_CARRYING_A_TYPE_DOES_NOT_ELIMINATE: &str = r#"
        use /std/{Eq, False, Nat};

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

        let bad : False =
            Eq/subst((t : Type) => t, types_equal(Nat, False), 0);

        /std/print("FORGED")
        "#;

const A_PROPOSITION_MAY_NOT_CARRY_A_TYPE_FIELD: &str = r#"
        struct Bad : pub Prop {
            carried : Type
        }

        /std/Io/pure(())
        "#;

const A_LIST_OF_PROOFS_IS_NOT_A_PROPOSITION: &str = r#"
        use /std/{Eq, List, True};

        let all_equal(@X : Prop, x : X, y : X) -> Eq(x, y) =
            Eq/refl();

        let one : List(True) = [True/qed()];
        let none : List(True) = [];

        let bad : Eq(one, none) =
            all_equal(one, none);

        /std/print("FORGED")
        "#;

/// The witness's lemma at a *genuine* proposition, which must stay accepted: `all_equal` is sound, and a fix that closed the hole by refusing `Prop`-abstracted binders would take this with it. Type-checked rather than run — it is the shape the erase boundary cannot lower.
const IRRELEVANCE_STILL_IDENTIFIES_A_PROPOSITIONS_INHABITANTS: &str = r#"
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

const A_LIST_OF_PROOFS_IS_STILL_A_LIST: &str = r#"
        use /std/{Nat, List, True};

        let one : List(True) = [True/qed()];

        /std/print(Nat/to_str(List/len(one)))
        "#;

const A_CATCH_ALL_IS_CHECKED_AT_ITS_SCRUTINEE: &str = r#"
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

const A_RECORD_OF_PROPOSITIONS_IS_A_PROPOSITION: &str = r#"
        use /std/{Nat, Eq};

        struct Holder : pub Prop {
            field : {Eq(0, 0), Eq(1, 1)}
        }

        /std/print(Nat/to_str(1))
        "#;

const THE_EMPTY_RECORD_IS_NOT_A_PROPOSITION: &str = r#"
        struct Holder : pub Prop {
            field : {}
        }

        /std/Io/pure(())
        "#;

const A_FUNCTION_INTO_A_PROPOSITION_IS_A_PROPOSITION: &str = r#"
        use /std/{Nat, Eq};

        struct Holder : pub Prop {
            field : (A : Type) -> Eq(0, 0)
        }

        /std/print(Nat/to_str(1))
        "#;

const A_FUNCTION_INTO_A_TYPE_IS_NOT_A_PROPOSITION: &str = r#"
        struct Holder : pub Prop {
            field : (A : Type) -> A
        }

        /std/Io/pure(())
        "#;

const A_PROPOSITION_STILL_ELIMINATES_INTO_A_FORMED_PROPOSITION: &str = r#"
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

const A_PROPOSITION_MAY_NOT_BE_ELIMINATED_INTO_A_FORMED_TYPE: &str = r#"
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

const A_NON_STRICT_OCCURRENCE_BEHIND_A_RECORD_IS_STILL_REFUSED: &str = r#"
        induct Bad : pub Type
        | mk(f : {((Bad) -> Prop) -> Prop})
        end

        /std/Io/pure(())
        "#;

const ETA_CONVERTS_A_FUNCTION_AND_A_RECORD_WITH_THEIR_EXPANSIONS: &str = r#"
        use /std/{Eq, Nat, Bool};

        let function(g : (Nat) -> Nat) -> Eq((x : Nat) => g(x), g) = Eq/refl();

        let record(p : {Nat, Bool}) -> Eq((p.0, p.1), p) = Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

const AN_EXPANSION_THAT_DROPS_ITS_BINDER_IS_NOT_ETA: &str = r#"
        use /std/{Eq, Nat};

        let dropped(g : (Nat) -> Nat) -> Eq((x : Nat) => g(0), g) = Eq/refl();

        /std/Io/pure(())
        "#;

const AN_EXPANSION_THAT_SWAPS_ITS_COMPONENTS_IS_NOT_ETA: &str = r#"
        use /std/{Eq, Nat};

        let swapped(p : {Nat, Nat}) -> Eq((p.1, p.0), p) = Eq/refl();

        /std/Io/pure(())
        "#;

const A_FUNCTION_INTO_A_PROPOSITION_IS_DISCHARGED_BEFORE_ETA: &str = r#"
        use /std/{Eq, Nat};

        let same(g : (Nat) -> Eq(0, 0), h : (Nat) -> Eq(0, 0)) -> Eq(g, h) = Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

const A_FUNCTION_INTO_A_TYPE_IS_NOT_DISCHARGED_UNCOMPARED: &str = r#"
        use /std/{Eq, Nat};

        let same(g : (Nat) -> Nat, h : (Nat) -> Nat) -> Eq(g, h) = Eq/refl();

        /std/Io/pure(())
        "#;

const ETA_HANDS_A_RECORDS_PROOF_COMPONENT_TO_IRRELEVANCE: &str = r#"
        use /std/{Eq, Nat};

        let same(g : (Nat) -> {Nat, Eq(0, 0)}, p : Eq(0, 0))
            -> Eq(g, (x : Nat) => (g(x).0, p)) = Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

const ETA_STILL_COMPARES_A_RECORDS_RELEVANT_COMPONENT: &str = r#"
        use /std/{Eq, Nat};

        let same(p : {Nat, Eq(0, 0)}) -> Eq(p, (0, p.1)) = Eq/refl();

        /std/Io/pure(())
        "#;

const A_GROUNDED_ARGUMENT_FORFEITS_IRRELEVANCE: &str = r#"
        use /std/{Eq, Nat};

        let ground(f : (Eq(0, 0)) -> Nat, p : Eq(0, 0), q : Eq(0, 0)) -> Eq(f(p), f(q)) =
            Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

const A_NOMINAL_STRUCTS_ETA_IS_NOT_FORFEITED_THERE: &str = r#"
        use /std/{Eq, Nat};

        struct Sealed : pub Type {
            one : Eq(0, 0),
            two : Eq(1, 1)
        }

        let same(f : (Sealed) -> Nat, b : Sealed, p : Eq(0, 0), q : Eq(1, 1))
            -> Eq(f(Sealed { one = p, two = q }), f(b)) = Eq/refl();

        /std/print(Nat/to_str(1))
        "#;

// The large-elimination guard, in the direction that matters for soundness. Every `Box` is definitionally equal to every other by proof irrelevance, so reading a `Nat` back out of one would make 0 and 7 convertible.
#[test]
fn a_multi_constructor_proposition_cannot_be_eliminated_into_data() {
    rejected_by(
        A_MULTI_CONSTRUCTOR_PROPOSITION_CANNOT_BE_ELIMINATED_INTO_DATA,
        "cannot eliminate the proposition",
    );
}

// The same guard, in the direction that matters for the language staying usable. A guard that rejected these would be indistinguishable from one that rejected everything, and `ex falso` and transport are both load-bearing: `/std/Eq/subst` in the prelude is the singleton case.
#[test]
fn an_empty_proposition_still_eliminates_into_data() {
    assert_eq!(run(AN_EMPTY_PROPOSITION_STILL_ELIMINATES_INTO_DATA), b"0");
}

#[test]
fn a_proposition_still_eliminates_into_another_proposition() {
    assert_eq!(
        run(A_PROPOSITION_STILL_ELIMINATES_INTO_ANOTHER_PROPOSITION),
        b"1"
    );
}

// `Prop` non-informativeness, which is what makes proof irrelevance safe: a proposition whose inhabitants differ observably is not a subsingleton, so identifying them would identify the data they carry.
#[test]
fn a_proposition_may_not_carry_informative_fields() {
    rejected_by(
        A_PROPOSITION_MAY_NOT_CARRY_INFORMATIVE_FIELDS,
        "is informative",
    );
}

// A concept is a structure, so the same rule must reach a `Prop`-sorted concept whose method returns data. Worth its own fixture: the concept path generates its record entry rather than declaring it, so it could regress independently.
#[test]
fn a_proposition_concept_may_not_carry_informative_methods() {
    rejected_by(
        A_PROPOSITION_CONCEPT_MAY_NOT_CARRY_INFORMATIVE_METHODS,
        "is informative",
    );
}

// Coverage. A missing arm leaves an elimination undefined at that constructor, which is a proof of the motive at an index nothing established.
#[test]
fn an_elimination_must_enumerate_its_constructors() {
    rejected_by(
        AN_ELIMINATION_MUST_ENUMERATE_ITS_CONSTRUCTORS,
        "missing match case",
    );
}

// The foreign wire contract. The embedder supplies these values, so a `foreign` admitted at an arbitrary type would let the host hand back an inhabitant of a proposition that nothing ever checked.
#[test]
fn a_foreign_declaration_is_confined_to_wire_types() {
    rejected_by(
        A_FOREIGN_DECLARATION_IS_CONFINED_TO_WIRE_TYPES,
        "expected a wire type",
    );
}

// `zonk_module`'s *extent*, which is where its soundness sits. Every program reaches this pass, and every "was not inferred" diagnostic is the rule firing, so what needed checking was how far it reaches rather than whether it runs. The assumption is that no unsolved metavariable survives into the module, and the module has exactly four term-bearing places for one to survive in: a definition's type, a definition's body (with the entrypoint body walked separately from both), an `induct` registry telescope, and a `struct` field telescope. The fields `zonk_module` deliberately skips carry `Vec<String>`, `Vec<(usize, Global)>` and `BTreeSet<Global>`, so its comment that concept metadata and witness markers hold no terms of their own is exact rather than approximate.
//
// The extent is where the soundness sits, because the assumption's second clause is that nothing can *later* be solved to a partial or negatively-occurring term. `check_positivity` and `record_totality` run after zonking and on the module zonking returned, and positivity reads a `Metavar` through `opaque`: its spine children at `Mixed`, and never its solution, which does not exist yet. A metavariable surviving into a registry telescope would therefore be analyzed as a hole while the term it is later solved to is analyzed not at all. Refusal before those passes run is what closes that, not the ordering by itself.
//
// Each fixture plants one unconstrained implicit in one of the four places; all four were refused. Null result, and the control is what keeps the row from being read as "declarations may not mention implicits" — it supplies the same argument in all four positions and requires the program to run.
#[test]
fn a_metavariable_does_not_survive_into_an_induct_telescope() {
    rejected_by(
        &format!("{AN_UNCONSTRAINED_IMPLICIT}{A_METAVARIABLE_IN_AN_INDUCT_TELESCOPE}"),
        "was not inferred",
    );
}

#[test]
fn a_metavariable_does_not_survive_into_a_struct_field() {
    rejected_by(
        &format!("{AN_UNCONSTRAINED_IMPLICIT}{A_METAVARIABLE_IN_A_STRUCT_FIELD}"),
        "was not inferred",
    );
}

#[test]
fn a_metavariable_does_not_survive_a_definitions_type() {
    rejected_by(
        &format!("{AN_UNCONSTRAINED_IMPLICIT}{A_METAVARIABLE_IN_A_DEFINITIONS_TYPE}"),
        "was not inferred",
    );
}

#[test]
fn a_metavariable_does_not_survive_the_entrypoint_body() {
    rejected_by(
        &format!("{AN_UNCONSTRAINED_IMPLICIT}{A_METAVARIABLE_IN_THE_ENTRYPOINT_BODY}"),
        "was not inferred",
    );
}

#[test]
fn a_solved_metavariable_still_reaches_every_zonked_position() {
    assert_eq!(
        run(&format!(
            "{AN_UNCONSTRAINED_IMPLICIT}{A_SOLVED_METAVARIABLE_IN_EVERY_POSITION}"
        )),
        b"0"
    );
}

// The large-elimination guard again, at its *singleton* rung. A one-constructor proposition may eliminate into data only when every payload binder is non-informative — a proposition itself, or *pinned* by the constructor's index targets, as `Eq`'s `refl(@z) : (z, z)` recovers `z`.
//
// Occurring in an index target is not the same as being determined by one. `blur` is constant, so `Loose(0)` is inhabited by `mk(0)` and by `mk(7)` alike, and no index tells them apart — proof irrelevance identifies the two inhabitants while `extract` would observe them apart, and the gap is a closed inhabitant of `False`. `singleton_eliminable` once read `a` as forced because it *occurs* in `blur(a)` — a syntactic occurrence test — and this program printed "FORGED". Both checkers now decide the condition by the shared `pinned_by_targets` walk: a binder counts only when matching a value against the target recovers it, which `blur(a)` never does.
//
// The two ends of the discrimination are covered alongside: the same declaration with target `(0)` is rejected below, and `(a)` is a genuinely forced binder that must stay accepted.
#[test]
fn a_non_injective_index_target_does_not_force_its_binder() {
    rejected_by(
        A_NON_INJECTIVE_INDEX_TARGET_DOES_NOT_FORCE_ITS_BINDER,
        "cannot eliminate the proposition",
    );
}

// Proof irrelevance and index inversion disagree about a `Prop`-valued index, and the disagreement is a closed inhabitant of `False`. Conversion identifies `Two/a()` with `Two/b()` — any two inhabitants of a proposition are equal — so `Ind(Two/a())` and `Ind(Two/b())` are the same type and `coerce` is well typed. Inversion decides a case is impossible by *syntactic* constructor clash (`invert_indices` decomposes constructor forms and clashes on distinct tags, with no sort condition), so it reads `only`'s target `Two/a()` against the actual index `Two/b()` as disjoint and accepts the arm-less elimination as vacuous — at a type conversion just proved inhabited.
//
// Verified against the compiler of the day, while the hole was open: this source compiled (`curios compile` exited 0, and `recheck_module_suffix` on the compile path certified `let /bad : False = boom(coerce(only()))`), and running it trapped at the `unreachable` the vacuous elimination emitted, which is the runtime witness that the impossibility claim was false. The rule that refuses it now exists: a clash may only be concluded at a position whose type distinguishes its inhabitants, and the shared walk both checkers reach decides that from the declaration's own `result_sort` (`curios-analysis/src/invert.rs`), deriving nothing at all — no clash, no equations — at a `Prop`-valued position.
//
// The refusal is the coverage rule's: with the clash retracted, `only` is an ordinary reachable constructor, and an elimination with no arm for it is missing one it cannot prove absent.
#[test]
fn a_proposition_valued_index_cannot_make_an_elimination_vacuous() {
    rejected_by(
        A_PROPOSITION_VALUED_INDEX_CANNOT_MAKE_AN_ELIMINATION_VACUOUS,
        "is not provably impossible at the scrutinee's indices",
    );
}

// The same disagreement through coverage rather than vacuity, which is why the fix belongs in the shared walk and not at one of its callers: here the elimination has an arm, and it is the *omitted* one that inversion wrongly excuses. `Ind/right()` inhabits `Ind(Two/a())` by the same conversion, so the match falls through every arm it enumerated.
#[test]
fn a_proposition_valued_index_cannot_excuse_an_omitted_arm() {
    rejected_by(
        A_PROPOSITION_VALUED_INDEX_CANNOT_EXCUSE_AN_OMITTED_ARM,
        "is not provably impossible at the scrutinee's indices",
    );
}

// The same rule one decomposition step down, which is where its implementation could most plausibly have parted from its statement. `invert_indices` decides the `Prop` condition from the *family of the values being compared* rather than from the declared type of the position, and it decides it wherever the recursion reaches rather than only at the top of one: here `Pair` is relevant and the `Two` it carries is not, so the condition has to fire inside a matching constructor's payload. Read off the index domain instead — `Pair : Type`, therefore relevant — or applied only at `top`, `Two/a()` against `Two/b()` would clash at depth exactly as it once did at the surface, and both routes below would be closed inhabitants of `False` again.
//
// Both were refused when run, and the part worth keeping is that `coerce` elaborated in each: conversion *did* identify the two `Ind` instances through the nested proof, so the premise each exploit needs was available and inversion's refusal to clash on it is the only thing that stood in the way. Null result; the probes are recorded so the rung is not re-attacked.
#[test]
fn a_nested_proposition_valued_index_cannot_make_an_elimination_vacuous() {
    rejected_by(
        A_NESTED_PROPOSITION_VALUED_INDEX_CANNOT_MAKE_AN_ELIMINATION_VACUOUS,
        "is not provably impossible at the scrutinee's indices",
    );
}

#[test]
fn a_nested_proposition_valued_index_cannot_excuse_an_omitted_arm() {
    rejected_by(
        A_NESTED_PROPOSITION_VALUED_INDEX_CANNOT_EXCUSE_AN_OMITTED_ARM,
        "is not provably impossible at the scrutinee's indices",
    );
}

// Coverage's *accepting* rung, which had no fixture of its own: an omitted arm excused because its index target genuinely clashes with the scrutinee's. Every fixture above asserts the refusing direction, so a change that made every absent arm mandatory would break the standard library and nothing in this file — and the standard library leans on this rung hard, at 14 excusals over `/syn/Str/Utf8`, `/std/Nat/Lte`, and `/std/Vec` in one kernel walk of the prelude, with no omitted arm ever coming back mandatory. `AN_EMPTY_PROPOSITION_STILL_ELIMINATES_INTO_DATA` is not this control: a family with no constructors leaves the coverage loop with nothing to iterate, so it exercises the loop's absence rather than its verdict.
//
// It is deliberately the nested shape, which is what discriminates the two refusals above rather than merely sitting beside them: the clash is at the same depth and differs only in that `Pair/mk(0)` and `Pair/mk(1)` are values a program can tell apart.
#[test]
fn a_nested_relevant_clash_still_excuses_an_omitted_arm() {
    assert_eq!(
        run(A_NESTED_RELEVANT_CLASH_STILL_EXCUSES_AN_OMITTED_ARM),
        b"0"
    );
}

// The lower end of that discrimination: drop `a` from the index target and the guard fires. Without this, a fix could "close" the hole above by rejecting every indexed proposition and nothing here would notice.
#[test]
fn an_unmentioned_payload_binder_is_not_forced() {
    rejected_by(
        AN_UNMENTIONED_PAYLOAD_BINDER_IS_NOT_FORCED,
        "cannot eliminate the proposition",
    );
}

// The singleton rung's side condition at its other half: a payload that is *itself a type*. `mk(A : Type)` pins nothing, so eliminating a `Box` recovers the type it was built with while irrelevance says every `Box` is the same one — `Eq/cong` then equates any two types and `Eq/subst` transports `0` into `False`.
//
// The elaborator refuses at `unbox`, so the items after it never elaborate; they document the route rather than being checked. The certifier is where this rule needed backing up and where it was wrong: `carries_information` reported a component whose type is a universe as carrying nothing, on the reasoning that erasure deletes a type either way — which confuses what the runtime observes with what conversion observes. `recheck_module_verdicts` certified the hand-built equivalent of this program with zero refusals, memos on and off.
//
// No surface program reaches that gate, which is what the `NotAsked` in this row's kernel column means. The executable guarantee therefore lives beside the rule: `curios_cert::recheck::tests::a_derivation_through_a_type_carrying_proposition_is_refused` holds the whole derivation shut, and the two singleton fixtures in `curios_cert::kernel::infer::eliminate::tests` pin the predicate at both halves of the clause that admitted it.
#[test]
fn a_singleton_carrying_a_type_does_not_eliminate_into_a_type() {
    rejected_by(
        A_SINGLETON_CARRYING_A_TYPE_DOES_NOT_ELIMINATE,
        "cannot eliminate the proposition",
    );
}

// `Prop` non-informativeness at the same half, and the shorter route: a field whose type is a universe holds a type as data, and a projection reads it back out meeting no elimination guard at all, so two convertible propositions hand `.0` two different types.
//
// Both gates asked `carries_information`, so `check_struct_decl` returned `Ok(())` while the hole was open. Its control in `curios-cert` — `a_proposition_may_carry_a_proof`, which keeps a genuine proof field legal — was itself mis-specified, typing its field as the universe `Prop` rather than as a `Prop`-sorted family, so it asserted the admitted shape and passed for the wrong reason. That is why the discrimination this row names went untested on the kernel side for as long as it did.
#[test]
fn a_proposition_may_not_carry_a_type_field() {
    rejected_by(A_PROPOSITION_MAY_NOT_CARRY_A_TYPE_FIELD, "is informative");
}

// Definitional proof irrelevance, at the premise the rule is stated over rather than at the rule: *which* types are propositions. `Prop` is the type of propositions, so anything admitted at `Prop` is one as far as every later rule is concerned, and irrelevance then identifies its inhabitants without looking at them.
//
// `List(P)` is not a proposition however propositional `P` is — a list has a length, so two of them are distinguishable where their elements are not, and `Sort::of` says exactly that in both checkers. The *typing* rule for a parameterized intrinsic former was a second implementation of that one rule and reported the element's sort as the former's, so `List(True)` inferred at `Prop` on both sides. Nothing here needed the former written in a `Prop` position: `@X : Prop` is solved by unification, and the solution is the reduced `ListType` node.
//
// From there every step is the ordinary machinery. `all_equal` is sound and stays accepted below — reflexivity discharges `Eq(@X, x, y)` because irrelevance identifies any two inhabitants of the proposition `X`. Instantiating `X` at `List(True)` yields `Eq(one, none)` for a one-element list against the empty one; `Eq/cong` through `List/len` carries that to `Eq(1, 0)`, and `Eq/subst` transports `()` into `False`.
//
// Verified against the compiler of the day, while the hole was open: this source elaborated and `recheck_module_suffix` on the compile path certified `let /bad : Eq(List True, /one, /none)` with zero refusals — the `--print=core-elab` dump shows the solved `@X` as the `List` former applied to `True`. It never reached a runtime, and not because a checker stopped it: erasure refuses *any* call whose every argument erases, which is a defect of the erase boundary rather than of this rule, and which the control below trips identically at a genuine proposition.
//
// Both controls are load-bearing, because the two ways to "close" this without fixing it are to stop believing `Prop` and to stop believing `List`. Irrelevance must still identify two genuinely different inhabitants of a real proposition, and a list of proofs must still be an ordinary list with a length. The first is asserted through the two-checker matrix rather than by running, since it is the program the erase boundary cannot lower; what it has to establish is that both checkers still accept the lemma and its instantiation.
#[test]
fn a_list_of_proofs_is_not_a_proposition() {
    rejected_by(A_LIST_OF_PROOFS_IS_NOT_A_PROPOSITION, "type mismatch");
}

#[test]
fn a_list_of_proofs_is_still_a_list() {
    assert_eq!(run(A_LIST_OF_PROOFS_IS_STILL_A_LIST), b"1");
}

// The arm rule at its one arm with no case value of its own. A `| _ =>` catch-all binds nothing and refines no index, so the only instance it can be checked at is the scrutinee's — which is the instance the elimination then hands its caller.
//
// This is the two-checker matrix's own quadrant rather than a route to `False`: the elaborator checks the catch-all at the actual scrutinee and the kernel opened the motive's scrutinee binder at the family *type* `/Three`, so this program elaborated and was then refused with `/Three` standing in both term positions of the `Eq`. That direction fails closed — nothing well-typed inhabits an expectation with a type substituted for a value — but the certifier's own judgment is the one that matters, and it established nothing about the scrutinee for any catch-all it accepted. No prelude catch-all has a scrutinee-dependent motive, so the disagreement count stayed at zero and the rule was never asked.
//
// The instance is pinned where the rule lives, in `curios_cert::kernel::infer::eliminate::tests`: `a_catch_all_sees_its_own_scrutinee` with its control `a_catch_all_at_another_value_of_the_family_is_refused`, which asserts the expectation itself so that not checking the catch-all cannot pass for closing the hole.
#[test]
fn a_catch_all_is_checked_at_its_scrutinee() {
    assert_eq!(run(A_CATCH_ALL_IS_CHECKED_AT_ITS_SCRUTINEE), b"1");
}

/// The premise every rule above is stated over and no entry of `soundness.md` names: a type is a *pure* term. It used to be enforced by `reduce_intrinsic`, whose `Cell`, `CellGet`, `CellSet`, `Foreign` and `ProcExit` arms each refused type-level reduction, and this derivation was refused as `CellGet cannot appear at the type level` on that account alone, with no refinement in play. Those arms are gone — a description sitting at the type level is a value, not an error — and what refuses the program now is the scrutinee's own type. `Cell/get(c) : Io(Bool)` *describes* a read instead of being one, so it is not a `Bool`, not something `match` can eliminate, and not something `Eq` can be stated over. The cell is forced on the line above so that the refusal lands here and not on the binding.
const AN_EFFECTFUL_SCRUTINEE_IS_NOT_A_VALUE: &str = r#"
    use /std/{Cell, Eq, Bool, False, Str};

    let c = Cell/new(true)!;

    let forged : Str =
        match Cell/get(c)
        | true =>
            let p : Eq(Cell/get(c), true) = Eq/refl();
            let done = Cell/set(c, false);
            match Cell/get(c)
            | true => "second read true"
            | false => match Bool/false_neq_true(p) end
            end
        | false => "first read false"
        end;

    /std/print(forged)
    "#;

/// The control, and it has more to guard than it used to. Only the refinement's escape into a *type* was ever at issue, so a fix that refused the elimination outright would be a brick — and now that the cure is a typing rule rather than a reduction guard, a rule that refused `Cell/get` in every position would be exactly that brick. Forcing the description yields an ordinary `Bool`, which matches like one.
const A_MATCH_ON_A_FORCED_CELL_READ_STILL_COMPILES: &str = r#"
    use /std/{Cell, Str};

    let c = Cell/new(true)!;
    let v = Cell/get(c)!;

    /std/print(
        match v
        | true => "t"
        | false => "f"
        end
    )
    "#;

// The purity premise, attacked through the one store that rewrites a term before the guard can see it. `refine_head`'s fallback registers the whole canonical scrutinee against the arm's case value, and the reducer consults that store *ahead of* folding the intrinsic — so inside `| true =>` the effectful `Cell/get(c)` reduces to `true` and never reaches `reduce_intrinsic`. The annotation `Eq(Cell/get(c), true)` is admitted on those terms, and it is stored **as written**: `p`'s recorded type keeps the `Cell/get(c)`, and only the conversion that discharged `Eq/refl` ever saw it as `true`.
//
// Two occurrences of one syntactic term then denote two different values. After `Cell/set(c, false)` the inner `match Cell/get(c)` refines that same term to `false`, `p` re-reads at `Eq(false, true)`, and `/std/Bool/false_neq_true` turns it into `/syn/False`. The arm deriving it is *reachable*: the first read is `true`, so the outer arm runs, and the second is `false`, so the inner one runs too.
//
// Verified while the hole was open, and the acceptance is the refinement's doing rather than a fixture that never reached the check: the identical program with the derivation moved to the inner `| true =>` arm — where the refinement is `true`, so `p` re-reads at `Eq(true, true)` — is refused by `curios-elab` with `type mismatch`, while this one passes elaboration entire. It never compiled, because `curios-cert` refuses it, but not by any rule of its own: `whnf` folds an `Intrinsic` at the top of its loop and only consults `refinement_of` on the value that comes back, so the same conversion dies on `reduction failed in the kernel`. The kernel's ordering is what stood between this and a closed inhabitant of `False`; this asserts the elaborator's half.
#[test]
fn an_effectful_scrutinee_is_not_a_value() {
    rejected_by(AN_EFFECTFUL_SCRUTINEE_IS_NOT_A_VALUE, "Io");
}

#[test]
fn a_match_on_a_forced_cell_read_still_compiles() {
    assert_eq!(run(A_MATCH_ON_A_FORCED_CELL_READ_STILL_COMPILES), b"t");
}

// **The four paragraphs below are the history of a hole that is now closed by typing rather than by any of the guards they describe.** They are kept because this row's grade rests on what was actually attacked, and because a reader who meets `Cell/get` in a scrutinee should find out why it was hard before it was impossible. The program is still a regression fixture; what refuses it changed.
//
// The same premise, past the guard that closed the entry above, because that guard asked a question weak-head reduction cannot answer. `refuses_type_level_reduction` reduced the scrutinee and read the refusal, and `reduce` stops at a *stuck head* handing the application back with its arguments untouched — so `f(Cell/get(c))`, a variable-headed application carrying the effect in an argument, never reaches `reduce_intrinsic`, answers `Ok`, and is registered as a spelling that fixes a value. The effect is then inside a type: `Eq(g(Cell/get(c)), true)` is admitted on those terms exactly as `Eq(Cell/get(c), true)` was.
//
// `curios-cert` has the same hole for the same reason and is not the backstop here: `assume_case_value` records at its own `whnf`, whose `step_apply` likewise stops at a stuck head without visiting an argument. Both checkers register the equation, which is what makes this agreement on a wrong rule rather than a disagreement — and why, unlike the entry above, it compiled.
//
// Two heads rather than one because a nested refinement of a *single* key is dropped by the kernel by accident: `assume_case_value` reduces the inner scrutinee under the outer arm's equation, gets the literal `true` back, and `Scope::refine` skips a local-free-less key. Refining `f(...)` outside and `g(...)` inside sidesteps that, and `h` carries the outer arm's knowledge across — so in `| true =>` the outer equation reads `h(Cell/get(c))` at `Eq(g(Cell/get(c)), true)`, which is `step`'s parameter type as written. After `Cell/set(c, false)` the inner `match g(Cell/get(c))` refines that same spelling to `false`, `p` re-reads at `Eq(false, true)`, and `/std/Bool/false_neq_true` turns it into `/syn/False`.
//
// Verified while the hole was open: the program **compiled**, the compile-path recheck raised nothing, and running it trapped in the Wasm — `False/absurd` on the forged proof erasing to the `unreachable` the arm reaches. The arm is reachable rather than merely well-typed: with the derivation replaced by a string the same program printed `REACHED: second read false`. And the acceptance was the refinement's doing rather than a fixture that never reached the check — the identical program with the derivation moved to the inner `| true =>` arm, where the spelling refines to `true`, was refused with `type mismatch`, `inferred Eq(true, true)` against `expected Eq(false, true)`.
const AN_EFFECT_BEHIND_A_STUCK_HEAD_IS_NOT_AN_ARGUMENT: &str = r#"
    use /std/{Cell, Eq, Bool, False, Str};

    /std/print(
        ((f : (Bool) -> Bool,
          g : (Bool) -> Bool,
          h : (x : Bool) -> Eq(g(x), f(x)),
          c : Cell(Bool)) =>
            match f(Cell/get(@Bool, c))
            | true =>
                let step(p : Eq(g(Cell/get(@Bool, c)), true)) -> Str =
                    let done = Cell/set(c, false);
                    match g(Cell/get(@Bool, c))
                    | true => "second read true"
                    | false => match Bool/false_neq_true(p) end
                    end;
                step(h(Cell/get(@Bool, c)))
            | false => "first read false"
            end
        )((b) => b, (b) => b, (x) => Eq/refl(), Cell/new(true)!)
    )
    "#;

/// The control, and it guards the brick this fix could have been: a scrutinee whose head is stuck is the *ordinary* case — `flip(b)` for a `b` nothing can instantiate — and refining it is what lets a hypothesis stated over the scrutinee re-read at the arm's value. So a guard that refused every stuck application, or every application it could not fully reduce, would still reject this.
///
/// The head is a *definition* here for historical reasons only. `fixes_no_value` could read a definition's body and so let the application fix whatever the callee fixed, while a parameter's body does not exist yet — so the parameter spelling had to move out of this control and into the derivation below. Nothing is walked now, and [`a_parameter_headed_scrutinee_refines_again`] is that spelling brought back.
const A_STUCK_APPLICATION_SCRUTINEE_STILL_REFINES: &str = r#"
    use /std/{Eq, Bool, False, Str};

    let flip(b : Bool) -> Bool = Bool/not(b);

    let refined(b : Bool, p : Eq(flip(b), true)) -> Str =
        match flip(b)
        | true => "t"
        | false => match Bool/false_neq_true(p) end
        end;

    /std/print(refined(false, Eq/refl()))
    "#;

/// Asserted on the *argument*, which is where this shape now dies. The old rule withheld an equation and the program failed where it needed one, so the discriminator was `p`'s type still reading the unrefined `g(Cell/get(c))`; there is no equation to withhold any more. `Cell/get(@Bool, c)` is an `Io(Bool)` and `f : (Bool) -> Bool` does not take one, so the derivation is refused at the first of its four occurrences and never reaches a refinement at all.
#[test]
fn an_effect_behind_a_stuck_head_is_not_an_argument() {
    rejected_by(AN_EFFECT_BEHIND_A_STUCK_HEAD_IS_NOT_AN_ARGUMENT, "Io");
}

#[test]
fn a_stuck_application_scrutinee_still_refines() {
    assert_eq!(run(A_STUCK_APPLICATION_SCRUTINEE_STILL_REFINES), b"t");
}

// The route no search over the term could have closed, and the one this whole discipline exists for. The scrutinee is `f(true)` for a *parameter* `f`: no `Intrinsic::CellGet` in it, none in anything it names, so every walk answered *pure* and both checkers recorded the equation. The caller then bound `f := (b) => Cell/get(c)`, and one spelling read `true` before the `Cell/set` and `false` after. Effectfulness of `f(true)` is not a property of `f(true)` — it is a property of the environment, and at the moment an arm records its equation the binder has no value to inspect, so asking the term was never sufficient.
//
// `fixes_no_value`'s cure was to ask a second question — does the walk read the body of every function the term would call — and refuse the equation when it does not. It worked and it was expensive in exactly the direction that matters: a *pure* opaque head stopped refining too, because nothing distinguished it. `(Bool) -> Bool` said nothing about purity, since the function space admitted `Cell/get`.
//
// Nothing is asked now, and the sentence that made the walk necessary is false. `(b) => Cell/get(c)` has type `(Bool) -> Io(Bool)`; it does not inhabit `(Bool) -> Bool`, so the *caller's argument* is refused and the derivation never reaches an arm, a refinement, or an equation. This is the entry `soundness.md` predicted when it said what removes the class is an effect discipline on the arrow rather than another clause in the walk — and [`a_parameter_headed_scrutinee_refines_again`] is what the walk was costing.
const AN_EFFECT_CANNOT_INHABIT_A_PURE_ARROW: &str = r#"
    use /std/{Cell, Eq, Bool, False, Str};

    let forge(f : (Bool) -> Bool, c : Cell(Bool), p : Eq(f(true), true)) -> Str =
        let done = Cell/set(c, false);
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
            )((b) => Cell/get(c))
        )(Cell/new(true)!)
    )
    "#;

/// Asserted on the offending *argument*: the refusal is that `(b) => Cell/get(c)` cannot be passed where a `(Bool) -> Bool` is wanted, so the description type has to appear in the diagnostic. A fixture refused anywhere else — at the cell, at `Eq/refl`, at an arm — would not produce that, and this file's rule is that a perimeter test asserts its own diagnostic.
#[test]
fn an_effect_cannot_inhabit_a_pure_arrow() {
    rejected_by(AN_EFFECT_CANNOT_INHABIT_A_PURE_ARROW, "Io");
}

/// What the deleted walk was costing, and the reason this campaign is refinement-*restoring* rather than merely analysis-deleting. A parameter-headed scrutinee is the shape `fixes_no_value` had to refuse — it could not read a binder's body, so it could not tell a pure `f` from an effectful one — and refusing it withheld a refinement from every program that stated a hypothesis over an opaque head. Purity is a typing fact now, so the equation is licensed and `p` re-reads at the arm's value.
const A_PARAMETER_HEADED_SCRUTINEE_REFINES_AGAIN: &str = r#"
    use /std/{Eq, Bool, False, Str};

    let refined(f : (Bool) -> Bool, b : Bool, p : Eq(f(b), true)) -> Str =
        match f(b)
        | true => "t"
        | false => match Bool/false_neq_true(p) end
        end;

    /std/print(refined((x) => x, true, Eq/refl()))
    "#;

#[test]
fn a_parameter_headed_scrutinee_refines_again() {
    assert_eq!(run(A_PARAMETER_HEADED_SCRUTINEE_REFINES_AGAIN), b"t");
}

/// A partial definition behind a `Type`-sorted carrier, reached four ways. The kernel's local gate does not fire — `Box` is neither a proposition nor a sort — so before the erasure obligations moved into `curios-cert` these were the class the trusted base took entirely on the elaborator's word.
const PARTIAL_DIRECT: &str = r#"
    use /std/{Nat, False};
    struct Box : pub Type { p : False }
    rec loop(n : Nat) -> Box = loop(n);
    let bad : False = loop(0).p;
    /std/print("FORGED")
    "#;

const PARTIAL_THROUGH_WITNESS: &str = r#"
    use /std/{Nat, False};
    struct Box : pub Type { p : False }
    concept Make(A : Type) : pub Type { make(A) -> Box, }
    rec loop(n : Nat) -> Box = loop(n);
    satisfy Make(Nat) { make(n) = loop(n), }
    let bad : False = Make/make(0).p;
    /std/print("FORGED")
    "#;

const PARTIAL_HIGHER_ORDER: &str = r#"
    use /std/{Nat, False};
    struct Box : pub Type { p : False }
    rec loop(n : Nat) -> Box = loop(n);
    let apply(f : (Nat) -> Box, n : Nat) -> Box = f(n);
    let bad : False = apply(loop, 0).p;
    /std/print("FORGED")
    "#;

const PARTIAL_IN_FIELD: &str = r#"
    use /std/{Nat, False};
    struct Box : pub Type { p : False }
    struct Holder : pub Type { run : (Nat) -> Box }
    rec loop(n : Nat) -> Box = loop(n);
    let holder : Holder = Holder { run = loop };
    let bad : False = holder.run(0).p;
    /std/print("FORGED")
    "#;

/// A diverging proof in a position the judgment *infers* rather than checks — a match scrutinee — inside a definition whose own type is relevant, so the body is not a proof position either.
///
/// The elaborator records every settled node with the type it settled at, checked or inferred alike, so it has always caught this. The kernel recorded only checked positions, and nothing here is one: the elimination conjured a `Nat` from a proof that never terminates. This row read `elab=refuses, cert=accepts` until the kernel began seeding inferred positions too — the quadrant this matrix exists to make visible.
const INFERRED_PROOF_POSITION: &str = r#"
    use /std/{Nat, False};
    struct Box : pub Type { p : False }
    rec loop(n : Nat) -> Box = loop(n);
    let conjured : Nat =
        match loop(0).p : (_) => Nat
        end;
    /std/print(Nat/to_str(conjured))
    "#;

/// A non-descending `rec` written *inline*, at a `Type`-sorted type so the kernel's local descent gate does not apply, inside a definition that is not itself a proof position.
///
/// Nothing but the classification walk can see this one: `make` has no name-level partiality to inherit and no proof-typed member to gate, so it is partial only if `locally_partial` descends into the `rec` group's member scopes rather than stopping at the node. The proof position is `bad`, which merely mentions `make`.
const INLINE_REC_UNDER_CARRIER: &str = r#"
    use /std/{Nat, False};
    struct Box : pub Type { p : False }
    let make : Box =
        rec r : Box = r;
        r;
    let bad : False = make.p;
    /std/print("FORGED")
    "#;

/// Obligation **(T)** rather than (V): a *type* that reaches a definition which is not known to terminate.
///
/// `spin` is legal — general recursion at a relevant type is the language's design — but a type mentioning it is not, because erasure deletes types too, and a type-level loop reties the negative knot strict positivity exists to forbid. The elaborator seeds this syntactically from written type positions; the kernel seeds it from its own typing, where the body of a definition whose type is a sort is checked against that sort. This row is the only coverage either seeding has for (T).
const TYPE_REACHING_PARTIAL: &str = r#"
    use /std/{Nat, Vec};
    rec spin(n : Nat) -> Nat = spin(n);
    let Sized(n : Nat) -> Type = Vec(Nat, spin(n));
    /std/print("FORGED")
    "#;

/// The induction hypothesis of a `Nat` fold, at the wrong instance.
///
/// In the successor arm the hypothesis is the motive at the *predecessor*, and the goal is the motive at the successor. Handing the hypothesis back directly proves `Eq(k + 1, 0)` from `Eq(k, 0)`, so a rule that typed the hypothesis at the scrutinee rather than at the peeled index would make every predicate provable by induction.
const INDUCTION_HYPOTHESIS_AT_THE_SCRUTINEE: &str = r#"
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
const DISPATCH_DEFAULT_AT_A_CASE: &str = r#"
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
const SATURATING_SUBTRACTION_IS_NOT_DESCENT: &str = r#"
    use /std/{Nat, False};
    rec bogus(n : Nat) -> False = bogus(n - 1);
    let bad : False = bogus(0);
    /std/print("FORGED")
    "#;

/// Permuting the arguments is not a descent either.
///
/// The classic size-change subtlety: every call maps a parameter to a parameter, so each call matrix is full of `Same` entries and none is `Less`. Composing a swap with itself returns the identity, so no cycle carries a strict decrease and the group cannot be total — an engine reading "the argument came from a parameter" as progress would certify a recursion that runs forever.
const PERMUTING_ARGUMENTS_IS_NOT_DESCENT: &str = r#"
    use /std/{Nat, False};
    rec bogus(a : Nat, b : Nat) -> False = bogus(b, a);
    let bad : False = bogus(0, 1);
    /std/print("FORGED")
    "#;

/// Two distinct recursions are not the same function.
///
/// Neither descends, so both are legal values, and both fold to themselves — which is the shape that puts conversion's recurrence rule to work: comparing them unfolds each once, arrives at the same goal, and a history that treated "already assumed" as "proved" would equate two definitions that differ. `f` is constantly zero and `g` constantly one, so equating them and transporting along the equality gives `Eq(0, 1)`.
const DISTINCT_RECURSIONS_ARE_NOT_EQUAL: &str = r#"
    use /std/{Nat, Eq};
    rec f(n : Nat) -> Nat =
        match n
        | 0 => 0
        | k + 1; _ => f(k)
        end;
    rec g(n : Nat) -> Nat =
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
const BOOL_ARM_AT_THE_WRONG_CASE: &str = r#"
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
const DISPATCH_LITERAL_AT_THE_WRONG_VALUE: &str = r#"
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
const A_SOUND_PROGRAM: &str = r#"
    use /std/{Nat};
    let double(n : Nat) -> Nat = n + n;
    /std/print(Nat/to_str(double(21)))
    "#;

/// What one checker did with a fixture.
#[derive(Debug)]
enum Verdict {
    Accepts,
    Refuses(String),
    /// Never asked. Elaboration produced no module, so this checker never saw the program at all — which is a fact about coverage, not a pass.
    NotAsked,
}

/// Put `source` to each checker independently, and report what each said.
///
/// The elaborator's erasure obligations are *reported* rather than raised (`typecheck_reporting`), so a program only it refuses still yields a module for the kernel to judge. Without that the kernel's column would read whatever the elaborator's short circuit left behind, which is exactly the disagreement this exists to expose.
fn both_checkers(source: &str) -> (Verdict, Verdict) {
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

/// What a fixture's row claims a checker does.
enum Expect {
    Accepts,
    /// Refused, by a diagnostic containing this fragment — the rule must be named, or a fixture broken in some unrelated way would pass.
    Refuses(&'static str),
    /// Never reached this checker. Not a pass: it records that the rule is enforced earlier, so whether this checker also enforces it is *unverified here*. Only the erasure obligations can be deferred far enough for the kernel to see a program the elaborator rejects — every other refusal happens while the module is still being built, so there is nothing to hand over.
    NotAsked,
}

/// Every fixture above, with what each checker is expected to say about it.
///
/// The pair is the point. Both refusing is a rule covered twice; both accepting is a program that must compile. The kernel refusing what the elaborator accepts would be recorded conversion incompleteness, the safe direction. The elaborator refusing what the kernel *accepts* would be the trusted base resting on an elaborator-only analysis — the shape that made a whole class of `False` certifiable — and the four `partial_*` rows read that way until the erasure obligations moved into `curios-cert`.
const CORPUS: &[(&str, &str, Expect, Expect)] = &[
    (
        "multi_constructor_prop",
        A_MULTI_CONSTRUCTOR_PROPOSITION_CANNOT_BE_ELIMINATED_INTO_DATA,
        Expect::Refuses("cannot eliminate the proposition"),
        Expect::NotAsked,
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
        Expect::NotAsked,
    ),
    (
        "informative_prop_method",
        A_PROPOSITION_CONCEPT_MAY_NOT_CARRY_INFORMATIVE_METHODS,
        Expect::Refuses("is informative"),
        Expect::NotAsked,
    ),
    (
        "coverage",
        AN_ELIMINATION_MUST_ENUMERATE_ITS_CONSTRUCTORS,
        Expect::Refuses("missing match case"),
        Expect::NotAsked,
    ),
    // Enforced by the grammar, before either checker exists.
    (
        "wire_types",
        A_FOREIGN_DECLARATION_IS_CONFINED_TO_WIRE_TYPES,
        Expect::Refuses("expected a wire type"),
        Expect::NotAsked,
    ),
    (
        "prop_index_vacuous",
        A_PROPOSITION_VALUED_INDEX_CANNOT_MAKE_AN_ELIMINATION_VACUOUS,
        Expect::Refuses("not provably impossible"),
        Expect::NotAsked,
    ),
    (
        "prop_index_omitted_arm",
        A_PROPOSITION_VALUED_INDEX_CANNOT_EXCUSE_AN_OMITTED_ARM,
        Expect::Refuses("not provably impossible"),
        Expect::NotAsked,
    ),
    (
        "non_injective_target",
        A_NON_INJECTIVE_INDEX_TARGET_DOES_NOT_FORCE_ITS_BINDER,
        Expect::Refuses("cannot eliminate the proposition"),
        Expect::NotAsked,
    ),
    (
        "unmentioned_binder",
        AN_UNMENTIONED_PAYLOAD_BINDER_IS_NOT_FORCED,
        Expect::Refuses("cannot eliminate the proposition"),
        Expect::NotAsked,
    ),
    // `NotAsked` here is the elaborator gating first, not the kernel being silent: its half of both rules is guarded in `curios-cert`'s own tests, which reach it by building the module directly.
    (
        "singleton_carrying_a_type",
        A_SINGLETON_CARRYING_A_TYPE_DOES_NOT_ELIMINATE,
        Expect::Refuses("cannot eliminate the proposition"),
        Expect::NotAsked,
    ),
    (
        "type_valued_prop_field",
        A_PROPOSITION_MAY_NOT_CARRY_A_TYPE_FIELD,
        Expect::Refuses("is informative"),
        Expect::NotAsked,
    ),
    // Both accepted this one: the sort of a parameterized intrinsic former was implemented twice on each side, and the typing rule's copy disagreed with `Sort::of`'s. Its kernel half is guarded where the rule lives, in `curios_cert::kernel::infer::tests`.
    (
        "list_of_proofs_is_not_a_prop",
        A_LIST_OF_PROOFS_IS_NOT_A_PROPOSITION,
        Expect::Refuses("type mismatch"),
        Expect::NotAsked,
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
        Expect::NotAsked,
    ),
    (
        "dispatch_default_at_a_case",
        DISPATCH_DEFAULT_AT_A_CASE,
        Expect::Refuses("type mismatch"),
        Expect::NotAsked,
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
        Expect::NotAsked,
    ),
    (
        "bool_arm_at_the_wrong_case",
        BOOL_ARM_AT_THE_WRONG_CASE,
        Expect::Refuses("type mismatch"),
        Expect::NotAsked,
    ),
    (
        "dispatch_literal_at_the_wrong_value",
        DISPATCH_LITERAL_AT_THE_WRONG_VALUE,
        Expect::Refuses("type mismatch"),
        Expect::NotAsked,
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
        Expect::NotAsked,
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
        Expect::NotAsked,
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
        Expect::NotAsked,
    ),
    (
        "non_strict_behind_record",
        A_NON_STRICT_OCCURRENCE_BEHIND_A_RECORD_IS_STILL_REFUSED,
        Expect::Refuses("positively, but not strictly"),
        Expect::NotAsked,
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
        Expect::NotAsked,
    ),
    (
        "eta_swaps_its_components",
        AN_EXPANSION_THAT_SWAPS_ITS_COMPONENTS_IS_NOT_ETA,
        Expect::Refuses("type mismatch"),
        Expect::NotAsked,
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
        Expect::NotAsked,
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
        Expect::NotAsked,
    ),
    // The quadrant this table's own documentation describes and had no instance of: the kernel refusing what the elaborator accepted, which is recorded conversion incompleteness and the safe direction. The grounded argument position is where it comes from.
    (
        "grounded_argument_forfeits_irrelevance",
        A_GROUNDED_ARGUMENT_FORFEITS_IRRELEVANCE,
        Expect::Accepts,
        Expect::Refuses("f(p), f(q)"),
    ),
    (
        "nominal_struct_eta_survives_grounding",
        A_NOMINAL_STRUCTS_ETA_IS_NOT_FORFEITED_THERE,
        Expect::Accepts,
        Expect::Accepts,
    ),
];

fn agrees(name: &str, checker: &str, expected: &Expect, actual: &Verdict) {
    match (expected, actual) {
        (Expect::Accepts, Verdict::Accepts) => {}
        (Expect::NotAsked, Verdict::NotAsked) => {}
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
            Expect::NotAsked => write!(formatter, "not-asked"),
            Expect::Refuses(fragment) => write!(formatter, "refuses({fragment})"),
        }
    }
}

// Sort formation, the row that had an argument and no program. Both of its rules are *accepting* — a Π into a proposition is a proposition whatever it quantifies over, and a record of propositions is one — so they widen what counts as a proof, and everything irrelevance and erasure do downstream turns on that verdict.
//
// The pair below is `tuple_sort`. A record is `Prop`-sorted only where every component pushed no level, which is what makes an anonymous Σ non-informative *by formation* where a declared `struct` needs `check_non_informative` to make it so — and the two predicates are the same one, so the declaration gate is what these fixtures read the formation verdict through. The empty record is the carve-out and has to fall the other way: `{}` is the unit type an effect returns, so calling it a proposition would erase a value the runtime needs.
#[test]
fn a_record_of_propositions_is_a_proposition() {
    assert_eq!(run(A_RECORD_OF_PROPOSITIONS_IS_A_PROPOSITION), b"1");
}

#[test]
fn the_empty_record_is_not_a_proposition() {
    rejected_by(THE_EMPTY_RECORD_IS_NOT_A_PROPOSITION, "is informative");
}

// `func_sort`, and the half that makes `Prop` impredicative: the domain's level is discarded when the codomain is a proposition, so quantifying over a universe still yields one. That is what puts Coquand–Paulin's construction in range, which is why this rule's soundness is not its own — see the positivity fixture below.
#[test]
fn a_function_into_a_proposition_is_a_proposition() {
    assert_eq!(run(A_FUNCTION_INTO_A_PROPOSITION_IS_A_PROPOSITION), b"1");
}

#[test]
fn a_function_into_a_type_is_not_a_proposition() {
    rejected_by(
        A_FUNCTION_INTO_A_TYPE_IS_NOT_A_PROPOSITION,
        "is informative",
    );
}

// The row's stated attack shape, answered: "a proposition arrived at by *formation* rather than by declaration, carrying something the guards above only ever check where a declaration is consulted". The large-elimination guard is one of those guards, and it does *not* only check where a declaration is consulted — it asks `Sort::of`, which computes formation. So eliminating a two-constructor proposition into an anonymous Σ of `Nat`s is refused exactly as into a bare `Nat`, and the control is the same elimination into an anonymous Σ of proofs, which must stay legal because a proposition eliminated into a proposition needs no condition at all.
#[test]
fn a_proposition_may_not_be_eliminated_into_a_formed_type() {
    rejected_by(
        A_PROPOSITION_MAY_NOT_BE_ELIMINATED_INTO_A_FORMED_TYPE,
        "cannot eliminate the proposition",
    );
}

#[test]
fn a_proposition_still_eliminates_into_a_formed_proposition() {
    assert_eq!(
        run(A_PROPOSITION_STILL_ELIMINATES_INTO_A_FORMED_PROPOSITION),
        b"1"
    );
}

// The other support the argument names, at a shape positivity's own twelve probes did not spell: they run the negative and the double negative bare, through an `induct` parameter, through a `struct` parameter, through a type alias, under `List`, behind a type-level `match`, and at a higher-kinded parameter — never behind an anonymous Σ, which is the construct this row is about.
//
// The diagnostic is what makes this more than a repeat. It reads *positively, but not strictly* rather than *negatively*, which is the same verdict the bare spelling gets: the polarity lattice is computed through the tuple component rather than the component being answered opaquely, since an opaque answer would join to `Mixed` and refuse with the other message. Refusing a merely-`Pos` diagonal is precisely what keeps `℘℘` out while `Prop` is impredicative, so this is the pairing the row rests on, checked where the row lives.
#[test]
fn a_non_strict_occurrence_behind_a_record_is_still_refused() {
    rejected_by(
        A_NON_STRICT_OCCURRENCE_BEHIND_A_RECORD_IS_STILL_REFUSED,
        "positively, but not strictly",
    );
}

// Eta and untyped child positions, the row that carried an argument and no program at all. Conversion is type-directed, so eta is what converts `f` with `(x) => f(x)` and `p` with `(p.0, p.1)` without either side having to be written in that shape. Both rules are *accepting*, so each widens what counts as equal, and the two refusals beside them are what keep the acceptance from reading as "any two functions convert" and "any two records convert": drop the binder from the expansion and the equation dies, swap the components and it dies. Without them a `compare` that answered `true` at every Π and every Σ would satisfy the accepting rung and nothing here would notice.
#[test]
fn eta_converts_a_function_and_a_record_with_their_expansions() {
    assert_eq!(
        run(ETA_CONVERTS_A_FUNCTION_AND_A_RECORD_WITH_THEIR_EXPANSIONS),
        b"1"
    );
}

#[test]
fn an_expansion_that_drops_its_binder_is_not_eta() {
    rejected_by(
        AN_EXPANSION_THAT_DROPS_ITS_BINDER_IS_NOT_ETA,
        "type mismatch",
    );
}

#[test]
fn an_expansion_that_swaps_its_components_is_not_eta() {
    rejected_by(
        AN_EXPANSION_THAT_SWAPS_ITS_COMPONENTS_IS_NOT_ETA,
        "type mismatch",
    );
}

// The composition the row named as unattacked — "eta at a function type whose codomain is a proposition, where the expansion's body lands at a `Prop`-sorted goal and irrelevance discharges it without comparing anything" — and at Π there is nothing to attack, because the shape cannot arise. `turn` tries irrelevance *before* it dispatches on the goal type's shape, and `func_sort` makes a Π into a proposition a proposition whatever it quantifies over, so the goal is discharged whole at the top and eta never opens a binder at all. Any two such functions are equal, which is this accepting rung.
//
// The relevant-codomain pair beside it is what says the discharge is the proposition's doing rather than conversion giving up on function types: the same two binders at `(Nat) -> Nat` are not identified.
#[test]
fn a_function_into_a_proposition_is_discharged_before_eta() {
    assert_eq!(
        run(A_FUNCTION_INTO_A_PROPOSITION_IS_DISCHARGED_BEFORE_ETA),
        b"1"
    );
}

#[test]
fn a_function_into_a_type_is_not_discharged_uncompared() {
    rejected_by(
        A_FUNCTION_INTO_A_TYPE_IS_NOT_DISCHARGED_UNCOMPARED,
        "type mismatch",
    );
}

// The same composition where it *is* reachable, which is Σ rather than Π. `tuple_sort` makes a record a proposition only when every component is one, so `{Nat, Eq(0, 0)}` stays relevant, irrelevance does not preempt eta, and eta is what hands the second component to a `Prop`-sorted goal. Both rules fire in one equation here — eta at Π opens the binder, eta at Σ splits the record — and only the relevant component is ever compared.
//
// The refusal beside it pins that last clause: replace the relevant component with a literal and the equation dies although the proof component still matches, so the acceptance above is not eta declining to look.
#[test]
fn eta_hands_a_records_proof_component_to_irrelevance() {
    assert_eq!(
        run(ETA_HANDS_A_RECORDS_PROOF_COMPONENT_TO_IRRELEVANCE),
        b"1"
    );
}

#[test]
fn eta_still_compares_a_records_relevant_component() {
    rejected_by(
        ETA_STILL_COMPARES_A_RECORDS_RELEVANT_COMPONENT,
        "type mismatch",
    );
}

// The row's second clause in the direction it claims: comparing an untyped child at `Type` forfeits rules rather than adding them. `p` and `q` inhabit one proposition, so they are interchangeable at their own type — but as arguments of an opaque head they meet `ground`, where the goal type is `Type` and irrelevance is never asked. What the forfeiture costs is exactly what the row says it costs, a *refusal*, and a refusal is a disagreement, which is the signal the second checker exists to produce.
//
// This is the first row of the matrix above to sit in that quadrant. The elaborator accepts, comparing the arguments at the domain the head assigns them; the kernel refuses, and the fragment asserted is the two argument spellings themselves, so a fixture broken anywhere else could not pass it.
#[test]
fn a_grounded_argument_forfeits_irrelevance() {
    rejected_by(A_GROUNDED_ARGUMENT_FORFEITS_IRRELEVANCE, "f(p), f(q)");
}

// **The clause is inexact, and this is the correction.** What grounding forfeits is *type-directed* eta and *goal-level* irrelevance — the two rules `turn` reaches by asking what the goal type is. `struct_eta` is neither. It reads the literal's own declaration, so it fires from `structural`, which is precisely where a grounded comparison lands, and it skips a `Prop`-sorted field by asking `Sort::of` about the declaration's field type. Both rules the clause says are gone are therefore live in an untyped position, and this is the sharpest spelling of it: every field of `Sealed` is a proposition, so the field walk compares *nothing at all* and the literal is equated with the neutral on the strength of the neutral restriction alone.
//
// It is not unsound, and the reason is not the one `struct_eta`'s own comment gives. That comment says the neutral restriction is what stops the literal being equated with "an arbitrary term that merely appeared in the same untyped position" — but a `Var` is such a term, and the restriction excludes only non-neutrals. What actually establishes that the neutral inhabits `Sealed` is that every grounded pair is the corresponding children of two parents already shown convertible, so their types agree by typing. That is a property of the *callers* of `ground` rather than of `struct_eta`, and it is written in neither place.
#[test]
fn a_nominal_structs_eta_is_not_forfeited_there() {
    assert_eq!(run(A_NOMINAL_STRUCTS_ETA_IS_NOT_FORFEITED_THERE), b"1");
}

/// Every perimeter fixture, put to both checkers, asserting what each says.
///
/// Each row judges the user suffix only, as `compile_entrypoint` does, so this costs what compiling sixteen small programs costs rather than sixteen walks of the standard library. It is the coverage map: where a rule's *second* opinion is recorded, or its absence admitted.
#[test]
fn the_two_checkers_agree_as_recorded() {
    for (name, source, expect_elaborator, expect_kernel) in CORPUS {
        let (elaborator, kernel) = both_checkers(source);
        agrees(name, "the elaborator", expect_elaborator, &elaborator);
        agrees(name, "the kernel", expect_kernel, &kernel);
    }
}
