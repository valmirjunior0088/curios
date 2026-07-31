//! End-to-end coverage for the soundness perimeter entries that nothing else guards.
//!
//! `DESIGN.md` states the consistency claim against an enumerated perimeter and marks each entry *probed*, *argued*, or *auditable only*. "Probed" is a claim about executable evidence, so it needs a test that fails when the rule stops holding — otherwise the column records what someone once tried by hand and decays the moment nobody remembers doing it.
//!
//! The entries with their own homes are not repeated here: strict positivity lives in `tests::positivity`, the two totality obligations in `tests::soundness`, and witness coherence in `tests::concepts`. What is left is the large-elimination guard, `Prop` non-informativeness, coverage, and the foreign wire contract — four rules the claim rests on that had no regression test at all.
//!
//! Each rejection asserts its *own* diagnostic, following `tests::soundness`. A perimeter test that accepts any error is worse than none: an invalid fixture passes it while the rule it names goes unchecked. That is not hypothetical — the first draft of these probes "passed" on `unbound variable`, having never reached the check at all.

use {
    super::run,
    curios_cert::recheck_module_suffix,
    curios_runtime::MockHost,
    curios_text::{Entrypoint, RootSource},
};

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

        ()
        "#;

const A_PROPOSITION_CONCEPT_MAY_NOT_CARRY_INFORMATIVE_METHODS: &str = r#"
        use /std/{Nat};

        concept Bad(A : Type) : pub Prop {
            get(A) -> Nat,
        }

        ()
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

        ()
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

// The large-elimination guard, in the direction that matters for soundness. Every `Box` is definitionally equal to every other by proof irrelevance, so reading a `Nat` back out of one would make 0 and 7 convertible.
#[test]
fn a_multi_constructor_proposition_cannot_be_eliminated_into_data() {
    let source = A_MULTI_CONSTRUCTOR_PROPOSITION_CANNOT_BE_ELIMINATED_INTO_DATA;
    rejected_by(source, "cannot eliminate the proposition");
}

// The same guard, in the direction that matters for the language staying usable. A guard that rejected these would be indistinguishable from one that rejected everything, and `ex falso` and transport are both load-bearing: `/std/Eq/subst` in the prelude is the singleton case.
#[test]
fn an_empty_proposition_still_eliminates_into_data() {
    let source = AN_EMPTY_PROPOSITION_STILL_ELIMINATES_INTO_DATA;
    assert_eq!(run(source), b"0");
}

#[test]
fn a_proposition_still_eliminates_into_another_proposition() {
    let source = A_PROPOSITION_STILL_ELIMINATES_INTO_ANOTHER_PROPOSITION;
    assert_eq!(run(source), b"1");
}

// `Prop` non-informativeness, which is what makes proof irrelevance safe: a proposition whose inhabitants differ observably is not a subsingleton, so identifying them would identify the data they carry.
#[test]
fn a_proposition_may_not_carry_informative_fields() {
    let source = A_PROPOSITION_MAY_NOT_CARRY_INFORMATIVE_FIELDS;
    rejected_by(source, "is informative");
}

// A concept is a structure, so the same rule must reach a `Prop`-sorted concept whose method returns data. Worth its own fixture: the concept path generates its record entry rather than declaring it, so it could regress independently.
#[test]
fn a_proposition_concept_may_not_carry_informative_methods() {
    let source = A_PROPOSITION_CONCEPT_MAY_NOT_CARRY_INFORMATIVE_METHODS;
    rejected_by(source, "is informative");
}

// Coverage. A missing arm leaves an elimination undefined at that constructor, which is a proof of the motive at an index nothing established.
#[test]
fn an_elimination_must_enumerate_its_constructors() {
    let source = AN_ELIMINATION_MUST_ENUMERATE_ITS_CONSTRUCTORS;
    rejected_by(source, "missing match case");
}

// The foreign wire contract. The embedder supplies these values, so a `foreign` admitted at an arbitrary type would let the host hand back an inhabitant of a proposition that nothing ever checked.
#[test]
fn a_foreign_declaration_is_confined_to_wire_types() {
    let source = A_FOREIGN_DECLARATION_IS_CONFINED_TO_WIRE_TYPES;
    rejected_by(source, "expected a wire type");
}

// The large-elimination guard again, at its *singleton* rung. A one-constructor proposition may eliminate into data only when every payload binder is non-informative — a proposition itself, or *pinned* by the constructor's index targets, as `Eq`'s `refl(@z) : (z, z)` recovers `z`.
//
// Occurring in an index target is not the same as being determined by one. `blur` is constant, so `Loose(0)` is inhabited by `mk(0)` and by `mk(7)` alike, and no index tells them apart — proof irrelevance identifies the two inhabitants while `extract` would observe them apart, and the gap is a closed inhabitant of `False`. `singleton_eliminable` once read `a` as forced because it *occurs* in `blur(a)` — a syntactic occurrence test — and this program printed "FORGED". Both checkers now decide the condition by the shared `pinned_by_targets` walk: a binder counts only when matching a value against the target recovers it, which `blur(a)` never does.
//
// The two ends of the discrimination are covered alongside: the same declaration with target `(0)` is rejected below, and `(a)` is a genuinely forced binder that must stay accepted.
#[test]
fn a_non_injective_index_target_does_not_force_its_binder() {
    let source = A_NON_INJECTIVE_INDEX_TARGET_DOES_NOT_FORCE_ITS_BINDER;
    rejected_by(source, "cannot eliminate the proposition");
}

// Proof irrelevance and index inversion disagree about a `Prop`-valued index, and the disagreement is a closed inhabitant of `False`. Conversion identifies `Two/a()` with `Two/b()` — any two inhabitants of a proposition are equal — so `Ind(Two/a())` and `Ind(Two/b())` are the same type and `coerce` is well typed. Inversion decides a case is impossible by *syntactic* constructor clash (`invert_indices` decomposes constructor forms and clashes on distinct tags, with no sort condition), so it reads `only`'s target `Two/a()` against the actual index `Two/b()` as disjoint and accepts the arm-less elimination as vacuous — at a type conversion just proved inhabited.
//
// Verified against the built compiler: this source compiles (`curios compile` exits 0, and `recheck_module_suffix` on the compile path certifies `let /bad : False = boom(coerce(only()))`), and running it traps at the `unreachable` the vacuous elimination emitted, which is the runtime witness that the impossibility claim was false. Ignored because the rule that refuses it does not exist yet: a clash may only be concluded at a position whose type distinguishes its inhabitants, and both checkers reach the shared walk through a `Judge` seam that today exposes no sort.
//
// The refusal is the coverage rule's: with the clash retracted, `only` is an ordinary reachable constructor, and an elimination with no arm for it is missing one it cannot prove absent.
#[test]
fn a_proposition_valued_index_cannot_make_an_elimination_vacuous() {
    let source = A_PROPOSITION_VALUED_INDEX_CANNOT_MAKE_AN_ELIMINATION_VACUOUS;
    rejected_by(
        source,
        "is not provably impossible at the scrutinee's indices",
    );
}

// The same disagreement through coverage rather than vacuity, which is why the fix belongs in the shared walk and not at one of its callers: here the elimination has an arm, and it is the *omitted* one that inversion wrongly excuses. `Ind/right()` inhabits `Ind(Two/a())` by the same conversion, so the match falls through every arm it enumerated.
#[test]
fn a_proposition_valued_index_cannot_excuse_an_omitted_arm() {
    let source = A_PROPOSITION_VALUED_INDEX_CANNOT_EXCUSE_AN_OMITTED_ARM;
    rejected_by(
        source,
        "is not provably impossible at the scrutinee's indices",
    );
}

// The lower end of that discrimination: drop `a` from the index target and the guard fires. Without this, a fix could "close" the hole above by rejecting every indexed proposition and nothing here would notice.
#[test]
fn an_unmentioned_payload_binder_is_not_forced() {
    let source = AN_UNMENTIONED_PAYLOAD_BINDER_IS_NOT_FORCED;
    rejected_by(source, "cannot eliminate the proposition");
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
    // A rule enforced by the grammar refuses here, before either checker exists. `foreign`'s wire contract is the standing example, and recording it is more honest than asserting the fixture parses: it says plainly that the rule is the parser's and neither checker backs it up.
    let entrypoint = match source.parse::<Entrypoint>() {
        Ok(entrypoint) => entrypoint,
        Err(error) => return (Verdict::Refuses(format!("{error:?}")), Verdict::NotAsked),
    };

    match curios_pipeline::typecheck_reporting(
        crate::DEFAULT_STEP_BUDGET,
        &entrypoint,
        RootSource::none(),
    ) {
        // Refused before a module existed: type-checking proper, not an erasure obligation.
        Err(error) => (Verdict::Refuses(error), Verdict::NotAsked),
        Ok((module, checked_from, obligations)) => {
            let elaborator = match obligations.into_iter().next() {
                Some(error) => Verdict::Refuses(error),
                None => Verdict::Accepts,
            };
            // The suffix, exactly as the compile path judges it: the archived prefix is defined on the archive's word rather than re-walked, which is both what production does and what keeps a fixture cheap.
            let kernel =
                match recheck_module_suffix(&module, crate::DEFAULT_STEP_BUDGET, checked_from)
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
        "sound_program",
        A_SOUND_PROGRAM,
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
