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

        ()
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
// Verified against the compiler of the day, while the hole was open: this source compiled (`curios compile` exited 0, and `recheck_module_suffix` on the compile path certified `let /bad : False = boom(coerce(only()))`), and running it trapped at the `unreachable` the vacuous elimination emitted, which is the runtime witness that the impossibility claim was false. The rule that refuses it now exists: a clash may only be concluded at a position whose type distinguishes its inhabitants, and the shared walk both checkers reach decides that from the declaration's own `result_sort` (`curios-cert/src/invert.rs`), deriving nothing at all — no clash, no equations — at a `Prop`-valued position.
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
    // `NotAsked` here is the elaborator gating first, not the kernel being silent: its half of both
    // rules is guarded in `curios-cert`'s own tests, which reach it by building the module directly.
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
