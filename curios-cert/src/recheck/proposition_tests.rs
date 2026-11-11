//! What a proposition may carry, and what a proof may reach.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::{Erased, test_support::*},
    curios_common::{Qualifier, fill},
    curios_core::{
        Atom, Definitions, Global, InductParam, Intrinsic, Item, Nat, Telescope, Term, Universes,
        test_support::*,
    },
    std::collections::BTreeMap,
};

/// The derivation a `Prop` carrying a type made possible, as a whole module.
///
/// `Box : Prop 1 | mk(a : Type 0)` is a legal declaration — a proposition at the level its payload fits under — and the large-elimination guard is what keeps a proposition holding a type from handing it back. Were the guard to admit `unbox` — `carries_information` reporting a universe-typed payload as carrying nothing, on the reasoning that erasure deletes a type either way — every step after that is ordinary: irrelevance makes `mk(A)` and `mk(B)` convertible at `Box`, so `refl` inhabits `Eq(@Box)(mk(A), mk(B))`; congruence through `unbox` carries that to `Eq(@Type 0)(A, B)` for *any* two types; and transport — the licensed singleton case, `refl`'s payload being pinned by its own targets — turns `()` into a proof of `False`.
///
/// No surface program reaches it: `curios-elab`'s `singleton_eliminable` refuses `unbox` at every surface spelling. The fixtures in `crate::certifier::infer::eliminate::large_elimination_tests` pin the predicate; this pins the consequence, and it is the reason the predicate's two call sites are worth guarding separately.
#[test]
fn a_derivation_through_a_type_carrying_proposition_is_refused() {
    let verdicts = fixture_verdicts(&forgery(), 1_000_000, &Globals::default(), SYNTAX);

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::LargeElimination(_))),
        "the certifier accepted a closed inhabitant of `False`: {verdicts:?}",
    );
}

/// (V) has two routes to a refusal, and no program reaches the second. `check_positions` first asks whether a recorded position *reaches a definition known partial* — the named route, which blames a global — and failing that asks whether the position is partial in itself, which blames nothing: whether it encloses a `rec` group the walk typed and found not to descend, or calls a host row that diverges.
///
/// Across a certifier walk of the whole prelude and every program in `curios`'s test corpus the anonymous route refuses nothing: every surface spelling that would reach it is refused during elaboration, so no module carries it here — `rec b : False = b; b`, the shape `curios`'s `tests::rules` totality fixtures use, never arrives.
///
/// A call to `/std/Proc/exit` is the trigger that isolates this route rather than merely reaching it. A non-descending `rec` at a proof type is refused by `check_group`'s own local gate before the position walk runs, so it demonstrates that gate instead; an exit meets no gate of its own. `exit` here yields `Io({})`, the type `qed` holds, and `Held/qed(exit(@{}, 0))` is therefore well typed at a proposition while carrying a computation that does not terminate. That is (V)'s whole subject: erasure deletes the proof, the exit never fires, and the program continues holding a certificate for something no total term established.
///
/// The control is the same module with `Io/pure(())` in place of the exit, which must stay accepted — a rule refusing every `Prop`-typed constructor application would satisfy the assertion above and nothing else here would notice.
#[test]
fn an_exit_inside_a_proof_is_refused_with_no_definition_to_blame() {
    let verdicts = fixture_verdicts(
        &proof_carrying_unit(Carried::Exit),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts.iter().any(|verdict| matches!(
            verdict.error,
            Error::NotTotal {
                erased: Erased::Proof,
                reached: None,
            }
        )),
        "the certifier accepted a proof carrying an exit: {verdicts:?}",
    );
}

/// The anonymous route's other trigger: a group that never descends, enclosed by a proof. `wait` is at `Io({})`, a relevant type, so `check_group`'s own gate lets it stand, and nothing else refuses it but the proof around it.
///
/// The walk types the group with the binders around it opened, and notes the verdict against every position and definition it was typed within — the only place the two can be joined, since the term holding the group spells it closed. Mutation-checked: a group closing without that note leaves the proof accepted.
#[test]
fn a_wait_inside_a_proof_is_refused_with_no_definition_to_blame() {
    let verdicts = fixture_verdicts(
        &proof_carrying_unit(Carried::Wait),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts.iter().any(|verdict| matches!(
            verdict.error,
            Error::NotTotal {
                erased: Erased::Proof,
                reached: None,
            }
        )),
        "the certifier accepted a proof enclosing a group that never descends: {verdicts:?}",
    );
}

/// The control for the fixtures above: the same proposition built from a description that does nothing stays accepted.
#[test]
fn a_proof_carrying_the_unit_value_is_accepted() {
    assert_eq!(
        fixture_verdicts(
            &proof_carrying_unit(Carried::Pure),
            1_000_000,
            &Globals::default(),
            SYNTAX
        ),
        Vec::new(),
        "an ordinary proof at a unit-carrying proposition was refused",
    );
}

/// A declaration's domains are typed whatever sort the declaration lands in, a `Prop`-sorted one included.
///
/// `check_signature` owes clause 6 two things — that every constructor or field domain is *well-sorted*, and that a `Type`-sorted one sits at or below the family's declared level — and the first must not run inside the second, which reads a domain only where the result reduces to a sort. Nothing else covers those positions: `check_arity` reaches an `induct`'s parameters and indices but not its constructor telescopes, a `struct` reaches no `check_arity` at all, and `check_non_informative` only *classifies* each field with `Sort::of`, the lookup that reads a claim rather than checking one.
///
/// The claim it reads is what turns the gap into a forgery. `Sort::of` classifies a stuck type-valued `match` by its **motive** — the same reading `check_motive` closes for the large-elimination guard by typing the motive where `infer` meets the elimination. A field type is never met by `infer`, so the lie is available there: `struct Wrap(b : Bool) : Prop { held : match b : Prop | false => Nat | true => Nat end }` classifies `Prop`, so the non-informativeness rule excuses the field, while `Wrap(true)` really carries a `Nat`.
///
/// From there the derivation is the one `check_non_informative`'s own documentation predicts. `Wrap(true)` is `Prop`-sorted, so irrelevance identifies `Wrap(true){0}` with `Wrap(true){1}`, and `refl(Wrap(true), Wrap(true){0})` therefore inhabits `Eq(@Wrap(true))(Wrap(true){0}, Wrap(true){1})` — the indices being compared at a `Prop`-sorted domain, which is the rule working correctly. Transporting along it under the *honest* motive `(s, t, q) => (Held(s.0)) -> Held(t.0)` yields `(Held(0)) -> Held(1)`, and `Held`'s only constructor targets `0`, so the vacuous elimination coverage licenses proves `False`.
///
/// No surface program reaches it, because `curios-elab` keeps the two clauses apart: `check_telescope_entries` types every declaration domain through `check_is_sort`, and `size_domains` is a separate walk, which returns early only where the result is no sort. So this is constructed by hand.
///
/// Its control is [`a_proposition_carrying_a_computed_proof_is_still_accepted`], the same computed field type with arms that really are propositions: a fix refusing every `Prop`-sorted declaration, or every field type it could not read off syntactically, would fail it.
///
/// The refusal is required to name `Wrap` and not merely to exist, because every later item in the derivation is built on the forged field and would mismatch for a downstream reason once anything at all went wrong. What has to be refused is the *declaration*, at the arm the motive lied about: a `Nat` at `Type 0` checked against the `Prop` the motive claims.
#[test]
fn a_proposition_may_not_carry_a_computed_relevant_field() {
    let verdicts = fixture_verdicts(
        &computed_field_forgery(),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );
    let wrap = Global::Authored(Qualifier::from(["Wrap"]));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name.as_ref() == Some(&wrap)
                && matches!(verdict.error, Error::Mismatch { .. })),
        "the certifier accepted a closed inhabitant of `False`: {verdicts:?}",
    );
}

/// The control: the same computed field type with `Prop`-sorted arms still declares, so the guard above refuses a motive that lies rather than every field a declaration has to reduce to read.
#[test]
fn a_proposition_carrying_a_computed_proof_is_still_accepted() {
    let true_name = Global::Authored(Qualifier::from(["True"]));
    let true_type = Term::induct_type(true_name, Vec::<Term>::new(), Vec::<Term>::new());
    let qed = Term::variant(true_name, Vec::<Term>::new(), "qed", Vec::<Term>::new());
    let true_decl = proposition(vec![(
        Atom::from("qed"),
        InductParam::new(Telescope::done(Vec::new()), Vec::new()),
    )]);

    let wrap_name = Global::Authored(Qualifier::from(["Wrap"]));
    let definitions = declaring(
        Definitions {
            items: vec![wrapped_at_true(&wrap_name, qed)].into(),
        },
        BTreeMap::from([(true_name, true_decl)]),
        BTreeMap::from([(
            wrap_name,
            computed_field_wrapper(true_type.clone(), true_type),
        )]),
        [],
    );

    assert_eq!(
        fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX),
        Vec::new()
    );
}

/// The walk states a group's member a proof as it states a definition one, whatever the module says: one at a proposition is stated a proof and read, by its type, by the declaration after it, and one at `Nat` is stated none, its module's mark notwithstanding.
#[test]
fn a_groups_member_is_stated_a_proof_where_its_type_is_a_proposition() {
    let (vouched_name, declaration) = vouched_declaration();
    let vouched = Term::induct_type(vouched_name, Vec::<Term>::new(), Vec::<Term>::new());
    let qed = Term::variant(
        vouched_name,
        Vec::<Term>::new(),
        "qed",
        [Term::tuple(Vec::<Term>::new())],
    );
    let lemma = Global::Authored(Qualifier::from(["lemma"]));

    let honest = collision_definitions(
        vec![
            proof_group(&lemma, vouched.clone(), qed),
            authored(
                &Global::Authored(Qualifier::from(["corollary"])),
                vouched,
                Term::instance(lemma, Universes::default()),
            ),
        ],
        BTreeMap::from([(vouched_name, declaration)]),
    );
    let stated = fixture_certified(&honest, 1_000_000, &Globals::default(), SYNTAX);
    assert_eq!(stated.verdicts, Vec::new());
    assert_eq!(member_proofs(&stated.definitions, &lemma), [true]);

    let lying = collision_definitions(
        vec![proof_group(
            &lemma,
            Term::intrinsic(Intrinsic::NatType),
            Term::intrinsic(Intrinsic::Nat(Nat::new(2usize))),
        )],
        BTreeMap::new(),
    );
    let stated = fixture_certified(&lying, 1_000_000, &Globals::default(), SYNTAX);
    assert_eq!(stated.verdicts, Vec::new());
    assert_eq!(member_proofs(&stated.definitions, &lemma), [false]);
}

/// Whether each member of the group `definitions` holds `name` in is a proof, as its walk stated it.
fn member_proofs(definitions: &Definitions, name: &Global) -> Vec<bool> {
    definitions
        .items
        .iter()
        .find_map(|item| match item {
            Item::Rec(rec) if rec.definitions.iter().any(|member| member.name == *name) => {
                Some(rec.definitions.iter().map(|member| member.proof).collect())
            }
            _ => None,
        })
        .expect("the module holds the group")
}

/// `/T` is `Bool`, written with a decorator stating that its value is a proposition, and the term this closes with is one at `/H(false)` built from one at `/H(true)`.
const FORGED_VALUE_SORT: &str = r#"
#{value_sort = Prop}
let /T : Type = #BoolType;
let /H : (x: /T) -> Prop = (x: /T) => /std/Bool/Holds(x);
((h: /H(false)) => h)(
    ((k: /H(true)) => k)(
        #{node = Variant}
        /std/Bool/True/qed()
    )
)
"#;

// A definition's value sort is a publication `Sort::of` reads with nothing run, ahead of the irrelevance gate: read as the decorator states it, `/T` would be a proposition, `true` and `false` one at it, and the two calls of `/H` one by their spines, so the closing term would stand at `Holds(false)`, which is `False`. The certifier states the sort `/T`'s value inhabits itself, `Type`, and the two calls stay apart.
#[test]
fn a_value_sort_a_module_states_decides_no_comparison() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${FORGED_VALUE_SORT}",
        PROPOSITIONS,
        FORGED_VALUE_SORT
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict {
                name: None,
                error: Error::Mismatch { .. }
            }]
        ),
        "{verdicts:?}"
    );
}

/// `/Box` is a proposition whose one payload stands at a sort it leaves to its caller, no index pinning it; `/H` takes the payload of a `/Box` of `Bool` into a type, and the term this closes with is one at `/H` of `box(false)` built from one at `/H` of `box(true)`.
const A_PAYLOAD_AT_THE_CALLERS_SORT: &str = r#"
#{universe_context = {quality_count = 1}}
induct /Box(A: Sort): pub Prop
| box(x: A)
end
let /H : (
    b:
        #{node = InductType, universes = {qualities = [Type]}}
        /Box(#BoolType)
) -> Prop =
    (
        b:
            #{node = InductType, universes = {qualities = [Type]}}
            /Box(#BoolType)
    ) =>
        match b: (b2) => Prop
        | box(x) =>
            /std/Bool/Holds(x)
        end;
((h: /H(
    (
        #{universes = {qualities = [Type]}}
        /Box/box)(@#BoolType, false)
)) => h)(
    ((k: /H(
        (
            #{universes = {qualities = [Type]}}
            /Box/box)(@#BoolType, true)
    )) => k)(
        #{node = Variant}
        /std/Bool/True/qed()
    )
)
"#;

// A proposition may hold data, a payload at a sort its caller states being a proof at `Prop` and a `Bool` at `Type`, and its two values at `Bool` are one by irrelevance: the closing term stands at `/H` of `box(false)` because the two calls of `/H` are one by their spines. What keeps that from `False` is that no payload is taken out of the proposition into a type: `/H` is refused, and the closing term, read against `/H` as it stands, is not.
//
// Mutation-check: let the certifier's large-elimination guard take a proposition of one constructor into a type whatever its payloads, as though each were pinned, and the module is admitted whole.
#[test]
fn a_proposition_holding_a_payload_at_its_callers_sort_is_not_taken_apart_into_a_type() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_PAYLOAD_AT_THE_CALLERS_SORT}",
        PROPOSITIONS,
        A_PAYLOAD_AT_THE_CALLERS_SORT
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict {
                name: Some(name),
                error: Error::LargeElimination(family)
            }] if *name == global("H") && *family == global("Box")
        ),
        "{verdicts:?}"
    );
}

/// A proof or a type reaching a definition not known to terminate, at each position the elaborator checks one: a `let` at a proposition, a structure's field, a case's payload, an implicit argument, a witness's method, a test's body, the type a definition states, a local group's tail, and the term the module closes with. `spin` hands back a `Box` it never builds, so its field is a `False` wherever a position takes one in, `decide` a `Bool` it never decides, and `h` in the tail is `spin` again as a group standing in a term.
const A_PROOF_OR_TYPE_AT_EACH_POSITION: &str = r#"
struct /Box: pub Type {
    p:
        #{node = InductType}
        /std/Bool/False()
}
#{node = Rec}
let /spin : (u: {}) ->
    #{node = StructType}
    /Box() =
    (u: {}) => /spin(u);
#{node = Rec}
let /decide : (u: {}) -> #BoolType =
    (u: {}) => /decide(u);
induct /Wit: pub Type
| w(
    p:
        #{node = InductType}
        /std/Bool/False()
)
end
concept /Lawful(A: Type): pub Type {
    op: (A) -> A,
    law: (x: A) ->
        #{node = InductType}
        /std/Bool/False()
}
let /in_a_let : #NatType =
    (let q:
        #{node = InductType}
        /std/Bool/False() =
        #{field = Struct(/Box, 0)}
        (/spin(())).0;
    0);
let /in_a_field :
    #{node = StructType}
    /Box() =
    /Box {
        #{field = Struct(/Box, 0)}
        (/spin(())).0
    };
let /in_a_payload :
    #{node = InductType}
    /Wit() =
    #{node = Variant}
    /Wit/w(
        #{field = Struct(/Box, 0)}
        (/spin(())).0
    );
let /take : (
    @p:
        #{node = InductType}
        /std/Bool/False()
) -> #NatType =
    (
        @p:
            #{node = InductType}
            /std/Bool/False()
    ) => 0;
let /in_an_implicit : #NatType =
    /take(@#{field = Struct(/Box, 0)} (/spin(())).0);
let /in_a_method :
    #{node = StructType}
    /Lawful(#NatType) =
    /Lawful(#NatType) {
        (x: #NatType) => x,
        (x2: #NatType) =>
            #{field = Struct(/Box, 0)}
            (/spin(())).0
    };
#{kind = Test}
let /in_a_test : #NatType =
    (let q:
        #{node = InductType}
        /std/Bool/False() =
        #{field = Struct(/Box, 0)}
        (/spin(())).0;
    0);
let /in_a_signature : (h: /std/Bool/Holds(/decide(()))) -> #NatType =
    (h: /std/Bool/Holds(/decide(()))) => 0;
let /in_a_groups_tail : #NatType =
    #{node = Rec}
    (let h: (u: {}) ->
        #{node = StructType}
        /Box() =
        (u2: {}) => h(u2);
    (let q:
        #{node = InductType}
        /std/Bool/False() =
        #{field = Struct(/Box, 0)}
        (h(())).0;
    0));
#{field = Struct(/Box, 0)}
(/spin(())).0
"#;

// The certifier records a position wherever it types a term at a proposition or a sort, so each is refused under the declaration that holds it: by the definition it reaches, or with no name to blame where it reaches a member of a group standing in a term, which it reads by value. `curios`'s `tests::rules::totality_partiality_tests` puts the positions a program writes to the elaborator under this name.
//
// Mutation-checked at `Certifier::record_checked`: with every position dropped before it is recorded, nothing here is refused `NotTotal`.
#[test]
fn a_proof_or_type_that_may_not_terminate_is_refused_wherever_it_stands() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_PROOF_OR_TYPE_AT_EACH_POSITION}",
        PROPOSITIONS,
        A_PROOF_OR_TYPE_AT_EACH_POSITION
    ));
    let refused = |name: Option<Global>, erased: Erased, reached: Option<Global>| {
        verdicts.iter().any(|verdict| {
            verdict.name == name && verdict.error == Error::NotTotal { erased, reached }
        })
    };

    for name in [
        "in_a_let",
        "in_a_field",
        "in_a_payload",
        "in_an_implicit",
        "in_a_method",
        "in_a_test",
    ] {
        assert!(
            refused(Some(global(name)), Erased::Proof, Some(global("spin"))),
            "`{name}` was not refused for reaching `spin`: {verdicts:?}"
        );
    }
    assert!(
        refused(
            Some(global("in_a_signature")),
            Erased::Type,
            Some(global("decide"))
        ),
        "{verdicts:?}"
    );
    assert!(
        refused(Some(global("in_a_groups_tail")), Erased::Proof, None),
        "{verdicts:?}"
    );
    assert!(
        refused(None, Erased::Proof, Some(global("spin"))),
        "{verdicts:?}"
    );
}

/// A proposition handed to a family's parameter that wants a type: `Holder(False)` would be a family of data over a proof's type.
const A_PROPOSITION_AT_A_TYPE_PARAMETER: &str = r#"
induct /Holder(A: Type): pub Type
| hold(a: A)
end
let /held : Type =
    #{node = InductType}
    /Holder(
        #{node = InductType}
        /std/Bool/False()
    );
/std/Bool/False
"#;

// An occurrence's parameters are checked as arguments are, by subsumption, which holds no proposition below a type: the head rule `certifier::infer::tests::sort`'s `the_head_rules_still_decide_a_bare_sort` pins, reached through a family's occurrence. `curios`'s `tests::rules::subsumption_tests` puts the program to the elaborator under this name.
//
// Mutation-checked at `subsumes`: with two sorts compared by their levels whatever their qualities, `held` types.
#[test]
fn a_proposition_is_no_parameter_a_type_is_wanted_at() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_PROPOSITION_AT_A_TYPE_PARAMETER}",
        PROPOSITIONS,
        A_PROPOSITION_AT_A_TYPE_PARAMETER
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("held"))
                && matches!(
                    &verdict.error,
                    Error::Mismatch { inferred, expected }
                        if **inferred == Term::prop() && **expected == Term::type_ground()
                )),
        "{verdicts:?}"
    );
}

/// Lean's subsingleton elimination: `Accessible`'s one constructor holds `below`, a proof no index pins, and `f` takes an accessibility proof apart into `Nat` along a relation that is not well founded, recursing on `below`'s application.
const A_PROPOSITION_HOLDING_A_PROOF_TAKEN_INTO_DATA: &str = r#"
#{universe_context = {parameter_count = 2}}
induct /Accessible(
    @A:
        #{level = u}
        Type,
    R: (A, A) ->
        #{level = v}
        Prop
): (A) -> pub
    #{level = max(u, v)}
    Prop
| intro(
    @x: A,
    below: (y: A, r: R(y, x)) ->
        #{node = InductType, universes = {levels = [u, v]}}
        /Accessible(A, R)(y)
): (x)
end
let /R : (y: #NatType, x: #NatType) -> Prop =
    (y: #NatType, x: #NatType) => /std/Bool/Holds(#NatLt(x, y));
#{node = Rec}
let /f : (
    n: #NatType,
    lt: (k: #NatType) -> /std/Bool/Holds(#NatLt(k, #NatAdd(k, 1))),
    a:
        #{node = InductType, universes = {levels = [0, 0]}}
        /Accessible(#NatType, /R)(n)
) -> #NatType =
    (
        n: #NatType,
        lt: (k: #NatType) -> /std/Bool/Holds(#NatLt(k, #NatAdd(k, 1))),
        a:
            #{node = InductType, universes = {levels = [0, 0]}}
            /Accessible(#NatType, /R)(n)
    ) =>
        #{result = Ambient(#NatType)}
        match a
        | intro(@x, below) =>
            /f(#NatAdd(n, 1), lt, below(#NatAdd(n, 1), lt(n)))
        end;
/std/Bool/True/qed()
"#;

// The elimination whose conversion Carneiro (*The Type Theory of Lean*, 2019, §3.1) shows no checker decides transitively is refused at its source: a proposition is taken apart into data only where its indices pin every payload, and `below` is a proof no index pins. `curios`'s `tests::rules::proposition_elimination_tests` puts the program to the elaborator under this name.
//
// Mutation-checked at `guard_large_elimination` (`curios-cert/src/certifier/infer/eliminate.rs`): with the singleton rung excusing a payload whose type is a proposition, as Lean's subsingleton elimination does, `/f` is admitted.
#[test]
fn a_proposition_holding_a_proof_cannot_be_eliminated_into_data() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_PROPOSITION_HOLDING_A_PROOF_TAKEN_INTO_DATA}",
        PROPOSITIONS,
        A_PROPOSITION_HOLDING_A_PROOF_TAKEN_INTO_DATA
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("f"))
                && verdict.error == Error::LargeElimination(global("Accessible"))),
        "{verdicts:?}"
    );
}

/// `Bot`'s method is a function to `False`, so a `Bot` dictionary is a proof of `False` at every value of its carrier.
const A_FALSE_CONCEPT: &str = r#"
concept /Bot(A: Type): pub Type {
    bot: (x: A) ->
        #{node = InductType}
        /std/Bool/False()
}
induct /Index: (i: #NatType) -> pub Type
| mk(n: #NatType): (n)
end
"#;

/// A dictionary whose method calls the dictionary's own: a group of one data member that does not descend.
const A_DICTIONARY_CALLING_ITSELF: &str = r#"
#{node = Rec}
let /w :
    #{node = StructType}
    /Bot(#NatType) =
    /Bot(#NatType) {
        (n: #NatType) =>
            (#{field = Struct(/Bot, 0)}
            /w.0)(n)
    };
/std/Bool/True/qed()
"#;

/// [`A_DICTIONARY_CALLING_ITSELF`] beside a natural of its group that hands a proof of `False` read off the dictionary to a function.
const A_SIBLING_READING_THE_DICTIONARY: &str = r#"
#{node = Rec}
let /n : #NatType =
    ((p:
        #{node = InductType}
        /std/Bool/False()) => 0)((#{field = Struct(/Bot, 0)}
    /w.0)(0))
and /w :
    #{node = StructType}
    /Bot(#NatType) =
    /Bot(#NatType) {
        (n2: #NatType) =>
            (#{field = Struct(/Bot, 0)}
            /w.0)(n2)
    };
/std/Bool/True/qed()
"#;

/// [`A_DICTIONARY_CALLING_ITSELF`] as a group standing in a definition's body.
const A_DICTIONARY_CALLING_ITSELF_IN_A_TERM: &str = r#"
let /inline : #NatType =
    #{node = Rec}
    (let w:
        #{node = StructType}
        /Bot(#NatType) =
        /Bot(#NatType) {
            (n: #NatType) =>
                (#{field = Struct(/Bot, 0)}
                w.0)(n)
        };
    0);
/std/Bool/True/qed()
"#;

/// A data member that does not descend, and a sibling whose type reads it.
const A_SIGNATURE_READING_ITS_SIBLING: &str = r#"
#{node = Rec}
let /d : (n: #NatType) -> #NatType =
    (n: #NatType) =>
        /d(n)
and /v :
    #{node = InductType}
    /Index()(/d(0)) =
    /v;
/std/Bool/True/qed()
"#;

// A member stands in its group's own signatures and bodies as the local it is assumed at, which holds no group, so a proof or a type there that reaches a member of a group that does not descend encloses the group by what the group is and not by what the position holds: each is refused under the item that holds the group, the group's first member for a group of the module's own, as a position holding the group closed is. Admitted, `w` would be a proof of `False` in every erased position that reads it, and `v`'s type one no reduction settles.
#[test]
fn a_group_that_does_not_descend_is_enclosed_by_every_proof_or_type_naming_a_member() {
    for (module, holder, erased) in [
        (A_DICTIONARY_CALLING_ITSELF, "w", Erased::Proof),
        (A_SIBLING_READING_THE_DICTIONARY, "n", Erased::Proof),
        (
            A_DICTIONARY_CALLING_ITSELF_IN_A_TERM,
            "inline",
            Erased::Proof,
        ),
        (A_SIGNATURE_READING_ITS_SIBLING, "d", Erased::Type),
    ] {
        let verdicts = core_text_verdicts(&fill!(
            "${PROPOSITIONS}\n${A_FALSE_CONCEPT}\n${module}",
            PROPOSITIONS,
            A_FALSE_CONCEPT,
            module
        ));

        assert!(
            verdicts
                .iter()
                .any(|verdict| verdict.name == Some(global(holder))
                    && verdict.error
                        == Error::NotTotal {
                            erased,
                            reached: None
                        }),
            "{holder}: {verdicts:?}"
        );
    }
}

/// `h` names the loop `f`, a member of its own group, in a parameter's type; `${parameter}` is that type, and the control's names no member.
const A_SIGNATURE_NAMING_A_LOOP: &str = r#"
#{absent = [/E/refl]}
induct /E(a: #NatType): (#NatType) -> pub Prop
| refl(): (a)
end

let /named : #NatType =
    #{node = Rec}
    (let f: (m: #NatType) -> #NatType = (m: #NatType) => #NatAdd(f(m), 1)
    and h: (p: ${parameter}) -> #NatType = (p: ${parameter}) => 0;
    0);
/std/Bool/False
"#;

// A member stands in its group's own signatures as a local that holds no group, so a type position naming one is not seen to enclose a group that does not descend by what it holds: the position is marked as naming it, and `/named`, whose value is no proof, is refused as holding a type that does not terminate. The control, whose signatures name no member, stands. Mutation-checked: with `Positions::enclose_partial_naming` marking nothing, `/named` stands and this fails.
#[test]
fn a_signature_naming_a_member_that_does_not_descend_is_a_type_that_does_not_terminate() {
    let named = |parameter: &str| {
        core_text_verdicts(&fill!(
            "${PROPOSITIONS}\n${GROUP}",
            PROPOSITIONS,
            GROUP = fill!(A_SIGNATURE_NAMING_A_LOOP, parameter)
        ))
    };

    let refused = named("#{node = InductType} /E(f(0))(f(0))");
    assert!(
        refused
            .iter()
            .any(|verdict| verdict.name == Some(global("named"))
                && matches!(verdict.error, Error::NotTotal { .. })),
        "{refused:?}"
    );

    let control = named("#NatType");
    assert!(
        control
            .iter()
            .all(|verdict| verdict.name != Some(global("named"))),
        "{control:?}"
    );
}
