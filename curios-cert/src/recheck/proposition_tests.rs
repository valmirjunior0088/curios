//! What a proposition may carry, and what a proof may reach.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_analysis::{Erased, test_support::*},
    curios_common::fill,
    curios_core::{Definitions, Global, Item},
    curios_surface::FormScope,
};

/// The derivation a `Prop` carrying a type made possible, as a whole module: `Box : Prop 1 | mk(a : Type 0)`, `unbox` taking a `Box` apart into its type, and `forged : False` cast along the equality of any two types `unbox` carries `mk(A) = mk(B)` to.
const TYPE_CARRYING_PROPOSITION: &str = r#"
induct /Box: pub #{level = 1} Prop
| mk(a: Type)
end

induct /Eq(A: #{level = 1} Type): (x: A, y: A) -> pub #{level = 1} Prop
| refl(z: A): (z, z)
end

induct /False: pub Prop
end

let /unbox: (b: #{node = InductType} /Box()) -> Type =
    (b: #{node = InductType} /Box()) => #{goal = Type} match b | mk(a) => a end;

let /boxes_equal: (A: Type, B: Type) ->
    #{node = InductType}
    /Eq(#{node = InductType} /Box())(#{node = Variant} /Box/mk(A), #{node = Variant} /Box/mk(B)) =
    (A: Type, B: Type) =>
        #{node = Variant} /Eq/refl(@#{node = InductType} /Box(), #{node = Variant} /Box/mk(A));

let /types_equal: (A: Type, B: Type) -> #{node = InductType} /Eq(Type)(A, B) =
    (A: Type, B: Type) =>
        #{goal = #{node = InductType} /Eq(Type)(/unbox(#{node = Variant} /Box/mk(A)), /unbox(#{node = Variant} /Box/mk(B)))}
        match /boxes_equal(A, B)
        | refl(z) => #{node = Variant} /Eq/refl(@Type, /unbox(z))
        end;

let /cast: (A: Type, B: Type, v: A) -> B =
    (A: Type, B: Type, v: A) =>
        (#{goal = (w: A) -> B} match /types_equal(A, B) | refl(z) => (w2: z) => w2 end)(v);

let /forged: #{node = InductType} /False() =
    /cast({}, #{node = InductType} /False(), ());

/forged
: #{node = InductType} /False()
"#;

/// `Box : Prop 1 | mk(a : Type 0)` is a legal declaration — a proposition at the level its payload fits under — and the large-elimination guard is what keeps a proposition holding a type from handing it back. Were the guard to admit `unbox` — `carries_information` reporting a universe-typed payload as carrying nothing, on the reasoning that erasure deletes a type either way — every step after that is ordinary: irrelevance makes `mk(A)` and `mk(B)` convertible at `Box`, so `refl` inhabits `Eq(@Box)(mk(A), mk(B))`; congruence through `unbox` carries that to `Eq(@Type 0)(A, B)` for *any* two types; and transport — the licensed singleton case, `refl`'s payload being pinned by its own targets — turns `()` into a proof of `False`.
///
/// No surface program reaches it: `curios-elab`'s `singleton_eliminable` refuses `unbox` at every surface spelling. The fixtures in `crate::certifier::infer::eliminate::large_elimination_tests` pin the predicate; this pins the consequence, and it is the reason the predicate's two call sites are worth guarding separately.
#[test]
fn a_derivation_through_a_type_carrying_proposition_is_refused() {
    let verdicts = text_refusals(TYPE_CARRYING_PROPOSITION);

    assert!(
        refuses(&verdicts, Some("unbox"), |error| matches!(
            error,
            Error::LargeElimination(family) if *family == global("Box")
        )),
        "the certifier accepted a closed inhabitant of `False`: {verdicts:?}",
    );
}

/// `Held : Prop | qed(u : Io({}))`, the host row `exit` that does not return, and `bad : Held = qed(${carried})`.
const A_PROOF_CARRYING: &str = r#"
induct /Held: pub Prop
| qed(u: #IoType({}))
end

foreign /exit(Byte) -> Never;

let /bad: #{node = InductType} /Held() =
    #{node = Variant} /Held/qed(${carried});
"#;

/// What the walk says of [`A_PROOF_CARRYING`] at `carried`, read where a row's form is made: against the registry these suites judge under, with no unit in scope.
fn carried_refusals(carried: &str) -> Vec<Refusal> {
    let scope = FormScope::over(SYNTAX, []);
    let put = Put {
        scope: Some(&scope),
        ..put()
    };
    let (definitions, entry) = put
        .read(&fill!(A_PROOF_CARRYING, carried))
        .unwrap_or_else(|unread| panic!("{unread:?}"));

    put.module(&definitions, entry.as_ref()).into_refusals()
}

/// Whether `bad` was refused as a proof not known to terminate that reaches no definition.
fn bad_refused_with_no_definition_to_blame(verdicts: &[Refusal]) -> bool {
    refuses(verdicts, Some("bad"), |error| {
        matches!(
            error,
            Error::NotTotal {
                erased: Erased::Proof,
                reached: None,
            }
        )
    })
}

/// Proof totality has two routes to a refusal, and no program reaches the second. `check_positions` first asks whether a recorded position *reaches a definition known partial* — the named route, which blames a global — and failing that asks whether the position is partial in itself, which blames nothing: whether it encloses a `rec` group the walk typed and found not to descend, or calls a host row that diverges.
///
/// Across a certifier walk of the whole prelude and every program in `curios`'s test corpus the anonymous route refuses nothing: every surface spelling that would reach it is refused during elaboration, so no module carries it here — `rec b : False = b; b`, the shape `curios`'s `tests::rules` totality fixtures use, never arrives.
///
/// A call to `/std/Proc/exit` is the trigger that isolates this route rather than merely reaching it. A non-descending `rec` at a proof type is refused by `check_group`'s own local gate before the position walk runs, so it demonstrates that gate instead; an exit meets no gate of its own. `exit` here yields `Io({})`, the type `qed` holds, and `Held/qed(exit(@{}, 0))` is therefore well typed at a proposition while carrying a computation that does not terminate. That is proof totality's whole subject: erasure deletes the proof, the exit never fires, and the program continues holding a certificate for something no total term established.
///
/// The control is the same module with `Io/pure(())` in place of the exit, which must stay accepted — a rule refusing every `Prop`-typed constructor application would satisfy the assertion above and nothing else here would notice.
#[test]
fn an_exit_inside_a_proof_is_refused_with_no_definition_to_blame() {
    let verdicts = carried_refusals("#{node = Foreign} /exit(@{}, #Byte(0))");

    assert!(
        bad_refused_with_no_definition_to_blame(&verdicts),
        "the certifier accepted a proof carrying an exit: {verdicts:?}",
    );
}

/// The anonymous route's other trigger: a group that never descends, enclosed by a proof. `wait` is at `Io({})`, a relevant type, so `check_group`'s own gate lets it stand, and nothing else refuses it but the proof around it.
///
/// The walk types the group with the binders around it opened, and notes the verdict against every position and definition it was typed within — the only place the two can be joined, since the term holding the group spells it closed. Mutation-checked: a group closing without that note leaves the proof accepted.
#[test]
fn a_wait_inside_a_proof_is_refused_with_no_definition_to_blame() {
    let verdicts = carried_refusals("#{node = Rec} (let wait: #IoType({}) = wait; wait)");

    assert!(
        bad_refused_with_no_definition_to_blame(&verdicts),
        "the certifier accepted a proof enclosing a group that never descends: {verdicts:?}",
    );
}

/// The control for the fixtures above: the same proposition built from a description that does nothing stays accepted.
#[test]
fn a_proof_carrying_the_unit_value_is_accepted() {
    assert_eq!(
        carried_refusals("#IoPure({}, ())"),
        Vec::new(),
        "an ordinary proof at a unit-carrying proposition was refused",
    );
}

/// `Wrap(b : Bool) : Prop` holding one field whose type is a `match` on `b` that states each arm a `Prop` and computes `Nat` in both, and the derivation of `False` that transport along `refl` at `Wrap(true)` makes from it.
const A_COMPUTED_RELEVANT_FIELD: &str = r#"
induct /Eq(A: #{level = 1} Type): (x: A, y: A) -> pub #{level = 1} Prop
| refl(z: A): (z, z)
end

induct /False: pub Prop
end

induct /Held: (n: #NatType) -> pub Type
| yes(): (0)
end

struct /Wrap(b: #BoolType): pub Prop {
    held: #{goal = Prop} match b | false => #NatType | true => #NatType end,
}

let /forged:
    #{node = InductType}
    /Eq(#{node = StructType} /Wrap(true))(/Wrap(true) { 0 }, /Wrap(true) { 1 }) =
    #{node = Variant} /Eq/refl(@#{node = StructType} /Wrap(true), /Wrap(true) { 0 });

let /cast: (w: #{node = InductType} /Held()(0)) -> #{node = InductType} /Held()(1) =
    #{goal =
        (w: #{node = InductType} /Held()(#{field = Struct(/Wrap, 0)} (/Wrap(true) { 0 }).0)) ->
            #{node = InductType}
            /Held()(#{field = Struct(/Wrap, 0)} (/Wrap(true) { 1 }).0)}
    match /forged
    | refl(z) => (w2: #{node = InductType} /Held()(#{field = Struct(/Wrap, 0)} z.0)) => w2
    end;

let /held: #{node = InductType} /Held()(1) =
    /cast(#{node = Variant} /Held/yes());

let /boom: #{node = InductType} /False() =
    #{goal = #{node = InductType} /False()} match /held end;
"#;

/// A declaration's domains are typed whatever sort the declaration lands in, a `Prop`-sorted one included.
///
/// `check_signature` owes clause 6 two things — that every constructor or field domain is *well-sorted*, and that a `Type`-sorted one sits at or below the family's declared level — and the first must not run inside the second, which reads a domain only where the result reduces to a sort. Nothing else covers those positions: `check_arity` reaches an `induct`'s parameters and indices but not its constructor telescopes, a `struct` reaches no `check_arity` at all, and `check_non_informative` only *classifies* each field with `Sort::of`, the lookup that reads a claim rather than checking one.
///
/// The claim it reads is what turns the gap into a forgery. `Sort::of` classifies a stuck type-valued `match` by its **goal** — the same reading `goal_sort` closes for the large-elimination guard by typing the goal where `infer` meets the elimination. A field type is never met by `infer`, so the lie is available there: `struct Wrap(b : Bool) : Prop { held : match b : Prop | false => Nat | true => Nat end }` classifies `Prop`, so the non-informativeness rule excuses the field, while `Wrap(true)` really carries a `Nat`.
///
/// From there the derivation is the one `check_non_informative`'s own documentation predicts. `Wrap(true)` is `Prop`-sorted, so irrelevance identifies `Wrap(true){0}` with `Wrap(true){1}`, and `refl(Wrap(true), Wrap(true){0})` therefore inhabits `Eq(@Wrap(true))(Wrap(true){0}, Wrap(true){1})` — the indices being compared at a `Prop`-sorted domain, which is the rule working correctly. Transporting along it at the *honest* goal `(Held(Wrap(true) { 0 }.0)) -> Held(Wrap(true) { 1 }.0)`, whose indices the arm reads at its case, yields `(Held(0)) -> Held(1)`, and `Held`'s only constructor targets `0`, so the vacuous elimination coverage licenses proves `False`.
///
/// No surface program reaches it, because `curios-elab` keeps the two clauses apart: `check_telescope_entries` types every declaration domain through `check_is_sort`, and `size_domains` is a separate walk, which returns early only where the result is no sort. So this is written as Core text.
///
/// Its control is [`a_proposition_carrying_a_computed_proof_is_still_accepted`], the same computed field type with arms that really are propositions: a fix refusing every `Prop`-sorted declaration, or every field type it could not read off syntactically, would fail it.
///
/// The refusal is required to name `Wrap` and not merely to exist, because every later item in the derivation is built on the forged field and would mismatch for a downstream reason once anything at all went wrong. What has to be refused is the *declaration*, at the arm the goal lied about: a `Nat` at `Type 0` checked against the `Prop` the goal claims.
#[test]
fn a_proposition_may_not_carry_a_computed_relevant_field() {
    let verdicts = text_refusals(A_COMPUTED_RELEVANT_FIELD);

    assert!(
        refuses(&verdicts, Some("Wrap"), |error| matches!(
            error,
            Error::Mismatch { .. }
        )),
        "the certifier accepted a closed inhabitant of `False`: {verdicts:?}",
    );
}

/// `Wrap(b : Bool) : Prop` holding one field whose type is a `match` on `b` computing `True` in both arms, and one value of it at `true`.
const A_COMPUTED_PROOF: &str = r#"
induct /True: pub Prop
| qed()
end

struct /Wrap(b: #BoolType): pub Prop {
    held: #{goal = Prop} match b
    | false => #{node = InductType} /True()
    | true => #{node = InductType} /True()
    end,
}

let /wrapped: #{node = StructType} /Wrap(true) =
    /Wrap(true) { #{node = Variant} /True/qed() };
"#;

/// The control: the same computed field type with `Prop`-sorted arms still declares, so the guard above refuses a goal that lies rather than every field a declaration has to reduce to read.
#[test]
fn a_proposition_carrying_a_computed_proof_is_still_accepted() {
    assert_eq!(text_refusals(A_COMPUTED_PROOF), Vec::new());
}

/// `Vouched : Prop | qed(u : {})`, the group of one member `lemma : Vouched = qed(())` marked no proof, and `corollary : Vouched = lemma`.
const A_PROOF_GROUP: &str = r#"
induct /Vouched: pub Prop
| qed(u: {})
end

#{node = Rec}
let /lemma: #{node = InductType} /Vouched() =
    #{node = Variant} /Vouched/qed(());

let /corollary: #{node = InductType} /Vouched() =
    /lemma;
"#;

/// The group of one member `lemma : Nat = 2`, marked a proof.
const A_GROUP_MARKED_A_PROOF_AT_NAT: &str = r#"
#{node = Rec, proof = true}
let /lemma: #NatType =
    2;
"#;

/// The walk states a group's member a proof as it states a definition one, whatever the module says: one at a proposition is stated a proof though its module marks it none, and read, by its type, by the declaration after it, and one at `Nat` is stated none though its module marks it one.
#[test]
fn a_groups_member_is_stated_a_proof_where_its_type_is_a_proposition() {
    let stated = text_certified(A_PROOF_GROUP);
    assert_eq!(stated.refusals(), Vec::new());
    assert_eq!(member_proofs(stated.definitions(), "lemma"), [true]);

    let stated = text_certified(A_GROUP_MARKED_A_PROOF_AT_NAT);
    assert_eq!(stated.refusals(), Vec::new());
    assert_eq!(member_proofs(stated.definitions(), "lemma"), [false]);
}

/// Whether each member of the group `definitions` holds `symbol` in is a proof, as its walk stated it.
fn member_proofs(definitions: &Definitions, symbol: &str) -> Vec<bool> {
    definitions
        .items
        .iter()
        .find_map(|item| match item {
            Item::Rec(rec)
                if rec
                    .exports()
                    .iter()
                    .any(|member| member.name == global(symbol)) =>
            {
                Some(rec.exports().iter().map(|member| member.proof).collect())
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
    let verdicts = core_text_refusals(&fill!(
        "${PROPOSITIONS}\n${FORGED_VALUE_SORT}",
        PROPOSITIONS,
        FORGED_VALUE_SORT
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Refusal {
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
        #{goal = Prop} match b
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
    let verdicts = core_text_refusals(&fill!(
        "${PROPOSITIONS}\n${A_PAYLOAD_AT_THE_CALLERS_SORT}",
        PROPOSITIONS,
        A_PAYLOAD_AT_THE_CALLERS_SORT
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Refusal {
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
let /in_a_type_in_a_groups_tail : #NatType =
    #{node = Rec}
    (let d: (u: {}) -> #BoolType = (u2: {}) => d(u2);
    (let f: (h: /std/Bool/Holds(d(()))) -> #NatType = (h2: /std/Bool/Holds(d(()))) => 0;
    0));
#{field = Struct(/Box, 0)}
(/spin(())).0
"#;

// The certifier records a position wherever it types a term at a proposition or a sort, so each is refused under the declaration that holds it: a proof by the definition it reaches, or with no name to blame where it reaches a member of a group standing in a term, which it reads by value; and a type where it holds such a group, a type naming a finished definition that does not descend, `in_a_signature`'s, being no fault, since the definition unfolds to nothing. `curios`'s `tests::rules::totality_partiality` puts the positions a program writes to the elaborator under this name.
//
// Mutation-checked at `Certifier::record_checked`: with every position dropped before it is recorded, nothing here is refused `NotTotal`.
#[test]
fn a_proof_or_type_that_may_not_terminate_is_refused_wherever_it_stands() {
    let verdicts = core_text_refusals(&fill!(
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
        verdicts
            .iter()
            .all(|verdict| verdict.name != Some(global("in_a_signature"))),
        "{verdicts:?}"
    );
    assert!(
        refused(Some(global("in_a_groups_tail")), Erased::Proof, None),
        "{verdicts:?}"
    );
    assert!(
        refused(
            Some(global("in_a_type_in_a_groups_tail")),
            Erased::Type,
            None
        ),
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

// An occurrence's parameters are checked as arguments are, by subsumption, which holds no proposition below a type: the head rule `certifier::infer::tests::sort`'s `the_head_rules_still_decide_a_bare_sort` pins, reached through a family's occurrence. `curios`'s `tests::rules::subsumption` puts the program to the elaborator under this name.
//
// Mutation-checked at `subsumes`: with two sorts compared by their levels whatever their qualities, `held` types.
#[test]
fn a_proposition_is_no_parameter_a_type_is_wanted_at() {
    let verdicts = core_text_refusals(&fill!(
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
                        if **inferred == term("Prop") && **expected == term("Type")
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
        #{goal = #NatType}
        match a
        | intro(@x, below) =>
            /f(#NatAdd(n, 1), lt, below(#NatAdd(n, 1), lt(n)))
        end;
/std/Bool/True/qed()
"#;

// The elimination whose conversion Carneiro (*The Type Theory of Lean*, 2019, §3.1) shows no checker decides transitively is refused at its source: a proposition is taken apart into data only where its indices pin every payload, and `below` is a proof no index pins. `curios`'s `tests::rules::proposition_elimination` puts the program to the elaborator under this name.
//
// Mutation-checked at `guard_large_elimination` (`curios-cert/src/certifier/infer/eliminate.rs`): with the singleton rung excusing a payload whose type is a proposition, as Lean's subsingleton elimination does, `/f` is admitted.
#[test]
fn a_proposition_holding_a_proof_cannot_be_eliminated_into_data() {
    let verdicts = core_text_refusals(&fill!(
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

// A member stands in its group's own signatures and bodies as the local it is assumed at, which names nothing, so a proof or a type there that reaches a member of a group that does not descend is read as it stands outside the group, under the item that holds the group: an item's member by its declaration, which the refusal names, and a member of a group in a term as that group closed, which the position encloses and no name holds. Mutation-checked: with `Positions::close_over_group` leaving its positions as they are, each refusal of an item's group names nothing and this fails.
#[test]
fn a_group_that_does_not_descend_is_reached_by_every_proof_or_type_naming_a_member() {
    for (module, holder, erased, reached) in [
        (A_DICTIONARY_CALLING_ITSELF, "w", Erased::Proof, Some("w")),
        (
            A_SIBLING_READING_THE_DICTIONARY,
            "n",
            Erased::Proof,
            Some("w"),
        ),
        (
            A_DICTIONARY_CALLING_ITSELF_IN_A_TERM,
            "inline",
            Erased::Proof,
            None,
        ),
        (
            A_SIGNATURE_READING_ITS_SIBLING,
            "d",
            Erased::Type,
            Some("d"),
        ),
    ] {
        let verdicts = core_text_refusals(&fill!(
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
                            reached: reached.map(global),
                        }),
            "{holder}: {verdicts:?}"
        );
    }
}

/// `h` names the loop `f`, a member of its own group, in a parameter's type; `${parameter}` is that type, and the control's names no member.
const A_SIGNATURE_NAMING_A_LOOP: &str = r#"
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

// A member stands in its group's own signatures as a local that holds no group, so a type position naming one is read as it stands outside the group, naming the member's declaration, and `/named`, whose value is no proof, is refused as holding a type that does not terminate. The control, whose signatures name no member, stands. Mutation-checked: with `Positions::close_over_group` leaving its positions as they are, `/named` stands and this fails.
#[test]
fn a_signature_naming_a_member_that_does_not_descend_is_a_type_that_does_not_terminate() {
    let named = |parameter: &str| {
        core_text_refusals(&fill!(
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

/// `T(b)`, `Type` at `true` and `Nat` at `false`, and `/d` splitting on `g(n)`: its `false` arm holds `5` at `T(g(n))`, a relevant position, and its `true` arm a type at `T(g(n))` that matches on a group written inline that does not descend. The `false` arm classifies `T(g(n))` first, under its own equation.
const AN_ERASED_HALF_UNDER_EACH_ARM: &str = r#"
let /T: (b: #BoolType) -> #{level = 1} Type =
    (b2: #BoolType) => #{goal = #{level = 1} Type} match b2 | false => #NatType | true => Type end;

let /d: (g: (x: #NatType) -> #BoolType, n: #NatType) -> #NatType =
    (g2: (x: #NatType) -> #BoolType, n2: #NatType) =>
        #{goal = #NatType}
        match g2(n2)
        | false =>
            let a: /T(g2(n2)) = 5;
            0
        | true =>
            let t: /T(g2(n2)) =
                #{goal = Type}
                match
                    (#{node = Rec}
                    (let l: (m: #NatType) -> #NatType = (m2: #NatType) => l(m2);
                    l))(0)
                | 0 => #NatType
                | k + 1 => #NatType
                end;
            1
        end;
"#;

// A position's erased half is remembered by its one door (`erased_half`) under the equations in force where it was classified, and goes where they do: the `true` arm classifies `T(g(n))` again, as a sort, and its term, a type holding a group that does not descend, is refused by type totality, with the memos on as off. Answered from the `false` arm, the term would be classified relevant and pass unconstrained.
#[test]
fn an_erased_half_read_under_one_arm_answers_nothing_under_its_sibling() {
    let definitions = core_text_module(AN_ERASED_HALF_UNDER_EACH_ARM);

    for uncached in [false, true] {
        let mut certifier = match uncached {
            false => Certifier::new(BUDGET, SYNTAX),
            true => Certifier::uncached(BUDGET, SYNTAX),
        };
        let verdicts = super::refusals_from(
            &mut certifier,
            &definitions,
            None,
            &Globals::default(),
            None,
        )
        .module()
        .into_refusals();
        assert!(
            verdicts.iter().any(|verdict| {
                verdict.name == Some(global("d"))
                    && verdict.error
                        == Error::NotTotal {
                            erased: Erased::Type,
                            reached: None,
                        }
            }),
            "with the memos off: {uncached}: {verdicts:?}"
        );
    }
}
