//! A declaration that leaves a sort to its caller: judged at every assignment of the sorts it leaves, and stated whole at each use.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_analysis::test_support::*,
    curios_common::fill,
    curios_core::{Item, Quality, UniverseContext},
    curios_surface::Spelling,
};

/// The refusal a verdict reached at one assignment, with the assignment: `None` for a refusal that names none.
fn at_sorts(verdict: &Verdict) -> Option<(&[Quality], &Error)> {
    match &verdict.error {
        Error::AtSorts { sorts, refusal } => Some((sorts, refusal)),
        _ => None,
    }
}

/// `Held`, a proposition whose one case is not defined.
const HELD: &str = r#"
induct /Held: pub Prop
| qed()
end
"#;

/// `id : (A : Sort, a : A) -> A`, leaving its one sort to its caller under `${node}`, and `id` used at `Type` and at `Prop`.
const USED_AT_BOTH: &str = r#"
#{${node}universe_context = {quality_count = 1}}
let /id: (A: Sort, a: A) -> A =
    (A: Sort, a: A) => a;

let /at_data: #NatType =
    (#{universes = {qualities = [Type]}} /id)(#NatType, 0);

let /at_proof: #{node = InductType} /Held() =
    (#{universes = {qualities = [Prop]}} /id)(
        #{node = InductType} /Held(),
        #{node = Variant} /Held/qed());
"#;

/// `back : (A : Sort) -> ${sort} = (A) => A`, leaving its one sort to its caller under `${node}`: well typed at the sorts `${sort}` holds.
const BACK: &str = r#"
#{${node}universe_context = {quality_count = 1}}
let /back: (A: Sort) -> ${sort} =
    (A: Sort) => A;
"#;

/// `id : (A : Sort, a : A) -> A`, leaving its one sort to its caller.
const ID: &str = r#"
#{universe_context = {quality_count = 1}}
let /id: (A: Sort, a: A) -> A =
    (A: Sort, a: A) => a;
"#;
#[test]
fn one_definition_is_used_at_a_type_and_at_a_proposition() {
    let node = "";
    let verdicts = text_verdicts(&fill!(
        "${HELD}${USED_AT_BOTH}",
        HELD,
        USED_AT_BOTH = fill!(USED_AT_BOTH, node)
    ));

    assert!(verdicts.is_empty(), "{verdicts:?}");
}

/// A use may state either sort, so a declaration is held to both: one well typed where its sort is `Prop` and at no other is refused at `Type`, and its mirror at `Prop`. A walk that judged the first assignment alone would certify the first, and one that judged the last alone the second. Put to each rule that judges a declaration: a definition's, a recursive group's and a registry entry's.
#[test]
fn a_declaration_ill_typed_at_one_sort_alone_is_refused_at_it() {
    for (sort, well_typed_at, refused_at) in [
        ("Prop", "`Prop`", Quality::Type),
        ("Type", "`Type`", Quality::Prop),
    ] {
        let (plain, grouped) = ("", "node = Rec, ");
        for (rule, text) in [
            ("a definition", fill!(BACK, node = plain, sort)),
            ("a recursive group", fill!(BACK, node = grouped, sort)),
            (
                "a registry entry",
                fill!(
                    r#"
let /only: (X: ${sort}) -> ${sort} =
    (X: ${sort}) => X;

#{universe_context = {quality_count = 1}}
induct /Through(A: Sort): pub Type
| through(x: /only(A))
end
"#,
                    sort
                ),
            ),
        ] {
            let verdicts = text_verdicts(&text);
            assert!(
                verdicts.iter().any(|verdict| matches!(
                    at_sorts(verdict),
                    Some((sorts, Error::Mismatch { .. })) if sorts == [refused_at]
                )),
                "{rule}, well typed at {well_typed_at} alone: {verdicts:?}",
            );
        }
    }
}

/// The control for the fixture above: the same definition whose codomain is the sort it is handed is well typed at both.
#[test]
fn a_definition_well_typed_at_every_sort_is_accepted() {
    let (node, sort) = ("", "Sort");
    let verdicts = text_verdicts(&fill!(BACK, node, sort));

    assert!(verdicts.is_empty(), "{verdicts:?}");
}

/// A refusal at one assignment spells the declaration's terms at it, where the declaration's text says `Sort`. Its report restates the type the declaration states, read at the assignment, which is where a reader sees which written sort stood at which.
#[test]
fn a_refusal_at_one_sort_restates_the_declaration_at_it() {
    let (node, sort) = ("", "Prop");
    let definitions = core_text_module(&fill!(BACK, node, sort));
    let reports = fixture_verdicts(&definitions, BUDGET, &Globals::default(), SYNTAX)
        .iter()
        .map(|verdict| {
            verdict.rendered(
                &definitions,
                &Spelling::for_refusal(&definitions, &[], &SYNTAX),
            )
        })
        .collect::<Vec<_>>();

    assert!(
        reports.iter().any(|report| report.starts_with(
            "with the sorts it leaves to its caller at `Type`, where it reads `(A: Type) -> Prop`: "
        )),
        "{reports:?}",
    );
}

/// An occurrence states as many sorts as its declaration leaves, each `Prop` or `Type`: one stating none or too few is short of what the type is read at, and one stating a parameter would be judged at a sort that is neither.
#[test]
fn an_occurrence_states_every_sort_its_declaration_leaves() {
    type Expected = fn(&Error) -> bool;
    let cases: [(&str, String, Expected); 3] = [
        (
            "no sort stated",
            fill!("${ID}\nlet /user: #NatType = /id(#NatType, 0);", ID),
            |error| {
                matches!(
                    error,
                    Error::Arity {
                        counted: Counted::Sorts,
                        expected: 1,
                        actual: 0,
                    }
                )
            },
        ),
        (
            "one sort stated of two",
            r#"
#{universe_context = {quality_count = 2}}
let /both: (A: Sort, B: #{quality = Param(1)} Sort, a: A) -> A =
    (A: Sort, B: #{quality = Param(1)} Sort, a: A) => a;

let /user: #NatType =
    (#{universes = {qualities = [Type]}} /both)(#NatType, #NatType, 0);
"#
            .to_string(),
            |error| {
                matches!(
                    error,
                    Error::Arity {
                        counted: Counted::Sorts,
                        expected: 2,
                        actual: 1,
                    }
                )
            },
        ),
        (
            "a parameter stated by a declaration that leaves none",
            fill!(
                "${ID}\nlet /user: #NatType = (#{universes = {qualities = [Param(0)]}} /id)(#NatType, 0);",
                ID
            ),
            |error| matches!(error, Error::OpenSort(_)),
        ),
    ];
    for (label, text, expected) in cases {
        let verdicts = text_verdicts(&text);

        assert!(
            refuses(&verdicts, Some("user"), expected),
            "{label}: {verdicts:?}",
        );
    }
}

/// A quality parameter past the ones its declaration leaves is bound by nothing: no assignment the declaration is judged at reaches it.
#[test]
fn a_sort_past_the_ones_a_declaration_leaves_is_refused() {
    let verdicts = text_verdicts(
        r#"
#{universe_context = {quality_count = 1}}
let /past: (A: #{quality = Param(1)} Sort) -> #{quality = Param(1)} Sort =
    (A: #{quality = Param(1)} Sort) => A;
"#,
    );

    assert!(
        refuses(&verdicts, Some("past"), |error| matches!(
            error,
            Error::OpenSort(_)
        )),
        "{verdicts:?}",
    );
}

/// A family over a sort its caller states is one declaration, held at a type and at a proposition: its telescopes are typed at each assignment, and what is decided of the declaration as data — its polarities, its variances — reads no sort.
#[test]
fn a_family_over_a_sort_holds_data_and_proofs() {
    let verdicts = text_verdicts(&fill!(
        r#"
#{universe_context = {quality_count = 1}}
induct /Box(A: Sort): pub Type
| box(a: A)
end
${HELD}
let /a_number: #{node = InductType, universes = {qualities = [Type]}} /Box(#NatType) =
    #{node = Variant, universes = {qualities = [Type]}} /Box/box(@#NatType, 0);

let /a_proof:
    #{node = InductType, universes = {qualities = [Prop]}}
    /Box(#{node = InductType} /Held()) =
    #{node = Variant, universes = {qualities = [Prop]}}
    /Box/box(@#{node = InductType} /Held(), #{node = Variant} /Held/qed());
"#,
        HELD
    ));

    assert!(verdicts.is_empty(), "{verdicts:?}");
}

/// A family's own sort is one sort whatever a use states: the rules that tell a proof from data by a declaration read the sort it declares, as written.
#[test]
fn a_family_whose_own_sort_is_left_to_its_caller_is_refused() {
    let verdicts = text_verdicts(
        r#"
#{universe_context = {quality_count = 1}}
induct /Box(A: Sort): pub (Sort)
| box(a: A)
end
"#,
    );

    assert!(
        refuses(&verdicts, Some("Box"), |error| matches!(
            error,
            Error::OpenSort(_)
        )),
        "{verdicts:?}",
    );
}

/// Two instances of one family apart in a sort are two types, whatever else they share: nothing of `Tagged` mentions the sort it leaves, and its instance at `Prop` is still no instance at `Type`. `curios`'s `tests::rules::subsumption_tests` puts two such instances to the elaborator under this name. Rename both or neither.
#[test]
fn two_instances_of_a_family_apart_in_a_sort_are_two_types() {
    let coercion = |to: &str| {
        text_verdicts(&fill!(
            r#"
#{universe_context = {quality_count = 1}}
induct /Tagged: pub Type
| tag()
end

let /coerce: (x: #{node = InductType, universes = {qualities = [Prop]}} /Tagged()) ->
    #{node = InductType, universes = {qualities = [${to}]}}
    /Tagged() =
    (x: #{node = InductType, universes = {qualities = [Prop]}} /Tagged()) => x;
"#,
            to
        ))
    };

    let apart = coercion("Type");
    assert!(
        refuses(&apart, Some("coerce"), |error| matches!(
            error,
            Error::Mismatch { .. }
        )),
        "{apart:?}",
    );
    let same = coercion("Prop");
    assert!(same.is_empty(), "{same:?}");
}

/// A top-level group leaves a sort to its caller as a definition does: judged at every assignment, defined as stated, and total under its own spelling where it is at each.
#[test]
fn a_recursive_group_is_used_at_a_type_and_at_a_proposition() {
    let node = "node = Rec, ";
    let verdicts = text_verdicts(&fill!(
        "${HELD}${USED_AT_BOTH}",
        HELD,
        USED_AT_BOTH = fill!(USED_AT_BOTH, node)
    ));

    assert!(verdicts.is_empty(), "{verdicts:?}");
}

/// A declaration is judged at two to the count of the sorts it leaves, so the count is bounded.
#[test]
fn a_declaration_leaves_no_more_sorts_than_the_limit() {
    let stated = UniverseContext::QUALITY_LIMIT + 1;
    let verdicts = text_verdicts(&fill!(
        "#{universe_context = {quality_count = ${stated}}}\nlet /many: #NatType = 0;",
        stated
    ));

    assert!(
        refuses(&verdicts, Some("many"), |error| matches!(
            error,
            Error::TooManySorts { stated: found } if *found == stated
        )),
        "{verdicts:?}",
    );
}

/// A definition generic in a sort stands at `Type` at one use, where matching a proposition of two constructors into it would tell its proofs apart: judged at `Prop` the match is a proof's, and at `Type` it is the large elimination the guard refuses, so the definition is refused at that assignment. `curios`'s `tests::rules::subsumption_tests` puts the same proposition to the elaborator under this name. Rename both or neither.
#[test]
fn a_proposition_is_not_matched_into_a_sort_left_to_a_caller() {
    let verdicts = text_verdicts(
        r#"
induct /Two: pub Prop
| a()
| b()
end

#{universe_context = {quality_count = 1}}
let /pick: (A: Sort, t: #{node = InductType} /Two(), x: A, y: A) -> A =
    (A: Sort, t: #{node = InductType} /Two(), x: A, y: A) =>
        match t: (s) => A | a() => x | b() => y end;
"#,
    );

    assert!(
        verdicts.iter().any(|verdict| matches!(
            at_sorts(verdict),
            Some((sorts, Error::LargeElimination(_))) if sorts == [Quality::Type]
        )),
        "{verdicts:?}",
    );
}

/// A proposition carries only proofs at every assignment of the sorts it leaves: `struct W(A: Sort): Prop { a: A }` holds a proof at `Prop` and a value at `Type`, where any two of its values would be one with their fields apart, so it is refused at that assignment. `curios`'s `tests::rules::subsumption_tests` puts the same structure to the elaborator under this name. Rename both or neither.
#[test]
fn a_proposition_holds_no_field_at_a_sort_left_to_its_caller() {
    let verdicts = text_verdicts(
        r#"
#{universe_context = {quality_count = 1}}
struct /W(A: Sort): pub Prop {
    a: A,
}
"#,
    );

    assert!(
        verdicts.iter().any(|verdict| matches!(
            at_sorts(verdict),
            Some((sorts, Error::Informative { .. })) if sorts == [Quality::Type]
        )),
        "{verdicts:?}",
    );
}

/// A value at a sort left to a caller is a proof at the use that states `Prop`, so a recursion answering one owes a proof's descent: `rec spin(A: Sort, n: Nat) -> A = spin(A, n)` is a proof's recursion at `Prop`, where it does not descend, and is refused at that assignment alone.
#[test]
fn a_recursion_into_a_sort_left_to_its_caller_descends_as_a_proof_must() {
    let verdicts = text_verdicts(
        r#"
#{node = Rec, universe_context = {quality_count = 1}}
let /spin: (A: Sort, n: #NatType) -> A =
    (A: Sort, n: #NatType) => (#{universes = {qualities = [Param(0)]}} /spin)(A, n);
"#,
    );

    assert!(
        verdicts.iter().any(|verdict| matches!(
            at_sorts(verdict),
            Some((sorts, Error::NotDescending { .. })) if sorts == [Quality::Prop]
        )),
        "{verdicts:?}",
    );
}

/// A recursive group's members state no sort its context does not leave: `rec id(A: Sort, a: A) -> A` under a context leaving none is refused, its members being read as definitions under the group's own context.
#[test]
fn a_group_stating_a_sort_its_context_does_not_leave_is_refused() {
    let verdicts = text_verdicts(
        r#"
#{node = Rec}
let /id: (A: Sort, a: A) -> A =
    (A: Sort, a: A) => a;
"#,
    );

    assert!(
        refuses(&verdicts, Some("id"), |error| matches!(
            error,
            Error::OpenSort(_)
        )),
        "{verdicts:?}",
    );
}

/// The sort the walk states a definition's value inhabits is the one every assignment of the sorts its declaration leaves reaches, whatever the module says: `T : Type = Nat`, leaving one sort, is stated `Type 0`, though the module says a sort past those it leaves.
#[test]
fn a_value_sort_is_stated_as_every_assignment_reaches_it() {
    let rechecked = text_certified(
        r#"
#{universe_context = {quality_count = 1}, value_sort = #{quality = Param(1)} Sort}
let /T: Type =
    #NatType;
"#,
    );

    assert_eq!(rechecked.verdicts, Vec::new());
    let Item::Let(definition) = &rechecked.definitions.items[0] else {
        panic!("the module holds one definition");
    };
    assert_eq!(definition.value_sort, Some(term("Type")));
}

/// A recursive occurrence that only a sort left to a caller would uncover: at `Prop` the match on `h` is its arm, an equation between two proofs being one, and the arm holds `Bad` left of an arrow. The entry is decided as it is stated, where the match stands, and a match the walk cannot see through is read at `Mixed`, so the occurrence is refused whatever a use states. `curios`'s `tests::rules::positivity_computed_type_tests` puts the same declaration to the elaborator under this name. Rename both or neither.
#[test]
fn an_occurrence_a_sort_left_to_a_caller_would_uncover_is_refused() {
    let verdicts = text_verdicts(
        r#"
#{universe_context = {quality_count = 1}}
induct /Eq(A: Sort): (x: A, y: A) -> pub Prop
| refl(@z: A): (z, z)
end

induct /False: pub Prop
end

#{universe_context = {quality_count = 1}}
induct /Bad(
    A: Sort,
    x: A,
    y: A,
    h: #{node = InductType, universes = {qualities = [Param(0)]}} /Eq(A)(x, y),
): pub Type
| c(
        f:
            #{cases = {stated = #{node = InductType, universes = {qualities = [Param(0)]}} /Eq(A)(x, y)}}
            match h: (x2, x3, x4) => Prop
            | refl(@z) =>
                (
                    b:
                        #{node = InductType, universes = {qualities = [Param(0)]}}
                        /Bad(A, x, y, h)) -> #{node = InductType} /False()
            end)
end
"#,
    );

    assert!(
        refuses(&verdicts, Some("Bad"), |error| matches!(
            error,
            Error::NotPositive { .. }
        )),
        "{verdicts:?}",
    );
}

/// A level carried only by a match that stands is held: `induct Lv(@A: Sort, x: A, y: A, h: Eq(x, y)): Type u | mk(f: match h : (_, _, _) => Type u | refl(@z) => Nat end) end` holds `u` in its motive alone, where conversion compares two such matches, since at `Type` a match on `h` stands whatever `x` and `y` are. So two instances of `Lv` at `Type` apart in `u` are two types, and a value of one is no value of the other.
#[test]
fn a_level_a_standing_match_carries_is_held_by_its_family() {
    let verdicts = text_verdicts(
        r#"
#{universe_context = {quality_count = 1}}
induct /Eq(A: Sort): (x: A, y: A) -> pub Prop
| refl(@z: A): (z, z)
end

#{universe_context = {parameter_count = 1, quality_count = 1}}
induct /Lv(
    A: Sort,
    x: A,
    y: A,
    h: #{node = InductType, universes = {qualities = [Param(0)]}} /Eq(A)(x, y),
): pub #{level = u} Type
| mk(
        f:
            #{cases = {stated = #{node = InductType, universes = {qualities = [Param(0)]}} /Eq(A)(x, y)}}
            match h: (x2, x3, x4) => #{level = u} Type | refl(@z) => #NatType end)
end

let /coerce: (
    A: Type,
    x: A,
    y: A,
    h: #{node = InductType, universes = {qualities = [Type]}} /Eq(A)(x, y),
    v: #{node = InductType, universes = {levels = [0], qualities = [Type]}} /Lv(A, x, y, h)) ->
    #{node = InductType, universes = {levels = [1], qualities = [Type]}}
    /Lv(A, x, y, h) =
    (
        A: Type,
        x: A,
        y: A,
        h: #{node = InductType, universes = {qualities = [Type]}} /Eq(A)(x, y),
        v: #{node = InductType, universes = {levels = [0], qualities = [Type]}} /Lv(A, x, y, h)) =>
        v;
"#,
    );

    assert!(
        !verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("Lv"))),
        "the family is accepted: {verdicts:?}",
    );
    assert!(
        refuses(&verdicts, Some("coerce"), |error| matches!(
            error,
            Error::Mismatch { .. }
        )),
        "{verdicts:?}",
    );
}

/// `/Box`'s payload stands at a sort its declaration leaves to its caller, and the term this closes with is `qed` at `Holds` of the payload of `box(false)` at `Bool`, read by a closed match.
const A_PAYLOAD_READ_AT_TYPE: &str = r#"
#{universe_context = {quality_count = 1}}
induct /Box(A: Sort): pub Type
| box(x: A)
end
((h: /std/Bool/Holds(
    #{result = Ambient(#BoolType)}
    match (
        #{universes = {qualities = [Type]}}
        /Box/box)(@#BoolType, false)
    | box(x) =>
        x
    end
)) => h)(
    #{node = Variant}
    /std/Bool/True/qed()
)
"#;

// A payload at a sort its family leaves to its caller is a proof at `Prop` and data at `Type`, and what its constructor states of it is stated once, of the declaration as written, and read at every assignment. At `Type` the closed match reads the payload `false` as data, so the closing term stands at `Holds(false)`, which is `False`, and `qed` is refused there: a payload bound at `Type` by what holds at `Prop`, as a proof no judgment reads, is still the value the match hands on. No mutation is claimed: it pins the reduct a closed computation reaches through such a payload, the verdict nearest the mark `certifier::stating::tests::a_binder_at_a_sort_left_to_the_caller_binds_no_proof` holds.
#[test]
fn a_payload_at_a_sort_left_to_the_caller_is_read_as_data_at_type() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_PAYLOAD_READ_AT_TYPE}",
        PROPOSITIONS,
        A_PAYLOAD_READ_AT_TYPE
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
