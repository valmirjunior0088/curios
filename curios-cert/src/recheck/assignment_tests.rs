//! A declaration that leaves a sort to its caller, attacked where a use reads it at an assignment: each module written as Core text, put to the kernel with nothing in scope and declaring the propositions it uses, closing on `False` were the refusal it meets not made, or holding a sort no assignment states.

use {
    super::test_support::*,
    crate::*,
    curios_common::fill,
    curios_core::{Atom, Quality},
};

/// `/same` holds any two values of a sort its caller states one, which irrelevance makes true where that sort is `Prop` and nothing makes true where it is `Type`; used at `Type`, it carries `Holds(true)` to `Holds(false)`.
const TWO_VALUES_ONE_BY_A_DEFINITION: &str = r#"
#{universe_context = {quality_count = 1}}
let /same : (@A: Sort, x: A, y: A, G: (z: A) -> Prop, g: G(x)) -> G(y) =
    (@A: Sort, x: A, y: A, G: (z: A) -> Prop, g: G(x)) => g;
(
    #{universes = {qualities = [Type]}}
    /same)(
    @#BoolType,
    true,
    false,
    (b: #BoolType) => /std/Bool/Holds(b),
    /std/Bool/True/qed()
)
"#;

/// `/same` again, as a top-level group.
const TWO_VALUES_ONE_BY_A_GROUP: &str = r#"
#{universe_context = {quality_count = 1}, node = Rec}
let /same : (@A: Sort, x: A, y: A, G: (z: A) -> Prop, g: G(x)) -> G(y) =
    (@A: Sort, x: A, y: A, G: (z: A) -> Prop, g: G(x)) => g;
(
    #{universes = {qualities = [Type]}}
    /same)(
    @#BoolType,
    true,
    false,
    (b: #BoolType) => /std/Bool/Holds(b),
    /std/Bool/True/qed()
)
"#;

/// `/Through`'s one payload is typed by `/only`, which takes a proposition, so the entry is well typed where the sort its caller states is `Prop` and at no other. At `Type`, `/only(Bool)` is a proposition by what `/only` declares and `Bool` by what it unfolds to: `through(true)` and `through(false)` are one by irrelevance, `/get` reads them apart, and the closing term carries `Holds(get(through(true)))` to `Holds(get(through(false)))`.
const AN_ENTRY_WELL_TYPED_AT_PROP_ALONE: &str = r#"
let /only : (X: Prop) -> Prop = (X: Prop) => X;
#{universe_context = {quality_count = 1}}
induct /Through(A: Sort): pub Type
| through(x: /only(A))
end
let /get : (t: #{node = InductType, universes = {qualities = [Type]}} /Through(#BoolType)) -> #BoolType =
    (t: #{node = InductType, universes = {qualities = [Type]}} /Through(#BoolType)) =>
        #{result = Ambient(#BoolType)}
        match t
        | through(x) => x
        end;
((h: /std/Bool/Holds(
    /get(
        (
            #{universes = {qualities = [Type]}}
            /Through/through)(@#BoolType, false)
    )
)) => h)(
    ((k: /std/Bool/Holds(
        /get(
            (
                #{universes = {qualities = [Type]}}
                /Through/through)(@#BoolType, true)
        )
    )) => k)(
        /std/Bool/True/qed()
    )
)
"#;

/// `/S` holds a Church boolean over a sort its caller states, and its level `u` sizes only its parameter, so its vector calls `u` irrelevant: at `Prop` the field is a proof and `/S` has one inhabitant, at `Type` it has two, `/tt` and `/ff`. `/up` and `/down` take an `/S` at one sort for one at the other, at another level, apart where the vector says nothing reads it, and `/same` holds any two values at `Prop` one by the shape: together they carry `Holds(read(tt))` to `Holds(read(ff))`.
const TWO_INSTANCES_APART_IN_A_SORT: &str = r#"
#{universe_context = {parameter_count = 1, quality_count = 1}}
struct /S(X: #{level = u} Type): pub #{level = u + 1} Type { f: (A: Sort, t: A, e: A) -> A }
let /tt : #{node = StructType, universes = {levels = [0], qualities = [Type]}} /S(#BoolType) =
    #{universes = {levels = [0], qualities = [Type]}}
    /S(#BoolType) { (A: Type, t: A, e: A) => t };
let /ff : #{node = StructType, universes = {levels = [0], qualities = [Type]}} /S(#BoolType) =
    #{universes = {levels = [0], qualities = [Type]}}
    /S(#BoolType) { (A: Type, t: A, e: A) => e };
let /read : (s: #{node = StructType, universes = {levels = [0], qualities = [Type]}} /S(#BoolType)) -> #BoolType =
    (s: #{node = StructType, universes = {levels = [0], qualities = [Type]}} /S(#BoolType)) =>
        (#{field = Struct(/S, 0)} (s).0)(#BoolType, true, false);
let /up : (s: #{node = StructType, universes = {levels = [0], qualities = [Type]}} /S(#BoolType)) -> #{node = StructType, universes = {levels = [1], qualities = [Prop]}} /S(#BoolType) =
    (s: #{node = StructType, universes = {levels = [0], qualities = [Type]}} /S(#BoolType)) => s;
let /same : (
    p: #{node = StructType, universes = {levels = [1], qualities = [Prop]}} /S(#BoolType),
    q: #{node = StructType, universes = {levels = [1], qualities = [Prop]}} /S(#BoolType),
    G: (r: #{node = StructType, universes = {levels = [1], qualities = [Prop]}} /S(#BoolType)) -> Prop,
    g: G(p)
) -> G(q) =
    (
        p: #{node = StructType, universes = {levels = [1], qualities = [Prop]}} /S(#BoolType),
        q: #{node = StructType, universes = {levels = [1], qualities = [Prop]}} /S(#BoolType),
        G: (r: #{node = StructType, universes = {levels = [1], qualities = [Prop]}} /S(#BoolType)) -> Prop,
        g: G(p)
    ) => g;
let /down : (p: #{node = StructType, universes = {levels = [1], qualities = [Prop]}} /S(#BoolType)) -> #{node = StructType, universes = {levels = [0], qualities = [Type]}} /S(#BoolType) =
    (p: #{node = StructType, universes = {levels = [1], qualities = [Prop]}} /S(#BoolType)) => p;
/same(
    /up(/tt),
    /up(/ff),
    (r: #{node = StructType, universes = {levels = [1], qualities = [Prop]}} /S(#BoolType)) => /std/Bool/Holds(/read(/down(r))),
    /std/Bool/True/qed()
)
"#;

/// `/Is(c)(x)` is inhabited where `x` is `c`: at `Prop` over `/Two`, `a()` and `b()` are one proof, so `/Is(a())(b())` holds `yes`. Inversion that read the instance's index as data would find `yes`'s target `a()` apart from `b()` and excuse its arm, and `/bad`'s empty match would take the `yes` the closing term hands it to `False`.
const TWO_PROOFS_AT_AN_INDEX_OF_A_SORT_LEFT_TO_A_CALLER: &str = r#"
induct /Two: pub Prop
| a()
| b()
end
#{universe_context = {quality_count = 1}}
induct /Is(@A: Sort, c: A): (x: A) -> pub Type
| yes(): (c)
end
let /bad : (
    w:
        #{node = InductType, universes = {qualities = [Prop]}}
        /Is(
            #{node = InductType}
            /Two(),
            #{node = Variant}
            /Two/a()
        )(
            #{node = Variant}
            /Two/b()
        )
) ->
    #{node = InductType}
    /std/Bool/False() =
    (
        w:
            #{node = InductType, universes = {qualities = [Prop]}}
            /Is(
                #{node = InductType}
                /Two(),
                #{node = Variant}
                /Two/a()
            )(
                #{node = Variant}
                /Two/b()
            )
    ) =>
        #{result = Ambient(#{node = InductType} /std/Bool/False())}
        match w
        end;
/bad(
    (
        #{universes = {qualities = [Prop]}}
        /Is/yes)(
        @#{node = InductType} /Two(),
        @#{node = Variant} /Two/a()
    )
)
"#;

/// `/Box` leaves one sort, and each definition writes a nominal node of it that states none, or two: read at such an instance the entry keeps its own `Param(0)`, or reads a sort past the one it leaves.
const NOMINAL_NODES_SHORT_OF_A_SORT_OR_PAST_ONE: &str = r#"
#{universe_context = {quality_count = 1}}
induct /Box(A: Sort): pub Type
| box(x: A)
end
let /a_type_stating_none : #{level = 1} Type =
    #{node = InductType, universes = {qualities = []}}
    /Box(#BoolType);
let /a_value_stating_none : #{node = InductType, universes = {qualities = [Type]}} /Box(#BoolType) =
    #{node = Variant, universes = {qualities = []}}
    /Box/box(@#BoolType, false);
let /a_value_stating_two : #{node = InductType, universes = {qualities = [Type]}} /Box(#BoolType) =
    #{node = Variant, universes = {qualities = [Type, Prop]}}
    /Box/box(@#BoolType, false);
/std/Bool/True/qed()
"#;

/// Each declaration leaves one sort, `Param(0)`, and states `Param(1)`, which nothing binds, at one route: a group's member, a `let`'s annotation, a record's field, the instance a match states, a motive, and an occurrence's instance. Read at every assignment of the one sort it leaves, each would still hold `Param(1)`, judged at a sort that is neither `Prop` nor `Type`.
const A_SORT_NO_DECLARATION_BINDS: &str = r#"
#{universe_context = {quality_count = 1}}
induct /Same(@A: Sort): (x: A, y: A) -> pub Prop
| refl(@z: A): (z, z)
end
#{universe_context = {quality_count = 1}}
let /id : (@A: Sort, a: A) -> A = (@A: Sort, a: A) => a;
#{universe_context = {quality_count = 1}, node = Rec}
let /in_a_member : (@A: Sort, n: #NatType) -> #NatType =
    (@A: Sort, n: #NatType) => ((X: #{quality = Param(1)} Sort) => n)(#BoolType);
#{universe_context = {quality_count = 1}}
let /in_a_let : (@A: Sort, n: #NatType) -> #NatType =
    (@A: Sort, n: #NatType) =>
        let X: #{level = 1, quality = Param(1)} Sort = #BoolType;
        n;
#{universe_context = {quality_count = 1}}
struct /InAField(A: Sort): pub #{level = 1} Type { f: #{quality = Param(1)} Sort }
#{universe_context = {quality_count = 1}}
let /in_a_stated_instance : (@A: Sort, x: A, y: A, h: #{node = InductType, universes = {qualities = [Param(0)]}} /Same(A)(x, y)) -> #NatType =
    (@A: Sort, x: A, y: A, h: #{node = InductType, universes = {qualities = [Param(0)]}} /Same(A)(x, y)) =>
        #{result = Ambient(#NatType), cases = {stated = #{node = InductType, universes = {qualities = [Param(1)]}} /Same(A)(x, y)}}
        match h
        | refl(@z) => 0
        end;
#{universe_context = {quality_count = 1}}
let /in_a_motive : (@A: Sort, x: A, y: A, h: #{node = InductType, universes = {qualities = [Param(0)]}} /Same(A)(x, y)) -> Prop =
    (@A: Sort, x: A, y: A, h: #{node = InductType, universes = {qualities = [Param(0)]}} /Same(A)(x, y)) =>
        match h: (x2, y2, h2) => #{level = 1, quality = Param(1)} Sort
        | refl(@z) =>
            #{node = InductType}
            /std/Bool/True()
        end;
#{universe_context = {quality_count = 1}}
let /in_an_instance : (@A: Sort, a: A) -> A =
    (@A: Sort, a: A) => (#{universes = {qualities = [Param(1)]}} /id)(@A, a);
/std/Bool/True/qed()
"#;

/// A group standing in a term of a declaration that leaves a sort binds none of its own: `loop` answers `A`, a proof where the caller states `Prop`, and it does not descend; used at `Prop` at `False`, `/spin` is a proof of `False` by a recursion that never ends.
const A_GROUP_INSIDE_A_TERM: &str = r#"
#{universe_context = {quality_count = 1}}
let /spin : (@A: Sort, n: #NatType) -> A =
    (@A: Sort, n: #NatType) =>
        #{node = Rec}
        (let loop: (k: #NatType) -> A = (k: #NatType) => loop(k);
        loop(n));
(
    #{universes = {qualities = [Prop]}}
    /spin)(@#{node = InductType} /std/Bool/False(), 0)
"#;

/// `/F`'s parameter `B` stands behind a match on `h`, an equation between two values of a sort its caller states: as stated the match stands, and where the caller states `Prop` it is its arm, which holds `B` left of an arrow. `/G` holds an `/F` at `Prop` with itself at `B`, and the closing term is Curry's paradox through it.
const A_PARAMETER_BEHIND_A_MATCH: &str = r#"
#{universe_context = {quality_count = 1}}
induct /Same(@A: Sort): (x: A, y: A) -> pub Prop
| refl(@z: A): (z, z)
end
#{universe_context = {quality_count = 1}}
induct /F(
    @A: Sort,
    x: A,
    y: A,
    h:
        #{node = InductType, universes = {qualities = [Param(0)]}}
        /Same(A)(x, y),
    B: Type
): pub Type
| mk(
    f:
        #{cases = {stated = #{node = InductType, universes = {qualities = [Param(0)]}} /Same(A)(x, y)}}
        match h: (x2, y2, h2) => Prop
        | refl(@z) =>
            (b: B) ->
                #{node = InductType}
                /std/Bool/False()
        end
)
end
induct /G: pub Type
| g(
    w:
        #{node = InductType, universes = {qualities = [Prop]}}
        /F(
            #{node = InductType}
            /std/Bool/True(),
            /std/Bool/True/qed(),
            /std/Bool/True/qed(),
            (
                #{universes = {qualities = [Prop]}}
                /Same/refl)(@#{node = InductType} /std/Bool/True(), @/std/Bool/True/qed()),
            #{node = InductType}
            /G()
        )
)
end
let /neg : (
    x:
        #{node = InductType}
        /G()
) ->
    #{node = InductType}
    /std/Bool/False() =
    (
        x:
            #{node = InductType}
            /G()
    ) =>
        #{result = Ambient(#{node = InductType} /std/Bool/False())}
        match x
        | g(w) =>
            #{result = Ambient(#{node = InductType} /std/Bool/False())}
            match w
            | mk(f) => f(x)
            end
        end;
/neg(
    (
        #{universes = {qualities = []}}
        /G/g)(
        (
            #{universes = {qualities = [Prop]}}
            /F/mk)(
            @#{node = InductType} /std/Bool/True(),
            @/std/Bool/True/qed(),
            @/std/Bool/True/qed(),
            @(
                #{universes = {qualities = [Prop]}}
                /Same/refl)(@#{node = InductType} /std/Bool/True(), @/std/Bool/True/qed()),
            @#{node = InductType} /G(),
            /neg
        )
    )
)
"#;

/// `/Lv`'s level `u` stands only in the arm of a match on `h`, an equation between two values of a sort its caller states, and its motive sits a level above it: where the caller states `Prop` the match is its arm, so `u` sizes the payload, and as stated the match stands holding it. `/up` takes an `/Lv` at `Prop` at `0` for one at `1`.
const A_LEVEL_IN_THE_ARM_OF_A_MATCH: &str = r#"
#{universe_context = {quality_count = 1}}
induct /Same(@A: Sort): (x: A, y: A) -> pub Prop
| refl(@z: A): (z, z)
end
#{universe_context = {parameter_count = 1, quality_count = 1}}
induct /Lv(
    @A: Sort,
    x: A,
    y: A,
    h:
        #{node = InductType, universes = {qualities = [Param(0)]}}
        /Same(A)(x, y)
): pub #{level = u + 2} Type
| mk(
    f:
        #{cases = {stated = #{node = InductType, universes = {qualities = [Param(0)]}} /Same(A)(x, y)}}
        match h: (x2, y2, h2) => #{level = u + 2} Type
        | refl(@z) =>
            #{level = u} Type
        end
)
end
let /up : (
    v:
        #{node = InductType, universes = {levels = [0], qualities = [Prop]}}
        /Lv(
            #{node = InductType}
            /std/Bool/True(),
            /std/Bool/True/qed(),
            /std/Bool/True/qed(),
            (
                #{universes = {qualities = [Prop]}}
                /Same/refl)(@#{node = InductType} /std/Bool/True(), @/std/Bool/True/qed())
        )
) ->
    #{node = InductType, universes = {levels = [1], qualities = [Prop]}}
    /Lv(
        #{node = InductType}
        /std/Bool/True(),
        /std/Bool/True/qed(),
        /std/Bool/True/qed(),
        (
            #{universes = {qualities = [Prop]}}
            /Same/refl)(@#{node = InductType} /std/Bool/True(), @/std/Bool/True/qed())
    ) =
    (
        v:
            #{node = InductType, universes = {levels = [0], qualities = [Prop]}}
            /Lv(
                #{node = InductType}
                /std/Bool/True(),
                /std/Bool/True/qed(),
                /std/Bool/True/qed(),
                (
                    #{universes = {qualities = [Prop]}}
                    /Same/refl)(@#{node = InductType} /std/Bool/True(), @/std/Bool/True/qed())
            )
    ) => v;
/std/Bool/True/qed()
"#;

/// The verdicts on `text` put after the propositions it names.
fn verdicts(text: &str) -> Vec<Verdict> {
    core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${TEXT}",
        PROPOSITIONS,
        TEXT = text
    ))
}

/// Whether some verdict refuses `name` at the one assignment `sort`, by a refusal `refusal` holds of.
fn refused_at(
    verdicts: &[Verdict],
    name: &str,
    sort: Quality,
    refusal: fn(&Error) -> bool,
) -> bool {
    verdicts.iter().any(|verdict| {
        verdict.name == Some(global(name))
            && matches!(
                &verdict.error,
                Error::AtSorts { sorts, refusal: at } if sorts[..] == [sort] && refusal(at)
            )
    })
}

/// A definition and a top-level group that hold two values of a sort left to their caller one are true at `Prop` alone, and refused at `Type`, where the closing term would carry `Holds(true)` to `Holds(false)`. `curios`'s `tests::rules::subsumption_tests` puts the definition to the elaborator under this name. Rename both or neither.
///
/// Mutation-check: cut `at_every_sort` to its first assignment, the one stating `Prop`, and each module is admitted at `False`.
#[test]
fn two_values_of_a_sort_left_to_a_caller_are_not_one() {
    for (rule, text) in [
        ("a definition", TWO_VALUES_ONE_BY_A_DEFINITION),
        ("a recursive group", TWO_VALUES_ONE_BY_A_GROUP),
    ] {
        let verdicts = verdicts(text);

        assert!(
            refused_at(&verdicts, "same", Quality::Type, |error| matches!(
                error,
                Error::Mismatch { .. }
            )),
            "{rule}: {verdicts:?}"
        );
    }
}

/// A registry entry typed by what irrelevance grants is judged at the assignment that grants nothing, as a definition is. Mutation-check: cut `at_every_sort` to its first assignment, the one stating `Prop`, and the module is admitted at `False`.
#[test]
fn an_entry_well_typed_at_prop_alone_is_refused_at_type() {
    let verdicts = verdicts(AN_ENTRY_WELL_TYPED_AT_PROP_ALONE);

    assert!(
        refused_at(&verdicts, "Through", Quality::Type, |error| matches!(
            error,
            Error::Mismatch { .. }
        )),
        "{verdicts:?}"
    );
}

/// A family's vector speaks of its levels and never of its sorts: two instances apart in a sort are two types where the levels they are also apart in are irrelevant. The route to `False` stands but for `/up` and `/down`.
///
/// Mutation-check: drop the comparison of sorts from `Kernel::instances_eq`, and the module is admitted at `False`.
#[test]
fn a_sort_is_compared_whatever_a_familys_vector_says_of_its_levels() {
    let verdicts = verdicts(TWO_INSTANCES_APART_IN_A_SORT);

    assert!(
        matches!(
            &verdicts[..],
            [
                Verdict { name: Some(up), error: Error::Mismatch { .. } },
                Verdict { name: Some(down), error: Error::Mismatch { .. } },
            ] if *up == global("up") && *down == global("down")
        ),
        "{verdicts:?}"
    );
}

/// Inversion reads the instance it is handed: an index at a sort its caller states `Prop` holds proofs, which no tag tells apart, so `yes` stays mandatory. The route to `False` stands but for `/bad`.
///
/// Mutation-check: let `curios-analysis`'s `Unifier::unify_node` take two variants of a `Prop`-sorted family apart by their tags, and the module is admitted at `False`.
#[test]
fn no_case_is_impossible_between_two_proofs_a_sort_left_to_a_caller_indexes() {
    let verdicts = verdicts(TWO_PROOFS_AT_AN_INDEX_OF_A_SORT_LEFT_TO_A_CALLER);

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(bad), error: Error::MissingArm { family, tag } }]
                if *bad == global("bad") && *family == global("Is") && *tag == Atom::from("yes")
        ),
        "{verdicts:?}"
    );
}

/// A nominal node states as many sorts as its family leaves, a family's type and a constructor's value alike, as `sort_tests::an_occurrence_states_every_sort_its_declaration_leaves` holds an occurrence of a definition to.
#[test]
fn a_nominal_node_states_every_sort_its_family_leaves() {
    let verdicts = verdicts(NOMINAL_NODES_SHORT_OF_A_SORT_OR_PAST_ONE);

    for (name, actual) in [
        ("a_type_stating_none", 0),
        ("a_value_stating_none", 0),
        ("a_value_stating_two", 2),
    ] {
        assert!(
            verdicts
                .iter()
                .any(|verdict| verdict.name == Some(global(name))
                    && matches!(
                        verdict.error,
                        Error::Arity {
                            counted: Counted::Sorts,
                            expected: 1,
                            actual: found,
                        } if found == actual
                    )),
            "{name}: {verdicts:?}"
        );
    }
}

/// The scan made before any judgment sees a sort at every route a declaration can state one: each is refused under its own name, by the scan's `OpenSort` and not only at an assignment.
///
/// Mutation-check: drop `open_sort` from `universe_residue`, and each declaration is refused only at its assignments, under `AtSorts`.
#[test]
fn a_sort_no_declaration_binds_is_refused_at_every_route_before_any_judgment() {
    let verdicts = verdicts(A_SORT_NO_DECLARATION_BINDS);

    for name in [
        "in_a_member",
        "in_a_let",
        "InAField",
        "in_a_stated_instance",
        "in_a_motive",
        "in_an_instance",
    ] {
        assert!(
            verdicts
                .iter()
                .any(|verdict| verdict.name == Some(global(name))
                    && matches!(verdict.error, Error::OpenSort(_))),
            "{name}: {verdicts:?}"
        );
    }
}

/// A group inside a term binds no sort of its own, and is read at each assignment with the declaration that holds it: at `Prop` its member is a proof's recursion, which owes the descent it does not make. `curios`'s `tests::rules::totality_partiality_tests` puts the same declaration to both checkers under this name. Rename both or neither.
///
/// Mutation-check: cut `at_every_sort` to its last assignment, the one stating `Type`, and `/spin` is refused at no assignment; obligation (V) still refuses the closing term, which reaches a definition enclosing a group that does not descend.
#[test]
fn a_group_inside_a_term_is_judged_at_every_assignment_of_its_declarations_sorts() {
    let verdicts = verdicts(A_GROUP_INSIDE_A_TERM);

    assert!(
        refused_at(&verdicts, "spin", Quality::Prop, |error| matches!(
            error,
            Error::NotDescending { .. }
        )),
        "{verdicts:?}"
    );
}

/// A parameter's polarity is decided on the entry as stated, where a match on an equation at a sort left to the caller stands and is read at `Mixed`, so a family that holds its parameter left of an arrow at `Prop` alone is never composed through as positive in it. The route to `False` stands but for `/G`. `curios`'s `tests::rules::positivity_computed_type_tests` puts the same declarations to the elaborator under this name. Rename both or neither.
///
/// Mutation-check: have positivity's `Walk::opaque` visit no child of a match, and `/F`'s vector reads `B` unused, `/G` is accepted, and the module is admitted at `False`.
#[test]
fn a_parameter_a_sort_left_to_a_caller_would_uncover_is_read_at_mixed() {
    let verdicts = verdicts(A_PARAMETER_BEHIND_A_MATCH);

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(g), error: Error::NotPositive { name, .. } }]
                if *g == global("G") && *name == global("G")
        ),
        "{verdicts:?}"
    );
}

/// A level only the arm of a match on an equation at a sort left to the caller holds is a level of the family: where the caller states `Prop` the arm is what the match is, and as stated the match stands holding it, so the variance walk reads every part of it. `sort_tests::a_level_a_standing_match_carries_is_held_by_its_family` holds a level the motive carries at `Type`.
///
/// Mutation-check: have the variance walk read a match as mentioning no level, and `/up` is accepted.
#[test]
fn a_level_the_arm_of_a_standing_match_holds_is_held_by_its_family() {
    let verdicts = verdicts(A_LEVEL_IN_THE_ARM_OF_A_MATCH);

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("up"))
                && matches!(verdict.error, Error::Mismatch { .. })),
        "{verdicts:?}"
    );
}
