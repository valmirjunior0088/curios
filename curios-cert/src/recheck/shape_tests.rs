//! What conversion equates by a type's shape alone — eta, one inhabitant, a proposition's proofs — held where the shape leaves more than one value: a hidden member, a shape or a sort an arm, a `let` or a caller decides, and a proof's payload, each written as Core text and put to the certifier with nothing in scope, declaring the propositions it uses, and each closing on `False` were the refusal it meets not made.

use {super::test_support::*, crate::*, curios_common::fill, curios_core::Quality};

/// `S`'s one relevant field, `k`, is hidden, and its plain field is a unit, so a reading of the shape that skipped what an author does not write would give `S` one inhabitant. `cast` puts `S { 1, () }` against the neutral `s` where the goal types them at `S`, so the literal's eta projects `s` field by field, `k` among them; at `G(s) = Holds(s.k == 1)` the term the module closes with is `False`.
const A_HIDDEN_FIELD_KEEPS_A_STRUCT_LITERAL_APART_FROM_A_NEUTRAL: &str = r#"
struct /S: pub Type { @k: #NatType, u: {} }
let /cast : (
    s:
        #{node = StructType}
        /S(),
    G: (
        t:
            #{node = StructType}
            /S()
    ) -> Prop,
    y: G(s)
) -> G(/S { 0, () }) =
    (
        s:
            #{node = StructType}
            /S(),
        G: (
            t:
                #{node = StructType}
                /S()
        ) -> Prop,
        y: G(s)
    ) =>
        y;
/cast(
    /S { 1, () },
    (
        t:
            #{node = StructType}
            /S()
    ) =>
        /std/Bool/Holds(
            #{result = Ambient(#BoolType)}
            match
                #{field = Struct(/S, 0)}
                (t).0
            | 1 =>
                true
            | _ =>
                false
            end
        ),
    /std/Bool/True/qed()
)
"#;

/// The same `S` with two neutrals, where no literal opens an eta: what the shape directs is read off `S` itself, and its hidden `k` has a second inhabitant.
const TWO_NEUTRALS_AT_A_STRUCT_WHOSE_RELEVANT_FIELD_IS_HIDDEN_STAY_APART: &str = r#"
struct /S: pub Type { @k: #NatType, u: {} }
let /cast : (
    r:
        #{node = StructType}
        /S(),
    s:
        #{node = StructType}
        /S(),
    G: (
        t:
            #{node = StructType}
            /S()
    ) -> Prop,
    y: G(r)
) -> G(s) =
    (
        r:
            #{node = StructType}
            /S(),
        s:
            #{node = StructType}
            /S(),
        G: (
            t:
                #{node = StructType}
                /S()
        ) -> Prop,
        y: G(r)
    ) =>
        y;
/cast(
    /S { 1, () },
    /S { 0, () },
    (
        t:
            #{node = StructType}
            /S()
    ) =>
        /std/Bool/Holds(
            #{result = Ambient(#BoolType)}
            match
                #{field = Struct(/S, 0)}
                (t).0
            | 1 =>
                true
            | _ =>
                false
            end
        ),
    /std/Bool/True/qed()
)
"#;

/// `Q`'s one constructor pins its plain payload `n` by its index and its hidden `k` by none, so `Q/mk(@0, 5)` and `Q/mk(@1, 5)` are one proof of `Q(5)` and `read` would tell them apart: `cast` carries `G` across them by irrelevance, and at `G(q) = Holds(read(5, q) == 0)` the term the module closes with is `False`.
const A_HIDDEN_PAYLOAD_NO_INDEX_PINS_IS_NOT_READ_OUT_OF_A_PROOF: &str = r#"
induct /Q: (#NatType) -> pub Prop
| mk(@k: #NatType, n: #NatType): (n)
end
let /read : (
    m: #NatType,
    q:
        #{node = InductType}
        /Q()(m)
) -> #NatType =
    (
        m: #NatType,
        q:
            #{node = InductType}
            /Q()(m)
    ) =>
        #{
            result = Ambient(#NatType),
            cases = {
                stated =
                    #{node = InductType}
                    /Q()(m)}}
        match q
        | mk(@k, n) =>
            k
        end;
let /cast : (
    G: (
        q:
            #{node = InductType}
            /Q()(5)
    ) -> Prop,
    y: G(/Q/mk(@0, 5))
) -> G(/Q/mk(@1, 5)) =
    (
        G: (
            q:
                #{node = InductType}
                /Q()(5)
        ) -> Prop,
        y: G(/Q/mk(@0, 5))
    ) =>
        y;
/cast(
    (
        q:
            #{node = InductType}
            /Q()(5)
    ) =>
        /std/Bool/Holds(
            #{result = Ambient(#BoolType)}
            match /read(5, q)
            | 0 =>
                true
            | _ =>
                false
            end
        ),
    /std/Bool/True/qed()
)
"#;

/// Reduction reads a tuple's slot off a constructor's value through the flat runtime view `(tag, payload...)`, so `(P/mk(1)).1` is `1`: typed, `get` would read a payload out of a proof, `P/mk(0)` and `P/mk(1)` being one proof of `P`, and the term the module closes with is `False`.
const A_PROOFS_PAYLOAD_IS_READ_BY_NO_TUPLE_SLOT: &str = r#"
induct /P: pub Prop
| mk(n: #NatType)
end
let /get : (
    p:
        #{node = InductType}
        /P()
) -> #NatType =
    (
        p:
            #{node = InductType}
            /P()
    ) =>
        (p).1;
let /cast : (
    G: (
        p:
            #{node = InductType}
            /P()
    ) -> Prop,
    y: G(/P/mk(0))
) -> G(/P/mk(1)) =
    (
        G: (
            p:
                #{node = InductType}
                /P()
        ) -> Prop,
        y: G(/P/mk(0))
    ) =>
        y;
/cast(
    (
        p:
            #{node = InductType}
            /P()
    ) =>
        /std/Bool/Holds(
            #{result = Ambient(#BoolType)}
            match /get(p)
            | 0 =>
                true
            | _ =>
                false
            end
        ),
    /std/Bool/True/qed()
)
"#;

/// `T(c)` is the unit at `false` and `Bool` at `true`. Each arm of `leak` binds its two values at `T(c)` with `c` the local the arm refines, so the `false` arm, checked first, holds them one where the type has one inhabitant, and the `true` arm asks the same of `T(c)` where it has two: a shape read under one case answering under the next is `leak(true)` carrying `Holds(true)` to `Holds(false)`.
const A_SHAPE_ONE_ARM_DECIDES_ANSWERS_IN_NO_OTHER_ARM: &str = r#"
let /T : (c: #BoolType) -> Type =
    (c: #BoolType) =>
        #{result = Ambient(Type)}
        match c
        | false =>
            {}
        | true =>
            #BoolType
        end;
let /H : (c: #BoolType) ->
    #{level = 1}
    Prop =
    (c: #BoolType) =>
        #{
            result = Ambient(
                #{level = 1}
                Prop)}
        match c
        | false =>
            (
                x: /T(false),
                y: /T(false),
                G: (z: /T(false)) -> Prop,
                g: G(x)
            ) -> G(y)
        | true =>
            (
                x2: /T(true),
                y2: /T(true),
                G2: (z2: /T(true)) -> Prop,
                g2: G2(x2)
            ) -> G2(y2)
        end;
let /leak : (c: #BoolType) -> /H(c) =
    (c: #BoolType) =>
        match c: (c2) => /H(c2)
        | false =>
            (
                x: /T(c),
                y: /T(c),
                G: (z: /T(c)) -> Prop,
                g: G(x)
            ) =>
                g
        | true =>
            (
                x2: /T(c),
                y2: /T(c),
                G2: (z2: /T(c)) -> Prop,
                g2: G2(x2)
            ) =>
                g2
        end;
/leak(true)(true, false, (b: #BoolType) => /std/Bool/Holds(b), /std/Bool/True/qed())
"#;

/// `F(c) : M(c)` is the proposition `True` at `false` and `Bool` at `true`, so the sort a lookup reads off `F`'s declaration at `c` is `Prop` under the `false` arm, checked first, and `Type` under the `true` arm: a sort read under one case answering under the next is `leak(true)` carrying `Holds(true)` to `Holds(false)`.
const A_SORT_ONE_ARM_DECIDES_ANSWERS_IN_NO_OTHER_ARM: &str = r#"
let /M : (c: #BoolType) ->
    #{level = 1}
    Type =
    (c: #BoolType) =>
        #{
            result = Ambient(
                #{level = 1}
                Type)}
        match c
        | false =>
            Prop
        | true =>
            Type
        end;
let /F : (c: #BoolType) -> /M(c) =
    (c: #BoolType) =>
        match c: (c2) => /M(c2)
        | false =>
            #{node = InductType}
            /std/Bool/True()
        | true =>
            #BoolType
        end;
let /H : (c: #BoolType) ->
    #{level = 1}
    Prop =
    (c: #BoolType) =>
        #{
            result = Ambient(
                #{level = 1}
                Prop)}
        match c
        | false =>
            (
                x: /F(false),
                y: /F(false),
                G: (z: /F(false)) -> Prop,
                g: G(x)
            ) -> G(y)
        | true =>
            (
                x2: /F(true),
                y2: /F(true),
                G2: (z2: /F(true)) -> Prop,
                g2: G2(x2)
            ) -> G2(y2)
        end;
let /leak : (c: #BoolType) -> /H(c) =
    (c: #BoolType) =>
        match c: (c2) => /H(c2)
        | false =>
            (
                x: /F(c),
                y: /F(c),
                G: (z: /F(c)) -> Prop,
                g: G(x)
            ) =>
                g
        | true =>
            (
                x2: /F(c),
                y2: /F(c),
                G2: (z2: /F(c)) -> Prop,
                g2: G2(x2)
            ) =>
                g2
        end;
/leak(true)(true, false, (b: #BoolType) => /std/Bool/Holds(b), /std/Bool/True/qed())
"#;

/// The same, the type named by a `let` bound ahead of the split: `T` stands for `F(c)` at `M(c)`, a proposition under the `false` arm and a type under the `true` arm, and every reader takes it by value.
const A_SORT_A_LET_DECIDES_ANSWERS_IN_NO_OTHER_ARM: &str = r#"
let /M : (c: #BoolType) ->
    #{level = 1}
    Type =
    (c: #BoolType) =>
        #{
            result = Ambient(
                #{level = 1}
                Type)}
        match c
        | false =>
            Prop
        | true =>
            Type
        end;
let /F : (c: #BoolType) -> /M(c) =
    (c: #BoolType) =>
        match c: (c2) => /M(c2)
        | false =>
            #{node = InductType}
            /std/Bool/True()
        | true =>
            #BoolType
        end;
let /H : (c: #BoolType) ->
    #{level = 1}
    Prop =
    (c: #BoolType) =>
        #{
            result = Ambient(
                #{level = 1}
                Prop)}
        match c
        | false =>
            (
                x: /F(false),
                y: /F(false),
                G: (z: /F(false)) -> Prop,
                g: G(x)
            ) -> G(y)
        | true =>
            (
                x2: /F(true),
                y2: /F(true),
                G2: (z2: /F(true)) -> Prop,
                g2: G2(x2)
            ) -> G2(y2)
        end;
let /leak : (c: #BoolType) -> /H(c) =
    (c: #BoolType) =>
        let T: /M(c) = /F(c);
        match c: (c2) => /H(c2)
        | false =>
            (
                x: T,
                y: T,
                G: (z: T) -> Prop,
                g: G(x)
            ) =>
                g
        | true =>
            (
                x2: T,
                y2: T,
                G2: (z2: T) -> Prop,
                g2: G2(x2)
            ) =>
                g2
        end;
/leak(true)(true, false, (b: #BoolType) => /std/Bool/Holds(b), /std/Bool/True/qed())
"#;

/// `Box(A)` has one inhabitant where `A` is a proposition and two where it is `Bool`, so `generic`, which leaves `A`'s sort to its caller and holds any two boxes one, is true at `Prop` alone: used at `Type`, it carries `Holds(true)` to `Holds(false)`.
const A_STRUCT_AT_A_SORT_LEFT_TO_A_CALLER_HAS_ONE_INHABITANT_ONLY_AT_PROP: &str = r#"
#{universe_context = {parameter_count = 1, quality_count = 1}}
struct /Box(
    A:
        #{level = u}
        Sort
): pub
    #{level = u}
    Type { a: A }
#{universe_context = {parameter_count = 1, quality_count = 1}}
let /generic : (
    @A:
        #{level = u}
        Sort,
    x:
        #{node = StructType, universes = {levels = [u], qualities = [Param(0)]}}
        /Box(A),
    y:
        #{node = StructType, universes = {levels = [u], qualities = [Param(0)]}}
        /Box(A),
    G: (
        z:
            #{node = StructType, universes = {levels = [u], qualities = [Param(0)]}}
            /Box(A)
    ) -> Prop,
    g: G(x)
) -> G(y) =
    (
        @A:
            #{level = u}
            Sort,
        x:
            #{node = StructType, universes = {levels = [u], qualities = [Param(0)]}}
            /Box(A),
        y:
            #{node = StructType, universes = {levels = [u], qualities = [Param(0)]}}
            /Box(A),
        G: (
            z:
                #{node = StructType, universes = {levels = [u], qualities = [Param(0)]}}
                /Box(A)
        ) -> Prop,
        g: G(x)
    ) =>
        g;
(
    #{universes = {levels = [0], qualities = [Type]}}
    /generic)(
    @#BoolType,
    #{universes = {levels = [0], qualities = [Type]}}
    /Box(#BoolType) { true },
    #{universes = {levels = [0], qualities = [Type]}}
    /Box(#BoolType) { false },
    (
        z:
            #{node = StructType, universes = {levels = [0], qualities = [Type]}}
            /Box(#BoolType)
    ) =>
        /std/Bool/Holds(
            #{field = Struct(/Box, 0)}
            (z).0
        ),
    /std/Bool/True/qed()
)
"#;

/// The verdicts on `text` put after the propositions it names.
fn verdicts(text: &str) -> Vec<Verdict> {
    core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${TEXT}",
        PROPOSITIONS,
        TEXT = text
    ))
}

/// Whether `verdicts` is one `Mismatch`, in `name`: the route to `False` stands but for it.
fn one_mismatch_in(verdicts: &[Verdict], name: &str) -> bool {
    matches!(
        verdicts,
        [Verdict { name: Some(refused), error: Error::Mismatch { .. } }] if *refused == global(name)
    )
}

/// A hidden member takes no position among the members an author writes and every position among a value's slots: the literal's eta compares `k`. Mutation-checked: with `struct_eta` walking a literal's plain fields alone, the module is admitted at `False`.
#[test]
fn a_hidden_field_keeps_a_struct_literal_apart_from_a_neutral() {
    let verdicts = verdicts(A_HIDDEN_FIELD_KEEPS_A_STRUCT_LITERAL_APART_FROM_A_NEUTRAL);

    assert!(one_mismatch_in(&verdicts, "cast"), "{verdicts:?}");
}

/// What one inhabitant reads of a struct is every field its declaration holds, a hidden one among them. Mutation-checked: with `fields_inhabited_once` passing a hidden field over, the module is admitted at `False`.
#[test]
fn two_neutrals_at_a_struct_whose_relevant_field_is_hidden_stay_apart() {
    let verdicts = verdicts(TWO_NEUTRALS_AT_A_STRUCT_WHOSE_RELEVANT_FIELD_IS_HIDDEN_STAY_APART);

    assert!(one_mismatch_in(&verdicts, "cast"), "{verdicts:?}");
}

/// The singleton rung asks every payload to be pinned, a hidden one as a plain one. Mutation-checked: with `wholly_pinned` asked of the plain payloads alone, the module is admitted at `False`.
#[test]
fn a_hidden_payload_no_index_pins_is_not_read_out_of_a_proof() {
    let verdicts = verdicts(A_HIDDEN_PAYLOAD_NO_INDEX_PINS_IS_NOT_READ_OUT_OF_A_PROOF);

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(read), error: Error::LargeElimination(family) }]
                if *read == global("read") && *family == global("Q")
        ),
        "{verdicts:?}"
    );
}

/// A slot is typed off what it is a slot of, and a family's value is no tuple, so the flat view reduction reads a payload through is never one a typed term reaches.
#[test]
fn a_proofs_payload_is_read_by_no_tuple_slot() {
    let verdicts = verdicts(A_PROOFS_PAYLOAD_IS_READ_BY_NO_TUPLE_SLOT);

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(get), error: Error::NotATuple(_) }] if *get == global("get")
        ),
        "{verdicts:?}"
    );
}

/// One inhabitant read off a shape a case made lives as long as the case.
#[test]
fn a_shape_one_arm_decides_answers_in_no_other_arm() {
    let verdicts = verdicts(A_SHAPE_ONE_ARM_DECIDES_ANSWERS_IN_NO_OTHER_ARM);

    assert!(one_mismatch_in(&verdicts, "leak"), "{verdicts:?}");
}

/// A sort read off a declaration at an argument a case refined lives as long as the case.
#[test]
fn a_sort_one_arm_decides_answers_in_no_other_arm() {
    let verdicts = verdicts(A_SORT_ONE_ARM_DECIDES_ANSWERS_IN_NO_OTHER_ARM);

    assert!(one_mismatch_in(&verdicts, "leak"), "{verdicts:?}");
}

/// A `let` bound ahead of the split is read by value, so its sort is the one each case gives its value.
#[test]
fn a_sort_a_let_decides_answers_in_no_other_arm() {
    let verdicts = verdicts(A_SORT_A_LET_DECIDES_ANSWERS_IN_NO_OTHER_ARM);

    assert!(one_mismatch_in(&verdicts, "leak"), "{verdicts:?}");
}

/// A declaration that leaves a sort to its caller is judged at each assignment, and at `Type` the box's field is relevant.
#[test]
fn a_struct_at_a_sort_left_to_a_caller_has_one_inhabitant_only_at_prop() {
    let verdicts = verdicts(A_STRUCT_AT_A_SORT_LEFT_TO_A_CALLER_HAS_ONE_INHABITANT_ONLY_AT_PROP);

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(generic), error: Error::AtSorts { sorts, refusal } }]
                if *generic == global("generic")
                    && sorts[..] == [Quality::Type]
                    && matches!(**refusal, Error::Mismatch { .. })
        ),
        "{verdicts:?}"
    );
}
