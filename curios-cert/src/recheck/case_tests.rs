//! A case read at its scrutinee's own instance: the parameters it opens at, the arms that instance excuses, the variable an arm refines and the goal it is checked at, each of a module the elaborator would not build, written as Core text and put to the certifier with nothing in scope, declaring the propositions it uses.

use {
    super::test_support::*,
    crate::*,
    curios_common::{Qualifier, fill},
    curios_core::{Atom, Global},
};

/// `A` and `B` are one group, each with a parameter named `n`, a `Nat` in one and a `Bool` in the other. `mkA`'s payload states `n == 1` of its own family's `n`, so an `A(0)` holds a `Holds(0 == 1)`; read at the sibling's parameter, or at another instance's, the `qed` it is built with would fit, and the match would hand it on at `False`.
const A_CASE_OF_A_FAMILY_IN_A_GROUP: &str = r#"
induct /A(n: #NatType): pub Type
| mkA(
    h: /std/Bool/Holds(#NatEql(n, 1)),
    other:
        #{node = InductType}
        /B(true)
)
end
and induct /B(n: #BoolType): pub Type
| mkB(
    h: /std/Bool/Holds(n),
    other:
        #{node = InductType}
        /A(0)
)
| base()
end
((
    x:
        #{node = InductType}
        /A(0)
) =>
    #{goal = /std/Bool/False} match x
    | mkA(h, o) =>
        h
    end)(
    #{node = Variant}
    /A/mkA(
        @0,
        /std/Bool/True/qed(),
        #{node = Variant}
        /B/base(@true)
    )
)
"#;

// The payload of `A(0)`'s case is typed at `A`'s own parameter instantiated at `0`, a `Holds(0 == 1)`, which `qed` does not inhabit: the value is refused where the term the module closes with builds it.
#[test]
fn a_case_reads_its_own_familys_parameter_in_a_group() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_CASE_OF_A_FAMILY_IN_A_GROUP}",
        PROPOSITIONS,
        A_CASE_OF_A_FAMILY_IN_A_GROUP
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

/// `Lift`'s case holds a `Type u` its index pins, so its level is one two instances are told apart by. The match on `p : Lift.{0}(X)` states `Lift.{<level>}(X)`, the instance its sole arm's payload is read off where it reduces: at `1`, a type of `Type 0` read as one of `Type 1`.
const A_MATCH_STATING_AN_INSTANCE_OF_LIFT: &str = r#"
#{universe_context = {parameter_count = 1}}
induct /Lift: (
    T: #{level = u} Type
) -> pub #{level = u + 1} Prop
| mk(B: #{level = u} Type): (B)
end
let /read : (
    X: Type,
    p:
        #{node = InductType, universes = {levels = [0]}}
        /Lift()(X)
) -> #{level = 1} Type =
    (
        X: Type,
        p:
            #{node = InductType, universes = {levels = [0]}}
            /Lift()(X)
    ) =>
        #{goal = #{level = 1} Type, 
            cases = {
                stated =
                    #{node = InductType, universes = {levels = [${level}]}}
                    /Lift()(X)}}
        match p
        | mk(B) =>
            B
        end;
/std/Bool/False
"#;

// The instance a match states is held to the type its scrutinee was inferred at, a family's levels compared where its variance calls them invariant, so the arm is read at the scrutinee's own instance or the match is refused; at the scrutinee's own level it stands.
#[test]
fn a_match_stating_its_scrutinee_at_another_level_is_refused() {
    let verdicts_at = |level: usize| {
        core_text_verdicts(&fill!(
            "${PROPOSITIONS}\n${lift}",
            PROPOSITIONS,
            lift = fill!(A_MATCH_STATING_AN_INSTANCE_OF_LIFT, level)
        ))
    };

    let another = verdicts_at(1);
    assert!(
        another
            .iter()
            .any(|verdict| verdict.name == Some(global("read"))
                && matches!(verdict.error, Error::Mismatch { .. })),
        "{another:?}"
    );
    let own = verdicts_at(0);
    assert!(own.iter().all(|verdict| verdict.name.is_none()), "{own:?}");
}

/// `T` is indexed by a proposition, so `T(Two/a())` and `T(Two/b())` are one type and `tb()` is a `T(Two/a())`. The match on it writes `ta`'s arm alone: excused as clashing at the index, `tb`'s absence would leave a closed match whose sole arm names no payload, which reduction takes without reading its scrutinee, so `tb()` would take `ta`'s arm and its `qed` would stand at `Code(Two/a(), tb())`, which is `False`.
const AN_ARM_OMITTED_AT_A_PROPOSITION_VALUED_INDEX: &str = r#"
induct /Two: pub Prop
| a()
| b()
end
induct /T: (
    x:
        #{node = InductType}
        /Two()
) -> pub Type
| ta(): (
    #{node = Variant}
    /Two/a()
)
| tb(): (
    #{node = Variant}
    /Two/b()
)
end
let /Code : (
    x:
        #{node = InductType}
        /Two(),
    t:
        #{node = InductType}
        /T()(x)
) -> Prop =
    (
        x:
            #{node = InductType}
            /Two(),
        t:
            #{node = InductType}
            /T()(x)
    ) =>
        #{goal = Prop}
        match t
        | ta() =>
            #{node = InductType}
            /std/Bool/True()
        | tb() =>
            #{node = InductType}
            /std/Bool/False()
        end;
((
    t:
        #{node = InductType}
        /T()(
            #{node = Variant}
            /Two/a()
        )
) =>
    #{goal = /Code(#{node = Variant} /Two/a(), t)} match t
    | ta() =>
        /std/Bool/True/qed()
    end)(
    #{node = Variant}
    /T/tb()
)
"#;

// An absent arm is excused only by a definite clash, and inversion concludes none at a `Prop`-valued position, where irrelevance makes `Two/a()` and `Two/b()` one value: `tb` is missing, and the arm reduction would take untyped is refused with it. `curios`'s `tests::rules::index_tests` puts the same proposition to the elaborator under this name.
#[test]
fn a_proposition_valued_index_cannot_excuse_an_omitted_arm() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${AN_ARM_OMITTED_AT_A_PROPOSITION_VALUED_INDEX}",
        PROPOSITIONS,
        AN_ARM_OMITTED_AT_A_PROPOSITION_VALUED_INDEX
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: None, error: Error::MissingArm { family, tag } }]
                if *family == global("T") && *tag == Atom::from("tb")
        ),
        "{verdicts:?}"
    );
}

/// An arm whose scrutinee already is a value of another case is not typed, since reduction never takes it. `prop`'s scrutinee is `Two/a()`, a proposition's, whose cases irrelevance makes one value, and its `b` arm holds `true`, no proof of `True`; `data`'s is `Pick/l()`, a relevant family's, and its `r` arm holds the same.
const ARMS_AT_ANOTHER_CASE: &str = r#"
induct /Two: pub Prop
| a()
| b()
end
induct /Pick: pub Type
| l()
| r()
end
let /prop :
    #{node = InductType}
    /std/Bool/True() =
    #{goal = #{node = InductType} /std/Bool/True()}
    match
        #{node = Variant}
        /Two/a()
    | a() =>
        /std/Bool/True/qed()
    | b() =>
        true
    end;
let /data :
    #{node = InductType}
    /std/Bool/True() =
    #{goal = #{node = InductType} /std/Bool/True()}
    match
        #{node = Variant}
        /Pick/l()
    | l() =>
        /std/Bool/True/qed()
    | r() =>
        true
    end;
/std/Bool/False
"#;

// `never_taken` asks the unifier that excuses an absent arm, which concludes no clash between two of a proposition's constructors: `prop`'s `b` arm is typed and refused, while `data`'s `r` arm, at a relevant family's other case, stands untyped.
#[test]
fn an_arm_at_another_case_of_a_proposition_is_typed() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${ARMS_AT_ANOTHER_CASE}",
        PROPOSITIONS,
        ARMS_AT_ANOTHER_CASE
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("prop"))
                && matches!(verdict.error, Error::Mismatch { .. })),
        "{verdicts:?}"
    );
    assert!(
        verdicts
            .iter()
            .all(|verdict| verdict.name != Some(global("data"))),
        "{verdicts:?}"
    );
}

/// `Code`'s case targets `Type`, a term of `Type 1`, and `c2`'s index is `Y`, a `let` whose value is `X : ${sort}`: the index reads by value as `X`, which inversion refines to `Type` where that fits `X`'s own sort, and the arm's `(A: X) => A` inhabits `(a: Type) -> Type` only there.
const A_REFINEMENT_THROUGH_A_LET: &str = r#"
induct /Code: (
    T: #{level = 1} Type
) -> pub Type
| big(): (Type)
end
let /through : (
    X: ${sort},
    c:
        #{node = InductType}
        /Code()(X)
) -> (a: X) -> X =
    (
        X: ${sort},
        c:
            #{node = InductType}
            /Code()(X)
    ) =>
        let Y: ${sort} =
            X;
        ((
            c2:
                #{node = InductType}
                /Code()(Y)
        ) =>
            #{goal = (a: Y) -> Y} match c2
            | big() =>
                (A: X) => A
            end)(c);
/std/Bool/False
"#;

// An outer variable's refinement is kept only where its target sits in the variable's own sort, so binding it at the target's type agrees with the variable's, read through a `let` as directly: at `Type` the refinement is dropped and the arm refused, at `Type 1` it is kept and the arm stands.
#[test]
fn a_refinement_read_through_a_let_is_kept_only_where_its_target_fits_its_variable() {
    let verdicts_at = |sort: &str| {
        core_text_verdicts(&fill!(
            "${PROPOSITIONS}\n${through}",
            PROPOSITIONS,
            through = fill!(A_REFINEMENT_THROUGH_A_LET, sort)
        ))
    };

    let below = verdicts_at("Type");
    assert!(
        below
            .iter()
            .any(|verdict| verdict.name == Some(global("through"))
                && matches!(verdict.error, Error::Mismatch { .. })),
        "{below:?}"
    );
    let fitting = verdicts_at("#{level = 1} Type");
    assert!(
        fitting
            .iter()
            .all(|verdict| verdict.name != Some(global("through"))),
        "{fitting:?}"
    );
}

/// `Ix` at `j + j`, a sum that is `0` at `j = 0` and a successor at every other `j`, so each case's target may meet it, and at `j + 1`, a successor no `zero` meets.
const AN_ARM_ABSENT_WHERE_A_SUM_MAY_MEET_ITS_TARGET: &str = r#"
induct /Ix: (i: #NatType) -> pub Type
| zero(): (0)
| succ(k: #NatType): (#NatAdd(k, 1))
end
let /only_zero : (
    j: #NatType,
    v:
        #{node = InductType}
        /Ix()(#NatAdd(j, j))
) -> #NatType =
    (
        j: #NatType,
        v:
            #{node = InductType}
            /Ix()(#NatAdd(j, j))
    ) =>
        #{goal = #NatType}
        match v
        | zero() =>
            0
        end;
let /only_succ : (
    j: #NatType,
    v:
        #{node = InductType}
        /Ix()(#NatAdd(j, j))
) -> #NatType =
    (
        j: #NatType,
        v:
            #{node = InductType}
            /Ix()(#NatAdd(j, j))
    ) =>
        #{goal = #NatType}
        match v
        | succ(k) =>
            k
        end;
let /excused : (
    j: #NatType,
    v:
        #{node = InductType}
        /Ix()(#NatAdd(j, 1))
) -> #NatType =
    (
        j: #NatType,
        v:
            #{node = InductType}
            /Ix()(#NatAdd(j, 1))
    ) =>
        #{goal = #NatType}
        match v
        | succ(k) =>
            k
        end;
/std/Bool/False
"#;

// An absent arm is excused only where the unifier finds a definite clash: `j + j` against `0` or against `k + 1` is a disagreement only arithmetic could settle, which keeps each arm mandatory, while `j + 1` against `0` clashes and excuses `zero`, the control. `curios`'s `tests::rules::coverage_tests` puts the program to the elaborator under this name.
//
// Mutation-checked at `check_induct_arms`: with the coverage loop taken out, neither `only_zero` nor `only_succ` is refused.
#[test]
fn an_arm_absent_where_a_sum_may_meet_its_target_is_refused() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${AN_ARM_ABSENT_WHERE_A_SUM_MAY_MEET_ITS_TARGET}",
        PROPOSITIONS,
        AN_ARM_ABSENT_WHERE_A_SUM_MAY_MEET_ITS_TARGET
    ));
    let missing = |name: &str, tag: &str| {
        verdicts.iter().any(|verdict| {
            verdict.name == Some(global(name))
                && verdict.error
                    == Error::MissingArm {
                        family: global("Ix"),
                        tag: Atom::from(tag),
                    }
        })
    };

    assert!(missing("only_zero", "succ"), "{verdicts:?}");
    assert!(missing("only_succ", "zero"), "{verdicts:?}");
    assert!(
        !verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("excused"))),
        "{verdicts:?}"
    );
}

/// An arm a reader wrote that does not type, `0` where `True` is wanted, under three scrutinees: one that computes to the other case wherever it stands, one that computes to the arm's own, and one stuck on its argument.
const AN_ILL_TYPED_ARM_UNDER_THREE_SCRUTINEES: &str = r#"
let /never : (n: #NatType) -> #BoolType =
    (n: #NatType) => false;
let /always : (n: #NatType) -> #BoolType =
    (n: #NatType) => true;
let /zero : (n: #NatType) -> #BoolType =
    (n: #NatType) =>
        #{goal = #BoolType}
        match n
        | 0 =>
            true
        | k + 1 =>
            false
        end;
let /excused : (n: #NatType) ->
    #{node = InductType}
    /std/Bool/True() =
    (n: #NatType) =>
        #{
            goal = 
                #{node = InductType}
                /std/Bool/True()}
        match /never(n)
        | false =>
            #{node = Variant}
            /std/Bool/True/qed()
        | true =>
            0
        end;
let /taken : (n: #NatType) ->
    #{node = InductType}
    /std/Bool/True() =
    (n: #NatType) =>
        #{
            goal = 
                #{node = InductType}
                /std/Bool/True()}
        match /always(n)
        | false =>
            #{node = Variant}
            /std/Bool/True/qed()
        | true =>
            0
        end;
let /undecided : (n: #NatType) ->
    #{node = InductType}
    /std/Bool/True() =
    (n: #NatType) =>
        #{
            goal = 
                #{node = InductType}
                /std/Bool/True()}
        match /zero(n)
        | false =>
            #{node = Variant}
            /std/Bool/True/qed()
        | true =>
            0
        end;
/std/Bool/False
"#;

// The elaborator refuses all three arms, as a reader wrote them; the certifier withdraws a refused arm only where its scrutinee computes to another case, so it is one no reduction takes, which `excused`'s is and `taken`'s and `undecided`'s are not.
//
// Mutation-checked at `unless_never_taken`: with a refused arm withdrawn whatever its scrutinee computes to, `taken` and `undecided` are admitted.
#[test]
fn an_ill_typed_arm_is_excused_only_where_no_reduction_takes_it() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${AN_ILL_TYPED_ARM_UNDER_THREE_SCRUTINEES}",
        PROPOSITIONS,
        AN_ILL_TYPED_ARM_UNDER_THREE_SCRUTINEES
    ));
    let refused = |name: &str| {
        verdicts.iter().any(|verdict| {
            verdict.name == Some(global(name)) && matches!(verdict.error, Error::Mismatch { .. })
        })
    };

    assert!(refused("taken"), "{verdicts:?}");
    assert!(refused("undecided"), "{verdicts:?}");
    assert!(
        !verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("excused"))),
        "{verdicts:?}"
    );
}

/// One family a carrier indexes for each shape, each with one case: `Ix` at a successor, `Iz` at zero and `By` at the empty value. The near misses meet that case's target at some value of their variables — a floorless sum, a product and a truncated difference are successors somewhere, a power of a base that may be zero is zero at `a = 0`, a window, a concatenation of chunks and a fill of unknown count are empty somewhere — and the controls at none: a successor against zero, a run behind a chunk, an appended byte and a fill of a successor count against the empty value. The term the module closes with types at `False` through `/sum`.
const AN_ARM_ABSENT_WHERE_A_CARRIERS_INDEX_MAY_MEET_ITS_TARGET: &str = r#"
induct /Ix: (i: #NatType) -> pub Type
| succ(k: #NatType): (#NatAdd(k, 1))
end
induct /Iz: (i: #NatType) -> pub Type
| zero(): (0)
end
induct /By: (b: #BytesType) -> pub Type
| e(): (x[])
end
let /sum : (
    a: #NatType,
    b: #NatType,
    v:
        #{node = InductType}
        /Ix()(#NatAdd(a, b))
) -> /std/Bool/False =
    (
        a: #NatType,
        b: #NatType,
        v:
            #{node = InductType}
            /Ix()(#NatAdd(a, b))
    ) =>
        #{goal = /std/Bool/False} match v
        end;
let /product : (
    a: #NatType,
    b: #NatType,
    v:
        #{node = InductType}
        /Ix()(#NatMul(a, b))
) -> /std/Bool/False =
    (
        a: #NatType,
        b: #NatType,
        v:
            #{node = InductType}
            /Ix()(#NatMul(a, b))
    ) =>
        #{goal = /std/Bool/False} match v
        end;
let /difference : (
    a: #NatType,
    b: #NatType,
    v:
        #{node = InductType}
        /Ix()(#NatSub(a, b))
) -> /std/Bool/False =
    (
        a: #NatType,
        b: #NatType,
        v:
            #{node = InductType}
            /Ix()(#NatSub(a, b))
    ) =>
        #{goal = /std/Bool/False} match v
        end;
let /power : (
    a: #NatType,
    b: #NatType,
    v:
        #{node = InductType}
        /Iz()(#NatPow(a, #NatAdd(b, 1)))
) -> /std/Bool/False =
    (
        a: #NatType,
        b: #NatType,
        v:
            #{node = InductType}
            /Iz()(#NatPow(a, #NatAdd(b, 1)))
    ) =>
        #{goal = /std/Bool/False} match v
        end;
let /window : (
    xs: #BytesType,
    s: #NatType,
    l: #NatType,
    p: /std/Bool/Holds(#NatLe(#NatAdd(s, l), #BytesLen(xs))),
    v:
        #{node = InductType}
        /By()(#BytesSlice(xs, s, l, p))
) -> /std/Bool/False =
    (
        xs: #BytesType,
        s: #NatType,
        l: #NatType,
        p: /std/Bool/Holds(#NatLe(#NatAdd(s, l), #BytesLen(xs))),
        v:
            #{node = InductType}
            /By()(#BytesSlice(xs, s, l, p))
    ) =>
        #{goal = /std/Bool/False} match v
        end;
let /chunks : (
    xs: #BytesType,
    ys: #BytesType,
    v:
        #{node = InductType}
        /By()(#BytesConcat([xs, ys]))
) -> /std/Bool/False =
    (
        xs: #BytesType,
        ys: #BytesType,
        v:
            #{node = InductType}
            /By()(#BytesConcat([xs, ys]))
    ) =>
        #{goal = /std/Bool/False} match v
        end;
let /fill : (
    n: #NatType,
    c: #ByteType,
    v:
        #{node = InductType}
        /By()(#BytesReplicate(n, c))
) -> /std/Bool/False =
    (
        n: #NatType,
        c: #ByteType,
        v:
            #{node = InductType}
            /By()(#BytesReplicate(n, c))
    ) =>
        #{goal = /std/Bool/False} match v
        end;
let /successor : (
    a: #NatType,
    v:
        #{node = InductType}
        /Iz()(#NatAdd(a, 1))
) -> /std/Bool/False =
    (
        a: #NatType,
        v:
            #{node = InductType}
            /Iz()(#NatAdd(a, 1))
    ) =>
        #{goal = /std/Bool/False} match v
        end;
let /trailing : (
    xs: #BytesType,
    v:
        #{node = InductType}
        /By()(#BytesConcat([xs, x[0x01]]))
) -> /std/Bool/False =
    (
        xs: #BytesType,
        v:
            #{node = InductType}
            /By()(#BytesConcat([xs, x[0x01]]))
    ) =>
        #{goal = /std/Bool/False} match v
        end;
let /single : (
    xs: #BytesType,
    c: #ByteType,
    v:
        #{node = InductType}
        /By()(#BytesAppend(xs, c))
) -> /std/Bool/False =
    (
        xs: #BytesType,
        c: #ByteType,
        v:
            #{node = InductType}
            /By()(#BytesAppend(xs, c))
    ) =>
        #{goal = /std/Bool/False} match v
        end;
let /filled : (
    n: #NatType,
    c: #ByteType,
    v:
        #{node = InductType}
        /By()(#BytesReplicate(#NatAdd(n, 1), c))
) -> /std/Bool/False =
    (
        n: #NatType,
        c: #ByteType,
        v:
            #{node = InductType}
            /By()(#BytesReplicate(#NatAdd(n, 1), c))
    ) =>
        #{goal = /std/Bool/False} match v
        end;
/sum(
    1,
    0,
    #{node = Variant}
    /Ix/succ(0)
)
"#;

// An absent arm is excused only where the carriers' algebra concludes a clash that holds at every value, so each near miss keeps its case mandatory and each control's case is excused; the closing term types, so the seven refusals are all that stand between the module and `False`. `curios`'s `tests::rules::coverage_tests` puts the program to the elaborator under this name.
//
// Mutation-checked at `Cancelled::<Natural, _>::deduction` (`curios-algebra/src/combination.rs`): with an impossible arm asking only that the emptied side's constant be zero, `self.left.constant.is_zero()` for `self.left.is_zero()` and its mirror, `/sum`, `/product` and `/difference` are excused. At `Word::against_identity` (`curios-algebra/src/word.rs`): with a window and a chunk counted positive, `/window`, `/chunks` and `/fill` are excused.
#[test]
fn an_arm_absent_where_a_carriers_index_may_meet_its_target_is_refused() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${AN_ARM_ABSENT_WHERE_A_CARRIERS_INDEX_MAY_MEET_ITS_TARGET}",
        PROPOSITIONS,
        AN_ARM_ABSENT_WHERE_A_CARRIERS_INDEX_MAY_MEET_ITS_TARGET
    ));
    let missing = |name: &str, family: &str, tag: &str| {
        verdicts.iter().any(|verdict| {
            verdict.name == Some(global(name))
                && verdict.error
                    == Error::MissingArm {
                        family: global(family),
                        tag: Atom::from(tag),
                    }
        })
    };

    assert!(missing("sum", "Ix", "succ"), "{verdicts:?}");
    assert!(missing("product", "Ix", "succ"), "{verdicts:?}");
    assert!(missing("difference", "Ix", "succ"), "{verdicts:?}");
    assert!(missing("power", "Iz", "zero"), "{verdicts:?}");
    assert!(missing("window", "By", "e"), "{verdicts:?}");
    assert!(missing("chunks", "By", "e"), "{verdicts:?}");
    assert!(missing("fill", "By", "e"), "{verdicts:?}");
    assert_eq!(verdicts.len(), 7, "{verdicts:?}");
}

/// `[Pr/b()]` and `[Pr/a()]` are one list by irrelevance, so `Ix/mk()` inhabits `Ix([Pr/b()])` and the term the module closes with types at `False` through `/absurd`. `/apart`'s `[]` against `[Pr/a()]` is a clash of length, which reads no element.
const TWO_LISTS_OF_PROOFS_THAT_IRRELEVANCE_HOLDS_ONE: &str = r#"
induct /Pr: pub Prop
| a()
| b()
end
induct /Ix: (
    l: #ListType(
        #{node = InductType}
        /Pr()
    )
) -> pub Type
| mk(): (
    #List(
        #{node = InductType}
        /Pr(),
        [/Pr/a()]
    )
)
end
let /absurd : (
    v:
        #{node = InductType}
        /Ix()(
            #List(
                #{node = InductType}
                /Pr(),
                [/Pr/b()]
            )
        )
) -> /std/Bool/False =
    (
        v:
            #{node = InductType}
            /Ix()(
                #List(
                    #{node = InductType}
                    /Pr(),
                    [/Pr/b()]
                )
            )
    ) =>
        #{goal = /std/Bool/False} match v
        end;
let /apart : (
    v:
        #{node = InductType}
        /Ix()(
            #List(
                #{node = InductType}
                /Pr(),
                []
            )
        )
) -> /std/Bool/False =
    (
        v:
            #{node = InductType}
            /Ix()(
                #List(
                    #{node = InductType}
                    /Pr(),
                    []
                )
            )
    ) =>
        #{goal = /std/Bool/False} match v
        end;
/absurd(/Ix/mk())
"#;

// No clash is concluded between two of a proposition's constructors, and a list's elements are terms rather than decided values, so two runs that differ at a proof are no clash: `mk` stays mandatory in `/absurd`, while `/apart`'s clash of length is excused. The refusal in `/absurd` is all that stands between the module and `False`. `curios`'s `tests::rules::coverage_tests` puts the program to the elaborator under this name.
//
// Mutation-checked at `Run for Vec<E>` (`curios-algebra/src/word.rs`): with `DECIDED` true, the two runs' heads clash, `/absurd` is excused and the walk returns no verdict.
#[test]
fn two_lists_of_proofs_that_irrelevance_holds_one_do_not_clash() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${TWO_LISTS_OF_PROOFS_THAT_IRRELEVANCE_HOLDS_ONE}",
        PROPOSITIONS,
        TWO_LISTS_OF_PROOFS_THAT_IRRELEVANCE_HOLDS_ONE
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(absurd), error: Error::MissingArm { family, tag } }]
                if *absurd == global("absurd")
                    && *family == global("Ix")
                    && *tag == Atom::from("mk")
        ),
        "{verdicts:?}"
    );
}

/// `drain` recurses on the payload of its proof's own case, which the size order reads as structure, at a proposition whose index is a proof: irrelevance makes `Q(v)` and `Q(Wit/s(v))` one type, so the call on the payload types, and `Q/nil()` stands at `Q(Wit/s(Wit/triv()))` for the same reason, so `drain(triv, nil)` closes the module at `False` were `nil`'s arm excused.
const A_RECURSION_ON_A_PROPOSITIONS_CASE: &str = r#"
induct /Wit: pub Prop
| triv()
| s(inner: #{node = InductType} /Wit())
end
induct /Q: (i: #{node = InductType} /Wit()) -> pub Prop
| nil(): (/Wit/triv())
| cons(
    v: #{node = InductType} /Wit(),
    inner: #{node = InductType} /Q()(v)
): (/Wit/s(v))
end
#{node = Rec, proof = true}
let /drain : (
    v: #{node = InductType} /Wit(),
    q: #{node = InductType} /Q()(/Wit/s(v))
) -> #{node = InductType} /std/Bool/False() =
    (
        v: #{node = InductType} /Wit(),
        q: #{node = InductType} /Q()(/Wit/s(v))
    ) =>
        #{goal = #{node = InductType} /std/Bool/False()}
        match q
        | cons(w, inner) => /drain(w, inner)
        end;
/drain(/Wit/triv(), /Q/nil())
"#;

// What stands between the reading and `False` is coverage: inversion concludes no clash between two of a proposition's constructors, so `nil`'s arm is mandatory and `drain` is refused for the arm it omits, not for its descent. `curios`'s `tests::rules::totality_tests` puts the program to the elaborator under this name.
//
// Mutation-checked at `unify_node` (`curios-analysis/src/invert.rs`): with its `Prop`-sorted family answering by tags, `Wit/triv()` against `Wit/s(v)` clashes, `nil` is excused and the walk returns no verdict.
#[test]
fn a_recursion_on_a_propositions_case_still_needs_the_arm_it_omits() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_RECURSION_ON_A_PROPOSITIONS_CASE}",
        PROPOSITIONS,
        A_RECURSION_ON_A_PROPOSITIONS_CASE
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("drain"))
                && verdict.error
                    == Error::MissingArm {
                        family: global("Q"),
                        tag: Atom::from("nil"),
                    }),
        "{verdicts:?}"
    );
}

/// Seven propositions `P*`, each with a `base` constructor and a `step` that holds a proof of the instance it targets, indexed where irrelevance makes the two targets one: a relevant family holding a proof, a tuple of proofs, a type family over a proposition, a proposition at level one, a proposition declared in a group, and a proposition whose index is a proof, matched once at a stated instance at `base`'s target and once at a type spelled through a definition.
const INDICES_HOLDING_PROOFS: &str = r#"
induct /Wit: pub Prop
| a()
| b()
end
induct /Box: pub Type
| box(w: #{node = InductType} /Wit())
end
induct /Opt(A: Prop): pub Type
| none()
| some(x: A)
end
induct /High: pub
    #{level = 1}
    Prop
| ha()
| hb()
end
induct /G: pub Prop
| ga()
| gb(x: #{node = InductType} /G2())
end
and induct /G2: pub Prop
| c(y: #{node = InductType} /G())
| d()
end
induct /Pbox: (i: #{node = InductType} /Box()) -> pub Prop
| base(): (/Box/box(/Wit/a()))
| step(p: #{node = InductType} /Pbox()(/Box/box(/Wit/b()))): (/Box/box(/Wit/b()))
end
induct /Ptuple: (i: {#{node = InductType} /Wit(), #{node = InductType} /Wit()}) -> pub Prop
| base(): ((/Wit/a(), /Wit/a()))
| step(p: #{node = InductType} /Ptuple()((/Wit/b(), /Wit/b()))): ((/Wit/b(), /Wit/b()))
end
induct /Popt: (i: #{node = InductType} /Opt(#{node = InductType} /Wit())) -> pub Prop
| base(): (/Opt/some(@#{node = InductType} /Wit(), /Wit/a()))
| step(p: #{node = InductType} /Popt()(/Opt/some(@#{node = InductType} /Wit(), /Wit/b()))): (/Opt/some(@#{node = InductType} /Wit(), /Wit/b()))
end
induct /Phigh: (i: #{node = InductType} /High()) -> pub Prop
| base(): (/High/ha())
| step(p: #{node = InductType} /Phigh()(/High/hb())): (/High/hb())
end
induct /Pgroup: (i: #{node = InductType} /G()) -> pub Prop
| base(): (/G/ga())
| step(p: #{node = InductType} /Pgroup()(/G/gb(/G2/d()))): (/G/gb(/G2/d()))
end
induct /Pwit: (i: #{node = InductType} /Wit()) -> pub Prop
| base(): (/Wit/a())
| step(p: #{node = InductType} /Pwit()(/Wit/b())): (/Wit/b())
end
let /Alias : (i: #{node = InductType} /Wit()) -> Prop =
    (i: #{node = InductType} /Wit()) => #{node = InductType} /Pwit()(i);
"#;

/// A recursion on each `P*`'s own case at `step`'s target, every arm recursing and `base`'s omitted, closing at `False` on `base`.
const RECURSIONS_OMITTING_BASE: &str = r#"
#{node = Rec, proof = true}
let /fbox : (p: #{node = InductType} /Pbox()(/Box/box(/Wit/b()))) -> #{node = InductType} /std/Bool/False() =
    (p: #{node = InductType} /Pbox()(/Box/box(/Wit/b()))) =>
        #{goal = #{node = InductType} /std/Bool/False()}
        match p
        | step(q) => /fbox(q)
        end;
#{node = Rec, proof = true}
let /ftuple : (p: #{node = InductType} /Ptuple()((/Wit/b(), /Wit/b()))) -> #{node = InductType} /std/Bool/False() =
    (p: #{node = InductType} /Ptuple()((/Wit/b(), /Wit/b()))) =>
        #{goal = #{node = InductType} /std/Bool/False()}
        match p
        | step(q) => /ftuple(q)
        end;
#{node = Rec, proof = true}
let /fopt : (p: #{node = InductType} /Popt()(/Opt/some(@#{node = InductType} /Wit(), /Wit/b()))) -> #{node = InductType} /std/Bool/False() =
    (p: #{node = InductType} /Popt()(/Opt/some(@#{node = InductType} /Wit(), /Wit/b()))) =>
        #{goal = #{node = InductType} /std/Bool/False()}
        match p
        | step(q) => /fopt(q)
        end;
#{node = Rec, proof = true}
let /fhigh : (p: #{node = InductType} /Phigh()(/High/hb())) -> #{node = InductType} /std/Bool/False() =
    (p: #{node = InductType} /Phigh()(/High/hb())) =>
        #{goal = #{node = InductType} /std/Bool/False()}
        match p
        | step(q) => /fhigh(q)
        end;
#{node = Rec, proof = true}
let /fgroup : (p: #{node = InductType} /Pgroup()(/G/gb(/G2/d()))) -> #{node = InductType} /std/Bool/False() =
    (p: #{node = InductType} /Pgroup()(/G/gb(/G2/d()))) =>
        #{goal = #{node = InductType} /std/Bool/False()}
        match p
        | step(q) => /fgroup(q)
        end;
#{node = Rec, proof = true}
let /fstated : (p: #{node = InductType} /Pwit()(/Wit/b())) -> #{node = InductType} /std/Bool/False() =
    (p: #{node = InductType} /Pwit()(/Wit/b())) =>
        #{goal = #{node = InductType} /std/Bool/False(), cases = {stated = #{node = InductType} /Pwit()(/Wit/a())}}
        match p
        | step(q) => /fstated(q)
        end;
#{node = Rec, proof = true}
let /falias : (p: /Alias(/Wit/b())) -> #{node = InductType} /std/Bool/False() =
    (p: /Alias(/Wit/b())) =>
        #{goal = #{node = InductType} /std/Bool/False()}
        match p
        | step(q) => /falias(q)
        end;
/fbox(/Pbox/base())
"#;

/// The same recursions with `base`'s arm written, at `True`.
const RECURSIONS_WRITING_BASE: &str = r#"
#{node = Rec, proof = true}
let /fbox : (p: #{node = InductType} /Pbox()(/Box/box(/Wit/b()))) -> #{node = InductType} /std/Bool/True() =
    (p: #{node = InductType} /Pbox()(/Box/box(/Wit/b()))) =>
        #{goal = #{node = InductType} /std/Bool/True()}
        match p
        | base() => /std/Bool/True/qed()
        | step(q) => /fbox(q)
        end;
#{node = Rec, proof = true}
let /ftuple : (p: #{node = InductType} /Ptuple()((/Wit/b(), /Wit/b()))) -> #{node = InductType} /std/Bool/True() =
    (p: #{node = InductType} /Ptuple()((/Wit/b(), /Wit/b()))) =>
        #{goal = #{node = InductType} /std/Bool/True()}
        match p
        | base() => /std/Bool/True/qed()
        | step(q) => /ftuple(q)
        end;
#{node = Rec, proof = true}
let /fopt : (p: #{node = InductType} /Popt()(/Opt/some(@#{node = InductType} /Wit(), /Wit/b()))) -> #{node = InductType} /std/Bool/True() =
    (p: #{node = InductType} /Popt()(/Opt/some(@#{node = InductType} /Wit(), /Wit/b()))) =>
        #{goal = #{node = InductType} /std/Bool/True()}
        match p
        | base() => /std/Bool/True/qed()
        | step(q) => /fopt(q)
        end;
#{node = Rec, proof = true}
let /fhigh : (p: #{node = InductType} /Phigh()(/High/hb())) -> #{node = InductType} /std/Bool/True() =
    (p: #{node = InductType} /Phigh()(/High/hb())) =>
        #{goal = #{node = InductType} /std/Bool/True()}
        match p
        | base() => /std/Bool/True/qed()
        | step(q) => /fhigh(q)
        end;
#{node = Rec, proof = true}
let /fgroup : (p: #{node = InductType} /Pgroup()(/G/gb(/G2/d()))) -> #{node = InductType} /std/Bool/True() =
    (p: #{node = InductType} /Pgroup()(/G/gb(/G2/d()))) =>
        #{goal = #{node = InductType} /std/Bool/True()}
        match p
        | base() => /std/Bool/True/qed()
        | step(q) => /fgroup(q)
        end;
#{node = Rec, proof = true}
let /fstated : (p: #{node = InductType} /Pwit()(/Wit/b())) -> #{node = InductType} /std/Bool/True() =
    (p: #{node = InductType} /Pwit()(/Wit/b())) =>
        #{goal = #{node = InductType} /std/Bool/True(), cases = {stated = #{node = InductType} /Pwit()(/Wit/a())}}
        match p
        | base() => /std/Bool/True/qed()
        | step(q) => /fstated(q)
        end;
#{node = Rec, proof = true}
let /falias : (p: /Alias(/Wit/b())) -> #{node = InductType} /std/Bool/True() =
    (p: /Alias(/Wit/b())) =>
        #{goal = #{node = InductType} /std/Bool/True()}
        match p
        | base() => /std/Bool/True/qed()
        | step(q) => /falias(q)
        end;
/fbox(/Pbox/base())
"#;

// The size order reads each `step(q)` as a descent, so a recursion that omits `base` is refused by coverage alone, at every shape an index carries its proof in: no clash is concluded between two of a proposition's constructors at any depth of an index, through a tuple, a type family's payload or a group, at any level, nor at a stated instance or a type a definition spells. The control writes `base`'s arm and every recursion stands, which is what makes each refusal coverage's and not the descent's. `curios`'s `tests::rules::totality_tests` puts the shapes a program can write to the elaborator under this name.
//
// Mutation-checked at `unify_node` (`curios-analysis/src/invert.rs`): with its `Prop`-sorted family answering by tags, every `base` is excused and no recursion above is refused.
#[test]
fn no_index_holding_a_proof_excuses_the_arm_a_recursion_on_a_propositions_case_omits() {
    let members = [
        ("fbox", "Pbox"),
        ("ftuple", "Ptuple"),
        ("fopt", "Popt"),
        ("fhigh", "Phigh"),
        ("fgroup", "Pgroup"),
        ("fstated", "Pwit"),
        ("falias", "Pwit"),
    ];

    let omitted = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${INDICES_HOLDING_PROOFS}\n${RECURSIONS_OMITTING_BASE}",
        PROPOSITIONS,
        INDICES_HOLDING_PROOFS,
        RECURSIONS_OMITTING_BASE
    ));
    for (member, family) in members {
        assert!(
            omitted
                .iter()
                .any(|verdict| verdict.name == Some(global(member))
                    && verdict.error
                        == Error::MissingArm {
                            family: global(family),
                            tag: Atom::from("base"),
                        }),
            "{member}: {omitted:?}"
        );
    }

    let written = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${INDICES_HOLDING_PROOFS}\n${RECURSIONS_WRITING_BASE}",
        PROPOSITIONS,
        INDICES_HOLDING_PROOFS,
        RECURSIONS_WRITING_BASE
    ));
    for (member, _) in members {
        assert!(
            !written
                .iter()
                .any(|verdict| verdict.name == Some(global(member))),
            "{member}: {written:?}"
        );
    }
}

/// A value of a case its family does not declare is refused as that case, by the family and the tag: the family is declared, and naming it undeclared would send a reader to look for a declaration that is there. Mutation-checked: with the lookup refusing the family as undeclared, the error is `Undeclared`.
#[test]
fn a_value_of_a_case_its_family_does_not_declare_is_refused_by_the_tag() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n#{node = Variant} /std/Bool/False/mk()\n",
        PROPOSITIONS
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict {
                name: None,
                error: Error::UndeclaredCase { family, tag },
            }] if *family == Global::Authored(Qualifier::from(["std", "Bool", "False"]))
                && tag.to_string() == "mk"
        ),
        "{verdicts:?}"
    );
}

/// Each index meets its family's one case at some value: two lists of proofs apart only in their order, a list of pairs apart only in a proof, an open element, a spread, an append and a window against `[0]`, and a binder forced at two positions to two lists of proofs and to a symbolic byte beside a literal one. The term the module closes with types at `False` through `/swapped`.
const TWO_LISTS_THAT_MEET_THEIR_CASE_AT_SOME_VALUE: &str = r#"
induct /Pr: pub Prop
| a()
| b()
end
induct /Ix: (l: #ListType(#{node = InductType} /Pr())) -> pub Type
| mk(): (#List(#{node = InductType} /Pr(), [/Pr/a(), /Pr/b()]))
end
induct /Mx: (l: #ListType({#{node = InductType} /Pr(), #NatType})) -> pub Type
| mk(): (#List({#{node = InductType} /Pr(), #NatType}, [(/Pr/a(), 0)]))
end
induct /Ln: (l: #ListType(#NatType)) -> pub Type
| one(): (#List(#NatType, [0]))
end
induct /D: (x: #ListType(#{node = InductType} /Pr()), y: #ListType(#{node = InductType} /Pr())) -> pub Type
| dup(l: #ListType(#{node = InductType} /Pr())): (l, l)
end
induct /E: (x: #BytesType, y: #BytesType) -> pub Type
| dup(l: #BytesType): (l, l)
end
let /swapped : (
    v: #{node = InductType} /Ix()(#List(#{node = InductType} /Pr(), [/Pr/b(), /Pr/a()]))) -> /std/Bool/False =
    (v: #{node = InductType} /Ix()(#List(#{node = InductType} /Pr(), [/Pr/b(), /Pr/a()]))) =>
        #{goal = /std/Bool/False} match v end;
let /mixed : (
    v: #{node = InductType} /Mx()(#List({#{node = InductType} /Pr(), #NatType}, [(/Pr/b(), 0)]))) -> /std/Bool/False =
    (v: #{node = InductType} /Mx()(#List({#{node = InductType} /Pr(), #NatType}, [(/Pr/b(), 0)]))) =>
        #{goal = /std/Bool/False} match v end;
let /open : (x: #NatType, v: #{node = InductType} /Ln()(#List(#NatType, [x]))) -> /std/Bool/False =
    (x: #NatType, v: #{node = InductType} /Ln()(#List(#NatType, [x]))) =>
        #{goal = /std/Bool/False} match v end;
let /spread : (
    x: #NatType,
    xs: #ListType(#NatType),
    v: #{node = InductType} /Ln()(#ListConcat(#NatType, [#List(#NatType, [x]), xs]))) -> /std/Bool/False =
    (
        x: #NatType,
        xs: #ListType(#NatType),
        v: #{node = InductType} /Ln()(#ListConcat(#NatType, [#List(#NatType, [x]), xs]))) =>
        #{goal = /std/Bool/False} match v end;
let /appended : (
    x: #NatType,
    xs: #ListType(#NatType),
    v: #{node = InductType} /Ln()(#ListAppend(#NatType, xs, x))) -> /std/Bool/False =
    (
        x: #NatType,
        xs: #ListType(#NatType),
        v: #{node = InductType} /Ln()(#ListAppend(#NatType, xs, x))) =>
        #{goal = /std/Bool/False} match v end;
let /window : (
    xs: #ListType(#NatType),
    s: #NatType,
    p: /std/Bool/Holds(#NatLe(#NatAdd(s, 1), #ListLen(#NatType, xs))),
    v: #{node = InductType} /Ln()(#ListSlice(#NatType, xs, s, 1, p))) -> /std/Bool/False =
    (
        xs: #ListType(#NatType),
        s: #NatType,
        p: /std/Bool/Holds(#NatLe(#NatAdd(s, 1), #ListLen(#NatType, xs))),
        v: #{node = InductType} /Ln()(#ListSlice(#NatType, xs, s, 1, p))) =>
        #{goal = /std/Bool/False} match v end;
let /twice : (
    v: #{node = InductType} /D()(#List(#{node = InductType} /Pr(), [/Pr/a()]), #List(#{node = InductType} /Pr(), [/Pr/b()]))) -> /std/Bool/False =
    (v: #{node = InductType} /D()(#List(#{node = InductType} /Pr(), [/Pr/a()]), #List(#{node = InductType} /Pr(), [/Pr/b()]))) =>
        #{goal = /std/Bool/False} match v end;
let /bytes : (c: #ByteType, v: #{node = InductType} /E()(#BytesAppend(x[], c), x[0x01])) -> /std/Bool/False =
    (c: #ByteType, v: #{node = InductType} /E()(#BytesAppend(x[], c), x[0x01])) =>
        #{goal = /std/Bool/False} match v end;
/swapped(#{node = Variant} /Ix/mk())
"#;

// Two `List` literals of one length are decided pointwise and two whose lengths differ by their lengths, and neither reads a clash where a value meets the case: two of a proposition's constructors are one value wherever they stand, in a list, in a pair or forced twice, and a chunk, an appended base and a window hold lengths no literal fixes. So each case stays mandatory, and the eight refusals are all that stand between the module and `False`. `curios`'s `tests::rules::coverage_tests` puts the program to the elaborator under this name, with its control. Mutation-checked: with the `Variant` arm of `Unifier::unify_node` (`curios-analysis/src/invert.rs`) deciding a proposition's constructors by their tags, the declarations over lists of proofs are excused and this fails.
#[test]
fn two_lists_that_meet_their_case_at_some_value_are_no_clash() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${TWO_LISTS_THAT_MEET_THEIR_CASE_AT_SOME_VALUE}",
        PROPOSITIONS,
        TWO_LISTS_THAT_MEET_THEIR_CASE_AT_SOME_VALUE
    ));
    let missing = |name: &str, family: &str, tag: &str| {
        verdicts.iter().any(|verdict| {
            verdict.name == Some(global(name))
                && verdict.error
                    == Error::MissingArm {
                        family: global(family),
                        tag: Atom::from(tag),
                    }
        })
    };

    for (name, family, tag) in [
        ("swapped", "Ix", "mk"),
        ("mixed", "Mx", "mk"),
        ("open", "Ln", "one"),
        ("spread", "Ln", "one"),
        ("appended", "Ln", "one"),
        ("window", "Ln", "one"),
        ("twice", "D", "dup"),
        ("bytes", "E", "dup"),
    ] {
        assert!(missing(name, family, tag), "{name}: {verdicts:?}");
    }
    assert_eq!(verdicts.len(), 8, "{verdicts:?}");
}

/// A value of a case holding one payload past its case's, and an arm binding one past it: read by a zip, the value would type as `mk(true)` and the arm would open its second binder at the `false` reduction carries.
const A_VALUE_AND_AN_ARM_ONE_PAYLOAD_LONG: &str = r#"
induct /Box: pub Type
| mk(x: #BoolType)
end
let /pick : (b: #{node = InductType} /Box()) -> #BoolType =
    (b: #{node = InductType} /Box()) =>
        #{goal = #BoolType}
        match b
        | mk(x, y) => y
        end;
let /forged : /std/Bool/Holds(/pick(#{node = Variant} /Box/mk(true, false))) =
    /std/Bool/True/qed();
/forged
"#;

// A constructor value's payload and an arm's binders are each counted against the case's signature, so the long value and the long arm are refused by their counts, under the names that hold them.
#[test]
fn a_value_and_an_arm_one_payload_past_their_case_are_refused_by_their_counts() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_VALUE_AND_AN_ARM_ONE_PAYLOAD_LONG}",
        PROPOSITIONS,
        A_VALUE_AND_AN_ARM_ONE_PAYLOAD_LONG
    ));
    let counted = |name: &str, what: Counted| {
        verdicts.iter().any(|verdict| {
            verdict.name == Some(global(name))
                && matches!(
                    verdict.error,
                    Error::Arity { counted, expected: 1, actual: 2 } if counted == what
                )
        })
    };

    assert!(counted("pick", Counted::ArmBinders), "{verdicts:?}");
    assert!(counted("forged", Counted::Payload), "{verdicts:?}");
}

/// `/C` holds a code, as `/std/Char` does, and `character` dispatches on it, the projection a character pattern reads, with `qed` at `Holds(c.0 == 97)` in the default. Read at the `97` arm's instance, the default would hold it, and `character(C { 98 })`, which the module closes with, would be a proof of `False`.
const A_CHARACTER_DISPATCH_DEFAULT: &str = r#"
struct /C: pub Type { code: #NatType }

#{proof = true}
let /character: (c: #{node = StructType} /C()) -> /std/Bool/Holds(
    #NatEql(#{field = Struct(/C, 0)} c.0, 97)) =
    (c: #{node = StructType} /C()) =>
        #{goal = /std/Bool/Holds(#NatEql(#{field = Struct(/C, 0)} c.0, 97))}
        match #{field = Struct(/C, 0)} c.0
        | 97 => /std/Bool/True/qed()
        | _ => /std/Bool/True/qed()
        end;

/character(/C { 98 })
"#;

// A dispatch on a projection records its equation on the projection and refines nothing in its default, which is checked at the scrutinee itself: `qed` is refused there, under `character`, and the closing term, which stands at `False`, is admitted, so `character`'s refusal is all that stands between the module and `False`.
#[test]
fn a_character_dispatch_default_is_checked_at_its_scrutinee() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_CHARACTER_DISPATCH_DEFAULT}",
        PROPOSITIONS,
        A_CHARACTER_DISPATCH_DEFAULT
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(character), error: Error::Mismatch { .. } }] if *character == global("character")
        ),
        "{verdicts:?}"
    );
}

/// `/S` holds bytes, as `/std/Str` does, and `string` splits them, the projection a string pattern reads, with `qed` at `Holds(s.0 == x[])` in the cons arm. Read as no case of its own, the cons arm would hold it, and `string(S { x[0x61] })` would be a proof of `False`.
const A_STRING_RUNS_CONS_ARM: &str = r#"
struct /S: pub Type { bytes: #BytesType }

#{proof = true}
let /string: (s: #{node = StructType} /S()) -> /std/Bool/Holds(
    #BytesEql(#{field = Struct(/S, 0)} s.0, x[])) =
    (s: #{node = StructType} /S()) =>
        #{goal = /std/Bool/Holds(#BytesEql(#{field = Struct(/S, 0)} s.0, x[]))}
        match #{field = Struct(/S, 0)} s.0
        | x[] => /std/Bool/True/qed()
        | x[b, ..rest] => /std/Bool/True/qed()
        end;

/string(/S { x[0x61] })
"#;

// A split of a projected run checks its cons arm at a byte ahead of a tail, where the bytes are no empty run: `qed` is refused there, under `string`, and the closing term is admitted.
#[test]
fn a_string_runs_cons_arm_is_checked_at_the_cons() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_STRING_RUNS_CONS_ARM}",
        PROPOSITIONS,
        A_STRING_RUNS_CONS_ARM
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(string), error: Error::Mismatch { .. } }] if *string == global("string")
        ),
        "{verdicts:?}"
    );
}

/// A guard on a character's code, as `| x, x == 'a' =>` compiles: the rows after the arm are the `false` arm, which states `qed` at `Holds(c.0 == 97)`. Read where the guard held, `guarded(C { 98 })` would be a proof of `False`.
const A_GUARDS_FALL_THROUGH: &str = r#"
struct /C: pub Type { code: #NatType }

#{proof = true}
let /guarded: (c: #{node = StructType} /C()) -> /std/Bool/Holds(
    #NatEql(#{field = Struct(/C, 0)} c.0, 97)) =
    (c: #{node = StructType} /C()) =>
        #{goal = /std/Bool/Holds(#NatEql(#{field = Struct(/C, 0)} c.0, 97))}
        match #NatEql(#{field = Struct(/C, 0)} c.0, 97)
        | false => /std/Bool/True/qed()
        | true => /std/Bool/True/qed()
        end;

/guarded(/C { 98 })
"#;

// The `false` arm is checked at the guard's failure, where the goal is `Holds(false)`: `qed` is refused there, under `guarded`, and the closing term is admitted.
#[test]
fn a_guards_fall_through_is_checked_where_the_guard_failed() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_GUARDS_FALL_THROUGH}",
        PROPOSITIONS,
        A_GUARDS_FALL_THROUGH
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(guarded), error: Error::Mismatch { .. } }] if *guarded == global("guarded")
        ),
        "{verdicts:?}"
    );
}

/// `x`'s type is `/C` only under `g(n) = true`, where `reduced` dispatches on `x.0` and states `qed` in the default at `Holds(x.0 == 97)`. Read at the `97` arm's instance, `reduced` at `g := (m) => m == 0`, `n := 0` and `x := C { 98 }`, which the module closes with, would be a proof of `False`.
const A_DISPATCH_ON_A_TYPE_AN_ARM_REDUCES: &str = r#"
struct /C: pub Type { code: #NatType }

let /T: (b: #BoolType) -> Type =
    (b: #BoolType) =>
        #{goal = Type}
        match b
        | false => #NatType
        | true => #{node = StructType} /C()
        end;

#{proof = true}
let /reduced: (
    g: (#NatType) -> #BoolType,
    n: #NatType,
    x: /T(g(n))) -> /std/Bool/Holds(
    #{goal = #BoolType}
    match g(n)
    | false => true
    | true => #NatEql(#{field = Struct(/C, 0)} x.0, 97)
    end) =
    (g: (#NatType) -> #BoolType, n: #NatType, x: /T(g(n))) =>
        #{
            goal = /std/Bool/Holds(
                #{goal = #BoolType}
                match g(n)
                | false => true
                | true => #NatEql(#{field = Struct(/C, 0)} x.0, 97)
                end)}
        match g(n)
        | false => /std/Bool/True/qed()
        | true =>
            #{
                goal = /std/Bool/Holds(
                    #{goal = #BoolType}
                    match true
                    | false => true
                    | true => #NatEql(#{field = Struct(/C, 0)} x.0, 97)
                    end)}
            match #{field = Struct(/C, 0)} x.0
            | 97 => /std/Bool/True/qed()
            | _ => /std/Bool/True/qed()
            end
        end;

/reduced((m: #NatType) => #NatEql(m, 0), 0, /C { 98 })
"#;

// The projection is typed where the outer arm's equation makes `x` a `/C`, and the inner default is checked at the scrutinee itself: `qed` is refused there, under `reduced`, and the closing term is admitted.
#[test]
fn a_character_dispatch_on_a_type_an_arm_reduces_is_checked_at_its_scrutinee() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_DISPATCH_ON_A_TYPE_AN_ARM_REDUCES}",
        PROPOSITIONS,
        A_DISPATCH_ON_A_TYPE_AN_ARM_REDUCES
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(reduced), error: Error::Mismatch { .. } }] if *reduced == global("reduced")
        ),
        "{verdicts:?}"
    );
}
