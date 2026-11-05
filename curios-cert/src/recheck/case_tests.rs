//! A case read at its scrutinee's own instance: the parameters it opens at, the arms that instance excuses, the variable an arm refines and the motive it is checked at, each of a module the elaborator would not build, written as Core text and put to the kernel with nothing in scope, declaring the propositions it uses.

use {
    super::test_support::*,
    crate::*,
    curios_common::fill,
    curios_core::{Atom, Term},
};

/// `Box`'s case states its family's parameter as a proposition where the family declares a type, so read off the case as written its payload is a proof and `mk(true)` is `mk(false)`: `cast` carries `Code`'s `True` at the one to its `False` at the other, and the term the module closes with is `False`.
const A_CASE_STATING_ITS_PARAMETER_AS_A_PROPOSITION: &str = r#"
#{absent = [/Box/mk]}
induct /Box(A: Type): pub Type
| #{params = [@Prop]} mk(x: A)
end
let /Code : (
    b:
        #{node = InductType}
        /Box(#BoolType)
) -> Prop =
    (
        b:
            #{node = InductType}
            /Box(#BoolType)
    ) =>
        #{result = Ambient(Prop)}
        match b
        | mk(x) =>
            /std/Bool/Holds(x)
        end;
let /cast : (
    F: (
        b:
            #{node = InductType}
            /Box(#BoolType)
    ) -> Prop,
    y: F(
        #{node = Variant}
        /Box/mk(@#BoolType, true)
    )
) -> F(
    #{node = Variant}
    /Box/mk(@#BoolType, false)
) =
    (
        F: (
            b:
                #{node = InductType}
                /Box(#BoolType)
        ) -> Prop,
        y: F(
            #{node = Variant}
            /Box/mk(@#BoolType, true)
        )
    ) =>
        y;
/cast(/Code, /std/Bool/True/qed())
"#;

// A case's parameters are its family's: `check_constructed` holds the prefix a case states to the family's arity, so `Box` is refused where it is formed, and conversion reads `mk`'s payload at the parameters of the value it compares, a `Bool`, so `cast` is refused too. The term the module closes with types at `False` through `cast`'s stated type, so those two refusals are all that stand between the module and `False`.
#[test]
fn a_case_whose_parameter_is_stated_a_proposition_is_refused_where_its_family_is_formed() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_CASE_STATING_ITS_PARAMETER_AS_A_PROPOSITION}",
        PROPOSITIONS,
        A_CASE_STATING_ITS_PARAMETER_AS_A_PROPOSITION
    ));

    assert!(
        matches!(
            &verdicts[..],
            [
                Verdict { name: Some(family), error: Error::Mismatch { inferred, expected } },
                Verdict { name: Some(cast), error: Error::Mismatch { .. } },
            ] if *family == global("Box")
                && **inferred == Term::prop()
                && **expected == Term::type_ground()
                && *cast == global("cast")
        ),
        "{verdicts:?}"
    );
}

/// `Pair`'s case states one of its family's two parameters and names the other in its payload by the head's word, so its telescope binds `A` and `C` and leaves `B` a local nothing in it binds.
const A_CASE_NAMING_A_PARAMETER_IT_DOES_NOT_STATE: &str = r#"
#{absent = [/Pair/mk]}
induct /Pair(A: Type, B: Type): pub Type
| #{params = [@Type]} mk(C: Type, y: B)
end
/std/Bool/False
"#;

// The free local is refused at the boundary every position is scanned at before any rule runs, a registry entry's telescopes among them, where a judgment would have read it as whatever binder a comparison mints next.
#[test]
fn a_case_that_names_a_parameter_it_does_not_state_is_refused_at_the_boundary() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_CASE_NAMING_A_PARAMETER_IT_DOES_NOT_STATE}",
        PROPOSITIONS,
        A_CASE_NAMING_A_PARAMETER_IT_DOES_NOT_STATE
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("Pair"))
                && matches!(verdict.error, Error::Unbound(_))),
        "{verdicts:?}"
    );
}

/// `A` and `B` are one group, each with a parameter named `n`, a `Nat` in one and a `Bool` in the other. `mkA`'s payload states `n == 1` of its own family's `n`, so an `A(0)` holds a `Holds(0 == 1)`; read at the sibling's parameter, or at another instance's, the `qed` it is built with would fit, and the match would hand it on at `False`.
const A_CASE_OF_A_FAMILY_IN_A_GROUP: &str = r#"
#{absent = [/A/mkA]}
induct /A(n: #NatType): pub Type
| mkA(
    h: /std/Bool/Holds(#NatEql(n, 1)),
    other:
        #{node = InductType}
        /B(true)
)
end
#{absent = [/B/mkB, /B/base]}
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
    match x: (x2) => /std/Bool/False
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
#{universe_context = {parameter_count = 1}, absent = [/Lift/mk]}
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
        #{
            cases = {
                stated =
                    #{node = InductType, universes = {levels = [${level}]}}
                    /Lift()(X)}}
        match p: (T, s) => #{level = 1} Type
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
#{absent = [/Two/a, /Two/b]}
induct /Two: pub Prop
| a()
| b()
end
#{absent = [/T/ta, /T/tb]}
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
        #{result = Ambient(Prop)}
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
    match t: (x, s) => /Code(x, s)
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
#{absent = [/Two/a, /Two/b]}
induct /Two: pub Prop
| a()
| b()
end
#{absent = [/Pick/l, /Pick/r]}
induct /Pick: pub Type
| l()
| r()
end
let /prop :
    #{node = InductType}
    /std/Bool/True() =
    match
        #{node = Variant}
        /Two/a(): (s) =>
        #{node = InductType}
        /std/Bool/True()
    | a() =>
        /std/Bool/True/qed()
    | b() =>
        true
    end;
let /data :
    #{node = InductType}
    /std/Bool/True() =
    match
        #{node = Variant}
        /Pick/l(): (s) =>
        #{node = InductType}
        /std/Bool/True()
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
#{absent = [/Code/big]}
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
            match c2: (T, s) => (a: T) -> T
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

/// `Box` has a parameter and no index, so its motive binds the scrutinee alone; this one also binds the parameter, as a motive abstracting it would, and opened at the arm's one case value it has a binder no term stands for.
const A_MOTIVE_BINDING_A_PARAMETER: &str = r#"
#{absent = [/Box/mk]}
induct /Box(A: Type): pub Type
| mk(x: A)
end
((
    b:
        #{node = InductType}
        /Box(#BoolType)
) =>
    match b: (A, s) => #BoolType
    | mk(x) =>
        x
    end)(
    #{node = Variant}
    /Box/mk(@#BoolType, true)
)
"#;

// A motive binds one name per index and then the scrutinee, a count `infer` holds it to before the motive is opened anywhere.
#[test]
fn a_motive_that_binds_a_parameter_is_refused() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_MOTIVE_BINDING_A_PARAMETER}",
        PROPOSITIONS,
        A_MOTIVE_BINDING_A_PARAMETER
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict {
                name: None,
                error: Error::Arity {
                    counted: Counted::MotiveBinders,
                    expected: 1,
                    actual: 2,
                },
            }]
        ),
        "{verdicts:?}"
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
        #{result = Ambient(#NatType)}
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
        #{result = Ambient(#NatType)}
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
        #{result = Ambient(#NatType)}
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
        #{result = Ambient(#BoolType)}
        match n
        | 0 =>
            true
        | k + 1; x =>
            false
        end;
let /excused : (n: #NatType) ->
    #{node = InductType}
    /std/Bool/True() =
    (n: #NatType) =>
        #{
            result = Ambient(
                #{node = InductType}
                /std/Bool/True())}
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
            result = Ambient(
                #{node = InductType}
                /std/Bool/True())}
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
            result = Ambient(
                #{node = InductType}
                /std/Bool/True())}
        match /zero(n)
        | false =>
            #{node = Variant}
            /std/Bool/True/qed()
        | true =>
            0
        end;
/std/Bool/False
"#;

// The elaborator refuses all three arms, as a reader wrote them; the kernel withdraws a refused arm only where its scrutinee computes to another case, so it is one no reduction takes, which `excused`'s is and `taken`'s and `undecided`'s are not.
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
