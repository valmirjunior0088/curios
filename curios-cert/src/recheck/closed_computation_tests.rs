//! A closed computation the machine answers, held to what reduction answers of the same term open in a variable: each module states a lemma whose type is open in `c`, which the recursive strategy reduces to `Holds(c)`, and closes with the lemma at `false` over `qed`, whose instance is closed and the machine's. Where the machine answered `true` the closing term would be `False`.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::test_support::*,
    curios_common::fill,
    curios_core::{Global, ReduceError, Term},
};

/// What `qed` is inferred at: `True`, as the registry this crate's suites judge under names it.
fn true_type() -> Term {
    Term::induct_type(
        Global::Authored(SYNTAX.proof.true_type.qualifier()),
        Vec::<Term>::new(),
        Vec::<Term>::new(),
    )
}

/// Whether `verdicts` is the closing term's refusal alone, `qed`'s `True` against an instance that did not reduce to it: the lemma admitted, so the strategy answered `c`, and the machine answering the instance otherwise than `true`.
fn the_closing_instance_alone_is_refused(verdicts: &[Verdict]) -> bool {
    matches!(
        verdicts,
        [Verdict { name: None, error: Error::Mismatch { inferred, .. } }] if **inferred == true_type()
    )
}

/// `/D` at level 2 is a type whose unfolding overflows a level's offset: `D.{2}` is well typed at `Type 1`, and reducing it is refused by `Reduce`.
const AN_ARGUMENT_THAT_ERRORS_BESIDE_THE_ONE_READ: &str = r#"
#{universe_context = {parameter_count = 1}}
let /D : #{level = 1} Type =
    ((x: #{level = u + 4294967294} Type) => #BoolType)(#NatType);
let /lemma : (
    c: #BoolType,
    h: /std/Bool/Holds(
        ((x: #{level = 1} Type, b: #BoolType) => b)(
            #{universes = {levels = [2]}}
            /D,
            c
        )
    )
) -> /std/Bool/Holds(c) =
    (
        c: #BoolType,
        h: /std/Bool/Holds(
            ((x: #{level = 1} Type, b: #BoolType) => b)(
                #{universes = {levels = [2]}}
                /D,
                c
            )
        )
    ) =>
        h;
/lemma(false, /std/Bool/True/qed())
"#;

// The machine evaluates a substituend before it substitutes it, and one whose evaluation errors falls back to its spelling in its own slot, as the strategy substitutes it as written: `b` reads `c`, and the closing instance is `Holds(false)`. A fallback that stood the spelling, or the error, anywhere else would answer `b` otherwise, and one that let the error through refuses the closing term by `Reduce`, which is the mutation check: `Machine::catch` returning every error outright.
#[test]
fn a_substituend_whose_evaluation_errors_falls_back_in_its_own_slot() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${AN_ARGUMENT_THAT_ERRORS_BESIDE_THE_ONE_READ}",
        PROPOSITIONS,
        AN_ARGUMENT_THAT_ERRORS_BESIDE_THE_ONE_READ
    ));

    assert!(
        the_closing_instance_alone_is_refused(&verdicts),
        "{verdicts:?}"
    );
}

/// The proof is a fold over `10^11`, which no budget evaluates.
const A_PROOF_BOUND_AS_WRITTEN_BESIDE_THE_DATA_READ: &str = r#"
induct /Pk: pub Type
| mk(p: /std/Bool/Holds(true), b: #BoolType)
end
let /lemma : (
    c: #BoolType,
    h: /std/Bool/Holds(
        #{result = Ambient(#BoolType)}
        match /Pk/mk(
            match 100000000000: (x2) => /std/Bool/Holds(true)
            | 0 =>
                /std/Bool/True/qed()
            | k + 1; x =>
                x
            end,
            c
        )
        | mk(p, b) =>
            let q: /std/Bool/Holds(true) = p;
            b
        end
    )
) -> /std/Bool/Holds(c) =
    (
        c: #BoolType,
        h: /std/Bool/Holds(
            #{result = Ambient(#BoolType)}
            match /Pk/mk(
                match 100000000000: (x2) => /std/Bool/Holds(true)
                | 0 =>
                    /std/Bool/True/qed()
                | k + 1; x =>
                    x
                end,
                c
            )
            | mk(p, b) =>
                let q: /std/Bool/Holds(true) = p;
                b
            end
        )
    ) =>
        h;
/lemma(false, /std/Bool/True/qed())
"#;

// The machine leaves a proof as written where its binder says it binds one: the constructor's wrapper at its proof binder, the arm at the payload `Pk`'s declaration states is a proof, and the `let` at its proof binder, so the fold no budget evaluates is never run and `b` reads `c`. Mutation checks: `ClosedHost::payload_proofs` answering no marks for the kernel, and `Machine::let_advance` ignoring `binds_proof`, each run the proof and refuse the closing term by `Reduce`.
#[test]
fn a_proof_payload_and_a_let_bound_proof_are_left_as_written_beside_the_data_read() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_PROOF_BOUND_AS_WRITTEN_BESIDE_THE_DATA_READ}",
        PROPOSITIONS,
        A_PROOF_BOUND_AS_WRITTEN_BESIDE_THE_DATA_READ
    ));

    assert!(
        the_closing_instance_alone_is_refused(&verdicts),
        "{verdicts:?}"
    );
}

/// `/W` at level 0 is a fold over `10^11`, which no budget evaluates.
const A_WITNESS_AT_AN_INSTANCE_BESIDE_THE_DATA_READ: &str = r#"
#{universe_context = {parameter_count = 1}}
let /W : #NatType =
    match 100000000000: (x2) => #NatType
    | 0 =>
        0
    | k + 1; x =>
        x
    end;
let /lemma : (
    c: #BoolType,
    h: /std/Bool/Holds(
        ((use w: #NatType, b: #BoolType) => b)(
            use
            #{universes = {levels = [0]}}
            /W,
            c
        )
    )
) -> /std/Bool/Holds(c) =
    (
        c: #BoolType,
        h: /std/Bool/Holds(
            ((use w: #NatType, b: #BoolType) => b)(
                use
                #{universes = {levels = [0]}}
                /W,
                c
            )
        )
    ) =>
        h;
/lemma(false, /std/Bool/True/qed())
"#;

// A name at a universe instance passed at a `use` position is bound as that name, as the strategy substitutes it, so its value is never run and `b` reads `c`. Passed at a plain position the same instance is evaluated, and the closing term is refused by `Reduce`; the mutation check is `Machine::beta` marking no witness written, which refuses it so here too.
#[test]
fn a_witness_named_at_an_instance_is_left_as_written_beside_the_data_read() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_WITNESS_AT_AN_INSTANCE_BESIDE_THE_DATA_READ}",
        PROPOSITIONS,
        A_WITNESS_AT_AN_INSTANCE_BESIDE_THE_DATA_READ
    ));

    assert!(
        the_closing_instance_alone_is_refused(&verdicts),
        "{verdicts:?}"
    );
}

/// Each scrutinee is a fold over `10^11`, which no budget evaluates: a proof of `R2(5, 9)`, whose indices pin both payloads, and a proof of `Same(true, true)`, matched by an arm that names no payload.
const MATCHES_THAT_READ_NOTHING_OF_THEIR_PROOF: &str = r#"
induct /Same: (x: #BoolType, y: #BoolType) -> pub Prop
| refl(@z: #BoolType): (z, z)
end
induct /R2: (n: #NatType, m: #NatType) -> pub Prop
| mk(x: #NatType, y: #NatType): (x, y)
end
let /lemma : (
    c: #BoolType,
    h: /std/Bool/Holds(
        #{
            result = Ambient(#BoolType),
            cases = {
                stated =
                    #{node = InductType}
                    /R2()(5, 9)}}
        match
            match 100000000000: (x2) =>
                #{node = InductType}
                /R2()(5, 9)
            | 0 =>
                /R2/mk(5, 9)
            | k + 1; x3 =>
                x3
            end
        | mk(x, y) =>
            #BoolOr(
                #BoolOr(#NatNeq(x, 5), #NatNeq(y, 9)),
                #{
                    result = Ambient(#BoolType),
                    cases = {
                        stated =
                            #{node = InductType}
                            /Same()(true, true)}}
                match
                    match 100000000000: (x4) =>
                        #{node = InductType}
                        /Same()(true, true)
                    | 0 =>
                        /Same/refl(@true)
                    | k2 + 1; x5 =>
                        x5
                    end
                | refl(@z) =>
                    c
                end
            )
        end
    )
) -> /std/Bool/Holds(c) =
    (
        c: #BoolType,
        h: /std/Bool/Holds(
            #{
                result = Ambient(#BoolType),
                cases = {
                    stated =
                        #{node = InductType}
                        /R2()(5, 9)}}
            match
                match 100000000000: (x2) =>
                    #{node = InductType}
                    /R2()(5, 9)
                | 0 =>
                    /R2/mk(5, 9)
                | k + 1; x3 =>
                    x3
                end
            | mk(x, y) =>
                #BoolOr(
                    #BoolOr(#NatNeq(x, 5), #NatNeq(y, 9)),
                    #{
                        result = Ambient(#BoolType),
                        cases = {
                            stated =
                                #{node = InductType}
                                /Same()(true, true)}}
                    match
                        match 100000000000: (x4) =>
                            #{node = InductType}
                            /Same()(true, true)
                        | 0 =>
                            /Same/refl(@true)
                        | k2 + 1; x5 =>
                            x5
                        end
                    | refl(@z) =>
                        c
                    end
                )
            end
        )
    ) =>
        h;
/lemma(false, /std/Bool/True/qed())
"#;

// A match with one arm and no catch-all is its arm with its scrutinee unread where the arm names no payload, and where it names one, bound to what the instance the match states says of it, each payload off the index that pins it: `x != 5 || y != 9` is `false`, and the cast's arm reads `c`. Mutation checks: `ClosedHost::stated_payload` answering `None` runs the proofs, refusing the closing term by `Reduce`; `pinned_instance` reading the binders off the indices in reverse makes the closing instance `true`, which `qed` then inhabits.
#[test]
fn a_match_that_reads_nothing_of_its_proof_answers_as_the_strategy_does() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${MATCHES_THAT_READ_NOTHING_OF_THEIR_PROOF}",
        PROPOSITIONS,
        MATCHES_THAT_READ_NOTHING_OF_THEIR_PROOF
    ));

    assert!(
        the_closing_instance_alone_is_refused(&verdicts),
        "{verdicts:?}"
    );
}

/// A closed match on a proof of `R2(5, 9)` stating that it eliminates `R2(7, 1)`: the machine reads `x := 7` off the stated instance with the scrutinee unread, and `x == 7` would make `qed` its proof.
const A_MATCH_STATING_ANOTHER_INSTANCE: &str = r#"
induct /R2: (n: #NatType, m: #NatType) -> pub Prop
| mk(x: #NatType, y: #NatType): (x, y)
end
let /elsewhere : /std/Bool/Holds(
    #{
        result = Ambient(#BoolType),
        cases = {
            stated =
                #{node = InductType}
                /R2()(7, 1)}}
    match /R2/mk(5, 9)
    | mk(x, y) =>
        #NatEql(x, 7)
    end
) =
    /std/Bool/True/qed();
/elsewhere
"#;

// The machine trusts the instance a closed match states, which typing holds to the scrutinee's own type: `/elsewhere`'s match is refused where it is typed, `R2(5, 9)` against the `R2(7, 1)` it states.
#[test]
fn a_match_stating_an_instance_its_scrutinee_does_not_have_is_refused_where_it_is_typed() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_MATCH_STATING_ANOTHER_INSTANCE}",
        PROPOSITIONS,
        A_MATCH_STATING_ANOTHER_INSTANCE
    ));

    assert!(
        matches!(
            &verdicts[..],
            [
                Verdict { name: Some(elsewhere), error: Error::Mismatch { .. } },
                Verdict { name: None, error: Error::Mismatch { .. } },
            ] if *elsewhere == global("elsewhere")
        ),
        "{verdicts:?}"
    );
}

/// `/second` answers its second argument, and `/pick(x)` names the binder it would be contracted over.
const LITERALS_THAT_ONLY_LOOK_LIKE_ETA_REDEXES: &str = r#"
let /second : (a: #BoolType, b: #BoolType) -> #BoolType =
    (a: #BoolType, b: #BoolType) =>
        b;
let /pick : (a: #BoolType) -> (b: #BoolType) -> #BoolType =
    (a: #BoolType) =>
        (b: #BoolType) =>
            #BoolOr(a, b);
let /lemma : (
    c: #BoolType,
    h: /std/Bool/Holds(
        #BoolOr(
            #BoolOr(
                ((x: #BoolType, y: #BoolType) => /second(y, x))(c, true),
                ((x: #BoolType) => /second(x, x))(c)
            ),
            ((x: #BoolType) => /pick(x)(x))(c)
        )
    )
) -> /std/Bool/Holds(c) =
    (
        c: #BoolType,
        h: /std/Bool/Holds(
            #BoolOr(
                #BoolOr(
                    ((x: #BoolType, y: #BoolType) => /second(y, x))(c, true),
                    ((x: #BoolType) => /second(x, x))(c)
                ),
                ((x: #BoolType) => /pick(x)(x))(c)
            )
        )
    ) =>
        h;
/lemma(false, /std/Bool/True/qed())
"#;

// Eta under the hosts' own probe contracts a literal only where it forwards each of its binders, in order and under its mark, to a head that names none: a literal forwarding them out of order, one forwarding a binder twice and one whose head names its binder are applied as they are, and each reads `c`. Contracted, the first is `/second(c, true)`, which is `true`; the mutation check is `eta_head` asking only that each argument be one of the binders, which admits the closing term.
#[test]
fn a_literal_that_only_looks_like_an_eta_redex_is_applied_as_it_is() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${LITERALS_THAT_ONLY_LOOK_LIKE_ETA_REDEXES}",
        PROPOSITIONS,
        LITERALS_THAT_ONLY_LOOK_LIKE_ETA_REDEXES
    ));

    assert!(
        the_closing_instance_alone_is_refused(&verdicts),
        "{verdicts:?}"
    );
}

/// `/D` as above: `D.{2}` is well typed and its reduction is refused.
const AN_ARM_OVER_A_PARAMETER_AN_IMPLICIT_AND_AN_ERROR: &str = r#"
#{universe_context = {parameter_count = 1}}
let /D : #{level = 1} Type =
    ((x: #{level = u + 4294967294} Type) => #BoolType)(#NatType);
induct /Bx(A: Type): pub #{level = 2} Type
| mk(@n: #BoolType, t: #{level = 1} Type, a: A, b: #BoolType)
end
let /lemma : (
    c: #BoolType,
    h: /std/Bool/Holds(
        #{result = Ambient(#BoolType)}
        match /Bx/mk(
            @#BoolType,
            @true,
            #{universes = {levels = [2]}}
            /D,
            true,
            c
        )
        | mk(@n, t, a, b) =>
            b
        end
    )
) -> /std/Bool/Holds(c) =
    (
        c: #BoolType,
        h: /std/Bool/Holds(
            #{result = Ambient(#BoolType)}
            match /Bx/mk(
                @#BoolType,
                @true,
                #{universes = {levels = [2]}}
                /D,
                true,
                c
            )
            | mk(@n, t, a, b) =>
                b
            end
        )
    ) =>
        h;
/lemma(false, /std/Bool/True/qed())
"#;

// An arm binds the values of a constructed value's payload, the kernel's rule on both sides: in order, past the family's parameter, with the implicit payload ahead of the plain ones and the one whose evaluation errors falling back to its spelling, so `b` reads `c`. The mutation check is `Machine::dispatch` running the payload without catching, which refuses the closing term by `Reduce`.
#[test]
fn an_arm_binds_a_constructed_values_payload_in_order_past_a_parameter_and_an_implicit() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${AN_ARM_OVER_A_PARAMETER_AN_IMPLICIT_AND_AN_ERROR}",
        PROPOSITIONS,
        AN_ARM_OVER_A_PARAMETER_AN_IMPLICIT_AND_AN_ERROR
    ));

    assert!(
        the_closing_instance_alone_is_refused(&verdicts),
        "{verdicts:?}"
    );
}

/// `parity(3)` is 1 and `parity(4)` is 0.
const TWO_CALLS_AT_BOTH_DEMANDS_IN_ONE_RUN: &str = r#"
#{node = Rec}
let /parity : (n: #NatType) -> #NatType =
    (n: #NatType) =>
        #{result = Ambient(#NatType)}
        match n
        | 0 =>
            0
        | k + 1; x =>
            #NatSub(1, /parity(k))
        end;
let /lemma : (
    c: #BoolType,
    h: /std/Bool/Holds(
        #BoolOr(
            #BoolAnd(
                #NatEql(/parity(3), 1),
                #NatEql(
                    #ListFold(#NatType, #NatType, #List(#NatType, []), /parity(3), (a: #NatType, b: #NatType) => b),
                    /parity(4)
                )
            ),
            c
        )
    )
) -> /std/Bool/Holds(c) =
    (
        c: #BoolType,
        h: /std/Bool/Holds(
            #BoolOr(
                #BoolAnd(
                    #NatEql(/parity(3), 1),
                    #NatEql(
                        #ListFold(#NatType, #NatType, #List(#NatType, []), /parity(3), (a: #NatType, b: #NatType) => b),
                        /parity(4)
                    )
                ),
                c
            )
        )
    ) =>
        h;
/lemma(false, /std/Bool/True/qed())
"#;

// One run reaches `parity(3)` forced, as an equation's operand, where it is filed under its value, and plainly, as an empty fold's initial value, where it is its own folded spelling and filed nowhere; the fold's answer is then forced against `parity(4)`. A memo or a `Tap` answering one demand with the other's value, or one call with the other's, would make `1 == 0` hold; each call keeps its own, and the conjunction is `false`.
#[test]
fn two_recursive_calls_reached_at_both_demands_in_one_run_keep_their_own_values() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${TWO_CALLS_AT_BOTH_DEMANDS_IN_ONE_RUN}",
        PROPOSITIONS,
        TWO_CALLS_AT_BOTH_DEMANDS_IN_ONE_RUN
    ));

    assert!(
        the_closing_instance_alone_is_refused(&verdicts),
        "{verdicts:?}"
    );
}

/// `/dispatch` ignores its local argument, so `dispatch(c0)` resolves to `3 <= 2`, which names no local, and the arm at `true` is one reduction never takes.
const AN_ARM_UNDER_A_DISPATCH_RESOLVING_TO_A_CLOSED_COMPARISON: &str = r#"
let /dispatch : (y: #BoolType) -> #BoolType =
    (y: #BoolType) =>
        #NatLe(3, 2);
let /w : {
    f: (c0: #BoolType) -> #BoolType,
    p: /std/Bool/Holds(#NatLe(3, 2))
} =
    (
        (c0: #BoolType) =>
            match /dispatch(c0): (b) => #BoolType
            | false =>
                false
            | true =>
                ((
                    h: /std/Bool/Holds(/dispatch(c0)),
                    j: /std/Bool/Holds(((z: #BoolType) => #NatLe(3, 2))(c0)),
                    k: /std/Bool/Holds(#NatLe(3, 2))
                ) =>
                    true)(/std/Bool/True/qed(), /std/Bool/True/qed(), /std/Bool/True/qed())
            end,
        /std/Bool/True/qed()
    );
/w.1
"#;

// No case equation answers a term the machine takes, so an arm's equation leaves nothing about a closed term behind it: the arm under `dispatch(c0) == true` is refused and withdrawn as one reduction never takes, and `/w`'s second component, checked after it, finds `Holds(3 <= 2)` uninhabited. Remembered for the declaration as the arm's equation would answer it, `3 <= 2` would be `true` there, and `/w.1`, which the module closes with and which stands at `False`, would be a proof of it: the closing term is admitted, and `/w`'s refusal is all that stands between the module and `False`.
#[test]
fn an_arm_whose_dispatch_resolves_to_a_closed_comparison_leaves_nothing_the_declaration_reads_after_it()
 {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${AN_ARM_UNDER_A_DISPATCH_RESOLVING_TO_A_CLOSED_COMPARISON}",
        PROPOSITIONS,
        AN_ARM_UNDER_A_DISPATCH_RESOLVING_TO_A_CLOSED_COMPARISON
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(w), error: Error::Mismatch { .. } }] if *w == global("w")
        ),
        "{verdicts:?}"
    );
}

/// `f(1)` matches on a fold of `[5]` whose step returns its accumulator, so the fold answers `f(0)`, which is `0`, and the match `7`; the same fold over `member(3)` answers `0`, so `c3`'s and `S`'s types are their arms at `0`, and over `pair(3)` it answers `(0, 1)`.
const A_MATCH_OR_A_PROJECTION_ON_A_FOLD_THAT_ANSWERS_A_RECURSIVE_CALL: &str = r#"
#{node = Rec}
let /f : (n: #NatType) -> #NatType =
    (n: #NatType) =>
        #{result = Ambient(#NatType)}
        match n
        | 0 =>
            0
        | k + 1; x =>
            #{result = Ambient(#NatType)}
            match #ListFold(#NatType, #NatType, #List(#NatType, [5]), /f(k), (a: #NatType, b: #NatType) => b)
            | 0 =>
                7
            | _ =>
                9
            end
        end;
let /y : /std/Bool/Holds(#NatEql(/f(1), 7)) =
    /std/Bool/True/qed();
#{node = Rec}
let /member : (n: #NatType) -> #NatType =
    (n: #NatType) =>
        #{result = Ambient(#NatType)}
        match n
        | 0 =>
            0
        | k + 1; x =>
            /member(k)
        end;
let /c3 : #{result = Ambient(Type)}
    match #ListFold(#NatType, #NatType, #List(#NatType, [5]), /member(3), (a: #NatType, b: #NatType) => b)
    | 0 =>
        #BoolType
    | _ =>
        #NatType
    end =
    true;
let /S : #{result = Ambient(#{level = 1} Type)}
    match #ListFold(#NatType, #NatType, #List(#NatType, [5]), /member(3), (a: #NatType, b: #NatType) => b)
    | 0 =>
        Type
    | _ =>
        Type
    end =
    #BoolType;
let /s : /S =
    true;
#{node = Rec}
let /pair : (n: #NatType) -> {#NatType, #NatType} =
    (n: #NatType) =>
        #{result = Ambient({#NatType, #NatType})}
        match n
        | 0 =>
            (0, 1)
        | k + 1; x =>
            /pair(k)
        end;
let /p : /std/Bool/Holds(
    #NatEql(
        (#ListFold(#NatType, {#NatType, #NatType}, #List(#NatType, [5]), /pair(3), (a: #NatType, b: {#NatType, #NatType}) => b)).0,
        0
    )
) =
    /std/Bool/True/qed();
/std/Bool/True/qed()
"#;

// A match and a projection force what they read, so where an intrinsic answers a recursive call the machine forces the call before either reads it, as the strategy forces what `whnf` answers: `y`'s and `p`'s instances are `True`, `c3`'s type is `Bool` and `S` is a sort. Handed the fold's answer at the weak demand, a match or a projection stands on the unforced call, `Frame::RecGuard` reads `f`'s step as neutral, and `y`, `s` and `p` are refused. `curios`'s `tests::rules::sequence_tests` puts the program to both checkers under this name.
#[test]
fn a_match_or_a_projection_forces_the_call_a_fold_answers() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_MATCH_OR_A_PROJECTION_ON_A_FOLD_THAT_ANSWERS_A_RECURSIVE_CALL}",
        PROPOSITIONS,
        A_MATCH_OR_A_PROJECTION_ON_A_FOLD_THAT_ANSWERS_A_RECURSIVE_CALL
    ));

    assert!(
        the_closing_instance_alone_is_refused(&verdicts),
        "{verdicts:?}"
    );
}

/// `f(n)` is `f(n) + 1`, a recursion that never descends, and `w` states a family at `f(0)` to be one at its own unfolding.
const A_PARTIAL_CALL_COMPARED_WITH_ITS_UNFOLDING: &str = r#"
#{node = Rec}
let /f : (n: #NatType) -> #NatType =
    (n: #NatType) =>
        #NatAdd(/f(n), 1);
let /w : (P: (n: #NatType) -> Type, p: P(/f(0))) -> P(#NatAdd(/f(0), 1)) =
    (P: (n: #NatType) -> Type, p: P(/f(0))) =>
        p;
/std/Bool/True/qed()
"#;

// An operand a fold reads forced whose value is a folded recursive call is forced on the machine's own stack, so a recursion through an intrinsic's operand nests no native frame per level: `f(0) + 1` unfolds until the budget runs out, and `w` is refused by `Reduce`, as its open spelling at a variable `n` is, besides being refused for reading a partial `f` in a type. Forced through the host's own reduction instead, each level started a machine of its own on the native stack, and the run died of a stack overflow before the budget ran out.
#[test]
fn a_recursion_through_an_operand_runs_out_of_budget_and_not_of_stack() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_PARTIAL_CALL_COMPARED_WITH_ITS_UNFOLDING}",
        PROPOSITIONS,
        A_PARTIAL_CALL_COMPARED_WITH_ITS_UNFOLDING
    ));

    assert!(
        matches!(
            &verdicts[..],
            [
                Verdict { name: Some(exhausted), error: Error::Reduce(ReduceError::Exhausted { .. }) },
                Verdict { name: Some(partial), error: Error::NotTotal { .. } },
                Verdict { name: None, .. },
            ] if *exhausted == global("w") && *partial == global("w")
        ),
        "{verdicts:?}"
    );
}
