//! An intrinsic node as the kernel types it, written as Core text and put with nothing in scope: at what its variant produces and with each operand at what its variant demands, whatever the text around it states, so a bound proves the decision its own operands make, and a handle no host gave is a value of no proposition outside the description it stands in.

use {super::test_support::*, crate::*, curios_common::fill};

/// Every byte is below 256, which the carriers' algebra decides with nothing written, so `below` is `qed` at every byte; a byte narrowed from 300 would read back as 300, and `below` at it is a proof of `Holds(300 < 256)`, which is `False`. The narrowing's bound is stated over its own operand, `Holds(300 < 256)`, and `qed` proves `True`.
const NARROWED_PAST_ITS_DOMAIN: &str = r#"
((
    below: (b: #ByteType) -> /std/Bool/Holds(#NatLt(#ByteToNat(b), 256))
) =>
    below(
        #NatToByte(
            300,
            #{node = Variant}
            /std/Bool/True/qed()
        )
    ))(
    (b2: #ByteType) =>
        #{node = Variant}
        /std/Bool/True/qed()
)
"#;

// `below` types, so the one refusal is the bound's, at the closing term: what stands first between the term and `False` is the operand `Intrinsic::signature` demands of `NatToByte`, and the fold's refusal of a narrowing past the carrier (`ReduceError::NatToByteAbove`) second.
//
// Mutation-checked at `Intrinsic::signature`: with the `NatToByte` row's bound stated as `holds(Self::Bool(true))` rather than over its `nat` operand, the bound types, and no mismatch is refused.
#[test]
fn a_narrowing_bound_is_a_proof_about_its_own_operand() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${NARROWED_PAST_ITS_DOMAIN}",
        PROPOSITIONS,
        NARROWED_PAST_ITS_DOMAIN
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

/// A description that waits on a handle no host gave and goes on as a row that does not return, yielding `False`: the row's declaration is written beside it so the text reads with no scope.
const SUSPENDED_ON_A_FORGED_HANDLE: &str = r#"
foreign /gone() -> Never;
let /gone : #NatType = 0;
#IoSuspend(
    /std/Bool/False,
    #List(#HandleType, [#Handle(2)]),
    #List(#CellType({}), []),
    #IoPure({}, ()),
    #{node = Foreign}
    /gone(@/std/Bool/False)
)
"#;

// What the suspension is, whatever it waits on, is a description of a proof of `False`, `Io(False)`, which nothing eliminates: the closing term is refused where a proof of `False` is asked, and as reaching an exit besides.
#[test]
fn a_description_waiting_on_a_handle_no_host_gave_proves_nothing() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${SUSPENDED_ON_A_FORGED_HANDLE}",
        PROPOSITIONS,
        SUSPENDED_ON_A_FORGED_HANDLE
    ));

    assert!(
        verdicts.iter().any(
            |verdict| verdict.name.is_none() && matches!(verdict.error, Error::Mismatch { .. })
        ),
        "{verdicts:?}"
    );
}

/// A description taken apart three ways, each as the elaborator refuses its program: matched by a family's case, projected as though it held its answer, and split by a `Bool` case form. `Io(False)` is inhabited by a row that does not return, `gone`, at `False`, so any of the three that typed would hand its caller the proof the description yields; the row's declaration is written beside it so the text reads with no scope.
const A_DESCRIPTION_TAKEN_APART: &str = r#"
induct /Wrap(A: Prop): pub Type
| mk(a: A)
end
let /matched : (
    io: #IoType(
        #{node = InductType}
        /std/Bool/False()
    )
) ->
    #{node = InductType}
    /std/Bool/False() =
    (
        io: #IoType(
            #{node = InductType}
            /std/Bool/False()
        )
    ) =>
        #{
            result = Ambient(
                #{node = InductType}
                /std/Bool/False())}
        match io
        | mk(a) =>
            a
        end;
let /projected : (
    io: #IoType(
        #{node = InductType}
        /std/Bool/False()
    )
) ->
    #{node = InductType}
    /std/Bool/False() =
    (
        io: #IoType(
            #{node = InductType}
            /std/Bool/False()
        )
    ) =>
        (io).0;
let /split : (io: #IoType(#BoolType)) -> #BoolType =
    (io: #IoType(#BoolType)) =>
        #{result = Ambient(#BoolType)}
        match io
        | false =>
            false
        | true =>
            true
        end;
foreign /gone() -> Never;
let /gone : #NatType = 0;
/matched(#{node = Foreign} /gone(@#{node = InductType} /std/Bool/False()))
"#;

// `Io` has no eliminator: its scrutinee is no family's occurrence and no `Bool`, so each case form refuses it as unclassified, and a projection finds no components in it. The closing term, which types through `matched`'s stated type, is refused all the same, by (V), the row that does not return standing in a proof with no definition to blame. `curios`'s `tests::rules::effect_tests` puts the program to the elaborator under this name.
//
// Mutation-checked at `check_cases`: with a `Bool` case form no longer held to a `Bool` scrutinee, `split` types and is not refused.
#[test]
fn a_description_is_taken_apart_by_no_elimination() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_DESCRIPTION_TAKEN_APART}",
        PROPOSITIONS,
        A_DESCRIPTION_TAKEN_APART
    ));
    let refused = |name: Option<&str>, pattern: fn(&Error) -> bool| {
        verdicts
            .iter()
            .any(|verdict| verdict.name == name.map(global) && pattern(&verdict.error))
    };

    assert!(
        refused(Some("matched"), |error| matches!(
            error,
            Error::Unclassified(_)
        )),
        "{verdicts:?}"
    );
    assert!(
        refused(Some("projected"), |error| matches!(
            error,
            Error::NotATuple(_)
        )),
        "{verdicts:?}"
    );
    assert!(
        refused(Some("split"), |error| matches!(
            error,
            Error::Unclassified(_)
        )),
        "{verdicts:?}"
    );
    assert!(
        refused(None, |error| matches!(
            error,
            Error::NotTotal { reached: None, .. }
        )),
        "{verdicts:?}"
    );
}
