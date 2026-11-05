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
