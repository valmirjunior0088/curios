//! Strict positivity as the kernel judges it, of families the elaborator refuses before a module exists: a family nested in itself through an intrinsic carrier, written as Core text and put to the kernel with nothing in scope, declaring the propositions it uses.

use {super::test_support::*, crate::*, curios_common::fill};

/// The propositions a module here uses, declared where the registry this crate's suites judge under names them: `False`, `True` and `Holds`, the reflection of a decision a bound states.
const PROPOSITIONS: &str = r#"
induct /std/Bool/False: pub Prop
end

induct /std/Bool/True: pub Prop
| qed()
end

let /std/Bool/Holds : (b: #BoolType) -> Prop =
    (b: #BoolType) =>
        #{result = Ambient(Prop)}
        match b
        | false =>
            #{node = InductType}
            /std/Bool/False()
        | true =>
            #{node = InductType}
            /std/Bool/True()
        end;
"#;

/// `induct Bad : (n: Nat) -> Type | c(fs: List((p: {m: Nat, x: Bad(m), h: Holds(0 < m)}) -> False)) : Bad(len(fs)) end`, closed with `w(1, c([w]), qed)`, where `w` reads the head of the list its argument was built from, the head there because the index says so: Curry's paradox through a list.
const CURRY_THROUGH_A_LIST: &str = r#"
#{absent = [/Bad/c]}
induct /Bad: (n: #NatType) -> pub Type
| c(
    fs: #ListType(
        (
            x2: {
                    m: #NatType,
                    x:
                        #{node = InductType}
                        /Bad()(m),
                    h: /std/Bool/Holds(#NatLt(0, m))
            }
        ) -> /std/Bool/False
    )
): (
    #ListLen(
        (
            x3: {
                    m: #NatType,
                    x:
                        #{node = InductType}
                        /Bad()(m),
                    h: /std/Bool/Holds(#NatLt(0, m))
            }
        ) -> /std/Bool/False,
        fs
    )
)
end

((
    p: {
            m: #NatType,
            x:
                #{node = InductType}
                /Bad()(m),
            h: /std/Bool/Holds(#NatLt(0, m))
    }
) =>
    (match (p).1: (k, y) => (h2: /std/Bool/Holds(#NatLt(0, k))) -> /std/Bool/False
    | c(payload) =>
        (
            h3: /std/Bool/Holds(
                #NatLt(
                    0,
                    #ListLen(
                        (
                            x2: {
                                    m: #NatType,
                                    x:
                                        #{node = InductType}
                                        /Bad()(m),
                                    h: /std/Bool/Holds(#NatLt(0, m))
                            }
                        ) -> /std/Bool/False,
                        payload
                    )
                )
            )
        ) =>
            #ListGet(
                (
                    x3: {
                            m: #NatType,
                            x:
                                #{node = InductType}
                                /Bad()(m),
                            h: /std/Bool/Holds(#NatLt(0, m))
                    }
                ) -> /std/Bool/False,
                payload,
                0,
                h3
            )(p)
    end)((p).2))(
    (
        1,
        #{node = Variant}
        /Bad/c(
            #List(
                (
                    x4: {
                            m: #NatType,
                            x:
                                #{node = InductType}
                                /Bad()(m),
                            h: /std/Bool/Holds(#NatLt(0, m))
                    }
                ) -> /std/Bool/False,
                [
                    (
                        p2: {
                                m: #NatType,
                                x:
                                    #{node = InductType}
                                    /Bad()(m),
                                h: /std/Bool/Holds(#NatLt(0, m))
                        }
                    ) =>
                        (match (p2).1: (k2, y2) => (
                            h4: /std/Bool/Holds(#NatLt(0, k2))
                        ) -> /std/Bool/False
                        | c(payload2) =>
                            (
                                h5: /std/Bool/Holds(
                                    #NatLt(
                                        0,
                                        #ListLen(
                                            (
                                                x5: {
                                                        m: #NatType,
                                                        x:
                                                            #{node = InductType}
                                                            /Bad()(m),
                                                        h: /std/Bool/Holds(#NatLt(0, m))
                                                }
                                            ) -> /std/Bool/False,
                                            payload2
                                        )
                                    )
                                )
                            ) =>
                                #ListGet(
                                    (
                                        x6: {
                                                m: #NatType,
                                                x:
                                                    #{node = InductType}
                                                    /Bad()(m),
                                                h: /std/Bool/Holds(#NatLt(0, m))
                                        }
                                    ) -> /std/Bool/False,
                                    payload2,
                                    0,
                                    h5
                                )(p2)
                        end)((p2).2)]
            )
        ),
        #{node = Variant}
        /std/Bool/True/qed()
    )
)
"#;

// `Bad` reaches itself only through the list, its index carrying the length a read at the head needs, and the term it closes with types at `False` once the family is admitted. `List` is covariant, so the arrow inside it counts where it stands, and the kernel's refusal of the family is the only one.
//
// Mutation-checked at `curios-analysis`'s positivity walk: reading nothing through `ListType` admits the module whole.
#[test]
fn curry_through_a_list_is_refused_at_the_family_alone() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${CURRY_THROUGH_A_LIST}",
        PROPOSITIONS,
        CURRY_THROUGH_A_LIST
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(name), error: Error::NotPositive { .. } }] if *name == global("Bad")
        ),
        "{verdicts:?}"
    );
}

// A record held by a carrier no value hands it back through: `struct Bad { held: Io((Bad) -> False) }`, a description of its own refutation, and `struct Bad { held: Cell(Bad) }` and `{ held: Channel(Bad) }`, which hold it as a plain payload would. `Io` is covariant, so the arrow inside it counts where it stands, and a cell or a channel is read and written, so either is invariant: each is refused, the shapes a carrier with an eliminator would turn into Curry's paradox.
#[test]
fn a_record_held_by_an_effectful_carrier_is_refused() {
    for (carrier, held) in [
        (
            "a description of its refutation",
            "#IoType((x: #{node = StructType} /Bad()) -> /std/Bool/False)",
        ),
        ("a cell", "#CellType(#{node = StructType} /Bad())"),
        ("a channel", "#ChannelType(#{node = StructType} /Bad())"),
    ] {
        let verdicts = core_text_verdicts(&fill!(
            "${PROPOSITIONS}\nstruct /Bad: pub Type { held: ${held} }\n\n/std/Bool/False",
            PROPOSITIONS,
            held
        ));

        assert!(
            verdicts
                .iter()
                .any(|verdict| verdict.name == Some(global("Bad"))
                    && matches!(verdict.error, Error::NotPositive { .. })),
            "a record held in {carrier} was not refused as not positive: {verdicts:?}",
        );
    }
}
