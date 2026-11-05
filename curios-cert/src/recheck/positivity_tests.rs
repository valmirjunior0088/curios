//! Strict positivity as the kernel judges it, and the premise it rests on, of families the elaborator refuses before a module exists: a family nested in itself through an intrinsic carrier, and a family indexed by a declaration that holds it, written as Core text and put to the kernel with nothing in scope, declaring the propositions it uses.

use {super::test_support::*, crate::*, curios_common::fill};

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

/// `struct S(G: (Type) -> Type) { f: (G(S(G))) -> False }`, closed at `G` the identity with `s.f(s)` over `s = S { (x) => x.f(x) }`: Curry's paradox with no recursion, the record reaching itself only as the argument of its own parameter, a function of types no vector speaks for.
const CURRY_THROUGH_A_PARAMETER: &str = r#"
struct /S(G: (A: Type) -> Type): pub Type {
    f: (
        x: G(
            #{node = StructType}
            /S(G)
        )
    ) -> /std/Bool/False
}
((
    s:
        #{node = StructType}
        /S((A2: Type) => A2)
) =>
    (#{field = Struct(/S, 0)} s.0)(s))(
    /S((A3: Type) => A3) {
        (
            x2:
                #{node = StructType}
                /S((A4: Type) => A4)
        ) =>
            (#{field = Struct(/S, 0)} x2.0)(x2)
    }
)
"#;

// Nothing says how a parameter uses its argument, so the record reaching itself as one is read as reaching itself at `Mixed`: the record is refused, and the closing term, which types once it is admitted, is refused for nothing of its own.
#[test]
fn a_record_reaching_itself_through_its_own_parameter_is_refused() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${CURRY_THROUGH_A_PARAMETER}",
        PROPOSITIONS,
        CURRY_THROUGH_A_PARAMETER
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(name), error: Error::NotPositive { .. } }] if *name == global("S")
        ),
        "{verdicts:?}"
    );
}

/// `/S` is indexed by `/W`, whose `wrap` holds an `/S`, the two declared as one group; `/A` and `/Z` are the same pair declared apart, `/A` before the family that holds it and `/Z` after; `/Box`'s parameter is a `/Tag`, whose `boxed` holds a `/Box`. `/Is`, indexed by `/Two`, which names nothing of it, is the control.
const INDEXED_BY_WHAT_HOLDS_IT: &str = r#"
induct /W: pub Prop
| base()
| wrap(x: #{node = InductType} /W(), s: #{node = InductType} /S()(x))
end
and induct /S: (w: #{node = InductType} /W()) -> pub Prop
| mk(v: #{node = InductType} /W()): (v)
end
induct /A: (v: #{node = InductType} /V()) -> pub Prop
| mk(): (#{node = Variant} /V/base())
end
induct /V: pub Prop
| base()
| wrap(x: #{node = InductType} /V(), s: #{node = InductType} /A()(x))
end
induct /U: pub Prop
| base()
| wrap(x: #{node = InductType} /U(), s: #{node = InductType} /Z()(x))
end
induct /Z: (u: #{node = InductType} /U()) -> pub Prop
| mk(): (#{node = Variant} /U/base())
end
struct /Box(t: #{node = InductType} /Tag()): pub Type { n: #NatType }
induct /Tag: pub Type
| none()
| boxed(b: #{node = StructType} /Box(#{node = Variant} /Tag/none()))
end
induct /Two: pub Prop
| a()
| b()
end
induct /Is(c: #{node = InductType} /Two()): (x: #{node = InductType} /Two()) -> pub Type
| yes(): (c)
end
/std/Bool/True/qed()
"#;

// Positivity walks no index binder type, which is sound only because a former's domains name nothing that holds the declaration: a family indexed by a family that holds it is induction-induction, which no decision admits. Each is refused under the declaration whose former names the other, declared in one group or apart, before or after the family that holds it, and a record's parameter alike, where positivity, which reads a family not yet judged at `Mixed`, refuses only the order that walks the holder first. `curios`'s `tests::rules::positivity_index_tests` puts the grouped shape to the elaborator under this name.
#[test]
fn a_family_is_indexed_by_nothing_that_holds_it() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${INDEXED_BY_WHAT_HOLDS_IT}",
        PROPOSITIONS,
        INDEXED_BY_WHAT_HOLDS_IT
    ));

    for (declaration, holder) in [("S", "W"), ("A", "V"), ("Z", "U"), ("Box", "Tag")] {
        assert!(
            verdicts
                .iter()
                .any(|verdict| verdict.name == Some(global(declaration))
                    && verdict.error
                        == Error::IndexedByItsGroup {
                            member: global(holder)
                        }),
            "{declaration}: {verdicts:?}"
        );
    }
    for control in ["Two", "Is"] {
        assert!(
            verdicts
                .iter()
                .all(|verdict| verdict.name != Some(global(control))),
            "{control}: {verdicts:?}"
        );
    }
}
