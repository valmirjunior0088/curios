//! Strict positivity as the certifier judges it, and the premise it rests on, of families the elaborator refuses before a module exists: a family nested in itself through an intrinsic carrier, and a family indexed by a declaration that holds it, written as Core text and put to the certifier with nothing in scope, declaring the propositions it uses.

use {super::test_support::*, crate::*, curios_common::fill};

/// `induct Bad : (n: Nat) -> Type | c(fs: List((p: {m: Nat, x: Bad(m), h: Holds(0 < m)}) -> False)) : Bad(len(fs)) end`, closed with `w(1, c([w]), qed)`, where `w` reads the head of the list its argument was built from, the head there because the index says so: Curry's paradox through a list.
const CURRY_THROUGH_A_LIST: &str = r#"
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
    ((mp: #NatType, xp: #{node = InductType} /Bad()(mp), hp: /std/Bool/Holds(#NatLt(0, mp))) => (#{goal = (h2: /std/Bool/Holds(#NatLt(0, mp))) -> /std/Bool/False} match xp
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
    end)(hp))((p).0, (p).1, (p).2))(
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
                        ((mp2: #NatType, xp2: #{node = InductType} /Bad()(mp2), hp2: /std/Bool/Holds(#NatLt(0, mp2))) => (#{goal = (h4: /std/Bool/Holds(#NatLt(0, mp2))) -> /std/Bool/False} match xp2
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
                        end)(hp2))((p2).0, (p2).1, (p2).2)]
            )
        ),
        #{node = Variant}
        /std/Bool/True/qed()
    )
)
"#;

// `Bad` reaches itself only through the list, its index carrying the length a read at the head needs, and the term it closes with types at `False` once the family is admitted. `List` is covariant, so the arrow inside it counts where it stands, and the certifier's refusal of the family is the only one.
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

// Positivity walks no index binder type, which is sound only because a former's domains name nothing that holds the declaration: a family indexed by a family that holds it is induction-induction, which no decision admits. Each is refused under the declaration whose former names the other, declared in one group or apart, before or after the family that holds it, and a record's parameter alike, where positivity, which reads a family not yet judged at `Mixed`, refuses only the order that walks the holder first. `curios`'s `tests::rules::positivity_index` puts the grouped shape to the elaborator under this name.
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

/// `/S` holds itself through its own field, `/Tup` through a component of a tuple, `/U` through the field `/Pair` holds its parameter at, and `/Grow` through its own declaration at an argument its field grows; `/T`, `/V` and `/Shrink` hold themselves through the family `/Opt` at the same places.
const HOLDING_ITSELF: &str = r#"
induct /Opt(A: Type): pub Type
| none()
| some(a: A)
end
struct /Pair(A: Type): pub Type { first: A, second: A }
struct /S: pub Type { next: #{node = StructType} /S() }
struct /Tup: pub Type { next: {#NatType, #{node = StructType} /Tup()} }
struct /U: pub Type { pair: #{node = StructType} /Pair(#{node = StructType} /U()) }
struct /Grow(A: Type): pub Type { next: #{node = StructType} /Grow({A, A}) }
struct /T: pub Type { next: #{node = InductType} /Opt(#{node = StructType} /T()) }
struct /V: pub Type { pair: #{node = StructType} /Pair(#{node = InductType} /Opt(#{node = StructType} /V())) }
struct /Shrink(A: Type): pub Type { next: #{node = InductType} /Opt(#{node = StructType} /Shrink({A, A})) }
/std/Bool/True/qed()
"#;

// A record that holds itself through its fields alone has no finite value, and the size order reads a field strictly below its record, so it is refused where it is declared, whichever way the path back is spelled; a family between ends the path, and the records holding themselves through one stand. `curios`'s `tests::rules::totality_projection` puts the same records to the elaborator under this name.
#[test]
fn a_record_holding_itself_through_fields_alone_is_refused() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${HOLDING_ITSELF}",
        PROPOSITIONS,
        HOLDING_ITSELF
    ));

    // The closing term is put to `False` and refused as any proof of it is; what the declarations are refused with is the question.
    let refused = verdicts
        .iter()
        .filter(|verdict| verdict.name.is_some())
        .map(|verdict| match verdict {
            Verdict {
                name: Some(name),
                error: Error::HoldsItself { .. },
            } => Some(*name),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        refused,
        ["S", "Tup", "U", "Grow"].map(|name| Some(global(name))),
        "{verdicts:?}"
    );
}

/// `D(r : Opt(Type)) | c(f : match r | none() => {} | some(X) => (X) -> Void end)` beside `G | g(D(some(G)))`: the head of a stuck `match` carries a type into the arm that puts it left of an arrow.
const A_HEAD_CARRYING_A_TYPE: &str = r#"
induct /Void: pub Type
end

induct /Opt(A: #{level = 1} Type): pub #{level = 1} Type
| none()
| some(x: A)
end

induct /D(r: #{node = InductType} /Opt(Type)): pub Type
| c(
    f:
        #{goal = Type}
        match r
        | none() => {}
        | some(X) => (x: X) -> #{node = InductType} /Void()
        end
)
end

induct /G: pub Type
| g(d: #{node = InductType} /D(#{node = Variant} /Opt/some(@Type, #{node = InductType} /G())))
end

/std/Bool/False
"#;

// The arm reads the binder its head supplies, so the head is read at `Mixed` and `/G`, holding a `/D` whose head holds `/G`, reaches itself both ways. `curios`'s `tests::rules::positivity_computed_type` puts the same declarations to the elaborator under this name. Rename both or neither.
#[test]
fn a_head_carrying_a_type_into_an_arm_is_read_at_mixed() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_HEAD_CARRYING_A_TYPE}",
        PROPOSITIONS,
        A_HEAD_CARRYING_A_TYPE
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("G"))
                && matches!(verdict.error, Error::NotPositive { .. })),
        "{verdicts:?}"
    );
}
