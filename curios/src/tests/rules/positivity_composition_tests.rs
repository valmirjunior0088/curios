//! Polarity travelling through a type former, which is the whole of what "modulo polarity" buys.
//!
//! A check recognizing recursive occurrences only in immediate payload positions rejects every shape here, `/std/Toml` among them. Each row names the former whose polarity vector the occurrence composes against.

use {super::test_support::*, crate::test_support::*, curios_core::Polarity, curios_elab::Error};

// `Json/arr(List(Json))`: a list is a finite product of its element, so the intrinsic is covariant and a strict occurrence stays strict. Without polarity on intrinsics this shape is rejected outright.
#[test]
fn recursion_through_a_covariant_intrinsic_is_admitted() {
    let source = r#"
        use /std/{List};

        induct Tree : pub Type
        | leaf()
        | node(kids : List(Tree))
        end

        let branch : Tree = Tree/node([Tree/leaf()]);

        match branch
        | leaf() => /std/print("no")
        | node(_) => /std/print("listed")
        end
        "#;
    assert_eq!(run(source), b"listed");
}

// `List` is covariant, so an occurrence under it is whatever it was inside: a strict one stays admitted, and the arrow's left is still the arrow's left. The control is the strict one.
#[test]
fn a_negative_occurrence_under_a_covariant_intrinsic_is_rejected() {
    control(
        r#"
        use /std/{List, Bool};

        induct Bad : pub Type
        | c(List((Bool) -> Bad))
        end

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("List((Bool) -> Bad)", "List((Bad) -> Bool)")])
    .by_elaborator_in("/Bad", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Neg,
                ..
            }
        )
    });
}

// `Json/obj(List({Str, Json}))`: polarity has to travel through an anonymous tuple type as well as the intrinsic, since a product is as positive as its factors.
#[test]
fn recursion_through_an_anonymous_tuple_is_admitted() {
    let source = r#"
        use /std/{List, Nat};

        induct Doc : pub Type
        | atom(Nat)
        | fields(List({Nat, Doc}))
        end

        match Doc/fields([])
        | atom(_) => /std/print("no")
        | fields(_) => /std/print("tupled")
        end
        "#;
    assert_eq!(run(source), b"tupled");
}

// `Cell` is invariant — it is read *and* written — so nothing recursive may travel through one, even though the occurrence looks like a plain payload. The control holds the same payload under the covariant `List`.
#[test]
fn recursion_through_an_invariant_intrinsic_is_rejected() {
    control(
        r#"
        use /std/{Cell, List};

        induct Knot : pub Type
        | tie(List(Knot))
        end

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| tie(List(Knot))", "| tie(Cell(Knot))")])
    .by_elaborator_in("/Knot", |error| {
        matches!(error, Error::NotStrictlyPositive { .. })
    });
}

// The composition case, and the one a check without polarity vectors would miss entirely: `Sink` is contravariant in its parameter, so `Trap`'s payload puts `Trap` left of an arrow one indirection away. Nothing about `Trap`'s own constructor looks negative — the rejection comes from `Sink`'s vector. The control's `Sink` is covariant, its payload producing an `A`.
#[test]
fn a_negative_occurrence_borrowed_through_another_declaration_is_rejected() {
    control(
        r#"
        use /std/{Nat};

        induct Sink(A : Type) : pub Type
        | drain(f : (Nat) -> A)
        end

        induct Trap : pub Type
        | caught(Sink(Trap))
        end

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("(Nat) -> A", "(A) -> Nat")])
    .by_elaborator_in("/Trap", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Neg,
                ..
            }
        )
    });
}

// The double negative borrowed the same way: `Twice` is positive in its parameter and not strictly so, and the diagonal it gives `Trap` is refused under the merely-positive phrase rather than read as an occurrence the composition cleaned. The control's `Twice` is strict in its parameter.
#[test]
fn a_non_strict_occurrence_borrowed_through_another_declaration_is_rejected() {
    control(
        r#"
        use /std/Bool/{False};

        induct Twice(A : Type) : pub Type
        | mk(f : (False) -> A)
        end

        induct Trap : pub Type
        | caught(Twice(Trap))
        end

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("(False) -> A", "((A) -> False) -> False")])
    .by_elaborator_in("/Trap", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Pos,
                ..
            }
        )
    });
}

// `/std/Toml`, whose recursion travels `Toml → Map(Str, Toml) → struct field → Option → Node`. This is the shape that makes "modulo polarity" load-bearing on day one rather than a later refinement: a check that only recognizes recursive occurrences in immediate payload positions rejects it.
#[test]
fn recursion_through_a_struct_into_a_parameterized_inductive_is_admitted() {
    let source = r#"
        use /std/{Nat, Option};

        induct Node(V : Type) : pub Type
        | leaf(key : Nat, value : V)
        | fork(zero : Node(V), one : Node(V))
        end

        struct Table(V : Type) : pub Type {
            size : Nat,
            root : Option(Node(V)),
        }

        induct Value : pub Type
        | num(Nat)
        | table(Table(Value))
        end

        match Value/table(Table { size = 0, root = Option/none() })
        | num(_) => /std/print("no")
        | table(_) => /std/print("three hops")
        end
        "#;
    assert_eq!(run(source), b"three hops");
}

// A `struct` parameter carries a polarity vector as an `induct`'s does, and a record is the declaration a consumer is likeliest to wrap a recursive type in. The control's `Sink` is covariant, its field producing an `A`.
#[test]
fn a_negative_occurrence_borrowed_through_a_struct_parameter_is_rejected() {
    control(
        r#"
        use /std/{Nat};

        struct Sink(A : Type) : pub Type { f : (Nat) -> A }

        induct Trap : pub Type
        | caught(Sink(Trap))
        end

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("(Nat) -> A", "(A) -> Nat")])
    .by_elaborator_in("/Trap", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Neg,
                ..
            }
        )
    });
}

// The same through a `struct` parameter. The control's `Twice` is strict in its parameter.
#[test]
fn a_non_strict_occurrence_borrowed_through_a_struct_parameter_is_rejected() {
    control(
        r#"
        use /std/Bool/{False};

        struct Twice(A : Type) : pub Type { f : (False) -> A }

        induct Trap : pub Type
        | caught(Twice(Trap))
        end

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("(False) -> A", "((A) -> False) -> False")])
    .by_elaborator_in("/Trap", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Pos,
                ..
            }
        )
    });
}

// `Node(V)` mentions itself, so its own parameter vector is an input to computing that vector. A single ordered pass reads `V` as `Unused` and then composes `Value → Table → Node` through a parameter it wrongly believes is never inspected; only running the two fixpoints to stability gets `Strict`.
#[test]
fn a_self_mentioning_parameter_needs_the_fixpoint_to_settle() {
    let source = r#"
        use /std/{Nat};

        induct Rose(A : Type) : pub Type
        | tip(A)
        | fork(left : Rose(A), right : Rose(A))
        end

        induct Holder : pub Type
        | held(Rose(Holder))
        | empty()
        end

        match Holder/held(Rose/tip(@Holder, Holder/empty()))
        | held(_) => /std/print("settled")
        | empty() => /std/print("no")
        end
        "#;
    assert_eq!(run(source), b"settled");
}

// A step and the suspended rest of it in miniature: two declarations in one `and` block, each reaching the other, with the recursive occurrence behind a thunk. The group boundary lives on `Item::Rec` and not on the registry entry, so this passes only because the occurrence relation is closed transitively rather than by asking who shares a declaration block.
#[test]
fn a_mutually_recursive_group_reaching_itself_through_a_thunk_is_admitted() {
    let source = r#"
        induct Step : Type
        | halt()
        | again(next : () -> Machine)
        and Machine : Type
        | done()
        | more(step : Step, rest : () -> Machine)
        end

        match Machine/more(Step/halt(), () => Machine/done())
        | done() => /std/print("no")
        | more(_, _) => /std/print("mutual")
        end
        "#;
    assert_eq!(run(source), b"mutual");
}

// A cycle whose negative step is in the *other* member. Neither declaration is negative on its own inspection, and the group boundary is not on the registry entry, so this is caught only by closing the occurrence relation transitively. The control's `back` produces a `Left` rather than consuming one.
#[test]
fn a_negative_cycle_through_a_mutual_group_is_rejected() {
    control(
        r#"
        use /std/Bool/{False};

        induct Left : Type
        | wrap(Right)
        and Right : Type
        | back(f : (False) -> Left)
        end

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("(False) -> Left", "(Left) -> False")])
    .by_elaborator_in("/Left", |error| {
        matches!(error, Error::NotStrictlyPositive { .. })
    });
}

// A declaration reaching itself through a *type-former parameter* is refused, and this pins that it is refused rather than assumed.
//
// `curios-analysis/src/positivity.rs` names this obligation deliberately out of scope: `F` is a binder with no known polarity, so `Mu` cannot be checked from its own body, and discharging it properly needs an inferred per-binder obligation in a side store. Out of scope leaves open which way the analysis falls, and only one way is safe — an unknown former must read as `Mixed`, never as `Strict`. Nothing in the corpus takes a type-former parameter on an `induct`, so nothing else exercises the choice, and a later improvement to `occurrences` that taught it to see through `F(Mu(F))` would flip it silently.
//
// What the wrong direction costs is Curry's paradox with no recursion in sight. Admitting `Mu` lets it be instantiated at a negative former — `let Neg(X : Type) -> Type = (X) -> False` — and `Mu(Neg)`'s constructor is then `fix : ((Mu(Neg)) -> False) -> Mu(Neg)`, the negative occurrence this gate exists to forbid, spelled through a parameter instead of directly. `out(m) = match m | fix(f) => f end` and `delta(m) = out(m)(m)` give `delta(Mu/fix(delta)) : False`.
//
// Refused as it stands rather than a closed hole, and the diagnostic is asserted so the fixture cannot pass on an unrelated failure. Its control applies the same former to `Nat` and builds a `Mu(Box)`, which shows the refusal is about the declaration reaching itself through a former whose polarity nothing knows, its diagonal being what decides, and not a blanket ban on higher-kinded parameters — a ban would take `/std`'s `Monad`-shaped abstractions with it.
#[test]
fn a_declaration_recursing_through_a_type_former_parameter_is_refused() {
    control(
        r#"
        use /std/{Nat};

        induct Mu(F : (Type) -> Type) : pub Type
        | fix(F(Nat))
        end

        induct Box(A : Type) : pub Type
        | put(A)
        end

        let boxed : Mu(Box) = Mu/fix(Box/put(7));

        match boxed
        | fix(_) => /std/print("applied")
        end
        "#,
        b"applied",
    )
    .refused(&[("| fix(F(Nat))", "| fix(F(Mu(F)))")])
    .by_elaborator_in("/Mu", |error| {
        matches!(error, Error::NotStrictlyPositive { .. })
    });
}

// A concept's dictionary is a nominal record of its methods, so a method consuming the recursive type puts it left of an arrow one indirection away, exactly as a struct parameter does — and the dictionary here is *sealed*, its representation private to this module. Privacy is the elaborator's, and the gate reads the declaration whole, so a sealed representation hides a non-positive occurrence from no one: were it to, `Refute`'s `refute : (Bad) -> False` would be the negative occurrence the gate exists to forbid, reachable through a witness. The control's method produces its `A`, which the gate reads through the same seal.
#[test]
fn a_negative_occurrence_borrowed_through_a_sealed_concept_is_rejected() {
    control(
        r#"
        use /std/Bool/{False};

        concept Refute(A : Type) : Type { refute(False) -> A }

        struct Bad : pub Type { d : Refute(Bad) }

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("refute(False) -> A", "refute(A) -> False")])
    .by_elaborator_in("/Bad", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Neg,
                ..
            }
        )
    });
}

// A concept's superclass is a hidden field of its dictionary, and the walk reads it as it reads a written one: `Sub`'s edge `use Refute(Sub(X))` puts `Sub` left of `refute`'s arrow one dictionary away, and skipping a field no projection counts would make that edge the paradox's negative occurrence. The control's method produces its `A`, so the same edge is strict.
#[test]
fn a_negative_occurrence_through_a_superclass_field_is_rejected() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{False};

        concept Refute(A : Type) : pub Type { refute(False) -> A }

        concept Sub(X : Type) : pub Type { use Refute(Sub(X)), tag() -> Nat }

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("refute(False) -> A", "refute(A) -> False")])
    .by_elaborator_in("/Sub", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Neg,
                ..
            }
        )
    });
}

// An associated type a witness chooses is a field of a dictionary the walk unfolds: `Fam/Carrier(@Box(Bad))` resolves to the generic witness for `Box(A)` and projects its `Carrier`, which that witness spells `(A) -> False`. Neither `Bad`'s field nor `Fam`'s declaration shows an arrow, so only reading through the wrapper, the witness and the projection finds the negative occurrence. The control's witness spells `Sq(A)`, which the same reading finds strict.
#[test]
fn a_negative_occurrence_a_generic_witness_chooses_is_rejected() {
    control(
        r#"
        use /std/Bool/{False};

        induct Sq(A : Type) : pub Prop
        | sq(a : A)
        end

        concept Fam(A : Type) : pub Type { Carrier : Prop }

        struct Box(A : Type) : pub Type { held : A }

        satisfy (@A : Type) => Fam(Box(A)) { Carrier = Sq(A) }

        struct Bad : pub Type { x : Fam/Carrier(@Box(Bad)) }

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Carrier = Sq(A)", "Carrier = (A) -> False")])
    .by_elaborator_in("/Bad", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Neg,
                ..
            }
        )
    });
}

// A type declared under a premise takes its dictionary as a parameter, and a field resolving through the premise projects it, so `S` uses its dictionary where nothing can see through: `Bad` hands `S` a dictionary whose `Carrier` is `(Bad) -> False`, and reading the dictionary parameter as unused would hand `Bad` that arrow as its own field with no occurrence recorded. The control holds the same `S` at `Nat`.
#[test]
fn a_negative_occurrence_in_the_dictionary_a_premise_takes_is_rejected() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{False};

        concept Fam(A : Type) : pub Type { Carrier : Prop }

        struct S(A : Type, use Fam(A)) : pub Type { x : Fam/Carrier(@A) }

        struct Bad : pub Type { s : S(Nat, use Fam(Nat) { Carrier = (Nat) -> False }) }

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "S(Nat, use Fam(Nat) { Carrier = (Nat) -> False })",
        "S(Bad, use Fam(Bad) { Carrier = (Bad) -> False })",
    )])
    .by_elaborator_in("/Bad", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Mixed,
                ..
            }
        )
    });
}

// A record's field holding a type is read where it is projected: `neg(Bad).T` is a definition's record literal, its field `(Bad) -> False`, and the projection reduces to it. Read as an opaque projection the field type would name `Bad` only inside a value, and the record the paradox is built from would carry its own consumer. The control's record holds `Sq(X)`, which the same reading finds strict.
#[test]
fn a_negative_occurrence_projected_from_a_record_is_rejected() {
    control(
        r#"
        use /std/Bool/{False};

        induct Sq(A : Type) : pub Prop
        | sq(a : A)
        end

        struct Pack : pub Type { T : Prop }

        let pack(X : Type) -> Pack = Pack { T = Sq(X) };

        struct Bad : pub Type { x : pack(Bad).T }

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Pack { T = Sq(X) }", "Pack { T = (X) -> False }")])
    .by_elaborator_in("/Bad", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Neg,
                ..
            }
        )
    });
}

// A body the compiler writes is a definition the walk unfolds like a written one: `Sel` picks `(X) -> False` where a derived `Eql` says `Tag/a()` is `Tag/a()`, so `Bad`'s field is the negative arrow only through the derived witness's body. The control selects at `Tag/b()`, where the derived witness answers `false` and the field is `True`.
#[test]
fn a_negative_occurrence_a_derived_witness_selects_is_rejected() {
    control(
        r#"
        use /std/{Bool};
        use /std/Bool/{True, False};
        use /std/ops/{Eql};

        induct Tag : pub Type | a() | b() end

        satisfy Eql(Tag) { .. }

        let Sel(t : Tag, X : Type) -> Prop = match t == Tag/a() | true => (X) -> False | false => True end;

        struct Bad : pub Type { x : Sel(Tag/b(), Bad) }

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("Sel(Tag/b(), Bad)", "Sel(Tag/a(), Bad)"),
    ])
    .by_elaborator_in("/Bad", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Neg,
                ..
            }
        )
    });
}
