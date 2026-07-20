use {curios_runtime::MockHost, std::time::Duration};

#[test]
fn named_fields_run_end_to_end() {
    let source = r#"
        use /std/{Vec, Nat, Io};
        let p : { n : Nat, v : Vec(Nat, n) } =
            (n = 2, v = Vec/cons(30, Vec/cons(12, Vec/nil())));
        rec total(@k : Nat, v : Vec(Nat, k), acc : Nat) -> Nat =
            match v : Nat
            | nil() => acc
            | cons(m, x, xs) => total(xs, Nat/add(acc, x))
            end;
        Io/print(Nat/to_str(Nat/add(total(p.v, 0), Nat/mul(p.0, 0))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"42");
}

// A transparent record: build with a pinned head, project by label and by
// index — both resolve to the same positional projection.
#[test]
fn struct_transparent_pair_projects() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair(Nat, Nat) { fst = 2, snd = 5 };
        Io/print(Nat/to_str(Nat/add(p.fst, p.1)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

// The bare-name head infers the parameters from the fields (and the expected
// type at the binding).
#[test]
fn struct_parameter_inference_at_construction() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 4, snd = 3 };
        Io/print(Nat/to_str(Nat/mul(p.fst, p.snd)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"12");
}

// A zero-cost newtype: a single positional field, projected with `.0`. It
// erases to its bare field, so the projection elides at runtime.
#[test]
fn struct_newtype_projects() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Meters : pub Type { Nat }
        let m : Meters = Meters { 5 };
        Io/print(Nat/to_str(m.0))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"5");
}

// A dependent field: a later field's type mentions an earlier field (the
// vector's length indexes its type).
#[test]
fn struct_dependent_fields_run_end_to_end() {
    let source = r#"
        use /std/{Vec, Nat, Io};
        pub struct Sized : pub Type { n : Nat, v : Vec(Nat, n) }
        let s : Sized = Sized { n = 2, v = Vec/cons(30, Vec/cons(12, Vec/nil())) };
        rec total(@k : Nat, v : Vec(Nat, k), acc : Nat) -> Nat =
            match v : Nat
            | nil() => acc
            | cons(m, x, xs) => total(xs, Nat/add(acc, x))
            end;
        Io/print(Nat/to_str(total(s.v, 0)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"42");
}

// The motivating case: an abstract type — public type, hidden representation —
// usable only through exported smart constructors/accessors in its module.
#[test]
fn struct_abstract_smart_constructor_round_trips() {
    let source = r#"
        use /std/{Nat, Io};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius : Type { Nat }
            pub let of_nat(n : Nat) -> Celsius = Celsius { n };
            pub let to_nat(c : Celsius) -> Nat = c.0;
        end
        Io/print(Nat/to_str(Celsius/to_nat(Celsius/of_nat(42))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"42");
}

// Constructing a private-representation struct from outside its declaring
// module is rejected (`PrivateRepresentation`).
#[test]
fn struct_private_construction_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius : Type { Nat }
        end
        let c : Celsius/Celsius = Celsius/Celsius { 42 };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("representation"),
        "unexpected error: {error}"
    );
}

// Projecting a private-representation struct's field from outside its module is
// rejected (`PrivateField`), even when the value was obtained legitimately.
#[test]
fn struct_private_projection_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius : Type { Nat }
            pub let of_nat(n : Nat) -> Celsius = Celsius { n };
        end
        let c : Celsius/Celsius = Celsius/of_nat(42);
        Io/print(Nat/to_str(c.0))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("field") && error.contains("private"),
        "unexpected error: {error}"
    );
}

// Diagnostics name binders with the source names the user wrote, not the
// `hint#counter` gensyms elaboration opens them under (axis (a)): the inferred
// function type must read `(n : Nat)`, never `(n#3 : Nat)`.
#[test]
fn struct_is_not_a_tuple() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : { fst : Nat, snd : Nat } = Pair { fst = 1, snd = 2 };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(10), source, system).is_err());
}

// A struct literal must supply exactly the declared fields, in order.
#[test]
fn struct_wrong_field_count_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 1 };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(10), source, system).is_err());
}

// Written field labels are validated positionally — no reordering.
#[test]
fn struct_field_label_out_of_order_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { snd = 1, fst = 2 };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(10), source, system).is_err());
}

// A struct literal whose head names a non-struct binding is rejected as
// `NotAStructType` (its type is reported), not misreported as unbound.
#[test]
fn struct_literal_non_struct_head_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        let Foo : Nat = 3;
        let bad : Nat = Foo { x = 1 };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(error.contains("struct type"), "unexpected error: {error}");
}

// A `Prop`-sorted struct whose fields are all propositions is a sub-singleton:
// every projection lands in `Prop`, so proof irrelevance leaks nothing. It is
// accepted, and — its content being non-informative — erases away: the program
// compiles and runs, the projected proof contributing no runtime code while the
// ordinary `Nat` computation still produces its result.
#[test]
fn prop_struct_with_prop_fields_runs() {
    let source = r#"
        use /std/{Nat, Eq, Io};
        struct And(A : Prop, B : Prop) : pub Prop { fst : A, snd : B }
        let p : And(Eq(0, 0), Eq(1, 1)) = And { Eq/refl(), Eq/refl() };
        let proof : Eq(0, 0) = p.fst;
        Io/print(Nat/to_str(7))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

// A `Prop`-sorted struct with an informative (`Type`-sorted) field is rejected
// at declaration. Projection is an unguarded eliminator, so admitting it under
// proof irrelevance proves `Eq(b0, b1)` for distinct `b0`, `b1` — and thence
// `Eq(0, 1)` and `False`. The soundness-critical regression (bare `: Prop`).
#[test]
fn prop_struct_with_informative_field_rejected() {
    let source = r#"
        use /std/{Nat, Eq, Io};
        struct Box : pub Prop { val : Nat }
        let b0 : Box = Box { 0 };
        let b1 : Box = Box { 1 };
        let irrelevant : Eq(b0, b1) = Eq/refl();
        let get(b : Box) -> Nat = b.val;
        let zero_eq_one : Eq(0, 1) = Eq/cong(get, irrelevant);
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(error.contains("informative"), "unexpected error: {error}");
}

// Control: the same record at the default `Type` sort gets no proof
// irrelevance, so `Eq(b0, b1)` for distinct values is correctly rejected by
// conversion — confirming the `Prop` sort was the only door to the
// contradiction, and that closing it leaves ordinary records untouched.
#[test]
fn type_struct_distinct_values_not_convertible() {
    let source = r#"
        use /std/{Nat, Eq, Io};
        struct Box : pub Type { val : Nat }
        let b0 : Box = Box { 0 };
        let b1 : Box = Box { 1 };
        let irrelevant : Eq(b0, b1) = Eq/refl();
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(10), source, system).is_err());
}

// The function-field sugar, end to end: `label(params) -> T` in a struct
// declaration and a Σ-type, `label(params) = body` in a struct literal and a
// tuple literal. The parser keeps the sugar in the AST; `into_core` undoes it —
// this pins the lowering, not just the grammar.
#[test]
fn function_field_sugar_runs_end_to_end() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Api : pub Type { base : Nat, bump(x : Nat) -> Nat }
        let api : Api = Api { base = 3, bump(x) = x + 1 };
        let pair : { seed : Nat, twice(x : Nat) -> Nat } =
            (seed = api.bump(api.base), twice(x) = x + x);
        Io/print(Nat/to_str(pair.twice(pair.seed)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"8");
}

// A `pub` item's signature may not expose a private sibling: the reference
// resolves through lexical scope (no publicness walk), so a dedicated
// interface audit closes the gap.
#[test]
fn pub_signature_exposing_private_sibling_is_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        mod M
            use /std/{Nat};
            struct Secret : Type { Nat }
            pub let f(s : Secret) -> Nat = 1;
        end
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("exposes private item '/M/Secret'"),
        "unexpected error: {error}"
    );
}

// The other privately-resolvable path: an item inside the module's own
// private child (the head segment resolves lexically, so resolution never
// checked the child's visibility).
#[test]
fn pub_signature_exposing_private_child_module_is_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        mod M
            mod Inner
                use /std/{Nat};
                pub struct T : pub Type { n : Nat }
            end
            use Inner/{T};
            pub let g(t : T) -> T = t;
        end
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("exposes private item '/M/Inner'"),
        "unexpected error: {error}"
    );
}

// A pub concept's field types are interface (its representation is always
// public), superclass edges included.
#[test]
fn pub_concept_with_private_superclass_is_rejected() {
    let source = r#"
        use /std/{Nat, Bool, Io};
        mod M
            use /std/{Bool};
            concept Hidden(A : Type) : Type {
                h(A) -> Bool
            }
            pub concept Loud(A : Type) : Type {
                use Hidden(A),
                l(A) -> Bool
            }
        end
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

// A pub inductive's constructors are its interface: a private payload type is
// rejected (the `Task`/`Pause` shape).
#[test]
fn pub_inductive_with_private_payload_type_is_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        mod M
            induct Secret : Type
            | mk()
            end
             pub induct Box : pub Type
            | wrap(Secret)
            end
        end
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("exposes private item '/M/Secret'"),
        "unexpected error: {error}"
    );
}

// An opaque struct's field types are not interface, while an inner-`pub`
// struct's fields are.
#[test]
fn hidden_struct_fields_are_not_interface_but_exposed_fields_are() {
    let hidden = r#"
        use /std/{Nat, Io};
        mod M
            use /std/{Nat};
            struct Secret : pub Type { n : Nat }
            pub struct Opaque : Type { Secret }
            pub let mk() -> Opaque = Opaque { Secret { n = 1 } };
        end
        let o = M/mk();
        Io/print("ok")
        "#;
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), hidden, system).expect("expected result");
    assert_eq!(io.output(), b"ok");

    let exposed = r#"
        use /std/{Nat, Io};
        mod M
            use /std/{Nat};
            struct Secret : pub Type { n : Nat }
            pub struct Open : pub Type { s : Secret }
        end
        Io/print("no")
        "#;
    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), exposed, system).unwrap_err();
    assert!(
        error.contains("exposes private item '/M/Secret'"),
        "unexpected error: {error}"
    );
}

// `T { ..base, f = x }` copies every unwritten field from `base`; a bare
// spread is the identity copy.
#[test]
fn struct_spread_identity_copy() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 4, snd = 3 };
        let q : Pair(Nat, Nat) = Pair { ..p };
        Io/print(Nat/to_str(Nat/mul(q.fst, q.snd)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"12");
}

// A single labeled override replaces its field; the rest copy across.
#[test]
fn struct_spread_single_override() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 4, snd = 3 };
        let q : Pair(Nat, Nat) = Pair { ..p, snd = 9 };
        Io/print(Nat/to_str(Nat/add(q.fst, q.snd)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"13");
}

// Overrides claim scattered positions (first and third), the gap copies —
// the order-preserving-subsequence rule with a hole in the middle.
#[test]
fn struct_spread_multi_override_with_gap() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Tri : pub Type { fst : Nat, snd : Nat, thd : Nat }
        let t : Tri = Tri { fst = 1, snd = 2, thd = 3 };
        let u : Tri = Tri { ..t, fst = 10, thd = 30 };
        Io/print(Nat/to_str(Nat/add(Nat/add(u.fst, u.snd), u.thd)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"42");
}

// A dependent record updates when the override keeps the dependency
// consistent: `n` and `v : Vec(Nat, n)` replaced together.
#[test]
fn struct_spread_dependent_override_runs() {
    let source = r#"
        use /std/{Nat, Vec, Io};
        pub struct Sized : pub Type { n : Nat, v : Vec(Nat, n) }
        let s : Sized = Sized { n = 2, v = Vec/cons(30, Vec/cons(12, Vec/nil())) };
        let t : Sized = Sized { ..s, n = 1, v = Vec/cons(42, Vec/nil()) };
        rec total(@k : Nat, v : Vec(Nat, k), acc : Nat) -> Nat =
            match v : Nat
            | nil() => acc
            | cons(m, x, xs) => total(xs, Nat/add(acc, x))
            end;
        Io/print(Nat/to_str(total(t.v, 0)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"42");
}

// Overriding a field that a copied field's type depends on is rejected: the
// copied `v` still has length 2, but the new telescope demands 3.
#[test]
fn struct_spread_dependent_field_mismatch_rejected() {
    let source = r#"
        use /std/{Nat, Vec, Io};
        pub struct Sized : pub Type { n : Nat, v : Vec(Nat, n) }
        let s : Sized = Sized { n = 2, v = Vec/cons(1, Vec/cons(2, Vec/nil())) };
        let bad : Sized = Sized { ..s, n = 3 };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(10), source, system).is_err());
}

// The head may re-pin parameters, so an update can change them: the base is a
// `Pair(Nat, Nat)`, the result a `Pair(Str, Nat)` — the copied `snd` is
// checked against the new instantiation.
#[test]
fn struct_spread_parameter_changing_update() {
    let source = r#"
        use /std/{Nat, Str, Io};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 1, snd = 42 };
        let q : Pair(Str, Nat) = Pair { ..p, fst = "x" };
        Io/print(Nat/to_str(q.snd))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"42");
}

// A bare head with a spread and no annotation: the parameter metavariables
// are minted inside the base's frame and solved from the copied projections.
#[test]
fn struct_spread_bare_head_inference() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 4, snd = 3 };
        let q = Pair { ..p, snd = 9 };
        Io/print(Nat/to_str(Nat/add(q.fst, q.snd)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"13");
}

// The function-field definition sugar works as a spread override.
#[test]
fn struct_spread_function_field_override() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Api : pub Type { base : Nat, bump : (Nat) -> Nat }
        let api : Api = Api { base = 40, bump(x) = x };
        let api2 : Api = Api { ..api, bump(x) = Nat/add(x, 2) };
        Io/print(Nat/to_str(api2.bump(api2.base)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"42");
}

// Overrides after a spread must be labeled: gaps make positions ambiguous.
#[test]
fn struct_spread_unlabeled_override_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 1, snd = 2 };
        let bad = Pair { ..p, 5 };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(error.contains("labeled"), "unexpected error: {error}");
}

// Overrides must follow the declared field order — the ordering law holds
// through a spread.
#[test]
fn struct_spread_out_of_order_override_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Tri : pub Type { fst : Nat, snd : Nat, thd : Nat }
        let t : Tri = Tri { fst = 1, snd = 2, thd = 3 };
        let bad = Tri { ..t, thd = 30, fst = 10 };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(error.contains("order"), "unexpected error: {error}");
}

// A repeated override is caught by the same subsequence walk.
#[test]
fn struct_spread_duplicate_override_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 1, snd = 2 };
        let bad = Pair { ..p, fst = 3, fst = 4 };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(error.contains("order"), "unexpected error: {error}");
}

// An override naming no declared field is an unknown field.
#[test]
fn struct_spread_unknown_field_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 1, snd = 2 };
        let bad = Pair { ..p, nope = 3 };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(error.contains("no field"), "unexpected error: {error}");
}

// The spread must be the first entry.
#[test]
fn struct_spread_not_first_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 1, snd = 2 };
        let bad = Pair { fst = 3, ..p };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(error.contains("first"), "unexpected error: {error}");
}

// At most one spread per literal.
#[test]
fn struct_spread_multiple_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 1, snd = 2 };
        let bad = Pair { ..p, ..p };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(error.contains("at most one"), "unexpected error: {error}");
}

// The base must be a value of the literal's own struct — a structurally
// matching tuple does not qualify.
#[test]
fn struct_spread_non_struct_base_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let bad = Pair { ..(fst = 1, snd = 2) };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("must itself be"),
        "unexpected error: {error}"
    );
}

// Nor does a same-shaped *other* record.
#[test]
fn struct_spread_wrong_struct_base_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        pub struct Dup(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let d : Dup(Nat, Nat) = Dup { fst = 1, snd = 2 };
        let bad = Pair { ..d };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("must itself be"),
        "unexpected error: {error}"
    );
}

// A spread is construction, so a private-representation struct cannot be
// spread-copied outside its declaring module either.
#[test]
fn struct_spread_private_outside_module_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius : Type { Nat }
            pub let of_nat(n : Nat) -> Celsius = Celsius { n };
        end
        let c : Celsius/Celsius = Celsius/of_nat(42);
        let bad = Celsius/Celsius { ..c };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(10), source, system).unwrap_err();
    assert!(
        error.contains("representation"),
        "unexpected error: {error}"
    );
}
