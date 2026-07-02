use {curios_rt::MockHost, std::time::Duration};

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

// `Io/read(h, n)` is the typed blocking read: each call yields a `chunk` of
// 1..n available bytes (here one injected line per refill, served in `n`-byte
// slices), and the third read past the data yields `eof`.
#[test]
fn struct_transparent_pair_projects() {
    let source = r#"
        use /std/{Nat, Io};
        pub record Pair(A : Type, B : Type) : Type { fst : A, snd : B }
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
        pub record Pair(A : Type, B : Type) : Type { fst : A, snd : B }
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
        pub record Meters : Type { Nat }
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
        pub record Sized : Type { n : Nat, v : Vec(Nat, n) }
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
        pub record Pair(A : Type, B : Type) : Type { fst : A, snd : B }
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
        pub record Pair(A : Type, B : Type) : Type { fst : A, snd : B }
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
        pub record Pair(A : Type, B : Type) : Type { fst : A, snd : B }
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
        record And(A : Prop, B : Prop) : Prop { fst : A, snd : B }
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
        record Box : Prop { val : Nat }
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
        record Box : Type { val : Nat }
        let b0 : Box = Box { 0 };
        let b1 : Box = Box { 1 };
        let irrelevant : Eq(b0, b1) = Eq/refl();
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(10), source, system).is_err());
}

// The function-field sugar, end to end: `label(params) -> T` in a record
// declaration and a Σ-type, `label(params) = body` in a struct literal and a
// tuple literal. The parser keeps the sugar in the AST; `to_core` undoes it —
// this pins the lowering, not just the grammar.
#[test]
fn function_field_sugar_runs_end_to_end() {
    let source = r#"
        use /std/{Nat, Io};
        pub record Api : Type { base : Nat, bump(x : Nat) -> Nat }
        let api : Api = Api { base = 3, bump(x) = x + 1 };
        let pair : { seed : Nat, twice(x : Nat) -> Nat } =
            (seed = api.bump(api.base), twice(x) = x + x);
        Io/print(Nat/to_str(pair.twice(pair.seed)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"8");
}
