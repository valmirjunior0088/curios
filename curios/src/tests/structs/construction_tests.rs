//! Declaring a struct and building one: labels, parameter inference, dependent fields, and the sorts a struct may carry.

use super::super::{error, run};

#[test]
fn named_fields_run_end_to_end() {
    let source = r#"
        use /std/{Vec, Nat, Handle};
        let p : { n : Nat, v : Vec(Nat, n) } =
            (n = 2, v = Vec/cons(30, Vec/cons(12, Vec/nil())));
        let total(@k : Nat, v : Vec(Nat, k), acc : Nat) -> Nat =
            match v : (_, _) => Nat
            | nil() => acc
            | cons(@m, x, xs) => total(xs, Nat/add(acc, x))
            end;
        /std/print(Nat/to_str(Nat/add(total(p.v, 0), Nat/mul(p.0, 0))))
        "#;

    assert_eq!(run(source), b"42");
}

// A transparent record: build with a pinned head, project by label and by index — both resolve to the same positional projection.
#[test]
fn struct_transparent_pair_projects() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair(Nat, Nat) { fst = 2, snd = 5 };
        /std/print(Nat/to_str(Nat/add(p.fst, p.1)))
        "#;

    assert_eq!(run(source), b"7");
}

// The bare-name head infers the parameters from the fields (and the expected type at the binding).
#[test]
fn struct_parameter_inference_at_construction() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 4, snd = 3 };
        /std/print(Nat/to_str(Nat/mul(p.fst, p.snd)))
        "#;

    assert_eq!(run(source), b"12");
}

// A zero-cost newtype: a single positional field, projected with `.0`. It erases to its bare field, so the projection elides at runtime.
#[test]
fn struct_newtype_projects() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Meters : pub Type { Nat }
        let m : Meters = Meters { 5 };
        /std/print(Nat/to_str(m.0))
        "#;

    assert_eq!(run(source), b"5");
}

// A dependent field: a later field's type mentions an earlier field (the vector's length indexes its type).
#[test]
fn struct_dependent_fields_run_end_to_end() {
    let source = r#"
        use /std/{Vec, Nat, Handle};
        pub struct Sized : pub Type { n : Nat, v : Vec(Nat, n) }
        let s : Sized = Sized { n = 2, v = Vec/cons(30, Vec/cons(12, Vec/nil())) };
        let total(@k : Nat, v : Vec(Nat, k), acc : Nat) -> Nat =
            match v : (_, _) => Nat
            | nil() => acc
            | cons(@m, x, xs) => total(xs, Nat/add(acc, x))
            end;
        /std/print(Nat/to_str(total(s.v, 0)))
        "#;

    assert_eq!(run(source), b"42");
}

// The motivating case: an abstract type — public type, hidden representation — usable only through exported smart constructors/accessors in its module.
#[test]
fn struct_abstract_smart_constructor_round_trips() {
    let source = r#"
        use /std/{Nat, Handle};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius : Type { Nat }
            pub let of_nat(n : Nat) -> Celsius = Celsius { n };
            pub let to_nat(c : Celsius) -> Nat = c.0;
        end
        /std/print(Nat/to_str(Celsius/to_nat(Celsius/of_nat(42))))
        "#;

    assert_eq!(run(source), b"42");
}

// Diagnostics name binders with the source names the user wrote, not the `hint#counter` gensyms elaboration opens them under (axis (a)): the inferred function type must read `(n : Nat)`, never `(n#3 : Nat)`.
#[test]
fn struct_is_not_a_tuple() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : { fst : Nat, snd : Nat } = Pair { fst = 1, snd = 2 };
        /std/print("no")
        "#;

    error(source);
}

// A struct literal must supply exactly the declared fields, in order.
#[test]
fn struct_wrong_field_count_rejected() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 1 };
        /std/print("no")
        "#;

    error(source);
}

// Written field labels are validated positionally — no reordering.
#[test]
fn struct_field_label_out_of_order_rejected() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { snd = 1, fst = 2 };
        /std/print("no")
        "#;

    error(source);
}

// A struct literal whose head names a non-struct binding is rejected as `NotAStructType` (its type is reported), not misreported as unbound.
#[test]
fn struct_literal_non_struct_head_rejected() {
    let source = r#"
        use /std/{Nat, Handle};
        let Foo : Nat = 3;
        let bad : Nat = Foo { x = 1 };
        /std/print("no")
        "#;

    let error = error(source);
    assert!(error.contains("struct type"), "unexpected error: {error}");
}

// A `Prop`-sorted struct whose fields are all propositions is a sub-singleton: every projection lands in `Prop`, so proof irrelevance leaks nothing. It is accepted, and — its content being non-informative — erases away: the program compiles and runs, the projected proof contributing no runtime code while the ordinary `Nat` computation still produces its result.
#[test]
fn prop_struct_with_prop_fields_runs() {
    let source = r#"
        use /std/{Nat, Eq, Handle};
        struct And(A : Prop, B : Prop) : pub Prop { fst : A, snd : B }
        let p : And(Eq(0, 0), Eq(1, 1)) = And { Eq/refl(), Eq/refl() };
        let proof : Eq(0, 0) = p.fst;
        /std/print(Nat/to_str(7))
        "#;

    assert_eq!(run(source), b"7");
}

// A `Prop`-sorted struct with an informative (`Type`-sorted) field is rejected at declaration. Projection is an unguarded eliminator, so admitting it under proof irrelevance proves `Eq(b0, b1)` for distinct `b0`, `b1` — and thence `Eq(0, 1)` and `False`. The soundness-critical regression (bare `: Prop`).
#[test]
fn prop_struct_with_informative_field_rejected() {
    let source = r#"
        use /std/{Nat, Eq, Handle};
        struct Box : pub Prop { val : Nat }
        let b0 : Box = Box { 0 };
        let b1 : Box = Box { 1 };
        let irrelevant : Eq(b0, b1) = Eq/refl();
        let get(b : Box) -> Nat = b.val;
        let zero_eq_one : Eq(0, 1) = Eq/cong(get, irrelevant);
        /std/print("no")
        "#;

    let error = error(source);
    assert!(error.contains("informative"), "unexpected error: {error}");
}

// Control: the same record at the default `Type` sort gets no proof irrelevance, so `Eq(b0, b1)` for distinct values is correctly rejected by conversion — confirming the `Prop` sort was the only door to the contradiction, and that closing it leaves ordinary records untouched.
#[test]
fn type_struct_distinct_values_not_convertible() {
    let source = r#"
        use /std/{Nat, Eq, Handle};
        struct Box : pub Type { val : Nat }
        let b0 : Box = Box { 0 };
        let b1 : Box = Box { 1 };
        let irrelevant : Eq(b0, b1) = Eq/refl();
        /std/print("no")
        "#;

    error(source);
}

// The function-field sugar, end to end: `label(params) -> T` in a struct declaration and a Σ-type, `label(params) = body` in a struct literal and a tuple literal. The parser keeps the sugar in the AST; `into_core` undoes it — this pins the lowering, not just the grammar.
#[test]
fn function_field_sugar_runs_end_to_end() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Api : pub Type { base : Nat, bump(x : Nat) -> Nat }
        let api : Api = Api { base = 3, bump(x) = x + 1 };
        let pair : { seed : Nat, twice(x : Nat) -> Nat } =
            (seed = api.bump(api.base), twice(x) = x + x);
        /std/print(Nat/to_str(pair.twice(pair.seed)))
        "#;

    assert_eq!(run(source), b"8");
}

// A parameter whose type is universe-polymorphic. The former's body is the `StructType` node over its own parameters, checked against the registry arity — which the lowerer files as the *written* telescope, carrying no universe instances. The binders that body meets come from the former's elaborated type and do carry them, and the two can never be reconciled: a universe-polymorphic global reached through a bare `Var` does not unfold, because there is no instance to substitute into its body. So the raw side is irreducible and conversion is handed a problem no reduction decides. The declared type's telescope is the one both sides now share.
#[test]
fn a_parameter_typed_by_a_universe_polymorphic_family_is_admitted() {
    let source = r#"
        use /std/{Nat, List, Handle};
        struct Boxed(xs : List(Nat)) : pub Type { size : Nat }
        let b : Boxed([1, 2]) = Boxed([1, 2]) { size = 2 };
        /std/print(Nat/to_str(b.size))
        "#;

    assert_eq!(run(source), b"2");
}

// The same, where the polymorphism is the declaration's own rather than an intrinsic's: a family carrying a `Type` payload is universe-polymorphic, and a bare reference to it is what the registry held.
#[test]
fn a_parameter_typed_by_a_family_carrying_a_type_payload_is_admitted() {
    let source = r#"
        use /std/{Nat, Handle};
        induct Carrier : pub Type | wrap(Type) end
        struct Tagged(c : Carrier) : pub Type { size : Nat }
        let t : Tagged(Carrier/wrap(Nat)) = Tagged(Carrier/wrap(Nat)) { size = 7 };
        /std/print(Nat/to_str(t.size))
        "#;

    assert_eq!(run(source), b"7");
}

// A parameter telescope whose later entry names an earlier one, so sharing has to open both sides against one binder rather than splice two telescopes side by side.
#[test]
fn a_dependent_parameter_telescope_is_admitted() {
    let source = r#"
        use /std/{Nat, List, Handle};
        struct Dep(A : Type, xs : List(A)) : pub Type { size : Nat }
        let d : Dep(Nat, [1]) = Dep(Nat, [1]) { size = 1 };
        /std/print(Nat/to_str(d.size))
        "#;

    assert_eq!(run(source), b"1");
}

// A concept is struct-backed, so its parameters reach the same check by the same route. Supplied as a dictionary rather than registered: a list literal is not a keyable head, which is the keying rule doing its job and not this test's subject.
#[test]
fn a_concept_parameter_typed_by_a_universe_polymorphic_family_is_admitted() {
    let source = r#"
        use /std/{Nat, List, Handle};
        concept Sized(F : List(Type)) : pub Type { size(Nat) -> Nat, }
        let s : Sized([Nat]) = Sized([Nat]) { size(n) = n };
        /std/print(Nat/to_str(Sized/size(use s, 4)))
        "#;

    assert_eq!(run(source), b"4");
}

// The field telescope stays with the late rebuild, and this is why: a field type may name the struct itself, which a parameter type cannot. Sharing the parameters early must not drag the fields forward with them.
#[test]
fn a_field_naming_its_own_struct_is_admitted_beside_a_shared_parameter() {
    let source = r#"
        use /std/{Nat, Option, Handle};
        struct Cell(A : Type) : pub Type { value : A, next : Option(Cell(A)) }
        let c : Cell(Nat) = Cell(Nat) { value = 3, next = Option/none() };
        /std/print(Nat/to_str(c.value))
        "#;

    assert_eq!(run(source), b"3");
}

// What a report calls an unlabeled tuple. Labels are part of a tuple type's identity, so a mismatch between a labeled and an unlabeled product is a real refusal — and the message has to spell the unlabeled side the way source writes it, rather than inventing positional names for fields that have none.
#[test]
fn a_mismatch_names_an_unlabeled_tuple_the_way_source_writes_it() {
    let source = r#"
        use /std/{Nat, Bool, Str, Handle};
        let p : {fst : Nat, snd : Bool, thd : Str} = (1, true, "x");
        let q : {Nat, Bool, Str} = p;
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("expected: {Nat, Bool, Str}"),
        "expected the unlabeled product spelled as written:\n{report}"
    );
}

// Structures whose fields name one another are declared as one group: the formers lower to one recursive item, so each field telescope elaborates with every former defined. A lone structure still names itself with nothing said.
#[test]
fn a_struct_group_may_name_one_another() {
    let source = r#"
        use /std/{Nat, Option, Handle};
        struct Node : pub Type { value: Nat, next: Option(Edge) }
        and Edge : pub Type { weight: Nat, to: Node }
        let n : Node = Node {
            value = 1,
            next = Option/some(Edge { weight = 2, to = Node { value = 3, next = Option/none() } }),
        };
        /std/print(Nat/to_str(match n.next | some(e) => e.to.value | none() => 0 end))
        "#;

    assert_eq!(run(source), b"3");
}
