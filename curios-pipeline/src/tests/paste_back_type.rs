//! A type a goal reports reads back where the goal stood: a signature, a function, a tuple, a family and its applications, a proposition, a lambda, an alias and a built-in operation.

use super::test_support::*;

#[test]
fn a_polymorphic_signature_over_imported_names_reads_back() {
    let source = r#"
        use /std/{List};
        pub let t: ? = List/map;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_local_function_type_reads_back() {
    let source = r#"
        use /std/{Nat};
        let double(n: Nat) -> Nat = n + n;
        pub let t: ? = double;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_tuple_type_reads_back() {
    let source = r#"
        use /std/{Nat, Bool};
        pub let t: ? = (1, true);
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn an_applied_family_reads_back() {
    let source = r#"
        use /std/{Nat, Option};
        pub let t: ? = Option/some(1);
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_proposition_over_an_implicit_parameter_reads_back() {
    let source = r#"
        use /std/{Nat, Eq};
        pub let t: ? = Eq/refl(@Nat, @3);
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_type_holding_a_lambda_reads_back() {
    let source = r#"
        use /std/{Nat, Eq};
        pub let t: ? = Eq/refl(@(Nat) -> Nat, @(n: Nat) => n);
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn an_indexed_family_and_its_applications_read_back() {
    let source = r#"
        use /std/{Nat};
        induct Vec(T: Type): (n: Nat) -> Type
        | nil(): (0)
        | cons(@m: Nat, x: T, xs: Vec(T)(m)): (m + 1)
        end
        pub let family: ? = Vec;
        pub let at_nat: ? = Vec(Nat);
        pub let value: ? = Vec/cons(7, Vec/nil());
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn an_operator_under_an_abstract_witness_reads_back() {
    // `a + a` elaborates to a projection off the `use Add(A)` binder, which prints as the operator.
    let source = r#"
        use /std/{Eq};
        use /std/ops/{Add};
        pub let f(@A: Type, use Add(A), a: A) -> Eq()(a + a, a + a) = Eq/refl();
        pub let t: ? = f;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_call_with_a_witness_between_two_implicits_reads_back() {
    // `pick(@K, use _, @V, key, value)`: resolution would restore the witness, so it is not spelled, but an `@` written past a slot left out would be read at that slot, so the slot holds its place.
    let source = r#"
        use /std/{Nat, Bool, Show};
        use /std/Bool/{Holds};
        pub let pick(@K: Type, use Show(K), @V: Type, key: K, value: V) -> Bool = true;
        pub let held(@K: Type, use Show(K), @V: Type, key: K, value: V) -> Holds(pick(key, value)) =
            /std/Bool/True/qed();
        pub let t: ? = held;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_standard_alias_taking_a_witness_reads_back() {
    // `/std/Parse`'s `Parse(I, use Input(I), A)`: the `use` argument is the function type's own binder.
    let source = r#"
        use /std/{Parse};
        pub let t: ? = Parse/pure;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_type_over_names_nothing_imported_reads_back() {
    // Nothing is imported, so `List` resolves nowhere, however unambiguous a suffix it is: every name is written by a path that reaches it.
    let source = r#"
        pub let t: ? = /std/List/map;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_built_in_operation_reads_back() {
    // A built-in operation is spelled as the `/std` declaration that re-exports it resolves from the reader, like any other name.
    let source = r#"
        use /std/{Nat, Eq};
        pub let t: ? = (a: Nat, b: Nat) => Eq/refl(@Nat, @Nat/shl(a, b));
        /std/print("")
    "#;

    reads_back(source).unwrap();
}
