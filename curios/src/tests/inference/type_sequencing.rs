//! Sequencing inside type expressions follows ordinary monadic typing; purity and totality remain the checkers' obligations.

use {crate::tests::test_support::*, curios_common::fill};

#[test]
fn a_function_computing_a_type_may_be_written_inside_its_annotation() {
    for body in [
        "let chosen() -> Option(Type) = let T = Option/some(Nat)!; Option/some(T); Option/unwrap_or(chosen(), Nat)",
        "let chosen: () -> Option(Type) = () => let T = Option/some(Nat)!; Option/some(T); Option/unwrap_or(chosen(), Nat)",
        "let chosen() -> Option(Type) = Monad/bind(Option/some(Nat), (T) => Option/some(T)); Option/unwrap_or(chosen(), Nat)",
    ] {
        let source = fill!(
            "use /std/{Nat, Option, Monad};\nlet x: (() => ${body})() = 7;\n/std/print(Nat/to_str(x))",
            body
        );
        assert_eq!(run(&source), b"7", "{body}");
    }
}

#[test]
fn a_bang_in_an_annotation_keeps_the_annotations_expected_type() {
    let error = typecheck(
        "use /std/{Nat, Io, Option};\nlet main() -> Io({}) = let x: Option/some(Nat)! = 7; Io/pure(()); main()",
    )
    .expect_err("an annotation keeps its type expectation");
    assert!(
        error.contains("the monad of this region was never determined"),
        "{error}"
    );
    assert!(error.contains("Option/some(Nat)!"), "{error}");
}

#[test]
fn an_io_description_of_a_type_is_no_type() {
    let error = typecheck(
        r#"
        use /std/{Nat, Io};
        let x: (() =>
            let chosen() -> Io(Type) =
                let T = Io/pure(Nat)!;
                Io/pure(T);
            chosen()
        )() = 7;
        /std/print(Nat/to_str(x))
        "#,
    )
    .expect_err("Io has no eliminator that could extract the type");
    assert!(
        error.contains("inferred: `Io(Type)`") && error.contains("expected: `Type`"),
        "{error}"
    );
}

// A type computed through a monadic loop stays folded: `chosen` does not descend, so `dependent`'s parameter type stands as `Option/unwrap_or(chosen(n), Nat)` and its declaration forms, and the call that hands it a `Nat` is refused, naming `chosen`.
#[test]
fn a_monadic_type_computation_that_does_not_descend_stays_folded() {
    let error = typecheck(
        r#"
        use /std/{Nat, Option};
        let chosen(n: Nat) -> Option(Type) =
            let T = Option/some(Nat)!;
            chosen(n);
        let dependent(n: Nat, x: Option/unwrap_or(chosen(n), Nat)) -> Nat = 0;
        /std/print(Nat/to_str(dependent(0, 5)))
        "#,
    )
    .expect_err("a type over a loop does not reduce");
    assert!(
        error.contains("type mismatch")
            && error.contains("`chosen` is kept folded")
            && !error.contains("not known to terminate"),
        "{error}"
    );
}
