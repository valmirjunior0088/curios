//! Sequencing inside type expressions follows ordinary monadic typing; purity and totality remain the checkers' obligations.

use {
    crate::tests::{run, typecheck},
    curios_utilities::fill,
};

#[test]
fn a_function_computing_a_type_may_be_written_inside_its_annotation() {
    for body in [
        "let chosen() -> Option(Type) = let T = Option/some(Nat)!; Option/some(T); Option/unwrap_or(chosen(), Nat)",
        "let chosen: () -> Option(Type) = () => let T = Option/some(Nat)!; Option/some(T); Option/unwrap_or(chosen(), Nat)",
        "let chosen() -> Option(Type) = Monad/bind(Option/some(Nat), (T) => Option/some(T)); Option/unwrap_or(chosen(), Nat)",
    ] {
        let source = fill!(
            "use /std/{Nat, Option, Monad};\nlet x: (${body}) = 7;\n/std/print(Nat/to_str(x))",
            body
        );
        assert_eq!(run(&source), b"7", "{body}");
    }
}

#[test]
fn a_value_computing_a_type_can_be_used_in_an_annotation() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Option};
        let chosen: Option(Type) =
            let T = Option/some(Nat)!;
            Option/some(T);
        let x: Option/unwrap_or(chosen, Nat) = 7;
        /std/print(Nat/to_str(x))
        "#),
        b"7"
    );
}

#[test]
fn a_bang_in_an_annotation_keeps_the_annotations_expected_type() {
    for body in [
        "let x: Option/some(Nat)! = 7; Io/pure(())",
        "let main() -> Io({}) = let x: Option/some(Nat)! = 7; Io/pure(()); main()",
    ] {
        let source = fill!("use /std/{Nat, Io, Option};\n${body}", body);
        let error = typecheck(&source).expect_err("an annotation keeps its type expectation");
        assert!(
            error.contains("the monad of this region was never determined"),
            "{error}"
        );
        assert!(error.contains("Option/some(Nat)!"), "{error}");
    }
}

#[test]
fn an_io_description_of_a_type_is_no_type() {
    let error = typecheck(
        r#"
        use /std/{Nat, Io};
        let x: (
            let chosen() -> Io(Type) =
                let T = Io/pure(Nat)!;
                Io/pure(T);
            chosen()
        ) = 7;
        /std/print(Nat/to_str(x))
        "#,
    )
    .expect_err("Io has no eliminator that could extract the type");
    assert!(
        error.contains("inferred: Io(Type)") && error.contains("expected: Type"),
        "{error}"
    );
}

#[test]
fn a_monadic_type_computation_still_must_terminate() {
    let error = typecheck(
        r#"
        use /std/{Nat, Option};
        let chosen(n: Nat) -> Option(Type) =
            let T = Option/some(Nat)!;
            chosen(n);
        let dependent(n: Nat, x: Option/unwrap_or(chosen(n), Nat)) -> Nat = 0;
        /std/print("unreachable")
        "#,
    )
    .expect_err("a type cannot depend on a partial computation");
    assert!(
        error.contains("type position") && error.contains("not known to terminate"),
        "{error}"
    );
}
