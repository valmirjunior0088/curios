//! The bound `Option/get` carries, and the report naming it when nothing fills it. The `Option` and `Result` surface itself is the corpus's `/aggregates/option_result`.

use crate::tests::error;

// An opaque option's bound reduces to nothing, and the report names the binder that was not filled rather than guessing.
#[test]
fn get_on_an_opaque_option_names_the_unfilled_bound() {
    let source = r#"
        use /std/{Option, Nat, Str};
        let first(o: Option(Nat)) -> Nat = Option/get(o);
        /std/print(Nat/to_str(first(Option/some(3))))
        "#;

    assert!(
        error(source).contains("ok"),
        "the refusal names the bound's binder"
    );
}

#[test]
fn reexported_option_serves_repeated_occurrences_and_higher_universes() {
    let source = r#"
        use /std/{Option, Nat, Str, Spell, Io};
        use /std/Option/{some, none};
        let pair(@A: Type, a: A) -> {Option(A), Option(A)} = (some(a), none());
        let small: {Option(Nat), Option(Nat)} = pair(7);
        let large: {Option(Type), Option(Type)} = pair(Nat);
        let carried: Type = match large.0 | some(t) => t | none() => Nat end;
        let nested: Option(Option(Nat)) = some(small.0);
        /std/print(Spell/spell(nested))
    "#;
    assert_eq!(crate::tests::run(source), b"Option/some(Option/some(7))");
}
