//! The `Option` and `Result` surface beyond `bind` and `map`: fallbacks, filtering, the conversions between them, and the proof-carrying `Option/get`.

use crate::tests::{error, run};

#[test]
fn option_fallbacks_filter_and_flatten() {
    let source = r#"
        use /std/{Option, Nat, Str, Show};
        let a: Option(Nat) = Option/some(1);
        let b: Option(Nat) = Option/none();
        /std/print(Str/join(" ", [
            Show/show(Option/or(b, a)),
            Show/show(Option/or_else(b, () => Option/some(9))),
            Show/show(Option/filter(a, (n: Nat) => n > 5)),
            Show/show(Option/flatten(Option/some(a))),
            Nat/to_str(Option/unwrap_or_else(b, () => 7))
        ]))
        "#;

    assert_eq!(run(source), b"some(1) some(9) none() some(1) 7");
}

#[test]
fn option_and_result_convert_into_each_other() {
    let source = r#"
        use /std/{Option, Result, Nat, Str, Show};
        let a: Option(Nat) = Option/some(1);
        let b: Option(Nat) = Option/none();
        let ok: Result(Nat, Str) = Result/success(2);
        let bad: Result(Nat, Str) = Result/failure("no");
        /std/print(Str/join(" ", [
            Show/show(Option/to_result(a, "missing")),
            Show/show(Option/to_result(b, "missing")),
            Show/show(Result/to_option(ok)),
            Show/show(Result/to_option(bad)),
            Nat/to_str(Result/unwrap_or_else(bad, (e: Str) => Str/len(e)))
        ]))
        "#;

    assert_eq!(run(source), b"success(1) failure(missing) some(2) none() 2");
}

// `IsSome` is decided by a match, so `get` on a literal `some` fills its bound by reduction.
#[test]
fn get_on_a_literal_some_discharges_its_bound() {
    let source = r#"
        use /std/{Option, Nat, Str};
        /std/print(Nat/to_str(Option/get(Option/some(3))))
        "#;

    assert_eq!(run(source), b"3");
}

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
