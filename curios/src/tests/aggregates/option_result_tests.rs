//! The bound `Option/get` carries, and the report naming it when nothing fills it. The `Option` and `Result` surface itself is the corpus's `/aggregates/option_result`.

use crate::tests::{error, run};

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
