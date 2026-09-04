//! The bound `Vec/get` carries past the end, which nothing fills. `Vec`'s surface — the index inside the length, the list round trip, `zip` and `fold` — is the corpus's `/aggregates/vec`.

use crate::tests::{error, run};

// Past the end the bound reduces to `False`, and nothing fills it: the report names the binder, exactly as an out-of-range `Str/get` does.
#[test]
fn get_past_the_length_is_refused_by_the_bound() {
    let source = r#"
        use /std/{Vec, Nat, Str};
        let v: Vec(Nat, 2) = Vec/cons(10, Vec/cons(20, Vec/nil()));
        /std/print(Nat/to_str(Vec/get(v, 2)))
        "#;

    assert!(
        error(source).contains("ok"),
        "the refusal names the bound's binder"
    );
}
