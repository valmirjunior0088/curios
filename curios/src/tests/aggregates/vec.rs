//! The bound a read of a vector's list carries past the vector's count, which nothing fills. `Vec`'s surface — its list read under its count, the list round trip and `zip` — is `/std/Vec`'s own tests'.

use crate::tests::test_support::*;

// Past the count the bound the vector's count carries onto its list reduces to `False`, and nothing fills it: the report names `List/get`'s binder.
#[test]
fn a_read_of_the_list_past_the_vectors_count_is_refused_by_the_bound() {
    let source = r#"
        use /std/{Vec, List, Nat, Str};
        let v: Vec(Nat, 2) = Vec/cons(10, Vec/cons(20, Vec/nil()));
        /std/print(Nat/to_str(List/get(v.list, 2)))
        "#;

    assert!(
        error(source).contains("ok"),
        "the refusal names the bound's binder"
    );
}
