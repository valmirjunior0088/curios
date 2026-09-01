//! `Vec` beyond its constructors: the proof-carrying index, the list round trip, `replicate`, `zip`, `fold`, and its witnesses.

use crate::tests::{error, run};

// The bound is decided, so a literal index inside the length discharges by reduction and the `cons` arm's successor cancellation is what carries it down the vector.
#[test]
fn get_reads_a_literal_index_under_the_decided_bound() {
    let source = r#"
        use /std/{Vec, Nat, Str};
        let v: Vec(Nat, 3) = Vec/cons(10, Vec/cons(20, Vec/cons(30, Vec/nil())));
        /std/print(Str/join(",", [Nat/to_str(Vec/get(v, 0)), Nat/to_str(Vec/get(v, 1)), Nat/to_str(Vec/get(v, 2))]))
        "#;

    assert_eq!(run(source), b"10,20,30");
}

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

// `of_list` cannot know its length statically, so it hands back a dependent pair; `to_list` on the vector inside it is the identity on the list.
#[test]
fn of_list_and_to_list_round_trip_through_the_dependent_pair() {
    let source = r#"
        use /std/{Vec, Nat, Str, List};
        let (n, v) = Vec/of_list([1, 2, 3]);
        /std/print(Str/flatten([Nat/to_str(n), ":", Str/join(",", List/map(Vec/to_list(v), Nat/to_str))]))
        "#;

    assert_eq!(run(source), b"3:1,2,3");
}

// Equal lengths are in the types, so `zip` demands no proof and `replicate` builds the second operand at exactly the first's length.
#[test]
fn zip_pairs_two_vectors_of_one_length_and_fold_walks_left_to_right() {
    let source = r#"
        use /std/{Vec, Nat, Str, List};
        let v: Vec(Nat, 3) = Vec/cons(1, Vec/cons(2, Vec/cons(3, Vec/nil())));
        let z = Vec/zip(v, Vec/replicate(3, "x"));
        let rendered = Vec/fold(z, "", (p, acc) => Str/flatten([acc, Nat/to_str(p.0), p.1]));
        /std/print(rendered)
        "#;

    assert_eq!(run(source), b"1x2x3x");
}

#[test]
fn a_vector_shows_and_compares_as_its_list() {
    let source = r#"
        use /std/{Vec, Nat, Str, Bool, Show};
        let v: Vec(Nat, 2) = Vec/cons(1, Vec/cons(2, Vec/nil()));
        let w: Vec(Nat, 2) = Vec/cons(1, Vec/cons(3, Vec/nil()));
        /std/print(Str/join(" ", [Show/show(v), Bool/to_str(v == v), Bool/to_str(v == w), Bool/to_str(v != w)]))
        "#;

    assert_eq!(run(source), b"[1, 2] true false true");
}
