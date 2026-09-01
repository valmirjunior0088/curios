//! The ordering witnesses beyond the numerics: `Cmp` and `Ord` on strings, bytes, booleans, lists, options and results, and `Eql`/`Show` on maps.

use crate::tests::run;

// UTF-8 bytewise order is scalar-value order, so `Cmp(Str)` compares code points without decoding: `é` (U+00E9) sorts after `z`, a prefix sorts before its extension, and the empty string before everything.
#[test]
fn strings_compare_by_code_point_and_bytes_and_booleans_bytewise() {
    let source = r#"
        use /std/{Str, Bytes, Bool};
        /std/print(Str/join(",", [
            Bool/to_str("a" < "b"), Bool/to_str("b" < "a"), Bool/to_str("z" < "é"),
            Bool/to_str("ab" < "abc"), Bool/to_str("" <= "a"), Bool/to_str("abc" >= "abd"),
            Bool/to_str(x[1, 2] < x[1, 3]), Bool/to_str(x[1, 2, 0] > x[1, 2]),
            Bool/to_str(false < true), Bool/to_str(true <= false)
        ]))
        "#;

    assert_eq!(
        run(source),
        b"true,false,true,true,true,false,true,true,true,false"
    );
}

// `List` is lexicographic with the shorter prefix first, `none` sits below `some`, and `failure` below `success`; each is stated once in its module and read here through `Ord/cmp`.
#[test]
fn lists_options_and_results_order_through_ord() {
    let source = r#"
        use /std/{Str, Nat, List, Option, Result, Order, Ord, Show, Bool};
        let none: Option(Nat) = Option/none();
        let bad: Result(Nat, Str) = Result/failure("e");
        /std/print(Str/join(",", [
            Show/show(Ord/cmp([1, 2], [1, 3])),
            Show/show(Ord/cmp([1, 2], [1, 2])),
            Show/show(Ord/cmp([1], [1, 0])),
            Show/show(Ord/cmp(["b"], ["a", "z"])),
            Show/show(Ord/cmp(none, Option/some(0))),
            Show/show(Ord/cmp(Option/some(2), Option/some(1))),
            Show/show(Ord/cmp(bad, Result/success(0))),
            Show/show(Ord/cmp(Result/failure("a"), bad)),
            Bool/to_str([1, 2] < [2]),
            Bool/to_str(Option/some(1) >= none)
        ]))
        "#;

    assert_eq!(run(source), b"lt,eq,lt,gt,lt,gt,lt,lt,true,true");
}

// The trie is canonical, so two maps with the same entries are one map whatever order built them, and `Show` renders the entries in key order — the keys as the `Bytes` the trie holds, so `"x"` is `78`.
#[test]
fn maps_compare_and_show_by_their_entries() {
    let source = r#"
        use /std/{Str, Nat, Map, Show, Bool};
        let a: Map(Nat) = Map/of([("x", 1), ("y", 2)]);
        let b: Map(Nat) = Map/of([("y", 2), ("x", 1)]);
        let c: Map(Nat) = Map/of([("x", 1), ("y", 3)]);
        /std/print(Str/join(" ", [Bool/to_str(a == b), Bool/to_str(a == c), Bool/to_str(a != c), Show/show(a)]))
        "#;

    assert_eq!(run(source), b"true false true {78: 1, 79: 2}");
}
