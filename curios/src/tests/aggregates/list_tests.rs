//! The `List` functions every peer ships: structural predicates and searches, the fold-shaped builders, `traverse` and `each` over a monad, and the stable sort.

use crate::tests::{run, typecheck};

#[test]
fn predicates_and_searches_answer_over_the_elements() {
    let source = r#"
        use /std/{Nat, List, Option, Str, Bool, Show};
        let l = [3, 1, 4, 1, 5];
        let empty: List(Nat) = [];
        /std/print(Str/join(",", [
            Bool/to_str(List/any(l, (x: Nat) => x > 4)), Bool/to_str(List/all(l, (x: Nat) => x > 0)), Bool/to_str(List/all(l, (x: Nat) => x > 1)),
            Bool/to_str(List/contains(l, 4)), Bool/to_str(List/contains(l, 2)),
            Show/show(List/index_of(l, 1)), Show/show(List/index_of(l, 9)),
            Show/show(List/find_index(l, (x: Nat) => x > 3)), Show/show(List/find(l, (x: Nat) => x > 3)),
            Show/show(List/last(l)), Show/show(List/last(empty))
        ]))
        "#;

    assert_eq!(
        run(source),
        b"true,true,false,true,false,some(1),none(),some(2),some(4),some(5),none()"
    );
}

#[test]
fn builders_reshape_a_list() {
    let source = r#"
        use /std/{Nat, List, Option, Str, Show};
        let l = [3, 1, 4, 1, 5];
        let (evens, odds) = List/partition(l, (x: Nat) => Nat/is_even(x));
        let (xs, ys) = List/unzip([(1, "a"), (2, "b")]);
        /std/print(Str/join(" ", [
            Show/show(List/reverse(l)),
            Show/show(List/filter(l, (x: Nat) => x > 2)),
            Show/show(List/filter_map(l, (x: Nat) => match x > 2 | true => Option/some(x * 10) | false => Option/none() end)),
            Show/show(List/take(l, 2)), Show/show(List/take(l, 9)),
            Show/show(List/take_while(l, (x: Nat) => x < 4)), Show/show(List/drop_while(l, (x: Nat) => x < 4)),
            Show/show(evens), Show/show(odds),
            Show/show(List/zip(l, ["a", "b"])), Show/show(xs), Show/show(ys),
            Show/show(List/range(2, 5)), Show/show(List/range(5, 2)), Show/show(List/replicate(3, 7)),
            Show/show(List/concat_map([1, 2], (x: Nat) => [x, x])), Show/show(List/intersperse([1, 2, 3], 0))
        ]))
        "#;

    assert_eq!(
        run(source),
        br#"[5, 1, 4, 1, 3] [3, 4, 5] [30, 40, 50] [3, 1] [3, 1, 4, 1, 5] [3, 1] [4, 1, 5] [4] [3, 1, 1, 5] [(3, a), (1, b)] [1, 2] [a, b] [2, 3, 4] [] [7, 7, 7] [1, 1, 2, 2] [1, 0, 2, 0, 3]"#
    );
}

// `traverse` collects under any `Monad`: over `Option` a single `none` sinks the whole list, and over `Io` the effects run in element order.
#[test]
fn traverse_and_each_run_under_a_monad() {
    let source = r#"
        use /std/{Nat, List, Option, Str, Show, Io};
        let halve(x: Nat) -> Option(Nat) = match Nat/is_even(x) | true => Option/some(x / 2) | false => Option/none() end;
        let all_even = List/traverse([2, 4, 6], halve);
        let one_odd = List/traverse([2, 3, 6], halve);
        let _ = /std/print(Str/join(" ", [Show/show(all_even), Show/show(one_odd), ""]))!;
        List/each([1, 2, 3], (x: Nat) => /std/print(Nat/to_str(x)))
        "#;

    assert_eq!(run(source), b"some([1, 2, 3]) none() 123");
}

// The sort is the `; ih` right fold inserting before equals, which is what makes it stable: the two `1`s keep their order, and `sort_by` under a reversed comparator reverses the order of the distinct keys while equal keys still keep theirs.
#[test]
fn sort_is_stable_and_sort_by_takes_a_comparator() {
    let source = r#"
        use /std/{Nat, Str, List, Ordering, Show};
        let pairs = [(2, "b"), (1, "x"), (2, "a"), (1, "y")];
        let by_key = List/sort_by(pairs, (p: {Nat, Str}, q: {Nat, Str}) => Nat/cmp(p.0, q.0));
        let by_key_desc = List/sort_by(pairs, (p: {Nat, Str}, q: {Nat, Str}) => Nat/cmp(q.0, p.0));
        /std/print(Str/join(" ", [Show/show(List/sort([3, 1, 2])), Show/show(List/sort(["b", "a", "c"])), Show/show(by_key), Show/show(by_key_desc)]))
        "#;

    assert_eq!(
        run(source),
        b"[1, 2, 3] [a, b, c] [(1, x), (1, y), (2, b), (2, a)] [(2, b), (2, a), (1, x), (1, y)]"
    );
}

// Every predicate and search is a case split on `[head, ..tail]` with a call on the tail, which the totality analysis grades as a decrease, so `find` and `any` are usable where a proposition depends on them — the shape the command-line specification asked of `find`.
#[test]
fn a_structural_search_is_accepted_in_a_type() {
    typecheck(
        r#"
        use /std/{Nat, List, Option, Bool, Eq, True};
        let found: Eq(List/find([1, 2, 3], (x: Nat) => x > 1), Option/some(2)) = Eq/refl();
        let some_even: Eq(List/any([1, 2, 3], (x: Nat) => Nat/is_even(x)), true) = Eq/refl();
        /std/print("ok")
        "#,
    )
    .expect("structural searches compute in a type");
}
