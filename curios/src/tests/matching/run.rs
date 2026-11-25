//! Runs: a sequence pattern's elements are patterns of their own, a closed run ends where they do, and an open one's rest follows them.

use crate::test_support::*;

/// A closed run matches a list of exactly its elements, and an open one a list they begin.
#[test]
fn a_run_matches_the_lists_it_spells() {
    let source = r#"
        use /std/{List, Nat, Str};
        let shape(l: List(Nat)) -> Nat =
            match l
            | [] => 0
            | [_] => 1
            | [_, _] => 2
            | [_, _, .._] => 3
            end;
        /std/print(Str/join(" ", List/map([shape([]), shape([7]), shape([7, 8]), shape([7, 8, 9])], Nat/to_str)))
        "#;

    assert_eq!(run(source), b"0 1 2 3");
}

/// An element is taken apart as a constructor's payload is, and the rest after it is the list's tail, which a recursion reads as smaller.
#[test]
fn an_element_is_taken_apart_as_a_payload_is() {
    let source = r#"
        use /std/{List, Nat, Option, Str};
        let present(l: List(Option(Nat))) -> List(Nat) =
            match l
            | [] => []
            | [none(), ..rest] => present(rest)
            | [some(value), ..rest] => [value, ..present(rest)]
            end;
        /std/print(Str/join(" ", List/map(present([Option/some(1), Option/none(), Option/some(3)]), Nat/to_str)))
        "#;

    assert_eq!(run(source), b"1 3");
}

/// A natural element splits its column as a natural payload does: `0` and a successor are induction's two cases.
#[test]
fn a_natural_element_splits_as_induction() {
    let source = r#"
        use /std/{List, Nat};
        let zeros(l: List(Nat)) -> Nat =
            match l
            | [] => 0
            | [0, ..rest] => 1 + zeros(rest)
            | [_ + 1, ..rest] => zeros(rest)
            end;
        /std/print(Nat/to_str(zeros([0, 5, 0, 0])))
        "#;

    assert_eq!(run(source), b"3");
}

/// A run of one stands before the open run it is a case of, as an exception before its general arm.
#[test]
fn a_run_of_one_is_an_exception_to_an_open_run() {
    let source = r#"
        use /std/{List, Nat, Option};
        let last(l: List(Nat)) -> Option(Nat) =
            match l
            | [] => Option/none()
            | [only] => Option/some(only)
            | [_, ..rest] => last(rest)
            end;
        /std/print(Nat/to_str(Option/unwrap_or(last([4, 5, 6]), 0)))
        "#;

    assert_eq!(run(source), b"6");
}

/// A recursion on a rest two elements down descends, the rest being a tail's tail.
#[test]
fn a_rest_two_elements_down_is_smaller_than_the_list() {
    let source = r#"
        use /std/{List, Nat};
        let pairs(l: List(Nat)) -> Nat =
            match l
            | [a, b, ..rest] => a * b + pairs(rest)
            | _ => 0
            end;
        /std/print(Nat/to_str(pairs([1, 2, 3, 4, 5])))
        "#;

    assert_eq!(run(source), b"14");
}

/// Two runs neither of which is a case of the other meet at the run both match, which the refusal writes flat.
#[test]
fn two_runs_that_meet_are_refused_at_the_run_they_share() {
    let source = r#"
        use /std/{List, Nat};
        let f(l: List(Nat)) -> Nat =
            match l
            | [0, .._] => 0
            | [_, 1] => 1
            | _ => 2
            end;
        /std/print(Nat/to_str(f([0, 1])))
        "#;

    let error = error(source);
    assert!(
        error.contains("both match `[0, 1]`"),
        "unexpected error: {error}"
    );
}

/// A closed run after an open one that begins with as much is a case of it, and no value reaches it.
#[test]
fn a_closed_run_an_open_one_covers_is_unreachable() {
    let source = r#"
        use /std/{List, Nat};
        let f(l: List(Nat)) -> Nat =
            match l
            | [a, .._] => a
            | [_, b] => b
            | [] => 0
            end;
        /std/print(Nat/to_str(f([1, 2])))
        "#;

    let error = error(source);
    assert!(
        error.contains("unreachable arm") && error.contains("`[a, .._]` is among them"),
        "unexpected error: {error}"
    );
}

/// A run its arms leave is refused as the value no arm matches, written as a run.
#[test]
fn a_run_no_arm_matches_is_reported_as_a_run() {
    for (source, missing) in [
        (
            r#"
            use /std/{List, Nat};
            let f(l: List(Nat)) -> Nat =
                match l
                | [] => 0
                | [_] => 1
                end;
            /std/print(Nat/to_str(f([])))
            "#,
            "no arm matches `[_, _, .._]`",
        ),
        (
            r#"
            use /std/{List, Nat, Option};
            let f(l: List(Option(Nat))) -> Nat =
                match l
                | [] => 0
                | [none(), .._] => 1
                end;
            /std/print(Nat/to_str(f([])))
            "#,
            "no arm matches `[some(_), .._]`",
        ),
    ] {
        let error = error(source);
        assert!(error.contains(missing), "unexpected error: {error}");
    }
}

/// A run stands under a guard, beside its alternatives and in a bind guard as any pattern does.
#[test]
fn a_run_takes_guards_alternatives_and_a_bind() {
    let source = r#"
        use /std/{List, Nat, Str};
        let gap(l: List(Nat)) -> Nat =
            match l
            | [a, b], a < b => b - a
            | [a] | [a, _] => a
            | _ => 0
            end;
        let ending(l: List(Nat)) -> Nat =
            match
            | [last, .._] = List/reverse(l) => last
            | _ => 0
            end;
        /std/print(Str/join(" ", List/map([gap([2, 7]), gap([7, 2]), gap([5]), gap([]), ending([1, 2, 3])], Nat/to_str)))
        "#;

    assert_eq!(run(source), b"5 7 5 0 3");
}

/// A packed run is a run under `0x[`, each element a byte.
#[test]
fn a_packed_run_reads_its_bytes_from_the_front() {
    let source = r#"
        use /std/{Byte, Bytes, Nat};
        let second(b: Bytes) -> Nat =
            match b
            | 0x[_, s, .._] => Byte/to_nat(s)
            | _ => 0
            end;
        /std/print(Nat/to_str(second(0x[1, 2, 3])))
        "#;

    assert_eq!(run(source), b"2");
}
