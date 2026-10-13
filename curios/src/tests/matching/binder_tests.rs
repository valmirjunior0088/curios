//! A binder beside a concrete pattern: the cases a row is reached under, the value no arm matches, and the arm no value reaches.

use crate::tests::{error, run};

/// `function` put to `calls` over `z`, a `Nat` that is zero: the answers as one numeral, the first call's in the units, the second's in the tens. Once with `z` written `0`, which leaves every call for the compiler to fold, and once with a zero no fold reads, which leaves each to run.
fn answers(function: &str, calls: &[&str]) -> [Vec<u8>; 2] {
    let answer = calls
        .iter()
        .enumerate()
        .map(|(place, call)| format!("{} * {call}", 10usize.pow(place as u32)))
        .collect::<Vec<_>>()
        .join(" + ");

    ["0", "Bytes/len(rand/bytes(0)!)"].map(|zero| {
        run(&format!(
            "use /std/{{Bool, Bytes, List, Nat, Option, rand}};\n{function}\nlet z = {zero};\n/std/print(Nat/to_str({answer}))\n"
        ))
    })
}

// A row that writes a binder where other rows name a case is reached under each of those cases, and under every case no row names. Each match answers what its arms state at every input, folded and executed.
#[test]
fn a_binder_beside_a_concrete_pattern_answers_as_its_arms_state() {
    let options = [
        "f(Option/some(1 + z), Option/some(2 + z))",
        "f(Option/some(1 + z), Option/none())",
        "f(Option/none(), Option/some(5 + z))",
        "f(Option/none(), Option/none())",
    ];

    for (function, calls, expected) in [
        (
            "let f(a: Option(Nat), b: Option(Nat)) -> Nat =\n    match (a, b) | (some(x), some(y)) => x + y | (_, none()) => 7 | (none(), some(y)) => y end;",
            &options[..],
            "7573",
        ),
        (
            "let f(a: Option(Nat), flag: Bool) -> Nat =\n    match (a, flag) | (_, true) => 1 | (some(x), false) => x | (none(), false) => 0 end;",
            &[
                "f(Option/some(5 + z), z == 0)",
                "f(Option/none(), z == 0)",
                "f(Option/some(5 + z), z == 1)",
                "f(Option/none(), z == 1)",
            ],
            "511",
        ),
        (
            "let f(p: Bool, q: Bool) -> Nat =\n    match (p, q) | (true, true) => 1 | (_, false) => 2 | (false, true) => 3 end;",
            &[
                "f(z == 0, z == 0)",
                "f(z == 0, z == 1)",
                "f(z == 1, z == 1)",
                "f(z == 1, z == 0)",
            ],
            "3221",
        ),
        (
            "let f(a: Option(Nat), b: Option(Nat)) -> Nat =\n    match (a, b) | (some(x), some(y)) => x + y | _ => 9 end;",
            &options[..],
            "9993",
        ),
        (
            "let f(a: Option(Nat), b: Option(Nat)) -> Nat =\n    match (a, b) | (some(_), _) => 1 | (_, _) => 2 end;",
            &[options[0], options[3]],
            "21",
        ),
        (
            "let f(m: Option(Option(Nat))) -> Nat =\n    match m | some(some(x)) => x | some(_) => 7 | none() => 1 end;",
            &[
                "f(Option/some(Option/some(3 + z)))",
                "f(Option/some(Option/none()))",
                "f(Option/none())",
            ],
            "173",
        ),
        (
            "let f(xs: List(Nat), ys: List(Nat)) -> Nat =\n    match (xs, ys) | ([], _) => 1 | ([_, .._], []) => 2 | ([x, .._], [y, .._]) => x + y end;",
            &["f([], [1 + z])", "f([1 + z], [])", "f([3 + z], [4 + z])"],
            "721",
        ),
        (
            "let f(n: Nat, b: Bool) -> Nat =\n    match (n, b) | (_, true) => 1 | (0, false) => 0 | (k + 1, false) => k end;",
            &[
                "f(z, z == 0)",
                "f(4 + z, z == 0)",
                "f(4 + z, z == 1)",
                "f(z, z == 1)",
            ],
            "311",
        ),
        (
            "let f(m: Option(Nat)) -> Nat =\n    match m | some(5) => 1 | some(n) => n | none() => 0 end;",
            &[
                "f(Option/some(5 + z))",
                "f(Option/some(7 + z))",
                "f(Option/none())",
            ],
            "71",
        ),
        (
            "struct P: Type { a: Option(Nat), b: Nat }\nlet f(p: P) -> Nat =\n    match p | P { a = some(x), b } => x + b | q => q.b end;",
            &[
                "f(P { a = Option/some(1 + z), b = 2 })",
                "f(P { a = Option/none(), b = 5 + z })",
            ],
            "53",
        ),
    ] {
        for answer in answers(function, calls) {
            assert_eq!(answer, expected.as_bytes(), "{function}");
        }
    }
}

// Rows of one constructor meet its payloads each by its own marks: one may write a hidden payload another leaves out.
#[test]
fn rows_of_one_constructor_may_write_different_hidden_payloads() {
    let function = r#"
        induct V: (length: Nat) -> Type
        | nil(): (0)
        | cons(@k: Nat, head: Nat, tail: V(k)): (k + 1)
        end
        let f(n: Nat, v: V(n), w: V(n)) -> Nat =
            match (v, w)
            | (cons(x, _), cons(@j, y, _)) => x + y + j
            | (nil(), _) => 0
            end;
        "#;

    for answer in answers(
        function,
        &["f(1, V/cons(1 + z, V/nil()), V/cons(2 + z, V/nil()))"],
    ) {
        assert_eq!(answer, b"3");
    }
}

// A constructor the scrutinee's indices rule out is no case a binder row is put under: `v: V(n + 1)` is no `nil()`, so the row that writes `_` there is reached under `cons` alone, and the match has no arm for anything else.
#[test]
fn a_binder_row_is_under_no_case_inversion_rules_out() {
    let function = r#"
        induct V: (length: Nat) -> Type
        | nil(): (0)
        | cons(@k: Nat, head: Nat, tail: V(k)): (k + 1)
        end
        let f(n: Nat, v: V(n + 1), b: Bool) -> Nat =
            match (v, b)
            | (cons(x, _), true) => x
            | (_, false) => 9
            end;
        "#;

    for answer in answers(
        function,
        &[
            "f(0, V/cons(4 + z, V/nil()), z == 0)",
            "f(0, V/cons(4 + z, V/nil()), z == 1)",
        ],
    ) {
        assert_eq!(answer, b"94");
    }
}

// A row's body rests on what its own pattern states, wherever the tree puts the row: `(_, true)` is reached under `some` and under `none`, cases it does not state, and the bound `g` asks for is decided by the `true` it does.
#[test]
fn a_row_rests_on_its_own_case_under_cases_it_does_not_state() {
    let function = r#"
        use /std/Bool/{Holds};
        let g(c: Bool, @ok: Holds(c)) -> Nat = 1;
        let f(p: {Option(Nat), Bool}) -> Nat =
            match p
            | (_, true) => g(p.1)
            | (some(x), false) => x
            | (none(), false) => 0
            end;
        "#;

    for answer in answers(
        function,
        &[
            "f((Option/some(5 + z), z == 0))",
            "f((Option/none(), z == 0))",
            "f((Option/some(5 + z), z == 1))",
        ],
    ) {
        assert_eq!(answer, b"511");
    }
}

// A value no arm matches is reported as the pattern that matches it: the cases read on the way to it, and `_` wherever none was.
#[test]
fn a_value_no_arm_matches_is_reported_as_its_pattern() {
    for (signature, arms, value) in [
        (
            "a: Option(Nat), b: Option(Nat)",
            "match (a, b) | (some(x), some(y)) => x + y | (none(), none()) => 0 end",
            "(some(_), none())",
        ),
        (
            "m: Option(Option(Nat))",
            "match m | some(some(x)) => x | none() => 0 end",
            "some(none())",
        ),
        ("b: Bool", "match b | true => 1 end", "false"),
        ("b: List(Nat)", "match b | [] => 0 end", "[_, .._]"),
        ("b: Nat", "match b | p + 1 => p end", "0"),
        ("b: Bytes", "match b | x[] => 0 end", "x[_, .._]"),
        ("b: Nat", "match b | 0 => 1 | 1 => 2 end", "2"),
        (
            "b: Option(Nat)",
            "match b | some(5) => 1 | some(6) => 2 | none() => 0 end",
            "some(0)",
        ),
    ] {
        let source = format!(
            "use /std/{{Bool, Bytes, List, Nat, Option}};\nlet f({signature}) -> Nat =\n    {arms};\n/std/print(\"\")\n"
        );

        let error = error(&source);
        assert!(
            error.contains(&format!("no arm matches `{value}`")),
            "{source:?} reported {error}"
        );
    }
}

// An arm written after one it is a case of is reached by no value, and so is a final `_` after arms that cover every case: each is refused where it is written, with an arm before it that takes its values.
#[test]
fn an_arm_no_value_reaches_is_refused() {
    for (source, arm) in [
        (
            "use /std/{Nat, Option};\n\nlet f(m: Option(Option(Nat))) -> Nat =\n    match m\n    | some(_) => 0\n    | some(some(x)) => x\n    | none() => 1\n    end;\n\n/std/print(Nat/to_str(f(Option/none())))\n",
            "    6 |     | some(some(x)) => x\n      |       ^^^^^^^^^^^^^",
        ),
        (
            "use /std/{Bool, Nat};\n\nlet f(b: Bool) -> Nat =\n    match b\n    | true => 1\n    | false => 0\n    | _ => 2\n    end;\n\n/std/print(Nat/to_str(f(true)))\n",
            "    7 |     | _ => 2\n      |       ^",
        ),
        (
            "use /std/{Nat, Option};\n\nlet f(o: Option(Nat)) -> Nat =\n    match o\n    | x => 0\n    | some(y) => y\n    end;\n\n/std/print(Nat/to_str(f(Option/some(3))))\n",
            "    6 |     | some(y) => y\n      |       ^^^^^^^",
        ),
    ] {
        let error = error(source);
        assert!(
            error.contains("duplicate or overlapping") && error.contains(arm),
            "{source:?} reported {error}"
        );
    }
}

// Two arms that match one value, neither a case of the other, would mean one thing in one order and another in the other: the match is refused with the value they share, which is the arm that parts them.
#[test]
fn two_arms_that_meet_are_refused_with_the_value_they_share() {
    let source = r#"
        use /std/{Bool, Nat};
        let f(p: Bool, q: Bool) -> Nat =
            match (p, q) | (true, _) => 1 | (_, true) => 2 | (false, false) => 3 end;
        /std/print(Nat/to_str(f(true, true)))
        "#;

    let error = error(source);
    assert!(
        error.contains("the arms `(true, _)` and `(_, true)` both match `(true, true)`"),
        "unexpected error: {error}"
    );
}

// A struct pattern names the structure it matches: one written under another head is refused, though both declare the field it reads.
#[test]
fn a_struct_pattern_names_the_structure_of_its_scrutinee() {
    let source = r#"
        use /std/{Nat};
        struct A: Type { x: Nat }
        struct B: Type { x: Nat }
        let f(a: A) -> Nat =
            match a | B { x } => x end;
        /std/print(Nat/to_str(f(A { x = 1 })))
        "#;

    let error = error(source);
    assert!(
        error.contains("a `B` pattern matches a value of that structure"),
        "unexpected error: {error}"
    );
}
