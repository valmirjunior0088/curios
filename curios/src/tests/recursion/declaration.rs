//! How a recursion is declared: a definition that names itself, at the top level and locally, a group declared with `and`, and two spellings of one recursion read as one function.

use crate::test_support::*;

#[test]
fn a_declared_group_of_definitions_is_mutually_recursive() {
    let source = r#"
        use /std/{Nat};
        let f(n : Nat) -> Nat => match n | 0 => 0 | p + 1 => g(p) end
        and g(n : Nat) -> Nat => match n | 0 => 1 | p + 1 => f(p) end;
        /std/print(Nat/to_str(f(3)))
        "#;

    assert_eq!(run(source), b"1");
}

#[test]
fn a_definition_that_names_itself_recurses() {
    let source = r#"
        use /std/{Nat};
        let count(n : Nat) -> Nat => match n | 0 => 0 | p + 1 => count(p) + 1 end;
        /std/print(Nat/to_str(count(3)))
        "#;

    assert_eq!(run(source), b"3");
}

#[test]
fn a_local_binding_that_names_itself_recurses() {
    let source = r#"
        use /std/{Nat};
        let twice(n : Nat) -> Nat =>
            let go(k : Nat) -> Nat => match k | 0 => 0 | p + 1 => go(p) + 2 end;
            go(n);
        /std/print(Nat/to_str(twice(3)))
        "#;

    assert_eq!(run(source), b"6");
}

#[test]
fn a_local_group_is_mutually_recursive() {
    let source = r#"
        use /std/{Nat};
        let parity(n : Nat) -> Nat =>
            let even(k : Nat) -> Nat => match k | 0 => 1 | p + 1 => odd(p) end
            and odd(k : Nat) -> Nat => match k | 0 => 0 | p + 1 => even(p) end;
            even(n);
        /std/print(Nat/to_str(parity(4)))
        "#;

    assert_eq!(run(source), b"1");
}

// A self-reference under a lambda is a knot forced by need, which the erased program ties through a cell: legal, and the shape a lazy structure takes.
#[test]
fn a_value_that_names_itself_under_a_lambda_is_admitted() {
    let source = r#"
        use /std/{Nat};
        struct Stream : pub Type { head: Nat, tail: () -> Stream }
        let ones : Stream = Stream { head = 1, tail = () => ones };
        /std/print(Nat/to_str(ones.tail().head + ones.head))
        "#;

    assert_eq!(run(source), b"2");
}

// Two definitions that write one recursion two ways are one function to both checkers, called and bare: their groups are compared as the binders they are, and neither is unfolded against the other.
#[test]
fn two_spellings_of_one_recursion_are_one_function() {
    let source = r#"
        use /std/{Nat, Eq};
        let f(n : Nat) -> Nat => match n | 0 => 0 | k + 1 => f(k) + 2 end;
        let h(n : Nat) -> Nat => match n | 0 => 0 | k + 1 => 2 + h(k) end;
        let called(n : Nat) -> Eq()(f(n), h(n)) => Eq/refl();
        let bare : Eq()(f, h) = Eq/refl();
        /std/print(Nat/to_str(f(3)))
        "#;

    assert_eq!(run(source), b"6");
}
