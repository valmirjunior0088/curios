//! Two applications of one definition: what holding their spines equal would solve is taken where the term that awaits them completes.
//!
//! `rest(xs)` against `rest([1, 2])` holds at `xs := [1, 2]` and at every list with that tail: one answer among several, so conversion decides the pair by reduction and leaves it parked where reduction is stuck, and the call takes the spines' answer only once it has read every argument, where nothing else gave the implicit and every such pair gives it the same. Each program that compiles is run, so the kernel has judged what the elaborator admitted.

use crate::tests::{error, run};

/// `body` over the declarations these programs share, its last line the text printed.
fn program(body: &str) -> String {
    format!(
        r#"
        use /std/{{Nat, Bool, Str, List, print}};
        let rest(xs: List(Nat)) -> List(Nat) = match xs | [] => [] | [_, ..t] => t end;
        let head(xs: List(Nat)) -> Nat = match xs | [] => 0 | [h, ..t] => h end;
        induct Tagged: (List(Nat)) -> Type | tag(xs: List(Nat)): (xs) end
        let first: Tagged(rest([1, 2])) = Tagged/tag([2]);
        let second: Tagged(rest([9, 2])) = Tagged/tag([2]);
        let and(s: Str, t: Str) -> Str = Str/concat(s, Str/concat(" ", t));
        {body}
        "#
    )
}

// An argument whose type states the implicit outright gives it, whichever side of it the argument that holds at several lists is declared on: the pair `rest(xs)` against `rest([1, 2])` waits, and is checked at the list the other argument gave.
#[test]
fn an_implicit_an_argument_states_is_not_taken_from_another_arguments_spine() {
    let source = program(
        r#"
        let before(@xs: List(Nat), a: Tagged(rest(xs)), b: Tagged(xs)) -> Nat = head(xs);
        let after(@xs: List(Nat), b: Tagged(xs), a: Tagged(rest(xs))) -> Nat = head(xs);
        print(and(Nat/to_str(before(first, Tagged/tag([9, 2]))), Nat/to_str(after(Tagged/tag([9, 2]), first))))
        "#,
    );

    assert_eq!(run(&source), b"9 9");
}

// Where nothing else gives the implicit, the call takes what the spines give once it has read its arguments: under a definition whose body holds a universe level as under one that holds none, under a definition that unfolds to another's call, where what reduction leaves holds neither call, and under a constructor's index, where the pair to hold stands inside the one parked.
#[test]
fn an_implicit_only_a_spine_gives_is_taken_where_the_call_completes() {
    let source = program(
        r#"
        let Wrap(c: Bool) -> Type = match c | true => List(Nat) | false => Nat end;
        let Wide(c: Bool) -> Type = match c | true => List(Type) | false => Nat end;
        let Listed(c: Bool) -> Type = List(Wrap(c));
        let wrapped(@c: Bool, h: Wrap(c)) -> Bool = c;
        let wide(@c: Bool, h: Wide(c)) -> Bool = c;
        let listed(@c: Bool, h: Listed(c)) -> Bool = c;
        let lone(@xs: List(Nat), a: Tagged(rest(xs))) -> Nat = head(xs);
        let given: Wrap(true) = [1];
        let types: Wide(true) = [Nat];
        let many: Listed(true) = [[1]];
        print(Str/join(" ", [
            Bool/to_str(wrapped(given)),
            Bool/to_str(wide(types)),
            Bool/to_str(listed(many)),
            Nat/to_str(lone(first)),
        ]))
        "#,
    );

    assert_eq!(run(&source), b"true true true 1");
}

// Two arguments whose spines give the implicit one value agree, and the implicit written says which of two is meant where they would not.
#[test]
fn two_spines_that_give_an_implicit_one_value_agree() {
    let source = program(
        r#"
        let pair(@xs: List(Nat), a: Tagged(rest(xs)), b: Tagged(rest(xs))) -> Nat = head(xs);
        print(and(Nat/to_str(pair(first, first)), Nat/to_str(pair(@[9, 2], first, second))))
        "#,
    );

    assert_eq!(run(&source), b"1 9");
}

// Two that give it two values refuse the call, in either order of the arguments, naming the implicit and both: `rest` is equal at the two lists, so taking either would be taking the order the arguments were written in.
#[test]
fn two_spines_that_give_an_implicit_two_values_refuse_the_call() {
    for (arguments, one, another) in [
        ("first, second", "[1, 2]", "[9, 2]"),
        ("second, first", "[9, 2]", "[1, 2]"),
    ] {
        let report = error(&program(&format!(
            r#"
            let pair(@xs: List(Nat), a: Tagged(rest(xs)), b: Tagged(rest(xs))) -> Nat = head(xs);
            print(Nat/to_str(pair({arguments})))
            "#
        )));

        assert!(
            report.contains("two types each give the implicit argument 'xs' of 'pair' a value, and the values differ")
                && report.contains(&format!("one: {one}"))
                && report.contains(&format!("another: {another}")),
            "unexpected report for `pair({arguments})`:\n{report}"
        );
    }
}
