//! A numeral whose type its term has yet to learn waits for it, whichever of the term's parts gives it.
//!
//! A numeral is a choice among the carriers it realizes at, and its default is taken only where nothing its term reads can still say otherwise: a later argument, an element, an arm or the expectation pins it as an earlier one does, and where none does, the numerals that wait on one type take one default together. Each program here is written in both orders and run, so the kernel has judged what the elaborator admitted.

use crate::tests::{error, run};

/// `body` over the declarations these programs share, its last line the text printed.
fn program(body: &str) -> String {
    format!(
        r#"
        use /std/{{Nat, Int, Byte, Bool, Flt, Char, Option, List, Eq, Str, Show, print}};
        let id(@A: Type, a: A) -> A = a;
        let first(@A: Type, x: A, pin: A) -> A = x;
        let second(@A: Type, pin: A, x: A) -> A = x;
        let pick(@A: Type, x: Option(A), pin: Option(A)) -> Option(A) = x;
        let shown(o: Option(Byte)) -> Str = match o | some(v) => Show/show(v) | none() => "none" end;
        struct Pair(T: Type): pub Type {{ a: T, z: T }}
        let b: Byte = 7;
        let i: Int = -2;
        let c: Bool = true;
        let ob: Option(Byte) = Option/some(b);
        let xs: List(Int) = [i];
        let and(s: Str, t: Str) -> Str = Str/concat(s, Str/concat(" ", t));
        {body}
        "#
    )
}

#[test]
fn a_numeral_takes_the_type_a_later_argument_gives() {
    let source = program(
        r#"
        let byte = and(Show/show(first(5, b)), Show/show(second(b, 5)));
        let bool = and(Bool/to_str(first(1, c)), Bool/to_str(second(c, 1)));
        let flt = and(Flt/to_str(first(1, 2.5)), Flt/to_str(second(2.5, 1)));
        let nested = and(Show/show(first(id(5), b)), Show/show(second(b, id(5))));
        print(and(and(byte, bool), and(flt, nested)))
        "#,
    );

    assert_eq!(run(&source), b"5 5 true true +1 +1 5 5");
}

// The call's own expectation is one more part: it reaches a numeral through the implicit the turnaround solves.
#[test]
fn a_numeral_takes_the_type_its_calls_expectation_gives() {
    let source = program(
        r#"
        let wrapped: Option(Int) = Option/some(5);
        let through: Byte = id(5);
        let twice: Option(Int) = id(Option/some(5));
        let int(o: Option(Int)) -> Str = match o | some(v) => Int/to_str(v) | none() => "none" end;
        print(and(int(wrapped), and(Show/show(through), int(twice))))
        "#,
    );

    assert_eq!(run(&source), b"+5 5 +5");
}

// The numerals that wait on one type take a default each of them realizes at: a written sign rules out `Nat` for all of them, and a character is its code point beside a number.
#[test]
fn numerals_that_wait_on_one_type_take_one_default() {
    let source = program(
        r#"
        let signed = and(Int/to_str(first(5, -3)), Int/to_str(second(-3, 5)));
        let character = and(Nat/to_str(first('a', 5)), Nat/to_str(second(5, 'a')));
        print(and(signed, character))
        "#,
    );

    assert_eq!(run(&source), b"+5 +5 97 97");
}

#[test]
fn a_numeral_in_a_literal_takes_the_type_a_sibling_gives() {
    let source = program(
        r#"
        let head(ys: List(Byte)) -> Str = match ys | [] => "none" | [h, ..t] => Show/show(h) end;
        let numeral_first = [5, b];
        let numeral_last = [b, 5];
        let list = and(head(numeral_first), head(numeral_last));
        let leading = Pair { a = 5, z = b };
        let trailing = Pair { a = b, z = 5 };
        let structure = and(Show/show(leading.a), Show/show(trailing.z));
        let payload = and(shown(pick(Option/some(5), ob)), shown(pick(ob, Option/some(5))));
        let Left = Eq()(5, b);
        let Right = Eq()(b, 5);
        print(and(list, and(structure, payload)))
        "#,
    );

    assert_eq!(run(&source), b"5 7 5 5 5 7");
}

// A match reads its arms in its eliminator's order, a `Bool` match its `false` arm first and a list match its empty arm first, so the fallback a program writes last is the arm read first.
#[test]
fn a_numeral_in_an_arm_takes_the_type_another_arm_gives() {
    let source = program(
        r#"
        let typed_first = match c | true => i | false => 5 end;
        let typed_last = match c | true => 5 | false => i end;
        let chosen = choose | c => i | _ => 5 end;
        let folded = match xs | [] => 0 | [h, ..t] => h end;
        print(and(and(Int/to_str(typed_first), Int/to_str(typed_last)), and(Int/to_str(chosen), Int/to_str(folded))))
        "#,
    );

    assert_eq!(run(&source), b"-2 +5 -2 -2");
}

// The numeral's type is an associated type read through a witness the call has not resolved where the numeral is read: it waits for the argument that gives the key.
#[test]
fn a_numeral_waits_for_the_witness_its_type_is_read_through() {
    let source = program(
        r#"
        pub concept Sized(A: Type): pub Type { Carrier: Type, zero: Carrier }
        satisfy Sized(Bool) { Carrier = Int, zero = 0 }
        let before(@A: Type, use Sized(A), x: Sized/Carrier(@A), pin: A) -> Sized/Carrier(@A) = x;
        let after(@A: Type, use Sized(A), pin: A, x: Sized/Carrier(@A)) -> Sized/Carrier(@A) = x;
        print(and(Int/to_str(before(5, true)), Int/to_str(after(true, 5))))
        "#,
    );

    assert_eq!(run(&source), b"+5 +5");
}

#[test]
fn a_literal_operand_takes_the_other_operands_type_on_either_side() {
    let source = program(
        r#"
        let compared = and(Bool/to_str(7 == b), Bool/to_str(b == 7));
        let summed = and(Flt/to_str(1 + 2.5), Flt/to_str(2.5 + 1));
        let defaulted = and(Int/to_str(5 + -3), Int/to_str(-3 + 5));
        print(and(compared, and(summed, defaulted)))
        "#,
    );

    assert_eq!(run(&source), b"true true +3.5 +3.5 +2 +2");
}

// An operator's bound is a proposition by its concept's word, whichever witness the literals come to choose: it waits with them, and is decided once they take their type.
#[test]
fn an_operator_between_literals_decides_its_bound_once_they_take_their_type() {
    let source = program(
        r#"
        let quotient = 20 / 5;
        let remainder = 20 % 6;
        let signed = -20 / +5;
        let pinned = and(Int/to_str(first(20 / 5, i)), Int/to_str(second(i, 20 / 5)));
        let stated: Eq()(4, 20 / 5) = Eq/refl();
        print(and(and(Nat/to_str(quotient), Nat/to_str(remainder)), and(Int/to_str(signed), pinned)))
        "#,
    );

    assert_eq!(run(&source), b"4 2 -4 +4 +4");
}

// A numeral under two calls waits on the inner call's implicit, which the middle call's holds and the outer call's holds in turn: the outer call is the term that awaits it, however deep the numeral stands.
#[test]
fn a_numeral_under_nested_calls_waits_for_the_outermost() {
    let source = program(
        r#"
        let near = and(Show/show(first(id(id(5)), b)), Show/show(second(b, id(id(5)))));
        let far = and(Show/show(first(id(id(id(5))), b)), Show/show(second(b, id(id(id(5))))));
        print(and(near, far))
        "#,
    );

    assert_eq!(run(&source), b"5 5 5 5");
}

// Inference is local to a statement: once a `let` has no annotation and its value no expectation, nothing more is coming, and the line after reads a type.
#[test]
fn a_numeral_nothing_pins_is_its_default_where_the_next_line_reads_it() {
    let report = error(&program(
        r#"
        let n = 5;
        let m: Int = n;
        print("unreachable")
        "#,
    ));

    assert!(
        report.contains("inferred: Nat") && report.contains("expected: Int"),
        "unexpected report:\n{report}"
    );
}
