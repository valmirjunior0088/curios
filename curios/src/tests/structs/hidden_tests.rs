//! A structure's hidden field: declared under `@`, left out where the value is built and where it is read, and a fact wherever the value is in scope.

use crate::tests::{error, run};

// An invariant written once, on the data it constrains. The literal writes the plain field and the bound is discharged as a call's would be; a function over the value has the bound as a fact, with nothing passed or matched to get it.
#[test]
fn a_hidden_field_is_filled_where_the_value_is_built_and_a_fact_where_it_is_read() {
    let source = r#"
        use /std/{Nat, Str, print};
        use /std/Bool/{Holds};
        struct Positive: pub Type { n: Nat, @Holds(0 < n) }
        let halve(n: Nat, @Holds(0 < n)) -> Nat = n / 2;
        let below(p: Positive) -> Nat = halve(p.n);
        print(Nat/to_str(below(Positive { n = 4 })))
        "#;

    assert_eq!(run(source), b"2");
}

// A hidden field that is a value is inferred from the fields that mention it, and may be written: under its label, alone in its run, or its place held.
#[test]
fn a_hidden_field_is_inferred_or_written_under_its_mark() {
    let source = r#"
        use /std/{Nat, Str, print};
        induct Vec(T: Type): (n: Nat) -> pub Type
        | nil(): (0)
        | cons(@n: Nat, head: T, tail: Vec(T)(n)): (n + 1)
        end
        struct Sized: pub Type { @len: Nat, items: Vec(Nat)(len) }
        let two: Vec(Nat)(2) = Vec/cons(7, Vec/cons(8, Vec/nil()));
        let a: Sized = Sized { items = two };
        let b: Sized = Sized { @len = 2, items = two };
        let c: Sized = Sized { @2, two };
        let d: Sized = Sized { @_, items = two };
        print(Nat/to_str(a.len + b.len + c.len + d.len))
        "#;

    assert_eq!(run(source), b"8");
}

// A hidden field takes no position where the value is read: `.0` and a pattern's positional field are the first plain field, and the hidden one is read by its label.
#[test]
fn a_hidden_field_takes_no_position_where_the_value_is_read() {
    let source = r#"
        use /std/{Nat, Str, print};
        induct Vec(T: Type): (n: Nat) -> pub Type
        | nil(): (0)
        | cons(@n: Nat, head: T, tail: Vec(T)(n)): (n + 1)
        end
        struct Sized: pub Type { @len: Nat, items: Vec(Nat)(len) }
        let count(@n: Nat, v: Vec(Nat)(n)) -> Nat = n;
        let read(s: Sized) -> Nat =
            let Sized { items } = s;
            let Sized { items = again, len = n } = s;
            count(items) + count(s.0) + count(again) + n;
        print(Nat/to_str(read(Sized { items = Vec/cons(7, Vec/cons(8, Vec/nil())) })))
        "#;

    assert_eq!(run(source), b"8");
}

// A bound nothing fills is refused where the value is built, as a field of its structure and by the proposition it states.
#[test]
fn a_hidden_field_nothing_fills_is_refused_at_the_literal() {
    let source = r#"
        use /std/{Nat, Str, print};
        use /std/Bool/{Holds};
        struct Positive: pub Type { n: Nat, @Holds(0 < n) }
        print(Nat/to_str(Positive { n = 0 }.n))
        "#;

    let report = error(source);
    assert!(
        report.contains("a hidden field of") && report.contains("was not filled"),
        "{report}"
    );
    assert!(
        report.contains("nothing discharged Holds(0 < 0)"),
        "{report}"
    );
    assert!(report.contains("{ @... }"), "{report}");
}

// A plain entry never fills a hidden field, a hidden entry is written ahead of the plain field it precedes, and its name is held to the field it fills.
#[test]
fn a_hidden_fields_entry_is_held_to_its_mark_its_run_and_its_name() {
    let prelude = r#"
        use /std/{Nat, Str, print};
        induct Vec(T: Type): (n: Nat) -> pub Type
        | nil(): (0)
        | cons(@n: Nat, head: T, tail: Vec(T)(n)): (n + 1)
        end
        struct Sized: pub Type { @len: Nat, items: Vec(Nat)(len) }
        let two: Vec(Nat)(2) = Vec/cons(7, Vec/cons(8, Vec/nil()));
        "#;
    let refused = |literal: &str| error(&format!("{prelude}\nprint(Nat/to_str({literal}.len))"));

    let plain = refused("Sized { 2, two }");
    assert!(
        plain.contains("has 1 field(s) but the literal supplies 2"),
        "{plain}"
    );

    let late = refused("Sized { two, @2 }");
    assert!(late.contains("this `@` member has no slot"), "{late}");

    let misnamed = refused("Sized { @size = 2, items = two }");
    assert!(
        misnamed.contains("has no field '@size' at that position (fields in order: @len, items)"),
        "{misnamed}"
    );
}

// A spread copies the plain fields and never the hidden one, which is stated of the fields the new value has: left out it is discharged anew, at the override or at the base's own field, and the author who means the base's proof writes it.
#[test]
fn a_spread_fills_a_hidden_field_anew() {
    let source = r#"
        use /std/{Nat, Str, print};
        use /std/Bool/{Holds};
        struct Positive: pub Type { n: Nat, @ok: Holds(0 < n) }
        let p: Positive = Positive { n = 4 };
        let q: Positive = Positive { ..p, n = 9 };
        let keep(p: Positive) -> Positive = Positive { ..p };
        let same(p: Positive) -> Positive = Positive { ..p, @ok = p.ok };
        print(Nat/to_str(q.n + keep(p).n + same(p).n))
        "#;

    assert_eq!(run(source), b"17");
}

// A spread whose override breaks the bound is refused as the literal written out would be: the base's proof is of the base's field, and is never carried over.
#[test]
fn a_spread_does_not_carry_a_bound_its_override_breaks() {
    let source = r#"
        use /std/{Nat, Str, print};
        use /std/Bool/{Holds};
        struct Positive: pub Type { n: Nat, @ok: Holds(0 < n) }
        let p: Positive = Positive { n = 4 };
        print(Nat/to_str(Positive { ..p, n = 0 }.n))
        "#;

    let report = error(source);
    assert!(
        report.contains("hidden field 'ok' of") && report.contains("was not filled"),
        "{report}"
    );
}

// A hidden field takes no part in a derived witness: `Spell` writes the literal its author writes, and the text read back infers the field again.
#[test]
fn a_derived_spelling_leaves_a_hidden_field_out() {
    let source = r#"
        use /std/{Nat, Str, Spell, print};
        use /std/Bool/{Holds};
        struct Positive: pub Type { n: Nat, @ok: Holds(0 < n) }
        satisfy Spell(Positive);
        print(Spell/spell(Positive { n = 4 }))
        "#;

    assert_eq!(run(source), b"Positive { n = 4 }");
}
