//! Filling a concept's fields explicitly: `use` entries, superclass slots, and literal spreads.

use crate::tests::{error, run};

// An explicit `use <term>` fill in a concept literal overrides table resolution for that field: the flipped equality rides inside the `Ord2` value, while the registered witness is untouched. The superclass field is anonymous, so the override is observed by resolution — with `o` in instance scope, the omitted `Eq2(Nat)` goal projects its superclass (the flipped equality), taking precedence over the global table.
#[test]
fn use_entry_fills_a_concept_field_explicitly() {
    let source = r#"
        use /std/{Nat, Bool, Str, Ordering};
        pub concept Eq2(A : Type) : pub Type {
            eq2(A, A) -> Bool
        }
        pub concept Ord2(A : Type) : pub Type {
            use Eq2(A),
            cmp2(A, A) -> Ordering
        }
        satisfy Eq2(Nat) {
            eq2(a, b) = a == b
        }
        let flipped : Eq2(Nat) = Eq2 { eq2(a, b) = false };
        let o : Ord2(Nat) = Ord2 { use flipped, cmp2(a, b) = Ordering/lt() };
        pub let observe(use Ord2(Nat)) -> Bool = Eq2/eq2(1, 1);
        /std/print(Bool/to_str(observe(use o)))
        "#;

    assert_eq!(run(source), b"false");
}

// A witness body is not a concept literal in this one respect: it never writes a superclass slot. Written, the registered `Ord(Mine)` would carry an `Eql(Mine)` other than the registered one, and `==` on one type would answer `false` through the table and `true` through the witness's own projection — two implicit resolutions disagreeing, which is what global coherence exists to rule out. The refusal names the rule, so a fixture broken some other way cannot pass for it.
#[test]
fn a_witness_never_writes_its_superclass_slot() {
    let source = r#"
        use /std/{print, Str, Bool, Ord, Ordering};
        use /std/ops/{Eql};
        induct Mine: pub Type | a() | b() end
        satisfy Eql(Mine);
        let always: Eql(Mine) = Eql { eql(x, y) = true, neq(x, y) = false };
        satisfy Ord(Mine) {
            use always,
            ord(x, y) = Ordering/eq(),
        }
        print("no")
        "#;

    let message = error(source);
    assert!(
        message.contains("a witness never writes a superclass slot"),
        "got: {message}"
    );
}

// The control, differing in the omitted slot alone: resolution fills it with the registered `Eql(Mine)`, so the table and the superclass projection of a local `Ord(A)` are one witness and answer alike.
#[test]
fn a_superclass_reads_alike_through_the_table_and_through_a_witness() {
    let source = r#"
        use /std/{print, Str, Bool, Ord, Ordering};
        use /std/ops/{Eql};
        induct Mine: pub Type | a() | b() end
        satisfy Eql(Mine);
        satisfy Ord(Mine) {
            ord(x, y) = Ordering/eq(),
        }
        let via_table(x: Mine, y: Mine) -> Bool = x == y;
        let via_super(@A: Type, use Ord(A), x: A, y: A) -> Bool = x == y;
        print(Str/concat(
            Bool/to_str(via_table(Mine/a(), Mine/b())),
            Bool/to_str(via_super(Mine/a(), Mine/b()))))
        "#;

    assert_eq!(run(source), b"falsefalse");
}

// A superclass field is anonymous, so its concept's former field name is not a label: assigning it is a plain unknown-field error, with no special `use`-field diagnostic (`Equal`'s superclass is reached by resolution, never by name).
#[test]
fn labeled_fill_of_a_former_superclass_is_unknown() {
    let source = r#"
        use /std/{Nat, Bool, Str, Ordering};
        pub concept Eq4(A : Type) : pub Type {
            eq4(A, A) -> Bool
        }
        pub concept Ord4(A : Type) : pub Type {
            use Eq4(A),
            cmp4(A, A) -> Ordering
        }
        satisfy Eq4(Nat) {
            eq4(a, b) = a == b
        }
        let bad : Ord4(Nat) = Ord4 { eq4 = Eq4 { eq4(a, b) = a == b } };
        /std/print("no")
        "#;

    let message = error(source);
    assert!(message.contains("'eq4'"), "got: {message}");
    assert!(message.contains("no field"), "got: {message}");
}

// `use` entries are rejected outside concept literals, and one written past the concept's last edge has no slot.
#[test]
fn misplaced_use_entries_are_errors() {
    let non_concept = r#"
        use /std/{Nat, Str};
        pub struct Pair : pub Type { fst : Nat, snd : Nat }
        let p = Pair { use 1, snd = 2 };
        /std/print("no")
        "#;
    assert!(error(non_concept).contains("not a concept"));

    let surplus = r#"
        use /std/{Nat, Bool, Str, Ordering};
        pub concept Eq5(A : Type) : pub Type {
            eq5(A, A) -> Bool
        }
        pub concept Ord5(A : Type) : pub Type {
            use Eq5(A),
            cmp5(A, A) -> Ordering
        }
        satisfy Eq5(Nat) {
            eq5(a, b) = a == b
        }
        let twice : Ord5(Nat) = Ord5 {
            use Eq5 { eq5(a, b) = a == b },
            use Eq5 { eq5(a, b) = a == b },
            cmp5(a, b) = Ordering/lt()
        };
        /std/print("no")
        "#;
    assert!(error(surplus).contains("this `use` member has no slot"));
}

// An omitted superclass field inside a *premised* witness resolves through the local `use` premise (resolution's local step), not the table: the element equality is the premise's, threaded structurally.
#[test]
fn omitted_superclass_resolves_from_a_premise() {
    let source = r#"
        use /std/{Nat, Bool, Str, Ordering, List};
        pub concept Eq6(A : Type) : pub Type {
            eq6(A, A) -> Bool
        }
        pub concept Ord6(A : Type) : pub Type {
            use Eq6(A),
            cmp6(A, A) -> Ordering
        }
        satisfy Eq6(Nat) {
            eq6(a, b) = a == b
        }
        satisfy (@A : Type, use Eq6(A)) => Eq6(List(A)) {
            eq6(a, b) = List/len(a) == List/len(b)
        }
        satisfy (@A : Type, use Ord6(A)) => Ord6(List(A)) {
            cmp6(a, b) = Ordering/lt()
        }
        satisfy Ord6(Nat) {
            cmp6(a, b) = Ordering/lt()
        }
        pub let same(@A : Type, use Ord6(A), x : A, y : A) -> Bool = Eq6/eq6(x, y);
        let l : List(Nat) = [1, 2];
        /std/print(Bool/to_str(same(l, l)))
        "#;

    assert_eq!(run(source), b"true");
}

// A spread in a concept literal *copies* the anonymous superclass field from the base rather than re-resolving it: `o` carries the flipped (always false) equality, and the update must preserve it — table resolution would find the registered `Eq2(Nat)` and answer true.
#[test]
fn concept_literal_spread_copies_superclass() {
    let source = r#"
        use /std/{Nat, Bool, Str, Ordering};
        pub concept Eq2(A : Type) : pub Type {
            eq2(A, A) -> Bool
        }
        pub concept Ord2(A : Type) : pub Type {
            use Eq2(A),
            cmp2(A, A) -> Ordering
        }
        satisfy Eq2(Nat) {
            eq2(a, b) = a == b
        }
        let flipped : Eq2(Nat) = Eq2 { eq2(a, b) = false };
        let o : Ord2(Nat) = Ord2 { use flipped, cmp2(a, b) = Ordering/lt() };
        let o2 : Ord2(Nat) = Ord2 { ..o, cmp2(a, b) = Ordering/gt() };
        pub let observe(use Ord2(Nat)) -> Bool = Eq2/eq2(1, 1);
        /std/print(Bool/to_str(observe(use o2)))
        "#;

    assert_eq!(run(source), b"false");
}

// An explicit `use <term>` entry after the spread still overrides the superclass, while the plain fields copy across.
#[test]
fn concept_literal_spread_use_override() {
    let source = r#"
        use /std/{Nat, Bool, Str, Ordering};
        pub concept Eq2(A : Type) : pub Type {
            eq2(A, A) -> Bool
        }
        pub concept Ord2(A : Type) : pub Type {
            use Eq2(A),
            cmp2(A, A) -> Ordering
        }
        let flipped : Eq2(Nat) = Eq2 { eq2(a, b) = false };
        let straight : Eq2(Nat) = Eq2 { eq2(a, b) = true };
        let o : Ord2(Nat) = Ord2 { use flipped, cmp2(a, b) = Ordering/lt() };
        let o2 : Ord2(Nat) = Ord2 { ..o, use straight };
        pub let observe(use Ord2(Nat)) -> Bool = Eq2/eq2(1, 1);
        /std/print(Bool/to_str(observe(use o2)))
        "#;

    assert_eq!(run(source), b"true");
}

// A `use` entry after a spread is still only legal in concept literals.
#[test]
fn concept_literal_spread_use_on_non_concept_rejected() {
    let source = r#"
        use /std/{Nat};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 1, snd = 2 };
        let bad = Pair { ..p, use 1 };
        /std/print("no")
        "#;

    assert!(error(source).contains("not a concept"));
}

// Concepts whose method types name one another's dictionaries are declared as one group, exactly as structures are: their formers lower to one recursive item. The dictionaries sit in result positions — a dictionary left of an arrow is a negative occurrence, and positivity refuses it as it would for an inductive. `a` is a recursive value: `back` closes over it under a lambda, which is a knot forced by need.
#[test]
fn a_concept_group_may_name_one_anothers_dictionaries() {
    let source = r#"
        use /std/{Nat};
        concept A(T : Type) : pub Type { fa(T) -> B(T) }
        and B(T : Type) : pub Type { fb(T) -> Nat, back(T) -> A(T) }
        let a : A(Nat) = A { fa(x) = B { fb(y) = x + y, back(y) = a } };
        let b : B(Nat) = A/fa(@_, use a, 1);
        let again : B(Nat) = A/fa(@_, use B/back(@_, use b, 0), 10);
        /std/print(Nat/to_str(B/fb(@_, use b, 2) + B/fb(@_, use again, 5)))
        "#;

    assert_eq!(run(source), b"18");
}

// A hidden member takes no position where a value is read. Over a concept value `.0` is the first method and a positional pattern field binds it, in a `let`, an arm and a parameter alike: the edge ahead of it is no part of the count, as it is none of a literal's.
#[test]
fn a_concept_value_is_read_past_its_edges() {
    let source = r#"
        use /std/{Nat, Str, print};
        concept Base(A : Type) : pub Type { base(A) -> Str }
        concept Over(A : Type) : pub Type { use Base(A), over(A) -> Str }
        satisfy Base(Nat) { base(n) = "base" }
        let by_position(dict : Over(Nat), n : Nat) -> Str = dict.0(n);
        let by_pattern(dict : Over(Nat), n : Nat) -> Str =
            let Over { over } = dict;
            over(n);
        let by_match(dict : Over(Nat), n : Nat) -> Str = match dict | Over { over } => over(n) end;
        let by_lambda : (Over(Nat), Nat) -> Str = (Over { over }, n) => over(n);
        let by_label(dict : Over(Nat), n : Nat) -> Str = dict.over(n);
        let dict : Over(Nat) = Over { over(n) = "over" };
        print(Str/concat(
            Str/concat(by_position(dict, 1), by_pattern(dict, 1)),
            Str/concat(by_match(dict, 1), Str/concat(by_lambda(dict, 1), by_label(dict, 1)))))
        "#;

    assert_eq!(run(source), b"overoveroveroverover");
}

// The count a refusal names is the reader's: a concept value of one method has one position, whatever edges stand ahead of it.
#[test]
fn a_position_past_a_concept_values_methods_is_out_of_bounds() {
    let source = r#"
        use /std/{Nat, Str, print};
        concept Base(A : Type) : pub Type { base(A) -> Str }
        concept Over(A : Type) : pub Type { use Base(A), over(A) -> Str }
        satisfy Base(Nat) { base(n) = "base" }
        let second(dict : Over(Nat), n : Nat) -> Str = dict.1(n);
        print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("tuple index 1 out of bounds (arity 1)"),
        "{report}"
    );
}

// A superclass edge is read off its field's elaborated type, so an alias of a concept application is an edge, as it is a premise, and what the alias names is reached through it.
#[test]
fn an_alias_of_a_concept_application_is_a_superclass_edge() {
    let source = r#"
        use /std/{Nat, Str};
        concept Base(A : Type) : pub Type { base(A) -> Str }
        satisfy Base(Nat) { base(n) = "base" }
        let BaseNat : Type = Base(Nat);
        concept Over(A : Type) : pub Type { use BaseNat, over(A) -> Str }
        satisfy Over(Nat) { over(n) = "over" }
        let through(use Over(Nat), n : Nat) -> Str = Base/base(n);
        /std/print(through(1))
        "#;

    assert_eq!(run(source), b"base");
}

// A `use` field whose type reduces to no concept application reaches nothing resolution could answer with, and is refused where the concept is declared, by the type as written.
#[test]
fn a_use_field_that_is_no_concept_application_is_refused() {
    let source = r#"
        use /std/{Nat, Str};
        concept Over(A : Type) : pub Type { use Nat, over(A) -> Str }
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("names 'Nat' as a superclass, but 'Nat' is not a concept application"),
        "got: {report}"
    );
}

// A superclass cycle is a resolution loop, so it is refused whether or not the two concepts are declared together.
#[test]
fn a_superclass_cycle_is_refused_inside_a_group() {
    let source = r#"
        use /std/{Nat};
        concept A(T : Type) : pub Type { use B(T), fa(T) -> Nat }
        and B(T : Type) : pub Type { use A(T), fb(T) -> Nat }
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("superclass"),
        "expected the cycle refused as a superclass cycle:\n{report}"
    );
}
