//! Filling a concept's fields explicitly: `use` entries, superclass slots, and literal spreads.

use crate::tests::{error, run};

// An explicit `use <term>` fill in a concept literal overrides table resolution for that field: the flipped equality rides inside the `Ord2` value, while the registered witness is untouched. The superclass field is anonymous, so the override is observed by resolution — with `o` in instance scope, the omitted `Eq2(Nat)` goal projects its superclass (the flipped equality), taking precedence over the global table.
#[test]
fn use_entry_fills_a_concept_field_explicitly() {
    let source = r#"
        use /std/{Nat, Bool, Handle, Str, Ordering};
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

// A witness body is a concept literal, so `use <term>` fills its superclass field there too.
#[test]
fn use_entry_fills_a_witness_superclass() {
    let source = r#"
        use /std/{Nat, Bool, Handle, Str, Ordering};
        pub concept Eq3(A : Type) : pub Type {
            eq3(A, A) -> Bool
        }
        pub concept Ord3(A : Type) : pub Type {
            use Eq3(A),
            cmp3(A, A) -> Ordering
        }
        satisfy Ord3(Nat) {
            use Eq3 { eq3(a, b) = a == b },
            cmp3(a, b) = Ordering/lt()
        }
        pub let same(@A : Type, use Ord3(A), x : A, y : A) -> Bool = Eq3/eq3(x, y);
        /std/print(Bool/to_str(same(2, 2)))
        "#;

    assert_eq!(run(source), b"true");
}

// A superclass field is anonymous, so its concept's former field name is not a label: assigning it is a plain unknown-field error, with no special `use`-field diagnostic (`Equal`'s superclass is reached by resolution, never by name).
#[test]
fn labeled_fill_of_a_former_superclass_is_unknown() {
    let source = r#"
        use /std/{Nat, Bool, Handle, Str, Ordering};
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

// `use` entries are rejected outside concept literals, and surplus entries are rejected against the concept's `use`-field count.
#[test]
fn misplaced_use_entries_are_errors() {
    let non_concept = r#"
        use /std/{Nat, Handle, Str};
        pub struct Pair : pub Type { fst : Nat, snd : Nat }
        let p = Pair { use 1, snd = 2 };
        /std/print("no")
        "#;
    assert!(error(non_concept).contains("not a concept"));

    let surplus = r#"
        use /std/{Nat, Bool, Handle, Str, Ordering};
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
        satisfy Ord5(Nat) {
            use Eq5 { eq5(a, b) = a == b },
            use Eq5 { eq5(a, b) = a == b },
            cmp5(a, b) = Ordering/lt()
        }
        /std/print("no")
        "#;
    assert!(error(surplus).contains("'use' entr"));
}

// An omitted superclass field inside a *premised* witness resolves through the local `use` premise (resolution's local step), not the table: the element equality is the premise's, threaded structurally.
#[test]
fn omitted_superclass_resolves_from_a_premise() {
    let source = r#"
        use /std/{Nat, Bool, Handle, Str, Ordering, List};
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
        use /std/{Nat, Bool, Handle, Str, Ordering};
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
        use /std/{Nat, Bool, Handle, Str, Ordering};
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
        use /std/{Nat, Handle};
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
        use /std/{Nat, Handle};
        concept A(T : Type) : pub Type { fa(T) -> B(T) }
        and B(T : Type) : pub Type { fb(T) -> Nat, back(T) -> A(T) }
        let a : A(Nat) = A { fa(x) = B { fb(y) = x + y, back(y) = a } };
        let b : B(Nat) = A/fa(use a, 1);
        let again : B(Nat) = A/fa(use B/back(use b, 0), 10);
        /std/print(Nat/to_str(B/fb(use b, 2) + B/fb(use again, 5)))
        "#;

    assert_eq!(run(source), b"18");
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
