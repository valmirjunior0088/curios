//! Derived `Ord`: constructors ranked then payloads, fieldwise, a proof taking no part, and the equality its concept requires.

use crate::tests::{error, run};

// --- The `Ord` derivation: constructors in declaration order, then payloads lexicographically through each one's own witness. ---

#[test]
fn derived_order_ranks_constructors_then_payloads() {
    let source = r#"
        use /std/{Nat, Str, Ord, Show, print};
        use /std/ops/{Eql};
        induct Tree: pub Type | leaf(Nat) | node(Tree, Tree) end
        satisfy Eql(Tree) { .. }
        satisfy Ord(Tree) { .. }
        let show(t: Tree, u: Tree) -> Str = Str/concat(Show/show(Ord/ord(t, u)), " ");
        let entry() -> /std/Io({}) =
            let _ = print(show(Tree/leaf(1), Tree/node(Tree/leaf(1), Tree/leaf(1))))!;
            let _ = print(show(Tree/node(Tree/leaf(1), Tree/leaf(1)), Tree/leaf(9)))!;
            let _ = print(show(Tree/leaf(1), Tree/leaf(2)))!;
            let _ = print(show(Tree/leaf(2), Tree/leaf(1)))!;
            let _ = print(show(Tree/leaf(2), Tree/leaf(2)))!;
            let _ = print(show(Tree/node(Tree/leaf(1), Tree/leaf(2)), Tree/node(Tree/leaf(1), Tree/leaf(3))))!;
            print(show(Tree/node(Tree/leaf(1), Tree/leaf(9)), Tree/node(Tree/leaf(2), Tree/leaf(0))));
        entry()
        "#;

    // A constructor outranks its declaration predecessors whatever the payloads say — `leaf(9)` is below `node(leaf(1), leaf(1))` — and within one constructor the first payload that differs decides.
    assert_eq!(run(source), b"lt gt lt gt eq lt lt ");
}

#[test]
fn a_struct_and_a_parameterized_family_order_fieldwise() {
    let source = r#"
        use /std/{Nat, Str, Ord, Show, print};
        use /std/ops/{Eql};
        struct Point: pub Type { x: Nat, y: Nat }
        induct Box(A: Type): pub Type | boxed(A) end
        satisfy Eql(Point) { .. }
        satisfy Ord(Point) { .. }
        satisfy (@A: Type, use Eql(A)) => Eql(Box(A)) { .. }
        satisfy (@A: Type, use Eql(A), use Ord(A)) => Ord(Box(A)) { .. }
        let show(@A: Type, use Ord(A), a: A, b: A) -> Str = Str/concat(Show/show(Ord/ord(a, b)), " ");
        let entry() -> /std/Io({}) =
            let _ = print(show(Point { x = 1, y = 2 }, Point { x = 1, y = 3 }))!;
            let _ = print(show(Point { x = 2, y = 0 }, Point { x = 1, y = 9 }))!;
            let _ = print(show(Point { x = 1, y = 2 }, Point { x = 1, y = 2 }))!;
            print(show(Box/boxed(Point { x = 1, y = 1 }), Box/boxed(Point { x = 1, y = 2 })));
        entry()
        "#;

    assert_eq!(run(source), b"lt gt eq lt ");
}

#[test]
fn proofs_and_implicit_payloads_take_no_part_in_ordering() {
    // A proof contributes no piece, which under a first-difference-wins fold is exactly what answering `eq` would be; an implicit index is fixed by the payloads it indexes and is never named.
    let source = r#"
        use /std/{Nat, Str, Eq, Ord, Show, print};
        use /std/ops/{Eql};
        induct Certified: pub Type | cert(n: Nat, proof: Eq()(n, n)) end
        satisfy Eql(Certified) { .. }
        satisfy Ord(Certified) { .. }
        let show(a: Certified, b: Certified) -> Str = Str/concat(Show/show(Ord/ord(a, b)), " ");
        let entry() -> /std/Io({}) =
            let _ = print(show(Certified/cert(1, Eq/refl()), Certified/cert(2, Eq/refl())))!;
            print(show(Certified/cert(2, Eq/refl()), Certified/cert(2, Eq/refl())));
        entry()
        "#;

    assert_eq!(run(source), b"lt eq ");
}

#[test]
fn an_order_derivation_needs_the_equality_its_concept_requires() {
    // `Ord`'s superclass slot is left unfilled by the derived record, so resolution asks for it exactly as it would for a written literal that omitted it — and reports at the declaration when it is missing.
    let source = r#"
        use /std/{Nat, Ord, print};
        induct Colour: pub Type | red() | green(Nat) end
        satisfy Ord(Colour) { .. }
        print("")
        "#;

    let report = error(source);
    assert!(
        report.contains(
            "no witness of /std/ops/Eql(Colour) found\n  needed by 'Ord' for its 'Eql' superclass"
        ),
        "{report}"
    );
    assert!(report.contains("satisfy Ord(Colour) { .. }"), "{report}");
}

#[test]
fn the_standard_library_derives_order_for_ordering() {
    // `/std/Ordering` carries the one derived `Ord` witness in the prelude, which is what exercises the derivation's scheduler edges: they matter only within a unit, and a program compiled against `/std` is a unit of its own.
    let source = r#"
        use /std/{Str, Ordering, Ord, Show, print};
        let show(a: Ordering, b: Ordering) -> Str = Str/concat(Show/show(Ord/ord(a, b)), " ");
        let entry() -> /std/Io({}) =
            let _ = print(show(Ordering/lt(), Ordering/eq()))!;
            let _ = print(show(Ordering/gt(), Ordering/eq()))!;
            print(show(Ordering/eq(), Ordering/eq()));
        entry()
        "#;

    assert_eq!(run(source), b"lt gt eq ");
}
