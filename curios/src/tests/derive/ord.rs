//! Derived `Ord`: constructors ranked then payloads, fieldwise, a proof taking no part, the equality its concept requires, and the order's laws.

use {crate::test_support::*, curios_common::fill};

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

// The comparison operators dispatch through the `lt` and `le` every `Ord` states, so a type that derives its order compares with them, as `/std`'s derived `Month` and `Ordering` and its written tuples and unit do: a derived `lt` is strict, and `>` and `>=` read `lt` and `le` over the operands swapped. A `>` read unswapped breaks the bounds `/std` states over `>` and `>=` before any test runs, so the swap is held here by the fixture alone.
#[test]
fn every_order_answers_the_comparison_operators() {
    let source = r#"
        use /std/{Nat, Bool, Str, Ord, Ordering, print};
        use /std/time/{Month};
        use /std/ops/{Eql};
        induct Tree: pub Type | leaf(Nat) | node(Tree, Tree) end
        satisfy Eql(Tree) { .. }
        satisfy Ord(Tree) { .. }
        let show(b: Bool) -> Str = Str/concat(Bool/to_str(b), " ");
        let pair: {Nat, Nat} = (1, 2);
        let entry() -> /std/Io({}) =
            let _ = print(show(Tree/leaf(2) < Tree/leaf(2)))!;
            let _ = print(show(Month/march() <= Month/february()))!;
            let _ = print(show(Ordering/gt() > Ordering/eq()))!;
            let _ = print(show(pair >= (1, 3)))!;
            print(show(() <= ()));
        entry()
        "#;

    assert_eq!(run(source), b"false false true false true ");
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
        satisfy (@A: Type, use Ord(A)) => Ord(Box(A)) { .. }
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

// A family nested through a witness over `use Ord(A)` alone derives its order: `Box`'s witness builds its superclass from the `Eql` inside its premise and publishes how it applies that premise, so `Forest`'s, comparing its children through `Box`'s, descends.
#[test]
fn a_family_nested_through_a_witness_over_its_order_alone_derives_its_order() {
    let source = r#"
        use /std/{Str, Ord, Show, print};
        use /std/ops/{Eql};
        induct Box(A: Type): pub Type | boxed(A) end
        induct Forest: pub Type | none() | some(Box(Forest)) end
        satisfy (@A: Type, use Eql(A)) => Eql(Box(A)) { .. }
        satisfy (@A: Type, use Ord(A)) => Ord(Box(A)) { .. }
        satisfy Eql(Forest) { .. }
        satisfy Ord(Forest) { .. }
        let show(a: Forest, b: Forest) -> Str = Str/concat(Show/show(Ord/ord(a, b)), " ");
        let entry() -> /std/Io({}) =
            let _ = print(show(Forest/none(), Forest/some(Box/boxed(Forest/none()))))!;
            print(show(Forest/some(Box/boxed(Forest/none())), Forest/some(Box/boxed(Forest/none()))));
        entry()
        "#;

    assert_eq!(run(source), b"lt eq ");
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
    // `Ord`'s superclass slot is left unfilled by the derived record, so resolution asks for it exactly as it would for a written literal that omitted it — and reports at the declaration when it is missing, the superclass spelled as its reader reaches it: `Eql` is in scope here by its path alone.
    let source = r#"
        use /std/{Nat, Ord, print};
        induct Colour: pub Type | red() | green(Nat) end
        satisfy Ord(Colour) { .. }
        print("")
        "#;

    let report = error(source);
    assert!(
        report.contains(
            "wanted: `/std/ops/Eql(Colour)`\n│  needed by `Ord` for its `/std/ops/Eql` superclass"
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

// --- The laws: that `ord` flips with its operands, carries across a middle value and answers `eq` only at one value, and that `lt` and `le` are what it answers, proved where a witness is declared. ---

// The derivation writes the five proofs, which a program reads through the concept as it reads the methods: over a recursive family, a family nested through `List`'s witness and through a pair's inside it, a parameterized family under a premise, a constructor whose payload is a proof, a family of constructors that hold nothing, a struct with a hidden proof field, and a struct of several fields.
#[test]
fn a_derived_witness_proves_the_orders_laws() {
    let source = r#"
        use /std/{Nat, Bool, Str, List, Eq, Ord, print};
        use /std/Ord/{Ordering};
        use /std/Bool/{True, Holds};
        use /std/ops/{Eql};

        induct Tree: Type | leaf(Nat) | node(Tree, Tree) end
        induct Doc: Type | num(Nat) | arr(List(Doc)) | obj(List({Str, Doc})) end
        induct Rose(A: Type): Type | rose(A, List(Rose(A))) end
        induct Certified: Type | cert(n: Nat, proof: Eq()(n, n)) end
        induct Day: Type | mon() | tue() | wed() | thu() end
        struct Small: Type { n: Nat, @below: Holds(n < 10) }
        struct Span: Type { from: Nat, to: Nat, tag: Str }

        satisfy Eql(Tree) { .. }
        satisfy Eql(Doc) { .. }
        satisfy (@A: Type, use Eql(A)) => Eql(Rose(A)) { .. }
        satisfy Eql(Certified) { .. }
        satisfy Eql(Day) { .. }
        satisfy Eql(Small) { .. }
        satisfy Eql(Span) { .. }

        satisfy Ord(Tree) { .. }
        satisfy Ord(Doc) { .. }
        satisfy (@A: Type, use Ord(A)) => Ord(Rose(A)) { .. }
        satisfy Ord(Certified) { .. }
        satisfy Ord(Day) { .. }
        satisfy Ord(Small) { .. }
        satisfy Ord(Span) { .. }

        let flips(@A: Type, use Ord(A), a: A, b: A) -> Eq()(Ord/ord(b, a), Ordering/flip(Ord/ord(a, b))) =
            Ord/flips(a, b);
        let carries(
            @A: Type,
            use Ord(A),
            a: A,
            b: A,
            c: A,
            p: Holds(Ordering/is_le(Ord/ord(a, b))),
            q: Holds(Ordering/is_le(Ord/ord(b, c))),
        ) -> Holds(Ordering/is_le(Ord/ord(a, c))) =
            Ord/trans(a, b, c, p, q);
        let tied(@A: Type, use Ord(A), a: A, b: A, h: Eq()(Ord/ord(a, b), Ordering/eq())) -> Eq()(a, b) =
            Ord/eq(a, b, h);
        let below(@A: Type, use Ord(A), a: A, b: A) -> Eq()(a < b, Ordering/is_lt(Ord/ord(a, b))) =
            Ord/is_lt(a, b);
        let at_most(@A: Type, use Ord(A), a: A, b: A) -> Eq()(a <= b, Ordering/is_le(Ord/ord(a, b))) =
            Ord/is_le(a, b);

        let _trees(a: Tree, b: Tree) -> Eq()(Ord/ord(b, a), Ordering/flip(Ord/ord(a, b))) = flips(a, b);
        let _docs(a: Doc, b: Doc) -> Eq()(Ord/ord(b, a), Ordering/flip(Ord/ord(a, b))) = flips(a, b);
        let _roses(a: Rose(Doc), b: Rose(Doc)) -> Eq()(Ord/ord(b, a), Ordering/flip(Ord/ord(a, b))) = flips(a, b);
        let _days(a: Day, b: Day) -> Eq()(Ord/ord(b, a), Ordering/flip(Ord/ord(a, b))) = flips(a, b);
        let _spans(a: Span, b: Span) -> Eq()(Ord/ord(b, a), Ordering/flip(Ord/ord(a, b))) = flips(a, b);

        let _across_docs(
            a: Doc,
            b: Doc,
            c: Doc,
            p: Holds(Ordering/is_le(Ord/ord(a, b))),
            q: Holds(Ordering/is_le(Ord/ord(b, c))),
        ) -> Holds(Ordering/is_le(Ord/ord(a, c))) =
            carries(a, b, c, p, q);
        let _across_days(
            a: Day,
            b: Day,
            c: Day,
            p: Holds(Ordering/is_le(Ord/ord(a, b))),
            q: Holds(Ordering/is_le(Ord/ord(b, c))),
        ) -> Holds(Ordering/is_le(Ord/ord(a, c))) =
            carries(a, b, c, p, q);
        let _across_small(
            a: Small,
            b: Small,
            c: Small,
            p: Holds(Ordering/is_le(Ord/ord(a, b))),
            q: Holds(Ordering/is_le(Ord/ord(b, c))),
        ) -> Holds(Ordering/is_le(Ord/ord(a, c))) =
            carries(a, b, c, p, q);

        let _one_tree(a: Tree, b: Tree, h: Eq()(Ord/ord(a, b), Ordering/eq())) -> Eq()(a, b) = tied(a, b, h);
        let _one_rose(a: Rose(Doc), b: Rose(Doc), h: Eq()(Ord/ord(a, b), Ordering/eq())) -> Eq()(a, b) =
            tied(a, b, h);
        let _one_certified(a: Certified, b: Certified, h: Eq()(Ord/ord(a, b), Ordering/eq())) -> Eq()(a, b) =
            tied(a, b, h);
        let _one_small(a: Small, b: Small, h: Eq()(Ord/ord(a, b), Ordering/eq())) -> Eq()(a, b) = tied(a, b, h);
        let _one_span(a: Span, b: Span, h: Eq()(Ord/ord(a, b), Ordering/eq())) -> Eq()(a, b) = tied(a, b, h);

        let _below_trees(a: Tree, b: Tree) -> Eq()(a < b, Ordering/is_lt(Ord/ord(a, b))) = below(a, b);
        let _at_most_spans(a: Span, b: Span) -> Eq()(a <= b, Ordering/is_le(Ord/ord(a, b))) = at_most(a, b);

        let _closed: Holds(Tree/leaf(9) < Tree/node(Tree/leaf(0), Tree/leaf(0))) = True/qed();

        print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// A witness is held to the laws where it is declared: an `ord` that answers `lt` both ways round has no proof that it flips, one that ties two constructors has no proof that they are one, and a witness that writes the methods alone is refused for the laws it leaves out. Each refused program breaks the one law and proves the others.
#[test]
fn a_witness_whose_order_breaks_a_law_is_refused_where_it_is_declared() {
    let program = |ord: &str, flips: &str, carries: &str, tied: &str| {
        fill!(
            r#"
            use /std/{Bool, Eq, Ord, print};
            use /std/Ord/{Ordering};
            use /std/Bool/{True};
            use /std/ops/{Eql};
            induct Side: Type | left() | right() end
            satisfy Eql(Side) { .. }
            let ord(a: Side, b: Side) -> Ordering =
                ${ord};
            satisfy Ord(Side) {
                ord = ord,
                lt(a, b) = Ordering/is_lt(ord(a, b)),
                le(a, b) = Ordering/is_le(ord(a, b)),
                flips(a, b) =
                    ${flips},
                trans(a, b, c, p, q) =
                    ${carries},
                eq(a, b, h) =
                    ${tied},
                is_lt(_, _) = Eq/refl(),
                is_le(_, _) = Eq/refl(),
            }
            print("ok")
            "#,
            ord,
            flips,
            carries,
            tied
        )
    };
    let by_cases = |left_right: &str, right_left: &str| {
        fill!(
            r#"match a
                | left() => match b | left() => Ordering/eq() | right() => Ordering/${left_right}() end
                | right() => match b | left() => Ordering/${right_left}() | right() => Ordering/eq() end
                end"#,
            left_right,
            right_left
        )
    };
    let every_case = r#"match a
                    | left() => match b | left() => Eq/refl() | right() => Eq/refl() end
                    | right() => match b | left() => Eq/refl() | right() => Eq/refl() end
                    end"#;
    let across = r#"match a
                    | left() => match c | left() => True/qed() | right() => True/qed() end
                    | right() =>
                        match b
                        | left() => match p end
                        | right() => match c | left() => match q end | right() => True/qed() end
                        end
                    end"#;
    let apart = r#"match a
                    | left() => match b | left() => Eq/refl() | right() => match h end end
                    | right() => match b | left() => match h end | right() => Eq/refl() end
                    end"#;

    let lawful = program(&by_cases("lt", "gt"), every_case, across, apart);
    assert_eq!(run(&lawful), b"ok");

    // `lt` both ways round: it carries across any middle side and ties no two, and the case of `flips` at two different sides asks that `lt` be `gt`.
    let unflipped = program(
        &by_cases("lt", "lt"),
        every_case,
        r#"match a
                    | left() => match c | left() => True/qed() | right() => True/qed() end
                    | right() => match c | left() => True/qed() | right() => True/qed() end
                    end"#,
        apart,
    );
    let report = error(&unflipped);
    assert!(
        report.contains("type mismatch")
            && report.contains("expected: `Eq(@Ordering)(Ordering/lt(), Ordering/gt())`"),
        "{report}"
    );

    // `eq` at every pair: it flips and carries across, and two different sides are tied and are not one.
    let conflating = program("Ordering/eq()", "Eq/refl()", "True/qed()", every_case);
    let report = error(&conflating);
    assert!(
        report.contains("type mismatch")
            && report.contains("expected: `Eq(@Side)(Side/left(), Side/right())`"),
        "{report}"
    );

    let lawless = r#"
        use /std/{Bool, Ord, print};
        use /std/Ord/{Ordering};
        use /std/ops/{Eql};
        induct Side: Type | left() | right() end
        satisfy Eql(Side) { .. }
        satisfy Ord(Side) {
            ord(a, b) = Ordering/eq(),
            lt(a, b) = false,
            le(a, b) = true,
        }
        print("unreachable")
        "#;
    let report = error(lawless);
    assert!(
        report.contains("method count mismatch")
            && report.contains("missing: `flips`, `trans`, `eq`, `is_lt`, `is_le`"),
        "{report}"
    );
}
