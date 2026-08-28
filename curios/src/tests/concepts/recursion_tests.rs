//! A witness that resolves through a witness — its own entry, or another's.
//!
//! A witness declaration registers before its body elaborates, so a body may resolve the very entry it is defining. That makes the definition name itself, which no `let` can express: cross-definition value recursion exists only inside a `rec` item, so such a witness becomes a group of one and its self-reference is bound by that group rather than left free for the kernel to refuse.
//!
//! A cycle between *two* witnesses has no such binding, and the boundary is what these pin: one witness naming a later one is ordinary and works, because the lowerer orders a witness ahead of what dispatches through it; two naming each other deadlock that ordering and are refused here, in the language's own terms, rather than reaching the kernel as an unbound internal name.

use crate::tests::{error, run};

// The shape the derived bodies of a structural concept take: match the constructors, call the concept's own method on each recursive payload. Before this it elaborated and the kernel then refused the definition it produced.
#[test]
fn a_witness_resolves_through_its_own_entry() {
    let source = r#"
        use /std/{Nat, Str, Show, Handle};
        induct Tree : pub Type | leaf(Nat) | node(Tree, Tree) end
        satisfy Show(Tree) {
            show(t) =
                match t
                | leaf(n) => Nat/to_str(n)
                | node(l, r) =>
                    Str/concat("(", Str/concat(Show/show(l), Str/concat(" ", Str/concat(Show/show(r), ")"))))
                end,
        }
        /std/print(Show/show(Tree/node(Tree/leaf(1), Tree/leaf(2))))
        "#;

    assert_eq!(run(source), b"(1 2)");
}

// A recursive occurrence under another type former, spelled through *that* former's witness. The recursion still returns to this witness's own entry, one level down, with no dictionary supplied by hand.
#[test]
fn a_witness_recurses_through_another_formers_witness() {
    let source = r#"
        use /std/{Nat, Str, List, Show, Handle};
        induct Rose : pub Type | rose(Nat, List(Rose)) end
        satisfy Show(Rose) {
            show(r) =
                match r
                | rose(n, kids) =>
                    Str/concat(Nat/to_str(n), Str/concat("[", Str/concat(Show/show(kids), "]")))
                end,
        }
        /std/print(Show/show(Rose/rose(1, [Rose/rose(2, [])])))
        "#;

    assert_eq!(run(source), b"1[[2[[]]]]");
}

// One witness naming another declared after it is not recursion and must keep working: the lowerer emits a witness ahead of whatever dispatches through its concept, so the reference points backwards by the time the kernel walks it.
#[test]
fn a_witness_naming_a_later_witness_is_ordered_not_refused() {
    let source = r#"
        use /std/{Nat, Str, Show, Handle};
        induct P : pub Type | p(Nat) end
        induct Q : pub Type | q(Nat) end
        satisfy Show(P) { show(x) = match x | p(n) => Show/show(Q/q(n)) end, }
        satisfy Show(Q) { show(x) = match x | q(n) => Nat/to_str(n) end, }
        /std/print(Show/show(P/p(4)))
        "#;

    assert_eq!(run(source), b"4");
}

// Two witnesses resolving each other have no order that satisfies both, so one would be emitted naming the other before it exists. Refused by name, at a span, with the way out — where the kernel would have said `unbound name /witness@1` about an id the reader cannot find in their own program.
#[test]
fn two_witnesses_resolving_each_other_are_refused_with_the_way_out() {
    let source = r#"
        use /std/{Nat, Str, Show, Handle};
        induct A : pub Type | a(Nat) | ab(B) and B : pub Type | b(Nat) | ba(A) end
        satisfy Show(A) { show(x) = match x | a(n) => Nat/to_str(n) | ab(y) => Show/show(y) end, }
        satisfy Show(B) { show(x) = match x | b(n) => Nat/to_str(n) | ba(y) => Show/show(y) end, }
        /std/print(Show/show(A/ab(B/ba(A/a(5)))))
        "#;

    let report = error(source);
    assert!(
        report.contains("witnesses for Show(A) and Show(B) resolve each other"),
        "expected the cycle named by its two concept applications:\n{report}"
    );
    assert!(
        report.contains("rec ... and"),
        "expected the report to name the way out:\n{report}"
    );
}

// The way out, run: the recursion lives in one top-level group and each witness delegates to it. This is the form every witness in `/std` already takes, and it must stay the answer the refusal above points at.
#[test]
fn mutual_recursion_hoisted_into_one_group_is_admitted() {
    let source = r#"
        use /std/{Nat, Str, Show, Handle};
        induct A : pub Type | a(Nat) | ab(B) and B : pub Type | b(Nat) | ba(A) end
        rec show_a(x : A) -> Str =
            match x | a(n) => Nat/to_str(n) | ab(y) => show_b(y) end
        and show_b(x : B) -> Str =
            match x | b(n) => Nat/to_str(n) | ba(y) => show_a(y) end;
        satisfy Show(A) { show(x) = show_a(x), }
        satisfy Show(B) { show(x) = show_b(x), }
        /std/print(Show/show(A/ab(B/ba(A/a(5)))))
        "#;

    assert_eq!(run(source), b"5");
}
