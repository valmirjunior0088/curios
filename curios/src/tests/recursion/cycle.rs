//! The refusals that carry the rule: a cycle the source did not declare, one through a registry entry, a declared group that is no cycle, a value that reads itself, and a rebinding that shadows into a self-reference.

use crate::test_support::*;

#[test]
fn an_undeclared_cycle_is_refused_with_the_way_out() {
    let source = r#"
        use /std/{Nat};
        let f(n : Nat) -> Nat => g(n);
        let g(n : Nat) -> Nat => f(n);
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("`f` and `g` need one another") && report.contains("`let ... and ...`"),
        "expected the cycle named with its way out:\n{report}"
    );
    // Placed at the first member's name: a cycle is about its declarations as wholes, and holds no position of its own.
    assert!(
        report.contains("let f(n : Nat) -> Nat => g(n);"),
        "expected the snippet at `f`'s declaration:\n{report}"
    );
    assert_eq!(
        report.matches("need one another").count(),
        1,
        "expected the cycle reported once:\n{report}"
    );
}

/// A cycle through a family's constructor and one through a record's field are cycles as one through two bodies is: a registry entry reaches what its payload and field types name, so each is refused by name, with the way out, by the one place that refuses any.
#[test]
fn a_cycle_through_a_registry_entry_is_refused_by_name() {
    let through_a_family = r#"
        induct Tree : pub Type | leaf() | node(Forest) end
        let Forest : Type = Tree;
        /std/print("unreachable")
        "#;
    let through_a_record = r#"
        struct Tree : pub Type { held: Forest }
        let Forest : Type = Tree;
        /std/print("unreachable")
        "#;

    for source in [through_a_family, through_a_record] {
        let report = error(source);
        assert!(
            report.contains("`Tree` and `Forest` need one another")
                && report.contains("`let ... and ...`"),
            "expected the cycle named with its way out:\n{report}"
        );
        assert_eq!(
            report.matches("need one another").count(),
            1,
            "expected the cycle reported once:\n{report}"
        );
    }
}

/// A top-level group ties declarations that need one another and nothing else, a declaration reading any other wherever it is written: one whose members fall into parts is refused once, at its first member, naming each part, whether it groups definitions, families or witnesses, and each is accepted declared part by part. A witness's resolutions count as what its member reaches.
#[test]
fn a_top_level_group_that_is_no_cycle_is_refused_naming_its_parts() {
    let cases = [
        (
            r#"
            use /std/{Nat};
            let alone(n: Nat) -> Nat => n
            and generic(A: Type, n: Nat) -> Nat => alone(n);
            /std/print(Nat/to_str(generic(Nat, 1)))
            "#,
            r#"
            use /std/{Nat};
            let alone(n: Nat) -> Nat => n;
            let generic(A: Type, n: Nat) -> Nat => alone(n);
            /std/print(Nat/to_str(generic(Nat, 1)))
            "#,
            "`alone` alone, and `generic` alone",
        ),
        (
            r#"
            induct Pause: pub Type
            | yield_now()
            and Step(A: Type): pub Type
            | done(A)
            | step(pause: Pause)
            end
            /std/print("1")
            "#,
            r#"
            induct Pause: pub Type
            | yield_now()
            end
            induct Step(A: Type): pub Type
            | done(A)
            | step(pause: Pause)
            end
            /std/print("1")
            "#,
            "`Pause` alone, and `Step` alone",
        ),
        (
            r#"
            use /std/{Show};
            induct A: pub Type | a() end
            induct B: pub Type | b() end
            satisfy Show(A) {
                show(_) => "1",
            }
            and Show(B) {
                show(_) => Show/show(A/a()),
            }
            /std/print(Show/show(B/b()))
            "#,
            r#"
            use /std/{Show};
            induct A: pub Type | a() end
            induct B: pub Type | b() end
            satisfy Show(A) {
                show(_) => "1",
            }
            satisfy Show(B) {
                show(_) => Show/show(A/a()),
            }
            /std/print(Show/show(B/b()))
            "#,
            "the witness of `Show(A)` alone, and the witness of `Show(B)` alone",
        ),
    ];

    for (grouped, apart, parts) in cases {
        let report = error(grouped);
        assert!(
            report.contains("are grouped, and do not all need one another")
                && report.contains(&format!("declare each part on its own: {parts}")),
            "expected the group refused, naming its parts:\n{report}"
        );
        assert_eq!(
            report.matches("do not all need one another").count(),
            1,
            "expected the group refused once:\n{report}"
        );
        assert_eq!(
            run(apart),
            b"1",
            "declared part by part, the group is accepted"
        );
    }
}

// Read outside every lambda, the self-reference is an initializer evaluating itself, which the erased verifier refuses. The refusal stays at the erase boundary on purpose: forcing on first use is what a recursive value means, and no syntactic net can tell this from the knots the language admits — a member read through a closure, a later member forced first, a self-knot nothing ever forces.
#[test]
fn a_value_that_reads_itself_is_refused_at_the_erase_boundary() {
    let source = r#"
        use /std/{Nat};
        let xs : Nat = xs + 1;
        /std/print(Nat/to_str(xs))
        "#;

    let report = error(source);
    assert!(
        report.contains("evaluates itself"),
        "expected the verifier's refusal:\n{report}"
    );
    assert!(
        report.contains("/std/print(Nat/to_str(xs))"),
        "expected the refusal framed at the read:\n{report}"
    );
    assert!(
        report.contains("let xs : Nat = xs + 1;"),
        "expected a note at the value's own line:\n{report}"
    );
}

#[test]
fn a_local_value_that_reads_itself_is_refused_at_the_erase_boundary() {
    let source = r#"
        use /std/{Nat};
        let f(n : Nat) -> Nat =>
            let m : Nat = m + 1;
            m;
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("evaluates itself"),
        "expected the verifier's refusal:\n{report}"
    );
    assert!(
        report.contains("let m : Nat = m + 1;"),
        "expected the refusal framed at the local's own line:\n{report}"
    );
}

// The shadowing idiom: the new `n` is in scope of its own value, so the value mentions it, and an untyped recursive binding is refused before elaboration — with the rename it needs.
#[test]
fn a_shadowing_rebinding_is_refused_as_a_self_reference() {
    let source = r#"
        use /std/{Nat};
        let f(n : Nat) -> Nat =>
            let n = n + 1;
            n;
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("`n` mentions itself and states no type")
            && report.contains("rename one of them"),
        "expected the self-reference named with the rename:\n{report}"
    );
}
