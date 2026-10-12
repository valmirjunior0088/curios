//! What a name that resolves to nothing is told: one refusal, of the declaration that wrote it, with the nearest name where it was looked up and the ways to reach one elsewhere.

use super::test_support::*;

fn refused(source: &str) -> String {
    compile(source).map(|_| ()).unwrap_err()
}

#[test]
fn a_misspelled_binder_is_offered_the_one_it_is_a_swap_from() {
    let error = refused(
        r#"
        use /std/{Nat};

        let f(count: Nat) -> Nat = coutn + 1;

        /std/print("x")
    "#,
    );
    assert!(
        error.contains("`coutn` is not in scope\n  did you mean `count`?"),
        "{error}"
    );
}

#[test]
fn a_misspelled_declaration_is_offered_its_sibling() {
    let error = refused(
        r#"
        use /std/{Nat};

        let total(n: Nat) -> Nat = n;

        let g(n: Nat) -> Nat = totl(n);

        /std/print("x")
    "#,
    );
    assert!(
        error.contains("`totl` is not in scope\n  did you mean `total`?"),
        "{error}"
    );
}

#[test]
fn a_name_that_differs_in_case_alone_is_offered() {
    let error = refused(
        r#"
        use /std/{Nat};

        let Total(n: Nat) -> Nat = n;

        let g(n: Nat) -> Nat = total(n);

        /std/print("x")
    "#,
    );
    assert!(
        error.contains("`total` is not in scope\n  did you mean `Total`?"),
        "{error}"
    );
}

// A path stops at the step that finds nothing, and says which module it had reached.
#[test]
fn a_misspelled_member_names_the_module_it_stopped_in() {
    let error = refused(
        r#"
        use /std/{Nat, Option};

        let g(n: Nat) -> Option(Nat) = Option/sme(n);

        /std/print("x")
    "#,
    );
    assert!(
        error.contains("`/std/Option` has no `sme`\n  did you mean `some`?"),
        "{error}"
    );
}

#[test]
fn a_misspelled_module_is_offered_the_one_it_is_near_wherever_it_is_looked_up() {
    for (path, said) in [
        (
            "Nat/el/add(a, a)",
            "`/std/Nat` has no module `el`\n  did you mean `le`?",
        ),
        (
            "/std/Nta/add(a, a)",
            "`/std` has no module `Nta`\n  did you mean `Nat`?",
        ),
        (
            "Nta/add(a, a)",
            "`Nta` is not in scope\n  did you mean `Nat`?",
        ),
    ] {
        let error = refused(&format!(
            r#"
            use /std/{{Nat}};

            let p(a: Nat) -> Nat = {path};

            /std/print("x")
            "#
        ));
        assert!(error.contains(said), "{path}: {error}");
    }
}

// A `use` that names nothing refuses its unit, in the words a name in a declaration is refused in.
#[test]
fn a_misspelled_selector_is_offered_the_member_it_is_near() {
    let error = refused(
        r#"
        use /std/Nat/{to_srt};

        /std/print("x")
    "#,
    );
    assert!(
        error.contains("`/std/Nat` has no `to_srt`\n  did you mean `to_str`?"),
        "{error}"
    );
}

#[test]
fn a_module_out_of_scope_is_told_how_to_reach_it() {
    let error = refused(
        r#"
        use /std/{Nat};

        let f(xs: Nat) -> Nat = List/len(xs);

        /std/print("x")
    "#,
    );
    assert!(
        error.contains(
            "`List` is not in scope\n  `List` is `/std/List`: write it absolute, or `use /std/{List};`"
        ),
        "{error}"
    );
}

#[test]
fn a_literals_head_that_names_nothing_is_offered_the_structure_it_is_near() {
    let error = refused(
        r#"
        use /std/{Nat};

        struct Pair: pub Type {
            fst: Nat,
            snd: Nat,
        }

        let p: Pair = Piar { 1, 2 };

        /std/print("x")
    "#,
    );
    assert!(
        error.contains("`Piar` is not in scope\n  did you mean `Pair`?"),
        "{error}"
    );
}

#[test]
fn a_private_member_is_said_to_be_private_to_its_module() {
    let error = refused(
        r#"
        use /std/{Nat};

        mod Inner
            use /std/{Nat};

            let hidden: Nat = 1;
            pub let shown: Nat = 2;
        end

        let a: Nat = Inner/hidden;

        /std/print("x")
    "#,
    );
    assert!(error.contains("`hidden` is private to `/Inner`"), "{error}");
}

// A name written in a declaration refuses that declaration and no other, a path as a bare name: both are reported in one run, each under its own declaration and in its own words, and a declaration that names neither stands.
#[test]
fn each_declaration_is_refused_for_the_name_it_wrote() {
    let error = refused(
        r#"
        use /std/{Nat, Option};

        let f(n: Nat) -> Nat = Nat/to_srt(n);

        let g(n: Nat) -> Option(Nat) = Option/sme(n);

        let h(n: Nat) -> Nat = n + 1;

        /std/print(Nat/to_str(h(1)))
    "#,
    );
    assert!(
        error
            .contains("while elaborating f:\n`/std/Nat` has no `to_srt`\n  did you mean `to_str`?"),
        "{error}"
    );
    assert!(
        error.contains("while elaborating g:\n`/std/Option` has no `sme`\n  did you mean `some`?"),
        "{error}"
    );
    assert!(!error.contains("while elaborating h:"), "{error}");
}

// A root the program may not write is a step that fails for a reason of its own, and the name reached for under it is offered as any unresolved name is: by the paths that carry it. The declaration that wrote it is refused alone.
#[test]
fn a_name_under_a_root_out_of_reach_refuses_its_declaration_alone() {
    let error = refused(
        r#"
        use /std/{Nat};

        let x: /sys/Nat = 1;

        let y: Nat = cuont;

        /std/print("x")
    "#,
    );
    assert!(
        error.contains(
            "while elaborating x:\n`sys` is internal to the standard library\n  `Nat` is `/std/Nat`: write it absolute, or `use /std/{Nat};`"
        ),
        "{error}"
    );
    assert!(
        error.contains("while elaborating y:\n`cuont` is not in scope"),
        "{error}"
    );
}
