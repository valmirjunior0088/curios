//! A goal no witness answers: what the refusal names — the concept, the key, the call, the premise and the type.

use crate::tests::error;

// No witness registered for the goal's head: a resolution-time error.
#[test]
fn missing_witness_is_an_error() {
    let source = r#"
        use /std/{Nat, Bool, Str};
        pub concept Show(A : Type) : pub Type {
            show(A) -> Str
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        let b : Bool = true;
        /std/print(Show/show(b))
        "#;

    assert!(error(source).contains("witness"));
}

// A missing operator witness used in an inductive's constructor *index type* once surfaced as a bare `?m ≡ ?n` metavariable mismatch: the constructor is elaborated twice, and reconciling the two elaborations parks a conversion between their (unsolvable) witness holes. It is now reported as the unresolved witness it is, naming the concept, the key, and the `+` that needed it.
#[test]
fn missing_witness_in_constructor_index_names_the_concept() {
    let source = r#"
        use /std/{Nat};
        use /std/ops/{Add};
        pub struct Wrap : pub Type { n : Nat }
        pub induct Foo : (w : Wrap) -> pub Type
        | mk(@w : Wrap, prev : Foo(w)) : (w + w)
        end
        /std/print("no")
        "#;

    let message = error(source);
    assert!(message.contains("witness"), "got: {message}");
    assert!(message.contains("Add(Wrap)"), "got: {message}");
}

// Which call needs the witness. A curried application — every partial application, and `Fmt/print(fmt)(a)(b)` in particular — heads its outer apply with another apply, so reading only the outermost node named `<function>` for exactly the calls a reader most needs identified.
//
// The argument is a *labeled* tuple deliberately: `/std/Tuple` shows every positional shape, and labels are part of a tuple type's identity, so the labeled product is what still has no `Show`.
#[test]
fn a_missing_witness_names_a_curried_head_by_its_innermost_reference() {
    let source = r#"
        use /std/{Nat, Bool, Fmt};
        let s = Fmt/print("issue % -> %")(42)((x = 1, y = true));
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("needed by '/std/Fmt/print'"),
        "expected the head named through the spine:\n{report}"
    );
}

// Which premise needs it. A `use` parameter is anonymous by design — `let`, `rec` and `satisfy` sugar declare one without a name — so naming the binder reported `_` for every premise a program actually writes. The position is always there to be named.
//
// The argument is a *labeled* tuple for the reason above: the positional shapes all have a `Show`.
#[test]
fn a_missing_witness_names_the_premise_by_position() {
    let source = r#"
        use /std/{Nat, Bool, Show, Str};
        let f(@A : Type, use Show(A), a : A) -> Str = Show/show(a);
        let s : Str = f((x = 1, y = true));
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("needed by '/f' for its 1st 'use' premise"),
        "expected the premise named by position:\n{report}"
    );
}

// The position is a position, not a decoration: a head with two premises whose *second* is the unsatisfied one says so, which is the whole reason a number beats a name here.
#[test]
fn a_later_premise_is_named_by_its_own_position() {
    let source = r#"
        use /std/{Nat, Str, Show};
        use /std/ops/{Eql};
        induct T : pub Type | t() end
        satisfy Show(T) { show(x) = "t", }
        let g(@A : Type, use Show(A), use Eql(A), a : A) -> Str = Show/show(a);
        let s : Str = g(T/t());
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("needed by '/g' for its 2nd 'use' premise"),
        "expected the second premise named as the second:\n{report}"
    );
}

#[test]
fn a_missing_witness_over_a_nominal_type_spells_its_name() {
    // A goal is rendered as every report renders a type. A nominal declaration is a recursive group of one, and a zonked solution spells it as the `Rec` node itself until the refold gives the name back — the deferred-goal report once skipped that refold and wrote `Spell(rec #0: Type = Opaque; #0)`.
    let report = error(
        r#"
        use /std/{Str, Spell};
        induct Opaque: pub Type | o() end
        let spelled: Str = Spell/spell(Opaque/o());
        /std/print(spelled)
        "#,
    );
    assert!(
        report.contains("no witness of Spell(Opaque) found"),
        "{report}"
    );
}
