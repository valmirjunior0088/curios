//! What a mark is followed by at each kind of site, and the rule each refused form is reported by.

use {crate::*, curios_utilities::Plicity};

/// Every form a signature takes — a function type, a definition telescope, a payload, a type parameter — parses and prints back as written: a plain or `@` member named or as its type alone, and a `use` premise as its type.
#[test]
fn a_signature_member_is_a_type_under_its_mark() {
    for source in [
        "(n : Nat, Nat, @m : Nat, @Holds(0 < n), @_ : Holds(0 < n), use Show(Nat)) -> Nat",
        "let f(n : Nat, _ : Nat, @m : Nat, @Holds(0 < n), @_ : Holds(0 < n), use Show(Nat)) -> Nat = n; f",
        "satisfy (@A : Type, @Holds(0 < 1), use Show(A)) => Show(List(A)) { show = f } u",
        "induct Pos : Type | at(n : Nat, Nat, @m : Nat, @Holds(0 < n)) end u",
        "struct Slot(K : Type, @V : Type, use Key(K)) : Type { key : K } u",
        "concept Fm(A : Type) : Type { use Show(A), fm(@B : Type, use Show(B), A, B) -> Str } u",
    ] {
        let entrypoint = source.parse::<Entrypoint>().unwrap();
        assert_eq!(
            entrypoint.to_string().parse::<Entrypoint>().unwrap(),
            entrypoint,
            "round-trip failed for {source:?}"
        );
    }
}

/// A member written as its type alone has no binder in the tree: the definition telescope holds `None`, where a `_` the author wrote is a binder that binds nothing.
#[test]
fn a_member_written_as_its_type_has_no_binder() {
    let TopItem::Let(item) =
        &"let f(n : Nat, @Holds(0 < n), @_ : Holds(0 < n), use Show(Nat)) -> Nat = n; u"
            .parse::<Entrypoint>()
            .unwrap()
            .module
            .items[0]
    else {
        panic!("expected a let");
    };
    let LetSignature::Func { params, .. } = &item[0].signature else {
        panic!("expected function sugar");
    };

    assert!(matches!(&params[0].binder, Some(Pattern::Binder(name)) if name.as_str() == "n"));
    assert_eq!(
        (params[1].plicity, &params[1].binder),
        (Plicity::Implicit, &None)
    );
    assert!(matches!(&params[2].binder, Some(Pattern::Binder(name)) if name.as_str() == "_"));
    assert_eq!(
        (params[3].plicity, &params[3].binder),
        (Plicity::Witness, &None)
    );
}

/// Every form a lambda and the definition sugar bind parses and prints back as written: a binder under `@` or no mark, annotated or not, and `use _`.
#[test]
fn a_binding_member_is_a_binder_under_its_mark() {
    for source in [
        "(n, m : Nat, _, @a, @b : Nat, @_, @_ : Holds(0 < n), use _) => n",
        "((lo, hi), @Pair { fst, snd }, use _) => lo",
        "Fm { fm(@B, use _, a, b : Nat) = b }",
        "(fm(@B, use _, a) = a)",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.to_string().parse::<Term>().unwrap(),
            term,
            "round-trip failed for {source:?}"
        );
    }

    let term = "(@a, use _, n) => n".parse::<Term>().unwrap();
    let Subterm::Func(func) = term.as_subterm() else {
        panic!("expected a lambda");
    };
    assert_eq!(func.params[1].plicity, Plicity::Witness);
    assert!(matches!(&func.params[1].pattern, Pattern::Binder(name) if name.as_str() == "_"));
    assert_eq!(func.params[1].annotation, None);
}

/// A parameter list is a function type's and a lambda's as far as its closing parenthesis, so a member one of them refuses is held until the arrow says which it is: each of these parses, whichever alternative reads the list first.
#[test]
fn a_list_two_grammars_share_is_refused_by_neither_before_its_arrow() {
    for source in [
        "(@A : Type, use Show(A), x : A) -> A",
        "(@A, use _, x) => x",
        "(f(@A, use dict, x), 2)",
        "f(@_, use _, x)",
    ] {
        assert!(source.parse::<Term>().is_ok(), "{source:?} stopped parsing");
    }
}

/// Each form a site refuses is reported by the rule it breaks, never by the token a later alternative expected.
#[test]
fn a_refused_form_names_its_rule() {
    for (source, rule) in [
        // Where the site declares.
        (
            "let f(use d : Show(A), x : A) -> Str = x; u",
            "a `use` member has no binder",
        ),
        (
            "(use d : Show(A), A) -> Str",
            "a `use` member has no binder",
        ),
        (
            "let f(@A : Type, use _, x : A) -> Str = x; u",
            "`use _` holds a place and states no type",
        ),
        (
            "concept C(A : Type) : Type { use e : E(A), c : Nat } u",
            "a `use` member has no binder",
        ),
        (
            "struct S(K : Type, use k : Key(K)) : Type { key : K } u",
            "a `use` member has no binder",
        ),
        (
            "satisfy (@A : Type, use s : Show(A)) => Show(List(A)) { show = f } u",
            "a `use` member has no binder",
        ),
        (
            "concept Fm(A : Type) : Type { fm(use s : Show(A), A) -> Str } u",
            "a `use` member has no binder",
        ),
        (
            "let f(x) -> Nat = x; u",
            "a definition names its plain parameters",
        ),
        (
            "let f(n : Nat, Nat) -> Nat = n; u",
            "a definition names its plain parameters",
        ),
        // Where the site binds.
        ("(@A, use d, x) => x", "the member is written `use _`"),
        (
            "(@A, use Show(A), x) => x",
            "a `use` member is written `use _` here",
        ),
        (
            "(@A, use _ : Show(A), x) => x",
            "a `use` member is written `use _` here",
        ),
        (
            "(n, @Holds(0 < n)) => n",
            "after `@` a binder is read here, not a type",
        ),
        (
            "Fm { fm(@B, use d, a, b) = b }",
            "the member is written `use _`",
        ),
        (
            "Fm { fm(@B, use Show(B), a, b) = b }",
            "a `use` member is written `use _` here",
        ),
        // Where no mark, or not this one, is taken.
        (
            "induct P : Type | pack(@A : Type, use Show(A), value : A) end u",
            "a constructor's payload takes no `use` member",
        ),
        (
            "induct Fin : (@n : Nat) -> Type | zero() : (1) end u",
            "an index takes no mark",
        ),
        (
            "struct At : Type { n : Nat, @ok : Holds(0 < n) } u",
            "a field takes no mark",
        ),
        (
            "struct Has(A : Type) : Type { use Show(A), value : A } u",
            "a field takes no mark",
        ),
        ("{use Show(Nat), Nat}", "a field takes no mark"),
        (
            "concept C(A : Type) : Type { @c : Nat } u",
            "a field takes no mark",
        ),
        ("P { @a = 1 }", "no field is implicit"),
        (
            "let Over { use _, over } = d; u",
            "a struct pattern takes no mark",
        ),
        ("let At { n, @ok } = a; u", "a struct pattern takes no mark"),
        (
            "match a | At { n, @ok } => n end",
            "a struct pattern takes no mark",
        ),
        (
            "match t | two(use n, m) => m end",
            "a pattern takes no `use` member",
        ),
    ] {
        let report = source
            .parse::<Entrypoint>()
            .expect_err("the form is refused")
            .format();
        assert!(report.contains(rule), "{source:?} reported {report}");
        assert!(
            !report.contains("Expected '"),
            "{source:?} reported {report}"
        );
    }
}
