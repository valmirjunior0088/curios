//! Modules, entrypoints, qualified paths, and the `use` forms that reach across them.

use {crate::*, curios_utilities::Qualifier};

#[test]
fn a_path_admits_no_whitespace_and_division_requires_it() {
    // Paths are whitespace-free and infix operators require whitespace on both sides (syntax.md's lexical rule), so `a/b` is only ever the path, `a / b` only ever the division, and the asymmetric spellings satisfy neither grammar.
    assert_eq!("a/b".parse::<Term>().unwrap().to_string(), "a/b");
    assert_eq!("/std/Nat".parse::<Term>().unwrap().to_string(), "/std/Nat");
    assert_eq!("a / b".parse::<Term>().unwrap().to_string(), "a / b");
    assert!("a/ b".parse::<Term>().is_err());
    assert!("a /b".parse::<Term>().is_err());
    assert!("/ std".parse::<Term>().is_err());
}

#[test]
fn parse_module_roundtrip() {
    let m = r#"
        use Bar/{x};
        pub let x : Type = Type;
        let f : Type = Type;
    "#
    .parse::<Module>()
    .unwrap();
    assert_eq!(m.items.len(), 3);
    assert!(matches!(m.items[0], TopItem::Use(_)));
    assert!(matches!(
        m.items[1],
        TopItem::Let(ref items) if items[0].vis_pub
    ));
    assert!(matches!(m.items[2], TopItem::Let(_)));
}

#[test]
fn parse_nested_module() {
    let m = r#"
        mod Inner
            pub let x : Type = Type;
        end
    "#
    .parse::<Module>()
    .unwrap();
    assert_eq!(
        m.items,
        vec![TopItem::Mod(TopMod {
            span: None,
            vis_pub: false,
            label: "Inner".to_string(),
            module: Some(Module {
                items: vec![TopItem::Let(vec![TopLet {
                    vis_pub: true,
                    label: "x".to_string(),
                    signature: LetSignature::Name {
                        type_: Some(Subterm::Type.into()),
                        body: Subterm::Type.into(),
                    },
                }])],
            }),
        })]
    );
}

#[test]
fn parse_entrypoint_roundtrip() {
    let entrypoint = r#"
        use Foo/{x};
        use Bar/{x};
        pub let f : Type = Type;
        let x : Type = Type;
        f
    "#
    .parse::<Entrypoint>()
    .unwrap();
    assert_eq!(entrypoint.module.items.len(), 4);
    assert!(matches!(entrypoint.module.items[0], TopItem::Use(_)));
    assert!(matches!(entrypoint.module.items[1], TopItem::Use(_)));
    assert!(matches!(entrypoint.module.items[2], TopItem::Let(_)));
    assert!(matches!(
        entrypoint.module.items[3],
        TopItem::Let(ref items) if !items[0].vis_pub
    ));
    assert_eq!(
        entrypoint.tail,
        Term::from(Subterm::Name(Name::from(["f".to_string()])))
    );
}

#[test]
fn parse_qualified_path() {
    assert_eq!(
        "Foo/bar/baz".parse::<Term>().unwrap(),
        Term::from(Subterm::Name(Name::from([
            "Foo".to_string(),
            "bar".to_string(),
            "baz".to_string()
        ])))
    );
}

#[test]
fn type_name_as_path_segment() {
    assert_eq!(
        "Nat/double".parse::<Term>().unwrap(),
        Term::from(Subterm::Name(Name::from([
            "Nat".to_string(),
            "double".to_string()
        ])))
    );
    assert_eq!(
        "Type/foo".parse::<Term>().unwrap(),
        Term::from(Subterm::Name(Name::from([
            "Type".to_string(),
            "foo".to_string()
        ])))
    );
}

#[test]
fn bare_type_names_parse_as_names() {
    assert_eq!(
        "Nat".parse::<Term>().unwrap(),
        Term::from(Subterm::Name(Name::from(["Nat".to_string()])))
    );
    assert_eq!(
        "Int".parse::<Term>().unwrap(),
        Term::from(Subterm::Name(Name::from(["Int".to_string()])))
    );
    assert_eq!(
        "Flt".parse::<Term>().unwrap(),
        Term::from(Subterm::Name(Name::from(["Flt".to_string()])))
    );
    assert_eq!("Type".parse::<Term>().unwrap(), Term::from(Subterm::Type));
}

#[test]
fn use_brace_group() {
    assert_eq!(
        "use /std/{Bin, List};".parse::<Module>().unwrap().items,
        vec![TopItem::Use(TopUse {
            vis_pub: false,
            name: Name::new(true, Qualifier::from(["std".to_string()])),
            group: UseGroup::Named(vec![
                GroupItem::Both("Bin".to_string()),
                GroupItem::Both("List".to_string()),
            ]),
        })]
    );
}

#[test]
fn use_brace_group_kinds() {
    assert_eq!(
        "use /std/{mod Bin, let Nat, List};"
            .parse::<Module>()
            .unwrap()
            .items,
        vec![TopItem::Use(TopUse {
            vis_pub: false,
            name: Name::new(true, Qualifier::from(["std".to_string()])),
            group: UseGroup::Named(vec![
                GroupItem::Mod("Bin".to_string()),
                GroupItem::Let("Nat".to_string()),
                GroupItem::Both("List".to_string()),
            ]),
        })]
    );
}

#[test]
fn use_brace_group_empty() {
    assert_eq!(
        "use /std/{};".parse::<Module>().unwrap().items,
        vec![TopItem::Use(TopUse {
            vis_pub: false,
            name: Name::new(true, Qualifier::from(["std".to_string()])),
            group: UseGroup::Named(vec![]),
        })]
    );
}

#[test]
fn parse_use_glob() {
    assert_eq!(
        "use /sys/Nat/*;".parse::<Module>().unwrap().items,
        vec![TopItem::Use(TopUse {
            vis_pub: false,
            name: Name::new(
                true,
                Qualifier::from(["sys".to_string(), "Nat".to_string()])
            ),
            group: UseGroup::Glob,
        })]
    );
}

#[test]
fn use_entries_are_struct_literal_only() {
    // A `use <term>` entry parses in a struct literal (a concept literal by intent — non-concept heads are rejected at elaboration, not parse)...
    let term = "Ord(Nat) { use my_eql, cmp = f }".parse::<Term>().unwrap();
    let Subterm::StructLit(StructLit { entries, .. }) = term.as_subterm() else {
        panic!("expected a struct literal");
    };
    assert!(matches!(entries[0], StructLitEntry::Use(_)));
    assert!(matches!(entries[1], StructLitEntry::Field(_)));

    // ...but not in a tuple literal: `use` is reserved, so the tuple parser cannot take it as a field, and the term fails to parse.
    assert!("(use my_eql, 2)".parse::<Term>().is_err());
}
