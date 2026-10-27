//! `unused-declaration`: a private definition or module nothing reaches, the roots that reach one, and what a kept declaration reads.

use {super::test_support::*, curios_utilities::fill};

#[test]
fn a_private_definition_nothing_reaches_is_reported_at_its_name() {
    assert_eq!(
        lints(
            r#"
        let helper : Type = Type;
        Type
    "#
        ),
        [
            "unused-declaration: unused declaration `helper`; delete it, make it `pub`, or name it `_helper` to keep it unused"
        ]
    );
}

#[test]
fn a_definition_the_entry_tail_reaches_is_used() {
    assert_eq!(
        lints(
            r#"
        let helper : Type = Type;
        helper
    "#
        ),
        Vec::<String>::new()
    );
}

#[test]
fn a_definition_a_public_one_reaches_is_used_and_the_public_one_is_a_root() {
    assert_eq!(
        lints(
            r#"
        let helper : Type = Type;
        pub let api : Type = helper;
        Type
    "#
        ),
        Vec::<String>::new()
    );
}

#[test]
fn a_definition_only_a_test_reaches_is_used() {
    assert_eq!(
        lints(
            r#"
        let helper : Type = Type;
        test it = helper;
        Type
    "#
        ),
        Vec::<String>::new()
    );
}

#[test]
fn a_definition_only_a_witness_reaches_is_used() {
    assert_eq!(
        lints(
            r#"
        pub concept Show(A : Type) : pub Type { show : A }
        struct Foo : Type { }
        satisfy Show(Foo) { show = Foo { } }
        Type
    "#
        ),
        Vec::<String>::new()
    );
}

#[test]
fn a_type_only_its_constructor_reaches_is_used() {
    assert_eq!(
        lints(
            r#"
        induct Foo : Type
        | mk()
        end
        Foo/mk()
    "#
        ),
        Vec::<String>::new()
    );
}

/// A literal names its structure in its head, which is no variable: the entry tail that builds one reaches the declaration as a definition that builds one does.
#[test]
fn a_structure_only_the_entry_tails_literal_builds_is_used() {
    assert_eq!(
        lints(
            r#"
        struct Point : Type { x : Type }
        let origin = Point { x = Type };
        origin
    "#
        ),
        Vec::<String>::new()
    );
}

#[test]
fn a_definition_used_only_by_itself_is_dead() {
    assert_eq!(
        lints(
            r#"
        let go : Type = go;
        Type
    "#
        ),
        [
            "unused-declaration: unused declaration `go`; delete it, make it `pub`, or name it `_go` to keep it unused"
        ]
    );
}

#[test]
fn two_dead_definitions_reaching_each_other_are_both_dead() {
    assert_eq!(
        lints(
            r#"
        let a : Type = b
        and b : Type = a;
        Type
    "#
        ),
        [
            "unused-declaration: unused declaration `a`; delete it, make it `pub`, or name it `_a` to keep it unused",
            "unused-declaration: unused declaration `b`; delete it, make it `pub`, or name it `_b` to keep it unused",
        ]
    );
}

#[test]
fn a_private_module_nothing_reaches_is_reported_once_at_the_mod() {
    assert_eq!(
        lints(
            r#"
        mod Internal
            pub let a : Type = Type;
            let b : Type = a;
            mod Deeper
                pub let c : Type = Type;
            end
        end
        Type
    "#
        ),
        ["unused-declaration: unused module `Internal`; delete it or make it `pub`"]
    );
}

#[test]
fn a_private_module_one_declaration_of_which_is_reached_reports_the_others() {
    assert_eq!(
        lints(
            r#"
        mod Internal
            pub let a : Type = Type;
            pub let b : Type = Type;
        end
        Internal/a
    "#
        ),
        [
            "unused-declaration: unused declaration `b`; delete it, make it `pub`, or name it `_b` to keep it unused"
        ]
    );
}

#[test]
fn a_facade_re_export_keeps_the_private_module_it_reaches_into() {
    assert_eq!(
        lints(
            r#"
        pub mod Api
            pub use /Impl/{f};
        end
        mod Impl
            pub let f : Type = Type;
        end
        Type
    "#
        ),
        Vec::<String>::new()
    );
}

#[test]
fn an_underscore_prefixed_declaration_and_a_public_one_are_kept() {
    assert_eq!(
        lints(
            r#"
        let _scratch : Type = Type;
        pub let api : Type = Type;
        Type
    "#
        ),
        Vec::<String>::new()
    );
}

/// A declaration a `_` keeps unused is a root: what it reads is used, as what a `test` reads is.
#[test]
fn what_a_declaration_kept_unused_reads_is_used() {
    assert_eq!(
        lints(
            r#"
        let helper : Type = Type;
        let _scratch : Type = helper;
        Type
    "#
        ),
        Vec::<String>::new()
    );
}

/// A `_` prefix marks a declaration, not a module: a private module none of whose declarations is reached is reported whatever its name begins with.
#[test]
fn an_underscore_prefixed_module_keeps_nothing() {
    assert_eq!(
        lints(
            r#"
        mod _Later
            pub let x : Type = Type;
        end
        Type
    "#
        ),
        ["unused-declaration: unused module `_Later`; delete it or make it `pub`"]
    );
}

/// The declarations the lints of `src` report unused.
fn unused(src: &str) -> Vec<String> {
    lints(src)
        .into_iter()
        .filter(|lint| lint.starts_with("unused-declaration"))
        .collect()
}

/// A unit declaring the `Spell` vocabulary of the fixture registry, its renderers private, around `rows`.
fn spelling(rows: &str) -> String {
    fill!(
        r#"
        pub induct Colour : pub Type | red() end
        ${rows}
        pub mod std
            pub mod Spell
                pub concept Spell(A : Type) : pub Type {
                    spell(A) -> Type,
                }
                let call : Type = Type;
                let record : Type = Type;
            end
            pub mod Str
                pub let Str : Type = Type;
            end
            pub mod Bool
                pub mod True
                    pub let qed : Type = Type;
                end
            end
        end
        Type
        "#,
        rows
    )
}

/// A derived witness lowers to a `Derive` transient, which carries no `Var`, so nothing in its body names the renderers the written body will apply: `derived_vocabulary` supplies them as references, and a renderer only a derived witness applies is used.
///
/// The names come from the fixture registry rather than from `/std`, so this is what makes those spellings load-bearing: a row whose concept does not match what the source declares yields no reference at all, and the control below is what says so.
#[test]
fn a_derived_witness_uses_the_vocabulary_its_body_will_name() {
    assert_eq!(
        unused(&spelling("satisfy /std/Spell/Spell(Colour) { .. }")),
        Vec::<String>::new(),
    );
    assert_eq!(
        unused(&spelling("")),
        [
            "unused-declaration: unused declaration `call`; delete it, make it `pub`, or name it `_call` to keep it unused",
            "unused-declaration: unused declaration `record`; delete it, make it `pub`, or name it `_record` to keep it unused",
        ],
        "the control stopped holding: with no derived witness, nothing names the renderers",
    );
}
