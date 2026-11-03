//! Local and module bindings, and the type a let is pinned through.

use crate::into_core::test_support::*;

#[test]
fn single_let_binding() {
    assert_eq!(
        lowered(
            r#"
            let x : Type = Type;
            x
        "#
        ),
        (
            vec![(global_name("/x"), written_type(0), written_type(1))],
            curios_core::Term::written(global_name("/x")),
        ),
    );
}

#[test]
fn nested_module_binding_reference() {
    assert_eq!(
        lowered(
            r#"
            mod Foo
                pub let f : Type = Type;
            end
            Foo/f
        "#
        ),
        (
            vec![(global_name("/Foo/f"), written_type(0), written_type(1))],
            curios_core::Term::written(global_name("/Foo/f")),
        ),
    );
}

#[test]
fn module_named_after_type_resolves_by_qualified_path() {
    assert_eq!(
        lowered(
            r#"
            mod Nat
                pub let double : Type = Type;
            end
            Nat/double
        "#
        ),
        (
            vec![(global_name("/Nat/double"), written_type(0), written_type(1))],
            curios_core::Term::written(global_name("/Nat/double")),
        ),
    );
}

#[test]
fn use_shorthand_resolves_qualifier() {
    assert_eq!(
        lowered(
            r#"
            mod Foo
                pub mod Bar
                    pub let f : Type = Type;
                end
            end
            use Foo/{Bar};
            Bar/f
        "#
        ),
        (
            vec![(global_name("/Foo/Bar/f"), written_type(0), written_type(1))],
            curios_core::Term::written(global_name("/Foo/Bar/f")),
        ),
    );
}

#[test]
fn allows_pub_on_root_items() {
    run(r#"
        pub mod Foo
            pub let f : Type = Type;
        end
        pub let g : Type = Type;
        Type
    "#);
}

// An unannotated local binding pinned through a *type alias* — `let Slot : Type = Cell(Option(Job));` — whose bare written `Type` mints a generalizable level while its value sits at one fixed level.
#[test]
fn an_unannotated_local_let_is_pinned_through_a_type_alias() {
    let definitions = elaborate_source(
        "induct Box(A : Type) : Type | empty() | wrap(A) end \
         induct Job : Type | job() end \
         let Slot : Type = Box(Job); \
         let outer(@A : Type, x : A) -> A = \
             let slot = Box/empty(); \
             let force(b : Slot) -> {} = (); \
             let _ = force(slot); \
             x; \
         outer",
    );
    assert!(
        definitions
            .items
            .iter()
            .any(|item| matches!(item, curios_core::Item::Let(d) if d.name.faithful().to_string() == "/outer")),
        "outer elaborated"
    );
}

// A local binding is in scope of its own value: one that names itself lowers to a core `rec`, and a lone binding that does not stays the plain `let` it always was.
#[test]
fn a_local_binding_that_names_itself_lowers_to_a_rec() {
    assert_eq!(
        closed_group(
            r#"
            let f(n : Type) -> Type = f(n);
            f
            "#
        ),
        Some(vec!["/f".to_string()])
    );
    assert!(
        !run(r#"
            let x : Type = Type;
            x
        "#)
        .mentions_rec()
    );
}

// A local `let … and …;` group is one core `rec`, whose members' bodies name one another.
#[test]
fn a_local_group_lowers_to_one_rec() {
    assert_eq!(
        closed_group(
            r#"
            let a(x : Type) -> Type = b(x)
            and b(x : Type) -> Type = a(x);
            a
            "#
        ),
        Some(vec!["/a".to_string(), "/b".to_string()])
    );
}

// The three shapes a recursive binding cannot take, each refused by name: no type, a pattern binder, an action.
#[test]
fn a_recursive_local_binding_without_a_type_is_refused() {
    let report = run_err("let x = x; x");
    assert!(
        report.contains("`x` mentions itself and states no type"),
        "unexpected report: {report}"
    );
}

#[test]
fn a_recursive_local_pattern_binding_is_refused() {
    let report = run_err("let (a, b) : Type = (a, b); a");
    assert!(
        report.contains("a recursive binding is a plain name"),
        "unexpected report: {report}"
    );
}

#[test]
fn a_recursive_local_action_binding_is_refused() {
    let report = run_err("(y : Type) => let x : Type = f(x)!; x");
    assert!(
        report.contains("`x` is bound by an action that mentions it"),
        "unexpected report: {report}"
    );
}

/// A nominal head that resolves to a local names no structure, and is filed under a binder of its own rather than under the root-level global its spelling would name.
///
/// Capture is what the binder is for: `Qualifier::from([head])` is exactly what an entry module's `struct Box` lowers to, so a head lowered to its spelling would read that declaration wherever a binder of the name shadows it — one name meaning the parameter to resolution and the structure to the pattern. The second source below declares the structure the capture would reach and is otherwise the first, so the two differ in nothing but whether the capture has a target.
#[test]
fn a_nominal_head_that_resolves_to_a_local_names_no_structure() {
    let shadowing = r#"
        let f(Box : Type, v : Box) -> Type =
            let Box { it } = v;
            Type;
        f
    "#;
    let declared = r#"
        struct Box(A : Type) : Type { it : A }

        let f(Box : Type, v : Box) -> Type =
            let Box { it } = v;
            Type;
        f
    "#;

    for (label, source) in [
        ("with nothing declared under the name", shadowing),
        ("with a structure declared under the name", declared),
    ] {
        let report = run_err(source);
        assert!(
            report.contains("`Box` names no structure in scope"),
            "{label}: unexpected report: {report}"
        );
    }
}

#[test]
fn a_test_declaration_registers_by_kind_in_declaration_order() {
    // `Module::tests` keeps declaration order across module nesting — the order the synthesized tail will schedule — while the definition itself is an ordinary item of kind `Test`, pinned through the registry-built `() -> /std/Test`.
    let definitions =
        lowered_definitions("mod Inner\ntest inner_holds = x;\nend\ntest outer_holds = y;\n()");
    assert_eq!(
        definitions.tests,
        vec![global_name("Inner/inner_holds"), global_name("outer_holds")],
    );
    let kinds: Vec<_> = definitions
        .items
        .iter()
        .filter_map(|item| match item {
            curios_core::Item::Let(def) => Some((def.name, def.kind.clone(), def.type_.clone())),
            curios_core::Item::Rec(_) => None,
        })
        .collect();
    let expected_type = curios_core::Term::func_type(
        [] as [(curios_core::Free, curios_core::Term); 0],
        curios_core::Term::apply(
            curios_core::Term::written(global_name("std/Test/Test")),
            [curios_core::Term::tuple_type_unit()],
        ),
    );
    for name in ["Inner/inner_holds", "outer_holds"] {
        let (_, kind, type_) = kinds
            .iter()
            .find(|(n, _, _)| *n == global_name(name))
            .expect("the test lowers to a definition");
        assert_eq!(*kind, curios_core::DefinitionKind::Test);
        assert_eq!(*type_, expected_type);
    }
}

#[test]
fn nothing_names_a_test() {
    // A test binds nothing, so a name written for one resolves as a name nothing declares does, bare or through its module.
    let bare = run_err("test t = Type;\nt");
    assert!(bare.contains("`t` is not in scope"), "{bare}");
    let qualified = run_err("mod Inner\ntest t = Type;\nend\nInner/t");
    assert!(qualified.contains("has no `t`"), "{qualified}");
}

#[test]
fn a_test_takes_its_name_among_its_modules_declarations() {
    // A test's path is its report line and what a filter selects, so its name is as unique as a binding's, whichever of the two is written first.
    for source in [
        "test t = Type;\nlet t: Type = Type;\n()",
        "let t: Type = Type;\ntest t = Type;\n()",
        "test t = Type;\ntest t = Type;\n()",
    ] {
        let report = run_err(source);
        assert!(
            report.contains("`t` is already declared in this module"),
            "{source}: {report}"
        );
    }
}

#[test]
fn a_built_unit_holds_no_test_and_a_tested_one_holds_each() {
    // A build passes every test as if it were not written, nested ones included: none is registered and none lowers to an item. The same source tested holds each, in declaration order.
    let entrypoint = "test outer = Type;\nmod Inner\ntest inner = Type;\nend\n()"
        .parse::<curios_surface::Entrypoint>()
        .unwrap();
    let lowered = |loader: &crate::RootSource| {
        crate::into_core::into_core_unit(
            &crate::into_core::UnitSource::entry(&entrypoint, loader),
            &[],
            syntax(),
        )
        .unwrap()
        .core()
        .clone()
    };
    let is_test = |item: &curios_core::Item| matches!(item, curios_core::Item::Let(def) if def.kind == curios_core::DefinitionKind::Test);

    let built = lowered(&crate::RootSource::none());
    assert!(built.tests.is_empty());
    assert!(!built.items.iter().any(is_test));

    let tested = lowered(&fixture_loader());
    assert_eq!(
        tested.tests,
        vec![global_name("outer"), global_name("Inner/inner")]
    );
    assert_eq!(tested.items.iter().filter(|item| is_test(item)).count(), 2);
}

#[test]
fn a_test_lowers_to_the_thunk_its_surface_no_longer_spells() {
    // A test takes no parameters, but it is still a `() -> Test({})` in Core: `Test/main` holds the whole schedule and forces only the one it selected, so the body cannot be a bare value, and the empty telescope is the thunk.
    let definitions = lowered_definitions("test t = Type;\n()");
    let definition = definitions
        .items
        .iter()
        .find_map(|item| match item {
            curios_core::Item::Let(def) if def.name == global_name("t") => Some(def),
            curios_core::Item::Let(_) | curios_core::Item::Rec(_) => None,
        })
        .expect("the test lowers to a definition");
    assert_eq!(definition.kind, curios_core::DefinitionKind::Test);
    let curios_core::Subterm::FuncType(func_type) = &*definition.type_ else {
        panic!("expected a function type, got {:?}", definition.type_);
    };
    assert!(func_type.plicities().is_empty());
    assert_eq!(
        *func_type.telescope.terminal(),
        curios_core::Term::apply(
            curios_core::Term::written(global_name("std/Test/Test")),
            [curios_core::Term::tuple_type_unit()]
        )
    );
    let curios_core::Subterm::Func(func) = &*definition.body else {
        panic!("expected a lambda, got {:?}", definition.body);
    };
    assert_eq!(func.telescope.len(), 0);
}
