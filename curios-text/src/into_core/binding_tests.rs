//! Local and module bindings, and the type a let is pinned through.

use super::test_support::*;

#[test]
fn single_let_binding() {
    assert_eq!(
        run(r#"
            let x : Type = Type;
            x
        "#),
        curios_core::Term::let_(
            &global("/x"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/x")))
        ),
    );
}

#[test]
fn nested_module_binding_reference() {
    assert_eq!(
        run(r#"
            mod Foo
                pub let f : Type = Type;
            end
            Foo/f
        "#),
        curios_core::Term::let_(
            &global("/Foo/f"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/Foo/f")))
        ),
    );
}

#[test]
fn module_named_after_type_resolves_by_qualified_path() {
    assert_eq!(
        run(r#"
            mod Nat
                pub let double : Type = Type;
            end
            Nat/double
        "#),
        curios_core::Term::let_(
            &global("/Nat/double"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/Nat/double")))
        ),
    );
}

#[test]
fn use_shorthand_resolves_qualifier() {
    assert_eq!(
        run(r#"
            mod Foo
                pub mod Bar
                    pub let f : Type = Type;
                end
            end
            use Foo/{Bar};
            Bar/f
        "#),
        curios_core::Term::let_(
            &global("/Foo/Bar/f"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/Foo/Bar/f")))
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

// `/std/Async/block_on` pins an unannotated local binding through a *type alias* — `let Slot : Type = Cell(Option(Job));` — whose bare written `Type` mints a generalizable level while its value sits at one fixed level.
#[test]
fn an_unannotated_local_let_is_pinned_through_a_type_alias() {
    let module = elaborate_source(
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
        module
            .items
            .iter()
            .any(|item| matches!(item, curios_core::Item::Let(d) if d.name.symbol() == "/outer")),
        "outer elaborated"
    );
}

// A local binding is in scope of its own value: one that names itself lowers to a core `rec`, and a lone binding that does not stays the plain `let` it always was.
#[test]
fn a_local_binding_that_names_itself_lowers_to_a_rec() {
    let lowered = run(r#"
        let f(n : Type) -> Type = f(n);
        f
    "#);
    assert!(lowered.mentions_rec() && lowered.free_vars().is_empty());
    assert!(
        !run(r#"
            let x : Type = Type;
            x
        "#)
        .mentions_rec()
    );
}

// A local `let … and …;` group is one core `rec`, its members bound in one another.
#[test]
fn a_local_group_lowers_to_one_rec() {
    let lowered = run(r#"
        let a(x : Type) -> Type = b(x)
        and b(x : Type) -> Type = a(x);
        a
    "#);
    assert!(lowered.mentions_rec() && lowered.free_vars().is_empty());
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

#[test]
fn a_test_declaration_registers_by_kind_in_declaration_order() {
    // `Module::tests` keeps declaration order across module nesting — the order the synthesized tail will schedule — while the definition itself is an ordinary item of kind `Test`, pinned through the registry-built `() -> /syn/Test`.
    let module =
        lowered_module("mod Inner\ntest inner_holds() = x;\nend\ntest outer_holds() = y;\n()");
    assert_eq!(
        module.tests,
        vec![global_name("Inner/inner_holds"), global_name("outer_holds")],
    );
    let kinds: Vec<_> = module
        .items
        .iter()
        .filter_map(|item| match item {
            curios_core::Item::Let(def) => {
                Some((def.name.clone(), def.kind.clone(), def.type_.clone()))
            }
            curios_core::Item::Rec(_) => None,
        })
        .collect();
    let expected_type = curios_core::Term::func_type(
        [] as [(curios_core::Free, curios_core::Term); 0],
        curios_core::Term::var(curios_core::Var::free(global("syn/Test/Test"))),
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
fn a_parameterized_test_lowers_under_its_telescope() {
    // The seam the property-testing decision opens: the written telescope becomes the Π-type's, with the registry-built `/syn/Test` closed under it as the output, and the body is the lambda binding every parameter — the same sugar a `let` lowers through, so nothing about a test's shape is decided twice.
    let module = lowered_module("test t(n: Type, m: Type) = n;\n()");
    let definition = module
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
    assert_eq!(
        func_type.plicities(),
        vec![
            curios_utilities::Plicity::Explicit,
            curios_utilities::Plicity::Explicit
        ]
    );
    assert_eq!(
        *func_type.telescope.terminal(),
        curios_core::Term::var(curios_core::Var::free(global("syn/Test/Test")))
    );
    let curios_core::Subterm::Func(func) = &*definition.body else {
        panic!("expected a lambda, got {:?}", definition.body);
    };
    assert_eq!(func.telescope.len(), 2);
}
