mod context;
use context::*;

mod elaborate;
use elaborate::*;

use {super::*, crate::core, std::collections::HashMap};

fn process_items(
    top_items: &[TopItem],
    context: &mut Context,
    flat_items: &mut Vec<FlatItem>,
    def_stack: &DefStack,
    loader: &dyn Loader,
) {
    let mut info = ModuleInfo::new();

    for top_item in top_items {
        match top_item {
            TopItem::Mod(mod_item) => {
                context.insert_scope(mod_item.label.clone(), context.prefixed(&mod_item.label));
                info.insert_child(mod_item.label.clone(), mod_item.is_pub);

                match &mod_item.module {
                    Some(module) => process_items(
                        &module.items,
                        &mut context.nested(&mod_item.label),
                        flat_items,
                        def_stack,
                        loader,
                    ),
                    None => {
                        let module = loader
                            .load(context.prefix(), &mod_item.label)
                            .unwrap_or_else(|e| panic!("{e}"));

                        process_items(
                            &module.items,
                            &mut context.nested(&mod_item.label),
                            flat_items,
                            def_stack,
                            loader,
                        );
                    }
                }
            }
            TopItem::Use(use_item) => {
                context.resolve_use(use_item);

                if use_item.is_pub {
                    let qualifier = use_item.name.last().to_string();
                    context.register_alias(&qualifier);
                    info.insert_child(qualifier, true);
                }
            }
            TopItem::Let(let_item) => {
                let elab = Elaborate::new(
                    context.scope(),
                    context.table(),
                    context.aliases(),
                    def_stack,
                );

                info.insert_binding(let_item.label.clone(), let_item.is_pub);

                flat_items.push(FlatItem::Let(FlatLet {
                    name: context.prefixed(&let_item.label),
                    type_: elab.term(&let_item.type_),
                    body: elab.term(&let_item.body),
                }));
            }
            TopItem::Rec(ls) => {
                flat_items.push(FlatItem::Rec(
                    ls.iter()
                        .map(|let_item| {
                            let elaborate = Elaborate::new(
                                context.scope(),
                                context.table(),
                                context.aliases(),
                                def_stack,
                            );

                            info.insert_binding(let_item.label.clone(), let_item.is_pub);

                            FlatLet {
                                name: context.prefixed(&let_item.label),
                                type_: elaborate.term(&let_item.type_),
                                body: elaborate.term(&let_item.body),
                            }
                        })
                        .collect(),
                ));
            }
            TopItem::Def(def_item) => {
                let name = context.prefixed(&def_item.label);

                let witness = Elaborate::new(
                    context.scope(),
                    context.table(),
                    context.aliases(),
                    def_stack,
                )
                .term(&def_item.witness);

                context.insert_scope(def_item.label.clone(), name.clone());
                info.insert_child(def_item.label.clone(), def_item.is_pub);
                info.insert_binding(def_item.label.clone(), def_item.is_pub);

                flat_items.push(FlatItem::Def(FlatDef {
                    name: name.clone(),
                    witness,
                }));

                let new_def_stack = def_stack.push(def_item.label.clone(), name);

                process_items(
                    &def_item.module.items,
                    &mut context.nested(&def_item.label),
                    flat_items,
                    &new_def_stack,
                    loader,
                );
            }
        }
    }

    context.finalize(info);
}

fn check_entrypoint(items: &[TopItem]) {
    for item in items {
        match item {
            TopItem::Mod(mod_item) if mod_item.is_pub => panic!("pub on top-level entrypoint item"),
            TopItem::Let(let_item) if let_item.is_pub => panic!("pub on top-level entrypoint item"),
            TopItem::Def(def_item) if def_item.is_pub => panic!("pub on top-level entrypoint item"),
            TopItem::Rec(let_items) if let_items.iter().any(|let_item| let_item.is_pub) => {
                panic!("pub on top-level entrypoint item")
            }
            _ => {}
        }
    }
}

fn fold_flat_item(acc: core::Term, item: FlatItem) -> core::Term {
    match item {
        FlatItem::Def(def) => core::Sealed::new(def.name.join(), def.witness, acc).into(),
        FlatItem::Let(let_) => core::Let::new(let_.name.join(), let_.type_, let_.body, acc).into(),
        FlatItem::Rec(items) => core::Rec::new(
            items
                .into_iter()
                .map(|item| (item.name.join(), item.type_, item.body)),
            acc,
        )
        .into(),
    }
}

pub fn to_core(entrypoint: &Entrypoint, loader: &dyn Loader) -> core::Term {
    check_entrypoint(&entrypoint.items);

    let mut table = HashMap::new();
    let mut aliases = HashMap::new();
    let mut context = Context::new(&mut table, &mut aliases);
    let mut flat_items = Vec::new();

    process_items(
        &entrypoint.items,
        &mut context,
        &mut flat_items,
        &DefStack::empty(),
        loader,
    );

    flat_items.into_iter().rev().fold(
        Elaborate::new(
            context.scope(),
            context.table(),
            context.aliases(),
            &DefStack::empty(),
        )
        .term(&entrypoint.tail),
        fold_flat_item,
    )
}

#[cfg(test)]
mod tests {
    use crate::{core, text};

    fn run(src: &str) -> core::Term {
        super::to_core(
            &src.parse::<text::Entrypoint>().unwrap(),
            &text::PanicLoader,
        )
    }

    #[test]
    fn no_items_simple_tail() {
        assert_eq!(run("Type"), core::Term::Type);
    }

    #[test]
    fn single_let_binding() {
        assert_eq!(
            run(r#"
                let x : Type = Type;
                x
            "#),
            core::Let::new("x", core::Type, core::Type, core::Var::free("x")).into(),
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
            core::Let::new("Foo/f", core::Type, core::Type, core::Var::free("Foo/f")).into(),
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
                use Foo/Bar;
                Bar/f
            "#),
            core::Let::new(
                "Foo/Bar/f",
                core::Type,
                core::Type,
                core::Var::free("Foo/Bar/f")
            )
            .into(),
        );
    }

    #[test]
    #[should_panic(expected = "pub on top-level entrypoint item")]
    fn rejects_pub_at_entrypoint_root() {
        run(r#"
            pub let f : Type = Type;
            Type
        "#);
    }

    #[test]
    #[should_panic(expected = "single-segment relative use is forbidden")]
    fn rejects_single_segment_relative_use() {
        run(r#"
            mod Foo
                let x : Type = Type;
            end
            use Foo;
            Type
        "#);
    }

    #[test]
    #[should_panic(expected = "unresolved qualifier")]
    fn rejects_unresolved_qualifier_in_term() {
        run("Foo/f");
    }

    #[test]
    #[should_panic(expected = "private binding")]
    fn rejects_private_binding_access() {
        run(r#"
            mod Foo
                let f : Type = Type;
            end
            Foo/f
        "#);
    }

    #[test]
    #[should_panic(expected = "private child module")]
    fn rejects_private_module_in_path() {
        run(r#"
            mod Foo
                mod Bar
                    pub let f : Type = Type;
                end
            end
            Foo/Bar/f
        "#);
    }

    #[test]
    #[should_panic(expected = "qualifier conflicts with existing scope entry")]
    fn rejects_conflicting_use_qualifiers() {
        run(r#"
            mod Foo
                pub mod Baz
                    pub let f : Type = Type;
                end
            end
            mod Bar
                pub mod Baz
                    pub let g : Type = Type;
                end
            end
            use Foo/Baz;
            use Bar/Baz;
            Type
        "#);
    }

    #[test]
    #[should_panic(expected = "child module not found")]
    fn rejects_use_of_nonexistent_child() {
        run(r#"
            mod Foo
            end
            use Foo/Nonexistent;
            Type
        "#);
    }

    #[test]
    #[should_panic(expected = "module not found")]
    fn rejects_absolute_use_of_nonexistent_module() {
        run(r#"
            use /Nonexistent;
            Type
        "#);
    }

    #[test]
    fn def_elaborates_to_sealed() {
        assert_eq!(
            run(r#"
                def Str(Bin)
                    pub let from : Bin -> Str = bin => Str.from bin;
                    pub let into : Str -> Bin = str => Str.into str;
                end
                Type
            "#),
            core::Sealed::new(
                "Str",
                core::Term::Prim(core::Prim::BinType),
                core::Let::new(
                    "Str/from",
                    core::FuncType::new(
                        "",
                        core::Term::Prim(core::Prim::BinType),
                        core::Var::free("Str")
                    ),
                    core::Func::new(
                        "bin",
                        core::Seal::new(core::Var::free("Str"), core::Var::free("bin"))
                    ),
                    core::Let::new(
                        "Str/into",
                        core::FuncType::new(
                            "",
                            core::Var::free("Str"),
                            core::Term::Prim(core::Prim::BinType)
                        ),
                        core::Func::new(
                            "str",
                            core::Unseal::new(core::Var::free("Str"), core::Var::free("str"))
                        ),
                        core::Term::Type,
                    ),
                ),
            )
            .into()
        );
    }

    #[test]
    #[should_panic(expected = "coercion outside def block")]
    fn rejects_coercion_outside_def_block() {
        run(r#"
            def Str(Bin)
            end
            Str.from 00
        "#);
    }

    #[test]
    fn def_inside_module_uses_qualified_name() {
        assert_eq!(
            run(r#"
                mod Foo
                    def Str(Bin)
                    end
                end
                Type
            "#),
            core::Sealed::new(
                "Foo/Str",
                core::Term::Prim(core::Prim::BinType),
                core::Term::Type
            )
            .into()
        );
    }

    #[test]
    fn pub_def_type_referenceable_by_qualified_name() {
        assert_eq!(
            run(r#"
                mod Foo
                    pub def Str(Bin)
                        pub let from : Bin -> Str = x => Str.from x;
                    end
                end
                Foo/Str
            "#),
            core::Sealed::new(
                "Foo/Str",
                core::Term::Prim(core::Prim::BinType),
                core::Let::new(
                    "Foo/Str/from",
                    core::FuncType::new(
                        "",
                        core::Term::Prim(core::Prim::BinType),
                        core::Var::free("Foo/Str")
                    ),
                    core::Func::new(
                        "x",
                        core::Seal::new(core::Var::free("Foo/Str"), core::Var::free("x"))
                    ),
                    core::Var::free("Foo/Str"),
                ),
            )
            .into()
        );
    }

    #[test]
    fn use_def_namespace_then_access_item() {
        assert_eq!(
            run(r#"
                mod Foo
                    pub def Str(Bin)
                        pub let from : Bin -> Str = x => Str.from x;
                    end
                end
                use Foo/Str;
                Str/from
            "#),
            core::Sealed::new(
                "Foo/Str",
                core::Term::Prim(core::Prim::BinType),
                core::Let::new(
                    "Foo/Str/from",
                    core::FuncType::new(
                        "",
                        core::Term::Prim(core::Prim::BinType),
                        core::Var::free("Foo/Str")
                    ),
                    core::Func::new(
                        "x",
                        core::Seal::new(core::Var::free("Foo/Str"), core::Var::free("x"))
                    ),
                    core::Var::free("Foo/Str/from"),
                ),
            )
            .into()
        );
    }

    #[test]
    #[should_panic(expected = "private binding")]
    fn rejects_private_def_type_by_qualified_name() {
        run(r#"
            mod Foo
                def Str(Bin)
                end
            end
            Foo/Str
        "#);
    }

    #[test]
    fn lambda_param_shadowing_def_name_captures_param_not_type() {
        assert_eq!(
            run(r#"
                def Str(Bin)
                    pub let foo : Str -> Bin = Str => Str.from Str;
                end
                Type
            "#),
            core::Sealed::new(
                "Str",
                core::Term::Prim(core::Prim::BinType),
                core::Let::new(
                    "Str/foo",
                    core::FuncType::new(
                        "",
                        core::Var::free("Str"),
                        core::Term::Prim(core::Prim::BinType)
                    ),
                    core::Func::new(
                        "Str",
                        core::Seal::new(core::Var::free("Str"), core::Var::free("Str"))
                    ),
                    core::Term::Type,
                ),
            )
            .into()
        );
    }

    #[test]
    fn lambda_param_shadows_def_in_nested_func() {
        assert_eq!(
            run(r#"
                def Str(Bin)
                    pub let foo : Str -> Str -> Bin = Str => str => Str.from str;
                end
                Type
            "#),
            core::Sealed::new(
                "Str",
                core::Term::Prim(core::Prim::BinType),
                core::Let::new(
                    "Str/foo",
                    core::FuncType::new(
                        "",
                        core::Var::free("Str"),
                        core::FuncType::new(
                            "",
                            core::Var::free("Str"),
                            core::Term::Prim(core::Prim::BinType)
                        ),
                    ),
                    core::Func::new(
                        "Str",
                        core::Func::new(
                            "str",
                            core::Seal::new(core::Var::free("Str"), core::Var::free("str"))
                        ),
                    ),
                    core::Term::Type,
                ),
            )
            .into()
        );
    }

    #[test]
    fn nested_def_outer_label_accessible_in_inner() {
        run(r#"
            def A(Bin)
                def B(Nat)
                    pub let f : Bin -> A = x => A.from x;
                end
            end
            Type
        "#);
    }

    #[test]
    #[should_panic(expected = "coercion outside def block: Foo")]
    fn rejects_coercion_with_wrong_def_label() {
        run(r#"
            def Str(Bin)
                pub let bad : Bin -> Str = x => Foo.from x;
            end
            Type
        "#);
    }

    #[test]
    fn pub_use_exposes_qualifier() {
        assert_eq!(
            run(r#"
                mod Foo
                    pub mod Bar
                        pub let f : Type = Type;
                    end
                end
                mod MyMod
                    pub use /Foo/Bar;
                end
                MyMod/Bar/f
            "#),
            core::Let::new(
                "Foo/Bar/f",
                core::Type,
                core::Type,
                core::Var::free("Foo/Bar/f")
            )
            .into(),
        );
    }

    #[test]
    #[should_panic(expected = "qualifier conflicts with existing scope entry: Bar")]
    fn rejects_mod_that_overwrites_prior_use() {
        run(r#"
            mod Foo
                pub mod Bar
                    pub let f : Type = Type;
                end
            end
            use Foo/Bar;
            mod Bar
            end
            Type
        "#);
    }

    #[test]
    #[should_panic(expected = "qualifier conflicts with existing scope entry: Bar")]
    fn rejects_def_that_overwrites_prior_use() {
        run(r#"
            mod Foo
                pub mod Bar
                    pub let f : Type = Type;
                end
            end
            use Foo/Bar;
            def Bar(Bin)
            end
            Type
        "#);
    }

    #[test]
    fn use_of_pub_use_path_resolves_through_alias() {
        assert_eq!(
            run(r#"
                mod Foo
                    pub mod Bar
                        pub let f : Type = Type;
                    end
                end
                mod MyMod
                    pub use /Foo/Bar;
                end
                use /MyMod/Bar;
                Bar/f
            "#),
            core::Let::new(
                "Foo/Bar/f",
                core::Type,
                core::Type,
                core::Var::free("Foo/Bar/f")
            )
            .into(),
        );
    }

    #[test]
    fn chained_pub_use_re_exports_transitively() {
        assert_eq!(
            run(r#"
                mod A
                    pub mod X
                        pub let f : Type = Type;
                    end
                end
                mod B
                    pub use /A/X;
                end
                mod C
                    pub use /B/X;
                end
                C/X/f
            "#),
            core::Let::new("A/X/f", core::Type, core::Type, core::Var::free("A/X/f")).into(),
        );
    }

    #[test]
    #[should_panic(expected = "child module not found")]
    fn private_use_does_not_expose_qualifier() {
        run(r#"
            mod Foo
                pub mod Bar
                    pub let f : Type = Type;
                end
            end
            mod MyMod
                use /Foo/Bar;
            end
            MyMod/Bar/f
        "#);
    }
}
