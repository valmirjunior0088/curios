mod context;
use context::*;

mod elaborate;
use elaborate::*;

use {super::*, crate::core, std::collections::HashMap};

fn scan_module_info(items: &[TopItem]) -> ModuleInfo {
    let mut info = ModuleInfo::new();

    for item in items {
        match item {
            TopItem::Mod(m) => info.insert_child(m.label.clone(), m.is_pub),
            TopItem::Use(u) if u.is_pub => info.insert_child(u.name.last().to_string(), true),
            TopItem::Let(l) => info.insert_binding(l.label.clone(), l.is_pub),
            TopItem::Def(d) => {
                info.insert_child(d.label.clone(), d.is_pub);
                info.insert_binding(d.label.clone(), d.is_pub);
            }
            TopItem::Rec(ls) => {
                for l in ls {
                    info.insert_binding(l.label.clone(), l.is_pub);
                }
            }
            _ => {}
        }
    }

    info
}

fn process_items(
    top_items: &[TopItem],
    context: &mut Context,
    flat_items: &mut Vec<FlatItem>,
    def_stack: &DefStack,
    loader: &dyn Loader,
) -> Result<(), Error> {
    context.finalize(scan_module_info(top_items));

    for top_item in top_items {
        match top_item {
            TopItem::Mod(m) => context.insert_scope(m.label.clone(), context.prefixed(&m.label)),
            TopItem::Def(d) => {
                context.insert_scope(d.label.clone(), context.prefixed(&d.label));
                context.insert_binding(d.label.clone(), context.prefixed(&d.label));
            }
            TopItem::Let(l) => context.insert_binding(l.label.clone(), context.prefixed(&l.label)),
            TopItem::Rec(labels) => {
                for l in labels {
                    context.insert_binding(l.label.clone(), context.prefixed(&l.label));
                }
            }
            _ => {}
        }
    }

    for top_item in top_items {
        match top_item {
            TopItem::Mod(mod_item) => match &mod_item.module {
                Some(module) => {
                    process_items(
                        &module.items,
                        &mut context.nested(&mod_item.label),
                        flat_items,
                        def_stack,
                        loader,
                    )?;
                }
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
                    )?;
                }
            },
            TopItem::Use(use_item) => {
                context.resolve_use(use_item);

                if use_item.is_pub {
                    let qualifier = use_item.name.last().to_string();
                    context.register_alias(&qualifier);
                }
            }
            TopItem::Let(let_item) => {
                let elab = Elaborate::new(
                    context.qualifiers(),
                    context.bindings(),
                    context.table(),
                    context.aliases(),
                    def_stack,
                );

                flat_items.push(FlatItem::Let(FlatLet {
                    name: context.prefixed(&let_item.label),
                    type_: elab.term(&let_item.type_)?,
                    body: elab.term(&let_item.body)?,
                }));
            }
            TopItem::Rec(ls) => {
                let items = ls
                    .iter()
                    .map(|let_item| {
                        let elaborate = Elaborate::new(
                            context.qualifiers(),
                            context.bindings(),
                            context.table(),
                            context.aliases(),
                            def_stack,
                        );

                        Ok(FlatLet {
                            name: context.prefixed(&let_item.label),
                            type_: elaborate.term(&let_item.type_)?,
                            body: elaborate.term(&let_item.body)?,
                        })
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                flat_items.push(FlatItem::Rec(items));
            }
            TopItem::Def(def_item) => {
                let name = context.prefixed(&def_item.label);

                let witness = Elaborate::new(
                    context.qualifiers(),
                    context.bindings(),
                    context.table(),
                    context.aliases(),
                    def_stack,
                )
                .term(&def_item.witness)?;

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
                )?;
            }
        }
    }

    Ok(())
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

pub fn to_core(entrypoint: &Entrypoint, loader: &dyn Loader) -> Result<core::Term, Error> {
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
    )?;

    let tail = Elaborate::new(
        context.qualifiers(),
        context.bindings(),
        context.table(),
        context.aliases(),
        &DefStack::empty(),
    )
    .term(&entrypoint.tail)?;

    Ok(flat_items.into_iter().rev().fold(tail, fold_flat_item))
}

#[cfg(test)]
mod tests {
    use crate::{core, text};

    fn run(src: &str) -> core::Term {
        super::to_core(
            &src.parse::<text::Entrypoint>().unwrap(),
            &text::PanicLoader,
        )
        .unwrap()
    }

    fn run_err(src: &str) -> String {
        super::to_core(
            &src.parse::<text::Entrypoint>().unwrap(),
            &text::PanicLoader,
        )
        .unwrap_err()
        .to_string()
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
    fn module_named_after_type_resolves_by_qualified_path() {
        assert_eq!(
            run(r#"
                mod Nat
                    pub let double : Type = Type;
                end
                Nat/double
            "#),
            core::Let::new(
                "Nat/double",
                core::Type,
                core::Type,
                core::Var::free("Nat/double")
            )
            .into(),
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
    fn allows_pub_on_root_items() {
        run(r#"
            pub mod Foo
                pub let f : Type = Type;
            end
            pub let g : Type = Type;
            pub def D(Bin)
            end
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
    fn rejects_unresolved_qualifier_in_term() {
        assert!(run_err("Foo/f").contains("unresolved qualifier"));
    }

    #[test]
    fn rejects_private_binding_access() {
        assert!(
            run_err(
                r#"
            mod Foo
                let f : Type = Type;
            end
            Foo/f
        "#
            )
            .contains("private binding")
        );
    }

    #[test]
    fn rejects_private_module_in_path() {
        assert!(
            run_err(
                r#"
            mod Foo
                mod Bar
                    pub let f : Type = Type;
                end
            end
            Foo/Bar/f
        "#
            )
            .contains("private child module")
        );
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
                        [("", core::Term::Prim(core::Prim::BinType))],
                        core::Var::free("Str")
                    ),
                    core::Func::new(
                        ["bin"],
                        core::Seal::new(core::Var::free("Str"), core::Var::free("bin"))
                    ),
                    core::Let::new(
                        "Str/into",
                        core::FuncType::new(
                            [("", core::Var::free("Str"))],
                            core::Term::Prim(core::Prim::BinType)
                        ),
                        core::Func::new(
                            ["str"],
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
    fn rejects_coercion_outside_def_block() {
        assert!(
            run_err(
                r#"
            def Str(Bin)
            end
            Str.from 00
        "#
            )
            .contains("coercion outside def block")
        );
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
                        [("", core::Term::Prim(core::Prim::BinType))],
                        core::Var::free("Foo/Str")
                    ),
                    core::Func::new(
                        ["x"],
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
                        [("", core::Term::Prim(core::Prim::BinType))],
                        core::Var::free("Foo/Str")
                    ),
                    core::Func::new(
                        ["x"],
                        core::Seal::new(core::Var::free("Foo/Str"), core::Var::free("x"))
                    ),
                    core::Var::free("Foo/Str/from"),
                ),
            )
            .into()
        );
    }

    #[test]
    fn rejects_private_def_type_by_qualified_name() {
        assert!(
            run_err(
                r#"
            mod Foo
                def Str(Bin)
                end
            end
            Foo/Str
        "#
            )
            .contains("private binding")
        );
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
                        [("", core::Var::free("Str"))],
                        core::Term::Prim(core::Prim::BinType)
                    ),
                    core::Func::new(
                        ["Str"],
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
                        [("", core::Var::free("Str"))],
                        core::FuncType::new(
                            [("", core::Var::free("Str"))],
                            core::Term::Prim(core::Prim::BinType)
                        ),
                    ),
                    core::Func::new(
                        ["Str"],
                        core::Func::new(
                            ["str"],
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
    fn rejects_coercion_with_wrong_def_label() {
        assert!(
            run_err(
                r#"
            def Str(Bin)
                pub let bad : Bin -> Str = x => Foo.from x;
            end
            Type
        "#
            )
            .contains("coercion outside def block: Foo")
        );
    }

    #[test]
    fn pub_use_exposes_qualifier() {
        assert_eq!(
            run(r#"
                pub mod Foo
                    pub mod Bar
                        pub let f : Type = Type;
                    end
                end
                pub mod MyMod
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
                pub mod Foo
                    pub mod Bar
                        pub let f : Type = Type;
                    end
                end
                pub mod MyMod
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
                pub mod A
                    pub mod X
                        pub let f : Type = Type;
                    end
                end
                pub mod B
                    pub use /A/X;
                end
                pub mod C
                    pub use /B/X;
                end
                C/X/f
            "#),
            core::Let::new("A/X/f", core::Type, core::Type, core::Var::free("A/X/f")).into(),
        );
    }

    #[test]
    #[should_panic(expected = "private child module")]
    fn rejects_private_root_module_via_absolute_path() {
        run(r#"
            mod Foo
                pub let f : Type = Type;
            end
            pub mod Bar
                use /Foo;
            end
            Type
        "#);
    }

    #[test]
    fn allows_pub_root_module_via_absolute_path() {
        run(r#"
            pub mod Foo
                pub let f : Type = Type;
            end
            pub mod Bar
                use /Foo;
            end
            Type
        "#);
    }

    #[test]
    fn private_use_does_not_expose_qualifier() {
        assert!(
            run_err(
                r#"
            pub mod Foo
                pub mod Bar
                    pub let f : Type = Type;
                end
            end
            pub mod MyMod
                use /Foo/Bar;
            end
            MyMod/Bar/f
        "#
            )
            .contains("child module not found")
        );
    }
}
