mod context;
use context::*;

mod elaborate;
use elaborate::*;

use {
    super::*,
    crate::core,
    std::collections::{BTreeMap, HashMap},
};

fn scan_module_info(items: &[TopItem]) -> ModuleInfo {
    let mut info = ModuleInfo::new();

    for item in items {
        match item {
            TopItem::Mod(m) => info.insert_child(m.label.clone(), m.is_pub),
            TopItem::Let(l) => info.insert_binding(l.label.clone(), l.is_pub),
            TopItem::Rec(ls) => {
                for l in ls {
                    info.insert_binding(l.label.clone(), l.is_pub);
                }
            }
            TopItem::Union(unions) => {
                for u in unions {
                    info.insert_child(u.label.clone(), u.is_pub);
                    info.insert_binding(u.label.clone(), u.is_pub);
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
    loader: &dyn Loader,
) -> Result<(), Error> {
    context.finalize(scan_module_info(top_items));

    for top_item in top_items {
        match top_item {
            TopItem::Mod(m) => context.insert_scope(m.label.clone(), context.prefixed(&m.label)),
            TopItem::Let(l) => context.insert_binding(l.label.clone(), context.prefixed(&l.label)),
            TopItem::Rec(labels) => {
                for l in labels {
                    context.insert_binding(l.label.clone(), context.prefixed(&l.label));
                }
            }
            TopItem::Union(unions) => {
                for u in unions {
                    context.insert_scope(u.label.clone(), context.prefixed(&u.label));
                    context.insert_binding(u.label.clone(), context.prefixed(&u.label));
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
                        loader,
                    )?;
                }
            },
            TopItem::Use(use_item) => {
                let imports: Vec<(String, UseResolved)> = match &use_item.group {
                    UseGroup::Named(items) => items
                        .iter()
                        .map(|item| {
                            let label = item.label().to_string();
                            let full = use_item.name.with(&label);

                            let resolved = match item {
                                GroupItem::Mod(_) => UseResolved {
                                    module: Some(context.resolve_module_use(&full)),
                                    binding: None,
                                },
                                GroupItem::Let(_) => UseResolved {
                                    module: None,
                                    binding: Some(context.resolve_binding_use(&full)),
                                },
                                GroupItem::Both(_) => context.resolve_both_use(&full),
                            };

                            (label, resolved)
                        })
                        .collect(),
                    UseGroup::Glob => context.resolve_glob(&use_item.name),
                };

                if use_item.is_pub {
                    for (label, resolved) in &imports {
                        if resolved.module.is_some() {
                            context.register_alias(label);
                            context.export_child(label.clone());
                        }

                        if resolved.binding.is_some() {
                            context.register_binding_alias(label);
                            context.export_binding(label.clone());
                        }
                    }
                }
            }
            TopItem::Let(let_item) => {
                let elab = Elaborate::new(context);

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
                        let elaborate = Elaborate::new(context);

                        Ok(FlatLet {
                            name: context.prefixed(&let_item.label),
                            type_: elaborate.term(&let_item.type_)?,
                            body: elaborate.term(&let_item.body)?,
                        })
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                flat_items.push(FlatItem::Rec(items));
            }
            TopItem::Union(unions) => {
                // Step 1: emit type bindings as a single rec group.
                let type_flat_items = unions
                    .iter()
                    .map(|u| {
                        let elab = Elaborate::new(context);

                        // Build atom type '[c_1, ..., c_m]
                        let atom_type: Term = Subterm::AtomType(AtomType {
                            atoms: u
                                .cases
                                .iter()
                                .map(|c| Atom::from(c.label.as_str()))
                                .collect(),
                        })
                        .into();

                        // For each case build payload tuple type.
                        let match_cases = u
                            .cases
                            .iter()
                            .map(|c| {
                                let payload: Term = Subterm::TupleType(TupleType {
                                    fields: c
                                        .payload_types
                                        .iter()
                                        .map(|t| (None, t.clone()))
                                        .collect::<Vec<_>>(),
                                })
                                .into();
                                (Atom::from(c.label.as_str()), payload)
                            })
                            .collect::<BTreeMap<_, _>>();

                        // match tag : Type | 'c_i => payload_i ...
                        let tag_match: Term = Subterm::Match(Match::Atom(AtomMatch {
                            head: Subterm::Name(Name::from(vec!["tag".to_string()])).into(),
                            motive: Motive {
                                label: None,
                                body: Subterm::Type.into(),
                            },
                            cases: match_cases,
                        }))
                        .into();

                        // { tag : '[...], <match> }
                        let tagged_tuple: Term = Subterm::TupleType(TupleType {
                            fields: vec![(Some("tag".to_string()), atom_type), (None, tag_match)],
                        })
                        .into();

                        // For parameterized unions wrap in a function.
                        let (type_ann, type_body): (Term, Term) = if u.params.is_empty() {
                            (Subterm::Type.into(), tagged_tuple)
                        } else {
                            let func_type: Term = Subterm::FuncType(FuncType {
                                params: u
                                    .params
                                    .iter()
                                    .map(|(n, t)| (Some(n.clone()), t.clone()))
                                    .collect(),
                                output: Subterm::Type.into(),
                            })
                            .into();
                            let func_body: Term = Subterm::Func(Func {
                                params: u.params.iter().map(|(n, _)| n.clone()).collect(),
                                body: tagged_tuple,
                            })
                            .into();
                            (func_type, func_body)
                        };

                        Ok(FlatLet {
                            name: context.prefixed(&u.label),
                            type_: elab.term(&type_ann)?,
                            body: elab.term(&type_body)?,
                        })
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                flat_items.push(FlatItem::Rec(type_flat_items));

                // Step 2 & 3: for each union register the constructor module and emit
                // constructor bindings.
                for u in unions {
                    // Register the constructor module's ModuleInfo so that
                    // `use Foo/T/{c}` and `use Foo/T/*` resolve correctly.
                    let mut ctor_info = ModuleInfo::new();
                    for c in &u.cases {
                        ctor_info.insert_binding(c.label.clone(), u.is_pub);
                    }
                    context.nested(&u.label).finalize(ctor_info);

                    // Build the output type term: T or T(A, B, ...).
                    let output_type: Term = if u.params.is_empty() {
                        Subterm::Name(Name::from(vec![u.label.clone()])).into()
                    } else {
                        Subterm::Apply(Apply {
                            head: Subterm::Name(Name::from(vec![u.label.clone()])).into(),
                            params: u
                                .params
                                .iter()
                                .map(|(n, _)| Subterm::Name(Name::from(vec![n.clone()])).into())
                                .collect(),
                        })
                        .into()
                    };

                    for c in &u.cases {
                        let k = c.payload_types.len();
                        let elab = Elaborate::new(context);

                        // Constructor type: (type_params..., _0 : T_0, ...) -> T
                        let all_params: Vec<(Option<String>, Term)> = u
                            .params
                            .iter()
                            .map(|(n, t)| (Some(n.clone()), t.clone()))
                            .chain(
                                c.payload_types
                                    .iter()
                                    .enumerate()
                                    .map(|(i, t)| (Some(format!("_{i}")), t.clone())),
                            )
                            .collect();

                        let ctor_type_term: Term = Subterm::FuncType(FuncType {
                            params: all_params,
                            output: output_type.clone(),
                        })
                        .into();

                        // Constructor body: (type_params..., _0, ...) => ('c, (_0, ...))
                        let all_param_names: Vec<String> = u
                            .params
                            .iter()
                            .map(|(n, _)| n.clone())
                            .chain((0..k).map(|i| format!("_{i}")))
                            .collect();

                        let payload_fields: Vec<Term> = (0..k)
                            .map(|i| Subterm::Name(Name::from(vec![format!("_{i}")])).into())
                            .collect();
                        let payload_tuple: Term = Subterm::Tuple(Tuple {
                            fields: payload_fields,
                        })
                        .into();

                        let ctor_body_term: Term = Subterm::Func(Func {
                            params: all_param_names,
                            body: Subterm::Tuple(Tuple {
                                fields: vec![
                                    Subterm::Atom(Atom::from(c.label.as_str())).into(),
                                    payload_tuple,
                                ],
                            })
                            .into(),
                        })
                        .into();

                        flat_items.push(FlatItem::Let(FlatLet {
                            name: context.prefixed(&u.label).with(&c.label),
                            type_: elab.term(&ctor_type_term)?,
                            body: elab.term(&ctor_body_term)?,
                        }));
                    }
                }
            }
        }
    }

    Ok(())
}

fn fold_flat_item(acc: core::Term, item: FlatItem) -> core::Term {
    match item {
        FlatItem::Let(let_) => core::Term::let_(let_.name.join(), let_.type_, let_.body, acc),
        FlatItem::Rec(items) => core::Term::rec(
            items
                .into_iter()
                .map(|item| (item.name.join(), item.type_, item.body)),
            acc,
        ),
    }
}

pub fn to_core(entrypoint: &Entrypoint, loader: &dyn Loader) -> Result<core::Term, Error> {
    let mut table = HashMap::new();
    let mut module_aliases = HashMap::new();
    let mut binding_aliases = HashMap::new();
    let mut context = Context::new(&mut table, &mut module_aliases, &mut binding_aliases);
    let mut flat_items = Vec::new();

    process_items(&entrypoint.items, &mut context, &mut flat_items, loader)?;

    let tail = Elaborate::new(&context).term(&entrypoint.tail)?;

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
        assert_eq!(run("Type"), core::Term::type_());
    }

    #[test]
    fn single_let_binding() {
        assert_eq!(
            run(r#"
                let x : Type = Type;
                x
            "#),
            core::Term::let_("x", core::Type, core::Type, core::Var::free("x")),
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
            core::Term::let_("Foo/f", core::Type, core::Type, core::Var::free("Foo/f")),
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
            core::Term::let_(
                "Nat/double",
                core::Type,
                core::Type,
                core::Var::free("Nat/double")
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
            core::Term::let_(
                "Foo/Bar/f",
                core::Type,
                core::Type,
                core::Var::free("Foo/Bar/f")
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
            use Foo/{Baz};
            use Bar/{Baz};
            Type
        "#);
    }

    #[test]
    #[should_panic(expected = "no module or binding named Nonexistent")]
    fn rejects_use_of_nonexistent_child() {
        run(r#"
            mod Foo
            end
            use Foo/{Nonexistent};
            Type
        "#);
    }

    #[test]
    #[should_panic(expected = "no module or binding named Nonexistent")]
    fn rejects_absolute_use_of_nonexistent_module() {
        run(r#"
            use /{Nonexistent};
            Type
        "#);
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
                    pub use /Foo/{Bar};
                end
                MyMod/Bar/f
            "#),
            core::Term::let_(
                "Foo/Bar/f",
                core::Type,
                core::Type,
                core::Var::free("Foo/Bar/f")
            ),
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
            use Foo/{Bar};
            mod Bar
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
                    pub use /Foo/{Bar};
                end
                use /MyMod/{Bar};
                Bar/f
            "#),
            core::Term::let_(
                "Foo/Bar/f",
                core::Type,
                core::Type,
                core::Var::free("Foo/Bar/f")
            ),
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
                    pub use /A/{X};
                end
                pub mod C
                    pub use /B/{X};
                end
                C/X/f
            "#),
            core::Term::let_("A/X/f", core::Type, core::Type, core::Var::free("A/X/f")),
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
                use /{Foo};
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
                use /{Foo};
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
                use /Foo/{Bar};
            end
            MyMod/Bar/f
        "#
            )
            .contains("child module not found")
        );
    }

    #[test]
    fn use_imports_binding_by_path() {
        run(r#"
            pub mod Foo
                pub let x : Type = Type;
            end
            use /Foo/{x};
            x
        "#);
    }

    #[test]
    #[should_panic(expected = "private binding: x")]
    fn rejects_use_of_private_binding() {
        run(r#"
            pub mod Foo
                let x : Type = Type;
            end
            use /Foo/{x};
            x
        "#);
    }

    #[test]
    fn pub_use_re_exports_binding() {
        run(r#"
            pub mod Foo
                pub let x : Type = Type;
            end
            pub mod Bar
                pub use /Foo/{x};
            end
            use /Bar/{x};
            x
        "#);
    }

    #[test]
    fn pub_use_binding_aliases_to_canonical_path() {
        run(r#"
            pub mod Foo
                pub let x : Type = Type;
            end
            pub mod Bar
                pub use /Foo/{x};
            end
            Bar/x
        "#);
    }

    #[test]
    #[should_panic(expected = "binding conflicts with existing scope entry: x")]
    fn rejects_use_followed_by_local_let_of_same_name() {
        run(r#"
            pub mod Foo
                pub let x : Type = Type;
            end
            use /Foo/{x};
            let x : Type = Type;
            x
        "#);
    }

    #[test]
    #[should_panic(expected = "binding conflicts with existing scope entry: x")]
    fn rejects_two_imports_of_same_name() {
        run(r#"
            pub mod Foo
                pub let x : Type = Type;
            end
            pub mod Bar
                pub let x : Type = Type;
            end
            use /Foo/{x};
            use /Bar/{x};
            x
        "#);
    }

    #[test]
    fn relative_use_imports_binding() {
        run(r#"
            pub mod Foo
                pub let x : Type = Type;
            end
            pub mod Bar
                use /{Foo};
                use Foo/{x};
                pub let y : Type = x;
            end
            Bar/y
        "#);
    }

    #[test]
    #[should_panic(expected = "no module or binding named nope")]
    fn rejects_use_of_unknown_item() {
        run(r#"
            pub mod Foo
                pub let x : Type = Type;
            end
            use /Foo/{nope};
            Type
        "#);
    }

    #[test]
    fn use_of_dual_existence_registers_both() {
        run(r#"
            pub mod Foo
                pub mod X
                    pub let y : Type = Type;
                end
                pub use X/{y};
            end
            pub mod Bar
                use /Foo/{y};
                pub let direct : Type = y;
                pub let via_path : Type = y;
            end
            Type
        "#);
    }

    #[test]
    fn dual_use_lets_bare_name_resolve_to_binding() {
        run(r#"
            pub mod Foo
                pub mod X
                    pub let y : Type = Type;
                end
                pub use X/{y};
            end
            use /Foo/{y};
            y
        "#);
    }

    #[test]
    fn dual_use_lets_path_resolve_through_module() {
        run(r#"
            pub mod Foo
                pub mod X
                    pub let y : Type = Type;
                    pub let q : Type = Type;
                end
                pub use X/{y};
            end
            Foo/y
        "#);
    }

    #[test]
    fn public_child_with_private_binding_imports_only_module() {
        run(r#"
            pub mod Foo
                pub mod X
                    pub let z : Type = Type;
                end
                use X/{z};
                let X : Type = z;
            end
            use /Foo/{X};
            X/z
        "#);
    }

    #[test]
    fn private_child_with_public_binding_imports_only_binding() {
        run(r#"
            pub mod Foo
                mod X
                    pub let z : Type = Type;
                end
                use X/{z};
                pub let X : Type = z;
            end
            use /Foo/{X};
            X
        "#);
    }

    #[test]
    fn use_of_dual_existence_from_outside_module() {
        run(r#"
            pub mod Foo
                pub mod X
                    pub let X : Type = Type;
                end
                pub use X/{X};
            end
            use /Foo/{X};
            X
        "#);
    }

    #[test]
    fn use_of_dual_existence_from_outside_qualifier_path() {
        run(r#"
            pub mod Foo
                pub mod X
                    pub let X : Type = Type;
                    pub let q : Type = Type;
                end
                pub use X/{X};
            end
            use /Foo/{X};
            X/q
        "#);
    }

    #[test]
    fn use_brace_group_imports_all_labels() {
        run(r#"
            pub mod Foo
                pub let x : Type = Type;
                pub let y : Type = Type;
            end
            use /Foo/{x, y};
            x
        "#);
        run(r#"
            pub mod Foo
                pub let x : Type = Type;
                pub let y : Type = Type;
            end
            use /Foo/{x, y};
            y
        "#);
    }

    #[test]
    #[should_panic(expected = "private child module: X")]
    fn rejects_use_when_both_sides_private() {
        run(r#"
            pub mod Foo
                mod X
                    pub let z : Type = Type;
                end
                use X/{z};
                let X : Type = z;
            end
            use /Foo/{X};
            Type
        "#);
    }

    #[test]
    fn use_glob_imports_all_public_bindings() {
        assert_eq!(
            run(r#"
                pub mod Foo
                    pub let x : Type = Type;
                    pub let y : Type = Type;
                end
                use /Foo/*;
                x
            "#),
            core::Term::let_(
                "Foo/x",
                core::Type,
                core::Type,
                core::Term::let_("Foo/y", core::Type, core::Type, core::Var::free("Foo/x"))
            ),
        );
    }

    #[test]
    fn use_glob_imports_child_modules_as_qualifiers() {
        run(r#"
            pub mod Foo
                pub mod Bar
                    pub let f : Type = Type;
                end
            end
            use /Foo/*;
            Bar/f
        "#);
    }

    #[test]
    fn use_glob_skips_private_child_modules() {
        assert!(
            run_err(
                r#"
            pub mod Foo
                mod Bar
                    pub let f : Type = Type;
                end
            end
            use /Foo/*;
            Bar/f
        "#
            )
            .contains("unresolved qualifier")
        );
    }

    #[test]
    fn relative_use_glob_imports_from_qualifier() {
        run(r#"
            pub mod Foo
                pub let x : Type = Type;
            end
            use Foo/*;
            x
        "#);
    }

    #[test]
    fn pub_use_glob_re_exports_all_public_labels() {
        run(r#"
            pub mod Foo
                pub let x : Type = Type;
                pub mod Bar
                    pub let f : Type = Type;
                end
            end
            pub mod Mine
                pub use /Foo/*;
            end
            use /Mine/{Bar};
            use /Mine/{x};
            Bar/f
        "#);
    }

    #[test]
    fn use_glob_on_dual_existence_imports_once() {
        run(r#"
            pub mod Foo
                pub mod X
                    pub let X : Type = Type;
                    pub let q : Type = Type;
                end
                pub use X/{X};
            end
            use /Foo/*;
            X/q
        "#);
    }
}
