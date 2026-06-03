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
                    type_: elab.term(&let_item.signature.type_())?,
                    body: elab.term(&let_item.signature.body())?,
                }));
            }
            TopItem::Rec(ls) => {
                let items = ls
                    .iter()
                    .map(|let_item| {
                        let elaborate = Elaborate::new(context);

                        Ok(FlatLet {
                            name: context.prefixed(&let_item.label),
                            type_: elaborate.term(&let_item.signature.type_())?,
                            body: elaborate.term(&let_item.signature.body())?,
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
mod tests;
