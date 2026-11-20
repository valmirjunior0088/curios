//! One source declaration lowered, from a `let` to a `satisfy`: what the walk over a module body calls for each declaration it does not keep, with the context of the body it stands in and the lowering state.

use {
    super::{
        Context, FlatItem, FlatLet, FlatNominal, Lowerer, Lowering, SortSite, duplicate_field,
        telescope_label, witness_concept_application,
    },
    crate::Error,
    curios_abi::ForeignFunction,
    curios_common::Plicity,
    curios_surface::{
        LetSignature, StructLit, StructLitEntry, Subterm, Term, TopItem, TupleField,
        func_sugar_type_params,
    },
    std::{collections::BTreeSet, sync::Arc},
};

/// Lower `item`, a declaration of the module body `context` stands in, into `lowered`: its items, the registry entries it declares and the `foreign` row it registers.
pub(super) fn lower_declaration(
    item: &TopItem,
    context: &Context,
    lowered: &mut Lowering,
) -> Result<(), Error> {
    match item {
        TopItem::Broken(_) | TopItem::Mod(_) | TopItem::Use(_) => {
            unreachable!("a head stands where its item does")
        }
        // A `let` item is a group of one or more definitions. It lowers to a `rec` item when it is a declared group or when its one member names itself — read off the lowered terms, so a definition's own name is in scope of its type and body without anything said in the source — and to a plain `let` item otherwise. The certifier needs the distinction and the programmer does not: a `Rec` binds its members' names, a `Let` leaves a self-reference unbound.
        TopItem::Let(ls) => {
            let mut items = ls
                .iter()
                .map(|let_item| {
                    let name = curios_core::Global::Authored(context.prefixed(&let_item.label));
                    context.record_import_scope(Some(&name));
                    let lower = Lowerer::new(context, Some(name));
                    let signature = lower.signature(&let_item.signature);
                    let type_ = context.with_sorts(SortSite::Signature, || {
                        lower.signature_type(&signature, None)
                    })?;
                    Ok(FlatLet {
                        kind: curios_core::DefinitionKind::Authored,
                        span: let_item.label.span().cloned(),
                        name: curios_core::Global::Authored(context.prefixed(&let_item.label)),
                        island: context.island(),
                        type_,
                        body: lower.signature_value(&signature, |body| lower.value(body))?,
                    })
                })
                .collect::<Result<Vec<_>, Error>>()?;

            let recursive = items.len() > 1 || items.iter().any(|let_| let_.mentions_itself());
            lowered.items.push(match recursive {
                true => FlatItem::Rec(items),
                false => FlatItem::Let(items.pop().expect("a `let` item has a member")),
            });
        }
        // A test takes no parameters, so it is not function sugar — but it lowers to a `() -> Test({})` thunk, because `Test/main` holds the whole schedule and must force only the one it selected. The output is emitted as core directly off the registry slot, since a synthesized `Var` carries the resolved identity and nothing here depends on the declaration being importable.
        TopItem::Test(test) => {
            let name = curios_core::Global::Authored(context.prefixed(&test.label));
            context.record_import_scope(Some(&name));
            let lower = Lowerer::new(context, Some(name));
            let output = curios_core::Term::apply(
                curios_core::Term::written(curios_core::Global::Authored(
                    context.syntax().test.test_type.qualifier(),
                )),
                [curios_core::Term::tuple_type_unit()],
            );
            // The empty telescope is not vestigial: it is the thunk. A test lowers to `() -> Test({})` so `Test/main` can hold it unforced and run only the one it selected.
            let type_ = lower.func_type_under(&func_sugar_type_params(&[]), || Ok(output))?;
            let body = curios_core::Term::func_marked(
                Vec::<(Plicity, curios_core::Free, curios_core::Term)>::new(),
                lower.value(&test.body)?,
            );
            // The thunk owns the full authored body extent; its interior keeps each bang's narrower extent for diagnostics.
            let body = match test.body.span() {
                Some(span) => context.placed(span, body),
                None => body,
            };
            let name = curios_core::Global::Authored(context.prefixed(&test.label));
            lowered.items.push(FlatItem::Let(FlatLet {
                kind: curios_core::DefinitionKind::Test,
                span: test.label.span().cloned(),
                name,
                island: context.island(),
                type_,
                body,
            }));
        }
        TopItem::Foreign(f) => {
            // A `foreign` is the function of its row. The row is registered under the name the declaration binds, which is its import, and the declaration's type and body are both read off it.
            let path = context.prefixed(&f.label);
            let function = ForeignFunction {
                name: path.join(),
                signature: f.signature.clone(),
            };
            lowered.foreigns.register(function.clone());

            let name = curios_core::Global::Authored(path);
            context.record_import_scope(Some(&name));
            let lower = Lowerer::new(context, Some(name));
            let (type_, body) = lower.foreign(&Arc::new(function), name);
            lowered.items.push(FlatItem::Let(FlatLet {
                kind: curios_core::DefinitionKind::Authored,
                span: f.label.span().cloned(),
                name,
                island: context.island(),
                type_,
                body,
            }));
        }
        TopItem::Induct(group) => {
            // An inductive's lowering is its signature throughout: parameters, indices and constructor telescopes.
            context.sorts_stand_in(SortSite::Signature);
            // Each family's entry, recorded in the inductive registry; its former is made of it ([`curios_core::Made::former`]), wrapping the family's `InductType` normal form in a `Func` over its type parameters and indices, so `Result(Bytes, Nat)` beta-reduces to `InductType { Result, [Bytes, Nat] }` and `Vec(Bytes, 3)` to `InductType { Vec, [Bytes], [3] }`.
            let nominals = group
                .iter()
                .map(|u| {
                    let name = curios_core::Global::Authored(context.prefixed(&u.label));
                    context.record_import_scope(Some(&name));
                    let lower = Lowerer::new(context, Some(name));

                    // Parameters and indices are minted before any of their types is lowered, and each type sees the binders before it — a later index type naming an earlier parameter must mean *that* binder.
                    let head_binders = lower.mint_placed(
                        u.params
                            .iter()
                            .map(|param| telescope_label(param.plicity, param.label.as_ref()))
                            .chain(u.indices.iter().map(|(_, word, _)| {
                                telescope_label(Plicity::Explicit, word.as_ref())
                            })),
                    );
                    let (param_binders, index_binders) = head_binders.split_at(u.params.len());

                    let mut head_prefixes = lower.prefixes(&head_binders);
                    let param_tys = u
                        .params
                        .iter()
                        .enumerate()
                        .map(|(i, param)| {
                            let ty = head_prefixes.under(i, || lower.input_type(&param.type_))?;
                            Ok((param.plicity, param_binders[i].1, ty))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;

                    // The head's index telescope. An unnamed entry has no name, so no later index type depends on it.
                    let index_tys = u
                        .indices
                        .iter()
                        .enumerate()
                        .map(|(i, (_, _, t))| {
                            let seen = u.params.len() + i;
                            let ty = head_prefixes.under(seen, || lower.input_type(t))?;
                            Ok((index_binders[i].1, ty))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;
                    drop(head_prefixes);

                    // Registry entry: the parameter telescope plus each case's payload under the parameters, ending in that *case's* target expressions over its payload binders. `Telescope::build_marked` captures the payload labels in the payload types and the terminal, and the case's scope the parameters', mirroring `func_type`.
                    let constructors = u
                        .cases
                        .iter()
                        .map(|c| {
                            let payload_binders =
                                lower.mint_placed(c.payload.iter().map(|param| {
                                    telescope_label(param.plicity, param.label.as_ref())
                                }));
                            let scope = [param_binders, &payload_binders].concat();
                            let mut prefixes = lower.prefixes(&scope);
                            let fields = c
                                .payload
                                .iter()
                                .enumerate()
                                .map(|(i, param)| {
                                    let ty = prefixes.under(param_binders.len() + i, || {
                                        lower.input_type(&param.type_)
                                    })?;
                                    Ok((payload_binders[i].1, ty))
                                })
                                .collect::<Result<Vec<_>, Error>>()?;

                            let target = prefixes.under(scope.len(), || {
                                c.target
                                    .iter()
                                    .flatten()
                                    .map(|t| lower.term(t))
                                    .collect::<Result<Vec<_>, Error>>()
                            })?;
                            drop(prefixes);

                            // The payload terminates in the index targets alone: the family and its parameters are fixed by the declaration, so a terminal carries nothing else. Each payload keeps its declared mark, and the parameters are the arity's.
                            let payload = curios_core::Telescope::build_marked(
                                c.payload
                                    .iter()
                                    .zip(fields)
                                    .map(|(param, (binder, ty))| (param.plicity, binder, ty)),
                                target,
                            );
                            let params = param_binders.iter().map(|(_, id)| id).collect::<Vec<_>>();

                            Ok((
                                curios_core::Atom::from(c.label.as_str()),
                                curios_core::InductParam::over(&params, payload),
                            ))
                        })
                        // Collected in written order: a constructor's position here is the runtime tag `erase` gives it (`InductDecl::constructors`), so the sequence is the declaration's, not a collation of its labels.
                        .collect::<Result<Vec<_>, Error>>()?;

                    // The declared result sort (`Type`/`Prop`) — closed, so it lowers in the base context. It is the registry entry's sort, which its former's type ends in.
                    let result_sort = lower.term(&u.result_sort)?;

                    lowered.induct_decls.insert(
                        name,
                        // Positivity has not run yet: `curios-elab` computes each declaration's parameter polarities after elaboration and writes them back here.
                        curios_core::InductDecl::new(
                            curios_core::UniverseContext::empty(),
                            curios_core::Telescope::build_marked(
                                param_tys,
                                curios_core::Telescope::build(index_tys, ()),
                            ),
                            curios_core::Constructors::try_from(constructors)
                                .expect("discovery refuses a repeated case"),
                            result_sort,
                            context.island(),
                            u.rep_pub,
                        ),
                    );

                    // Its constructors are names its former declares beside itself, each written at its tag, which the family's entry makes and no definition holds.
                    let owner = context.prefixed(&u.label);
                    let members = u
                        .cases
                        .iter()
                        .map(|case| {
                            (
                                curios_core::Global::Authored(owner.with(case.label.as_str())),
                                case.label.span().cloned(),
                            )
                        })
                        .collect();
                    Ok(FlatNominal {
                        name,
                        entry: curios_core::Nominal::Inductive(lowered.induct_decls[&name].clone()),
                        span: u.label.span().cloned(),
                        members,
                        fields: Vec::new(),
                    })
                })
                .collect::<Result<Vec<_>, Error>>()?;

            lowered.items.push(FlatItem::Nominal(nominals));
        }
        // A struct lowers to its registry entry, which its former is made of — no value-constructor binding (the literal elaborates directly) and no indices.
        // A group of structures lowers to one nominal item, as an `induct` group does, so each member's fields may name the others.
        TopItem::Struct(group) => {
            // A structure's lowering is its signature throughout: parameters and fields.
            context.sorts_stand_in(SortSite::Signature);
            let mut nominals = Vec::with_capacity(group.len());
            for s in group {
                let name = curios_core::Global::Authored(context.prefixed(&s.label));
                context.record_import_scope(Some(&name));
                let lower = Lowerer::new(context, Some(name));

                // Declaring module: the type-former's qualifier prefix — identical to core's per-item `island` — for the representation-privacy checks.
                let module = context.prefixed(&s.label).without_last();

                let param_binders = lower.mint_placed(
                    s.params
                        .iter()
                        .map(|param| telescope_label(param.plicity, param.label.as_ref())),
                );
                let mut prefixes = lower.prefixes(&param_binders);
                let param_tys = s
                    .params
                    .iter()
                    .enumerate()
                    .map(|(i, param)| {
                        let ty = prefixes.under(i, || lower.input_type(&param.type_))?;
                        Ok((param.plicity, param_binders[i].1, ty))
                    })
                    .collect::<Result<Vec<_>, Error>>()?;
                drop(prefixes);

                // A repeated label is refused at the one that arrived second, as a repeated declaration is. Nothing else catches it: a structure's fields are not module declarations, so they never reach `insert_binding`'s check, and the telescope below would keep both — a literal would set them both and every `.label` read the first, silently. The tuple-type twin of this is `curios_elab`'s `DuplicateTupleLabel`, which a `struct` never reaches because its fields never elaborate as one.
                let mut declared = BTreeSet::new();
                for field in &s.fields {
                    if let Some(label) = &field.param.label
                        && !declared.insert(label.as_str())
                    {
                        return Err(duplicate_field(label));
                    }
                }

                // Field types, each under the fields before it, so a later field type can depend on an earlier field that has a name. The signature sugar `f(params) -> T` is undone here.
                let field_binders = lower.mint_placed(
                    s.fields
                        .iter()
                        .map(|field| telescope_label(field.plicity, field.param.label.as_ref())),
                );
                let field_scope = [&param_binders[..], &field_binders].concat();
                let mut prefixes = lower.prefixes(&field_scope);
                let field_tys = s
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(i, field)| {
                        let ty = prefixes.under(param_binders.len() + i, || {
                            lower.input_type(&field.param.desugared_type())
                        })?;
                        Ok((field.plicity, field_binders[i].1, ty))
                    })
                    .collect::<Result<Vec<_>, Error>>()?;
                drop(prefixes);

                // Registry entry: the parameter telescope, and the full field telescope (parameter binders first — field types may mention them — then field binders), as in `Inductive::indices`. The declared result sort (`Type`/`Prop`) — closed; the registry entry's sort, which its former's type ends in.
                let result_sort = lower.term(&s.result_sort)?;

                lowered.struct_decls.insert(
                    name,
                    curios_core::StructDecl {
                        universe_context: curios_core::UniverseContext::empty(),
                        arity: curios_core::Telescope::build_marked(
                            param_tys,
                            curios_core::Telescope::build_marked(field_tys, ()),
                        ),
                        result_sort,
                        module,
                        rep_public: s.rep_pub,
                        // Positivity has not run yet: `curios-elab` computes each declaration's parameter polarities after elaboration and writes them back here.
                        polarities: Vec::new(),
                        variances: Vec::new(),
                    },
                );

                // The entry its former, `Pair : (A : Type, B : Type) -> Type`, is made of. No value constructor.
                nominals.push(FlatNominal {
                    name,
                    entry: curios_core::Nominal::Struct(lowered.struct_decls[&name].clone()),
                    span: s.label.span().cloned(),
                    members: Vec::new(),
                    fields: s
                        .fields
                        .iter()
                        .filter_map(|field| {
                            let label = field.param.label.as_ref()?;
                            Some((label.to_string(), label.span().cloned()))
                        })
                        .collect(),
                });
            }
            lowered.items.push(FlatItem::Nominal(nominals));
        }
        // A concept lowers to a representation-public nominal `StructDecl`, which its former is made of, beside a concept-registry entry (field labels, superclass edges, the parameter telescope).
        // A concept group lowers to one nominal item as a struct group does; its methods are its fields, which no item declares.
        TopItem::Concept(group) => {
            context.sorts_stand_in(SortSite::Concept);
            let mut nominals = Vec::with_capacity(group.len());
            for concept in group {
                let name = curios_core::Global::Authored(context.prefixed(&concept.label));
                let module = context.prefixed(&concept.label).without_last();

                // A concept's premise is a superclass field, which every dictionary carries and resolution reaches through; a `use` parameter would instead make the premise's dictionary part of the concept's own identity, so two witnesses of one concept at one type could differ in nothing a key reads.
                if let Some(param) = concept
                    .params
                    .iter()
                    .find(|param| param.plicity == Plicity::Witness)
                {
                    let error = Error::ConceptUseParameter {
                        concept: concept.label.to_string(),
                    };
                    return Err(match param.type_.span() {
                        Some(span) => error.at(span.clone()),
                        None => error,
                    });
                }

                context.record_import_scope(Some(&name));
                let lower = Lowerer::new(context, Some(name));
                let param_binders = lower.mint_placed(
                    concept
                        .params
                        .iter()
                        .map(|param| telescope_label(param.plicity, param.label.as_ref())),
                );
                let mut prefixes = lower.prefixes(&param_binders);
                let param_tys = concept
                    .params
                    .iter()
                    .enumerate()
                    .map(|(i, param)| {
                        let ty = prefixes.under(i, || lower.input_type(&param.type_))?;
                        Ok((param.plicity, param_binders[i].1, ty))
                    })
                    .collect::<Result<Vec<_>, Error>>()?;
                drop(prefixes);

                // Field types, lowered under the parameter scope (a method field's label is the binder for later fields; a superclass field binds nothing). The signature sugar `f(params) -> T` is undone here. A superclass field is anonymous in a program: it has no label, so nothing projects it or names it in a later field's type, and resolution reaches it by index.
                let field_binders = lower.mint_placed(
                    concept
                        .fields
                        .iter()
                        .map(|field| telescope_label(field.plicity, field.label.as_ref())),
                );
                let field_scope = [&param_binders[..], &field_binders].concat();
                let mut prefixes = lower.prefixes(&field_scope);
                let field_tys = concept
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(i, field)| {
                        let ty = prefixes.under(param_binders.len() + i, || {
                            lower.input_type(&field.desugared_type())
                        })?;
                        // A superclass edge is the field telescope's `use` member.
                        let mark = match field.is_super() {
                            true => Plicity::Witness,
                            false => Plicity::Explicit,
                        };
                        Ok((mark, field_binders[i].1, ty))
                    })
                    .collect::<Result<Vec<_>, Error>>()?;
                drop(prefixes);

                let result_sort = lower.term(&concept.result_sort)?;

                // The record shape drives struct literals and projections, and a method's call is read off it ([`curios_core::method_spelling`]).
                let arity = curios_core::Telescope::build_marked(
                    param_tys,
                    curios_core::Telescope::build_marked(field_tys, ()),
                );
                lowered.struct_decls.insert(
                    name,
                    curios_core::StructDecl {
                        universe_context: curios_core::UniverseContext::empty(),
                        arity,
                        result_sort,
                        module,
                        rep_public: concept.rep_pub,
                        // Positivity has not run yet: `curios-elab` computes each declaration's parameter polarities after elaboration and writes them back here.
                        polarities: Vec::new(),
                        variances: Vec::new(),
                    },
                );

                lowered.concepts.insert(
                    name,
                    curios_core::ConceptDecl {
                        // What each `use` field reaches is read off its elaborated type, where the fields are elaborated.
                        supers: Vec::new(),
                    },
                );

                // Each labelled field is a method, reached by its path in the namespace the concept's head seeds for it, and declared by the former alone: a superclass edge has no label.
                let methods = concept
                    .fields
                    .iter()
                    .filter_map(|field| field.label.as_ref())
                    .map(|label| {
                        (
                            curios_core::Global::Authored(
                                context.prefixed(&concept.label).with(label),
                            ),
                            label.span().cloned(),
                        )
                    })
                    .collect();
                // The entry its former is made of, exactly like a representation-public struct's.
                nominals.push(FlatNominal {
                    name,
                    entry: curios_core::Nominal::Concept {
                        concept: lowered.concepts[&name].clone(),
                        record: lowered.struct_decls[&name].clone(),
                    },
                    span: concept.label.span().cloned(),
                    members: methods,
                    fields: Vec::new(),
                });
            }
            lowered.items.push(FlatItem::Nominal(nominals));
        }
        // A witness desugars to an anonymous top-level definition satisfy (tele) -> C(args) = C { f = e, … }; and marks it for registration in the program-wide witness table. It gets an *identity*, not a manufactured name: a `satisfy` block has no name a programmer wrote, and the module a diagnostic reports for it comes from `Definition::island`. The identity it is lowered under is its place in its module; the one it keeps is what its signature spells, read once every item is lowered (`name_witnesses`).
        // A group `satisfy … and …` lowers to one `rec` item, so its members' anonymous names are bound in one another; a lone witness stays a `let` item, and may still resolve through its own entry — that repair is elaboration's, since a witness references itself by resolution rather than by name.
        TopItem::Witness(group) => {
            context.sorts_stand_in(SortSite::Witness);
            let mut items = group
                .iter()
                .map(|witness| {
                    let name = curios_core::Global::Witness(context.fresh_witness());
                    context.record_import_scope(Some(&name));

                    let concept_app = witness_concept_application(
                        &witness.concept,
                        witness.args.as_deref(),
                        witness.head.as_ref(),
                    );
                    // A written body is the concept literal over its fields alone, so every `use`-marked position is left to resolution, under a bare head: the declared type states the arguments once, and the literal is checked against it. A derived one is the `Derive` transient, spanned at the concept application so a refusal lands on the declaration. Either way the telescope below wraps it identically.
                    let body: Term = match &witness.body {
                        Some(fields) => Subterm::StructLit(StructLit {
                            head: witness.concept.clone(),
                            params: None,
                            entries: fields
                                .iter()
                                .map(|field| {
                                    StructLitEntry::Field(TupleField {
                                        label: Some(field.label.clone()),
                                        func_params: field.func_params.clone(),
                                        value: field.value.clone(),
                                    })
                                })
                                .collect(),
                        })
                        .into(),
                        None => {
                            let derive: Term = Subterm::Derive.into();
                            match witness
                                .args
                                .iter()
                                .flatten()
                                .next()
                                .and_then(|arg| arg.term.span())
                            {
                                Some(span) => derive.with_span(span.clone()),
                                None => derive,
                            }
                        }
                    };

                    // Kept across the move into the signature: under a telescope the declared type is a `FuncType` built here, and a refusal about the *witness* — duplicate, orphan, unkeyable, irregular premise — belongs on the concept application it wraps rather than nowhere.
                    let declared = concept_app.span().cloned();
                    let signature = if witness.params.is_empty() {
                        LetSignature::Name {
                            type_: Some(concept_app),
                            body,
                        }
                    } else {
                        LetSignature::Func {
                            params: witness.params.clone(),
                            output: concept_app,
                            body,
                        }
                    };

                    let lower = Lowerer::new(context, Some(name));
                    let signature = lower.signature(&signature);
                    let item = FlatLet {
                        kind: curios_core::DefinitionKind::Witness,
                        span: declared.clone(),
                        name,
                        island: context.island(),
                        // Without a telescope the type is the application itself, which carries this span already.
                        type_: lower.signature_type(&signature, declared.as_ref())?,
                        body: lower.signature_value(&signature, |body| lower.value(body))?,
                    };
                    Ok(item)
                })
                .collect::<Result<Vec<_>, Error>>()?;

            lowered.items.push(match items.len() {
                1 => FlatItem::Let(items.pop().expect("a `satisfy` item has a member")),
                _ => FlatItem::Rec(items),
            });
        }
    }

    Ok(())
}
