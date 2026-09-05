use crate::TermBuilders;
use {
    super::*,
    curios_core::{Global, Level, UniverseContext, instantiate_universe_levels_scoped},
};

fn instantiate_struct_decl(
    context: &mut Context,
    struct_decl: StructDecl,
    universes: Option<&[Level]>,
) -> Result<(StructDecl, Vec<Level>), Error> {
    let (arity, universes) = match universes {
        Some(universes) => (
            context.instantiate_universe_bound_at(
                &struct_decl.universe_context,
                &struct_decl.arity,
                universes,
            )?,
            universes.to_vec(),
        ),
        None => {
            context.instantiate_universe_bound(&struct_decl.universe_context, &struct_decl.arity)?
        }
    };
    let arguments = universes.clone();
    let result_sort = instantiate_universe_levels_scoped(&struct_decl.result_sort, &arguments)
        .map_err(Error::from)?;
    Ok((
        StructDecl {
            universe_context: UniverseContext::empty(),
            arity,
            result_sort,
            module: struct_decl.module,
            rep_public: struct_decl.rep_public,
            polarities: struct_decl.polarities,
        },
        universes,
    ))
}

/// Type a struct type against its registry entry: the parameters are checked pointwise (dependently) through the parameter telescope, and the whole node is a `Type`. The struct analogue of `elaborate_induct_type`, with no indices.
///
/// An *empty* parameter list on a parameterized struct is the inferred-head form (the type struct destructuring gives its temp): mint one fresh metavariable per declared parameter, exactly as the bare-name struct literal does (`elaborate_struct`), so the head can be solved by unification against the scrutinee's type.
pub(super) fn elaborate_struct_type(
    context: &mut Context,
    st: &StructType,
    term: &Term,
) -> Result<(Term, Term), Error> {
    let StructType {
        name,
        universes,
        params,
    } = st;

    let Some(struct_decl) = context.struct_decl(name).cloned() else {
        return Err(match context.assumption(&Free::from(name)) {
            Some(found) => Error::not_a_struct_type(found.clone()),
            None => Error::unknown_declaration(name.symbol()),
        });
    };
    let explicit_universes = (!universes.is_empty()).then_some(universes.as_slice());
    let (struct_decl, universes) =
        instantiate_struct_decl(context, struct_decl, explicit_universes)?;

    if params.is_empty() && struct_decl.param_count() != 0 {
        let mut resolved = Vec::with_capacity(struct_decl.param_count());
        let mut tele = struct_decl.arity.clone();
        while let Telescope::Cons(ty, rest) = tele {
            let binder = binder_name(rest.first_hint());
            let proposition = crate::is_prop(context, &ty).unwrap_or(false);
            let arg = context.fresh_metavar(
                ty.clone(),
                term.span(),
                ImplicitOrigin {
                    func: name.symbol(),
                    binder,
                },
                proposition,
            );
            tele = rest.open(&[&arg]);
            resolved.push(arg);
        }
        return Ok((
            Term::struct_type_at(name.clone(), universes, resolved),
            struct_decl.result_sort.clone(),
        ));
    }

    if params.len() != struct_decl.param_count() {
        return Err(Error::struct_arity_mismatch(
            name.symbol(),
            struct_decl.param_count(),
            params.len(),
        ));
    }

    let (elaborated, _fields) = check_args_against(context, struct_decl.arity, params)?;

    Ok((
        Term::struct_type_at(name.clone(), universes, elaborated),
        struct_decl.result_sort,
    ))
}

/// Where one field position's value comes from: a written term to check, or — for a concept's `use`-marked field with no written fill — a witness goal to mint at the position's instantiated type.
pub(super) enum FieldSource<'a> {
    Written(&'a Term),
    Resolve { func: String, binder: String },
}

/// Check each positional field against its type in a dependent field telescope, pushing the elaborated fields onto `elaborated`. Shared by struct and tuple literal elaboration. The rest of the telescope is opened with the *elaborated* field, not the raw surface term: the elaborated form carries label projections rebuilt positionally (and implicits inserted), whereas a raw `Field::Label` substituted into a later field type would panic once that type is reduced (e.g. `Async(b.A)` arising from a field typed `Async(A)` in `{ A : Type, t : Async(A) }`). A `Resolve` source mints a witness metavar plus an eagerly-attempted resolution goal — the `insert_auto_argument` pattern — anchored at `origin`, and the metavar threads the telescope like any elaborated field.
pub(super) fn check_dependent_fields(
    context: &mut Context,
    tele: Telescope<()>,
    sources: &[FieldSource],
    origin: &Term,
    elaborated: &mut Vec<Term>,
) -> Result<(), Error> {
    match tele {
        Telescope::Done(_) => Ok(()),
        Telescope::Cons(ty, rest) => {
            let head = match &sources[0] {
                FieldSource::Written(field) => check(context, field, ty)?,
                FieldSource::Resolve { func, binder } => {
                    let provenance = WitnessOrigin {
                        func: func.clone(),
                        binder: format!("its 'use' field '{binder}'"),
                    };
                    let (id, metavar) = context.fresh_witness_metavar(
                        ty.clone(),
                        origin.span(),
                        provenance.clone(),
                    );
                    attempt_witness_goal(context, id, &ty, provenance, origin)?;
                    metavar
                }
            };
            let rest = rest.open(&[&head]);
            elaborated.push(head);
            check_dependent_fields(context, rest, &sources[1..], origin, elaborated)
        }
    }
}

/// Type a struct literal against its registry entry. The struct's `name` makes it self-describing, so this synthesizes (like `elaborate_variant`, not the purely-checked `elaborate_tuple`): the parameters come from the written head — a bare-name head mints one fresh metavariable per parameter, solved by the field checks (and, in `Check` mode, the `expect` turnaround unifying the result `StructType` against the expected type) — and the fields are checked in declaration order through the (dependent) field telescope.
pub(super) fn elaborate_struct(
    context: &mut Context,
    s: &Struct,
    term: &Term,
    mode: &Mode,
) -> Result<(Term, Term), Error> {
    let Struct {
        name,
        universes: written_universes,
        params,
        fields,
        entries,
    } = s;

    let Some(struct_decl) = context.struct_decl(name).cloned() else {
        return Err(match context.assumption(&Free::from(name)) {
            Some(found) => Error::not_a_struct_type(found.clone()),
            None => Error::unknown_declaration(name.symbol()),
        });
    };
    let explicit_universes =
        (!written_universes.is_empty()).then_some(written_universes.as_slice());
    let (struct_decl, universes) =
        instantiate_struct_decl(context, struct_decl, explicit_universes)?;

    // Construction privacy: a private-representation struct may be built only within its declaring module's subtree. Checked here (alongside projection privacy in `elaborate_proj`) via `island`, set per item by `elaborate_module_suffix`.
    if !struct_decl.rep_public
        && context
            .island()
            .is_some_and(|island| !island.is_within(&struct_decl.module))
    {
        return Err(Error::private_representation(name.symbol()));
    }

    // A written-but-wrong parameter count is an error; an *empty* list is the bare-name head, which mints one fresh metavariable per parameter.
    if !params.is_empty() && params.len() != struct_decl.param_count() {
        return Err(Error::struct_arity_mismatch(
            name.symbol(),
            struct_decl.param_count(),
            params.len(),
        ));
    }

    // A `..base` spread takes its own path: the base is let-bound in a fresh frame and every unwritten position copies from it. At most one spread, and it must be the first entry.
    match entries
        .iter()
        .filter(|e| matches!(e, StructEntry::Spread))
        .count()
    {
        0 => {}
        1 if matches!(entries[0], StructEntry::Spread) => {
            return elaborate_struct_spread(context, &struct_decl, &universes, s, term, mode);
        }
        1 => return Err(Error::spread_not_first(name.symbol())),
        _ => return Err(Error::multiple_spreads(name.symbol())),
    }

    let resolved = resolve_struct_params(context, name, &struct_decl, params, term)?;
    seed_struct_expectation(context, name, &universes, &resolved, term, mode)?;

    // Instantiate the field telescope at the resolved parameters.
    let field_telescope = struct_decl.fields_at(&resolved);

    // A concept's `use`-marked (superclass) fields leave the positional field sequence, exactly like witness slots at call sites: plain written fields pair with the plain positions, explicit `use <term>` entries pair with the `use` positions in declaration order (no skipping), and every remaining `use` position becomes a witness-resolution goal. Note the check order is telescope order, not written order — the same model as call-site witness arguments.
    let use_positions: Vec<usize> = match context.concept(name) {
        Some(concept) => concept.supers.iter().map(|(index, _)| *index).collect(),
        None => Vec::new(),
    };
    debug_assert!(use_positions.windows(2).all(|w| w[0] < w[1]));

    // Partition the written entries; an empty entry list is all-plain-unlabeled (the internal normal form).
    let mut plain: Vec<(Option<&str>, &Term)> = Vec::new();
    let mut fills: Vec<&Term> = Vec::new();
    if entries.is_empty() {
        plain.extend(fields.iter().map(|field| (None, field)));
    } else {
        for (entry, field) in entries.iter().zip(fields) {
            match entry {
                StructEntry::Field(label) => plain.push((label.as_deref(), field)),
                StructEntry::Use => fills.push(field),
                StructEntry::Spread => unreachable!("a spread literal takes the spread path"),
            }
        }
    }

    if !fills.is_empty() && context.concept(name).is_none() {
        return Err(Error::use_entry_outside_concept(name.symbol()));
    }

    if fills.len() > use_positions.len() {
        return Err(Error::too_many_use_entries(
            name.symbol(),
            use_positions.len(),
            fills.len(),
        ));
    }

    // Superclass fields are anonymous, so no written label can target one: a labeled entry naming a former superclass is just an unknown field, caught by the positional validation below.
    let labels = field_telescope.labels();
    let plain_labels: Vec<&str> = labels
        .iter()
        .enumerate()
        .filter(|(position, _)| !use_positions.contains(position))
        .map(|(_, label)| *label)
        .collect();

    if plain.len() != plain_labels.len() {
        return Err(Error::wrong_number_of_fields(
            name.symbol(),
            plain_labels.len(),
            plain.len(),
        ));
    }

    // Written field names are checked positionally against the declared labels and then dropped — the rebuilt literal is name-free. Reordering is not supported: in a dependent telescope the written order is the check order.
    for (position, (written, _)) in plain.iter().enumerate() {
        let Some(written) = written else { continue };
        let declared = plain_labels.get(position).copied().unwrap_or_default();
        if declared != *written {
            return Err(Error::unknown_struct_field(
                name.symbol(),
                (*written).to_string(),
                plain_labels
                    .iter()
                    .filter(|l| !l.is_empty())
                    .map(|l| l.to_string())
                    .collect(),
            ));
        }
    }

    // Merge into one source per declared position: `use` positions consume the written fills first, then fall back to resolution; plain positions consume the plain values (counts validated above).
    let mut plain_values = plain.iter().map(|(_, field)| *field);
    let mut fill_values = fills.iter().copied();
    let mut sources = Vec::with_capacity(field_telescope.len());
    for position in 0..field_telescope.len() {
        if use_positions.contains(&position) {
            sources.push(match fill_values.next() {
                Some(fill) => FieldSource::Written(fill),
                // A `use` position is an anonymous superclass field; its minted internal label must never surface, so the goal's provenance names it `_` (the goal itself already shows the concept).
                None => FieldSource::Resolve {
                    func: name.symbol(),
                    binder: "_".to_string(),
                },
            });
        } else {
            let field = plain_values
                .next()
                .expect("plain field count was validated against the telescope");
            sources.push(FieldSource::Written(field));
        }
    }

    let mut elaborated = Vec::with_capacity(sources.len());
    check_dependent_fields(context, field_telescope, &sources, term, &mut elaborated)?;

    Ok((
        Term::struct_at(
            name.clone(),
            universes.clone(),
            resolved.clone(),
            elaborated,
        ),
        Term::struct_type_at(name.clone(), universes, resolved),
    ))
}

/// Resolve a struct literal's head parameters, threading the (dependent) parameter telescope so each minted metavariable is born at its binder's instantiated type: written arguments are checked, omitted ones minted fresh.
pub(super) fn resolve_struct_params(
    context: &mut Context,
    name: &Global,
    struct_decl: &StructDecl,
    params: &[Term],
    term: &Term,
) -> Result<Vec<Term>, Error> {
    let mut written = params.iter();
    let mut resolved = Vec::with_capacity(struct_decl.param_count());
    let mut tele = struct_decl.arity.clone();
    while let Telescope::Cons(ty, rest) = tele {
        let arg = match written.next() {
            Some(arg) => check(context, arg, ty.clone())?,
            None => {
                let binder = binder_name(rest.first_hint());
                let proposition = crate::is_prop(context, &ty).unwrap_or(false);
                context.fresh_metavar(
                    ty.clone(),
                    term.span(),
                    ImplicitOrigin {
                        func: name.to_string(),
                        binder,
                    },
                    proposition,
                )
            }
        };
        tele = rest.open(&[&arg]);
        resolved.push(arg);
    }
    Ok(resolved)
}

/// Seed omitted parameters from the checking expectation *before* the fields elaborate: a field checked against a type carrying an unsolved parameter metavariable can strand flex-flex constraints (e.g. a `match` tail's inferred motive against `Result(Str, {Nat, ?P})`) that nothing wakes. Only a same-named struct expectation seeds — anything else falls through to the dispatch-level `expect`, preserving implicit insertion and the ordinary mismatch diagnostics.
pub(super) fn seed_struct_expectation(
    context: &mut Context,
    name: &Global,
    universes: &[Level],
    resolved: &[Term],
    term: &Term,
    mode: &Mode,
) -> Result<(), Error> {
    if let Mode::Check(expected) = mode
        && let Subterm::StructType(StructType {
            name: expected_name,
            ..
        }) = Term::unwrap_or_clone(reduce_with(context, expected)?)
        && expected_name == name.clone()
    {
        let seeded = Term::struct_type_at(name.clone(), universes.to_vec(), resolved.to_vec());
        expect(context, term, &seeded, expected)?;
    }
    Ok(())
}

/// The `..base` spread path of a struct literal: the base is elaborated once and let-bound in a fresh frame, written overrides claim their declared positions by label — an order-preserving subsequence of the field telescope, so written order stays check order — explicit `use <term>` fills pair with the concept's `use`-marked positions as in the plain path, and every remaining position, plain and `use` alike, copies from the base by positional projection (a superclass field is *copied*, not re-resolved).
///
/// The parameters are minted *inside* the frame: an omitted parameter's metavariable may need to solve to a projection of the bound base (e.g. `?A := b.A`), which is only in scope there. The result type is reduced before the frame closes — the `elaborate_let` discipline — so occurrences of the binder unfold to the base before escaping the rebuilt `let b = base; Name { … }`, which downstream stages see as existing nodes.
pub(super) fn elaborate_struct_spread(
    context: &mut Context,
    struct_decl: &StructDecl,
    universes: &[Level],
    s: &Struct,
    term: &Term,
    mode: &Mode,
) -> Result<(Term, Term), Error> {
    let Struct {
        name,
        params,
        fields,
        entries,
        ..
    } = s;

    // The base must be a value of this very struct: positional projections would happily copy from a structurally-matching tuple or a same-shaped foreign struct otherwise. Its *parameters* may differ from the literal's — the parameter-changing update — since every copied field is checked against the new instantiated field type anyway.
    let (base, base_type) = elaborate(context, &fields[0], Mode::Infer)?;
    let base_type = reduce_with(context, &base_type)?;
    if !matches!(
        &*base_type,
        Subterm::StructType(StructType { name: base_name, .. }) if base_name == name
    ) {
        return Err(Error::spread_base_type_mismatch(
            name.symbol(),
            base_type.clone(),
        ));
    }

    let label = context.fresh(Some("base"));

    let (rebuilt, result_type) = context.with_frame(|context| {
        context.define_assuming(&label, &base_type, &base, None);

        let resolved = resolve_struct_params(context, name, struct_decl, params, term)?;
        seed_struct_expectation(context, name, universes, &resolved, term, mode)?;

        let field_telescope = struct_decl.fields_at(&resolved);

        let use_positions: Vec<usize> = match context.concept(name) {
            Some(concept) => concept.supers.iter().map(|(index, _)| *index).collect(),
            None => Vec::new(),
        };
        debug_assert!(use_positions.windows(2).all(|w| w[0] < w[1]));

        // Partition the overrides (everything after the spread). Positional values are ambiguous across the spread's gaps, so every plain override must be labeled.
        let mut plain: Vec<(&str, &Term)> = Vec::new();
        let mut fills: Vec<&Term> = Vec::new();
        for (entry, field) in entries[1..].iter().zip(&fields[1..]) {
            match entry {
                StructEntry::Field(Some(written)) => plain.push((written, field)),
                StructEntry::Field(None) => {
                    return Err(Error::unlabeled_spread_override(name.symbol()));
                }
                StructEntry::Use => fills.push(field),
                StructEntry::Spread => unreachable!("spread multiplicity was validated"),
            }
        }

        if !fills.is_empty() && context.concept(name).is_none() {
            return Err(Error::use_entry_outside_concept(name.symbol()));
        }

        if fills.len() > use_positions.len() {
            return Err(Error::too_many_use_entries(
                name.symbol(),
                use_positions.len(),
                fills.len(),
            ));
        }

        let labels = field_telescope.labels();
        let plain_positions: Vec<(usize, &str)> = labels
            .iter()
            .enumerate()
            .filter(|(position, _)| !use_positions.contains(position))
            .map(|(position, label)| (position, *label))
            .collect();

        // Overrides claim declared positions by label, as an order-preserving subsequence of the telescope: a label found ahead of the cursor claims its position; found only behind, it is repeated or out of order; found nowhere, it is unknown.
        let mut overrides: Vec<Option<&Term>> = vec![None; field_telescope.len()];
        let mut cursor = 0;
        for (written, field) in plain {
            let listed = || {
                plain_positions
                    .iter()
                    .filter(|(_, l)| !l.is_empty())
                    .map(|(_, l)| l.to_string())
                    .collect::<Vec<_>>()
            };
            match plain_positions[cursor..]
                .iter()
                .position(|(_, declared)| *declared == written)
            {
                Some(ahead) => {
                    let (position, _) = plain_positions[cursor + ahead];
                    overrides[position] = Some(field);
                    cursor += ahead + 1;
                }
                None if plain_positions[..cursor]
                    .iter()
                    .any(|(_, declared)| *declared == written) =>
                {
                    return Err(Error::spread_override_out_of_order(
                        name.symbol(),
                        written.to_string(),
                        listed(),
                    ));
                }
                None => {
                    return Err(Error::unknown_struct_field(
                        name.symbol(),
                        written.to_string(),
                        listed(),
                    ));
                }
            }
        }

        // Explicit `use` fills pair with the `use` positions in declaration order (no skipping), exactly as in the plain path.
        let mut fill_values = fills.iter().copied();
        for position in &use_positions {
            match fill_values.next() {
                Some(fill) => overrides[*position] = Some(fill),
                None => break,
            }
        }

        // One value per declared position: the override where written, a positional projection of the bound base everywhere else.
        let values: Vec<Term> = overrides
            .iter()
            .enumerate()
            .map(|(position, override_)| match override_ {
                Some(field) => (*field).clone(),
                None => Term::proj(Term::free_var(&label), position),
            })
            .collect();

        let sources: Vec<FieldSource> = values.iter().map(FieldSource::Written).collect();
        let mut elaborated = Vec::with_capacity(sources.len());
        check_dependent_fields(context, field_telescope, &sources, term, &mut elaborated)?;

        // Reduce inside the frame, where the binder is defined: occurrences of it in the result type unfold to the base before escaping the `let`.
        let result_type = reduce_with(
            context,
            &Term::struct_type_at(name.clone(), universes.to_vec(), resolved.clone()),
        )?;

        Ok::<_, Error>((
            Term::struct_at(name.clone(), universes.to_vec(), resolved, elaborated),
            result_type,
        ))
    })?;

    Ok((Term::let_(&label, base_type, base, rebuilt), result_type))
}
