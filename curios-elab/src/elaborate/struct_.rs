use {
    super::{Misaligned, align, binder_name, check_args_against, is_placeholder, premise_label},
    crate::{
        Context, Error, Mode, attempt_witness_goal, check, elaborate, expect, is_prop, reduce_with,
    },
    curios_core::{
        CalleeId, Free, Global, ImplicitOrigin, Level, Probe, Struct, StructDecl, StructEntry,
        StructType, Subterm, Telescope, Term, UniverseContext, WitnessOrigin,
        instantiate_universe_levels_scoped,
    },
    curios_utilities::Plicity,
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
            variances: struct_decl.variances,
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
        let mut cursor = struct_decl.arity.cursor();
        while let Some((hint, ty)) = cursor.entry() {
            let binder = binder_name(hint);
            let proposition = is_prop(context, &ty).probed()?.unwrap_or(false);
            let (_, arg) = context.fresh_metavar(
                ty,
                term.span(),
                ImplicitOrigin {
                    func: CalleeId::Function(Free::Global(*name)),
                    binder,
                },
                proposition,
                None,
            );
            cursor.advance(arg);
        }
        return Ok((
            Term::struct_type_at(*name, universes, cursor.into_args()),
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
        Term::struct_type_at(*name, universes, elaborated),
        struct_decl.result_sort,
    ))
}

/// Where one field position's value comes from: a written term to check, or — for a concept's `use`-marked field with no written fill — a witness goal to mint at the position's instantiated type.
pub(super) enum FieldSource<'a> {
    Written(&'a Term),
    /// An unfilled `use` position: a superclass edge the literal left to resolution. `edge` names the concept edged to rather than a field label, because the field is anonymous — there is no label to carry, and the edge is what a reader needs.
    Resolve {
        func: CalleeId,
        edge: String,
    },
}

/// Check each positional field against its type in a dependent field telescope, pushing the elaborated fields onto `elaborated`. Shared by struct and tuple literal elaboration. The rest of the telescope is opened with the *elaborated* field, not the raw surface term: the elaborated form carries label projections rebuilt positionally (and implicits inserted), whereas a raw `Field::Label` substituted into a later field type would panic once that type is reduced (e.g. `Async(b.A)` arising from a field typed `Async(A)` in `{ A : Type, t : Async(A) }`). A `Resolve` source mints a witness metavar plus an eagerly-attempted resolution goal — the `insert_auto_argument` pattern — anchored at `origin`, and the metavar threads the telescope like any elaborated field.
pub(super) fn check_dependent_fields(
    context: &mut Context,
    tele: Telescope<()>,
    sources: &[FieldSource],
    origin: &Term,
    elaborated: &mut Vec<Term>,
) -> Result<(), Error> {
    curios_profile::profile!("struct::check_dependent_fields");
    // One walk, each field type opened once at every field before it: reopening the rest after each field would rewrite every later type, whose metavariable spines name every earlier field, and make a long literal cubic.
    let (fields, ()) = tele.walk_producing(|index, _, ty| match &sources[index] {
        FieldSource::Written(field) => check(context, field, ty),
        FieldSource::Resolve { func, edge } => {
            let provenance = WitnessOrigin {
                func: func.clone(),
                binder: format!("its '{edge}' superclass"),
            };
            let (id, metavar) =
                context.fresh_witness_metavar(ty.clone(), origin.span(), provenance.clone());
            attempt_witness_goal(context, id, &ty, provenance, origin)?;
            Ok(metavar)
        }
    })?;

    elaborated.extend(fields);
    Ok(())
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

    // A concept's `use`-marked (superclass) fields are the hidden slots of its field telescope, and the entries meet them as a call's arguments meet a function's ([`align`]): the plain entries are the fields, in order, and before each the `use` entries written are the first of the edges that precede it, in order. An edge left out, or written `use _`, becomes a witness-resolution goal. The check order is telescope order.
    let slots = field_telescope.marks();
    // The concept each `use` position edges to is kept beside it rather than dropped: an unfilled position becomes a resolution goal, and that goal's provenance is the one place the superclass can be named as itself.
    let use_positions: Vec<(usize, Global)> = match context.concept(name) {
        Some(concept) => concept.edges(&field_telescope),
        None => Vec::new(),
    };

    // The written entries in order, each under its mark; an empty entry list is all-plain-unlabeled (the internal normal form).
    let written: Vec<(Plicity, Option<&str>, &Term)> = match entries.is_empty() {
        true => fields
            .iter()
            .map(|field| (Plicity::Explicit, None, field))
            .collect(),
        false => entries
            .iter()
            .zip(fields)
            .map(|(entry, field)| match entry {
                StructEntry::Field(label) => (Plicity::Explicit, label.as_deref(), field),
                StructEntry::Use => (Plicity::Witness, None, field),
                StructEntry::Spread => unreachable!("a spread literal takes the spread path"),
            })
            .collect(),
    };
    let plain: Vec<(Option<&str>, &Term)> = written
        .iter()
        .filter(|(mark, ..)| *mark == Plicity::Explicit)
        .map(|(_, label, field)| (*label, *field))
        .collect();

    if plain.len() != written.len() && context.concept(name).is_none() {
        return Err(Error::use_entry_outside_concept(name.symbol()));
    }

    // Superclass fields are anonymous, so no written label can target one: a labeled entry naming a superclass is just an unknown field, caught by the positional validation below.
    let labels = field_telescope.labels();
    let plain_labels: Vec<&str> = labels
        .iter()
        .enumerate()
        .filter(|(position, _)| slots[*position] == Plicity::Explicit)
        .map(|(_, label)| *label)
        .collect();

    if plain.len() != plain_labels.len() {
        // Where every written entry carries its label, the labels say which fields are absent and which are no field of the struct; a positional literal says only how many it wrote, so what it lacks is the declared tail.
        let written = plain
            .iter()
            .map(|(label, _)| *label)
            .collect::<Option<Vec<_>>>();
        let named = |labels: Vec<&str>| {
            labels
                .into_iter()
                .filter(|label| !label.is_empty())
                .map(str::to_string)
                .collect::<Vec<_>>()
        };
        let (missing, surplus) = match &written {
            Some(written) => (
                named(
                    plain_labels
                        .iter()
                        .copied()
                        .filter(|label| !written.contains(label))
                        .collect(),
                ),
                named(
                    written
                        .iter()
                        .copied()
                        .filter(|label| !plain_labels.contains(label))
                        .collect(),
                ),
            ),
            None => (
                named(plain_labels.iter().copied().skip(plain.len()).collect()),
                Vec::new(),
            ),
        };

        return Err(Error::wrong_number_of_fields(
            name.symbol(),
            plain_labels.len(),
            plain.len(),
            missing,
            surplus,
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

    // One source per declared position, by the alignment walk. The plain fields were counted against the telescope above, with the labels that report can name, and a literal's only hidden mark is `use`, so the one refusal left is a `use` entry with no edge in its run: written after the field the edges precede, or past the last of them.
    let marks = written.iter().map(|(mark, ..)| *mark).collect::<Vec<_>>();
    let fills = align(&slots, &marks).map_err(|misaligned| match misaligned {
        Misaligned::Surplus { member } => {
            Error::hidden_member_without_slot(Plicity::Witness).at_opt(written[member].2.span())
        }
        Misaligned::Plain | Misaligned::Mark { .. } => {
            unreachable!(
                "a literal's plain fields are counted, and its hidden entries are all `use`"
            )
        }
    })?;
    let sources = fills
        .iter()
        .zip(&slots)
        .enumerate()
        .map(|(position, (fill, slot))| {
            // An edge written `use _` holds its place and says nothing, so it is resolved as one left out is.
            let field = fill
                .map(|member| written[member].2)
                .filter(|field| *slot == Plicity::Explicit || !is_placeholder(context, field));
            match field {
                Some(field) => FieldSource::Written(field),
                // A `use` position is an anonymous superclass field, so the provenance names the concept it *edges to* rather than reaching for a label, which it does not have. The short name, since the goal's own line already carries the application it is wanted at.
                None => FieldSource::Resolve {
                    func: CalleeId::Function(Free::Global(*name)),
                    edge: use_positions
                        .iter()
                        .find(|(index, _)| *index == position)
                        .and_then(|(_, edge)| edge.qualifier())
                        .map(|path| path.last().to_string())
                        .unwrap_or_default(),
                },
            }
        })
        .collect::<Vec<_>>();

    let mut elaborated = Vec::with_capacity(sources.len());
    check_dependent_fields(context, field_telescope, &sources, term, &mut elaborated)?;

    Ok((
        Term::struct_at(*name, universes.clone(), resolved.clone(), elaborated),
        Term::struct_type_at(*name, universes, resolved),
    ))
}

/// Resolve a struct literal's head parameters, threading the (dependent) parameter telescope so each minted metavariable is born at its binder's instantiated type: written arguments are checked, omitted ones minted fresh.
///
/// An omitted `use` parameter is a witness slot, as it is where the type former is applied without it: resolution finds the dictionary once the parameters it is keyed on are known, and an expected type that already names one settles the slot by unification first.
pub(super) fn resolve_struct_params(
    context: &mut Context,
    name: &Global,
    struct_decl: &StructDecl,
    params: &[Term],
    term: &Term,
) -> Result<Vec<Term>, Error> {
    let mut written = params.iter();
    let mut premises = 0;
    let mut cursor = struct_decl.arity.cursor();
    while let Some((hint, ty)) = cursor.entry() {
        let premise = cursor.mark() == Some(Plicity::Witness);
        let arg = match written.next() {
            Some(arg) => check(context, arg, ty.clone())?,
            None if premise => {
                let provenance = WitnessOrigin {
                    func: CalleeId::Function(Free::Global(*name)),
                    binder: premise_label(premises),
                };
                let (id, metavar) =
                    context.fresh_witness_metavar(ty.clone(), term.span(), provenance.clone());
                attempt_witness_goal(context, id, &ty, provenance, term)?;
                metavar
            }
            None => {
                let binder = binder_name(hint);
                let proposition = is_prop(context, &ty).probed()?.unwrap_or(false);
                context
                    .fresh_metavar(
                        ty.clone(),
                        term.span(),
                        ImplicitOrigin {
                            func: CalleeId::Function(Free::Global(*name)),
                            binder,
                        },
                        proposition,
                        None,
                    )
                    .1
            }
        };
        premises += usize::from(premise);
        cursor.advance(arg);
    }
    Ok(cursor.into_args())
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
        && expected_name == *name
    {
        let seeded = Term::struct_type_at(*name, universes.to_vec(), resolved.to_vec());
        expect(context, term, &seeded, expected)?;
    }
    Ok(())
}

/// The `..base` spread path of a struct literal: the base is elaborated once and let-bound in a fresh frame, written overrides claim their declared positions by label — an order-preserving subsequence of the field telescope, so written order stays check order — a `use <term>` entry claims the next superclass edge after the entries before it, and every remaining position, plain and `use` alike, copies from the base by positional projection (a superclass field is *copied*, not re-resolved, and `use _` says so in writing).
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

    let label = context.fresh(None);

    let (rebuilt, result_type) = context.with_frame(|context| {
        context.define_assuming(&label, &base_type, &base, None);

        let resolved = resolve_struct_params(context, name, struct_decl, params, term)?;
        seed_struct_expectation(context, name, universes, &resolved, term, mode)?;

        let field_telescope = struct_decl.fields_at(&resolved);

        // The edges are the telescope's `use` fields; what one reaches is unused here, because a spread *copies* a superclass field from the base rather than re-resolving it.
        let slots = field_telescope.marks();

        if entries[1..]
            .iter()
            .any(|entry| matches!(entry, StructEntry::Use))
            && context.concept(name).is_none()
        {
            return Err(Error::use_entry_outside_concept(name.symbol()));
        }

        let labels = field_telescope.labels();
        let is_edge = |position: usize| slots[position] == Plicity::Witness;
        let listed = || {
            labels
                .iter()
                .enumerate()
                .filter(|(position, label)| !is_edge(*position) && !label.is_empty())
                .map(|(_, label)| label.to_string())
                .collect::<Vec<_>>()
        };

        // The entries after the spread, in written order, over one cursor: the first position an entry may still claim. A labeled override claims its field, found ahead of the cursor, so the written overrides are an order-preserving subsequence of the fields; positional values would be ambiguous across the spread's gaps, so every plain override is labeled. A `use` entry claims the next superclass edge at or after the cursor — the alignment rule ([`align`]) with the labeled overrides for the plain members written, so between two of them the edges are written from the first. An edge written `use _` is claimed and left as the spread leaves it.
        let mut overrides: Vec<Option<&Term>> = vec![None; field_telescope.len()];
        let mut cursor = 0;
        for (entry, field) in entries[1..].iter().zip(&fields[1..]) {
            match entry {
                StructEntry::Field(Some(written)) => {
                    let named = |position: &usize| {
                        !is_edge(*position) && labels[*position] == written.as_str()
                    };
                    match (cursor..labels.len()).find(named) {
                        Some(position) => {
                            overrides[position] = Some(field);
                            cursor = position + 1;
                        }
                        // Found only behind the cursor: repeated, out of order, or written after a `use` entry whose edge follows it.
                        None if (0..cursor).any(|position| named(&position)) => {
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
                StructEntry::Field(None) => {
                    return Err(Error::unlabeled_spread_override(name.symbol()));
                }
                StructEntry::Use => {
                    let Some(position) = (cursor..labels.len()).find(|position| is_edge(*position))
                    else {
                        return Err(Error::hidden_member_without_slot(Plicity::Witness)
                            .at_opt(field.span()));
                    };
                    if !is_placeholder(context, field) {
                        overrides[position] = Some(field);
                    }
                    cursor = position + 1;
                }
                StructEntry::Spread => unreachable!("spread multiplicity was validated"),
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
            &Term::struct_type_at(*name, universes.to_vec(), resolved.clone()),
        )?;

        Ok::<_, Error>((
            Term::struct_at(*name, universes.to_vec(), resolved, elaborated),
            result_type,
        ))
    })?;

    Ok((Term::let_(&label, base_type, base, rebuilt), result_type))
}
