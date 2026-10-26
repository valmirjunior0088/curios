mod demand;
use demand::*;

mod own_method;
use own_method::*;

mod recovery;
use recovery::*;

#[cfg(test)]
mod tests;

use {
    super::{Mode, elaborate, elaborate_func_type_sized},
    crate::{
        Context, Declared, ErasedSite, Error, Established, GoalReport, GoalSite, Published,
        Readers, ScheduledTest, Traces, Zonked, body_levels, check, check_concept_registry,
        check_is_sort, check_proof_totality, check_rec_item_totality, check_type_totality,
        check_written_type_totality, compared_levels, decide_polarities, definition_variances,
        field_variances, finalize_definition, goal_sites, hold_prop, hold_variances,
        parameter_field, record_definition_totality, record_superclasses, record_variances,
        reduce_with, register_witness, report_goals, result_sort_only_metas, sort_family_levels,
        stamp_totality, state_proof_binders, superclass_targets, test_program_tail,
        validate_entry_universes, validate_universes, zonk, zonk_arity, zonk_entry, zonk_published,
        zonk_solved_term_metas,
    },
    curios_analysis::group_totality,
    curios_core::{
        Advance, Bound, ConceptDecl, Declarations, Definition, Entrypoint, Free, FuncType, Global,
        InductDecl, InductParam, Intrinsic, Item, Items, Level, Minted, Module, Nominal, Order,
        Places, Quality, RecItem, Revised, SelfReference, StructDecl, Subterm, Telescope, Term,
        Totality, UniverseConstraintKind, UniverseConstraintOrigin, Universes,
        stamp_declaration_instance,
    },
    curios_utilities::{Candidates, Plicity, Qualifier, grown},
    std::{collections::BTreeSet, rc::Rc},
};

/// Record what a declaration's domains impose on its result sort, each at the level typing read it at, one level to each domain: the first `uniform_count` are parameters, one rung above the result's level, and the rest must fit under it.
///
/// **Typing's level, not the reduct's**, the level the kernel sizes a domain at, which types it as written: "a former sized as typing read its parts sits where the kernel types it" (`documentation/design/theory/a-universe-level-is-implicit-cumulative-and-settles-by-where-it-stands.md`). Read off a domain's reduct, an occurrence at a level its value drops — `/sys/List/List.{c}(Nat)`, which reduces to a node at `0`, or a type-valued definition's call — leaves the declaration's levels free to put the domain above its family, which the kernel refuses.
fn size_domains(
    context: &mut Context,
    declaration: &Global,
    domains: &[(Plicity, Free, Term)],
    levels: &[Level],
    uniform_count: usize,
    result_sort: &Term,
    kind: UniverseConstraintKind,
) -> Result<(), Error> {
    let Subterm::Sort(_, result_level) = &*reduce_with(context, result_sort)? else {
        return Ok(());
    };
    let result_level = result_level.clone();

    for (position, ((_, binder, domain), level)) in domains.iter().zip(levels).enumerate() {
        let upper = match position < uniform_count {
            true => result_level.checked_add(1).map_err(Error::from)?,
            false => result_level.clone(),
        };
        context
            .universes_mut()
            .add_leq(
                level.clone(),
                upper,
                UniverseConstraintOrigin {
                    extent: domain.extent(),
                    kind: kind.clone(),
                    declaration: Some(declaration.faithful().to_string()),
                    binder: binder.hint().map(str::to_string),
                    instantiated: None,
                },
            )
            .map_err(Error::from)?;
    }

    Ok(())
}

/// A telescope's members as a walk opened them: each under its mark, at the binder minted for it.
type Members = Vec<(Plicity, Free, Term)>;

/// Walk a (params-first) telescope, checking each binder's type against `Type` under the earlier binders (fresh-gensym, assume), and return the rebuilt `(mark, label, type)` entries, the level typing read each at ([`size_domains`]'s), and the telescope's terminal — opened under those binders. Runs in the caller's frame; the same gensym-then-relabel discipline as `elaborate_tuple_type`.
///
/// **Each member is entered under the mark its entry states.** A `use` entry joins the witness scope as well as the ordinary one, so resolution in the *later* entries' types finds it — which is what a function telescope does for a `use` premise, and what a concept's superclass edge needs to be visible to the field types below it ([`Context::enter`]).
fn check_telescope_entries<B: Bound>(
    context: &mut Context,
    telescope: Telescope<B>,
) -> Result<(Members, Vec<Level>, B), Error> {
    let mut entries = Vec::new();
    let mut levels = Vec::new();
    let mut cursor = telescope.cursor();
    while let Some((_, ty)) = cursor.entry() {
        let mark = cursor.mark().expect("a mark stands at an entry");
        let (rebuilt, sort) = check_is_sort(context, &ty)?;
        let label = cursor.advance_fresh(|hint| context.fresh(hint));
        context.enter(&label, &rebuilt, mark);
        entries.push((mark, label, rebuilt));
        levels.push(sort.level);
    }
    Ok((
        entries,
        levels,
        cursor.body().expect("a cursor past every entry"),
    ))
}

/// A struct former's written type, elaborated as [`check_is_sort`] elaborates a type, with the level typing read each of its parameters at handed back beside it, in order: what the struct's parameters are sized by ([`elaborate_struct`]). A former with no parameter is declared at its result sort and hands back none.
///
/// Its function type is walked by the walk every function type is, which reads each domain's sort, and around it this does what [`elaborate`] does around a term, as `elaborate_func_settle` does: the refusal placed at the term, the extent restamped, the settled node recorded. The elaboration cache is not consulted, a former's type being elaborated once.
fn check_former_type(context: &mut Context, type_: &Term) -> Result<(Term, Vec<Level>), Error> {
    let Subterm::FuncType(func_type) = &**type_ else {
        return Ok((check_is_sort(context, type_)?.0, Vec::new()));
    };
    let (rebuilt, sort, levels) =
        elaborate_func_type_sized(context, func_type).map_err(|error| {
            error
                .at_opt(type_.extent())
                .in_scope(context.reader_scope())
        })?;
    let rebuilt = match type_.extent() {
        Some(extent) => rebuilt.with_extent(extent),
        None => rebuilt,
    };
    context.record_checked(&rebuilt, &sort.term());
    Ok((rebuilt, levels))
}

/// Open an already-elaborated telescope: assume one fresh binder per entry and hand the entries back, without checking them again.
///
/// The counterpart to [`check_telescope_entries`] for a telescope that has been through elaboration once already. Re-checking such a telescope is not merely wasted work — it elaborates terms that carry universe instances a *second* time, and what a caller then files is a set of instances no earlier check ever agreed to.
fn assume_telescope_entries<B: Bound>(
    context: &mut Context,
    telescope: Telescope<B>,
) -> (Members, B) {
    let mut entries = Vec::new();
    let mut cursor = telescope.cursor();
    while let Some((_, ty)) = cursor.entry() {
        let mark = cursor.mark().expect("a mark stands at an entry");
        let label = cursor.advance_fresh(|hint| context.fresh(hint));
        context.enter(&label, &ty, mark);
        entries.push((mark, label, ty));
    }
    (entries, cursor.body().expect("a cursor past every entry"))
}

/// Rebuild a registry entry's `params`/`indices` telescopes with *elaborated* types. `into_core` records the declaration's lowered spellings, and a lowered type must never leak into later reduction: implicit insertion saturates applications during elaboration, and an index type lowered without the hidden arguments elaboration inserts (e.g. `Eq`'s `@A`, which its written application never states) would open a telescope at the wrong arity the first time `reduce` meets the registry copy.
///
/// Called from `elaborate_module_rec` after the group's signatures are reassumed rebuilt and *before* any body is checked — index types may mention the group's own members (resolved through the assumed signatures), and the type-constructor bodies' `InductType` nodes check their arguments against this very telescope. A name with no registry entry is an ordinary binding; no-op.
fn elaborate_induct_indices(context: &mut Context, name: &Global) -> Result<(), Error> {
    let Some(induct_decl) = context.induct_decl(name).cloned() else {
        return Ok(());
    };

    let owned = |labels: Vec<&str>| {
        labels
            .iter()
            .map(|label| label.to_string())
            .collect::<Vec<_>>()
    };
    let param_labels = owned(induct_decl.arity.labels());
    let index_labels = {
        let mut walk = &induct_decl.arity;
        loop {
            match walk {
                Telescope::Cons(_, _, rest, ..) => walk = rest.body(),
                Telescope::Done(indices) => break owned(indices.labels()),
            }
        }
    };

    // Walk the parameters, then the index telescope they terminate in, checking each entry type against `Type` under the binders before it.
    let (param_entries, index_entries) = context.with_frame(|context| {
        // A `use` parameter is in the witness scope of the index types below it, as it is in the type constructor's own function type.
        // A family's arity is typed and not sized, by either checker: its constructors' telescopes, which hold its parameters again, are.
        let (params, _, inner) = check_telescope_entries(context, induct_decl.arity.clone())?;
        let (indices, _, ()) = check_telescope_entries(context, inner)?;

        Ok::<_, Error>((params, indices))
    })?;

    let param_refs = param_labels.iter().map(String::as_str).collect::<Vec<_>>();
    let index_refs = index_labels.iter().map(String::as_str).collect::<Vec<_>>();
    let arity = Telescope::build_marked(
        param_entries,
        Telescope::build_marked(index_entries, ()).relabel(&index_refs),
    )
    .relabel(&param_refs);

    context.update_induct(
        name,
        InductDecl {
            universe_context: induct_decl.universe_context,
            arity,
            constructors: induct_decl.constructors,
            result_sort: induct_decl.result_sort,
            module: induct_decl.module,
            rep_public: induct_decl.rep_public,
            polarities: induct_decl.polarities,
            variances: induct_decl.variances,
        },
    );

    Ok(())
}

/// Rebuild a registry entry's constructor signatures with *elaborated* types — the second phase of the registry rebuild (see [`elaborate_induct_indices`]). Payload types may apply the inductive group's type constructors, so this runs from `elaborate_module_rec` only after the group's rebuilt bodies are defined; each terminal — the constructed `InductType` normal form — routes through `elaborate_induct_type`, which checks the parameters and the case's target indices against the already-rebuilt index telescope and returns another `InductType` node, the shape `case_target_indices` and the match elaborators rely on.
fn elaborate_induct_constructors(context: &mut Context, name: &Global) -> Result<(), Error> {
    let Some(induct_decl) = context.induct_decl(name).cloned() else {
        return Ok(());
    };

    let mut constructors = Vec::new();
    for (tag, param) in &induct_decl.constructors {
        let signature = &param.telescope;
        let labels = signature
            .labels()
            .iter()
            .map(|label| label.to_string())
            .collect::<Vec<_>>();

        let (entries, levels, targets) = context.with_frame(|context| {
            // Under the signature's own marks: a `use` parameter of the family is a witness slot of every constructor, in scope for the payload types after it.
            let (entries, levels, targets) = check_telescope_entries(context, signature.clone())?;

            // The targets are still checked by `elaborate_induct_type`, which is what compares them against the rebuilt index telescope — so the constructed type is rebuilt here from what the declaration fixes (this family, at the constructor's own parameter binders) and the targets the signature states, elaborated, and its indices taken back.
            let params = entries
                .iter()
                .take(induct_decl.param_count())
                .map(|(_, binder, _)| Term::free_var(binder))
                .collect::<Vec<_>>();
            let constructed = check_is_sort(context, &Term::induct_type(*name, params, targets))?.0;

            match &*constructed {
                Subterm::InductType(constructed) => {
                    Ok::<_, Error>((entries, levels, constructed.indices.clone()))
                }
                _ => Err(Error::UniverseInvariant(
                    "a constructor's terminal elaborates to its own family".into(),
                )),
            }
        })?;
        size_domains(
            context,
            name,
            &entries,
            &levels,
            induct_decl.param_count(),
            &induct_decl.result_sort,
            UniverseConstraintKind::ConstructorSizing,
        )?;

        let label_refs = labels.iter().map(String::as_str).collect::<Vec<_>>();
        constructors.push((
            tag.clone(),
            // Each entry is rebuilt under the mark it had: elaboration re-checks the types and never changes the calling convention.
            InductParam {
                telescope: Telescope::build_marked(entries, targets).relabel(&label_refs),
            },
        ));
    }

    context.update_induct(
        name,
        InductDecl {
            universe_context: induct_decl.universe_context,
            arity: induct_decl.arity,
            constructors: constructors.into(),
            result_sort: induct_decl.result_sort,
            module: induct_decl.module,
            rep_public: induct_decl.rep_public,
            polarities: induct_decl.polarities,
            variances: induct_decl.variances,
        },
    );

    Ok(())
}

/// Rebuild a struct's registry telescopes with *elaborated* types, so the field types `erase` and later construction sites consult are saturated (implicit insertion) and reduce correctly — the struct analogue of [`elaborate_induct_indices`], over the single (params-first) field telescope. Called from `elaborate_module_let` once the type-former is defined (field types may mention the struct itself and earlier items). A name with no registry entry is an ordinary binding; no-op.
///
/// `parameters` are the levels typing read the struct's parameters at, where its former's type was elaborated ([`check_former_type`]), one to each: the parameters are opened here rather than checked, so no check of their own reads them again.
fn elaborate_struct(
    context: &mut Context,
    name: &Global,
    parameters: &[Level],
) -> Result<(), Error> {
    let Some(struct_decl) = context.struct_decl(name).cloned() else {
        return Ok(());
    };

    // Obligation (T)'s early net, on the one written type position that rides on no definition's type. Every other declaration position reaches it through some `Definition`: a parameter through the type former's own type, an `induct` payload through its constructor wrapper's, a `concept` method through the method wrapper's. A field is projected rather than constructed, so no wrapper names it, so without this the field type below would be elaborated with nothing having looked at it — a productive `Shape(inf)` would unfold without end.
    //
    // Read off the *lowered* telescope, before `check_telescope_entries` touches it, which is the whole point of an early net. Entries stay closed under the binders before them; only globals matter here, so nothing needs opening.
    let mut written = &struct_decl.arity;
    let fields = loop {
        match written {
            Telescope::Cons(_, entry, rest, ..) => {
                check_written_type_totality(context, entry, ErasedSite::FieldOf(*name))?;
                written = rest.body();
            }
            Telescope::Done(fields) => break &**fields,
        }
    };
    let mut written = fields;
    while let Telescope::Cons(_, entry, rest, ..) = written {
        check_written_type_totality(context, entry, ErasedSite::FieldOf(*name))?;
        written = rest.body();
    }

    let owned = |labels: Vec<&str>| {
        labels
            .iter()
            .map(|label| label.to_string())
            .collect::<Vec<_>>()
    };
    let param_labels = owned(struct_decl.arity.labels());
    let field_labels = owned(struct_decl.fields().labels());

    let declared_prop = matches!(
        &*reduce_with(context, &struct_decl.result_sort)?,
        Subterm::Sort(Quality::Prop, _)
    );

    // Open the parameters, then check the field telescope they terminate in against `Type` under the binders before it.
    //
    // The parameters are opened rather than checked: `share_struct_params` elaborated them before the former's body was, and they are the terms that body was checked against. Elaborating them again would file a second set of universe instances beside the ones already in play.
    let (param_entries, field_entries, field_levels, supers) =
        context.with_frame(|context| -> Result<_, Error> {
            // Under their declared marks, so a `use` parameter is in the witness scope of the fields, as it is in the former's own function type, and a concept's superclass edge in that of the fields below it.
            let (params, inner) = assume_telescope_entries(context, struct_decl.arity.clone());
            let (fields, field_levels, ()) = check_telescope_entries(context, inner)?;

            // Soundness of a `Prop`-sorted struct: a `Prop` is governed by proof irrelevance, yet projection is an *unguarded* eliminator — it reads a field out of a value the theory believes is interchangeable with any other. That is consistent only when every field is a proof, so that every projection lands in a `Prop`, the side irrelevance governs; it is checked here at declaration time rather than per projection, so `elaborate_proj` needs no guard.
            if declared_prop {
                for ((_, _, ty), label) in fields.iter().zip(&field_labels) {
                    if !hold_prop(context, ty, ty.extent())? {
                        return Err(Error::informative_prop_struct(
                            *name,
                            label.clone(),
                            ty.clone(),
                        ));
                    }
                }
            }

            // What each superclass edge of a concept reaches, read while the parameters its type may name are in scope.
            let supers = match context.concept(name).is_some() {
                true => {
                    let edges = fields
                        .iter()
                        .filter(|(mark, ..)| *mark == Plicity::Witness)
                        .map(|(_, _, ty)| ty.clone())
                        .collect::<Vec<_>>();
                    Some(superclass_targets(context, name, &edges)?)
                }
                false => None,
            };

            Ok((params, fields, field_levels, supers))
        })?;

    assert_eq!(
        parameters.len(),
        param_entries.len(),
        "a struct's former's type is a function type over its parameters, one domain to each"
    );
    let domains = param_entries
        .iter()
        .chain(&field_entries)
        .cloned()
        .collect::<Vec<_>>();
    let levels = parameters
        .iter()
        .chain(&field_levels)
        .cloned()
        .collect::<Vec<_>>();
    size_domains(
        context,
        name,
        &domains,
        &levels,
        struct_decl.param_count(),
        &struct_decl.result_sort,
        UniverseConstraintKind::FieldSizing,
    )?;

    let param_refs = param_labels.iter().map(String::as_str).collect::<Vec<_>>();
    let field_refs = field_labels.iter().map(String::as_str).collect::<Vec<_>>();
    let arity = Telescope::build_marked(
        param_entries,
        Telescope::build_marked(field_entries, ()).relabel(&field_refs),
    )
    .relabel(&param_refs);

    context.update_struct(
        name,
        StructDecl {
            universe_context: struct_decl.universe_context,
            arity,
            result_sort: struct_decl.result_sort,
            module: struct_decl.module,
            rep_public: struct_decl.rep_public,
            polarities: struct_decl.polarities,
            variances: struct_decl.variances,
        },
    );
    if let Some(supers) = supers {
        record_superclasses(context, name, supers)?;
    }

    Ok(())
}

/// Replace a struct's registry parameter telescope with the one its type-former's declared type already carries, before that former's body is checked.
///
/// The lowerer files a `StructDecl` whose arity is the *written* telescope, untouched by elaboration (`curios-text/src/into_core.rs`, `universe_context: empty()`). The former's body is the `StructType` node over its own parameters, and `elaborate_struct_type` checks those parameters against that arity — against terms carrying no universe instances, while the binders the body meets come from `type_`, which was just elaborated and does carry them.
///
/// The two can never be reconciled, and not for want of trying: [`Frames::var_reduct`](crate::context) refuses to unfold a universe-polymorphic global reached through a bare `Var`, because there is no instance to substitute into its body. So the raw side is irreducible by construction, and conversion is being handed a problem no reduction could decide. Elaborating the telescope a second time here would only mint a *third* set of instances; taking the one the former's type already holds makes the comparison compare a term with itself.
///
/// Parameters only. A field type may mention the struct itself, so the field telescope stays with [`elaborate_struct`], which runs once the former is defined; a parameter type cannot, since the struct is not yet a type where its own parameters are written.
fn share_struct_params(context: &mut Context, name: &Global, type_: &Term) {
    let Some(struct_decl) = context.struct_decl(name).cloned() else {
        return;
    };
    let count = struct_decl.param_count();
    // A nullary struct has no parameters to share, and its former is declared at the result sort rather than at a function type.
    if count == 0 {
        return;
    }

    // Both invariants below belong to the lowerer, and both are stated rather than tolerated because `elaborate_struct` *opens* this telescope instead of re-checking it: a silent return would leave the raw arity in place with nothing downstream left to notice.
    let Subterm::FuncType(FuncType { telescope, .. }) = &**type_ else {
        unreachable!("a parameterized struct's type-former is declared at a function type");
    };

    let mut written = telescope.cursor();
    let mut lowered = struct_decl.arity.cursor();
    let mut params = Vec::with_capacity(count);
    for _ in 0..count {
        let (Some((_, domain)), Some((hint, _))) = (written.entry(), lowered.entry()) else {
            unreachable!(
                "the former's telescope and the registry arity lower from one parameter list"
            );
        };
        // Under the mark the former's own type states, which is the registry arity's too: a `use` parameter is in the witness scope of the fields below it.
        let mark = written.mark().expect("an entry stands at the cursor");
        // One binder opens both sides, so a later domain and the field telescope below refer to the same one.
        let binder = context.fresh(hint);
        let occurrence = Term::free_var(&binder);
        written.advance(occurrence.clone());
        lowered.advance(occurrence);
        params.push((mark, binder, domain));
    }

    let fields = lowered
        .body()
        .expect("the arity's parameters are exactly `param_count` entries");

    context.update_struct(
        name,
        StructDecl {
            universe_context: struct_decl.universe_context,
            arity: Telescope::build_marked(params, fields),
            result_sort: struct_decl.result_sort,
            module: struct_decl.module,
            rep_public: struct_decl.rep_public,
            polarities: struct_decl.polarities,
            variances: struct_decl.variances,
        },
    );
}

/// Type-check a single non-recursive top-level definition, `define` it into the *current* (persistent base) frame, and return its rebuilt form. The flat analogue of `elaborate_let`'s per-binding work, minus the `with_frame`/tail recursion: the binding must stay in scope for every later item and the entrypoint body. The *rebuilt* body is `define`d (implicit insertion makes the lowered one no longer interchangeable; see the comment below), and the rebuilt `Definition` flows on to `zonk`/`erase`.
fn elaborate_module_let(context: &mut Context, def: &Definition) -> Result<Item, Error> {
    // Γ and the definition store key on the free-variable identity; the nominal registries key on the definition's own name.
    let name = Free::from(&def.name);
    let outer_site = context.set_checked_site(def.name);
    check_written_type_totality(context, &def.type_, ErasedSite::TypeOf(def.name))?;
    // A struct's former is typed over its parameters, which are sized at the levels that typing reads ([`elaborate_struct`]).
    let (type_, parameters) = match context.struct_decl(&def.name).is_some() {
        true => check_former_type(context, &def.type_)?,
        false => (check_is_sort(context, &def.type_)?.0, Vec::new()),
    };

    // A witness declaration registers into the program-wide table as soon as its signature is known — *before* its body elaborates, so a recursive witness (a `Show(Tree)` whose fields show subtrees) can resolve through its own entry.
    if context.is_witness_declaration(&def.name) {
        register_witness(
            context,
            &def.name,
            &type_,
            def.universe_context.clone(),
            &def.island,
        )
        .map_err(|error| error.at_opt(def.type_.extent()))?;
    }

    // Before the body: a struct former's body checks its parameters against the registry arity, which is lowered raw. Share the elaborated telescope the type above just produced, so the two sides are one term.
    share_struct_params(context, &def.name, &type_);

    context.enter_value(&def.kind);
    let body = check(context, &def.body, type_.clone())?;
    context.sweep_parked()?;
    // A witness that reads one of its own methods reads that method's term, so its body names itself only where a method reaches itself.
    let body = match context.is_witness_declaration(&def.name) {
        true => own_methods_read(context, &name, &zonk_solved_term_metas(context, &body)),
        false => body,
    };

    // Define the *rebuilt* body at the *rebuilt* type, not the lowered ones: implicit-argument insertion saturates applications during elaboration, and the untyped reducer (type-level evaluation in later items' types) would meet a lowered form's under-applied calls and open a telescope at the wrong arity.
    context.define_assuming(&name, &type_, &body, Some(&def.kind));

    // A struct's type-former lowers to a standalone `let`; rebuild its registry telescopes now that the former is defined (no-op for an ordinary let).
    elaborate_struct(context, &def.name, &parameters)?;

    // A witness may resolve through its own table entry — the registration above exists to let it — and a body that does names itself. Cross-definition value recursion is unexpressible outside a `rec` item by construction, so such a witness becomes a single-member group below, and its self-references are bound by that group's binder rather than free.
    let recursive_witness = context.is_witness_declaration(&def.name) && {
        // Read the *zonked* body: the self-reference arrives as a witness metavariable that resolution solved, so an unzonked body does not mention the name yet. `finalize_definition` zonks again, which costs nothing and keeps that its own business.
        zonk_solved_term_metas(context, &body).mentions_free(&name)
    };
    let self_reference = match recursive_witness {
        true => SelfReference::Bound,
        false => SelfReference::Free,
    };

    let (universe_context, type_, body) =
        finalize_definition(context, &def.name, &def.kind, type_, body, self_reference)?;
    context.reassume(&name, &type_);
    context.define(&name, &body, Some(&def.kind));
    context.set_assumption_universe_context(&name, universe_context.clone());
    if context.is_witness_declaration(&def.name) {
        context.update_witness_scheme(&def.name, universe_context.clone(), type_.clone());
    }
    // A lone `struct` lowers to this `let`: its variance is settled as a group's is, for the later items that compare two of its instances. A name with no registry entry records nothing.
    record_variances(context, &[def.name])?;
    // After a struct's own vector, which its former's body is read by. A witness that names itself becomes a group of one below, and a member carries none.
    let (variances, field_variances, parameter_field) = match recursive_witness {
        true => (Vec::new(), Vec::new(), None),
        false => (
            definition_variances(context, &universe_context, &type_, &body)?,
            field_variances(&universe_context, &type_, &body),
            parameter_field(&type_, &body),
        ),
    };

    context.restore_checked_site(outer_site);

    let definition = Definition {
        name: def.name,
        kind: def.kind.clone(),
        universe_context,
        island: def.island,
        totality: def.totality,
        proof: def.proof,
        variances,
        field_variances,
        parameter_field,
        // Stated where the item is published, over its final terms.
        value_sort: None,
        type_,
        body,
    };

    if !recursive_witness {
        // Classify now, so a later item's *written* type can be refused before it is reduced. A `let` is its own group, hence total as far as the group verdict is concerned.
        record_definition_totality(context, &definition, Totality::Total)?;
        return Ok(Item::Let(Box::new(definition)));
    }

    // The witness is a recursive group of one. Its members' occurrences were stamped `Bound` above, which is what `RecItem::try_new` then captures; the context binding moves from the self-mentioning body to the group's own projection, so a later item reducing this witness meets the fold rather than a free name the kernel would refuse.
    let rec = RecItem::try_new(vec![definition]).map_err(Error::from)?;
    let member = Term::rec_proj(rec.group.clone(), 0);
    context.define(&name, &member, Some(&def.kind));

    // Classified as the group it now is, not assumed total the way a `let` may be: a witness that evaluates itself descends on nothing, and the erasure obligations are entitled to know.
    let verdict = group_totality(context, &rec.group)?;
    for definition in rec.definitions() {
        record_definition_totality(context, &definition, verdict)?;
    }

    Ok(Item::Rec(rec))
}

/// Type-check a flat top-level `rec` item and return its rebuilt structural group. The input is opened over the export names for elaboration; the output captures those names back into one [`RecItem`] and publishes each export as its folded structural member.
fn elaborate_module_rec(context: &mut Context, rec: &RecItem) -> Result<RecItem, Error> {
    let defs = rec.definitions();
    // Where the group's own solutions begin, so they can be stamped with its instance once it has one.
    let solved_from = context.solutions_committed();
    // See `elaborate_module_let`: Γ keys on identities, the registries on names.
    let names = defs
        .iter()
        .map(|def| Free::from(&def.name))
        .collect::<Vec<_>>();
    for (def, name) in defs.iter().zip(&names) {
        context.assume(name, &def.type_);
    }

    let mut types = Vec::with_capacity(defs.len());
    // Each struct former's parameters, at the levels typing read them at, as `elaborate_module_let` reads a lone one's.
    let mut parameters = Vec::with_capacity(defs.len());
    for def in &defs {
        check_written_type_totality(context, &def.type_, ErasedSite::TypeOf(def.name))?;
        // A member's type is that member's own: a proof written in it credits the member's binders, not those of the member the item was entered at.
        context.enter_declaration(Some(def.name));
        let (type_, levels) = match context.struct_decl(&def.name).is_some() {
            true => check_former_type(context, &def.type_)?,
            false => (check_is_sort(context, &def.type_)?.0, Vec::new()),
        };
        types.push(type_);
        parameters.push(levels);
    }

    // Upgrade the assumptions to the *rebuilt* signatures before any body is checked (see `elaborate_rec`): a lowered (under-applied) type must not leak into later reduction. The lowered forms were only needed above, while the signatures checked each other.
    for (name, type_) in names.iter().zip(&types) {
        context.reassume(name, type_);
    }

    // A `satisfy … and …` group registers every member's signature before any body elaborates, as `elaborate_module_let` does for one witness: the bodies resolve through one another's entries, and through their own.
    for (def, type_) in defs.iter().zip(&types) {
        if context.is_witness_declaration(&def.name) {
            register_witness(
                context,
                &def.name,
                type_,
                def.universe_context.clone(),
                &def.island,
            )
            .map_err(|error| error.at_opt(def.type_.extent()))?;
        }
    }

    // A struct group's formers check their parameters against registry arities lowered raw, exactly as a lone former does — see `share_struct_params`.
    for (def, type_) in defs.iter().zip(&types) {
        share_struct_params(context, &def.name, type_);
    }

    // An inductive's type bindings always lower as one `rec` group whose member names are the registry keys. Rebuild the registry index telescopes here — after the rebuilt signatures are assumed (index types may mention the group), before any body's `InductType` node checks against them.
    for def in &defs {
        elaborate_induct_indices(context, &def.name)?;
    }

    // A body reads a member by its name alone and a signature through the member's slot, as `elaborate_rec` states.
    let mut slots = Vec::with_capacity(defs.len());
    let mut through = Vec::with_capacity(defs.len());
    for ((def, name), type_) in defs.iter().zip(&names).zip(&types) {
        let (id, slot) = context.fresh_rec_slot(name, type_.clone());
        context.define(name, &slot, Some(&def.kind));
        slots.push(id);
        through.push((*name, slot));
    }
    let read = types
        .iter()
        .map(|type_| type_.substitute(&through))
        .collect::<Vec<_>>();
    for (name, type_) in names.iter().zip(&read) {
        context.reassume(name, type_);
    }

    let mut bodies = Vec::with_capacity(defs.len());
    for ((def, type_), slot) in defs.iter().zip(&read).zip(slots) {
        let outer_site = context.set_checked_site(def.name);
        context.enter_declaration(Some(def.name));
        context.enter_value(&def.kind);
        let body = check(context, &def.body, type_.clone())?;
        context.restore_checked_site(outer_site);
        context.fill_rec_slot(slot, body.substitute(&through));
        bodies.push(body);
    }
    context.retry_parked()?;

    // Registry rebuild, phase two: constructor payload types may apply the group's type constructors, so their signatures (and `InductType` terminals) elaborate only now that the rebuilt bodies are defined.
    for def in &defs {
        elaborate_induct_constructors(context, &def.name)?;
    }
    // A struct or concept group's field telescopes elaborate now that every former is defined, since each may name the others.
    for (def, levels) in defs.iter().zip(&parameters) {
        elaborate_struct(context, &def.name, levels)?;
    }
    context.sweep_parked()?;

    let types = types
        .iter()
        .map(|type_| zonk_solved_term_metas(context, type_))
        .collect::<Vec<_>>();
    let bodies = bodies
        .iter()
        .map(|body| zonk_solved_term_metas(context, body))
        .collect::<Vec<_>>();
    for def in &defs {
        let Some(induct_decl) = context.induct_decl(&def.name).cloned() else {
            continue;
        };
        context.update_induct(
            &def.name,
            InductDecl {
                universe_context: induct_decl.universe_context,
                arity: zonk_solved_term_metas(context, &induct_decl.arity),
                constructors: induct_decl
                    .constructors
                    .into_iter()
                    .map(|(tag, constructor)| {
                        (
                            tag,
                            InductParam {
                                telescope: zonk_solved_term_metas(context, &constructor.telescope),
                            },
                        )
                    })
                    .collect(),
                result_sort: zonk_solved_term_metas(context, &induct_decl.result_sort),
                module: induct_decl.module,
                rep_public: induct_decl.rep_public,
                polarities: induct_decl.polarities,
                variances: induct_decl.variances,
            },
        );
    }
    for def in &defs {
        if let Some(struct_decl) = context.struct_decl(&def.name).cloned() {
            let arity = zonk_arity(context, &struct_decl.arity)?;
            let result_sort = zonk(context, &struct_decl.result_sort)?;
            context.update_struct(
                &def.name,
                StructDecl {
                    universe_context: struct_decl.universe_context,
                    arity,
                    result_sort,
                    module: struct_decl.module,
                    rep_public: struct_decl.rep_public,
                    polarities: struct_decl.polarities,
                    variances: struct_decl.variances,
                },
            );
        }
        if let Some(concept) = context.concept(&def.name).cloned() {
            context.update_concept(
                &def.name,
                ConceptDecl {
                    universe_context: concept.universe_context,
                    params: zonk_solved_term_metas(context, &concept.params),
                    fields: concept.fields,
                    supers: concept.supers,
                },
            );
        }
    }

    // A top-level group is one cycle of what its members reach: read once their terms and registry entries are elaborated, so a witness's resolutions count, and before the group's levels close, which a member outside the cycle would share.
    let parts = group_parts(context, &defs, &types, &bodies);
    if parts.len() > 1 {
        return Err(Error::group_apart(&defs, &types, &parts));
    }

    // As for a single definition, the group's interface is its member signatures and registry telescopes; levels reachable only through a member body are internal and minimized rather than generalized, and so are the levels of a member's result sort that no member's domain mentions — see [`result_sort_only_metas`]. The rule is the one `finalize_definition` applies, read over the group: a level one member keeps in its interface stays there for all of them, since the group shares a context, and only a level every member confines to its result sort is determined. A group of one is then generalized exactly as the same signature would be as a `let`, which is what keeps the path a definition takes from showing in its scheme.
    let mut interface = types
        .iter()
        .flat_map(|type_| context.universe_metas_in(type_))
        .collect::<BTreeSet<_>>();
    let mut determined = BTreeSet::new();
    let mut kept = BTreeSet::new();
    for type_ in &types {
        let member_determined = result_sort_only_metas(context, type_);
        for meta in context.universe_metas_in(type_) {
            if !member_determined.contains(&meta) {
                kept.insert(meta);
            }
        }
        determined.extend(member_determined);
    }
    determined.retain(|meta| !kept.contains(meta));
    interface.retain(|meta| !determined.contains(meta));
    let mut internal = bodies
        .iter()
        .flat_map(|body| context.universe_metas_in(body))
        .collect::<BTreeSet<_>>();
    internal.extend(determined.iter().copied());
    let names = defs.iter().map(|def| def.name).collect::<Vec<_>>();
    let mut compared = sort_family_levels(context, &names, &mut interface, &mut internal);
    for type_ in &types {
        compared.extend(compared_levels(context, type_));
    }
    // A member that answers a type is read as a `let` that does: what its body holds joins the group's interface, held.
    for (type_, body) in types.iter().zip(&bodies) {
        let unfolded = body_levels(context, type_, body, &determined);
        interface.extend(unfolded.iter().copied());
        compared.extend(unfolded);
    }
    let universe_context = context.finalize_universe_metas(interface, internal, compared)?;

    // The group's members are monomorphic in their own universes, so every occurrence of one member inside the group — in a signature, a body, or a rebuilt registry telescope — denotes the group's own instance. Those occurrences were elaborated before the parameters existed and carry no instance at all until this rewrite gives them one.
    let instance = universe_context.identity_instance();
    let owned = defs.iter().map(|def| def.name).collect::<BTreeSet<_>>();
    fn stamp<B: Bound>(
        context: &Context,
        value: &B,
        owned: &BTreeSet<Global>,
        self_reference: SelfReference,
        instance: &Universes,
    ) -> Result<B, Error> {
        Ok(stamp_declaration_instance(
            &context.zonk_universe_levels(value)?,
            owned,
            self_reference,
            instance,
        ))
    }

    // `RecItem::try_new` captures the member names into the group's binder, so the signatures and bodies reach it with their self-references bound.
    let types = types
        .iter()
        .map(|type_| stamp(context, type_, &owned, SelfReference::Bound, &instance))
        .collect::<Result<Vec<_>, _>>()?;
    let bodies = bodies
        .iter()
        .map(|body| stamp(context, body, &owned, SelfReference::Bound, &instance))
        .collect::<Result<Vec<_>, _>>()?;

    for def in &defs {
        let Some(induct_decl) = context.induct_decl(&def.name).cloned() else {
            continue;
        };
        let constructors = induct_decl
            .constructors
            .into_iter()
            .map(|(tag, constructor)| {
                Ok((
                    tag,
                    // A registry telescope is stored outside the group and instantiated per use site, so its self-references are free and must carry the instance themselves.
                    InductParam {
                        telescope: stamp(
                            context,
                            &constructor.telescope,
                            &owned,
                            SelfReference::Free,
                            &instance,
                        )?,
                    },
                ))
            })
            .collect::<Result<_, Error>>()?;
        let free = SelfReference::Free;
        let arity = stamp(context, &induct_decl.arity, &owned, free, &instance)?;
        let result_sort = stamp(context, &induct_decl.result_sort, &owned, free, &instance)?;
        context.update_induct(
            &def.name,
            InductDecl {
                universe_context: universe_context.clone(),
                arity,
                constructors,
                result_sort,
                module: induct_decl.module,
                rep_public: induct_decl.rep_public,
                polarities: induct_decl.polarities,
                variances: induct_decl.variances,
            },
        );
    }
    // A struct or concept registry entry is stamped as an inductive's is: stored outside the group, so its self-references are free and carry the instance themselves.
    for def in &defs {
        let free = SelfReference::Free;
        if let Some(struct_decl) = context.struct_decl(&def.name).cloned() {
            context.update_struct(
                &def.name,
                StructDecl {
                    universe_context: universe_context.clone(),
                    arity: stamp(context, &struct_decl.arity, &owned, free, &instance)?,
                    result_sort: stamp(context, &struct_decl.result_sort, &owned, free, &instance)?,
                    module: struct_decl.module,
                    rep_public: struct_decl.rep_public,
                    polarities: struct_decl.polarities,
                    variances: struct_decl.variances,
                },
            );
        }
        if let Some(concept) = context.concept(&def.name).cloned() {
            context.update_concept(
                &def.name,
                ConceptDecl {
                    universe_context: universe_context.clone(),
                    params: stamp(context, &concept.params, &owned, free, &instance)?,
                    fields: concept.fields,
                    supers: concept.supers,
                },
            );
        }
    }
    // The solutions committed while the group elaborated are terms it produced too, and a later zonk substitutes them long after this rewrite — the proof-totality check fills in every type elaboration recorded, and a goal's report its scope — so they are stamped as a registry entry is, being stored outside the group. `Eq()(a, a)` in a constructor's payload solves `Eq`'s `@A` to the member's reduct from before its generalization, with no instance, and `Eq()`'s recorded type carries it into that check.
    let restamped = context
        .solutions_since(solved_from)
        .into_iter()
        .map(|(id, solution)| {
            let stamped = stamp(context, &solution, &owned, SelfReference::Free, &instance)?;
            Ok((id, solution, stamped))
        })
        .collect::<Result<Vec<_>, Error>>()?
        .into_iter()
        .filter(|(_, solution, stamped)| solution != stamped)
        .map(|(id, _, stamped)| (id, stamped))
        .collect();
    context.restamp_solutions(restamped);

    let definitions = defs
        .iter()
        .zip(types)
        .zip(bodies)
        .map(|((def, type_), body)| Definition {
            name: def.name,
            kind: def.kind.clone(),
            universe_context: universe_context.clone(),
            island: def.island,
            totality: def.totality,
            proof: def.proof,
            variances: Vec::new(),
            field_variances: Vec::new(),
            parameter_field: None,
            value_sort: None,
            type_,
            body,
        })
        .collect();
    let rec = RecItem::try_new(definitions)?;
    // Before the members are defined into the context, because the shape this rejects makes reducing a *use* of one of them unfold without end.
    check_rec_item_totality(context, &rec)?;

    for (index, definition) in rec.definitions.iter().enumerate() {
        let name = Free::from(&definition.name);
        context.reassume(&name, &rec.group.member_type(index));
        context.define(
            &name,
            &Term::rec_proj(rec.group.clone(), index),
            Some(&definition.kind),
        );
        context.set_assumption_universe_context(&name, universe_context.clone());
        if context.is_witness_declaration(&definition.name) {
            context.update_witness_scheme(
                &definition.name,
                universe_context.clone(),
                rec.group.member_type(index),
            );
        }
    }

    // A later item may compare two instances of a family this group declares, so each one's variance is settled here, with the formers defined at their finalized bodies and the registry entries stamped.
    let declared = rec
        .definitions
        .iter()
        .map(|definition| definition.name)
        .collect::<Vec<_>>();
    record_variances(context, &declared)?;

    // Classify the whole group at once: its members may mention each other, so no member's verdict is settled until the group's descent is.
    let group = group_totality(context, &rec.group)?;
    for definition in rec.definitions() {
        record_definition_totality(context, &definition, group)?;
    }

    Ok(rec)
}

/// The parts a top-level group's members fall into by what each reaches of the others, through its type, its body and its registry entry: the cycles of that relation, each its members' positions in the order they are written, the parts in the order of their first members. One part where the group is one cycle.
///
/// A declaration reads any other wherever it is written, so a group ties nothing but declarations that need one another, and one part is all a group may be ([`Error::GroupApart`]).
fn group_parts(
    context: &Context,
    defs: &[Definition],
    types: &[Term],
    bodies: &[Term],
) -> Vec<Vec<usize>> {
    let names = defs.iter().map(|def| def.name).collect::<Vec<_>>();
    let edges = names
        .iter()
        .zip(types.iter().zip(bodies))
        .map(|(name, (type_, body))| {
            let mut reached = type_.reaches();
            reached.extend(body.reaches());
            if let Some(entry) = context.induct_decl(name) {
                reached.extend(entry.reaches());
            }
            if let Some(entry) = context.struct_decl(name) {
                reached.extend(entry.reaches());
            }
            if let Some(entry) = context.concept(name) {
                reached.extend(entry.reaches());
            }
            names
                .iter()
                .enumerate()
                .filter(|(_, other)| reached.contains(other))
                .map(|(index, _)| index)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let reach = (0..names.len())
        .map(|start| {
            let mut seen = BTreeSet::from([start]);
            let mut stack = vec![start];
            while let Some(at) = stack.pop() {
                for &next in &edges[at] {
                    if seen.insert(next) {
                        stack.push(next);
                    }
                }
            }
            seen
        })
        .collect::<Vec<_>>();

    let mut placed = vec![false; names.len()];
    let mut parts = Vec::new();
    for member in 0..names.len() {
        if placed[member] {
            continue;
        }
        let part = (member..names.len())
            .filter(|&other| {
                !placed[other] && reach[member].contains(&other) && reach[other].contains(&member)
            })
            .collect::<Vec<_>>();
        for &other in &part {
            placed[other] = true;
        }
        parts.push(part);
    }

    parts
}

/// Elaborate one persistent module item and perform every item-boundary obligation. Both module drivers use this path so universe transactions, parked work, witnesses, privacy islands, and error attribution cannot drift.
///
/// One attempt: what it returns stands only where the attempt read nothing unfinished, which [`Demand`] asks the context after it.
///
/// **An item is published as it finishes.** What it elaborated to is zonked, strictly, and the context is made to hold that — the bindings of its names, its registry entries, the terms it recorded — so nothing of the state it elaborated in is left for another declaration to read, the next one elaborating in a state of its own ([`Context::attempt`]). A hole it left unsolved is its own refusal, raised where it stands.
///
/// **What is decided of it over its final terms is decided here, once** ([`settle_published`]): the polarity vectors of the entries it declares, that the variance vectors they carry claim no more than their telescopes grant, each definition's totality, and its two erasure obligations. No pass over the unit decides any of them again.
///
/// **An item that wrote a goal is held.** Its goals are found here and reported once every declaration of the unit has finished, in the state it elaborated in, which is the only one that knows them ([`Holding`]). It is published by its types alone ([`Context::publish_held`]), so the declarations that read it elaborate and report their own.
fn elaborate_module_item(context: &mut Context, item: &Item) -> Result<Elaborated, Error> {
    curios_profile::profile!("elaborate_module_item");
    let item_module = match item {
        Item::Let(definition) => definition.island,
        Item::Rec(rec) => rec.island(),
    };
    context.set_island(item_module);

    let declared = item
        .declared_names()
        .into_iter()
        .copied()
        .collect::<Vec<_>>();
    let owner = declared.first();
    // One span per top-level item, aggregated per item rather than across all of them: the fixed prelude is over a thousand declarations whose costs differ by three orders of magnitude, and a single averaged row cannot say which one a pass is actually spending on.
    curios_profile::profile!("declaration", group = %item.describe());

    // The step budget is per declaration, so it is restored here rather than drained across the module: whether this item typechecks must not depend on how much the items before it happened to spend.
    context.restore_budget();
    context.enter_item(owner.copied());
    let recorded = context.checked_mark();

    let elaborated = match item {
        Item::Let(definition) => elaborate_module_let(context, definition),
        Item::Rec(rec) => elaborate_module_rec(context, rec).map(Item::Rec),
    }
    // Attribute *every* failure to the item that caused it, not only universe invariants. A whole-module diagnostic with no declaration name — an exhausted budget or an effect reduced at the type level — costs a full prelude rebuild to localize, which is the expensive way to learn one string.
    .map_err(|error| error.in_declaration(declared.iter().copied()))?;

    context
        .drain_parked()
        .map_err(|error| error.in_declaration(declared.iter().copied()))?;
    context.close_qualities();
    context.finish_universe_transaction();

    if context.holds_goals() {
        let sites = goal_sites(context, &alone(context, &elaborated), None);
        if !sites.is_empty() {
            // No obligation is read off a term that holds a goal: the unit is refused for the goal.
            context.truncate_checked(recorded);
            // A type former, a concept and a witness are read by more than their types — a registry entry, a table entry — so one that holds a goal is taken out, and what reads it is withheld.
            let plain = item.declared_names().into_iter().all(|name| {
                context.induct_decl(name).is_none()
                    && context.struct_decl(name).is_none()
                    && context.concept(name).is_none()
                    && !context.is_witness_declaration(name)
            });
            let published = plain && context.publish_held(&elaborated).is_ok();

            return Ok(Elaborated::Held {
                item: elaborated,
                sites,
                published,
            });
        }
    }

    let mut published = zonk_published(context, &elaborated)
        .map_err(|error| error.in_declaration(declared.iter().copied()))?;
    // Each binder says whether it binds a proof before any declaration can read it: the closed machine reads that where it substitutes.
    state_proof_binders(context, &mut published)
        .map_err(|error| error.in_declaration(declared.iter().copied()))?;
    context.publish(&published);
    context
        .settle_checked(recorded)
        .map_err(|error| error.in_declaration(declared.iter().copied()))?;
    // An attempt that read something unfinished is void whatever it concludes, so nothing is decided of what it published.
    // An obligation the item breaks is the item's as much as a refusal of it is, and is reported under its name.
    let obligations = match context.needs_nothing() {
        true => settle_published(context, &mut published, recorded)
            .map_err(|error| error.in_declaration(declared.iter().copied()))?
            .into_iter()
            .map(|error| {
                error
                    .settled(context)
                    .in_declaration(declared.iter().copied())
            })
            .collect(),
        false => Vec::new(),
    };

    Ok(Elaborated::Published {
        item: published.item,
        obligations,
    })
}

/// Decide what a published declaration's cells hold beyond its terms, and hand back the erasure obligations it breaks: the polarity vector of each entry it declares, written on the entry; that the variance vector each carries claims no irrelevance its final telescopes deny; each definition's totality, recorded and stamped on the item; and obligations (T) and (V) over the item's positions and the terms recorded since `recorded`, which are taken.
///
/// **Here, and nowhere after.** Each of these is a cell of the declaration, written once, by it, before any other declaration reads it (`documentation/design/compilation/a-declaration-is-a-function-of-what-it-reads.md`): every declaration it reads has finished, so what the analyses meet outside it is final, and nothing that finishes later can mention it into a different answer. A pass over the unit would decide them a second time, on a budget every declaration of the unit shares.
///
/// **Each judgment spends a budget restored for it**, as the item's elaboration does, so whether one fits is a fact about this declaration and never about what the others cost.
///
/// The obligations are handed back rather than raised, for the reason [`elaborate_and_zonk_program_reporting`] gives.
fn settle_published(
    context: &mut Context,
    published: &mut Published,
    recorded: usize,
) -> Result<Vec<Error>, Error> {
    if !(published.induct_decls.is_empty() && published.struct_decls.is_empty()) {
        context.restore_budget();
        decide_polarities(context, published)?;
        context.restore_budget();
        hold_variances(context, published)?;
    }
    context.restore_budget();
    stamp_totality(context, &mut published.item)?;

    context.restore_budget();
    let alone = Module {
        items: vec![carrying(context, published.item.clone())].into(),
        mounts: Vec::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    };
    // One cache across both: they are seeded from the same recorded entries, so a cache each zonks every distinct recorded type twice.
    let mut zonked = Zonked::default();

    Ok([
        check_type_totality(context, &alone, None, recorded, &mut zonked),
        check_proof_totality(context, recorded, &mut zonked),
    ]
    .into_iter()
    .filter_map(Result::err)
    .collect())
}

/// What one attempt at an item came to.
enum Elaborated {
    /// The item, zonked, as the context now holds it, with the erasure obligations it breaks.
    Published { item: Item, obligations: Vec<Error> },
    /// The item as it elaborated, where it holds each goal it wrote, and whether it was published by its types alone or has to be taken out.
    Held {
        item: Item,
        sites: Vec<GoalSite>,
        published: bool,
    },
}

/// `item` as a module of its own, with the registry entries its names declare as the context holds them: what its goals are found in.
fn alone(context: &Context, item: &Item) -> Module {
    Module {
        items: vec![carrying(context, item.clone())].into(),
        mounts: Vec::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    }
}

/// `item` with each of its formers carrying the registry entries the context holds under its name: what a finished item is read and assembled as. An entry is elaborated in the context's tables, beside its former and after it, so the one the lowering wrote into the former is replaced here, where the item leaves the elaboration that made it; a former whose name the context holds nothing under keeps the one it has.
fn carrying(context: &Context, item: Item) -> Item {
    item.with_nominals(|name, lowered| {
        Some(match lowered {
            Nominal::Inductive(_) => Nominal::Inductive(context.induct_decl(name)?.clone()),
            Nominal::Struct(_) => Nominal::Struct(context.struct_decl(name)?.clone()),
            Nominal::Concept { .. } => Nominal::Concept {
                concept: context.concept(name)?.clone(),
                record: context.struct_decl(name)?.clone(),
            },
        })
    })
}

/// Each witness `module` declares among `items`, under the concept and the key its name states ([`WitnessId`](curios_core::WitnessId)), at the place `declarations` gives its name. The lowering read both off how the signature is spelled, over every item of the unit, so a whole compile and a recompile hold one key.
fn declared_witnesses(
    module: &Module,
    items: &[&Item],
    declarations: &Declarations,
) -> Vec<(Global, Declared)> {
    items
        .iter()
        .flat_map(|item| item.definitions())
        .filter(|definition| module.witnesses.contains(&definition.name))
        .map(|definition| {
            let declared = Declared {
                spelled: definition.name.witness_key(),
                module: definition.island,
                position: declarations
                    .item(&definition.name)
                    .expect("a name an item declares is placed"),
            };

            (definition.name, declared)
        })
        .collect()
}

/// What one unit's elaboration produced, before anything is finalized: the module itself and the refusals it recovered from.
struct ElaboratedSuffix {
    /// The items that elaborated, with the registry entries, witness markers and tests of those alone.
    module: Module,
    /// A program's entry, stating the type it was judged at, when it elaborated and nothing depends on a refusal.
    entry: Option<Entrypoint>,
    /// Every item's refusal in item order, the entry's after them. Non-empty means the module is not the program that was written, and the caller raises these rather than finalizing it as one.
    refusals: Vec<Error>,
    /// The erasure obligations the kept items break, in item order: reported with the entry's, and never a refusal, since the items stand.
    obligations: Vec<Error>,
    /// The declarations that wrote goals, each with the state it elaborated in: reported with the entry's once every item has finished.
    held: Vec<Holding>,
    /// What each declaration asked of the declarations around it: in the attempt that stood, or where the baseline elaborated it for one kept.
    traces: Traces,
    /// Every name of a declaration elaborated again over a baseline: every other the unit declares is kept as the baseline holds it. Empty for a unit compiled whole.
    made: BTreeSet<Global>,
    /// Who asked what, of `traces`, where the unit was compiled over a baseline: the baseline's readers with the edges of what the walk made or dropped replaced.
    readers: Option<Readers>,
}

/// Elaborate a [`Module`] against an already-elaborated scope, returning what it added. Each top-level item is checked and `define`d *cumulatively in the persistent base frame* — never a popped `with_frame` — so every definition stays in scope for later items, the entrypoint's body and the type it states. Returns the rebuilt suffix, its entry stating the type its body was judged at, reduced through the accumulated definitions.
///
/// Elaboration is authoritative: the returned module — not the lowered input — is what `erase` reads, each item zonked where it was published ([`zonk_published`]).
///
/// **One protocol serves both entry points**, and that is the point. `established` is [`Established::nothing`] for a from-scratch elaboration and the units in scope otherwise; every step below degenerates to the whole-module reading when it is empty, so there is no second implementation to keep in agreement by reading — the shape of a configuration-dependent defect, a rule correct in the configuration its author was looking at and wrong in the other one.
///
/// A scope is read through where it stands and nothing of it is elaborated or brought in ([`Context::read_through`]), and so are the declarations a unit compiled `over` a baseline keeps of it: `established` holds the baseline as its last layer ([`Established::keeping`]), answering for every declaration the walk does not hold, and for each it holds from where the walk keeps it. `declarations` are the ones `module`'s items make, by which the walk asks for one.
fn elaborate_module_suffix<'s>(
    context: &mut Context<'s>,
    established: Option<&'s Established<'_>>,
    over: Option<Over<'_>>,
    module: &Module,
    declarations: &Declarations,
    minted: &Rc<Minted>,
    tail: Option<Tail<'_>>,
) -> Result<ElaboratedSuffix, Error> {
    curios_profile::profile!("elaborate_module_suffix");
    // The scope is read through, and nothing of it goes in but each unit's mounts, which a privilege question is answered out of and which the unit's own items may ask while they elaborate. `register_*` rejects a key the scope holds, and the unit declares only its own, so the two cannot collide: a baseline answers for no name before the walk keeps its declaration.
    if let Some(established) = established {
        established.coherent()?;
        context.read_through(established);
        for unit in established.modules() {
            context.mount(&unit.mounts);
        }
    }
    // The unit's own mounts, after its scope's. Mount sets are pairwise disjoint, so the order only fixes which entry a lookup finds first among equals, and there are none.
    context.mount(&module.mounts);

    // Elaboration goes on minting in the spaces the unit's lowering minted in, so each counter starts above the lowering's count: a lowered hole and an inserted implicit are distinct metavariables, and an unbound name — a free local the lowering minted — is never an elaborated binder, which would find it bound instead of reporting it. The scope's terms carry neither, so nothing a predecessor minted can meet these.
    context.seed(minted);

    // The prover's vocabulary, as far as the unit declares it: asked for first, each member bringing in what it reads.
    let vocabulary = context
        .syntax()
        .entailment
        .targets()
        .map(|name| Global::Authored(name.qualifier()))
        .filter(|name| declarations.item(name).is_some())
        .collect::<BTreeSet<_>>();

    // Every item, because `module` carries only its own: the prefix arrived as scope through the replay above rather than as a run of leading items to skip. Each elaborates when it is asked for, after what it reads, which is `demand`'s; over a baseline, the walk holds what the edit could reach, and the baseline answers for every other declaration from here on.
    //
    // A refused item is reported, undone and poisoned, and the other items are elaborated as if it had never been written; an item that reaches a poisoned name is withheld and reports nothing of its own. What is undone, what is left and why a dependent is silent is `recovery`'s.
    let entry_stamp = ItemStamp(module.items.len());
    let mut demand = Demand::new(
        module,
        declarations,
        Poison::seeded(context.take_broken()),
        Survivors::default(),
        over,
    );
    demand.reach(context, &vocabulary);
    let held_items = demand.held_items().collect::<Vec<_>>();

    // The entries the lowered formers of the declarations the walk holds carry, each registered under its former's name: every family, then every record, then every concept, with the witness markers alongside — witness *table* entries register per item (`elaborate_module_let`), once the elaborated head exists. The baseline answers for every other declaration's.
    let nominals = || held_items.iter().flat_map(|item| item.nominals());
    for (name, nominal) in nominals() {
        if let Some(declaration) = nominal.inductive() {
            context.register_induct(name, declaration.clone())?;
        }
    }
    for (name, nominal) in nominals() {
        if let Some(record) = nominal.record() {
            context.register_struct(name, record.clone())?;
        }
    }
    for (name, nominal) in nominals() {
        if let Some(concept) = nominal.concept() {
            context.register_concept(name, concept.clone())?;
        }
    }
    for name in held_items.iter().flat_map(|item| item.declared_names()) {
        if module.witnesses.contains(name) {
            context.mark_witness_declaration(name);
        }
    }
    check_concept_registry(context)?;

    // Where each name of the unit is written, and which of them the walk holds: what an item reads of a declaration that has not elaborated is then a need, never an absence. Every witness among them goes in under the key its name states, which is what a question about its concept is answered by until it elaborates.
    for (name, declared) in declared_witnesses(module, &held_items, declarations) {
        context.declare_witness(name, declared);
    }
    context.begin_unit(declarations.clone(), demand.held_names());
    {
        curios_profile::profile!("demand::all");
        demand.all(context, &vocabulary);
    }
    if let Some(established) = established {
        demand.note_foreign(established);
    }
    let made = demand.made_names();
    let (poison, mut survivors, traces, readers) = demand.into_parts();

    context.set_island(Qualifier::empty());
    // The entrypoint expression is not an item, so it gets its own budget on the same footing as one.
    context.restore_budget();
    // A test program's tail is synthesized here rather than handed in: it is built over the elaborated definitions the items just produced, in the scope they were defined into, so the discharge it chooses for each test can consult them — and over the tests no refusal poisoned, since a test of a refused declaration is that refusal's dependent and is no longer in the module to schedule. The filter asks the poison rather than the survivors because a unit's tests are scheduled into the tail of the program compiled after it, whose own module declares none of them, so a filter over this module's survivors would drop every one. The unit's own written entry — the ordinary program — is not part of a test program and is neither checked nor kept.
    let synthesized;
    let entry = match tail {
        None => None,
        Some(Tail::Entry(entry)) => Some(entry),
        Some(Tail::Tests(scheduled)) => {
            let scheduled = scheduled
                .iter()
                .filter(|test| !poison.holds(&test.name))
                .cloned()
                .collect::<Vec<_>>();
            // A test program's tail is judged at what `Test/main` answers, whether its tests held: the verdict is the value the entry hands back, where a program's unit says nothing a runner could read.
            synthesized = Entrypoint {
                body: test_program_tail(context, &scheduled),
                type_: Some(Term::intrinsic(Intrinsic::io_type(Term::intrinsic(
                    Intrinsic::BoolType,
                )))),
            };
            // Choosing each test's discharge reduces bodies under their telescopes; that is spent on the declarations' behalf, so the tail itself still checks on a full budget.
            context.restore_budget();
            Some(&synthesized)
        }
    };

    // The entry is checked as an item is — under its own stamp, undone when refused, withheld when it reaches a poisoned name — except that nothing depends on it, so its refusal poisons nothing. The type it states is part of it: an annotation naming a refused declaration makes the entry that declaration's dependent.
    let withheld = entry.is_some_and(|entry| poison.reaches_entry(entry));
    let mark = ItemMark::begin(context);
    context.attempt(&[]);
    // Elaborated, drained of what it parked and reduced to the type it states, as one attempt: a refusal at any of the three undoes what the entry recorded, so the passes over the terms left recorded read nothing of a refused entry.
    let attempted = |context: &mut Context| {
        let elaborated = elaborate_entry(context, entry)?;
        context.drain_parked()?;
        let Some(Entrypoint { body, type_ }) = elaborated else {
            return Ok(None);
        };
        // The type the entry states from here on is its judged type reduced through the unit's definitions, the spelling every later stage reads it in.
        let type_ = type_.expect("elaboration states the type the entry was judged at");
        let type_ = reduce_with(context, &type_)?;
        Ok::<_, Error>(Some(Entrypoint {
            body,
            type_: Some(type_),
        }))
    };
    let entry = match withheld {
        true => None,
        false => match attempted(context) {
            Ok(entry) => entry,
            Err(error) => {
                let error = error.settled(context);
                mark.undo(context, &[]);
                survivors.refuse(entry_stamp, error);
                None
            }
        },
    };
    // Every item has finished, so nothing the entry read was unfinished: its one attempt stands.
    assert!(
        context.take_needs().is_empty(),
        "the entry read a declaration no item finished"
    );

    // The output carries the *rebuilt* registry entries (pulled back from the context, where the per-group rebuild re-registered them), so `erase` sees elaborated telescopes, zonked where their declarations were published. An entry the context no longer holds was a refused or withheld item's, taken out with it: the module carries the survivors' entries and no other, since an entry whose declaring item is absent would reach the kernel and erasure in its lowered form, which nothing zonked.
    let names = survivors.names();
    let witnesses = names
        .iter()
        .filter(|name| module.witnesses.contains(*name))
        .copied()
        .collect();
    let tests = module
        .tests
        .iter()
        .filter(|name| names.contains(*name))
        .cloned()
        .collect();
    let Survived {
        items,
        refusals,
        obligations,
        held,
    } = survivors.into_parts();

    // Each former leaves with the entry the context rebuilt for it: elaborated telescopes, zonked, with the vectors its settling wrote.
    let items = items
        .into_iter()
        .map(|item| carrying(context, item))
        .collect::<Vec<_>>();
    let module = Module {
        items: items.into(),
        mounts: module.mounts.clone(),
        witnesses,
        tests,
    };

    Ok(ElaboratedSuffix {
        module,
        entry,
        refusals,
        obligations,
        held,
        traces,
        made,
        readers,
    })
}

/// Elaborate the entry at the type it states — first the type, then the body against it — or, where it states none, infer the body's; either way the rebuilt entry states the type its body was judged at.
///
/// A stated type is a written type like any item's, and elaborating it is what makes it usable as an expectation: a universe-polymorphic head arrives instantiated, and an application of a type former reduces to the intrinsic it denotes. Left raw it would stay exactly as lowered, so `List(Nat)` would reach conversion as an `Apply` no unfolding could reconcile with the inferred `Intrinsic::ListType` — a mismatch reported between two spellings of the same type. Elaborating here rather than in the caller keeps it in the frame every item was just defined into, which is the scope its globals resolve against.
///
/// The rebuilt entry carries the type it was judged at whether it was stated or inferred, because every stage after this one reads it there: the kernel rechecks the body against it, `zonk` walks it, and erasure seals the entry at it. Keeping only a written annotation would leave the program contract a compile supplies nowhere in the module, so the kernel would infer the body's type rather than check it, and erasure would take the type from a second channel.
fn elaborate_entry(
    context: &mut Context,
    entry: Option<&Entrypoint>,
) -> Result<Option<Entrypoint>, Error> {
    // A unit with no entrypoint has none to elaborate and no type for one — the `Entrypoint` carries both, which is what keeps them one fact from here to the kernel.
    let Some(entry) = entry else {
        return Ok(None);
    };
    context.enter_item(None);
    let mode = match &entry.type_ {
        Some(stated) => Mode::Check(check_is_sort(context, stated)?.0),
        None => Mode::Infer,
    };
    let (body, type_) = elaborate(context, &entry.body, mode)?;

    Ok(Some(Entrypoint {
        body,
        type_: Some(type_),
    }))
}

/// What finalizing an elaborated unit decided: the module, the entry a program closes with — stating the type it was judged at — and the erasure obligations this checker decides but does not always raise; see [`elaborate_and_zonk_program_reporting`] for why they are reported rather than thrown.
struct Finalized {
    module: Module,
    entry: Option<Entrypoint>,
    /// The (T) and (V) obligations, reported rather than raised. Empty when the module and its entry carry none.
    obligations: Vec<Error>,
    /// The order the module's items were put in ([`Module::in_order`]).
    order: Order,
}

/// A program elaborated, zonked and checked, with the erasure obligations elaboration decided over it reported rather than raised — see [`elaborate_and_zonk_program_reporting`].
pub struct FinalizedProgram {
    pub module: Module,
    /// The entry the program closes with, stating the type it was judged at. `None` only where it was withheld for reaching an item the lowering could not read — see [`elaborate_and_zonk_program`].
    pub entry: Option<Entrypoint>,
    /// The (T) and (V) obligations, reported rather than raised. Empty when the program carries none.
    pub obligations: Vec<Error>,
}

/// Finalize an elaborated module, and a program's entry beside it: report the goals the unit wrote, hold the module's universes to their schemes, put it in the order it is stored in, and zonk the entry and decide its erasure obligations.
///
/// Every check of a declaration the consistency claim rests on was applied as the declaration was published ([`settle_published`]), and every entry point that produces an elaborated module comes through there and through here. What is left for this function is what belongs to no declaration: the goals, reported once every declaration has finished, and the entry, which is no item.
///
/// **No item is zonked here, or searched for a goal.** An item is published zonked, strictly, with the registry entries it declares ([`zonk_published`]), and a declaration that wrote a goal is held out of the module, so a second pass over the items would find what the first left: nothing.
///
/// The order is load-bearing, in two places:
///
/// - `default_universes` then `zonk_entry` come first, which is what makes the entry metavariable-free by construction — it errors on an unsolved hole, so no metavariable can later be solved to a partial term — and it holds the entry's universes to the module's schemes, as [`validate_universes`] holds the module's own before it.
/// - [`Context::restore_budget`] precedes the entry's obligations because they reduce, and they have to spend on the same footing as an item's rather than on whatever the entry's check left.
///
/// `obligations` are the ones the items broke, in item order; the entry's follow them.
fn finalize_and_check(
    context: &mut Context,
    module: Module,
    entry: Option<Entrypoint>,
    held: Vec<Holding>,
    mut obligations: Vec<Error>,
) -> Result<Finalized, Error> {
    curios_profile::profile!("finalize_and_check");
    let mut entry_terms = Vec::new();
    if let Some(entry) = &entry {
        entry_terms.push(entry.body.clone());
        entry_terms.extend(entry.type_.clone());
    }
    let mut entry_terms = context
        .default_universes(&entry_terms.iter().collect::<Vec<_>>())?
        .into_iter();
    let entry = entry.map(|entry| Entrypoint {
        body: entry_terms.next().expect("entry body was finalized"),
        type_: entry
            .type_
            .map(|_| entry_terms.next().expect("entry type was finalized")),
    });

    let nothing = Module {
        items: Vec::new().into(),
        mounts: Vec::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    };
    // Written goals report as one complete batch before zonking: the items', each found as its item finished (`held`), then the entry's, whose collection meets exactly the set strict zonk would (committed solutions included), so a goal-bearing program fails with every goal located rather than with the first (`goal_sites`).
    let sites = goal_sites(context, &nothing, entry.as_ref());
    if !(held.is_empty() && sites.is_empty()) {
        return Err(Error::goals(report_written_goals(
            context,
            &module,
            entry.as_ref(),
            held,
            &sites,
        )));
    }

    validate_universes(&module)?;
    // The order the unit is stored in, given once: every walk after this one takes the items as they come.
    let (module, order) = module.in_order();
    // A refusal of the entry's terms is settled for its reader here, the state it arose in still standing.
    let entry = entry
        .map(|entry| zonk_entry(context, &module, &entry))
        .transpose()
        .map_err(|error| error.settled(context))?;
    context.restore_budget();

    // Reported rather than raised. `curios-cert` decides these same two obligations independently, and a fixture this checker refuses must still be able to reach it — a short circuit here would return no module at all, leaving "would the kernel have caught it?" unobservable, which is exactly the quadrant the trusted base most needs to see. The public entry points raise the first verdict, so nothing on the compile path is weakened.
    //
    // The entry's alone: every item's were decided as it was published, and took the terms it recorded, so what is left recorded is the entry's. One cache across both, which are seeded from the same recorded entries.
    let mut zonked = Zonked::default();
    let checked = [
        check_type_totality(context, &nothing, entry.as_ref(), 0, &mut zonked),
        check_proof_totality(context, 0, &mut zonked),
    ];
    obligations.extend(
        checked
            .into_iter()
            .filter_map(Result::err)
            .map(|error| error.settled(context)),
    );

    Ok(Finalized {
        module,
        entry,
        obligations,
        order,
    })
}

/// Report every goal a unit wrote, the items' in item order and the entry's (`sites`) after them, each with what fits it drawn from the unit as it was written.
///
/// A declaration that wrote a goal is not among `module`'s items, which are the ones that finished. It is put back among them here, once every declaration of the unit has finished, so a goal is offered the definitions written beside its own whichever of them elaborated first. Each declaration's goals are reported in the state it elaborated in ([`Context::within`]), and the entry's in the context's own.
fn report_written_goals(
    context: &mut Context,
    module: &Module,
    entry: Option<&Entrypoint>,
    held: Vec<Holding>,
    sites: &[GoalSite],
) -> Vec<GoalReport> {
    /// Where `item` is written among the unit's items.
    fn place(context: &Context, item: &Item) -> Option<usize> {
        item.declared_names()
            .first()
            .and_then(|name| context.written_at(name))
    }

    let mut written = module.items.to_vec();
    written.extend(
        held.iter()
            .flat_map(|holding| holding.items.iter().map(|(item, _)| item.clone())),
    );
    written.sort_by_key(|item| place(context, item));

    let mut reports = Vec::new();
    for Holding { state, items } in held {
        context.within(state, |context| {
            for (item, sites) in &items {
                reports.push((
                    place(context, item),
                    report_goals(context, sites, &written, entry),
                ));
            }
        });
    }
    reports.sort_by_key(|(place, _)| *place);
    let mut reports = reports
        .into_iter()
        .flat_map(|(_, reports)| reports)
        .collect::<Vec<_>>();
    reports.extend(report_goals(context, sites, &written, entry));

    reports
}

/// The erasure-obligation verdicts, as one error — how every caller but the path that reports them beside the kernel's verdicts consumes [`finalize_and_check`]'s report. Both obligations are reported when both fail: they are decided independently, and a reader fixing one is owed the other.
fn raise(outcome: Finalized) -> Result<(Module, Option<Entrypoint>), Error> {
    let Finalized {
        module,
        entry,
        obligations,
        ..
    } = outcome;
    match obligations.is_empty() {
        true => Ok((module, entry)),
        false => Err(Error::batch(obligations)),
    }
}

/// What a run with refusals raises: every refusal in item order, then what finalizing the survivors said — their goal batch or a zonk refusal, or the erasure obligations of a module that finalized. The survivors are finalized anyway so that the goals beside the refusals are reported in the same run.
fn refused(refusals: Vec<Error>, finalized: Result<Finalized, Error>) -> Error {
    let mut errors = refusals;
    match finalized {
        Ok(finalized) => errors.extend(finalized.obligations),
        Err(error) => errors.push(error),
    }

    Error::batch(errors)
}

/// Elaborate a whole [`Module`] with no cached prefix, then zonk and check it.
pub fn elaborate_and_zonk_module(
    context: &mut Context,
    module: &Module,
    minted: &Rc<Minted>,
) -> Result<Module, Error> {
    curios_profile::profile!("elaborate_and_zonk_module");
    grown(|| {
        let declarations = Declarations::of(module);
        let suffix =
            elaborate_module_suffix(context, None, None, module, &declarations, minted, None)?;
        let finalized = finalize_and_check(
            context,
            suffix.module,
            None,
            suffix.held,
            suffix.obligations,
        );
        if !suffix.refusals.is_empty() {
            return Err(refused(suffix.refusals, finalized));
        }

        raise(finalized?).map(|(module, _)| module)
    })
}

/// Which final term a program's elaboration checks: the entry it closes with, or a test program's tail synthesized over the unit's registered tests once its items are defined. A policy rather than an entry handed in, because the tail cannot be built before the items it schedules have elaborated and must not be built anywhere else.
#[derive(Debug, Clone, Copy)]
pub enum Tail<'a> {
    /// The program's own [`Entrypoint`], exactly as lowered.
    Entry(&'a Entrypoint),
    /// `Test/main([...])` over these tests, replacing the program's written entry; a unit with no tests gets `Test/main([])`.
    Tests(&'a [ScheduledTest]),
}

/// Elaborate one [`Module`] against the units already established, then zonk and check it.
///
/// `established` is that scope: the modules elaborated ahead of this one, in dependency order and *borrowed* rather than merged. What a predecessor contributes is read where it stands — a name's type and body, a registry entry, a totality verdict, the witness at a key — and nothing of it is seeded into the [`Context`] but its mounts.
///
/// Sound because a scope is unit-independent: its items never see this unit's code, and — since top-level definitions are excluded from a metavariable's Γ (`Context::identity_snapshot`) — an item elaborates against the identical local context it would with no scope at all, so the solutions (and the zonked output) are identical.
///
/// **The returned module holds this unit's items and no one else's.** That is a contract rather than an artifact of how the elaboration happens to be written: [`crate::erase_unit`] erases a unit *onto* what its scope already erased, and re-deriving the standard library on every compilation is exactly what carrying it here would cost.
///
/// Beside it comes what each of its declarations asked of the declarations around it ([`Traces`]), which a later compile of the unit keeps a declaration by.
pub fn elaborate_and_zonk_unit<'s>(
    context: &mut Context<'s>,
    established: &'s Established<'_>,
    module: &Module,
    minted: &Rc<Minted>,
) -> Result<(Module, Traces), Error> {
    let (finalized, traces) = elaborate_and_zonk(context, established, module, minted, None)?;
    raise(finalized).map(|(module, _)| (module, traces))
}

/// [`elaborate_and_zonk_unit`] for a program: `module` and the term `tail` closes it with, both elaborated, zonked and checked.
///
/// The entry comes back `None` only where it was withheld for reaching an item the lowering could not read. Withholding records no refusal — that is `recovery`'s decision for every dependent of a broken item — so the caller, which holds the broken items, reports them, and a program with none always has its entry.
pub fn elaborate_and_zonk_program<'s>(
    context: &mut Context<'s>,
    established: &'s Established<'_>,
    module: &Module,
    minted: &Rc<Minted>,
    tail: Tail<'_>,
) -> Result<(Module, Option<Entrypoint>), Error> {
    elaborate_and_zonk_entry(context, established, module, minted, tail)
        .map(|(module, entry, _)| (module, entry))
}

/// [`elaborate_and_zonk_program`] with what each declaration of `module` asked beside it: what a program's declarations are compiled by where they are a unit, kept and compiled over like any other, and the term that closes the program is elaborated in their walk.
///
/// One walk and not two, so what is reported of a program holding a refused declaration is every refusal and what the entry said beside them, as it is of a program compiled whole.
pub fn elaborate_and_zonk_entry<'s>(
    context: &mut Context<'s>,
    established: &'s Established<'_>,
    module: &Module,
    minted: &Rc<Minted>,
    tail: Tail<'_>,
) -> Result<(Module, Option<Entrypoint>, Traces), Error> {
    let (finalized, traces) = elaborate_and_zonk(context, established, module, minted, Some(tail))?;
    raise(finalized).map(|(module, entry)| (module, entry, traces))
}

/// [`elaborate_and_zonk_program`] with the erasure obligations *reported* rather than raised.
///
/// Elaboration is no less checked: everything that decides whether the program is well-typed still short-circuits. What is handed back instead of thrown is the pair of obligations `curios-cert` also decides — (T) and (V) — and the reason is that a fixture this checker refuses must still be able to reach the kernel. Raising leaves no program, so whether the kernel would have refused the same program is unobservable, and that is precisely the disagreement worth seeing: an obligation only this side enforces is the trusted base resting on an elaborator-only analysis. The two-checker fixture harness in `curios` is the caller; every other caller wants [`elaborate_and_zonk_program`].
pub fn elaborate_and_zonk_program_reporting<'s>(
    context: &mut Context<'s>,
    established: &'s Established<'_>,
    module: &Module,
    minted: &Rc<Minted>,
    tail: Tail<'_>,
) -> Result<FinalizedProgram, Error> {
    let (
        Finalized {
            module,
            entry,
            obligations,
            ..
        },
        _,
    ) = elaborate_and_zonk(context, established, module, minted, Some(tail))?;

    Ok(FinalizedProgram {
        module,
        entry,
        obligations,
    })
}

/// The walk every unit and program is elaborated by: the items, then the term `tail` closes a program with, finalized together, with what each declaration asked beside them.
fn elaborate_and_zonk<'s>(
    context: &mut Context<'s>,
    established: &'s Established<'_>,
    module: &Module,
    minted: &Rc<Minted>,
    tail: Option<Tail<'_>>,
) -> Result<(Finalized, Traces), Error> {
    curios_profile::profile!("elaborate_and_zonk_with_prelude");
    grown(|| {
        let declarations = Declarations::of(module);
        let elaborated = elaborate_module_suffix(
            context,
            Some(established),
            None,
            module,
            &declarations,
            minted,
            tail,
        )?;
        let finalized = finalize_and_check(
            context,
            elaborated.module,
            elaborated.entry,
            elaborated.held,
            elaborated.obligations,
        );
        if !elaborated.refusals.is_empty() {
            return Err(refused(elaborated.refusals, finalized));
        }
        // Nothing is merged back in. The unit's items, the unit's declarations: what a predecessor contributes is scope, and every consumer past this point takes it as such — `Globals` at the certifier, a view at erasure.
        let finalized = finalized?;
        let mut traces = elaborated.traces;
        traces.ordered(finalized.order.clone());

        Ok((finalized, traces))
    })
}

/// What a unit compiled over a baseline hands elaboration: the baseline, as an earlier text of the unit elaborated, with the places of its names; what each of its declarations asked as it elaborated, and who asked what of those traces; and what the new text's lowering made of it: the declarations its items make, the names it lowered anew, and the names it no longer declares.
///
/// The scope elaboration is handed holds the baseline as its last layer ([`Established::keeping`]), which is the caller's to arrange, as it is that the baseline was compiled after the units this one is.
pub struct Recompile<'a> {
    pub baseline: &'a Module,
    pub places: &'a Places,
    pub traces: &'a Traces,
    pub readers: &'a Readers,
    /// The declarations the new text's lowered items make.
    pub declarations: &'a Declarations,
    /// The names whose lowered item is not the baseline's own: new to the unit, or lowered again.
    pub relowered: &'a BTreeSet<Global>,
    /// The names the baseline declares and the new text's lowering does not.
    pub dropped: &'a BTreeSet<Global>,
    /// What a store holds of declarations elaborated before, where a declaration to be made may be taken from one of its records (`curios_elab::Candidating`).
    pub candidates: Option<&'a dyn Candidates>,
}

/// A unit elaborated over a baseline: its module, what each of its declarations asked and who asked what of that, and every name of a declaration elaborated again.
pub struct Recompiled {
    pub module: Module,
    /// Where `module` holds each name, where its baseline's places were handed on with the names of what the walk changed placed again: `None` where they are made the first time something asks.
    pub places: Option<Places>,
    pub traces: Traces,
    pub readers: Readers,
    /// Every name of a declaration elaborated again: every other the unit declares is kept as the baseline holds it.
    pub made: BTreeSet<Global>,
}

/// [`elaborate_and_zonk_unit`] for a unit compiled over a baseline: the walk visits what the edit could reach, a declaration is kept as the baseline holds it where nothing it asked moved, read where the baseline holds it, and every other is elaborated (`demand`). The module holds every item, the baseline's own for a declaration kept, in the order a whole compile stores it in (`Module::ordered`).
///
/// Everything decided of a declaration is decided of the ones elaborated: each decides its own vectors, totality and erasure obligations as it is published, reading a kept declaration's as the baseline left them, and a kept item is zonked and stamped already, so what is finalized is what was elaborated.
pub fn elaborate_and_zonk_unit_over<'s>(
    context: &mut Context<'s>,
    established: &'s Established<'_>,
    recompile: Recompile<'_>,
    module: &Module,
    minted: &Rc<Minted>,
) -> Result<Recompiled, Error> {
    elaborate_and_zonk_over(context, established, recompile, module, minted, None)
        .map(|(recompiled, _)| recompiled)
}

/// [`elaborate_and_zonk_entry`] for a program's declarations compiled over a baseline: the walk [`elaborate_and_zonk_unit_over`] makes, with the term `tail` closes the program with elaborated at its end, as it is where nothing is kept. The entry is elaborated on every compile: it has no name to be kept under.
pub fn elaborate_and_zonk_entry_over<'s>(
    context: &mut Context<'s>,
    established: &'s Established<'_>,
    recompile: Recompile<'_>,
    module: &Module,
    minted: &Rc<Minted>,
    tail: Tail<'_>,
) -> Result<(Recompiled, Option<Entrypoint>), Error> {
    elaborate_and_zonk_over(context, established, recompile, module, minted, Some(tail))
}

/// The walk a unit compiled over a baseline is elaborated by, with the term `tail` closes a program with where there is one.
fn elaborate_and_zonk_over<'s>(
    context: &mut Context<'s>,
    established: &'s Established<'_>,
    recompile: Recompile<'_>,
    module: &Module,
    minted: &Rc<Minted>,
    tail: Option<Tail<'_>>,
) -> Result<(Recompiled, Option<Entrypoint>), Error> {
    curios_profile::profile!("elaborate_and_zonk_unit_over");
    grown(|| {
        let over = Over {
            baseline: recompile.baseline,
            places: recompile.places,
            traces: recompile.traces,
            relowered: recompile.relowered,
            dropped: recompile.dropped,
            readers: recompile.readers,
            established,
            candidates: recompile.candidates,
        };
        let elaborated = elaborate_module_suffix(
            context,
            Some(established),
            Some(over),
            module,
            recompile.declarations,
            minted,
            tail,
        )?;
        let finalized = finalize_and_check(
            context,
            elaborated.module,
            elaborated.entry,
            elaborated.held,
            elaborated.obligations,
        );
        if !elaborated.refusals.is_empty() {
            return Err(refused(elaborated.refusals, finalized));
        }
        let Finalized {
            module: made,
            entry,
            obligations,
            ..
        } = finalized?;
        let (mut traces, made_names, readers) =
            (elaborated.traces, elaborated.made, elaborated.readers);
        let Assembled {
            module,
            places,
            order,
        } = {
            curios_profile::profile!("assemble_unit_over");
            recompile.assembled(module, &made_names, made)
        };
        traces.ordered(order.clone());

        // What was finalized holds the declarations elaborated now, so the entry's universes were held to their schemes alone: here to every declaration's, the kept ones' with them.
        if let Some(entry) = &entry {
            validate_entry_universes(&module, entry)?;
        }

        raise(Finalized {
            module,
            entry,
            obligations,
            order,
        })
        .map(|(module, entry)| {
            (
                Recompiled {
                    module,
                    places,
                    traces,
                    readers: readers.expect("a walk over a baseline keeps who asked what"),
                    made: made_names,
                },
                entry,
            )
        })
    })
}

/// What [`Recompile::assembled`] makes of a recompile's declarations: the module, in the order a module is stored in, where it holds each name, where that was handed on, and the order.
struct Assembled {
    module: Module,
    places: Option<Places>,
    order: Order,
}

impl Recompile<'_> {
    /// The declarations elaborated and the ones kept as one module, in the order a module is stored in: `made`, the items of the declarations named in `made_names`, with the baseline's own item and registry entries for every other declaration of `lowered`, and no item of a name the recompile dropped. The order is the baseline's revised by what was made, every item it does not move standing where the baseline holds it ([`Order::revised`]); where no revision resolves it, the module is put in order whole.
    ///
    /// The markers are the lowering's: every run that produces a unit dropped nothing, so every item a marker names is there.
    fn assembled(
        &self,
        lowered: &Module,
        made_names: &BTreeSet<Global>,
        made: Module,
    ) -> Assembled {
        let made = made.items.into_iter().collect::<Vec<_>>();
        let module = |items| Module {
            items,
            mounts: lowered.mounts.clone(),
            witnesses: lowered.witnesses.clone(),
            tests: lowered.tests.clone(),
        };
        if let Some(Revised {
            items,
            places,
            order,
        }) = self
            .traces
            .order()
            .revised(self.baseline, self.places, &made, self.dropped)
        {
            return Assembled {
                module: module(items),
                places,
                order,
            };
        }

        curios_profile::profile!("order_unit_over");
        let mut items = Items::from(made);
        items.extend(
            self.baseline
                .items
                .iter()
                .filter(|item| {
                    item.declared_names().first().is_some_and(|name| {
                        !made_names.contains(*name) && !self.dropped.contains(*name)
                    })
                })
                .cloned(),
        );
        let (module, order) = module(items).in_order();

        Assembled {
            module,
            places: None,
            order,
        }
    }
}
