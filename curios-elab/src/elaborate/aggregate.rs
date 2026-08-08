use super::*;
use curios_core::{SelfReference, stamp_declaration_instance};

pub(super) fn elaborate_tuple_type(
    context: &mut Context,
    tt: &TupleType,
) -> Result<(Term, Term), Error> {
    fn walk(
        context: &mut Context,
        tele: Telescope<()>,
        fields: &mut Vec<(Free, Term)>,
    ) -> Result<(), Error> {
        match tele {
            Telescope::Done(_) => Ok(()),
            Telescope::Cons(ty, rest) => {
                let field = crate::check_is_sort(context, &ty)?.0;
                let name = context.fresh(rest.first_hint());
                let x = Term::free_var(&name);
                // The *rebuilt* field type, as in `elaborate_func_type`.
                context.assume(&name, &field);
                fields.push((name, field));
                walk(context, rest.open(&[&x]), fields)
            }
        }
    }

    // Labels are part of the type's identity and the target of `.label` resolution, so they must be unique and survive the rebuild verbatim (the walk gensyms its binders to keep nested frames collision-free; `relabel` restores the written names afterwards).
    let source_labels = tt.telescope.labels();
    for (position, label) in source_labels.iter().enumerate() {
        if !label.is_empty() && source_labels[..position].contains(label) {
            return Err(Error::duplicate_tuple_label(label.to_string()));
        }
    }

    let mut fields = Vec::new();
    context.with_frame(|context| walk(context, tt.telescope.clone(), &mut fields))?;

    let telescope = Telescope::build(fields, ()).relabel(&source_labels);

    let rebuilt: Term = Subterm::TupleType(TupleType { telescope }).into();

    // A tuple type's sort: `Prop` when every field is a proposition (`{}` is the base case), `Type` otherwise — so a record of proofs checks against `Prop`.
    let sort = sort_term(context, &rebuilt)?;
    Ok((rebuilt, sort))
}

pub(super) fn elaborate_proj(context: &mut Context, proj: &Proj) -> Result<(Term, Term), Error> {
    let Proj { head, field } = proj;

    let (head, head_type) = elaborate(context, head, Mode::Infer)?;
    let head_type = reduce_with(context, &head_type)?;

    // Both tuples and structs project; the field telescope is the tuple type's own, or the struct's (instantiated at the head type's parameters). A struct additionally enforces representation privacy here.
    let telescope = match &*head_type {
        Subterm::TupleType(TupleType { telescope }) => telescope.clone(),
        Subterm::StructType(StructType {
            name,
            universes,
            params,
        }) => {
            let Some(struct_decl) = context.struct_decl(name).cloned() else {
                return Err(Error::unknown_declaration(name.symbol()));
            };
            let arity = context.instantiate_universe_bound_at(
                &struct_decl.universe_context,
                &struct_decl.arity,
                universes,
            )?;

            // The use-site module is the enclosing item's qualifier prefix (`Context::island`, set per item by `elaborate_module`). A private representation is transparent within its declaring module's subtree, so the check is containment, not equality: the declaring module and its descendants may open it, its ancestors and siblings may not.
            if !struct_decl.rep_public
                && context
                    .island()
                    .is_some_and(|island| !island.is_within(&struct_decl.module))
            {
                let field = match field {
                    Field::Index(index) => index.to_string(),
                    Field::Label(label) => label.clone(),
                };
                return Err(Error::private_field(name.symbol(), field));
            }

            arity.open(&params.iter().collect::<Vec<_>>())
        }
        other => return Err(Error::not_a_tuple(other.clone())),
    };

    // A label projection resolves to its position here and is rebuilt positionally — nothing below elaboration ever sees a label. Lookup is unambiguous because duplicate labels are rejected when the tuple type itself elaborates.
    let index = match field {
        Field::Index(index) => *index,
        Field::Label(label) => {
            let labels = telescope.labels();
            match labels.iter().position(|l| l == label) {
                Some(index) => index,
                None => {
                    // A concept's superclass fields carry a minted internal label and are not projectable by name — never surface them among the available fields.
                    let supers: Vec<usize> = match &*head_type {
                        Subterm::StructType(StructType { name, .. }) => context
                            .concept(name)
                            .map(|concept| concept.supers.iter().map(|(i, _)| *i).collect())
                            .unwrap_or_default(),
                        _ => Vec::new(),
                    };
                    return Err(Error::unknown_tuple_label(
                        label.clone(),
                        labels
                            .iter()
                            .enumerate()
                            .filter(|(i, l)| !l.is_empty() && !supers.contains(i))
                            .map(|(_, l)| l.to_string())
                            .collect(),
                    ));
                }
            }
        }
    };

    if index >= telescope.len() {
        return Err(Error::tuple_index_out_of_bounds(index, telescope.len()));
    }

    let field_type = telescope
        .nth(index, |j| Term::proj(head.clone(), j))
        .expect("index in range");

    Ok((Term::proj(head, index), field_type))
}

/// Type an intrinsic inductive type against its registry entry: the parameters are checked pointwise (dependently) through the declaration's arity, then the indices through the telescope that arity terminates in, and the whole node is a `Type`.
pub(super) fn elaborate_induct_type(
    context: &mut Context,
    ut: &InductType,
) -> Result<(Term, Term), Error> {
    let InductType {
        name,
        universes: written_universes,
        params,
        indices,
    } = ut;

    let Some(induct_decl) = context.induct_decl(name).cloned() else {
        return Err(Error::unknown_declaration(name.symbol()));
    };
    let (arity, result_sort, universes) = if written_universes.is_empty() {
        let (arity, universes) = context
            .instantiate_universe_bound(&induct_decl.universe_context, &induct_decl.arity)?;
        let result_sort = instantiate_universe_levels_scoped(&induct_decl.result_sort, &universes)
            .map_err(Error::from)?;
        (arity, result_sort, universes)
    } else {
        let induct_decl = context.instantiate_induct_decl_at(&induct_decl, written_universes)?;
        (
            induct_decl.arity,
            induct_decl.result_sort,
            written_universes.clone(),
        )
    };

    if params.len() != arity.len() {
        return Err(Error::wrong_number_of_arguments(arity.len(), params.len()));
    }

    // The parameters against the arity, then the indices against the telescope it terminates in — already scoped under the parameters just checked.
    let (elaborated_params, index_telescope) = check_args_against(context, arity, params)?;

    if indices.len() != index_telescope.len() {
        return Err(Error::wrong_number_of_arguments(
            index_telescope.len(),
            indices.len(),
        ));
    }

    let (elaborated_indices, ()) = check_args_against(context, index_telescope, indices)?;

    Ok((
        Term::induct_type_at(
            name.clone(),
            universes,
            elaborated_params,
            elaborated_indices,
        ),
        result_sort,
    ))
}

/// Type an intrinsic constructor value against its registry signature: the instantiated parameters and the payload are checked through the constructor's full telescope, whose terminal gives the constructed `InductType`.
pub(super) fn elaborate_variant(
    context: &mut Context,
    uc: &Variant,
    term: &Term,
) -> Result<(Term, Term), Error> {
    let Variant {
        name,
        universes: written_universes,
        params,
        tag,
        payload,
    } = uc;

    let Some(induct_decl) = context.induct_decl(name).cloned() else {
        return Err(Error::unknown_declaration(name.symbol()));
    };

    if !induct_decl.rep_public
        && context
            .island()
            .is_some_and(|island| !island.is_within(&induct_decl.module))
    {
        return Err(Error::private_representation(name.symbol()));
    }

    let (signature, universes) = if written_universes.is_empty() {
        let Some(signature) = induct_decl.constructor(tag).map(|c| c.telescope.clone()) else {
            return Err(Error::match_case_missing(term.clone(), tag.clone()));
        };
        let (signature, universes) =
            context.instantiate_universe_bound(&induct_decl.universe_context, &signature)?;
        (signature, universes)
    } else {
        let induct_decl = context.instantiate_induct_decl_at(&induct_decl, written_universes)?;
        let Some(signature) = induct_decl
            .constructor(tag)
            .map(|constructor| constructor.telescope.clone())
        else {
            return Err(Error::match_case_missing(term.clone(), tag.clone()));
        };
        (signature, written_universes.clone())
    };

    let args: Vec<Term> = params.iter().chain(payload.iter()).cloned().collect();

    if args.len() != signature.len() {
        return Err(Error::wrong_number_of_arguments(
            signature.len(),
            args.len(),
        ));
    }

    let (elaborated, targets) = check_args_against(context, signature, &args)?;

    // The constructed type, rebuilt from the targets the signature states and what the declaration already fixes: this family, at the parameters this occurrence supplied.
    let output = stamp_declaration_instance(
        &Term::induct_type_at(
            name.clone(),
            universes.clone(),
            elaborated[..params.len()].iter().cloned(),
            targets,
        ),
        &BTreeSet::from([name.clone()]),
        SelfReference::Free,
        &universes,
    );
    let rebuilt = Term::variant_at(
        name.clone(),
        universes,
        elaborated[..params.len()].iter().cloned(),
        tag.clone(),
        elaborated[params.len()..].iter().cloned(),
    );

    Ok((rebuilt, output))
}
