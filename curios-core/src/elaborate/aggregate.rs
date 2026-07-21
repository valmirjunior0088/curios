use super::*;

pub(super) fn elaborate_tuple_type(
    context: &mut Context,
    tt: &TupleType,
) -> Result<(Term, Term), Error> {
    fn walk(
        context: &mut Context,
        tele: Telescope<()>,
        fields: &mut Vec<(String, Term)>,
    ) -> Result<(), Error> {
        match tele {
            Telescope::Done(_) => Ok(()),
            Telescope::Cons(ty, rest) => {
                let field = check(context, &ty, Term::type_())?;
                let name = context.fresh(rest.first_label());
                let x = Term::free_var(&name);
                // The *rebuilt* field type, as in `elaborate_func_type`.
                context.assume(&name, &field);
                fields.push((name, field));
                walk(context, rest.open(&[&x]), fields)
            }
        }
    }

    // Labels are part of the type's identity and the target of `.label`
    // resolution, so they must be unique and survive the rebuild verbatim
    // (the walk gensyms its binders to keep nested frames collision-free;
    // `relabel` restores the written names afterwards).
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

    // A tuple type's sort: `Prop` when every field is a proposition (`{}` is the
    // base case), `Type` otherwise — so a record of proofs checks against `Prop`.
    let sort = sort_term(context, &rebuilt)?;
    Ok((rebuilt, sort))
}

pub(super) fn elaborate_proj(context: &mut Context, proj: &Proj) -> Result<(Term, Term), Error> {
    let Proj { head, field } = proj;

    let (head, head_type) = elaborate(context, head, Mode::Infer)?;
    let head_type = reduce_with(context, &head_type)?;

    // Both tuples and structs project; the field telescope is the tuple type's
    // own, or the struct's (instantiated at the head type's parameters). A struct
    // additionally enforces representation privacy here (§7).
    let telescope = match &*head_type {
        Subterm::TupleType(TupleType { telescope }) => telescope.clone(),
        Subterm::StructType(StructType { name, params }) => {
            let Some(structure) = context.structure(name).cloned() else {
                return Err(Error::unbound_variable(Term::free_var(name)));
            };

            // The use-site module is the enclosing item's qualifier prefix
            // (`Context::island`, set per item by `elaborate_module`);
            // the island model grants no descendant access, so the check is
            // exact qualifier equality.
            if context.privacy_enforced()
                && !structure.rep_public
                && *context.island() != structure.module
            {
                let field = match field {
                    Field::Index(index) => index.to_string(),
                    Field::Label(label) => label.clone(),
                };
                return Err(Error::private_field(name.clone(), field));
            }

            structure.fields_at(params)
        }
        other => return Err(Error::not_a_tuple(other.clone())),
    };

    // A label projection resolves to its position here and is rebuilt
    // positionally — nothing below elaboration ever sees a label. Lookup is
    // unambiguous because duplicate labels are rejected when the tuple type
    // itself elaborates.
    let index = match field {
        Field::Index(index) => *index,
        Field::Label(label) => {
            let labels = telescope.labels();
            match labels.iter().position(|l| l == label) {
                Some(index) => index,
                None => {
                    // A concept's superclass fields carry a minted internal
                    // label and are not projectable by name — never surface
                    // them among the available fields.
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

/// Type a primitive inductive type against its registry entry: the parameters
/// and indices are checked pointwise (dependently) as one flat argument list
/// through the declaration's full index telescope (whose leading binders are
/// the parameters), and the whole node is a `Type`.
pub(super) fn elaborate_inductive_type(
    context: &mut Context,
    ut: &InductiveType,
) -> Result<(Term, Term), Error> {
    let InductiveType {
        name,
        params,
        indices,
    } = ut;

    let Some(inductive) = context.inductive(name).cloned() else {
        return Err(Error::unbound_variable(Term::free_var(name)));
    };

    let args: Vec<Term> = params.iter().chain(indices.iter()).cloned().collect();

    if args.len() != inductive.indices.len() {
        return Err(Error::wrong_number_of_arguments(
            inductive.indices.len(),
            args.len(),
        ));
    }

    let (elaborated, ()) = check_args_against(context, inductive.indices, &args)?;

    Ok((
        Term::inductive_type(
            name,
            elaborated[..params.len()].iter().cloned(),
            elaborated[params.len()..].iter().cloned(),
        ),
        inductive.result_sort,
    ))
}

/// Type a primitive constructor value against its registry signature: the
/// instantiated parameters and the payload are checked through the
/// constructor's full telescope, whose terminal gives the constructed
/// `InductiveType`.
pub(super) fn elaborate_variant(
    context: &mut Context,
    uc: &Variant,
    term: &Term,
) -> Result<(Term, Term), Error> {
    let Variant {
        name,
        params,
        tag,
        payload,
    } = uc;

    let Some(inductive) = context.inductive(name).cloned() else {
        return Err(Error::unbound_variable(Term::free_var(name)));
    };

    if context.privacy_enforced() && !inductive.rep_public && *context.island() != inductive.module
    {
        return Err(Error::private_representation(name.clone()));
    }

    let Some(signature) = inductive.constructors.get(tag).map(|c| c.telescope.clone()) else {
        return Err(Error::match_case_missing(term.clone(), tag.clone()));
    };

    let args: Vec<Term> = params.iter().chain(payload.iter()).cloned().collect();

    if args.len() != signature.len() {
        return Err(Error::wrong_number_of_arguments(
            signature.len(),
            args.len(),
        ));
    }

    let (elaborated, output) = check_args_against(context, signature, &args)?;

    let rebuilt = Term::variant(
        name,
        elaborated[..params.len()].iter().cloned(),
        tag.clone(),
        elaborated[params.len()..].iter().cloned(),
    );

    Ok((rebuilt, output))
}
