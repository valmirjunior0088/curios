//! Whether a type has one inhabitant by its shape, which conversion reads where eta cannot be fired: between two neutrals at a nominal struct, and at the type a lookup gives two sides their position handed none.

use {
    super::{Binders, Sort, instantiate_bound_at},
    crate::{Context, reduce_forced},
    curios_analysis::struct_reaches_itself,
    curios_core::{
        Cost, Free, FuncType, Global, ReduceError, StructType, Subterm, Telescope, Term, TupleType,
    },
    curios_utilities::recurse,
};

/// Whether `type_` has one inhabitant by its shape: a proposition, the empty Σ, a Σ or a nominal struct whose every field's type has one, or a function type whose codomain has one.
///
/// It is what irrelevance and eta derive, read off the type: any two inhabitants of such a type convert by them, down to each field. Read off the type it poses no problem about the two sides, so it answers where eta cannot be fired. `curios-cert`'s classifier of the same name carries the argument, and this one answers the same question so that a program the two disagree on does not exist.
///
/// It ends at a struct that reaches itself: one met again while it is being judged answers no where its declaration reaches itself, and is judged again where it was met through a parameter. Every other type is forced as eta by the goal's type forces it, and bounded by the budget as that is. A field's type is judged under the fields before it, opened at binders, and one that still waits on a metavariable answers no.
pub(super) fn one_inhabitant(
    context: &mut Context,
    binders: &Binders,
    type_: &Term,
) -> Result<bool, ReduceError> {
    curios_profile::profile!("convert::one_inhabitant");
    inhabited_once(context, binders, type_, &mut Vec::new(), &mut Vec::new())
}

/// [`one_inhabitant`], under the binders the problem is posed under and those a surrounding telescope opened, and inside the structs in `entered`, whose fields this type was reached through.
///
/// A function type is a proposition where its codomain is one and a record where every field is, so each is read through to what it ends in, and a sort is asked only of what is neither: no part of the type is walked twice.
fn inhabited_once(
    context: &mut Context,
    binders: &Binders,
    type_: &Term,
    opened: &mut Vec<(Free, Term)>,
    entered: &mut Vec<Global>,
) -> Result<bool, ReduceError> {
    recurse(|| {
        context.spend(Cost::STEP)?;

        let at = reduce_forced(context, type_.clone())?;
        let proposition = |context: &mut Context, opened: &mut Vec<(Free, Term)>| {
            Ok(matches!(
                Sort::of_under(context, binders, opened, &at)?,
                Sort::Prop
            ))
        };

        match &*at {
            Subterm::FuncType(FuncType { telescope, .. }) => {
                let mark = opened.len();
                let (_, codomain) = telescope.clone().walk_producing(|_, hint, domain| {
                    let binder = context.fresh(hint);
                    let variable = Term::free_var(&binder);
                    opened.push((binder, domain));
                    Ok(variable)
                })?;
                let verdict = inhabited_once(context, binders, &codomain, opened, entered);
                opened.truncate(mark);

                verdict
            }
            Subterm::TupleType(TupleType { telescope, .. }) => {
                fields_inhabited_once(context, binders, telescope.clone(), opened, entered)
            }
            Subterm::StructType(StructType {
                name,
                universes,
                params,
            }) => {
                if proposition(context, opened)? {
                    return Ok(true);
                }
                if entered.contains(name) && struct_reaches_itself(context, name) {
                    return Ok(false);
                }
                let Some(declaration) = context.struct_decl(name).cloned() else {
                    return Ok(false);
                };
                if declaration.param_count() != params.len() {
                    return Ok(false);
                }
                let arity = instantiate_bound_at(
                    context,
                    &declaration.universe_context,
                    &declaration.arity,
                    universes,
                )?;
                let fields = arity.open(&params.iter().collect::<Vec<_>>());

                entered.push(*name);
                let verdict = fields_inhabited_once(context, binders, fields, opened, entered);
                entered.pop();

                verdict
            }
            _ => proposition(context, opened),
        }
    })
}

/// Whether every field of `telescope` has one inhabitant, each judged under the fields before it.
fn fields_inhabited_once(
    context: &mut Context,
    binders: &Binders,
    telescope: Telescope<()>,
    opened: &mut Vec<(Free, Term)>,
    entered: &mut Vec<Global>,
) -> Result<bool, ReduceError> {
    let mark = opened.len();
    let mut every = true;
    telescope.walk_producing(|_, hint, field| {
        every = every && inhabited_once(context, binders, &field, opened, entered)?;
        let binder = context.fresh(hint);
        let variable = Term::free_var(&binder);
        opened.push((binder, field));
        Ok(variable)
    })?;
    opened.truncate(mark);

    Ok(every)
}
