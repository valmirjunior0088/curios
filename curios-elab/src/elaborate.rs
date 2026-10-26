mod align;
use align::*;

mod apply;
use apply::*;
pub use apply::{SlotPositions, attempt_discharge, ordinal, premise_label, retry_discharge};

mod aggregate;
use aggregate::*;

mod struct_;
use struct_::*;

mod binding;
use binding::*;
pub use binding::{embeds, is_monad, monad_shape, numeral_default, region_flex};

mod metavar;
use metavar::*;
pub use metavar::{trivially_inhabited, waits_as_reference};

mod intrinsic;
use intrinsic::*;

mod match_;
use match_::*;

mod matrix;
use matrix::*;

mod module;
pub use module::*;

#[cfg(test)]
mod intrinsic_tests;
#[cfg(test)]
mod tests;

use {
    super::{Context, Error, check, check_is_sort, elaborate_derive, expect},
    curios_core::{
        Ascribed, Free, Func, FuncType, InstanceHead, Subterm, Term, Transient, Universes, Var,
        foreign_operands, foreign_produced, former_spelling, instantiate_universes_scoped,
        intrinsic_former,
    },
    curios_utilities::{Plicity, recurse},
    std::sync::Arc,
};

/// The elaboration mode. `Infer` synthesizes a type; `Check(expected)` drives the term against a known type, hitting `expect` at each synthesizable node's turnaround and consuming `expected` directly at naturally-checked nodes (`Func`, `Tuple`, `Metavar`).
#[derive(Debug, Clone)]
pub enum Mode {
    Infer,
    Check(Term),
}

impl Mode {
    /// The expected type this mode carries — the elaboration-cache key's second component (`None` in `Infer`).
    fn expected(&self) -> Option<Term> {
        match self {
            Mode::Check(expected) => Some(expected.clone()),
            Mode::Infer => None,
        }
    }
}

pub fn elaborate(context: &mut Context, term: &Term, mode: Mode) -> Result<(Term, Term), Error> {
    // Guarded by [`recurse`]: a child position is elaborated by descending into it, so the cycle `elaborate → elaborate_apply → check → elaborate` costs native frames per link of a right-nested argument spine. Nothing in the corpus produces a deep one — a string literal's proof is constant-size — but depth is a function of the input rather than of anything anyone wrote, and this is what makes that affordable rather than fatal.
    recurse(|| {
        // Route through the elaboration cache: ground, local-free subterms — which the lowerer emits as `Rc`-shared DAGs — elaborate once per distinct node instead of once per occurrence. Placing stays outside it, since extents are excluded from `Term` equality: occurrences differing only in extent share one un-stamped entry and restamp per occurrence.
        let expected = mode.expected();
        let result = context.get_or_init_elaborated(term, expected.as_ref(), |context| {
            elaborate_subterm(context, term, mode)
        });

        // `Error::at` is first-wins, so stamping as the recursion unwinds reports the deepest node that failed.
        let (rebuilt, type_) =
            result.map_err(|error| error.at_opt(term.extent()).in_scope(context.reader_scope()))?;

        let rebuilt = match term.extent() {
            Some(extent) => rebuilt.with_extent(extent),
            None => rebuilt,
        };

        // Obligation (V)'s seed. Every settled node passes here with the type it settled at, which a walk over the finished term could only re-derive. Taken after the restamp so the recorded term is the one that reaches the module, outside the cache so a hit records too, and independent of `Mode` because `type_` is the term's type whether it was checked or inferred.
        context.record_checked(&rebuilt, &type_);

        Ok((rebuilt, type_))
    })
}

/// A term stated at a type: the type is elaborated as one, the term is checked against it, and the term is what is left — the node itself never reaches a later stage. Where a type is expected of the whole, the stated one is measured against it first, so the term is checked at a type that already carries what the position knows.
fn elaborate_ascribed(
    context: &mut Context,
    ascribed: &Ascribed,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let (type_, _) = check_is_sort(context, &ascribed.type_)?;
    if let Mode::Check(expected) = &mode {
        expect(context, term, &type_, expected)?;
    }
    let checked = check(context, &ascribed.term, type_.clone())?;

    Ok((checked, type_))
}

/// Synthesize a lambda no expectation reached, for the term that awaits its type and has read its parts ([`Context::complete`](super::Context)): the infer walk with each unannotated domain admitted as a metavariable named after its binder, pinned by the body or by whatever the settled type later unifies with, rather than refused, and the body read with no expectation, as its annotations leave it. Only a completion may ask for it — while the expectation could still gain structure, committing the lambda's written shape would guess. Deliberately outside the elaboration cache: admitting the holes is a side effect, and a cached settle result under an `Infer` key would let a plain inference of the same lambda succeed where it must refuse. The extent restamp and checked-record mirror [`elaborate`]'s tail, so the settled node reaches the module exactly as an ordinarily elaborated one does.
pub fn elaborate_func_settle(
    context: &mut Context,
    func: &Func,
    term: &Term,
) -> Result<(Term, Term), Error> {
    let (rebuilt, type_) =
        elaborate_func_infer(context, &func.telescope, &func.plicities(), Some(term))
            .map_err(|error| error.at_opt(term.extent()).in_scope(context.reader_scope()))?;
    let rebuilt = match term.extent() {
        Some(extent) => rebuilt.with_extent(extent),
        None => rebuilt,
    };
    context.record_checked(&rebuilt, &type_);
    Ok((rebuilt, type_))
}

fn elaborate_subterm(
    context: &mut Context,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // A checked node declared in another monad than the region's is lifted before it elaborates, as a `!` action is — a region's tail is the second place an embedding is written, and the same declared edge carries it.
    if let Mode::Check(expected) = &mode
        && let Some(lifted) = lift_on_check(context, term, expected)?
    {
        return elaborate(context, &lifted, mode);
    }

    // A bare variable whose type leads with a hidden binder waits where its expectation has not decided whether that binder is inserted.
    if let Mode::Check(expected) = &mode
        && waits_as_reference(context, term, expected)?
        && context.may_park()
    {
        return park_checking(context, term, expected);
    }

    // An intrinsic former written by its name and applied in full is elaborated as its node.
    if let Some(node) = written_intrinsic_former(term) {
        return elaborate(context, &node, mode);
    }

    // Synthesizable nodes compute their type and hit the `expect` turnaround in `Check` mode; naturally-checked nodes (and the mode-propagating `Let`/`Rec`) consume `mode` directly and return early. Every arm returns the rebuilt term — binders re-closed, lambda domains solved.
    let (rebuilt, type_) = match &**term {
        Subterm::Sort(_, level) => (
            term.clone(),
            Term::type_at(level.succ().map_err(Error::from)?),
        ),
        // A host call, elaborated through the same walk an intrinsic is: each operand against what `foreign_operands` demands of it, and the result — unit, a bare value, or a named record, inside `Io` — read off the same row. The arity is an invariant of construction (the prelude builds the argument list from the same signature).
        Subterm::Foreign(function, declared, args) => {
            let syntax = context.syntax();
            let demands = foreign_operands(function, &syntax);

            assert_eq!(
                args.len(),
                demands.len(),
                "{} operand count does not match its signature",
                function.name()
            );

            let elaborated = context.awaiting(|context| {
                let mut elaborated = Vec::with_capacity(args.len());
                for (arg, demand) in args.iter().zip(&demands) {
                    elaborated.push(elaborate_demand(context, arg, demand, || term.clone())?);
                }
                Ok(elaborated)
            })?;

            // Read off the elaborated operands, so a diverging call yields the type its own operand was elaborated to.
            let type_ = foreign_produced(function, &elaborated, &syntax, |label| {
                context.fresh(Some(label))
            });

            (
                Term::foreign(Arc::clone(function), *declared, elaborated),
                type_,
            )
        }
        Subterm::Instance(instance) => {
            let type_ = match &instance.head {
                InstanceHead::RecProj(group, index) => {
                    context
                        .universes_mut()
                        .instantiate_at(group.universe_context(), &instance.universes, None)
                        .map_err(Error::from)?;

                    instantiate_universes_scoped(&group.member_type(*index), &instance.universes)
                        .map_err(Error::from)?
                }
                InstanceHead::Var(var) => context
                    .instantiate_assumption_at(var.unwrap(), &instance.universes, term.extent())?
                    .ok_or_else(|| context.unbound_variable(instance.head.to_term()))?,
            };
            (term.clone(), type_)
        }
        Subterm::Intrinsic(intrinsic) => {
            return context.awaiting(|context| elaborate_intrinsic(context, term, intrinsic, mode));
        }
        Subterm::Match(m) => return elaborate_match(context, m, term, mode),
        Subterm::FuncType(ft) => elaborate_func_type(context, ft)?,
        Subterm::Apply(apply) => {
            return context.awaiting(|context| elaborate_apply(context, apply, term, mode));
        }
        Subterm::TupleType(tt) => elaborate_tuple_type(context, tt, term.extent())?,
        Subterm::Proj(proj) => elaborate_proj(context, proj)?,
        Subterm::Let(let_) => return elaborate_let(context, let_, mode),
        Subterm::Rec(rec) => return elaborate_rec(context, rec, mode),
        Subterm::Var(var) => match context.instantiate_assumption(var.unwrap(), term.extent())? {
            Some((type_, universes)) => {
                let rebuilt =
                    if let Some(former) = spelled_former(context, var, &type_, &universes)? {
                        match term.extent() {
                            Some(extent) => former.with_extent(extent),
                            None => former,
                        }
                    } else if universes.is_empty() {
                        term.clone()
                    } else {
                        // The occurrence's extent moves onto the wrapping instance: the typed head carries none.
                        let instance = Term::instance(InstanceHead::Var(var.clone()), universes);
                        match term.extent() {
                            Some(extent) => instance.with_extent(extent),
                            None => instance,
                        }
                    };
                (rebuilt, type_)
            }
            None => return Err(context.unbound_variable(Term::var(var.clone()))),
        },
        Subterm::Func(func) => return elaborate_func(context, func, term, mode),
        Subterm::Tuple(tuple) => return elaborate_tuple(context, tuple, term, mode),
        Subterm::Transient(Transient::Infix(infix)) => {
            return context.awaiting(|context| elaborate_infix(context, infix, term, mode));
        }
        Subterm::Transient(Transient::NumLit(num_lit)) => {
            return elaborate_num_lit(context, num_lit, term, mode);
        }
        Subterm::Transient(Transient::Bang(bang)) => {
            return elaborate_bang(context, bang, term, mode);
        }
        Subterm::Transient(Transient::Derive) => return elaborate_derive(context, term, mode),
        Subterm::Transient(Transient::Ascribed(ascribed)) => {
            return elaborate_ascribed(context, ascribed, term, mode);
        }
        Subterm::Transient(Transient::Matrix(matrix)) => {
            return elaborate_matrix(context, matrix, term, mode);
        }
        Subterm::Metavar(metavar) => return elaborate_metavar(context, metavar, term, mode),
        Subterm::InductType(ut) => elaborate_induct_type(context, ut, term.extent())?,
        Subterm::Variant(uc) => elaborate_variant(context, uc, term.extent())?,
        Subterm::StructType(st) => elaborate_struct_type(context, st, term)?,
        Subterm::Struct(s) => {
            context.awaiting(|context| elaborate_struct(context, s, term, &mode))?
        }
    };

    if let Mode::Check(expected) = &mode {
        let (rebuilt, type_) = insert_implicits_on_check(context, term, rebuilt, type_, expected)?;
        expect(context, term, &type_, expected)?;
        return Ok((rebuilt, type_));
    }

    Ok((rebuilt, type_))
}

/// An intrinsic type former written by its name and applied in full, every argument explicit, or a closed one written bare, as the node it is ([`intrinsic_former`]): what elaboration reads in its place. Read before it is elaborated, so its operands are checked against the node's own signature and no level of its name's instance is minted: `/sys/List/List.{c}(Nat)` stands at `Type c` where `ListType(Nat)` stands at its operand's level, and an instance minted and then dropped would leave its levels to close on nothing. `None` for any other term.
fn written_intrinsic_former(term: &Term) -> Option<Term> {
    let (head, operands) = match &**term {
        Subterm::Var(var) => (var, Vec::new()),
        Subterm::Apply(apply)
            if apply
                .arguments
                .iter()
                .all(|argument| argument.plicity == Plicity::Explicit) =>
        {
            let Subterm::Var(var) = &*apply.head else {
                return None;
            };
            (var, apply.params().cloned().collect())
        }
        _ => return None,
    };
    let node = Term::intrinsic(intrinsic_former(head.as_free()?.as_global()?, &operands)?);

    Some(match term.extent() {
        Some(extent) => node.with_extent(extent),
        None => node,
    })
}

/// What a type former's name is elaborated to, at an occurrence instantiated at `universes` whose type is `type_`: a nominal former's body as its entry spells it ([`former_spelling`]) at those levels — its η-form, one lambda to a call, or the node where it takes nothing — and an intrinsic former that takes an operand, written bare, as the lambda over that former at the domain its name's type states. Applied over its call, either is made as what the call reduces to ([`curios_core::former_eta`]), so no elaborated term holds a former's name. `None` for a name that is no former's.
///
/// The entry is read where the name's type already was, and no body is: a body read is a dependency the incremental record keeps.
fn spelled_former(
    context: &mut Context,
    var: &Var,
    type_: &Term,
    universes: &Universes,
) -> Result<Option<Term>, Error> {
    let Some(global) = var.as_free().and_then(Free::as_global).copied() else {
        return Ok(None);
    };
    // Spelled where the entry stands, and nothing of it copied: a record's entry holds its fields, which its former does not read.
    let spelled = match (context.induct_decl(&global), context.struct_decl(&global)) {
        (Some(family), _) => Some(former_spelling(
            global,
            &family.universe_context,
            &family.arity,
            &family.result_sort,
            true,
        )),
        (None, Some(record)) => Some(former_spelling(
            global,
            &record.universe_context,
            &record.arity,
            &record.result_sort,
            false,
        )),
        (None, None) => None,
    };
    if let Some((_, body)) = spelled {
        return Ok(Some(match universes.is_empty() {
            true => body,
            false => instantiate_universes_scoped(&body, universes).map_err(Error::from)?,
        }));
    }

    // An intrinsic former over one operand, its name's type a function of one parameter into a sort.
    let Subterm::FuncType(FuncType { telescope, .. }) = &**type_ else {
        return Ok(None);
    };
    if telescope.len() != 1 || !matches!(&**telescope.terminal(), Subterm::Sort(..)) {
        return Ok(None);
    }
    let Some((hint, domain)) = telescope.cursor().entry() else {
        return Ok(None);
    };
    let binder = context.fresh(hint);
    let Some(former) = intrinsic_former(&global, &[Term::free_var(&binder)]) else {
        return Ok(None);
    };

    Ok(Some(Term::func_marked(
        [(Plicity::Explicit, binder, domain)],
        Term::intrinsic(former),
    )))
}
