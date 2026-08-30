mod apply;
pub(crate) use apply::premise_label;
use apply::*;

mod aggregate;
use aggregate::*;

mod struct_;
use struct_::*;

mod binding;
use binding::*;

mod metavar;
use metavar::*;

mod intrinsic;
use intrinsic::*;

mod match_;
use match_::*;

mod module;
pub use module::*;

#[cfg(test)]
mod tests;

use {
    super::{
        Context, Error, ParkedWork, attempt_witness_goal, blocked_on_metavar, check, expect,
        reduce_with, sort_term, transitively_ground,
    },
    curios_core::{
        Apply, Bang, Bound, Field, Free, Func, FuncType, ImplicitOrigin, InductType, Infix,
        InstanceHead, Intrinsic, Let, Metavar, MetavarId, MetavarOrigin, Nat, NumLit, One, Proj,
        Rec, Scope, Struct, StructDecl, StructEntry, StructType, Subterm, Telescope, Term,
        Transient, Tuple, TupleType, Variant, WitnessOrigin, instantiate_universe_levels_scoped,
        wire_term,
    },
    curios_num::{Floating, Integer},
    curios_utilities::{InfixOp, Plicity, recurse},
    std::{
        collections::{BTreeSet, VecDeque},
        sync::Arc,
    },
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

pub(crate) fn elaborate(
    context: &mut Context,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // Guarded by [`recurse`]: a child position is elaborated by descending into it, so the cycle `elaborate → elaborate_apply → check → elaborate` costs native frames per link of a right-nested argument spine. Nothing in the corpus produces one today — a string literal's proof became constant-size — but depth is a function of the input rather than of anything anyone wrote, and this is what makes that affordable rather than fatal.
    recurse(|| {
        // Route through the elaboration cache: ground, local-free subterms — which the lowerer emits as `Rc`-shared DAGs — elaborate once per distinct node instead of once per occurrence. Span stamping stays outside it, since spans are excluded from `Term` equality: occurrences differing only in span share one un-stamped entry and restamp per occurrence.
        let expected = mode.expected();
        let result = context.get_or_init_elaborated(term, expected.as_ref(), |context| {
            elaborate_subterm(context, term, mode)
        });

        // `Error::at` is first-wins, so stamping as the recursion unwinds reports the deepest node that failed.
        let (rebuilt, type_) = result.map_err(|error| error.at_opt(term.span()))?;

        let rebuilt = match term.span() {
            Some(span) => rebuilt.with_span(span),
            None => rebuilt,
        };

        // Obligation (V)'s seed. Every settled node passes here with the type it settled at, which is the judgment `reach.rs` used to re-derive from the finished term. Taken after the restamp so the recorded term is the one that reaches the module, outside the cache so a hit records too, and independent of `Mode` because `type_` is the term's type whether it was checked or inferred.
        context.record_checked(&rebuilt, &type_);

        Ok((rebuilt, type_))
    })
}

fn elaborate_subterm(
    context: &mut Context,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // Synthesizable nodes compute their type and hit the `expect` turnaround in `Check` mode; naturally-checked nodes (and the mode-propagating `Let`/`Rec`) consume `mode` directly and return early. Every arm returns the rebuilt term — binders re-closed, lambda domains solved.
    let (rebuilt, type_) = match &**term {
        Subterm::Type(level) => (
            term.clone(),
            Term::type_at(level.succ().map_err(Error::from)?),
        ),
        Subterm::Prop => (term.clone(), Term::type_ground()),
        // A store-described host call: each operand checks against its wire type, and the result shape (unit, bare value, named record) is read off the signature. The arity is an invariant of construction (the prelude builds the argument list from the same signature).
        Subterm::Foreign(function, args) => {
            let signature = &function.signature;

            assert_eq!(
                args.len(),
                signature.params.len(),
                "{} operand count does not match its signature",
                function.name
            );

            let mut elaborated = Vec::with_capacity(args.len());
            for (arg, (_, wire_type)) in args.iter().zip(&signature.params) {
                elaborated.push(elaborate(context, arg, Mode::Check(wire_term(wire_type)))?.0);
            }

            let result = match signature.results.as_slice() {
                [] => Term::tuple_type_unit(),
                [(_, wire_type)] => wire_term(wire_type),
                results => Term::tuple_type(
                    results
                        .iter()
                        .map(|(label, wire_type)| {
                            (context.fresh(Some(label)), wire_term(wire_type))
                        })
                        .collect::<Vec<_>>(),
                ),
            };

            (
                Term::foreign(Arc::clone(function), elaborated),
                Term::intrinsic(Intrinsic::io_type(result)),
            )
        }
        Subterm::Instance(instance) => {
            let type_ = match &instance.head {
                InstanceHead::RecProj(group, index) => {
                    context
                        .universes_mut()
                        .instantiate_at(group.universe_context(), &instance.levels)
                        .map_err(Error::from)?;

                    instantiate_universe_levels_scoped(&group.member_type(*index), &instance.levels)
                        .map_err(Error::from)?
                }
                InstanceHead::Var(var) => context
                    .instantiate_assumption_at(var.unwrap(), &instance.levels)?
                    .ok_or_else(|| Error::unbound_variable(instance.head.to_term()))?,
            };
            (term.clone(), type_)
        }
        Subterm::Intrinsic(intrinsic) => {
            return elaborate_intrinsic(context, term, intrinsic, mode);
        }
        Subterm::Match(m) => return elaborate_match(context, m, term, mode),
        Subterm::FuncType(ft) => elaborate_func_type(context, ft)?,
        Subterm::Apply(apply) => return elaborate_apply(context, apply, term, mode),
        Subterm::TupleType(tt) => elaborate_tuple_type(context, tt)?,
        Subterm::Proj(proj) => elaborate_proj(context, proj)?,
        Subterm::Let(let_) => return elaborate_let(context, let_, mode),
        Subterm::Rec(rec) => return elaborate_rec(context, rec, mode),
        Subterm::Var(var) => match context.instantiate_assumption(var.unwrap())? {
            Some((type_, levels)) => {
                let rebuilt = if levels.is_empty() {
                    term.clone()
                } else {
                    // The occurrence's span moves onto the wrapping instance: the typed head carries none.
                    let instance = Term::instance(InstanceHead::Var(var.clone()), levels);
                    match term.span() {
                        Some(span) => instance.with_span(span),
                        None => instance,
                    }
                };
                (rebuilt, type_)
            }
            None => return Err(Error::unbound_variable(Term::var(var.clone()))),
        },
        Subterm::Func(func) => return elaborate_func(context, func, term, mode),
        Subterm::Tuple(tuple) => return elaborate_tuple(context, tuple, term, mode),
        Subterm::Transient(Transient::Infix(infix)) => {
            return elaborate_infix(context, infix, term, mode);
        }
        Subterm::Transient(Transient::NumLit(num_lit)) => {
            return elaborate_num_lit(context, num_lit, term, mode);
        }
        Subterm::Transient(Transient::Bang(bang)) => {
            return elaborate_bang(context, bang, term, mode);
        }
        Subterm::Metavar(metavar) => return elaborate_metavar(context, metavar, term, mode),
        Subterm::InductType(ut) => elaborate_induct_type(context, ut)?,
        Subterm::Variant(uc) => elaborate_variant(context, uc, term)?,
        Subterm::StructType(st) => elaborate_struct_type(context, st, term)?,
        Subterm::Struct(s) => elaborate_struct(context, s, term, &mode)?,
    };

    if let Mode::Check(expected) = &mode {
        let (rebuilt, type_) = insert_implicits_on_check(context, term, rebuilt, type_, expected)?;
        expect(context, term, &type_, expected)?;
        return Ok((rebuilt, type_));
    }

    Ok((rebuilt, type_))
}
