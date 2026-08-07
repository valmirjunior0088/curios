mod apply;
use apply::*;

mod aggregate;
use aggregate::*;

mod struct_;
use struct_::*;

mod binding;
use binding::*;

mod metavar;
use metavar::*;

mod prim;
use prim::*;

mod match_;
use match_::*;

mod module;
pub use module::*;

#[cfg(test)]
mod tests;

use {
    super::{
        Context, ElabProbe, ElaborationStamp, Error, ParkedWork, attempt_witness_goal,
        blocked_on_metavar, check, expect, reduce_with, sort_term, transitively_ground,
    },
    curios_base::{Flt, Int, NumOp, Plicity, Span},
    curios_core::{
        Apply, Bound, Field, Free, Func, FuncType, ImplicitOrigin, InductType, Infix, Let, MetaId,
        Metavar, MetavarOrigin, Nat, NumLit, One, Prim, Proj, Rec, Scope, Struct, StructDecl,
        StructEntry, StructType, Subterm, Telescope, Term, Tuple, TupleType, Variant,
        WitnessOrigin, instantiate_universe_levels_scoped,
    },
    num_bigint::BigInt,
    num_traits::ToPrimitive,
    std::collections::{BTreeSet, VecDeque},
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

/// One dispatch of the [`elaborate`] driver: descend into a child node, or settle the current node with its un-restamped `(rebuilt, type)` result and the node it belongs to (for span restamping).
enum Step {
    Descend(Term, Mode),
    Settle(Term, (Term, Term)),
}

pub(crate) fn elaborate(
    context: &mut Context,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // The elaboration driver. Most nodes elaborate through the recursive `elaborate_subterm` (routed via the elaboration cache), their native depth tracking the written program's binder nesting. The exception is a *ground, all-explicit application*: its argument nesting is data-shaped (a string literal's `Utf8` derivation is one `more(c, st, t, rest)` link per byte), so recursing once per link would cost native stack in the data's length. Those are defunctionalized onto an explicit frame stack — the reducer's `PendingMatch` move, one level up — so the whole spine is absorbed by this one loop's `frames` vector at O(1) native depth per link. See `elaborate/apply.rs` for the frame and walk.
    //
    // Span restamping stays at this boundary, outside the cache: each settled node's rebuilt term takes its own span (innermost-wins) before it feeds a parent frame, matching the recursive `check(rest)`. Errors drain and return rather than `?`-unwind: `locate` stamps the failing node's span first (deepest, first-wins) then each pending frame's, reproducing native unwinding's deepest-node report.
    let mut frames: Vec<ElabFrame> = Vec::new();
    let mut work_term = term.clone();
    let mut work_mode = mode;

    loop {
        let mut step = match context.dispatch_node(&work_term, work_mode.clone(), &mut frames) {
            Ok(step) => step,
            Err(error) => return Err(error.locate(work_term.span(), &frames)),
        };
        // Drain: a settled result takes its own span (innermost-wins), then returns (no frame left) or resolves against the innermost pending frame, which itself settles or descends again.
        loop {
            match step {
                Step::Descend(child_term, child_mode) => {
                    work_term = child_term;
                    work_mode = child_mode;
                    break;
                }
                Step::Settle(node, (rebuilt, type_)) => {
                    let rebuilt = match node.span() {
                        Some(span) => rebuilt.with_span(span),
                        None => rebuilt,
                    };
                    // Obligation (V)'s seed. Every settled node passes here with the type it settled at, which is the judgment `reach.rs` used to re-derive from the finished term. Taken after the restamp so the recorded term is the one that reaches the module, and independent of `Mode` because `type_` is the term's type whether it was checked or inferred.
                    context.record_checked(&rebuilt, &type_);
                    match frames.pop() {
                        None => return Ok((rebuilt, type_)),
                        Some(frame) => {
                            let span = frame.span();
                            step = match frame.resume(context, rebuilt) {
                                Ok(Walk::Suspend { frame, arg, ty }) => {
                                    frames.push(*frame);
                                    Step::Descend(arg, Mode::Check(ty))
                                }
                                Ok(Walk::Done { term, result }) => Step::Settle(term, result),
                                Err(error) => return Err(error.locate(span, &frames)),
                            };
                        }
                    }
                }
            }
        }
    }
}

impl Context {
    /// Dispatch one node for the [`elaborate`] driver. A ground, all-explicit application takes the iterative fast path (probe the cache, then start its frame walk, pushing onto `frames`); every other node — and every application the fast path declines — routes through the recursive `elaborate_subterm` behind the elaboration cache, exactly as before.
    fn dispatch_node(
        &mut self,
        term: &Term,
        mode: Mode,
        frames: &mut Vec<ElabFrame>,
    ) -> Result<Step, Error> {
        if let Subterm::Apply(apply) = &**term
            && fast_apply_eligible(term, apply)
        {
            let record = match self.probe_elaborated(term, mode.expected().as_ref()) {
                ElabProbe::Hit(hit) => return Ok(Step::Settle(term.clone(), hit)),
                ElabProbe::Miss(stamp) => Some(stamp),
                ElabProbe::Uncacheable => None,
            };
            return match ElabFrame::start(self, apply, term, mode, record)? {
                Walk::Suspend { frame, arg, ty } => {
                    frames.push(*frame);
                    Ok(Step::Descend(arg, Mode::Check(ty)))
                }
                Walk::Done { term, result } => Ok(Step::Settle(term, result)),
            };
        }

        let expected = mode.expected();
        let result = self.get_or_init_elaborated(term, expected.as_ref(), |context| {
            elaborate_subterm(context, term, mode)
        })?;
        Ok(Step::Settle(term.clone(), result))
    }
}

impl Error {
    /// Locate a draining elaboration error: stamp the failing node's span first — deepest, and [`Error::at`] is first-wins — then each pending frame innermost-first (the top of the stack is the deepest pending application), reproducing the span native `?`-unwinding would report.
    fn locate(self, span: Option<Span>, frames: &[ElabFrame]) -> Self {
        let mut error = self.at_opt(span);
        for frame in frames.iter().rev() {
            error = error.at_opt(frame.span());
        }
        error
    }
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
                elaborated.push(
                    elaborate(context, arg, Mode::Check(curios_core::wire_term(wire_type)))?.0,
                );
            }

            let result = match signature.results.as_slice() {
                [] => Term::tuple_type_unit(),
                [(_, wire_type)] => curios_core::wire_term(wire_type),
                results => Term::tuple_type(
                    results
                        .iter()
                        .map(|(label, wire_type)| {
                            (
                                context.fresh(Some(label)),
                                curios_core::wire_term(wire_type),
                            )
                        })
                        .collect::<Vec<_>>(),
                ),
            };

            (
                Term::foreign(std::sync::Arc::clone(function), elaborated),
                Term::prim(Prim::io_type(result)),
            )
        }
        Subterm::UniverseInst(instance) => {
            let type_ = if let Some((group, index)) = instance.head.as_rec_proj() {
                context
                    .universes_mut()
                    .instantiate_at(group.universe_context(), &instance.levels)
                    .map_err(Error::from)?;

                instantiate_universe_levels_scoped(&group.member_type(index), &instance.levels)
                    .map_err(Error::from)?
            } else {
                match &*instance.head {
                    Subterm::Var(var) => context
                        .instantiate_assumption_at(var.unwrap(), &instance.levels)?
                        .ok_or_else(|| Error::unbound_variable(instance.head.clone()))?,
                    _ => {
                        return Err(Error::UniverseInvariant(
                            "a universe instance must wrap a variable or a projection of a recursive group".into(),
                        ));
                    }
                }
            };
            (term.clone(), type_)
        }
        Subterm::Prim(prim) => return elaborate_prim(context, term, prim, mode),
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
                    Term::universe_inst(term.clone(), levels)
                };
                (rebuilt, type_)
            }
            None => return Err(Error::unbound_variable(Term::var(var.clone()))),
        },
        Subterm::Func(func) => return elaborate_func(context, func, term, mode),
        Subterm::Tuple(tuple) => return elaborate_tuple(context, tuple, term, mode),
        Subterm::Infix(infix) => return elaborate_infix(context, infix, term, mode),
        Subterm::NumLit(num_lit) => return elaborate_num_lit(context, num_lit, term, mode),
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
