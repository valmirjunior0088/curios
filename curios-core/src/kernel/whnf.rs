//! Weak-head normalization: the kernel's reduction strategy.
//!
//! This is the kernel's answer to "what does this term compute to, as far as
//! its head". It is deliberately the *whole* strategy — beta, delta, iota,
//! projection, universe instantiation, primitive folding, and `rec` unfolding —
//! and deliberately nothing else. In particular there is no reduction cache, no
//! scrutinee refinement, and no metavariable resolution. Those are what make
//! the elaborator's reducer fast and forgiving; here they would be three more
//! ways for the answer to come from somewhere other than the term.
//!
//! It resembles the elaborator's reducer closely, and that resemblance is the
//! point of writing it out rather than sharing it. Reduction decides which
//! programs convert, and conversion decides which programs typecheck; a bug
//! shared by both checkers is a bug neither can catch. The crate boundary
//! enforces this — `curios-elab`'s reducer is not visible from here — so the
//! duplication cannot quietly collapse back into a call.
//!
//! Two things *are* shared, and both are representation rather than judgment:
//! the binder discipline that `open`/`release` implement, and
//! [`reduce_prim`](crate::reduce_prim), which decides what `2 + 2` folds to.
//! Neither can admit an ill-typed program on its own.

#[cfg(test)]
mod tests;

use {
    super::Kernel,
    crate::{
        Apply, Bound, Carrier, Cases, Field, FreeMonoid, Func, Layer, Let, Many, Match, Nat, Proj,
        Rec, ReduceError, Reducer, Scope, Struct, Subterm, Term, Tuple, UniverseInst, Var, Variant,
        instantiate_universe_levels_scoped, reduce_prim,
    },
    num_traits::ToPrimitive,
};

/// The kernel's reduction strategy: everything unfolds, nothing is remembered.
impl Reducer for Kernel {
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
        whnf(self, term)
    }

    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
        let reduced = whnf(self, term)?;
        force(self, reduced)
    }
}

/// One step's outcome: another term to reduce, or a weak-head normal form.
enum Step {
    Continue(Term),
    Stop(Term),
}

/// A `match` whose scrutinee is still being reduced.
///
/// Scrutinee reduction runs on this explicit stack rather than by recursion.
/// A tower of matches over a deep closed spine — the scan-state chain a string
/// literal lowers to — would otherwise consume native stack once per link, and
/// the kernel has to check exactly the terms the elaborator produced, on the
/// same default thread stack.
///
/// The scrutinee as *written* is not kept: an arm binds the payload of the
/// value the scrutinee reduced to, so nothing downstream refers back to the
/// original spelling.
struct Pending {
    motive: Scope<Many>,
    cases: Cases,
}

/// Reduce `term` until its head constructor is stable.
pub fn whnf(kernel: &mut Kernel, term: Term) -> Result<Term, ReduceError> {
    let mut term = term;
    let mut pending: Vec<Pending> = Vec::new();

    loop {
        kernel.spend()?;

        let mut step = match Term::unwrap_or_clone(term) {
            Subterm::Prim(prim) => Step::Stop(reduce_prim(kernel, &prim)?.into()),
            Subterm::Var(var) => step_var(kernel, var),
            Subterm::Apply(apply) => step_apply(kernel, apply)?,
            Subterm::Proj(proj) => step_proj(kernel, proj)?,
            Subterm::Func(func) => step_func(kernel, func),
            Subterm::Let(let_) => Step::Continue(step_let(let_)),
            Subterm::UniverseInst(instance) => step_universe_inst(kernel, instance)?,
            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => {
                pending.push(Pending { motive, cases });
                Step::Continue(head)
            }
            // `InductType`/`Variant`, `StructType`/`Struct`, `Tuple`,
            // `FuncType`, `Type`/`Prop`, `Rec`/`RecMember`, and a `Metavar` no
            // kernel input should contain are all weak-head normal already:
            // their sub-terms are not reduced in this position.
            other => Step::Stop(other.into()),
        };

        // A finished value resolves against the pending frames innermost-first.
        loop {
            match step {
                Step::Continue(next) => {
                    term = next;
                    break;
                }
                Step::Stop(value) => match pending.pop() {
                    None => return Ok(value),
                    Some(frame) => {
                        let forced = force(kernel, value)?;
                        step = step_match(forced, frame.motive, frame.cases);
                    }
                },
            }
        }
    }
}

/// Delta: unfold a definition, or leave the variable as the normal form it is.
fn step_var(kernel: &Kernel, var: Var) -> Step {
    match kernel.value(var.unwrap()) {
        Some(value) => Step::Continue(value.clone()),
        None => Step::Stop(Term::var(var)),
    }
}

/// Beta: open a function's telescope over the arguments applied to it.
///
/// A `rec` head is exposed but not unfolded — the folded spelling stays the
/// normal form of a recursive call, and [`force`] is what demands otherwise.
fn step_apply(kernel: &mut Kernel, apply: Apply) -> Result<Step, ReduceError> {
    let Apply {
        head,
        params,
        plicities,
    } = apply;

    let head = whnf(kernel, head)?;
    let head = expose_rec_tail(kernel, head)?;

    Ok(match Term::unwrap_or_clone(head) {
        Subterm::Func(Func { telescope, .. }) => {
            let refs = params.iter().collect::<Vec<_>>();
            Step::Continue(telescope.open(&refs))
        }
        Subterm::RecMember(member) => Step::Stop(Term::from(Subterm::Apply(Apply {
            head: Term::rec_member(member.group, member.index),
            params,
            plicities,
        }))),
        head => Step::Stop(Term::from(Subterm::Apply(Apply {
            head: head.into(),
            params,
            plicities,
        }))),
    })
}

/// Projection: select a component out of a tuple, a struct, or a constructor's
/// payload.
///
/// A `Variant` is projected through the flat runtime view `(tag, payload...)`,
/// so field `i + 1` is payload component `i`; a `Struct` has no tag and is
/// projected positionally. A label that survived to here has no positional
/// meaning yet and stays stuck.
fn step_proj(kernel: &mut Kernel, proj: Proj) -> Result<Step, ReduceError> {
    let Proj { head, field } = proj;

    let Field::Index(index) = field else {
        return Ok(Step::Stop(Term::from(Subterm::Proj(Proj { head, field }))));
    };

    let head = whnf(kernel, head)?;
    let head = force(kernel, head)?;

    Ok(match Term::unwrap_or_clone(head) {
        Subterm::Tuple(Tuple { fields, .. }) if index < fields.len() => {
            Step::Continue(fields.into_iter().nth(index).expect("index bounded above"))
        }
        Subterm::Variant(ctor) if (1..=ctor.payload.len()).contains(&index) => Step::Continue(
            ctor.payload
                .into_iter()
                .nth(index - 1)
                .expect("index bounded above"),
        ),
        Subterm::Struct(Struct { fields, .. }) if index < fields.len() => {
            Step::Continue(fields.into_iter().nth(index).expect("index bounded above"))
        }
        head => Step::Stop(Term::proj(Term::from(head), index)),
    })
}

/// Eta for functions: `(x) => f(x)` is `f`, provided `f` does not itself
/// mention `x`.
///
/// Contracting here rather than only at conversion means the two spellings have
/// one normal form, so every consumer of a weak-head normal form sees them as
/// the same term without having to know the rule.
fn step_func(kernel: &mut Kernel, func: Func) -> Step {
    let arity = func.telescope.len();

    let binders = (0..arity).map(|_| kernel.fresh(None)).collect::<Vec<_>>();
    let occurrences = binders.iter().map(Term::free_var).collect::<Vec<_>>();
    let refs = occurrences.iter().collect::<Vec<_>>();

    match Term::unwrap_or_clone(func.telescope.open(&refs)) {
        Subterm::Apply(Apply { head, params, .. })
            if params.len() == arity
                && params.iter().enumerate().all(|(i, param)| {
                    matches!(param.as_ref(), Subterm::Var(var) if var.unwrap() == &binders[i])
                })
                && binders.iter().all(|binder| !head.free_vars().contains(binder)) =>
        {
            Step::Continue(head)
        }
        _ => Step::Stop(Term::from(Subterm::Func(func))),
    }
}

/// Zeta: substitute a `let`'s bindings into its tail.
///
/// The elaborator instead binds each value as a fresh definition and opens the
/// tail over *those*, which avoids copying a value into every use. The kernel
/// substitutes, because a substitution is visibly the rule and an environment
/// is a second place a variable's meaning can come from. Bindings are
/// non-recursive and bind left to right, so binding `i` sees exactly the values
/// before it.
fn step_let(let_: Let) -> Term {
    let mut values: Vec<Term> = Vec::with_capacity(let_.bindings.len());

    for binding in &let_.bindings {
        let refs = values.iter().collect::<Vec<_>>();
        values.push(binding.value().release(&refs));
    }

    let refs = values.iter().collect::<Vec<_>>();
    let_.tail.open(&refs)
}

/// Instantiate a universe-polymorphic definition at a stated instance.
///
/// This is the only position from which a polymorphic definition unfolds — a
/// bare occurrence of one denotes no particular instance, so
/// [`Kernel::value`](super::Kernel) withholds it there.
fn step_universe_inst(kernel: &mut Kernel, instance: UniverseInst) -> Result<Step, ReduceError> {
    let UniverseInst { head, levels } = instance;

    let reduct = match &*head {
        Subterm::Var(var) => kernel.value_at(var.unwrap()).cloned(),
        _ => Some(head.clone()),
    };

    let Some(reduct) = reduct else {
        return Ok(Step::Stop(Term::universe_inst(head, levels)));
    };

    Ok(Step::Continue(match &*reduct {
        Subterm::RecMember(member) => Term::rec_member(
            member
                .group
                .instantiate_universes(&levels)
                .map_err(ReduceError::Universe)?,
            member.index,
        ),
        _ => instantiate_universe_levels_scoped(&reduct, &levels).map_err(ReduceError::Universe)?,
    }))
}

/// Iota: dispatch a `match` on the value `forced` its scrutinee reduced to.
///
/// An arm binds that value's payload components directly. They are themselves
/// unreduced — a `Variant` is a weak-head normal form whose sub-terms this
/// strategy never entered — so binding them is call-by-name, not call-by-value.
///
/// The elaborator instead binds each arm to a *projection of the scrutinee as
/// written*, because a reduced payload can carry annotation holes its zonker
/// would then have to solve. The kernel has no zonker and no holes, so it takes
/// the direct route.
fn step_match(forced: Term, motive: Scope<Many>, cases: Cases) -> Step {
    match cases {
        Cases::Bool {
            false_case,
            true_case,
        } => match forced.as_bool() {
            Some(false) => Step::Continue(false_case),
            Some(true) => Step::Continue(true_case),
            None => Step::Stop(Term::from(Subterm::Match(Match {
                head: forced,
                motive,
                cases: Cases::Bool {
                    false_case,
                    true_case,
                },
            }))),
        },

        // A literal `Nat` is a floor over a `Zero` inner, so a zero inner is
        // exactly "this is a concrete `k`". A literal takes its case, or the
        // default when no case names it (including a value past the `u32` keys);
        // anything symbolic rebuilds the neutral switch.
        Cases::Switch { cases, default } => {
            let (value, inner) = Nat::decompose(&forced);

            match Nat::is_zero(&inner) {
                true => Step::Continue(
                    value
                        .to_u32()
                        .and_then(|key| cases.get(&key))
                        .unwrap_or(&default)
                        .clone(),
                ),
                false => Step::Stop(Term::from(Subterm::Match(Match {
                    head: forced,
                    motive,
                    cases: Cases::Switch { cases, default },
                }))),
            }
        }

        Cases::Induct { cases, default } => {
            if let Subterm::Variant(Variant { tag, payload, .. }) = &*forced {
                if let Some((_, arm)) = cases.iter().find(|(candidate, _)| candidate == tag) {
                    let refs = payload.iter().collect::<Vec<_>>();
                    return Step::Continue(arm.open(&refs));
                }

                // A constructor with no arm of its own takes the catch-all,
                // which binds nothing.
                if let Some(default) = &default {
                    return Step::Continue(default.clone());
                }
            }

            Step::Stop(Term::from(Subterm::Match(Match {
                head: forced,
                motive,
                cases: Cases::Induct { cases, default },
            })))
        }

        // Structural induction over a native free-monoid carrier
        // (`Nat`/`Bin`/`Lst`). `FreeMonoid::uncons` owns the carrier-specific
        // one-step decode; this is the catamorphism over it. The cons arm binds
        // the peeled generator (absent for the unary `Nat`), the tail, and an
        // induction hypothesis that recurses symbolically on that tail.
        Cases::FreeMonoid { carrier } => {
            let layer = match &carrier {
                Carrier::Nat { .. } => FreeMonoid::Unary,
                Carrier::Bin { grain, .. } => FreeMonoid::Bin(*grain),
                Carrier::Lst { .. } => FreeMonoid::Lst,
            }
            .uncons(Term::unwrap_or_clone(forced));

            match layer {
                Layer::Empty => Step::Continue(match carrier {
                    Carrier::Nat { empty_case, .. }
                    | Carrier::Bin { empty_case, .. }
                    | Carrier::Lst { empty_case, .. } => empty_case,
                }),
                Layer::Cons { head: elem, tail } => {
                    let hypothesis: Term = Subterm::Match(Match {
                        head: tail.clone(),
                        motive: motive.clone(),
                        cases: Cases::FreeMonoid {
                            carrier: carrier.clone(),
                        },
                    })
                    .into();

                    Step::Continue(match &carrier {
                        Carrier::Nat { cons_case, .. } => cons_case.open(&[&tail, &hypothesis]),
                        Carrier::Bin { cons_case, .. } | Carrier::Lst { cons_case, .. } => {
                            cons_case.open(&[
                                elem.as_ref().expect("a Bin/Lst cons layer carries a head"),
                                &tail,
                                &hypothesis,
                            ])
                        }
                    })
                }
                Layer::Stuck(stuck) => Step::Stop(Term::from(Subterm::Match(Match {
                    head: stuck.into(),
                    motive,
                    cases: Cases::FreeMonoid { carrier },
                }))),
            }
        }
    }
}

/// Strip `rec` binding syntax without unfolding a member's fixed point, turning
/// `rec f = ...; f` into the structural `RecMember` it denotes.
fn expose_rec_tail(kernel: &mut Kernel, term: Term) -> Result<Term, ReduceError> {
    let mut term = term;

    loop {
        match Term::unwrap_or_clone(term) {
            Subterm::Rec(rec) => term = whnf(kernel, unfold_rec(rec))?,
            other => return Ok(other.into()),
        }
    }
}

/// Open a `rec` group's tail over its members. A pure binder operation: it
/// mints nothing and unfolds no fixed point.
fn unfold_rec(rec: Rec) -> Term {
    let members = rec.group.members();
    let refs = members.iter().collect::<Vec<_>>();

    rec.tail.open(&refs)
}

/// Unfold a `rec` head that some eliminator demands the value of.
///
/// The main loop treats a `rec` as a normal form, which is what keeps a
/// recursive definition from unfolding forever at every occurrence. An
/// eliminator that actually needs the value calls this, which unfolds and
/// re-reduces until it reaches one.
///
/// An unfolding that lands on a stuck neutral is *discarded* and the folded
/// spelling returned. That is what stops the unfold-and-restuck cycle: without
/// it, a recursive function applied to a symbolic argument would grow one more
/// copy of its own body at every demand and never reach a normal form. A
/// non-productive group still spins until the budget runs out, exactly as a
/// top-level `rec` does.
fn force(kernel: &mut Kernel, term: Term) -> Result<Term, ReduceError> {
    let folded = term.clone();
    let mut term = term;

    loop {
        kernel.spend()?;

        match Term::unwrap_or_clone(term) {
            Subterm::Rec(rec) => term = whnf(kernel, unfold_rec(rec))?,
            Subterm::RecMember(member) => {
                term = whnf(kernel, member.group.member_body(member.index))?;
            }
            Subterm::Apply(apply) => match unfold_rec_apply(kernel, apply)? {
                Some(unfolded) => term = whnf(kernel, unfolded)?,
                None => return Ok(folded),
            },
            other => {
                return Ok(match other {
                    Subterm::Match(_)
                    | Subterm::Var(_)
                    | Subterm::Metavar(_)
                    | Subterm::Proj(_) => folded,
                    value => value.into(),
                });
            }
        }
    }
}

/// Unfold one folded recursive application, when its result shape is demanded.
fn unfold_rec_apply(kernel: &mut Kernel, apply: Apply) -> Result<Option<Term>, ReduceError> {
    let Apply { head, params, .. } = apply;

    let head = whnf(kernel, head)?;
    let head = expose_rec_tail(kernel, head)?;

    let Subterm::RecMember(member) = Term::unwrap_or_clone(head) else {
        return Ok(None);
    };

    let body = whnf(kernel, member.group.member_body(member.index))?;
    let body = force(kernel, body)?;

    let Subterm::Func(Func { telescope, .. }) = Term::unwrap_or_clone(body) else {
        return Ok(None);
    };

    let refs = params.iter().collect::<Vec<_>>();
    Ok(Some(telescope.open(&refs)))
}
