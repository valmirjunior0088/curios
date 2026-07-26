mod prim;
use prim::*;

#[cfg(test)]
mod tests;

use {
    super::{
        Apply, Bound, Carrier, Cases, Context, Field, FreeMonoid, Func, FuncType, InductType,
        Layer, Let, Many, Match, Metavar, Nat, One, Prim, Proj, Rec, ReduceError, Scope, Struct,
        StructType, Subterm, Telescope, Term, Tuple, TupleType, UniverseInst, Var, Variant,
        instantiate_universe_levels_scoped,
    },
    crate::Instant,
    num_traits::ToPrimitive,
};

enum Reduce {
    Continue(Term),
    Break(Term),
}

/// One `match` waiting for its scrutinee's value on [`reduce`]'s explicit
/// scrutinee stack. `head` is the *original* scrutinee term: `Induct`
/// dispatch binds arms to projections of it (call-by-name), and the finished
/// value is cached under it exactly as the previously nested `reduce` call
/// would have cached it.
struct PendingMatch {
    head: Term,
    motive: Scope<Many>,
    cases: Cases,
}

/// Open a `rec` group's tail over structural folded member terms. This is a
/// pure binder operation: it neither mints names nor mutates the context.
pub(crate) fn unfold_rec(_context: &mut Context, rec: Rec) -> Term {
    let members = rec.group.members();
    let refs = members.iter().collect::<Vec<_>>();
    rec.tail.open(&refs)
}

/// Expose only `Rec` binding syntax, without unfolding a selected member's
/// fixed point. This turns `rec f = ...; f` into its structural `RecMember`
/// value while preserving the folded recursive call as the canonical neutral.
fn expose_rec_tail(context: &mut Context, mut term: Term) -> Result<Term, ReduceError> {
    loop {
        match Term::unwrap_or_clone(term) {
            Subterm::Rec(rec) => {
                let tail = unfold_rec(context, rec);
                term = reduce(context, tail)?;
            }
            other => return Ok(other.into()),
        }
    }
}

/// Delta-unfold one folded recursive application. Ordinary reduction does not
/// call this: eliminators and conversion use it only when the folded call's
/// result shape is demanded or must be compared with a differently-shaped
/// term.
pub(crate) fn unfold_rec_apply(
    context: &mut Context,
    apply: Apply,
) -> Result<Option<Term>, ReduceError> {
    let Apply {
        head,
        params,
        plicities: _,
    } = apply;
    let head = reduce(context, head)?;
    let head = expose_rec_tail(context, head)?;
    let Subterm::RecMember(member) = Term::unwrap_or_clone(head) else {
        return Ok(None);
    };

    let body = reduce(context, member.group.member_body(member.index))?;
    let body = force_rec(context, body)?;
    let Subterm::Func(Func { telescope, .. }) = Term::unwrap_or_clone(body) else {
        return Ok(None);
    };
    let param_refs = params.iter().collect::<Vec<_>>();
    Ok(Some(telescope.open(&param_refs)))
}

/// Force a `rec` group in WHNF position. The main loop treats a `Rec` node as a
/// normal form, so an eliminator that demands its value unfolds it here and
/// re-reduces, repeating if the opened tail is itself a `rec`. A non-productive
/// group spins until the reduce deadline — exactly as a top-level `rec` does.
///
/// The force either reaches a value some eliminator can absorb or returns the
/// input unchanged: an unfolding that lands on a stuck neutral (a blocked
/// match, variable, or projection) is discarded so the folded spelling — not
/// an unfolded copy of the member body — stays the canonical normal form.
fn force_rec(context: &mut Context, term: Term) -> Result<Term, ReduceError> {
    let folded = term.clone();
    let mut term = term;
    loop {
        if Instant::now() > context.deadline() {
            return Err(ReduceError::Preempted);
        }

        match Term::unwrap_or_clone(term) {
            Subterm::Rec(rec) => {
                let tail = unfold_rec(context, rec);
                term = reduce(context, tail)?;
            }
            Subterm::RecMember(member) => {
                term = reduce(context, member.group.member_body(member.index))?;
            }
            Subterm::Apply(apply) => match unfold_rec_apply(context, apply)? {
                Some(unfolded) => term = reduce(context, unfolded)?,
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

/// Reduce to WHNF and then force a `rec` head: used wherever an eliminator
/// (`match`/application/projection) demands a value, so an inner `rec` reduces
/// just like a top-level one instead of staying stuck.
pub(crate) fn reduce_forced(context: &mut Context, term: Term) -> Result<Term, ReduceError> {
    let reduced = reduce(context, term)?;
    force_rec(context, reduced)
}

/// The canonical form of a (potential) scrutinee refinement key: the head kept
/// verbatim — so the refined function (`classify`, `Nat/in_range`) is *not*
/// unfolded and stays the key — with each argument reduced to WHNF. Storing and
/// probing through one canonicalizer makes occurrences that differ only in
/// argument spelling (`c` vs `Bin/at(cons(c,t),0,_)`, `lo` vs a projection that
/// reduces to it) collapse to the same key. A non-application is its own
/// canonical form.
///
/// Argument reduction is *best-effort*: an argument that cannot reduce at the
/// type level (a runtime-only IO primitive like `is_ready`'s `/sys/poll`
/// result, or an out-of-range access) is kept verbatim rather than forced. Such
/// an argument was never going to differ in spelling — the only occurrence is
/// the scrutinee itself, which matches the key raw — so keeping it raw both
/// avoids forcing effects at elaboration and still matches. A `Preempted`
/// deadline is the one error that propagates.
pub(crate) fn canonical_scrutinee(context: &mut Context, term: &Term) -> Result<Term, ReduceError> {
    let canonical = match &**term {
        Subterm::Apply(Apply {
            head,
            params,
            plicities,
        }) => {
            let params = params
                .iter()
                .map(|p| match reduce(context, p.clone()) {
                    Ok(reduced) => Ok(reduced),
                    Err(ReduceError::Preempted) => Err(ReduceError::Preempted),
                    Err(_) => Ok(p.clone()),
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(Subterm::Apply(Apply {
                head: head.clone(),
                params,
                plicities: plicities.clone(),
            })
            .into())
        }
        _ => Ok(term.clone()),
    }?;
    // Universe arguments cannot affect computation: Curios has no universe
    // reflection and erasure removes them. Refinement keys therefore compare
    // the same applied definition across independently fresh scheme instances
    // by its computational spelling, not by inference-local level ids.
    Ok(super::project_erased_universes(&canonical))
}

fn reduce_apply(context: &mut Context, apply: Apply) -> Result<Reduce, ReduceError> {
    let Apply {
        head,
        params,
        plicities,
    } = apply;

    let param_refs = params.iter().collect::<Vec<_>>();

    let head = reduce(context, head)?;
    let head = expose_rec_tail(context, head)?;
    match Term::unwrap_or_clone(head) {
        Subterm::Func(Func { telescope, .. }) => Ok(Reduce::Continue(telescope.open(&param_refs))),
        Subterm::RecMember(member) => Ok(Reduce::Break(Term::from(Subterm::Apply(Apply {
            head: Term::rec_member(member.group, member.index),
            params,
            plicities,
        })))),
        head => Ok(Reduce::Break(Term::from(Subterm::Apply(Apply {
            head: head.into(),
            params,
            plicities,
        })))),
    }
}

fn reduce_proj(context: &mut Context, proj: Proj) -> Result<Reduce, ReduceError> {
    let Proj { head, field } = proj;
    // Label projections are normally resolved (and rebuilt positionally) by
    // elaborate. One path reaches here with a label still attached:
    // `elaborate_apply` substitutes a *postponed* argument's raw surface term
    // into the remaining telescope and the result type (that raw spelling is
    // load-bearing — beta-reducing it through the result is what lets `expect`
    // pin the metavariables the postponed slot is waiting on). If such an
    // argument is a lambda whose body projects by label, beta-reduction here
    // manufactures a label projection on a not-yet-solved head, which no
    // earlier pass could have resolved. Leave it stuck rather than panicking:
    // the conversion it sits under then fails as an ordinary mismatch at its
    // origin span, or succeeds once the slot is settled and re-opened.
    let Field::Index(index) = field else {
        return Ok(Reduce::Break(Term::from(Subterm::Proj(Proj {
            head,
            field,
        }))));
    };

    if let Some(v) = context.proj_reduct(&head, index) {
        return Ok(Reduce::Continue(v.clone()));
    }

    match Term::unwrap_or_clone(reduce_forced(context, head)?) {
        Subterm::Tuple(Tuple { fields, .. }) => Ok(Reduce::Continue(
            fields
                .into_iter()
                .nth(index)
                .expect("Proj: index out of bounds"),
        )),
        // The untyped reducer's flat view of a constructor value, mirroring the
        // runtime layout `(tag, payload...)`: field i + 1 is the i-th payload
        // component. `reduce_induct_match` relies on this to bind arms by
        // projection (call-by-name). Field 0 (the tag) is never projected at
        // the term level — dispatch inspects the `Variant` directly.
        Subterm::Variant(ctor) if (1..=ctor.payload.len()).contains(&index) => {
            Ok(Reduce::Continue(
                ctor.payload
                    .into_iter()
                    .nth(index - 1)
                    .expect("index bounded above"),
            ))
        }
        // A struct value is projected positionally with *no* tag offset
        // (unlike `Variant`, whose field 0 is the tag): `Proj(Struct, i)` is
        // field `i`.
        Subterm::Struct(Struct { fields, .. }) if index < fields.len() => Ok(Reduce::Continue(
            fields.into_iter().nth(index).expect("index bounded above"),
        )),
        head => {
            let head: Term = head.into();
            match context.proj_reduct(&head, index) {
                Some(v) => Ok(Reduce::Continue(v.clone())),
                None => Ok(Reduce::Break(Term::proj(head, index))),
            }
        }
    }
}

fn reduce_func_eta(context: &mut Context, func: Func) -> Result<Reduce, ReduceError> {
    let n = func.telescope.len();

    let freshs = (0..n).map(|_| context.fresh(None)).collect::<Vec<_>>();

    let ys = freshs.iter().map(Term::free_var).collect::<Vec<_>>();

    let y_refs = ys.iter().collect::<Vec<_>>();

    match Term::unwrap_or_clone(func.telescope.open(&y_refs)) {
            Subterm::Apply(Apply { head, params, .. })
                if params.len() == n
                    && params.iter().enumerate().all(|(i, p)| {
                        matches!(p.as_ref(), Subterm::Var(v) if v.unwrap() == freshs[i].as_str())
                    })
                    && freshs.iter().all(|f| !head.free_vars().contains(f)) =>
            {
                Ok(Reduce::Continue(head))
            }
            _ => Ok(Reduce::Break(Term::from(Subterm::Func(func)))),
        }
}

/// Dispatch a `match` over its scrutinee's already-reduced-and-forced value.
/// `head` is the *original* scrutinee term, which the `Induct` arm projects
/// (call-by-name — see its comment); `forced` is what `reduce_forced` produced
/// for it. Scrutinee reduction itself happens on [`reduce`]'s explicit
/// scrutinee stack rather than by recursing here: a tower of matches over a
/// deep closed spine — a string literal's scan-state chain — would otherwise
/// consume native stack once per link.
fn reduce_match(head: Term, forced: Term, motive: Scope<Many>, cases: Cases) -> Reduce {
    match cases {
        Cases::Bool {
            false_case,
            true_case,
        } => match Term::unwrap_or_clone(forced) {
            Subterm::Prim(Prim::Bool(false)) => Reduce::Continue(false_case),
            Subterm::Prim(Prim::Bool(true)) => Reduce::Continue(true_case),
            forced => Reduce::Break(Term::from(Subterm::Match(Match {
                head: forced.into(),
                motive,
                cases: Cases::Bool {
                    false_case,
                    true_case,
                },
            }))),
        },

        Cases::Switch { cases, default } => {
            let scrutinee = forced;
            // A literal `Nat` is the kernel's spine floor over a `Zero` inner, so
            // `is_zero` on the peeled inner is exactly "is this a concrete `k`?" — the
            // same spine view the arithmetic family reads. A literal dispatches to its
            // case, or the default when none matches (including a value beyond the
            // `u32` case keys); a symbolic scrutinee rebuilds the neutral switch.
            let (value, inner) = Nat::decompose(&scrutinee);

            match Nat::is_zero(&inner) {
                true => {
                    let body = value
                        .to_u32()
                        .and_then(|k| cases.get(&k))
                        .unwrap_or(&default);

                    Reduce::Continue(body.clone())
                }
                false => Reduce::Break(Term::from(Subterm::Match(Match {
                    head: scrutinee,
                    motive,
                    cases: Cases::Switch { cases, default },
                }))),
            }
        }

        // Dispatch on the reduced scrutinee — a `Variant` directly, or one
        // reached through a match-arm refinement (`refine_head` registers
        // `head := ctor_val`, which `reduce` follows). The selected arm's
        // binders are bound to *projections of the original head term*
        // (`head.(i + 1)`, the flat view in `reduce_proj`), not to the reduced
        // payload values: call-by-name. Substituting reduced payloads would
        // inline evaluated definition internals (including local-`let`
        // annotation holes that elaboration never births) into types that
        // flow on to `zonk`.
        Cases::Induct { cases, default } => {
            if let Subterm::Variant(ctor) = &*forced {
                if let Some((_, scope)) = cases.iter().find(|(tag, _)| tag == &ctor.tag) {
                    let projections = (0..scope.arity())
                        .map(|i| Term::proj(head.clone(), i + 1))
                        .collect::<Vec<_>>();

                    let projection_refs = projections.iter().collect::<Vec<_>>();

                    return Reduce::Continue(scope.open(&projection_refs));
                }

                // A concrete constructor with no enumerated arm takes the
                // catch-all default, which binds nothing (no scope to open).
                if let Some(default) = &default {
                    return Reduce::Continue(default.clone());
                }
            }

            Reduce::Break(Term::from(Subterm::Match(Match {
                head: forced,
                motive,
                cases: Cases::Induct { cases, default },
            })))
        }

        // Structural induction on a native free-monoid primitive (`Nat`/`Bin`/`Lst`).
        // The carrier-specific one-step decode lives in `FreeMonoid::uncons` (the
        // eliminator-side analogue of `spine::peel_prim`); this driver is the shared
        // catamorphism over it. An identity `Layer` takes the empty arm; a cons
        // `Layer` peels a generator (its head absent for the unary `Nat`) and
        // recurses symbolically for the induction hypothesis; a stuck scrutinee
        // rebuilds.
        Cases::FreeMonoid { carrier } => {
            let scrutinee = Term::unwrap_or_clone(forced);

            let layer = match &carrier {
                Carrier::Nat { .. } => FreeMonoid::Unary,
                Carrier::Bin { grain, .. } => FreeMonoid::Bin(*grain),
                Carrier::Lst { .. } => FreeMonoid::Lst,
            }
            .uncons(scrutinee);

            match layer {
                Layer::Empty => Reduce::Continue(match carrier {
                    Carrier::Nat { empty_case, .. }
                    | Carrier::Bin { empty_case, .. }
                    | Carrier::Lst { empty_case, .. } => empty_case,
                }),
                Layer::Cons { head, tail } => {
                    let ih: Term = Subterm::Match(Match {
                        head: tail.clone(),
                        motive: motive.clone(),
                        cases: Cases::FreeMonoid {
                            carrier: carrier.clone(),
                        },
                    })
                    .into();

                    // The cons arm binds the generator's payload (a head, absent for
                    // the unary `Nat`), then the tail and the induction hypothesis.
                    Reduce::Continue(match &carrier {
                        Carrier::Nat { cons_case, .. } => cons_case.open(&[&tail, &ih]),
                        Carrier::Bin { cons_case, .. } | Carrier::Lst { cons_case, .. } => {
                            cons_case.open(&[
                                head.as_ref().expect("Bin/Lst cons layer carries a head"),
                                &tail,
                                &ih,
                            ])
                        }
                    })
                }
                Layer::Stuck(scrutinee) => Reduce::Break(Term::from(Subterm::Match(Match {
                    head: scrutinee.into(),
                    motive,
                    cases: Cases::FreeMonoid { carrier },
                }))),
            }
        }
    }
}

fn reduce_let(context: &mut Context, let_: Let) -> Reduce {
    // Bind each value as a fresh definition and continue with the tail opened
    // over those definitions — an environment step (like `unfold_rec`) rather
    // than a substitution, so no value is copied into the tail. Left to right:
    // a `let` is non-recursive, so binding `i` sees only labels `0..i`, which
    // are already defined; each value is released against just that prefix. The
    // definitions land in the enclosing context and outlive this call; their
    // labels are entropy-fresh, so nothing collides.
    let labels = let_
        .tail
        .label_iter()
        .map(|label| context.fresh(label))
        .collect::<Vec<_>>();

    let label_terms = labels
        .iter()
        .map(Var::free)
        .map(Term::var)
        .collect::<Vec<_>>();
    let label_refs = label_terms.iter().collect::<Vec<_>>();

    for (i, (label, binding)) in labels.iter().zip(&let_.bindings).enumerate() {
        context.define(label, &binding.value().release(&label_refs[..i]), None);
    }

    Reduce::Continue(let_.tail.open(&label_refs))
}

fn reduce_var(context: &Context, var: Var) -> Reduce {
    match context.var_reduct(var.unwrap()) {
        Some(next) => Reduce::Continue(next.clone()),
        None => Reduce::Break(Term::var(var)),
    }
}

fn reduce_metavar(context: &Context, metavar: Metavar) -> Reduce {
    // Resolution rewrites the (birth-named) solution through the occurrence's
    // spine, so a solution mentioning a sibling binder lands on whatever that
    // binder corresponds to here.
    match context.resolve_metavar(&metavar) {
        Some(solution) => Reduce::Continue(solution),
        None => Reduce::Break(Term::from(Subterm::Metavar(metavar))),
    }
}

fn reduce_universe_inst(context: &Context, instance: UniverseInst) -> Result<Reduce, ReduceError> {
    let reduct = match &*instance.head {
        Subterm::Var(var) => context.var_reduct_at(var.unwrap()).cloned(),
        Subterm::RecMember(_) => Some(instance.head.clone()),
        _ => Some(instance.head.clone()),
    };
    let Some(reduct) = reduct else {
        return Ok(Reduce::Break(Term::universe_inst(
            instance.head,
            instance.levels,
        )));
    };
    let arguments = instance.levels;
    let reduct = match &*reduct {
        Subterm::RecMember(member) => Term::rec_member(
            member
                .group
                .instantiate_universes(&arguments)
                .map_err(ReduceError::Universe)?,
            member.index,
        ),
        _ => instantiate_universe_levels_scoped(&reduct, &arguments)
            .map_err(ReduceError::Universe)?,
    };
    Ok(Reduce::Continue(reduct))
}

pub(crate) fn reduce(context: &mut Context, mut term: Term) -> Result<Term, ReduceError> {
    if let Some(cached) = context.cached_reduced(&term) {
        return Ok(cached);
    }

    let entry = term.clone();

    // Matches waiting for their scrutinees. Reducing a scrutinee re-enters
    // this loop under a pushed frame instead of recursing, so a tower of
    // matches over a deep closed spine — a string literal's scan-state
    // chain — costs one `PendingMatch` per level rather than native stack.
    // A finished value resolves against these frames innermost-first; each
    // landing reduct — a frame's scrutinee value, or with no frame left the
    // entry term's overall value — is cached under the term it reduces.
    let mut pending: Vec<PendingMatch> = Vec::new();

    loop {
        if Instant::now() > context.deadline() {
            return Err(ReduceError::Preempted);
        }

        let mut step = 'step: {
            // Rung B for stuck applications (convertibility-keyed). Gated cheaply
            // — store non-empty, then a refined applied-head symbol — before
            // canonicalizing the candidate's arguments and looking the key up.
            if context.has_scrutinee_refinements()
                && let Some(head) = term.head_label()
                && context.scrutinee_head_refined(head)
            {
                let canonical = canonical_scrutinee(context, &term)?;

                if context.refinements_suppressed() {
                    // Withhold the value, but keep an application key neutral —
                    // as a `Var` key already is — so `solve_refinement_free`'s
                    // committed spelling stays a term the live refinement can
                    // fire on (the canonical form, never the unfolded body).
                    if context.is_scrutinee_key(&canonical) {
                        break 'step Reduce::Break(canonical);
                    }
                } else if let Some(value) = context.scrutinee_reduct(&canonical) {
                    break 'step Reduce::Continue(value.clone());
                }
            }

            match Term::unwrap_or_clone(term) {
                Subterm::Prim(prim) => Reduce::Break(reduce_prim(context, &prim)?.into()),
                Subterm::Match(m) => match context.cached_reduced(&m.head) {
                    // A warm scrutinee dispatches immediately — the
                    // frame-free analogue of the nested call's cache hit.
                    Some(value) => {
                        let forced = force_rec(context, value)?;
                        reduce_match(m.head, forced, m.motive, m.cases)
                    }
                    None => {
                        pending.push(PendingMatch {
                            head: m.head.clone(),
                            motive: m.motive,
                            cases: m.cases,
                        });
                        Reduce::Continue(m.head)
                    }
                },
                Subterm::Apply(apply) => reduce_apply(context, apply)?,
                Subterm::Proj(proj) => reduce_proj(context, proj)?,
                Subterm::Func(func) => reduce_func_eta(context, func)?,
                Subterm::Let(let_) => reduce_let(context, let_),
                Subterm::Var(var) => reduce_var(context, var),
                Subterm::Metavar(metavar) => reduce_metavar(context, metavar),
                Subterm::UniverseInst(instance) => reduce_universe_inst(context, instance)?,
                // `InductType`/`Variant` and `StructType`/`Struct` are primitive
                // normal forms, like `Tuple`: their sub-terms are not reduced
                // in WHNF.
                term => Reduce::Break(term.into()),
            }
        };

        loop {
            match step {
                Reduce::Continue(next) => {
                    term = next;
                    break;
                }
                Reduce::Break(result) => match pending.pop() {
                    None => {
                        context.reduce(entry, &result);
                        return Ok(result);
                    }
                    Some(frame) => {
                        context.reduce(frame.head.clone(), &result);
                        let forced = force_rec(context, result)?;
                        step = reduce_match(frame.head, forced, frame.motive, frame.cases);
                    }
                },
            }
        }
    }
}

/// Reduce `term` to a deep normal form for **diagnostic display**: every
/// position is taken to weak-head normal form and its sub-terms recursively
/// normalized, opening the type-former binders (`FuncType`/`Func`/`TupleType`)
/// under fresh variables.
///
/// `reduce` alone stops at the head: an inductive type's indices are not
/// sub-reduced, so a concept-method projection standing in an index position —
/// `Vec(Nat, Add/add(0, 1))`, spelled `Vec(Nat, (sys/witness@0).0(0, 1))` once
/// resolution has picked the primitive witness — survives verbatim into a
/// type-mismatch message. Normalizing the index collapses it to the value it
/// denotes (`Vec(Nat, 1)`), or, when an operand is symbolic, to the underlying
/// operator primitive (`Vec(Nat, n + m)`) the printer spells infix.
///
/// Display-only and best-effort: the result is never fed back into the kernel,
/// and a preemption (the reduce deadline) propagates so callers can fall back
/// to the un-normalized spelling. The binder-heavy stuck forms (`Rec`, `Match`)
/// keep their WHNF shape rather than being reduced under their own binders —
/// they seldom carry the arithmetic this targets, and opening every case arm
/// buys a diagnostic nothing.
pub(crate) fn normalize(context: &mut Context, term: Term) -> Result<Term, ReduceError> {
    let reduced = reduce_forced(context, term)?;
    let span = reduced.span();

    let inner = match Term::unwrap_or_clone(reduced) {
        Subterm::Apply(Apply {
            head,
            params,
            plicities,
        }) => Subterm::Apply(Apply {
            head: normalize(context, head)?,
            params: normalize_each(context, params)?,
            plicities,
        }),
        Subterm::Proj(Proj { head, field }) => Subterm::Proj(Proj {
            head: normalize(context, head)?,
            field,
        }),
        Subterm::InductType(InductType {
            name,
            universes,
            params,
            indices,
        }) => Subterm::InductType(InductType {
            name,
            universes,
            params: normalize_each(context, params)?,
            indices: normalize_each(context, indices)?,
        }),
        Subterm::StructType(StructType {
            name,
            universes,
            params,
        }) => Subterm::StructType(StructType {
            name,
            universes,
            params: normalize_each(context, params)?,
        }),
        Subterm::Variant(Variant {
            name,
            universes,
            params,
            tag,
            payload,
        }) => Subterm::Variant(Variant {
            name,
            universes,
            params: normalize_each(context, params)?,
            tag,
            payload: normalize_each(context, payload)?,
        }),
        Subterm::Struct(Struct {
            name,
            universes,
            params,
            fields,
            entries,
        }) => Subterm::Struct(Struct {
            name,
            universes,
            params: normalize_each(context, params)?,
            fields: normalize_each(context, fields)?,
            entries,
        }),
        Subterm::UniverseInst(instance) => Subterm::UniverseInst(UniverseInst {
            head: normalize(context, instance.head)?,
            levels: instance.levels,
        }),
        Subterm::Tuple(Tuple { fields, names }) => Subterm::Tuple(Tuple {
            fields: normalize_each(context, fields)?,
            names,
        }),
        Subterm::FuncType(FuncType {
            telescope,
            plicities,
        }) => Subterm::FuncType(FuncType {
            telescope: normalize_telescope(context, telescope)?,
            plicities,
        }),
        Subterm::Func(Func {
            telescope,
            plicities,
        }) => Subterm::Func(Func {
            telescope: normalize_telescope(context, telescope)?,
            plicities,
        }),
        Subterm::TupleType(TupleType { telescope }) => Subterm::TupleType(TupleType {
            telescope: normalize_tuple_telescope(context, telescope)?,
        }),
        Subterm::Metavar(Metavar { id, spine, origin }) => Subterm::Metavar(Metavar {
            id,
            spine: normalize_each(context, spine.to_vec())?.into(),
            origin,
        }),
        // Leaves (`Type`/`Prop`/`Var`/`Prim`, the last already carrying reduced
        // operands) and the binder-heavy stuck forms (`Let`/`Rec`/`Match`) keep
        // their weak-head normal shape.
        other => other,
    };

    Ok(match span {
        Some(span) => Term::spanned(span, inner),
        None => Term::from(inner),
    })
}

fn normalize_each(context: &mut Context, terms: Vec<Term>) -> Result<Vec<Term>, ReduceError> {
    terms.into_iter().map(|t| normalize(context, t)).collect()
}

/// Normalize a function/Π telescope (`Func`/`FuncType`): each parameter type,
/// then the body opened under a fresh variable and re-closed under its label —
/// the display-side counterpart of [`convert`]'s `compare_func_type` walk.
fn normalize_telescope(
    context: &mut Context,
    telescope: Telescope<Term>,
) -> Result<Telescope<Term>, ReduceError> {
    match telescope {
        Telescope::Done(body) => Ok(Telescope::Done(Box::new(normalize(context, *body)?))),
        Telescope::Cons(ty, rest) => {
            let ty = normalize(context, ty)?;
            let label = context.fresh(rest.first_label());
            let inner = normalize_telescope(context, rest.open(&[&Term::free_var(&label)]))?;
            Ok(Telescope::Cons(
                ty,
                Scope::close(One, &[label.as_str()], inner),
            ))
        }
    }
}

/// Normalize a Σ telescope (`TupleType`): its field types, opening each field's
/// binder under a fresh variable exactly like [`normalize_telescope`]. The
/// `Done` body is `()`, carrying nothing to reduce.
fn normalize_tuple_telescope(
    context: &mut Context,
    telescope: Telescope<()>,
) -> Result<Telescope<()>, ReduceError> {
    match telescope {
        Telescope::Done(_) => Ok(Telescope::Done(Box::new(()))),
        Telescope::Cons(ty, rest) => {
            let ty = normalize(context, ty)?;
            let label = context.fresh(rest.first_label());
            let inner = normalize_tuple_telescope(context, rest.open(&[&Term::free_var(&label)]))?;
            Ok(Telescope::Cons(
                ty,
                Scope::close(One, &[label.as_str()], inner),
            ))
        }
    }
}
