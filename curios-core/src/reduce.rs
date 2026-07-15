mod prim;
use prim::*;

#[cfg(test)]
mod tests;

use {
    super::{
        Apply, Bound, Carrier, Cases, Context, Field, FreeMonoid, Func, FuncType, InductiveType,
        Layer, Let, Match, Metavar, Nat, One, Prim, Proj, Rec, RecId, ReduceError, Scope, Struct,
        StructType, Subterm, Telescope, Term, Tuple, TupleType, Var, Variant,
    },
    crate::Instant,
    num_traits::ToPrimitive,
};

enum Reduce {
    Continue(Term),
    Break(Term),
}

/// Open a `rec` group: register each binding as a definition (so `reduce_var`
/// unfolds the recursive calls) and return the opened tail. The definitions
/// land in the enclosing context and outlive this call; their labels are
/// entropy-fresh, so nothing collides. Registered via `define_rec_members` —
/// one call for the whole group — so every member is marked recursive by
/// construction (see `DefEntry`).
fn unfold_rec_raw(context: &mut Context, rec: Rec) -> Term {
    let labels = rec
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

    let members = labels
        .iter()
        .cloned()
        .zip(rec.items.iter().map(|(_, body)| body.open(&label_refs)))
        .collect::<Vec<_>>();
    context.define_rec_members(&members);

    rec.tail.open(&label_refs)
}

/// Open a `rec` group, memoized per id (`Context::rec_unfold`/`remember_rec_unfold`):
/// every unfolding of the same local `rec` group yields the *same* opened
/// tail, so its recursive labels are stable. Without this, each unfolding
/// mints fresh labels, and conversion comparing two separately reduced copies
/// of one recursive call never sees them syntactically equal — each stuck
/// match's arms unfold another copy, and the comparison diverges. Keyed by
/// `Rec::id` rather than whole-term structural equality, so the memo stays
/// valid across any capture/release or cloning of the node. Shared with
/// `convert`, which enqueues the tail rather than reducing it eagerly.
///
/// `RecId::Unelaborated` is `Term::rec`'s placeholder for a group
/// `elaborate_rec` hasn't minted a real id for yet — every not-yet-elaborated
/// `Rec` shares it. A raw group *can* still reach here: a `rec`-group
/// member's unelaborated body is registered for mutual reference before its
/// own nested locals are elaborated (`elaborate_module_rec`), and a
/// sibling's conversion goal can force it in that window. Memoizing under the
/// shared placeholder would serve one raw group's cached unfolding to every
/// other unrelated raw group — so an unelaborated id always takes the
/// uncached path.
pub(crate) fn unfold_rec(context: &mut Context, rec: Rec) -> Term {
    let RecId::Elaborated(id) = rec.id else {
        return unfold_rec_raw(context, rec);
    };

    if let Some(tail) = context.rec_unfold(id) {
        return tail.clone();
    }

    let tail = unfold_rec_raw(context, rec);
    context.remember_rec_unfold(id, tail.clone());

    tail
}

/// Force a `rec` group in WHNF position. The main loop treats a `Rec` node as a
/// normal form, so an eliminator that demands its value unfolds it here and
/// re-reduces, repeating if the opened tail is itself a `rec`. A non-productive
/// group spins until the reduce deadline — exactly as a top-level `rec` does.
fn force_rec(context: &mut Context, mut term: Term) -> Result<Term, ReduceError> {
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

/// Reduce to WHNF and then force a `rec` head: used wherever an eliminator
/// (`match`/application/projection) demands a value, so an inner `rec` reduces
/// just like a top-level one instead of staying stuck.
fn reduce_forced(context: &mut Context, term: Term) -> Result<Term, ReduceError> {
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
/// type level (a runtime-only IO primitive like `is_ready`'s `/sys/Io/poll`
/// result, or an out-of-range access) is kept verbatim rather than forced. Such
/// an argument was never going to differ in spelling — the only occurrence is
/// the scrutinee itself, which matches the key raw — so keeping it raw both
/// avoids forcing effects at elaboration and still matches. A `Preempted`
/// deadline is the one error that propagates.
pub(crate) fn canonical_scrutinee(context: &mut Context, term: &Term) -> Result<Term, ReduceError> {
    match &**term {
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
    }
}

fn reduce_apply(context: &mut Context, apply: Apply) -> Result<Reduce, ReduceError> {
    let Apply {
        head,
        params,
        plicities,
    } = apply;

    // A recursive call keeps its *folded application* as the normal form for
    // two independent reasons — one permanent, one an elaboration-window
    // artifact — that share the same refold:
    //
    //   * Match-guarded delta: the unfolding leaves a stuck `match`, its
    //     arguments never reaching the shapes the dispatch demands. This is a
    //     genuine neutral, post-elaboration too. Unfolding anyway hands
    //     conversion a match whose arms hold further recursive calls, and
    //     comparing two distinct such trees unfolds forever; folded neutrals
    //     compare spine-wise instead. A later refinement of an argument
    //     re-fires the unfolding — `refine` clears the reduction cache.
    //
    //   * Transient leak: the unfolding's result carries elaboration-transient
    //     material at any depth (`Term::contains_transient` — a
    //     `NumLit`/`Infix`, or a nested `rec` still carrying
    //     `RecId::Unelaborated`, possibly wrapped in a constructor or other
    //     value shell), meaning the definition's raw, not-yet-elaborated body
    //     leaked through its mutual-checking window. The check is per *call*,
    //     on the reduced result — not on the registered body — so a clean
    //     path through a partly-raw body still reduces (a type family's
    //     `T(2) -> Nat` mid-window, with another arm still raw).
    //
    // Either way, folding back to the application — naming the head again
    // rather than keeping whatever structure `reduce` stopped at — lets a
    // later retry re-resolve the head from scratch once elaboration has
    // replaced its definition; the raw structure itself is a disconnected copy
    // nothing ever updates. Conversion then parks the goal
    // (`unfold_guarded_apply`'s `StillElaborating`) instead of comparing the
    // raw material and mismatching.
    let recursive = matches!(
        &*head,
        Subterm::Var(var) if context.is_recursive(var.unwrap())
    );

    let param_refs = params.iter().collect::<Vec<_>>();

    match Term::unwrap_or_clone(reduce_forced(context, head.clone())?) {
        Subterm::Func(Func { telescope }) => {
            let opened = telescope.open(&param_refs);

            if recursive {
                let result = reduce(context, opened)?;

                let refold = matches!(&*result, Subterm::Match(_)) || result.contains_transient();

                return Ok(Reduce::Break(if refold {
                    Term::from(Subterm::Apply(Apply {
                        head,
                        params,
                        plicities,
                    }))
                } else {
                    result
                }));
            }

            Ok(Reduce::Continue(opened))
        }
        head => Ok(Reduce::Break(Term::from(Subterm::Apply(Apply {
            head: head.into(),
            params,
            plicities,
        })))),
    }
}

/// The outcome of [`unfold_guarded_apply`].
pub(crate) enum GuardedUnfold {
    /// The head is not a recursive name, or does not resolve to a function —
    /// the caller's own fallback (structural comparison, a hard mismatch)
    /// applies; there is nothing more unfolding could reveal.
    NotApplicable,
    /// Unfolded to a genuine value.
    Unfolded(Term),
    /// The head is recursive, but its registered body is still raw:
    /// unfolding surfaced elaboration-transient material (see
    /// [`Term::contains_transient`]) instead of a settled value. Not a
    /// mismatch — the owning `rec` group is still inside its mutual-checking
    /// window; the caller should park the goal rather than compare the raw
    /// material.
    StillElaborating,
}

/// The one-step unfolding of a *guarded* recursive application — the reduced
/// body, even when that is a stuck match the guard in [`reduce_apply`] would
/// re-fold. Conversion's lazy-delta fallback uses this when a folded neutral
/// meets a shape it cannot match spine-wise: the literal match body it must
/// convert against can only be reached by unfolding.
pub(crate) fn unfold_guarded_apply(
    context: &mut Context,
    apply: &Apply,
) -> Result<GuardedUnfold, ReduceError> {
    let recursive = matches!(
        &*apply.head,
        Subterm::Var(var) if context.is_recursive(var.unwrap())
    );

    if !recursive {
        return Ok(GuardedUnfold::NotApplicable);
    }

    let param_refs = apply.params.iter().collect::<Vec<_>>();

    match Term::unwrap_or_clone(reduce_forced(context, apply.head.clone())?) {
        Subterm::Func(Func { telescope }) => {
            let result = reduce(context, telescope.open(&param_refs))?;

            Ok(match result.contains_transient() {
                true => GuardedUnfold::StillElaborating,
                false => GuardedUnfold::Unfolded(result),
            })
        }
        _ => Ok(GuardedUnfold::NotApplicable),
    }
}

fn reduce_proj(context: &mut Context, proj: Proj) -> Result<Reduce, ReduceError> {
    let Proj { head, field } = proj;
    // Label projections are resolved (and rebuilt positionally) by elaborate;
    // reduction only ever sees post-elaboration terms.
    let Field::Index(index) = field else {
        unreachable!("unresolved label projection reached reduction");
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
        // component. `reduce_inductive_match` relies on this to bind arms by
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

fn reduce_match(context: &mut Context, m: Match) -> Result<Reduce, ReduceError> {
    let Match {
        head,
        motive,
        cases,
    } = m;

    match cases {
        Cases::Bln {
            false_case,
            true_case,
        } => match Term::unwrap_or_clone(reduce_forced(context, head)?) {
            Subterm::Prim(Prim::Bln(false)) => Ok(Reduce::Continue(false_case)),
            Subterm::Prim(Prim::Bln(true)) => Ok(Reduce::Continue(true_case)),
            head => Ok(Reduce::Break(Term::from(Subterm::Match(Match {
                head: head.into(),
                motive,
                cases: Cases::Bln {
                    false_case,
                    true_case,
                },
            })))),
        },

        Cases::Switch { cases, default } => {
            let scrutinee = reduce_forced(context, head)?;
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

                    Ok(Reduce::Continue(body.clone()))
                }
                false => Ok(Reduce::Break(Term::from(Subterm::Match(Match {
                    head: scrutinee,
                    motive,
                    cases: Cases::Switch { cases, default },
                })))),
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
        Cases::Inductive {
            cases,
            pattern,
            default,
        } => {
            let head_reduced = reduce_forced(context, head.clone())?;

            if let Subterm::Variant(ctor) = &*head_reduced {
                if let Some(scope) = cases.get(&ctor.tag) {
                    let projections = (0..scope.arity())
                        .map(|i| Term::proj(head.clone(), i + 1))
                        .collect::<Vec<_>>();

                    let projection_refs = projections.iter().collect::<Vec<_>>();

                    return Ok(Reduce::Continue(scope.open(&projection_refs)));
                }

                // A concrete constructor with no enumerated arm takes the
                // catch-all default, which binds nothing (no scope to open).
                if let Some(default) = &default {
                    return Ok(Reduce::Continue(default.clone()));
                }
            }

            Ok(Reduce::Break(Term::from(Subterm::Match(Match {
                head: head_reduced,
                motive,
                cases: Cases::Inductive {
                    cases,
                    pattern,
                    default,
                },
            }))))
        }

        // Structural induction on a native free-monoid primitive (`Nat`/`Bin`/`Lst`).
        // The carrier-specific one-step decode lives in `FreeMonoid::uncons` (the
        // eliminator-side analogue of `spine::peel_prim`); this driver is the shared
        // catamorphism over it. An identity `Layer` takes the empty arm; a cons
        // `Layer` peels a generator (its head absent for the unary `Nat`) and
        // recurses symbolically for the induction hypothesis; a stuck scrutinee
        // rebuilds.
        Cases::FreeMonoid { carrier } => {
            let scrutinee = Term::unwrap_or_clone(reduce_forced(context, head)?);

            let layer = match &carrier {
                Carrier::Nat { .. } => FreeMonoid::Unary,
                Carrier::Bin { grain, .. } => FreeMonoid::Bin(*grain),
                Carrier::Lst { .. } => FreeMonoid::Lst,
            }
            .uncons(scrutinee);

            match layer {
                Layer::Empty => Ok(Reduce::Continue(match carrier {
                    Carrier::Nat { empty_case, .. }
                    | Carrier::Bin { empty_case, .. }
                    | Carrier::Lst { empty_case, .. } => empty_case,
                })),
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
                    Ok(Reduce::Continue(match &carrier {
                        Carrier::Nat { cons_case, .. } => cons_case.open(&[&tail, &ih]),
                        Carrier::Bin { cons_case, .. } | Carrier::Lst { cons_case, .. } => {
                            cons_case.open(&[
                                head.as_ref().expect("Bin/Lst cons layer carries a head"),
                                &tail,
                                &ih,
                            ])
                        }
                    }))
                }
                Layer::Stuck(scrutinee) => Ok(Reduce::Break(Term::from(Subterm::Match(Match {
                    head: scrutinee.into(),
                    motive,
                    cases: Cases::FreeMonoid { carrier },
                })))),
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

    for (i, (label, (_, value))) in labels.iter().zip(let_.bindings.iter()).enumerate() {
        context.define(label, &value.release(&label_refs[..i]));
    }

    Reduce::Continue(let_.tail.open(&label_refs))
}

fn reduce_var(context: &Context, var: Var) -> Reduce {
    // `var_reduct` withholds a recursive member still inside its raw window
    // (see `Context::var_reduct`): the name breaks as its own neutral — the
    // value-level counterpart of the refold `reduce_apply` performs for a
    // recursive *application* — rather than substituting the
    // elaboration-transient body and letting a consumer freeze a disconnected
    // copy of it (a conversion comparison, or the subtler leak: a
    // metavariable *solved* to it, `?a := reduce(x)`).
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

pub(crate) fn reduce(context: &mut Context, term: Term) -> Result<Term, ReduceError> {
    context.get_or_init_reduced(term, |context, mut term| {
        loop {
            if Instant::now() > context.deadline() {
                break Err(ReduceError::Preempted);
            }

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
                        break Ok(canonical);
                    }
                } else if let Some(value) = context.scrutinee_reduct(&canonical) {
                    term = value.clone();
                    continue;
                }
            }

            let step = match Term::unwrap_or_clone(term) {
                Subterm::Prim(prim) => Reduce::Break(reduce_prim(context, &prim)?.into()),
                Subterm::Match(m) => reduce_match(context, m)?,
                Subterm::Apply(apply) => reduce_apply(context, apply)?,
                Subterm::Proj(proj) => reduce_proj(context, proj)?,
                Subterm::Func(func) => reduce_func_eta(context, func)?,
                Subterm::Let(let_) => reduce_let(context, let_),
                Subterm::Var(var) => reduce_var(context, var),
                Subterm::Metavar(metavar) => reduce_metavar(context, metavar),
                // `InductiveType`/`Variant` and `StructType`/`Struct` are primitive
                // normal forms, like `Tuple`: their sub-terms are not reduced
                // in WHNF.
                term => Reduce::Break(term.into()),
            };

            match step {
                Reduce::Continue(next) => term = next,
                Reduce::Break(result) => break Ok(result),
            }
        }
    })
}

/// Reduce `term` to a deep normal form for **diagnostic display**: every
/// position is taken to weak-head normal form and its sub-terms recursively
/// normalized, opening the type-former binders (`FuncType`/`Func`/`TupleType`)
/// under fresh variables.
///
/// `reduce` alone stops at the head: an inductive type's indices are not
/// sub-reduced, so a concept-method projection standing in an index position —
/// `Vec(Nat, Add/add(0, 1))`, spelled `Vec(Nat, (sys/witness#0).0(0, 1))` once
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
    let reduced = reduce(context, term)?;
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
        Subterm::InductiveType(InductiveType {
            name,
            params,
            indices,
        }) => Subterm::InductiveType(InductiveType {
            name,
            params: normalize_each(context, params)?,
            indices: normalize_each(context, indices)?,
        }),
        Subterm::StructType(StructType { name, params }) => Subterm::StructType(StructType {
            name,
            params: normalize_each(context, params)?,
        }),
        Subterm::Variant(Variant {
            name,
            params,
            tag,
            payload,
        }) => Subterm::Variant(Variant {
            name,
            params: normalize_each(context, params)?,
            tag,
            payload: normalize_each(context, payload)?,
        }),
        Subterm::Struct(Struct {
            name,
            params,
            fields,
            entries,
        }) => Subterm::Struct(Struct {
            name,
            params: normalize_each(context, params)?,
            fields: normalize_each(context, fields)?,
            entries,
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
        Subterm::Func(Func { telescope }) => Subterm::Func(Func {
            telescope: normalize_telescope(context, telescope)?,
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
