//! The result-side change: reassociate a monoid-deferred recursion into a
//! tail-threaded accumulator.
//!
//! A fold whose recursive result feeds a registered monoid operator (a
//! `count_w(…) + 1`, a `chunk(h) ++ rest(t)`) cannot be a tail call: the combine
//! sits after the recursion, so the engine grows a frame per element. But
//! `g(x, acc) = acc ⊕ f(x)` *is* a loop, so this threads an accumulator seeded at
//! the monoid's identity and moves each combine's addend into it:
//!
//! ```text
//!   rec f = (xs) => … f(t) + k …
//!   ⟶  let f = (xs) => f@w(xs, 0)
//!      rec f@w = (xs, acc) => … f@w(t, acc + k) …
//! ```
//!
//! Recognising the shape at one `⊕(self, k)` node — its operator keyed in the
//! post-erasure monoid table, not the source operators — is what the desugared
//! `cont` would force us to reconstruct from blocks. Soundness needs the operator
//! to reassociate (associativity + an erasure-stable identity) and every absorbed
//! addend `k` to be **pure** (the [`CallGraph`] gate): the combine moves `k` across
//! the recursion, so a `k` that performed or triggered an effect would have it run
//! in a different order — or, after the base arm collapses, not at all.

use {
    super::{Change, Lower, ThreadedParam, tail_call},
    crate::{
        Argument, Func, Match, NatMatch, Prim, PurePrim, Subterm, Term,
        optm::{CallGraph, rewrite},
    },
    std::mem,
};

/// The result-side worker/wrapper change. See the module documentation.
pub(super) struct MonoidAccumulator;

/// A recognised summing recursion: the monoid every combine agrees on, and the
/// accumulator name to thread.
pub(super) struct MonoidPlan {
    monoid: Monoid,
    acc: String,
}

impl Change for MonoidAccumulator {
    type Plan = MonoidPlan;

    fn recognize(name: &str, func: &Func, cg: &CallGraph) -> Option<MonoidPlan> {
        let mut scan = Scan::default();
        scan_tail(&func.body, name, &mut scan);

        // One uniform monoid, at least one combine, every self-call on the
        // rewritable tail spine, and no declined non-commutative shape.
        if !scan.valid || scan.combines == 0 {
            return None;
        }
        let monoid = scan.monoid?;
        if count_self_calls(&func.body, name) != scan.tail_self_calls {
            return None;
        }

        // Purity gate: every addend the combine moves into the accumulator must be
        // pure — no effect, and no call to anything that performs one.
        if !scan.addends.iter().all(|addend| cg.term_is_pure(addend)) {
            return None;
        }

        Some(MonoidPlan {
            monoid,
            acc: format!("{name}@acc"),
        })
    }

    fn threaded(plan: &MonoidPlan) -> Vec<ThreadedParam> {
        vec![ThreadedParam {
            param: Argument::from(plan.acc.as_str()),
            seed: plan.monoid.identity(),
        }]
    }

    fn lower(plan: &MonoidPlan, ctx: &Lower, body: &mut Term) {
        let owned = mem::replace(body, Subterm::Erased.into());
        *body = rewrite_tail(owned, ctx, plan.monoid, &plan.acc);
    }
}

/// A monoid registered for accumulator reassociation, keyed in the *post-erasure*
/// `ersd` operators. No `And`/`all` row: boolean `BlnAnd` and bitwise `NatAnd` erase
/// to the same `NatAnd` with different, empty-observable identities, so no single
/// erased seed is sound for both (`suffix_view` / spec §5.1). `NatOr` (the `any`
/// fold, identity `0`, shared by boolean-or and bitwise-or) does ship.
#[derive(Clone, Copy, PartialEq)]
enum Monoid {
    NatAdd,
    NatMul,
    NatOr,
    IntAdd,
    IntMul,
    IntOr,
    BinAppend,
    LstAppend,
}

impl Monoid {
    /// The identity element seeded into the accumulator.
    fn identity(self) -> Term {
        let prim = match self {
            Monoid::NatAdd | Monoid::NatOr => PurePrim::Nat(0),
            Monoid::NatMul => PurePrim::Nat(1),
            Monoid::IntAdd | Monoid::IntOr => PurePrim::Int(0),
            Monoid::IntMul => PurePrim::Int(1),
            Monoid::BinAppend => PurePrim::Bin(Vec::new()),
            Monoid::LstAppend => PurePrim::Lst(Vec::new()),
        };
        Subterm::Prim(Prim::Pure(prim)).into()
    }

    /// Whether the operator commutes. The two append monoids do not, which limits
    /// where their addend may sit (see [`scan_tail`]).
    fn commutative(self) -> bool {
        !matches!(self, Monoid::BinAppend | Monoid::LstAppend)
    }

    /// `left ⊕ right` for this monoid.
    fn build(self, left: Term, right: Term) -> Term {
        let prim = match self {
            Monoid::NatAdd => PurePrim::NatAdd(left, right),
            Monoid::NatMul => PurePrim::NatMul(left, right),
            Monoid::NatOr => PurePrim::NatOr(left, right),
            Monoid::IntAdd => PurePrim::IntAdd(left, right),
            Monoid::IntMul => PurePrim::IntMul(left, right),
            Monoid::IntOr => PurePrim::IntOr(left, right),
            Monoid::BinAppend => PurePrim::BinAppend(left, right),
            Monoid::LstAppend => PurePrim::LstAppend(left, right),
        };
        Subterm::Prim(Prim::Pure(prim)).into()
    }

    /// The operands of `prim` if it is this monoid's operator, else the prim back.
    fn split(self, prim: PurePrim) -> Result<(Term, Term), PurePrim> {
        match (self, prim) {
            (Monoid::NatAdd, PurePrim::NatAdd(l, r))
            | (Monoid::NatMul, PurePrim::NatMul(l, r))
            | (Monoid::NatOr, PurePrim::NatOr(l, r))
            | (Monoid::IntAdd, PurePrim::IntAdd(l, r))
            | (Monoid::IntMul, PurePrim::IntMul(l, r))
            | (Monoid::IntOr, PurePrim::IntOr(l, r))
            | (Monoid::BinAppend, PurePrim::BinAppend(l, r))
            | (Monoid::LstAppend, PurePrim::LstAppend(l, r)) => Ok((l, r)),
            (_, other) => Err(other),
        }
    }
}

/// Classify a pure prim as a registered monoid operator, returning the monoid and
/// its two operands.
fn monoid_combine(prim: &PurePrim) -> Option<(Monoid, &Term, &Term)> {
    Some(match prim {
        PurePrim::NatAdd(l, r) => (Monoid::NatAdd, l, r),
        PurePrim::NatMul(l, r) => (Monoid::NatMul, l, r),
        PurePrim::NatOr(l, r) => (Monoid::NatOr, l, r),
        PurePrim::IntAdd(l, r) => (Monoid::IntAdd, l, r),
        PurePrim::IntMul(l, r) => (Monoid::IntMul, l, r),
        PurePrim::IntOr(l, r) => (Monoid::IntOr, l, r),
        PurePrim::BinAppend(l, r) => (Monoid::BinAppend, l, r),
        PurePrim::LstAppend(l, r) => (Monoid::LstAppend, l, r),
        _ => return None,
    })
}

/// What the tail-spine scan accumulates: the uniform monoid (set by the first
/// combine), the combine and tail-self-call counts, each absorbed addend (for the
/// purity gate), and whether the shape stayed within the firing envelope.
struct Scan<'a> {
    monoid: Option<Monoid>,
    combines: usize,
    tail_self_calls: usize,
    valid: bool,
    addends: Vec<&'a Term>,
}

impl Default for Scan<'_> {
    fn default() -> Self {
        Scan {
            monoid: None,
            combines: 0,
            tail_self_calls: 0,
            valid: true,
            addends: Vec::new(),
        }
    }
}

/// Walk the tail positions, tallying combines and tail self-calls and gating the
/// monoid. A combine is `⊕(self, k)` for a registered `⊕` with exactly one self
/// operand; mixed monoids, or a non-commutative combine with the recursion on the
/// *left* (`f(rest) ++ k` — needs a difference list, out of scope), set `valid`
/// false so recognition declines.
fn scan_tail<'a>(term: &'a Term, name: &str, scan: &mut Scan<'a>) {
    match term.as_subterm() {
        Subterm::NatMatch(NatMatch::Dispatch { cases, default, .. }) => {
            cases.values().for_each(|case| scan_tail(case, name, scan));
            scan_tail(default, name, scan);
        }
        Subterm::NatMatch(NatMatch::Induction {
            zero_case,
            succ_case,
            ..
        }) => {
            scan_tail(zero_case, name, scan);
            scan_tail(succ_case, name, scan);
        }
        Subterm::Match(Match { cases, .. }) => {
            cases.iter().for_each(|case| scan_tail(case, name, scan));
        }
        Subterm::Let(let_) => scan_tail(&let_.tail, name, scan),
        // A bare tail self-call carries the accumulator forward unchanged.
        Subterm::Apply(apply) if super::is_named(&apply.head, name) => scan.tail_self_calls += 1,
        Subterm::Prim(Prim::Pure(prim)) => {
            let Some((monoid, left, right)) = monoid_combine(prim) else {
                return;
            };
            let left_self = is_self_call(left, name);
            let right_self = is_self_call(right, name);
            if !(left_self ^ right_self) {
                return;
            }

            match scan.monoid {
                None => scan.monoid = Some(monoid),
                Some(prev) if prev == monoid => {}
                Some(_) => scan.valid = false,
            }
            scan.combines += 1;
            scan.tail_self_calls += 1;
            if !monoid.commutative() && left_self {
                scan.valid = false;
            }
            scan.addends.push(if left_self { right } else { left });
        }
        _ => {}
    }
}

/// Rewrite the tail positions to thread the accumulator into the worker. Non-tail
/// sub-terms (a `Let`'s bound value, a call's arguments) are left as is —
/// recognition has established no self-call hides there.
fn rewrite_tail(term: Term, ctx: &Lower, monoid: Monoid, acc: &str) -> Term {
    match term.into_subterm() {
        Subterm::NatMatch(NatMatch::Dispatch {
            head,
            cases,
            default,
        }) => Subterm::NatMatch(NatMatch::Dispatch {
            head,
            cases: cases
                .into_iter()
                .map(|(tag, case)| (tag, rewrite_tail(case, ctx, monoid, acc)))
                .collect(),
            default: rewrite_tail(default, ctx, monoid, acc),
        })
        .into(),
        Subterm::NatMatch(NatMatch::Induction {
            head,
            zero_case,
            pred,
            ih,
            succ_case,
        }) => Subterm::NatMatch(NatMatch::Induction {
            head,
            zero_case: rewrite_tail(zero_case, ctx, monoid, acc),
            pred,
            ih,
            succ_case: rewrite_tail(succ_case, ctx, monoid, acc),
        })
        .into(),
        Subterm::Match(Match { head, cases }) => Subterm::Match(Match {
            head,
            cases: cases
                .into_iter()
                .map(|case| rewrite_tail(case, ctx, monoid, acc))
                .collect(),
        })
        .into(),
        Subterm::Let(let_) => Subterm::Let(crate::Let {
            name: let_.name,
            body: let_.body,
            tail: rewrite_tail(let_.tail, ctx, monoid, acc),
        })
        .into(),
        // A bare tail self-call carries the accumulator forward unchanged.
        Subterm::Apply(apply) if ctx.is_self_call(&apply.head) => {
            tail_call(ctx.worker, apply.params, super::name_term(acc))
        }
        // A combine moves its addend into the accumulator; a non-combine prim in
        // tail position is a plain return wrapped as `acc ⊕ value`.
        Subterm::Prim(Prim::Pure(prim)) => match monoid.split(prim) {
            Ok((left, right)) if is_self_call_term(ctx, &left) => {
                let params = into_apply_params(left);
                tail_call(
                    ctx.worker,
                    params,
                    monoid.build(super::name_term(acc), right),
                )
            }
            Ok((left, right)) if is_self_call_term(ctx, &right) => {
                let params = into_apply_params(right);
                tail_call(
                    ctx.worker,
                    params,
                    monoid.build(super::name_term(acc), left),
                )
            }
            Ok((left, right)) => monoid.build(super::name_term(acc), monoid.build(left, right)),
            Err(prim) => monoid.build(
                super::name_term(acc),
                Subterm::Prim(Prim::Pure(prim)).into(),
            ),
        },
        // A plain return `v` becomes `acc ⊕ v`.
        other => monoid.build(super::name_term(acc), other.into()),
    }
}

/// Every self-call in the term, tail or not — to confirm they all sit on the
/// rewritable tail spine.
fn count_self_calls(term: &Term, name: &str) -> usize {
    let here =
        matches!(term.as_subterm(), Subterm::Apply(apply) if super::is_named(&apply.head, name));
    here as usize
        + rewrite::children(term)
            .iter()
            .map(|sub| count_self_calls(sub, name))
            .sum::<usize>()
}

/// Whether `term` is a self-call to the original name (recognition phase, before
/// any redirection to the worker).
fn is_self_call(term: &Term, name: &str) -> bool {
    matches!(term.as_subterm(), Subterm::Apply(apply) if super::is_named(&apply.head, name))
}

/// Whether `term` is a self-call to the recursion (lowering phase, where the head
/// may already be the worker).
fn is_self_call_term(ctx: &Lower, term: &Term) -> bool {
    matches!(term.as_subterm(), Subterm::Apply(apply) if ctx.is_self_call(&apply.head))
}

/// The arguments of a self-call term (owned).
fn into_apply_params(term: Term) -> Vec<Term> {
    match term.into_subterm() {
        Subterm::Apply(apply) => apply.params,
        _ => unreachable!("just checked it is a self-call"),
    }
}
