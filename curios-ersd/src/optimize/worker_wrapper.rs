//! The worker/wrapper engine: generalize a linear non-tail self-recursion into a
//! thin wrapper around a tail-recursive *worker*, so the ordinary lowering plus
//! [`convert_tail_recursion`](curios_cont::optimize) turn the worker into a loop.
//!
//! A non-tail linear recursion `f(x) = E[ f(d(x)) ]` carries two independent costs,
//! each addressed by an orthogonal, composable *change* over the original body:
//!
//! - **Stack** — the context `E` defers work past the call. When `E` is a
//!   composition over a registered monoid (`… + 1`, `… ++ k`), the deferred work
//!   reassociates into an accumulator threaded in tail position:
//!   [`MonoidAccumulator`]. Sound only when `E` is *pure* (the [`CallGraph`] gate).
//! - **Per-step cost** — the argument `d(x)` re-slices an `O(n)` buffer suffix.
//!   When the buffer is only *observed* (`len`/`get`/drop-front `slice`), the copy
//!   is unnecessary and an integer cursor over the fixed base replaces it:
//!   [`SliceCursor`] (the slice re-base laws across recursion).
//!
//! `count_w` (`Str/len`) needs both. The driver recognises each change on the
//! *original* body, then lowers them in a fixed order — **cursor first, then
//! monoid** — so the cursor renames the self-calls to the worker and appends the
//! offset, and the monoid then sees worker-headed self-calls and appends the
//! accumulator. The worker's parameters `[orig…, offset, acc]` line up with the
//! arguments each lowered self-call now carries.
//!
//! "All-or-nothing" is *per change*: one illegible use makes that change's
//! `recognize` return `None`, and the changes do not gate each other —
//! monoid-only is the old `accumulate`, cursor-only the old `offsets`, both
//! declining leaves `f` untouched (`suffixes`, mutual recursion, non-monoidal
//! deferral). Sound-but-incomplete is the contract: outside the envelope the engine
//! is a no-op, never a miscompile.

mod cursor;
mod monoid;

use {
    super::{CallGraph, refresh_captures},
    crate::{Apply, Argument, Func, Item, Module, Name, Prim, PurePrim, Rec, Subterm, Term},
    cursor::SliceCursor,
    monoid::MonoidAccumulator,
    std::{collections::HashMap, iter, mem},
};

/// Introduce worker/wrapper transforms across the module, in place.
///
/// Visits every top-level definition and, bottom-up, every nested single-binding
/// `rec` inside one — exactly the recursions whose self-name is in scope. A
/// multi-binding (mutual) `rec` declines naturally: recognition keys on each
/// member's own name, and a mutual member makes no direct self-call.
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub(crate) fn introduce_worker_wrappers(module: &mut Module) {
    let graph = CallGraph::build(module);

    for item in &mut module.items {
        match item {
            Item::Let { body, .. } => descend(body, &graph),
            Item::Rec { names, items } => {
                // `names` and `items` are parallel; snapshot the names so the items
                // can be walked mutably alongside them.
                let names = names.clone();
                for (name, term) in names.iter().zip(items.iter_mut()) {
                    descend(term, &graph);
                    try_transform(name, term, &graph);
                }
            }
        }
    }
}

/// Walk a term bottom-up, transforming every single-binding `rec` whose body is a
/// recognised recursion. `Prim` operands are not descended into — a `rec` never
/// sits inside a primitive operand, so skipping them is a safe under-approximation.
/// The freshly-built `rec f@w` the transform installs is never re-entered (the walk
/// is over the *original* children).
fn descend(term: &mut Term, graph: &CallGraph) {
    for child in subterms_mut(term) {
        descend(child, graph);
    }

    if let Subterm::Rec(rec) = term.as_subterm_mut()
        && rec.names.len() == 1
        && rec.items.len() == 1
    {
        let name = rec.names[0].clone();
        try_transform(&name, &mut rec.items[0], graph);
    }
}

/// The mutable rename context a change's `lower` consults: the original self-name
/// and the worker it is being redirected to.
struct Lower<'a> {
    name: &'a str,
    worker: &'a str,
}

impl Lower<'_> {
    /// Whether `head` names the recursion — either still the original name (the
    /// first change to lower sees these) or the worker (a later change sees the
    /// heads an earlier one already redirected).
    fn is_self_call(&self, head: &Term) -> bool {
        matches!(head.as_subterm(), Subterm::Name(named) if named.as_str() == self.name || named.as_str() == self.worker)
    }
}

/// A parameter a change threads into the worker: the worker's extra binder and the
/// seed value the wrapper passes for it on the initial call.
struct ThreadedParam {
    param: Argument,
    seed: Term,
}

/// A worker/wrapper *change*: an orthogonal axis (result-side monoid, argument-side
/// cursor) that recognises its shape on the original body, declares the parameters
/// it threads into the worker, and lowers the body in place. Concrete instances are
/// composed by name in a fixed order (not a dynamic registry) so each can be
/// lowered against the other's redirected self-calls.
trait Change {
    type Plan;

    /// Detect the shape and gate it, read-only. `None` declines (the change does
    /// not fire); the function is left untouched unless another change fires.
    fn recognize(name: &str, func: &Func, cg: &CallGraph) -> Option<Self::Plan>;

    /// The worker parameters this change adds, with their wrapper seeds, appended
    /// after the original parameters in lowering order.
    fn threaded(plan: &Self::Plan) -> Vec<ThreadedParam>;

    /// Rewrite the body in place: thread the change's parameter and redirect each
    /// self-call to the worker (via [`Lower::is_self_call`]).
    fn lower(plan: &Self::Plan, ctx: &Lower, body: &mut Term);
}

/// Try to wrap a single recursion. Recognises both changes on the original body; if
/// neither fires, leaves `term` untouched and returns `false`. Otherwise builds the
/// worker (original parameters plus each firing change's threaded parameters) and a
/// wrapper that seeds them, lowering the changes **cursor-first, then monoid** so
/// the worker's parameters line up with the arguments each lowered self-call now
/// carries.
fn try_transform(name: &str, term: &mut Term, graph: &CallGraph) -> bool {
    let Subterm::Func(func) = term.as_subterm() else {
        return false;
    };

    let cursor_plan = SliceCursor::recognize(name, func, graph);
    let monoid_plan = MonoidAccumulator::recognize(name, func, graph);
    if cursor_plan.is_none() && monoid_plan.is_none() {
        return false;
    }

    // Threaded parameters in fixed lowering order: cursor's offset, then monoid's
    // accumulator. The order is an invariant (see the module doc and the `lower`
    // calls below).
    let mut threaded = Vec::new();
    if let Some(plan) = &cursor_plan {
        threaded.extend(SliceCursor::threaded(plan));
    }
    if let Some(plan) = &monoid_plan {
        threaded.extend(MonoidAccumulator::threaded(plan));
    }

    let worker = format!("{name}@w");

    let Subterm::Func(Func {
        captures,
        params,
        body,
    }) = mem::replace(term, Subterm::Erased.into()).into_subterm()
    else {
        unreachable!("just matched a Func")
    };

    // Lower in place, cursor first so the monoid sees worker-headed self-calls.
    let ctx = Lower {
        name,
        worker: &worker,
    };
    let mut worker_body = body;
    if let Some(plan) = &cursor_plan {
        SliceCursor::lower(plan, &ctx, &mut worker_body);
    }
    if let Some(plan) = &monoid_plan {
        MonoidAccumulator::lower(plan, &ctx, &mut worker_body);
    }

    // The names the rewrite newly introduces, with the candidate flag each gets
    // when it surfaces as a closure capture (the worker is a function — a
    // specialization candidate; the offset and accumulator are scalars).
    let introduced = iter::once((worker.clone(), true))
        .chain(
            threaded
                .iter()
                .map(|t| (t.param.name.clone(), t.param.candidate)),
        )
        .collect::<HashMap<String, bool>>();

    // Worker: `(orig…, threaded…) => worker_body`.
    let mut worker_params = clone_args(&params);
    worker_params.extend(threaded.iter().map(|t| clone_arg(&t.param)));
    let worker_func: Term = Subterm::Func(Func {
        captures: clone_args(&captures),
        params: worker_params,
        body: worker_body,
    })
    .into();

    // Wrapper body: `rec f@w = worker_func; f@w(orig…, seed…)`.
    let seed = params
        .iter()
        .map(|param| name_term(&param.name))
        .chain(threaded.into_iter().map(|t| t.seed))
        .collect::<Vec<_>>();
    let wrapper_body: Term = Subterm::Rec(Rec {
        names: vec![worker.clone()],
        items: vec![worker_func],
        tail: Subterm::Apply(Apply {
            head: name_term(&worker),
            params: seed,
        })
        .into(),
    })
    .into();

    *term = Subterm::Func(Func {
        captures,
        params,
        body: wrapper_body,
    })
    .into();

    // The rewrite changed which names are free where (self-calls became `worker`,
    // the threaded parameters are now read), but the closures it ran through carry
    // stale capture lists. Recompute every closure's captures bottom-up — exactly
    // once, after *both* changes have lowered — so each lists its body's free
    // variables; the wrapper recomputes too, dropping its now-unused self-ref.
    refresh_captures(term, &introduced);
    true
}

/// A tail call to the worker: its original (already-rewritten) arguments, then one
/// appended threaded argument.
fn tail_call(worker: &str, mut params: Vec<Term>, threaded: Term) -> Term {
    params.push(threaded);
    Subterm::Apply(Apply {
        head: name_term(worker),
        params,
    })
    .into()
}

/// A by-value copy of an argument list (`Argument` is not `Clone`).
fn clone_args(args: &[Argument]) -> Vec<Argument> {
    args.iter().map(clone_arg).collect()
}

fn clone_arg(arg: &Argument) -> Argument {
    Argument {
        name: arg.name.clone(),
        candidate: arg.candidate,
    }
}

fn is_named(head: &Term, name: &str) -> bool {
    matches!(head.as_subterm(), Subterm::Name(named) if named.as_str() == name)
}

fn name_term(name: &str) -> Term {
    Subterm::Name(Name::from(name)).into()
}

fn nat(value: u32) -> Term {
    Subterm::Prim(Prim::Pure(PurePrim::Nat(value))).into()
}

fn nat_add(left: Term, right: Term) -> Term {
    Subterm::Prim(Prim::Pure(PurePrim::NatAdd(left, right))).into()
}

fn nat_sub(left: Term, right: Term) -> Term {
    Subterm::Prim(Prim::Pure(PurePrim::NatSub(left, right))).into()
}

/// The immediate sub-terms of a term, mutably — the structural children only, *not*
/// primitive operands (a `rec` never nests inside one, so the cursor walk skips
/// them).
fn subterms_mut(term: &mut Term) -> Vec<&mut Term> {
    use crate::NatMatch;

    match term.as_subterm_mut() {
        Subterm::Erased
        | Subterm::Unreachable
        | Subterm::Atom(_)
        | Subterm::Name(_)
        | Subterm::Prim(_) => vec![],
        Subterm::NatMatch(NatMatch::Induction {
            head,
            zero_case,
            succ_case,
            ..
        }) => vec![head, zero_case, succ_case],
        Subterm::NatMatch(NatMatch::Dispatch {
            head,
            cases,
            default,
        }) => iter::once(head)
            .chain(cases.values_mut())
            .chain(iter::once(default))
            .collect(),
        Subterm::Func(func) => vec![&mut func.body],
        Subterm::Apply(apply) => iter::once(&mut apply.head)
            .chain(&mut apply.params)
            .collect(),
        Subterm::Tuple(tuple) => tuple.fields.iter_mut().collect(),
        Subterm::Proj(proj) => vec![&mut proj.head],
        Subterm::Match(m) => iter::once(&mut m.head)
            .chain(m.cases.values_mut())
            .chain(m.default.iter_mut())
            .collect(),
        Subterm::Let(let_) => let_
            .bindings
            .iter_mut()
            .map(|(_, body)| body)
            .chain(iter::once(&mut let_.tail))
            .collect(),
        Subterm::Rec(rec) => rec
            .items
            .iter_mut()
            .chain(iter::once(&mut rec.tail))
            .collect(),
    }
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        crate::{CellPrim, Match, NatMatch},
        curios_base::Grain,
    };

    fn call(head: &str, args: Vec<Term>) -> Term {
        Subterm::Apply(Apply {
            head: name_term(head),
            params: args,
        })
        .into()
    }

    fn prim(prim: PurePrim) -> Term {
        Subterm::Prim(Prim::Pure(prim)).into()
    }

    fn bin_len(buffer: Term) -> Term {
        prim(PurePrim::BinLen(Grain::X, buffer))
    }

    fn bin_get(buffer: Term, index: Term) -> Term {
        prim(PurePrim::BinGet(Grain::X, buffer, index))
    }

    fn bin_slice(buffer: Term, from: Term, upto: Term) -> Term {
        prim(PurePrim::BinSlice(Grain::X, buffer, from, upto))
    }

    fn lst_len(buffer: Term) -> Term {
        prim(PurePrim::LstLen(buffer))
    }

    fn lst_get(buffer: Term, index: Term) -> Term {
        prim(PurePrim::LstGet(buffer, index))
    }

    fn lst_slice(buffer: Term, from: Term, upto: Term) -> Term {
        prim(PurePrim::LstSlice(buffer, from, upto))
    }

    fn add(left: Term, right: Term) -> Term {
        prim(PurePrim::NatAdd(left, right))
    }

    fn or(left: Term, right: Term) -> Term {
        prim(PurePrim::NatOr(left, right))
    }

    fn and(left: Term, right: Term) -> Term {
        prim(PurePrim::NatAnd(left, right))
    }

    fn cell_get(cell: Term) -> Term {
        Subterm::Prim(Prim::Cell(CellPrim::Get(cell))).into()
    }

    /// `Nat.match head | 0 => zero | _ => default` — the shape a `Bin`/`Lst` length
    /// fold erases to.
    fn nat_match(head: Term, zero: Term, default: Term) -> Term {
        Subterm::NatMatch(NatMatch::Dispatch {
            head,
            cases: iter::once((0, zero)).collect(),
            default,
        })
        .into()
    }

    /// `match head | @0 => a | @1 => b` — a two-arm value match (a list
    /// eliminator), fully covered (keyed positionally, no catch-all).
    fn value_match(head: Term, cases: Vec<Term>) -> Term {
        Subterm::Match(Match {
            head,
            cases: cases.into_iter().enumerate().collect(),
            default: None,
        })
        .into()
    }

    /// A module of one single-binding `rec name = (params) => body`, the recursion
    /// closing over its own name (as the erased IR builds it).
    fn rec_module(name: &str, params: Vec<&str>, body: Term) -> Module {
        Module {
            items: vec![Item::Rec {
                names: vec![name.to_owned()],
                items: vec![
                    Subterm::Func(Func {
                        captures: vec![Argument::from(name)],
                        params: params.into_iter().map(Argument::from).collect(),
                        body,
                    })
                    .into(),
                ],
            }],
            body: Subterm::Erased.into(),
        }
    }

    fn run(mut module: Module) -> String {
        introduce_worker_wrappers(&mut module);
        format!("{module}")
    }

    // --- Migrated from the deleted `offsets` pass (now asserting on `f@w`). ---

    #[test]
    fn drop_front_recursion_threads_a_cursor() {
        // `rec f(b) = match len b | 0 => 0 | _ => f(slice b 1 (len b))`: the suffix
        // recursion is recognised and rewritten to a cursor loop. The one slice — the
        // drop-front — is gone, replaced by an advanced `offset`.
        let body = nat_match(
            bin_len(name_term("b")),
            nat(0),
            call(
                "f",
                vec![bin_slice(name_term("b"), nat(1), bin_len(name_term("b")))],
            ),
        );
        let printed = run(rec_module("f", vec!["b"], body));

        assert!(
            printed.contains("#f@w"),
            "expected a worker loop:\n{printed}"
        );
        assert!(
            printed.contains("#b@offset"),
            "expected a threaded offset:\n{printed}"
        );
        assert!(
            !printed.contains("Bin.slice"),
            "the drop-front slice should be gone:\n{printed}"
        );
    }

    #[test]
    fn buffer_escaping_to_another_primitive_is_declined() {
        // `b` flows into `Bin.eql` (a whole-buffer read), so the virtual suffix would
        // have to be materialised — the recursion is left untouched.
        let body = value_match(
            prim(PurePrim::BinEql(Grain::X, name_term("b"), name_term("b"))),
            vec![
                call(
                    "f",
                    vec![bin_slice(name_term("b"), nat(1), bin_len(name_term("b")))],
                ),
                nat(0),
            ],
        );
        let printed = run(rec_module("f", vec!["b"], body));

        assert!(
            !printed.contains("#f@w"),
            "the escaping buffer must not be threaded:\n{printed}"
        );
    }

    #[test]
    fn non_suffix_recursion_is_declined() {
        // The recursion drops the *back*, not the front: `slice b 0 (len b − 1)` is
        // not a suffix (its end is not `len b`), so no cursor applies.
        let body = nat_match(
            bin_len(name_term("b")),
            nat(0),
            call(
                "f",
                vec![bin_slice(
                    name_term("b"),
                    nat(0),
                    nat_sub(bin_len(name_term("b")), nat(1)),
                )],
            ),
        );
        let printed = run(rec_module("f", vec!["b"], body));

        assert!(
            !printed.contains("#f@w"),
            "a non-suffix recursion must be declined:\n{printed}"
        );
    }

    // --- New coverage: monoid instances and a generic cursor walk. ---

    #[test]
    fn linked_list_len_threads_an_accumulator() {
        // `rec len(xs) = match xs | nil => 0 | cons(_, t) => 1 + len(t)` — result-side
        // monoid only (no buffer), the `+ 1` reassociated into an accumulator.
        let body = value_match(
            name_term("xs"),
            vec![nat(0), add(nat(1), call("len", vec![name_term("t")]))],
        );
        let printed = run(rec_module("len", vec!["xs"], body));

        assert!(
            printed.contains("#len@w"),
            "expected a worker loop:\n{printed}"
        );
        assert!(
            printed.contains("#len@acc"),
            "expected a threaded accumulator:\n{printed}"
        );
    }

    #[test]
    fn array_sum_composes_cursor_and_accumulator() {
        // `rec sum(xs) = match len xs | 0 => 0 | _ => get(xs, 0) + sum(slice xs 1 (len
        // xs))` — both axes at once: the cursor threads an offset over `xs` and the
        // accumulator absorbs the `get`, so the drop-front slice disappears.
        let body = nat_match(
            lst_len(name_term("xs")),
            nat(0),
            add(
                lst_get(name_term("xs"), nat(0)),
                call(
                    "sum",
                    vec![lst_slice(name_term("xs"), nat(1), lst_len(name_term("xs")))],
                ),
            ),
        );
        let printed = run(rec_module("sum", vec!["xs"], body));

        assert!(printed.contains("#sum@w"), "expected a worker:\n{printed}");
        assert!(
            printed.contains("#xs@offset"),
            "expected a threaded offset:\n{printed}"
        );
        assert!(
            printed.contains("#sum@acc"),
            "expected a threaded accumulator:\n{printed}"
        );
        assert!(
            !printed.contains("Lst.slice"),
            "the drop-front slice should be gone:\n{printed}"
        );
    }

    #[test]
    fn boolean_any_threads_an_or_accumulator() {
        // `rec any(xs) = match xs | nil => 0 | cons(h, t) => or(h, any(t))` — the
        // `NatOr` fold (`Bln/any` erases to it; identity `0`).
        let body = value_match(
            name_term("xs"),
            vec![
                nat(0),
                or(name_term("h"), call("any", vec![name_term("t")])),
            ],
        );
        let printed = run(rec_module("any", vec!["xs"], body));

        assert!(printed.contains("#any@w"), "expected a worker:\n{printed}");
        assert!(
            printed.contains("#any@acc"),
            "expected a threaded accumulator:\n{printed}"
        );
        assert!(
            printed.contains("Nat.or"),
            "the accumulator should combine with Nat.or:\n{printed}"
        );
    }

    #[test]
    fn index_only_buffer_walk_threads_a_cursor() {
        // `rec scan(b) = match len b | 0 => _ | _ => let h = get(b, 0); scan(slice b 1
        // (len b))` — a cursor instance beyond `count_w`: the buffer is only indexed,
        // no monoid, yet the re-slice still becomes an offset.
        let step = Subterm::Let(crate::Let {
            bindings: vec![("h".to_owned(), bin_get(name_term("b"), nat(0)))],
            tail: call(
                "scan",
                vec![bin_slice(name_term("b"), nat(1), bin_len(name_term("b")))],
            ),
        })
        .into();
        let body = nat_match(bin_len(name_term("b")), Subterm::Erased.into(), step);
        let printed = run(rec_module("scan", vec!["b"], body));

        assert!(printed.contains("#scan@w"), "expected a worker:\n{printed}");
        assert!(
            printed.contains("#b@offset"),
            "expected a threaded offset:\n{printed}"
        );
        assert!(
            !printed.contains("Bin.slice"),
            "the drop-front slice should be gone:\n{printed}"
        );
    }

    // --- No-fire: left untouched (meaning unchanged), never miscompiled. ---

    #[test]
    fn suffixes_keeps_the_copy() {
        // `rec suffixes(b) = match len b | 0 => _ | _ => (b, suffixes(slice b 1 (len
        // b)))` — `b` is stored in the result, so it escapes: both changes decline and
        // the copy stays.
        let step = Subterm::Tuple(crate::Tuple {
            fields: vec![
                name_term("b"),
                call(
                    "suffixes",
                    vec![bin_slice(name_term("b"), nat(1), bin_len(name_term("b")))],
                ),
            ],
        })
        .into();
        let body = nat_match(bin_len(name_term("b")), Subterm::Erased.into(), step);
        let printed = run(rec_module("suffixes", vec!["b"], body));

        assert!(
            !printed.contains("#suffixes@w"),
            "an escaping buffer must not be threaded:\n{printed}"
        );
    }

    #[test]
    fn tree_recursion_is_declined() {
        // `rec f(x) = f(left) + f(right)` — non-linear (two self-calls), so neither a
        // monoid accumulator (both operands recurse) nor a cursor applies.
        let body = add(
            call("f", vec![name_term("left")]),
            call("f", vec![name_term("right")]),
        );
        let printed = run(rec_module("f", vec!["x"], body));

        assert!(
            !printed.contains("#f@w"),
            "a tree recursion must be declined:\n{printed}"
        );
    }

    #[test]
    fn mutual_recursion_is_declined() {
        // `rec even(n) = … odd(n) and odd(n) = … even(n)` — recognition keys on each
        // member's own name, and neither makes a direct self-call.
        let mut module = Module {
            items: vec![Item::Rec {
                names: vec!["even".to_owned(), "odd".to_owned()],
                items: vec![
                    Subterm::Func(Func {
                        captures: vec![Argument::from("odd")],
                        params: vec![Argument::from("n")],
                        body: nat_match(name_term("n"), nat(1), call("odd", vec![name_term("n")])),
                    })
                    .into(),
                    Subterm::Func(Func {
                        captures: vec![Argument::from("even")],
                        params: vec![Argument::from("n")],
                        body: nat_match(name_term("n"), nat(0), call("even", vec![name_term("n")])),
                    })
                    .into(),
                ],
            }],
            body: Subterm::Erased.into(),
        };
        introduce_worker_wrappers(&mut module);
        let printed = format!("{module}");

        assert!(
            !printed.contains("#even@w") && !printed.contains("#odd@w"),
            "a mutual recursion must be declined:\n{printed}"
        );
    }

    #[test]
    fn effectful_addend_fails_the_purity_gate() {
        // `rec f(xs) = match xs | nil => 0 | cons => Cell.get(c) + f(t)` — the combine
        // would move an effectful read across the recursion, so the monoid declines.
        let body = value_match(
            name_term("xs"),
            vec![
                nat(0),
                add(cell_get(name_term("c")), call("f", vec![name_term("t")])),
            ],
        );
        let printed = run(rec_module("f", vec!["xs"], body));

        assert!(
            !printed.contains("#f@w"),
            "an effectful addend must not be accumulated:\n{printed}"
        );
    }

    #[test]
    fn bitwise_and_fold_is_declined() {
        // `rec all(xs) = match xs | nil => 1 | cons(h, t) => and(h, all(t))` — the
        // dropped AND row: `BlnAnd`/bitwise `NatAnd` erase alike with empty-observable
        // identities, so no `NatAnd` monoid is registered and the fold stays as is.
        let body = value_match(
            name_term("xs"),
            vec![
                nat(1),
                and(name_term("h"), call("all", vec![name_term("t")])),
            ],
        );
        let printed = run(rec_module("all", vec!["xs"], body));

        assert!(
            !printed.contains("#all@w"),
            "the unsound AND row must not fire:\n{printed}"
        );
    }
}
