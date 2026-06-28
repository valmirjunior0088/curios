use {
    super::{
        Apply, Argument, Func, Item, Match, Module, Name, NatMatch, Prim, PurePrim, Rec, Subterm,
        Term,
    },
    std::iter,
};

/// Accumulator-introduction for `Nat`-summing self-recursion, on the erased IR
/// where the shape is still legible.
///
/// A fold whose recursive result feeds `NatAdd(self-call, k)` — `/std/Str/count_w`
/// (`Str/len`)'s lead arm is `count_w(step, t) + 1` — can't be a tail call: the
/// add sits after the recursion. Lowered as is, the engine grows a frame per
/// element and a few thousand of them overflow the wasm stack. But `g(x, acc) =
/// acc + f(x)` *is* a loop, so this rewrites
///
/// ```text
///   rec f = (xs) => … f(t) + 1 …
/// ```
///
/// into a thin wrapper around an inner accumulating recursion:
///
/// ```text
///   rec f = (xs) => (rec f@sum = (xs, acc) => … f@sum(t, acc + 1) …)(xs, 0)
/// ```
///
/// `f@sum` is now plainly tail-recursive, so the ordinary lowering plus
/// [`convert_tail_recursion`](crate::optm) turn it into a loop — O(1) stack. `f`'s
/// own signature is untouched (the accumulator is internal), so no caller changes.
/// Recognising the shape here, at one `NatAdd(Apply(self), lit)` node, is what the
/// desugared `cont` would force us to reconstruct from blocks.
pub fn introduce_accumulators(module: &mut Module) {
    for item in &mut module.items {
        let Item::Rec { names, items } = item else {
            continue;
        };

        // `names` and `items` are parallel; snapshot the names so the items can be
        // walked mutably alongside them.
        let names = names.clone();
        for (name, term) in names.iter().zip(items.iter_mut()) {
            accumulate(name, term);
        }
    }
}

fn accumulate(name: &str, term: &mut Term) {
    let Subterm::Func(func) = term.as_subterm() else {
        return;
    };
    if !is_summing(name, func) {
        return;
    }

    let owned = std::mem::replace(term, Subterm::Erased.into());
    let Subterm::Func(Func {
        captures,
        params,
        body,
    }) = owned.into_subterm()
    else {
        unreachable!("just matched a Func")
    };

    let inner = format!("{name}@sum");
    let accumulator = "acc@sum";

    // Thread the accumulator: self-calls become tail calls to the inner
    // recursion, the `+ k` of a combine moves into the accumulator, and each
    // return becomes `acc + value`.
    let inner_body = rewrite_tail(body, name, &inner, accumulator);

    // The inner recursion still closes over a self-reference — now its *own*
    // name; the wrapper recurses through neither, so it drops the reference.
    let inner_captures = captures
        .iter()
        .map(|capture| Argument {
            name: if capture.name == name {
                inner.clone()
            } else {
                capture.name.clone()
            },
            candidate: capture.candidate,
        })
        .collect::<Vec<_>>();
    let wrapper_captures = captures
        .into_iter()
        .filter(|capture| capture.name != name)
        .collect::<Vec<_>>();

    // Inner accumulating recursion: (params…, acc) => inner_body.
    let mut inner_params = params
        .iter()
        .map(|param| Argument {
            name: param.name.clone(),
            candidate: param.candidate,
        })
        .collect::<Vec<_>>();
    inner_params.push(Argument::from(accumulator));

    let inner_func: Term = Subterm::Func(Func {
        captures: inner_captures,
        params: inner_params,
        body: inner_body,
    })
    .into();

    // Wrapper body: `rec f@sum = inner_func; f@sum(params…, 0)`.
    let seed = params
        .iter()
        .map(|param| name_term(&param.name))
        .chain(iter::once(nat(0)))
        .collect::<Vec<_>>();

    let wrapper_body: Term = Subterm::Rec(Rec {
        names: vec![inner.clone()],
        items: vec![inner_func],
        tail: Subterm::Apply(Apply {
            head: name_term(&inner),
            params: seed,
        })
        .into(),
    })
    .into();

    *term = Subterm::Func(Func {
        captures: wrapper_captures,
        params,
        body: wrapper_body,
    })
    .into();
}

/// A function is a summing recursion when it has at least one combine self-call
/// and *every* self-call sits at a rewritable tail position — a bare tail call or
/// the self side of a `NatAdd(self, _)`. A self-call anywhere else (a non-tail
/// argument, a non-`NatAdd` combine) would be missed by the rewrite, so the whole
/// function is declined.
fn is_summing(name: &str, func: &Func) -> bool {
    let TailTally {
        self_calls,
        combines,
    } = tally_tail(&func.body, name);
    combines >= 1 && count_self_calls(&func.body, name) == self_calls
}

#[derive(Default)]
struct TailTally {
    self_calls: usize,
    combines: usize,
}

impl TailTally {
    fn plus(self, other: TailTally) -> TailTally {
        TailTally {
            self_calls: self.self_calls + other.self_calls,
            combines: self.combines + other.combines,
        }
    }
}

/// Count the self-calls reachable at tail position (each tail call, plus the self
/// operand of each `NatAdd` combine), and how many of those are combines.
fn tally_tail(term: &Term, name: &str) -> TailTally {
    match term.as_subterm() {
        Subterm::NatMatch(NatMatch::Dispatch { cases, default, .. }) => cases
            .values()
            .chain(iter::once(default))
            .fold(TailTally::default(), |acc, case| {
                acc.plus(tally_tail(case, name))
            }),
        Subterm::NatMatch(NatMatch::Induction {
            zero_case,
            succ_case,
            ..
        }) => tally_tail(zero_case, name).plus(tally_tail(succ_case, name)),
        Subterm::Match(Match { cases, .. }) => {
            cases.iter().fold(TailTally::default(), |acc, case| {
                acc.plus(tally_tail(case, name))
            })
        }
        Subterm::Let(let_) => tally_tail(&let_.tail, name),
        Subterm::Apply(apply) if is_named(&apply.head, name) => TailTally {
            self_calls: 1,
            combines: 0,
        },
        Subterm::Prim(Prim::Pure(PurePrim::NatAdd(left, right)))
            if is_self_call(left, name) ^ is_self_call(right, name) =>
        {
            TailTally {
                self_calls: 1,
                combines: 1,
            }
        }
        _ => TailTally::default(),
    }
}

/// Every self-call in the term, tail or not.
fn count_self_calls(term: &Term, name: &str) -> usize {
    let here = matches!(term.as_subterm(), Subterm::Apply(apply) if is_named(&apply.head, name));
    here as usize
        + subterms(term)
            .iter()
            .map(|sub| count_self_calls(sub, name))
            .sum::<usize>()
}

/// Rewrite the term's tail positions to thread the accumulator into the inner
/// recursion. Non-tail sub-terms (a `Let`'s bound value, a call's arguments) are
/// left as is — `is_summing` has already established no self-call hides there.
fn rewrite_tail(term: Term, name: &str, inner: &str, accumulator: &str) -> Term {
    match term.into_subterm() {
        Subterm::NatMatch(NatMatch::Dispatch {
            head,
            cases,
            default,
        }) => Subterm::NatMatch(NatMatch::Dispatch {
            head,
            cases: cases
                .into_iter()
                .map(|(tag, case)| (tag, rewrite_tail(case, name, inner, accumulator)))
                .collect(),
            default: rewrite_tail(default, name, inner, accumulator),
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
            zero_case: rewrite_tail(zero_case, name, inner, accumulator),
            pred,
            ih,
            succ_case: rewrite_tail(succ_case, name, inner, accumulator),
        })
        .into(),
        Subterm::Match(Match { head, cases }) => Subterm::Match(Match {
            head,
            cases: cases
                .into_iter()
                .map(|case| rewrite_tail(case, name, inner, accumulator))
                .collect(),
        })
        .into(),
        Subterm::Let(let_) => Subterm::Let(super::Let {
            name: let_.name,
            body: let_.body,
            tail: rewrite_tail(let_.tail, name, inner, accumulator),
        })
        .into(),
        // A bare tail self-call carries the accumulator forward unchanged.
        Subterm::Apply(apply) if is_named(&apply.head, name) => {
            tail_call(inner, apply.params, name_term(accumulator))
        }
        // A combine moves its addend into the accumulator.
        Subterm::Prim(Prim::Pure(PurePrim::NatAdd(left, right))) => {
            if is_self_call(&left, name) {
                let Subterm::Apply(apply) = left.into_subterm() else {
                    unreachable!("just checked it is a self-call")
                };
                tail_call(inner, apply.params, nat_add(name_term(accumulator), right))
            } else if is_self_call(&right, name) {
                let Subterm::Apply(apply) = right.into_subterm() else {
                    unreachable!("just checked it is a self-call")
                };
                tail_call(inner, apply.params, nat_add(name_term(accumulator), left))
            } else {
                let value = Subterm::Prim(Prim::Pure(PurePrim::NatAdd(left, right))).into();
                nat_add(name_term(accumulator), value)
            }
        }
        // A plain return `v` becomes `acc + v`.
        other => nat_add(name_term(accumulator), other.into()),
    }
}

/// A tail call to the inner recursion: its original arguments, then the
/// accumulator expression.
fn tail_call(inner: &str, mut params: Vec<Term>, accumulator: Term) -> Term {
    params.push(accumulator);
    Subterm::Apply(Apply {
        head: name_term(inner),
        params,
    })
    .into()
}

/// The immediate sub-terms of a term — used only for the all-positions self-call
/// count, so closure captures (names, not terms) are irrelevant.
fn subterms(term: &Term) -> Vec<&Term> {
    match term.as_subterm() {
        Subterm::Erased | Subterm::Unreachable | Subterm::Atom(_) | Subterm::Name(_) => vec![],
        Subterm::Prim(prim) => prim.operands(),
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
            .chain(cases.values())
            .chain(iter::once(default))
            .collect(),
        Subterm::Func(func) => vec![&func.body],
        Subterm::Apply(apply) => iter::once(&apply.head).chain(&apply.params).collect(),
        Subterm::Tuple(tuple) => tuple.fields.iter().collect(),
        Subterm::Proj(proj) => vec![&proj.head],
        Subterm::Match(m) => iter::once(&m.head).chain(&m.cases).collect(),
        Subterm::Let(let_) => vec![&let_.body, &let_.tail],
        Subterm::Rec(rec) => rec.items.iter().chain(iter::once(&rec.tail)).collect(),
    }
}

fn is_named(head: &Term, name: &str) -> bool {
    matches!(head.as_subterm(), Subterm::Name(named) if named.as_str() == name)
}

fn is_self_call(term: &Term, name: &str) -> bool {
    matches!(term.as_subterm(), Subterm::Apply(apply) if is_named(&apply.head, name))
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
