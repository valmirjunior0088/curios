use {
    super::{Apply, Argument, Func, Item, Module, Name, NatMatch, Prim, PurePrim, Rec, Subterm, Term},
    std::{
        collections::{HashMap, HashSet},
        iter, mem,
    },
};

/// Drop-front cursor introduction for `Bin`/`Arr`-consuming self-recursion, on the
/// erased IR where the buffer's shape is still legible.
///
/// The `Bin`/`Arr` eliminators erase to a `Nat` loop on the buffer's length, and a
/// hand-rolled recursion that walks the buffer front-to-back — `/std/Str/count_w`
/// (`Str/len`) is the canonical one — therefore advances by *re-slicing*: each step
/// recurses on `Bin.slice b 1 (Bin.len b)`, the tail after the head byte. That
/// slice copies the remaining bytes, so an `O(n)` walk runs in `O(n²)` time.
///
/// But the suffix never needs to be materialised. A recursion whose buffer
/// parameter `b` is
///
/// - recursed only as a *drop-front suffix* `slice(b, k, len b)` (`end = len b`), and
/// - otherwise read only through `len b`, `get b i`, and `slice b p q`
///
/// can thread an integer `offset` cursor over the *original* buffer instead. With
/// `b` held fixed as the base and `offset` the front position,
///
/// ```text
///   len b    ↦  len base − offset
///   get b i  ↦  get base (offset + i)
///   slice b k (len b)  ↦  recurse with offset := offset + k     (no copy)
/// ```
///
/// so every step is `O(1)` and the whole walk is `O(n)`. This rewrites
///
/// ```text
///   rec f = (b) => … f(slice b 1 (len b)) …
/// ```
///
/// into a thin wrapper around an inner cursor recursion:
///
/// ```text
///   rec f = (b) => (rec f@cur = (b, offset) => … f@cur(b, offset + 1) …)(b, 0)
/// ```
///
/// `f`'s signature is untouched (the offset is internal), so no caller changes. The
/// pass runs after [`introduce_accumulators`](super::introduce_accumulators), so on
/// `count_w` it threads the cursor through the already-tail-recursive `count_w@sum`
/// inner loop — yielding a loop that is `O(1)` in *both* stack and per-step time.
/// It descends into nested `rec`s to reach exactly that accumulator-introduced inner
/// recursion.
pub fn introduce_offsets(module: &mut Module) {
    for item in &mut module.items {
        match item {
            Item::Let { body, .. } => descend(body),
            // A single-binding `rec` is itself a transform candidate (a drop-front
            // recursion the accumulator pass left alone); its body may also house a
            // nested one. Transform bottom-up: descend first, then this node.
            Item::Rec { names, items } if names.len() == 1 && items.len() == 1 => {
                descend(&mut items[0]);
                transform(&names[0], &mut items[0]);
            }
            Item::Rec { items, .. } => items.iter_mut().for_each(descend),
        }
    }
}

/// Walk the term bottom-up, transforming every single-binding `rec` whose body is a
/// drop-front recursion. `Prim` operands are not descended into — a `rec` never sits
/// inside a primitive's operand, so skipping them is a safe under-approximation.
fn descend(term: &mut Term) {
    for child in subterms_mut(term) {
        descend(child);
    }

    if let Subterm::Rec(rec) = term.as_subterm_mut() {
        if rec.names.len() == 1 && rec.items.len() == 1 {
            let name = rec.names[0].clone();
            transform(&name, &mut rec.items[0]);
        }
    }
}

/// The buffer carrier a cursor threads over: `Bin` or `Arr`. Selects which
/// `len`/`get`/`slice` primitives are recognised and emitted.
#[derive(Clone, Copy, PartialEq)]
enum Carrier {
    Bin,
    Arr,
}

/// Rewrite a single `rec` binding's body in place: if some parameter is a
/// drop-front buffer cursor, replace the function with a wrapper that seeds an
/// `offset` of `0` and delegates to an inner cursor recursion. A no-op when no
/// parameter qualifies.
fn transform(name: &str, item: &mut Term) {
    let Subterm::Func(func) = item.as_subterm() else {
        return;
    };
    let Some((index, carrier)) = drop_front_param(name, func) else {
        return;
    };

    let Subterm::Func(Func {
        captures,
        params,
        body,
    }) = mem::replace(item, Subterm::Erased.into()).into_subterm()
    else {
        unreachable!("just matched a Func")
    };

    let cursor = format!("{name}@cur");
    let base = params[index].name.clone();
    let offset = format!("{base}@offset");

    // Rewrite buffer reads onto base+offset and self-calls into offset-threading
    // tail calls to the cursor.
    let mut cursor_body = body;
    rewrite(&mut cursor_body, name, &cursor, &base, &offset, index, carrier);

    // Inner cursor recursion: (params…, offset) => cursor_body.
    let mut cursor_params = clone_args(&params);
    cursor_params.push(Argument::from(offset.as_str()));

    let cursor_func: Term = Subterm::Func(Func {
        captures: clone_args(&captures),
        params: cursor_params,
        body: cursor_body,
    })
    .into();

    // Wrapper body: `rec f@cur = cursor_func; f@cur(params…, 0)`.
    let seed = params
        .iter()
        .map(|param| name_term(&param.name))
        .chain(iter::once(nat(0)))
        .collect::<Vec<_>>();

    let wrapper_body: Term = Subterm::Rec(Rec {
        names: vec![cursor.clone()],
        items: vec![cursor_func],
        tail: Subterm::Apply(Apply {
            head: name_term(&cursor),
            params: seed,
        })
        .into(),
    })
    .into();

    *item = Subterm::Func(Func {
        captures: clone_args(&captures),
        params,
        body: wrapper_body,
    })
    .into();

    // The rewrite changed which names are free inside the cursor body — `name`
    // became `cursor`, and `offset` is now read — but the closures it ran through
    // (a motive-arm thunk like `Str/at`'s) carry their own stale capture lists.
    // Recompute every closure's captures bottom-up so each lists exactly its body's
    // free variables; the wrapper recomputes too, dropping its now-unused self-ref.
    refresh_captures(item, &cursor, &offset);
}

/// A by-value copy of an argument list (`Argument` is not `Clone`).
fn clone_args(args: &[Argument]) -> Vec<Argument> {
    args.iter()
        .map(|arg| Argument {
            name: arg.name.clone(),
            candidate: arg.candidate,
        })
        .collect()
}

/// Recompute the captures of every `Func` in `term`, bottom-up: a closure's
/// captures are exactly its body's free names minus its parameters. Candidate flags
/// are preserved from the closure's prior captures; the two names the rewrite can
/// newly introduce inherit theirs (`cursor` from the recursion it replaced — a
/// function, hence a specialization candidate — and `offset`, a `Nat`, which is
/// not). Bottom-up order matters: an outer body's free names depend on its inner
/// closures' freshly-fixed captures.
fn refresh_captures(term: &mut Term, cursor: &str, offset: &str) {
    for child in subterms_mut(term) {
        refresh_captures(child, cursor, offset);
    }

    if let Subterm::Func(func) = term.as_subterm_mut() {
        let mut candidate = func
            .captures
            .iter()
            .map(|capture| (capture.name.clone(), capture.candidate))
            .collect::<HashMap<String, bool>>();
        candidate.insert(cursor.to_owned(), true);
        candidate.insert(offset.to_owned(), false);

        let params = func.params.iter().map(|param| param.name.as_str()).collect::<HashSet<&str>>();

        func.captures = func
            .body
            .free_names()
            .into_iter()
            .filter(|name| !params.contains(name.as_str()))
            .map(|name| Argument {
                candidate: candidate.get(&name).copied().unwrap_or(false),
                name,
            })
            .collect();
    }
}

/// Find a parameter that is a drop-front buffer cursor, if any. A parameter `b`
/// (index `k`) qualifies when:
///
/// - the function recurses (`>= 1` self-call), every self-call has the original
///   arity, and `b`'s slot in each is either `b` unchanged or a drop-front suffix
///   `slice(b, _, len b)` — with at least one slice, so the recursion makes progress;
/// - `b` flows nowhere but the first operand of `len`/`get`/`slice` and those slots,
///   and the self-reference `name` appears only as a call head ([`legible`]).
///
/// All self-calls agree on the carrier (they slice the one parameter). The first
/// qualifying parameter wins.
fn drop_front_param(name: &str, func: &Func) -> Option<(usize, Carrier)> {
    let mut self_calls = Vec::new();
    collect_self_calls(&func.body, name, &mut self_calls);

    if self_calls.is_empty() || self_calls.iter().any(|call| call.params.len() != func.params.len())
    {
        return None;
    }

    (0..func.params.len()).find_map(|index| {
        let base = &func.params[index].name;
        let slots = self_calls.iter().map(|call| &call.params[index]).collect::<Vec<_>>();

        let carrier = slots.iter().find_map(|slot| slice_carrier(slot))?;

        let drops_front = slots
            .iter()
            .all(|slot| is_named(slot, base) || is_drop_front(slot, base, carrier));

        (drops_front && legible(&func.body, name, base, index, carrier)).then_some((index, carrier))
    })
}

/// Whether `b` and the self-reference `name` appear only in cursor-threadable
/// positions throughout `term` (see [`drop_front_param`]). A bare `b` or `name`
/// anywhere else — passed to a function, compared, returned — fails: the virtual
/// suffix would have to be materialised, defeating the rewrite.
fn legible(term: &Term, name: &str, base: &str, index: usize, carrier: Carrier) -> bool {
    match term.as_subterm() {
        // `b` under its own buffer reads: the base operand is fine, the index and
        // bounds must themselves be legible.
        subterm if is_base_len(subterm, base, carrier) => true,
        Subterm::Prim(Prim::Pure(prim)) => match (carrier, prim) {
            (Carrier::Bin, PurePrim::BinGet(buffer, place))
            | (Carrier::Arr, PurePrim::ArrGet(buffer, place))
                if is_named(buffer, base) =>
            {
                legible(place, name, base, index, carrier)
            }
            (Carrier::Bin, PurePrim::BinSlice(buffer, from, upto))
            | (Carrier::Arr, PurePrim::ArrSlice(buffer, from, upto))
                if is_named(buffer, base) =>
            {
                legible(from, name, base, index, carrier)
                    && legible(upto, name, base, index, carrier)
            }
            _ => prim.operands().iter().all(|operand| legible(operand, name, base, index, carrier)),
        },
        // A self-call: every non-cursor argument must be legible, and the cursor
        // slot must be `b` or a drop-front suffix (whose start is itself legible).
        Subterm::Apply(apply) if is_named(&apply.head, name) => {
            apply.params.iter().enumerate().all(|(position, param)| {
                if position == index {
                    is_named(param, base)
                        || (is_drop_front(param, base, carrier)
                            && legible(slice_start(param), name, base, index, carrier))
                } else {
                    legible(param, name, base, index, carrier)
                }
            })
        }
        // A bare `b` or self-reference outside the positions above is illegible.
        Subterm::Name(named) if named.as_str() == base || named.as_str() == name => false,
        _ => subterms(term)
            .iter()
            .all(|child| legible(child, name, base, index, carrier)),
    }
}

/// Rewrite every buffer read of `base` onto the `base + offset` cursor and every
/// self-call into an offset-threading tail call to `cursor`. Runs in place over the
/// cursor function's body; [`drop_front_param`] has established it is legible.
fn rewrite(
    term: &mut Term,
    name: &str,
    cursor: &str,
    base: &str,
    offset: &str,
    index: usize,
    carrier: Carrier,
) {
    // `len base` becomes `len base − offset`. Done first, by replacing the whole
    // term, so the new `len base` is not itself rewritten.
    if is_base_len(term.as_subterm(), base, carrier) {
        let length = mem::replace(term, nat(0));
        *term = nat_sub(length, name_term(offset));
        return;
    }

    match term.as_subterm_mut() {
        Subterm::Apply(apply) if is_named(&apply.head, name) => {
            rewrite_self_call(apply, name, cursor, base, offset, index, carrier);
        }
        Subterm::Prim(Prim::Pure(prim)) => match (carrier, &mut *prim) {
            // `get base i` becomes `get base (offset + i)`.
            (Carrier::Bin, PurePrim::BinGet(buffer, place))
            | (Carrier::Arr, PurePrim::ArrGet(buffer, place))
                if is_named(buffer, base) =>
            {
                rewrite(place, name, cursor, base, offset, index, carrier);
                shift(place, offset);
            }
            // `slice base p q` becomes `slice base (offset + p) (offset + q)`.
            (Carrier::Bin, PurePrim::BinSlice(buffer, from, upto))
            | (Carrier::Arr, PurePrim::ArrSlice(buffer, from, upto))
                if is_named(buffer, base) =>
            {
                rewrite(from, name, cursor, base, offset, index, carrier);
                rewrite(upto, name, cursor, base, offset, index, carrier);
                shift(from, offset);
                shift(upto, offset);
            }
            _ => {
                for operand in prim.operands_mut() {
                    rewrite(operand, name, cursor, base, offset, index, carrier);
                }
            }
        },
        Subterm::Prim(prim) => {
            for operand in prim.operands_mut() {
                rewrite(operand, name, cursor, base, offset, index, carrier);
            }
        }
        _ => {
            for child in subterms_mut(term) {
                rewrite(child, name, cursor, base, offset, index, carrier);
            }
        }
    }
}

/// Rewrite a self-call into a tail call to the cursor: redirect the head, hold the
/// base buffer fixed in the cursor slot, advance the threaded offset, and rewrite
/// the remaining arguments.
fn rewrite_self_call(
    apply: &mut Apply,
    name: &str,
    cursor: &str,
    base: &str,
    offset: &str,
    index: usize,
    carrier: Carrier,
) {
    // The advanced offset, taken from the cursor slot before it is reset to `base`.
    let advanced = {
        let slot = &mut apply.params[index];
        if is_named(slot, base) {
            // `b` passed unchanged — the offset does not move.
            name_term(offset)
        } else {
            let slice = mem::replace(slot, name_term(base));
            let mut start = into_slice_start(slice, carrier);
            rewrite(&mut start, name, cursor, base, offset, index, carrier);
            nat_add(name_term(offset), start)
        }
    };

    for (position, param) in apply.params.iter_mut().enumerate() {
        if position != index {
            rewrite(param, name, cursor, base, offset, index, carrier);
        }
    }

    apply.head = name_term(cursor);
    apply.params.push(advanced);
}

/// `*place := offset + *place`, in place.
fn shift(place: &mut Term, offset: &str) {
    let inner = mem::replace(place, nat(0));
    *place = nat_add(name_term(offset), inner);
}

/// Whether `subterm` is `len base` for the carrier — the suffix end-marker and the
/// one buffer read with no further operands.
fn is_base_len(subterm: &Subterm, base: &str, carrier: Carrier) -> bool {
    match (carrier, subterm) {
        (Carrier::Bin, Subterm::Prim(Prim::Pure(PurePrim::BinLen(buffer))))
        | (Carrier::Arr, Subterm::Prim(Prim::Pure(PurePrim::ArrLen(buffer)))) => {
            is_named(buffer, base)
        }
        _ => false,
    }
}

/// The carrier of a `slice` term, or `None` if it is not a slice.
fn slice_carrier(term: &Term) -> Option<Carrier> {
    match term.as_subterm() {
        Subterm::Prim(Prim::Pure(PurePrim::BinSlice(..))) => Some(Carrier::Bin),
        Subterm::Prim(Prim::Pure(PurePrim::ArrSlice(..))) => Some(Carrier::Arr),
        _ => None,
    }
}

/// Whether `term` is a drop-front suffix `slice(base, _, len base)`.
fn is_drop_front(term: &Term, base: &str, carrier: Carrier) -> bool {
    match (carrier, term.as_subterm()) {
        (Carrier::Bin, Subterm::Prim(Prim::Pure(PurePrim::BinSlice(buffer, _, upto))))
        | (Carrier::Arr, Subterm::Prim(Prim::Pure(PurePrim::ArrSlice(buffer, _, upto)))) => {
            is_named(buffer, base) && is_base_len(upto.as_subterm(), base, carrier)
        }
        _ => false,
    }
}

/// The start operand of a slice term (borrowed).
fn slice_start(term: &Term) -> &Term {
    match term.as_subterm() {
        Subterm::Prim(Prim::Pure(PurePrim::BinSlice(_, from, _)))
        | Subterm::Prim(Prim::Pure(PurePrim::ArrSlice(_, from, _))) => from,
        _ => unreachable!("a drop-front slot is a slice"),
    }
}

/// The start operand of a slice term (owned).
fn into_slice_start(term: Term, carrier: Carrier) -> Term {
    let Subterm::Prim(Prim::Pure(slice)) = term.into_subterm() else {
        unreachable!("a drop-front slot is a slice")
    };
    match (carrier, slice) {
        (Carrier::Bin, PurePrim::BinSlice(_, from, _)) | (Carrier::Arr, PurePrim::ArrSlice(_, from, _)) => {
            from
        }
        _ => unreachable!("a drop-front slot is a carrier slice"),
    }
}

/// Collect every self-call (an `Apply` whose head names `name`) reachable in
/// `term`, descending into the calls' own arguments to catch nested ones.
fn collect_self_calls<'a>(term: &'a Term, name: &str, out: &mut Vec<&'a Apply>) {
    if let Subterm::Apply(apply) = term.as_subterm() {
        if is_named(&apply.head, name) {
            out.push(apply);
        }
    }

    for child in subterms(term) {
        collect_self_calls(child, name, out);
    }
}

/// The immediate sub-terms of a term, including primitive operands — the read-only
/// mirror of [`subterms_mut`], used by the legibility walk.
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

/// The immediate sub-terms of a term, mutably — the structural children only, *not*
/// primitive operands (a `rec` never nests inside one, so the cursor walk skips
/// them).
fn subterms_mut(term: &mut Term) -> Vec<&mut Term> {
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
        Subterm::Apply(apply) => iter::once(&mut apply.head).chain(&mut apply.params).collect(),
        Subterm::Tuple(tuple) => tuple.fields.iter_mut().collect(),
        Subterm::Proj(proj) => vec![&mut proj.head],
        Subterm::Match(m) => iter::once(&mut m.head).chain(&mut m.cases).collect(),
        Subterm::Let(let_) => vec![&mut let_.body, &mut let_.tail],
        Subterm::Rec(rec) => rec.items.iter_mut().chain(iter::once(&mut rec.tail)).collect(),
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

#[cfg(test)]
mod tests {
    use super::*;

    fn bin_len(buffer: Term) -> Term {
        Subterm::Prim(Prim::Pure(PurePrim::BinLen(buffer))).into()
    }

    fn bin_slice(buffer: Term, from: Term, upto: Term) -> Term {
        Subterm::Prim(Prim::Pure(PurePrim::BinSlice(buffer, from, upto))).into()
    }

    fn bin_eql(left: Term, right: Term) -> Term {
        Subterm::Prim(Prim::Pure(PurePrim::BinEql(left, right))).into()
    }

    fn apply(head: &str, params: Vec<Term>) -> Term {
        Subterm::Apply(Apply {
            head: name_term(head),
            params,
        })
        .into()
    }

    /// A walk over a buffer parameter: `match len(b) | 0 => 0 | _ => <step>` where
    /// `<step>` is the (default) successor body. Modelled on the single-peel shape
    /// `count_w` erases to.
    fn walk(step: Term) -> Term {
        Subterm::NatMatch(NatMatch::Dispatch {
            head: bin_len(name_term("b")),
            cases: iter::once((0, nat(0))).collect(),
            default: step,
        })
        .into()
    }

    fn rec_module(body: Term) -> Module {
        Module {
            items: vec![Item::Rec {
                names: vec!["f".to_owned()],
                items: vec![Subterm::Func(Func {
                    captures: vec![Argument::from("f")],
                    params: vec![Argument::from("b")],
                    body,
                })
                .into()],
            }],
            body: Subterm::Erased.into(),
        }
    }

    #[test]
    fn drop_front_recursion_threads_a_cursor() {
        // `rec f(b) = match len b | 0 => 0 | _ => f(slice b 1 (len b))`: the suffix
        // recursion is recognised and rewritten to a cursor loop. The one slice — the
        // drop-front — is gone, replaced by an advanced `offset`.
        let recursion = walk(apply(
            "f",
            vec![bin_slice(name_term("b"), nat(1), bin_len(name_term("b")))],
        ));
        let mut module = rec_module(recursion);

        introduce_offsets(&mut module);

        let printed = format!("{module}");
        assert!(printed.contains("f@cur"), "expected a cursor loop:\n{printed}");
        assert!(printed.contains("b@offset"), "expected a threaded offset:\n{printed}");
        // The recursion's only `slice` was the drop-front; the cursor eliminates it.
        assert!(!printed.contains("Bin.slice"), "the drop-front slice should be gone:\n{printed}");
    }

    #[test]
    fn buffer_escaping_to_another_primitive_is_declined() {
        // `b` flows into `Bin.eql` (a whole-buffer read), so the virtual suffix would
        // have to be materialised — the recursion is left untouched.
        let recursion = walk(Subterm::Match(super::super::Match {
            head: bin_eql(name_term("b"), name_term("b")),
            cases: vec![
                apply("f", vec![bin_slice(name_term("b"), nat(1), bin_len(name_term("b")))]),
                nat(0),
            ],
        })
        .into());
        let mut module = rec_module(recursion);

        introduce_offsets(&mut module);

        let printed = format!("{module}");
        assert!(!printed.contains("f@cur"), "the escaping buffer must not be threaded:\n{printed}");
    }

    #[test]
    fn non_suffix_recursion_is_declined() {
        // The recursion drops the *back*, not the front: `slice b 0 (len b − 1)` is
        // not a suffix (its end is not `len b`), so no cursor applies.
        let recursion = walk(apply(
            "f",
            vec![bin_slice(
                name_term("b"),
                nat(0),
                nat_sub(bin_len(name_term("b")), nat(1)),
            )],
        ));
        let mut module = rec_module(recursion);

        introduce_offsets(&mut module);

        let printed = format!("{module}");
        assert!(!printed.contains("f@cur"), "a non-suffix recursion must be declined:\n{printed}");
    }
}
