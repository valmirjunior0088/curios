//! The argument-side change: thread an integer cursor over a fixed base buffer in
//! place of a re-slicing recursion.
//!
//! The `Bin`/`Arr` eliminators erase to a `Nat` loop on the buffer's length, so a
//! hand-rolled front-to-back walk advances by *re-slicing*: each step recurses on
//! `slice(b, k, len b)`, the tail after the head. That slice copies the remaining
//! elements, turning an `O(n)` walk into `O(n²)`. But the suffix never needs to be
//! materialised. A recursion whose buffer parameter `b` is recursed only as a
//! drop-front suffix `slice(b, k, len b)` and otherwise read only through `len b`,
//! `get b i`, and `slice b p q` (the [`suffix_view`](curios_base::suffix_view)
//! laws) can thread an integer `offset` over the original buffer instead:
//!
//! ```text
//!   len b              ↦  len base − offset
//!   get b i            ↦  get base (offset + i)
//!   slice b k (len b)  ↦  recurse with offset := offset + k     (no copy)
//! ```
//!
//! so every step is `O(1)` and the whole walk `O(n)`. The escape gate is
//! whole-function and all-or-nothing ([`legible`]): a single bare use of `b` outside
//! those positions — passed to a function, compared, returned — would force the
//! virtual suffix to be materialised, so the change declines and the copy stays.

use {
    super::{
        Change, Lower, ThreadedParam, is_named, name_term, nat, nat_add, nat_sub, subterms,
        subterms_mut,
    },
    crate::{Apply, Argument, Func, Prim, PurePrim, Subterm, Term, optm::CallGraph},
    curios_base::suffix_view::Carrier,
    std::mem,
};

/// The argument-side worker/wrapper change. See the module documentation.
pub(super) struct SliceCursor;

/// A recognised drop-front recursion: which parameter is the buffer cursor, its
/// carrier, the base name held fixed, and the offset name threaded over it.
pub(super) struct CursorPlan {
    index: usize,
    carrier: Carrier,
    base: String,
    offset: String,
}

impl Change for SliceCursor {
    type Plan = CursorPlan;

    fn recognize(name: &str, func: &Func, _cg: &CallGraph) -> Option<CursorPlan> {
        let (index, carrier) = drop_front_param(name, func)?;
        let base = func.params[index].name.clone();
        let offset = format!("{base}@offset");
        Some(CursorPlan {
            index,
            carrier,
            base,
            offset,
        })
    }

    fn threaded(plan: &CursorPlan) -> Vec<ThreadedParam> {
        vec![ThreadedParam {
            param: Argument::from(plan.offset.as_str()),
            seed: nat(0),
        }]
    }

    fn lower(plan: &CursorPlan, ctx: &Lower, body: &mut Term) {
        rewrite(
            body,
            ctx,
            &plan.base,
            &plan.offset,
            plan.index,
            plan.carrier,
        );
    }
}

/// Find a parameter that is a drop-front buffer cursor, if any. A parameter `b`
/// (index `k`) qualifies when:
///
/// - the function recurses (`>= 1` self-call), every self-call has the original
///   arity, and `b`'s slot in each is either `b` unchanged or a drop-front suffix
///   `slice(b, _, len b)` — with at least one slice, so the recursion progresses;
/// - `b` flows nowhere but the first operand of `len`/`get`/`slice` and those slots,
///   and the self-reference `name` appears only as a call head ([`legible`]).
///
/// The first qualifying parameter wins.
fn drop_front_param(name: &str, func: &Func) -> Option<(usize, Carrier)> {
    let mut self_calls = Vec::new();
    collect_self_calls(&func.body, name, &mut self_calls);

    if self_calls.is_empty()
        || self_calls
            .iter()
            .any(|call| call.params.len() != func.params.len())
    {
        return None;
    }

    (0..func.params.len()).find_map(|index| {
        let base = &func.params[index].name;
        let slots = self_calls
            .iter()
            .map(|call| &call.params[index])
            .collect::<Vec<_>>();

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
            _ => prim
                .operands()
                .iter()
                .all(|operand| legible(operand, name, base, index, carrier)),
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
/// self-call into an offset-threading tail call to the worker. Runs in place over
/// the body; [`drop_front_param`] has established it is legible.
fn rewrite(term: &mut Term, ctx: &Lower, base: &str, offset: &str, index: usize, carrier: Carrier) {
    // `len base` becomes `len base − offset`. Done first, by replacing the whole
    // term, so the new `len base` is not itself rewritten.
    if is_base_len(term.as_subterm(), base, carrier) {
        let length = mem::replace(term, nat(0));
        *term = nat_sub(length, name_term(offset));
        return;
    }

    match term.as_subterm_mut() {
        Subterm::Apply(apply) if ctx.is_self_call(&apply.head) => {
            rewrite_self_call(apply, ctx, base, offset, index, carrier);
        }
        Subterm::Prim(Prim::Pure(prim)) => match (carrier, &mut *prim) {
            // `get base i` becomes `get base (offset + i)`.
            (Carrier::Bin, PurePrim::BinGet(buffer, place))
            | (Carrier::Arr, PurePrim::ArrGet(buffer, place))
                if is_named(buffer, base) =>
            {
                rewrite(place, ctx, base, offset, index, carrier);
                shift(place, offset);
            }
            // `slice base p q` becomes `slice base (offset + p) (offset + q)`.
            (Carrier::Bin, PurePrim::BinSlice(buffer, from, upto))
            | (Carrier::Arr, PurePrim::ArrSlice(buffer, from, upto))
                if is_named(buffer, base) =>
            {
                rewrite(from, ctx, base, offset, index, carrier);
                rewrite(upto, ctx, base, offset, index, carrier);
                shift(from, offset);
                shift(upto, offset);
            }
            _ => {
                for operand in prim.operands_mut() {
                    rewrite(operand, ctx, base, offset, index, carrier);
                }
            }
        },
        Subterm::Prim(prim) => {
            for operand in prim.operands_mut() {
                rewrite(operand, ctx, base, offset, index, carrier);
            }
        }
        _ => {
            for child in subterms_mut(term) {
                rewrite(child, ctx, base, offset, index, carrier);
            }
        }
    }
}

/// Rewrite a self-call into a tail call to the worker: redirect the head, hold the
/// base buffer fixed in the cursor slot, advance the threaded offset, and rewrite
/// the remaining arguments.
fn rewrite_self_call(
    apply: &mut Apply,
    ctx: &Lower,
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
            rewrite(&mut start, ctx, base, offset, index, carrier);
            nat_add(name_term(offset), start)
        }
    };

    for (position, param) in apply.params.iter_mut().enumerate() {
        if position != index {
            rewrite(param, ctx, base, offset, index, carrier);
        }
    }

    apply.head = name_term(ctx.worker);
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
        (Carrier::Bin, PurePrim::BinSlice(_, from, _))
        | (Carrier::Arr, PurePrim::ArrSlice(_, from, _)) => from,
        _ => unreachable!("a drop-front slot is a carrier slice"),
    }
}

/// Collect every self-call (an `Apply` whose head names `name`) reachable in
/// `term`, descending into the calls' own arguments to catch nested ones.
fn collect_self_calls<'a>(term: &'a Term, name: &str, out: &mut Vec<&'a Apply>) {
    if let Subterm::Apply(apply) = term.as_subterm()
        && is_named(&apply.head, name)
    {
        out.push(apply);
    }

    for child in subterms(term) {
        collect_self_calls(child, name, out);
    }
}
