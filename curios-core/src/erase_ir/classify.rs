//! Sort-driven erasability classification, shared by every erasure walk:
//! whether a type's values are dropped at runtime, the per-binder mask of a
//! telescope, and the motive pattern's binder slots.

use crate::{
    Bound, Context, Error, FuncType, MotivePattern, MotiveSlot, Subterm, Telescope, Term, is_prop,
    reduce_with,
};

/// Whether a value of type `type_` is dropped at runtime. Erasure is sort-driven:
/// a value erases iff it is a *type/prop-as-value* (`type_` reduces to the
/// universe `Type` or `Prop`), a *genuine proposition* — a `Prop`-sorted
/// nominal/neutral type (`Eq`, `False`, `Le`, `Utf8`, a stuck `Nat/Lt` match)
/// whose inhabitants are pure proof-irrelevant witnesses — or a *function into*
/// such a thing (a proof-/type-producing function is itself pure content-free).
///
/// [`super::Sort::of`] classifies the empty tuple `{}` as `Type`, not a prop —
/// `{}` is the result type of effects (`print`'s `let _ = write(..); ()`) and
/// must be kept — so `{}`, `{ .., {} }`, and `X -> {}` are not erased. A
/// `FuncType` erases only when its ultimate codomain does, recursing past the
/// parameters into the return type, which lands on `{}` (kept) or on a genuine
/// proposition / universe (erased).
/// Every std `@`-marker was either such a proposition/type or a function
/// returning one, so this stays output-equivalent.
///
/// CRITICAL: evaluate against the binder's *declared* (signature) type, opened
/// only with the surrounding binders as opaque variables — never with concrete
/// call arguments. A polymorphic field `value : A` is kept (its abstract `A` is
/// neither prop nor type); re-classifying it at a call where `A := SomeProp`
/// would diverge the construction's arity from the constructor function's fixed
/// arity. [`erasure_mask`] enforces the opaque-open discipline.
pub(crate) fn is_erasable(context: &mut Context, type_: &Term) -> Result<bool, Error> {
    match Term::unwrap_or_clone(reduce_with(context, type_)?) {
        Subterm::Type | Subterm::Prop => Ok(true),
        // A function erases iff what it ultimately returns does — a proof-/type-
        // producing function is pure, content-free; an effectful `X -> {}` is not.
        // Recurse past the parameters (opened opaquely) into the codomain.
        Subterm::FuncType(FuncType { telescope, .. }) => {
            let vars: Vec<Term> = (0..telescope.len())
                .map(|_| Term::free_var(context.fresh(None)))
                .collect();
            let refs: Vec<&Term> = vars.iter().collect();
            is_erasable(context, &telescope.open(&refs))
        }
        _ => is_prop(context, type_),
    }
}

/// The per-binder erasability mask of a telescope, classifying each domain with
/// the *preceding* binders opened as fresh opaque variables — the signature-only
/// view that keeps a function's runtime arity fixed across every instantiation
/// (see [`is_erasable`]). The terminal body is ignored. Pairs with a concrete
/// walk over the actual values: the mask decides which to drop, the concrete
/// walk erases the kept ones against their (dependent, instantiated) types.
pub(crate) fn erasure_mask<B: Bound>(
    context: &mut Context,
    mut telescope: Telescope<B>,
) -> Result<Vec<bool>, Error> {
    let mut mask = Vec::new();
    loop {
        match telescope {
            Telescope::Cons(ty, rest) => {
                mask.push(is_erasable(context, &ty)?);
                let x = Term::free_var(context.fresh(rest.first_label()));
                telescope = rest.open(&[&x]);
            }
            Telescope::Done(_) => break Ok(mask),
        }
    }
}

/// The motive pattern's binder slots, positionally (validated by elaborate):
/// `true` marks a parameter position (opened with the actual parameter),
/// `false` an index position (opened with the case's target index). `Term`
/// slots carry no binder and are dropped.
pub(crate) fn pattern_binder_slots(
    pattern: Option<&MotivePattern>,
    n_params: usize,
) -> Vec<(bool, usize)> {
    pattern
        .map(|p| {
            p.slots
                .iter()
                .enumerate()
                .filter_map(|(position, slot)| match slot {
                    MotiveSlot::Binder if position < n_params => Some((true, position)),
                    MotiveSlot::Binder => Some((false, position - n_params)),
                    MotiveSlot::Term(_) => None,
                })
                .collect()
        })
        .unwrap_or_default()
}
