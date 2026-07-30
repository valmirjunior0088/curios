//! Sort-driven erasability classification, shared by every erasure walk: whether a type's values are dropped at runtime, and the signature view of a telescope — one walk ([`signature_entries`]) with a labelled and a mask-only ([`erasure_mask`]) reading.

use crate::{Context, Error, is_prop, reduce_with};
use curios_core::{Bound, FuncType, Subterm, Telescope, Term};

/// Whether a value of type `type_` is dropped at runtime. Erasure is sort-driven: a value erases iff it is a *type/prop-as-value* (`type_` reduces to the universe `Type` or `Prop`), a *genuine proposition* — a `Prop`-sorted nominal/neutral type (`Eq`, `False`, `Le`, `Utf8`, a stuck `Nat/Lt` match) whose inhabitants are pure proof-irrelevant witnesses — or a *function into* such a thing (a proof-/type-producing function is itself pure content-free).
///
/// [`super::Sort::of`] classifies the empty tuple `{}` as `Type`, not a prop — `{}` is the result type of effects (`print`'s `let _ = write(..); ()`) and must be kept — so `{}`, `{ .., {} }`, and `X -> {}` are not erased. A `FuncType` erases only when its ultimate codomain does, recursing past the parameters into the return type, which lands on `{}` (kept) or on a genuine proposition / universe (erased). Every std `@`-marker was either such a proposition/type or a function returning one, so this stays output-equivalent.
///
/// CRITICAL: evaluate against the binder's *declared* (signature) type, opened only with the surrounding binders as opaque variables — never with concrete call arguments. A polymorphic field `value : A` is kept (its abstract `A` is neither prop nor type); re-classifying it at a call where `A := SomeProp` would diverge the construction's arity from the constructor function's fixed arity. [`signature_entries`] enforces the opaque-open discipline.
pub(crate) fn is_erasable(context: &mut Context, type_: &Term) -> Result<bool, Error> {
    match Term::unwrap_or_clone(reduce_with(context, type_)?) {
        Subterm::Type(_) | Subterm::Prop => Ok(true),
        Subterm::UniverseInst(instance) => is_erasable(context, &instance.head),
        // A function erases iff what it ultimately returns does — a proof-/type-producing function is pure, content-free; an effectful `X -> {}` is not. Recurse past the parameters (opened opaquely) into the codomain.
        Subterm::FuncType(FuncType { telescope, .. }) => {
            let vars: Vec<Term> = (0..telescope.len())
                .map(|_| Term::free_var(&context.fresh(None)))
                .collect();
            let refs: Vec<&Term> = vars.iter().collect();
            is_erasable(context, &telescope.open(&refs))
        }
        _ => is_prop(context, type_),
    }
}

/// The signature view of a telescope: one entry per binder — its label and whether it is erased — classifying each domain with the *preceding* binders opened as fresh opaque variables (see [`is_erasable`]). That opaque-open discipline is what keeps a function's runtime arity fixed across every instantiation, so this is the only walk that computes it; the terminal body is ignored. Pairs with a concrete walk over the actual values: the signature decides which to drop, the concrete walk erases the kept ones against their (dependent, instantiated) types.
pub(crate) fn signature_entries<B: Bound>(
    context: &mut Context,
    mut telescope: Telescope<B>,
) -> Result<Vec<(Option<String>, bool)>, Error> {
    let mut entries = Vec::new();
    loop {
        match telescope {
            Telescope::Cons(type_, rest) => {
                let label = rest.first_hint().map(str::to_string);
                let erasable = is_erasable(context, &type_)?;
                let variable = Term::free_var(&context.fresh(label.as_deref()));
                entries.push((label, erasable));
                telescope = rest.open(&[&variable]);
            }
            Telescope::Done(_) => break Ok(entries),
        }
    }
}

/// The label-free reading of [`signature_entries`]: the per-binder erasability mask, for the sites that decide drops without naming the slots.
pub(crate) fn erasure_mask<B: Bound>(
    context: &mut Context,
    telescope: Telescope<B>,
) -> Result<Vec<bool>, Error> {
    Ok(signature_entries(context, telescope)?
        .into_iter()
        .map(|(_, erased)| erased)
        .collect())
}
