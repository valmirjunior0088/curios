//! Sort-driven erasability classification, shared by every erasure walk: whether a type's values are dropped at runtime, and the signature view of a telescope — one walk ([`signature_entries`]) with a labelled and a mask-only ([`erasure_mask`]) reading. Beside it, the carrier-shape classification of constructor payloads ([`field_shape`], [`constructor_entries`]): erasure is the last walk that still holds the Core field types, so the shape is recorded here and read by `curios-ersd`'s lowering when it decides a family's encoding.

use {
    crate::{Context, Error, is_prop, reduce_with},
    curios_core::{Bound, FuncType, Global, Intrinsic, Subterm, Telescope, Term, TupleType},
    std::collections::BTreeSet,
};

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

/// The constructor-payload view of a telescope: [`signature_entries`] plus the recorded carrier shape of each kept domain. An erased entry's shape is `Opaque`; nothing reads it.
pub(crate) fn constructor_entries<B: Bound>(
    context: &mut Context,
    mut telescope: Telescope<B>,
) -> Result<Vec<(Option<String>, bool, curios_ersd::FieldShape)>, Error> {
    let mut entries = Vec::new();
    loop {
        match telescope {
            Telescope::Cons(type_, rest) => {
                let label = rest.first_hint().map(str::to_string);
                let erasable = is_erasable(context, &type_)?;
                let shape = match erasable {
                    true => curios_ersd::FieldShape::Opaque,
                    false => field_shape(context, &mut BTreeSet::new(), &type_)?,
                };
                let variable = Term::free_var(&context.fresh(label.as_deref()));
                entries.push((label, erasable, shape));
                telescope = rest.open(&[&variable]);
            }
            Telescope::Done(_) => break Ok(entries),
        }
    }
}

/// The erased carrier shape of a kept field's declared type: `Immediate` iff every runtime value of the type lives in the uniform carrier's immediate population — an intrinsic head riding the i31 carrier, or a chain of single-relevant-field collapses (newtype structs, subset tuples) landing on one. Everything else answers `Opaque`, and the asymmetry is the point: a conservative answer only misses an encoding, an aggressive one would corrupt it.
///
/// `visited` makes the chain-chasing terminate: a self-referential struct elaborates (it is merely uninhabited), so the recursion through nominal declarations can cycle, and a cycle classifies `Opaque`. The chain recurses into at most one field per level, so the set is the ancestor chain and never needs unwinding.
pub(crate) fn field_shape(
    context: &mut Context,
    visited: &mut BTreeSet<Global>,
    type_: &Term,
) -> Result<curios_ersd::FieldShape, Error> {
    match Term::unwrap_or_clone(reduce_with(context, type_)?) {
        Subterm::Intrinsic(
            Intrinsic::NatType | Intrinsic::BoolType | Intrinsic::ByteType | Intrinsic::IntType,
        ) => Ok(curios_ersd::FieldShape::Immediate),
        Subterm::StructType(struct_type) => {
            if !visited.insert(struct_type.name.clone()) {
                return Ok(curios_ersd::FieldShape::Opaque);
            }
            let Some(struct_decl) = context.struct_decl(&struct_type.name).cloned() else {
                return Ok(curios_ersd::FieldShape::Opaque);
            };
            single_relevant_shape(context, visited, struct_decl.fields_at(&struct_type.params))
        }
        Subterm::TupleType(TupleType { telescope }) => {
            single_relevant_shape(context, visited, telescope)
        }
        // A single-constructor family is *always* its payload row under the collapsed encoding — one constructor, no discrimination — so the chain continues through it exactly as through a newtype struct. A multi-constructor family mixes shapes and is never chased, whatever its own encoding. Inductives are legitimately recursive, so the same visited guard cuts their cycles.
        Subterm::InductType(induct_type) => {
            if !visited.insert(induct_type.name.clone()) {
                return Ok(curios_ersd::FieldShape::Opaque);
            }
            let Some(induct_decl) = context.induct_decl(&induct_type.name).cloned() else {
                return Ok(curios_ersd::FieldShape::Opaque);
            };
            let tags: Vec<_> = induct_decl.constructor_order().collect();
            let [tag] = tags.as_slice() else {
                return Ok(curios_ersd::FieldShape::Opaque);
            };
            let Some(telescope) = induct_decl.instantiate(tag, &induct_type.params) else {
                return Ok(curios_ersd::FieldShape::Opaque);
            };
            match leading_relevant_domains(context, telescope)? {
                // No payload rides the interned `Nat` zero; one payload is the value itself.
                (None, false) => Ok(curios_ersd::FieldShape::Immediate),
                (Some(domain), false) => field_shape(context, visited, &domain),
                _ => Ok(curios_ersd::FieldShape::Opaque),
            }
        }
        _ => Ok(curios_ersd::FieldShape::Opaque),
    }
}

/// The shape a product-shaped telescope collapses to: exactly one relevant entry recurses into that entry's declared type — the newtype chain — and any other relevant count is `Opaque` (zero relevant is an allocated empty product, two or more a boxed one).
fn single_relevant_shape<B: Bound>(
    context: &mut Context,
    visited: &mut BTreeSet<Global>,
    telescope: Telescope<B>,
) -> Result<curios_ersd::FieldShape, Error> {
    match leading_relevant_domains(context, telescope)? {
        (Some(domain), false) => field_shape(context, visited, &domain),
        _ => Ok(curios_ersd::FieldShape::Opaque),
    }
}

/// The leading relevant domains of a telescope, up to the two any collapse rule asks about: the first relevant domain, and whether a second exists. Domains are classified with the preceding binders opened opaque, the same discipline as [`signature_entries`].
fn leading_relevant_domains<B: Bound>(
    context: &mut Context,
    mut telescope: Telescope<B>,
) -> Result<(Option<Term>, bool), Error> {
    let mut first = None;
    while let Telescope::Cons(type_, rest) = telescope {
        if !is_erasable(context, &type_)? {
            if first.is_some() {
                return Ok((first, true));
            }
            first = Some(type_);
        }
        let variable = Term::free_var(&context.fresh(None));
        telescope = rest.open(&[&variable]);
    }
    Ok((first, false))
}
