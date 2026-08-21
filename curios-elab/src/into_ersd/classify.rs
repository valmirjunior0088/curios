//! Sort-driven erasability classification, shared by every erasure walk: whether a type's values are dropped at runtime, and the signature view of a telescope — one walk ([`signature_entries`]) with a labelled and a mask-only ([`erasure_mask`]) reading. Beside it, the carrier-shape classification of constructor payloads ([`field_shape`], [`constructor_entries`]): erasure is the last walk that still holds the Core field types, so the shape is recorded here and read by `curios-ersd`'s lowering when it decides a family's encoding.

use {
    super::Lowering,
    crate::{Context, Error, is_prop, reduce_with},
    curios_core::{Bound, FuncType, Global, Intrinsic, Subterm, Telescope, Term, TupleType},
    curios_utilities::Grain,
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
    lowering: &mut Lowering,
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
                    false => field_shape(lowering, context, &mut BTreeSet::new(), &type_)?,
                };
                let variable = Term::free_var(&context.fresh(label.as_deref()));
                entries.push((label, erasable, shape));
                telescope = rest.open(&[&variable]);
            }
            Telescope::Done(_) => break Ok(entries),
        }
    }
}

/// The erased carrier shape of a kept field's declared type — the full recorder behind [`curios_ersd::FieldShape`]. `Immediate` iff every runtime value of the type lives in the uniform carrier's immediate population — an intrinsic head riding the i31 carrier, or a chain of single-relevant-field collapses (newtype structs, subset tuples) landing on one. The other shaped answers name the erased carrier the type always takes: the boxed `Flt` struct, a packed grain (a `Handle` token is its bytes at the byte grain, the ABI's encoding), a list rope, a closure at its kept arity, a boxed product row at its relevant width, or a multi-constructor family. Everything unstated answers `Opaque`, and the asymmetry is the point: a conservative answer only misses an encoding or a census entry, an aggressive one would corrupt what is spent on it.
///
/// `visited` makes the chain-chasing terminate: a self-referential struct elaborates (it is merely uninhabited), so the recursion through nominal declarations can cycle, and a cycle classifies `Opaque`. The chain recurses into at most one field per level, so the set is the ancestor chain and never needs unwinding.
pub(crate) fn field_shape(
    lowering: &mut Lowering,
    context: &mut Context,
    visited: &mut BTreeSet<Global>,
    type_: &Term,
) -> Result<curios_ersd::FieldShape, Error> {
    match Term::unwrap_or_clone(reduce_with(context, type_)?) {
        Subterm::Intrinsic(Intrinsic::NatType | Intrinsic::BoolType | Intrinsic::ByteType) => Ok(
            curios_ersd::FieldShape::Immediate(curios_ersd::Sign::Unsigned),
        ),
        Subterm::Intrinsic(Intrinsic::IntType) => Ok(curios_ersd::FieldShape::Immediate(
            curios_ersd::Sign::Signed,
        )),
        Subterm::Intrinsic(Intrinsic::FltType) => Ok(curios_ersd::FieldShape::Flt),
        Subterm::Intrinsic(Intrinsic::BinType(grain)) => Ok(curios_ersd::FieldShape::Packed(grain)),
        Subterm::Intrinsic(Intrinsic::HandleType) => Ok(curios_ersd::FieldShape::Packed(Grain::X)),
        Subterm::Intrinsic(Intrinsic::ListType(_)) => Ok(curios_ersd::FieldShape::List),
        // The field is kept, so the codomain does not erase; the value is a closure over the kept binders of the outermost telescope, which is the arity the lowering gives it.
        Subterm::FuncType(FuncType { telescope, .. }) => {
            let entries = signature_entries(context, telescope)?;
            Ok(curios_ersd::FieldShape::Closure(
                entries.iter().filter(|(_, erased)| !erased).count(),
            ))
        }
        Subterm::StructType(struct_type) => {
            let Some(struct_decl) = context.struct_decl(&struct_type.name).cloned() else {
                return Ok(curios_ersd::FieldShape::Opaque);
            };
            match relevant_chain(context, struct_decl.fields_at(&struct_type.params))? {
                Chain::None => Ok(curios_ersd::FieldShape::Opaque),
                Chain::One(domain) => field_shape(lowering, context, visited, &domain),
                // A struct that reaches itself is uninhabited but elaborates, so its own registration is still in flight here and there is no schema to name yet. Answering `Opaque` costs a shape no value will ever occupy.
                Chain::Many => match lowering.in_flight(&struct_type.name) {
                    true => Ok(curios_ersd::FieldShape::Opaque),
                    false => Ok(lowering
                        .struct_row(context, &struct_type.name)?
                        .schema
                        .map_or(curios_ersd::FieldShape::Opaque, |schema| {
                            curios_ersd::FieldShape::Product(schema)
                        })),
                },
            }
        }
        // An anonymous tuple's row is shared by every tuple of its width and records no shape, so there is no type to declare a slot at — the chain still runs through a one-field tuple, which is a newtype like any other.
        Subterm::TupleType(TupleType { telescope }) => match relevant_chain(context, telescope)? {
            Chain::One(domain) => field_shape(lowering, context, visited, &domain),
            Chain::None | Chain::Many => Ok(curios_ersd::FieldShape::Opaque),
        },
        // A multi-constructor family is its own shape, and its *identity* is what a typed slot needs — never chased, whatever its encoding. A single-constructor family is always its payload row under the collapsed encoding, so the chain continues through it exactly as through a newtype struct; the `visited` guard cuts the cycle a legitimately recursive one would otherwise spin in.
        Subterm::InductType(induct_type) => {
            let Some(induct_decl) = context.induct_decl(&induct_type.name).cloned() else {
                return Ok(curios_ersd::FieldShape::Opaque);
            };
            let tags: Vec<_> = induct_decl.constructor_order().collect();
            let [tag] = tags.as_slice() else {
                return Ok(curios_ersd::FieldShape::Family(
                    lowering.family_identity(context, &induct_type.name)?,
                ));
            };
            if !visited.insert(induct_type.name.clone()) {
                return Ok(curios_ersd::FieldShape::Opaque);
            }
            let Some(telescope) = induct_decl.instantiate(tag, &induct_type.params) else {
                return Ok(curios_ersd::FieldShape::Opaque);
            };
            match relevant_chain(context, telescope)? {
                // No payload rides the interned `Nat` zero; one payload is the value itself; two or more are the family's own row, which the collapsed encoding lays out exactly as the equivalent struct.
                Chain::None => Ok(curios_ersd::FieldShape::Immediate(
                    curios_ersd::Sign::Unsigned,
                )),
                Chain::One(domain) => field_shape(lowering, context, visited, &domain),
                Chain::Many => Ok(curios_ersd::FieldShape::Family(
                    lowering.family_identity(context, &induct_type.name)?,
                )),
            }
        }
        _ => Ok(curios_ersd::FieldShape::Opaque),
    }
}

/// What a product-shaped telescope's relevant entries amount to: none, exactly one (the newtype chain, which the caller continues through), or a row of two or more.
enum Chain {
    None,
    One(Term),
    Many,
}

fn relevant_chain<B: Bound>(
    context: &mut Context,
    mut telescope: Telescope<B>,
) -> Result<Chain, Error> {
    let mut first = None;
    let mut relevant = 0;
    while let Telescope::Cons(type_, rest) = telescope {
        if !is_erasable(context, &type_)? {
            relevant += 1;
            if first.is_none() {
                first = Some(type_);
            }
        }
        let variable = Term::free_var(&context.fresh(None));
        telescope = rest.open(&[&variable]);
    }
    match (relevant, first) {
        (1, Some(domain)) => Ok(Chain::One(domain)),
        (0, _) => Ok(Chain::None),
        _ => Ok(Chain::Many),
    }
}
