//! Sort-driven erasability classification, shared by every erasure walk: whether a type's values carry anything at runtime, and the view of a nominal declaration's payload that says which of its fields are stored ([`constructor_entries`]), with the carrier shape of each ([`field_shape`]), and the register carrier a binder's type names ([`register_shape`]): erasure is the last walk that still holds the Core types, so a shape is recorded here, a field's read by `curios-ersd`'s lowering when it decides a family's encoding and a binder's by `curios-cont`'s storage analysis when it decides what a reference may feed.
//!
//! **Only a declaration drops.** A structure's fields and a constructor's payload are classified once, against the declaration's own signature, and every construction and elimination reads that one row. A function type and an anonymous tuple type have no declaration: the type a value of one stands at differs from site to site by a substitution or a refinement, and a domain one site reads as a proof or a type another reads as a variable. So neither is classified for its shape at all — a function has a parameter for each binder and a tuple a field for each member — and what [`is_erasable`] decides of a slot is only what fills it.

use {
    super::Lowering,
    crate::{Error, Opened, Retyping},
    curios_core::{
        Advance, Bound, FuncType, Global, Intrinsic, Subterm, Telescope, Term, TupleType,
    },
    std::collections::BTreeSet,
};

/// Whether a value of type `type_` carries nothing at run time, by the engine's rule read through erasure's reader ([`crate::is_erasable_in`]): a type, a proof, or a function that ultimately answers one. The empty tuple is none of them, so `{}`, `{ .., {} }` and `X -> {}` are kept.
///
/// CRITICAL: an answer here is one reading's. A polymorphic field `value : A` is kept (its abstract `A` is neither prop nor type) and reads as a proof at a construction where `A := SomeProp`. So a runtime shape is decided from this only for a nominal declaration, against its *declared* signature opened with the surrounding binders as opaque variables ([`constructor_entries`]); everywhere else the answer decides what a slot holds and never whether the slot is there.
pub(crate) fn is_erasable(context: &mut Retyping, type_: &Term) -> Result<bool, Error> {
    context.is_erasable_in(&mut Opened::default(), type_)
}

/// The payload view of a nominal declaration's telescope: one entry per binder — its label, whether it is erased, and the recorded carrier shape of a kept one — classifying each domain with the *preceding* binders opened as fresh opaque variables carrying their declared types (see [`crate::is_erasable_in`]). That opaque-open discipline is what makes a row a fact of its declaration, the same at every instantiation, so this is the only walk that computes one; the terminal body is ignored. It pairs with a concrete walk over the actual values: the row decides which to drop, the concrete walk erases the kept ones against their (dependent, instantiated) types. An erased entry's shape is `Opaque`; nothing reads it.
pub(crate) fn constructor_entries<B: Bound>(
    lowering: &mut Lowering<'_>,
    context: &mut Retyping,
    telescope: Telescope<B>,
) -> Result<Vec<(Option<String>, bool, curios_ersd::FieldShape)>, Error> {
    let mut entries = Vec::new();
    let mut opened = Opened::default();
    let mut cursor = telescope.cursor();
    while let Some((hint, type_)) = cursor.entry() {
        let label = hint.map(str::to_string);
        let erasable = context.is_erasable_in(&mut opened, &type_)?;
        let shape = match erasable {
            true => curios_ersd::FieldShape::Opaque,
            false => field_shape(lowering, context, &mut BTreeSet::new(), &type_)?,
        };
        let name = cursor.advance_fresh(|hint| context.fresh(hint));
        entries.push((label, erasable, shape));
        opened.push(name, type_);
    }
    Ok(entries)
}

/// The erased carrier shape of a kept field's declared type — the full recorder behind [`curios_ersd::FieldShape`]. `Immediate` iff every runtime value of the type lives in the uniform carrier's immediate population — an intrinsic head riding the i31 carrier, or a chain of single-field collapses (a structure with one relevant field, a tuple of one field) landing on one. `Number` is a `Nat` or `Int`, an i31 while small and a boxed magnitude past it. The other shaped answers name the erased carrier the type always takes: the boxed `Flt` struct, a packed `Bytes` (a `Handle` token is its bytes, the ABI's encoding), a list rope, a closure at its telescope's arity, a boxed product row at its relevant width, or a multi-constructor family. Everything unstated answers `Opaque`, and the asymmetry is the point: a conservative answer only misses an encoding or a census entry, an aggressive one would corrupt what is spent on it.
///
/// `visited` makes the chain-chasing terminate: a self-referential struct elaborates (it is merely uninhabited), so the recursion through nominal declarations can cycle, and a cycle classifies `Opaque`. The chain recurses into at most one field per level, so the set is the ancestor chain and never needs unwinding.
pub(crate) fn field_shape(
    lowering: &mut Lowering<'_>,
    context: &mut Retyping,
    visited: &mut BTreeSet<Global>,
    type_: &Term,
) -> Result<curios_ersd::FieldShape, Error> {
    shape_walk(lowering, Rows::Named, context, visited, type_)
}

/// The register carrier a value of `type_` is held in where a register holds it ([`curios_ersd::RegisterShape`]): read by the walk a field's shape is read by, so a value and a field of one type agree, with a product and a family answering nothing rather than naming their rows — a value is held as a reference whatever row it holds.
pub(crate) fn register_shape(
    lowering: &mut Lowering<'_>,
    context: &mut Retyping,
    type_: &Term,
) -> Result<Option<curios_ersd::RegisterShape>, Error> {
    Ok(
        match shape_walk(
            lowering,
            Rows::Unnamed,
            context,
            &mut BTreeSet::new(),
            type_,
        )? {
            curios_ersd::FieldShape::Immediate => Some(curios_ersd::RegisterShape::Immediate),
            curios_ersd::FieldShape::Flt => Some(curios_ersd::RegisterShape::Flt),
            _ => None,
        },
    )
}

/// Whether a walk names a product's or a family's row, which registers it in the fragment, or answers `Opaque` for one: what a value needs, and what keeps reading a value's type from registering a row the program never builds.
#[derive(Clone, Copy)]
enum Rows {
    Named,
    Unnamed,
}

/// What a nominal declaration's row says a value of it is, read off the row's mask: no relevant field, exactly one, at its place in the declaration, which the value is, or a row of two or more.
///
/// A mask is the declaration's, so the answer holds at every instantiation: one reading that erases a field the declaration keeps, a parameter instantiated at a proposition, holds the value as the declaration lays it out all the same.
#[derive(Clone, Copy)]
pub(super) enum Relevance {
    None,
    One(usize),
    Many,
}

impl Relevance {
    pub(super) fn of(mask: &[bool]) -> Self {
        let mut relevant = mask
            .iter()
            .enumerate()
            .filter(|&(_, &erased)| !erased)
            .map(|(place, _)| place);
        match (relevant.next(), relevant.next()) {
            (None, _) => Self::None,
            (Some(place), None) => Self::One(place),
            (Some(_), Some(_)) => Self::Many,
        }
    }
}

/// The type of the entry at `place` of `telescope`, the entries before it opened as fresh variables.
fn entry_at<B: Bound>(
    context: &mut Retyping,
    telescope: Telescope<B>,
    place: usize,
) -> Option<Term> {
    let mut cursor = telescope.cursor();
    for _ in 0..place {
        if cursor.is_done() {
            return None;
        }
        cursor.advance_fresh(|hint| context.fresh(hint));
    }
    cursor.entry().map(|(_, type_)| type_)
}

/// The walk behind [`field_shape`] and [`register_shape`]. A nominal type is what its declaration's row says ([`Relevance`]), read where the row is registered, and `rows` says whether a product or a family names its row.
fn shape_walk(
    lowering: &mut Lowering<'_>,
    rows: Rows,
    context: &mut Retyping,
    visited: &mut BTreeSet<Global>,
    type_: &Term,
) -> Result<curios_ersd::FieldShape, Error> {
    let reduced = context.reduce_with(type_)?;
    match &*reduced {
        Subterm::Intrinsic(Intrinsic::BoolType | Intrinsic::ByteType) => {
            Ok(curios_ersd::FieldShape::Immediate)
        }
        // A `Nat` or `Int` rides the i31 only while it is small and is a boxed magnitude past it, so it is sometimes an immediate: not `Immediate`, but a shape of its own the family encoding can still discriminate.
        Subterm::Intrinsic(Intrinsic::NatType | Intrinsic::IntType) => {
            Ok(curios_ersd::FieldShape::Number)
        }
        Subterm::Intrinsic(Intrinsic::FltType) => Ok(curios_ersd::FieldShape::Flt),
        Subterm::Intrinsic(Intrinsic::BytesType) => Ok(curios_ersd::FieldShape::Bytes),
        Subterm::Intrinsic(Intrinsic::HandleType) => Ok(curios_ersd::FieldShape::Bytes),
        Subterm::Intrinsic(Intrinsic::ListType(_)) => Ok(curios_ersd::FieldShape::List),
        // The field is kept, so the codomain does not erase; the value is a closure over every binder of the outermost telescope, which is the arity the lowering gives it.
        Subterm::FuncType(FuncType { telescope, .. }) => {
            Ok(curios_ersd::FieldShape::Closure(telescope.len()))
        }
        Subterm::StructType(struct_type) => {
            // A struct that reaches itself is uninhabited but elaborates, so its own registration is still in flight here and there is no row to read yet. Answering `Opaque` costs a shape no value will ever occupy.
            let Some(relevance) = lowering.struct_relevance(context, &struct_type.name)? else {
                return Ok(curios_ersd::FieldShape::Opaque);
            };
            match relevance {
                Relevance::None => Ok(curios_ersd::FieldShape::Opaque),
                // The cycle the family arm below cuts, cut here too: a struct naming itself elaborates, so chasing its one relevant field returns to this declaration and the chain never lands. Guarding only where the chain continues keeps `visited` the ancestor chain rather than a seen-set.
                Relevance::One(place) => {
                    if !visited.insert(struct_type.name) {
                        return Ok(curios_ersd::FieldShape::Opaque);
                    }
                    let Some(fields) = context
                        .struct_decl(&struct_type.name)
                        .map(|struct_decl| struct_decl.fields_at(&struct_type.params))
                    else {
                        return Ok(curios_ersd::FieldShape::Opaque);
                    };
                    match entry_at(context, fields, place) {
                        Some(domain) => shape_walk(lowering, rows, context, visited, &domain),
                        None => Ok(curios_ersd::FieldShape::Opaque),
                    }
                }
                Relevance::Many => match rows {
                    Rows::Named => Ok(lowering
                        .struct_row(context, &struct_type.name)?
                        .schema
                        .map_or(curios_ersd::FieldShape::Opaque, |schema| {
                            curios_ersd::FieldShape::Product(schema)
                        })),
                    Rows::Unnamed => Ok(curios_ersd::FieldShape::Opaque),
                },
            }
        }
        // An anonymous tuple's row is shared by every tuple of its width and records no shape, so there is no type to declare a slot at — the chain still runs through a tuple of one field, which is that field. A tuple drops no field, so its width is its telescope's.
        Subterm::TupleType(TupleType { telescope }) => match telescope.len() {
            1 => {
                let (_, domain) = telescope.cursor().entry().expect("a tuple of one field");

                shape_walk(lowering, rows, context, visited, &domain)
            }
            _ => Ok(curios_ersd::FieldShape::Opaque),
        },
        // A multi-constructor family is its own shape, and its *identity* is what a typed slot needs — never chased, whatever its encoding. A single-constructor family is always its payload row under the collapsed encoding, so the chain continues through it exactly as through a newtype struct; the `visited` guard cuts the cycle a legitimately recursive one would otherwise spin in.
        Subterm::InductType(induct_type) => {
            let family = |lowering: &mut Lowering<'_>, context: &mut Retyping| match rows {
                Rows::Named => Ok(curios_ersd::FieldShape::Family(
                    lowering.family_identity(context, &induct_type.name)?,
                )),
                Rows::Unnamed => Ok(curios_ersd::FieldShape::Opaque),
            };
            match context
                .induct_decl(&induct_type.name)
                .map(|induct_decl| induct_decl.constructors.len())
            {
                Some(1) => {}
                Some(_) => return family(lowering, context),
                None => return Ok(curios_ersd::FieldShape::Opaque),
            }
            if !visited.insert(induct_type.name) {
                return Ok(curios_ersd::FieldShape::Opaque);
            }
            // A family whose row is being registered right now has no mask to read yet: a field of it is the one reaching it, whose shape the cycle makes `Opaque`.
            let Some(relevance) = lowering.constructor_relevance(context, &induct_type.name)?
            else {
                return Ok(curios_ersd::FieldShape::Opaque);
            };
            match relevance {
                // No payload rides the interned `Nat` zero; one payload is the value itself; two or more are the family's own row, which the collapsed encoding lays out exactly as the equivalent struct.
                Relevance::None => Ok(curios_ersd::FieldShape::Immediate),
                Relevance::One(place) => {
                    let Some(payload) =
                        context
                            .induct_decl(&induct_type.name)
                            .and_then(|induct_decl| {
                                let (tag, _) = induct_decl.constructors.first()?;
                                induct_decl.instantiate(tag, &induct_type.params)
                            })
                    else {
                        return Ok(curios_ersd::FieldShape::Opaque);
                    };
                    match entry_at(context, payload, place) {
                        Some(domain) => shape_walk(lowering, rows, context, visited, &domain),
                        None => Ok(curios_ersd::FieldShape::Opaque),
                    }
                }
                Relevance::Many => family(lowering, context),
            }
        }
        _ => Ok(curios_ersd::FieldShape::Opaque),
    }
}
