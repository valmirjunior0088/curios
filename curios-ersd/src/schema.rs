//! Product schemas, variant families, and constructors.
//!
//! Structures and inductives register their post-erasure shape here exactly once, from their declarations: a product schema is the ordered row of relevant fields, a variant family is the ordered set of its constructors, and a constructor is its payload row plus its position in the family — the position *is* the discriminant. Construction, projection, and matching reference these identities, which is what keeps the semantic shapes legible to the transformations and hands the lowering everything a tag layout needs.

use {
    super::{ConstructorId, FamilyId, ProductId},
    curios_utilities::Grain,
};

/// The post-erasure field row of a structure (or tuple). Field names are debug metadata only; identity and layout are positional.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct ProductSchema {
    pub debug_name: Option<String>,
    /// One entry per relevant field, in declaration order.
    pub fields: Vec<Field>,
    /// Whether every row of this width shares this schema.
    ///
    /// The interned anonymous row is shared, and that is why it records no shape: it is written with no type in hand, and any tuple of its arity answers to it. It is also the row a multi-result host call or cell operation packs its results into — a site that never names a schema at all — so a shared row must stay a structural tuple downstream, where a declared structure's row earns a heap type of its own.
    pub shared: bool,
}

impl ProductSchema {
    /// The number of fields.
    pub fn width(&self) -> usize {
        self.fields.len()
    }
}

/// The post-erasure shape of an inductive type: its constructors in declaration order. A constructor's index in this list is its discriminant.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct VariantFamily {
    pub debug_name: Option<String>,
    pub constructors: Vec<ConstructorId>,
}

/// One constructor of a [`VariantFamily`]: its payload row and a back-link to its family. The discriminant is the constructor's position in the family's list, not stored here.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct Constructor {
    pub debug_name: Option<String>,
    pub family: FamilyId,
    /// One entry per relevant payload field, in declaration order.
    pub fields: Vec<Field>,
}

impl Constructor {
    /// The number of payload fields.
    pub fn width(&self) -> usize {
        self.fields.len()
    }
}

/// One relevant field of a schema row — a [`Constructor`] payload entry or a [`ProductSchema`] entry: its optional debug name, and its recorded carrier shape.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct Field {
    pub debug_name: Option<String>,
    pub shape: FieldShape,
}

impl Field {
    /// A field whose values are unsigned immediates at runtime.
    pub fn immediate(debug_name: Option<String>) -> Self {
        Self {
            debug_name,
            shape: FieldShape::Immediate(Sign::Unsigned),
        }
    }

    /// A field with no shape guarantee — the conservative entry.
    pub fn opaque(debug_name: Option<String>) -> Self {
        Self {
            debug_name,
            shape: FieldShape::Opaque,
        }
    }
}

/// The erased carrier shape of one relevant field, recorded by erasure — the one walk that still holds the Core field types — for every constructor payload and product entry. One variant is *spent* today: `Immediate` is read by the lowering into Cont when it decides a family's encoding, and it means every runtime value of the field's declared type lives in the uniform carrier's immediate population — an intrinsic head riding the i31 carrier, or a chain of single-relevant-field collapses landing on one. Every other shaped variant is pure record: the census over these rows is what prices the typed-slot campaign, and a recorded shape must therefore be *true* rather than useful — `Opaque` covers polymorphic fields and everything unstated, and never misleads, where a wrong shape would.
///
/// `Immediate` carries the raw carrier its population occupies ([`Sign`]), which the family encoding ignores and a typed slot spends. `Immediate` means *always*, never *sometimes*: since the map-wall campaign a small `Bytes` value rides the i31, so a packed carrier is sometimes-immediate — and sometimes is `Packed`, never `Immediate`, because the `Immediate` family encoding's discrimination is disjoint only while the bare payload can never box. `packed_unary_payload_declines_the_immediate_encoding`, in `curios`'s codegen tests, pins the consequence end to end. The same always-versus-sometimes line runs through the rest of the roster: `Flt` is the boxed `f32` struct, `Packed` a `Bytes`/`Bits` value at its grain (immediate inside the envelope, a rope past it — `Handle` tokens classify as byte-grain packed, the ABI's own encoding), `List` a list rope, `Closure` a function value at its erased arity, `Product` a boxed product row *named by its schema* (always two or more relevant — zero and one collapse through the newtype chain before this is recorded), and `Family` a value of the named variant family. The last two carry an identity rather than a width or a bare marker because a heap type keyed by a schema or a family is the only thing a slot can be declared at: a width names one type per arity and is therefore no type at all.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum FieldShape {
    Immediate(Sign),
    Flt,
    Packed(Grain),
    List,
    Closure(usize),
    Product(ProductId),
    Family(FamilyId),
    Opaque,
}

/// Which raw carrier an immediate field's values occupy once a slot holds them unboxed: the unsigned population — `Nat`, `Bool`, `Byte`, and the nullary constructor riding the interned zero — or the signed `Int` one.
///
/// Both ride the i31, so the family encoding's admission reads [`FieldShape::Immediate`] without looking here. The distinction is spent one step later, by the slot a typed heap field is declared at: a slot naming the wrong carrier is not a wrong answer — the bit pattern round-trips either way — but every read of it coerces, which is exactly the cost typing the slot exists to delete.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum Sign {
    Unsigned,
    Signed,
}
