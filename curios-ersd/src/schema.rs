//! Product schemas, variant families, and constructors.
//!
//! Structures and inductives register their post-erasure shape here exactly once, from their declarations: a product schema is the ordered row of relevant fields, a variant family is the ordered set of its constructors, and a constructor is its payload row plus its position in the family — the position *is* the discriminant. Construction, projection, and matching reference these identities, which is what keeps the semantic shapes legible to the transformations and hands the lowering everything a tag layout needs.
//!
//! **A row another module declares is an import** ([`Imports`](crate::Imports)), and holds what this module reads of it: a product its width alone, a family the constructors this module names, each at the position its import states. Only the module that declares a row lists it whole, and a program, which has no import, holds every row it names whole.

use super::{ConstructorId, FamilyId, ProductId};

/// The post-erasure field row of a structure (or tuple). Field names are debug metadata only; identity and layout are positional.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct ProductSchema {
    pub debug_name: Option<String>,
    pub fields: ProductFields,
}

/// What a product schema states of its row: a structure's fields, or a width every tuple of it shares.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub enum ProductFields {
    /// A structure's row: one entry per relevant field, in declaration order, each at the shape its declaration states. None where the module imports the schema, whose width the import states ([`Module::product_width`](crate::Module::product_width)).
    Declared(Vec<Field>),
    /// The anonymous row every tuple of this width shares, stated by its width alone: it is written with no type in hand and any tuple of its width answers to it, so it records no shape, and each of its fields reads as opaque. It is also the row a multi-result host call or cell operation packs its results into — a site that never names a schema at all — so a shared row stays a structural tuple downstream, where a declared structure's row earns a heap type of its own.
    Shared(usize),
}

impl ProductSchema {
    /// The number of fields it states: a declared row's, a shared row's width, and none where the module imports the schema, whose width the import states ([`Module::product_width`](crate::Module::product_width)).
    pub fn width(&self) -> usize {
        match &self.fields {
            ProductFields::Declared(fields) => fields.len(),
            ProductFields::Shared(width) => *width,
        }
    }

    /// Whether every row of its width shares it ([`ProductFields::Shared`]).
    pub fn is_shared(&self) -> bool {
        matches!(self.fields, ProductFields::Shared(_))
    }

    /// The fields a declared row lists, and none for a shared one, whose fields state nothing a width does not.
    pub fn declared_fields(&self) -> &[Field] {
        match &self.fields {
            ProductFields::Declared(fields) => fields,
            ProductFields::Shared(_) => &[],
        }
    }

    /// The carrier shape of each field in order: a declared row's as its declaration states, and a shared row's opaque.
    pub fn shapes(&self) -> Vec<FieldShape> {
        match &self.fields {
            ProductFields::Declared(fields) => fields.iter().map(|field| field.shape).collect(),
            ProductFields::Shared(width) => vec![FieldShape::Opaque; *width],
        }
    }
}

/// The post-erasure shape of an inductive type: the constructors the module holds of it. Where the module declares the family, that is every one in declaration order, and a constructor's index in the list is its discriminant; where it imports the family, the ones it names, each at the position its import states ([`Module::constructor_position`](crate::Module::constructor_position)).
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct VariantFamily {
    pub debug_name: Option<String>,
    pub constructors: Vec<ConstructorId>,
}

/// One constructor of a [`VariantFamily`]: its payload row and a back-link to its family. The discriminant is the constructor's position in its family's declaration, not stored here: its index in the list where the module declares the family, and what its import states otherwise.
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
            shape: FieldShape::Immediate,
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

/// The erased carrier shape of one relevant field, recorded by erasure — the one walk that still holds the Core field types — for every constructor payload and product entry. The lowering into Cont reads it twice. `Immediate` and `Number` decide a family's encoding: `Immediate` means every runtime value of the field's declared type lives in the uniform carrier's immediate population — an intrinsic head riding the i31 carrier, or a chain of single-relevant-field collapses landing on one — and `Number` that every one is a `Nat` or `Int`, an i31 or the boxed magnitude past it. And every shape names the carrier of the row slot the field is stored in. A recorded shape must therefore be *true* rather than useful — `Opaque` covers polymorphic fields and everything unstated, and never misleads, where a wrong shape would. Its register half, `Immediate` and `Flt`, is what a binder states of its own type ([`RegisterShape`]).
///
/// `Immediate` is a word population — a `Bool`, a `Byte`, a payload-less constructor riding the zero — which a typed slot holds as an unsigned word. A `Nat` or `Int` is not one of them: either is an i31 only while it is small and a boxed magnitude otherwise, so it is sometimes-immediate and records `Number`, whose slot is a reference. `Immediate` means *always*, never *sometimes*: a small `Bytes` value rides the i31, so a packed carrier is sometimes-immediate — and sometimes is `Bytes`, never `Immediate`, because the `Immediate` family encoding's discrimination is disjoint only while the bare payload's boxed form is a type no row can be. A `Number` meets that — its boxed form is the final `$big`, which the discrimination admits beside the i31 — and a rope does not, so a `Number` payload rides bare and a `Bytes` one never does. `packed_unary_payload_declines_the_immediate_encoding`, in `curios`'s codegen tests, pins the consequence end to end. The same always-versus-sometimes line runs through the rest of the roster: `Flt` is the boxed `f64` struct, `Bytes` a packed value (immediate inside the envelope, a rope past it — `Handle` tokens classify as `Bytes`, the ABI's own encoding), `List` a list rope, `Closure` a function value at its erased arity, `Product` a boxed product row *named by its schema* (always two or more relevant — zero and one collapse through the newtype chain before this is recorded), and `Family` a value of the named variant family. The last two carry an identity rather than a width or a bare marker because a heap type keyed by a schema or a family is the only thing a slot can be declared at: a width names one type per arity and is therefore no type at all.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum FieldShape {
    Immediate,
    Number,
    Flt,
    Bytes,
    List,
    Closure(usize),
    Product(ProductId),
    Family(FamilyId),
    Opaque,
}

/// The register carrier a binder's type names, recorded by erasure — the one walk that still holds the type — where the binder's producer names none: a function parameter, a call's result, a field or payload read, a sequence element. It is the register half of [`FieldShape`], read by the walk that reads a field's, and like it must be *true* rather than useful: every runtime value the binder holds is one of the population named, and a type that does not say so states nothing. A product or a family states nothing here even where a field of that type would name its row: a binder is held as a reference whatever row it holds.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum RegisterShape {
    /// An unsigned word — a `Bool`, a `Byte`, a payload-less constructor riding the zero — which as a reference is always the i31 holding it.
    Immediate,
    /// A binary64, which as a reference is always the `Flt` box holding it.
    Flt,
}
