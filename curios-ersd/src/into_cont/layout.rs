//! How a nominal shape becomes a Cont heap type: the encoding each variant family gets, the slot every field lands in, and the row identity that names it.
//!
//! One question, asked from every construction site and every match arm, whose answer has to be the same each time — so it is computed once per family or schema and memoized. [`lay_out`] is the packing rule the answers are built from: slots grouped by carrier rather than by field position, so two writers agreeing on a carrier share its slots and only a disagreement costs width.

use {
    super::{ConstructorId, FamilyId, FieldShape, Module, ProductId, Sign},
    std::collections::BTreeMap,
};

/// How a variant family is encoded at runtime, decided per family from its registered schema alone — see [`Layout::family_encoding`].
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum FamilyEncoding {
    /// Every constructor a tagged tuple `(tag, payload…)` — the general encoding.
    Tagged,
    /// A single-constructor family: nothing ever needs discriminating, so it encodes as the struct with the same relevant row would — one payload is the bare value, several are an untagged tuple, none is the `Nat` zero — and a match on it never dispatches.
    Collapsed,
    /// A multi-constructor family whose one immediate-unary constructor rides as its bare payload; every other constructor keeps its tagged tuple. Discrimination is an `IsImmediate` test — the payload is always an immediate and every other constructor a struct, so the two answers are disjoint by construction, and exactly one such constructor is admitted because two would collide on the same immediates.
    Immediate { constructor: ConstructorId },
}

/// Where one erased row's writers live in its Cont heap type: the arity every construction of it carries, and the slot each writer's relevant fields occupy. The identity is not here — it is claimed before the layout exists, so it lives in the map that hands it out.
///
/// A variant family has one writer per constructor, indexed by the constructor's position — which is its tag. A product schema has exactly one, at index zero.
#[derive(Debug, Clone)]
pub(super) struct RowLayout {
    pub(super) width: usize,
    places: Vec<Vec<usize>>,
}

/// Lay out one nominal row: a tag slot where the row carries one, then a slot range per carrier sized to the widest writer's count of it, and the slot each writer's fields land in.
///
/// Grouping by carrier rather than by field position is what lets every slot name a carrier without the row widening: two writers agreeing on a carrier share its slots, so only a disagreement costs width. A row with a single writer — a product schema, a collapsed family — has no disagreement to pay for, and the grouping degenerates to a permutation of its fields.
fn lay_out(
    tagged: bool,
    writers: &[Vec<curios_cont::CpsSlot>],
) -> (Vec<curios_cont::CpsSlot>, Vec<Vec<usize>>) {
    let mut widths = BTreeMap::<curios_cont::CpsSlot, usize>::new();
    for carriers in writers {
        let mut here = BTreeMap::<curios_cont::CpsSlot, usize>::new();
        for &carrier in carriers {
            *here.entry(carrier).or_default() += 1;
        }
        for (carrier, count) in here {
            let width = widths.entry(carrier).or_default();
            *width = (*width).max(count);
        }
    }

    let mut slots = match tagged {
        true => vec![curios_cont::CpsSlot::Tag],
        false => Vec::new(),
    };
    let mut starts = BTreeMap::<curios_cont::CpsSlot, usize>::new();
    for (&carrier, &count) in &widths {
        starts.insert(carrier, slots.len());
        slots.extend(std::iter::repeat_n(carrier, count));
    }

    let places = writers
        .iter()
        .map(|carriers| {
            let mut taken = BTreeMap::<curios_cont::CpsSlot, usize>::new();
            carriers
                .iter()
                .map(|carrier| {
                    let offset = taken.entry(*carrier).or_default();
                    let place = starts[carrier] + *offset;
                    *offset += 1;
                    place
                })
                .collect()
        })
        .collect();

    (slots, places)
}

/// The Cont row every nominal shape lowers through, and the identity each one was given.
///
/// Split from [`Lowerer`](super::Lowerer) because it is the one part of the lowering that answers a question rather than emitting a step: given a family or a product schema, what does its heap type look like and what is it called? The answers are memoized, and the two maps of identities are separate from the two of layouts because an identity is claimed *before* the layout that would justify it exists — a row whose slots name itself has to terminate.
///
/// The Cont module is passed in rather than held, because the lowering owns it and writes to it between these calls. Everything else this needs is the erased module, which never changes.
pub(super) struct Layout<'a> {
    source: &'a Module,
    /// The Cont layout of each tagged family, computed on first use. Only the tagged encodings register one: a collapsed family builds a bare value or a structural tuple, and an immediate family's bare constructor is a scalar, so neither has a family heap type to key.
    families: BTreeMap<FamilyId, RowLayout>,
    /// The Cont layout of each product schema, computed on first use.
    products: BTreeMap<ProductId, RowLayout>,
    /// The Cont identity of each row, claimed *before* its layout is computed. A row's slots may name other rows and a self-referential declaration names its own, so identity has to be answerable while the layout that would answer it is still being built.
    family_ids: BTreeMap<FamilyId, curios_cont::CpsRowId>,
    product_ids: BTreeMap<ProductId, curios_cont::CpsRowId>,
}

impl<'a> Layout<'a> {
    pub(super) fn new(source: &'a Module) -> Self {
        Self {
            source,
            families: BTreeMap::new(),
            products: BTreeMap::new(),
            family_ids: BTreeMap::new(),
            product_ids: BTreeMap::new(),
        }
    }

    /// The slot carrier a recorded field shape names, resolving a nominal shape to the Cont row that holds it.
    ///
    /// Two shapes answer [`curios_cont::CpsSlot::Opaque`] whatever else is true, and for one reason: a carrier that is *sometimes* an immediate has no single heap type to name. A packed value is one; so is a value of an [`FamilyEncoding::Immediate`] family, whose bare constructor rides the i31 while its siblings allocate. Everything unshaped is opaque by definition.
    ///
    /// A *family*-typed field is named here but not necessarily kept: slots are grouped by carrier, so giving a family its own carrier can cost the row width it would otherwise share with the uniform range. [`Layout::compute_row_layout`] lays the row out both ways and keeps this one only where it is free.
    pub(super) fn slot_of(
        &mut self,
        module: &mut curios_cont::CpsModule,
        shape: FieldShape,
        family_typed: bool,
    ) -> curios_cont::CpsSlot {
        match shape {
            FieldShape::Immediate(Sign::Unsigned) => curios_cont::CpsSlot::Nat,
            FieldShape::Immediate(Sign::Signed) => curios_cont::CpsSlot::Int,
            FieldShape::Flt => curios_cont::CpsSlot::Flt,
            FieldShape::List => curios_cont::CpsSlot::List,
            FieldShape::Closure(arity) => curios_cont::CpsSlot::Closure(arity),
            FieldShape::Product(schema) => {
                curios_cont::CpsSlot::Row(self.product_identity(module, schema))
            }
            // An immediate family's values are *sometimes* the row struct and sometimes the bare payload riding the i31, so no single heap type names its population — the same always-never-sometimes line that keeps a packed carrier out. Every other encoding allocates the row for every constructor.
            FieldShape::Family(family)
                if family_typed
                    && !matches!(
                        self.family_encoding(family),
                        FamilyEncoding::Immediate { .. }
                    ) =>
            {
                curios_cont::CpsSlot::Row(self.row_identity(module, family))
            }
            FieldShape::Family(_) | FieldShape::Packed(_) | FieldShape::Opaque => {
                curios_cont::CpsSlot::Opaque
            }
        }
    }

    /// The carriers each of `family`'s constructors writes, with its family-typed fields named or left uniform.
    pub(super) fn row_writers(
        &mut self,
        module: &mut curios_cont::CpsModule,
        family: FamilyId,
        bare: Option<ConstructorId>,
        family_typed: bool,
    ) -> Vec<Vec<curios_cont::CpsSlot>> {
        let constructors = self
            .source
            .family(family)
            .expect("live family")
            .constructors
            .clone();
        constructors
            .iter()
            .map(|&constructor| match Some(constructor) == bare {
                true => Vec::new(),
                false => {
                    let shapes: Vec<FieldShape> = self
                        .source
                        .constructor(constructor)
                        .expect("live constructor")
                        .fields
                        .iter()
                        .map(|field| field.shape)
                        .collect();
                    shapes
                        .into_iter()
                        .map(|shape| self.slot_of(module, shape, family_typed))
                        .collect()
                }
            })
            .collect()
    }
    /// The encoding of `family`: a pure function of the registered schema, so every construction and match site computes the same answer and no state has to keep them agreeing.
    pub(super) fn family_encoding(&self, family: FamilyId) -> FamilyEncoding {
        let constructors = &self
            .source
            .family(family)
            .expect("live family")
            .constructors;
        if let [_] = constructors.as_slice() {
            return FamilyEncoding::Collapsed;
        }
        let mut immediate_unary = constructors.iter().filter(|&&constructor| {
            let fields = &self
                .source
                .constructor(constructor)
                .expect("live constructor")
                .fields;
            matches!(fields.as_slice(), [field] if matches!(field.shape, FieldShape::Immediate(_)))
        });
        match (immediate_unary.next(), immediate_unary.next()) {
            (Some(&constructor), None) => FamilyEncoding::Immediate { constructor },
            _ => FamilyEncoding::Tagged,
        }
    }

    /// The Cont layout of `family`, computed on first use and memoized.
    ///
    /// Slot zero is the tag; the payload slots are grouped by carrier, each group as wide as the constructor holding the most fields of that carrier. So two constructors agreeing on a carrier share its slots and only a disagreement costs width, which is what lets every slot name a carrier without the family widening: over the standard library this settles 22 slots against positional assignment's 11, for ten slots more across the whole roster and no growth at all in the families that allocate hot.
    ///
    /// A [`FamilyEncoding::Immediate`] family's bare constructor writes no slot — its value *is* its payload — so it contributes nothing to the widths.
    pub(super) fn row_layout(
        &mut self,
        module: &mut curios_cont::CpsModule,
        family: FamilyId,
    ) -> &RowLayout {
        self.row_identity(module, family);
        &self.families[&family]
    }

    pub(super) fn compute_row_layout(
        &mut self,
        module: &mut curios_cont::CpsModule,
        family: FamilyId,
        id: curios_cont::CpsRowId,
    ) -> RowLayout {
        let definition = self.source.family(family).expect("live family");
        let debug_name = definition.debug_name.clone();
        let encoding = self.family_encoding(family);
        // A collapsed family discriminates nothing, so it mints no tag and encodes exactly as the struct with the same relevant row does. An immediate family's bare constructor writes no slot at all — its value *is* its payload — so it contributes nothing to the widths.
        let tagged = !matches!(encoding, FamilyEncoding::Collapsed);
        let bare = match encoding {
            FamilyEncoding::Immediate { constructor } => Some(constructor),
            FamilyEncoding::Collapsed | FamilyEncoding::Tagged => None,
        };
        // Both layouts, and the typed one only where it is free.
        //
        // A family-typed slot cannot share the uniform range, so a family whose constructors disagree pays width for it — `/std/Map/Node` goes four slots to six to make its two children concrete, on the corpus's hottest allocated row, and what that buys is the *cheap* kind of cast: an exact compare against a final type, not the `is_subtype` libcall a list or closure slot deletes. Weighed against the `trees` finding that live bytes convert to time under an all-live collector, that trade is declined. The criterion is exact rather than a heuristic — the row widens or it does not — so every free win is still taken; `family_slot_probe` in `curios`'s codegen tests is its figure.
        let typed = self.row_writers(module, family, bare, true);
        let uniform = self.row_writers(module, family, bare, false);
        let (typed_slots, typed_places) = lay_out(tagged, &typed);
        let (slots, places) = match lay_out(tagged, &uniform) {
            (uniform_slots, _) if uniform_slots.len() == typed_slots.len() => {
                (typed_slots, typed_places)
            }
            uniform => uniform,
        };
        let width = slots.len();
        module.define_row(id, curios_cont::CpsRow { debug_name, slots });
        RowLayout { width, places }
    }

    /// The Cont layout of a product schema, computed on first use. One writer, so every slot is written and none is ever a filler.
    pub(super) fn product_layout(
        &mut self,
        module: &mut curios_cont::CpsModule,
        schema: ProductId,
    ) -> &RowLayout {
        self.product_identity(module, schema);
        &self.products[&schema]
    }

    /// Whether every row of this width shares `schema` — see [`ProductSchema::shared`](crate::ProductSchema::shared).
    pub(super) fn is_shared(&self, schema: ProductId) -> bool {
        self.source.product(schema).expect("live product").shared
    }

    /// The Cont identity of a product schema, laying its row out on first use. See [`Layout::row_identity`] for why the reservation comes first.
    pub(super) fn product_identity(
        &mut self,
        module: &mut curios_cont::CpsModule,
        schema: ProductId,
    ) -> curios_cont::CpsRowId {
        if let Some(&id) = self.product_ids.get(&schema) {
            return id;
        }
        let id = module.reserve_row();
        self.product_ids.insert(schema, id);
        {
            let definition = self.source.product(schema).expect("live product");
            let debug_name = definition.debug_name.clone();
            // One writer, so every field takes a slot of its own and no carrier can widen the row: the typed layout is free by construction.
            let shapes: Vec<FieldShape> =
                definition.fields.iter().map(|field| field.shape).collect();
            let writer: Vec<curios_cont::CpsSlot> = shapes
                .into_iter()
                .map(|shape| self.slot_of(module, shape, true))
                .collect();
            let (slots, places) = lay_out(false, std::slice::from_ref(&writer));
            let width = slots.len();
            module.define_row(id, curios_cont::CpsRow { debug_name, slots });
            self.products.insert(schema, RowLayout { width, places });
        }
        id
    }

    /// The slot each of a product schema's relevant fields occupies.
    pub(super) fn product_slots(
        &mut self,
        module: &mut curios_cont::CpsModule,
        schema: ProductId,
    ) -> Vec<usize> {
        self.product_layout(module, schema).places[0].clone()
    }

    /// The Cont identity of `family`, laying its row out on first use.
    ///
    /// The identity is registered *before* the layout is computed, which is what lets a row whose slots name itself terminate: the recursive call finds the reservation and returns. Reserving and defining are one operation so that no identity can be handed out for a row nothing ever declares.
    pub(super) fn row_identity(
        &mut self,
        module: &mut curios_cont::CpsModule,
        family: FamilyId,
    ) -> curios_cont::CpsRowId {
        if let Some(&id) = self.family_ids.get(&family) {
            return id;
        }
        let id = module.reserve_row();
        self.family_ids.insert(family, id);
        let layout = self.compute_row_layout(module, family, id);
        self.families.insert(family, layout);
        id
    }

    /// The arity every construction of `family` is padded to.
    pub(super) fn row_width(
        &mut self,
        module: &mut curios_cont::CpsModule,
        family: FamilyId,
    ) -> usize {
        self.row_layout(module, family).width
    }

    /// The slot each of `constructor`'s relevant fields occupies in its family's heap type.
    pub(super) fn constructor_slots(
        &mut self,
        module: &mut curios_cont::CpsModule,
        constructor: ConstructorId,
    ) -> Vec<usize> {
        let family = self.constructor_family(constructor);
        let tag = self.constructor_tag(constructor) as usize;
        self.row_layout(module, family).places[tag].clone()
    }

    /// The family a constructor belongs to.
    pub(super) fn constructor_family(&self, constructor: ConstructorId) -> FamilyId {
        self.source
            .constructor(constructor)
            .expect("live constructor")
            .family
    }

    /// The runtime tag of a constructor: its position within its family.
    pub(super) fn constructor_tag(&self, constructor: ConstructorId) -> u32 {
        let family = self
            .source
            .constructor(constructor)
            .expect("live constructor")
            .family;
        self.source
            .family(family)
            .expect("live family")
            .constructors
            .iter()
            .position(|&candidate| candidate == constructor)
            .expect("a constructor belongs to its family") as u32
    }
}
