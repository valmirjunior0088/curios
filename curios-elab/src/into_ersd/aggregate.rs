//! Products, structs, variants, and projections — schema registration and the construction/projection sites.
//!
//! A declaration's post-erasure shape is registered once, by the item that declares it, from the opaque signature view: parameters opened as fresh abstract variables, each field classified against the preceding binders. Every other item imports what it reads of the row, off that item's erased form (the order a module's items are stored in puts it first), which a program resolves to the declaration's own: a structure's schema at its width, and an inductive's family with each constructor the item names, at its position and width. Neither says anything of a field, and a mask is read where the declaring item holds it, so an item holds of a row what it names of it. The mask is therefore fixed per declaration, so construction, matching, and projection agree on the relevant-field arithmetic at every instantiation; a kept slot whose *instantiation* is a proof is filled with the unit constant at the site instead ([`Lowering::kept_operand`]).
//!
//! Collapses: a structure or subset tuple left with a single relevant field erases to that bare field (no product, no projection); anonymous tuples share one interned schema per relevant width.

use {
    super::{
        ConstructorRow, Error, FamilyRow, Lowering, Outcome, ProductRow, Proj, Relevance, Retyping,
        Struct, StructType, Subterm, Telescope, Term, Tuple, TupleType, Variant,
        constructor_entries, emitted,
    },
    curios_core::{Bound, Quality},
    std::borrow::Cow,
};

/// A structure's row as erasure reads it: its schema, and its mask, borrowed from the item that declares the row where another item does.
pub(super) struct StructRow<'s> {
    pub(super) schema: Option<curios_ersd::ProductId>,
    pub(super) mask: Cow<'s, [bool]>,
}

impl StructRow<'_> {
    /// The runtime projection index of declared field `index`: the count of relevant fields before it.
    fn relevant_before(&self, index: usize) -> u32 {
        self.mask[..index].iter().filter(|&&erased| !erased).count() as u32
    }
}

/// Open a parameter telescope with fresh assumed variables, handing back the abstract parameter terms a declaration's fields are instantiated at.
fn open_opaque<B: Bound>(context: &mut Retyping, telescope: Telescope<B>) -> Vec<Term> {
    let mut cursor = telescope.cursor();
    while let Some((_, type_)) = cursor.entry() {
        context.advance_assumed(&mut cursor, &type_);
    }
    cursor.into_args()
}

impl<'s> Lowering<'s> {
    /// The memoized layout of a registered structure.
    pub(super) fn struct_row(
        &mut self,
        context: &mut Retyping,
        name: &curios_core::Global,
    ) -> Result<StructRow<'s>, Error> {
        if let Some(row) = self.environment.declared_struct_row(name) {
            return Ok(StructRow {
                schema: row.schema,
                mask: Cow::Owned(row.mask.clone()),
            });
        }
        if let Some(row) = self.imported_struct_row(name) {
            return Ok(row);
        }
        let struct_decl = context
            .struct_decl(name)
            .cloned()
            .expect("erase: a registered struct");
        self.in_flight.insert(*name);
        let entries = context.with_frame(|context| {
            let params = open_opaque(context, struct_decl.arity.clone());
            constructor_entries(self, context, struct_decl.fields_at(&params))
        });
        self.in_flight.remove(name);
        let entries = entries?;
        let mask: Vec<bool> = entries.iter().map(|(_, erased, _)| *erased).collect();
        let relevant: Vec<curios_ersd::Field> = entries
            .into_iter()
            .filter(|(_, erased, _)| !erased)
            .map(|(label, _, shape)| curios_ersd::Field {
                debug_name: label,
                shape,
            })
            .collect();
        // A single relevant field is a newtype: the bare field, no schema.
        let schema = (!ProductRow::collapses(relevant.len())).then(|| {
            self.builder.product(curios_ersd::ProductSchema {
                debug_name: Some(name.faithful().to_string()),
                fields: curios_ersd::ProductFields::Declared(relevant),
            })
        });
        let row = StructRow {
            schema,
            mask: Cow::Owned(mask.clone()),
        };
        self.environment
            .register_struct_row(name, ProductRow { schema, mask });
        Ok(row)
    }

    /// The family `name`'s row is erased into: a read of the family alone, which imports none of its constructors where another item declares it, and registers the row where this one does.
    pub(super) fn induct_family(
        &mut self,
        context: &mut Retyping,
        name: &curios_core::Global,
    ) -> Result<curios_ersd::FamilyId, Error> {
        if let Some(row) = self.environment.declared_induct_row(name) {
            return Ok(row.family);
        }
        if let Some(family) = self.imported_induct_family(name) {
            return Ok(family);
        }
        self.build_induct_row(context, name)?;

        Ok(self.declared_induct_row(name).family)
    }

    /// The row of `name`'s constructor at `index`: a read of the one constructor, which imports it alone where another item declares the family, and registers the row where this one does.
    pub(super) fn induct_constructor(
        &mut self,
        context: &mut Retyping,
        name: &curios_core::Global,
        index: usize,
    ) -> Result<ConstructorRow, Error> {
        if let Some(row) = self.environment.declared_induct_row(name) {
            return Ok(row.constructors[index].clone());
        }
        if let Some(row) = self.imported_induct_constructor(name, index) {
            return Ok(row);
        }
        self.build_induct_row(context, name)?;

        Ok(self.declared_induct_row(name).constructors[index].clone())
    }

    /// What the declaration of the structure `name` says a value of it is, read off its mask where its row is registered: by this item, or by the item declaring it, read there and imported nothing of. Registered here where this item declares it and has not yet; `None` while that registration is in flight.
    pub(super) fn struct_relevance(
        &mut self,
        context: &mut Retyping,
        name: &curios_core::Global,
    ) -> Result<Option<Relevance>, Error> {
        if self.in_flight(name) {
            return Ok(None);
        }
        if let Some(row) = self.environment.declared_struct_row(name) {
            return Ok(Some(Relevance::of(&row.mask)));
        }
        if let Some(row) = self
            .declaring(name)
            .and_then(|declaring| declaring.environment().declared_struct_row(name))
        {
            return Ok(Some(Relevance::of(&row.mask)));
        }

        Ok(Some(Relevance::of(&self.struct_row(context, name)?.mask)))
    }

    /// [`struct_relevance`](Self::struct_relevance) for the one constructor of the inductive `name`.
    pub(super) fn constructor_relevance(
        &mut self,
        context: &mut Retyping,
        name: &curios_core::Global,
    ) -> Result<Option<Relevance>, Error> {
        if self.pending_families.contains_key(name) {
            return Ok(None);
        }
        if let Some(row) = self.environment.declared_induct_row(name) {
            return Ok(Some(Relevance::of(&row.constructors[0].mask)));
        }
        if let Some(row) = self
            .declaring(name)
            .and_then(|declaring| declaring.environment().declared_induct_row(name))
        {
            return Ok(Some(Relevance::of(&row.constructors[0].mask)));
        }

        Ok(Some(Relevance::of(
            &self.induct_constructor(context, name, 0)?.mask,
        )))
    }

    /// The row this item has just registered for `name`.
    fn declared_induct_row(&self, name: &curios_core::Global) -> &FamilyRow {
        self.environment
            .declared_induct_row(name)
            .expect("erase: a registered row")
    }

    /// Build and register `name`'s row from its declaration, each constructor read where the walk over them stands.
    fn build_induct_row(
        &mut self,
        context: &mut Retyping,
        name: &curios_core::Global,
    ) -> Result<(), Error> {
        let induct_decl = context
            .induct_decl(name)
            .cloned()
            .expect("erase: a registered inductive");
        let family = self.builder.family(Some(name.faithful().to_string()));
        // Registered before the constructor walk, not after: classifying a field of this very inductive is the ordinary case, not an edge one.
        self.pending_families.insert(*name, family);
        // A `Prop`-sorted family is proof-irrelevant, so erasure drops its inhabitants wholesale — payloads included. Classifying each payload on its own type would keep a `Type`-sorted one (`Eq`'s `refl(@z : A)` has an abstract `A`, which is neither prop nor universe), leaving a live field inside an erased proof: rebuilding the constructor would then compute that field from binders the same erasure had dropped. `Prop` structures already guarantee this by declaration — their fields must be non-informative — so this aligns inductives with them.
        let proof_family = matches!(
            &*context.reduce_with(&induct_decl.result_sort)?,
            Subterm::Sort(Quality::Prop, _)
        );
        let mut constructors = Vec::new();
        // In declaration order, which is runtime-tag order, each constructor read where the walk stands rather than looked up by its tag again.
        for (tag, param) in &induct_decl.constructors {
            let entries = context.with_frame(|context| {
                let params = open_opaque(context, induct_decl.arity.clone());
                let telescope = param.telescope.clone().open_params(&params);
                constructor_entries(self, context, telescope)
            })?;
            let entries: Vec<(Option<String>, bool, curios_ersd::FieldShape)> = entries
                .into_iter()
                .map(|(label, erased, shape)| (label, erased || proof_family, shape))
                .collect();
            let mask: Vec<bool> = entries.iter().map(|(_, erased, _)| *erased).collect();
            let relevant: Vec<curios_ersd::Field> = entries
                .into_iter()
                .filter(|(_, erased, _)| !erased)
                .map(|(label, _, shape)| curios_ersd::Field {
                    debug_name: label,
                    shape,
                })
                .collect();
            let id = self
                .builder
                .constructor(family, Some(tag.to_string()), relevant);
            constructors.push(ConstructorRow { id, mask });
        }
        let row = FamilyRow {
            family,
            constructors,
        };
        self.environment.register_induct_row(name, row);
        Ok(())
    }

    /// The row the item declaring the structure `name` registered, as this item reads it: its schema imported at its width, the first time it is read, and its mask borrowed where that item holds it. `None` where no item in reach declares the row, which is this item declaring it.
    fn imported_struct_row(&mut self, name: &curios_core::Global) -> Option<StructRow<'s>> {
        let declaring = self.declaring(name)?;
        let declared = declaring.environment().declared_struct_row(name)?;
        let schema = match self.environment.imported_struct_row(name) {
            Some(schema) => schema,
            None => {
                let schema = declared.schema.map(|schema| {
                    let held = declaring
                        .module()
                        .product(schema)
                        .expect("a declared row's schema is its fragment's");
                    self.builder
                        .import_product(held.debug_name.clone(), held.width())
                });
                self.environment.import_struct_row(name, schema);
                schema
            }
        };

        Some(StructRow {
            schema,
            mask: Cow::Borrowed(&declared.mask),
        })
    }

    /// The family of the inductive `name` as this item imports it, of as many constructors as its declaration has, imported the first time it is read. `None` where no item in reach declares the row, which is this item declaring it.
    fn imported_induct_family(
        &mut self,
        name: &curios_core::Global,
    ) -> Option<curios_ersd::FamilyId> {
        if let Some(row) = self.environment.imported_induct_row(name) {
            return Some(row.family);
        }
        let declaring = self.declaring(name)?;
        let declared = declaring.environment().declared_induct_row(name)?;
        let family = self.builder.import_family(
            declaring
                .module()
                .family(declared.family)
                .expect("a declared row's family is its fragment's")
                .debug_name
                .clone(),
            declared.constructors.len(),
        );
        self.environment.import_induct_family(name, family);

        Some(family)
    }

    /// The constructor at `index` of the inductive `name` as this item imports it: at its position and width, imported the first time it is read, under the declaration's mask.
    fn imported_induct_constructor(
        &mut self,
        name: &curios_core::Global,
        index: usize,
    ) -> Option<ConstructorRow> {
        let family = self.imported_induct_family(name)?;
        let declaring = self.declaring(name)?;
        let declared = &declaring
            .environment()
            .declared_induct_row(name)?
            .constructors[index];
        let imported = self
            .environment
            .imported_induct_row(name)
            .and_then(|row| row.constructors.get(&index).copied());
        let id = match imported {
            Some(id) => id,
            None => {
                let held = declaring
                    .module()
                    .constructor(declared.id)
                    .expect("a declared row's constructor is its fragment's");
                let id = self.builder.import_constructor(
                    family,
                    index,
                    held.debug_name.clone(),
                    held.width(),
                );
                self.environment.import_induct_constructor(name, index, id);
                id
            }
        };

        Some(ConstructorRow {
            id,
            mask: declared.mask.clone(),
        })
    }

    /// Whether `name`'s structure row is being registered right now — see [`Lowering::in_flight`].
    pub(super) fn in_flight(&self, name: &curios_core::Global) -> bool {
        self.in_flight.contains(name)
    }

    /// The family identity of a registered inductive, minting its row if this is the first reference.
    pub(super) fn family_identity(
        &mut self,
        context: &mut Retyping,
        name: &curios_core::Global,
    ) -> Result<curios_ersd::FamilyId, Error> {
        if let Some(&family) = self.pending_families.get(name) {
            return Ok(family);
        }
        self.induct_family(context, name)
    }

    /// The interned anonymous product schema of the given relevant width.
    pub(super) fn tuple_schema(&mut self, width: usize) -> curios_ersd::ProductId {
        if let Some(schema) = self.environment.tuple_schema(width) {
            return schema;
        }
        // An anonymous tuple's row is written with no type information at hand, so it states its width and no shape.
        let schema = self.builder.product(curios_ersd::ProductSchema {
            debug_name: None,
            fields: curios_ersd::ProductFields::Shared(width),
        });
        self.environment.register_tuple_schema(width, schema);
        schema
    }

    /// Lower a tuple against its checked tuple type. Every field has its slot, a proof or a type there as the stand-in, and a tuple of one field is that field: its width is its telescope's length, which [`erase_proj`](Self::erase_proj) reads back.
    pub(super) fn erase_tuple(
        &mut self,
        context: &mut Retyping,
        tuple: &Tuple,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let telescope = match Term::unwrap_or_clone(context.reduce_with(expected)?) {
            Subterm::TupleType(TupleType { telescope }) => telescope,
            _ => unreachable!("erase: tuple checked against non-tuple type"),
        };
        assert_eq!(
            tuple.fields.len(),
            telescope.len(),
            "erase: tuple width disagrees with the tuple type",
        );

        // An anonymous tuple has no declaration, and the type it is built at is one of the several it is read at: a field one reads as a proof or a type, another reads as a variable. So no field is dropped, and construction and projection agree on the one thing every reading has, the telescope's length.
        let atoms = match self.slots(context, telescope, &tuple.fields)? {
            Ok(atoms) => atoms,
            Err(diverged) => return Ok(diverged),
        };

        if ProductRow::collapses(atoms.len()) {
            return Ok(Outcome::Emitted(
                atoms.into_iter().next().expect("one field"),
            ));
        }
        let schema = self.tuple_schema(atoms.len());
        Ok(self.bind(
            hint,
            curios_ersd::Rhs::Product {
                schema,
                fields: atoms,
            },
        ))
    }

    /// Lower a struct value: a newtype is its bare field; anything else is a product over the declaration's schema.
    pub(super) fn erase_struct(
        &mut self,
        context: &mut Retyping,
        value: &Struct,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let row = self.struct_row(context, &value.name)?;
        let struct_decl = context
            .struct_decl(&value.name)
            .cloned()
            .expect("erase: a registered struct");
        let struct_decl = context.instantiate_struct_decl_at(&struct_decl, &value.universes)?;
        let telescope = struct_decl.fields_at(&value.params);
        let atoms = match self.masked_fields(context, &row.mask, telescope, &value.fields)? {
            Ok(atoms) => atoms,
            Err(diverged) => return Ok(diverged),
        };
        match row.schema {
            None => Ok(Outcome::Emitted(
                atoms.into_iter().next().expect("a newtype's one field"),
            )),
            Some(schema) => Ok(self.bind(
                hint,
                curios_ersd::Rhs::Product {
                    schema,
                    fields: atoms,
                },
            )),
        }
    }

    /// Lower a constructor value to a schema-carrying `Construct`; the discriminant is the constructor's registry position, which the declaring item's family lists it at and an importing item's import states.
    pub(super) fn erase_variant(
        &mut self,
        context: &mut Retyping,
        variant: &Variant,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let index = context
            .induct_decl(&variant.name)
            .expect("erase: a registered inductive")
            .constructor_index(&variant.tag)
            .expect("erase: constructor tag registered with its inductive");
        let constructor = self.induct_constructor(context, &variant.name, index)?;
        // The one constructor this value is built with, instantiated alone: a family of many cases is erased a construction at a time in its constructor's size.
        let telescope = context
            .induct_facet(&variant.name, &variant.universes, |at| {
                at.constructor(&variant.tag)
                    .expect("erase: constructor tag registered with its inductive")
            })
            .expect("erase: a registered inductive")?
            .telescope
            .open_params(&variant.params);
        let mask = constructor.mask.clone();
        let id = constructor.id;
        let atoms = match self.masked_fields(context, &mask, telescope, &variant.payload)? {
            Ok(atoms) => atoms,
            Err(diverged) => return Ok(diverged),
        };
        Ok(self.bind(
            hint,
            curios_ersd::Rhs::Construct {
                constructor: id,
                fields: atoms,
            },
        ))
    }

    /// Lower a projection: by relevant-field arithmetic out of a structure, whose declaration says which fields it keeps, and by position out of an anonymous tuple, which keeps them all. A head left with a single field already *is* that field, so the projection vanishes.
    pub(super) fn erase_proj(
        &mut self,
        context: &mut Retyping,
        proj: &Proj,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let Proj { head, field } = proj;
        let Some(index) = field.slot() else {
            unreachable!("unresolved label projection reached erasure");
        };

        let head_type = self.type_of(context, head)?;
        let head_type = context.reduce_with(&head_type)?;

        // Projecting an *erased* field yields proof content only: the unit constant, never a runtime projection (the field has no slot).
        let erased_field = |mask: &[bool], index: usize| mask.get(index).copied().unwrap_or(false);

        // A tuple keeps every field, so its slot is the field's index; a structure's is the count of relevant fields before it.
        let (schema, field) = match &*head_type {
            Subterm::TupleType(TupleType { telescope }) => {
                let width = telescope.len();
                assert!(index < width, "erase: projection out of range");
                let schema = (!ProductRow::collapses(width)).then(|| self.tuple_schema(width));
                (schema, index as u32)
            }
            Subterm::StructType(StructType { name, .. }) => {
                let row = self.struct_row(context, name)?;
                assert!(index < row.mask.len(), "erase: projection out of range");
                if erased_field(&row.mask, index) {
                    return Ok(Outcome::Emitted(self.unit()));
                }
                (row.schema, row.relevant_before(index))
            }
            _ => unreachable!("erase: projected a non-tuple/struct"),
        };

        match schema {
            // The head collapsed to its single relevant field.
            None => self.walk(context, head, &head_type, hint),
            Some(schema) => {
                let product = emitted!(self.walk(context, head, &head_type, None)?);
                self.bind_stated(
                    context,
                    hint,
                    curios_ersd::Rhs::Project {
                        schema,
                        product,
                        field,
                    },
                    expected,
                )
            }
        }
    }
}
