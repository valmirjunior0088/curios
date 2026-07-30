//! Products, structs, variants, and projections — schema registration and the construction/projection sites.
//!
//! Declarations register their post-erasure shape exactly once, lazily on first use (the dominance-ordered item chain guarantees their dependencies are defined by then), from the opaque signature view: parameters opened as fresh abstract variables, each field classified against the preceding binders. The mask is therefore fixed per declaration, so construction, matching, and projection agree on the relevant-field arithmetic at every instantiation; a kept slot whose *instantiation* is a proof is filled with the unit constant at the site instead ([`Lowering::kept_operand`]).
//!
//! Collapses: a structure or subset tuple left with a single relevant field erases to that bare field (no product, no projection); anonymous tuples share one interned schema per relevant width.

use super::{
    Context, Error, Field, Lowering, Outcome, Proj, Struct, StructType, Subterm, Telescope, Term,
    Tuple, TupleType, Variant, emitted, erasure_mask, reduce_with, signature_entries,
};

/// Open a parameter telescope with fresh assumed variables, handing back the abstract parameter terms a declaration's fields are instantiated at.
fn open_opaque(context: &mut Context, mut telescope: Telescope<()>) -> Vec<Term> {
    let mut params = Vec::new();
    loop {
        match telescope {
            Telescope::Cons(type_, rest) => {
                let name = context.fresh(rest.first_hint());
                context.assume(&name, &type_);
                let variable = Term::free_var(&name);
                telescope = rest.open(&[&variable]);
                params.push(variable);
            }
            Telescope::Done(_) => break params,
        }
    }
}

impl Lowering {
    /// The memoized layout of a registered structure.
    pub(super) fn struct_row(
        &mut self,
        context: &mut Context,
        name: &curios_core::Global,
    ) -> Result<super::ProductRow, Error> {
        if let Some(row) = self.environment.struct_row(name) {
            return Ok(row.clone());
        }
        let struct_decl = context
            .struct_decl(name)
            .cloned()
            .expect("erase: a registered struct");
        let entries = context.with_frame(|context| {
            let params = open_opaque(context, struct_decl.params.clone());
            signature_entries(context, struct_decl.fields_at(&params))
        })?;
        let mask: Vec<bool> = entries.iter().map(|(_, erased)| *erased).collect();
        let relevant: Vec<Option<String>> = entries
            .into_iter()
            .filter(|(_, erased)| !erased)
            .map(|(label, _)| label)
            .collect();
        // A single relevant field is a newtype: the bare field, no schema.
        let schema = (relevant.len() != 1).then(|| {
            self.builder.product(curios_ersd::ProductSchema {
                debug_name: Some(name.to_string()),
                fields: relevant,
            })
        });
        let row = super::ProductRow { schema, mask };
        self.environment.register_struct_row(name, row.clone());
        Ok(row)
    }

    /// The memoized layout of a registered inductive.
    pub(super) fn induct_row(
        &mut self,
        context: &mut Context,
        name: &curios_core::Global,
    ) -> Result<super::FamilyRow, Error> {
        if let Some(row) = self.environment.induct_row(name) {
            return Ok(row.clone());
        }
        let induct_decl = context
            .induct_decl(name)
            .cloned()
            .expect("erase: a registered inductive");
        let family = self.builder.family(Some(name.to_string()));
        // A `Prop`-sorted family is proof-irrelevant, so erasure drops its inhabitants wholesale — payloads included. Classifying each payload on its own type would keep a `Type`-sorted one (`Eq`'s `refl(@z : A)` has an abstract `A`, which is neither prop nor universe), leaving a live field inside an erased proof: rebuilding the constructor would then compute that field from binders the same erasure had dropped. `Prop` structures already guarantee this by declaration — their fields must be non-informative — so this aligns inductives with them.
        let proof_family = matches!(
            &*reduce_with(context, &induct_decl.result_sort)?,
            Subterm::Prop
        );
        let mut constructors = Vec::new();
        for tag in induct_decl.constructor_order() {
            let entries = context.with_frame(|context| {
                let params = open_opaque(context, induct_decl.params.clone());
                let telescope = induct_decl
                    .instantiate(tag, &params)
                    .expect("erase: constructor instantiates at its inductive's parameters");
                signature_entries(context, telescope)
            })?;
            let entries: Vec<(Option<String>, bool)> = entries
                .into_iter()
                .map(|(label, erased)| (label, erased || proof_family))
                .collect();
            let mask: Vec<bool> = entries.iter().map(|(_, erased)| *erased).collect();
            let relevant: Vec<Option<String>> = entries
                .into_iter()
                .filter(|(_, erased)| !erased)
                .map(|(label, _)| label)
                .collect();
            let id = self
                .builder
                .constructor(family, Some(tag.to_string()), relevant);
            constructors.push(super::ConstructorRow { id, mask });
        }
        let row = super::FamilyRow {
            family,
            constructors,
        };
        self.environment.register_induct_row(name, row.clone());
        Ok(row)
    }

    /// The interned anonymous product schema of the given relevant width.
    fn tuple_schema(&mut self, width: usize) -> curios_ersd::ProductId {
        if let Some(schema) = self.environment.tuple_schema(width) {
            return schema;
        }
        let schema = self.builder.product(curios_ersd::ProductSchema {
            debug_name: None,
            fields: vec![None; width],
        });
        self.environment.register_tuple_schema(width, schema);
        schema
    }

    /// Lower a tuple against its checked tuple type. Erasable fields drop; a type left with one relevant field collapses to it, whether or not a drop produced that width — the same relevant-arity rule [`struct_row`] applies, and the rule [`erase_proj`] reads back.
    ///
    /// [`struct_row`]: Self::struct_row [`erase_proj`]: Self::erase_proj
    pub(super) fn erase_tuple(
        &mut self,
        context: &mut Context,
        tuple: &Tuple,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let telescope = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
            Subterm::TupleType(TupleType { telescope }) => telescope,
            _ => unreachable!("erase: tuple checked against non-tuple type"),
        };
        assert_eq!(
            tuple.fields.len(),
            telescope.len(),
            "erase: tuple width disagrees with the tuple type",
        );

        // Anonymous tuples have no declaration; the per-site signature mask is the layout, and construction and projection read the same concrete tuple type, so they agree.
        let mask = erasure_mask(context, telescope.clone())?;
        let atoms = match self.masked_fields(context, &mask, telescope, &tuple.fields)? {
            Ok(atoms) => atoms,
            Err(diverged) => return Ok(diverged),
        };

        if atoms.len() == 1 {
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
        context: &mut Context,
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

    /// Lower a constructor value to a schema-carrying `Construct`; the discriminant is the constructor's registry position, recorded in the registered family.
    pub(super) fn erase_variant(
        &mut self,
        context: &mut Context,
        variant: &Variant,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let row = self.induct_row(context, &variant.name)?;
        let induct_decl = context
            .induct_decl(&variant.name)
            .cloned()
            .expect("erase: a registered inductive");
        let induct_decl = context.instantiate_induct_decl_at(&induct_decl, &variant.universes)?;
        let index = induct_decl
            .constructor_index(&variant.tag)
            .expect("erase: constructor tag registered with its inductive");
        let constructor = &row.constructors[index];
        let telescope = induct_decl
            .instantiate(&variant.tag, &variant.params)
            .expect("erase: constructor instantiates at its inductive's parameters");
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

    /// Lower a projection by relevant-field arithmetic. A head left with a single relevant field already *is* that field, so the projection vanishes.
    pub(super) fn erase_proj(
        &mut self,
        context: &mut Context,
        proj: &Proj,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let Proj { head, field } = proj;
        let Field::Index(index) = field else {
            unreachable!("unresolved label projection reached erasure");
        };

        let head_type = super::infer(context, head)?;
        let head_type = reduce_with(context, &head_type)?;

        // Projecting an *erased* field yields proof content only: the unit constant, never a runtime projection (the field has no slot).
        let erased_field = |mask: &[bool], index: usize| mask.get(index).copied().unwrap_or(false);

        let (row, width) = match &*head_type {
            Subterm::TupleType(TupleType { telescope }) => {
                let mask = erasure_mask(context, telescope.clone())?;
                let relevant = mask.iter().filter(|&&erased| !erased).count();
                let schema = (relevant != 1).then(|| self.tuple_schema(relevant));
                (super::ProductRow { schema, mask }, telescope.len())
            }
            Subterm::StructType(StructType { name, .. }) => {
                let row = self.struct_row(context, name)?;
                let width = row.mask.len();
                (row, width)
            }
            _ => unreachable!("erase: projected a non-tuple/struct"),
        };
        assert!(*index < width, "erase: projection out of range");

        if erased_field(&row.mask, *index) {
            return Ok(Outcome::Emitted(self.unit()));
        }

        match row.schema {
            // The head collapsed to its single relevant field.
            None => self.walk(context, head, &head_type, hint),
            Some(schema) => {
                let product = emitted!(self.walk(context, head, &head_type, None)?);
                Ok(self.bind(
                    hint,
                    curios_ersd::Rhs::Project {
                        schema,
                        product,
                        field: row.relevant_before(*index),
                    },
                ))
            }
        }
    }
}
