//! Products, structs, variants, and projections — schema registration and the
//! construction/projection sites.
//!
//! Declarations register their post-erasure shape exactly once, lazily on
//! first use (the dominance-ordered item chain guarantees their dependencies
//! are defined by then), from the opaque signature view: parameters opened as
//! fresh abstract variables, each field classified against the preceding
//! binders. The mask is therefore fixed per declaration, so construction,
//! matching, and projection agree on the relevant-field arithmetic at every
//! instantiation; a kept slot whose *instantiation* is a proof is filled with
//! the unit constant at the site instead ([`Lowering::kept_operand`]).
//!
//! Collapses: a structure or subset tuple left with a single relevant field
//! erases to that bare field (no product, no projection); anonymous tuples
//! share one interned schema per relevant width.

use super::{
    Context, Error, Field, Lowering, Outcome, Proj, Struct, StructType, Subterm, Telescope, Term,
    Tuple, TupleType, Variant, emitted, is_erasable, reduce_with,
};

/// The opaque signature view of a telescope: one entry per binder — its label
/// and whether it is erased — classified with the preceding binders opened as
/// fresh abstract variables.
fn signature_entries<B: super::Bound>(
    context: &mut Context,
    mut telescope: Telescope<B>,
) -> Result<Vec<(Option<String>, bool)>, Error> {
    let mut entries = Vec::new();
    loop {
        match telescope {
            Telescope::Cons(type_, rest) => {
                let label = rest.first_label().map(str::to_string);
                let erasable = is_erasable(context, &type_)?;
                let variable = Term::free_var(context.fresh(label.as_deref()));
                entries.push((label, erasable));
                telescope = rest.open(&[&variable]);
            }
            Telescope::Done(_) => break Ok(entries),
        }
    }
}

/// Open a parameter telescope with fresh assumed variables, handing back the
/// abstract parameter terms a declaration's fields are instantiated at.
fn open_opaque(context: &mut Context, mut telescope: Telescope<()>) -> Vec<Term> {
    let mut params = Vec::new();
    loop {
        match telescope {
            Telescope::Cons(type_, rest) => {
                let name = context.fresh(rest.first_label());
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
    pub(super) fn structure_row(
        &mut self,
        context: &mut Context,
        name: &str,
    ) -> Result<super::ProductRow, Error> {
        if let Some(row) = self.environment.structure_row(name) {
            return Ok(row.clone());
        }
        let structure = context
            .structure(name)
            .cloned()
            .expect("erase_ir: a registered struct");
        let entries = context.with_frame(|context| {
            let params = open_opaque(context, structure.params.clone());
            signature_entries(context, structure.fields_at(&params))
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
        self.environment.register_structure_row(name, row.clone());
        Ok(row)
    }

    /// The memoized layout of a registered inductive.
    pub(super) fn inductive_row(
        &mut self,
        context: &mut Context,
        name: &str,
    ) -> Result<super::FamilyRow, Error> {
        if let Some(row) = self.environment.inductive_row(name) {
            return Ok(row.clone());
        }
        let inductive = context
            .inductive(name)
            .cloned()
            .expect("erase_ir: a registered inductive");
        let family = self.builder.family(Some(name.to_string()));
        let mut constructors = Vec::new();
        for tag in inductive.constructor_order() {
            let entries = context.with_frame(|context| {
                let params = open_opaque(context, inductive.params.clone());
                let telescope = inductive
                    .instantiate(tag, &params)
                    .expect("erase_ir: constructor instantiates at its inductive's parameters");
                signature_entries(context, telescope)
            })?;
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
        self.environment.register_inductive_row(name, row.clone());
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

    /// Erase each value against its telescope domain under `mask`, opening the
    /// telescope with the un-erased value so later dependent domains stay
    /// correct. Erasable slots are dropped entirely; kept slots erase through
    /// [`kept_operand`](Self::kept_operand).
    fn masked_fields<B: super::Bound>(
        &mut self,
        context: &mut Context,
        mask: &[bool],
        mut telescope: Telescope<B>,
        values: &[Term],
    ) -> Result<Result<Vec<curios_ersd::ErasedAtom>, Outcome>, Error> {
        let mut atoms = Vec::with_capacity(values.len());
        for (index, value) in values.iter().enumerate() {
            match telescope {
                Telescope::Cons(type_, rest) => {
                    if !mask[index] {
                        match self.kept_operand(context, value, &type_)? {
                            Outcome::Emitted(atom) => atoms.push(atom),
                            diverged => return Ok(Err(diverged)),
                        }
                    }
                    telescope = rest.open(&[value]);
                }
                Telescope::Done(_) => unreachable!("erase_ir: arity checked by elaborate"),
            }
        }
        Ok(Ok(atoms))
    }

    /// Lower a tuple against its checked tuple type. Erasable fields drop; a
    /// subset type left with one relevant field collapses to it (guarded on a
    /// drop having happened, so an ordinary one-field tuple keeps its
    /// product).
    pub(super) fn erase_tuple(
        &mut self,
        context: &mut Context,
        tuple: &Tuple,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let telescope = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
            Subterm::TupleType(TupleType { telescope }) => telescope,
            _ => unreachable!("erase_ir: tuple checked against non-tuple type"),
        };
        assert_eq!(
            tuple.fields.len(),
            telescope.len(),
            "erase_ir: tuple width disagrees with the tuple type",
        );

        // Anonymous tuples have no declaration; the per-site signature mask is
        // the layout, and construction and projection read the same concrete
        // tuple type, so they agree.
        let mask: Vec<bool> = signature_entries(context, telescope.clone())?
            .into_iter()
            .map(|(_, erased)| erased)
            .collect();
        let atoms = match self.masked_fields(context, &mask, telescope, &tuple.fields)? {
            Ok(atoms) => atoms,
            Err(diverged) => return Ok(diverged),
        };

        let dropped_any = atoms.len() != tuple.fields.len();
        if atoms.len() == 1 && dropped_any {
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

    /// Lower a struct value: a newtype is its bare field; anything else is a
    /// product over the declaration's schema.
    pub(super) fn erase_struct(
        &mut self,
        context: &mut Context,
        value: &Struct,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let row = self.structure_row(context, &value.name)?;
        let structure = context
            .structure(&value.name)
            .cloned()
            .expect("erase_ir: a registered struct");
        let telescope = structure.fields_at(&value.params);
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

    /// Lower a constructor value to a schema-carrying `Construct`; the
    /// discriminant is the constructor's registry position, recorded in the
    /// registered family.
    pub(super) fn erase_variant(
        &mut self,
        context: &mut Context,
        variant: &Variant,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let row = self.inductive_row(context, &variant.name)?;
        let inductive = context
            .inductive(&variant.name)
            .cloned()
            .expect("erase_ir: a registered inductive");
        let index = inductive
            .constructor_index(&variant.tag)
            .expect("erase_ir: constructor tag registered with its inductive");
        let constructor = &row.constructors[index];
        let telescope = inductive
            .instantiate(&variant.tag, &variant.params)
            .expect("erase_ir: constructor instantiates at its inductive's parameters");
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

    /// Lower a projection by relevant-field arithmetic. A head left with a
    /// single relevant field already *is* that field, so the projection
    /// vanishes.
    pub(super) fn erase_proj(
        &mut self,
        context: &mut Context,
        proj: &Proj,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let Proj { head, field } = proj;
        let Field::Index(index) = field else {
            unreachable!("unresolved label projection reached erase_ir");
        };

        let head_type = super::infer(context, head)?;
        let head_type = reduce_with(context, &head_type)?;

        // Projecting an *erased* field yields proof content only: the unit
        // constant, never a runtime projection (the field has no slot).
        let erased_field = |mask: &[bool], index: usize| mask.get(index).copied().unwrap_or(false);

        let (row, width) = match &*head_type {
            Subterm::TupleType(TupleType { telescope }) => {
                let mask: Vec<bool> = signature_entries(context, telescope.clone())?
                    .into_iter()
                    .map(|(_, erased)| erased)
                    .collect();
                let relevant = mask.iter().filter(|&&erased| !erased).count();
                let schema = (relevant != 1).then(|| self.tuple_schema(relevant));
                (super::ProductRow { schema, mask }, telescope.len())
            }
            Subterm::StructType(StructType { name, .. }) => {
                let row = self.structure_row(context, name)?;
                let width = row.mask.len();
                (row, width)
            }
            _ => unreachable!("erase_ir: projected a non-tuple/struct"),
        };
        assert!(*index < width, "erase_ir: projection out of range");

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
