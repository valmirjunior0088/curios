//! What elaboration starts from: modules already elaborated, seeded into a fresh [`Context`].
//!
//! Elaboration of a compilation unit happens against everything already in scope, and that scope is not a *part* of the unit — it is a set of finished modules whose items are replayed rather than re-checked. This names it, so a stage that used to take a bare `Option<&Module>` and re-seed a context by hand takes a thing that says what it is.
//!
//! # Why a type rather than a parameter
//!
//! The seeding is four separable jobs — recorded totality, the nominal registries, the definitions themselves, and the witness table — and every one of them was written inline in the middle of a two-hundred-line function. Any future question of the form "what does an environment give elaboration?" was answered by reading that function and noticing which blocks were guarded by `if let Some(prefix)`.
//!
//! It is also what lets the answer stop being *one* module. A cached-modules design seeds from N of them, and with the shape below that is a different constructor rather than a different signature through every caller. Today N is one or zero, and the empty case is a from-scratch elaboration: `Established::nothing()` degenerates every step to the whole-module reading, so there is no second implementation to keep in agreement.

use {
    super::{Context, Error, recorded_totality, register_witness},
    curios_core::{Free, Item, Module, Term},
};

/// The modules already elaborated, as scope for elaborating another.
#[derive(Default, Clone, Copy)]
pub struct Established<'a> {
    /// The units already elaborated, in dependency order. Empty for a from-scratch elaboration, where the unit defines every name it mentions.
    ///
    /// Borrowed per unit rather than merged into one module: merging would copy every predecessor's items into every compilation, which is the cost retiring the splice removed.
    modules: &'a [&'a Module],
}

impl<'a> Established<'a> {
    /// Nothing in scope: the unit is elaborated whole, against itself alone.
    pub fn nothing() -> Self {
        Self { modules: &[] }
    }

    /// Everything `modules` established, in dependency order, as scope.
    pub fn over(modules: &'a [&'a Module]) -> Self {
        Self { modules }
    }

    /// The nominal registries and the recorded totality verdicts, before any item is checked.
    ///
    /// An inductive's type-constructor and value-constructor definitions reference their own registry entry, and `elaborate_struct`/`elaborate_proj` consult the struct registry — so these go in ahead of the unit's own. The totality flags were settled when this scope's archive was built, which is what lets a written type be refused against them before it reduces.
    pub(crate) fn seed_registries(&self, context: &mut Context) -> Result<(), Error> {
        for module in self.modules {
            // Before the registries, because a mount is what a privilege question is answered out of and the unit's own items may ask one while they elaborate. What this scope mounted is what it declares its prefixes to be — the elaborator never derives a mount from a name.
            context.mount(&module.mounts);
            context.seed_totality(&recorded_totality(module));
            for (name, induct_decl) in &module.induct_decls {
                context.register_induct(name, induct_decl.clone())?;
            }
            for (name, struct_decl) in &module.struct_decls {
                context.register_struct(name, struct_decl.clone())?;
            }
            for (name, concept) in &module.concepts {
                context.register_concept(name, concept.clone())?;
            }
        }

        Ok(())
    }

    /// The definitions themselves, into the persistent base frame.
    ///
    /// `define_assuming_scheme` reproduces exactly the state `elaborate_module_let`/`_rec` leave behind — assume the type, define the body — with no re-checking, because these terms are already elaborated. A witness additionally re-registers its already-elaborated signature, since the witness table is per-elaboration state and is not carried on a module.
    pub(crate) fn replay_definitions(&self, context: &mut Context) -> Result<(), Error> {
        for module in self.modules {
            for item in &module.items {
                match item {
                    Item::Let(def) => {
                        context.define_assuming_scheme(
                            &Free::from(&def.name),
                            &def.type_,
                            &def.body,
                            Some(&def.kind),
                            def.universe_context.clone(),
                        );
                        if module.witnesses.contains(&def.name) {
                            register_witness(
                                context,
                                &def.name,
                                &def.type_,
                                def.universe_context.clone(),
                                &def.island,
                            )?;
                        }
                    }
                    Item::Rec(rec) => {
                        for (index, definition) in rec.definitions.iter().enumerate() {
                            let name = Free::from(&definition.name);
                            context.assume(&name, &rec.group.member_type(index));
                            context.set_assumption_universe_context(
                                &name,
                                rec.group.universe_context().clone(),
                            );
                            context.define(
                                &name,
                                &Term::rec_proj(rec.group.clone(), index),
                                Some(&definition.kind),
                            );
                            // A witness that resolves through its own entry is lowered as a group of one, so the table has to be rebuilt from this arm as well — a replayed unit would otherwise hand its successors a scope in which that witness is undeclared.
                            if module.witnesses.contains(&definition.name) {
                                register_witness(
                                    context,
                                    &definition.name,
                                    &rec.group.member_type(index),
                                    rec.group.universe_context().clone(),
                                    &definition.island,
                                )?;
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// The highest binder floor any unit in scope carries.
    ///
    /// A bound rather than a verdict: the unit's own terms are elaborated against terms whose binders were minted by earlier walks, so its floor has to clear every one of them. Combining by maximum can only ever widen, and widening cannot refuse a module that was fine.
    pub(crate) fn binder_floor(&self) -> usize {
        self.modules
            .iter()
            .map(|module| module.binder_floor)
            .max()
            .unwrap_or(0)
    }

    /// The totality verdicts this scope recorded, which the erasure obligations inherit rather than recompute.
    ///
    /// Flattened across every unit in scope. Names are disjoint by mount, so the order of the merge cannot decide a verdict.
    pub(crate) fn recorded_totality(
        &self,
    ) -> std::collections::BTreeMap<curios_core::Global, curios_core::Totality> {
        self.modules
            .iter()
            .flat_map(|module| recorded_totality(module))
            .collect()
    }
}
