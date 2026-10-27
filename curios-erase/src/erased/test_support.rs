//! What a suite reads of an erased unit and no judgment does: each family its items declare, as the declaring item erased it.
//!
//! Compiled for this crate's tests and behind the `test-support` feature, so no build that ships holds it.

use {
    super::{ErasedForms, ErasedUnit},
    crate::test_support::{ErasedFamily, ErasedField},
};

impl ErasedUnit {
    /// [`declared_families`](crate::test_support::declared_families)' answer: each family read off the fragment of the item that declares it, a field at a family naming it.
    pub fn declared_families(&self) -> Vec<ErasedFamily> {
        self.items()
            .flat_map(ErasedForms::forms)
            .flat_map(|form| {
                let module = form.module();

                form.environment().declared_induct_rows().map(move |row| {
                    let family = module
                        .family(row.family)
                        .expect("a declared row's family is its fragment's");

                    ErasedFamily {
                        name: family.debug_name.clone().unwrap_or_default(),
                        constructors: family
                            .constructors
                            .iter()
                            .map(|&id| {
                                module
                                    .constructor(id)
                                    .expect("a declared row's constructor is its fragment's")
                                    .fields
                                    .iter()
                                    .map(|field| ErasedField {
                                        shape: field.shape,
                                        family: match field.shape {
                                            curios_anf::FieldShape::Family(family) => module
                                                .family(family)
                                                .and_then(|family| family.debug_name.clone()),
                                            _ => None,
                                        },
                                    })
                                    .collect()
                            })
                            .collect(),
                    }
                })
            })
            .collect()
    }
}
