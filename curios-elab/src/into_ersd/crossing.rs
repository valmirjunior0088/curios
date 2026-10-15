//! A host call's options: what crosses for one, and what the guest reads back.
//!
//! An option crosses as its element's list would, holding the one value `some` holds, so below this a call takes and answers a list where its row states an option; the conversion to and from `/sys/Option` is made here, beside the call, where the family is known.

use {
    super::{Lowering, suspension::Polled},
    curios_abi::WireType,
};

impl Lowering<'_> {
    /// The list `option` crosses as: none of a value for `none`, and the one `some` holds.
    pub(super) fn option_crossing(
        &mut self,
        option: curios_ersd::Atom,
        polled: Polled,
    ) -> curios_ersd::Atom {
        let value = self.builder.value(None);
        let held = self.block(|lowering| {
            lowering.let_atom(curios_ersd::Rhs::Sequence {
                operation: curios_ersd::SequenceOp::ListBuild,
                operands: vec![curios_ersd::Atom::Value(value)],
            })
        });
        let empty = self.block(|lowering| {
            lowering.let_atom(curios_ersd::Rhs::Sequence {
                operation: curios_ersd::SequenceOp::ListBuild,
                operands: Vec::new(),
            })
        });

        self.let_atom(curios_ersd::Rhs::MatchVariant {
            family: polled.family,
            scrutinee: option,
            arms: vec![curios_ersd::VariantArm {
                constructor: polled.some,
                bindings: vec![value],
                block: held,
            }],
            default: Some(empty),
        })
    }

    /// The option `list`, what one crossed as, reads back as: `none` for an empty list, and `some` of its first value otherwise.
    pub(super) fn option_crossed(
        &mut self,
        list: curios_ersd::Atom,
        polled: Polled,
    ) -> curios_ersd::Atom {
        let element = self.builder.value(None);
        let suffix = self.builder.value(None);
        let empty = self.block(|lowering| {
            lowering.let_atom(curios_ersd::Rhs::Construct {
                constructor: polled.none,
                fields: Vec::new(),
            })
        });
        let held = self.block(|lowering| {
            lowering.let_atom(curios_ersd::Rhs::Construct {
                constructor: polled.some,
                fields: vec![curios_ersd::Atom::Value(element)],
            })
        });

        self.let_atom(curios_ersd::Rhs::UnconsSequence {
            grain: curios_ersd::SequenceGrain::List,
            scrutinee: list,
            empty,
            cons: curios_ersd::UnconsSequenceStep {
                element,
                suffix,
                block: held,
            },
        })
    }

    /// `answer`, what a call of a row answering `results` binds, with each option among them read back from the list it crossed as: a lone result as itself, and a record's fields each in its place.
    pub(super) fn answered(
        &mut self,
        answer: curios_ersd::Atom,
        results: &[WireType],
        polled: Polled,
    ) -> curios_ersd::Atom {
        match results {
            [single] if single.admits_null() => self.option_crossed(answer, polled),
            fields if fields.len() > 1 && fields.iter().any(|wire| wire.admits_null()) => {
                let schema = self.tuple_schema(fields.len());
                let mut read = Vec::with_capacity(fields.len());

                for (index, wire) in fields.iter().enumerate() {
                    let field = self.let_atom(curios_ersd::Rhs::Project {
                        schema,
                        product: answer,
                        field: u32::try_from(index).expect("a row's results fit a field index"),
                    });

                    read.push(match wire.admits_null() {
                        true => self.option_crossed(field, polled),
                        false => field,
                    });
                }

                self.let_atom(curios_ersd::Rhs::Product {
                    schema,
                    fields: read,
                })
            }
            _ => answer,
        }
    }
}
