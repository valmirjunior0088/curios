//! A host call's crossing: the options a row takes and answers, and the lane every row that returns carries.
//!
//! An option crosses as its element's list would, holding the one value `some` holds, so below this a call takes and answers a list where its row states an option; the conversion to and from `/sys/Option` is made here, beside the call, where the family is known.
//!
//! The lane is `curios_abi::LANE_IN` and `curios_abi::LANE_OUT`: a call is made with no token, and where it answers a token instead of a reply, its description suspends on the token's handle, asks again with the token once it is ready, and is cancelled by dropping the token through the same row. A call that asks again or drops sends fillers for the row's operands, which its answerer does not read once a token is given. The suspension itself is one shape every row shares ([`curios_anf::SuspensionOperation::Later`]), so all a row defines is its call.

use {
    super::{Lowering, Polled},
    crate::Error,
    curios_abi::WireType,
    curios_cert::Reader,
    curios_num::{Binary, Floating, Integer, Natural},
};

impl Lowering<'_> {
    /// How a value of `wire` crosses: as the option the row states, where the wire admits none, read at the instance `curios_core::wire_term` gives it; as itself otherwise.
    pub fn crossed(
        &mut self,
        context: &mut Reader,
        wire: &WireType,
    ) -> Result<Option<Polled>, Error> {
        if !wire.admits_null() {
            return Ok(None);
        }
        let syntax = context.syntax();

        self.polled(context, &curios_core::wire_term(wire, &syntax))
            .map(Some)
    }

    /// The list `option` crosses as: none of a value for `none`, and the one `some` holds.
    pub fn option_crossing(
        &mut self,
        option: curios_anf::Atom,
        polled: Polled,
    ) -> curios_anf::Atom {
        let value = self.builder.value(None);
        let held = self.block(|lowering| {
            lowering.let_atom(curios_anf::Rhs::Sequence {
                operation: curios_anf::SequenceOp::ListBuild,
                operands: vec![curios_anf::Atom::Value(value)],
            })
        });
        let empty = self.block(|lowering| lowering.empty_list());

        self.let_atom(curios_anf::Rhs::MatchVariant {
            family: polled.family,
            scrutinee: option,
            arms: vec![curios_anf::VariantArm {
                constructor: polled.some,
                bindings: vec![value],
                block: held,
            }],
            default: Some(empty),
        })
    }

    /// The option `list`, what one crossed as, reads back as: `none` for an empty list, and `some` of its first value otherwise.
    fn option_crossed(&mut self, list: curios_anf::Atom, polled: Polled) -> curios_anf::Atom {
        let element = self.builder.value(None);
        let suffix = self.builder.value(None);
        let empty = self.block(|lowering| {
            lowering.let_atom(curios_anf::Rhs::Construct {
                constructor: polled.none,
                fields: Vec::new(),
            })
        });
        let held = self.block(|lowering| {
            lowering.let_atom(curios_anf::Rhs::Construct {
                constructor: polled.some,
                fields: vec![curios_anf::Atom::Value(element)],
            })
        });

        self.let_atom(curios_anf::Rhs::UnconsSequence {
            grain: curios_anf::SequenceGrain::List,
            scrutinee: list,
            empty,
            cons: curios_anf::UnconsSequenceStep {
                element,
                suffix,
                block: held,
            },
        })
    }

    fn empty_list(&mut self) -> curios_anf::Atom {
        self.let_atom(curios_anf::Rhs::Sequence {
            operation: curios_anf::SequenceOp::ListBuild,
            operands: Vec::new(),
        })
    }

    /// Intern `constant` and hand back its atom.
    fn constant_atom(&mut self, constant: curios_anf::Constant) -> curios_anf::Atom {
        curios_anf::Atom::Constant(self.builder.constant(constant))
    }

    /// A value of `wire` standing where an operand's answerer reads none: the call that asks again or drops, which names its call by the token alone.
    fn filler(&mut self, wire: WireType) -> curios_anf::Atom {
        match wire {
            WireType::Nat => self.constant_atom(curios_anf::Constant::Nat(Natural::zero())),
            WireType::Int => self.constant_atom(curios_anf::Constant::Int(Integer::from(0u8))),
            WireType::Bool => self.constant_atom(curios_anf::Constant::Bool(false)),
            WireType::Byte => self.constant_atom(curios_anf::Constant::Byte(0)),
            WireType::Flt => self.constant_atom(curios_anf::Constant::Flt(Floating::zero(false))),
            WireType::Bytes => self.constant_atom(curios_anf::Constant::Bytes(Binary::empty())),
            WireType::Handle => self.constant_atom(curios_anf::Constant::Handle(0)),
            WireType::List(_) | WireType::Option(_) => self.empty_list(),
        }
    }

    /// The performance of a call of the row `foreign`, which returns: the description that calls it over `operands` with no token, answering what it answers, or where it answers a token, waiting for it.
    ///
    /// Two functions are defined beside the call. `step(operands, token, dropped)` is the description that calls the row with the lane `(token, dropped)`, and answers the row's answer, its options read back, where the call answers no token; where it answers one, the suspension [`curios_anf::SuspensionOperation::Later`] builds on it, which every row shares. `again(token, dropped)` is `step` over fillers, since a call that names its token is not read for its operands.
    pub fn lane_call(
        &mut self,
        foreign: curios_anf::ForeignId,
        operands: Vec<curios_anf::Atom>,
        params: &[WireType],
        results: &[WireType],
        crossed: &[Option<Polled>],
        name: &str,
    ) -> curios_anf::Atom {
        let step = self.builder.reserve_function();
        let again = self.builder.reserve_function();

        let parameters = params
            .iter()
            .map(|_| self.builder.value(None))
            .collect::<Vec<_>>();
        let token = self.builder.value(None);
        let dropped = self.builder.value(None);
        let body = self.block(|lowering| {
            lowering.description(name, |lowering| {
                let arguments = parameters
                    .iter()
                    .chain([&token, &dropped])
                    .map(|&value| curios_anf::Atom::Value(value))
                    .collect();
                let reply = lowering.let_atom(curios_anf::Rhs::Foreign {
                    foreign,
                    operands: arguments,
                });
                let lane = match results.len() {
                    0 => reply,
                    width => lowering.field(reply, width + 1, width),
                };
                let element = lowering.builder.value(None);
                let suffix = lowering.builder.value(None);
                let answered =
                    lowering.block(|lowering| lowering.answered(reply, results, crossed));
                let pending = lowering.block(|lowering| {
                    lowering.let_atom(curios_anf::Rhs::Suspension {
                        operation: curios_anf::SuspensionOperation::Later,
                        operands: vec![lane, curios_anf::Atom::Function(again)],
                    })
                });

                lowering.let_atom(curios_anf::Rhs::UnconsSequence {
                    grain: curios_anf::SequenceGrain::List,
                    scrutinee: lane,
                    empty: answered,
                    cons: curios_anf::UnconsSequenceStep {
                        element,
                        suffix,
                        block: pending,
                    },
                })
            })
        });
        self.builder.define_function(
            step,
            Some(format!("{name}/step")),
            parameters.into_iter().chain([token, dropped]).collect(),
            body,
        );

        let token = self.builder.value(None);
        let dropped = self.builder.value(None);
        let body = self.block(|lowering| {
            let fillers = params
                .iter()
                .map(|&wire| lowering.filler(wire))
                .chain([
                    curios_anf::Atom::Value(token),
                    curios_anf::Atom::Value(dropped),
                ])
                .collect();

            lowering.applied(curios_anf::Atom::Function(step), fillers)
        });
        self.builder.define_function(
            again,
            Some(format!("{name}/again")),
            vec![token, dropped],
            body,
        );
        // Closed over nothing, so they are items of their own: a description of a call names `again`, and one copied out of the declaration that made it still reaches it.
        self.builder.hoist_functions(vec![step, again]);

        let none = self.empty_list();
        let asking = self.constant_atom(curios_anf::Constant::Bool(false));
        self.applied(
            curios_anf::Atom::Function(step),
            operands.into_iter().chain([none, asking]).collect(),
        )
    }

    /// Field `index` of `product`, a tuple `width` wide.
    fn field(&mut self, product: curios_anf::Atom, width: usize, index: usize) -> curios_anf::Atom {
        let schema = self.tuple_schema(width);

        self.let_atom(curios_anf::Rhs::Project {
            schema,
            product,
            field: u32::try_from(index).expect("a row's results fit a field index"),
        })
    }

    /// What a call of a row answering `results` answers, read out of `reply`, which holds those results and then the lane: the unit for none, the one result for one, and the record of them for several, each option among them read back from the list it crossed as, as `crossed` says for each.
    fn answered(
        &mut self,
        reply: curios_anf::Atom,
        results: &[WireType],
        crossed: &[Option<Polled>],
    ) -> curios_anf::Atom {
        let width = results.len() + 1;
        let mut read = Vec::with_capacity(results.len());

        for (index, polled) in crossed.iter().enumerate() {
            let field = self.field(reply, width, index);

            read.push(match polled {
                Some(polled) => self.option_crossed(field, *polled),
                None => field,
            });
        }

        match read.len() {
            0 => self.unit(),
            1 => read.remove(0),
            fields => {
                let schema = self.tuple_schema(fields);

                self.let_atom(curios_anf::Rhs::Product {
                    schema,
                    fields: read,
                })
            }
        }
    }
}
