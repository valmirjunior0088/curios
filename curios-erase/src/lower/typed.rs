//! The type of a finished term, read off it rather than judged.
//!
//! Erasure is directed by types. The walk carries the type each expression was checked against, and an elimination's head has none to carry: a call's callee, a projection's record, a match's scrutinee. Its type is read here — from what a name was assumed at, by opening a head's telescope at the arguments an application supplies, from the field a projection selects, from what a constructor or a literal states of itself.
//!
//! Nothing is checked. The term is certified, and judging it again would run the kernel's rules over what they already accepted. Every rule here is the type half of the kernel's, with the checks left where they were made, and what it asks of a name, a neutral or a type's head it asks of the kernel that certified the term ([`inferred`], [`reduced`]).
//!
//! **A term's type is read once per item, and once per arm.** A chain of eliminations asks for the type of each prefix of itself — the call for its callee's, the callee for its own head's — so an answer is kept ([`Lowering::types`]) rather than read again at every level above it. A name is minted once and assumed at one type, so what is kept about a term holds wherever the term stands, but in an arm: there the kernel binds a variable the case solves to its value and reads every type by value, so an arm is read with nothing kept from outside it and keeps nothing past it (`Lowering::in_arm`). A type kept across two arms would read one arm's family in the other, which shares its tags and widths and is not the row its value was built at (`curios`'s `tests::codegen::specialize`).

use {
    super::Lowering,
    crate::{Error, advance_assumed, inferred, reduced},
    curios_cert::Reader,
    curios_core::Bound,
    curios_utilities::recurse,
};

impl Lowering<'_> {
    /// The type `term` has, read at most once for the item being erased.
    pub fn type_of(
        &mut self,
        context: &mut Reader,
        term: &curios_core::Term,
    ) -> Result<curios_core::Term, Error> {
        if let Some(known) = self.types.get(term) {
            return Ok(known.clone());
        }

        let type_ = recurse(|| self.read_type(context, term))?;
        self.types.insert(term.clone(), type_.clone());

        Ok(type_)
    }

    /// One reading. A step is charged for each, as the kernel charges its typing walk, so the budget bounds this one too.
    fn read_type(
        &mut self,
        context: &mut Reader,
        term: &curios_core::Term,
    ) -> Result<curios_core::Term, Error> {
        context
            .spend(curios_core::Cost::STEP)
            .map_err(|refusal| Error::reducing(term, refusal))?;

        // A member selected from its group carries its type, which is read off the group rather than by entering it.
        if let Some((group, index)) = term.as_rec_proj() {
            return Ok(group.member_type(index));
        }

        match &**term {
            curios_core::Subterm::Apply(apply) => {
                let head_type = self.type_of(context, &apply.head)?;
                let reduced = reduced(context, &head_type)?;
                let curios_core::Subterm::FuncType(curios_core::FuncType { telescope, .. }) =
                    &*reduced
                else {
                    unreachable!("erase: applied a non-function");
                };
                let arguments = apply.params().collect::<Vec<_>>();
                assert_eq!(
                    arguments.len(),
                    telescope.len(),
                    "erase: application arity disagrees with the function type",
                );

                Ok(telescope.open(&arguments))
            }
            // Erasure runs on an elaborated term, whose every projection is a slot: the field says how the fields before it are read.
            curios_core::Subterm::Proj(curios_core::Proj { head, field }) => {
                let head_type = self.type_of(context, head)?;
                let field_type = match &*reduced(context, &head_type)? {
                    curios_core::Subterm::TupleType(curios_core::TupleType { telescope }) => {
                        telescope.field_type_from(head, field)
                    }
                    curios_core::Subterm::StructType(curios_core::StructType {
                        name,
                        universes,
                        params,
                    }) => context
                        .struct_decl(name)
                        .expect("erase: a registered struct")
                        .at(universes)
                        .field_type(params, head, field)?,
                    _ => unreachable!("erase: projected a non-tuple/struct"),
                };

                Ok(field_type.expect("erase: projection out of range"))
            }
            // The Π over the lambda's own telescope, its codomain the body's type under those binders.
            curios_core::Subterm::Func(func) => self.lambda_type(context, func),
            // The Σ over the components' types: non-dependent, as a tuple no type was stated for is.
            curios_core::Subterm::Tuple(curios_core::Tuple { fields, .. }) => {
                let mut entries = Vec::with_capacity(fields.len());
                for field in fields {
                    entries.push((context.fresh(None), self.type_of(context, field)?));
                }

                Ok(curios_core::Term::tuple_type(entries))
            }
            curios_core::Subterm::Struct(value) => {
                Ok(curios_core::Subterm::StructType(curios_core::StructType {
                    name: value.name,
                    universes: value.universes.clone(),
                    params: value.params.clone(),
                })
                .into())
            }
            // The family at the value's parameters and at the index targets its constructor's signature ends in.
            curios_core::Subterm::Variant(curios_core::Variant {
                name,
                universes,
                params,
                tag,
                payload,
            }) => {
                let indices = context
                    .induct_decl(name)
                    .expect("erase: a registered inductive")
                    .at(universes)
                    .constructor(tag)
                    .expect("erase: constructor instantiates at its inductive's parameters")?
                    .telescope
                    .open_params(params)
                    .open(&payload.iter().collect::<Vec<_>>());

                Ok(curios_core::Subterm::InductType(curios_core::InductType {
                    name: *name,
                    universes: universes.clone(),
                    params: params.clone(),
                    indices,
                })
                .into())
            }
            // An elimination's type is its result at this scrutinee, which for a family is opened at the scrutinee's actual indices.
            curios_core::Subterm::Match(curios_core::Match {
                head,
                result,
                cases,
            }) => {
                let indices = match cases {
                    curios_core::Cases::Induct { .. } => {
                        let head_type = self.type_of(context, head)?;
                        match &*reduced(context, &head_type)? {
                            curios_core::Subterm::InductType(curios_core::InductType {
                                indices,
                                ..
                            }) => indices.clone(),
                            _ => unreachable!(
                                "erase: inductive match scrutinee checked by elaborate"
                            ),
                        }
                    }
                    curios_core::Cases::Bool { .. }
                    | curios_core::Cases::Switch { .. }
                    | curios_core::Cases::FreeMonoid { .. } => Vec::new(),
                };

                Ok(result.of(head, &indices))
            }
            // Each binding substituted away, which is the rule reduction uses, so the type names nothing the `let` bound.
            curios_core::Subterm::Let(curios_core::Let { bindings, tail }) => {
                let mut values = Vec::<curios_core::Term>::with_capacity(bindings.len());
                for binding in bindings {
                    let value = binding.value().release(&values.iter().collect::<Vec<_>>());
                    values.push(value);
                }

                self.type_of(context, &tail.open(&values.iter().collect::<Vec<_>>()))
            }
            curios_core::Subterm::Rec(rec) => self.group_tail_type(context, rec),
            curios_core::Subterm::Intrinsic(intrinsic) => {
                match intrinsic.signature(&context.syntax()).produced {
                    curios_core::Produced::Fixed(type_) => Ok(type_),
                    curios_core::Produced::Sort => inferred(context, term),
                }
            }
            curios_core::Subterm::Foreign(function, _, arguments) => {
                let syntax = context.syntax();
                let signature =
                    curios_core::foreign_signature(function, arguments, &syntax, |label| {
                        context.fresh(Some(label))
                    });
                let curios_core::Produced::Fixed(type_) = signature.produced else {
                    unreachable!("a foreign call produces a fixed `Io` type");
                };

                Ok(type_)
            }
            curios_core::Subterm::Sort(..)
            | curios_core::Subterm::FuncType(_)
            | curios_core::Subterm::TupleType(_)
            | curios_core::Subterm::InductType(_)
            | curios_core::Subterm::StructType(_) => inferred(context, term),
            // A name, or a declaration at an instance: what it was assumed or defined at, as the kernel reads it.
            curios_core::Subterm::Var(_) | curios_core::Subterm::Instance(_) => {
                inferred(context, term)
            }
            curios_core::Subterm::Metavar(_) => {
                unreachable!("metavariable survived zonking into erasure")
            }
            curios_core::Subterm::Transient(_) => {
                unreachable!("transient node survived elaboration into erasure")
            }
        }
    }

    fn lambda_type(
        &mut self,
        context: &mut Reader,
        func: &curios_core::Func,
    ) -> Result<curios_core::Term, Error> {
        context.scoped(|context| {
            let mut domains = Vec::with_capacity(func.telescope.len());
            let mut cursor = func.telescope.cursor();
            while let Some((_, domain)) = cursor.entry() {
                let name = advance_assumed(context, &mut cursor, &domain);
                domains.push((name, domain));
            }
            let body = cursor.body().expect("a cursor past every entry");
            let output = self.type_of(context, &body)?;

            Ok(curios_core::Term::func_type_marked(
                func.plicities()
                    .iter()
                    .zip(domains)
                    .map(|(&plicity, (name, domain))| (plicity, name, domain)),
                output,
            ))
        })
    }

    /// The type of a local group's tail, with each member it names folded back to the group's own projection, so what leaves names nothing the group's scope bound.
    fn group_tail_type(
        &mut self,
        context: &mut Reader,
        rec: &curios_core::Rec,
    ) -> Result<curios_core::Term, Error> {
        let curios_core::Rec { group, tail } = rec;
        let names = tail
            .hint_iter()
            .map(|label| context.fresh(label))
            .collect::<Vec<_>>();
        let members = names
            .iter()
            .map(curios_core::Term::free_var)
            .collect::<Vec<_>>();
        let member_refs = members.iter().collect::<Vec<_>>();

        // Each member bound at its type to its selection, as the kernel's own group tail binds it.
        let type_ = context.scoped(|context| {
            for (index, (name, member)) in names.iter().zip(group.iter()).enumerate() {
                context.bind(
                    name,
                    &member.type_.open(&member_refs),
                    &curios_core::Term::rec_proj(group.clone(), index),
                );
            }

            self.type_of(context, &tail.open(&member_refs))
        })?;

        let folded = group.members();
        let binders = names.iter().collect::<Vec<_>>();

        Ok(
            curios_core::Scope::close(curios_core::Many(names.len()), &binders, type_)
                .open(&folded.iter().collect::<Vec<_>>()),
        )
    }
}
