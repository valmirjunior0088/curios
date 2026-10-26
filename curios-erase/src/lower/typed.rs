//! The type of a finished term, read off it rather than judged.
//!
//! Erasure is directed by types. The walk carries the type each expression was checked against, and an elimination's head has none to carry: a call's callee, a projection's record, a match's scrutinee. Its type is read here — from what a name was assumed at, by opening a head's telescope at the arguments an application supplies, from the field a projection selects, from what a constructor or a literal states of itself.
//!
//! Nothing is checked. The term is elaborated and certified, and judging it again would run the rules that decide what an author may write — which marks a binder may carry, which module may open a representation — over a term no author wrote, in a context holding no concept and no witness. Every rule here is the type half of the kernel's, with the checks left where they were made.
//!
//! **A term's type is read once per item.** A chain of eliminations asks for the type of each prefix of itself — the call for its callee's, the callee for its own head's — so an answer is kept ([`Lowering::types`]) rather than read again at every level above it. A name is minted once and assumed at one type, so what is kept about a term holds wherever the term stands.

use {
    super::Lowering, crate::Error, curios_core::Bound, curios_elab::Retyping,
    curios_utilities::recurse,
};

impl Lowering<'_> {
    /// The type `term` has, read at most once for the item being erased.
    pub fn type_of(
        &mut self,
        context: &mut Retyping,
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
        context: &mut Retyping,
        term: &curios_core::Term,
    ) -> Result<curios_core::Term, Error> {
        context
            .spend(curios_core::Cost::STEP)
            .map_err(|error| exhausted(error, term))?;

        // A member selected from its group carries its type, which is read off the group rather than by entering it.
        if let Some((group, index)) = term.as_rec_proj() {
            return Ok(group.member_type(index));
        }

        match &**term {
            curios_core::Subterm::Apply(apply) => {
                let head_type = self.type_of(context, &apply.head)?;
                let reduced = context.reduce_with(&head_type)?;
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
                let field_type = match &*context.reduce_with(&head_type)? {
                    curios_core::Subterm::TupleType(curios_core::TupleType { telescope }) => {
                        telescope.field_type_from(head, field)
                    }
                    curios_core::Subterm::StructType(curios_core::StructType {
                        name,
                        universes,
                        params,
                    }) => context
                        .struct_facet(name, universes, |at| at.field_type(params, head, field))
                        .expect("erase: a registered struct")?,
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
                    .induct_facet(name, universes, |at| {
                        at.constructor(tag)
                            .expect("erase: constructor instantiates at its inductive's parameters")
                    })
                    .expect("erase: a registered inductive")?
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
                        match &*context.reduce_with(&head_type)? {
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
                    curios_core::Produced::Sort => Ok(context.sort_term(term)?),
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
            | curios_core::Subterm::StructType(_) => Ok(context.sort_term(term)?),
            // A name, or a member of a group read off the group it carries.
            curios_core::Subterm::Var(_) | curios_core::Subterm::Instance(_) => Ok(context
                .synth_neutral(term)
                .map_err(|error| exhausted(error, term))?
                .expect("erase: a name in scope")),
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
        context: &mut Retyping,
        func: &curios_core::Func,
    ) -> Result<curios_core::Term, Error> {
        context.with_frame(|context| {
            let mut domains = Vec::with_capacity(func.telescope.len());
            let mut cursor = func.telescope.cursor();
            while let Some((_, domain)) = cursor.entry() {
                let name = context.advance_assumed(&mut cursor, &domain);
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
        context: &mut Retyping,
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

        let type_ = context.with_frame(|context| {
            for (name, member) in names.iter().zip(group.iter()) {
                context.assume(name, &member.type_.open(&member_refs));
                context.set_assumption_universe_context(name, group.universe_context().clone());
            }
            for (index, name) in names.iter().enumerate() {
                context.define(
                    name,
                    &curios_core::Term::rec_proj(group.clone(), index),
                    None,
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

/// A budget this reading exhausted, reported against the term it was reading.
fn exhausted(error: curios_core::ReduceError, term: &curios_core::Term) -> Error {
    curios_elab::Error::from_reduce(error, |refusal| {
        curios_elab::Error::reduce_exhausted(term.clone(), refusal)
    })
    .into()
}
