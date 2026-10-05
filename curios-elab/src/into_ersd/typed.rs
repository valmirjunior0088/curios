//! The type of a finished term, read off it rather than judged.
//!
//! Erasure is directed by types. The walk carries the type each expression was checked against, and an elimination's head has none to carry: a call's callee, a projection's record, a match's scrutinee. Its type is read here — from what a name was assumed at, by opening a head's telescope at the arguments an application supplies, from the field a projection selects, from what a constructor or a literal states of itself.
//!
//! Nothing is checked. The term is elaborated and certified, and judging it again would run the rules that decide what an author may write — which marks a binder may carry, which module may open a representation — over a term no author wrote, in a context holding no concept and no witness. Every rule here is the type half of the kernel's, with the checks left where they were made.
//!
//! **A term's type is read once per item.** A chain of eliminations asks for the type of each prefix of itself — the call for its callee's, the callee for its own head's — so an answer is kept ([`Lowering::types`]) rather than read again at every level above it. A name is minted once and assumed at one type, so what is kept about a term holds wherever the term stands.

use {
    super::{
        Bound, Cases, Context, Error, Func, FuncType, InductType, Let, Lowering, Many, Match, Proj,
        Rec, Scope, StructType, Subterm, Term, Tuple, TupleType, Variant, reduce_with, sort_term,
        synth_neutral,
    },
    crate::Binders,
    curios_core::{Cost, Produced, ReduceError, foreign_signature},
    curios_utilities::recurse,
};

impl Lowering {
    /// The type `term` has, read at most once for the item being erased.
    pub(super) fn type_of(&mut self, context: &mut Context, term: &Term) -> Result<Term, Error> {
        if let Some(known) = self.types.get(term) {
            return Ok(known.clone());
        }

        let type_ = recurse(|| self.read_type(context, term))?;
        self.types.insert(term.clone(), type_.clone());

        Ok(type_)
    }

    /// One reading. A step is charged for each, as the kernel charges its typing walk, so the budget bounds this one too.
    fn read_type(&mut self, context: &mut Context, term: &Term) -> Result<Term, Error> {
        context
            .spend(Cost::STEP)
            .map_err(|error| exhausted(error, term))?;

        // A member selected from its group carries its type, which is read off the group rather than by entering it.
        if let Some((group, index)) = term.as_rec_proj() {
            return Ok(group.member_type(index));
        }

        match &**term {
            Subterm::Apply(apply) => {
                let head_type = self.type_of(context, &apply.head)?;
                let Subterm::FuncType(FuncType { telescope, .. }) =
                    Term::unwrap_or_clone(reduce_with(context, &head_type)?)
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
            Subterm::Proj(Proj { head, field }) => {
                // Core's selector, qualified for the reason `erase_proj` gives.
                let curios_core::Field::Index(index) = field else {
                    unreachable!("unresolved label projection reached erasure");
                };
                let head_type = self.type_of(context, head)?;
                let field_type = match Term::unwrap_or_clone(reduce_with(context, &head_type)?) {
                    Subterm::TupleType(TupleType { telescope }) => {
                        telescope.field_type_from(head, *index)
                    }
                    Subterm::StructType(StructType {
                        name,
                        universes,
                        params,
                    }) => {
                        let declaration = context
                            .struct_decl(&name)
                            .cloned()
                            .expect("erase: a registered struct");
                        context
                            .instantiate_struct_decl_at(&declaration, &universes)?
                            .fields_at(&params)
                            .field_type_from(head, *index)
                    }
                    _ => unreachable!("erase: projected a non-tuple/struct"),
                };

                Ok(field_type.expect("erase: projection out of range"))
            }
            // The Π over the lambda's own telescope, its codomain the body's type under those binders.
            Subterm::Func(func) => self.lambda_type(context, func),
            // The Σ over the components' types: non-dependent, as a tuple no type was stated for is.
            Subterm::Tuple(Tuple { fields, .. }) => {
                let mut entries = Vec::with_capacity(fields.len());
                for field in fields {
                    entries.push((context.fresh(None), self.type_of(context, field)?));
                }

                Ok(Term::tuple_type(entries))
            }
            Subterm::Struct(value) => Ok(Subterm::StructType(StructType {
                name: value.name,
                universes: value.universes.clone(),
                params: value.params.clone(),
            })
            .into()),
            // The family at the value's parameters and at the index targets its constructor's signature ends in.
            Subterm::Variant(Variant {
                name,
                universes,
                params,
                tag,
                payload,
            }) => {
                let declaration = context
                    .induct_decl(name)
                    .cloned()
                    .expect("erase: a registered inductive");
                let indices = context
                    .instantiate_induct_decl_at(&declaration, universes)?
                    .instantiate(tag, params)
                    .expect("erase: constructor instantiates at its inductive's parameters")
                    .open(&payload.iter().collect::<Vec<_>>());

                Ok(Subterm::InductType(InductType {
                    name: *name,
                    universes: universes.clone(),
                    params: params.clone(),
                    indices,
                })
                .into())
            }
            // An elimination's type is its result at this scrutinee, which for a family is opened at the scrutinee's actual indices.
            Subterm::Match(Match {
                head,
                result,
                cases,
            }) => {
                let indices = match cases {
                    Cases::Induct { .. } => {
                        let head_type = self.type_of(context, head)?;
                        match Term::unwrap_or_clone(reduce_with(context, &head_type)?) {
                            Subterm::InductType(InductType { indices, .. }) => indices,
                            _ => unreachable!(
                                "erase: inductive match scrutinee checked by elaborate"
                            ),
                        }
                    }
                    Cases::Bool { .. } | Cases::Switch { .. } | Cases::FreeMonoid { .. } => {
                        Vec::new()
                    }
                };

                Ok(result.of(head, &indices))
            }
            // Each binding substituted away, which is the rule reduction uses, so the type names nothing the `let` bound.
            Subterm::Let(Let { bindings, tail }) => {
                let mut values = Vec::<Term>::with_capacity(bindings.len());
                for binding in bindings {
                    let value = binding.value().release(&values.iter().collect::<Vec<_>>());
                    values.push(value);
                }

                self.type_of(context, &tail.open(&values.iter().collect::<Vec<_>>()))
            }
            Subterm::Rec(rec) => self.group_tail_type(context, rec),
            Subterm::Intrinsic(intrinsic) => {
                match intrinsic.signature(&context.syntax()).produced {
                    Produced::Fixed(type_) => Ok(type_),
                    Produced::Sort => sort_term(context, term),
                }
            }
            Subterm::Foreign(function, arguments) => {
                let signature =
                    foreign_signature(function, arguments, |label| context.fresh(Some(label)));
                let Produced::Fixed(type_) = signature.produced else {
                    unreachable!("a foreign call produces a fixed `Io` type");
                };

                Ok(type_)
            }
            Subterm::Type(_)
            | Subterm::Prop
            | Subterm::FuncType(_)
            | Subterm::TupleType(_)
            | Subterm::InductType(_)
            | Subterm::StructType(_) => sort_term(context, term),
            // A name, or a member of a group read off the group it carries.
            Subterm::Var(_) | Subterm::Instance(_) => {
                Ok(synth_neutral(context, &Binders::default(), &[], term)
                    .map_err(|error| exhausted(error, term))?
                    .expect("erase: a name in scope"))
            }
            Subterm::Metavar(_) => unreachable!("metavariable survived zonking into erasure"),
            Subterm::Transient(_) => {
                unreachable!("transient node survived elaboration into erasure")
            }
        }
    }

    fn lambda_type(&mut self, context: &mut Context, func: &Func) -> Result<Term, Error> {
        context.with_frame(|context| {
            let mut domains = Vec::with_capacity(func.telescope.len());
            let mut cursor = func.telescope.cursor();
            while let Some((_, domain)) = cursor.entry() {
                let name = context.advance_assumed(&mut cursor, &domain);
                domains.push((name, domain));
            }
            let body = cursor.body().expect("a cursor past every entry");
            let output = self.type_of(context, &body)?;

            Ok(Term::func_type_marked(
                func.plicities()
                    .iter()
                    .zip(domains)
                    .map(|(&plicity, (name, domain))| (plicity, name, domain)),
                output,
            ))
        })
    }

    /// The type of a local group's tail, with each member it names folded back to the group's own projection, so what leaves names nothing the group's scope bound.
    fn group_tail_type(&mut self, context: &mut Context, rec: &Rec) -> Result<Term, Error> {
        let Rec { group, tail } = rec;
        let names = tail
            .hint_iter()
            .map(|label| context.fresh(label))
            .collect::<Vec<_>>();
        let members = names.iter().map(Term::free_var).collect::<Vec<_>>();
        let member_refs = members.iter().collect::<Vec<_>>();

        let type_ = context.with_frame(|context| {
            for (name, member) in names.iter().zip(group.iter()) {
                context.assume(name, &member.type_.open(&member_refs));
                context.set_assumption_universe_context(name, group.universe_context().clone());
            }
            for (index, name) in names.iter().enumerate() {
                context.define(name, &Term::rec_proj(group.clone(), index), None);
            }

            self.type_of(context, &tail.open(&member_refs))
        })?;

        let folded = group.members();
        let binders = names.iter().collect::<Vec<_>>();

        Ok(Scope::close(Many(names.len()), &binders, type_)
            .open(&folded.iter().collect::<Vec<_>>()))
    }
}

/// A budget this reading exhausted, reported against the term it was reading.
fn exhausted(error: ReduceError, term: &Term) -> Error {
    Error::from_reduce(error, |refusal| {
        Error::reduce_exhausted(term.clone(), refusal)
    })
}
