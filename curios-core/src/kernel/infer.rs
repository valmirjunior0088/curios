//! The typing judgment: what type a term has, and whether it has the one it
//! was supposed to.
//!
//! This is the rule set. Everything else in the kernel exists to serve it —
//! `whnf` so a type can be looked at, `sort_of` so a proposition can be
//! recognized, `convert` so two types can be compared. What is written here is
//! the language's typing rules, one per term form, and it is meant to be read
//! that way.
//!
//! # Bidirectional, but only just
//!
//! Two judgments: [`infer`] synthesizes a term's type, and [`check`] verifies a
//! term against a type it is given. The elaborator's versions of these carry a
//! great deal more — implicit-argument insertion, metavariable invention,
//! postponement, overload resolution — because their job is to *make* a
//! well-typed term out of what someone wrote. The kernel's job is to look at a
//! finished term and say yes or no, so [`check`] is almost trivial: infer, then
//! ask whether the result is subsumed by the expectation. The interesting
//! content is all in [`infer`], which is where it belongs.
//!
//! # What the kernel refuses
//!
//! Three term forms are elaboration-only — a metavariable, an unresolved infix
//! operator, and a polymorphic numeric literal — and reaching one means a term
//! arrived here before elaboration finished with it. Refusing them is what
//! makes "the kernel checks finished terms" a checked statement rather than a
//! convention.
//!
//! Beyond those, the kernel refuses whatever it cannot determine. A type whose
//! sort is unclear, a nominal name with no declaration, a list literal with no
//! element to read a type from: each is a refusal, never a default. The reason
//! is in the [`kernel`](super) module documentation — a guessed answer from a
//! second opinion is worse than no second opinion.

mod prim;
use prim::infer_prim;

#[cfg(test)]
mod tests;

use {
    super::{
        Kernel, KernelError,
        convert::convert,
        sort::{self, sort_of},
    },
    crate::{
        Apply, Bound, Field, Func, FuncType, InductType, Let, Proj, Rec, RecMember, Reducer,
        Struct, StructType, Subterm, Telescope, Term, Tuple, TupleType, UniverseInst, Variant,
        instantiate_universe_levels_scoped,
    },
};

/// The type of `term`.
pub fn infer(kernel: &mut Kernel, term: &Term) -> Result<Term, KernelError> {
    kernel.spend()?;

    match &**term {
        // `Type u : Type (u + 1)`, and `Prop : Type 0`. The hierarchy is what
        // makes `Type : Type` — and Girard's paradox with it — unstatable.
        Subterm::Type(level) => Ok(Term::type_at(level.succ()?)),
        Subterm::Prop => Ok(Term::type_ground()),

        Subterm::Prim(prim) => infer_prim(kernel, prim),

        // A variable has the type it was bound or declared at. There is no
        // fallback: an unbound name in a finished term is a broken term.
        Subterm::Var(var) => kernel
            .type_of(var.unwrap())
            .cloned()
            .ok_or_else(|| KernelError::Unbound(var.unwrap().clone())),

        // A type former is a type, at the universe `sort_of` computes for it —
        // the join of its parts, or `Prop` when it lands there.
        Subterm::FuncType(_) | Subterm::TupleType(_) => Ok(sort_of(kernel, term)?.term()),

        // λ: check each domain is a type, then the body under those binders.
        // The result is the Π over the same telescope.
        Subterm::Func(Func {
            telescope,
            plicities,
        }) => {
            let telescope = infer_telescope(kernel, telescope.clone())?;

            Ok(Subterm::FuncType(FuncType {
                telescope,
                plicities: plicities.clone(),
            })
            .into())
        }

        // Application: the head must be a function of matching arity, each
        // argument checks against its domain, and the result is the codomain
        // with the arguments substituted — which is where dependency lives.
        Subterm::Apply(Apply { head, params, .. }) => {
            let head_type = infer(kernel, head)?;

            let Subterm::FuncType(FuncType { telescope, .. }) =
                Term::unwrap_or_clone(kernel.reduce_forced(head_type.clone())?)
            else {
                return Err(KernelError::NotAFunction(head_type));
            };

            if telescope.len() != params.len() {
                return Err(KernelError::Arity {
                    expected: telescope.len(),
                    actual: params.len(),
                });
            }

            let mut telescope = telescope;
            for param in params {
                let Telescope::Cons(domain, rest) = telescope else {
                    unreachable!("arity was checked above")
                };

                check(kernel, param, &domain)?;
                telescope = rest.open(&[param]);
            }

            match telescope {
                Telescope::Done(result) => Ok(*result),
                Telescope::Cons(..) => unreachable!("arity was checked above"),
            }
        }

        // A tuple's type is the Σ over its components' types. Non-dependent:
        // a component's type is inferred in the scope it stands in, so nothing
        // here can make a later component depend on an earlier one. A term that
        // needs that dependency carries the Σ and is *checked* against it.
        Subterm::Tuple(Tuple { fields, .. }) => {
            let mut entries = Vec::with_capacity(fields.len());
            for field in fields {
                entries.push((kernel.fresh(None), infer(kernel, field)?));
            }

            Ok(Term::tuple_type(entries))
        }

        // Projection: the component's type, with earlier components named by
        // projections of this same head — which is what makes a Σ dependent.
        Subterm::Proj(Proj { head, field }) => {
            let Field::Index(index) = field else {
                return Err(KernelError::Unclassified(term.clone()));
            };

            let head_type = infer(kernel, head)?;

            match Term::unwrap_or_clone(kernel.reduce_forced(head_type.clone())?) {
                Subterm::TupleType(TupleType { telescope }) => telescope
                    .nth(*index, |j| Term::proj(head.clone(), j))
                    .ok_or(KernelError::Arity {
                        expected: *index,
                        actual: 0,
                    }),
                Subterm::StructType(StructType {
                    name,
                    universes,
                    params,
                }) => {
                    let declaration = kernel
                        .struct_decl(&name)
                        .ok_or_else(|| KernelError::Undeclared(name.clone()))?;
                    let fields = instantiate_universe_levels_scoped(
                        &declaration.fields.clone(),
                        &universes,
                    )?;

                    fields
                        .open_params(&params)
                        .nth(*index, |j| Term::proj(head.clone(), j))
                        .ok_or(KernelError::Arity {
                            expected: *index,
                            actual: 0,
                        })
                }
                _ => Err(KernelError::NotATuple(head_type)),
            }
        }

        // A fully applied nominal family has the sort its declaration states.
        Subterm::InductType(_) | Subterm::StructType(_) => Ok(sort_of(kernel, term)?.term()),

        // A constructor application: its signature, instantiated at the
        // declaration's parameters, ends in the type it constructs — including
        // the index targets this particular case aims at.
        Subterm::Variant(Variant {
            name,
            universes,
            params,
            tag,
            payload,
        }) => {
            let declaration = kernel
                .induct_decl(name)
                .ok_or_else(|| KernelError::Undeclared(name.clone()))?
                .clone();
            let declaration = instantiate_induct_decl(&declaration, universes)?;

            let signature = declaration
                .instantiate(tag, params)
                .ok_or_else(|| KernelError::Undeclared(name.clone()))?;

            if signature.len() != payload.len() {
                return Err(KernelError::Arity {
                    expected: signature.len(),
                    actual: payload.len(),
                });
            }

            let mut signature = signature;
            for component in payload {
                let Telescope::Cons(field, rest) = signature else {
                    unreachable!("arity was checked above")
                };

                check(kernel, component, &field)?;
                signature = rest.open(&[component]);
            }

            match signature {
                Telescope::Done(constructed) => Ok(*constructed),
                Telescope::Cons(..) => unreachable!("arity was checked above"),
            }
        }

        // A nominal record: its fields check against the declaration's field
        // telescope, and its type is the family at the same parameters.
        Subterm::Struct(Struct {
            name,
            universes,
            params,
            fields,
            ..
        }) => {
            let declaration = kernel
                .struct_decl(name)
                .ok_or_else(|| KernelError::Undeclared(name.clone()))?
                .clone();
            let telescope = instantiate_universe_levels_scoped(&declaration.fields, universes)?
                .open_params(params);

            if telescope.len() != fields.len() {
                return Err(KernelError::Arity {
                    expected: telescope.len(),
                    actual: fields.len(),
                });
            }

            let mut telescope = telescope;
            for field in fields {
                let Telescope::Cons(expected, rest) = telescope else {
                    unreachable!("arity was checked above")
                };

                check(kernel, field, &expected)?;
                telescope = rest.open(&[field]);
            }

            Ok(Subterm::StructType(StructType {
                name: name.clone(),
                universes: universes.clone(),
                params: params.clone(),
            })
            .into())
        }

        // An elimination's type is its motive, at this scrutinee. The motive
        // binds the family's indices and then the scrutinee itself, so opening
        // it at those is the whole rule.
        //
        // The *arms* are not checked here. Checking them is what makes an
        // elimination sound, and it needs the per-constructor index refinement
        // the elaborator performs; until the kernel does that, an elimination is
        // typed but not verified, and this is the largest gap in the judgment.
        Subterm::Match(m) => {
            let scrutinee_type = infer(kernel, &m.head)?;
            let indices = match Term::unwrap_or_clone(kernel.reduce_forced(scrutinee_type)?) {
                Subterm::InductType(InductType { indices, .. }) => indices,
                _ => Vec::new(),
            };

            if m.motive.arity() != indices.len() + 1 {
                return Err(KernelError::Arity {
                    expected: indices.len() + 1,
                    actual: m.motive.arity(),
                });
            }

            let mut arguments = indices;
            arguments.push(m.head.clone());
            let refs = arguments.iter().collect::<Vec<_>>();

            Ok(m.motive.open(&refs))
        }

        // `let` is checked binding by binding and then substituted away, which
        // is the same rule reduction uses. Each binding sees exactly the values
        // before it: a `let` is non-recursive, and self-reference is `rec`'s.
        Subterm::Let(Let { bindings, tail }) => {
            let mut values = Vec::with_capacity(bindings.len());

            for binding in bindings {
                let refs = values.iter().collect::<Vec<_>>();
                let type_ = binding.type_().release(&refs);
                let value = binding.value().release(&refs);

                sort_of(kernel, &type_)?;
                check(kernel, &value, &type_)?;
                values.push(value);
            }

            let refs = values.iter().collect::<Vec<_>>();
            let tail = tail.open(&refs);

            infer(kernel, &tail)
        }

        // A recursive group: every member is assumed at its declared type
        // — mutually, so a member may call any other — and then every body is
        // checked against that type. Totality is *not* decided here; `rec` is
        // general recursion by design, and the obligation that keeps it sound
        // is positional, enforced by (T) and (V) over the whole module.
        Subterm::Rec(Rec { group, tail }) => {
            let members = group.members();
            let refs = members.iter().collect::<Vec<_>>();

            for index in 0..group.length() {
                let type_ = group.member_type(index);
                sort_of(kernel, &type_)?;
                check(kernel, &group.member_body(index), &type_)?;
            }

            let tail = tail.open(&refs);

            infer(kernel, &tail)
        }

        // A folded recursive call carries its own type on its group.
        Subterm::RecMember(RecMember { group, index }) => Ok(group.member_type(*index)),

        // A polymorphic name at a stated instance: its scheme, substituted.
        Subterm::UniverseInst(UniverseInst { head, levels }) => {
            match sort::synth_neutral(kernel, term)? {
                Some(type_) => Ok(type_),
                None => {
                    let _ = (head, levels);
                    Err(KernelError::Unclassified(term.clone()))
                }
            }
        }

        // Elaboration-only syntax. Reaching one means a term arrived here
        // before elaboration was finished with it.
        Subterm::Metavar(_) | Subterm::Infix(_) | Subterm::NumLit(_) => {
            Err(KernelError::NotCore(term.clone()))
        }
    }
}

/// Verify that `term` has type `expected`.
pub fn check(kernel: &mut Kernel, term: &Term, expected: &Term) -> Result<(), KernelError> {
    let inferred = infer(kernel, term)?;

    match subsumes(kernel, &inferred, expected)? {
        true => Ok(()),
        false => Err(KernelError::Mismatch {
            inferred: Box::new(inferred),
            expected: Box::new(expected.clone()),
        }),
    }
}

/// Whether a term of type `inferred` may stand where `expected` is wanted.
///
/// Conversion, plus cumulativity: a `Type u` sits inside every `Type v` above
/// it, and `Prop` sits inside every `Type`. Cumulativity is checked only at the
/// head, not under a function type's codomain. That is the incomplete
/// direction, which refuses programs rather than admitting them.
fn subsumes(kernel: &mut Kernel, inferred: &Term, expected: &Term) -> Result<bool, KernelError> {
    let lower = kernel.reduce_forced(inferred.clone())?;
    let upper = kernel.reduce_forced(expected.clone())?;

    match (&*lower, &*upper) {
        (Subterm::Type(lower), Subterm::Type(upper)) => return Ok(lower.structurally_leq(upper)),
        // `Prop : Type 0`, and a proposition is admitted wherever a type is.
        (Subterm::Prop, Subterm::Type(_)) => return Ok(true),
        _ => {}
    }

    convert(kernel, &Term::type_ground(), inferred, expected)
}

/// Check that every domain of a λ's telescope is a type, then its body under
/// those binders, rebuilding the telescope as the Π the λ inhabits.
fn infer_telescope(
    kernel: &mut Kernel,
    telescope: Telescope<Term>,
) -> Result<Telescope<Term>, KernelError> {
    match telescope {
        Telescope::Done(body) => {
            let type_ = infer(kernel, &body)?;

            Ok(Telescope::Done(Box::new(type_)))
        }
        Telescope::Cons(domain, rest) => {
            sort_of(kernel, &domain)?;

            let binder = kernel.fresh(rest.first_hint());
            let mark = kernel.mark();
            kernel.assume(&binder, &domain);

            let inner = infer_telescope(kernel, rest.open(&[&Term::free_var(&binder)]));
            kernel.retract(mark);

            Ok(Telescope::Cons(
                domain,
                crate::Scope::close(crate::One, &[&binder], inner?),
            ))
        }
    }
}

/// A declaration with its universe parameters replaced by this occurrence's
/// instance, so its constructor signatures speak of the right levels.
fn instantiate_induct_decl(
    declaration: &crate::InductDecl,
    levels: &[crate::Level],
) -> Result<crate::InductDecl, KernelError> {
    let mut instantiated = declaration.clone();

    instantiated.params = instantiate_universe_levels_scoped(&instantiated.params, levels)?;
    instantiated.indices = instantiate_universe_levels_scoped(&instantiated.indices, levels)?;
    instantiated.result_sort =
        instantiate_universe_levels_scoped(&instantiated.result_sort, levels)?;
    for constructor in instantiated.signatures_mut() {
        constructor.telescope = instantiate_universe_levels_scoped(&constructor.telescope, levels)?;
    }
    instantiated.universe_context = crate::UniverseContext::empty();

    Ok(instantiated)
}
