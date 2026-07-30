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

mod eliminate;
use eliminate::check_induct_arms;

mod prim;
use prim::infer_prim;

#[cfg(test)]
mod tests;

use {
    super::{
        Kernel, KernelError,
        convert::{convert, scoped},
        sort::{self, sort_of},
    },
    crate::{
        Apply, Bound, Cases, Field, Func, FuncType, InductType, Let, Proj, Rec, RecMember, Reducer,
        Struct, StructType, Subterm, Telescope, Term, Tuple, TupleType, UniverseInst, Variant,
        instantiate_universe_levels_scoped,
    },
};

/// Record `obligations` for [`infer`]'s loop, innermost-last.
///
/// Reversed on the way in so popping yields them in source order, which is the
/// order the recursion they replace visited them in.
fn defer(deferred: &mut Vec<Obligation>, obligations: Vec<Obligation>) {
    deferred.extend(obligations.into_iter().rev());
}

/// A child whose type must be checked, deferred rather than descended into.
///
/// See [`infer`]: the pair is a term and the type it has to inhabit, recorded
/// at the context it was written in.
type Obligation = (Term, Term);

/// The type of `term`.
///
/// # Why this drives a stack instead of recursing
///
/// `check` is `infer` followed by `subsumes`, and `infer` on an application
/// checks each argument — so `infer → check → infer` descends two native frames
/// per link of a right-nested chain. A `Str` literal's UTF-8 derivation is one
/// such link per byte, which made the depth a function of the *data* rather
/// than of what anyone wrote. Measured at 21.5KiB of stack per level in a debug
/// build, that exhausted a 2MiB thread partway through `/std/Toml`, and no
/// reduction budget can prevent it: a budget bounds steps, and depth is not
/// steps.
///
/// The child obligations of an application, a constructor, and a record are
/// therefore *deferred* to this loop rather than descended into. Those three
/// open no binders — they instantiate their telescopes by substituting the
/// child term, not by binding it — so every obligation they defer is checked in
/// the same context it was recorded in. Arms that *do* open binders keep
/// recursing, because their depth is the nesting someone wrote, which is the
/// bound `AGENTS.md` allows.
///
/// Order is preserved exactly: obligations are pushed in reverse and popped, so
/// a child is fully checked before its next sibling, which is what recursion
/// did. `infer` drains before it returns, so nothing outside this function can
/// observe an obligation in flight.
pub fn infer(kernel: &mut Kernel, term: &Term) -> Result<Term, KernelError> {
    let mut deferred = Vec::new();
    let inferred = infer_node(kernel, term, &mut deferred)?;

    while let Some((term, expected)) = deferred.pop() {
        let actual = infer_node(kernel, &term, &mut deferred)?;

        if !subsumes(kernel, &actual, &expected)? {
            return Err(KernelError::Mismatch {
                inferred: Box::new(actual),
                expected: Box::new(expected),
            });
        }
    }

    Ok(inferred)
}

/// One node's type, with its deferrable children pushed onto `deferred`.
fn infer_node(
    kernel: &mut Kernel,
    term: &Term,
    deferred: &mut Vec<Obligation>,
) -> Result<Term, KernelError> {
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
            let mut obligations = Vec::with_capacity(params.len());
            for param in params {
                let Telescope::Cons(domain, rest) = telescope else {
                    unreachable!("arity was checked above")
                };

                // The codomain needs the argument *substituted*, not checked,
                // so the result is available without descending into it.
                obligations.push((param.clone(), domain));
                telescope = rest.open(&[param]);
            }
            defer(deferred, obligations);

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
                    kernel.check_instance(&declaration.universe_context, &universes)?;
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
            let declaration = instantiate_induct_decl(kernel, &declaration, universes)?;

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
            let mut obligations = Vec::with_capacity(payload.len());
            for component in payload {
                let Telescope::Cons(field, rest) = signature else {
                    unreachable!("arity was checked above")
                };

                obligations.push((component.clone(), field));
                signature = rest.open(&[component]);
            }
            defer(deferred, obligations);

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
            kernel.check_instance(&declaration.universe_context, universes)?;
            let telescope = instantiate_universe_levels_scoped(&declaration.fields, universes)?
                .open_params(params);

            if telescope.len() != fields.len() {
                return Err(KernelError::Arity {
                    expected: telescope.len(),
                    actual: fields.len(),
                });
            }

            let mut telescope = telescope;
            let mut obligations = Vec::with_capacity(fields.len());
            for field in fields {
                let Telescope::Cons(expected, rest) = telescope else {
                    unreachable!("arity was checked above")
                };

                obligations.push((field.clone(), expected));
                telescope = rest.open(&[field]);
            }
            defer(deferred, obligations);

            Ok(Subterm::StructType(StructType {
                name: name.clone(),
                universes: universes.clone(),
                params: params.clone(),
            })
            .into())
        }

        // An elimination's type is its motive at this scrutinee. The motive
        // binds the family's indices and then the scrutinee itself, so opening
        // it at those is the rule for the *type*.
        //
        // Whether the term deserves that type is `eliminate`'s job: each arm
        // must inhabit the motive at its own constructor's index targets, and a
        // proposition may not be eliminated into a relevant result unless it
        // carries nothing to extract.
        Subterm::Match(m) => {
            let scrutinee_type = infer(kernel, &m.head)?;
            let family = match Term::unwrap_or_clone(kernel.reduce_forced(scrutinee_type)?) {
                Subterm::InductType(family) => Some(family),
                _ => None,
            };
            let indices = family
                .as_ref()
                .map(|family| family.indices.clone())
                .unwrap_or_default();

            if m.motive.arity() != indices.len() + 1 {
                return Err(KernelError::Arity {
                    expected: indices.len() + 1,
                    actual: m.motive.arity(),
                });
            }

            check_cases(kernel, family.as_ref(), &m.motive, &m.cases, &m.head)?;

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

/// Check an elimination's arms against its motive.
///
/// A nominal elimination is the one that can be unsound, and it is verified in
/// full. The primitive carriers are checked where the arm's case is a value the
/// motive can be opened at: `Bool` has two such cases, and a `Switch`'s
/// enumerated cases are literals — and a variable scrutinee *is* that value
/// within the arm, so the arm is checked with the equation substituted, the
/// zero-index instance of the specialization `eliminate` gives nominal arms.
/// What is *not* yet checked is a `Switch`'s default and the free-monoid
/// carriers' arms, whose binders would have to be typed against the carrier's
/// own successor structure. Those arms are typed by their bodies but not
/// verified against the motive — a hole, and a narrower one than the whole of
/// elimination was.
fn check_cases(
    kernel: &mut Kernel,
    family: Option<&InductType>,
    motive: &crate::Scope<crate::Many>,
    cases: &Cases,
    scrutinee: &Term,
) -> Result<(), KernelError> {
    let at = |kernel: &mut Kernel, value: Term, body: &Term| {
        let expected = motive.open(&[&value]);

        // A variable scrutinee stands refined to this case's value in the arm.
        let solutions = match &**scrutinee {
            Subterm::Var(var)
                if var.as_bound().is_none() && kernel.local_type(var.unwrap()).is_some() =>
            {
                vec![(var.unwrap().clone(), value)]
            }
            _ => Vec::new(),
        };

        let mark = kernel.mark();
        eliminate::shadow(kernel, &solutions);
        let outcome = check(
            kernel,
            &eliminate::substitute(body, &solutions),
            &eliminate::substitute(&expected, &solutions),
        );
        kernel.retract(mark);

        outcome
    };

    match cases {
        Cases::Induct { cases, default } => {
            let Some(family) = family else {
                return Err(KernelError::Unclassified(scrutinee.clone()));
            };
            let declaration = kernel
                .induct_decl(&family.name)
                .ok_or_else(|| KernelError::Undeclared(family.name.clone()))?
                .clone();
            let declaration = instantiate_induct_decl(kernel, &declaration, &family.universes)?;

            check_induct_arms(
                kernel,
                &declaration,
                family,
                motive,
                cases,
                default.as_ref(),
                scrutinee,
            )
        }

        Cases::Bool {
            false_case,
            true_case,
        } => {
            at(kernel, Term::prim(crate::Prim::Bool(false)), false_case)?;
            at(kernel, Term::prim(crate::Prim::Bool(true)), true_case)
        }

        Cases::Switch { cases, default } => {
            for (key, body) in cases {
                let literal = Term::prim(crate::Prim::Nat(crate::Nat::new(*key as usize)));
                at(kernel, literal, body)?;
            }

            // The default stands for every value not enumerated, so the only
            // instance of the motive it can be checked at is the scrutinee's —
            // which refines nothing.
            let expected = motive.open(&[scrutinee]);
            check(kernel, default, &expected)
        }

        // The free-monoid carriers bind a peeled generator, a tail, and an
        // induction hypothesis, and checking their arms means typing those
        // binders against the carrier's own structure. Not yet written.
        Cases::FreeMonoid { .. } => Ok(()),
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

/// Whether a term of type `inferred` may stand where `expected` is wanted —
/// the subsumption relation `inferred ≤ expected`.
///
/// This states cumulativity as a rule rather than leaving it to a traversal
/// order. `Γ ⊢ t : A` and `A ≤ B` give `Γ ⊢ t : B`, and `≤` is:
///
/// ```text
/// Type u ≤ Type v          when the level algebra proves u ≤ v
/// Prop   ≤ Type v          a proposition stands wherever a type is wanted
/// Π(x:A).B ≤ Π(x:A').B'    when A ≡ A' and, under x, B ≤ B'
/// A      ≤ B              otherwise, when A ≡ B
/// ```
///
/// **Domains are invariant, codomains cumulative.** Comparing domains by
/// conversion rather than contravariantly is the choice Coq makes, and it is
/// the freely-revisable side of the fork: widening to contravariance later
/// accepts strictly more, so it breaks nothing already accepted, while shipping
/// contravariance and withdrawing it would break programs.
///
/// The elaborator reaches the same verdicts by a different route — it is
/// bidirectional, so checking a λ against a Π pushes the comparison down to the
/// leaves, where both sides are sorts and the head rule suffices, and it never
/// forms the Π being subsumed here. Deciding this structurally instead is what
/// makes the rule readable, and what lets the two checkers disagree if
/// elaboration's traversal order ever changes.
fn subsumes(kernel: &mut Kernel, inferred: &Term, expected: &Term) -> Result<bool, KernelError> {
    kernel.spend()?;

    let lower = kernel.reduce_forced(inferred.clone())?;
    let upper = kernel.reduce_forced(expected.clone())?;

    match (&*lower, &*upper) {
        (Subterm::Type(lower), Subterm::Type(upper)) => return Ok(kernel.level_leq(lower, upper)),
        // `Prop : Type 0`, and a proposition is admitted wherever a type is.
        (Subterm::Prop, Subterm::Type(_)) => return Ok(true),
        // Plicity is part of a function type's identity, exactly as in
        // `convert`: `(A) -> A` and `(@A) -> A` have different calling
        // conventions, so a difference there is a mismatch and not a codomain
        // question.
        (Subterm::FuncType(lower), Subterm::FuncType(upper))
            if lower.plicities == upper.plicities =>
        {
            let (lower, upper) = (lower.telescope.clone(), upper.telescope.clone());

            return scoped(kernel, |kernel| subsumes_telescope(kernel, lower, upper));
        }
        _ => {}
    }

    convert(kernel, &Term::type_ground(), inferred, expected)
}

/// [`subsumes`] through a function type's telescope: each domain by conversion,
/// the terminal codomains by subsumption, under one shared set of binders.
///
/// Opening both sides at the *same* occurrence is what makes the codomain
/// comparison meaningful — the domains have just been shown convertible, so a
/// single binder stands for both.
fn subsumes_telescope(
    kernel: &mut Kernel,
    this: Telescope<Term>,
    that: Telescope<Term>,
) -> Result<bool, KernelError> {
    let (mut this, mut that) = (this, that);

    loop {
        match (this, that) {
            (Telescope::Cons(left, left_rest), Telescope::Cons(right, right_rest)) => {
                if !convert(kernel, &Term::type_ground(), &left, &right)? {
                    return Ok(false);
                }

                let binder = kernel.fresh(left_rest.first_hint());
                kernel.assume(&binder, &left);
                let occurrence = Term::free_var(&binder);

                this = left_rest.open(&[&occurrence]);
                that = right_rest.open(&[&occurrence]);
            }
            (Telescope::Done(left), Telescope::Done(right)) => {
                return subsumes(kernel, &left, &right);
            }
            // Different arities. A function type is not curried in this
            // representation, so this is a real mismatch rather than a shape to
            // normalize.
            _ => return Ok(false),
        }
    }
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
    kernel: &Kernel,
    declaration: &crate::InductDecl,
    levels: &[crate::Level],
) -> Result<crate::InductDecl, KernelError> {
    kernel.check_instance(&declaration.universe_context, levels)?;

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
