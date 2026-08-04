//! The typing judgment: what type a term has, and whether it has the one it was supposed to.
//!
//! This is the rule set. Everything else in the kernel exists to serve it — `whnf` so a type can be looked at, `Sort::of` so a proposition can be recognized, `convert` so two types can be compared. What is written here is the language's typing rules, one per term form, and it is meant to be read that way.
//!
//! # Bidirectional, but only just
//!
//! Two judgments: [`infer`] synthesizes a term's type, and [`check`] verifies a term against a type it is given. The elaborator's versions of these carry a great deal more — implicit-argument insertion, metavariable invention, postponement, overload resolution — because their job is to *make* a well-typed term out of what someone wrote. The kernel's job is to look at a finished term and say yes or no, so [`check`] is almost trivial: infer, then ask whether the result is subsumed by the expectation. The interesting content is all in [`infer`], which is where it belongs.
//!
//! # What the kernel refuses
//!
//! Three term forms are elaboration-only — a metavariable, an unresolved infix operator, and a polymorphic numeric literal — and reaching one means a term arrived here before elaboration finished with it. Refusing them is what makes "the kernel checks finished terms" a checked statement rather than a convention.
//!
//! Beyond those, the kernel refuses whatever it cannot determine. A type whose sort is unclear, a nominal name with no declaration, a list literal with no element to read a type from: each is a refusal, never a default. The reason is in the [`kernel`](super) module documentation — a guessed answer from a second opinion is worse than no second opinion.

mod eliminate;
use eliminate::check_induct_arms;

mod prim;
use prim::infer_prim;

#[cfg(test)]
mod tests;

use {
    super::{
        Kernel, KernelError, Sort, check_group, convert::convert, sort::as_sort, synth_neutral,
    },
    curios_base::{Grain, PackedBin},
    curios_core::{
        Apply, Bound, Carrier, Cases, Field, Free, Func, FuncType, InductType, Let, Many, Nat, One,
        Prim, Proj, Rec, Reducer, Scope, Struct, StructType, Subterm, Telescope, Term, Tuple,
        TupleType, UniverseInst, Variant,
    },
};

/// Record `obligations` for [`infer`]'s loop, innermost-last.
///
/// Reversed on the way in so popping yields them in source order, which is the order the recursion they replace visited them in.
fn defer(deferred: &mut Vec<Obligation>, obligations: Vec<Obligation>) {
    deferred.extend(obligations.into_iter().rev());
}

/// A child whose type must be checked, deferred rather than descended into.
///
/// See [`infer`]: the pair is a term and the type it has to inhabit, recorded at the context it was written in.
type Obligation = (Term, Term);

/// The type of `term`.
///
/// # Why this drives a stack instead of recursing
///
/// `check` is `infer` followed by `subsumes`, and `infer` on an application checks each argument — so `infer → check → infer` descends two native frames per link of a right-nested chain. A `Str` literal's UTF-8 derivation is one such link per byte, which made the depth a function of the *data* rather than of what anyone wrote. Measured at 21.5KiB of stack per level in a debug build, that exhausted a 2MiB thread partway through `/std/Toml`, and no reduction budget can prevent it: a budget bounds steps, and depth is not steps.
///
/// The child obligations of an application, a constructor, and a record are therefore *deferred* to this loop rather than descended into. Those three open no binders — they instantiate their telescopes by substituting the child term, not by binding it — so every obligation they defer is checked in the same context it was recorded in. Arms that *do* open binders keep recursing, because their depth is the nesting someone wrote, which is a bound the default stack tolerates.
///
/// Order is preserved exactly: obligations are pushed in reverse and popped, so a child is fully checked before its next sibling, which is what recursion did. `infer` drains before it returns, so nothing outside this function can observe an obligation in flight.
pub fn infer(kernel: &mut Kernel, term: &Term) -> Result<Term, KernelError> {
    let mut deferred = Vec::new();
    let inferred = infer_node(kernel, term, &mut deferred)?;
    // Seed for the erasure obligations, at an *inferred* position. A term's type is its type however the judgment arrived at it, so a proof reached only by inference — a match scrutinee, most consequentially — is a proof position exactly as a checked one is. Recording only checked positions left a diverging proof in a scrutinee unseeded, and the elimination conjured a relevant value from it.
    kernel.record_checked(term, &inferred);

    while let Some((term, expected)) = deferred.pop() {
        let actual = infer_node(kernel, &term, &mut deferred)?;
        kernel.record_checked(&term, &actual);

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
        // `Type u : Type (u + 1)`, and `Prop : Type 0`. The hierarchy is what makes `Type : Type` — and Girard's paradox with it — unstatable.
        Subterm::Type(level) => Ok(Term::type_at(level.succ()?)),
        Subterm::Prop => Ok(Term::type_ground()),

        Subterm::Prim(prim) => infer_prim(kernel, prim),

        // A variable has the type it was bound or declared at. There is no fallback: an unbound name in a finished term is a broken term.
        Subterm::Var(var) => kernel
            .type_of(var.unwrap())
            .cloned()
            .ok_or_else(|| KernelError::Unbound(var.unwrap().clone())),

        // A type former is a type, at the universe `Sort::of` computes for it — the join of its parts, or `Prop` when it lands there.
        Subterm::FuncType(_) | Subterm::TupleType(_) => Ok(Sort::of(kernel, term)?.term()),

        // λ: check each domain is a type, then the body under those binders. The result is the Π over the same telescope.
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

        // Application: the head must be a function of matching arity, each argument checks against its domain, and the result is the codomain with the arguments substituted — which is where dependency lives.
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

                // The codomain needs the argument *substituted*, not checked, so the result is available without descending into it.
                obligations.push((param.clone(), domain));
                telescope = rest.open(&[param]);
            }
            defer(deferred, obligations);

            match telescope {
                Telescope::Done(result) => Ok(*result),
                Telescope::Cons(..) => unreachable!("arity was checked above"),
            }
        }

        // A tuple's type is the Σ over its components' types. Non-dependent: a component's type is inferred in the scope it stands in, so nothing here can make a later component depend on an earlier one. A term that needs that dependency carries the Σ and is *checked* against it.
        Subterm::Tuple(Tuple { fields, .. }) => {
            let mut entries = Vec::with_capacity(fields.len());
            for field in fields {
                entries.push((kernel.fresh(None), infer(kernel, field)?));
            }

            Ok(Term::tuple_type(entries))
        }

        // Projection: the component's type, with earlier components named by projections of this same head — which is what makes a Σ dependent.
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
                }) => kernel
                    .struct_at(&name, &universes, &params)?
                    .fields()
                    .nth(*index, |j| Term::proj(head.clone(), j))
                    .ok_or(KernelError::Arity {
                        expected: *index,
                        actual: 0,
                    }),
                _ => Err(KernelError::NotATuple(head_type)),
            }
        }

        // A fully applied nominal family has the sort its declaration states.
        Subterm::InductType(_) | Subterm::StructType(_) => Ok(Sort::of(kernel, term)?.term()),

        // A constructor application: its signature, instantiated at the declaration's parameters, ends in the type it constructs — including the index targets this particular case aims at.
        Subterm::Variant(Variant {
            name,
            universes,
            params,
            tag,
            payload,
        }) => {
            // The handle checks the universe instance and the parameter count before any of the declaration is read at them. `open_params` is tolerant — too few parameters leaves the declaration's own parameter binders unopened, so they read as payload slots and the arity check below would compare against the wrong number.
            let signature = kernel
                .induct_at_params(name, universes, params)?
                .signature(tag)
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

            // The constructed type, rebuilt from what the terminal states and what the declaration already fixes: this family, at the parameters this occurrence supplied.
            match signature {
                Telescope::Done(targets) => Ok(Subterm::InductType(InductType {
                    name: name.clone(),
                    universes: universes.clone(),
                    params: params.clone(),
                    indices: *targets,
                })
                .into()),
                Telescope::Cons(..) => unreachable!("arity was checked above"),
            }
        }

        // A nominal record: its fields check against the declaration's field telescope, and its type is the family at the same parameters.
        Subterm::Struct(Struct {
            name,
            universes,
            params,
            fields,
            ..
        }) => {
            let telescope = kernel.struct_at(name, universes, params)?.fields();

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

        // An elimination's type is its motive at this scrutinee. The motive binds the family's indices and then the scrutinee itself, so opening it at those is the rule for the *type*.
        //
        // Whether the term deserves that type is `eliminate`'s job: each arm must inhabit the motive at its own constructor's index targets, and a proposition may not be eliminated into a relevant result unless it carries nothing to extract.
        Subterm::Match(m) => {
            let scrutinee_type = infer(kernel, &m.head)?;
            let scrutinee_type = kernel.reduce_forced(scrutinee_type)?;
            let family = match &*scrutinee_type {
                Subterm::InductType(family) => Some(family.clone()),
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

            check_cases(
                kernel,
                family.as_ref(),
                &m.motive,
                &m.cases,
                &m.head,
                &scrutinee_type,
            )?;

            let mut arguments = indices;
            arguments.push(m.head.clone());
            let refs = arguments.iter().collect::<Vec<_>>();

            Ok(m.motive.open(&refs))
        }

        // `let` is checked binding by binding and then substituted away, which is the same rule reduction uses. Each binding sees exactly the values before it: a `let` is non-recursive, and self-reference is `rec`'s.
        Subterm::Let(Let { bindings, tail }) => {
            let mut values = Vec::with_capacity(bindings.len());

            for binding in bindings {
                let refs = values.iter().collect::<Vec<_>>();
                let type_ = binding.type_().release(&refs);
                let value = binding.value().release(&refs);

                infer_type(kernel, &type_)?;
                check(kernel, &value, &type_)?;
                values.push(value);
            }

            let refs = values.iter().collect::<Vec<_>>();
            let tail = tail.open(&refs);

            infer(kernel, &tail)
        }

        // A recursive group, checked by the one rule that holds it — asked here rather than restated, so a `rec` in a term and a `rec` at the top level cannot come to disagree about what makes a group legal.
        Subterm::Rec(Rec { group, tail }) => check_group(kernel, group, |kernel, names| {
            let members = names.iter().map(Term::free_var).collect::<Vec<_>>();
            let refs = members.iter().collect::<Vec<_>>();

            let type_ = infer(kernel, &tail.open(&refs))?;

            // The tail's type may mention the members, and their binders retract with the group's scope. Re-fold the knot so what leaves this judgment is closed: the folded spelling denotes the same member and needs nothing in scope to do it.
            let folded = group.members();
            let folded = folded.iter().collect::<Vec<_>>();
            let binders = names.iter().collect::<Vec<_>>();

            Ok(Scope::close(Many(names.len()), &binders, type_).open(&folded))
        }),

        // A polymorphic name at a stated instance: its scheme, substituted.
        Subterm::UniverseInst(UniverseInst { head, levels }) => {
            // `synth_neutral` reads a projection's type off the group it carries, which is a lookup and must stay one — so the group is certified *here*, before the read, at the generic spelling the instance was taken from. Skipping this would leave the instance spelling as a way to type a member of a group nothing checked.
            if let Some((group, _)) = head.as_rec_proj() {
                let group = group.clone();
                check_group(kernel, &group, |_, _| Ok(()))?;
            }

            match synth_neutral(kernel, term)? {
                Some(type_) => Ok(type_),
                None => {
                    let _ = (head, levels);
                    Err(KernelError::Unclassified(term.clone()))
                }
            }
        }

        // Elaboration-only syntax. Reaching one means a term arrived here before elaboration was finished with it.
        Subterm::Metavar(_) | Subterm::Infix(_) | Subterm::NumLit(_) => {
            Err(KernelError::NotCore(term.clone()))
        }
    }
}

/// Establish that `type_` is a type by **typing** it, and hand back the sort it lands in.
///
/// The kernel had two ways to accept a type: this judgment, and `Sort::of`, which classifies a term structurally without typing it. Only one of them is a judgment, and the classifier was the one every caller used — so a declared type reached reduction, conversion and erasure having been *read* rather than *checked*, and every function downstream trusted a shape nothing had established. That is the root the motive clause, the β step, the elimination arm and the recursive twin all came through.
///
/// Coq's `type_of_case` and `infer_type` are this rule: compute the term's type, reduce it, destruct it as a sort. Lean's kernel enters through `inferType`; Agda carries the sort on the type itself so a type in hand is one that was checked. None of them has a second, weaker way to accept a type, and neither does this crate now.
///
/// **The reduction comes first, and it is not incidental.** `Sort::of` opens by reducing, and typing the unreduced spelling instead answers a different question: `Lst.{v,w}(Waker)` types as `Type v` — its former's promised codomain — while its reduced form `LstType(Waker)` is the minimal `Type 0` that the constructor size condition needs, and a computed type is only the shape it computes to once reduced. Without this line the standard library loses twelve items to the size condition and one to a projection through `/std/Fmt`.
///
/// That reduction is also why `whnf` must be *total* on arbitrary terms rather than merely correct on well-typed ones: this judgment hands it a term nothing has typed yet, by construction.
pub(super) fn infer_type(kernel: &mut Kernel, type_: &Term) -> Result<Sort, KernelError> {
    let reduced = kernel.reduce_forced(type_.clone())?;
    let inferred = infer(kernel, &reduced)?;

    as_sort(kernel, &inferred)
}

/// A motive is a claim the term makes about its own result, and two rules downstream read it: `infer` takes the elimination's type from it, and `Sort::of` classifies a type-valued `match` by it. Nothing established that the claim is true — the arms are checked *against* the motive, which a lie survives, because a motive reduces honestly at each arm's concrete case while reading as whatever it states at the abstract binders. So the motive is checked here, generically, and required to land in a sort.
///
/// Coq's `type_of_case` is this clause: compute the predicate's type, reduce it, `destSort` it, and refuse with a dedicated `error_elim_arity` otherwise. Without it a motive may state `Prop` while its arms inhabit `Type`, and `guard_large_elimination` — which returns immediately when the result is not relevant, because a proposition eliminated into a proposition needs no condition — never runs. That was a two-constructor proposition eliminated into `Nat`, certified with zero refusals.
///
/// The binders are the motive's real ones: the family's own index domains, then the scrutinee at the family instantiated *at those binders* rather than at the elimination's actual indices. Opening at the actuals instead would check the motive at one instance and miss exactly the lie, since a motive scrutinising its own index binder reduces once that index is concrete.
///
/// Placed before the dispatch in [`check_cases`], so it covers every `Cases` form with one clause and does not inherit `check_induct_arms`'s skip for a vacuous elimination: an elimination that cannot run still hands its caller a type read off this motive. No exploit through that path was demonstrated; what makes it unconditional is that the type propagates whether or not the elimination runs.
///
/// The sort it derives is handed to the guard, which used to re-ask `Sort::of` under binders it assumed at `Type` — a second reading of the same question, and the one that was wrong.
fn check_motive(
    kernel: &mut Kernel,
    family: Option<&InductType>,
    motive: &Scope<Many>,
    scrutinee_type: &Term,
) -> Result<Sort, KernelError> {
    kernel.scoped(|kernel| {
        let mut opened: Vec<Term> = Vec::new();

        match family {
            Some(family) => {
                let mut indices = kernel.induct_at(family)?.indices();
                while let Telescope::Cons(domain, rest) = indices {
                    let binder = kernel.fresh(rest.first_hint());
                    kernel.assume(&binder, &domain);
                    let occurrence = Term::free_var(&binder);
                    indices = rest.open(&[&occurrence]);
                    opened.push(occurrence);
                }

                let at_binders = Term::induct_type_at(
                    family.name.clone(),
                    family.universes.clone(),
                    family.params.clone(),
                    opened.clone(),
                );
                let binder = kernel.fresh(None);
                kernel.assume(&binder, &at_binders);
                opened.push(Term::free_var(&binder));
            }
            None => {
                let binder = kernel.fresh(None);
                kernel.assume(&binder, scrutinee_type);
                opened.push(Term::free_var(&binder));
            }
        }

        let refs = opened.iter().collect::<Vec<_>>();
        let body = motive.open(&refs);

        // A budget failure is not a malformed motive, so it keeps its own diagnostic; every other refusal is reported as the rule that was violated rather than as whichever mismatch happened to expose it.
        let type_ = match infer(kernel, &body) {
            Ok(type_) => type_,
            Err(error @ KernelError::Reduce(_)) => return Err(error),
            Err(_) => return Err(KernelError::NotAMotive(body.clone())),
        };

        // The motive's own sort *is* its type read as one: a body typed `Prop` is a proposition, a body typed `Type u` is relevant. So the well-formedness check and the answer the guard needs are one step.
        as_sort(kernel, &type_).map_err(|_| KernelError::NotAMotive(body.clone()))
    })
}

/// Check an elimination's arms against its motive.
///
/// A nominal elimination is verified in full, each arm at its own constructor's index targets. The primitive carriers are verified at their case values: `Bool`'s two literals, a `Switch`'s enumerated literals (its default at the scrutinee's own instance, the only one it has), and the free-monoid carriers' identity and cons arms — the typing face of the fact `uncons` computes with and `close` traverses by, that every carrier value is the identity or one generator over a shorter value. A variable scrutinee *is* the case's value within an arm, so every arm is checked with that equation substituted, the zero-index instance of the specialization `eliminate` gives nominal arms.
fn check_cases(
    kernel: &mut Kernel,
    family: Option<&InductType>,
    motive: &Scope<Many>,
    cases: &Cases,
    scrutinee: &Term,
    scrutinee_type: &Term,
) -> Result<(), KernelError> {
    let motive_sort = check_motive(kernel, family, motive, scrutinee_type)?;

    let at = |kernel: &mut Kernel, value: Term, body: &Term| {
        let expected = motive.open(&[&value]);

        kernel.scoped(|kernel| {
            let mut solutions = Vec::new();
            eliminate::assume_case_value(kernel, scrutinee, &value, &mut solutions);
            eliminate::shadow(kernel, &solutions);

            check(
                kernel,
                &eliminate::substitute(body, &solutions),
                &eliminate::substitute(&expected, &solutions),
            )
        })
    };

    match cases {
        Cases::Induct { cases, default } => {
            let Some(family) = family else {
                return Err(KernelError::Unclassified(scrutinee.clone()));
            };
            let at = kernel.induct_at(family)?;

            // A match with no arms and no catch-all is a vacuous elimination: the coverage loop must then prove *every* constructor impossible at the scrutinee's indices, so the eliminated instance is uninhabited and discharging it into a relevant result leaks nothing. The guard exists for eliminations that can run; this one cannot. It sits here rather than inside the arm rule because the sort it consumes is derived here, by `check_motive`, and a second derivation is what this whole clause exists to remove.
            if !(cases.is_empty() && default.is_none()) {
                eliminate::guard_large_elimination(kernel, &at, family, motive_sort)?;
            }

            check_induct_arms(
                kernel,
                &at,
                family,
                motive,
                cases,
                default.as_ref(),
                scrutinee,
            )
        }

        // The carrier a case form names is a claim about the scrutinee, established here for the same reason [`check_free_monoid`] establishes its own: the arms are typed at the case values while the result is typed at the motive of the *scrutinee*, so a form that does not match the carrier types the arms at one type and runs them at another.
        Cases::Bool {
            false_case,
            true_case,
        } => {
            if !matches!(&**scrutinee_type, Subterm::Prim(Prim::BoolType)) {
                return Err(KernelError::Unclassified(scrutinee_type.clone()));
            }

            at(kernel, Term::prim(Prim::Bool(false)), false_case)?;
            at(kernel, Term::prim(Prim::Bool(true)), true_case)
        }

        Cases::Switch { cases, default } => {
            if !matches!(&**scrutinee_type, Subterm::Prim(Prim::NatType)) {
                return Err(KernelError::Unclassified(scrutinee_type.clone()));
            }

            for (key, body) in cases {
                let literal = Term::prim(Prim::Nat(Nat::new(*key as usize)));
                at(kernel, literal, body)?;
            }

            // The default stands for every value not enumerated, so the only instance of the motive it can be checked at is the scrutinee's — which refines nothing.
            let expected = motive.open(&[scrutinee]);
            check(kernel, default, &expected)
        }

        Cases::FreeMonoid { carrier } => {
            check_free_monoid(kernel, motive, scrutinee, scrutinee_type, carrier, &at)
        }
    }
}

/// The free-monoid arm rule: the identity arm inhabits the motive at the carrier's empty value, and the cons arm — under a peeled generator, a tail, and an induction hypothesis at that tail — inhabits it at one generator prepended to the tail. The case values are spelled exactly as elaboration spelled them (`pred + 1`, the singleton-concat for `Lst`, the append-to-empty singleton for `Bin`, whose packed literals cannot hold a symbolic atom), and conversion's free-monoid peel is what makes those spellings and reduction's forms one normal form.
///
/// The carrier's own element type must agree with the scrutinee's: the arms are typed against the carrier's copy, and a value flowing through the match carries the scrutinee's, so a disagreement would type the arms at one type and run them at another.
fn check_free_monoid(
    kernel: &mut Kernel,
    motive: &Scope<Many>,
    scrutinee: &Term,
    scrutinee_type: &Term,
    carrier: &Carrier,
    at: &impl Fn(&mut Kernel, Term, &Term) -> Result<(), KernelError>,
) -> Result<(), KernelError> {
    // One cons arm: open the binders, assume them at the carrier's types with the induction hypothesis at the tail, and check the body at the motive of the cons value — with the scrutinee standing refined to that value, exactly as in every other arm.
    let cons = |kernel: &mut Kernel,
                binders: Vec<(&Free, Term)>,
                cons_value: Term,
                body: &Term|
     -> Result<(), KernelError> {
        kernel.scoped(|kernel| {
            for (binder, type_) in &binders {
                kernel.assume(binder, type_);
            }

            let expected = motive.open(&[&cons_value]);

            let mut solutions = Vec::new();
            eliminate::assume_case_value(kernel, scrutinee, &cons_value, &mut solutions);
            eliminate::shadow(kernel, &solutions);

            check(
                kernel,
                &eliminate::substitute(body, &solutions),
                &eliminate::substitute(&expected, &solutions),
            )
        })
    };

    match carrier {
        Carrier::Nat {
            empty_case,
            cons_case,
        } => {
            if !matches!(&**scrutinee_type, Subterm::Prim(Prim::NatType)) {
                return Err(KernelError::Unclassified(scrutinee_type.clone()));
            }

            at(kernel, Term::prim(Prim::Nat(Nat::new(0usize))), empty_case)?;

            let pred = kernel.fresh(cons_case.first_hint());
            let ih = kernel.fresh(cons_case.second_hint());
            let pred_occurrence = Term::free_var(&pred);
            let succ_value = Term::prim(Prim::nat_add(
                pred_occurrence.clone(),
                Term::prim(Prim::Nat(Nat::new(1usize))),
            ));
            let body = cons_case.open(&[&pred_occurrence, &Term::free_var(&ih)]);

            cons(
                kernel,
                vec![
                    (&pred, Term::prim(Prim::NatType)),
                    (&ih, motive.open(&[&pred_occurrence])),
                ],
                succ_value,
                &body,
            )
        }

        Carrier::Lst {
            elem,
            empty_case,
            cons_case,
        } => {
            let Subterm::Prim(Prim::LstType(scrutinee_elem)) = &**scrutinee_type else {
                return Err(KernelError::Unclassified(scrutinee_type.clone()));
            };
            if !convert(kernel, &Term::type_ground(), elem, scrutinee_elem)? {
                return Err(KernelError::Mismatch {
                    inferred: Box::new(elem.clone()),
                    expected: Box::new(scrutinee_elem.clone()),
                });
            }

            at(
                kernel,
                Term::prim(Prim::Lst(elem.clone(), Vec::new())),
                empty_case,
            )?;

            let head = kernel.fresh(cons_case.first_hint());
            let tail = kernel.fresh(cons_case.second_hint());
            let ih = kernel.fresh(cons_case.third_hint());
            let tail_occurrence = Term::free_var(&tail);
            let cons_value = Term::prim(Prim::LstConcat(
                elem.clone(),
                vec![
                    Term::prim(Prim::Lst(elem.clone(), vec![Term::free_var(&head)])),
                    tail_occurrence.clone(),
                ],
            ));
            let body = cons_case.open(&[
                &Term::free_var(&head),
                &tail_occurrence,
                &Term::free_var(&ih),
            ]);

            cons(
                kernel,
                vec![
                    (&head, elem.clone()),
                    (&tail, Term::prim(Prim::LstType(elem.clone()))),
                    (&ih, motive.open(&[&tail_occurrence])),
                ],
                cons_value,
                &body,
            )
        }

        Carrier::Bin {
            grain,
            empty_case,
            cons_case,
        } => {
            if !matches!(&**scrutinee_type, Subterm::Prim(Prim::BinType(found)) if found == grain) {
                return Err(KernelError::Unclassified(scrutinee_type.clone()));
            }

            let empty = Term::prim(Prim::Bin(*grain, PackedBin::empty()));
            at(kernel, empty.clone(), empty_case)?;

            let atom_type = Term::prim(match grain {
                Grain::X => Prim::ByteType,
                Grain::B => Prim::BoolType,
            });
            let head = kernel.fresh(cons_case.first_hint());
            let tail = kernel.fresh(cons_case.second_hint());
            let ih = kernel.fresh(cons_case.third_hint());
            let tail_occurrence = Term::free_var(&tail);
            let singleton = Term::prim(Prim::BinAppend(*grain, empty, Term::free_var(&head)));
            let cons_value = Term::prim(Prim::BinConcat(
                *grain,
                vec![singleton, tail_occurrence.clone()],
            ));
            let body = cons_case.open(&[
                &Term::free_var(&head),
                &tail_occurrence,
                &Term::free_var(&ih),
            ]);

            cons(
                kernel,
                vec![
                    (&head, atom_type),
                    (&tail, Term::prim(Prim::BinType(*grain))),
                    (&ih, motive.open(&[&tail_occurrence])),
                ],
                cons_value,
                &body,
            )
        }
    }
}

/// Verify that `term` has type `expected`.
pub fn check(kernel: &mut Kernel, term: &Term, expected: &Term) -> Result<(), KernelError> {
    // Seed for the erasure obligations, recorded before the rules below dispatch so a position counts however it is checked. Classified here rather than afterwards: the expectation routinely mentions binders this item opened, and they are retracted the moment its check returns, so nothing later can ask for their sorts. A memo keyed on the type keeps that to one question per distinct type.
    kernel.record_checked(term, expected);

    // A `let` carries no type of its own — the tail's type is the whole term's — so the expectation descends through it: the same binding validation as inference, with only the tail's mode changed. Without this, a dependent tuple or lambda under a `let` reaches the checked rules below as an inference and manufactures the non-dependent type they exist to avoid.
    if let Subterm::Let(Let { bindings, tail }) = &**term {
        let mut values = Vec::with_capacity(bindings.len());
        for binding in bindings {
            let refs = values.iter().collect::<Vec<_>>();
            let type_ = binding.type_().release(&refs);
            let value = binding.value().release(&refs);

            infer_type(kernel, &type_)?;
            check(kernel, &value, &type_)?;
            values.push(value);
        }

        let refs = values.iter().collect::<Vec<_>>();
        let tail = tail.open(&refs);

        return check(kernel, &tail, expected);
    }

    // The Π-introduction half of the checked rules below: a lambda checks against a function type by walking both telescopes under one shared binder set — each domain pair invariant by conversion, exactly as subsumption compares them — and checking the body against the expected codomain. Routing the body through `check` rather than inference is what lets a tuple body reach the Σ rule with its expectation intact; the inferred route would manufacture the non-dependent codomain first.
    if let Subterm::Func(Func {
        telescope,
        plicities,
    }) = &**term
    {
        let reduced = kernel.reduce_forced(expected.clone())?;
        if let Subterm::FuncType(expected_func) = &*reduced
            && *plicities == expected_func.plicities
            && telescope.len() == expected_func.telescope.len()
        {
            let lambda = telescope.clone();
            let against = expected_func.telescope.clone();
            return kernel.scoped(|kernel| check_lambda(kernel, lambda, against));
        }
        // Anything else — a non-Π expectation, a plicity or arity mismatch — falls through, so the refusal keeps its ordinary inferred-versus-expected shape.
    }

    // The Σ-introduction rule stated directly: a tuple literal checks against a dependent telescope entry-wise, each component at its entry's type opened over the actual preceding components. Inference cannot reach this verdict — inferring the components independently manufactures a non-dependent tuple type whose telescope binds nothing, and no conversion can relate that to a telescope whose later entries mention its binders.
    if let Subterm::Tuple(Tuple { fields, .. }) = &**term {
        match Term::unwrap_or_clone(kernel.reduce_forced(expected.clone())?) {
            Subterm::TupleType(TupleType { telescope }) => {
                return check_fields(kernel, fields, telescope);
            }
            Subterm::StructType(StructType {
                name,
                universes,
                params,
            }) => {
                let telescope = kernel.struct_at(&name, &universes, &params)?.fields();

                return check_fields(kernel, fields, telescope);
            }
            // Not a telescope-shaped expectation: fall through, so the mismatch keeps its ordinary inferred-versus-expected shape.
            _ => {}
        }
    }

    let inferred = infer(kernel, term)?;

    match subsumes(kernel, &inferred, expected)? {
        true => Ok(()),
        false => Err(KernelError::Mismatch {
            inferred: Box::new(inferred),
            expected: Box::new(expected.clone()),
        }),
    }
}

/// The telescope walk of the lambda rule above, under [`Kernel::scoped`]'s retraction bracket: the guards on plicity and arity ran at the dispatch, so the two telescopes are structurally parallel by construction.
fn check_lambda(
    kernel: &mut Kernel,
    lambda: Telescope<Term>,
    against: Telescope<Term>,
) -> Result<(), KernelError> {
    let (mut lambda, mut against) = (lambda, against);

    loop {
        match (lambda, against) {
            (Telescope::Cons(mine, mine_rest), Telescope::Cons(theirs, theirs_rest)) => {
                infer_type(kernel, &mine)?;
                if !convert(kernel, &Term::type_ground(), &mine, &theirs)? {
                    return Err(KernelError::Mismatch {
                        inferred: Box::new(mine),
                        expected: Box::new(theirs),
                    });
                }

                let binder = kernel.fresh(mine_rest.first_hint());
                kernel.assume(&binder, &theirs);
                let occurrence = Term::free_var(&binder);

                lambda = mine_rest.open(&[&occurrence]);
                against = theirs_rest.open(&[&occurrence]);
            }
            (Telescope::Done(body), Telescope::Done(codomain)) => {
                return check(kernel, &body, &codomain);
            }
            _ => unreachable!("the dispatch guarded the arities equal"),
        }
    }
}

/// The entry-wise walk of the tuple rule above: each component checked at its entry's type, the telescope then opened at that *actual* component so later entries see the value the binder stands for.
fn check_fields(
    kernel: &mut Kernel,
    fields: &[Term],
    mut telescope: Telescope<()>,
) -> Result<(), KernelError> {
    if telescope.len() != fields.len() {
        return Err(KernelError::Arity {
            expected: telescope.len(),
            actual: fields.len(),
        });
    }

    for field in fields {
        let Telescope::Cons(type_, rest) = telescope else {
            unreachable!("the arity guard bounds the walk to the telescope's length");
        };
        check(kernel, field, &type_)?;
        telescope = rest.open(&[field]);
    }

    Ok(())
}

/// Whether a term of type `inferred` may stand where `expected` is wanted — the subsumption relation `inferred ≤ expected`.
///
/// This states cumulativity as a rule rather than leaving it to a traversal order. `Γ ⊢ t : A` and `A ≤ B` give `Γ ⊢ t : B`, and `≤` is:
///
/// ```text
/// Type u ≤ Type v          when the level algebra proves u ≤ v
/// Prop   ≤ Type v          a proposition stands wherever a type is wanted
/// Π(x:A).B ≤ Π(x:A').B'    when A ≡ A' and, under x, B ≤ B'
/// A      ≤ B              otherwise, when A ≡ B
/// ```
///
/// **Domains are invariant, codomains cumulative.** Comparing domains by conversion rather than contravariantly is the choice Coq makes, and it is the freely-revisable side of the fork: widening to contravariance later accepts strictly more, so it breaks nothing already accepted, while shipping contravariance and withdrawing it would break programs.
///
/// The elaborator reaches the same verdicts by a different route — it is bidirectional, so checking a λ against a Π pushes the comparison down to the leaves, where both sides are sorts and the head rule suffices, and it never forms the Π being subsumed here. Deciding this structurally instead is what makes the rule readable, and what lets the two checkers disagree if elaboration's traversal order ever changes.
fn subsumes(kernel: &mut Kernel, inferred: &Term, expected: &Term) -> Result<bool, KernelError> {
    kernel.spend()?;

    let lower = kernel.reduce_forced(inferred.clone())?;
    let upper = kernel.reduce_forced(expected.clone())?;

    match (&*lower, &*upper) {
        (Subterm::Type(lower), Subterm::Type(upper)) => return Ok(kernel.level_leq(lower, upper)),
        // `Prop : Type 0`, and a proposition is admitted wherever a type is.
        (Subterm::Prop, Subterm::Type(_)) => return Ok(true),
        // Plicity is part of a function type's identity, exactly as in `convert`: `(A) -> A` and `(@A) -> A` have different calling conventions, so a difference there is a mismatch and not a codomain question.
        (Subterm::FuncType(lower), Subterm::FuncType(upper))
            if lower.plicities == upper.plicities =>
        {
            let (lower, upper) = (lower.telescope.clone(), upper.telescope.clone());

            return kernel.scoped(|kernel| subsumes_telescope(kernel, lower, upper));
        }
        _ => {}
    }

    convert(kernel, &Term::type_ground(), inferred, expected)
}

/// [`subsumes`] through a function type's telescope: each domain by conversion, the terminal codomains by subsumption, under one shared set of binders.
///
/// Opening both sides at the *same* occurrence is what makes the codomain comparison meaningful — the domains have just been shown convertible, so a single binder stands for both.
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
            // Different arities. A function type is not curried in this representation, so this is a real mismatch rather than a shape to normalize.
            _ => return Ok(false),
        }
    }
}

/// Check that every domain of a λ's telescope is a type, then its body under those binders, rebuilding the telescope as the Π the λ inhabits.
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
            infer_type(kernel, &domain)?;

            let binder = kernel.fresh(rest.first_hint());
            let inner = kernel.scoped(|kernel| {
                kernel.assume(&binder, &domain);

                infer_telescope(kernel, rest.open(&[&Term::free_var(&binder)]))
            });

            Ok(Telescope::Cons(
                domain,
                Scope::close(One, &[&binder], inner?),
            ))
        }
    }
}
