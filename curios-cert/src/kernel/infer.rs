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
//! Beyond those, the kernel refuses whatever it cannot determine. A type whose sort is unclear, a nominal name with no declaration: each is a refusal, never a default. The reason is in the [`kernel`](super) module documentation — a guessed answer from a second opinion is worse than no second opinion.

mod eliminate;
use eliminate::*;

mod intrinsic;
use intrinsic::*;

#[cfg(test)]
mod declaration_tests;
#[cfg(test)]
mod intrinsic_tests;
#[cfg(test)]
mod signature_tests;
#[cfg(test)]
mod sort_tests;
#[cfg(test)]
mod structure_tests;
#[cfg(test)]
mod test_support;
#[cfg(test)]
mod typing_tests;

use {
    super::{
        Counted, Error, Kernel, Sort, check_group, convert::convert, sort::as_sort,
        sort::infer_sort, synth_neutral,
    },
    curios_analysis::spine,
    curios_core::{
        Bound, Carrier, Cases, Cost, Field, Free, Func, FuncType, InductType, Instance,
        InstanceHead, Intrinsic, Let, Lockstep, Many, MatchResult, Nat, Produced, Proj, Rec,
        Reducer, Scope, Step, Struct, StructType, Subterm, Telescope, Term, Tuple, TupleType,
        Variant, foreign_signature,
    },
    curios_num::{Binary, Grain, Natural},
    curios_utilities::recurse,
};

/// The type of `term`.
///
/// A child position is checked by descending into it. `check` is `infer` followed by `subsumes`, so `infer → check → infer` costs two native frames per link of a right-nested chain, and a `Str` literal's UTF-8 derivation is one such link per byte — depth as a function of the *data* rather than of what anyone wrote. [`recurse`] is what makes that affordable; a budget cannot, since a budget bounds steps and depth is not steps.
///
/// Deferring children onto an explicit worklist instead would change the *rule*: a deferred child is inferred and subsumed, which skips the three checked rules `check` dispatches first — let-descent, Π-introduction, Σ-introduction — and the deferred positions are exactly arguments, constructor payloads and record fields, so a lambda or a dependent tuple in argument position would take the inferred route and manufacture the non-dependent type those rules exist to avoid. See `documentation/design/soundness/introduction/checked-rules-at-deferred-child-positions.md`.
///
/// A local-free term's type is remembered for the rest of the declaration, as its reduct is (`Kernel::infer_hit`): a term the kernel types can be a graph whose tree is exponential in its depth — a text position built a character at a time mentions the one before it four times, and a claim about text in a type carries such values, which its print does not finish unfolding — so typing it per path is exponential in the claim's length. The position a hit answers is still recorded as checked, and the positions inside it were recorded when it was first typed.
pub fn infer(kernel: &mut Kernel, term: &Term) -> Result<Term, Error> {
    recurse(|| {
        // Taken before the memo is consulted, so a hit cannot leave it standing for the next judgment. A hit records no call either, and need not: a local-free term names no member.
        let spine_head = kernel.calls.take_spine_head();
        // Whether a group that does not descend is typed inside this term, read off the count before and after. A hit types nothing, so it counts again what the term's first typing closed.
        let before = kernel.partial_groups();
        let inferred = match kernel.infer_hit(term) {
            Some(inferred) => {
                kernel.calls.recall_partial(term);

                inferred
            }
            None => {
                let inferred = infer_within(kernel, term, spine_head)?;
                if kernel.partial_groups() > before {
                    kernel.calls.remember_partial(term);
                }
                kernel.infer_store(term.clone(), inferred.clone());

                inferred
            }
        };
        // Seed for the erasure obligations, at an *inferred* position. A term's type is its type however the judgment arrived at it, so a proof reached only by inference — a match scrutinee, most consequentially — is a proof position exactly as a checked one is. Recording only checked positions would leave a diverging proof in a scrutinee unseeded, and the elimination would conjure a relevant value from it.
        let position = kernel.record_checked(term, &inferred);
        if kernel.partial_groups() > before {
            kernel.enclose_partial(position);
        }

        Ok(inferred)
    })
}

/// [`infer`]'s rules, one per term form. `spine_head` says `term` is the head of an application spine, whose call the spine records whole.
fn infer_within(kernel: &mut Kernel, term: &Term, spine_head: bool) -> Result<Term, Error> {
    kernel.spend(Cost::STEP)?;

    match &**term {
        // `Type u : Type (u + 1)`, and `Prop : Type 0`. The hierarchy is what makes `Type : Type` — and Girard's paradox with it — unstatable.
        Subterm::Type(level) => Ok(Term::type_at(level.succ()?)),
        Subterm::Prop => Ok(Term::type_ground()),

        Subterm::Intrinsic(intrinsic) => infer_intrinsic(kernel, intrinsic),

        // A host call described by its ABI row, typed through the same walk an intrinsic is: each operand checks against what `foreign_signature` demands of it, and the result — unit, a bare value, or a named record, inside `Io` — is read off the same row. The row, not this crate, states the signature, and for a builtin the row is the roster's, reached through the identity the term carries.
        Subterm::Foreign(function, args) => {
            let signature = foreign_signature(function, args, |label| kernel.fresh(Some(label)));

            if args.len() != signature.operands.len() {
                return Err(Error::Arity {
                    counted: Counted::Arguments,
                    expected: signature.operands.len(),
                    actual: args.len(),
                });
            }

            check_operands(kernel, args, &signature.operands)?;

            let Produced::Fixed(type_) = signature.produced else {
                unreachable!("a foreign call produces a fixed `Io` type");
            };

            Ok(type_)
        }

        // A variable has the type it was bound or declared at. There is no fallback: an unbound name in a finished term is a broken term.
        //
        // A member of a group being checked, named anywhere but at the head of an application, is a call no size relation can be read off — passed along, it may be applied to anything — so it is recorded with no arguments, which grades as unknown throughout.
        Subterm::Var(var) => {
            if !spine_head && let Some(free) = var.as_free() {
                kernel.record_call(free, &[])?;
            }

            kernel
                .type_of(var.unwrap())?
                .cloned()
                .ok_or_else(|| Error::Unbound(*var.unwrap()))
        }

        // A type former is a type, at the universe its parts join to — computed by the judgment role, which types those parts, rather than by the lookup, which only classifies them.
        Subterm::FuncType(_) | Subterm::TupleType(_) => Ok(infer_sort(kernel, term)?.term()),

        // λ: check each domain is a type, then the body under those binders. The result is the Π over the same telescope.
        Subterm::Func(func) => infer_lambda(kernel, func, &[]),

        // Application: the head must be a function of matching arity, each argument checks against its domain, and the result is the codomain with the arguments substituted — which is where dependency lives.
        //
        // A lambda applied on the spot, inside a group's body, is typed with each binder standing for its argument, for the call recorder alone: a call inside reads its arguments as what they are rather than as fresh binders nothing is below, which is what keeps an arm generalized over a hypothesis and applied back to it from hiding the descent it carries. Outside every group there is no call to read, and the lambda takes the ordinary route, memo included. Every other head is typed as the head of this spine, whose call is recorded here whole once the arguments are typed.
        Subterm::Apply(apply) => {
            let head_type = match &*apply.head {
                Subterm::Func(func) if kernel.recording() => {
                    let arguments = apply.params().cloned().collect::<Vec<_>>();
                    let before = kernel.partial_groups();
                    let head_type = infer_lambda(kernel, func, &arguments)?;
                    let position = kernel.record_checked(&apply.head, &head_type);
                    if kernel.partial_groups() > before {
                        kernel.enclose_partial(position);
                    }

                    head_type
                }
                _ => {
                    kernel.calls.set_spine_head();
                    infer(kernel, &apply.head)?
                }
            };

            let Subterm::FuncType(FuncType { telescope, .. }) =
                Term::unwrap_or_clone(kernel.reduce_forced(head_type.clone())?)
            else {
                return Err(Error::NotAFunction(head_type));
            };

            if telescope.len() != apply.arguments.len() {
                return Err(Error::Arity {
                    counted: Counted::Arguments,
                    expected: telescope.len(),
                    actual: apply.arguments.len(),
                });
            }

            let mut cursor = telescope.cursor();
            for param in apply.params() {
                let (_, domain) = cursor.entry().expect("arity was checked above");

                check(kernel, param, &domain)?;
                cursor.advance(param.clone());
            }

            if !spine_head {
                let (head, arguments) = spine(term);
                if let Subterm::Var(var) = &*head
                    && let Some(free) = var.as_free()
                {
                    kernel.record_call(free, &arguments)?;
                }
            }

            Ok(cursor.body().expect("arity was checked above"))
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
                return Err(Error::Unclassified(term.clone()));
            };

            let head_type = infer(kernel, head)?;

            match Term::unwrap_or_clone(kernel.reduce_forced(head_type.clone())?) {
                Subterm::TupleType(TupleType { telescope }) => {
                    let len = telescope.len();
                    telescope.field_type_from(head, *index).ok_or(Error::Arity {
                        counted: Counted::Components,
                        expected: *index + 1,
                        actual: len,
                    })
                }
                Subterm::StructType(StructType {
                    name,
                    universes,
                    params,
                }) => {
                    let fields = kernel.struct_at(&name, &universes, &params)?.fields();
                    let len = fields.len();
                    fields.field_type_from(head, *index).ok_or(Error::Arity {
                        counted: Counted::Components,
                        expected: *index + 1,
                        actual: len,
                    })
                }
                _ => Err(Error::NotATuple(head_type)),
            }
        }

        // A fully applied nominal family has the sort its declaration states, and its arguments have the types the declaration states *them* at.
        //
        // Every rule that consults a declaration reads those arguments — `Sort::of` for the sort, the arm rule for a constructor's signature, inversion for its index targets, `induct_type_args` for a comparison — and each reads them at the declared domain. Nothing established they inhabit it. Counts are the boundary's job and are checked there; the shapes are typing's, and reading one unestablished would admit `Eq(@True)(0, 1)` as a type: `0` and `1` are `Nat`s claiming a `Prop`-sorted domain, so `induct_type_args` would discharge both by irrelevance and `refl` would inhabit the forgery, whose elimination then transports between two instances of a relevant family.
        Subterm::InductType(family) => {
            let sort = infer_sort(kernel, term)?;
            let at = kernel.induct_at(family)?;

            check_along(kernel, at.parameters(), &family.params)?;
            // The index telescope arrives already opened at those parameters, which is what makes a later index able to mention an earlier one.
            check_along(kernel, at.indices(), &family.indices)?;

            Ok(sort.term())
        }

        Subterm::StructType(StructType {
            name,
            universes,
            params,
        }) => {
            let sort = infer_sort(kernel, term)?;
            let at = kernel.struct_at(name, universes, params)?;

            check_along(kernel, at.parameters(), params)?;

            Ok(sort.term())
        }

        // A constructor application: its signature, instantiated at the declaration's parameters, ends in the type it constructs — including the index targets this particular case aims at.
        //
        // The parameters are typed first, as an occurrence's are, and for the same reason: the signature is read at them and the constructed type carries them, so every rule that meets the value reads them at the declared domains. Only counted, a value would hand over its family's arguments on its own word — `false` where the declaration says `Nat` — and nothing downstream would look again.
        Subterm::Variant(Variant {
            name,
            universes,
            params,
            tag,
            payload,
        }) => {
            // The handle checks the universe instance and the parameter count before any of the declaration is read at them. `open_params` is tolerant — too few parameters leaves the declaration's own parameter binders unopened, so they read as payload slots and the arity check below would compare against the wrong number.
            let at = kernel.induct_at_params(name, universes, params)?;
            check_along(kernel, at.parameters(), params)?;
            let signature = at.signature(tag).ok_or_else(|| Error::Undeclared(*name))?;

            if signature.len() != payload.len() {
                return Err(Error::Arity {
                    counted: Counted::Payload,
                    expected: signature.len(),
                    actual: payload.len(),
                });
            }

            // The constructed type, rebuilt from what the terminal states and what the declaration already fixes: this family, at the parameters this occurrence supplied.
            let targets = check_along(kernel, signature, payload)?;
            Ok(Subterm::InductType(InductType {
                name: *name,
                universes: universes.clone(),
                params: params.clone(),
                indices: targets,
            })
            .into())
        }

        // A nominal record: its parameters check against the declaration's, as a constructor application's do, its fields against the field telescope at them, and its type is the family at the same parameters.
        Subterm::Struct(Struct {
            name,
            universes,
            params,
            fields,
            ..
        }) => {
            let at = kernel.struct_at(name, universes, params)?;
            check_along(kernel, at.parameters(), params)?;
            let telescope = at.fields();

            if telescope.len() != fields.len() {
                return Err(Error::Arity {
                    counted: Counted::Fields,
                    expected: telescope.len(),
                    actual: fields.len(),
                });
            }

            check_along(kernel, telescope, fields)?;

            Ok(Subterm::StructType(StructType {
                name: *name,
                universes: universes.clone(),
                params: params.clone(),
            })
            .into())
        }

        // An elimination's type is its result at this scrutinee: a motive binds the family's indices and then the scrutinee itself, so opening it at those is the rule for the *type*; an ambient goal is that type as written.
        //
        // Whether the term deserves that type is `eliminate`'s job: each arm must inhabit the result at its own constructor's index targets, and a proposition may not be eliminated into a relevant result unless it carries nothing to extract.
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

            if let Some(motive) = m.result.family()
                && motive.arity() != indices.len() + 1
            {
                return Err(Error::Arity {
                    counted: Counted::MotiveBinders,
                    expected: indices.len() + 1,
                    actual: motive.arity(),
                });
            }

            check_cases(
                kernel,
                family.as_ref(),
                &m.result,
                &m.cases,
                &m.head,
                &scrutinee_type,
            )?;

            Ok(m.result.of(&m.head, &indices))
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
        //
        // The occurrence is the head's, so a member named here is the call the `Var` rule above records, on the same reading: the levels spell the occurrence rather than stand between it and the member. A member reached only through this spelling would leave its group closing with no call at all, which is the one direction the recorder must not take. A projection head names its own group, which is certified below rather than recorded, since the group being checked holds its members as locals.
        Subterm::Instance(Instance { head, .. }) => {
            if !spine_head && let Some(free) = head.head_name() {
                kernel.record_call(free, &[])?;
            }

            // `synth_neutral` reads a projection's type off the group it carries, which is a lookup and must stay one — so the group is certified *here*, before the read, at the generic spelling the instance was taken from. Skipping this would leave the instance spelling as a way to type a member of a group nothing checked.
            if let InstanceHead::RecProj(group, _) = head {
                let group = group.clone();
                check_group(kernel, &group, |_, _| Ok(()))?;
            }

            match synth_neutral(kernel, term)? {
                Some(type_) => Ok(type_),
                None => Err(Error::Unclassified(term.clone())),
            }
        }

        // Elaboration-only syntax. Reaching a metavariable or any transient means a term arrived here before elaboration was finished with it.
        Subterm::Metavar(_) | Subterm::Transient(_) => Err(Error::NotCore(term.clone())),
    }
}

/// Establish that `type_` is a type by **typing** it, and hand back the sort it lands in.
///
/// `Sort::of` classifies a term structurally without typing it, and only this is a judgment: a declared type accepted by the classifier would reach reduction, conversion and erasure having been *read* rather than *checked*, and every function downstream would trust a shape nothing established.
///
/// Coq's `type_of_case` and `infer_type` are this rule: compute the term's type, reduce it, destruct it as a sort. Lean's kernel enters through `inferType`; Agda carries the sort on the type itself so a type in hand is one that was checked. None of them has a second, weaker way to accept a type, and neither does this crate.
///
/// **The type is typed as written**, never its reduct, which would accept what a redex dropped — an argument or an arm — with nothing having typed it. The elaborator settles an occurrence at its recorded floor, its argument's level (`curios-elab`'s `UniverseSolver::finalize`), so a written type lands in the sort its reduct does and the constructor size condition loses nothing to reading it.
pub(super) fn infer_type(kernel: &mut Kernel, type_: &Term) -> Result<Sort, Error> {
    let inferred = infer(kernel, type_)?;

    as_sort(kernel, &inferred)
}

/// A motive is a claim the term makes about its own result, and two rules downstream read it: `infer` takes the elimination's type from it, and `Sort::of` classifies a type-valued `match` by it. Nothing established that the claim is true — the arms are checked *against* the motive, which a lie survives, because a motive reduces honestly at each arm's concrete case while reading as whatever it states at the abstract binders. So the motive is checked here, generically, and required to land in a sort.
///
/// Coq's `type_of_case` is this clause: compute the predicate's type, reduce it, `destSort` it, and refuse with a dedicated `error_elim_arity` otherwise. Without it a motive may state `Prop` while its arms inhabit `Type`, and `guard_large_elimination` — which returns immediately when the result is not relevant, because a proposition eliminated into a proposition needs no condition — never runs, which would certify a two-constructor proposition eliminated into `Nat`.
///
/// The binders are the motive's real ones: the family's own index domains, then the scrutinee at the family instantiated *at those binders* rather than at the elimination's actual indices. Opening at the actuals instead would check the motive at one instance and miss exactly the lie, since a motive scrutinising its own index binder reduces once that index is concrete.
///
/// Placed before the dispatch in [`check_cases`], so it covers every `Cases` form with one clause and does not inherit `check_induct_arms`'s skip for a vacuous elimination: an elimination that cannot run still hands its caller a type read off this motive. No exploit through that path was demonstrated; what makes it unconditional is that the type propagates whether or not the elimination runs.
///
/// The sort it derives is handed to the guard, so the question is asked once, under the real binders.
fn check_motive(
    kernel: &mut Kernel,
    family: Option<&InductType>,
    result: &MatchResult,
    scrutinee_type: &Term,
) -> Result<Sort, Error> {
    // An ambient goal was typed where it stands, so its well-formedness is asked in the ambient context and under no binder; what the guard needs is still its sort.
    let motive = match result {
        MatchResult::Family(motive) => motive,
        MatchResult::Ambient(goal) => return motive_sort(kernel, goal),
    };

    kernel.scoped(|kernel| {
        let mut opened: Vec<Term> = Vec::new();

        match family {
            Some(family) => {
                let indices = kernel.induct_at(family)?.indices();
                let mut cursor = indices.cursor();
                while let Some((_, domain)) = cursor.entry() {
                    let binder = kernel.advance_assumed(&mut cursor, &domain);
                    opened.push(Term::free_var(&binder));
                }

                let at_binders = Term::induct_type_at(
                    family.name,
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

        motive_sort(kernel, &body)
    })
}

/// The sort `stated` lands in, where `stated` is a motive's body under its binders or an ambient goal where it stands.
///
/// The motive's own sort *is* its type read as one: a body typed `Prop` is a proposition, a body typed `Type u` is relevant. So the well-formedness check and the answer the guard needs are one step.
///
/// A budget failure is not a malformed motive, so it keeps its own diagnostic — on the way to the type and on the way from the type to its sort alike, the second being a reduction as the first is. Every other refusal is reported as the rule that was violated rather than as whichever mismatch happened to expose it.
fn motive_sort(kernel: &mut Kernel, stated: &Term) -> Result<Sort, Error> {
    let refusal = |error| match error {
        error @ Error::Reduce(_) => error,
        _ => Error::NotAMotive(stated.clone()),
    };

    let type_ = infer(kernel, stated).map_err(refusal)?;
    as_sort(kernel, &type_).map_err(refusal)
}

/// Check an elimination's arms against its motive.
///
/// A nominal elimination is verified in full, each arm at its own constructor's index targets. The intrinsic carriers are verified at their case values: `Bool`'s two literals, a `Switch`'s enumerated literals (its default at the scrutinee's own instance, the only one it has), and the free-monoid carriers' identity and cons arms — the typing face of the fact `uncons` computes with and `close` traverses by, that every carrier value is the identity or one generator over a shorter value. A variable scrutinee *is* the case's value within an arm, so every arm is checked with that equation substituted, the zero-index instance of the specialization `eliminate` gives nominal arms.
fn check_cases(
    kernel: &mut Kernel,
    family: Option<&InductType>,
    result: &MatchResult,
    cases: &Cases,
    scrutinee: &Term,
    scrutinee_type: &Term,
) -> Result<(), Error> {
    let motive_sort = check_motive(kernel, family, result, scrutinee_type)?;

    let at = |kernel: &mut Kernel, value: Term, body: &Term| {
        let expected = result.at(scrutinee, &[], &[], &value);

        kernel.scoped(|kernel| {
            kernel.assume_guard(scrutinee, &value)?;
            let mut solutions = Vec::new();
            eliminate::assume_case_value(kernel, scrutinee, &value, &mut solutions)?;
            kernel.assume_arm(scrutinee, &value, &solutions)?;
            eliminate::shadow(kernel, &solutions);

            check(
                kernel,
                &body.substitute(&solutions),
                &expected.substitute(&solutions),
            )
        })
    };

    match cases {
        Cases::Induct { cases, default } => {
            let Some(family) = family else {
                return Err(Error::Unclassified(scrutinee.clone()));
            };
            let at = kernel.induct_at(family)?;

            // A match with no arms and no catch-all is a vacuous elimination: the coverage loop must then prove *every* constructor impossible at the scrutinee's indices, so the eliminated instance is uninhabited and discharging it into a relevant result leaks nothing. The guard exists for eliminations that can run; this one cannot. It sits here rather than inside the arm rule because the sort it consumes is derived here, by `check_motive`, and a second derivation is what this clause exists to avoid.
            if !(cases.is_empty() && default.is_none()) {
                eliminate::guard_large_elimination(kernel, &at, family, motive_sort)?;
            }

            check_induct_arms(
                kernel,
                &at,
                family,
                result,
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
            if !matches!(&**scrutinee_type, Subterm::Intrinsic(Intrinsic::BoolType)) {
                return Err(Error::Unclassified(scrutinee_type.clone()));
            }

            at(kernel, Term::intrinsic(Intrinsic::Bool(false)), false_case)?;
            at(kernel, Term::intrinsic(Intrinsic::Bool(true)), true_case)
        }

        Cases::Switch { cases, default } => {
            if !matches!(&**scrutinee_type, Subterm::Intrinsic(Intrinsic::NatType)) {
                return Err(Error::Unclassified(scrutinee_type.clone()));
            }

            for (key, body) in cases {
                let literal = Term::intrinsic(Intrinsic::Nat(Nat::new(key.clone())));
                at(kernel, literal, body)?;
            }

            // The default stands for every value not enumerated, so the only instance of the result it can be checked at is the scrutinee's — which refines nothing. Enumerating zero is what rules zero out of it, which a call inside may descend on.
            let expected = result.of(scrutinee, &[]);
            kernel.scoped(|kernel| {
                if let Subterm::Var(var) = &**scrutinee
                    && let Some(binder) = var.as_free()
                    && cases.iter().any(|(key, _)| key.is_zero())
                {
                    kernel.assume_nonzero(*binder)?;
                }

                check(kernel, default, &expected)
            })
        }

        Cases::FreeMonoid { carrier } => {
            check_free_monoid(kernel, result, scrutinee, scrutinee_type, carrier, &at)
        }
    }
}

/// Whether a fold's cons arm reads its induction hypothesis, the last binder its scope closes over.
fn reads_hypothesis(carrier: &Carrier) -> bool {
    match carrier {
        Carrier::Nat { cons_case, .. } => cons_case.uses(1),
        Carrier::Bin { cons_case, .. } | Carrier::List { cons_case, .. } => cons_case.uses(2),
    }
}

/// The free-monoid arm rule: the identity arm inhabits the result at the carrier's empty value, and the cons arm — under a peeled generator, a tail, and the induction hypothesis at that tail when the arm reads it — inhabits it at one generator prepended to the tail. The case values are spelled exactly as elaboration spelled them (`pred + 1`, the singleton-concat for `List`, the append-to-empty singleton for `Bin`, whose packed literals cannot hold a symbolic atom), and conversion's free-monoid peel is what makes those spellings and reduction's forms one normal form.
///
/// **The hypothesis is assumed exactly when the arm reads it, and only a family types it**, at the motive opened at the tail — the rule erasure's split-or-fold reading and the elaborator's state too. Its two preconditions therefore bind exactly the folds that read it: an ambient goal has no tail to be taken at once the head is substituted away, so it cannot type one ([`Error::AmbientFold`]); and a family that reaches the scrutinee other than through its binder would type it at the arm's own goal ([`Error::FoldMotiveCapturesScrutinee`]). An arm that reads none — a case split — is checked as a `Bool` arm is, at its case value under either form of result: its reduct `arm(h, t, fold(t))` never contains the fold at the tail, so nothing needs the type the hypothesis would have had.
///
/// The carrier's own element type must agree with the scrutinee's: the arms are typed against the carrier's copy, and a value flowing through the match carries the scrutinee's, so a disagreement would type the arms at one type and run them at another.
fn check_free_monoid(
    kernel: &mut Kernel,
    result: &MatchResult,
    scrutinee: &Term,
    scrutinee_type: &Term,
    carrier: &Carrier,
    at: &impl Fn(&mut Kernel, Term, &Term) -> Result<(), Error>,
) -> Result<(), Error> {
    let motive = match (reads_hypothesis(carrier), result) {
        (false, _) => None,
        (true, MatchResult::Ambient(goal)) => return Err(Error::AmbientFold(goal.clone())),
        // The capture is refused syntactically — `match n : (_) => Eq()(n, 0) | 0 => refl | k + 1; ih => ih end` would prove `Eq()(n, 0)` for every `n` — which is exact for a variable scrutinee and, for an expression, covers every occurrence the case equation recorded against that spelling could reach.
        (true, MatchResult::Family(motive)) => {
            if motive.body().mentions_term(scrutinee) {
                return Err(Error::FoldMotiveCapturesScrutinee(scrutinee.clone()));
            }
            Some(motive)
        }
    };

    // The hypothesis at a tail, for an arm that reads one.
    let hypothesis = |tail: &Term| motive.map(|motive| motive.open(&[tail]));

    // One cons arm: open the binders, assume them at the carrier's types, and check the body at the result of the cons value — with the scrutinee standing refined to that value, exactly as in every other arm.
    //
    // `size_value` is the cons value as the call recorder reads a size: the same value, spelled as the carrier's layered form where the typing's spelling is an operation the size order does not read through.
    let cons = |kernel: &mut Kernel,
                binders: Vec<(&Free, Term)>,
                cons_value: Term,
                size_value: Term,
                body: &Term|
     -> Result<(), Error> {
        kernel.scoped(|kernel| {
            for (binder, type_) in &binders {
                kernel.assume(binder, type_);
            }

            let expected = result.at(scrutinee, &[], &[], &cons_value);

            let mut solutions = Vec::new();
            eliminate::assume_case_value(kernel, scrutinee, &cons_value, &mut solutions)?;
            kernel.assume_arm(scrutinee, &size_value, &solutions)?;
            eliminate::shadow(kernel, &solutions);

            check(
                kernel,
                &body.substitute(&solutions),
                &expected.substitute(&solutions),
            )
        })
    };

    match carrier {
        Carrier::Nat {
            empty_case,
            cons_case,
        } => {
            if !matches!(&**scrutinee_type, Subterm::Intrinsic(Intrinsic::NatType)) {
                return Err(Error::Unclassified(scrutinee_type.clone()));
            }

            at(
                kernel,
                Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
                empty_case,
            )?;

            let pred = kernel.fresh(cons_case.first_hint());
            let ih = kernel.fresh(cons_case.second_hint());
            let pred_occurrence = Term::free_var(&pred);
            let succ_value = Term::intrinsic(Intrinsic::nat_add(
                pred_occurrence.clone(),
                Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
            ));
            let body = cons_case.open(&[&pred_occurrence, &Term::free_var(&ih)]);

            // `pred + 1` as one successor layer over `pred`, which is how the size order reads a `Nat`.
            let size_value = Term::intrinsic(Intrinsic::Nat(Nat::Succ(
                Natural::from(1usize),
                pred_occurrence.clone(),
            )));
            let mut binders = vec![(&pred, Term::intrinsic(Intrinsic::NatType))];
            binders.extend(hypothesis(&pred_occurrence).map(|type_| (&ih, type_)));

            cons(kernel, binders, succ_value, size_value, &body)
        }

        Carrier::List {
            elem,
            empty_case,
            cons_case,
        } => {
            let Subterm::Intrinsic(Intrinsic::ListType(scrutinee_elem)) = &**scrutinee_type else {
                return Err(Error::Unclassified(scrutinee_type.clone()));
            };
            if !convert(kernel, &Term::type_ground(), elem, scrutinee_elem)? {
                return Err(Error::Mismatch {
                    inferred: Box::new(elem.clone()),
                    expected: Box::new(scrutinee_elem.clone()),
                });
            }

            at(
                kernel,
                Term::intrinsic(Intrinsic::List {
                    element: elem.clone(),
                    items: Vec::new(),
                }),
                empty_case,
            )?;

            let head = kernel.fresh(cons_case.first_hint());
            let tail = kernel.fresh(cons_case.second_hint());
            let ih = kernel.fresh(cons_case.third_hint());
            let tail_occurrence = Term::free_var(&tail);
            let cons_value = Term::intrinsic(Intrinsic::ListConcat {
                element: elem.clone(),
                operands: vec![
                    Term::intrinsic(Intrinsic::List {
                        element: elem.clone(),
                        items: vec![Term::free_var(&head)],
                    }),
                    tail_occurrence.clone(),
                ],
            });
            let body = cons_case.open(&[
                &Term::free_var(&head),
                &tail_occurrence,
                &Term::free_var(&ih),
            ]);

            let mut binders = vec![
                (&head, elem.clone()),
                (&tail, Term::intrinsic(Intrinsic::ListType(elem.clone()))),
            ];
            binders.extend(hypothesis(&tail_occurrence).map(|type_| (&ih, type_)));

            cons(kernel, binders, cons_value.clone(), cons_value, &body)
        }

        Carrier::Bin {
            grain,
            empty_case,
            cons_case,
        } => {
            if !matches!(&**scrutinee_type, Subterm::Intrinsic(Intrinsic::BinType(found)) if found == grain)
            {
                return Err(Error::Unclassified(scrutinee_type.clone()));
            }

            let empty = Term::intrinsic(Intrinsic::Bin(*grain, Binary::empty()));
            at(kernel, empty.clone(), empty_case)?;

            let atom_type = Term::intrinsic(match grain {
                Grain::X => Intrinsic::ByteType,
                Grain::B => Intrinsic::BoolType,
            });
            let head = kernel.fresh(cons_case.first_hint());
            let tail = kernel.fresh(cons_case.second_hint());
            let ih = kernel.fresh(cons_case.third_hint());
            let tail_occurrence = Term::free_var(&tail);
            let singleton = Term::intrinsic(Intrinsic::BinAppend {
                grain: *grain,
                bin: empty,
                element: Term::free_var(&head),
            });
            let cons_value = Term::intrinsic(Intrinsic::BinConcat {
                grain: *grain,
                operands: vec![singleton, tail_occurrence.clone()],
            });
            let body = cons_case.open(&[
                &Term::free_var(&head),
                &tail_occurrence,
                &Term::free_var(&ih),
            ]);

            let mut binders = vec![
                (&head, atom_type),
                (&tail, Term::intrinsic(Intrinsic::BinType(*grain))),
            ];
            binders.extend(hypothesis(&tail_occurrence).map(|type_| (&ih, type_)));

            cons(kernel, binders, cons_value.clone(), cons_value, &body)
        }
    }
}

/// Verify that `term` has type `expected`.
pub fn check(kernel: &mut Kernel, term: &Term, expected: &Term) -> Result<(), Error> {
    // Whether this check opens a member body's leading lambdas — taken first, so it holds for this term alone and the λ rule is the one reader that carries it on.
    let parameters = kernel.calls.take_parameters();

    // Seed for the erasure obligations, recorded before the rules dispatch so a position counts however it is checked. Classified here rather than afterwards: the expectation routinely mentions binders this item opened, and they are retracted the moment its check returns, so nothing later can ask for their sorts. A memo keyed on the type keeps that to one question per distinct type.
    let before = kernel.partial_groups();
    let position = kernel.record_checked(term, expected);
    let checked = check_rules(kernel, term, expected, parameters);
    // Whether a group that does not descend was typed inside this term — noted on the position, since the group was typed with the binders around it opened and the position's term holds it closed.
    if kernel.partial_groups() > before {
        kernel.enclose_partial(position);
    }

    checked
}

/// [`check`]'s rules, past the position it records: `let`'s descent, Π- and Σ-introduction, and inference for the rest.
fn check_rules(
    kernel: &mut Kernel,
    term: &Term,
    expected: &Term,
    parameters: bool,
) -> Result<(), Error> {
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
    if let Subterm::Func(func) = &**term {
        let reduced = kernel.reduce_forced(expected.clone())?;
        if let Subterm::FuncType(expected_func) = &*reduced
            && func.plicities() == expected_func.plicities()
            && func.telescope.len() == expected_func.telescope.len()
        {
            let lambda = func.telescope.clone();
            let against = expected_func.telescope.clone();
            return kernel.scoped(|kernel| check_lambda(kernel, lambda, against, parameters));
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
        false => Err(Error::Mismatch {
            inferred: Box::new(inferred),
            expected: Box::new(expected.clone()),
        }),
    }
}

/// The telescope walk of the lambda rule above, under [`Kernel::scoped`]'s retraction bracket: the guards on plicity and arity ran at the dispatch, so the two telescopes are structurally parallel by construction.
///
/// `parameters` says this λ leads a member body, so each binder it opens is one of that member's parameters to the call recorder, and so is each binder a λ leading its body opens in turn.
fn check_lambda(
    kernel: &mut Kernel,
    lambda: Telescope<Term>,
    against: Telescope<Term>,
    parameters: bool,
) -> Result<(), Error> {
    let mut walk = Lockstep::new(&lambda, &against);

    loop {
        match walk.step() {
            Step::Entries {
                left: mine,
                right: theirs,
                ..
            } => {
                infer_type(kernel, &mine)?;
                if !convert(kernel, &Term::type_ground(), &mine, &theirs)? {
                    return Err(Error::Mismatch {
                        inferred: Box::new(mine),
                        expected: Box::new(theirs),
                    });
                }
                let binder = kernel.advance_assumed(&mut walk, &theirs);
                if parameters {
                    kernel.bind_parameter(&binder);
                }
            }
            Step::Bodies(body, codomain) => {
                if parameters {
                    kernel.calls.continue_parameters();
                }
                return check(kernel, &body, &codomain);
            }
            Step::Mismatch => unreachable!("the dispatch guarded the arities equal"),
        }
    }
}

/// The entry-wise walk of the tuple rule above: each component checked at its entry's type, the telescope then opened at that *actual* component so later entries see the value the binder stands for.
fn check_fields(
    kernel: &mut Kernel,
    fields: &[Term],
    telescope: Telescope<()>,
) -> Result<(), Error> {
    if telescope.len() != fields.len() {
        return Err(Error::Arity {
            counted: Counted::Fields,
            expected: telescope.len(),
            actual: fields.len(),
        });
    }

    let mut cursor = telescope.cursor();
    for field in fields {
        let (_, type_) = cursor
            .entry()
            .expect("the arity guard bounds the walk to the telescope's length");
        check(kernel, field, &type_)?;
        cursor.advance(field.clone());
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
fn subsumes(kernel: &mut Kernel, inferred: &Term, expected: &Term) -> Result<bool, Error> {
    kernel.spend(Cost::STEP)?;

    let lower = kernel.reduce_forced(inferred.clone())?;
    let upper = kernel.reduce_forced(expected.clone())?;

    match (&*lower, &*upper) {
        (Subterm::Type(lower), Subterm::Type(upper)) => return Ok(kernel.level_leq(lower, upper)),
        // `Prop : Type 0`, and a proposition is admitted wherever a type is.
        (Subterm::Prop, Subterm::Type(_)) => return Ok(true),
        // Plicity is part of a function type's identity, exactly as in `convert`: `(A) -> A` and `(@A) -> A` have different calling conventions, so a difference there is a mismatch and not a codomain question.
        (Subterm::FuncType(lower), Subterm::FuncType(upper))
            if lower.plicities() == upper.plicities() =>
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
) -> Result<bool, Error> {
    let mut walk = Lockstep::new(&this, &that);

    loop {
        match walk.step() {
            Step::Entries { left, right, .. } => {
                if !convert(kernel, &Term::type_ground(), &left, &right)? {
                    return Ok(false);
                }
                kernel.advance_assumed(&mut walk, &left);
            }
            Step::Bodies(left, right) => return subsumes(kernel, &left, &right),
            // Different arities. A function type is not curried in this representation, so this is a real mismatch rather than a shape to normalize.
            Step::Mismatch => return Ok(false),
        }
    }
}

/// Check each of `arguments` against its domain in `telescope`, each later domain opened at the arguments before it, and hand back the terminal opened at all of them.
///
/// The caller has established that the counts agree — a handle counted an occurrence's parameters or indices, and the arity checks count a payload or a record's fields — so the walk never runs out of entries.
fn check_along<B: Bound>(
    kernel: &mut Kernel,
    telescope: Telescope<B>,
    arguments: &[Term],
) -> Result<B, Error> {
    let mut cursor = telescope.cursor();
    for argument in arguments {
        let (_, domain) = cursor.entry().expect("the caller checked the count");

        check(kernel, argument, &domain)?;
        cursor.advance(argument.clone());
    }

    Ok(cursor.body().expect("the caller checked the count"))
}

/// The Π a λ inhabits, with each binder standing for the corresponding one of `arguments` where the λ is applied on the spot.
fn infer_lambda(kernel: &mut Kernel, func: &Func, arguments: &[Term]) -> Result<Term, Error> {
    let telescope = infer_telescope(kernel, func.telescope.clone(), arguments)?;

    Ok(Subterm::FuncType(FuncType::new(telescope, func.plicities().to_vec())).into())
}

/// Check that every domain of a λ's telescope is a type, then its body under those binders, rebuilding the telescope as the Π the λ inhabits.
///
/// One walk under one retraction bracket, each domain opened once at the binders before it and the Π built in one pass at the end — where recursing into the reopened rest and re-closing each level would rewrite the whole inner telescope once per binder.
///
/// A binder with an argument in `arguments` stands for it within the bracket, to the call recorder alone — a size fact, never an equation the typing sees, so the Π built is the λ's own.
fn infer_telescope(
    kernel: &mut Kernel,
    telescope: Telescope<Term>,
    arguments: &[Term],
) -> Result<Telescope<Term>, Error> {
    kernel.scoped(|kernel| {
        let mut entries = Vec::new();
        let mut cursor = telescope.cursor();

        while let Some((_, domain)) = cursor.entry() {
            infer_type(kernel, &domain)?;

            let binder = kernel.advance_assumed(&mut cursor, &domain);
            if let Some(argument) = arguments.get(entries.len()) {
                kernel.refine_size(&binder, argument)?;
            }
            entries.push((binder, domain));
        }

        let body = cursor.body().expect("a cursor past every entry");
        let type_ = infer(kernel, &body)?;

        Ok(Telescope::build(entries, type_))
    })
}
