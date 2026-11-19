//! The typing judgment: what type a term has, and whether it has the one it was supposed to.
//!
//! This is the rule set. Everything else in the certifier exists to serve it — `whnf` so a type can be looked at, `Sort::of` so a proposition can be recognized, `convert` so two types can be compared. What is written here is the language's typing rules, one per term form, and it is meant to be read that way.
//!
//! # Bidirectional, but only just
//!
//! Two judgments: [`infer`] synthesizes a term's type, and [`check`] verifies a term against a type it is given. The elaborator's versions of these carry a great deal more — implicit-argument insertion, metavariable invention, postponement, overload resolution — because their job is to *make* a well-typed term out of what someone wrote. The certifier's job is to look at a finished term and say yes or no, so [`check`] is almost trivial: infer, then ask whether the result is subsumed by the expectation. The interesting content is all in [`infer`], which is where it belongs.
//!
//! # What the certifier refuses
//!
//! Three term forms are elaboration-only — a metavariable, an unresolved infix operator, and a polymorphic numeric literal — and reaching one means a term arrived here before elaboration finished with it. Refusing them is what makes "the certifier checks finished terms" a checked statement rather than a convention.
//!
//! Beyond those, the certifier refuses whatever it cannot determine. A type whose sort is unclear, a nominal name with no declaration: each is a refusal, never a default. The reason is in the [`certifier`](super) module documentation — a guessed answer from a second opinion is worse than no second opinion.

mod eliminate;
use eliminate::*;
pub use eliminate::{enter_arm, enter_case};

mod intrinsic;
use intrinsic::*;

#[cfg(test)]
mod test_support;
#[cfg(test)]
mod tests;

use {
    super::{
        Certifier, Counted, Error, Handed, Sort, as_sort, check_group, convert, field_handed,
        infer_sort, synth_neutral,
    },
    curios_analysis::{Segment, call_path, catch_all_tests},
    curios_common::recurse,
    curios_core::{
        Bound, Carrier, Cases, Cost, Field, Foreign, Free, Func, FuncType, InductType, Intrinsic,
        LetBinding, Lockstep, Many, Nat, Produced, Proj, Quality, Reducer, Scope, Step, Struct,
        StructType, Subterm, Telescope, Term, Totality, Tuple, TupleType, Variant,
        foreign_operands, foreign_signature, goal_at,
    },
    curios_num::{Binary, Natural},
};

/// The type of `term`.
///
/// A child position is checked by descending into it. `check` is `infer` followed by `subsumes`, so `infer → check → infer` costs two native frames per link of a right-nested chain, and a `Str` literal's UTF-8 derivation is one such link per byte — depth as a function of the *data* rather than of what anyone wrote. [`recurse`] is what makes that affordable; a budget cannot, since a budget bounds steps and depth is not steps.
///
/// Deferring children onto an explicit worklist instead would change the *rule*: a deferred child is inferred and subsumed, which skips the three checked rules `check` dispatches first — let-descent, Π-introduction, Σ-introduction — and the deferred positions are exactly arguments, constructor payloads and record fields, so a lambda or a dependent tuple in argument position would take the inferred route and manufacture the non-dependent type those rules exist to avoid.
///
/// A term's type is remembered as its reduct is (`Certifier::infer_hit`) — for the rest of the declaration where the term is local-free, and while the scope it was read off stands where it names a local or stands under an arm: a term the certifier types can be a graph whose tree is exponential in its depth — a text position built a character at a time mentions the one before it four times, and a claim about text in a type carries such values, which its print does not finish unfolding — so typing it per path is exponential in the claim's length. The position a hit answers is still recorded as checked, and the positions inside it were recorded when it was first typed: a refused walk that takes them back takes the typing with them (`Certifier::tried`).
pub fn infer(certifier: &mut Certifier, term: &Term) -> Result<Term, Error> {
    recurse(|| {
        // Taken before the memo is consulted, so a hit cannot leave it standing for the next judgment. A hit records no call either, and need not: a term naming a member whose body is being checked is never one.
        let spine_head = certifier.calls.take_spine_head();
        // Whether a group that does not descend is typed inside this term, read off the count before and after. A hit types nothing, so it counts again what the term's first typing closed.
        let before = certifier.partial_groups();
        let inferred = match certifier.infer_hit(term) {
            Some((inferred, closed)) => {
                if closed {
                    certifier.calls.recall_partial();
                }

                inferred
            }
            None => {
                let inferred = certifier
                    .typing_once(term, |certifier| infer_within(certifier, term, spine_head))?;
                let closed = certifier.partial_groups() > before;
                certifier.infer_store(term.clone(), inferred.clone(), closed);

                inferred
            }
        };
        // Seed for the erasure obligations, at an *inferred* position. A term's type is its type however the judgment arrived at it, so a proof reached only by inference — a match scrutinee, most consequentially — is a proof position exactly as a checked one is. Recording only checked positions would leave a diverging proof in a scrutinee unseeded, and the elimination would conjure a relevant value from it.
        let position = certifier.record_checked(term, &inferred);
        if certifier.partial_groups() > before {
            certifier.enclose_partial(position);
        }

        Ok(inferred)
    })
}

/// [`infer`]'s rules, one per term form. `spine_head` says `term` is the head of an application spine, whose call the spine records whole.
fn infer_within(certifier: &mut Certifier, term: &Term, spine_head: bool) -> Result<Term, Error> {
    certifier.spend(Cost::STEP)?;

    // A recursive group is read by value: its verdict and its members' are filed under the group, and the obligations look them up from recorded positions, which are by value.
    let by_value;
    let term = match &**term {
        Subterm::Rec(_) => {
            by_value = certifier.by_value(term);
            &by_value
        }
        _ => term,
    };

    match &**term {
        // `Type u : Type (u + 1)`, and `Prop : Type 0`. The hierarchy is what makes `Type : Type` — and Girard's paradox with it — unstatable.
        Subterm::Sort(curios_core::Sort {
            quality: Quality::Type | Quality::Prop,
            level,
        }) => Ok(Term::type_at(level.succ()?)),
        Subterm::Sort(curios_core::Sort {
            quality: Quality::Param(_) | Quality::Meta(_),
            level: _,
        }) => Err(Error::OpenSort(term.clone())),

        Subterm::Intrinsic(intrinsic) => infer_intrinsic(certifier, intrinsic),

        // A host call described by its ABI row, typed through the same walk an intrinsic is: each operand checks against what `foreign_signature` demands of it, and the result — unit, a bare value, or a named record, inside `Io` — is read off the same row. The row, not this crate, states the signature.
        Subterm::Foreign(Foreign {
            function,
            arguments: args,
            ..
        }) => {
            let syntax = certifier.syntax();
            // Counted before the row's result is read, which a row that does not return reads off the call's first operand.
            let expected = foreign_operands(function, &syntax).len();
            if args.len() != expected {
                return Err(Error::Arity {
                    counted: Counted::Arguments,
                    expected,
                    actual: args.len(),
                });
            }

            let signature = foreign_signature(function, args, &syntax, |label| {
                certifier.fresh(Some(label))
            });
            check_operands(certifier, args, &signature.operands)?;

            let Produced::Fixed(type_) = signature.produced else {
                unreachable!("a foreign call produces a fixed `Io` type");
            };

            Ok(type_)
        }

        // A variable has the type it was bound at. There is no fallback: an unbound name in a finished term is a broken term.
        //
        // A member of a group being checked, named anywhere but at the head of an application, is a call no size relation can be read off — passed along, it may be applied to anything — so it is recorded with no arguments, which grades as unknown throughout; save where it is handed to a definition that applies it as its summary states, whose applications are the calls it makes ([`Certifier::record_handed`]).
        //
        // A *bound* index reaching here is the other broken term, and it is refused rather than looked up: a term closed under the scopes above it has none, which the boundary holds every position to (`curios_core::open_positions_of`), and the scope holds no type for a variable with no identity to look one up by (`Var::as_free`). The depth is all the refusal can name, the index having no identity, and it is the rule's own as much as the boundary's — a rule that aborted where a boundary already refused would die before the verdict was read.
        Subterm::Var(var) => {
            let Some(free) = var.as_free() else {
                return Err(Error::NotClosed {
                    reach: term.reach(),
                });
            };

            if !spine_head {
                match certifier.calls.take_handed(free) {
                    Some(handed) => certifier.record_handed(free, None, &handed)?,
                    None => certifier.record_call(free, &[])?,
                }
            }

            certifier
                .local_type(free)
                .cloned()
                .ok_or(Error::Unbound(*free))
        }

        // A type former is a type, at the universe its parts join to — computed by the judgment role, which types those parts, rather than by the lookup, which only classifies them.
        Subterm::FuncType(_) | Subterm::TupleType(_) => Ok(infer_sort(certifier, term)?.term()),

        // λ: check each domain is a type, then the body under those binders. The result is the Π over the same telescope.
        Subterm::Func(func) => infer_lambda(certifier, func, &[]),

        // Application: the head must be a function of matching arity, each argument checks against its domain, and the result is the codomain with the arguments substituted — which is where dependency lives.
        //
        // A lambda applied on the spot, inside a group's body, is typed with each binder standing for its argument, for the call recorder alone: a call inside reads its arguments as what they are rather than as fresh binders nothing is below, which is what keeps an arm generalized over a hypothesis and applied back to it from hiding the descent it carries. Outside every group there is no call to read, and the lambda takes the ordinary route, memo included. Every other head is typed as the head of this spine, whose call is recorded here whole once the arguments are typed.
        //
        // A curried spine is read whole, as the elaborator's walk reads it: the head of `h(a)(b)` is typed with `[b]` applied after it, so a lambda applied on the spot binds its inner lambda's binders to the later lists too, and a callee's summary grades every list's arguments, the head's own taken there and each later list's taken by the application around it once its head is typed. Nothing else reads the later lists, so a spine inside an argument starts from none.
        Subterm::Apply(apply) => {
            let later = certifier.calls.take_later();
            // A member applied to its own arguments, handed where its callee applies it, takes what it is handed before its arguments are typed, so no member local inside them takes it.
            let path = (!spine_head).then(|| call_path(term));
            let member_handed = path.as_ref().and_then(|(head, steps)| match &**head {
                Subterm::Var(var)
                    if steps
                        .iter()
                        .all(|step| matches!(step, Segment::Arguments(_))) =>
                {
                    let free = var.as_free()?;
                    Some((*free, certifier.calls.take_handed(free)?))
                }
                _ => None,
            });
            let mut from_head = Vec::new();
            let head_type = match &*apply.head {
                Subterm::Func(func) if certifier.recording() => {
                    // The argument lists before the first field the spine projects: what the lambda's binders, and its inner lambdas', stand for.
                    let arguments = apply
                        .params()
                        .chain(
                            later
                                .iter()
                                .map_while(|step| match step {
                                    Segment::Arguments(arguments) => Some(arguments),
                                    Segment::Field(_) => None,
                                })
                                .flatten(),
                        )
                        .cloned()
                        .collect::<Vec<_>>();
                    let before = certifier.partial_groups();
                    let head_type = infer_lambda(certifier, func, &arguments)?;
                    let position = certifier.record_checked(&apply.head, &head_type);
                    if certifier.partial_groups() > before {
                        certifier.enclose_partial(position);
                    }

                    head_type
                }
                Subterm::Apply(_) | Subterm::Proj(_) => {
                    let mut steps = vec![Segment::Arguments(apply.params().cloned().collect())];
                    steps.extend(later.iter().cloned());
                    certifier.calls.set_later(steps);
                    certifier.calls.set_spine_head();
                    let head_type = infer(certifier, &apply.head);
                    // A head answered from the memo takes neither, so neither outlives it.
                    certifier.calls.take_later();
                    from_head = certifier.calls.take_rest_handed();
                    head_type?
                }
                _ => {
                    certifier.calls.set_spine_head();
                    infer(certifier, &apply.head)?
                }
            };

            let reduced = certifier.reduce_forced(head_type.clone())?;
            let Subterm::FuncType(FuncType { telescope, .. }) = &*reduced else {
                return Err(Error::NotAFunction(head_type));
            };

            if telescope.len() != apply.arguments.len() {
                return Err(Error::Arity {
                    counted: Counted::Arguments,
                    expected: telescope.len(),
                    actual: apply.arguments.len(),
                });
            }

            // A definition applying a parameter as its summary states hands a lambda there bounds for its binders, and a member there the calls it makes, over its spine's every list: this list's positions taken here, and the rest left for the applications around this one.
            let (mut handed, rest) = match &*apply.head {
                Subterm::Instance(instance) => {
                    let mut steps = vec![Segment::Arguments(apply.params().cloned().collect())];
                    steps.extend(later.iter().cloned());
                    let mut handed = certifier.handed_to(&instance.name, &steps)?;
                    let rest = handed.split_off(apply.arguments.len().min(handed.len()));
                    (handed, rest)
                }
                Subterm::Apply(_) | Subterm::Proj(_) => {
                    let rest = from_head.split_off(apply.arguments.len().min(from_head.len()));
                    (from_head, rest)
                }
                _ => (Vec::new(), Vec::new()),
            };

            let mut cursor = telescope.cursor();
            for (position, argument) in apply.arguments.iter().enumerate() {
                let (_, domain) = cursor.entry().expect("arity was checked above");
                // An argument stands under its parameter's mark. Held here, where the parameter is at hand, so a spine's marks are a function of its head's type and conversion has nothing to read in them.
                let declared = cursor.mark().expect("arity was checked above");
                if argument.plicity != declared {
                    return Err(Error::Mark {
                        counted: Counted::Arguments,
                        position: position + 1,
                        declared,
                        written: argument.plicity,
                    });
                }

                if let Some(handed) = handed.get_mut(position).and_then(Option::take) {
                    certifier.calls.hand(&argument.term, handed);
                }
                let checked = check(certifier, &argument.term, &domain);
                certifier.calls.clear_handed();
                checked?;
                cursor.advance(argument.term.clone());
            }

            if !rest.is_empty() {
                certifier.calls.leave_rest_handed(rest);
            }

            if let Some((head, steps)) = &path
                && let Subterm::Var(var) = &**head
                && let Some(free) = var.as_free()
            {
                match member_handed {
                    Some((_, handed)) => {
                        let given = steps
                            .iter()
                            .flat_map(Segment::arguments)
                            .cloned()
                            .collect::<Vec<_>>();
                        certifier.record_handed(free, Some(&given), &handed)?
                    }
                    None => certifier.record_call(free, steps)?,
                }
            }

            Ok(cursor.body().expect("arity was checked above"))
        }

        // A tuple's type is the Σ over its components' types. Non-dependent: a component's type is inferred in the scope it stands in, so nothing here can make a later component depend on an earlier one. A term that needs that dependency carries the Σ and is *checked* against it.
        Subterm::Tuple(Tuple { fields, .. }) => {
            let mut entries = Vec::with_capacity(fields.len());
            for field in fields {
                entries.push((certifier.fresh(None), infer(certifier, field)?));
            }

            Ok(Term::tuple_type(entries))
        }

        // Projection: the component's type, with earlier components named by projections of this same head — which is what makes a Σ dependent.
        //
        // A slot says what it is a slot of, and the head's type has to be that: a tuple's off a tuple, a structure's off a value of the structure it names. The name is what a reader of the term reads the field by, with no type in hand, so a slot naming one structure over a value of another would be read as a field the value does not have.
        Subterm::Proj(Proj { head, field }) => {
            let Some(index) = field.slot() else {
                return Err(Error::Unclassified(term.clone()));
            };
            let out_of_range = |len| Error::Arity {
                counted: Counted::Components,
                expected: index + 1,
                actual: len,
            };

            // A projection is a step of the path its head starts, typed as part of it, and the path's call is recorded where the path ends: a member's field applied is a call of the part it reaches, and one named unapplied a call it cannot grade ([`Certifier::record_call`]).
            // The steps after this projection, its own field first, go to its head where the head is a step of the same path, an application or a projection, as an application hands its own, so a definition's call the path roots at reads the part it reaches; whatever the head hands the argument lists after them stays for the application around this one. Any other head — a `let`, a match — types its applications as spines of their own, which reach no field of this path.
            let later = certifier.calls.take_later();
            if !later.is_empty() && matches!(&**head, Subterm::Apply(_) | Subterm::Proj(_)) {
                let mut steps = vec![Segment::Field(index)];
                steps.extend(later);
                certifier.calls.set_later(steps);
            }
            certifier.calls.set_spine_head();
            let head_type = infer(certifier, head);
            // A head answered from the memo takes none, so none outlives it.
            certifier.calls.take_later();
            let head_type = head_type?;
            if !spine_head {
                let (root, steps) = call_path(term);
                if let Subterm::Var(var) = &*root
                    && let Some(free) = var.as_free()
                {
                    certifier.record_call(free, &steps)?;
                }
            }
            let reduced = certifier.reduce_forced(head_type.clone())?;

            match (&*reduced, field) {
                (Subterm::TupleType(TupleType { telescope }), Field::Tuple(_)) => {
                    let len = telescope.len();
                    telescope
                        .field_type_from(head, field)
                        .ok_or_else(|| out_of_range(len))
                }
                (
                    Subterm::StructType(StructType {
                        name,
                        universes,
                        params,
                    }),
                    Field::Struct(structure, _),
                ) if name == structure => {
                    let at = certifier.struct_at(name, universes, params)?;
                    at.field_type(certifier, head, field)?
                        .ok_or_else(|| out_of_range(at.field_count(certifier)))
                }
                (Subterm::TupleType(_) | Subterm::StructType(_), _) => Err(Error::NotASlotOf {
                    named: match field {
                        Field::Struct(structure, _) => Some(*structure),
                        _ => None,
                    },
                    head_type,
                }),
                _ => Err(Error::NotATuple(head_type)),
            }
        }

        // A fully applied nominal family has the sort its declaration states, and its arguments have the types the declaration states *them* at.
        //
        // Every rule that consults a declaration reads those arguments — `Sort::of` for the sort, the arm rule for a constructor's signature, inversion for its index targets, `induct_type_args` for a comparison — and each reads them at the declared domain. Nothing established they inhabit it. Counts are the boundary's job and are checked there; the shapes are typing's, and reading one unestablished would admit `Eq(@True)(0, 1)` as a type: `0` and `1` are `Nat`s claiming a `Prop`-sorted domain, so `induct_type_args` would discharge both by irrelevance and `refl` would inhabit the forgery, whose elimination then transports between two instances of a relevant family.
        Subterm::InductType(family) => {
            let sort = infer_sort(certifier, term)?;
            let at = certifier.induct_at(family)?;

            check_along(certifier, at.parameters(), &family.params)?;
            // The index telescope arrives already opened at those parameters, which is what makes a later index able to mention an earlier one.
            check_along(certifier, at.indices(), &family.indices)?;

            Ok(sort.term())
        }

        Subterm::StructType(StructType {
            name,
            universes,
            params,
        }) => {
            let sort = infer_sort(certifier, term)?;
            let at = certifier.struct_at(name, universes, params)?;

            check_along(certifier, at.parameters(certifier)?, params)?;

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
            // The handle checks the universe instance and the parameter count before any of the declaration is read at them: a case's payload opens at exactly its family's parameters, asserting their count.
            let at = certifier.induct_at_params(name, universes, params)?;
            check_along(certifier, at.parameters(), params)?;
            let signature = at
                .signature(certifier, tag)?
                .ok_or_else(|| Error::UndeclaredCase {
                    family: *name,
                    tag: tag.clone(),
                })?;

            if signature.len() != payload.len() {
                return Err(Error::Arity {
                    counted: Counted::Payload,
                    expected: signature.len(),
                    actual: payload.len(),
                });
            }

            // The constructed type, rebuilt from what the terminal states and what the declaration already fixes: this family, at the parameters this occurrence supplied.
            let targets = check_along(certifier, signature, payload)?;
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
            let record = certifier.calls.take_record();
            let literal = certifier.calls.take_literal();
            let at = certifier.struct_at(name, universes, params)?;
            check_along(certifier, at.parameters(certifier)?, params)?;
            let telescope = at.fields(certifier)?;

            if telescope.len() != fields.len() {
                return Err(Error::Arity {
                    counted: Counted::Fields,
                    expected: telescope.len(),
                    actual: fields.len(),
                });
            }

            let mut cursor = telescope.cursor();
            for (slot, field) in fields.iter().enumerate() {
                let (_, domain) = cursor.entry().expect("the arity guard bounds the walk");
                check_field(certifier, slot, field, &domain, record, literal.as_ref())?;
                cursor.advance(field.clone());
            }

            Ok(Subterm::StructType(StructType {
                name: *name,
                universes: universes.clone(),
                params: params.clone(),
            })
            .into())
        }

        // An elimination's type is its goal as written.
        //
        // Whether the term deserves that type is `eliminate`'s job: each arm must inhabit the goal at its own constructor's index targets, and a proposition may not be eliminated into a relevant result unless it carries nothing to extract.
        //
        // **The scrutinee is typed where it stands and read by value, and so is the goal.** The rule decides by the scrutinee's spelling — a variable is solved and the goal read over it, an expression has its equation recorded and the goal's occurrences of it replaced. A `let`-bound name in either would hide what each of those reads: under `let m = n`, an arm of `match n` solves `n`, and a goal that spells `m` is one the solution does not reach. So both are read as a substituted `let` leaves them, and the arms are typed by name.
        Subterm::Match(m) => {
            let scrutinee_type = infer(certifier, &m.head)?;
            let scrutinee_type = certifier.reduce_forced(scrutinee_type)?;
            let head = certifier.by_value(&m.head);
            // What an arm establishes of the scrutinee's size is established of it as a graded call reads it, a local an index inversion solved standing as written, since the size order reads nothing an inversion concluded.
            let sized = certifier.graded(&m.head);
            let goal = certifier.by_value(&m.goal);
            let family = match &*scrutinee_type {
                Subterm::InductType(family) => Some(family.clone()),
                _ => None,
            };

            check_cases(
                certifier,
                family.as_ref(),
                &goal,
                &m.cases,
                &head,
                &sized,
                &scrutinee_type,
            )?;

            Ok(goal)
        }

        // The tail's type may name the binders, which close with the `let`: it is stated by value, while they stand.
        Subterm::Let(block) => {
            let (bindings, tail) = (block.bindings(), block.tail());
            under_bindings(certifier, bindings, tail, |certifier, tail| {
                let inferred = infer(certifier, tail)?;

                Ok(certifier.by_value(&inferred))
            })
        }

        // A recursive group, checked by the one rule that holds it — asked here rather than restated, so a `rec` in a term and a `rec` at the top level cannot come to disagree about what makes a group legal.
        //
        // **A member is bound in the tail, as a `let` binds.** The group's bodies are checked over members that are locals, which is what keeps a body from re-entering its own check — each unfolding in reduction once its turn has found it known to terminate, and standing for nothing until then (`check_group`); the tail is what the group is in scope of, and there a member is its folded selection, the term a `rec` reduces its tail over ([`unfold_rec`](super::unfold_rec)) and the one a top-level member is defined as. Held opaque there, a member computes nowhere in the function that declares it: `f(4)` is no number, and a term that spells the member by value — the elaborator defines it so, and a solution commits what it reduced — never converts with its name.
        Subterm::Rec(block) => {
            let (group, tail) = (block.group(), block.tail());
            check_group(certifier, group, |certifier, names| {
                let total = certifier.group_verdict(group) == Some(Totality::Total);
                for (index, (name, member)) in names.iter().zip(group.members()).enumerate() {
                    let partial = !total || certifier.member_encloses_partial(group, index);
                    certifier.bind(name, &group.member_type(index), &member, partial);
                }
                let members = names.iter().map(Term::free_var).collect::<Vec<_>>();
                let refs = members.iter().collect::<Vec<_>>();

                let type_ = infer(certifier, &tail.open(&refs))?;

                // The tail's type may name the members, whose binders retract with the group's scope: it is stated by value, as a `let`'s is, and the folded spelling a member stands for needs nothing in scope.
                Ok(certifier.by_value(&type_))
            })
        }

        // A polymorphic declaration at a stated instance: its scheme, substituted. It is no call a group records, since a group's members are assumed under locals while its bodies are checked, and an instance names a declaration.
        Subterm::Instance(instance) => match synth_neutral(certifier, term)? {
            Some(type_) => Ok(type_),
            // An instance's type is its scheme's, so one with none names no declaration in scope.
            None => Err(Error::Undeclared(instance.name)),
        },

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
pub fn infer_type(certifier: &mut Certifier, type_: &Term) -> Result<Sort, Error> {
    let inferred = infer(certifier, type_)?;

    as_sort(certifier, &inferred)
}

/// A goal is a claim the term makes about its own type, and two rules downstream read it: `infer` takes the elimination's type from it, and `Sort::of` classifies a type-valued `match` by it. So it is checked here, where it stands, and required to land in a sort, whose answer the large-elimination guard needs: a goal stating `Prop` while its arms inhabit `Type` would skip the guard, which returns immediately when the result is not relevant, and certify a two-constructor proposition eliminated into `Nat`. Coq's `type_of_case` is this clause.
///
/// Placed before the dispatch in [`check_cases`], so it covers every `Cases` form with one clause and does not inherit `check_induct_arms`'s skip for a vacuous elimination: an elimination that cannot run still hands its caller its goal as its type.
///
/// The goal's own sort *is* its type read as one: a goal typed `Prop` is a proposition, a goal typed `Type u` is relevant. So the well-formedness check and the answer the guard needs are one step.
///
/// A budget failure is not a malformed goal, so it keeps its own diagnostic — on the way to the type and on the way from the type to its sort alike, the second being a reduction as the first is. Every other refusal is reported as the rule that was violated rather than as whichever mismatch happened to expose it.
fn goal_sort(certifier: &mut Certifier, goal: &Term) -> Result<Sort, Error> {
    let refusal = |error| match error {
        error @ Error::Reduce(_) => error,
        refusal => Error::NotAGoal {
            stated: goal.clone(),
            refusal: Box::new(refusal),
        },
    };

    let type_ = infer(certifier, goal).map_err(refusal)?;
    as_sort(certifier, &type_).map_err(refusal)
}

/// Check an elimination's arms against its goal.
///
/// A nominal elimination is verified in full, each arm at its own constructor's index targets. The intrinsic carriers are verified at their case values: `Bool`'s two literals, a `Switch`'s enumerated literals (its default at the scrutinee's own instance, the only one it has), and the free-monoid carriers' identity and cons arms — the typing face of the fact `uncons` computes with and `close` traverses by, that every carrier value is the identity or one generator over a shorter value. A variable scrutinee *is* the case's value within an arm, so every arm is checked with that equation substituted, the zero-index instance of the specialization `eliminate` gives nominal arms.
fn check_cases(
    certifier: &mut Certifier,
    family: Option<&InductType>,
    goal: &Term,
    cases: &Cases,
    scrutinee: &Term,
    sized: &Term,
    scrutinee_type: &Term,
) -> Result<(), Error> {
    let goal_sort = goal_sort(certifier, goal)?;

    // An arm taken once, at the scrutinee itself: not typed where a scrutinee that already is another case never takes it, and otherwise at its case, the scrutinee standing refined to it.
    let at = |certifier: &mut Certifier, value: Term, body: &Term| {
        if eliminate::never_taken(certifier, scrutinee, &value, &[])? {
            return Ok(());
        }
        let expected = goal_at(goal, scrutinee, &[], &[], &value);

        let mut assumed = Vec::new();
        let checked = certifier.tried(|certifier| {
            certifier.assume_guard(sized, &value)?;
            let mut solutions = Vec::new();
            assumed = eliminate::assume_case_value(certifier, scrutinee, &value, &mut solutions)?;
            certifier.assume_arm(sized, &value, &[])?;
            eliminate::bind_solutions(certifier, &solutions)?;

            check(certifier, body, &expected)
        });

        eliminate::unless_never_taken(certifier, checked, &assumed, &[])
    };

    match cases {
        Cases::Induct {
            cases,
            default,
            stated,
        } => {
            let Some(family) = family else {
                return Err(Error::Unclassified(scrutinee.clone()));
            };
            // The instance a match states is held to the type its scrutinee was inferred at, and typed, so that a call in it is one this check graded: reduction reads the payload of the one arm off it and looks no binder up ([`unread_payload`](super::unread_payload)).
            if let Some(stated) = stated {
                infer_type(certifier, stated)?;
                if !convert(certifier, &Term::type_ground(), stated, scrutinee_type)? {
                    return Err(Error::Mismatch {
                        inferred: Box::new(scrutinee_type.clone()),
                        expected: Box::new(stated.clone()),
                    });
                }
            }
            let at = certifier.induct_at(family)?;

            // A match with no arms and no catch-all is a vacuous elimination: the coverage loop must then prove *every* constructor impossible at the scrutinee's indices, so the eliminated instance is uninhabited and discharging it into a relevant result leaks nothing. The guard exists for eliminations that can run; this one cannot. It sits here rather than inside the arm rule because the sort it consumes is derived here, by `goal_sort`, and a second derivation is what this clause exists to avoid.
            if !(cases.is_empty() && default.is_none()) {
                eliminate::guard_large_elimination(certifier, &at, family, goal_sort)?;
            }

            check_induct_arms(
                certifier,
                &at,
                family,
                goal,
                cases,
                default.as_ref(),
                eliminate::Scrutinee {
                    typed: scrutinee,
                    sized,
                },
            )
        }

        // The carrier a case form names is a claim about the scrutinee, established here for the same reason [`check_free_monoid`] establishes its own: the arms are typed at the case values while the match is typed at its goal, so a form that does not match the carrier types the arms at one type and runs them at another.
        Cases::Bool {
            false_case,
            true_case,
        } => {
            if !matches!(&**scrutinee_type, Subterm::Intrinsic(Intrinsic::BoolType)) {
                return Err(Error::Unclassified(scrutinee_type.clone()));
            }

            at(
                certifier,
                Term::intrinsic(Intrinsic::Bool(false)),
                false_case,
            )?;
            at(certifier, Term::intrinsic(Intrinsic::Bool(true)), true_case)
        }

        Cases::Switch { keys, default } => {
            if *scrutinee_type != keys.carrier() {
                return Err(Error::Unclassified(scrutinee_type.clone()));
            }

            for (literal, body) in keys.literals() {
                at(certifier, literal, body)?;
            }

            // The default stands for every value not enumerated, so the only instance of the result it can be checked at is the scrutinee's — which refines nothing. It is reached past every key, so it stands where each key's test failed, as a lone guard's `false` arm stands under its test. Enumerating zero is what rules zero out of it, which a call inside may descend on.
            let expected = goal.clone();
            certifier.scoped(|certifier| {
                if let Subterm::Var(var) = &**sized
                    && let Some(binder) = var.as_free()
                    && keys.names_zero()
                {
                    certifier.assume_nonzero(*binder)?;
                }
                for test in catch_all_tests(scrutinee, keys) {
                    certifier.refine(test, Term::intrinsic(Intrinsic::Bool(false)))?;
                }

                check(certifier, default, &expected)
            })
        }

        Cases::FreeMonoid { carrier } => check_free_monoid(
            certifier,
            goal,
            scrutinee,
            sized,
            scrutinee_type,
            carrier,
            &at,
        ),
    }
}

/// The free-monoid arm rule: the identity arm inhabits the result at the carrier's empty value, and the cons arm, under a peeled generator and a tail, inhabits it at one generator prepended to the tail; each is checked as every other arm is, at its case value with the scrutinee standing refined to it (`at`). The case values are spelled exactly as elaboration spelled them (`pred + 1`, the singleton-concat for `List`, the append-to-empty singleton for `Bytes`, whose packed literals cannot hold a symbolic atom), and conversion's free-monoid peel is what makes those spellings and reduction's forms one normal form.
///
/// The carrier's own element type must agree with the scrutinee's: the arms are typed against the carrier's copy, and a value flowing through the match carries the scrutinee's, so a disagreement would type the arms at one type and run them at another.
fn check_free_monoid(
    certifier: &mut Certifier,
    goal: &Term,
    scrutinee: &Term,
    sized: &Term,
    scrutinee_type: &Term,
    carrier: &Carrier,
    at: &impl Fn(&mut Certifier, Term, &Term) -> Result<(), Error>,
) -> Result<(), Error> {
    // One cons arm: open the binders, assume them at the carrier's types, and check the body at the result of the cons value, with the scrutinee standing refined to that value, exactly as in every other arm.
    //
    // `size_value` is the cons value as the call recorder reads a size: the same value, spelled as the carrier's layered form where the typing's spelling is an operation the size order does not read through.
    let cons = |certifier: &mut Certifier,
                binders: Vec<(&Free, Term)>,
                cons_value: Term,
                size_value: Term,
                body: &Term|
     -> Result<(), Error> {
        certifier.scoped(|certifier| {
            for (binder, type_) in &binders {
                certifier.assume(binder, type_);
            }

            let names = binders
                .iter()
                .map(|(binder, _)| **binder)
                .collect::<Vec<_>>();
            if eliminate::never_taken(certifier, scrutinee, &cons_value, &names)? {
                return Ok(());
            }

            let expected = goal_at(goal, scrutinee, &[], &[], &cons_value);

            let mut assumed = Vec::new();
            let checked = certifier.tried(|certifier| {
                let mut solutions = Vec::new();
                assumed = eliminate::assume_case_value(
                    certifier,
                    scrutinee,
                    &cons_value,
                    &mut solutions,
                )?;
                certifier.assume_arm(sized, &size_value, &names)?;
                eliminate::bind_solutions(certifier, &solutions)?;

                check(certifier, body, &expected)
            });

            eliminate::unless_never_taken(certifier, checked, &assumed, &names)
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
                certifier,
                Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
                empty_case,
            )?;

            let pred = certifier.fresh(cons_case.first_hint());
            let pred_occurrence = Term::free_var(&pred);
            let succ_value = Term::intrinsic(Intrinsic::nat_add(
                pred_occurrence.clone(),
                Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
            ));
            let body = cons_case.open(&[&pred_occurrence]);

            // `pred + 1` as one successor layer over `pred`, which is how the size order reads a `Nat`.
            let size_value = Term::intrinsic(Intrinsic::Nat(Nat::Succ(
                Natural::from(1usize),
                pred_occurrence.clone(),
            )));
            let binders = vec![(&pred, Term::intrinsic(Intrinsic::NatType))];

            cons(certifier, binders, succ_value, size_value, &body)
        }

        Carrier::List {
            elem,
            empty_case,
            cons_case,
        } => {
            let Subterm::Intrinsic(Intrinsic::ListType(scrutinee_elem)) = &**scrutinee_type else {
                return Err(Error::Unclassified(scrutinee_type.clone()));
            };
            // The element type a split states is typed before it is held to its scrutinee's, as a stated instance is: a type is accepted by typing it, and a call in it is one this check grades.
            infer_type(certifier, elem)?;
            if !convert(certifier, &Term::type_ground(), elem, scrutinee_elem)? {
                return Err(Error::Mismatch {
                    inferred: Box::new(elem.clone()),
                    expected: Box::new(scrutinee_elem.clone()),
                });
            }

            at(
                certifier,
                Term::intrinsic(Intrinsic::List {
                    element: elem.clone(),
                    items: Vec::new(),
                }),
                empty_case,
            )?;

            let head = certifier.fresh(cons_case.first_hint());
            let tail = certifier.fresh(cons_case.second_hint());
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
            let body = cons_case.open(&[&Term::free_var(&head), &tail_occurrence]);

            let binders = vec![
                (&head, elem.clone()),
                (&tail, Term::intrinsic(Intrinsic::ListType(elem.clone()))),
            ];

            cons(certifier, binders, cons_value.clone(), cons_value, &body)
        }

        Carrier::Bytes {
            empty_case,
            cons_case,
        } => {
            if !matches!(&**scrutinee_type, Subterm::Intrinsic(Intrinsic::BytesType)) {
                return Err(Error::Unclassified(scrutinee_type.clone()));
            }

            let empty = Term::intrinsic(Intrinsic::Bytes(Binary::empty()));
            at(certifier, empty.clone(), empty_case)?;

            let atom_type = Term::intrinsic(Intrinsic::ByteType);
            let head = certifier.fresh(cons_case.first_hint());
            let tail = certifier.fresh(cons_case.second_hint());
            let tail_occurrence = Term::free_var(&tail);
            let singleton = Term::intrinsic(Intrinsic::BytesAppend {
                bytes: empty,
                element: Term::free_var(&head),
            });
            let cons_value = Term::intrinsic(Intrinsic::BytesConcat {
                operands: vec![singleton, tail_occurrence.clone()],
            });
            let body = cons_case.open(&[&Term::free_var(&head), &tail_occurrence]);

            let binders = vec![
                (&head, atom_type),
                (&tail, Term::intrinsic(Intrinsic::BytesType)),
            ];

            cons(certifier, binders, cons_value.clone(), cons_value, &body)
        }
    }
}

/// Verify that `term` has type `expected`.
pub fn check(certifier: &mut Certifier, term: &Term, expected: &Term) -> Result<(), Error> {
    // Whether this check opens a member body's leading lambdas — taken first, so it holds for this term alone and the λ rule is the one reader that carries it on.
    let parameters = certifier.calls.take_parameters();
    // What a lambda checked here is handed where its callee applies it, taken here for the same reason.
    let assumed = certifier.calls.take_assumed();

    // Seed for the erasure obligations, recorded before the rules dispatch so a position counts however it is checked. Classified here rather than afterwards: the expectation routinely mentions binders this item opened, and they are retracted the moment its check returns, so nothing later can ask for their sorts. The answer is remembered beside the certifier's sorts, which keeps that to one question per distinct type.
    let before = certifier.partial_groups();
    let position = certifier.record_checked(term, expected);
    // A check is remembered as an inferred type is, and a hit counts again what the first check closed. The three rules that never reach `infer` are why it has a table of its own: a tuple whose two fields are one node is checked against its record once per path unless the check itself is remembered.
    let checked = match certifier.check_hit(term, expected) {
        Some(closed) => {
            if closed {
                certifier.calls.recall_partial();
            }

            Ok(())
        }
        None => {
            let checked = check_rules(certifier, term, expected, parameters, assumed);
            if checked.is_ok() {
                let closed = certifier.partial_groups() > before;
                certifier.check_store(term, expected, closed);
            }

            checked
        }
    };
    // Whether a group that does not descend was typed inside this term — noted on the position, since the group was typed with the binders around it opened and the position's term holds it closed.
    if certifier.partial_groups() > before {
        certifier.enclose_partial(position);
    }

    checked
}

/// Check a `let`'s bindings in order and judge its tail under the locals they bind.
///
/// **A `let` is bound, not substituted.** Each value is checked once, where the `let` stands, and the tail is typed over a local that stands for it ([`Certifier::bind`]). Substituting the values hands typing the tail's tree: a value named twice is typed twice, and under an arm each copy again under that arm's own equation, which no table answers across. Each binding sees exactly the binders before it: a `let` is non-recursive, and self-reference is `rec`'s. Reduction keeps substituting a `let` it meets (`step_let`), and what it builds is counted.
///
/// `judge` is handed the tail opened over the binders, and runs while they stand.
fn under_bindings<T>(
    certifier: &mut Certifier,
    bindings: &[LetBinding],
    tail: &Scope<Many>,
    judge: impl FnOnce(&mut Certifier, &Term) -> Result<T, Error>,
) -> Result<T, Error> {
    certifier.scoped(|certifier| {
        let mut binders: Vec<Term> = Vec::with_capacity(bindings.len());

        for (index, binding) in bindings.iter().enumerate() {
            let refs = binders.iter().collect::<Vec<_>>();
            let type_ = binding.type_().release(&refs);
            let value = binding.value().release(&refs);

            infer_type(certifier, &type_)?;
            let partial = certifier.partial_groups();
            check(certifier, &value, &type_)?;
            let binder = certifier.fresh(tail.hint(index));
            certifier.bind(
                &binder,
                &type_,
                &value,
                certifier.partial_groups() > partial,
            );
            binders.push(Term::free_var(&binder));
        }

        let refs = binders.iter().collect::<Vec<_>>();
        judge(certifier, &tail.open(&refs))
    })
}

/// [`check`]'s rules, past the position it records: `let`'s descent, Π- and Σ-introduction, and inference for the rest.
fn check_rules(
    certifier: &mut Certifier,
    term: &Term,
    expected: &Term,
    parameters: bool,
    assumed: Option<Handed>,
) -> Result<(), Error> {
    // A `let` carries no type of its own — the tail's type is the whole term's — so the expectation descends through it: the same binding validation as inference, with only the tail's mode changed. Without this, a dependent tuple or lambda under a `let` reaches the checked rules below as an inference and manufactures the non-dependent type they exist to avoid.
    if let Subterm::Let(block) = &**term {
        let (bindings, tail) = (block.bindings(), block.tail());
        return under_bindings(certifier, bindings, tail, |certifier, tail| {
            check(certifier, tail, expected)
        });
    }

    // The Π-introduction half of the checked rules below: a lambda checks against a function type by walking both telescopes under one shared binder set — each domain pair invariant by conversion, exactly as subsumption compares them — and checking the body against the expected codomain. Routing the body through `check` rather than inference is what lets a tuple body reach the Σ rule with its expectation intact; the inferred route would manufacture the non-dependent codomain first.
    if let Subterm::Func(func) = &**term {
        let reduced = certifier.reduce_forced(expected.clone())?;
        if let Subterm::FuncType(expected_func) = &*reduced
            && func.telescope.same_marks(&expected_func.telescope)
            && func.telescope.len() == expected_func.telescope.len()
        {
            let lambda = func.telescope.clone();
            let against = expected_func.telescope.clone();
            return certifier.scoped(|certifier| {
                check_lambda(certifier, lambda, against, parameters, assumed.as_ref())
            });
        }
        // Anything else — a non-Π expectation, a plicity or arity mismatch — falls through, so the refusal keeps its ordinary inferred-versus-expected shape.
    }

    // The Σ-introduction rule stated directly: a tuple literal checks against a dependent telescope entry-wise, each component at its entry's type opened over the actual preceding components. Inference cannot reach this verdict — inferring the components independently manufactures a non-dependent tuple type whose telescope binds nothing, and no conversion can relate that to a telescope whose later entries mention its binders.
    //
    // At a Σ and at nothing else. A structure's type has one literal, the structure's own, which names it and is inferred: a tuple literal held at one would be a value no projection reads, since a slot of a structure reads that structure's literal alone (`curios_core::project_literal`). Any other expectation falls through, so the mismatch keeps its ordinary inferred-versus-expected shape.
    if let Subterm::Tuple(Tuple { fields, .. }) = &**term {
        let reduced = certifier.reduce_forced(expected.clone())?;
        if let Subterm::TupleType(TupleType { telescope }) = &*reduced {
            return check_fields(certifier, fields, telescope, parameters, assumed.as_ref());
        }
    }

    // A structure's literal leading a member body, under its leading lambdas, is the record whose fields are parts of their own: its rule checks each as the part it is. Taken back past the judgment, which a remembered typing answers without reading it.
    if parameters && matches!(&**term, Subterm::Struct(_)) {
        certifier.calls.mark_record();
    }
    // A structure's literal handed where its callee applies it hands each field what its path is applied with, as a tuple's does above.
    if let Some(assumed) = assumed
        && matches!(&**term, Subterm::Struct(_))
    {
        certifier.calls.set_literal(assumed);
    }
    let inferred = infer(certifier, term);
    certifier.calls.take_record();
    certifier.calls.take_literal();
    let inferred = inferred?;

    match subsumes(certifier, &inferred, expected)? {
        true => Ok(()),
        false => Err(Error::Mismatch {
            inferred: Box::new(inferred),
            expected: Box::new(expected.clone()),
        }),
    }
}

/// The telescope walk of the lambda rule above, under [`Certifier::scoped`]'s retraction bracket: the guards on plicity and arity ran at the dispatch, so the two telescopes are structurally parallel by construction.
///
/// `parameters` says this λ leads a member body, so each binder it opens is one of that member's parameters to the call recorder, and so is each binder a λ leading its body opens in turn.
fn check_lambda(
    certifier: &mut Certifier,
    lambda: Telescope<Term>,
    against: Telescope<Term>,
    parameters: bool,
    assumed: Option<&Handed>,
) -> Result<(), Error> {
    let mut walk = Lockstep::new(&lambda, &against);
    let mut binders = Vec::new();

    loop {
        match walk.step() {
            Step::Entries {
                left: mine,
                right: theirs,
                ..
            } => {
                infer_type(certifier, &mine)?;
                if !convert(certifier, &Term::type_ground(), &mine, &theirs)? {
                    return Err(Error::Mismatch {
                        inferred: Box::new(mine),
                        expected: Box::new(theirs),
                    });
                }
                let binder = certifier.advance_assumed(&mut walk, &theirs);
                if parameters {
                    certifier.bind_parameter(&binder);
                }
                binders.push(binder);
            }
            Step::Bodies(body, codomain) => {
                if parameters {
                    certifier.calls.continue_parameters();
                }
                if let Some(handed) = assumed {
                    certifier.assume_handed(&binders, handed);
                }
                return check(certifier, &body, &codomain);
            }
            Step::Mismatch => unreachable!("the dispatch guarded the arities equal"),
        }
    }
}

/// Check `field`, slot `slot` of a record literal, at `type_`; where `record` says the literal is what a member's leading lambdas answer, or a record held at a field of one, as the part the field is ([`Certifier::enter_field`]), so the calls its lambda makes are that part's; and where the literal is `handed` where its callee applies it, with what the field's own path is applied with ([`field_handed`]).
fn check_field(
    certifier: &mut Certifier,
    slot: usize,
    field: &Term,
    type_: &Term,
    record: bool,
    handed: Option<&Handed>,
) -> Result<(), Error> {
    if let Some(handed) = handed.map(|handed| field_handed(handed, slot))
        && !handed.is_empty()
    {
        certifier.calls.hand(field, handed);
    }
    let checked = match record {
        false => check(certifier, field, type_),
        true => {
            let saved = certifier.enter_field(slot);
            let checked = check(certifier, field, type_);
            certifier.leave_field(saved);
            checked
        }
    };
    certifier.calls.clear_handed();
    checked
}

/// The entry-wise walk of the tuple rule above: each component checked at its entry's type, the telescope then opened at that *actual* component so later entries see the value the binder stands for; where `record` says the tuple is a member body's record, each as the part it is, and where it is `handed` where its callee applies it, each with what its path is applied with ([`check_field`]).
fn check_fields(
    certifier: &mut Certifier,
    fields: &[Term],
    telescope: &Telescope<()>,
    record: bool,
    handed: Option<&Handed>,
) -> Result<(), Error> {
    if telescope.len() != fields.len() {
        return Err(Error::Arity {
            counted: Counted::Fields,
            expected: telescope.len(),
            actual: fields.len(),
        });
    }

    let mut cursor = telescope.cursor();
    for (slot, field) in fields.iter().enumerate() {
        let (_, type_) = cursor
            .entry()
            .expect("the arity guard bounds the walk to the telescope's length");
        check_field(certifier, slot, field, &type_, record, handed)?;
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
/// Prop u ≤ Prop v          when the level algebra proves u ≤ v
/// Π(x:A).B ≤ Π(x:A').B'    when A ≡ A' and, under x, B ≤ B'
/// A      ≤ B              otherwise, when A ≡ B
/// ```
///
/// **No proposition stands where a type is wanted.** `Prop u ≤ Type v` is no rule: a term at a `Type`, a variable among them, is then no proposition under any substitution, so whether a type is a proposition is a fact about the type that an arm's solution cannot change. With the rule, `F(c)` that is a proposition at `true` and data at `false` has its inhabitants made one inside the arm that fixes `c` and apart outside it.
///
/// **Domains are invariant, codomains cumulative.** Comparing domains by conversion rather than contravariantly is the choice Coq makes, and it is the freely-revisable side of the fork: widening to contravariance later accepts strictly more, so it breaks nothing already accepted, while shipping contravariance and withdrawing it would break programs.
///
/// The elaborator decides the same relation by the same rule, inside its conversion: each problem there carries the relation it is asked at, a function type's domains being equations and its codomain keeping the relation, so a pair that waits on a metavariable resumes as the subsumption it was. The certifier meets finished terms and has nothing to wait on. Stating the rule structurally in both is what makes it readable, and what lets the two checkers disagree where one of them drifts.
fn subsumes(certifier: &mut Certifier, inferred: &Term, expected: &Term) -> Result<bool, Error> {
    certifier.spend(Cost::STEP)?;

    let lower = certifier.reduce_forced(inferred.clone())?;
    let upper = certifier.reduce_forced(expected.clone())?;

    match (&*lower, &*upper) {
        // Each sort is cumulative in its own levels, and neither stands for the other.
        (
            Subterm::Sort(curios_core::Sort {
                quality: below,
                level: lower,
            }),
            Subterm::Sort(curios_core::Sort {
                quality: above,
                level: upper,
            }),
        ) if below == above => {
            return Ok(certifier.level_leq(lower, upper));
        }
        // Plicity is part of a function type's identity, exactly as in `convert`: `(A) -> A` and `(@A) -> A` have different calling conventions, so a difference there is a mismatch and not a codomain question.
        (Subterm::FuncType(lower), Subterm::FuncType(upper))
            if lower.telescope.same_marks(&upper.telescope) =>
        {
            let (lower, upper) = (lower.telescope.clone(), upper.telescope.clone());

            return certifier.scoped(|certifier| subsumes_telescope(certifier, lower, upper));
        }
        _ => {}
    }

    convert(certifier, &Term::type_ground(), inferred, expected)
}

/// [`subsumes`] through a function type's telescope: each domain by conversion, the terminal codomains by subsumption, under one shared set of binders.
///
/// Opening both sides at the *same* occurrence is what makes the codomain comparison meaningful — the domains have just been shown convertible, so a single binder stands for both.
fn subsumes_telescope(
    certifier: &mut Certifier,
    this: Telescope<Term>,
    that: Telescope<Term>,
) -> Result<bool, Error> {
    let mut walk = Lockstep::new(&this, &that);

    loop {
        match walk.step() {
            Step::Entries { left, right, .. } => {
                if !convert(certifier, &Term::type_ground(), &left, &right)? {
                    return Ok(false);
                }
                certifier.advance_assumed(&mut walk, &left);
            }
            Step::Bodies(left, right) => return subsumes(certifier, &left, &right),
            // Different arities. A function type is not curried in this representation, so this is a real mismatch rather than a shape to normalize.
            Step::Mismatch => return Ok(false),
        }
    }
}

/// Check each of `arguments` against its domain in `telescope`, each later domain opened at the arguments before it, and hand back the terminal opened at all of them.
///
/// The caller has established that the counts agree — a handle counted an occurrence's parameters or indices, and the arity checks count a payload or a record's fields — so the walk never runs out of entries.
fn check_along<B: Bound>(
    certifier: &mut Certifier,
    telescope: Telescope<B>,
    arguments: &[Term],
) -> Result<B, Error> {
    let mut cursor = telescope.cursor();
    for argument in arguments {
        let (_, domain) = cursor.entry().expect("the caller checked the count");

        check(certifier, argument, &domain)?;
        cursor.advance(argument.clone());
    }

    Ok(cursor.body().expect("the caller checked the count"))
}

/// The Π a λ inhabits, with each binder standing for the corresponding one of `arguments` where the λ is applied on the spot.
fn infer_lambda(certifier: &mut Certifier, func: &Func, arguments: &[Term]) -> Result<Term, Error> {
    let telescope = infer_telescope(certifier, func.telescope.clone(), arguments)?;

    Ok(Subterm::FuncType(FuncType { telescope }).into())
}

/// Check that every domain of a λ's telescope is a type, then its body under those binders, rebuilding the telescope as the Π the λ inhabits.
///
/// One walk under one retraction bracket, each domain opened once at the binders before it and the Π built in one pass at the end — where recursing into the reopened rest and re-closing each level would rewrite the whole inner telescope once per binder.
///
/// A binder with an argument in `arguments` stands for it within the bracket, to the call recorder alone — a size fact, never an equation the typing sees, so the Π built is the λ's own.
fn infer_telescope(
    certifier: &mut Certifier,
    telescope: Telescope<Term>,
    arguments: &[Term],
) -> Result<Telescope<Term>, Error> {
    certifier.scoped(|certifier| {
        let mut entries = Vec::new();
        let mut cursor = telescope.cursor();

        while let Some((_, domain)) = cursor.entry() {
            let mark = cursor.mark().expect("a mark stands at an entry");
            infer_type(certifier, &domain)?;

            let binder = certifier.advance_assumed(&mut cursor, &domain);
            // The binder stands for its argument as a graded call reads it, a `let`-bound local by what it stands for, so what an arm rules of the binder reaches the parameter a call is graded against.
            if let Some(argument) = arguments.get(entries.len()) {
                let graded = certifier.graded(argument);
                certifier.refine_size(&binder, &graded)?;
            }
            entries.push((mark, binder, domain));
        }

        let body = cursor.body().expect("a cursor past every entry");
        // Arguments past the telescope are what the lambda the body is gets applied to on the spot, its binders standing for them as these do: a curried spine's later lists. Typed as the application's head lambda is, its position recorded here, since the memo the ordinary route would consult cannot hold a size fact.
        let rest = arguments.get(entries.len()..).unwrap_or_default();
        let type_ = match &*body {
            Subterm::Func(func) if !rest.is_empty() => {
                let before = certifier.partial_groups();
                let type_ = infer_lambda(certifier, func, rest)?;
                let position = certifier.record_checked(&body, &type_);
                if certifier.partial_groups() > before {
                    certifier.enclose_partial(position);
                }
                type_
            }
            _ => infer(certifier, &body)?,
        };

        // The Π keeps each binder's mark, which is part of its identity.
        Ok(Telescope::build_marked(entries, type_))
    })
}
