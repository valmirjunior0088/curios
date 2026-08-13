//! Definitional equality: whether two terms are interchangeable at a type.
//!
//! Conversion is the rule that decides which programs typecheck, so it is the most consequential thing in this crate. It is *type-directed*: the type drives eta-expansion and it is what makes proof irrelevance possible, so every goal carries the type at which the two sides are being compared.
//!
//! The rules, in the order they are tried:
//!
//! 1. **Proof irrelevance.** A goal at a `Prop`-sorted type is discharged without looking at either side. Any two inhabitants of a proposition are definitionally equal, which is what lets erasure drop them wholesale. 2. **Eta.** At a function type both sides are applied to fresh binders and compared at the codomain; at a Σ type both are projected and compared componentwise. So `f` and `(x) => f(x)` convert, and so do `p` and `(p.0, p.1)`, without either side having to be in that shape. 3. **Structure.** Both sides are reduced to weak-head normal form and their heads compared, recursing on the children.
//!
//! # Termination, and the recurrence rule
//!
//! Two folded recursive calls can unfold forever without ever disagreeing — that is what an equirecursive type is. Conversion therefore keeps a history of the goals it is already inside, and a goal that recurs is *assumed to hold*. This is the coinductive reading: a genuine cycle leaves nothing but itself to check, and any finite disagreement surfaces on a sibling goal before the cycle closes.
//!
//! That rule is where a conversion checker is most likely to be unsound, and the danger is precise: a history hit on two goals that are *not* the same goal accepts terms that are not equal. The guard here is that an entry records the local context alongside the goal, and that every binder in scope is renamed to its position before the entry is made. Two entries collide only when the same comparison is being made under binders of the same types in the same order — which is the same comparison.
//!
//! `curios-elab`'s conversion checker canonicalizes differently: it renames the binders *it* minted, in mint order, and does not key on the context at all. For a strictly nested walk like this one the two orderings coincide, since the binders in scope at a goal are exactly the path to it. The elaborator's walk is not strictly nested — it has a worklist and it parks goals — so whether the two schemes agree there is a genuinely open question, recorded as such in `documentation/DESIGN.md`. This module deliberately does not inherit the answer.
//!
//! # Where this is incomplete, and why that is the safe direction
//!
//! Two concessions remain. A `rec` group is compared syntactically. And every child position without a typed context — an application spine's arguments, a stuck elimination's motive and arms under their opaque binders, and a struct type-former's parameters — is compared at `Type` rather than at the types its head assigns, which forfeits eta and irrelevance there. Each is a place where the kernel may reject a term the elaborator accepted. An inductive type-former's arguments left this list when the compile path put real programs through the kernel: they are compared at the declaration's own index telescope (`induct_type_args`), which is what lets `Eq(@P, p, q)` at a `Prop`-sorted `P` convert with `Eq(@P, p, p)`.
//!
//! That direction is deliberate. An incomplete conversion refuses programs; an unsound one admits them. A refusal is visible — it is a disagreement between the two checkers, which is precisely the signal this kernel exists to produce — whereas an over-eager acceptance is silent and is exactly what a second opinion is supposed to catch. Every one of these can be strengthened later against a real program that needs it, and none can be strengthened back from having been wrong.

mod intrinsic;
use intrinsic::convert_intrinsic;

#[cfg(test)]
mod tests;

use {
    super::{Kernel, KernelError, Sort, unfold_spelling},
    curios_base::recurse,
    curios_core::{
        Bound, Carrier, Cases, Field, FuncType, Global, InductType, Level, Many, Proj, Reducer,
        Scope, Struct, StructType, Subterm, Telescope, Term, Three, Tuple, TupleType, Two,
        UniverseInst, instantiate_universe_levels_scoped,
    },
    std::collections::HashSet,
};

/// Whether `this` and `that` are definitionally equal at `type_`.
pub fn convert(
    kernel: &mut Kernel,
    type_: &Term,
    this: &Term,
    that: &Term,
) -> Result<bool, KernelError> {
    let mut history = History::default();

    compare(kernel, &mut history, type_, this, that)
}

/// The goals conversion is currently inside.
///
/// A goal is stored with the types of the binders in scope at it, and with every one of those binders renamed to its position. Without the rename, the same comparison reached on two rounds of an unfolding cycle differs in nothing but the identities of the binders opened on the way, and the cycle is never recognized; without the context, two different comparisons that happen to be spelled alike would be conflated, which is the unsound direction.
#[derive(Default)]
struct History {
    seen: HashSet<Goal>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct Goal {
    context: Vec<Term>,
    type_: Term,
    this: Term,
    that: Term,
}

impl History {
    /// Enter a goal: `None` when it is already in progress — the coinductive assumption — otherwise the recorded key, to be handed back to [`History::leave`] when the goal completes.
    ///
    /// The rename is by position in the local context, so a goal reached again under an identically-typed prefix maps onto the same entry. `capture` turns each binder into a bound index; the results are keys and are never opened, so the loose indices they leave behind are inert.
    fn enter(&mut self, kernel: &Kernel, type_: &Term, this: &Term, that: &Term) -> Option<Goal> {
        let binders = kernel.local_names();
        let refs = binders.iter().collect::<Vec<_>>();
        let rename = |term: &Term| term.capture(&refs);

        let goal = Goal {
            context: kernel.local_types().iter().map(rename).collect(),
            type_: rename(type_),
            this: rename(this),
            that: rename(that),
        };

        match self.seen.insert(goal.clone()) {
            true => Some(goal),
            false => None,
        }
    }

    /// Leave a completed goal, whatever its outcome. The set must hold exactly the goals on the current path: a goal *refuted* in one subtree that lingered here would be assumed to hold when a later subtree re-derives it — the accepting direction — and a goal proven under in-progress assumptions is not a fact once they are gone, so neither outcome may stay.
    fn leave(&mut self, goal: &Goal) {
        self.seen.remove(goal);
    }
}

/// Compare `this` and `that` at `type_`, under the binders currently in scope.
///
/// Every goal entered stays in `seen` until this call returns, because retry *N+1* is reached from inside retry *N* and both are in progress at once. The call stack is what records that, and unwinding it retires the innermost goal first.
///
/// An unfolding retry recurses back into here, and what bounds that chain is the budget spent on entry rather than a count of how deep it has gone — a constant standing in for the call stack is what [`recurse`] makes unnecessary. Measured over the whole `curios` corpus — every test, the entire fixed prelude — the deepest chain real code reaches is three.
fn compare(
    kernel: &mut Kernel,
    history: &mut History,
    type_: &Term,
    this: &Term,
    that: &Term,
) -> Result<bool, KernelError> {
    recurse(|| {
        kernel.spend()?;

        // Cheapest first: a term converts with itself at any type, and structural sharing makes this hit constantly on terms built by substitution.
        if this == that {
            return Ok(true);
        }

        // Proof irrelevance. Deliberately before reduction: the point is that neither side is examined, and reducing a proof in order to discover it equals another proof is work whose answer was already known.
        if Sort::of(kernel, type_)?.is_prop() {
            return Ok(true);
        }

        let Some(goal) = history.enter(kernel, type_, this, that) else {
            return Ok(true);
        };

        let outcome = match Term::unwrap_or_clone(kernel.reduce_forced(type_.clone())?) {
            Subterm::FuncType(FuncType { telescope, .. }) => {
                eta_function(kernel, history, telescope, this, that)
            }
            Subterm::TupleType(TupleType { telescope }) if !telescope.is_empty() => {
                eta_tuple(kernel, history, telescope, this, that)
            }
            _ => {
                let this = kernel.reduce_forced(this.clone())?;
                let that = kernel.reduce_forced(that.clone())?;

                structural(kernel, history, &this, &that)
            }
        };

        history.leave(&goal);
        outcome
    })
}

/// Eta at a function type: apply both sides to the same fresh binders and compare the results at the codomain.
///
/// This is why `f` converts with `(x) => f(x)` without either being reduced into the other's shape — the rule is stated once, here, instead of as a special case in every structural arm.
fn eta_function(
    kernel: &mut Kernel,
    history: &mut History,
    telescope: Telescope<Term>,
    this: &Term,
    that: &Term,
) -> Result<bool, KernelError> {
    kernel.scoped(|kernel| {
        let mut telescope = telescope;
        let mut arguments = Vec::new();

        let codomain = loop {
            match telescope {
                Telescope::Cons(domain, rest) => {
                    let binder = kernel.fresh(rest.first_hint());
                    kernel.assume(&binder, &domain);
                    let occurrence = Term::free_var(&binder);
                    telescope = rest.open(&[&occurrence]);
                    arguments.push(occurrence);
                }
                Telescope::Done(codomain) => break *codomain,
            }
        };

        compare(
            kernel,
            history,
            &codomain,
            &Term::apply(this.clone(), arguments.clone()),
            &Term::apply(that.clone(), arguments),
        )
    })
}

/// Eta at a Σ type: compare the two sides componentwise through projections.
///
/// A later field's type may mention an earlier one, and names it by a projection of the *left* side — sound because the earlier components have already been shown equal by the time that type is used.
fn eta_tuple(
    kernel: &mut Kernel,
    history: &mut History,
    telescope: Telescope<()>,
    this: &Term,
    that: &Term,
) -> Result<bool, KernelError> {
    let mut telescope = telescope;
    let mut index = 0;

    loop {
        match telescope {
            Telescope::Cons(field, rest) => {
                let left = Term::proj(this.clone(), index);
                let right = Term::proj(that.clone(), index);

                if !compare(kernel, history, &field, &left, &right)? {
                    return Ok(false);
                }

                telescope = rest.open(&[&left]);
                index += 1;
            }
            Telescope::Done(_) => return Ok(true),
        }
    }
}

/// Compare two weak-head normal forms by their heads.
///
/// Children with no type the head determines are compared at `Type` through [`ground`]. That is a weaker comparison than a typed one — it declines to fire eta or irrelevance — so it can only reject where a typed comparison would have accepted. See the module documentation on incompleteness.
fn structural(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Term,
    that: &Term,
) -> Result<bool, KernelError> {
    match (&**this, &**that) {
        // Levels compare under the item's assumed constraints: two levels the hypotheses force equal are equal in every instance that satisfies them, which is what checking generically means.
        (Subterm::Type(left), Subterm::Type(right)) => Ok(kernel.level_eq(left, right)),
        (Subterm::Prop, Subterm::Prop) => Ok(true),

        (Subterm::Intrinsic(left), Subterm::Intrinsic(right)) => {
            convert_intrinsic(kernel, history, left, right)
        }

        (Subterm::Var(left), Subterm::Var(right)) => Ok(left.unwrap() == right.unwrap()),

        // A metavariable is elaboration-only syntax, and refusing it *here* is what makes the exclusion the kernel's own rather than an inherited guarantee of `zonk_module`'s traversal. `whnf` still treats one as a stuck neutral — a reduction stance, not an admission: the only ways a term is admitted are `infer` and this comparison, and both refuse. The syntactic fast path in `compare` does admit a metavariable against *itself*, and soundly: reflexivity decides nothing about the unknown, which is exactly what this arm exists to prevent.
        (Subterm::Metavar(_), _) | (_, Subterm::Metavar(_)) => {
            Err(KernelError::NotCore(this.clone()))
        }

        // Plicity is part of a function type's identity: `(A) -> A` and `(@A) -> A` have different calling conventions, and conflating them would let a value be applied through the wrong one.
        (Subterm::FuncType(left), Subterm::FuncType(right)) => Ok(left.plicities
            == right.plicities
            && compare_telescope(
                kernel,
                history,
                left.telescope.clone(),
                right.telescope.clone(),
            )?),

        // Two lambdas with no expected type to eta against: compare their bodies under one shared set of binders.
        (Subterm::Func(left), Subterm::Func(right)) => Ok(left.plicities == right.plicities
            && compare_telescope(
                kernel,
                history,
                left.telescope.clone(),
                right.telescope.clone(),
            )?),

        (Subterm::TupleType(left), Subterm::TupleType(right)) => compare_field_telescope(
            kernel,
            history,
            left.telescope.clone(),
            right.telescope.clone(),
        ),
        (
            Subterm::Tuple(Tuple { fields: left, .. }),
            Subterm::Tuple(Tuple { fields: right, .. }),
        ) => compare_each(kernel, history, left, right),

        // Spine against spine, and when that fails, one definitional unfolding each: two applications of the same fold can differ in an argument position the fold discards — `is_trimmed(h ++ rest)` against `is_trimmed(rest)` — so a spine mismatch is not yet a verdict when either head is a folded recursive call.
        (Subterm::Apply(left), Subterm::Apply(right)) => {
            if left.plicities == right.plicities
                && ground(kernel, history, &left.head, &right.head)?
                && compare_each(kernel, history, &left.params, &right.params)?
            {
                return Ok(true);
            }

            unfolded_retry(kernel, history, this, that)
        }

        (
            Subterm::Proj(Proj {
                head: left,
                field: left_field,
            }),
            Subterm::Proj(Proj {
                head: right,
                field: right_field,
            }),
        ) => Ok(left_field == right_field && ground(kernel, history, left, right)?),

        (
            Subterm::InductType(InductType {
                name: left_name,
                universes: left_universes,
                params: left_params,
                indices: left_indices,
            }),
            Subterm::InductType(InductType {
                name: right_name,
                universes: right_universes,
                params: right_params,
                indices: right_indices,
            }),
        ) => Ok(left_name == right_name
            && kernel.levels_eq(left_universes, right_universes)
            && induct_type_args(
                kernel,
                history,
                left_name,
                left_universes,
                (left_params, left_indices),
                (right_params, right_indices),
            )?),

        (
            Subterm::StructType(StructType {
                name: left_name,
                universes: left_universes,
                params: left_params,
            }),
            Subterm::StructType(StructType {
                name: right_name,
                universes: right_universes,
                params: right_params,
            }),
        ) => Ok(left_name == right_name
            && kernel.levels_eq(left_universes, right_universes)
            && compare_each(kernel, history, left_params, right_params)?),

        (Subterm::Variant(left), Subterm::Variant(right)) => Ok(left.name == right.name
            && left.tag == right.tag
            && kernel.levels_eq(&left.universes, &right.universes)
            && compare_each(kernel, history, &left.params, &right.params)?
            && compare_each(kernel, history, &left.payload, &right.payload)?),

        (
            Subterm::Struct(Struct {
                name: left_name,
                universes: left_universes,
                params: left_params,
                fields: left_fields,
                ..
            }),
            Subterm::Struct(Struct {
                name: right_name,
                universes: right_universes,
                params: right_params,
                fields: right_fields,
                ..
            }),
        ) => Ok(left_name == right_name
            && kernel.levels_eq(left_universes, right_universes)
            && compare_each(kernel, history, left_params, right_params)?
            && compare_each(kernel, history, left_fields, right_fields)?),

        // Eta at a nominal struct, against a neutral inhabitant only — see `struct_eta` for the rule and the restriction.
        (Subterm::Struct(literal), _) if matches!(&**that, Subterm::Var(_) | Subterm::Proj(_)) => {
            struct_eta(kernel, history, literal, that)
        }
        (_, Subterm::Struct(literal)) if matches!(&**this, Subterm::Var(_) | Subterm::Proj(_)) => {
            struct_eta(kernel, history, literal, this)
        }

        (
            Subterm::UniverseInst(UniverseInst {
                head: left,
                levels: left_levels,
            }),
            Subterm::UniverseInst(UniverseInst {
                head: right,
                levels: right_levels,
            }),
        ) => Ok(
            kernel.levels_eq(left_levels, right_levels) && ground(kernel, history, left, right)?
        ),

        // A stuck elimination. Everything is compared up to conversion: the scrutinee because that is the position an unfolding cycle travels through, and the motive and arms because a delta-unfolded caller and its spelled-out twin differ exactly there — `step(c, st)` against `step(at(cons(c, t), 0, _), st)` reduces to two stuck matches whose arms are convertible but not identical. The shape stays rigid: tags, plicities, arity, and default presence must agree exactly, because two eliminations enumerating different constructors compute differently on some input even where they agree on this one.
        (Subterm::Match(left), Subterm::Match(right)) => {
            Ok(ground(kernel, history, &left.head, &right.head)?
                && ground_scope(kernel, history, &left.motive, &right.motive)?
                && ground_cases(kernel, history, &left.cases, &right.cases)?)
        }

        // A folded recursive call, and a `rec` that forcing declined to unfold. Both are compared syntactically: the interesting case — a cycle that unfolds without disagreeing — is handled by the recurrence rule above, not here. A `rec` whose tail computes something is *not* this case, and falls through to the delta step below.
        (Subterm::Rec(_), Subterm::Rec(_))
            if this.as_rec_proj().is_some() && that.as_rec_proj().is_some() =>
        {
            Ok(this.as_rec_proj() == that.as_rec_proj())
        }

        // Two spellings of one recursive call: `force` keeps the folded application as a recursive call's normal form, while an arm's induction hypothesis is the raw stuck fold-match on the same argument. When the heads disagree, grant each side the one definitional unfolding `force` withheld and compare what results.
        _ => unfolded_retry(kernel, history, this, that),
    }
}

/// Argument-wise comparison of two instances of one inductive family, each pair at the type the declaration's full index telescope assigns, opened at the left instance's preceding actuals — the typed context the grounded comparison forfeits. Irrelevance at a `Prop`-typed index is the observable difference: `Eq(@P, p, q)` with `P : Prop` converts with `Eq(@P, p, p)`, because no proof of a proposition is distinguishable from another. The struct-parameter analog of this gap is still grounded — no witness has forced it.
fn induct_type_args(
    kernel: &mut Kernel,
    history: &mut History,
    name: &Global,
    universes: &[Level],
    (left_params, left_indices): (&[Term], &[Term]),
    (right_params, right_indices): (&[Term], &[Term]),
) -> Result<bool, KernelError> {
    if left_params.len() != right_params.len() || left_indices.len() != right_indices.len() {
        return Ok(false);
    }

    let arity = match kernel.induct_decl(name) {
        Some(declaration) => declaration.arity.clone(),
        // A declaration the registry seeding refused: keep the grounded comparison rather than inventing types.
        None => {
            return Ok(compare_each(kernel, history, left_params, right_params)?
                && compare_each(kernel, history, left_indices, right_indices)?);
        }
    };
    let mut arity = instantiate_universe_levels_scoped(&arity, universes)?;

    // The parameters, against the outer telescope; each one opens the rest, because a later domain may name it.
    for (left, right) in left_params.iter().zip(right_params) {
        let Telescope::Cons(type_, rest) = arity else {
            return Ok(false);
        };
        if !compare(kernel, history, &type_, left, right)? {
            return Ok(false);
        }
        arity = rest.open(&[left]);
    }

    // Then the indices, which the parameter telescope terminates in — already scoped under the parameters just opened.
    let Telescope::Done(indices) = arity else {
        return Ok(false);
    };
    let mut telescope = *indices;

    for (left, right) in left_indices.iter().zip(right_indices) {
        let Telescope::Cons(type_, rest) = telescope else {
            return Ok(false);
        };
        if !compare(kernel, history, &type_, left, right)? {
            return Ok(false);
        }
        telescope = rest.open(&[left]);
    }

    Ok(true)
}

/// Eta at a nominal struct, untyped: a literal against a *neutral* inhabitant, projected field-wise. A `Prop`-sorted field converts by irrelevance without being compared — the declaration's field telescope says which those are — and the neutral restriction is what keeps an all-`Prop` or empty struct's vacuous field walk from equating its literal with an arbitrary term that merely appeared in the same untyped position.
fn struct_eta(
    kernel: &mut Kernel,
    history: &mut History,
    literal: &Struct,
    other: &Term,
) -> Result<bool, KernelError> {
    // Through the checked handle: a literal at the wrong parameter count would otherwise reach `fields_at`, which opens the arity and asserts. Declining is conversion's own answer for a shape it cannot decide, and it is the right one here too.
    let Ok(at) = kernel.struct_at(&literal.name, &literal.universes, &literal.params) else {
        return Ok(false);
    };

    let mut telescope = at.fields();
    for (index, field) in literal.fields.iter().enumerate() {
        let Telescope::Cons(type_, rest) = telescope else {
            return Ok(false);
        };
        telescope = rest.open(&[field]);

        if Sort::of(kernel, &type_)?.is_prop() {
            continue;
        }

        let projection = Term::from(Subterm::Proj(Proj {
            head: other.clone(),
            field: Field::Index(index),
        }));
        if !ground(kernel, history, field, &projection)? {
            return Ok(false);
        }
    }

    // The walk is driven by the literal's fields, so a literal shorter than the declaration ends it early; whether the telescope was consumed is what says the walk covered the type. Anything left standing is a field the neutral was never asked about, and accepting there would equate a malformed literal with any neutral at all.
    Ok(telescope.is_empty())
}

/// The last chance before a structural refusal: grant each side the one definitional unfolding `force` withheld, and compare the results. A refusal when neither side has a folded recursive spelling to open.
///
/// The unfoldings are compared untyped, through [`ground`]. Each retry opens a spelling whose spine may have *grown*, so its goals never recur into `seen` and the coinductive rule cannot close the chain; what stops an unproductive pair is [`compare`]'s budget, spent once per entry, rather than a count of how deep the retries have gone.
fn unfolded_retry(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Term,
    that: &Term,
) -> Result<bool, KernelError> {
    let left = unfold_spelling(kernel, this)?;
    let right = unfold_spelling(kernel, that)?;

    match (left, right) {
        (None, None) => Ok(false),
        (left, right) => ground(
            kernel,
            history,
            &left.unwrap_or_else(|| this.clone()),
            &right.unwrap_or_else(|| that.clone()),
        ),
    }
}

/// Open both scopes at one shared set of opaque binders and compare the bodies at `Type`. The binders are assumed at `Type` as a stand-in, sound because `ground` is already the untyped concession: a binder's recorded type feeds only the conversion history's context key, identically on both sides.
fn ground_scope(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Scope<Many>,
    that: &Scope<Many>,
) -> Result<bool, KernelError> {
    if this.arity() != that.arity() {
        return Ok(false);
    }

    kernel.scoped(|kernel| {
        let occurrences = opaque_binders(kernel, this.arity());
        let refs = occurrences.iter().collect::<Vec<_>>();
        ground(kernel, history, &this.open(&refs), &that.open(&refs))
    })
}

/// [`ground_scope`] at the free-monoid cons arities, whose scopes carry their binder count in the type.
fn ground_scope_two(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Scope<Two>,
    that: &Scope<Two>,
) -> Result<bool, KernelError> {
    kernel.scoped(|kernel| {
        let o = opaque_binders(kernel, 2);
        ground(
            kernel,
            history,
            &this.open(&[&o[0], &o[1]]),
            &that.open(&[&o[0], &o[1]]),
        )
    })
}

fn ground_scope_three(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Scope<Three>,
    that: &Scope<Three>,
) -> Result<bool, KernelError> {
    kernel.scoped(|kernel| {
        let o = opaque_binders(kernel, 3);
        ground(
            kernel,
            history,
            &this.open(&[&o[0], &o[1], &o[2]]),
            &that.open(&[&o[0], &o[1], &o[2]]),
        )
    })
}

fn opaque_binders(kernel: &mut Kernel, arity: usize) -> Vec<Term> {
    (0..arity)
        .map(|_| {
            let binder = kernel.fresh(None);
            kernel.assume(&binder, &Term::type_ground());
            Term::free_var(&binder)
        })
        .collect()
}

/// Compare two stuck eliminations' arm sets up to conversion, shape held rigid: matching variants, tags in the same canonical order, equal plicities, and agreeing default presence.
fn ground_cases(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Cases,
    that: &Cases,
) -> Result<bool, KernelError> {
    match (this, that) {
        (
            Cases::Bool {
                false_case: this_false,
                true_case: this_true,
            },
            Cases::Bool {
                false_case: that_false,
                true_case: that_true,
            },
        ) => Ok(ground(kernel, history, this_false, that_false)?
            && ground(kernel, history, this_true, that_true)?),

        (
            Cases::Switch {
                cases: this_cases,
                default: this_default,
            },
            Cases::Switch {
                cases: that_cases,
                default: that_default,
            },
        ) => {
            if this_cases.len() != that_cases.len() {
                return Ok(false);
            }
            for ((this_key, this_body), (that_key, that_body)) in this_cases.iter().zip(that_cases)
            {
                if this_key != that_key || !ground(kernel, history, this_body, that_body)? {
                    return Ok(false);
                }
            }
            ground(kernel, history, this_default, that_default)
        }

        (
            Cases::Induct {
                cases: this_cases,
                default: this_default,
            },
            Cases::Induct {
                cases: that_cases,
                default: that_default,
            },
        ) => {
            if this_cases.len() != that_cases.len() {
                return Ok(false);
            }
            for ((this_tag, this_arm), (that_tag, that_arm)) in this_cases.iter().zip(that_cases) {
                if this_tag != that_tag
                    || this_arm.plicities != that_arm.plicities
                    || !ground_scope(kernel, history, &this_arm.body, &that_arm.body)?
                {
                    return Ok(false);
                }
            }
            match (this_default, that_default) {
                (None, None) => Ok(true),
                (Some(this_default), Some(that_default)) => {
                    ground(kernel, history, this_default, that_default)
                }
                _ => Ok(false),
            }
        }

        (Cases::FreeMonoid { carrier: this }, Cases::FreeMonoid { carrier: that }) => {
            match (this, that) {
                (
                    Carrier::Nat {
                        empty_case: this_empty,
                        cons_case: this_cons,
                    },
                    Carrier::Nat {
                        empty_case: that_empty,
                        cons_case: that_cons,
                    },
                ) => Ok(ground(kernel, history, this_empty, that_empty)?
                    && ground_scope_two(kernel, history, this_cons, that_cons)?),
                (
                    Carrier::Bin {
                        grain: this_grain,
                        empty_case: this_empty,
                        cons_case: this_cons,
                    },
                    Carrier::Bin {
                        grain: that_grain,
                        empty_case: that_empty,
                        cons_case: that_cons,
                    },
                ) => Ok(this_grain == that_grain
                    && ground(kernel, history, this_empty, that_empty)?
                    && ground_scope_three(kernel, history, this_cons, that_cons)?),
                (
                    Carrier::List {
                        elem: this_elem,
                        empty_case: this_empty,
                        cons_case: this_cons,
                    },
                    Carrier::List {
                        elem: that_elem,
                        empty_case: that_empty,
                        cons_case: that_cons,
                    },
                ) => Ok(ground(kernel, history, this_elem, that_elem)?
                    && ground(kernel, history, this_empty, that_empty)?
                    && ground_scope_three(kernel, history, this_cons, that_cons)?),
                _ => Ok(false),
            }
        }

        _ => Ok(false),
    }
}

/// Compare two telescopes: domains pairwise, opening one shared binder per position so both dependent tails speak of the same variable, then whatever the terminal clause decides.
///
/// Π and Σ differ *only* there — a function type's terminal is its codomain and must be compared, a record type's carries nothing — so the terminal clause is the only thing either caller states, and the shared-binder discipline every dependent comparison rests on is written once.
fn compare_binders<B: Bound>(
    kernel: &mut Kernel,
    history: &mut History,
    this: Telescope<B>,
    that: Telescope<B>,
    terminal: impl FnOnce(&mut Kernel, &mut History, B, B) -> Result<bool, KernelError>,
) -> Result<bool, KernelError> {
    kernel.scoped(|kernel| {
        let (mut this, mut that) = (this, that);

        loop {
            match (this, that) {
                (Telescope::Cons(left, left_rest), Telescope::Cons(right, right_rest)) => {
                    if !ground(kernel, history, &left, &right)? {
                        return Ok(false);
                    }

                    let binder = kernel.fresh(left_rest.first_hint());
                    kernel.assume(&binder, &left);
                    let occurrence = Term::free_var(&binder);

                    this = left_rest.open(&[&occurrence]);
                    that = right_rest.open(&[&occurrence]);
                }
                (Telescope::Done(left), Telescope::Done(right)) => {
                    return terminal(kernel, history, *left, *right);
                }
                // Different arities. A function type is not curried in this representation, so this is a real mismatch rather than a shape to normalize.
                _ => return Ok(false),
            }
        }
    })
}

/// [`compare_binders`] for a Π or a λ, whose terminal is a codomain to be compared.
fn compare_telescope(
    kernel: &mut Kernel,
    history: &mut History,
    this: Telescope<Term>,
    that: Telescope<Term>,
) -> Result<bool, KernelError> {
    compare_binders(
        kernel,
        history,
        this,
        that,
        |kernel, history, left, right| ground(kernel, history, &left, &right),
    )
}

/// [`compare_binders`] for a Σ, whose terminal carries nothing.
fn compare_field_telescope(
    kernel: &mut Kernel,
    history: &mut History,
    this: Telescope<()>,
    that: Telescope<()>,
) -> Result<bool, KernelError> {
    compare_binders(kernel, history, this, that, |_, _, (), ()| Ok(true))
}

/// Compare two term sequences pairwise at `Type`. Length is part of the shape.
fn compare_each(
    kernel: &mut Kernel,
    history: &mut History,
    this: &[Term],
    that: &[Term],
) -> Result<bool, KernelError> {
    if this.len() != that.len() {
        return Ok(false);
    }

    for (left, right) in this.iter().zip(that) {
        if !ground(kernel, history, left, right)? {
            return Ok(false);
        }
    }

    Ok(true)
}

/// [`compare`] at `Type`, for a child position whose type its head does not hand us. Weaker than a typed comparison, never stronger: see the module documentation on incompleteness.
fn ground(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Term,
    that: &Term,
) -> Result<bool, KernelError> {
    compare(kernel, history, &Term::type_ground(), this, that)
}
