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
//! `curios-elab`'s conversion checker canonicalizes differently: it renames the binders *it* minted, in mint order, and does not key on the context at all. For a strictly nested walk like this one the two orderings coincide, since the binders in scope at a goal are exactly the path to it. The elaborator's walk is not strictly nested — it has a worklist and it parks goals — so whether the two schemes agree there is a genuinely open question, recorded as such in `documentation/soundness/per-term-rules/conversion-recurrence.md`. This module deliberately does not inherit the answer. For one population it is answered: on two instances of one recursive group the elaborator's key collided where this one never could, and each checker now decides the pair by its levels instead — `rec_instances` here, the level identification there — so neither reaches its recurrence rule on it.
//!
//! # Where this is incomplete, and why that is the safe direction
//!
//! One concession remains — a `rec` group used to be a second, compared syntactically, until two instances of one group at two universe levels showed a syntactic refusal turning into an unfolding that never returned; `rec_instances` now decides such a pair by its levels under the item's hypotheses, the equation `induct_type_args` and the instance arms already apply, and two different groups are refused as before. Every child position without a typed context — a stuck elimination's motive and arms under their opaque binders, and a projection's or an instance's head — is compared at `Type` rather than at the types its head assigns, which forfeits eta and irrelevance there. Each is a place where the kernel may reject a term the elaborator accepted. An application spine's arguments left that list when a *variable* head was found to carry the telescope they inhabit: reading it is a lookup rather than an inference, so `compare_arguments` types them at no new cost and at no new thing consulted, and a head that names no type still grounds. A struct type's, a struct literal's and a constructor's parameters left it with `params_at`, which reads the declaration's outer telescope the way `induct_type_args` reads a family's — the asymmetry between them was an accident of which shape a witness forced first, not a rule. An inductive type-former's arguments left this list when the compile path put real programs through the kernel: they are compared at the declaration's own index telescope (`induct_type_args`), which is what lets `Eq(@P, p, q)` at a `Prop`-sorted `P` convert with `Eq(@P, p, p)`. A struct literal's fields and a constructor's payload left it when the proof-carrying idiom met it: two `Str`s built from different proofs of the same bytes were unequal here and equal to the elaborator, so both now compare at the declaration's telescope (`compare_fields_at`), which is what lets a proof field discharge without being read.
//!
//! That direction is deliberate. An incomplete conversion refuses programs; an unsound one admits them. A refusal is visible — it is a disagreement between the two checkers, which is precisely the signal this kernel exists to produce — whereas an over-eager acceptance is silent and is exactly what a second opinion is supposed to catch. Every one of these can be strengthened later against a real program that needs it, and none can be strengthened back from having been wrong.

mod intrinsic;
use intrinsic::convert_intrinsic;

#[cfg(test)]
mod conversion_tests;
#[cfg(test)]
mod irrelevance_tests;
#[cfg(test)]
mod recursion_tests;
#[cfg(test)]
mod test_support;

use {
    super::{Counted, Kernel, KernelError, Sort, infer, unfold_spelling},
    curios_core::{
        Apply, Bound, Carrier, Cases, Cost, Field, FuncType, Global, InductType, Instance, Level,
        Many, MatchResult, Proj, Reducer, Scope, Struct, StructType, Subterm, Telescope, Term,
        Three, Tuple, TupleType, Two, decide_bool, instantiate_universe_levels_scoped,
        is_bool_connective, strip_universe_levels,
    },
    curios_utilities::recurse,
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
        curios_profile::profile!("convert::enter");
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
/// An unfolding retry recurses back into here, and what bounds that chain is the budget spent on entry rather than a count of how deep it has gone — a constant standing in for the call stack is what [`recurse`] makes unnecessary. Measured over the whole `curios` corpus — every test, the entire fixed prelude — the deepest chain real code reaches is three. One chain the budget could not bound in practice is closed at its head instead: two instances of one recursive group at two universe instances reproduce themselves under every unfolding, and [`rec_instances`] makes such a pair a verdict before any retry is granted.
fn compare(
    kernel: &mut Kernel,
    history: &mut History,
    type_: &Term,
    this: &Term,
    that: &Term,
) -> Result<bool, KernelError> {
    recurse(|| {
        kernel.spend(Cost::STEP)?;

        // Cheapest first: a term converts with itself at any type, and structural sharing makes this hit constantly on terms built by substitution.
        if this == that {
            return Ok(true);
        }

        // Proof irrelevance. Deliberately before reduction: the point is that neither side is examined, and reducing a proof in order to discover it equals another proof is work whose answer was already known.
        if Sort::of(kernel, type_)?.is_prop() {
            return Ok(true);
        }

        // Two projections of one recursive group at two universe instances are a verdict, not a comparison: unfolding either reproduces the pair one level down, so their levels decide here — before `reduce_forced` opens a function member into its lambda, and before the goal is entered, so nothing has to be left.
        if let Some(verdict) = rec_instances(kernel, this, that) {
            return Ok(verdict);
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
        (Subterm::FuncType(left), Subterm::FuncType(right)) => Ok(left.plicities()
            == right.plicities()
            && compare_telescope(
                kernel,
                history,
                left.telescope.clone(),
                right.telescope.clone(),
            )?),

        // Two lambdas with no expected type to eta against: compare their bodies under one shared set of binders.
        (Subterm::Func(left), Subterm::Func(right)) => Ok(left.plicities() == right.plicities()
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
        ) => compare_each(kernel, history, left.iter(), right.iter()),

        // Spine against spine, and when that fails, one definitional unfolding each: two applications of the same fold can differ in an argument position the fold discards — `is_trimmed(h ++ rest)` against `is_trimmed(rest)` — so a spine mismatch is not yet a verdict when either head is a folded recursive call. Two heads that are instances of one group are decided by their levels first: at unequal levels the pair is refused outright, because an unfolding reproduces the same two heads on the recursive call and would recurse until the host died; at equal levels the spines decide, and a mismatch there still earns the retry.
        (Subterm::Apply(left), Subterm::Apply(right)) => {
            if left.plicities().eq(right.plicities()) {
                let heads = match rec_instances(kernel, &left.head, &right.head) {
                    Some(false) => return Ok(false),
                    Some(true) => true,
                    None => ground(kernel, history, &left.head, &right.head)?,
                };

                if heads && compare_arguments(kernel, history, left, right)? {
                    return Ok(true);
                }
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
        ) => {
            if left_name != right_name || !kernel.levels_eq(left_universes, right_universes) {
                return Ok(false);
            }
            let telescope = struct_params(kernel, left_name, left_universes);
            params_at(kernel, history, telescope, left_params, right_params)
        }

        // The payload compares at the constructor's own telescope, opened at the left side's parameters and then at each preceding payload, so a `Prop`-sorted payload discharges by irrelevance without being read — the discipline `eta_tuple` follows at a Σ, and what the elaborator's `compare_variant` does. A tag the declaration does not carry leaves the telescope absent, and the payload then compares at `Type` as it always did, which admits nothing new.
        (Subterm::Variant(left), Subterm::Variant(right)) => {
            if left.name != right.name
                || left.tag != right.tag
                || !kernel.levels_eq(&left.universes, &right.universes)
            {
                return Ok(false);
            }
            let params = induct_params(kernel, &left.name, &left.universes);
            if !params_at(kernel, history, params, &left.params, &right.params)? {
                return Ok(false);
            }
            let telescope = kernel
                .induct_at_params(&left.name, &left.universes, &left.params)
                .ok()
                .and_then(|at| at.signature(&left.tag));
            compare_fields_at(kernel, history, telescope, &left.payload, &right.payload)
        }

        // A struct literal's fields compare at the declaration's field telescope, as the variant's payload does above — the typed half of what `struct_eta` reads the same telescope for.
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
        ) => {
            if left_name != right_name || !kernel.levels_eq(left_universes, right_universes) {
                return Ok(false);
            }
            let params = struct_params(kernel, left_name, left_universes);
            if !params_at(kernel, history, params, left_params, right_params)? {
                return Ok(false);
            }
            let telescope = kernel
                .struct_at(left_name, left_universes, left_params)
                .ok()
                .map(|at| at.fields());
            compare_fields_at(kernel, history, telescope, left_fields, right_fields)
        }

        // Eta at a nominal struct, against a neutral inhabitant only — see `struct_eta` for the rule and the restriction.
        // A *stuck application* is a neutral inhabitant as much as a variable or a projection is — it survived `reduce_forced`, so its head is stuck — and it is the shape a standard-library law meets: `State`'s left identity sets `State/bind`'s literal against the neutral `f(a)`. It reaches `struct_eta` through the same door, with one difference that is not optional: where a variable and a projection have nothing left to unfold, an application may, so a refusal here falls through to the retry this arm used to reach directly rather than standing as the verdict.
        (Subterm::Struct(literal), _) if matches!(&**that, Subterm::Apply(_)) => {
            match struct_eta(kernel, history, literal, that)? {
                true => Ok(true),
                false => unfolded_retry(kernel, history, this, that),
            }
        }
        (_, Subterm::Struct(literal)) if matches!(&**this, Subterm::Apply(_)) => {
            match struct_eta(kernel, history, literal, this)? {
                true => Ok(true),
                false => unfolded_retry(kernel, history, this, that),
            }
        }

        (Subterm::Struct(literal), _) if matches!(&**that, Subterm::Var(_) | Subterm::Proj(_)) => {
            struct_eta(kernel, history, literal, that)
        }
        (_, Subterm::Struct(literal)) if matches!(&**this, Subterm::Var(_) | Subterm::Proj(_)) => {
            struct_eta(kernel, history, literal, this)
        }

        (
            Subterm::Instance(Instance {
                head: left,
                levels: left_levels,
            }),
            Subterm::Instance(Instance {
                head: right,
                levels: right_levels,
            }),
        ) => Ok(kernel.levels_eq(left_levels, right_levels)
            && ground(kernel, history, &left.to_term(), &right.to_term())?),

        // A stuck elimination. Everything is compared up to conversion: the scrutinee because that is the position an unfolding cycle travels through, and the motive and arms because a delta-unfolded caller and its spelled-out twin differ exactly there — `step(c, st)` against `step(at(cons(c, t), 0, _), st)` reduces to two stuck matches whose arms are convertible but not identical. The shape stays rigid: tags, plicities, arity, and default presence must agree exactly, because two eliminations enumerating different constructors compute differently on some input even where they agree on this one.
        (Subterm::Match(left), Subterm::Match(right)) => {
            Ok(ground(kernel, history, &left.head, &right.head)?
                && match (&left.result, &right.result) {
                    (MatchResult::Family(this), MatchResult::Family(that)) => {
                        ground_scope(kernel, history, this, that)?
                    }
                    (MatchResult::Ambient(this), MatchResult::Ambient(that)) => {
                        ground(kernel, history, this, that)?
                    }
                    // One source match reaches both forms: written over a variable it is an ambient goal, and that goal substituted at an expression — a definition's `match o` unfolded at `o := f(x)` — meets the family the same match elaborates to where it was written over `f(x)`. A family at the scrutinee itself *is* the elimination's type, so the two results compare at that instance.
                    (MatchResult::Family(motive), MatchResult::Ambient(goal))
                    | (MatchResult::Ambient(goal), MatchResult::Family(motive)) => {
                        let at_head = family_at_head(kernel, motive, &left.head)?;
                        ground(kernel, history, &at_head, goal)?
                    }
                }
                && ground_cases(kernel, history, &left.cases, &right.cases)?)
        }

        // A folded recursive call, and a `rec` that forcing declined to unfold. Two projections of one group are compared up to their universe instance, the levels decided by entailment; two different groups are refused here without a retry, because the interesting case — a cycle that unfolds without disagreeing — is handled by the recurrence rule above, not here. A `rec` whose tail computes something is *not* this case, and falls through to the delta step below.
        (Subterm::Rec(_), Subterm::Rec(_))
            if this.as_rec_proj().is_some() && that.as_rec_proj().is_some() =>
        {
            Ok(rec_instances(kernel, this, that) == Some(true))
        }

        // A `Bool` connective against a term that is no intrinsic at all — absorption's shape, `b || (b && c)` against the bare `b` — which the intrinsic congruence never sees. The truth table over the two sides' atoms decides it equal or says nothing, and saying nothing leaves the pair where it was.
        _ if is_bool_connective(this) || is_bool_connective(that) => {
            match decide_bool(kernel, this, that)? {
                true => Ok(true),
                false => unfolded_retry(kernel, history, this, that),
            }
        }

        // Two spellings of one recursive call: `force` keeps the folded application as a recursive call's normal form, while an arm's induction hypothesis is the raw stuck fold-match on the same argument. When the heads disagree, grant each side the one definitional unfolding `force` withheld and compare what results.
        _ => unfolded_retry(kernel, history, this, that),
    }
}

/// Two parameter vectors at the types a declaration's outer telescope assigns them, each opened at the left side's preceding actuals because a later domain may name an earlier parameter.
///
/// `None` for the telescope keeps the grounded comparison: a declaration the registry seeding refused has no types to compare at, and inventing some would be worse than the concession. Shared by the three nominal shapes that carry parameters — a struct type, a struct literal and a constructor value — which reach it from `StructDecl::arity`'s outer half and `InductDecl::arity`'s, the same halves `induct_type_args` walks for a family.
fn params_at(
    kernel: &mut Kernel,
    history: &mut History,
    telescope: Option<Telescope<Telescope<()>>>,
    this: &[Term],
    that: &[Term],
) -> Result<bool, KernelError> {
    if this.len() != that.len() {
        return Ok(false);
    }

    let Some(mut telescope) = telescope else {
        return compare_each(kernel, history, this.iter(), that.iter());
    };

    for (left, right) in this.iter().zip(that) {
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

/// A struct declaration's parameter telescope at `universes`, or `None` where the declaration is absent or its levels do not instantiate.
fn struct_params(
    kernel: &Kernel,
    name: &Global,
    universes: &[Level],
) -> Option<Telescope<Telescope<()>>> {
    let arity = kernel.struct_decl(name)?.arity.clone();

    instantiate_universe_levels_scoped(&arity, universes).ok()
}

/// The same for an inductive family, which a constructor value's parameters are the family's.
fn induct_params(
    kernel: &Kernel,
    name: &Global,
    universes: &[Level],
) -> Option<Telescope<Telescope<()>>> {
    let arity = kernel.induct_decl(name)?.arity.clone();

    instantiate_universe_levels_scoped(&arity, universes).ok()
}

/// Argument-wise comparison of two instances of one inductive family, each pair at the type the declaration's full index telescope assigns, opened at the left instance's preceding actuals — the typed context the grounded comparison forfeits. Irrelevance at a `Prop`-typed index is the observable difference: `Eq(@P, p, q)` with `P : Prop` converts with `Eq(@P, p, p)`, because no proof of a proposition is distinguishable from another. The struct and constructor analogs are [`params_at`]'s, which reads the same outer telescope this does.
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
            return Ok(
                compare_each(kernel, history, left_params.iter(), right_params.iter())?
                    && compare_each(kernel, history, left_indices.iter(), right_indices.iter())?,
            );
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

/// Eta at a nominal struct: a literal against a *neutral* inhabitant — a variable, a projection or a stuck application — projected field-wise and compared at the type the declaration gives each field. A `Prop`-sorted field converts by irrelevance without being compared at all, which the same telescope decides.
///
/// **What licenses the projection is an invariant about the callers, not the shape of `other`.** Conversion is only ever asked whether two terms *of one type* are equal: the entry point carries the type, every typed recursion passes the one its position assigns — a field's from this telescope, an argument's from its head's, an index's from the family's — and [`ground`] discards the kernel's *knowledge* of that type without changing the fact. So `other` inhabits the struct type the literal is a value of, and eta for a single-constructor record — every inhabitant `x` equals `S { x.0, …, x.(n-1) }` — is what decides the pair.
///
/// That invariant is stated here and checked nowhere, which is why the walk is restricted to neutrals at all: it is a proxy, not a second guarantee. A `Var` is as arbitrary a term as any other, so what the restriction actually buys is that a pair arriving from a caller that broke the invariant is unlikely to be *shaped* like an inhabitant — thin, and worth knowing it is thin, because an all-`Prop` or empty struct's field walk compares nothing and answers `true`. Making it load-bearing instead means handing this function the goal type, which `structural` does not receive; under [`ground`] that type is `Type` and would forfeit the walk exactly where it is reached untyped today.
///
/// The field comparison used to be [`ground`]'s, and a function-typed field is what showed the cost: a literal's lambda never meets a neutral's projection at `Type`, so `/std/State`'s identity laws — its own monad's equations — were refused by this kernel and accepted by the elaborator.
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
        // At the field's own declared type, which this walk is already holding: the declaration says what the field is, so comparing there is what lets eta fire on a *function*-typed field — a literal's lambda against the neutral's projection, which at `Type` never meet. `/std/State`'s monad laws are the shape that needs it, `State/bind` building a literal whose one field is a lambda and the law's other side being a neutral `State`.
        if !compare(kernel, history, &type_, field, &projection)? {
            return Ok(false);
        }
    }

    // The walk is driven by the literal's fields, so a literal shorter than the declaration ends it early; whether the telescope was consumed is what says the walk covered the type. Anything left standing is a field the neutral was never asked about, and accepting there would equate a malformed literal with any neutral at all.
    Ok(telescope.is_empty())
}

/// The last chance before a structural refusal: grant each side the one definitional unfolding `force` withheld, and compare the results. A refusal when neither side has a folded recursive spelling to open.
///
/// The unfoldings are compared untyped, through [`ground`]. Each retry opens a spelling whose spine may have *grown*, so its goals never recur into `seen` and the coinductive rule cannot close the chain; what stops an unproductive pair is [`compare`]'s budget, spent once per entry, rather than a count of how deep the retries have gone. One pair the budget never reached is refused at the door: two instances of one group at unequal levels unfold to the same two instances, and every round added two opaque binders to a context the history key copies whole, so the walk grew until the host died with the budget barely spent. [`rec_instances`] answers that pair before and after the unfolding, and a retry is granted only to heads it does not decide.
fn unfolded_retry(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Term,
    that: &Term,
) -> Result<bool, KernelError> {
    if rec_instances(kernel, applied_head(this), applied_head(that)) == Some(false) {
        return Ok(false);
    }

    let left = unfold_spelling(kernel, this)?;
    let right = unfold_spelling(kernel, that)?;

    match (left, right) {
        (None, None) => Ok(false),
        (left, right) => {
            let left = left.unwrap_or_else(|| this.clone());
            let right = right.unwrap_or_else(|| that.clone());

            // A `rec` block whose tail applies one of its members reaches `Apply(rec_proj)` only once that tail is opened, which is the unfolding just taken, so the guard is asked again of what it produced.
            if rec_instances(kernel, applied_head(&left), applied_head(&right)) == Some(false) {
                return Ok(false);
            }

            ground(kernel, history, &left, &right)
        }
    }
}

/// `Some(verdict)` when `this` and `that` are projections of one recursive group at the same member, differing in nothing but universe levels — the verdict is whether those levels are equal under the item's hypotheses. `None` for any other pair, which the structural rules judge as before.
///
/// This is the equation the `InductType`, `StructType`, `Variant`, `Struct` and `Instance` arms already apply to their heads, reaching one more head kind. Equal skeletons mean the same positions carry a level on both sides — `strip_universe_levels`'s sentinel is what makes an unvisited `Type 0` and a stripped level distinguishable — so the two terms are one skeleton over two aligned level vectors, and a vector pair that is syntactically equal or, at depth zero, mutually entailed under the assumed constraints denotes one term in every instance satisfying them. Nothing is erased: `wrap(Type 1)` against `wrap(Type 2)` refuses on the levels, `wrap(Type 0)` against `wrap(Type 1)` on the skeletons. What it does not decide is refused, the safe direction: `Type 0` against a `Type u` the hypotheses force to zero has two skeletons.
fn rec_instances(kernel: &Kernel, this: &Term, that: &Term) -> Option<bool> {
    let (Some((_, left)), Some((_, right))) = (this.as_rec_proj(), that.as_rec_proj()) else {
        return None;
    };

    if left != right {
        return None;
    }

    if this == that {
        return Some(true);
    }

    let (this_skeleton, this_levels) = strip_universe_levels(this);
    let (that_skeleton, that_levels) = strip_universe_levels(that);

    (this_skeleton == that_skeleton).then(|| kernel.level_pairs_eq(&this_levels, &that_levels))
}

/// The head of an application, or the term itself: what `rec_instances` is asked about when a folded recursive call arrives applied.
fn applied_head(term: &Term) -> &Term {
    match &**term {
        Subterm::Apply(apply) => &apply.head,
        _ => term,
    }
}

/// Open both scopes at one shared set of opaque binders and compare the bodies at `Type`. The binders are assumed at `Type` as a stand-in, sound because `ground` is already the untyped concession: a binder's recorded type feeds only the conversion history's context key, identically on both sides.
/// A family opened at the scrutinee's actual indices and the scrutinee — the elimination's own type. An unindexed family binds the scrutinee alone; an indexed one is opened at the indices its scrutinee's type carries, read by inference, which the head's own typing has already paid for.
fn family_at_head(
    kernel: &mut Kernel,
    motive: &Scope<Many>,
    head: &Term,
) -> Result<Term, KernelError> {
    let mut arguments = Vec::with_capacity(motive.arity());
    if motive.arity() > 1 {
        let head_type = infer(kernel, head)?;
        let head_type = kernel.reduce_forced(head_type)?;
        if let Subterm::InductType(InductType { indices, .. }) = &*head_type {
            arguments.extend(indices.iter().cloned());
        }
    }
    arguments.push(head.clone());
    if arguments.len() != motive.arity() {
        return Err(KernelError::Arity {
            counted: Counted::MotiveBinders,
            expected: motive.arity(),
            actual: arguments.len(),
        });
    }
    let refs = arguments.iter().collect::<Vec<_>>();
    Ok(motive.open(&refs))
}

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
                    || this_arm.plicities() != that_arm.plicities()
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
/// A spine's arguments at the types its head assigns them, and at `Type` where it assigns none.
///
/// **The head's type is the typed context this position was said to lack.** A variable head carries one — it was assumed or declared at it — so the domains of its function type are what an argument inhabits, exactly as a struct's field telescope is what a field inhabits ([`compare_fields_at`]). Comparing there is what lets eta and irrelevance fire: `f(p)` against `f(q)` for two proofs of one proposition is discharged without reading either, which is the acceptance `a_grounded_argument_forfeits_irrelevance` recorded the kernel refusing while the elaborator accepted.
///
/// **What still grounds.** A head that is not a variable — a projection, a stuck elimination, a `rec` member — hands back no telescope here, and neither does a variable whose recorded type is not a function of this arity: a binder opened by [`ground_scope`] carries the stand-in `Type`, so a comparison under one keeps the untyped concession it was opened with, and `a_grounded_motive_binder_carries_the_stand_in_rather_than_its_real_type` is the fixture that says so. Reading the type is a lookup rather than an inference, so a spine costs what it did.
///
/// The justification is the callers': every pair compared here is the corresponding children of two parents already shown convertible, so the two heads have one type and one telescope to assign.
fn compare_arguments(
    kernel: &mut Kernel,
    history: &mut History,
    left: &Apply,
    right: &Apply,
) -> Result<bool, KernelError> {
    let Some(telescope) = spine_telescope(kernel, &left.head, left.arguments.len())? else {
        return compare_each(kernel, history, left.params(), right.params());
    };

    let this = left.params().cloned().collect::<Vec<_>>();
    let that = right.params().cloned().collect::<Vec<_>>();

    compare_fields_at(kernel, history, Some(telescope), &this, &that)
}

/// The function type a variable head was bound at, opened for `arity` arguments — or `None` where the head names no type, or names one that is not a function of that arity.
fn spine_telescope(
    kernel: &mut Kernel,
    head: &Term,
    arity: usize,
) -> Result<Option<Telescope<Term>>, KernelError> {
    let Subterm::Var(var) = &**head else {
        return Ok(None);
    };
    let Some(name) = var.as_free() else {
        return Ok(None);
    };
    let Some(type_) = kernel.type_of(name)?.cloned() else {
        return Ok(None);
    };

    let Subterm::FuncType(FuncType { telescope, .. }) =
        Term::unwrap_or_clone(kernel.reduce_forced(type_)?)
    else {
        return Ok(None);
    };

    match telescope.len() == arity {
        true => Ok(Some(telescope)),
        false => Ok(None),
    }
}

fn compare_each<'a>(
    kernel: &mut Kernel,
    history: &mut History,
    this: impl ExactSizeIterator<Item = &'a Term>,
    that: impl ExactSizeIterator<Item = &'a Term>,
) -> Result<bool, KernelError> {
    if this.len() != that.len() {
        return Ok(false);
    }

    for (left, right) in this.zip(that) {
        if !ground(kernel, history, left, right)? {
            return Ok(false);
        }
    }

    Ok(true)
}

/// Compare two field sequences at the types `telescope` assigns them, each `rest` opened at the left field's value — so a field at a proposition is discharged by irrelevance without being read, and a dependent field is compared at the type its predecessors determine. A telescope shorter than the fields, or none at all, leaves the remainder at `Type`, which is what the untyped comparison did and can admit nothing it did not. Length is part of the shape.
fn compare_fields_at<B: Bound>(
    kernel: &mut Kernel,
    history: &mut History,
    mut telescope: Option<Telescope<B>>,
    this: &[Term],
    that: &[Term],
) -> Result<bool, KernelError> {
    if this.len() != that.len() {
        return Ok(false);
    }

    for (left, right) in this.iter().zip(that) {
        let type_ = match telescope.take() {
            Some(Telescope::Cons(type_, rest)) => {
                telescope = Some(rest.open(&[left]));
                type_
            }
            _ => Term::type_ground(),
        };
        if !compare(kernel, history, &type_, left, right)? {
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
