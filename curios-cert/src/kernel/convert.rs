//! Definitional equality: whether two terms are interchangeable at a type.
//!
//! Conversion is the rule that decides which programs typecheck, so it is the most consequential thing in this crate. It is *type-directed*: the type drives eta-expansion and it is what makes proof irrelevance possible, so every goal carries the type at which the two sides are being compared.
//!
//! The rules, in the order they are tried:
//!
//! 1. **Proof irrelevance.** A goal at a `Prop`-sorted type is discharged without looking at either side. Any two inhabitants of a proposition are definitionally equal, which is what lets erasure drop them wholesale. 2. **One definition's spines.** Two applications of one definition are compared argument by argument before either is unfolded; agreeing spines decide the goal and disagreeing ones decide nothing. 3. **Eta.** At a function type both sides are applied to fresh binders and compared at the codomain; at a Σ type both are projected and compared componentwise, so at the empty Σ nothing is left to compare and any two inhabitants convert. So `f` and `(x) => f(x)` convert, and so do `p` and `(p.0, p.1)`, without either side having to be in that shape. A nominal struct has no eta by its type — its literal opens one — so between two neutrals at one the goal is decided where the struct has one inhabitant by its shape (`one_inhabitant`), which is all eta would decide between them. 4. **Structure.** Both sides are reduced to weak-head normal form and their heads compared, recursing on the children.
//!
//! # Termination, and the recurrence rule
//!
//! Two folded recursive calls can unfold forever without ever disagreeing — that is what an equirecursive type is. Conversion therefore keeps a history of the goals it is already inside, and a goal that recurs is *assumed to hold*. This is the coinductive reading: a genuine cycle leaves nothing but itself to check, and any finite disagreement surfaces on a sibling goal before the cycle closes.
//!
//! That rule is where a conversion checker is most likely to be unsound, and the danger is precise: a history hit on two goals that are *not* the same goal accepts terms that are not equal. The guard here is that an entry records the local context alongside the goal, and that every binder in scope is renamed to its position before the entry is made. Two entries collide only when the same comparison is being made under binders of the same types in the same order — which is the same comparison.
//!
//! `curios-elab`'s conversion checker canonicalizes differently: it renames the binders *it* minted, in mint order, and does not key on the context at all. For a strictly nested walk like this one the two orderings coincide, since the binders in scope at a goal are exactly the path to it. The elaborator's walk is not strictly nested — it has a worklist and it parks goals — so whether the two schemes agree there is a genuinely open question, recorded as such in `documentation/design/soundness/conversion/conversion-recurrence.md`. This module deliberately does not inherit the answer. For one population it is answered: two instances of one recursive group would collide under the elaborator's key where this one never could, and each checker decides such a pair by its levels instead — `rec_instances` here, the level identification there — so neither reaches its recurrence rule on it.
//!
//! # What is remembered
//!
//! A goal decided with no goal in progress assumed is a fact, and its verdict is kept for as long as what it read of the scope stands (`Memos`): two terms that are equal graphs built apart reach each pair of their nodes along every path, and remembered by pair each is compared once. A goal whose deciding met one in progress is not kept, whichever way it went: accepted, it may hold only by the assumption; refused, it may have been refused by a classing that left two atoms apart because their comparison was in progress. `History::assumed` counts both, and a verdict is filed only where the count stood still across it. The sets are plain, with no closure taken over accepted pairs, for the reason Lean's kernel gives for its own: a closure's result would depend on the order goals were met in.
//!
//! # Where a child has no type of its own, and why refusing is the safe direction
//!
//! Some child positions are handed no type: a stuck elimination's scrutinee, its motive and its arms' bodies, and a projection's or an instance's head are compared at `Type`. Nothing a type directs is forfeited there. Between two sides a lookup types — a neutral by its head, a stuck elimination by its result at its scrutinee, a constructor's value by its declaration (`looked_up`) — what the type directs is read off it (`by_their_own_type`): that it is a proposition, or has one inhabitant by its shape. Eta by a literal needs no type — a lambda, a tuple literal or a struct literal against a neutral inhabitant states what the type would have, and fires there as it does anywhere (`function_eta`, `tuple_eta`, `struct_eta`) — and two literals are compared by their parts. And a binder a motive or an arm opens is opened at the type its position gives it, read off the scrutinee's looked-up type (`motive_binders`, `ground_cases`), so a lookup under it reads what the elimination's typing read. The stand-in `Type` is what such a binder keeps where no lookup types the scrutinee, which no checked term reaches and which can only refuse. Everything else is typed. An application spine's arguments compare at the telescope its head carries — a variable, a universe instance of one or a projection of a `rec` group, and an application or a record projection of one, and a stuck elimination, each read by `synth_neutral` as a lookup rather than an inference (`compare_arguments`). An inductive type-former's arguments compare at the declaration's own index telescope (`induct_type_args`), which is what lets `Eq(@P)(p, q)` at a `Prop`-sorted `P` convert with `Eq(@P)(p, p)`; a struct type's, a struct literal's and a constructor's parameters at the declaration's outer telescope (`params_at`); and a struct literal's fields and a constructor's payload at the declaration's telescope (`compare_fields_at`), which is what lets a proof field discharge without being read, so two `Str`s built from different proofs of the same bytes are one value. Two applications of one definition are compared by their spines *before* either is unfolded and ahead of eta by the goal's type, as the elaborator compares them (`one_definition_by_its_spines`), which keeps the two calls folded: unfolded first a proof argument lands in a stuck scrutinee, where the type a lookup gives it is what equates it, and after eta the pair is no application of the definition at its head. A nominal node, a type or a value of it, compares its levels where its family is invariant in them and at nothing where it is irrelevant (`Kernel::instances_eq`). Two instances of one `rec` group are decided by their levels under the item's hypotheses (`rec_instances`), every one of them, the equation a definition's instance takes, and two different groups are refused; two instances of a family's former that part in a level are left to the nodes they build (`projects_a_former`).
//!
//! Where a lookup gives no type, the direction is deliberate. An incomplete conversion refuses programs; an unsound one admits them. A refusal is visible — it is a disagreement between the two checkers, which is precisely the signal this kernel exists to produce — whereas an over-eager acceptance is silent and is exactly what a second opinion is supposed to catch. A refusal can be strengthened later against a real program that needs it, and none can be strengthened back from having been wrong.

mod intrinsic;
use intrinsic::*;

#[cfg(test)]
mod conversion_tests;
#[cfg(test)]
mod irrelevance_tests;
#[cfg(test)]
mod recursion_tests;
#[cfg(test)]
mod test_support;
#[cfg(test)]
mod variance_tests;

use {
    super::{Counted, Error, Kernel, Sort, synth_neutral, unfold_spelling},
    curios_analysis::struct_reaches_itself,
    curios_core::{
        Apply, Bound, Carrier, Cases, Cost, Cursor, Field, Func, FuncType, Global, InductType,
        Instance, InstanceHead, Level, Lockstep, Many, Match, MatchResult, Probe, Proj, Reducer,
        Scope, Step, Struct, StructType, Subterm, Telescope, Term, Tuple, TupleType, Variant,
        instantiate_universe_levels_scoped,
    },
    curios_utilities::recurse,
    std::collections::HashSet,
};

/// Whether `this` and `that` are definitionally equal at `type_`.
pub fn convert(kernel: &mut Kernel, type_: &Term, this: &Term, that: &Term) -> Result<bool, Error> {
    curios_profile::profile!("convert");
    let mut history = History::default();

    // Conversion reads by value, as reduction does and for its reason: it compares spellings, keys its history and its verdicts on them, and opens no `let`.
    let (type_, this, that) = (
        kernel.by_value(type_),
        kernel.by_value(this),
        kernel.by_value(that),
    );
    kernel.reading(|kernel| compare(kernel, &mut history, &type_, &this, &that))
}

/// The goals conversion is currently inside.
///
/// A goal is stored with the types of the binders in scope at it, and with every one of those binders renamed to its position. Without the rename, the same comparison reached on two rounds of an unfolding cycle differs in nothing but the identities of the binders opened on the way, and the cycle is never recognized; without the context, two different comparisons that happen to be spelled alike would be conflated, which is the unsound direction.
#[derive(Default)]
struct History {
    seen: HashSet<Goal>,
    /// How many times a goal was met while already in progress — and so assumed, or in a classing left apart. A verdict reached while this moved rests on the goals that were in progress then, and is a fact about that path alone; one reached while it stood still rests on none of them, and is what [`compare`] remembers.
    assumed: u64,
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
            context: kernel.history_context(),
            type_: rename(type_),
            this: rename(this),
            that: rename(that),
        };

        match self.seen.insert(goal.clone()) {
            true => Some(goal),
            false => {
                self.assumed += 1;

                None
            }
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
/// An unfolding retry recurses back into here, and what bounds that chain is the budget spent on entry rather than a count of how deep it has gone — a constant standing in for the call stack is what [`recurse`] makes unnecessary. One chain the budget could not bound in practice is closed at its head instead: two instances of one recursive group at two universe instances reproduce themselves under every unfolding, and [`rec_instances`] makes such a pair a verdict before any retry is granted. A family's former is the member that reproduces nothing, and a refusal of one is left to the nodes it builds ([`projects_a_former`]).
fn compare(
    kernel: &mut Kernel,
    history: &mut History,
    type_: &Term,
    this: &Term,
    that: &Term,
) -> Result<bool, Error> {
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

        // Where the goal's type is a sort, nothing has typed this pair: it is two types, or a child whose type was not at hand. What a type directs between two neutrals is then asked of the type a lookup gives both sides.
        let untyped = matches!(&**type_, Subterm::Type(_) | Subterm::Prop);
        if untyped && by_their_own_type(kernel, history, this, that)? {
            return Ok(true);
        }

        // Two projections of one recursive group at two universe instances are a verdict, not a comparison: unfolding either reproduces the pair one level down, so their levels decide here — before `reduce_forced` opens a function member into its lambda, and before the goal is entered, so nothing has to be left. A family's former reproduces nothing, so its levels refuse nothing here: the pair goes on to the nodes it builds, whose arm compares them by the family's variance.
        if let Some(verdict) = rec_instances(kernel, this, that)
            && (verdict || !projects_a_former(this))
        {
            return Ok(verdict);
        }

        // A verdict reached before with nothing assumed is the verdict here, whatever goals are in progress now: it rested on none.
        if let Some(verdict) = kernel.convert_hit(type_, this, that) {
            return Ok(verdict);
        }

        let Some(goal) = history.enter(kernel, type_, this, that) else {
            return Ok(true);
        };
        let assumed = history.assumed;

        // Two calls of one definition by their spines, on the pair as posed and ahead of eta by the goal's type. Eta hands on each call applied to a binder or projected, which is no call of a definition at its head, so tried after it the spines would never be compared at a function or a record type.
        let outcome = match one_definition_by_its_spines(kernel, history, this, that)? {
            true => Ok(true),
            false => by_the_type(kernel, history, type_, this, that),
        };

        history.leave(&goal);
        // Remembered where deciding it met no goal in progress, and never where it ran the budget out, which is no verdict.
        if let Ok(verdict) = &outcome
            && history.assumed == assumed
        {
            kernel.convert_store(type_, this, that, *verdict);
        }
        outcome
    })
}

/// A goal the spines of one definition did not decide: eta where its type directs one, and the two sides' heads otherwise.
fn by_the_type(
    kernel: &mut Kernel,
    history: &mut History,
    type_: &Term,
    this: &Term,
    that: &Term,
) -> Result<bool, Error> {
    let at = kernel.reduce_forced(type_.clone())?;

    match &*at {
        Subterm::FuncType(FuncType { telescope, .. }) => {
            eta_function(kernel, history, telescope.clone(), this, that)
        }
        Subterm::TupleType(TupleType { telescope }) => {
            eta_tuple(kernel, history, telescope.clone(), this, that)
        }
        _ => {
            let forced_this = kernel.reduce_forced(this.clone())?;
            let forced_that = kernel.reduce_forced(that.clone())?;

            // A nominal struct has no eta by its type: a literal opens one, against a neutral (`struct_eta`) and field by field against another, and two neutrals have none to open. What eta would decide between them is read off the type instead, and they are left to their heads where it has more than one inhabitant.
            let literal = |term: &Term| matches!(&**term, Subterm::Struct(_));
            if matches!(&*at, Subterm::StructType(_))
                && !literal(&forced_this)
                && !literal(&forced_that)
                && one_inhabitant(kernel, &at)?
            {
                return Ok(true);
            }

            // A side that was a redex as posed is a neutral a lookup types only now.
            if matches!(&*at, Subterm::Type(_) | Subterm::Prop)
                && (forced_this != *this || forced_that != *that)
                && by_their_own_type(kernel, history, &forced_this, &forced_that)?
            {
                return Ok(true);
            }

            structural(kernel, history, &at, &forced_this, &forced_that)
        }
    }
}

/// What a type directs between two terms, asked of the type a lookup reads where the position handed none: two terms a lookup types at one type that has one inhabitant are equal.
///
/// **It is the typed rule, with the type looked up.** Conversion is asked about two terms of one type, and here that is checked rather than assumed: each side's type is read by [`looked_up`], a lookup, a substitution and a reduction, and the two are compared. Without it a stuck elimination's scrutinee is compared at `Type`, two eliminations of two proofs of one proposition stay apart, and two calls of one definition that differ in a proof part wherever reduction unfolds them before the pair is posed, their verdict following a spelling.
///
/// **It is all a type directs between two neutrals.** Eta between two neutrals decides nothing [`one_inhabitant`] does not: where a field or the codomain has a second inhabitant, the two sides' projections or applications are equal only where their heads are, which the structural rules decide. So nothing is forfeited by reading the type and expanding neither side, and expanding is what cannot be done here. Eta at a looked-up type would apply or project both sides; the projections' heads are compared with no type, this lookup would type them again, and the goal it posed would be one already in progress, which the recurrence rule assumes: any two variables at a record type would convert. Reading the type poses no goal about the two sides, so nothing can recur.
///
/// A lookup that fails for any reason but a spent budget is no answer, and the pair goes on untyped; so does a pair one side of which is a lambda or a tuple literal, which is decided by its parts.
fn by_their_own_type(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Term,
    that: &Term,
) -> Result<bool, Error> {
    let (Some(this_type), Some(that_type)) = (looked_up(kernel, this)?, looked_up(kernel, that)?)
    else {
        return Ok(false);
    };

    Ok(one_inhabitant(kernel, &this_type)?
        && compare(
            kernel,
            history,
            &Term::type_ground(),
            &this_type,
            &that_type,
        )?)
}

/// The type a lookup reads for one side of a pair its position handed no type: a neutral's, off its head's binder or declaration ([`synth_neutral`]), and a constructor's value's off its declaration — the family at the value's parameters and at the index targets its constructor states for its payload. Each is a lookup and a substitution, and neither reaches conversion. `None` where the side is neither, or its declaration refuses the occurrence.
fn looked_up(kernel: &mut Kernel, term: &Term) -> Result<Option<Term>, Error> {
    match &**term {
        Subterm::Variant(Variant {
            name,
            universes,
            params,
            tag,
            payload,
        }) => {
            let Ok(at) = kernel.induct_at_params(name, universes, params) else {
                return Ok(None);
            };
            let Some(signature) = at.signature(tag) else {
                return Ok(None);
            };
            if signature.len() != payload.len() {
                return Ok(None);
            }
            let targets = signature.open(&payload.iter().collect::<Vec<_>>());

            Ok(Some(Term::induct_type_at(
                *name,
                universes.clone(),
                params.clone(),
                targets,
            )))
        }
        _ if names_a_type(term) => Ok(synth_neutral(kernel, term).probed()?.flatten()),
        _ => Ok(None),
    }
}

/// Whether `type_` has one inhabitant by its shape: a proposition, the empty Σ, a Σ or a nominal struct whose every field's type has one, or a function type whose codomain has one.
///
/// **It is what the rules above derive, read off the type.** Any two inhabitants of such a type convert by them: irrelevance at the proposition, eta at the function and at the record down to each field, and at a struct the eta its literal has against a neutral, taken on both sides. Read off the type it poses no goal about the two sides, so it answers where eta cannot be fired: between two neutrals at a struct, and at the type a lookup gives ([`by_their_own_type`]).
///
/// **It ends at a struct that reaches itself.** A struct met again while it is being judged answers no where its declaration reaches itself ([`struct_reaches_itself`]): a declaration is opened by this walk and by no reduction, so nothing else would end it, and the answer is the refusing one. One met again through a parameter it was instantiated at is judged again: its declaration abbreviates a record of its fields, and the parameter is a part of the type the walk began at, or what a definition that ends made of one. Every other type is forced as eta by the goal's type forces it, a recursive definition's call unfolding as far as it computes, and is bounded as that walk is, by the budget.
///
/// A field's type is judged under the fields before it, opened at binders, so one that computes from an earlier field counts only where it reduces whatever that field is. A type no sort judgment classifies, and an occurrence its declaration refuses, answer no.
fn one_inhabitant(kernel: &mut Kernel, type_: &Term) -> Result<bool, Error> {
    curios_profile::profile!("convert::one_inhabitant");
    inhabited_once(kernel, type_, &mut Vec::new())
}

/// [`one_inhabitant`], inside the structs in `entered`, whose fields this type was reached through.
///
/// A function type is a proposition where its codomain is one and a record where every field is, so each is read through to what it ends in, and a sort is asked only of what is neither: no part of the type is walked twice.
fn inhabited_once(
    kernel: &mut Kernel,
    type_: &Term,
    entered: &mut Vec<Global>,
) -> Result<bool, Error> {
    recurse(|| {
        kernel.spend(Cost::STEP)?;

        let Some(at) = kernel.reduce_forced(type_.clone()).probed()? else {
            return Ok(false);
        };
        let proposition = |kernel: &mut Kernel| -> Result<bool, Error> {
            Ok(Sort::of(kernel, &at)
                .probed()?
                .is_some_and(|sort| sort.is_prop()))
        };

        match &*at {
            Subterm::FuncType(FuncType { telescope, .. }) => kernel.scoped(|kernel| {
                let mut cursor = telescope.cursor();
                while let Some((_, domain)) = cursor.entry() {
                    kernel.advance_assumed(&mut cursor, &domain);
                }
                let codomain = cursor.body().expect("a cursor past every entry");

                inhabited_once(kernel, &codomain, entered)
            }),
            Subterm::TupleType(TupleType { telescope }) => {
                fields_inhabited_once(kernel, telescope.clone(), entered)
            }
            Subterm::StructType(StructType {
                name,
                universes,
                params,
            }) => {
                if proposition(kernel)? {
                    return Ok(true);
                }
                if entered.contains(name) && struct_reaches_itself(kernel, name) {
                    return Ok(false);
                }
                let Ok(declared) = kernel.struct_at(name, universes, params) else {
                    return Ok(false);
                };

                entered.push(*name);
                let verdict = fields_inhabited_once(kernel, declared.fields(), entered);
                entered.pop();

                verdict
            }
            _ => proposition(kernel),
        }
    })
}

/// Whether every field of `telescope` has one inhabitant, each judged under the fields before it.
fn fields_inhabited_once(
    kernel: &mut Kernel,
    telescope: Telescope<()>,
    entered: &mut Vec<Global>,
) -> Result<bool, Error> {
    kernel.scoped(|kernel| {
        let mut cursor = telescope.cursor();
        while let Some((_, field)) = cursor.entry() {
            if !inhabited_once(kernel, &field, entered)? {
                return Ok(false);
            }
            kernel.advance_assumed(&mut cursor, &field);
        }

        Ok(true)
    })
}

/// Two applications of one definition, decided by their spines before either is unfolded — congruence, which is sufficient and never necessary, so a mismatch decides nothing and the pair goes on to eta and to be forced.
///
/// It is tried on the pair as posed, at whatever type, which is where the elaborator tries it: congruence holds at every type, and eta by the goal's type would hand on a pair this rule no longer recognizes. A curried spine is one definition's too — `h(p)(x)` against `h(q)(x)`, two heads that are themselves two calls of one definition by this rule — as the elaborator's rule looks through one.
///
/// Forcing first would open two calls whose spines already say they are equal, as the elaborator, which compares the spines of one global or one `rec` member before it unfolds, does not. Unfolded, a proof lands in a stuck match's scrutinee, where the type a lookup gives it is what equates it ([`by_their_own_type`]), and a recursive function carrying a proof would unfold under fresh binders until the budget ran out, each round lengthening the context the recurrence key records so the goal never recurs. The spine compares at the head's telescope through [`compare_arguments`], so the proof meets irrelevance at its own type. `tests::board`'s `a_definition_applied_to_two_proofs_converts_before_unfolding` and `a_recursive_function_carrying_a_proof_converts_without_unfolding` are the programs; this crate's `irrelevance_tests` and `recursion_tests` put the same two to this function directly.
///
/// Only heads that would unfold qualify, because a head with nothing to unfold reaches the same spine comparison in [`structural`] anyway, and asking twice would double the cost of every mismatch.
fn one_definition_by_its_spines(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Term,
    that: &Term,
) -> Result<bool, Error> {
    let (Subterm::Apply(left), Subterm::Apply(right)) = (&**this, &**that) else {
        return Ok(false);
    };
    if !left.plicities().eq(right.plicities()) {
        return Ok(false);
    }

    let one_head = match rec_instances(kernel, &left.head, &right.head) {
        Some(verdict) => verdict,
        None => match (&*left.head, &*right.head) {
            (Subterm::Var(left), Subterm::Var(right)) => {
                let name = left.unwrap();
                name == right.unwrap() && kernel.value(name).is_some()
            }
            (
                Subterm::Instance(Instance {
                    head: InstanceHead::Var(left),
                    levels: left_levels,
                }),
                Subterm::Instance(Instance {
                    head: InstanceHead::Var(right),
                    levels: right_levels,
                }),
            ) => {
                let name = left.unwrap();
                name == right.unwrap()
                    && kernel.value_at(name).is_some()
                    && kernel.levels_eq(left_levels, right_levels)
            }
            (Subterm::Apply(_), Subterm::Apply(_)) => {
                one_definition_by_its_spines(kernel, history, &left.head, &right.head)?
            }
            _ => false,
        },
    };

    Ok(one_head && compare_arguments(kernel, history, left, right)?)
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
) -> Result<bool, Error> {
    kernel.scoped(|kernel| {
        let mut cursor = telescope.cursor();
        while let Some((_, domain)) = cursor.entry() {
            kernel.advance_assumed(&mut cursor, &domain);
        }
        let codomain = cursor.body().expect("a cursor past every entry");
        let arguments = cursor.into_args();

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
///
/// **At the empty Σ this is unit eta.** No component is left to compare, so any two terms convert at `{}` without either being read: the type has one inhabitant, and conversion is asked about two terms of the goal's type, the invariant irrelevance discharges a proposition on. It composes with the rules above it — two terms at a record of units, or two functions into one, convert by the eta that reaches the unit — and where a child is compared at `Type` the same verdict is read off the type a lookup gives ([`one_inhabitant`]).
fn eta_tuple(
    kernel: &mut Kernel,
    history: &mut History,
    telescope: Telescope<()>,
    this: &Term,
    that: &Term,
) -> Result<bool, Error> {
    let mut cursor = telescope.cursor();

    while let Some((_, field)) = cursor.entry() {
        let index = cursor.args().len();
        let left = Term::proj(this.clone(), index);
        let right = Term::proj(that.clone(), index);

        if !compare(kernel, history, &field, &left, &right)? {
            return Ok(false);
        }

        cursor.advance(left);
    }

    Ok(true)
}

/// Compare two weak-head normal forms by their heads, at `at`, the goal's type in weak-head normal form.
///
/// Children with no type the head determines are compared at `Type` through [`ground`], which fires no eta and reads what a type directs off a lookup ([`by_their_own_type`]). See the module documentation on a child with no type of its own.
///
/// The goal's type is read by one rule, a literal's eta, and only to refuse it ([`another_former`]).
fn structural(
    kernel: &mut Kernel,
    history: &mut History,
    at: &Term,
    this: &Term,
    that: &Term,
) -> Result<bool, Error> {
    match (&**this, &**that) {
        // Levels compare under the item's assumed constraints: two levels the hypotheses force equal are equal in every instance that satisfies them, which is what checking generically means.
        (Subterm::Type(left), Subterm::Type(right)) => Ok(kernel.level_eq(left, right)),
        (Subterm::Prop, Subterm::Prop) => Ok(true),

        (Subterm::Intrinsic(left), Subterm::Intrinsic(right)) => {
            convert_intrinsic(kernel, history, left, right)
        }

        (Subterm::Var(left), Subterm::Var(right)) => Ok(left.unwrap() == right.unwrap()),

        // A metavariable is elaboration-only syntax, and refusing it *here* is what makes the exclusion the kernel's own rather than an inherited guarantee of `zonk_module`'s traversal. `whnf` still treats one as a stuck neutral — a reduction stance, not an admission: the only ways a term is admitted are `infer` and this comparison, and both refuse. The syntactic fast path in `compare` does admit a metavariable against *itself*, and soundly: reflexivity decides nothing about the unknown, which is exactly what this arm exists to prevent.
        (Subterm::Metavar(_), _) | (_, Subterm::Metavar(_)) => Err(Error::NotCore(this.clone())),

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

        // A lambda or a tuple literal against a neutral at a type former: `compare` fires eta by a function and a record type before it comes here, so the former is not the literal's, the two sides are not of one type, and the literal's eta is refused.
        (Subterm::Func(_) | Subterm::Tuple(_), _)
            if neutral(kernel, that) && another_former(kernel, at) =>
        {
            Ok(false)
        }
        (_, Subterm::Func(_) | Subterm::Tuple(_))
            if neutral(kernel, this) && another_former(kernel, at) =>
        {
            Ok(false)
        }

        // Eta at a function and at a record, by the literal against a neutral inhabitant, where the goal's type did not direct it — see `function_eta` and `tuple_eta` for the rule, and `neutral` for the set. A neutral may still have a folded spelling to open, so a refusal falls through to the unfolding retry, which refuses where neither side has one.
        (Subterm::Func(function), _) if neutral(kernel, that) => {
            match function_eta(kernel, history, function, that)? {
                true => Ok(true),
                false => unfolded_retry(kernel, history, this, that),
            }
        }
        (_, Subterm::Func(function)) if neutral(kernel, this) => {
            match function_eta(kernel, history, function, this)? {
                true => Ok(true),
                false => unfolded_retry(kernel, history, this, that),
            }
        }
        (Subterm::Tuple(literal), _) if neutral(kernel, that) => {
            match tuple_eta(kernel, history, literal, that)? {
                true => Ok(true),
                false => unfolded_retry(kernel, history, this, that),
            }
        }
        (_, Subterm::Tuple(literal)) if neutral(kernel, this) => {
            match tuple_eta(kernel, history, literal, this)? {
                true => Ok(true),
                false => unfolded_retry(kernel, history, this, that),
            }
        }

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
            && kernel.instances_eq(left_name, left_universes, right_universes)
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
            if left_name != right_name
                || !kernel.instances_eq(left_name, left_universes, right_universes)
            {
                return Ok(false);
            }
            let telescope = struct_params(kernel, left_name, left_universes);
            params_at(kernel, history, telescope, left_params, right_params)
        }

        // The payload compares at the constructor's own telescope, opened at the left side's parameters and then at each preceding payload, so a `Prop`-sorted payload discharges by irrelevance without being read — the discipline `eta_tuple` follows at a Σ, and what the elaborator's `compare_variant` does. A tag the declaration does not carry leaves the telescope absent, and the payload then compares at `Type`, the untyped concession, which can only refuse more.
        (Subterm::Variant(left), Subterm::Variant(right)) => {
            if left.name != right.name
                || left.tag != right.tag
                || !kernel.instances_eq(&left.name, &left.universes, &right.universes)
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
            if left_name != right_name
                || !kernel.instances_eq(left_name, left_universes, right_universes)
            {
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

        // A struct literal against a neutral at a type former that is not the literal's own struct: refused, as a lambda's and a tuple's is above.
        (Subterm::Struct(literal), _)
            if neutral(kernel, that) && another_struct(kernel, at, literal) =>
        {
            Ok(false)
        }
        (_, Subterm::Struct(literal))
            if neutral(kernel, this) && another_struct(kernel, at, literal) =>
        {
            Ok(false)
        }

        // Eta at a nominal struct, against a neutral inhabitant — see `struct_eta` for the rule and `neutral` for the set. A stuck application is the shape a standard-library law meets: `State`'s left identity sets `State/bind`'s literal against the neutral `f(a)`. A neutral may still have a folded spelling to open, so a refusal falls through to the unfolding retry.
        (Subterm::Struct(literal), _) if neutral(kernel, that) => {
            match struct_eta(kernel, history, literal, that)? {
                true => Ok(true),
                false => unfolded_retry(kernel, history, this, that),
            }
        }
        (_, Subterm::Struct(literal)) if neutral(kernel, this) => {
            match struct_eta(kernel, history, literal, this)? {
                true => Ok(true),
                false => unfolded_retry(kernel, history, this, that),
            }
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
            if !ground(kernel, history, &left.head, &right.head)? {
                return Ok(false);
            }
            // The two scrutinees are one term by now, so one type is theirs: what the motive's and the arms' binders are opened at.
            let scrutinee = scrutinee_type(kernel, &left.head)?;

            let results = match (&left.result, &right.result) {
                (MatchResult::Family(this), MatchResult::Family(that)) => {
                    compare_motives(kernel, history, scrutinee.as_ref(), this, that)?
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
            };

            Ok(results && ground_cases(kernel, history, left, scrutinee.as_ref(), &right.cases)?)
        }

        // A folded recursive call, and a `rec` that forcing declined to unfold. Two projections of one group are compared up to their universe instance, the levels decided by entailment; two different groups are refused here without a retry, because the interesting case — a cycle that unfolds without disagreeing — is handled by the recurrence rule above, not here. A `rec` whose tail computes something is *not* this case, and falls through to the delta step below.
        (Subterm::Rec(_), Subterm::Rec(_))
            if this.as_rec_proj().is_some() && that.as_rec_proj().is_some() =>
        {
            Ok(rec_instances(kernel, this, that) == Some(true))
        }

        // A `Bool` connective against a term that is no intrinsic at all — absorption's shape, `b || (b && c)` against the bare `b` — which the intrinsic congruence never sees: `curios-analysis`'s `connectives_agree` decides it equal or says nothing, and saying nothing leaves the pair where it was.
        //
        // Then two spellings of one recursive call: `force` keeps the folded application as a recursive call's normal form, while an arm's induction hypothesis is the raw stuck fold-match on the same argument. When the heads disagree, grant each side the one definitional unfolding `force` withheld and compare what results.
        _ => match connectives_convert(kernel, history, this, that)? {
            true => Ok(true),
            false => unfolded_retry(kernel, history, this, that),
        },
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
) -> Result<bool, Error> {
    if this.len() != that.len() {
        return Ok(false);
    }

    let Some(telescope) = telescope else {
        return compare_each(kernel, history, this.iter(), that.iter());
    };

    let mut cursor = telescope.cursor();
    for (left, right) in this.iter().zip(that) {
        let Some((_, type_)) = cursor.entry() else {
            return Ok(false);
        };
        if !compare(kernel, history, &type_, left, right)? {
            return Ok(false);
        }
        cursor.advance(left.clone());
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

/// Argument-wise comparison of two instances of one inductive family, each pair at the type the declaration's full index telescope assigns, opened at the left instance's preceding actuals — the typed context the grounded comparison forfeits. Irrelevance at a `Prop`-typed index is the observable difference: `Eq(@P)(p, q)` with `P : Prop` converts with `Eq(@P)(p, p)`, because no proof of a proposition is distinguishable from another. The struct and constructor analogs are [`params_at`]'s, which reads the same outer telescope this does.
fn induct_type_args(
    kernel: &mut Kernel,
    history: &mut History,
    name: &Global,
    universes: &[Level],
    (left_params, left_indices): (&[Term], &[Term]),
    (right_params, right_indices): (&[Term], &[Term]),
) -> Result<bool, Error> {
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
    let arity = instantiate_universe_levels_scoped(&arity, universes)?;

    // The parameters, against the outer telescope; each one opens what follows, because a later domain may name it.
    let mut cursor = arity.cursor();
    for (left, right) in left_params.iter().zip(right_params) {
        let Some((_, type_)) = cursor.entry() else {
            return Ok(false);
        };
        if !compare(kernel, history, &type_, left, right)? {
            return Ok(false);
        }
        cursor.advance(left.clone());
    }

    // Then the indices, which the parameter telescope terminates in — opened at the parameters just compared.
    let Some(indices) = cursor.body() else {
        return Ok(false);
    };

    let mut cursor = indices.cursor();
    for (left, right) in left_indices.iter().zip(right_indices) {
        let Some((_, type_)) = cursor.entry() else {
            return Ok(false);
        };
        if !compare(kernel, history, &type_, left, right)? {
            return Ok(false);
        }
        cursor.advance(left.clone());
    }

    Ok(true)
}

/// Whether `term`, in weak-head normal form, is a neutral inhabitant, which is all an eta fired by a literal's shape is taken against: every term whose head does not show its type — a variable, a projection, a stuck application or elimination, a `rec` block, a universe instance, an intrinsic operation at a type its operands state ([`Subterm::shows_its_type`], `curios-core`, which the elaborator reads too).
///
/// Neutrality is no part of eta, which holds for every inhabitant of the type. It is a proxy for the invariant the rule stands on where nothing checks it, that the two sides have one type ([`struct_eta`]): a head that shows its type and is not the literal's former is a term of another type, and every other head says nothing either way. So the set is every head that says nothing; a narrower one refuses equations that hold and guards nothing the wider one admits.
fn neutral(kernel: &Kernel, term: &Term) -> bool {
    !term.shows_its_type(&kernel.syntax())
}

/// Whether `at`, a goal's type in weak-head normal form, is a type former a literal met in [`structural`] cannot inhabit. Conversion is asked about two terms of one type, and a literal's eta stands on it: where the goal states a type that is not the literal's, the invariant is broken at this goal and the rule is refused, where the neutral restriction alone would let a literal with no field convert with any neutral.
///
/// A sort says nothing: it is what [`ground`] compares a child at when its type is not at hand. Neither does a neutral type. There the rule stands on its callers, as [`struct_eta`] says.
fn another_former(kernel: &Kernel, at: &Term) -> bool {
    at.is_type_former(&kernel.syntax())
}

/// [`another_former`] for a struct literal, whose own type [`compare`] leaves to [`structural`]: a struct type of the literal's name is the literal's, and any other former is not.
fn another_struct(kernel: &Kernel, at: &Term, literal: &Struct) -> bool {
    match &**at {
        Subterm::StructType(StructType { name, .. }) => *name != literal.name,
        _ => another_former(kernel, at),
    }
}

/// Eta at a function, by the lambda: a lambda against a *neutral* inhabitant, where the goal's type did not direct the comparison. The lambda's own telescope is opened at the domains it is annotated with, the neutral is applied to the same binders at the lambda's plicities, and the two are compared at `Type`, the codomain being stated nowhere.
///
/// [`compare`] fires this rule by the goal's type wherever that is a function type ([`eta_function`]); here the type is not at hand — a child [`ground`] compares, a stuck elimination's arm or a projection's head — and the lambda says what the type would have. Without it conversion is no congruence there: an expansion equal to its neutral at the goal is refused once both sit under a stuck `match`, and the elaborator, which fires eta by the lambda at every goal, has accepted the pair by then.
///
/// **The invariant and the restriction are [`struct_eta`]'s.** Conversion is asked about two terms of one type, so `other` is a function of the lambda's type and applying it is typed; and the walk is taken against a neutral alone, the proxy for that invariant where nothing checks it.
fn function_eta(
    kernel: &mut Kernel,
    history: &mut History,
    function: &Func,
    other: &Term,
) -> Result<bool, Error> {
    kernel.scoped(|kernel| {
        let mut cursor = function.telescope.cursor();
        while let Some((_, domain)) = cursor.entry() {
            kernel.advance_assumed(&mut cursor, &domain);
        }
        let body = cursor.body().expect("a cursor past every entry");
        let applied = Term::apply_marked(
            other.clone(),
            function.plicities().iter().copied().zip(cursor.into_args()),
        );

        ground(kernel, history, &body, &applied)
    })
}

/// Eta at a record, by the literal: a tuple literal against a *neutral* inhabitant, each field compared at `Type` with the neutral's projection, under the invariant and the restriction [`function_eta`] has. [`compare`] fires the rule by the goal's type at a Σ ([`eta_tuple`]), and this is the same rule where no such type is at hand.
///
/// A literal with no field compares nothing and answers `true`, as an empty struct's does at [`struct_eta`]: `{}` has one inhabitant, and a neutral of the literal's type is it.
fn tuple_eta(
    kernel: &mut Kernel,
    history: &mut History,
    literal: &Tuple,
    other: &Term,
) -> Result<bool, Error> {
    for (index, field) in literal.fields.iter().enumerate() {
        if !ground(kernel, history, field, &Term::proj(other.clone(), index))? {
            return Ok(false);
        }
    }

    Ok(true)
}

/// Eta at a nominal struct: a literal against a *neutral* inhabitant ([`neutral`]), projected field-wise and compared at the type the declaration gives each field. A `Prop`-sorted field converts by irrelevance without being compared at all, which the same telescope decides.
///
/// **What licenses the projection is an invariant about the callers, not the shape of `other`.** Conversion is only ever asked whether two terms *of one type* are equal: the entry point carries the type, every typed recursion passes the one its position assigns — a field's from this telescope, an argument's from its head's, an index's from the family's — and [`ground`] discards the kernel's *knowledge* of that type without changing the fact. So `other` inhabits the struct type the literal is a value of, and eta for a single-constructor record — every inhabitant `x` equals `S { x.0, …, x.(n-1) }` — is what decides the pair.
///
/// That invariant is checked where the goal states a type and nowhere else. At a goal whose type is a former other than the literal's own struct, [`structural`] refuses the rule before it reaches here ([`another_struct`]). Under [`ground`] the goal's type is `Type`, which says nothing, and there the walk is restricted to neutrals as a proxy, not a second guarantee: a `Var` is as arbitrary a term as any other, so what the restriction buys is that a pair arriving from a caller that broke the invariant is unlikely to be *shaped* like an inhabitant — thin, and worth knowing it is thin, because an all-`Prop` or empty struct's field walk compares nothing and answers `true`.
fn struct_eta(
    kernel: &mut Kernel,
    history: &mut History,
    literal: &Struct,
    other: &Term,
) -> Result<bool, Error> {
    // Through the checked handle: a literal at the wrong parameter count would otherwise reach `fields_at`, which opens the arity and asserts. Declining is conversion's own answer for a shape it cannot decide, and it is the right one here too.
    let Ok(at) = kernel.struct_at(&literal.name, &literal.universes, &literal.params) else {
        return Ok(false);
    };

    let fields = at.fields();
    let mut cursor = fields.cursor();
    for (index, field) in literal.fields.iter().enumerate() {
        let Some((_, type_)) = cursor.entry() else {
            return Ok(false);
        };
        cursor.advance(field.clone());

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
    Ok(cursor.is_done())
}

/// The last chance before a structural refusal: grant each side the one definitional unfolding `force` withheld, and compare the results. A refusal when neither side has a folded recursive spelling to open.
///
/// The unfoldings are compared untyped, through [`ground`]. Each retry opens a spelling whose spine may have *grown*, so its goals never recur into `seen` and the coinductive rule cannot close the chain; what stops an unproductive pair is [`compare`]'s budget, spent once per entry, rather than a count of how deep the retries have gone. One pair the budget never reaches is refused at the door: two instances of one group at unequal levels unfold to the same two instances, and every round adds two opaque binders to a context the history key copies whole, so the walk would grow until the host died with the budget barely spent. [`rec_instances`] answers that pair before and after the unfolding, and a retry is granted only to heads it does not decide.
fn unfolded_retry(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Term,
    that: &Term,
) -> Result<bool, Error> {
    curios_profile::profile!("convert::unfolded_retry");
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

/// `Some(verdict)` when `this` and `that` are projections of one recursive group at the same member, differing in nothing but universe levels — the verdict is whether those levels are equal under the item's hypotheses. `None` for any other pair, which the structural rules judge.
///
/// This is the equation the `Instance` arm applies to its head, reaching one more head kind; a nominal node's levels compare by its family's variance instead (`Kernel::instances_eq`), and a projection keeps every one here, a family's former included: [`compare`] is what leaves a refusal of one to the nodes it builds. Terms that differ in nothing but levels — [`Term::level_differences`], which counts a ground `Type 0` as a level and walks the pair's graph rather than its tree — denote one term in every instance satisfying the assumed constraints when each differing pair sits at depth zero and is mutually entailed under them. Nothing is erased: `wrap(Type 1)` against `wrap(Type 2)` refuses on the levels, `wrap(Type 0)` against `wrap(Type 1)` on the skeletons. What it does not decide is refused, the safe direction: `Type 0` against a `Type u` the hypotheses force to zero has two skeletons.
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

    this.level_differences(that, |_| false).map(|differences| {
        differences.iter().all(|(depth, this_level, that_level)| {
            *depth == 0 && kernel.level_eq(this_level, that_level)
        })
    })
}

/// Whether `term` is a projection of a group at a family's former: a member whose body is lambdas over the nominal node it builds.
///
/// Such a member names no member of its group at its head, so unfolding it reproduces no pair: forced it is the node, or a function eta applies to binders and then forces to the node, and a node is a weak-head normal form that holds none of its constructors. So [`compare`] does not take [`rec_instances`]' refusal of two instances of one as a verdict, and what compares their levels is the node's arm, by the family's variance (`Kernel::instances_eq`). Without it a pair converts as two instances of the family's name, which force to the nodes, and is refused once something has reduced both to the projections: one pair, two answers. The closed body is read, before any member is substituted into it.
fn projects_a_former(term: &Term) -> bool {
    let Some((group, index)) = term.as_rec_proj() else {
        return false;
    };
    let Some(member) = group.iter().nth(index) else {
        return false;
    };

    let mut body = member.body.body();
    while let Subterm::Func(Func { telescope, .. }) = &**body {
        body = telescope.terminal();
    }

    matches!(&**body, Subterm::InductType(_) | Subterm::StructType(_))
}

/// The head of an application spine, or the term itself: what `rec_instances` is asked about when a folded recursive call arrives applied — past its own parameters too, where the call is the head of an application of its own.
fn applied_head(term: &Term) -> &Term {
    let mut term = term;
    while let Subterm::Apply(apply) = &**term {
        term = &apply.head;
    }

    term
}

/// A family opened at the scrutinee's actual indices and the scrutinee — the elimination's own type. An unindexed family binds the scrutinee alone; an indexed one is opened at the indices its scrutinee's type carries, read by the lookup a neutral spine has, since conversion looks a type up and never infers one. A scrutinee the lookup does not type — a stuck match of its own — leaves the indices unread, and the binder count below refuses the pair.
fn family_at_head(kernel: &mut Kernel, motive: &Scope<Many>, head: &Term) -> Result<Term, Error> {
    let mut arguments = Vec::with_capacity(motive.arity());
    if motive.arity() > 1
        && let Some(head_type) = synth_neutral(kernel, head)?
    {
        let head_type = kernel.reduce_forced(head_type)?;
        if let Subterm::InductType(InductType { indices, .. }) = &*head_type {
            arguments.extend(indices.iter().cloned());
        }
    }
    arguments.push(head.clone());
    if arguments.len() != motive.arity() {
        return Err(Error::Arity {
            counted: Counted::MotiveBinders,
            expected: motive.arity(),
            actual: arguments.len(),
        });
    }
    let refs = arguments.iter().collect::<Vec<_>>();
    Ok(motive.open(&refs))
}

/// The type of two eliminations' scrutinee, in weak-head normal form, read by a lookup once the two are one term. `None` where no lookup types it, which leaves the motive's and the arms' binders at the stand-in.
fn scrutinee_type(kernel: &mut Kernel, head: &Term) -> Result<Option<Term>, Error> {
    let Some(type_) = looked_up(kernel, head)? else {
        return Ok(None);
    };

    Ok(kernel.reduce_forced(type_).probed()?)
}

/// A binder opened at `type_`.
fn assumed(kernel: &mut Kernel, type_: &Term) -> Term {
    let binder = kernel.fresh(None);
    kernel.assume(&binder, type_);

    Term::free_var(&binder)
}

/// Two motives under one shared set of binders, each at the type its position gives it ([`motive_binders`]), their bodies compared at `Type`, which is what two types are compared at.
fn compare_motives(
    kernel: &mut Kernel,
    history: &mut History,
    scrutinee: Option<&Term>,
    this: &Scope<Many>,
    that: &Scope<Many>,
) -> Result<bool, Error> {
    if this.arity() != that.arity() {
        return Ok(false);
    }

    kernel.scoped(|kernel| {
        let binders = motive_binders(kernel, scrutinee, this.arity());
        let refs = binders.iter().collect::<Vec<_>>();

        ground(kernel, history, &this.open(&refs), &that.open(&refs))
    })
}

/// A motive's binders, opened as `check_motive` opens them where it types the motive: the family's index domains and then the scrutinee at the family over those binders, or the scrutinee's own type for a carrier that has no index. Where no lookup typed the scrutinee, or the motive binds another count than its family states, they are opened at the stand-in ([`opaque_binders`]).
fn motive_binders(kernel: &mut Kernel, scrutinee: Option<&Term>, arity: usize) -> Vec<Term> {
    match scrutinee.map(|type_| (type_, &**type_)) {
        Some((_, Subterm::InductType(family))) => {
            let indices = match kernel.induct_at(family) {
                Ok(at) => at.indices(),
                Err(_) => return opaque_binders(kernel, arity),
            };
            if indices.len() + 1 != arity {
                return opaque_binders(kernel, arity);
            }

            let mut opened = Vec::with_capacity(arity);
            let mut cursor = indices.cursor();
            while let Some((_, domain)) = cursor.entry() {
                let binder = kernel.advance_assumed(&mut cursor, &domain);
                opened.push(Term::free_var(&binder));
            }
            let over_them = Term::induct_type_at(
                family.name,
                family.universes.clone(),
                family.params.clone(),
                opened.clone(),
            );
            opened.push(assumed(kernel, &over_them));

            opened
        }
        Some((type_, _)) if arity == 1 => vec![assumed(kernel, type_)],
        _ => opaque_binders(kernel, arity),
    }
}

/// A cons arm's binders: the carrier's own domains, and the hypothesis at the result at the tail, which only a one-binder family states. An ambient goal types no hypothesis and its arm reads none, so that binder is opened at the stand-in.
fn cons_binders(kernel: &mut Kernel, carrier: &Carrier, result: &MatchResult) -> Vec<Term> {
    let mut opened = carrier
        .cons_domains()
        .iter()
        .map(|domain| assumed(kernel, domain))
        .collect::<Vec<_>>();
    let tail = opened.last().expect("a cons arm binds its tail");
    let hypothesis = match result {
        MatchResult::Family(motive) if motive.arity() == 1 => motive.open(&[tail]),
        _ => Term::type_ground(),
    };
    opened.push(assumed(kernel, &hypothesis));

    opened
}

/// Open both scopes at one shared set of opaque binders and compare the bodies at `Type`: what an arm keeps where no lookup typed its scrutinee, or its family states no constructor of its arity. The binders are assumed at the stand-in `Type`, which `Sort::of` reads like any recorded type: it is the least informative answer it can give a binder, so the stand-in can only lose an accepting rule, never gain one — `irrelevance_tests`' `a_binders_stand_in_type_decides_a_goal_the_way_a_relevant_type_does` holds that, and the one exception it records.
fn ground_scope(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Scope<Many>,
    that: &Scope<Many>,
) -> Result<bool, Error> {
    if this.arity() != that.arity() {
        return Ok(false);
    }

    kernel.scoped(|kernel| {
        let occurrences = opaque_binders(kernel, this.arity());
        let refs = occurrences.iter().collect::<Vec<_>>();
        ground(kernel, history, &this.open(&refs), &that.open(&refs))
    })
}

fn opaque_binders(kernel: &mut Kernel, arity: usize) -> Vec<Term> {
    (0..arity)
        .map(|_| assumed(kernel, &Term::type_ground()))
        .collect()
}

/// Compare two stuck eliminations' arm sets up to conversion, shape held rigid: matching variants, tags in the same canonical order, equal plicities, and agreeing default presence.
///
/// Each arm's binders are opened at the types its position gives them, read off the left elimination, whose scrutinee and result the right one's have just been compared with: a constructor's arm at its telescope over the scrutinee's parameters, and a cons arm at its carrier's domains and its hypothesis ([`cons_binders`]). So a proof an arm binds is one where the arm's body is compared, as it is where the arm was typed. Where no lookup typed the scrutinee, or its family states no constructor of the arm's arity, the arm keeps the stand-in ([`ground_scope`]). An arm's body is compared at `Type`: with its binders typed, a lookup reads what a type directs between two neutrals in it, a literal opens its own eta, and two literals are compared by their parts.
fn ground_cases(
    kernel: &mut Kernel,
    history: &mut History,
    left: &Match,
    scrutinee: Option<&Term>,
    that: &Cases,
) -> Result<bool, Error> {
    match (&left.cases, that) {
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
            let family = match scrutinee.map(|type_| &**type_) {
                Some(Subterm::InductType(family)) => kernel.induct_at(family).ok(),
                _ => None,
            };
            for ((this_tag, this_arm), (that_tag, that_arm)) in this_cases.iter().zip(that_cases) {
                if this_tag != that_tag
                    || this_arm.plicities() != that_arm.plicities()
                    || this_arm.body.arity() != that_arm.body.arity()
                {
                    return Ok(false);
                }
                let signature = family
                    .as_ref()
                    .and_then(|at| at.signature(this_tag))
                    .filter(|signature| signature.len() == this_arm.body.arity());
                let converge = match signature {
                    Some(signature) => kernel.scoped(|kernel| {
                        let mut cursor = signature.cursor();
                        while let Some((_, field)) = cursor.entry() {
                            kernel.advance_assumed(&mut cursor, &field);
                        }
                        let payload = cursor.into_args();
                        let refs = payload.iter().collect::<Vec<_>>();

                        ground(
                            kernel,
                            history,
                            &this_arm.body.open(&refs),
                            &that_arm.body.open(&refs),
                        )
                    })?,
                    None => ground_scope(kernel, history, &this_arm.body, &that_arm.body)?,
                };
                if !converge {
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
                    && kernel.scoped(|kernel| {
                        let o = cons_binders(kernel, this, &left.result);
                        ground(
                            kernel,
                            history,
                            &this_cons.open(&[&o[0], &o[1]]),
                            &that_cons.open(&[&o[0], &o[1]]),
                        )
                    })?),
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
                    && kernel.scoped(|kernel| {
                        let o = cons_binders(kernel, this, &left.result);
                        ground(
                            kernel,
                            history,
                            &this_cons.open(&[&o[0], &o[1], &o[2]]),
                            &that_cons.open(&[&o[0], &o[1], &o[2]]),
                        )
                    })?),
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
                    && kernel.scoped(|kernel| {
                        let o = cons_binders(kernel, this, &left.result);
                        ground(
                            kernel,
                            history,
                            &this_cons.open(&[&o[0], &o[1], &o[2]]),
                            &that_cons.open(&[&o[0], &o[1], &o[2]]),
                        )
                    })?),
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
    terminal: impl FnOnce(&mut Kernel, &mut History, B, B) -> Result<bool, Error>,
) -> Result<bool, Error> {
    kernel.scoped(|kernel| {
        let mut walk = Lockstep::new(&this, &that);

        loop {
            match walk.step() {
                Step::Entries { left, right, .. } => {
                    if !ground(kernel, history, &left, &right)? {
                        return Ok(false);
                    }
                    kernel.advance_assumed(&mut walk, &left);
                }
                Step::Bodies(left, right) => return terminal(kernel, history, left, right),
                // Different arities. A function type is not curried in this representation, so this is a real mismatch rather than a shape to normalize.
                Step::Mismatch => return Ok(false),
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
) -> Result<bool, Error> {
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
) -> Result<bool, Error> {
    compare_binders(kernel, history, this, that, |_, _, (), ()| Ok(true))
}

/// A spine's arguments at the types its head assigns them, and at `Type` where it assigns none.
///
/// **The head's type is the typed context this position was said to lack.** A variable head carries one — it was assumed or declared at it — so the domains of its function type are what an argument inhabits, exactly as a struct's field telescope is what a field inhabits ([`compare_fields_at`]). Comparing there is what lets eta and irrelevance fire: `f(p)` against `f(q)` for two proofs of one proposition is discharged without reading either.
///
/// **What still grounds.** A universe instance of a variable, a `rec` member and a stuck elimination carry a telescope as a variable does, and so does an application or a projection of one, whose type is the head's opened at its arguments or read off at its field (see [`spine_telescope`]). A head whose looked-up type is no function of this arity hands back none, and its arguments are compared at `Type`, where a lookup reads what their own types direct. Reading the type is a lookup rather than an inference, so a spine costs what it did.
///
/// The justification is the callers': every pair compared here is the corresponding children of two parents already shown convertible, so the two heads have one type and one telescope to assign.
fn compare_arguments(
    kernel: &mut Kernel,
    history: &mut History,
    left: &Apply,
    right: &Apply,
) -> Result<bool, Error> {
    let Some(telescope) = spine_telescope(kernel, &left.head, left.arguments.len())? else {
        return compare_each(kernel, history, left.params(), right.params());
    };

    let this = left.params().cloned().collect::<Vec<_>>();
    let that = right.params().cloned().collect::<Vec<_>>();

    compare_fields_at(kernel, history, Some(telescope), &this, &that)
}

/// The function type a head was bound or declared at, opened for `arity` arguments — or `None` where the head names no type, or names one that is not a function of that arity.
///
/// The heads that name one are a variable, a universe instance of one, a projection of a `rec` group, whose member type the group carries, and a stuck elimination, whose result states it, and an application or a record projection of such a head ([`names_a_type`]) — each read by [`synth_neutral`], which is a lookup, a substitution and a reduction, and never reaches conversion. They are the heads the elaborator types a spine under, so `f(n)(p)` against `f(n)(q)` for two proofs meets irrelevance in both checkers or in neither.
fn spine_telescope(
    kernel: &mut Kernel,
    head: &Term,
    arity: usize,
) -> Result<Option<Telescope<Term>>, Error> {
    if !names_a_type(head) {
        return Ok(None);
    }
    let Some(type_) = synth_neutral(kernel, head)? else {
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

/// Whether `head` is a spine [`synth_neutral`] reads a type for: a variable, a universe instance of one, a projection of a `rec` group or a stuck elimination, under any run of applications and projections.
fn names_a_type(head: &Term) -> bool {
    match &**head {
        Subterm::Var(var) => var.as_free().is_some(),
        Subterm::Instance(_) | Subterm::Match(_) => true,
        Subterm::Apply(apply) => names_a_type(&apply.head),
        Subterm::Proj(proj) => names_a_type(&proj.head),
        _ => head.as_rec_proj().is_some(),
    }
}

/// Compare two term sequences pairwise at `Type`. Length is part of the shape.
fn compare_each<'a>(
    kernel: &mut Kernel,
    history: &mut History,
    this: impl ExactSizeIterator<Item = &'a Term>,
    that: impl ExactSizeIterator<Item = &'a Term>,
) -> Result<bool, Error> {
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

/// Compare two field sequences at the types `telescope` assigns them, each `rest` opened at the left field's value — so a field at a proposition is discharged by irrelevance without being read, and a dependent field is compared at the type its predecessors determine. A telescope shorter than the fields, or none at all, leaves the remainder at `Type`, the untyped comparison, which can only refuse more. Length is part of the shape.
fn compare_fields_at<B: Bound>(
    kernel: &mut Kernel,
    history: &mut History,
    telescope: Option<Telescope<B>>,
    this: &[Term],
    that: &[Term],
) -> Result<bool, Error> {
    if this.len() != that.len() {
        return Ok(false);
    }

    let mut cursor = telescope.as_ref().map(Telescope::cursor);
    for (left, right) in this.iter().zip(that) {
        let entry = cursor.as_ref().and_then(Cursor::entry);
        let type_ = match (entry, cursor.as_mut()) {
            (Some((_, type_)), Some(cursor)) => {
                cursor.advance(left.clone());
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

/// [`compare`] at `Type`, for a child position whose type its head does not hand us: see the module documentation on a child with no type of its own.
fn ground(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Term,
    that: &Term,
) -> Result<bool, Error> {
    compare(kernel, history, &Term::type_ground(), this, that)
}
