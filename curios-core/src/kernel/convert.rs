//! Definitional equality: whether two terms are interchangeable at a type.
//!
//! Conversion is the rule that decides which programs typecheck, so it is the
//! most consequential thing in this crate. It is *type-directed*: the type
//! drives eta-expansion and it is what makes proof irrelevance possible, so
//! every goal carries the type at which the two sides are being compared.
//!
//! The rules, in the order they are tried:
//!
//! 1. **Proof irrelevance.** A goal at a `Prop`-sorted type is discharged
//!    without looking at either side. Any two inhabitants of a proposition are
//!    definitionally equal, which is what lets erasure drop them wholesale.
//! 2. **Eta.** At a function type both sides are applied to fresh binders and
//!    compared at the codomain; at a Σ type both are projected and compared
//!    componentwise. So `f` and `(x) => f(x)` convert, and so do `p` and
//!    `(p.0, p.1)`, without either side having to be in that shape.
//! 3. **Structure.** Both sides are reduced to weak-head normal form and their
//!    heads compared, recursing on the children.
//!
//! # Termination, and the recurrence rule
//!
//! Two folded recursive calls can unfold forever without ever disagreeing —
//! that is what an equirecursive type is. Conversion therefore keeps a history
//! of the goals it is already inside, and a goal that recurs is *assumed to
//! hold*. This is the coinductive reading: a genuine cycle leaves nothing but
//! itself to check, and any finite disagreement surfaces on a sibling goal
//! before the cycle closes.
//!
//! That rule is where a conversion checker is most likely to be unsound, and
//! the danger is precise: a history hit on two goals that are *not* the same
//! goal accepts terms that are not equal. The guard here is that an entry
//! records the local context alongside the goal, and that every binder in scope
//! is renamed to its position before the entry is made. Two entries collide
//! only when the same comparison is being made under binders of the same types
//! in the same order — which is the same comparison.
//!
//! `curios-elab`'s conversion checker canonicalizes differently: it renames the
//! binders *it* minted, in mint order, and does not key on the context at all.
//! For a strictly nested walk like this one the two orderings coincide, since
//! the binders in scope at a goal are exactly the path to it. The elaborator's
//! walk is not strictly nested — it has a worklist and it parks goals — so
//! whether the two schemes agree there is a genuinely open question, recorded
//! as such in `documentation/DESIGN.md`. This module deliberately does not
//! inherit the answer.
//!
//! # Where this is incomplete, and why that is the safe direction
//!
//! Several positions are compared syntactically rather than up to conversion: a
//! stuck elimination's motive and arms, a `rec` group, and the arguments of a
//! spine, which are compared at `Type` rather than at the types the head
//! assigns them. Each is a place where the kernel may reject a term the
//! elaborator accepted.
//!
//! That direction is deliberate. An incomplete conversion refuses programs; an
//! unsound one admits them. A refusal is visible — it is a disagreement between
//! the two checkers, which is precisely the signal this kernel exists to
//! produce — whereas an over-eager acceptance is silent and is exactly what a
//! second opinion is supposed to catch. Every one of these can be strengthened
//! later against a real program that needs it, and none can be strengthened
//! back from having been wrong.

mod prim;
use prim::convert_prim;

#[cfg(test)]
mod tests;

use {
    super::{Kernel, KernelError, sort::sort_of},
    crate::{
        Bound, FuncType, InductType, Proj, RecMember, Reducer, Struct, StructType, Subterm,
        Telescope, Term, Tuple, TupleType, UniverseInst,
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
/// A goal is stored with the types of the binders in scope at it, and with
/// every one of those binders renamed to its position. Without the rename, the
/// same comparison reached on two rounds of an unfolding cycle differs in
/// nothing but the identities of the binders opened on the way, and the cycle
/// is never recognized; without the context, two different comparisons that
/// happen to be spelled alike would be conflated, which is the unsound
/// direction.
#[derive(Default)]
struct History {
    seen: HashSet<Goal>,
}

#[derive(PartialEq, Eq, Hash)]
struct Goal {
    context: Vec<Term>,
    type_: Term,
    this: Term,
    that: Term,
}

impl History {
    /// Record a goal, reporting whether it was already there.
    ///
    /// The rename is by position in the local context, so a goal reached again
    /// under an identically-typed prefix maps onto the same entry. `capture`
    /// turns each binder into a bound index; the results are keys and are never
    /// opened, so the loose indices they leave behind are inert.
    fn recurs(&mut self, kernel: &Kernel, type_: &Term, this: &Term, that: &Term) -> bool {
        let binders = kernel.local_names();
        let refs = binders.iter().collect::<Vec<_>>();
        let rename = |term: &Term| term.capture(&refs);

        !self.seen.insert(Goal {
            context: kernel.local_types().iter().map(rename).collect(),
            type_: rename(type_),
            this: rename(this),
            that: rename(that),
        })
    }
}

/// Run `walk` with every binder it opens closed again afterwards, on the
/// failing path as well as the succeeding one. A comparison that left binders
/// behind would leak them into the conversion history, where the context is
/// part of the key.
pub(super) fn scoped<T>(kernel: &mut Kernel, walk: impl FnOnce(&mut Kernel) -> T) -> T {
    let mark = kernel.mark();
    let outcome = walk(kernel);
    kernel.retract(mark);

    outcome
}

/// Compare `this` and `that` at `type_`, under the binders currently in scope.
fn compare(
    kernel: &mut Kernel,
    history: &mut History,
    type_: &Term,
    this: &Term,
    that: &Term,
) -> Result<bool, KernelError> {
    kernel.spend()?;

    // Cheapest first: a term converts with itself at any type, and structural
    // sharing makes this hit constantly on terms built by substitution.
    if this == that {
        return Ok(true);
    }

    // Proof irrelevance. Deliberately before reduction: the point is that
    // neither side is examined, and reducing a proof in order to discover it
    // equals another proof is work whose answer was already known.
    if sort_of(kernel, type_)?.is_prop() {
        return Ok(true);
    }

    if history.recurs(kernel, type_, this, that) {
        return Ok(true);
    }

    match Term::unwrap_or_clone(kernel.reduce_forced(type_.clone())?) {
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
    }
}

/// Eta at a function type: apply both sides to the same fresh binders and
/// compare the results at the codomain.
///
/// This is why `f` converts with `(x) => f(x)` without either being reduced
/// into the other's shape — the rule is stated once, here, instead of as a
/// special case in every structural arm.
fn eta_function(
    kernel: &mut Kernel,
    history: &mut History,
    telescope: Telescope<Term>,
    this: &Term,
    that: &Term,
) -> Result<bool, KernelError> {
    scoped(kernel, |kernel| {
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
/// A later field's type may mention an earlier one, and names it by a
/// projection of the *left* side — sound because the earlier components have
/// already been shown equal by the time that type is used.
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
/// Children with no type the head determines are compared at `Type` through
/// [`ground`]. That is a weaker comparison than a typed one — it declines to
/// fire eta or irrelevance — so it can only reject where a typed comparison
/// would have accepted. See the module documentation on incompleteness.
fn structural(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Term,
    that: &Term,
) -> Result<bool, KernelError> {
    match (&**this, &**that) {
        // Levels compare under the item's assumed constraints: two levels the
        // hypotheses force equal are equal in every instance that satisfies
        // them, which is what checking generically means.
        (Subterm::Type(left), Subterm::Type(right)) => Ok(kernel.level_eq(left, right)),
        (Subterm::Prop, Subterm::Prop) => Ok(true),

        (Subterm::Prim(left), Subterm::Prim(right)) => convert_prim(kernel, history, left, right),

        (Subterm::Var(left), Subterm::Var(right)) => Ok(left.unwrap() == right.unwrap()),

        // A metavariable is elaboration-only syntax, and refusing it *here* is
        // what makes the exclusion the kernel's own rather than an inherited
        // guarantee of `zonk_module`'s traversal. `whnf` still treats one as a
        // stuck neutral — a reduction stance, not an admission: the only ways a
        // term is admitted are `infer` and this comparison, and both refuse.
        // The syntactic fast path in `compare` does admit a metavariable
        // against *itself*, and soundly: reflexivity decides nothing about the
        // unknown, which is exactly what this arm exists to prevent.
        (Subterm::Metavar(_), _) | (_, Subterm::Metavar(_)) => {
            Err(KernelError::NotCore(this.clone()))
        }

        // Plicity is part of a function type's identity: `(A) -> A` and
        // `(@A) -> A` have different calling conventions, and conflating them
        // would let a value be applied through the wrong one.
        (Subterm::FuncType(left), Subterm::FuncType(right)) => Ok(left.plicities
            == right.plicities
            && compare_telescope(
                kernel,
                history,
                left.telescope.clone(),
                right.telescope.clone(),
            )?),

        // Two lambdas with no expected type to eta against: compare their
        // bodies under one shared set of binders.
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

        (Subterm::Apply(left), Subterm::Apply(right)) => Ok(left.plicities == right.plicities
            && ground(kernel, history, &left.head, &right.head)?
            && compare_each(kernel, history, &left.params, &right.params)?),

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
            && compare_each(kernel, history, left_params, right_params)?
            && compare_each(kernel, history, left_indices, right_indices)?),

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

        // A stuck elimination. The scrutinee is compared up to conversion,
        // because that is the position an unfolding cycle travels through; the
        // motive and the arms are required to be identical. Two eliminations
        // that enumerate different constructors compute differently on some
        // input even where they agree on this one, and deciding *which*
        // differences are harmless is a judgment this kernel does not yet make.
        (Subterm::Match(left), Subterm::Match(right)) => Ok(left.motive == right.motive
            && left.cases == right.cases
            && ground(kernel, history, &left.head, &right.head)?),

        // A folded recursive call, and a `rec` that forcing declined to unfold.
        // Both are compared syntactically: the interesting case — a cycle that
        // unfolds without disagreeing — is handled by the recurrence rule
        // above, not here.
        (
            Subterm::RecMember(RecMember {
                group: left,
                index: left_index,
            }),
            Subterm::RecMember(RecMember {
                group: right,
                index: right_index,
            }),
        ) => Ok(left_index == right_index && left == right),

        _ => Ok(false),
    }
}

/// Compare two Π/λ telescopes: domains pairwise, then codomains, opening one
/// shared binder per position so both dependent tails speak of the same
/// variable.
fn compare_telescope(
    kernel: &mut Kernel,
    history: &mut History,
    this: Telescope<Term>,
    that: Telescope<Term>,
) -> Result<bool, KernelError> {
    scoped(kernel, |kernel| {
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
                    return ground(kernel, history, &left, &right);
                }
                // Different arities. A function type is not curried in this
                // representation, so this is a real mismatch rather than a
                // shape to normalize.
                _ => return Ok(false),
            }
        }
    })
}

/// [`compare_telescope`] for a Σ, whose terminal carries nothing.
fn compare_field_telescope(
    kernel: &mut Kernel,
    history: &mut History,
    this: Telescope<()>,
    that: Telescope<()>,
) -> Result<bool, KernelError> {
    scoped(kernel, |kernel| {
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
                (Telescope::Done(_), Telescope::Done(_)) => return Ok(true),
                _ => return Ok(false),
            }
        }
    })
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

/// [`compare`] at `Type`, for a child position whose type its head does not
/// hand us. Weaker than a typed comparison, never stronger: see the module
/// documentation on incompleteness.
pub(in crate::kernel::convert) fn ground(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Term,
    that: &Term,
) -> Result<bool, KernelError> {
    compare(kernel, history, &Term::type_ground(), this, that)
}
