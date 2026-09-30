//! What the carriers' readers read in a pair conversion decided nothing about, forced once for all of them.
//!
//! **The readers key atoms on spelling.** Cancellation, a monomial's factors, a connective's leaves, the truth table and the linear views each identify two atoms by identity ([`Atoms`](crate::Atoms), the truth table's own table), and the fold leaves a stuck application's arguments exactly as written, since weak-head reduction stops at the head it is stuck on. So `f(a + b)` and `f(b + a)` are two atoms to every reader, though conversion decides that pair the moment it compares it directly. What a pair of them came to then fell to the positional congruence, whose operand order is a structural hash, and one equation held or failed with the order its binders were declared in.
//!
//! **Forced once, for every reader.** [`force_atoms`] walks the nodes the readers read through and hands each atom back with its arguments reduced under the rigid head they are stuck on — recursively, since an argument's own applications are stuck the same way, and never under a binder, where reduction would meet loose indices — and with every sum and product inside those arguments put in one order. Both places the readers run ask it: the conversion chain reads the forced pair with every reader it has, where a `Nat`-only retry of the sum peel was before this, and the truth table a connective is put to against a term that is no intrinsic reads it again the same way.
//!
//! **Probe-side only, and sound for the reason each half is.** Reduction is definitional and putting a sum's summands or a product's factors in order is the carrier's commutativity, so a verdict on the forced pair is one on the pair as written. Each reduction is a [`Probe`]: an argument with no value at the type level is kept as written. Nothing here is written back: a chain that still decides nothing leaves its congruence the spelling it was handed, so no reordered term reaches the reducer and the oscillation [`Nat::summands`] records is not re-entered.

use {
    crate::{
        Apply, Argument, Bound, Cost, Declaration, Intrinsic, Nat, Probe, ReduceError, Reducer,
        Subterm, Term, Var, Visit, int_in_order,
    },
    curios_algebra::{Carrier, Operation},
    curios_utilities::recurse,
    std::{cell::RefCell, collections::HashMap, rc::Rc},
};

/// `this` and `that` with every atom the carriers' readers read in them forced, or `None` where no atom moved — so a caller reads a pair again only where the forcing changed it.
///
/// A pair neither side of which the readers read through — two `get`s, two lengths — is `None` at once: it is the congruence's, which compares its operands one by one and needs nothing forced. Otherwise each side is walked, a node the readers read through down to its atoms and any other term as the atom it is, with one memo across the two, since the sides of one comparison share their subterms.
pub fn force_atoms(
    reducer: &mut impl Reducer,
    this: &Term,
    that: &Term,
) -> Result<Option<(Term, Term)>, ReduceError> {
    let read =
        |term: &Term| matches!(&**term, Subterm::Intrinsic(intrinsic) if read_through(intrinsic));
    if !read(this) && !read(that) {
        return Ok(None);
    }
    let mut forcing = Forcing::default();
    let forced_this = forcing.spine(reducer, this)?;
    let forced_that = forcing.spine(reducer, that)?;
    if forced_this == *this && forced_that == *that {
        return Ok(None);
    }
    Ok(Some((forced_this, forced_that)))
}

/// Whether the readers read through `intrinsic` to its operands: a sum, a product, a connective or a comparison of the carriers the algebra decides, and a `Nat`'s successor floor.
fn read_through(intrinsic: &Intrinsic) -> bool {
    if matches!(intrinsic, Intrinsic::Nat(Nat::Succ(..))) {
        return true;
    }
    matches!(
        intrinsic.algebra(),
        Declaration::Operation {
            carrier: Carrier::Natural | Carrier::Integer | Carrier::Boolean,
            operation: Operation::Sum
                | Operation::Product
                | Operation::And
                | Operation::Or
                | Operation::Xor
                | Operation::Equal
                | Operation::Unequal
                | Operation::Less
                | Operation::AtMost,
            ..
        }
    )
}

/// One pair's forcing: every argument forced once, keyed on the node's identity and holding the node alive beside its answer, since an identity is an address — a reduct is a graph, and a walk that forced it once per path would expand it.
#[derive(Default)]
struct Forcing {
    arguments: HashMap<usize, (Term, Term)>,
}

impl Forcing {
    /// A node the readers read through with its operands walked in turn, and an atom with its arguments forced and put in order.
    fn spine<R: Reducer>(&mut self, reducer: &mut R, term: &Term) -> Result<Term, ReduceError> {
        recurse(|| match &**term {
            Subterm::Intrinsic(intrinsic) if read_through(intrinsic) => {
                self.operands(reducer, term, intrinsic, Self::spine)
            }
            Subterm::Apply(apply) => self.applied(reducer, term, apply, Self::atom_argument),
            Subterm::Intrinsic(intrinsic) => {
                self.operands(reducer, term, intrinsic, Self::atom_argument)
            }
            _ => Ok(term.clone()),
        })
    }

    /// One argument of an atom: forced, then with every sum and product in it put in order.
    fn atom_argument<R: Reducer>(
        &mut self,
        reducer: &mut R,
        argument: &Term,
    ) -> Result<Term, ReduceError> {
        let forced = self.argument(reducer, argument)?;
        Ok(ordered(&forced))
    }

    /// An argument reduced as a [`Probe`], and every application and operation it is stuck on forced the same way — the head kept verbatim, as a refinement key's is, since reducing the node would unfold the very definition it is stuck on.
    fn argument<R: Reducer>(&mut self, reducer: &mut R, term: &Term) -> Result<Term, ReduceError> {
        if let Some((_, done)) = self.arguments.get(&term.identity()) {
            return Ok(done.clone());
        }
        let reduced = reducer
            .reduce(term.clone())
            .probed()?
            .unwrap_or_else(|| term.clone());
        let forced = recurse(|| match &*reduced {
            Subterm::Apply(apply) => self.applied(reducer, &reduced, apply, Self::argument),
            Subterm::Intrinsic(intrinsic) => {
                self.operands(reducer, &reduced, intrinsic, Self::argument)
            }
            _ => Ok(reduced.clone()),
        })?;
        self.arguments
            .insert(term.identity(), (term.clone(), forced.clone()));
        Ok(forced)
    }

    /// `apply` with each argument put through `each`, rebuilt only where one changed as a term — the ordering hands back a node of its own for a spelling it left as it was, and rebuilding the atom around it would be charged for nothing.
    fn applied<R: Reducer>(
        &mut self,
        reducer: &mut R,
        term: &Term,
        apply: &Apply,
        mut each: impl FnMut(&mut Self, &mut R, &Term) -> Result<Term, ReduceError>,
    ) -> Result<Term, ReduceError> {
        let mut arguments = Vec::with_capacity(apply.arguments.len());
        let mut changed = false;
        for argument in &apply.arguments {
            let forced = each(self, reducer, &argument.term)?;
            changed |= forced != argument.term;
            arguments.push(Argument {
                term: forced,
                plicity: argument.plicity,
            });
        }
        if !changed {
            return Ok(term.clone());
        }
        reducer.spend(Cost::term(1 + arguments.len() as u64))?;
        Ok(Subterm::Apply(Apply {
            head: apply.head.clone(),
            arguments,
        })
        .into())
    }

    /// `intrinsic` with each operand put through `each`, rebuilt only where one changed. The operands are `Intrinsic::traverse`'s, the one definition of what an intrinsic's operands are, taken out by one pass and put back by another.
    fn operands<R: Reducer>(
        &mut self,
        reducer: &mut R,
        term: &Term,
        intrinsic: &Intrinsic,
        mut each: impl FnMut(&mut Self, &mut R, &Term) -> Result<Term, ReduceError>,
    ) -> Result<Term, ReduceError> {
        let mut masking = Visit::masking(|_, _: &Var| None, Term::type_ground());
        intrinsic.traverse(&mut masking);

        let mut operands = Vec::new();
        let mut changed = false;
        for operand in masking.take_masked_children() {
            let forced = each(self, reducer, &operand)?;
            changed |= forced != operand;
            operands.push(forced);
        }
        if !changed {
            return Ok(term.clone());
        }
        reducer.spend(Cost::term(1 + operands.len() as u64))?;

        let mut operands = operands.into_iter();
        let rebuilt = intrinsic.traverse(&mut Visit::rewriting(
            |_, _: &Var| None,
            Box::new(move |_, operand: &Term| {
                Some(operands.next().unwrap_or_else(|| operand.clone()))
            }),
        ));
        Ok(Subterm::Intrinsic(rebuilt).into())
    }
}

/// `term` with every sum and every product inside it rebuilt in one order, under binders too, so two spellings of one number become one term.
///
/// **Reduction is not enough on its own, and this is the half it does not do.** [`Nat::summands`] keeps first-appearance order — which is what makes read-then-rebuild the identity, and a rebuild that reordered would hand the reducer a new term every pass — so `x + y` and `y + x` reduce to two distinct sums and stay two atoms apart. A product's factors are ordered when the fold builds it, by their hashes as written, so a factor whose own arguments the forcing changed can sit out of place. Ordering by [`Term::structural_hash`] is the discipline the product fold already keeps, applied to both carriers' sums and products alike; sound because each is commutative and congruence carries that under the head.
///
/// One memo across every nested ordering, so a subterm shared across summands is ordered once.
fn ordered(term: &Term) -> Term {
    Ordering::default().of(term)
}

/// The orderings one [`ordered`] call has made, shared by the nested orderings its hook asks for.
#[derive(Clone, Default)]
struct Ordering(Rc<RefCell<HashMap<usize, (Term, Term)>>>);

impl Ordering {
    fn of(&self, term: &Term) -> Term {
        if let Some((_, done)) = self.0.borrow().get(&term.identity()) {
            return done.clone();
        }
        let nested = self.clone();
        let result = recurse(|| {
            term.traverse(&mut Visit::rewriting_shared(
                |_, _: &Var| None,
                // A pre-hook does not descend into what it replaces, so the recursion is the hook's own: each summand or factor is ordered before the node it sits in is.
                Box::new(move |_, node: &Term| {
                    let order = |child: &Term| nested.of(child);
                    Nat::in_order(node, &order).or_else(|| int_in_order(node, &order))
                }),
            ))
        });
        self.0
            .borrow_mut()
            .insert(term.identity(), (term.clone(), result.clone()));
        result
    }
}
