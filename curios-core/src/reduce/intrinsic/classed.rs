//! The atoms the carriers' readers read in a pair, handed out to be classed and read back one spelling to a class.
//!
//! **The readers key atoms on spelling.** Cancellation, a monomial's factors, a connective's leaves, the truth table, the linear views and the symmetric peel each identify two atoms by identity ([`Atoms`](crate::Atoms), the truth table's own table), so two atoms that convert without being identical — two calls differing in a proof of one bound, in a function argument whose body commutes a sum, in a stuck `match`'s arm — are two atoms to every one of them. With one such atom on each side a reader hands the pair on as a residual; with two it pairs nothing.
//!
//! **Which atoms are one is not decided here.** [`atoms_of`] hands out the atoms of a pair, a checker says of two of them whether they convert ([`Classes::of`]), and [`classed`] hands the pair back with each atom spelled as its class's representative, so every reader pairs them by the identity it already reads. Nothing here asks a judgment: the question is the caller's closure, asked by the checker that holds the answer.
//!
//! **A node is forced before it is read, as the truth table forces it.** A stuck connective keeps its right operand as written, and an operand written as a call — a negation, an operator still spelled through its witness — is an operation only once it is reduced, so each node is reduced as a [`Probe`] before the walk asks whether the readers read through it. An atom is therefore handed out in weak-head form, which is what lets its head say whether it may be another's.
//!
//! **Probe-side only.** A term replaced by its reduct, or by one it converts with, denotes what it did, so a verdict on the classed pair is one on the pair as written. Nothing is written back: a reading that still decides nothing leaves its congruence the spelling it was handed.

use {
    crate::{
        Cost, Declaration, Intrinsic, Nat, Probe, ReduceError, Reducer, Subterm, Term, Var, Visit,
    },
    curios_algebra::{Carrier, Family, Operation, declares},
    curios_utilities::recurse,
    std::{collections::HashMap, mem},
};

/// Which atoms of one pair are one: each atom that converts with an earlier one, under the earlier one, its class's representative.
#[derive(Debug, Default)]
pub struct Classes {
    representatives: HashMap<Term, Term>,
}

impl Classes {
    /// `atoms` classed by `same`, in the order they were handed out: an atom joins the first class whose representative `same` says it converts with, and otherwise opens one.
    ///
    /// **A pair is asked only where it may be one**, read off the two atoms' heads: two applications of one head to as many arguments, two stuck operations of one kind, or two other terms neither of which is a variable, a metavariable or a stuck operation. It is a cost filter failing toward refusal: a pair it skips stays two atoms, as every pair was before any was classed.
    pub fn of<E>(
        atoms: &[Term],
        mut same: impl FnMut(&Term, &Term) -> Result<bool, E>,
    ) -> Result<Self, E> {
        let mut opened: Vec<&Term> = Vec::new();
        let mut representatives = HashMap::new();
        for atom in atoms {
            let mut joined = None;
            for representative in &opened {
                if may_be_one(representative, atom) && same(representative, atom)? {
                    joined = Some(*representative);
                    break;
                }
            }
            match joined {
                Some(representative) => {
                    representatives.insert(atom.clone(), representative.clone());
                }
                None => opened.push(atom),
            }
        }
        Ok(Classes { representatives })
    }

    /// Whether no atom joined another's class, so the pair reads as it did.
    pub fn is_empty(&self) -> bool {
        self.representatives.is_empty()
    }
}

/// The atoms the readers read in `this` and `that`, each once, in the order a walk of `this` and then `that` meets them: every operand of a node the readers read through that is not, once forced, itself such a node.
///
/// **A pair neither side of which the readers read through has no atoms.** Two `get`s, two lengths, two calls are each the whole of their side, so the only pair to class would be the pair being decided, and a checker asked whether it converts is asked the question it is in the middle of answering: the kernel's recurrence rule would assume it, and the elaborator would ask without end. Such a pair is the congruence's, which compares its operands one by one.
pub fn atoms_of(
    reducer: &mut impl Reducer,
    this: &Term,
    that: &Term,
) -> Result<Vec<Term>, ReduceError> {
    if !read_through_forced(reducer, this)? && !read_through_forced(reducer, that)? {
        return Ok(Vec::new());
    }
    let mut atoms = Vec::new();
    let mut walk = Walk::new(reducer, |atom: &Term| {
        if !atoms.contains(atom) {
            atoms.push(atom.clone());
        }
        atom.clone()
    });
    walk.spine(this)?;
    walk.spine(that)?;
    Ok(atoms)
}

/// Whether some two of `atoms` may be one, so classing them could change what the readers read.
pub fn classable(atoms: &[Term]) -> bool {
    atoms
        .iter()
        .enumerate()
        .any(|(at, atom)| atoms[..at].iter().any(|earlier| may_be_one(earlier, atom)))
}

/// `this` and `that` as the readers read them once classed — every node forced, every atom spelled as its class's representative — or `None` where no atom joined another's class. Each rebuilt node is charged as the term it is.
pub fn classed(
    reducer: &mut impl Reducer,
    this: &Term,
    that: &Term,
    classes: &Classes,
) -> Result<Option<(Term, Term)>, ReduceError> {
    if classes.is_empty() {
        return Ok(None);
    }
    let mut walk = Walk::new(reducer, |atom: &Term| {
        classes
            .representatives
            .get(atom)
            .cloned()
            .unwrap_or_else(|| atom.clone())
    });
    let classed_this = walk.spine(this)?;
    let classed_that = walk.spine(that)?;
    let built = walk.built;
    reducer.spend(Cost::term(built))?;
    Ok(Some((classed_this, classed_that)))
}

/// Whether two atoms could convert without being identical, read off their heads once forced. A variable converts with nothing but itself, and a metavariable with nothing a comparison that solves nothing can say. Two applications are asked about where they are of one head to as many arguments, two stuck operations where they are of one kind, and a stuck operation converts with nothing else. Any other two — a call beside a stuck `match`, which a folded recursive call and its own unfolding are — are asked about.
fn may_be_one(this: &Term, that: &Term) -> bool {
    match (&**this, &**that) {
        (Subterm::Var(_) | Subterm::Metavar(_), _) | (_, Subterm::Var(_) | Subterm::Metavar(_)) => {
            false
        }
        (Subterm::Apply(this), Subterm::Apply(that)) => {
            this.head == that.head && this.arguments.len() == that.arguments.len()
        }
        (Subterm::Intrinsic(this), Subterm::Intrinsic(that)) => {
            mem::discriminant(this) == mem::discriminant(that)
        }
        (Subterm::Intrinsic(_), _) | (_, Subterm::Intrinsic(_)) => false,
        _ => true,
    }
}

/// `term` forced as the walk forces every node: its weak-head form, or `term` itself where it has none at the type level.
fn forced(reducer: &mut impl Reducer, term: &Term) -> Result<Term, ReduceError> {
    Ok(reducer
        .reduce_forced(term.clone())
        .probed()?
        .unwrap_or_else(|| term.clone()))
}

fn read_through_forced(reducer: &mut impl Reducer, term: &Term) -> Result<bool, ReduceError> {
    Ok(matches!(
        &*forced(reducer, term)?,
        Subterm::Intrinsic(intrinsic) if read_through(intrinsic)
    ))
}

/// Whether the readers read through `intrinsic` to its operands: a sum, a product, a connective or a comparison of the carriers the algebra decides, a `Nat`'s successor floor, and every operation the law table declares commutative, whose operands a symmetric reader pairs.
fn read_through(intrinsic: &Intrinsic) -> bool {
    if matches!(intrinsic, Intrinsic::Nat(Nat::Succ(..))) {
        return true;
    }
    match intrinsic.algebra() {
        Declaration::Operation {
            carrier, operation, ..
        } => {
            declares(carrier, operation, Family::Commutativity)
                || matches!(
                    (carrier, operation),
                    (
                        Carrier::Natural | Carrier::Integer | Carrier::Boolean,
                        Operation::Sum
                            | Operation::Product
                            | Operation::And
                            | Operation::Or
                            | Operation::Xor
                            | Operation::Equal
                            | Operation::Unequal
                            | Operation::Less
                            | Operation::AtMost
                    )
                )
        }
        Declaration::Opaque => false,
    }
}

/// One walk over the nodes the readers read through, each node forced, each atom put through `atom`, and each node rebuilt only where an operand changed. A node is walked once, keyed on its identity and held alive beside its answer, since an identity is an address and the sides of one comparison share their subterms.
struct Walk<'r, R, F> {
    reducer: &'r mut R,
    atom: F,
    walked: HashMap<usize, (Term, Term)>,
    /// The size of what the walk rebuilt, for its caller to charge.
    built: u64,
}

impl<'r, R: Reducer, F: FnMut(&Term) -> Term> Walk<'r, R, F> {
    fn new(reducer: &'r mut R, atom: F) -> Self {
        Walk {
            reducer,
            atom,
            walked: HashMap::new(),
            built: 0,
        }
    }

    fn spine(&mut self, term: &Term) -> Result<Term, ReduceError> {
        if let Some((_, done)) = self.walked.get(&term.identity()) {
            return Ok(done.clone());
        }
        let walked = recurse(|| {
            let forced = forced(self.reducer, term)?;
            match &*forced {
                Subterm::Intrinsic(intrinsic) if read_through(intrinsic) => {
                    self.operands(&forced, intrinsic)
                }
                _ => Ok((self.atom)(&forced)),
            }
        })?;
        self.walked
            .insert(term.identity(), (term.clone(), walked.clone()));
        Ok(walked)
    }

    /// `intrinsic` with each operand walked. The operands are `Intrinsic::traverse`'s, the one definition of what an intrinsic's operands are, taken out by one pass and put back by another.
    fn operands(&mut self, term: &Term, intrinsic: &Intrinsic) -> Result<Term, ReduceError> {
        let mut masking = Visit::masking(|_, _: &Var| None, Term::type_ground());
        intrinsic.traverse(&mut masking);

        let mut operands = Vec::new();
        let mut changed = false;
        for operand in masking.take_masked_children() {
            let walked = self.spine(&operand)?;
            changed |= walked != operand;
            operands.push(walked);
        }
        if !changed {
            return Ok(term.clone());
        }
        self.built += 1 + operands.len() as u64;

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
