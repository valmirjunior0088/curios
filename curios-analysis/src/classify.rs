//! The classification of a term at a carrier: its layers of the algebra over opaque atoms, each classed once from the leaves up.
//!
//! **What a layer is, is the table's.** `Intrinsic::algebra` says which intrinsic is an operation and which of its operands its meaning reads. An operation is a node of the walk, and anything else, once forced, is an opaque atom. No reader is asked what it reads through, so the walk and the readers cannot come to say two things.
//!
//! **An opaque atom is classed by its checker.** [`atoms_of`] hands out the atoms of a pair, a checker says of two of them whether they convert ([`Classes::of`]), and each is spelled as its class's representative. Nothing here asks a judgment: the question is the caller's closure, asked by the checker that holds the answer. A reader that reads its terms one at a time and compares none of them with another asks the same of each ([`atoms_in`], [`Classes::join`], [`classed_in`]).
//!
//! **A node is classed by the readers.** A node's operands are spelled first, and the node over them is held to the nodes of its operation spelled before it, by the chain's readers alone ([`agree`]), and takes the first one's spelling. So an operation one reader reads and the reader above it takes whole is one atom of the reader above wherever its own reader holds it one, and no checker is asked what a node's operands are. The walk folds nothing: a fold over operands spelled alike is a reducer's to take again, where it takes one ([`refold`]).
//!
//! **A node is read against the nodes that stand over its atoms, and no others.** Holding a node asks a reading against each earlier node of its operation, and a term holds many of one: a long sum of operations would be read pair by pair, at every fold above it. The readers that pair operands hold two operations one where their operands pair off, and the linear ones where their normal forms agree, each over one set of atoms, so the earlier nodes are kept by the atoms each stands over (`bucket`) and two nodes over two sets are told apart with nothing read. A cost filter failing toward refusal, as the one on two atoms' heads is.
//!
//! **A node the table leaves an operand out of is its checker's.** A quotient carries a proof its meaning does not read, and two quotients of one dividend by one divisor are one where their proofs convert, which only a checker says. Such a node is handed out as an atom once the operands its meaning reads are walked, and is spelled as its class's representative is, over that one's operands.
//!
//! **A node is forced before it is read, as the truth table forces it.** A stuck connective keeps its right operand as written, and an operand written as a call — a negation, an operator still spelled through its witness — is an operation only once it is reduced, so each node is reduced as a [`Probe`] before the walk asks the table what it is. An atom is therefore handed out in weak-head form, which is what lets its head say whether it may be another's.
//!
//! **Probe-side only.** A term replaced by its reduct, or by one it converts with, denotes what it did, so a verdict on the classed pair is one on the pair as written. Nothing is written back: a reading that still decides nothing leaves its congruence the spelling it was handed.

#[cfg(test)]
mod tests;

#[cfg(test)]
mod test_support;

use {
    crate::{Outcome, Read, read},
    curios_algebra::{Carrier, Family, Operation, declares},
    curios_core::{
        Cost, Declaration, Free, Intrinsic, Probe, ReduceError, Reducer, Subterm, Term, Var, Visit,
        reduce_intrinsic,
    },
    curios_utilities::recurse,
    std::{
        collections::{HashMap, HashSet},
        hash::{DefaultHasher, Hash, Hasher},
        mem,
        rc::Rc,
    },
};

/// What the table declares a node: an operation at its carrier. A rounding direction and a grain are part of it, so two sums rounded two ways, and a concatenation of bytes beside one of bits, are nodes of two kinds.
type Kind = (Carrier, Operation);

/// The opaque atoms a term stands over as it is spelled: a hash of each one's spelling, in order and once.
type Over = Rc<[u64]>;

/// What a node shares with every earlier node it is read against: the operation the table declares it, and a hash of the atoms it stands over.
type Bucket = (Kind, u64);

/// Which terms are one: each opaque atom that converts with an earlier one under the earlier one, its class's representative, and each node the readers hold one with an earlier node of its operation under that node's spelling.
#[derive(Debug, Default)]
pub struct Classes {
    representatives: HashMap<Term, Term>,
    /// Each class's representative, in the order its class was opened.
    opened: Vec<Term>,
    /// The nodes held so far, each as it is spelled, by what a later node shares with the ones it is read against ([`bucket`]) and in the order they were met.
    nodes: HashMap<Bucket, Vec<Term>>,
    /// Every node met, as it was spelled over its operands, under the node it is held to — itself where it opened a class.
    held: HashMap<Term, Term>,
    /// How an atom that stands for its own class is spelled, where that is not as itself: a node its checker classes, over its spelled operands. Kept with the classes, so a reader that reads a term at a time spells a representative as it did where it met it.
    spelled: HashMap<Term, Term>,
    /// The atoms each spelled term met stands over, so a node's are its operands' joined and a term two nodes share is read once.
    over: HashMap<Term, Over>,
}

impl Classes {
    /// `atoms` classed by `same`, in the order they were handed out: an atom joins the first class whose representative `same` says it converts with, and otherwise opens one.
    ///
    /// **A pair is asked only where it may be one**, read off the two atoms' heads: two applications of one head to as many arguments, two stuck operations of one kind, or two other terms neither of which is a variable, a metavariable or a stuck operation. It is a cost filter failing toward refusal: a pair it skips stays two atoms, as every pair was before any was classed.
    ///
    /// **Two applications apart in their heads' universe instances are asked about by their arguments.** Each written call of a universe-polymorphic definition takes a level of its own, so `size(f(a + b))` written twice is two heads, and a checker handed the two whole compares the levels first, which no value of the two reads: an atom the walks hand out is a value of a carrier that holds no type, an operand of an operation the table declares at one or a term its reader reads at one, and a universe instance is no part of such a value ([`curios_core::same_atom`]). So `same` is asked of each pair of arguments in turn, the heads being one head already, and the two are one where every pair is.
    ///
    /// **A classing made while another is asking is held inside what that one asked.** `asked` is the two atoms a classing in progress is having compared, where this one is made inside that comparison, and an atom is then classed only where it stands inside one of the two and is neither: every pair asked about is smaller than the pair it is asked for, so classings nest no deeper than a term does. Without it they nest without end: the two calls of a recursive function unfold to two stuck `match`es whose arms hold the calls one step on, each pair the atoms of a product nothing pairs, and comparing one pair classes the next. Those atoms come of an unfolding and stand inside nothing that was asked, so they stay two atoms, which is the refusing direction.
    pub fn of<E>(
        atoms: &[Term],
        asked: Option<&(Term, Term)>,
        same: impl FnMut(&Term, &Term) -> Result<bool, E>,
    ) -> Result<Self, E> {
        let mut classes = Classes::default();
        classes.join(atoms, asked, same)?;
        Ok(classes)
    }

    /// `atoms` classed among the classes already made, by the rule [`Classes::of`] states: what a reader asks that meets its atoms a term at a time and keeps its classes from one term to the next, so an atom is in one class wherever it was read. An atom already classed is asked about no more.
    pub fn join<E>(
        &mut self,
        atoms: &[Term],
        asked: Option<&(Term, Term)>,
        mut same: impl FnMut(&Term, &Term) -> Result<bool, E>,
    ) -> Result<(), E> {
        for atom in atoms {
            if asked.is_some_and(|asked| !stands_inside(atom, asked)) {
                continue;
            }
            if self.representatives.contains_key(atom) || self.opened.contains(atom) {
                continue;
            }
            let mut joined = None;
            for representative in &self.opened {
                if may_be_one(representative, atom) && one(representative, atom, &mut same)? {
                    joined = Some(representative.clone());
                    break;
                }
            }
            match joined {
                Some(representative) => {
                    self.representatives.insert(atom.clone(), representative);
                }
                None => self.opened.push(atom.clone()),
            }
        }
        Ok(())
    }
}

/// The opaque atoms of `this` and `that`, each once, in the order a walk of `this` and then `that` meets them: every operand a node's meaning reads that is not, once forced, itself a node, and every node the table leaves an operand out of.
///
/// **A pair neither side of which the walk reads into has no atoms.** Two calls, two stuck `match`es, two operations the table does not declare, two quotients are each the whole of their side, so the only pair to class would be the pair being decided, and a checker asked whether it converts is asked the question it is in the middle of answering, without end. Such a pair is the congruence's, which compares its operands one by one.
pub fn atoms_of(
    reducer: &mut impl Reducer,
    this: &Term,
    that: &Term,
) -> Result<Vec<Term>, ReduceError> {
    if !read_into(&forced(reducer, this)?) && !read_into(&forced(reducer, that)?) {
        return Ok(Vec::new());
    }
    let mut atoms = Vec::new();
    let mut walk = Walk::new(reducer, Reading::Collected(&mut atoms));
    walk.spine(this, None)?;
    walk.spine(that, None)?;
    Ok(atoms)
}

/// The opaque atoms of `term`, each once, in the order a walk meets them, and `term` itself, forced, where it is no node: [`atoms_of`] for a term read with nothing beside it, which a reader asks that compares the term with none and classes its atoms among those of every term it read before ([`Classes::join`]).
pub fn atoms_in(reducer: &mut impl Reducer, term: &Term) -> Result<Vec<Term>, ReduceError> {
    let mut atoms = Vec::new();
    let mut walk = Walk::new(reducer, Reading::Collected(&mut atoms));
    walk.spine(term, None)?;
    Ok(atoms)
}

/// The opaque atoms under one stuck operation, each once, in the order a walk of its operands meets them: empty where the table declares it no operation.
///
/// **The operation and its operands are taken as they stand.** It is the reduct a reducer has just reached, so forcing it would ask that reducer for the term it is in the middle of reducing, and its operands are as the fold left them (`Standing`). A node the fold left as written — a connective's right operand behind a stuck left — is an atom here as it is spelled.
pub fn atoms_within(
    reducer: &mut impl Reducer,
    operation: &Intrinsic,
) -> Result<Vec<Term>, ReduceError> {
    let Some((_, read)) = layer(operation) else {
        return Ok(Vec::new());
    };
    let mut atoms = Vec::new();
    let mut standing = Standing(reducer);
    let mut walk = Walk::new(&mut standing, Reading::Collected(&mut atoms));
    walk.operands(&Term::intrinsic(operation.clone()), operation, read)?;
    Ok(atoms)
}

/// A stuck `operation` folded once more over its operands as `classes` spells them: the fold of the classed operation where it is no longer that operation — a literal, an operand, another operation — and `None` where no operand moved or the fold still stands at the operation.
///
/// **A fold pairs atoms by their identity, and which terms are one is conversion's to say.** `f(a + b) == f(b + a)` is stuck to the fold and `true` to conversion, as `Nat/and(x, y) == Nat/and(y, x)` is, so a `match` on either would not reduce where conversion holds its two sides one term. The checker classes the operation's opaque atoms ([`atoms_within`], [`Classes::of`]), the readers its nodes, and the fold is taken again over one spelling to a class.
///
/// **A fold that decides no more keeps the spelling it had.** A representative is picked by the order a walk met the terms in, which is no property of the term, so a classed operation that is still stuck at its operation is dropped and the operation stands as it was written: nothing respelled is written back. What is kept is a term the fold built out of the operation's own operands, as it builds one for two operands spelled alike.
pub fn refold(
    reducer: &mut impl Reducer,
    operation: &Intrinsic,
    classes: &mut Classes,
) -> Result<Option<Term>, ReduceError> {
    let Some((_, read)) = layer(operation) else {
        return Ok(None);
    };
    let term = Term::intrinsic(operation.clone());
    let mut standing = Standing(&mut *reducer);
    let mut walk = Walk::new(&mut standing, Reading::Spelled(classes));
    let (_, respelled, _) = walk.operands(&term, operation, read)?;
    let built = walk.built;
    reducer.spend(Cost::term(built))?;
    let Subterm::Intrinsic(respelled) = &*respelled else {
        return Ok(None);
    };
    if respelled == operation {
        return Ok(None);
    }
    collapsed(reducer, respelled)
}

/// Whether some two of `atoms` may be one, so classing them could change what the readers read.
pub fn classable(atoms: &[Term]) -> bool {
    // A variable is one with nothing but itself, and a term's atoms are mostly variables: only the rest are paired.
    let paired = atoms
        .iter()
        .filter(|atom| !matches!(&***atom, Subterm::Var(_) | Subterm::Metavar(_)))
        .collect::<Vec<_>>();
    paired
        .iter()
        .enumerate()
        .any(|(at, atom)| paired[..at].iter().any(|earlier| may_be_one(earlier, atom)))
}

/// `this` and `that` as the readers read them once classed — every node forced, every opaque atom spelled as its class's representative, every node as the first node its readers hold it one with — or `None` where that moved neither side. Each rebuilt node is charged as the term it is.
pub fn classed(
    reducer: &mut impl Reducer,
    this: &Term,
    that: &Term,
    classes: &mut Classes,
) -> Result<Option<(Term, Term)>, ReduceError> {
    let mut walk = Walk::new(reducer, Reading::Spelled(classes));
    let (this_key, classed_this) = walk.spine(this, None)?;
    let (that_key, classed_that) = walk.spine(that, None)?;
    let built = walk.built;
    reducer.spend(Cost::term(built))?;
    Ok((classed_this != this_key || classed_that != that_key)
        .then_some((classed_this, classed_that)))
}

/// `term` as the readers read it under `classes`, handed back whether or not anything moved: [`classed`] for a term read with nothing beside it, so a reader that keeps its classes holds one spelling of a term from one reading to the next.
pub fn classed_in(
    reducer: &mut impl Reducer,
    term: &Term,
    classes: &mut Classes,
) -> Result<Term, ReduceError> {
    let mut walk = Walk::new(reducer, Reading::Spelled(classes));
    let (_, classed) = walk.spine(term, None)?;
    let built = walk.built;
    reducer.spend(Cost::term(built))?;
    Ok(classed)
}

/// Whether the chain's readers hold two nodes of one operation one as they are spelled, outright and with no judgment asked: what holds a node to an earlier node of its bucket.
///
/// **Two nodes that are one term up to universe instances are one.** Each is a value of a carrier that holds no type, which a universe instance is no part of ([`curios_core::same_atom`]).
///
/// **A residual is not followed.** A reader that pairs all but one operand of each side hands the other two back for a checker to compare, and here they are spelled already: two that were one are one term, which the reader pairs, and two that are left are two.
pub(crate) fn agree(
    reducer: &mut impl Reducer,
    this: &Term,
    that: &Term,
) -> Result<bool, ReduceError> {
    if this == that || this.erased_universes() == that.erased_universes() {
        return Ok(true);
    }
    let (Subterm::Intrinsic(left), Subterm::Intrinsic(right)) = (&**this, &**that) else {
        return Ok(false);
    };
    Ok(matches!(
        read(reducer, left.clone(), right.clone())?,
        Read::Decided(Outcome::Equal)
    ))
}

/// Whether `same` holds two atoms that may be one: asked of the two whole, or of each pair of their arguments where they are two applications apart in their heads' universe instances and in nothing else of their heads.
fn one<E>(
    this: &Term,
    that: &Term,
    same: &mut impl FnMut(&Term, &Term) -> Result<bool, E>,
) -> Result<bool, E> {
    match (&**this, &**that) {
        (Subterm::Apply(left), Subterm::Apply(right))
            if left.head != right.head
                && left.head.erased_universes() == right.head.erased_universes() =>
        {
            for (left, right) in left.params().zip(right.params()) {
                if !same(left, right)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        _ => same(this, that),
    }
}

/// Whether `atom` stands inside one of the two atoms `asked` about and is neither of them.
fn stands_inside(atom: &Term, asked: &(Term, Term)) -> bool {
    let within = |outer: &Term| outer != atom && outer.mentions_term(atom);
    within(&asked.0) || within(&asked.1)
}

/// Whether two atoms could convert without being identical, read off their heads once forced. A variable converts with nothing but itself, and a metavariable with nothing a comparison that solves nothing can say. Two applications are asked about where they are of one head, up to its universe instances, to as many arguments, two stuck operations where they are of one kind, and a stuck operation converts with nothing else. Any other two — a call beside a stuck `match`, which a folded recursive call and its own unfolding are — are asked about.
fn may_be_one(this: &Term, that: &Term) -> bool {
    match (&**this, &**that) {
        (Subterm::Var(_) | Subterm::Metavar(_), _) | (_, Subterm::Var(_) | Subterm::Metavar(_)) => {
            false
        }
        (Subterm::Apply(this), Subterm::Apply(that)) => {
            this.arguments.len() == that.arguments.len()
                && (this.head == that.head
                    || this.head.erased_universes() == that.head.erased_universes())
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

/// `intrinsic` as a layer of the walk: the operation the table declares it, at its carrier, and how many of its operands, the first of [`Intrinsic::traverse`]'s, that operation's meaning reads. `None` where the table declares it no operation, or one at a carrier whose values hold a type, which is an opaque atom.
///
/// **No operation over a list is a node.** An atom the walk hands out is then an operand of an operation at a carrier that holds no type, which is what lets two of them be asked about, and held one, up to universe instances.
fn layer(intrinsic: &Intrinsic) -> Option<(Kind, usize)> {
    match intrinsic.algebra() {
        Declaration::Operation {
            carrier,
            operation,
            operands,
        } if !carrier.holds_types() => Some(((carrier, operation), operands.as_slice().len())),
        _ => None,
    }
}

/// Whether the walk reads into `term` and hands it out to no one: an operation the table declares over every operand it has. One the table leaves an operand out of is handed out whole once its operands are walked, as an opaque atom is.
fn read_into(term: &Term) -> bool {
    match &**term {
        Subterm::Intrinsic(intrinsic) => {
            layer(intrinsic).is_some_and(|(_, read)| children(intrinsic).len() <= read)
        }
        _ => false,
    }
}

/// The bucket a node of `kind` is held in: its operation, and the atoms it stands over.
///
/// **The truth table is left its whole operation.** It holds two formulas one over two sets of atoms, `p && (p || q)` beside `p && (p || r)`, where every other reader of two operations of one kind holds them one over one set. So an operation over truth values is read against every earlier one of its kind.
fn bucket(over: &mut HashMap<Term, Over>, kind: Kind, node: &Term) -> Bucket {
    let atoms = match kind.0 {
        Carrier::Boolean => 0,
        _ => hashed(&*atoms_under(over, node)),
    };
    (kind, atoms)
}

/// The opaque atoms `term` stands over as it is spelled: its operands' where the walk reads into it, and itself otherwise, known by its spelling with its universe instances erased, which is what a reader of a carrier that holds no type knows an atom by.
fn atoms_under(over: &mut HashMap<Term, Over>, term: &Term) -> Over {
    if let Some(kept) = over.get(term) {
        return kept.clone();
    }
    let operands = match &**term {
        Subterm::Intrinsic(intrinsic) => layer(intrinsic).and_then(|(_, read)| {
            let operands = children(intrinsic);
            (operands.len() <= read).then_some(operands)
        }),
        _ => None,
    };
    let atoms: Over = match operands {
        Some(operands) => {
            let mut joined = Vec::new();
            for operand in &operands {
                joined.extend_from_slice(&recurse(|| atoms_under(over, operand)));
            }
            joined.sort_unstable();
            joined.dedup();
            joined.into()
        }
        None => Rc::from([hashed(&term.erased_universes())]),
    };
    over.insert(term.clone(), atoms.clone());
    atoms
}

/// One hash of `value`, the same in every run, so a cost counted twice is counted alike.
fn hashed(value: &(impl Hash + ?Sized)) -> u64 {
    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish()
}

/// An intrinsic's operands, in the order [`Intrinsic::traverse`] meets them: the one definition of what an intrinsic's operands are.
fn children(intrinsic: &Intrinsic) -> Vec<Term> {
    let mut masking = Visit::masking(|_, _: &Var| None, Term::type_ground());
    intrinsic.traverse(&mut masking);
    masking.take_masked_children()
}

/// Whether a node of `kind` standing under `parent` is held to the earlier nodes of its operation. One standing under a node of its own operation is not, where the law table declares that operation associative: a reader that holds an association flattens it, so the inner node is no atom of the outer one's reader, and holding it would cost a reading against every node of a long sum for a spelling nothing reads.
fn held_under(parent: Option<&Intrinsic>, kind: Kind) -> bool {
    let outer = parent.and_then(layer).map(|(outer, _)| outer);
    !(outer == Some(kind) && declares(kind.0, kind.1, Family::Associativity))
}

/// The fold of a respelled operation where it is no longer that operation: a literal, one of its own operands, or another operation.
fn collapsed(
    reducer: &mut impl Reducer,
    respelled: &Intrinsic,
) -> Result<Option<Term>, ReduceError> {
    let folded: Term = reduce_intrinsic(reducer, respelled)?.into();
    let stands = matches!(
        &*folded,
        Subterm::Intrinsic(still) if mem::discriminant(still) == mem::discriminant(respelled)
    );
    if !stands {
        return Ok(Some(folded));
    }
    Ok(children(respelled).contains(&folded).then_some(folded))
}

/// A reducer that takes every term as reduced already, for a walk over a stuck operation a reducer has just left: its operands are as the fold reduced them, and forcing them again is work the kernel's memo replays the identities of on every read, which a walk made at every stuck fold would compound. What the walk builds is still charged to the reducer this one stands before.
struct Standing<'a, R>(&'a mut R);

impl<R: Reducer> Reducer for Standing<'_, R> {
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
        Ok(term)
    }

    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
        Ok(term)
    }

    fn spend(&mut self, cost: Cost) -> Result<(), ReduceError> {
        self.0.spend(cost)
    }

    fn fresh_binder(&mut self, hint: Option<&str>) -> Free {
        self.0.fresh_binder(hint)
    }
}

/// What a walk does with what it meets.
enum Reading<'c> {
    /// Hand each opaque atom out, once, in the order it is met.
    Collected(&'c mut Vec<Term>),
    /// Spell each opaque atom as its class's representative and each node as the node it is held to.
    Spelled(&'c mut Classes),
}

/// One walk over a term's nodes, each forced, in two spellings: the *key*, the term with every node forced and nothing respelled, which is what an atom is handed out as and looked up by; and the term as the reading spells it. A node is walked once for each parent's answer to whether it is held, keyed on its identity and kept alive beside its answer, since an identity is an address and the sides of one comparison share their subterms.
struct Walk<'r, 'c, R> {
    reducer: &'r mut R,
    reading: Reading<'c>,
    /// Each term met, forced, kept alive beside its answer.
    forced: HashMap<usize, (Term, Term)>,
    walked: HashMap<(usize, bool), (Term, Term)>,
    /// The atoms handed out so far, so each is handed out once however many a term holds.
    handed: HashSet<Term>,
    /// The size of what the walk rebuilt, for its caller to charge.
    built: u64,
}

impl<'r, 'c, R: Reducer> Walk<'r, 'c, R> {
    fn new(reducer: &'r mut R, reading: Reading<'c>) -> Self {
        Walk {
            reducer,
            reading,
            forced: HashMap::new(),
            walked: HashMap::new(),
            handed: HashSet::new(),
            built: 0,
        }
    }

    /// `term` as its key and as the reading spells it, standing under `parent`.
    fn spine(
        &mut self,
        term: &Term,
        parent: Option<&Intrinsic>,
    ) -> Result<(Term, Term), ReduceError> {
        let forced = match self.forced.get(&term.identity()) {
            Some((_, forced)) => forced.clone(),
            None => {
                let forced = forced(self.reducer, term)?;
                self.forced
                    .insert(term.identity(), (term.clone(), forced.clone()));
                forced
            }
        };
        let declared = match &*forced {
            Subterm::Intrinsic(intrinsic) => layer(intrinsic).map(|layer| (intrinsic, layer)),
            _ => None,
        };
        let held = declared.is_none_or(|(_, (kind, _))| held_under(parent, kind));
        if let Some(done) = self.walked.get(&(term.identity(), held)) {
            return Ok(done.clone());
        }
        let done = recurse(|| match declared {
            Some((intrinsic, layer)) => self.node(&forced, intrinsic, layer, held),
            None => Ok(self.atom(forced.clone(), forced.clone())),
        })?;
        self.walked.insert((term.identity(), held), done.clone());
        Ok(done)
    }

    /// What is handed out under `key`, which this walk spelled `spelled` over its operands: collected, or spelled as its class's representative is.
    fn atom(&mut self, key: Term, spelled: Term) -> (Term, Term) {
        match &mut self.reading {
            Reading::Collected(atoms) => {
                if self.handed.insert(key.clone()) {
                    atoms.push(key.clone());
                }
                (key.clone(), key)
            }
            Reading::Spelled(classes) => {
                // A representative is met before what joined it, by the walk that handed the atoms out and by every walk that spells them, each over the same terms in the same order: so its spelling is kept by the time it is asked for.
                let class = classes.representatives.get(&key).unwrap_or(&key);
                if let Some(kept) = classes.spelled.get(class) {
                    return (key.clone(), kept.clone());
                }
                if *class != key {
                    return (key.clone(), class.clone());
                }
                if spelled != key {
                    classes.spelled.insert(key.clone(), spelled.clone());
                }
                (key, spelled)
            }
        }
    }

    /// A node of `kind`: its operands first, then the node over them among the earlier nodes of its operation.
    fn node(
        &mut self,
        forced: &Term,
        intrinsic: &Intrinsic,
        (kind, read): (Kind, usize),
        held: bool,
    ) -> Result<(Term, Term), ReduceError> {
        let (key, spelled, whole) = self.operands(forced, intrinsic, read)?;
        if !whole {
            // An operand its meaning does not read stands as written, and whether two of those convert is a checker's to say.
            return Ok(self.atom(key, spelled));
        }
        if matches!(self.reading, Reading::Collected(_)) {
            return Ok((key.clone(), key));
        }
        let spelled = match held {
            true => self.held(spelled, kind)?,
            false => spelled,
        };
        Ok((key, spelled))
    }

    /// `node`, of `kind`, as the first node of its bucket the readers hold it one with, and itself where there is none.
    fn held(&mut self, node: Term, kind: Kind) -> Result<Term, ReduceError> {
        let Reading::Spelled(classes) = &mut self.reading else {
            return Ok(node);
        };
        if let Some(earlier) = classes.held.get(&node) {
            return Ok(earlier.clone());
        }
        let bucket = bucket(&mut classes.over, kind, &node);
        let earlier = classes.nodes.entry(bucket).or_default();
        // What holding a node asks: a reading against each earlier node of its bucket, until one agrees.
        curios_profile::sample!("classify::held", earlier.len());
        let mut first = None;
        for earlier in earlier.iter() {
            if agree(self.reducer, earlier, &node)? {
                first = Some(earlier.clone());
                break;
            }
        }
        let first = first.unwrap_or_else(|| {
            earlier.push(node.clone());
            node.clone()
        });
        classes.held.insert(node, first.clone());
        Ok(first)
    }

    /// `intrinsic` with each of its first `read` operands walked and the rest as they stand, as its key and as the reading spells it, and whether `read` was every operand it has. The operands are taken out by one traversal ([`children`]) and put back by another.
    fn operands(
        &mut self,
        term: &Term,
        intrinsic: &Intrinsic,
        read: usize,
    ) -> Result<(Term, Term, bool), ReduceError> {
        let children = children(intrinsic);
        let whole = children.len() <= read;

        let mut keys = Vec::new();
        let mut operands = Vec::new();
        let (mut keyed, mut changed) = (false, false);
        for (index, operand) in children.into_iter().enumerate() {
            let (key, walked) = match index < read {
                true => self.spine(&operand, Some(intrinsic))?,
                false => (operand.clone(), operand.clone()),
            };
            keyed |= key != operand;
            changed |= walked != operand;
            keys.push(key);
            operands.push(walked);
        }
        let rebuild = |operands: Vec<Term>| -> Term {
            let mut operands = operands.into_iter();
            let rebuilt = intrinsic.traverse(&mut Visit::rewriting(
                |_, _: &Var| None,
                Box::new(move |_, operand: &Term| {
                    Some(operands.next().unwrap_or_else(|| operand.clone()))
                }),
            ));
            Subterm::Intrinsic(rebuilt).into()
        };
        let key = match keyed {
            true => rebuild(keys),
            false => term.clone(),
        };
        if !changed {
            return Ok((key, term.clone(), whole));
        }
        self.built += 1 + operands.len() as u64;
        Ok((key, rebuild(operands), whole))
    }
}
