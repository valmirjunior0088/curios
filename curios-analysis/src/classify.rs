//! The classification of a term at a carrier: its layers of the algebra over opaque atoms, each classed once from the leaves up.
//!
//! **What a layer is, is the table's.** `Intrinsic::algebra` says which intrinsic is an operation and which of its operands its meaning reads. An operation is a node of the walk, and anything else, once forced, is an opaque atom. No reader is asked what it reads through, so the walk and the readers cannot come to say two things.
//!
//! **An opaque atom is classed by its checker.** [`atoms_of`] hands out the atoms of a pair, a checker says of two of them whether they convert ([`Classes::of`]), and each is spelled as its class's representative. Nothing here asks a judgment: the question is the caller's closure, asked by the checker that holds the answer. A reader that reads its terms one at a time and compares none of them with another asks the same of each ([`atoms_in`], [`Classes::join`], [`classed_in`]). An atom is put to its checker only against the representatives it can be one with, read off a key its head carries ([`Pairing`]), the first opened first.
//!
//! **A node is classed by the readers.** A node's operands are spelled first, and the node over them is held to the nodes of its operation spelled before it, by the chain's readers alone ([`agree`]), and takes the first one's spelling. So an operation one reader reads and the reader above it takes whole is one atom of the reader above wherever its own reader holds it one, and no checker is asked what a node's operands are. The walk folds nothing but a choice a fold made by its atoms' order: a magnitude is spelled by its own fold over its spelled operand ([`Operation::oriented`]), since which monomial leads it is the order of its operand's atoms, and the representatives may stand in another. It is Shostak's combined canonizer, the theory's canonizer taken once a term's subterms are replaced by their representatives (Rueß and Shankar, [*Combining Shostak Theories*](https://www.csl.sri.com/people/shankar/rta02.pdf)), for the operations whose canonizer is their fold. Any other fold over operands spelled alike is a reducer's to take again, where it takes one ([`refold`]).
//!
//! **A node is read against the nodes that stand over its atoms, and no others.** Holding a node asks a reading against each earlier node of its operation, and a term holds many of one: a long sum of operations would be read pair by pair, at every fold above it. The readers that pair operands hold two operations one where their operands pair off, and the linear ones where their normal forms agree, each over one set of atoms, so the earlier nodes are kept by the atoms each stands over (`bucket`) and two nodes over two sets are told apart with nothing read. A cost filter failing toward refusal, as the one on two atoms' heads is.
//!
//! **A node the table leaves an operand out of is one with another of its kind where the operands it reads are.** A quotient carries a proof its meaning does not read, and its value is a function of its dividend and its divisor: two quotients of one dividend by one divisor are one, whatever their proofs, with no checker asked. Such a node is handed out as an atom once the operands its meaning reads are walked, no checker is asked whether two of them convert, and a walk that spells it holds it to the first node of its kind spelled over the same operands ([`Classes`]'s `unread`).
//!
//! **A node is forced before it is read, as the truth table forces it.** A stuck connective keeps its right operand as written, and an operand written as a call — a negation, an operator still spelled through its witness — is an operation only once it is reduced, so each node is reduced as a [`Probe`] before the walk asks the table what it is. An atom is therefore handed out in weak-head form, which is what lets its head say whether it may be another's.
//!
//! **A stuck operation is classified once, from what its operands' classifications found.** A reducer folds a spine from its leaves and takes each stuck fold again ([`atoms_within`], [`refold`]), so the operation at each level stands over operations classified a moment before. Each classification is the walk's state once the operation's operands are walked ([`Classification`]), which a reducer files beside the operation and hands back to the operation above it. The walk meets an operand before anything after it, in its own order, so a first operand's classification is exactly where the walk of the operation above stands once that operand is walked; every later operand is walked against it, a node walked already met by its identity and not entered; and where the operands before a classification are small beside it, they are walked alone and placed before it, each key they share decided as the walk would decide it, and where one would move, the walk starts from the first operand instead. So what a fold above a spine walks is what is new at it, and what it decides is what one walk of the whole would decide.
//!
//! **Probe-side only.** A term replaced by its reduct, or by one it converts with, denotes what it did, so a verdict on the classed pair is one on the pair as written. Nothing is written back: a reading that still decides nothing leaves its congruence the spelling it was handed.

#[cfg(test)]
mod tests;

#[cfg(test)]
mod extension_tests;

#[cfg(test)]
mod test_support;

use {
    crate::{Outcome, Read, read},
    curios_algebra::{Carrier, Family, Operation, declares},
    curios_core::{
        Cost, Declaration, Free, Intrinsic, Probe, ReduceError, Reducer, Subterm, Term, Var, Visit,
        reduce_intrinsic,
    },
    curios_utilities::{Ordered, Persistent, recurse},
    std::{
        collections::HashMap,
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

/// The nodes a walk has met, by identity and by whether the node above holds them, beside what each was keyed and spelled as: the terms kept alive in the answers, since an identity is an address.
type Walked = Persistent<(usize, bool), (Term, Term)>;

/// What an atom can be one with, read off its head once forced: the one statement of which two atoms a checker is asked about.
///
/// A variable converts with nothing but itself, and a metavariable with nothing a comparison that solves nothing can say, so neither has a key. Two applications may be one where they are of one head, up to its universe instances, to as many arguments; two stuck operations where they are of one kind, a quotient never, its value read by its operands ([`Classes`]'s `unread`); and a stuck operation with nothing else. Any other two — a call beside a stuck `match`, which a folded recursive call and its own unfolding are — may be one. A cost filter failing toward refusal: a pair it skips stays two atoms.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Pairing {
    /// An application, by its head up to universe instances and its arity.
    Applied(Term, usize),
    /// An intrinsic whose meaning reads every operand it has, by its kind.
    Intrinsic(mem::Discriminant<Intrinsic>),
    /// Any other atom that is no variable or metavariable.
    Other,
}

impl Pairing {
    /// `atom`'s key, where it has one.
    fn of(atom: &Term) -> Option<Self> {
        match &**atom {
            Subterm::Var(_) | Subterm::Metavar(_) => None,
            Subterm::Apply(apply) => Some(Pairing::Applied(
                apply.head.erased_universes(),
                apply.arguments.len(),
            )),
            Subterm::Intrinsic(intrinsic) => (!leaves_one_out(intrinsic))
                .then(|| Pairing::Intrinsic(mem::discriminant(intrinsic))),
            _ => Some(Pairing::Other),
        }
    }

    /// What an atom of this key may be one with besides an atom of its own key: an application with any atom of no other kind, and an atom of no other kind with any application.
    fn beside(&self) -> Beside {
        match self {
            Pairing::Applied(..) => Beside::Others,
            Pairing::Other => Beside::Applications,
            Pairing::Intrinsic(_) => Beside::Nothing,
        }
    }
}

/// The atoms of other keys an atom may be one with ([`Pairing::beside`]).
enum Beside {
    Nothing,
    /// Every atom of no other kind.
    Others,
    /// Every application, whatever its head.
    Applications,
}

/// How many atoms of each key a classification holds: what says whether a new atom could be one with any of them.
#[derive(Debug, Clone, Default)]
struct Census {
    keys: Persistent<Pairing, usize>,
    applied: usize,
    other: usize,
}

impl Census {
    fn count(&mut self, key: &Pairing) {
        let held = self.keys.get(key).copied().unwrap_or(0);
        self.keys.insert(key.clone(), held + 1);
        match key {
            Pairing::Applied(..) => self.applied += 1,
            Pairing::Other => self.other += 1,
            Pairing::Intrinsic(_) => {}
        }
    }

    /// Whether some atom counted may be one with an atom of `key`.
    fn meets(&self, key: &Pairing) -> bool {
        self.keys.contains_key(key)
            || match key.beside() {
                Beside::Nothing => false,
                Beside::Others => self.other > 0,
                Beside::Applications => self.applied > 0,
            }
    }
}

/// Which terms are one: each opaque atom that converts with an earlier one under the earlier one, its class's representative, and each node the readers hold one with an earlier node of its operation under that node's spelling.
///
/// **What a walk met first is placed first.** A representative and a node held are kept at the place they were opened at, so an atom is put to the representatives it can be one with in the order the walk met them, and a node read against its bucket in that order; a classification placed before another takes places below the other's least.
#[derive(Debug, Clone, Default)]
pub struct Classes {
    representatives: Persistent<Term, Term>,
    /// Each class's representative, at the place it was opened at.
    opened: Persistent<Term, i64>,
    /// The representatives of each pairing key, by place.
    keyed: Persistent<Pairing, Ordered<Term>>,
    /// The representatives that are applications, by place: what an atom of no other kind may be one with, beside its own kind.
    applications: Ordered<Term>,
    /// Every atom classed, by its key.
    census: Census,
    /// Whether some pair was put to the checker, so that the classes rest on what its conversion answered.
    asked: bool,
    /// The nodes held so far, each as it is spelled, by what a later node shares with the ones it is read against ([`bucket`]), by place.
    nodes: Persistent<Bucket, Ordered<Term>>,
    /// Every node met, as it was spelled over its operands, under the node it is held to — itself where it opened a class.
    held: Persistent<Term, Term>,
    /// How an atom that stands for its own class is spelled, where that is not as itself: a node its checker classes, over its spelled operands. Kept with the classes, so a reader that reads a term at a time spells a representative as it did where it met it.
    spelled: Persistent<Term, Term>,
    /// The atoms each spelled term met stands over, so a node's are its operands' joined and a term two nodes share is read once.
    over: Persistent<Term, Over>,
    /// Each node the table leaves an operand out of, as first spelled, by its operation and the operands its meaning reads as they are spelled, universe instances erased: a later one over the same operands is spelled as it.
    unread: Persistent<(Kind, Vec<Term>), Term>,
    /// The place the next class or node takes.
    next: i64,
    /// The least place taken.
    least: i64,
}

impl Classes {
    /// `atoms` classed by `same`, in the order they were handed out: an atom joins the first class whose representative `same` says it converts with, and otherwise opens one.
    ///
    /// **A pair is asked only where it may be one**, read off the two atoms' keys (`Pairing`). It is a cost filter failing toward refusal: a pair it skips stays two atoms, as every pair was before any was classed.
    ///
    /// **Two applications apart in their heads' universe instances are asked about by their arguments.** Each written call of a universe-polymorphic definition takes a level of its own, so `size(f(a + b))` written twice is two heads, and a checker handed the two whole compares the levels first, which no value of the two reads: an atom the walks hand out is a value of a carrier that holds no type, an operand of an operation the table declares at one or a term its reader reads at one, and a universe instance is no part of such a value ([`curios_core::same_atom`]). So `same` is asked of each pair of arguments in turn, the heads being one head already, and the two are one where every pair is.
    ///
    /// **A classing made inside another's question is bounded as every comparison is.** `same` is the checker's conversion, asked inside the pairs of the goal the atoms belong to, so two calls of a recursive function met one unfolding on are unfolded against each other once and are apart met again (`documentation/design/soundness/conversion-recurrence.md`). Nothing here limits what is asked, so what `same` answers of two atoms is a function of the two.
    pub fn of<E>(
        atoms: &[Term],
        same: impl FnMut(&Term, &Term) -> Result<bool, E>,
    ) -> Result<Self, E> {
        let mut classes = Classes::default();
        classes.join(atoms, same)?;
        Ok(classes)
    }

    /// `atoms` classed among the classes already made, by the rule [`Classes::of`] states: what a reader asks that meets its atoms a term at a time and keeps its classes from one term to the next, so an atom is in one class wherever it was read. An atom already classed is asked about no more.
    pub fn join<E>(
        &mut self,
        atoms: &[Term],
        mut same: impl FnMut(&Term, &Term) -> Result<bool, E>,
    ) -> Result<(), E> {
        for atom in atoms {
            if self.representatives.contains_key(atom) || self.opened.contains_key(atom) {
                continue;
            }
            let key = Pairing::of(atom);
            let mut joined = None;
            if let Some(key) = &key {
                for representative in self.candidates(key) {
                    self.asked = true;
                    if one(&representative, atom, &mut same)? {
                        joined = Some(representative);
                        break;
                    }
                }
                self.census.count(key);
            }
            match joined {
                Some(representative) => {
                    self.representatives.insert(atom.clone(), representative);
                }
                None => {
                    let place = self.place();
                    self.open(atom, key.as_ref(), place);
                }
            }
        }
        Ok(())
    }

    /// The representatives an atom of `key` may be one with, in the order they were opened.
    fn candidates(&self, key: &Pairing) -> Vec<Term> {
        let own = self.keyed.get(key);
        let beside = match key.beside() {
            Beside::Nothing => None,
            Beside::Others => self.keyed.get(&Pairing::Other),
            Beside::Applications => Some(&self.applications),
        };
        let mut own = own.into_iter().flat_map(|keyed| keyed.iter()).peekable();
        let mut beside = beside
            .into_iter()
            .flat_map(|beside| beside.iter())
            .peekable();
        let mut candidates = Vec::new();
        loop {
            let take_own = match (own.peek(), beside.peek()) {
                (None, None) => return candidates,
                (Some(_), None) => true,
                (None, Some(_)) => false,
                (Some((at, _)), Some((beside_at, _))) => at < beside_at,
            };
            let next = match take_own {
                true => own.next(),
                false => beside.next(),
            };
            candidates.extend(next.map(|(_, representative)| representative.clone()));
        }
    }

    /// The place the next class or node takes.
    fn place(&mut self) -> i64 {
        let place = self.next;
        self.next += 1;
        place
    }

    /// `atom` opened as a class of its own, at `place`.
    fn open(&mut self, atom: &Term, key: Option<&Pairing>, place: i64) {
        self.opened.insert(atom.clone(), place);
        if let Some(key) = key {
            let mut keyed = self.keyed.get(key).cloned().unwrap_or_default();
            keyed.insert(place, atom.clone());
            self.keyed.insert(key.clone(), keyed);
            if matches!(key, Pairing::Applied(..)) {
                self.applications.insert(place, atom.clone());
            }
        }
    }

    /// `atom`, a representative, taken out of the place it was opened at.
    fn close(&mut self, atom: &Term, key: Option<&Pairing>, place: i64) {
        if let Some(key) = key {
            if let Some(mut keyed) = self.keyed.get(key).cloned() {
                keyed.remove(place);
                self.keyed.insert(key.clone(), keyed);
            }
            if matches!(key, Pairing::Applied(..)) {
                self.applications.remove(place);
            }
        }
        self.opened.remove(atom);
    }

    /// These classes with no node held: the atoms' classes alone.
    fn without_nodes(&self) -> Self {
        Self {
            nodes: Persistent::new(),
            held: Persistent::new(),
            spelled: Persistent::new(),
            over: Persistent::new(),
            unread: Persistent::new(),
            ..self.clone()
        }
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

/// What classifying a stuck operation found: the walk's state once the operation's operands are walked, which a reducer files beside the operation for the operation above it to extend ([`atoms_within`], [`refold`]).
///
/// **It lives as long as what it read.** A classification no conversion class entered reads the operation alone — the walk forces nothing and the readers reduce through nothing — and one taken over classes its checker decided reads what that conversion read ([`Classification::classed`]).
#[derive(Debug, Clone)]
pub struct Classification {
    classes: Classes,
    /// The atoms handed out.
    handed: Persistent<Term, ()>,
    walked: Walked,
    /// The operation as the walk spelled its operands.
    spelled: Term,
}

impl Classification {
    /// Whether some pair was put to the checker's conversion, so that the classification rests on what that conversion read.
    pub fn classed(&self) -> bool {
        self.classes.asked
    }

    /// How much the walk met: what a seed is chosen by.
    fn size(&self) -> usize {
        self.walked.len()
    }
}

/// The operands of `operation` a walk reads, in order: what a reducer looks a filed [`Classification`] up for before it hands them to [`atoms_within`] and [`refold`]. Empty where the table declares it no operation.
pub fn read_operands(operation: &Intrinsic) -> Vec<Term> {
    match layer(operation) {
        Some((_, read)) => children(operation).into_iter().take(read).collect(),
        None => Vec::new(),
    }
}

/// Where the walk of a stuck operation's operands starts.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Seed {
    /// From nothing, every operand walked.
    Nothing,
    /// From the first operand's classification.
    First,
    /// From the classification of the operand at this index, the operands before it walked alone and placed before it.
    Prepended(usize),
}

/// A stuck operation's atoms as [`atoms_within`] hands them out: the classes its operands' classifications kept, and the atoms met that none of them classed, in the order met, for the checker to class.
#[derive(Debug)]
pub struct Within {
    classes: Classes,
    handed: Persistent<Term, ()>,
    pending: Vec<Term>,
    seed: Seed,
}

impl Within {
    /// Whether some atom still to class may be one with an atom classed or with another still to class, so that classing them puts a pair to the checker.
    pub fn classable(&self) -> bool {
        let mut pending = Census::default();
        self.pending.iter().filter_map(Pairing::of).any(|key| {
            let meets = self.classes.census.meets(&key) || pending.meets(&key);
            pending.count(&key);
            meets
        })
    }

    /// How many atoms are still to class.
    pub fn atoms(&self) -> usize {
        self.pending.len()
    }

    /// The atoms still to class, classed by `same` among the classes kept ([`Classes::join`]).
    pub fn class<E>(&mut self, same: impl FnMut(&Term, &Term) -> Result<bool, E>) -> Result<(), E> {
        let pending = mem::take(&mut self.pending);
        self.classes.join(&pending, same)
    }

    /// The atoms still to class, each opened as a class of its own: what classing them comes to where none may be one with another ([`Within::classable`]).
    pub fn open(&mut self) {
        let pending = mem::take(&mut self.pending);
        let Ok(()) = self
            .classes
            .join(&pending, |_, _| Ok::<_, std::convert::Infallible>(false));
    }
}

/// The opaque atoms under one stuck operation, each once, in the order a walk of its operands meets them, beside the classes `kept` holds — one classification or none for each of [`read_operands`]: empty where the table declares it no operation.
///
/// **The operation and its operands are taken as they stand.** It is the reduct a reducer has just reached, so forcing it would ask that reducer for the term it is in the middle of reducing, and its operands are as the fold left them (`Standing`). A node the fold left as written — a connective's right operand behind a stuck left — is an atom here as it is spelled.
///
/// **What a classification kept is not walked again** ([`Classification`]): the atoms it handed out are classed already, and only the atoms met past it are handed out, to be classed by the checker ([`Within::class`]).
pub fn atoms_within(
    reducer: &mut impl Reducer,
    operation: &Intrinsic,
    kept: &[Option<Rc<Classification>>],
) -> Result<Within, ReduceError> {
    let empty = || Within {
        classes: Classes::default(),
        handed: Persistent::new(),
        pending: Vec::new(),
        seed: Seed::Nothing,
    };
    if layer(operation).is_none() {
        return Ok(empty());
    }
    let operands = read_operands(operation);
    let mut standing = Standing(reducer);

    let mut seed = plan(&operands, kept);
    let mut start = None;
    if let Seed::Prepended(at) = seed {
        let mut prefix = Vec::new();
        let mut walk = Walk::new(&mut standing, Reading::Collected(&mut prefix));
        for operand in &operands[..at] {
            walk.spine(operand, Some(operation))?;
        }
        let visited = walk.walked;
        let base = kept_at(kept, at).expect("a seed is a classification kept");
        match prepended_atoms(&prefix, base) {
            Some((classes, handed)) => {
                let mut walked = base.walked.clone();
                for (identity, done) in visited.iter() {
                    if !walked.contains_key(identity) {
                        walked.insert(*identity, done.clone());
                    }
                }
                start = Some((classes, handed, walked, at));
            }
            None => seed = fallen(kept),
        }
    }
    if start.is_none()
        && seed == Seed::First
        && let Some(first) = kept_at(kept, 0)
    {
        start = Some((
            first.classes.clone(),
            first.handed.clone(),
            first.walked.clone(),
            0,
        ));
    }

    let mut pending = Vec::new();
    let (classes, handed) = match start {
        Some((classes, handed, walked, at)) => {
            let mut walk = Walk::seeded(
                &mut standing,
                Reading::Collected(&mut pending),
                walked,
                handed,
            );
            walk.seeded_node(
                &operands[at],
                &kept_at(kept, at).expect("a seed").spelled,
                operation,
            )?;
            for operand in &operands[at + 1..] {
                walk.spine(operand, Some(operation))?;
            }
            (classes, walk.handed)
        }
        None => {
            seed = Seed::Nothing;
            let mut walk = Walk::new(&mut standing, Reading::Collected(&mut pending));
            for operand in &operands {
                walk.spine(operand, Some(operation))?;
            }
            (Classes::default(), walk.handed)
        }
    };

    Ok(Within {
        classes,
        handed,
        pending,
        seed,
    })
}

/// A stuck `operation` folded once more over its operands as `within` classes them, beside what its classification found for the operation above it to extend: the fold of the classed operation where it is no longer that operation — a literal, an operand, another operation — and `None` where no operand moved or the fold still stands at the operation; no classification where the table declares it no operation.
///
/// **A fold pairs atoms by their identity, and which terms are one is conversion's to say.** `f(a + b) == f(b + a)` is stuck to the fold and `true` to conversion, as `Nat/and(x, y) == Nat/and(y, x)` is, so a `match` on either would not reduce where conversion holds its two sides one term. The checker classes the operation's opaque atoms ([`atoms_within`], [`Within::class`]), the readers its nodes, and the fold is taken again over one spelling to a class.
///
/// **A fold that decides no more keeps the spelling it had.** A representative is picked by the order a walk met the terms in, which is no property of the term, so a classed operation that is still stuck at its operation is dropped and the operation stands as it was written: nothing respelled is written back. What is kept is a term the fold built out of the operation's own operands, as it builds one for two operands spelled alike.
///
/// **What it rebuilds is charged, and what a classification kept is not walked again.** A walk charges the nodes it rebuilt, and a node a kept classification spelled was charged where it was spelled.
pub fn refold(
    reducer: &mut impl Reducer,
    operation: &Intrinsic,
    within: Within,
    kept: &[Option<Rc<Classification>>],
) -> Result<(Option<Term>, Option<Classification>), ReduceError> {
    let Some((_, read)) = layer(operation) else {
        return Ok((None, None));
    };
    let term = Term::intrinsic(operation.clone());
    let operands = read_operands(operation);
    let Within {
        mut classes,
        handed,
        seed,
        ..
    } = within;
    let mut standing = Standing(&mut *reducer);

    let mut results = Vec::with_capacity(operands.len());
    let mut built = 0;
    let mut walked = Persistent::new();
    let mut from = 0;
    match seed {
        Seed::Prepended(at) => {
            let base = kept_at(kept, at).expect("a seed is a classification kept");
            let mut prefix = Classes::default();
            let mut walk = Walk::new(&mut standing, Reading::Spelled(&mut prefix));
            let mut prefixed = Vec::with_capacity(at);
            for operand in &operands[..at] {
                prefixed.push(walk.spine(operand, Some(operation))?);
            }
            let (visited, prefix_built) = (walk.walked, walk.built);
            match prepended_nodes(&mut standing, &prefix, &visited, &classes, &base.walked)? {
                Some((merged, merged_walked)) => {
                    classes = merged;
                    walked = merged_walked;
                    built = prefix_built;
                    results = prefixed;
                    let mut walk = Walk::seeded(
                        &mut standing,
                        Reading::Spelled(&mut classes),
                        walked,
                        handed.clone(),
                    );
                    results.push(walk.seeded_node(&operands[at], &base.spelled, operation)?);
                    walked = walk.walked;
                    built += walk.built;
                    from = at + 1;
                }
                None => classes = classes.without_nodes(),
            }
        }
        Seed::First => {
            let first = kept_at(kept, 0).expect("a seed is a classification kept");
            let mut walk = Walk::seeded(
                &mut standing,
                Reading::Spelled(&mut classes),
                first.walked.clone(),
                handed.clone(),
            );
            results.push(walk.seeded_node(&operands[0], &first.spelled, operation)?);
            walked = walk.walked;
            built = walk.built;
            from = 1;
        }
        Seed::Nothing => {}
    }

    let mut walk = Walk::seeded(
        &mut standing,
        Reading::Spelled(&mut classes),
        walked,
        handed.clone(),
    );
    for operand in &operands[from..] {
        results.push(walk.spine(operand, Some(operation))?);
    }
    let (_, respelled, _) = walk.assemble(&term, operation, read, results);
    built += walk.built;
    let walked = walk.walked;
    reducer.spend(Cost::term(built))?;

    let classification = Classification {
        classes,
        handed,
        walked,
        spelled: respelled.clone(),
    };
    let Subterm::Intrinsic(respelled) = &*respelled else {
        return Ok((None, Some(classification)));
    };
    if respelled == operation {
        return Ok((None, Some(classification)));
    }
    Ok((collapsed(reducer, respelled)?, Some(classification)))
}

/// The classification `kept` holds for the operand at `at`.
fn kept_at(kept: &[Option<Rc<Classification>>], at: usize) -> Option<&Classification> {
    kept.get(at).and_then(Option::as_deref)
}

/// Where the walk of `operands` starts, given what `kept` holds for each: from the largest classification where the operands before it are small beside it, from the first operand's where it has one, and from nothing otherwise.
///
/// **Smaller beside larger.** Walking an operand against what stands costs what it holds that the state does not, and placing what stands before a classification costs what stands, so a walk seeded from the larger side costs the smaller: a spine of operations each a summand longer than the one before costs a summand a level, and so does a difference whose subtrahend is a spine.
fn plan(operands: &[Term], kept: &[Option<Rc<Classification>>]) -> Seed {
    let largest = (0..operands.len())
        .filter_map(|at| kept_at(kept, at).map(|kept| (at, kept.size())))
        .fold(
            None,
            |largest: Option<(usize, usize)>, (at, size)| match largest {
                Some((_, most)) if most >= size => largest,
                _ => Some((at, size)),
            },
        );
    match largest {
        None => Seed::Nothing,
        Some((0, _)) => Seed::First,
        Some((at, size)) => {
            let before = operands[..at]
                .iter()
                .enumerate()
                .map(|(index, operand)| {
                    kept_at(kept, index)
                        .map(Classification::size)
                        .or_else(|| few(operand))
                })
                .sum::<Option<usize>>();
            match before {
                Some(before) if 2 * before <= size => Seed::Prepended(at),
                _ => fallen(kept),
            }
        }
    }
}

/// Where the walk starts where no classification after the first seeds it.
fn fallen(kept: &[Option<Rc<Classification>>]) -> Seed {
    match kept_at(kept, 0) {
        Some(_) => Seed::First,
        None => Seed::Nothing,
    }
}

/// What walking `operand` costs where no classification is filed for it and that is a few nodes: an atom, which the walk does not enter, or a closed literal.
fn few(operand: &Term) -> Option<usize> {
    match &**operand {
        Subterm::Intrinsic(intrinsic) if layer(intrinsic).is_some() => operand
            .as_nat()
            .and_then(|value| value.to_natural())
            .map(|_| 2),
        _ => Some(1),
    }
}

/// `prefix`, the atoms a walk of the operands before `base`'s handed out, placed before `base`'s: `None` where an atom of `prefix` may be one with another of `prefix` or of `base`, which the checker would have to be asked, or is one of `base`'s that joined another's class there.
///
/// An atom both hold opens its class where `prefix` meets it, before every class `base` opened, as the walk of the whole would open it.
fn prepended_atoms(
    prefix: &[Term],
    base: &Classification,
) -> Option<(Classes, Persistent<Term, ()>)> {
    let mut classes = base.classes.clone();
    let mut handed = base.handed.clone();
    let mut own = Census::default();
    let first = classes.least - prefix.len() as i64;
    for (at, atom) in prefix.iter().enumerate() {
        let place = first + at as i64;
        let key = Pairing::of(atom);
        if let Some(key) = &key {
            if own.meets(key) {
                return None;
            }
            own.count(key);
        }
        match handed.contains_key(atom) {
            true => {
                let opened = *classes.opened.get(atom)?;
                classes.close(atom, key.as_ref(), opened);
                classes.open(atom, key.as_ref(), place);
            }
            false => {
                if let Some(key) = &key {
                    if classes.census.meets(key) {
                        return None;
                    }
                    classes.census.count(key);
                }
                handed.insert(atom.clone(), ());
                classes.open(atom, key.as_ref(), place);
            }
        }
    }
    classes.least = classes.least.min(first);
    Some((classes, handed))
}

/// `prefix`'s nodes, as a walk of the operands before a seed's spelled them alone, placed before the seed's, which `classes` and `walked` hold: `None` where a node of the seed's would be held to one of `prefix`'s, or where a term both met was answered two ways.
///
/// A node both hold is placed where `prefix` met it, before every node the seed's walk placed, as the walk of the whole would place it.
fn prepended_nodes(
    reducer: &mut impl Reducer,
    prefix: &Classes,
    visited: &Walked,
    classes: &Classes,
    walked: &Walked,
) -> Result<Option<(Classes, Walked)>, ReduceError> {
    let mut classes = classes.clone();
    let mut walked = walked.clone();
    if !placed_alike(&mut walked, visited)
        || !placed_alike(&mut classes.held, &prefix.held)
        || !placed_alike(&mut classes.spelled, &prefix.spelled)
        || !placed_alike(&mut classes.unread, &prefix.unread)
    {
        return Ok(None);
    }
    for (term, over) in prefix.over.iter() {
        if !classes.over.contains_key(term) {
            classes.over.insert(term.clone(), over.clone());
        }
    }
    let offset = classes.least - prefix.next;
    for (bucket, earlier) in prefix.nodes.iter() {
        let mut later = classes.nodes.get(bucket).cloned().unwrap_or_default();
        for (_, node) in later.iter() {
            if earlier.iter().any(|(_, first)| first == node) {
                continue;
            }
            for (_, first) in earlier.iter() {
                if agree(reducer, first, node)? {
                    return Ok(None);
                }
            }
        }
        for (place, node) in earlier.iter() {
            let standing = later
                .iter()
                .find(|(_, held)| *held == node)
                .map(|(standing, _)| standing);
            if let Some(standing) = standing {
                later.remove(standing);
            }
            later.insert(place + offset, node.clone());
        }
        classes.nodes.insert(*bucket, later);
    }
    classes.least = classes.least.min(prefix.least + offset);
    Ok(Some((classes, walked)))
}

/// `into` with `from`'s entries added, `false` where the two answer one key two ways.
fn placed_alike<K: Hash + Eq + Clone, V: PartialEq + Clone>(
    into: &mut Persistent<K, V>,
    from: &Persistent<K, V>,
) -> bool {
    for (key, value) in from.iter() {
        match into.get(key) {
            Some(standing) if standing != value => return false,
            Some(_) => {}
            None => into.insert(key.clone(), value.clone()),
        }
    }
    true
}

/// Whether some two of `atoms` may be one, so classing them could change what the readers read.
pub fn classable(atoms: &[Term]) -> bool {
    let mut census = Census::default();
    atoms.iter().filter_map(Pairing::of).any(|key| {
        let meets = census.meets(&key);
        census.count(&key);
        meets
    })
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

/// Whether the table declares `intrinsic` an operation and leaves an operand of it out: a quotient's proof, which its meaning does not read.
fn leaves_one_out(intrinsic: &Intrinsic) -> bool {
    layer(intrinsic).is_some_and(|(_, read)| children(intrinsic).len() > read)
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
fn bucket(over: &mut Persistent<Term, Over>, kind: Kind, node: &Term) -> Bucket {
    let atoms = match kind.0 {
        Carrier::Boolean => 0,
        _ => hashed(&*atoms_under(over, node)),
    };
    (kind, atoms)
}

/// The opaque atoms `term` stands over as it is spelled: its operands' where the walk reads into it, and itself otherwise, known by its spelling with its universe instances erased, which is what a reader of a carrier that holds no type knows an atom by.
fn atoms_under(over: &mut Persistent<Term, Over>, term: &Term) -> Over {
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

/// A reducer that takes every term as reduced already, for a walk over a stuck operation a reducer has just left: its operands are as the fold reduced them, and forcing them again is a lookup at least, and a reduction where the memo has none, at every operand of every stuck fold. What the walk builds is still charged to the reducer this one stands before.
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
    walked: Walked,
    /// The atoms handed out so far, so each is handed out once however many a term holds.
    handed: Persistent<Term, ()>,
    /// The size of what the walk rebuilt, for its caller to charge.
    built: u64,
}

impl<'r, 'c, R: Reducer> Walk<'r, 'c, R> {
    fn new(reducer: &'r mut R, reading: Reading<'c>) -> Self {
        Self::seeded(reducer, reading, Persistent::new(), Persistent::new())
    }

    /// A walk that stands where another left off: the nodes it walked and the atoms it handed out.
    fn seeded(
        reducer: &'r mut R,
        reading: Reading<'c>,
        walked: Walked,
        handed: Persistent<Term, ()>,
    ) -> Self {
        Walk {
            reducer,
            reading,
            forced: HashMap::new(),
            walked,
            handed,
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

    /// `operand`, an operation whose operands a kept classification walked and spelled `spelled` over, standing under `parent`: its node step taken where the walk stands, as [`Walk::spine`] takes it over operands it walked itself.
    fn seeded_node(
        &mut self,
        operand: &Term,
        spelled: &Term,
        parent: &Intrinsic,
    ) -> Result<(Term, Term), ReduceError> {
        let Subterm::Intrinsic(intrinsic) = &**operand else {
            return self.spine(operand, Some(parent));
        };
        let Some((kind, read)) = layer(intrinsic) else {
            return self.spine(operand, Some(parent));
        };
        let held = held_under(Some(parent), kind);
        if let Some(done) = self.walked.get(&(operand.identity(), held)) {
            return Ok(done.clone());
        }
        let whole = children(intrinsic).len() <= read;
        let done = self.finish(
            operand,
            (kind, read),
            held,
            operand.clone(),
            spelled.clone(),
            whole,
        )?;
        self.walked.insert((operand.identity(), held), done.clone());
        Ok(done)
    }

    /// What is handed out under `key`, which this walk spelled `spelled` over its operands: collected, or spelled as its class's representative is.
    fn atom(&mut self, key: Term, spelled: Term) -> (Term, Term) {
        match &mut self.reading {
            Reading::Collected(atoms) => {
                if !self.handed.contains_key(&key) {
                    self.handed.insert(key.clone(), ());
                    atoms.push(key.clone());
                }
                (key.clone(), key)
            }
            Reading::Spelled(classes) => {
                // A representative is met before what joined it, by the walk that handed the atoms out and by every walk that spells them, each over the same terms in the same order: so its spelling is kept by the time it is asked for.
                let class = classes
                    .representatives
                    .get(&key)
                    .cloned()
                    .unwrap_or_else(|| key.clone());
                if let Some(kept) = classes.spelled.get(&class) {
                    return (key, kept.clone());
                }
                if class != key {
                    return (key, class);
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
        layer: (Kind, usize),
        held: bool,
    ) -> Result<(Term, Term), ReduceError> {
        let (key, spelled, whole) = self.operands(forced, intrinsic, layer.1)?;
        self.finish(forced, layer, held, key, spelled, whole)
    }

    /// A node of `kind` whose operands are spelled `spelled` and keyed `key`, among the earlier nodes of its operation.
    fn finish(
        &mut self,
        forced: &Term,
        (kind, read): (Kind, usize),
        held: bool,
        key: Term,
        spelled: Term,
        whole: bool,
    ) -> Result<(Term, Term), ReduceError> {
        if !whole {
            // An operand its meaning does not read stands as written, and the node is one with the first of its kind over operands spelled as its are: its value reads no other.
            let spelled = self.unread(spelled, kind, read);
            return Ok(self.atom(key, spelled));
        }
        if matches!(self.reading, Reading::Collected(_)) {
            return Ok((key.clone(), key));
        }
        let spelled = match kind.1.oriented() && spelled != *forced {
            true => self.oriented(spelled)?,
            false => spelled,
        };
        // A magnitude whose fold drew a negation out of it is a node of another operation, which the reader above reads.
        let still = matches!(
            &*spelled,
            Subterm::Intrinsic(spelled) if layer(spelled).is_some_and(|(of, _)| of == kind)
        );
        let spelled = match held && still {
            true => self.held(spelled, kind)?,
            false => spelled,
        };
        Ok((key, spelled))
    }

    /// `node`, a magnitude spelled over operands that moved, as its own fold spells it: which monomial of its operand leads is the order of that operand's atoms, so the fold is taken again over the atoms as they are now spelled, and two spellings of one class are led alike.
    fn oriented(&mut self, node: Term) -> Result<Term, ReduceError> {
        let Subterm::Intrinsic(intrinsic) = &*node else {
            return Ok(node);
        };
        Ok(reduce_intrinsic(self.reducer, intrinsic)?.into())
    }

    /// `node`, of `kind`, a node the table leaves an operand out of, spelled as the first of its kind whose first `read` operands are spelled as its are, and itself where there is none; as it is where the walk spells nothing.
    fn unread(&mut self, node: Term, kind: Kind, read: usize) -> Term {
        let Reading::Spelled(classes) = &mut self.reading else {
            return node;
        };
        let Subterm::Intrinsic(intrinsic) = &*node else {
            return node;
        };
        let operands = children(intrinsic)
            .iter()
            .take(read)
            .map(Term::erased_universes)
            .collect();
        let key = (kind, operands);
        match classes.unread.get(&key) {
            Some(first) => first.clone(),
            None => {
                classes.unread.insert(key, node.clone());
                node
            }
        }
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
        let mut earlier = classes.nodes.get(&bucket).cloned().unwrap_or_default();
        // What holding a node asks: a reading against each earlier node of its bucket, until one agrees.
        curios_profile::sample!("classify::held", earlier.len());
        let mut first = None;
        for (_, earlier) in earlier.iter() {
            if agree(self.reducer, earlier, &node)? {
                first = Some(earlier.clone());
                break;
            }
        }
        let first = match first {
            Some(first) => first,
            None => {
                let place = classes.place();
                earlier.insert(place, node.clone());
                classes.nodes.insert(bucket, earlier);
                node.clone()
            }
        };
        classes.held.insert(node, first.clone());
        Ok(first)
    }

    /// `intrinsic` with each of its first `read` operands walked and the rest as they stand, as its key and as the reading spells it, and whether `read` was every operand it has.
    fn operands(
        &mut self,
        term: &Term,
        intrinsic: &Intrinsic,
        read: usize,
    ) -> Result<(Term, Term, bool), ReduceError> {
        let mut results = Vec::with_capacity(read);
        for operand in children(intrinsic).iter().take(read) {
            results.push(self.spine(operand, Some(intrinsic))?);
        }
        Ok(self.assemble(term, intrinsic, read, results))
    }

    /// `intrinsic` rebuilt over `results`, the key and spelling of each of its first `read` operands, the rest as they stand: as its key and as the reading spells it, and whether `read` was every operand it has. The operands are taken out by one traversal ([`children`]) and put back by another.
    fn assemble(
        &mut self,
        term: &Term,
        intrinsic: &Intrinsic,
        read: usize,
        results: Vec<(Term, Term)>,
    ) -> (Term, Term, bool) {
        let children = children(intrinsic);
        let whole = children.len() <= read;

        let mut results = results.into_iter();
        let mut keys = Vec::new();
        let mut operands = Vec::new();
        let (mut keyed, mut changed) = (false, false);
        for (index, operand) in children.into_iter().enumerate() {
            let (key, walked) = match index < read {
                true => results.next().expect("a result for every operand read"),
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
            return (key, term.clone(), whole);
        }
        self.built += 1 + operands.len() as u64;
        (key, rebuild(operands), whole)
    }
}
