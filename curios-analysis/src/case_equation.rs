//! What both checkers share about an arm's case equation and the scrutinee it is recorded under: which spellings of a scrutinee carry one, which value of a scrutinee is another case than an arm's, what an elimination reads a stuck scrutinee as, how far a dispatch is opened to resolve its spelling, which recorded equation a probe could reach, and which terms an equation answers.

#[cfg(test)]
mod tests;

use {
    crate::{
        Classes, Driver, IntrinsicDriver, Invert, Judge, atoms_in, atoms_of, classable, classed,
        classed_in, intrinsics_agree, invert_indices,
    },
    curios_algebra::{Carrier, Operation},
    curios_common::{SyntaxRegistry, recurse},
    curios_core::{
        Apply, Atom, Bound, Cases, Cost, Declaration, Free, Func, Global, Instance, Intrinsic,
        Keys, Nat, ReduceError, Reducer, Subterm, Telescope, Term, Variant, bool_value,
        is_bool_connective,
    },
    std::{collections::HashMap, mem, slice},
};

/// How many application layers a dispatched scrutinee is opened through to reach the spelling it resolves to: each step consumes one layer of an elaborated dispatch, and a spine that has not settled in this many is not a dispatch.
///
/// One number because the checkers answer the same occurrences at the same point only while they resolve the same spellings: the certifier's `resolved_spelling`, the elaborator's `spine_whnf` and its `open_until` all stop here.
pub const RESOLVED_SPELLING_LAYERS: usize = 16;

/// Whether an arm's case equation may be recorded under `spelling` — the scrutinee as the certifier is handed it, local definitions substituted, or a spelling a dispatch resolves it to: only where it mentions a local.
///
/// **The certifier's evaluation memos are why.** A local-free term's reduct is remembered for the whole declaration, so an equation about one would leave an entry resting on an equation the arm's exit retracts; a local-bearing term's entry is cleared with the equations in force. The rule costs nothing a program needs: a local-free scrutinee reduces to its case value rather than sticking, so an arm under one is either the arm reduction takes or dead.
///
/// **Both checkers call it, and the elaborator on the certifier's spelling.** An equation one checker records and the other does not reads one arm two ways, and the arm is always a dead one: a proof the elaborator accepted there, under a guard over a top-level name, a global's projection, a local definition of a closed term or a dispatch whose method ignores its locals, is one the certifier refuses.
pub fn records_case_equation(spelling: &Term) -> bool {
    spelling.has_local_free()
}

/// The tests a dispatch on `head` over `keys` is reached past at its catch-all, one a key: `head == key`, by its carrier's equality. Each is false wherever the catch-all is taken, at a literal no key names or, for a natural, at a successor floor past every key, so the catch-all is typed under each at `false`, as a lone guard's `false` arm is typed under its test.
///
/// **Both checkers read it.** A test one checker assumed and the other did not would read the catch-all two ways; each is recorded by [`records_case_equation`], as every equation is.
pub fn catch_all_tests<'c>(head: &'c Term, keys: &'c Keys) -> impl Iterator<Item = Term> + 'c {
    keys.literals().map(|(literal, _)| match keys {
        Keys::Nat(_) => Term::intrinsic(Intrinsic::NatEql(head.clone(), literal)),
        Keys::Int(_) => Term::intrinsic(Intrinsic::IntEql(head.clone(), literal)),
        // A byte's equality is its code's, as `/std/Byte`'s `==` reduces, the key's code the natural its literal folds to.
        Keys::Byte(_) => {
            let Subterm::Intrinsic(Intrinsic::Byte(key)) = &*literal else {
                unreachable!("a byte's key is a byte literal");
            };
            Term::intrinsic(Intrinsic::NatEql(
                Term::intrinsic(Intrinsic::ByteToNat(head.clone())),
                Term::intrinsic(Intrinsic::Nat(Nat::new(u32::from(*key)))),
            ))
        }
    })
}

/// Whether `scrutinee`, read as reduction leaves it, is a value of another case than `case`: a literal or a constructed value that the unifier excusing an absent arm ([`invert_indices`]) finds in a definite clash with `case`. An arm at `case` under such a scrutinee is one no reduction takes. `binders` are the case's own, which the unifier may solve; a case's value names each of them once, so the answer is the same where none is named.
///
/// **Both checkers read it, each over what its own reduction answers.** The certifier types no such arm. The elaborator types every arm a reader wrote, and words its refusal of one by this.
pub fn another_case<J: Judge>(
    judge: &mut J,
    scrutinee: &Term,
    case: &Term,
    binders: &[Free],
) -> Result<bool, J::Error> {
    if !stands_as_a_case(scrutinee) {
        return Ok(false);
    }

    Ok(matches!(
        invert_indices(
            judge,
            slice::from_ref(scrutinee),
            slice::from_ref(case),
            binders
        )?,
        Invert::Impossible
    ))
}

/// Whether `term` already is a value that names its case: a constructed value, or a literal of a carrier an elimination reads.
fn stands_as_a_case(term: &Term) -> bool {
    match &**term {
        Subterm::Variant(_) => true,
        Subterm::Intrinsic(Intrinsic::Bool(_) | Intrinsic::Bytes(..) | Intrinsic::List { .. }) => {
            true
        }
        Subterm::Intrinsic(Intrinsic::Nat(_)) => Nat::is_zero(&Nat::decompose(term).1),
        _ => false,
    }
}

/// The family and the constructor of the value `term` is as it is written: a constructed value's, or that of a call of a constructor's definition (`called_constructor`).
pub fn constructed<D: Driver>(driver: &D, term: &Term) -> Option<(Global, Atom)> {
    match &**term {
        Subterm::Variant(Variant { name, tag, .. }) => Some((*name, tag.clone())),
        _ => called_constructor(driver, term),
    }
}

/// The family and the constructor of the value a call builds where it hands every argument to a definition whose body is a lambda answering a constructed value, as a constructor's definition is ([`Made::constructors`](curios_core::Made::constructors)), which the call is by one substitution. The body is read where the call names it and not reduced, and the name is asked of nothing else: a name beside it, its family's among them, may be one whose declaration waits on the term being read.
fn called_constructor<D: Driver>(driver: &D, term: &Term) -> Option<(Global, Atom)> {
    let Subterm::Apply(Apply { head, arguments }) = &**term else {
        return None;
    };
    let Subterm::Instance(Instance { name, .. }) = &**head else {
        return None;
    };
    let Subterm::Func(Func { telescope }) = &**driver.body(name)? else {
        return None;
    };
    let mut taken = 0;
    let mut entry = telescope;
    while let Telescope::Cons(_, _, rest, ..) = entry {
        taken += 1;
        entry = rest.body();
    }
    let Telescope::Done(answer) = entry else {
        unreachable!("a telescope past its entries is done");
    };
    match &***answer {
        Subterm::Variant(Variant { name, tag, .. }) if taken == arguments.len() => {
            Some((*name, tag.clone()))
        }
        _ => None,
    }
}

/// What [`read_scrutinee`] came to.
pub enum ScrutineeRead {
    /// The value the elimination takes apart: a literal, or a successor.
    Value(Term),
    /// Nothing decided, which leaves the elimination where it stood.
    Silent,
    /// Nothing decided, and some two of the scrutinee's atoms may be one: the checker classes them and asks [`read_scrutinee_classed`].
    Atoms(Vec<Term>),
}

/// What the elimination `cases` reads its scrutinee as, `forced` being the scrutinee as reduction forced it, where the carriers' algebra decides more of it than the fold did: a `Bool` connective as the literal its decision diagram is ([`bool_value`]), and a structural `Nat` with no floor as the successor its products of two symbolic sums distribute to ([`Nat::normalize`]). Conversion holds the scrutinee equal to that value and is a congruence, so it holds the match equal to that value's arm; read here, reduction reaches the arm too.
///
/// **A value only where the elimination then takes an arm**, a literal or a successor, so no stuck form is respelled; and never a disequality, a diagram that is no constant saying nothing. A literal switch reads a literal, which no distribution of a symbolic product makes, and a match of any other kind reads nothing here.
///
/// **Both checkers call it, in both reductions**, since the diagram and the distribution ask no judgment, as a fold asks none. Where the diagram decides nothing and some two of the connective's atoms may be one, they are handed back for a judgment's reduction to class; where none may, the readers class the connective's nodes and it is read again, as [`connectives_agree`](crate::connectives_agree) reads a pair.
pub fn read_scrutinee(
    reducer: &mut impl Reducer,
    cases: &Cases,
    forced: &Term,
) -> Result<ScrutineeRead, ReduceError> {
    match cases {
        Cases::Bool { .. } if is_bool_connective(forced) => {
            curios_common::profile!("read_scrutinee");
            if let Some(value) = bool_value(reducer, forced)? {
                return Ok(ScrutineeRead::Value(Term::intrinsic(Intrinsic::Bool(
                    value,
                ))));
            }
            let atoms = atoms_in(reducer, forced)?;
            if classable(&atoms) {
                return Ok(ScrutineeRead::Atoms(atoms));
            }
            Ok(
                match read_scrutinee_classed(reducer, forced, &mut Classes::default())? {
                    Some(value) => ScrutineeRead::Value(value),
                    None => ScrutineeRead::Silent,
                },
            )
        }
        Cases::FreeMonoid {
            carrier: curios_core::Carrier::Nat { .. },
        } if Nat::decompose(forced).0.is_zero() && Nat::has_stuck_product(forced) => {
            curios_common::profile!("read_scrutinee");
            let distributed = Nat::normalize(reducer, forced.clone())?;
            Ok(match Nat::decompose(&distributed).0.is_zero() {
                true => ScrutineeRead::Silent,
                false => ScrutineeRead::Value(distributed),
            })
        }
        _ => Ok(ScrutineeRead::Silent),
    }
}

/// [`read_scrutinee`]'s connective read again as its partition spells it, each opaque atom as its class's representative and each node as the first node its readers hold it one with: the literal it is then, or `None`, which says nothing.
pub fn read_scrutinee_classed(
    reducer: &mut impl Reducer,
    forced: &Term,
    classes: &mut Classes,
) -> Result<Option<Term>, ReduceError> {
    let classed = classed_in(reducer, forced, classes)?;
    if classed == *forced {
        return Ok(None);
    }
    Ok(bool_value(reducer, &classed)?.map(|value| Term::intrinsic(Intrinsic::Bool(value))))
}

/// Whether an equation keyed on `key` could answer `candidate`: whether `candidate` could be a term conversion holds equal to a reduct of `key`, read as conversion reads it and reducing only what conversion would reduce to compare it. `proofs` is the binders `candidate` names that are themselves proofs. The reduction is the caller's, and plain.
///
/// Reduction substitutes only closed definition bodies and subterms of the term it is reducing, so it can introduce a *global* name and can drop a local, but can never introduce a local the term did not already mention. A reduct of the key names no binder the key does not.
///
/// **A binder counts where conversion reads it.** Conversion compares two stuck forms part by part, reducing each part before it compares it, so a binder one names that its reduction erases is one it never reads — `f(b + 0 * c)` is `f(b)` to it, and so are `f(Pair { left = b, right = c }.left)` and `ft(Box(b + 0 * c))` — and it reads no proof: `w(a, p2) < 5` is the scrutinee `w(a, p1) < 5` under another proof of one bound. So a binder the key does not name ends the test where it stands as a variable in a part reduction has left — the candidate as reduction leaves a stuck form, and its head, a fold's operands, a projection's head and a match's scrutinee, each as reduction left it — and every other part that names one, an argument, a field, a type former's parameters and indices, a telescope's entries outside its binders, a match's goal, is reduced ([`Reducer::reduce_forced`]) and read in turn. A binder that is a proof is passed over, and so is an application of one, which is a proof too; and so is any part the checker's conversion compares at a proposition ([`ReadsProofs::proof_parts`]), asked only of a part that would otherwise end the test. Each checker says which are proofs.
///
/// **What conversion compares only in step passes.** A part conversion meets only by opening a binder or entering an arm — a lambda's body, a stuck match's arms, a telescope's entries beneath its binders — it compares with the other side's, both reduced in step, and stops at the first difference. Reduced one side alone it has nothing to stop it: a stuck match's arm unfolds a recursive call into a match stuck again, as `/std/Nat/log2`'s does, without end. So the walk passes it, and the comparison an admitted form is put to reads it. So does a part eta never reads: a lambda's annotations, and a struct literal's parameters, each compared with a neutral through its type.
///
/// **Elaboration's own forms end the test.** An unsolved metavariable and an elaboration-transient node are no Core term the certifier compares, and the elaborator's conversion holds a metavariable equal to nothing but itself inside a question, so one naming a binder the key does not is no reduct of the key. Its spine read reduced would unfold a local whose definition holds the metavariable again while a recursive definition is elaborated, which the walk, filing what it read of a part only once that part is read, would not meet again.
///
/// That makes this a filter and not a rule: every candidate an eager key would match still passes it, because such a candidate *is* a reduct of the key. What it excludes is the traffic — every stuck form produced under a binder some other judgment opened, which is most of what a probe at a stuck reduct sees — most of it at the first such binder, standing as an operand reduction has already left, before anything is reduced.
///
/// Globals are deliberately not tested, and the asymmetry is the point: a reduct's globals are not bounded by the key's, so testing them would exclude exactly the unfoldings a settlement exists to perform.
///
/// Admitting every form refuses nothing that holds, and costs what it puts to the equations: under a guard over a web of definitions, an arm whose stuck forms name other binders settles and classes each of them against the web, which `curios`' `tests::unfolding::a_guard_over_a_web_puts_forms_over_other_binders_to_no_equation` holds within the default budget. Tightening it does change verdicts, silently, by dropping refinements; `curios-cert`'s `whnf::equation_scope_tests::a_reduct_that_drops_a_local_is_still_reached` is the guard on that direction.
pub fn could_answer(
    reducer: &mut impl ReadsProofs,
    key: &Term,
    candidate: &Term,
    proofs: &[Free],
) -> Result<bool, ReduceError> {
    let named = key.mentions();
    let foreign = |name: &Free| !named.contains(name) && !proofs.contains(name);
    let mut walk = Walk {
        foreign: &foreign,
        proofs,
        read: HashMap::new(),
    };

    walk.reads(reducer, candidate, Stands::Left)
}

/// What [`could_answer`] asks its checker beyond reduction: which parts of a stuck form its conversion compares at a proposition, and so reads nowhere.
pub trait ReadsProofs: Reducer {
    /// The parts of `term` this checker's conversion compares at a proposition, by their identities, typed as it types each: an application's arguments by its head's telescope, a type former's parameters and indices and a constructor's parameters by its arity, a constructor's payload and a struct literal's fields by the declaration's, each opened at the parts before it, and an operation's operands by its signature. Empty where no lookup types them, which reads every part.
    fn proof_parts(&mut self, term: &Term) -> Result<Vec<usize>, ReduceError>;
}

/// How a part of a stuck form stands to reduction, which is how [`could_answer`] reads it.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Stands {
    /// As reduction left it: read as it stands, its other parts reduced.
    Left,
    /// Where conversion reduces it before comparing it: reduced and read, or passed where a binder of the form's is in reach, which conversion compares only in step.
    Unreduced,
}

/// One [`could_answer`] walk: the binders it counts, the proofs it passes over, and what it has read of each part.
///
/// **A part is read once, however many paths reach it.** A stuck form is a graph, a part shared by many of its parts, and read once per path it would be read as many times as the graph has paths, which grows with the product of its sharing. Keyed by the part's identity and how it stands, each answer is kept with the part itself, so no identity is reused while the walk holds it.
struct Walk<'w, F> {
    foreign: &'w F,
    proofs: &'w [Free],
    read: HashMap<(usize, Stands), (Term, bool)>,
}

impl<F: Fn(&Free) -> bool> Walk<'_, F> {
    /// Whether `term` reads no binder the walk counts, as [`could_answer`] reads it where it `stands`.
    fn reads(
        &mut self,
        reducer: &mut impl ReadsProofs,
        term: &Term,
        stands: Stands,
    ) -> Result<bool, ReduceError> {
        if !term.mentions().locals().any(self.foreign) {
            return Ok(true);
        }
        let at = (term.identity(), stands);
        if let Some((_, read)) = self.read.get(&at) {
            return Ok(*read);
        }
        let read = recurse(|| self.reads_part(reducer, term, stands))?;
        self.read.insert(at, (term.clone(), read));

        Ok(read)
    }

    fn reads_part(
        &mut self,
        reducer: &mut impl ReadsProofs,
        term: &Term,
        stands: Stands,
    ) -> Result<bool, ReduceError> {
        if stands == Stands::Unreduced {
            if term.reach() > 0 {
                return Ok(true);
            }
            let forced = reducer.reduce_forced(term.clone())?;
            return self.reads(reducer, &forced, Stands::Left);
        }
        match &**term {
            Subterm::Var(_) | Subterm::Metavar(_) | Subterm::Transient(_) => return Ok(false),
            _ if spine_head(term).is_some_and(|head| self.proofs.contains(head)) => {
                return Ok(true);
            }
            _ => {}
        }
        // The parts one reduction left as they are: its head, a fold's operands, a projection's head and a match's scrutinee.
        let left = match &**term {
            Subterm::Apply(apply) => vec![apply.head.identity()],
            Subterm::Intrinsic(_) => children(term),
            Subterm::Proj(proj) => vec![proj.head.identity()],
            Subterm::Match(matched) => vec![matched.head.identity()],
            _ => Vec::new(),
        };
        // The parts conversion meets only in step, entering an arm, and the ones eta never reads: a lambda whole, its annotations and its body, a stuck match's arms, and a struct literal's parameters.
        let unread = match &**term {
            Subterm::Func(_) => children(term),
            Subterm::Match(matched) => {
                let mut read = vec![matched.head.identity(), matched.goal.identity()];
                if let Cases::Induct {
                    stated: Some(stated),
                    ..
                } = &matched.cases
                {
                    read.push(stated.identity());
                }
                children(term)
                    .into_iter()
                    .filter(|child| !read.contains(child))
                    .collect()
            }
            Subterm::Struct(record) => record.params.iter().map(Term::identity).collect(),
            _ => Vec::new(),
        };
        let mut parts = Vec::new();
        term.any_child_term(&mut |child| {
            let identity = child.identity();
            if !unread.contains(&identity) {
                let stands = match left.contains(&identity) {
                    true => Stands::Left,
                    false => Stands::Unreduced,
                };
                parts.push((child.clone(), stands));
            }
            false
        });
        // A part the checker's conversion compares at a proposition is one it reads nowhere, asked only of a part that would otherwise end the test.
        let mut proofs = None;
        for (part, stands) in parts {
            if self.reads(reducer, &part, stands)? {
                continue;
            }
            let proofs = match &mut proofs {
                Some(proofs) => proofs,
                None => proofs.insert(reducer.proof_parts(term)?),
            };
            if !proofs.contains(&part.identity()) {
                return Ok(false);
            }
        }

        Ok(true)
    }
}

/// The identities of `term`'s parts, in the order a walk of them meets them.
fn children(term: &Term) -> Vec<usize> {
    let mut children = Vec::new();
    term.any_child_term(&mut |child| {
        children.push(child.identity());
        false
    });
    children
}

/// The local an application spine is headed by, where a local heads it.
fn spine_head(term: &Term) -> Option<&Free> {
    let mut head = term;
    while let Subterm::Apply(apply) = &**head {
        head = &apply.head;
    }
    match &**head {
        Subterm::Var(var) => var.as_free(),
        _ => None,
    }
}

/// Whether `value`, the value an arm's equation assumes of its scrutinee, is of a carrier whose values hold no type: the literal of a truth value, a number, a byte, a float or a packed sequence, a successor over a pattern among them. There a universe instance is no part of the value ([`curios_core::same_atom`]), so a stuck call met whole and the equation's reduced spelling apart in their head's instance alone are one where their arguments are ([`one_up_to_instances`](crate::one_up_to_instances)). A list's value and a constructed one may hold a type.
pub fn holds_no_type(value: &Term) -> bool {
    let Subterm::Intrinsic(literal) = &**value else {
        return false;
    };
    let carrier = match literal {
        Intrinsic::Bool(_) => Carrier::Boolean,
        Intrinsic::Nat(_) => Carrier::Natural,
        Intrinsic::Int(_) => Carrier::Integer,
        Intrinsic::Byte(_) => Carrier::Byte,
        Intrinsic::Flt(_) => Carrier::Float,
        Intrinsic::Bytes(_) => Carrier::Bytes,
        _ => return false,
    };
    !carrier.holds_types()
}

/// What an arm's equation says of a term.
#[derive(Debug, PartialEq)]
pub enum Answered {
    /// The equation gives the term this value.
    Value(Term),
    /// It says nothing of the term: nothing the carriers' readers can see, and nothing a checker's conversion could.
    Silent,
    /// The readers decide nothing of the term against the key, and some two of the pair's atoms may be one: the checker classes them by its own conversion ([`Classes::of`]) and asks [`answers_classed`].
    Atoms(Vec<Term>),
    /// The term and the key are no two operations of the algebra, and may be one term, being one former over heads that may be one: the checker asks its own conversion whether they are, with the equation and every equation inside it withheld, as the key's reduced spelling was settled — under its own equation the key is its case value, and nothing would be the key.
    Whole,
}

/// What an arm's equation says of `term`, where the equation assumes `value` of a scrutinee whose reduct is `key`. Both terms are as reduction leaves a stuck form, weak-head with their operands reduced.
///
/// **One rule, for both reducers.** An equation is a claim about one term, and which terms are that term is answered here once. The term is the key; or the carriers' readers hold the two equal outright (`intrinsics_agree`), as conversion holds them wherever no arm stands — a sum commuted, a comparison with both sides moved, an `==` swapped; or, where the equation's value is a `Bool`, the readers hold the term equal to the key negated, and the value is negated on the way — a guard's dual, however it is spelled. Past that the readers pair atoms by identity, and which atoms are one is the checker's conversion's to say: the rule hands the question back ([`Answered::Atoms`], [`Answered::Whole`]) and is asked again with the answer ([`answers_classed`]). A reduction that asks nothing reads either as [`Answered::Silent`].
///
/// **It calls no judgment**, since it runs inside reduction.
///
/// **Nothing is reduced.** Both terms are reduced already, so the readers are handed a driver that takes every term as it stands (`Reduced`): forcing the term would ask the reducer for the stuck form it is in the middle of probing, forcing the key would meet the key's own equation and read the case value in its place, and forcing an operand again is a lookup at least, and a reduction where the memo has none, at every operand of every stuck form a lookup is made at. A node the fold left as written below an operand — a connective's right operand behind a stuck left — is read as the atom it spells, which declines where reducing it might have answered.
///
/// **Only two terms of one kind are put to the readers.** The chain is written for the two sides of one comparison, which have one type, and a lookup presents whatever stuck form reduction reached: a pair is read only where both produce a `Bool` or both produce a value of one carrier.
pub fn answers(
    driver: &mut impl IntrinsicDriver,
    term: &Term,
    key: &Term,
    value: &Term,
) -> Result<Answered, ReduceError> {
    if term == key {
        return Ok(Answered::Value(value.clone()));
    }
    let operations = match (&**term, &**key) {
        (Subterm::Intrinsic(this), Subterm::Intrinsic(that)) => {
            let produced = produces(this);
            (produced.is_some() && produced == produces(that)).then_some((this, that))
        }
        _ => None,
    };
    let Some((this, that)) = operations else {
        // No two operations of one kind: one term only where the two are one former whose heads say they may be — which a variable, a metavariable and two operations of different kinds never are, and a spelling still to be unfolded is not until it is reduced.
        let whole = mem::discriminant(&**term) == mem::discriminant(&**key)
            && classable(&[term.clone(), key.clone()]);
        return Ok(match whole {
            true => Answered::Whole,
            false => Answered::Silent,
        });
    };

    let negated = negation(key);
    let mut reduced = Reduced(driver);
    if let Some(value) = decided(&mut reduced, this, that, &negated, value)? {
        return Ok(Answered::Value(value));
    }
    let atoms = atoms_of(&mut reduced, term, key)?;
    if classable(&atoms) {
        return Ok(Answered::Atoms(atoms));
    }
    // No two of the pair's atoms could be one to a checker, so no question is handed back: the pair is read once more as the readers class its nodes.
    Ok(
        match answers_classed(driver, term, key, value, &mut Classes::default())? {
            Some(value) => Answered::Value(value),
            None => Answered::Silent,
        },
    )
}

/// [`answers`] asked again of a pair as its partition spells it, each opaque atom as its class's representative and each node as the first node its readers hold it one with: the value the equation gives the term, or `None` where the classed pair decides nothing either.
///
/// The classed spellings are read as they stand, as the pair was: each is the term it was built from with atoms replaced by reduced terms they convert with.
pub fn answers_classed(
    driver: &mut impl IntrinsicDriver,
    term: &Term,
    key: &Term,
    value: &Term,
    classes: &mut Classes,
) -> Result<Option<Term>, ReduceError> {
    let mut reduced = Reduced(driver);
    let Some((this, that)) = classed(&mut reduced, term, key, classes)? else {
        return Ok(None);
    };
    if this == that {
        return Ok(Some(value.clone()));
    }
    let (Subterm::Intrinsic(this_operation), Subterm::Intrinsic(that_operation)) = (&*this, &*that)
    else {
        return Ok(None);
    };

    let negated = negation(&that);
    decided(
        &mut reduced,
        this_operation,
        that_operation,
        &negated,
        value,
    )
}

/// The value the readers give `this` against `that`, an equation's key assumed to be `value`: `value` where they hold the two equal outright, and where `value` is a `Bool`, its negation where they hold `this` equal to `negated`, the key negated.
fn decided(
    driver: &mut impl IntrinsicDriver,
    this: &Intrinsic,
    that: &Intrinsic,
    negated: &Term,
    value: &Term,
) -> Result<Option<Term>, ReduceError> {
    if intrinsics_agree(driver, this.clone(), that.clone())? {
        return Ok(Some(value.clone()));
    }
    let (Some(literal), Subterm::Intrinsic(negation)) = (value.as_bool(), &**negated) else {
        return Ok(None);
    };
    Ok(intrinsics_agree(driver, this.clone(), negation.clone())?
        .then(|| Term::intrinsic(Intrinsic::Bool(!literal))))
}

/// `key` negated, as `Bool/not` unfolds: the spelling the readers take a comparison's dual from.
fn negation(key: &Term) -> Term {
    Term::intrinsic(Intrinsic::BoolXor(
        key.clone(),
        Term::intrinsic(Intrinsic::Bool(true)),
    ))
}

/// What an operation produces, as far as its declaration says: a `Bool` for a comparison and for every operation on `Bool`, a `Nat` for a length, and otherwise a value of the carrier it is declared at. `None` for an intrinsic the algebra does not declare.
fn produces(intrinsic: &Intrinsic) -> Option<Carrier> {
    match intrinsic.algebra() {
        Declaration::Operation {
            carrier, operation, ..
        } => Some(match operation {
            Operation::Equal | Operation::Unequal | Operation::Less | Operation::AtMost => {
                Carrier::Boolean
            }
            Operation::Length => Carrier::Natural,
            _ => carrier,
        }),
        Declaration::Opaque => None,
    }
}

/// A driver that takes every term as reduced already: asked to reduce one it hands it back, and what the readers build is still charged to the checker's own driver.
struct Reduced<'a, D>(&'a mut D);

impl<D: IntrinsicDriver> Reducer for Reduced<'_, D> {
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

impl<D: IntrinsicDriver> IntrinsicDriver for Reduced<'_, D> {
    fn prepare(&mut self, intrinsic: Intrinsic) -> Intrinsic {
        self.0.prepare(intrinsic)
    }

    fn solving_view(&mut self, this: &Intrinsic, that: &Intrinsic) -> Option<bool> {
        self.0.solving_view(this, that)
    }

    fn syntax(&self) -> SyntaxRegistry {
        self.0.syntax()
    }
}
