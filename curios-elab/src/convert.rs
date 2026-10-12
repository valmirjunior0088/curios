mod intrinsic;
use intrinsic::*;

mod linear;
pub(crate) use linear::*;

mod neutral;
pub(crate) use neutral::*;

mod occurrence;
use occurrence::*;

mod pairs;
pub(crate) use pairs::*;

mod sort;
pub(crate) use sort::*;

#[cfg(test)]
mod conversion_tests;
#[cfg(test)]
mod imitation_tests;
#[cfg(test)]
mod intrinsic_tests;
#[cfg(test)]
mod solve_tests;
#[cfg(test)]
mod structural_tests;
#[cfg(test)]
mod test_support;
#[cfg(test)]
mod variance_tests;

use {
    super::{
        Context, applied_head, check, infer, reduce, reduce_forced, stalled_unfolding, unfold_rec,
        unfold_rec_apply,
    },
    crate::{
        Declined, Error, metavar_origins, metavar_spines, named_members, read_outside_spines,
        zonk_solved_term_metas,
    },
    curios_analysis::Classes,
    curios_core::{
        Advance, Apply, Bound, Carrier, Cases, Cost, Cursor, Exhaustion, Extent, Families, Free,
        Func, FuncType, Global, InductDecl, InductType, Instance, InstanceHead, Intrinsic, Level,
        Lockstep, Many, Match, MatchResult, Metavar, MetavarId, MetavarOrigin, Probe, Proj,
        Quality, Rec, ReduceError, Scope, Step, Struct, StructType, Subterm, Telescope, Term,
        Three, Tuple, TupleType, UniverseConstraintKind, UniverseConstraintOrigin, UniverseContext,
        Universes, Variance, Variant, past_bool_cap,
    },
    curios_utilities::{Keyed, Plicity, recurse},
    std::{
        collections::{HashMap, HashSet, VecDeque},
        mem,
    },
};

/// The outcome of attempting to solve a metavariable against a candidate.
enum Solved {
    /// Committed `m := t`.
    Done,
    /// Not yet solvable (an embedded unsolved metavariable, or a candidate re-validation could not judge yet): the problem is parked on the `blocked` queue and retried after later progress.
    Postponed,
    /// Unsolvable: occurs-check, scope-check, or re-validation failure.
    Failed,
}

/// What re-validation made of a candidate against its metavariable's frozen type.
enum Revalidated {
    /// It checks, and this is the term the check rebuilt.
    Fits(Term),
    /// The check failed where it wanted to park: a type it met still waits on a metavariable, and nothing has been judged.
    Undecided,
    /// It does not check, and this is the check's refusal.
    Refused(Error),
}

/// A candidate conversion found for a metavariable, which the check where the metavariable was born refused: the occurrence it was offered at, the candidate in that occurrence's names, and the check's refusal. The problem that offered it is then no pair found apart, and its report is this candidate's ([`Beside::Rejected`]).
#[derive(Debug)]
pub(crate) struct Rejected {
    pub(crate) metavar: Metavar,
    pub(crate) candidate: Term,
    pub(crate) error: Error,
}

/// What a conversion run says beside a mismatch, of the problem the run ended on.
#[derive(Debug)]
pub(crate) enum Beside {
    /// What it left uncompared: the two sides the mismatch shows are two it did not decide between.
    Declined(Declined),
    /// The candidate it found and the metavariable's birth refused: the two sides it was comparing were not found apart, and what is refused is a solution.
    Rejected(Box<Rejected>),
}

impl Beside {
    /// What was left uncompared, where that is what was said.
    pub(crate) fn declined(self) -> Option<Declined> {
        match self {
            Self::Declined(declined) => Some(declined),
            Self::Rejected(_) => None,
        }
    }
}

/// The binders a candidate lambda abstracts: each one's mark, its label and its domain.
type CandidateBinders = Vec<(Plicity, Free, Term)>;

fn instantiate_bound_at<B: Bound>(
    context: &mut Context,
    universe_context: &UniverseContext,
    value: &B,
    universes: &Universes,
) -> Result<B, ReduceError> {
    context
        .instantiate_universe_bound_at(universe_context, value, universes)
        .map_err(ReduceError::Universe)
}

pub(crate) fn convert(
    context: &mut Context,
    type_: &Term,
    this: &Term,
    that: &Term,
) -> Result<bool, ReduceError> {
    // The strict boolean-oracle view: "not yet decidable" is *not* "equal". Everything that needs a definite yes/no (re-validation, the inverter, type-level dispatch) uses this; only the elaboration turnaround (`expect`) may treat `Blocked` as provisional success by parking it.
    Ok(matches!(
        convert_outcome(context, type_, this, that)?,
        Outcome::Converts
    ))
}

/// Whether `this` and `that` convert at `Type` with no solution and no universe constraint committed: the comparison runs in the bracket a witness probe uses and is rolled back whatever it found, so a pair that would need a metavariable solved is not one, and asking commits nothing. Nor is a level equation tried: two levels the store does not already read as one are withheld ([`Context::withholds`]), so the step that would have joined them does not hold and the pair is read by its value, as the kernel reads two instances it finds apart, by unfolding. Two occurrences of one definition, each at an open level of its own that the value does not read, are one term here whatever the two levels close at. A weak equation the comparison recorded is no commitment: it requires nothing of its two levels, a nominal family being irrelevant in them, and it goes with the bracket. What a classing asks of two atoms (`intrinsic`'s `classes`), and what reduction asks of two stuck forms (`reduce`'s `asked_classes` and `asked_scrutinee`).
pub(crate) fn same_uncommitted(
    context: &mut Context,
    this: &Term,
    that: &Term,
) -> Result<bool, ReduceError> {
    same_uncommitted_inside(context, &Pairs::default(), this, that)
}

/// [`same_uncommitted`] for a pair asked about inside the unfolding of `pairs`: what a comparison asks of a part of the problem it has in hand, a classing's atoms among them.
pub(crate) fn same_uncommitted_inside(
    context: &mut Context,
    pairs: &Pairs,
    this: &Term,
    that: &Term,
) -> Result<bool, ReduceError> {
    // Hand-paired rather than bracketed by a closure, as the witness probe's is: this sits on conversion's recursion.
    let mark = context.solution_mark();
    let solutions = context.solutions_committed();
    let universes = context.universes().state_token();
    let outer = context.withhold(Some(false));
    let converts = convert_under(
        context,
        &Binders::default(),
        pairs,
        &Term::type_ground(),
        this,
        that,
    )
    .map(|outcome| matches!(outcome, Outcome::Converts));
    let withheld = context.withhold(outer) == Some(true);
    let committed = context.solutions_committed() != solutions
        || !context
            .universes()
            .state_token()
            .requires_the_same_as(&universes);
    context.rollback_solutions(mark);
    context.end_solutions(mark);
    // How many pairs were asked about, and how many of them a solution or a level constraint stood between: an answer of the second kind can change once the solver moves, so whatever reduction is in progress remembers nothing taken across it.
    curios_profile::sample!("convert::same_uncommitted", u64::from(committed));
    // An answer other than yes reached past a withheld level equation can change the same way; a yes reached past one holds at every level.
    if committed || (withheld && !matches!(converts, Ok(true))) {
        context.note_declined();
    }
    Ok(converts? && !committed)
}

/// Which of `atoms` are one by `same`, as a classing asks of them (`curios-analysis`'s `Classes::of`): each pair stands as the pair asked while it is compared, so a classing made inside that comparison is held inside the two atoms, and this one inside the pair a classing in progress asked about. The comparison is one conversion inside another, so it buys its depth with stack.
pub(crate) fn classing(
    context: &mut Context,
    atoms: &[Term],
    same: impl FnMut(&mut Context, &Term, &Term) -> Result<bool, ReduceError>,
) -> Result<Classes, ReduceError> {
    let mut classes = Classes::default();
    classing_among(context, &mut classes, atoms, same)?;
    Ok(classes)
}

/// [`classing`] among the classes `classes` already holds (`curios-analysis`'s `Classes::join`): what a reader asks that meets its atoms a term at a time, as the reader of the facts a bound follows from does.
pub(crate) fn classing_among(
    context: &mut Context,
    classes: &mut Classes,
    atoms: &[Term],
    mut same: impl FnMut(&mut Context, &Term, &Term) -> Result<bool, ReduceError>,
) -> Result<(), ReduceError> {
    let within = context.asked().cloned();
    classes.join(atoms, within.as_ref(), |this, that| {
        let stood = context.ask(Some((this.clone(), that.clone())));
        let same = recurse(|| same(context, this, that));
        context.ask(stood);
        same
    })
}

pub(crate) fn convert_outcome(
    context: &mut Context,
    type_: &Term,
    this: &Term,
    that: &Term,
) -> Result<Outcome, ReduceError> {
    convert_under(
        context,
        &Binders::default(),
        &Pairs::default(),
        type_,
        this,
        that,
    )
}

/// [`convert_outcome`] for a pair posed under `binders` and inside the unfolding of `pairs`: a comparison conversion makes of its own, of a part of the problem it has in hand.
pub(crate) fn convert_under(
    context: &mut Context,
    binders: &Binders,
    pairs: &Pairs,
    type_: &Term,
    this: &Term,
    that: &Term,
) -> Result<Outcome, ReduceError> {
    let problem = Problem {
        type_: type_.clone(),
        this: this.clone(),
        that: that.clone(),
        binders: binders.clone(),
        pairs: pairs.clone(),
        relation: Relation::Equal,
    };

    problem_outcome(context, problem, None)
}

/// Whether a term of type `lower` may stand where `upper` is wanted: the two types posed at [`Relation::Below`], under no binder. `extent` is the term's, which a level bound raised on the way reports.
pub(crate) fn subsume_outcome(
    context: &mut Context,
    lower: &Term,
    upper: &Term,
    extent: Option<Extent>,
) -> Result<Outcome, ReduceError> {
    let problem = Problem {
        type_: Term::type_ground(),
        this: lower.clone(),
        that: upper.clone(),
        binders: Binders::default(),
        pairs: Pairs::default(),
        relation: Relation::Below,
    };

    problem_outcome(context, problem, extent)
}

/// Decide `problem` at the relation it carries: a question asked afresh, and the retry of a goal an earlier run surrendered, which resumes as what it was asked.
pub(crate) fn problem_outcome(
    context: &mut Context,
    problem: Problem,
    extent: Option<Extent>,
) -> Result<Outcome, ReduceError> {
    curios_profile::profile!("convert::outcome");
    Convert::new(problem, extent).outcome(context)
}

/// Whether what holding spines equal solves stands.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Spines {
    /// A completing term's question of a conversion it awaits: what holding the spines inside it equal would solve, withdrawn once read.
    Offered,
    /// The term's answer: the spines held equal, and what that solves committed.
    Held,
}

/// What a conversion asked again with spines held came to, where that decided anything of it ([`hold_spines`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct HeldConversion {
    /// What it solved, each solution as it is spelled once all are in.
    pub solved: Vec<(MetavarId, Term)>,
    /// What is left of it undecided, which stands for it from here on: nothing where it converts, and otherwise the pairs that still wait, as the arguments of an imitated application do where both sides of one are open.
    pub left: Vec<Problem>,
}

/// Ask `problem` again with spines held: every pair of two applications of one global definition met in deciding it, at any depth, agrees where its spines can be made to, by whatever that solves, and an application of an unsolved metavariable met against a type former's node is imitated ([`Context::holding_spines`]). It is the last resort of a conversion left parked, which the term that awaits it takes once it has read its parts. Answers what that came to where it converts or solves anything, and `None` where the problem is no further decided so, or is refused: a refusal of a guess says nothing of the problem. A head that carries an instance is read as a monomorphic one is: what a call infers does not follow a level the definition's body holds.
pub(crate) fn hold_spines(
    context: &mut Context,
    problem: &Problem,
    reading: Spines,
) -> Result<Option<HeldConversion>, ReduceError> {
    let mark = context.solution_mark();
    let committed = context.solutions_committed();
    let outcome =
        context.holding_spines(|context| Convert::new(problem.clone(), None).outcome(context));
    let solved = context
        .solutions_since(committed)
        .into_iter()
        .map(|(id, solution)| (id, zonk_solved_term_metas(context, &solution)))
        .collect::<Vec<_>>();
    let held = match &outcome {
        Ok(Outcome::Converts) => Some(HeldConversion {
            solved,
            left: Vec::new(),
        }),
        Ok(Outcome::Blocked(left)) if !solved.is_empty() => Some(HeldConversion {
            solved,
            left: left.clone(),
        }),
        _ => None,
    };
    if held.is_none() || reading == Spines::Offered {
        context.rollback_solutions(mark);
    }
    context.end_solutions(mark);
    outcome?;

    Ok(held)
}

/// Whether two answers offered for the metavariable `id` are apart: compared at the type it was born at, under the context and the refinements it was born under, as an oracle and with nothing committed. Two that cannot yet be told apart are not.
pub(crate) fn answers_differ(
    context: &mut Context,
    id: MetavarId,
    one: &Term,
    another: &Term,
) -> Result<bool, ReduceError> {
    let Some(entry) = context.metavar_entry(id) else {
        return Ok(false);
    };
    let telescope = entry.telescope.clone();
    let born = entry.frames.clone();
    let result = entry.result.clone();

    let mark = context.solution_mark();
    let outcome = context.at_birth(&born, |context| {
        context.with_frame(|context| {
            for (name, ty) in telescope.iter() {
                context.assume(name, ty);
            }

            context.with_oracle(|context| convert_outcome(context, &result, one, another))
        })
    });
    context.rollback_solutions(mark);
    context.end_solutions(mark);

    Ok(matches!(outcome?, Outcome::Mismatch(_)))
}

/// `candidate` with every `let`'s local that the occurrence `metavar` does not name read as its value, the solutions a value holds materialized as the candidate's are.
///
/// A block offers its type for a metavariable born outside it, an implicit of the call the block is an argument of, and that type names the block's local, which the metavariable's context does not hold. The local stands for its value, so the candidate is that value's: read otherwise, the scope check fails it, and the call is admitted with the block read after the argument that gives the implicit and refused with it read before. So is the candidate a problem parked in a block offers the stand-in its metavariable was restricted to where the block ended ([`Context::block_restriction`]): the block's frame stands again for the retry, and the stand-in's spine names none of its labels. A recursive group's member stands for no value, its label being defined as the slot its body fills and then as its projection out of the group, and is left to the scope check.
fn by_value(context: &Context, metavar: &Metavar, mut candidate: Term) -> Term {
    if !candidate.has_local_free() {
        return candidate;
    }
    let named = metavar
        .spine
        .iter()
        .flat_map(|entry| entry.free_vars())
        .collect::<HashSet<_>>();

    // A value names only the bindings before its own, so each round reads the latest away, and the rounds end where one reads nothing: a local that only an embedded metavariable's spine names stands there ([`read_outside_spines`]), and that metavariable is restricted to the occurrence's context or holds the candidate back.
    loop {
        let values = candidate
            .free_vars_shared()
            .iter()
            // A block's local is a binder, and the test asks nothing of a declaration: `Context::is_top_level` would note a question in the trace for every declaration a candidate names.
            .filter(|name| matches!(name, Free::Local(_)) && !named.contains(*name))
            .filter_map(|name| {
                let value = context.definition_body(name)?;
                let member = value.as_rec_proj().is_some()
                    || matches!(&**value, Subterm::Metavar(slot) if context.is_rec_slot(slot.id));
                (!member).then(|| (*name, value.clone()))
            })
            .collect::<Vec<_>>();
        let read = zonk_solved_term_metas(context, &read_outside_spines(&candidate, &values));
        if read == candidate {
            return candidate;
        }
        candidate = read;
    }
}

/// What a reading of two sides as two applications of one global definition came to.
enum Spun {
    /// The sides are no such pair, or their spines differ with no metavariable for a solution to change, or the name is a type former's, whose node keeps its arguments.
    Apart,
    /// Their spines do not agree under the reading, and one holds a metavariable nothing has solved, so they may yet; and the definition is one reduction unfolds, so what is left of the pair once reduced holds neither call.
    Open,
    /// As [`Spun::Open`], and their spines agree by what that would solve: the two calls are equal there, so whatever their reduction comes to they are not apart.
    Solvable,
    /// They agree.
    Agreed,
}

/// The verdict of a conversion run, distinguishing "provably unequal" from "not yet decidable".
#[derive(Debug)]
pub(crate) enum Outcome {
    /// Definitionally equal.
    Converts,
    /// A hard structural mismatch — provably unequal, no solution can help — beside what the problem that ended the run declined to compare, where it declined anything, or the candidate it found and saw refused: the report then says the two sides were not compared rather than found different, or is that candidate's.
    Mismatch(Option<Beside>),
    /// Quiesced with constraints still blocked on unsolved metavariables: undecided either way. The blocked problems are surrendered to the caller — the elaboration turnaround parks them on the `Context` to be retried when a watched metavariable is solved.
    Blocked(Vec<Problem>),
}

/// The relation a conversion problem is asked at.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Relation {
    /// `this ≡ that`.
    Equal,
    /// `this ≤ that`, subsumption: a term of type `this` may stand where `that` is wanted. It differs from equal between two sorts and in a function type's codomain, and is conversion everywhere else (`documentation/design/theory/subsumption-is-a-relation-not-a-traversal-order.md`).
    Below,
}

/// Which side of a problem at below a metavariable stands on: the direction a level of its own is bounded in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Standing {
    /// `?m ≤ t`: the metavariable is the lower side.
    Under,
    /// `t ≤ ?m`: the metavariable is the upper side.
    Over,
}

/// One conversion constraint, `this ≡ that : type_`, or `this ≤ that` between two types (comparison is type-directed: the type drives η-expansion and proof irrelevance). Public because a [`Outcome::Blocked`] verdict surrenders its still-blocked problems to the caller, which parks them on the [`Context`] (as `ParkedWork::Conversion`) for retry once a watched metavariable is solved.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Problem {
    pub type_: Term,
    pub this: Term,
    pub that: Term,
    /// The binders this problem is posed under, which a retry of it is posed under too.
    pub binders: Binders,
    /// The unfoldings it stands inside, which a retry of it stands inside too.
    pub pairs: Pairs,
    /// The relation it is asked at, which a problem that waits keeps: one parked at below resumes at below.
    pub relation: Relation,
}

/// A problem as a run remembers having posed it: its type, its two sides, the relation it is asked at and the unfoldings it stands inside. The binders it is posed under are no part of it: a label is minted once and at one type, so two problems equal in their terms stand under the same binders.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Posed {
    type_: Term,
    this: Term,
    that: Term,
    relation: Relation,
    pairs: Pairs,
}

#[derive(Debug)]
pub(crate) struct Convert {
    // Every problem this run has posed. A run holds where each of its problems does, so one posed again adds nothing it does not already owe, and is not posed: what lets the walk take two equal graphs built apart in their size. Nothing is assumed by it, a problem never being the one posed inside its own unfolding ([`Pairs`]).
    posed: HashSet<Keyed<Posed>>,
    pending: VecDeque<Problem>,
    // Constraints postponed because a side is flexible but not yet solvable (flex–flex with distinct heads, or a candidate carrying an unsolved metavariable). Retried whenever a fresh solution lands.
    blocked: Vec<Problem>,
    // Whether a metavariable was solved since the last `blocked` sweep — the signal that retrying `blocked` could make further progress.
    progress: bool,
    // What the problem in hand declined to compare. Cleared as each problem is taken up, so one that is parked leaves nothing behind and only the problem a mismatch ends on speaks in its report.
    declined: Option<Declined>,
    // The candidate the problem in hand found for a metavariable and saw refused where the metavariable was born. Cleared as each problem is taken up, as what it declined is.
    rejected: Option<Box<Rejected>>,
    // The binders the problem in hand is posed under, and those this step has opened since: what a problem enqueued now is posed under.
    binders: Binders,
    // The unfoldings the problem in hand stands inside, and the one this step has opened since: what a problem enqueued now stands inside.
    pairs: Pairs,
    // The level pairs the levels-only shortcut left to a curried head's type, in the order it met them. Nothing compares the two levels of one where the run goes on to hold its sides one, so each is a weak equation then and nothing where it does not.
    apart: Vec<(Level, Level)>,
    // The extent of the term this run was asked for, where it was asked for one: what a level bound raised at below reports.
    extent: Option<Extent>,
}

impl Convert {
    /// Commit `left` and `right` equal, as conversion holds two instances or two sorts one, answering whether they are held. A question asked with nothing committed joins no two levels ([`Context::withholds`]), so there a pair the store does not already read as one is not held, and the step that needed it does not hold: the reduced comparison after a spine and the structural path after the levels-only shortcut read the pair by its value.
    fn join_levels(
        context: &mut Context,
        left: &Level,
        right: &Level,
    ) -> Result<bool, ReduceError> {
        if context.withholds(left, right)? {
            return Ok(false);
        }
        context
            .universes_mut()
            .add_eq(
                left.clone(),
                right.clone(),
                UniverseConstraintOrigin::new(UniverseConstraintKind::Conversion),
            )
            .map_err(ReduceError::Universe)?;

        Ok(true)
    }

    fn compare_levels(
        context: &mut Context,
        left: &[Level],
        right: &[Level],
    ) -> Result<bool, ReduceError> {
        if left.len() != right.len() {
            return Ok(false);
        }
        for (left, right) in left.iter().zip(right) {
            if !Self::join_levels(context, left, right)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// Whether two instances are one: the sorts they state held the same ([`Context::same_quality`]), then [`Self::compare_levels`] over their levels.
    fn compare_universes(
        context: &mut Context,
        left: &Universes,
        right: &Universes,
    ) -> Result<bool, ReduceError> {
        Ok(Self::compare_qualities(context, left, right)
            && Self::compare_levels(context, &left.levels, &right.levels)?)
    }

    /// Whether two instances state the same sorts, an unsolved one solved to the other: two instances apart in a sort are two types, whatever their levels.
    fn compare_qualities(context: &mut Context, left: &Universes, right: &Universes) -> bool {
        left.qualities.len() == right.qualities.len()
            && left
                .qualities
                .iter()
                .zip(&right.qualities)
                .all(|(left, right)| context.same_quality(*left, *right))
    }

    /// [`Self::compare_levels`] for two instances of `name`, a family or a definition applied in full: one equation per level its vector calls invariant, and a weak one where it is irrelevant, two instances apart in such a level alone being one term. The weak equation refuses nothing; it is what lets the declaration's closing hold the two occurrences at one instance where it can ([`UniverseSolver::add_weak_eq`](crate::UniverseSolver::add_weak_eq)).
    fn compare_instance_levels(
        context: &mut Context,
        name: &Global,
        left: &Universes,
        right: &Universes,
    ) -> Result<bool, ReduceError> {
        if left.levels.len() != right.levels.len() || !Self::compare_qualities(context, left, right)
        {
            return Ok(false);
        }
        for (index, (left, right)) in left.levels.iter().zip(&right.levels).enumerate() {
            if context.variance(name, index) == Variance::Irrelevant {
                context
                    .universes_mut()
                    .add_weak_eq(left.clone(), right.clone());
                continue;
            }
            if !Self::join_levels(context, left, right)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    fn new(problem: Problem, extent: Option<Extent>) -> Self {
        let mut run = Self::under(problem.binders.clone(), problem.pairs.clone(), extent);
        run.posed.insert(Keyed::new(Posed {
            type_: problem.type_.clone(),
            this: problem.this.clone(),
            that: problem.that.clone(),
            relation: problem.relation,
            pairs: problem.pairs.clone(),
        }));
        run.pending.push_back(problem);

        run
    }

    /// A run with nothing posed yet: what it is handed to compare is posed under `binders` and inside `pairs`.
    fn under(binders: Binders, pairs: Pairs, extent: Option<Extent>) -> Self {
        Self {
            posed: HashSet::new(),
            binders,
            pairs,
            pending: VecDeque::new(),
            blocked: Vec::new(),
            progress: false,
            declined: None,
            rejected: None,
            apart: Vec::new(),
            extent,
        }
    }

    /// Mint a fresh label for opening scopes under structural comparison, recording the type its position gives it, where it gives one, for the lookups that meet the label under it ([`Binders`]). The label itself is ordinary entropy freshness — recording changes nothing about the terms conversion builds.
    fn opening(&mut self, context: &mut Context, hint: Option<&str>, type_: Option<Term>) -> Free {
        let binder = context.fresh(hint);
        if let Some(type_) = type_ {
            self.binders = self.binders.opening(binder, type_);
        }
        binder
    }

    /// Open every binder of `telescope` at a fresh label recorded at its domain, and hand back the labels and what the telescope ends in.
    fn open_telescope<B: Bound>(
        &mut self,
        context: &mut Context,
        telescope: &Telescope<B>,
    ) -> Result<(Vec<Term>, B), ReduceError> {
        telescope.walk_producing(|_, _, domain| {
            Ok(Term::free_var(&self.opening(context, None, Some(domain))))
        })
    }

    /// A motive's binders, opened as elaboration opens them where it types the motive: the family's index domains and then the scrutinee at the family over those binders, or the scrutinee's own type for a carrier that has no index. Where no lookup typed the scrutinee, or the motive binds another count than its family states, they carry no type. The kernel opens a motive's the same way.
    fn motive_binders(
        &mut self,
        context: &mut Context,
        scrutinee: Option<&Term>,
        arity: usize,
    ) -> Result<Vec<Term>, ReduceError> {
        let untyped = |convert: &mut Self, context: &mut Context| {
            (0..arity)
                .map(|_| Term::free_var(&convert.opening(context, None, None)))
                .collect::<Vec<_>>()
        };

        match scrutinee.map(|type_| (type_, &**type_)) {
            Some((
                _,
                Subterm::InductType(InductType {
                    name,
                    universes,
                    params,
                    ..
                }),
            )) => {
                let Some(declaration) = declared_at(context, name, universes, params)? else {
                    return Ok(untyped(self, context));
                };
                let indices = declaration.indices_at(params);
                if indices.len() + 1 != arity {
                    return Ok(untyped(self, context));
                }

                let (mut opened, ()) = self.open_telescope(context, &indices)?;
                let over_them =
                    Term::induct_type_at(*name, universes.clone(), params.clone(), opened.clone());
                opened.push(Term::free_var(&self.opening(
                    context,
                    None,
                    Some(over_them),
                )));

                Ok(opened)
            }
            Some((type_, _)) if arity == 1 => Ok(vec![Term::free_var(&self.opening(
                context,
                None,
                Some(type_.clone()),
            ))]),
            _ => Ok(untyped(self, context)),
        }
    }

    /// A cons arm's binders: the carrier's own domains, and the hypothesis at the result at the tail, which only a one-binder family states. An ambient goal types no hypothesis and its arm reads none, so that binder carries no type.
    fn cons_binders(
        &mut self,
        context: &mut Context,
        domains: Vec<Term>,
        result: &MatchResult,
    ) -> Vec<Term> {
        let mut opened = domains
            .into_iter()
            .map(|domain| Term::free_var(&self.opening(context, None, Some(domain))))
            .collect::<Vec<_>>();
        let tail = opened.last().expect("a cons arm binds its tail");
        let hypothesis = match result {
            MatchResult::Family(motive) if motive.arity() == 1 => Some(motive.open(&[tail])),
            _ => None,
        };
        opened.push(Term::free_var(&self.opening(context, None, hypothesis)));

        opened
    }

    pub(crate) fn enqueue(&mut self, type_: Term, this: Term, that: Term) {
        self.enqueue_at(Relation::Equal, type_, this, that);
    }

    /// Whether `this ≡ that` at `type_`, asked as a problem of its own under this run's binders and inside its unfoldings, in the strict reading: a pair not yet decidable is not equal.
    pub(super) fn converts(
        &self,
        context: &mut Context,
        type_: &Term,
        this: &Term,
        that: &Term,
    ) -> Result<bool, ReduceError> {
        let outcome = convert_under(context, &self.binders, &self.pairs, type_, this, that)?;

        Ok(matches!(outcome, Outcome::Converts))
    }

    /// [`Self::enqueue`] at `relation`.
    fn enqueue_at(&mut self, relation: Relation, type_: Term, this: Term, that: Term) {
        let posed = Keyed::new(Posed {
            type_: type_.clone(),
            this: this.clone(),
            that: that.clone(),
            relation,
            pairs: self.pairs.clone(),
        });
        if !self.posed.insert(posed) {
            return;
        }

        self.pending.push_back(Problem {
            type_,
            this,
            that,
            binders: self.binders.clone(),
            pairs: self.pairs.clone(),
            relation,
        });
    }

    /// Stand what this step poses from here on inside one more unfolding: of `this` against `that` where both were opened, and of the one that was where the other has nothing to open. Every step that opens a recursive definition poses what it opened through here, so a problem posed inside an unfolding is never the problem posed outside it ([`Posed`]).
    fn unfolded(&mut self, this: Option<&Term>, that: Option<&Term>) {
        self.pairs = match (this, that) {
            (Some(this), Some(that)) => self.pairs.unfolding(this.clone(), that.clone()),
            (Some(head), None) | (None, Some(head)) => self.pairs.opening(head.clone()),
            (None, None) => return,
        };
    }

    /// Enqueue the bodies of two arity-3 cons arms (`Bin`/`List`) for comparison, opened under shared fresh binders for `(head, tail, ih)` at the carrier's domains and the hypothesis ([`Convert::cons_binders`]).
    fn compare_cons_three(
        &mut self,
        context: &mut Context,
        domains: Vec<Term>,
        result: &MatchResult,
        this: Scope<Three>,
        that: Scope<Three>,
    ) {
        let o = self.cons_binders(context, domains, result);
        self.enqueue(
            Term::type_ground(),
            this.open(&[&o[0], &o[1], &o[2]]),
            that.open(&[&o[0], &o[1], &o[2]]),
        );
    }

    fn dequeue(&mut self, context: &Context) -> Result<Option<Problem>, ReduceError> {
        context.spend(Cost::STEP)?;

        Ok(self.pending.pop_front())
    }

    /// Two function types, the domains as equations whatever is asked of the types, and the codomains at `codomain`: below where the two types are, which is what makes subsumption a relation over the function type and no property of who walks it.
    fn compare_func_type(
        &mut self,
        context: &mut Context,
        this: FuncType,
        that: FuncType,
        codomain: Relation,
    ) -> Result<bool, ReduceError> {
        // Plicity is part of a function type's identity and calling convention: an explicit slot and an implicit/witness slot are *not* convertible even when their domains and results are, so that a convertible annotation or alias can never reinterpret which binders elaboration inserts. Compare the whole plicity vector (arity included) up front — the telescope walk below then compares the dependent domains.
        if !this.telescope.same_marks(&that.telescope) {
            return Ok(false);
        }
        // Both telescopes opened at one shared variable per binder, each entry once, so the dependent domains speak of the same binder without either side's tail being rewritten per binder.
        let mut walk = Lockstep::new(&this.telescope, &that.telescope);
        loop {
            match walk.step() {
                Step::Entries { left, right, .. } => {
                    let domain = left.clone();
                    self.enqueue(Term::type_ground(), left, right);
                    walk.advance_fresh(|hint| self.opening(context, hint, Some(domain.clone())));
                }
                Step::Bodies(left, right) => {
                    self.enqueue_at(codomain, Term::type_ground(), left, right);
                    return Ok(true);
                }
                Step::Mismatch => return Ok(false),
            }
        }
    }

    /// η-frame at a lambda: open its telescope at fresh argument variables (recorded openings — see [`Convert::opening`]), each at the domain the lambda annotates it with, and recover the codomain from `type_` after instantiating them, falling back to `Type` when `type_` does not reduce to a function type. Hands back the variables, the lambda's body at them, and that codomain.
    fn func_eta_args(
        &mut self,
        context: &mut Context,
        telescope: &Telescope<Term>,
        type_: Term,
    ) -> Result<(Vec<Term>, Term, Term), ReduceError> {
        let (ys, body) = self.open_telescope(context, telescope)?;
        let output_type = match Term::unwrap_or_clone(reduce(context, type_)?) {
            Subterm::FuncType(FuncType { telescope, .. }) => {
                telescope.open(&ys.iter().collect::<Vec<_>>())
            }
            _ => Term::type_ground(),
        };
        Ok((ys, body, output_type))
    }

    fn compare_func(
        &mut self,
        context: &mut Context,
        this: Func,
        that: Func,
        type_: Term,
    ) -> Result<bool, ReduceError> {
        // Plicity is part of a function's canonical identity. Well-typed functions compared at the same function type necessarily agree, but the explicit check preserves the Core invariant and rejects malformed or pre-elaboration terms that reach conversion unexpectedly.
        if !this.telescope.same_marks(&that.telescope) {
            return Ok(false);
        }
        let (ys, this_body, output_type) = self.func_eta_args(context, &this.telescope, type_)?;
        let y_refs = ys.iter().collect::<Vec<_>>();
        self.enqueue(output_type, this_body, that.telescope.open(&y_refs));

        Ok(true)
    }

    /// The relation's own rules, for a problem at below, both sides read forced as the kernel's `subsumes` reads them: two sorts of one quality add `lower ≤ upper`, and two function types of one plicity vector pose each domain as an equation and their codomains at below. Answers whether they decided the problem. A `Prop` against a `Type` is none of them, so it goes on as the equation it is not.
    ///
    /// **A sort left to inference takes the quality of the first sort it meets.** The sort of a type the elaborator left to inference — an unannotated binder's, a motive's result, a parked term's — has a quality nothing has stated ([`Context::fresh_sort`]), and holding it to a `Prop` or to a `Type` solves it ([`Context::same_quality`]), as holding it to another such sort makes the two one. The kernel has no such rule and needs none: a quality still open where its declaration closes is a type's, so no elaborated term holds one.
    ///
    /// Any other pair is the equation it is, taken from its written spelling through every step an equation takes: that is sufficient for a subsumption, and it is the whole of one wherever neither side can come to a sort or a function type. A problem those steps leave waiting on a metavariable keeps the relation it was asked at, so it resumes here.
    fn decide_below(
        &mut self,
        context: &mut Context,
        this: &Term,
        that: &Term,
    ) -> Result<bool, ReduceError> {
        let lower = reduce_forced(context, this.clone())?;
        let upper = reduce_forced(context, that.clone())?;

        match (&*lower, &*upper) {
            (Subterm::Sort(below, lower), Subterm::Sort(above, upper)) => {
                // No proposition stands where a type is wanted: two sorts are related where they are one quality, which solves one still open, and each is cumulative in its own levels.
                if !context.same_quality_at(*below, *above, self.extent) {
                    return Ok(false);
                }
                // What is put below is named for the report: a proposition's level and a type's are told apart by nothing else on the path.
                let kind = match context.universes().quality(*below) {
                    Quality::Prop => UniverseConstraintKind::PropositionCumulativity,
                    _ => UniverseConstraintKind::Cumulativity,
                };
                context
                    .universes_mut()
                    .add_leq(
                        lower.clone(),
                        upper.clone(),
                        UniverseConstraintOrigin {
                            extent: self.extent,
                            kind,
                            declaration: None,
                            binder: None,
                        },
                    )
                    .map_err(ReduceError::Universe)?;

                Ok(true)
            }
            // Plicity is part of a function type's identity, so two of different marks are no codomain question: they go on as the equation, which refuses them.
            (Subterm::FuncType(lower), Subterm::FuncType(upper))
                if lower.telescope.same_marks(&upper.telescope) =>
            {
                self.compare_func_type(context, lower.clone(), upper.clone(), Relation::Below)
            }
            _ => Ok(false),
        }
    }

    /// The value a folded recursive application forces to, or the one-step unfolding when forcing reaches no value — `None` only when neither moves.
    ///
    /// Forcing first is what the kernel does on both sides of every comparison, and on a closed call it is one machine run. The fallback keeps what the one-step rule bought: a call restuck on a variable comes back folded from the force, and unfolding it once still lets the comparison meet a written body.
    fn force_folded_call(context: &mut Context, apply: Apply) -> Result<Option<Term>, ReduceError> {
        let folded: Term = Subterm::Apply(apply.clone()).into();
        let forced = reduce_forced(context, folded.clone())?;
        if forced != folded {
            return Ok(Some(forced));
        }

        unfold_rec_apply(context, apply)
    }

    fn compare_apply(
        &mut self,
        context: &mut Context,
        this: Apply,
        that: Apply,
    ) -> Result<bool, ReduceError> {
        if this.arguments.len() != that.arguments.len() {
            return Ok(false);
        }

        self.enqueue(Term::type_ground(), this.head.clone(), that.head.clone());
        self.compare_spines(context, this, that)?;

        Ok(true)
    }

    /// The arguments of two applications of one length, each pair posed at the type the left head's telescope gives it: what is left of two applications whose heads are posed, or held one.
    fn compare_spines(
        &mut self,
        context: &mut Context,
        this: Apply,
        that: Apply,
    ) -> Result<(), ReduceError> {
        // Recover the real argument types from the head's function type so η fires at the correct type (e.g. a unit-typed argument is compared at `()`, where proof irrelevance makes distinct neutrals equal). Falls back to `Term::type_ground()` when the head's type is unavailable.
        let param_types = apply_param_types(context, &self.binders, &this.head, &this.arguments)?;

        for (i, (a, b)) in this
            .arguments
            .into_iter()
            .map(|argument| argument.term)
            .zip(that.arguments.into_iter().map(|argument| argument.term))
            .enumerate()
        {
            let type_ = param_types
                .as_ref()
                .and_then(|types| types.get(i).cloned())
                .unwrap_or_else(Term::type_ground);
            self.enqueue(type_, a, b);
        }

        Ok(())
    }

    /// What two recursive groups are, compared as the binders they are ([`Self::compare_rec`]): member for member their types and their bodies, every member of both opened at one shared label, so no member is unfolded and nothing recurs. It is asked in a run of its own, under the binders and inside the pairs of the problem that asks, so a pair it does not hold leaves nothing behind and goes on to what its caller does next, and only where neither group binds a universe of its own, as the kernel asks it. A run that ends blocked has decided nothing, and says so: its caller neither holds the pair nor refuses it on that.
    fn two_groups(
        context: &mut Context,
        binders: &Binders,
        pairs: &Pairs,
        this: &Rec,
        that: &Rec,
    ) -> Result<Outcome, ReduceError> {
        if this.group.universe_context().width() != 0 || that.group.universe_context().width() != 0
        {
            return Ok(Outcome::Mismatch(None));
        }

        let mark = context.solution_mark();
        let mut run = Self::under(binders.clone(), pairs.clone(), None);
        let outcome = match run.compare_rec(context, this.clone(), that.clone())? {
            true => run.outcome(context)?,
            false => Outcome::Mismatch(None),
        };
        match &outcome {
            Outcome::Converts => context.end_solutions(mark),
            Outcome::Mismatch(_) | Outcome::Blocked(_) => context.rollback_solutions(mark),
        }

        Ok(outcome)
    }

    /// Two applications of one global definition decided by their spines, on the raw spellings before either side is reduced. Two calls whose arguments are equal are equal, and reduction, which unfolds a `let`-defined head into its body on both sides, may leave the two where no rule sees it: a proof the calls differ in lands in a stuck match's scrutinee. A `rec` member's application survives reduction as a folded neutral and reaches [`Self::compare_same_rec_apply`], which compares spines before it unfolds; this is the same rule for a head that does not survive, so what a program's conversion accepts cannot depend on whether a definition happened to name itself (a fold through `; ih` names nothing, and the corpus fixture's `/big_nat/trim` is one). Congruence is sufficient and never necessary: agreeing spines decide the problem, and a spine that mismatches or blocks decides nothing — the problem falls through to the reduced comparison, where a genuinely different argument may still produce the same value.
    ///
    /// **The spines decide a pair only where they agree with no metavariable solved, until its term holds them.** `trim(?t)` against `trim(X)` holds at `?t := X`, and wherever else `trim` takes that value: one answer among those a function that is not injective has, so a choice, and a choice is the pair's term's to take where it has read its parts. Taken where the pair is first met, of two pairs that give one implicit two answers the first would decide the call. So a pair whose spines hold a metavariable and do not agree as they stand answers [`Spun::Open`]: what it reduces to is decided as a run of its own, which solves what it forces — a definition that unfolds to a type former gives its arguments back — and where that is undecided the pair parks whole, under its raw spelling. The term that awaits it asks it again with spines held ([`hold_spines`], [`Context::holding_spines`]), and agreement may then rest on what it solves, under an instance head as under a monomorphic one.
    ///
    /// A `use` slot still open is never the spines' to solve: it would take whatever dictionary the other spine names, and under a definition that drops its premise — `let Shelf(b: Bool, use Key(Pick(b))) -> Type = List(Pick(b));` — that is a dictionary the same call takes no notice of where its implicit is written (`documentation/design/surface/concepts-resolve-with-global-coherence.md`). Where spines are held it is passed over, and the pair then goes on to reduction with what its other positions solved. Left open, the slot is resolved as every other is; and where the definition keeps its dictionary the reduced types hold it, and comparing them solves the slot there.
    ///
    /// Nor may accepting a pair depend on whether the definition is universe-polymorphic, which one mentioning `Eq` is: its occurrences are instances of one global at levels minted per occurrence. So an instance head qualifies too, on two conditions that keep it an acceptance. Nothing in either spine may be flexible where spines are not held, because extending the rule to every polymorphic function and type former changes what `/std` elaborates to. And the heads are identified *after* the spines, because identifying two instances commits their levels equal — a commitment made only once the spines have earned it, never on the strength of an attempt that falls through, which is the reading `documentation/design/soundness/case-equations-and-their-key.md` gives a speculative match. The kernel compares the same pair the same way, before it unfolds either side.
    fn compare_same_global_apply(
        context: &mut Context,
        binders: &Binders,
        pairs: &Pairs,
        this_term: &Term,
        that_term: &Term,
    ) -> Result<Spun, ReduceError> {
        // What agreement may rest on: nothing solved, unless a completing term is holding the spines of a pair it awaits.
        let holding = context.holds_spines();
        // Each side as the solutions already committed spell it. A side a solved metavariable stands for is the call it was solved to, and read as written this rule would not see it: the verdict would follow whether an implicit was stated or inferred. Only a side that is such a metavariable is read through; an argument one spells is resolved by the comparison it is handed to.
        let through = |term: &Term| match &**term {
            Subterm::Metavar(_) => Some(zonk_solved_term_metas(context, term)),
            _ => None,
        };
        let (this_solved, that_solved) = (through(this_term), through(that_term));
        let this_term = this_solved.as_ref().unwrap_or(this_term);
        let that_term = that_solved.as_ref().unwrap_or(that_term);

        let (Subterm::Apply(this), Subterm::Apply(that)) = (&**this_term, &**that_term) else {
            return Ok(Spun::Apart);
        };
        if this.arguments.len() != that.arguments.len() {
            return Ok(Spun::Apart);
        }
        let global = |head: &Term| match &**applied_head(head) {
            Subterm::Var(var) => var
                .as_free()
                .and_then(Free::as_global)
                .map(|global| (*global, false)),
            Subterm::Instance(Instance {
                head: InstanceHead::Var(var),
                ..
            }) => var
                .as_free()
                .and_then(Free::as_global)
                .map(|global| (*global, true)),
            _ => None,
        };
        let (Some((this_global, polymorphic)), Some((that_global, _))) =
            (global(&this.head), global(&that.head))
        else {
            return Ok(Spun::Apart);
        };
        if this_global != that_global {
            return Ok(Spun::Apart);
        }
        // Spines that do not agree may yet, where one holds a metavariable nothing has solved. A type former's are not read so: its node keeps its arguments, so what reduction leaves of the pair is each argument against its own, and each waits as what it is.
        let flexible = this_term.has_metavar() || that_term.has_metavar();
        let former = context.induct_decl(&this_global).is_some()
            || context.struct_decl(&this_global).is_some();
        let apart = |context: &Context| {
            let open = flexible
                && !former
                && (zonk_solved_term_metas(context, this_term).has_metavar()
                    || zonk_solved_term_metas(context, that_term).has_metavar());
            match open {
                true => Spun::Open,
                false => Spun::Apart,
            }
        };
        if !holding && polymorphic && flexible {
            return Ok(apart(context));
        }

        let param_types = apply_param_types(context, binders, &this.head, &this.arguments)?;
        let mark = context.solution_mark();
        let committed = context.solutions_committed();
        let mut converts = polymorphic
            || matches!(
                convert_under(
                    context,
                    binders,
                    pairs,
                    &Term::type_ground(),
                    &this.head,
                    &that.head
                )?,
                Outcome::Converts
            );
        let open_slot = |context: &Context, term: &Term| matches!(&**term, Subterm::Metavar(metavar) if matches!(metavar.origin, MetavarOrigin::Witness(_)) && context.metavar_solution(metavar.id).is_none());
        let mut passed_over = false;
        for (index, (a, b)) in this.arguments.iter().zip(&that.arguments).enumerate() {
            if !converts {
                break;
            }
            // A `use` slot still open is passed over where spines are held: it is resolution's to fill, or reduction's where the definition keeps its dictionary, and held to the other spine's it would take a dictionary the call takes no notice of where its implicit is written.
            if holding
                && a.plicity == Plicity::Witness
                && (open_slot(context, &a.term) || open_slot(context, &b.term))
            {
                passed_over = true;
                continue;
            }
            let param_type = param_types
                .as_ref()
                .and_then(|types| types.get(index).cloned())
                .unwrap_or_else(Term::type_ground);
            converts = matches!(
                convert_under(context, binders, pairs, &param_type, &a.term, &b.term)?,
                Outcome::Converts
            );
        }
        // Agreement reached by solving a metavariable is one answer among those a function that is not injective has: no verdict of conversion's, until the pair's term asks for it. It does say the two calls are not apart.
        let solvable = converts && !holding && context.solutions_committed() != committed;
        converts &= !solvable;
        if polymorphic && converts {
            // A family's name applied in full by one spine is the node it builds, and a definition's is its value at those arguments: the two instances are identified by the name's vector. A curried head is identified as two heads are, and where that declines, the pair goes on to reduction and meets as what it reduces to.
            let instances = this_term
                .applied_in_full(&*context)
                .zip(that_term.applied_in_full(&*context))
                .filter(|_| matches!(&*this.head, Subterm::Instance(_)))
                .map(|((name, this), (_, that))| (*name, this.clone(), that.clone()));
            converts = match instances {
                Some((name, this, that)) => {
                    Self::compare_instance_levels(context, &name, &this, &that)?
                }
                None => {
                    identify_universe_levels(context, &this.head, &that.head, Curried::Declined)?
                        == Identification::Identified
                }
            };
        }
        if !converts {
            context.rollback_solutions(mark);
        }
        context.end_solutions(mark);

        // Past an open slot the spines have decided nothing of the pair, and what they solved elsewhere stands for the reduced comparison.
        Ok(match (converts && !passed_over, solvable && !former) {
            (true, _) => Spun::Agreed,
            (false, true) => Spun::Solvable,
            (false, false) => apart(context),
        })
    }

    /// Compare applications of the same structural recursive member without assuming that member is injective. Convertible spines establish the result by congruence. A genuinely different spine instead triggers one symmetric delta step: distinct arguments may still produce equal results for a recursive function.
    fn compare_same_rec_apply(
        &mut self,
        context: &mut Context,
        this: Apply,
        that: Apply,
        type_: Term,
    ) -> Result<bool, ReduceError> {
        if this.arguments.len() != that.arguments.len() {
            return Ok(false);
        }

        let param_types = apply_param_types(context, &self.binders, &this.head, &this.arguments)?;
        let this_params = this.params().cloned().collect::<Vec<_>>();
        let that_params = that.params().cloned().collect::<Vec<_>>();
        let mut blocked = false;
        for (index, (a, b)) in this_params.iter().zip(&that_params).enumerate() {
            let param_type = param_types
                .as_ref()
                .and_then(|types| types.get(index).cloned())
                .unwrap_or_else(Term::type_ground);
            match convert_under(context, &self.binders, &self.pairs, &param_type, a, b)? {
                Outcome::Converts => {}
                Outcome::Blocked(_) => blocked = true,
                Outcome::Mismatch(_) => {
                    // A pair of calls is unfolded against itself once: inside its own unfolding it is apart, since unfolded again it poses itself again.
                    if self.pairs.hold(&this.head, &that.head) {
                        return Ok(false);
                    }
                    let (this_head, that_head) = (this.head.clone(), that.head.clone());
                    let this = unfold_rec_apply(context, this)?;
                    let that = unfold_rec_apply(context, that)?;
                    return match (this, that) {
                        (Some(this), Some(that)) => {
                            self.unfolded(Some(&this_head), Some(&that_head));
                            self.enqueue(type_, this, that);
                            Ok(true)
                        }
                        _ => Ok(false),
                    };
                }
            }
        }

        match blocked {
            true => self.compare_apply(context, this, that),
            false => Ok(true),
        }
    }

    fn compare_tuple_type(
        &mut self,
        context: &mut Context,
        this: TupleType,
        that: TupleType,
    ) -> Result<bool, ReduceError> {
        // Field labels are part of a tuple type's identity: `{ a : Nat } ≢ { Nat } ≢ { b : Nat }` (the unlabeled "" is just another label). This is deliberately tuple-only — function-type parameter names stay alpha-convertible (see `compare_func_type`, where the hint feeds freshness, never equality). Equal label lists also settle the arity, so the walk below never meets a mismatch.
        if this.telescope.labels() != that.telescope.labels() {
            return Ok(false);
        }
        let mut walk = Lockstep::new(&this.telescope, &that.telescope);
        while let Step::Entries { left, right, .. } = walk.step() {
            let field = left.clone();
            self.enqueue(Term::type_ground(), left, right);
            walk.advance_fresh(|hint| self.opening(context, hint, Some(field.clone())));
        }
        Ok(true)
    }

    fn compare_tuple(
        &mut self,
        context: &mut Context,
        this: Tuple,
        that: Tuple,
        type_: Term,
    ) -> Result<bool, ReduceError> {
        let n = this.fields.len();
        if n != that.fields.len() {
            return Ok(false);
        }

        let cur = match Term::unwrap_or_clone(reduce(context, type_)?) {
            Subterm::TupleType(TupleType { telescope, .. }) if telescope.len() == n => {
                Some(telescope)
            }
            _ => None,
        };

        self.enqueue_fields(this.fields, that.fields, cur);

        Ok(true)
    }

    fn compare_proj(&mut self, this: Proj, that: Proj) -> Result<bool, ReduceError> {
        if this.field != that.field {
            return Ok(false);
        }
        self.enqueue(Term::type_ground(), this.head, that.head);
        Ok(true)
    }

    fn compare_induct_type(
        &mut self,
        context: &mut Context,
        this: InductType,
        that: InductType,
    ) -> Result<bool, ReduceError> {
        if this.name != that.name
            || this.params.len() != that.params.len()
            || this.indices.len() != that.indices.len()
        {
            return Ok(false);
        }
        if !Self::compare_instance_levels(context, &this.name, &this.universes, &that.universes)? {
            return Ok(false);
        }

        // Recover the declared types from the registry so each argument compares at its own type rather than a flat `Type`. When an index is a proof, that type is a proposition and irrelevance applies — two instances of a family indexed by a proof convert whatever the proofs. Falls back to `Type` if the inductive is somehow absent or arity-mismatched.
        let arity = match context.induct_decl(&this.name).cloned() {
            Some(induct_decl) => Some(instantiate_bound_at(
                context,
                &induct_decl.universe_context,
                &induct_decl.arity,
                &this.universes,
            )?),
            None => None,
        };
        // The parameters against the arity, then the indices against the telescope it terminates in — the same order the arguments are compared in below.
        let arg_types = match arity {
            Some(arity) if arity.len() == this.params.len() => {
                let mut types = Vec::with_capacity(this.params.len() + this.indices.len());
                let indices = arity.walk(&this.params, |_, _, ty| -> Result<(), ReduceError> {
                    types.push(ty.clone());
                    Ok(())
                })?;

                match indices.len() == this.indices.len() {
                    true => {
                        indices.walk(&this.indices, |_, _, ty| -> Result<(), ReduceError> {
                            types.push(ty.clone());
                            Ok(())
                        })?;
                        Some(types)
                    }
                    false => None,
                }
            }
            _ => None,
        };

        let args = this
            .params
            .into_iter()
            .chain(this.indices)
            .zip(that.params.into_iter().chain(that.indices));
        for (i, (a, b)) in args.enumerate() {
            let type_ = arg_types
                .as_ref()
                .and_then(|types| types.get(i).cloned())
                .unwrap_or_else(Term::type_ground);
            self.enqueue(type_, a, b);
        }

        Ok(true)
    }

    /// Enqueue each `this`/`that` field pair at the type the telescope assigns it (each `rest` opened at the field value), defaulting to `Type` once the telescope is exhausted or absent. Shared by `compare_variant`, `compare_struct`, and `compare_tuple`: η and proof irrelevance must fire per field rather than at a flat `Type`.
    fn enqueue_fields<B: Bound>(
        &mut self,
        this: Vec<Term>,
        that: Vec<Term>,
        telescope: Option<Telescope<B>>,
    ) {
        let mut cursor = telescope.as_ref().map(Telescope::cursor);
        for (a, b) in this.into_iter().zip(that) {
            let entry = cursor.as_ref().and_then(Cursor::entry);
            let type_ = match (entry, cursor.as_mut()) {
                (Some((_, ty)), Some(cursor)) => {
                    cursor.advance(a.clone());
                    ty
                }
                _ => Term::type_ground(),
            };
            self.enqueue(type_, a, b);
        }
    }

    fn compare_variant(
        &mut self,
        context: &mut Context,
        this: Variant,
        that: Variant,
    ) -> Result<bool, ReduceError> {
        if this.name != that.name
            || this.tag != that.tag
            || this.params.len() != that.params.len()
            || this.payload.len() != that.payload.len()
        {
            return Ok(false);
        }
        if !Self::compare_instance_levels(context, &this.name, &this.universes, &that.universes)? {
            return Ok(false);
        }

        // Recover the payload types from the constructor's registry telescope (instantiated at this side's parameters) so each payload field compares at its own type — η and proof irrelevance fire per field, instead of a structural compare at `Type`. Same recovery `erase` uses; falls back to `Type` if the inductive is somehow absent.
        let telescope = match context.induct_decl(&this.name).cloned() {
            Some(induct_decl) => {
                let constructor = induct_decl.constructor(&this.tag);
                match constructor {
                    Some(constructor) => Some(
                        instantiate_bound_at(
                            context,
                            &induct_decl.universe_context,
                            &constructor.telescope,
                            &this.universes,
                        )?
                        .open_params(&this.params),
                    ),
                    None => None,
                }
            }
            None => None,
        };

        for (a, b) in this.params.into_iter().zip(that.params) {
            self.enqueue(Term::type_ground(), a, b);
        }

        self.enqueue_fields(this.payload, that.payload, telescope);

        Ok(true)
    }

    fn compare_struct_type(
        &mut self,
        context: &mut Context,
        this: StructType,
        that: StructType,
    ) -> Result<bool, ReduceError> {
        if this.name != that.name || this.params.len() != that.params.len() {
            return Ok(false);
        }
        if !Self::compare_instance_levels(context, &this.name, &this.universes, &that.universes)? {
            return Ok(false);
        }

        for (a, b) in this.params.into_iter().zip(that.params) {
            self.enqueue(Term::type_ground(), a, b);
        }

        Ok(true)
    }

    fn compare_struct(
        &mut self,
        context: &mut Context,
        this: Struct,
        that: Struct,
    ) -> Result<bool, ReduceError> {
        if this.name != that.name
            || this.params.len() != that.params.len()
            || this.fields.len() != that.fields.len()
        {
            return Ok(false);
        }
        if !Self::compare_instance_levels(context, &this.name, &this.universes, &that.universes)? {
            return Ok(false);
        }

        // Recover the field types from the registry (instantiated at this side's parameters) so each field compares at its own type — η and proof irrelevance fire per field, as `compare_tuple` does with the tuple type's telescope. Falls back to `Type` if the struct is somehow absent.
        let telescope = match context.struct_decl(&this.name).cloned() {
            Some(struct_decl) => Some(
                instantiate_bound_at(
                    context,
                    &struct_decl.universe_context,
                    &struct_decl.arity,
                    &this.universes,
                )?
                .open(&this.params.iter().collect::<Vec<_>>()),
            ),
            None => None,
        };

        for (a, b) in this.params.into_iter().zip(that.params) {
            self.enqueue(Term::type_ground(), a, b);
        }

        self.enqueue_fields(this.fields, that.fields, telescope);

        Ok(true)
    }

    /// Two stuck eliminations, part by part. The motive's and the arms' binders are opened at the types their position gives them, read off the left elimination's scrutinee, which the right one's is posed as equal to: a constructor's arm at its telescope over the scrutinee's parameters, a cons arm at its carrier's domains and its hypothesis ([`Convert::cons_binders`]), a motive's as [`Convert::motive_binders`] opens them. So a proof an arm binds is one where the arm's body is compared, as it is in the kernel. Where no lookup types the scrutinee, or its family states no constructor of the arm's arity, the binders carry no type. An arm's body is compared at `Type`: with its binders typed, a lookup reads what a type directs between two neutrals in it, a literal opens its own eta, and two literals are compared by their parts.
    fn compare_match(
        &mut self,
        context: &mut Context,
        this: Match,
        that: Match,
    ) -> Result<bool, ReduceError> {
        let head = this.head.clone();
        let result = this.result.clone();
        // Where the two matches state the instance they eliminate, the instances are posed as equal, the scrutinees at the instance, a typed position where two proofs are one unread, and the instance is the scrutinee's type with no lookup, as the kernel takes it. One that states an instance and one that states none are two terms.
        let scrutinee = match (this.cases.stated().cloned(), that.cases.stated().cloned()) {
            (Some(this_stated), Some(that_stated)) => {
                self.enqueue(Term::type_ground(), this_stated.clone(), that_stated);
                self.enqueue(this_stated.clone(), this.head, that.head);

                Some(reduce_forced(context, this_stated)?)
            }
            (None, None) => {
                let scrutinee = match looked_up(context, &self.binders, &head)? {
                    Some(type_) => Some(reduce_forced(context, type_)?),
                    None => None,
                };
                self.enqueue(Term::type_ground(), this.head, that.head);

                scrutinee
            }
            _ => return Ok(false),
        };

        // A motive's arity is part of its shape — 1 except for an annotated inductive-match motive (pattern binders then the scrutinee); different arities are structurally distinct. One source match reaches both forms: written over a variable it is an ambient goal, and that goal substituted at an expression — a definition's `match o` unfolded at `o := f(x)` — meets the family the same match elaborates to where it was written over `f(x)`. A family at the scrutinee itself *is* the elimination's type, so the two results compare at that instance.
        match (this.result, that.result) {
            (MatchResult::Family(this_motive), MatchResult::Family(that_motive)) => {
                if this_motive.arity() != that_motive.arity() {
                    return Ok(false);
                }

                let labels =
                    self.motive_binders(context, scrutinee.as_ref(), this_motive.arity())?;
                let label_refs = labels.iter().collect::<Vec<_>>();
                self.enqueue(
                    Term::type_ground(),
                    this_motive.open(&label_refs),
                    that_motive.open(&label_refs),
                );
            }
            (MatchResult::Ambient(this_goal), MatchResult::Ambient(that_goal)) => {
                self.enqueue(Term::type_ground(), this_goal, that_goal);
            }
            (MatchResult::Family(motive), MatchResult::Ambient(goal))
            | (MatchResult::Ambient(goal), MatchResult::Family(motive)) => {
                let Some(at_head) = family_at_head(context, &motive, &head)? else {
                    return Ok(false);
                };
                self.enqueue(Term::type_ground(), at_head, goal);
            }
        }

        match (this.cases, that.cases) {
            (
                Cases::Bool {
                    false_case: this_false,
                    true_case: this_true,
                },
                Cases::Bool {
                    false_case: that_false,
                    true_case: that_true,
                },
            ) => {
                self.enqueue(Term::type_ground(), this_false, that_false);
                self.enqueue(Term::type_ground(), this_true, that_true);
                Ok(true)
            }

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

                for ((kl, vl), (kr, vr)) in this_cases.into_iter().zip(that_cases) {
                    if kl != kr {
                        return Ok(false);
                    }
                    self.enqueue(Term::type_ground(), vl, vr);
                }

                self.enqueue(Term::type_ground(), this_default, that_default);
                Ok(true)
            }

            // The pattern is elaboration-time data, fully reflected in the motive scope once checked — convertibility ignores it. The default, though, is a real arm: two matches convert only if their catch-alls agree in presence and body.
            (
                Cases::Induct {
                    cases: this_cases,
                    default: this_default,
                    ..
                },
                Cases::Induct {
                    cases: that_cases,
                    default: that_default,
                    ..
                },
            ) => {
                if this_cases.len() != that_cases.len() {
                    return Ok(false);
                }
                let family = match scrutinee.as_deref() {
                    Some(Subterm::InductType(InductType {
                        name,
                        universes,
                        params,
                        ..
                    })) => declared_at(context, name, universes, params)?
                        .map(|declaration| (declaration, params.clone())),
                    _ => None,
                };

                for ((this_atom, this_scope), (that_atom, that_scope)) in
                    this_cases.into_iter().zip(that_cases)
                {
                    if this_atom != that_atom || this_scope.arity() != that_scope.arity() {
                        return Ok(false);
                    }

                    let signature = family
                        .as_ref()
                        .and_then(|(declaration, params)| {
                            declaration.instantiate(&this_atom, params)
                        })
                        .filter(|signature| signature.len() == this_scope.arity());
                    let binders = match signature {
                        Some(signature) => self.open_telescope(context, &signature)?.0,
                        None => (0..this_scope.arity())
                            .map(|_| Term::free_var(&self.opening(context, None, None)))
                            .collect::<Vec<_>>(),
                    };
                    let binder_refs = binders.iter().collect::<Vec<_>>();

                    self.enqueue(
                        Term::type_ground(),
                        this_scope.open(&binder_refs),
                        that_scope.open(&binder_refs),
                    );
                }

                match (this_default, that_default) {
                    (None, None) => {}
                    (Some(this), Some(that)) => self.enqueue(Term::type_ground(), this, that),
                    _ => return Ok(false),
                }

                Ok(true)
            }

            (
                Cases::FreeMonoid {
                    carrier: this_carrier,
                },
                Cases::FreeMonoid {
                    carrier: that_carrier,
                },
            ) => {
                let domains = this_carrier.cons_domains();

                match (this_carrier, that_carrier) {
                    (
                        Carrier::Nat {
                            empty_case: this_empty,
                            cons_case: this_cons,
                        },
                        Carrier::Nat {
                            empty_case: that_empty,
                            cons_case: that_cons,
                        },
                    ) => {
                        self.enqueue(Term::type_ground(), this_empty, that_empty);

                        // The unary cons arm binds (predecessor, ih); open both under shared fresh binders and compare the bodies.
                        let o = self.cons_binders(context, domains, &result);
                        self.enqueue(
                            Term::type_ground(),
                            this_cons.open(&[&o[0], &o[1]]),
                            that_cons.open(&[&o[0], &o[1]]),
                        );

                        Ok(true)
                    }
                    (
                        Carrier::Bin {
                            empty_case: this_empty,
                            cons_case: this_cons,
                            ..
                        },
                        Carrier::Bin {
                            empty_case: that_empty,
                            cons_case: that_cons,
                            ..
                        },
                    ) => {
                        self.enqueue(Term::type_ground(), this_empty, that_empty);
                        self.compare_cons_three(context, domains, &result, this_cons, that_cons);

                        Ok(true)
                    }
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
                    ) => {
                        self.enqueue(Term::type_ground(), this_elem, that_elem);
                        self.enqueue(Term::type_ground(), this_empty, that_empty);
                        self.compare_cons_three(context, domains, &result, this_cons, that_cons);

                        Ok(true)
                    }
                    // Distinct carriers are never structurally convertible.
                    _ => Ok(false),
                }
            }

            // Unreachable under the dispatch guard (same `Cases` discriminant); distinct kinds are never structurally convertible.
            _ => Ok(false),
        }
    }

    fn compare_rec(
        &mut self,
        context: &mut Context,
        this: Rec,
        that: Rec,
    ) -> Result<bool, ReduceError> {
        if this.group.length() != that.group.length() {
            return Ok(false);
        }

        let names = (0..this.group.length())
            .map(|_| self.opening(context, None, None))
            .collect::<Vec<_>>();
        let labels = names.iter().map(Term::free_var).collect::<Vec<_>>();
        let labels = labels.iter().collect::<Vec<_>>();
        // Each member at the type the group states for it, which may name every member.
        for (name, member) in names.iter().zip(this.group.iter()) {
            self.binders = self.binders.opening(*name, member.type_.open(&labels));
        }

        for (this_member, that_member) in this.group.iter().zip(that.group.iter()) {
            let (this_type, this_body) = (&this_member.type_, &this_member.body);
            let (that_type, that_body) = (&that_member.type_, &that_member.body);
            self.enqueue(
                Term::type_ground(),
                this_type.open(&labels),
                that_type.open(&labels),
            );
            self.enqueue(
                Term::type_ground(),
                this_body.open(&labels),
                that_body.open(&labels),
            );
        }

        self.enqueue(
            Term::type_ground(),
            this.tail.open(&labels),
            that.tail.open(&labels),
        );

        Ok(true)
    }

    fn eta_expand_func(
        &mut self,
        context: &mut Context,
        func: Func,
        other: Term,
        type_: Term,
    ) -> Result<bool, ReduceError> {
        let type_ = reduce(context, type_)?;
        if !neutral(context, &other)
            || (!matches!(&*type_, Subterm::FuncType(_)) && another_former(context, &type_))
        {
            return Ok(false);
        }

        let (ys, body, output_type) = self.func_eta_args(context, &func.telescope, type_)?;
        self.enqueue(output_type, body, Term::apply(other, ys));
        Ok(true)
    }

    fn eta_expand_tuple(
        &mut self,
        context: &mut Context,
        tuple: Tuple,
        other: Term,
        type_: Term,
    ) -> Result<bool, ReduceError> {
        let n = tuple.fields.len();
        // A literal with no field has no eta, as the kernel's has none: answering here would say every neutral at `{}` is `()`.
        if n == 0 || !neutral(context, &other) {
            return Ok(false);
        }

        let type_ = reduce(context, type_)?;
        let cur = match &*type_ {
            Subterm::TupleType(TupleType { telescope, .. }) if telescope.len() == n => {
                Some(telescope.clone())
            }
            _ if another_former(context, &type_) => return Ok(false),
            _ => None,
        };

        let projections = (0..n)
            .map(|i| Term::proj(other.clone(), i))
            .collect::<Vec<_>>();
        self.enqueue_fields(tuple.fields, projections, cur);

        Ok(true)
    }

    fn eta_expand_struct(
        &mut self,
        context: &mut Context,
        struct_: Struct,
        other: Term,
        type_: Term,
    ) -> Result<bool, ReduceError> {
        let n = struct_.fields.len();
        if !neutral(context, &other) {
            return Ok(false);
        }

        // Recover the field types from the registry, exactly as `compare_struct` does — a `Struct` value carries no telescope of its own, unlike a `Tuple`'s inline `TupleType`.
        let type_ = reduce(context, type_)?;
        let cur = match &*type_ {
            Subterm::StructType(StructType {
                name,
                universes,
                params,
            }) if *name == struct_.name => match context.struct_decl(name).cloned() {
                Some(struct_decl) => Some(
                    instantiate_bound_at(
                        context,
                        &struct_decl.universe_context,
                        &struct_decl.arity,
                        universes,
                    )?
                    .open(&params.iter().collect::<Vec<_>>()),
                ),
                None => None,
            },
            _ if another_former(context, &type_) => return Ok(false),
            _ => None,
        };

        // A literal holding no field that is data has no eta, as the kernel's has none: every comparison below would be a proof's or none at all, and any neutral would be the literal by the shape of its type.
        let declared = match &cur {
            Some(fields) => Some(fields.clone()),
            None => match context.struct_decl(&struct_.name).cloned() {
                Some(struct_decl) => Some(
                    instantiate_bound_at(
                        context,
                        &struct_decl.universe_context,
                        &struct_decl.arity,
                        &struct_.universes,
                    )?
                    .open(&struct_.params.iter().collect::<Vec<_>>()),
                ),
                None => None,
            },
        };
        if let Some(fields) = declared
            && !holds_data(context, &self.binders, fields, &struct_.fields)?
        {
            return Ok(false);
        }

        let projections = (0..n)
            .map(|i| Term::proj_struct(other.clone(), struct_.name, i))
            .collect::<Vec<_>>();
        self.enqueue_fields(struct_.fields, projections, cur);

        Ok(true)
    }

    fn eta_expand_neutral(
        &mut self,
        context: &mut Context,
        this: Term,
        that: Term,
        type_: Term,
    ) -> Result<bool, ReduceError> {
        match Term::unwrap_or_clone(reduce(context, type_)?) {
            Subterm::FuncType(FuncType { telescope, .. }) => {
                let (ys, output_type) = self.open_telescope(context, &telescope)?;
                self.enqueue(
                    output_type,
                    Term::apply(this, ys.clone()),
                    Term::apply(that, ys),
                );
                Ok(true)
            }
            // Each projection at the type its field has, opened at the left side's preceding projections, as the kernel's Σ eta compares them: a field at a proposition is decided by that type, where at `Type` two neutrals' projections never meet. The Σ with no field has no eta, here as in the kernel.
            Subterm::TupleType(TupleType { telescope, .. }) if !telescope.is_empty() => {
                let n = telescope.len();
                let projections = |term: &Term| {
                    (0..n)
                        .map(|i| Term::proj(term.clone(), i))
                        .collect::<Vec<_>>()
                };
                self.enqueue_fields(projections(&this), projections(&that), Some(telescope));
                Ok(true)
            }
            // A nominal struct has no eta between two neutrals, here as in the kernel: projecting both poses their heads again, which only two equal heads pass.
            _ => Ok(false),
        }
    }

    /// Solve `?id[spine] ≈ t` (the rigid side, already in weak-head normal form). Implements the pattern fragment: embedded-metavariable guard, occurs check, spine-as-renaming inversion (which subsumes the scope check), and re-validation against the frozen birth context, before committing the solution in birth-named form.
    fn solve(
        &mut self,
        context: &mut Context,
        metavar: &Metavar,
        t: &Term,
    ) -> Result<Solved, ReduceError> {
        curios_profile::profile!("convert::solve");
        let id = metavar.id;
        // No unknown of conversion's: a recursive group fills its slot and a parked problem its placeholder, and a problem that meets either waits for it.
        if context.owner_fills(id) {
            return Ok(Solved::Postponed);
        }

        // Materialize committed solutions in the candidate before any analysis. A solved metavariable's occurrence still spells the metavariable, and its spine names the frame it was born under — which may hold binders `id`'s frame lacks. Left unmaterialized, those spine names surface in `free_vars` and the scope check refuses a candidate whose *resolution* is perfectly scoped (a ground `Cell(Option(Nat))` refused for a continuation binder riding an already-solved metavariable's spine — `a_solved_metavariable_in_a_candidate_does_not_strand_the_wake_cascade` holds the case). Materializing also lets the occurs check see a cycle hidden behind a solved metavariable's solution, which the raw spelling conceals.
        // And spell the published recursive members it holds by their names: a candidate arrives reduced, a stuck call headed by its member's group, and committed so it would hold a copy of the callee for the re-validation below to elaborate again ([`named_members`]).
        // And read by value the `let`'s locals the occurrence does not name ([`by_value`]), ahead of the scope check that would fail the candidate for them.
        let t = &named_members(
            context,
            &by_value(context, metavar, zonk_solved_term_metas(context, t)),
        );
        let metavars = t.metavars();

        // Occurs check: a candidate mentioning `id` itself is an infinite solution.
        if metavars.contains(&id) {
            curios_profile::note!(target: "curios_elab::solve", meta = id.0, "failed: occurs check");
            return Ok(Solved::Failed);
        }

        // Embedded-metavariable guard: any *other* unsolved metavariable in the candidate may carry a wider context than `id`'s, so solving now could let the solution escape its scope. A metavariable whose birth context is *contained* in `id`'s provably cannot smuggle a name out: everything it can ever inject — its spine entries now, its own solution's names later, arriving only through that scope-checked spine — lies inside `id`'s scope already. The exemption is what lets a settle-synthesized lambda type, whose unannotated domains are metavariables minted at the settling expectation's own scope, commit instead of stranding.
        //
        // Nor can one whose birth context goes past `id`'s only by binders the candidate binds itself at each occurrence ([`Context::metavar_bound_by_candidate`]): the occurrence sits where the metavariable was born, so what it comes to names nothing the candidate does not bind. That is a settled lambda's codomain, born under the lambda's binders so that the body's type may name them, in the function type the lambda states.
        //
        // One not contained is *restricted* to `id`'s context instead of holding the candidate back ([`Context::metavar_restriction`]): solved to a stand-in born over `id`'s binders, under the refinements both were born under, and the candidate solved again through it — contained, so the retry does not come back here. Waiting would be for a solution that, once `id` embeds it, could only escape: an implicit minted under a match arm's `v` and embedded in the match's type would leave that type open until a later `read!` pinned it to the region's monad. What cannot be restricted still waits, and so does a candidate the retry does not commit, which leaves no restriction behind.
        let blocking = metavars
            .iter()
            .copied()
            .filter(|other| {
                context.metavar_solution(*other).is_none()
                    && !context.metavar_context_contained(*other, id)
                    && !context.metavar_bound_by_candidate(*other, &metavar_spines(t, *other), id)
            })
            .collect::<Vec<_>>();
        if !blocking.is_empty() {
            let occurrences = metavar_origins(&[t]);
            let restrictions = blocking
                .iter()
                .map(|other| {
                    let provenance = occurrences.get(other)?;
                    let spines = metavar_spines(t, *other);
                    let restriction =
                        context.metavar_restriction(*other, &provenance.0, &spines, metavar)?;
                    Some((*other, provenance.clone(), restriction))
                })
                .collect::<Option<Vec<_>>>();
            if let Some(restrictions) = restrictions {
                let mark = context.solution_mark();
                for (other, provenance, restriction) in restrictions {
                    context.restrict_metavar(other, provenance, restriction);
                }
                let retried = self.solve(context, metavar, t);
                let committed = matches!(retried, Ok(Solved::Done));
                if !committed {
                    context.rollback_solutions(mark);
                }
                context.end_solutions(mark);
                retried?;
                if committed {
                    return Ok(Solved::Done);
                }
            }

            // Record what blocked this candidate before postponing. A metavariable that never solves is reported by the item drain, and the drain has only the goal's own terms to look at — the blocker rides inside a definition body it cannot see, so the edge must be kept here or the cause is lost.
            context.note_solve_blockers(
                id,
                metavars
                    .iter()
                    .filter(|other| context.metavar_solution(**other).is_none())
                    .filter_map(|other| {
                        occurrences
                            .get(other)
                            .map(|(origin, extent)| (*other, origin.clone(), *extent))
                    })
                    .collect(),
            );

            curios_profile::note!(
                target: "curios_elab::solve",
                meta = id.0,
                blockers = %metavars
                    .iter()
                    .filter(|other| context.metavar_solution(**other).is_none())
                    .map(|other| {
                        let result = context
                            .metavar_entry(*other)
                            .map(|entry| format!("{}", entry.result))
                            .unwrap_or_else(|| "<no birth record>".into());
                        format!("?{} : {result}", other.0)
                    })
                    .collect::<Vec<_>>()
                    .join(", "),
                "postponed: embedded metavariable",
            );
            return Ok(Solved::Postponed);
        }

        let Some(entry) = context.metavar_entry(id) else {
            // No birth record (e.g. a synthesis-position hole that never reached a checking site): nothing to validate against, cannot solve.
            curios_profile::note!(target: "curios_elab::solve", meta = id.0, "failed: no birth record");
            return Ok(Solved::Failed);
        };
        let telescope = entry.telescope.clone();
        let born = entry.frames.clone();
        let result = entry.result.clone();

        // Every birthed occurrence carries its full spine: `elaborate_apply` opens telescopes with rebuilt arguments, so no lowered bare copy of a birthed hole survives into compared types. (An empty telescope's identity spine is legitimately empty.)
        assert_eq!(
            metavar.spine.len(),
            telescope.len(),
            "metavariable spine arity diverged from its birth telescope"
        );

        // Materialize *solved-metavariable* occurrences in the entries too — a solved entry stands for its (possibly variable) value, chains terminate because the occurs check forbids solution cycles, and a deep solved occurrence must take the same spelling the materialized candidate now carries or the abstraction subjects below would never match it. Entries are otherwise deliberately unreduced: a name backed by a definition must stay that name, or an obviously-invertible renaming looks flexible.
        let entries = metavar
            .spine
            .iter()
            .map(|term| zonk_solved_term_metas(context, term))
            .collect::<Vec<_>>();

        // Invert the spine through its *pattern* entries — a syntactic free variable whose name no other entry shares. A non-variable or duplicated entry is simply not invertible; the solution then may not depend on that slot, which the scope check below enforces — pruning in its simplest form.
        //
        // Each entry's multiplicity is counted once, over the spine, rather than by rescanning the spine per entry: a spine is as long as the birth telescope, which is every binder in scope, and a region a hundred `let`s deep solves a metavariable per `!` — rescanning would be a cubic term in a long chain's elaboration.
        let image: Vec<_> = {
            let names = entries
                .iter()
                .map(|term| match &**term {
                    Subterm::Var(var) => var.as_free(),
                    _ => None,
                })
                .collect::<Vec<_>>();
            let mut multiplicity = HashMap::<&Free, usize>::new();
            for name in names.iter().flatten() {
                *multiplicity.entry(name).or_default() += 1;
            }

            names
                .iter()
                .zip(telescope.iter())
                .filter_map(|(name, (birth, _))| {
                    let name = (*name)?;
                    // A duplicated image name is ambiguous to invert.
                    let unique = multiplicity.get(name) == Some(&1);
                    unique.then_some((*name, birth))
                })
                .collect()
        };
        let mut entry_multiplicity = HashMap::<&Term, usize>::new();
        for entry in &entries {
            *entry_multiplicity.entry(entry).or_default() += 1;
        }

        // Non-pattern entries that are meta-free and pairwise distinct become *abstraction subjects*: every occurrence of the entry inside the candidate rewrites to the entry's birth binder, extending inversion beyond the pattern fragment (the practical "abstracting over non-variable terms" move; the choice of all occurrences is checked by the round-trip verification below and by re-validation). An entry embedding a metavariable, or equal to another entry, stays ambiguous, and the candidate may not depend on it. Each subject contributes both its spellings — as written, and as the reducer exposes it at a whnf position (the candidate's root arrives reduced while deep positions do not) — except a reduced form that is a bare variable, which would collide with the renaming machinery.
        let mut subjects = Vec::new();
        for (entry, (birth, _)) in entries.iter().zip(telescope.iter()) {
            if matches!(&**entry, Subterm::Var(_))
                || !entry.metavars().is_empty()
                || entry_multiplicity.get(entry) != Some(&1)
            {
                continue;
            }

            subjects.push((entry.clone(), *birth));

            // An entry the type level may not reduce (an effectful scrutinee, say) simply contributes no reduced spelling.
            let Some(reduced) = reduce(context, entry.clone()).probed()? else {
                continue;
            };
            let ambiguous = matches!(&*reduced, Subterm::Var(_))
                || entries.contains(&reduced)
                || subjects.iter().any(|(s, _)| *s == reduced);
            if !ambiguous {
                subjects.push((reduced, *birth));
            }
        }

        let abstracted = match subjects.is_empty() {
            true => t.clone(),
            false => abstract_occurrences(t, &subjects),
        };

        // Scope check, through the inversion: every free variable of the (abstracted) candidate must correspond to exactly one birth binder. A name that is no entry at all can never become one — out of scope; a name only reachable through a non-pattern or duplicated slot is not provably determined — postpone.
        let allowed = image
            .iter()
            .map(|(name, _)| *name)
            .chain(subjects.iter().map(|(_, birth)| *birth))
            .collect::<HashSet<_>>();
        for name in abstracted.free_vars() {
            if allowed.contains(&name) {
                continue;
            }
            // A top-level definition is a global constant, in scope everywhere and absent from Γ's spine by construction (Γ holds only local binders); a solution may mention it freely. This is what lets an item elaborate independently of the ambient prelude.
            if context.is_top_level(&name) {
                continue;
            }
            let mentioned = entries.is_empty()
                || entries
                    .iter()
                    .any(|entry| entry.free_vars().contains(&name));
            curios_profile::note!(
                target: "curios_elab::solve",
                meta = id.0,
                name = %name.faithful(),
                mentioned,
                "scope check rejected a free name",
            );
            return Ok(match mentioned {
                true => Solved::Postponed,
                false => Solved::Failed,
            });
        }

        // Invert, storing the solution in birth-named form. The identity renaming (every invertible entry still its own birth binder, and nothing abstracted) skips the rewrite — from the abstracted candidate, never the raw one.
        let inverted = if subjects.is_empty() && image.iter().all(|(img, birth)| img == *birth) {
            abstracted.clone()
        } else {
            let binders = image.iter().map(|(name, _)| name).collect::<Vec<_>>();
            let birth_vars = image
                .iter()
                .map(|(_, birth)| Term::free_var(birth))
                .collect::<Vec<_>>();
            let refs = birth_vars.iter().collect::<Vec<_>>();
            abstracted.capture(&binders).release(&refs)
        };

        // The equation must hold by construction: resolving the candidate solution back through this occurrence's spine must reproduce the candidate. This guards the whole abstraction/inversion pair — a missed occurrence or an unfaithful rename postpones instead of committing a wrong solution. Syntactic equality is the fast path; an abstraction that matched a *reduced* spelling resolves back to the raw one, so the fallback criterion is definitional — a strict conversion, which cannot solve anything here since past the embedded guard the sides carry at most context-contained metavariables, and an undecided verdict postpones.
        if !metavar.spine.is_empty() {
            let binders = telescope.iter().map(|(name, _)| name).collect::<Vec<_>>();
            let refs = entries.iter().collect::<Vec<_>>();
            let resolved = inverted.capture(&binders).release(&refs);
            if resolved != *t && !convert(context, &Term::type_ground(), &resolved, t)? {
                curios_profile::note!(
                    target: "curios_elab::solve",
                    meta = id.0,
                    "postponed: inversion round-trip disagreed",
                );
                return Ok(Solved::Postponed);
            }
        }

        // Re-validation: the (inverted) candidate must *check* against the metavariable's frozen result type, under its birth context Γ and the refinements it was born under — nothing of an arm it is solved in, all of the arm it was born in — as an *oracle*, constraint parking suppressed (see `Context::with_oracle`). Stable definitions are kept. Checking (rather than synthesizing then converting) admits candidates that are checkable but not inferable — a bare lambda whose domain only `result` knows, an unannotated tuple — which are still the correct solution at the frozen type. What is committed is the term the check rebuilt, as everywhere elaboration checks a term: the check compares a lambda's annotation with the domain it is checked at by subsumption and rebuilds the lambda at that domain, while the kernel compares the two by conversion, so a candidate committed as it came keeps an annotation the kernel refuses — `(T: Type 0) => Io(T)`, the reduct of `Io` at one instance, standing where `(Type 1) -> Type 1` is wanted — and which side of a problem solved the metavariable would decide whether the program certifies. The validation run itself can solve *other* metavariables (inference may mint and pin fresh implicits); the mark/rollback bracket unwinds those if the candidate is rejected, so a failed oracle leaves no fingerprints.
        let mark = context.solution_mark();
        let revalidated = context.at_birth(&born, |context| {
            context.with_frame(|context| {
                for (name, ty) in telescope.iter() {
                    context.assume(name, ty);
                }

                // The check has three answers. A candidate that checks fits. One that fails where the check wanted to park has not been judged: a type it met — the frozen type itself, while the metavariable's own type is unsolved, or one inside it — still waits on a metavariable, and outside the oracle the same check would have waited for it. Read as a rejection, that would make the verdict follow the order parked work is retried in, a candidate refused one retry short of the solution that types it. Any other failure is a meta-free, well-scoped candidate that is not validly typed here. The check is a [`Probe`]: a candidate it could not afford is no rejected candidate, and the budget's refusal propagates instead.
                context.with_oracle(|context| {
                    let error = match check(context, &inverted, result.clone()) {
                        Ok(rebuilt) => return Ok(Revalidated::Fits(rebuilt)),
                        Err(error) => error,
                    };
                    if let Some(refusal) = error.refusal() {
                        return Err(refusal.clone());
                    }
                    curios_profile::note!(
                        target: "curios_elab::solve",
                        meta = id.0,
                        error = %error,
                        "re-validation rejected the candidate",
                    );

                    // The refusal is what explains a rejected candidate that looks correct where it was offered, and the report of the problem that offered it ([`Rejected`]).
                    Ok(match context.declined_to_park() {
                        true => Revalidated::Undecided,
                        false => Revalidated::Refused(error),
                    })
                })
            })
        });
        let revalidated = match revalidated {
            Ok(revalidated) => revalidated,
            Err(spent) => {
                context.rollback_solutions(mark);
                context.end_solutions(mark);
                return Err(spent);
            }
        };

        let solved = match revalidated {
            Revalidated::Fits(rebuilt) => {
                context.end_solutions(mark);
                context.solve_metavar(id, rebuilt);
                self.progress = true;
                return Ok(Solved::Done);
            }
            Revalidated::Undecided => {
                curios_profile::note!(
                    target: "curios_elab::solve",
                    meta = id.0,
                    against = %result,
                    "postponed: re-validation could not judge the candidate yet",
                );
                Solved::Postponed
            }
            Revalidated::Refused(error) => {
                curios_profile::note!(
                    target: "curios_elab::solve",
                    meta = id.0,
                    against = %result,
                    "failed: re-validation",
                );
                self.rejected = Some(Box::new(Rejected {
                    metavar: metavar.clone(),
                    candidate: t.clone(),
                    error,
                }));
                Solved::Failed
            }
        };
        context.rollback_solutions(mark);
        context.end_solutions(mark);
        Ok(solved)
    }

    /// Park a problem from inside a structural arm, as it is written. Returns `Ok(true)` — a blocked problem is undecided, never a mismatch.
    fn block(&mut self, problem: Problem) -> Result<bool, ReduceError> {
        self.blocked.push(problem);
        Ok(true)
    }

    /// The binders a candidate for `metavar` applied to `arity` arguments abstracts: those of its frozen birth type, each opened at a fresh label under the mark that type gives it — plicity is part of a function's identity (see `compare_func`), so a candidate convertible with that type copies them. `None` where there is no birth record, or the type does not reduce to a function type of that arity.
    fn birth_binders(
        context: &mut Context,
        metavar: &Metavar,
        arity: usize,
    ) -> Result<Option<CandidateBinders>, ReduceError> {
        let Some(entry) = context.metavar_entry(metavar.id) else {
            return Ok(None);
        };
        let result = reduce(context, entry.result.clone())?;
        let Subterm::FuncType(func_type) = &*result else {
            return Ok(None);
        };
        let plicities = func_type.plicities();
        if plicities.len() != arity {
            return Ok(None);
        }

        // Peeling opens each binder as a free variable of its own label (elaborated labels are entropy-fresh), so dependent domains stay well-scoped.
        let mut domains = Vec::with_capacity(arity);
        let mut cursor = func_type.telescope.cursor();
        for plicity in plicities {
            let (_, ty) = cursor.entry().expect("plicities parallel the telescope");
            let binder = cursor.advance_fresh(|hint| context.fresh(hint));
            domains.push((plicity, binder, ty));
        }

        Ok(Some(domains))
    }

    /// Whether `rigid`, reduced under the refinements in view, is a spelling `metavar` may be solved from: where those are not its birth's, the spelling its birth's alone give `rigid_raw` must be the same. The invariant [`Self::solve_at_birth`] protects, for a rule that builds its candidate from the rigid side where that one offers the side itself: a solution must not be derived from a spelling that holds only under refinements the metavariable was not born under.
    fn spelled_at_birth(
        context: &mut Context,
        metavar: &Metavar,
        rigid: &Term,
        rigid_raw: &Term,
    ) -> Result<bool, ReduceError> {
        let Some(birth) = context
            .metavar_entry(metavar.id)
            .map(|entry| entry.frames.clone())
        else {
            return Ok(true);
        };
        if context.reads_the_refinements_of(&birth) {
            return Ok(true);
        }
        let at_birth = context.at_birth(&birth, |context| reduce(context, rigid_raw.clone()))?;

        Ok(at_birth == *rigid)
    }

    /// Flex-apply abstraction — the pattern rule under an application: for `?m(x₁ … xₖ) ≡ t` where each `xᵢ` is a distinct local variable `?m`'s own spine does not carry ([`pattern_variables`]), `?m := λx₁…xₖ. t` is the one solution, and it is offered to [`Self::solve`] over the binders of `?m`'s frozen birth type. `None` where `flex` is no such pattern, or no candidate can be built for it — no birth type of that arity, a spelling that holds only under refinements `?m` was not born under — and the pair then goes on to the rules that stood before this one.
    ///
    /// Ahead of [`Self::imitate_flex_apply`], which stays the rule for arguments that are no such variables. For a pattern the abstraction is what the equation states and imitation a guess at it: against `Option(Nat)`, `?m(n)` is imitated as `(n) => Option(n)`, which then holds `n` to `Nat`.
    fn abstract_flex_apply(
        &mut self,
        context: &mut Context,
        flex: &Term,
        rigid: &Term,
        rigid_raw: &Term,
        standing: Option<Standing>,
    ) -> Result<Option<Solved>, ReduceError> {
        let Subterm::Apply(apply) = &**flex else {
            return Ok(None);
        };
        let Some(metavar) = as_metavar(&apply.head).cloned() else {
            return Ok(None);
        };
        if context.owner_fills(metavar.id) {
            return Ok(None);
        }
        let Some(variables) = pattern_variables(context, &metavar, apply) else {
            return Ok(None);
        };
        if !Self::spelled_at_birth(context, &metavar, rigid, rigid_raw)? {
            return Ok(None);
        }
        let Some(domains) = Self::birth_binders(context, &metavar, variables.len())? else {
            return Ok(None);
        };

        // The body at levels of its own where the pair is asked at below, as a bare metavariable's candidate is ([`Self::sorts_of_its_own`]), minted in the transaction that offers it.
        let mark = context.solution_mark();
        let own = match standing {
            Some(standing) => self.sorts_of_its_own(context, &metavar, rigid, standing),
            None => Ok(None),
        };
        let solved = own.and_then(|own| {
            let binders = domains
                .iter()
                .map(|(_, binder, _)| Term::free_var(binder))
                .collect::<Vec<_>>();
            let body = own
                .as_ref()
                .unwrap_or(rigid)
                .capture(&variables.iter().collect::<Vec<_>>())
                .release(&binders.iter().collect::<Vec<_>>());
            let candidate = Term::func_marked(domains, body);

            self.solve(context, &metavar, &candidate)
        });
        if !matches!(solved, Ok(Solved::Done)) {
            context.rollback_solutions(mark);
        }
        context.end_solutions(mark);

        solved.map(Some)
    }

    /// Flex-apply imitation — the restricted higher-order rule: for `?m(a₁ … aₖ) ≡ T(p̄ ++ ī)` where `T` is a nominal (inductive or struct) type constructor or a unary intrinsic type former (`List`, `Cell`), commit the *imitation* solution `?m := λx₁…xₖ. T(x₁, …, xₖ)` and equate arguments pairwise. This is what unifies `?M(?A) ≡ Option(Nat)` for higher-kinded concepts — the same commitment Lean's first-order approximation and Agda's constructor-headed unification make.
    ///
    /// Taken only where it is asked for ([`Context::imitates`]): by a completing term over the conversions it awaits, each having offered its answer first, or by a `!` reading its region's monad. Anywhere else the pair blocks as it is written, since a later part of the term may state the function the imitation would guess at, and `?m(a)` against `T(b, a)` is as well answered by `(x) => T(x, a)`.
    ///
    /// Deliberate restrictions (cf. the guards in `solve`):
    /// - **right-biased** partial application — for a spine of `k` arguments against `n` rigid arguments, `k < n` commits `?m := λx₁…xₖ. T(b₁, …, b_{n−k}, x₁, …, xₖ)`, the conventional choice kind-currying makes (it is why `Monad (Either e)` works in Haskell). The fixed prefix is taken from the rigid side, which a telescope's left-to-right dependency order keeps well-scoped, and `k` is never guessed: it is the spine's own length, checked against `?m`'s frozen birth arity. An equation whose intended solution abstracts a *prefix* is guessed wrong and blocked — incompleteness moves rather than disappearing — and a kind-wrong split (abstracting an index the birth type sorts differently) is refused by the re-validation below before it commits;
    /// - imitation only — the constant (`?m := λ_. T(b̄)`) and projection (`?m := λx. x`) solutions are never produced;
    /// - a nominal `T` is imitated at an occurrence of its own ([`Self::occurrence_of`]), so the candidate is what a written `(x̄) => T(b̄, x̄)` elaborates to and not a function of the one instance it was solved from;
    /// - rigid heads only — nominal constructors and the unary intrinsic formers (`List`, `Cell`), whose argument rides inside the `Intrinsic` node;
    /// - flex–flex stays blocked (dispatched before the structural match);
    /// - a rejected or postponed guess *blocks* the problem, never hard-fails it: refuting the imitation does not prove the equation unsatisfiable (a constant solution could still exist), and blocking preserves the drain's retry semantics — a permanently blocked problem surfaces as a type mismatch at its origin.
    ///
    /// The callers' arms fire for *any* `Apply` against a nominal type; the unsolved-bare-metavariable-head test lives here, and its else branch is the `eta_expand_neutral` fallthrough every other pair takes.
    fn imitate_flex_apply(
        &mut self,
        context: &mut Context,
        flex: Apply,
        rigid: Term,
        rigid_raw: &Term,
        problem: Problem,
    ) -> Result<bool, ReduceError> {
        let Some(metavar) = as_metavar(&flex.head).cloned() else {
            return self.eta_expand_neutral(context, problem.this, problem.that, problem.type_);
        };
        // An imitation is one function among those the application may stand for, so it is no rule of conversion's: the pair waits as it is written for the term that awaits it, which imitates where it completes, as a `!` does for its region.
        if !context.imitates() {
            return self.block(problem);
        }

        // The rigid side's arguments, how its node is rebuilt over other arguments at an instance, and, for a nominal node, the family and the instance the rigid side stands at.
        type MkBody = Box<dyn Fn(Universes, &[Term]) -> Term>;
        type Family = Option<(Global, Universes)>;
        let (rigid_args, mk_body, family): (Vec<Term>, MkBody, Family) = match &*rigid {
            Subterm::InductType(induct_decl) => {
                let name = induct_decl.name;
                let n_params = induct_decl.params.len();
                let args = induct_decl
                    .params
                    .iter()
                    .chain(&induct_decl.indices)
                    .cloned()
                    .collect();
                (
                    args,
                    Box::new(move |universes, vars| {
                        let (params, indices) = vars.split_at(n_params);
                        Term::induct_type_at(
                            name,
                            universes,
                            params.iter().cloned(),
                            indices.iter().cloned(),
                        )
                    }),
                    Some((name, induct_decl.universes.clone())),
                )
            }
            // Struct types carry no indices.
            Subterm::StructType(struct_decl) => {
                let name = struct_decl.name;
                (
                    struct_decl.params.clone(),
                    Box::new(move |universes, vars| {
                        Term::struct_type_at(name, universes, vars.iter().cloned())
                    }),
                    Some((name, struct_decl.universes.clone())),
                )
            }
            // The unary intrinsic type formers: their argument rides inside the `Intrinsic` node, so the imitation body rebuilds the node over the binder (`?m := λT. List(T)` for `?m(?A) ≡ List(Nat)`). They carry no instance.
            Subterm::Intrinsic(Intrinsic::ListType(elem)) => (
                vec![elem.clone()],
                Box::new(|_, vars| Term::intrinsic(Intrinsic::ListType(vars[0].clone()))),
                None,
            ),
            Subterm::Intrinsic(Intrinsic::CellType(elem)) => (
                vec![elem.clone()],
                Box::new(|_, vars| Term::intrinsic(Intrinsic::CellType(vars[0].clone()))),
                None,
            ),
            Subterm::Intrinsic(Intrinsic::ChannelType(elem)) => (
                vec![elem.clone()],
                Box::new(|_, vars| Term::intrinsic(Intrinsic::ChannelType(vars[0].clone()))),
                None,
            ),
            Subterm::Intrinsic(Intrinsic::IoType(elem)) => (
                vec![elem.clone()],
                Box::new(|_, vars| Term::intrinsic(Intrinsic::IoType(vars[0].clone()))),
                None,
            ),
            _ => unreachable!("the callers pass a nominal or intrinsic-former rigid side"),
        };
        let arity = rigid_args.len();
        let spine = flex.arguments.len();

        // Right-biased: a spine shorter than the rigid argument list abstracts the *suffix*, with the prefix baked in from the rigid side; a longer spine has no imitation.
        if spine > arity {
            return self.block(problem);
        }

        // The candidate's binders come from `?m`'s frozen birth type, which must itself reduce to a function type of the spine's arity.
        let Some(domains) = Self::birth_binders(context, &metavar, spine)? else {
            return self.block(problem);
        };

        // The candidate body mirrors the rigid node's shape (for an inductive, its params/indices split — `elaborate_induct_type` re-checks the node against the full telescope during re-validation and rejects a wrong split).
        let binder_vars = domains
            .iter()
            .map(|(_, binder, _)| Term::free_var(binder))
            .collect::<Vec<_>>();
        let (rigid_prefix, rigid_suffix) = rigid_args.split_at(arity - spine);

        // The invariant `solve_at_birth` protects, upheld here by hand ([`Self::spelled_at_birth`]): where the spelling at `?m`'s birth differs, the nominal shape may be the other refinements' doing — postpone rather than commit. Asked before the candidate's instance is minted, so every return past it has one transaction to close.
        if !Self::spelled_at_birth(context, &metavar, &rigid, rigid_raw)? {
            return self.block(problem);
        }

        // The instance is minted, paired with the rigid side's and offered to `solve` in one transaction: `solve` brackets its own re-validation alone, so a candidate it postpones or refuses before that would leave the occurrence and its equations standing with no term to hold them.
        let mark = context.solution_mark();
        let instance = match &family {
            Some((name, universes)) => Self::occurrence_of(context, name, universes),
            None => Ok(Universes::default()),
        };
        // A family in the fixed prefix, a transformer's base monad, is at an occurrence of its own as the node is ([`Self::family_arguments`]).
        let prefix = Self::family_arguments(context, rigid_prefix);
        // `solve` supplies the occurs check, the embedded-metavariable guard, the spine inversion and scope check (against `?m`'s own birth spine), and re-validation: the candidate is `check`ed against the frozen birth type under the birth context, which is what rejects an ill-kinded imitation before it commits.
        let solved = instance.and_then(|instance| {
            let full_args = prefix?
                .into_iter()
                .chain(binder_vars.iter().cloned())
                .collect::<Vec<_>>();
            let candidate = Term::func_marked(domains.clone(), mk_body(instance, &full_args));

            self.solve(context, &metavar, &candidate)
        });
        if !matches!(solved, Ok(Solved::Done)) {
            context.rollback_solutions(mark);
        }
        context.end_solutions(mark);
        match solved? {
            Solved::Done => {}
            Solved::Postponed | Solved::Failed => return self.block(problem),
        }

        // Equal the spine against the rigid *suffix* pairwise, at the candidate telescope's domains (mirroring `compare_apply`'s recovered argument types). The baked-in prefix needs no equations: after substitution both sides carry it verbatim.
        for ((flex_arg, rigid_arg), (_, _, domain)) in flex
            .arguments
            .into_iter()
            .map(|argument| argument.term)
            .zip(rigid_suffix.iter().cloned())
            .zip(domains)
        {
            self.enqueue(domain, flex_arg, rigid_arg);
        }

        Ok(true)
    }

    /// The instance a candidate imitating `name`'s node is built at: a fresh occurrence of the family, paired with the instance `universes` the rigid side stands at as conversion pairs two instances, an equation at each level the family is invariant in and a weak one where it is irrelevant.
    ///
    /// Built at the rigid side's own instance, the candidate holds `?m`'s binders to the levels of the one occurrence `?m` happened to be solved from: `?M(?A)` against `Result.{0,0}(Str, Nat)` makes `M` a monad of small payloads alone, and a region that binds a larger one through `Monad/bind` written out is refused where the same region written with `!`, which is handed its monad, is accepted. The weak equation keeps the rigid side's instance wherever it served: the declaration's closing joins the two unless the candidate's binders need the level elsewhere.
    ///
    /// A family still inside its own group stands at levels its unfinished scheme does not count, and one no registry holds has no scheme to instantiate; either is imitated at the rigid side's instance, every level of it being compared there.
    fn occurrence_of(
        context: &mut Context,
        name: &Global,
        universes: &Universes,
    ) -> Result<Universes, ReduceError> {
        let scheme = context
            .induct_decl(name)
            .map(|declaration| declaration.universe_context.clone())
            .or_else(|| {
                context
                    .struct_decl(name)
                    .map(|declaration| declaration.universe_context.clone())
            })
            .filter(|scheme| {
                scheme.parameter_count == universes.levels.len()
                    && scheme.quality_count == universes.qualities.len()
            });
        let Some(scheme) = scheme else {
            return Ok(universes.clone());
        };

        let fresh = context
            .universes_mut()
            .instantiate(&scheme)
            .map_err(ReduceError::Universe)?;
        Self::compare_instance_levels(context, name, &fresh, universes)?;

        Ok(fresh)
    }

    /// [`Self::solve`] with a candidate read off a problem's rigid side, at levels of its own: a family among them taken at an occurrence of its own ([`Self::family_at_its_own_occurrence`]), and, where the problem is asked at below, a side that ends in a sort taken with that sort at a level of its own ([`Self::sorts_of_its_own`]). The levels are minted and the candidate offered in one transaction, so one `solve` postpones or refuses leaves no level standing with no term to hold it.
    fn solve_rigid(
        &mut self,
        context: &mut Context,
        metavar: &Metavar,
        candidate: &Term,
        standing: Option<Standing>,
    ) -> Result<Solved, ReduceError> {
        let mark = context.solution_mark();
        let own = match standing {
            Some(standing) => self.sorts_of_its_own(context, metavar, candidate, standing),
            None => Ok(None),
        };
        let own = match own {
            Ok(None) => Self::family_at_its_own_occurrence(context, candidate),
            decided => decided,
        };
        let own = match own {
            Ok(Some(own)) => own,
            Ok(None) => {
                context.end_solutions(mark);
                return self.solve(context, metavar, candidate);
            }
            Err(spent) => {
                context.rollback_solutions(mark);
                context.end_solutions(mark);
                return Err(spent);
            }
        };

        let solved = self.solve(context, metavar, &own);
        if !matches!(solved, Ok(Solved::Done)) {
            context.rollback_solutions(mark);
        }
        context.end_solutions(mark);
        solved
    }

    /// `term` with the sort it ends in at a level of its own, where it ends in one: a `Type` or a `Prop`, or a function type whose codomain ends in one, each read forced as the relation reads it. `None` where it ends in anything else, and the candidate is then the term as it stands.
    ///
    /// A metavariable met at below is held to an inequality, and the other side's own sort is only the nearest solution of it. Solved to that, `S` in `both(@S, f: (Bool) -> S, g: (Bool) -> S)` is the first argument's sort and the second is held to it: the call is admitted in one order and refused in the other, and a declaration that makes it bounds its own callers by that order. The fresh level is bounded by the one it copies in the problem's direction, so the problem holds as asked and a later bound moves the copy alone, the declaration's closing taking the join. It stands for the sort the program would have written at the implicit and takes that level's provenance ([`UniverseSolver::fresh_beside`](crate::UniverseSolver::fresh_beside)), so the call elaborates to what it does with `@Type` or `@Prop` written. The copy is a sort of the kind it copies: a proposition stands for no type, so a metavariable met at `Prop` is a `Prop`. Rocq's `refresh_universes` gives an evar met under cumulativity the same copy, for a universe that cannot move.
    ///
    /// Each reading is under the refinements `metavar` was born under and no others, for the reason [`Self::solve_at_birth`] gives. The caller holds the transaction: a level minted and bounded here goes with a candidate `solve` does not commit.
    fn sorts_of_its_own(
        &mut self,
        context: &mut Context,
        metavar: &Metavar,
        term: &Term,
        standing: Standing,
    ) -> Result<Option<Term>, ReduceError> {
        let birth = context
            .metavar_entry(metavar.id)
            .map(|entry| entry.frames.clone());
        let forced = match &birth {
            Some(birth) => {
                context.at_birth(birth, |context| reduce_forced(context, term.clone()))?
            }
            None => reduce_forced(context, term.clone())?,
        };

        match &*forced {
            Subterm::Sort(quality, level) => {
                // What is put below is named for the report, as the relation's own rule names it.
                let kind = match context.universes().quality(*quality) {
                    Quality::Prop => UniverseConstraintKind::PropositionCumulativity,
                    _ => UniverseConstraintKind::Cumulativity,
                };
                let origin = UniverseConstraintOrigin {
                    extent: self.extent,
                    kind,
                    declaration: None,
                    binder: None,
                };
                let own = context
                    .fresh_universe_beside(level, Some(origin.clone()))
                    .map_err(ReduceError::Universe)?;
                let (lower, upper) = match standing {
                    Standing::Over => (level.clone(), own.clone()),
                    Standing::Under => (own.clone(), level.clone()),
                };
                context
                    .universes_mut()
                    .add_leq(lower, upper, origin)
                    .map_err(ReduceError::Universe)?;

                Ok(Some(Term::sort_at(*quality, own)))
            }
            Subterm::FuncType(FuncType { telescope }) => {
                let mut binders = Vec::with_capacity(telescope.len());
                let mut cursor = telescope.cursor();
                while let Some((_, domain)) = cursor.entry() {
                    let mark = cursor.mark().expect("a mark stands at an entry");
                    let binder = cursor.advance_fresh(|hint| context.fresh(hint));
                    binders.push((mark, binder, domain));
                }
                let codomain = cursor.body().expect("a cursor past every entry");

                Ok(self
                    .sorts_of_its_own(context, metavar, &codomain, standing)?
                    .map(|own| Term::func_type_marked(binders, own)))
            }
            _ => Ok(None),
        }
    }

    /// `term` at an occurrence of the family's own ([`Self::occurrence_of`]) where it is a family, and `None` where it is anything else or stands at such an occurrence already. A family arrives in three spellings: its name with no argument, the bare projection of its group, which is the name as reduction spells it, and a lambda over a nominal type, each family among that type's parameters taken the same way.
    ///
    /// A type's instance is the one the two sides of its problem are compared at. A family is a function, and what stands at its irrelevant levels is each application's to choose: committed at the instance it was read off, the metavariable is a family of the one occurrence it was solved from. `?M` against the `Option` of an action at `Try(Option, Str, Type)` made the base monad one of payloads at that action's level alone, and a region binding a larger one through `Try/bind` or `Monad/bind` written out was refused where `!`, which is handed its monad, accepted it. At an occurrence of its own the family is what `Option` or `(A) => Try(Option, Str, A)` written there elaborates to, held to the instance it was read off by an equation where the family is invariant and by a weak one where it is not.
    fn family_at_its_own_occurrence(
        context: &mut Context,
        term: &Term,
    ) -> Result<Option<Term>, ReduceError> {
        if let Some((name, levels)) = Self::bare_family(context, term) {
            let fresh = Self::occurrence_of(context, &name, &levels)?;

            return Ok((fresh != levels).then(|| Term::instance_of(&Free::from(&name), fresh)));
        }

        let Subterm::Func(Func { telescope, .. }) = &**term else {
            return Ok(None);
        };
        let mut binders = Vec::with_capacity(telescope.len());
        let mut cursor = telescope.cursor();
        while let Some((_, annotation)) = cursor.entry() {
            let mark = cursor.mark().expect("a mark stands at an entry");
            let binder = cursor.advance_fresh(|hint| context.fresh(hint));
            binders.push((mark, binder, annotation));
        }
        let body = cursor.body().expect("a cursor past every entry");

        let family = match &*body {
            Subterm::Func(_) => Self::family_at_its_own_occurrence(context, &body)?,
            Subterm::InductType(node) => {
                let universes = Self::occurrence_of(context, &node.name, &node.universes)?;
                let params = Self::family_arguments(context, &node.params)?;
                (universes != node.universes || params != node.params).then(|| {
                    Subterm::InductType(InductType {
                        universes,
                        params,
                        ..node.clone()
                    })
                    .into()
                })
            }
            Subterm::StructType(node) => {
                let universes = Self::occurrence_of(context, &node.name, &node.universes)?;
                let params = Self::family_arguments(context, &node.params)?;
                (universes != node.universes || params != node.params).then(|| {
                    Subterm::StructType(StructType {
                        universes,
                        params,
                        ..node.clone()
                    })
                    .into()
                })
            }
            _ => None,
        };

        Ok(family.map(|body| Term::func_marked(binders, body)))
    }

    /// The family `term` is the bare name of, with the instance it stands at: a name of a family that takes a parameter, written with no argument or spelled by reduction as the bare projection of its group. A family that takes none is a type under its bare name, and its instance is compared.
    fn bare_family(context: &Context, term: &Term) -> Option<(Global, Universes)> {
        if let Some((name, universes)) = term.bare_former() {
            return Some((*name, universes.clone()));
        }
        let Subterm::Instance(Instance {
            head: InstanceHead::Var(var),
            universes,
        }) = &**term
        else {
            return None;
        };
        let name = *var.as_free()?.as_global()?;
        let parameters = context
            .induct_decl(&name)
            .map(|declaration| declaration.param_count() + declaration.index_count())
            .or_else(|| {
                context
                    .struct_decl(&name)
                    .map(|declaration| declaration.param_count())
            })?;

        (parameters > 0).then(|| (name, universes.clone()))
    }

    /// `arguments`, the parameters a family's node is applied to, each one that is itself a family at an occurrence of its own ([`Self::family_at_its_own_occurrence`]), as a transformer's base monad is.
    ///
    /// A family among a node's parameters is compared at its function type, whose domain is the level it was written at, so the node holds that level for every application of the candidate as the node's own instance would.
    fn family_arguments(
        context: &mut Context,
        arguments: &[Term],
    ) -> Result<Vec<Term>, ReduceError> {
        arguments
            .iter()
            .map(|argument| {
                Ok(Self::family_at_its_own_occurrence(context, argument)?
                    .unwrap_or_else(|| argument.clone()))
            })
            .collect()
    }

    /// Solve flex–rigid with a candidate spelled under the refinements the metavariable was born under. The drain reduces problems under the live frame, where the refinements of the arm the problem sits in apply — sound for discharging a problem, but not for committing a solution: a metavariable must not be pinned to a value that holds only inside an arm it was not born in (`?k := 0` because the nil arm refined `n := 0`). Where the refinements in view are its birth's — the metavariable solved in the arm it was born in — the rigid side is its spelling, or what it was reduced from where the reduct does not re-check; otherwise the original rigid term is re-reduced under its birth's alone and solved against that spelling. Refinements only ever *add* reductions, so a solution found this way still discharges the problem as it stands. A problem whose verdict changes at the birth's refinements is the other refinements' doing — nothing is globally forced, so it postpones rather than failing or committing.
    fn solve_at_birth(
        &mut self,
        context: &mut Context,
        metavar: &Metavar,
        rigid: &Term,
        rigid_raw: &Term,
        standing: Option<Standing>,
    ) -> Result<Solved, ReduceError> {
        let birth = match context.metavar_entry(metavar.id) {
            Some(entry) => entry.frames.clone(),
            // No birth record: `solve` refuses it, as it refuses any.
            None => return self.solve_rigid(context, metavar, rigid, standing),
        };

        if context.reads_the_refinements_of(&birth) {
            // A rigid side whose reduction only unfolded a name into a stuck form — `double(n)` to the folded call's neutral — is committed as written when that passes every check, and falls back to the reduced spelling on any other verdict, so no equation the reduced spelling decides is lost. Reduction takes the name straight back to the same neutral, so nothing downstream can tell the two solutions apart; what differs is the spelling a report materializes, which keeps the name instead of the whole recursive group, and the Core term the kernel receives, which carries the call instead of an inlined copy of the group.
            //
            // A `use` slot is committed as written for the same reason, whatever its dictionary unfolds to: a witness is a name for a structure of methods, and committed reduced the slot would spell the structure out at every use, for the kernel to receive and for erasure to build again where the program calls a global.
            // A raw side headed by a solved metavariable spells what that one was solved to: an implicit another call hands on, as `Eq/sym` does each side of the equation it turns, holds the name its reduct no longer shows.
            let behind = match &**applied_head(rigid_raw) {
                Subterm::Metavar(Metavar { id, .. }) if context.metavar_solution(*id).is_some() => {
                    Some(zonk_solved_term_metas(context, rigid_raw))
                }
                _ => None,
            };
            let rigid_raw = behind.as_ref().unwrap_or(rigid_raw);
            let written = stalled_unfolding(rigid_raw, rigid)
                || (matches!(metavar.origin, MetavarOrigin::Witness(_)) && rigid_raw != rigid);
            if written
                && let Solved::Done = self.solve_rigid(context, metavar, rigid_raw, standing)?
            {
                return Ok(Solved::Done);
            }

            // A reduct need not re-check where what it was reduced from does. Typing a match reads its scrutinee's type as spelled, and a reduct that inlined a definition's absurd arm — `hop`'s `match some end`, typed there because the arm refines the variable `b` — spells that type afresh from the substituted scrutinee, where no equation reaches it, and re-validation refuses it; the kernel reads a match's scrutinee type the same way. A refused reduct is therefore tried as written before the problem fails, unless the rule above already tried that spelling — which can only turn a refusal into a solution. `implicit_tests`' `a_solution_whose_reduct_does_not_recheck_is_committed_as_written` is the program.
            return Ok(match self.solve_rigid(context, metavar, rigid, standing)? {
                Solved::Failed if !written && rigid_raw != rigid => {
                    match self.solve_rigid(context, metavar, rigid_raw, standing)? {
                        Solved::Done => Solved::Done,
                        Solved::Postponed | Solved::Failed => Solved::Failed,
                    }
                }
                verdict => verdict,
            });
        }

        let at_birth = context.at_birth(&birth, |context| reduce(context, rigid_raw.clone()))?;

        // The refinements beyond the birth's made no difference for this term: the unguarded path, hard verdicts included.
        if at_birth == *rigid {
            return self.solve_rigid(context, metavar, rigid, standing);
        }

        // The spelling at birth is itself flexible — only the other refinements made the side look rigid. Undecided.
        if as_metavar(&at_birth).is_some() {
            return Ok(Solved::Postponed);
        }

        Ok(
            match self.solve_rigid(context, metavar, &at_birth, standing)? {
                Solved::Done => Solved::Done,
                // A verdict the spelling at birth cannot reach (out of scope, ill-typed at the birth context) is not a hard failure of the problem — the spelling in view may still discharge it once the metavariable is pinned elsewhere.
                Solved::Postponed | Solved::Failed => Solved::Postponed,
            },
        )
    }

    fn outcome(&mut self, context: &mut Context) -> Result<Outcome, ReduceError> {
        loop {
            if !self.drain(context)? {
                // A candidate refused is what the problem came to, whatever it left uncompared on the way.
                let rejected = self.rejected.take().map(Beside::Rejected);
                let declined = self.declined.take().map(Beside::Declined);
                return Ok(Outcome::Mismatch(rejected.or(declined)));
            }

            // Fixpoint: retry postponed constraints only when a fresh solution since the last sweep could have unblocked them.
            if self.progress && !self.blocked.is_empty() {
                self.pending = mem::take(&mut self.blocked).into();
                self.progress = false;
            } else {
                break;
            }
        }

        // Nothing was found apart, so what the shortcut left to a curried head's type was no difference: one instance is what the declaration should hold where it can. A run that parks a problem records them too, a weak equation refusing nothing whatever the retry finds.
        for (this_level, that_level) in mem::take(&mut self.apart) {
            context.universes_mut().add_weak_eq(this_level, that_level);
        }

        // A constraint still blocked at quiescence is undecided, not unequal: surrender it to the caller rather than conflating the two.
        Ok(match self.blocked.is_empty() {
            true => Outcome::Converts,
            false => Outcome::Blocked(mem::take(&mut self.blocked)),
        })
    }

    /// Drain `pending` once. Returns `Ok(false)` on a hard mismatch; `Ok(true)` when the queue empties (possibly leaving `blocked` constraints).
    fn drain(&mut self, context: &mut Context) -> Result<bool, ReduceError> {
        curios_profile::profile!("convert::drain");
        while let Some(Problem {
            type_,
            this,
            that,
            binders: posed,
            pairs: posed_pairs,
            relation,
        }) = self.dequeue(context)?
        {
            self.declined = None;
            self.rejected = None;
            self.binders = posed.clone();
            self.pairs = posed_pairs.clone();
            // Reflexivity needs no evaluation. In particular, do not force an identical folded recursive computation merely because it sits under a strict intrinsic operation.
            if this == that {
                continue;
            }

            // A subsumption is its own rules first; what they do not decide is the equation below, and waits as the subsumption it was asked as.
            if relation == Relation::Below && self.decide_below(context, &this, &that)? {
                continue;
            }

            // Sides differing only in universe levels are decided by identifying the levels, never by erasing them — see `identify_universe_levels` for the rule, its license, and the refuted premise of a projection comparison. Sound at every problem type, universes included: `Type a ~ Type b` gets the same equality constraint the structural arm below would emit, without the descent. A curried head apart in a level its value does not hold is declined, since what it is follows the type this problem goes on to read.
            if this.has_universe_data() || that.has_universe_data() {
                // Each side is read as solved. The type a binder was declared at spells its implicit arguments and its operators' witnesses as the metavariables elaboration solved, and the type it is checked against spells them substituted: left as they stand, the two are apart in a spelling, and the question here is whether they are apart in levels alone.
                let solved_this = zonk_solved_term_metas(context, &this);
                let solved_that = zonk_solved_term_metas(context, &that);
                match identify_universe_levels(
                    context,
                    &solved_this,
                    &solved_that,
                    Curried::Declined,
                )? {
                    Identification::Identified => continue,
                    Identification::Curried(apart) => {
                        for pair in apart {
                            if !self.apart.contains(&pair) {
                                self.apart.push(pair);
                            }
                        }
                    }
                    Identification::Distinct
                    | Identification::GroundUnequal
                    | Identification::UnderBinder
                    | Identification::Withheld => {}
                }
            }

            // Definitional proof irrelevance before either side is reduced, where neither side is flexible — the kernel's order, for the kernel's reason: reducing a proof to discover it equals another proof is work whose answer was already known. Here the work could be unbounded, too: under an arm's case equation a proof can reduce forever, and comparing it with another proof would spend the budget on a goal irrelevance decides outright. A side mentioning a metavariable still goes through the dispatch below, so it is solved against the other side rather than left dangling, and the check after that dispatch covers it.
            if !this.has_metavar()
                && !that.has_metavar()
                && Sort::of_under(context, &posed, &mut Vec::new(), &type_)?
                    .is_prop_to(context, "Convert::drain")
            {
                continue;
            }

            // The unreduced spellings, kept for the flex–rigid case: the reductions below apply every match-arm refinement in view, and a candidate *solution* must be derived under those its metavariable was born under alone (see `solve_at_birth`).
            let this_raw = this.clone();
            let that_raw = that.clone();

            // Two applications of one global definition are decided by their spines first, on these raw spellings — see `compare_same_global_apply` for why this must precede the reduction below.
            let open = match Self::compare_same_global_apply(
                context,
                &posed,
                &posed_pairs,
                &this_raw,
                &that_raw,
            )? {
                Spun::Agreed => continue,
                Spun::Solvable => Some(true),
                Spun::Open => Some(false),
                Spun::Apart => None,
            };

            let this = reduce(context, this)?;
            let that = reduce(context, that)?;
            // Forced, as the kernel reads a goal's type: weak-head form leaves a member of a declaration group folded, and no rule keyed on the type's shape sees through it.
            let type_ = reduce_forced(context, type_)?;

            if this == that {
                continue;
            }

            // A problem that must park does so under its *raw* spelling. The reduced forms applied the live frame's counterfactual refinements and any already-solved motive, so parking them loses exactly what a retry needs: the watch set misses the metavariables only the raw spelling mentions (an unsolved motive heading the expected type), and the refinement-suppression analysis can no longer recover the solvable unrefined spelling — a candidate instantiated through an arm refinement is well-typed only under it, which re-validation rightly rejects forever. The retry re-reduces the raw problem under its restored frame, so nothing is lost by deferring the reduction.
            let raw_problem = |type_: &Term| Problem {
                type_: type_.clone(),
                this: this_raw.clone(),
                that: that_raw.clone(),
                binders: posed.clone(),
                pairs: posed_pairs.clone(),
                relation,
            };

            // Two calls of one definition whose spines may yet agree are decided by what they reduce to as a run of its own. What that forces is solved; where it leaves the pair undecided the pair waits whole, under its raw spelling, and what the run left blocked goes with it: a residue alone — `Pick(?b)` against `Nat`, of `Shelf(?b)` against `Shelf(true)` — holds nothing of the two calls, and the term that awaits it would have no spines to hold where it completes.
            //
            // A pair whose spines agree by what that would solve waits the same way where its reduction finds the two apart: they are equal at that solution, so the refusal is reduction's, which reads a fold stuck on `?a % 28` as no `1`, and what the run solved on its way there is withdrawn with it.
            if let Some(solvable) = open
                && (this != this_raw || that != that_raw)
            {
                let mark = context.solution_mark();
                let committed = context.solutions_committed();
                let reduced = Problem {
                    type_: type_.clone(),
                    this,
                    that,
                    binders: posed.clone(),
                    pairs: posed_pairs.clone(),
                    relation,
                };
                let outcome = Convert::new(reduced, self.extent).outcome(context);
                let waits = match &outcome {
                    Ok(Outcome::Blocked(_)) => true,
                    Ok(Outcome::Mismatch(_)) => solvable,
                    Ok(Outcome::Converts) | Err(_) => false,
                };
                if waits && matches!(outcome, Ok(Outcome::Mismatch(_))) {
                    context.rollback_solutions(mark);
                }
                context.end_solutions(mark);
                self.progress |= context.solutions_committed() != committed;
                match outcome? {
                    Outcome::Converts => continue,
                    _ if waits => {
                        self.blocked.push(raw_problem(&type_));
                        continue;
                    }
                    Outcome::Mismatch(beside) => {
                        match beside {
                            Some(Beside::Declined(declined)) => self.declined = Some(declined),
                            Some(Beside::Rejected(rejected)) => self.rejected = Some(rejected),
                            None => {}
                        }
                        return Ok(false);
                    }
                    Outcome::Blocked(_) => unreachable!("a blocked pair waits"),
                }
            }

            // Flexible heads are dispatched before the structural/η fallthrough — a flexible head must never be η-expanded into a spine.
            match (as_metavar(&this).cloned(), as_metavar(&that).cloned()) {
                (Some(this_m), Some(that_m)) => {
                    // Same head, different spines (node duplication under different openings): entrywise spine agreement is a *sufficient* congruence condition. Probed only when both spines are meta-free — a probe that could solve metavariables would overcommit, since agreement is not necessary for the problem to hold.
                    if this_m.id == that_m.id
                        && this_m
                            .spine
                            .iter()
                            .chain(that_m.spine.iter())
                            .all(|entry| entry.metavars().is_empty())
                    {
                        let mut entrywise = true;
                        for (a, b) in this_m.spine.iter().zip(that_m.spine.iter()) {
                            if !matches!(
                                convert_under(
                                    context,
                                    &posed,
                                    &posed_pairs,
                                    &Term::type_ground(),
                                    a,
                                    b
                                )?,
                                Outcome::Converts
                            ) {
                                entrywise = false;
                                break;
                            }
                        }
                        if entrywise {
                            continue;
                        }
                    }

                    // Holes their owner fills, and distinct heads (or an undecided probe): flex–flex does no intersection — postpone.
                    self.blocked.push(raw_problem(&type_));
                    continue;
                }
                (Some(metavar), None) => {
                    if context.owner_fills(metavar.id) {
                        self.blocked.push(raw_problem(&type_));
                        continue;
                    }
                    let standing = (relation == Relation::Below).then_some(Standing::Under);
                    match self.solve_at_birth(context, &metavar, &that, &that_raw, standing)? {
                        Solved::Done => continue,
                        Solved::Postponed => {
                            self.blocked.push(raw_problem(&type_));
                            continue;
                        }
                        Solved::Failed => return Ok(false),
                    }
                }
                (None, Some(metavar)) => {
                    if context.owner_fills(metavar.id) {
                        self.blocked.push(raw_problem(&type_));
                        continue;
                    }
                    let standing = (relation == Relation::Below).then_some(Standing::Over);
                    match self.solve_at_birth(context, &metavar, &this, &this_raw, standing)? {
                        Solved::Done => continue,
                        Solved::Postponed => {
                            self.blocked.push(raw_problem(&type_));
                            continue;
                        }
                        Solved::Failed => return Ok(false),
                    }
                }
                (None, None) => {}
            }

            // Definitional proof irrelevance: any two inhabitants of a strict proposition are convertible. Placed after the metavar dispatch so a flexible side is still solved against the other (a metavar is not left dangling merely because its type is a proposition); a pair with neither side flexible was decided before either was reduced, above.
            if Sort::of_under(context, &posed, &mut Vec::new(), &type_)?
                .is_prop_to(context, "Convert::drain")
            {
                continue;
            }

            // Where the problem's type is a sort, nothing has typed this pair: it is two types, or a child whose type was not at hand — a stuck elimination's scrutinee, a projection's head. What a type directs between two neutrals is then asked of the type a lookup gives both sides, as the kernel asks it.
            if matches!(&*type_, Subterm::Sort(..))
                && by_their_own_type(context, &posed, &posed_pairs, &this, &that)?
            {
                continue;
            }

            let problem = Problem {
                type_: type_.clone(),
                this: this.clone(),
                that: that.clone(),
                binders: posed.clone(),
                pairs: posed_pairs.clone(),
                relation,
            };

            // Eta by the goal's type, ahead of every structural rule, as the kernel fires it: at a function type both sides are applied to fresh binders and at a record type with a field both are projected, whatever their shapes. A lambda or a tuple literal is left to its own arm below, which is the same expansion with the literal opened in place. Left to the dispatch's last arm, two sides of one stuck shape would be compared structurally first — two stuck eliminations arm against arm, at `Type` — and held apart where each converts with the literal and the kernel, which projects them, accepts.
            let literal = |term: &Term| matches!(&**term, Subterm::Func(_) | Subterm::Tuple(_));
            // The record type with no field has no eta, so a pair at it goes on to the dispatch.
            let expands = match &*type_ {
                Subterm::FuncType(_) => true,
                Subterm::TupleType(TupleType { telescope, .. }) => !telescope.is_empty(),
                _ => false,
            };
            if expands && !literal(&this) && !literal(&that) {
                let expanded = self.eta_expand_neutral(context, this, that, type_)?;
                assert!(
                    expanded,
                    "eta by a function or a record type enqueues its goals"
                );
                continue;
            }

            // A `Bool` connective against a term that is no intrinsic at all — absorption's shape, `b || (b && c)` against the bare `b` — which the intrinsic congruence below never sees: `curios-analysis`'s `connectives_agree` decides it equal or says nothing, and saying nothing leaves the pair to the dispatch as it was. The kernel asks the same function in its own fallback.
            if !matches!(
                (&*this, &*that),
                (Subterm::Intrinsic(_), Subterm::Intrinsic(_))
            ) && connectives_convert(context, &posed_pairs, &this, &that)?
            {
                continue;
            }

            // A family's former applied in full on both sides is the two nodes it builds. Compared as nodes, their levels go by the family's variance; as two projections of one group they would be a level question over every level the instantiation touched, a binder's annotation included.
            if let (Some(this_node), Some(that_node)) =
                (this.applied_former(), that.applied_former())
            {
                self.enqueue(type_, this_node, that_node);
                continue;
            }

            // A metavariable applied to distinct bound variables has one solution, the other side abstracted over them, and it is taken ahead of every rule that would guess at one. A candidate that cannot be committed yet waits, as a bare metavariable's does: no other rule's answer is this pair's.
            let mut abstracted = None;
            for (flex, rigid, rigid_raw, standing) in [
                (&this, &that, &that_raw, Standing::Under),
                (&that, &this, &this_raw, Standing::Over),
            ] {
                let standing = (relation == Relation::Below).then_some(standing);
                match self.abstract_flex_apply(context, flex, rigid, rigid_raw, standing)? {
                    Some(Solved::Done) => {
                        abstracted = Some(Solved::Done);
                        break;
                    }
                    Some(Solved::Postponed) => abstracted = Some(Solved::Postponed),
                    Some(Solved::Failed) | None => {}
                }
            }
            match abstracted {
                Some(Solved::Done) => continue,
                Some(Solved::Postponed) => {
                    self.blocked.push(raw_problem(&problem.type_));
                    continue;
                }
                Some(Solved::Failed) | None => {}
            }

            let ok = match (Term::unwrap_or_clone(this), Term::unwrap_or_clone(that)) {
                (Subterm::Intrinsic(this), Subterm::Intrinsic(that)) => {
                    convert_intrinsic(self, context, this, that)?
                }
                (Subterm::Sort(this_quality, this), Subterm::Sort(that_quality, that)) => {
                    context.same_quality_at(this_quality, that_quality, self.extent)
                        && Self::join_levels(context, &this, &that)?
                }
                (Subterm::Instance(this), Subterm::Instance(that)) => {
                    if !Self::compare_universes(context, &this.universes, &that.universes)? {
                        false
                    } else {
                        self.enqueue(type_, this.head.to_term(), that.head.to_term());
                        true
                    }
                }
                // Same-kind matches compare structurally; cross-kind pairs fall through to `eta_expand_neutral` (e.g. proof irrelevance at unit). One gate: a match stuck on a *flex* scrutinee decomposes against another match only when their motives and case tables are syntactically identical — then the two are instances of one source match and the scrutinee problem is an exact congruence (what solves `?b1 := yh` when `xor3(xh, ?b1, c)` meets `xor3(xh, yh, c)`). With differing arms the pair may be layer-misaligned — reduction carried the rigid-scrutinee side past the layer the flex side is stuck at (`xor3(xh, false, c)` reduces through its `b`-match while `xor3(xh, ?b1, c)` cannot) — and the scrutinee problem would solve the metavariable with the wrong layer's scrutinee. Such a pair falls through, and the blocked-on-metavariable path below parks it until the scrutinee's metavariable resolves.
                (Subterm::Match(this), Subterm::Match(that))
                    if mem::discriminant(&this.cases) == mem::discriminant(&that.cases)
                        && ((!flex_scrutinee(&this.head) && !flex_scrutinee(&that.head))
                            || (this.cases == that.cases && this.result == that.result)) =>
                {
                    self.compare_match(context, this, that)?
                }
                (Subterm::FuncType(this), Subterm::FuncType(that)) => {
                    self.compare_func_type(context, this, that, Relation::Equal)?
                }
                (Subterm::Func(this), Subterm::Func(that)) => {
                    self.compare_func(context, this, that, type_.clone())?
                }
                (Subterm::Func(func), other) => {
                    self.eta_expand_func(context, func, other.into(), type_.clone())?
                }
                (other, Subterm::Func(func)) => {
                    self.eta_expand_func(context, func, other.into(), type_.clone())?
                }
                // Both sides already reached `reduce`'s weak-head normal form before this dispatch (see `drain`), so a surviving `Apply` is genuinely stuck — same head or not, there is nothing further unfolding could reveal; `compare_apply` handles both uniformly (enqueuing the heads themselves when they differ, which recurs to a hard mismatch or resolves through whatever solves the head). Two folded calls are what the arm opens with, and unfolding does reveal more of them: a pair is unfolded against itself once ([`Pairs`]), and a comparison that grows past that spends the budget.
                (Subterm::Apply(this_a), Subterm::Apply(that_a)) => {
                    let this_head = this_a.head.as_rec_proj().map(|(g, i)| (g.clone(), i));
                    let that_head = that_a.head.as_rec_proj().map(|(g, i)| (g.clone(), i));

                    // Two projections of one group at two universe instances are one head exactly when their levels are identified: the kernel decides the same pair by its levels under the item's hypotheses, and an elaborator that assumed it would hand the kernel a pair it refuses. Identified, the spines decide by congruence; two unequal ground levels, or levels under a universe binder, are refused as the kernel refuses them; a pair that only an unsolved metavariable keeps from being a level question takes the delta step below, which is what solves the metavariable, *and* puts the two heads to the walk as a problem of their own, so the level question is decided once the metavariable is. They are posed at `Type`, where no eta opens them into the two calls again, so the problem parks where it is posed — the delta step alone could decide the pair structurally (a literal count reduces both matches to their zero arms) and accept it with the levels untouched, which is the pair the kernel refuses once the metavariable is zonked away; a pair the skeletons keep apart is two groups and takes the delta step alone.
                    let instances = match (&this_head, &that_head) {
                        (Some((this_group, index)), Some((that_group, that_index)))
                            if index == that_index && this_group != that_group =>
                        {
                            Some(level_question(
                                context,
                                &Term::rec_proj(this_group.clone(), *index),
                                &Term::rec_proj(that_group.clone(), *index),
                            )?)
                        }
                        _ => None,
                    };
                    if instances == Some(LevelQuestion::Blocked) {
                        self.enqueue(
                            Term::type_ground(),
                            this_a.head.clone(),
                            that_a.head.clone(),
                        );
                    }

                    // The same member of two groups whose skeletons part is one member where the groups are one as binders, and its two calls are then their spines. Sufficient and never necessary: a pair the comparison does not hold goes on to the delta step below.
                    let groups = match (&*this_a.head, &*that_a.head) {
                        (Subterm::Rec(this_rec), Subterm::Rec(that_rec))
                            if instances == Some(LevelQuestion::Distinct)
                                && this_a.arguments.len() == that_a.arguments.len() =>
                        {
                            Some(Self::two_groups(
                                context,
                                &self.binders,
                                &self.pairs,
                                this_rec,
                                that_rec,
                            )?)
                        }
                        _ => None,
                    };
                    if matches!(groups, Some(Outcome::Converts)) {
                        self.compare_spines(context, this_a, that_a)?;
                        continue;
                    }
                    // Whether an unsolved metavariable is what keeps a rule above from deciding the two heads: that they are one member, or one as binders.
                    let undecided = instances == Some(LevelQuestion::Blocked)
                        || matches!(groups, Some(Outcome::Blocked(_)));

                    match (this_head, that_head) {
                        (Some(this), Some(that))
                            if this == that || instances == Some(LevelQuestion::Identified) =>
                        {
                            self.compare_same_rec_apply(context, this_a, that_a, type_.clone())?
                        }
                        _ if matches!(
                            instances,
                            Some(
                                LevelQuestion::GroundUnequal
                                    | LevelQuestion::UnderBinder
                                    | LevelQuestion::Withheld
                            )
                        ) =>
                        {
                            false
                        }
                        // Applications of *different* folded members (or a folded member against another stuck head) may still compute the same value, so congruence proves nothing: take one symmetric delta step instead.
                        (Some(_), _) | (_, Some(_)) => {
                            let this_unfolded = unfold_rec_apply(context, this_a.clone())?;
                            let that_unfolded = unfold_rec_apply(context, that_a.clone())?;
                            match (this_unfolded, that_unfolded) {
                                (None, None) => self.compare_apply(context, this_a, that_a)?,
                                (this_unfolded, that_unfolded) => {
                                    // Both opened is a pair unfolded against itself, once: inside its own unfolding it is apart, since unfolded again it poses itself again. Where a metavariable keeps its heads undecided it waits for that instead, as the question of its levels does: a pair refused there would be refused by the order its metavariables were solved in.
                                    let pair = this_unfolded.is_some() && that_unfolded.is_some();
                                    let nested =
                                        pair && self.pairs.hold(&this_a.head, &that_a.head);
                                    if nested && undecided {
                                        self.blocked.push(raw_problem(&type_));
                                        continue;
                                    }
                                    if !nested {
                                        self.unfolded(
                                            this_unfolded.as_ref().map(|_| &this_a.head),
                                            that_unfolded.as_ref().map(|_| &that_a.head),
                                        );
                                        let this = this_unfolded
                                            .unwrap_or_else(|| Subterm::Apply(this_a).into());
                                        let that = that_unfolded
                                            .unwrap_or_else(|| Subterm::Apply(that_a).into());
                                        self.enqueue(type_.clone(), this, that);
                                    }
                                    !nested
                                }
                            }
                        }
                        _ => self.compare_apply(context, this_a, that_a)?,
                    }
                }
                // A folded recursive call against anything that is not one is *forced* before it is compared, exactly as the kernel's conversion forces both sides: a closed scan then runs as one machine run rather than as one unfolding per round. Unfolding it a step at a time would make a `Str` literal's validity check quadratic in retention and superlinear in wall clock — every round re-reducing the next folded spelling, whose argument carries the scan's state unreduced, one `step` deeper per character, and storing it. Forcing that makes no progress — a call restuck on a variable — takes the one-step unfold, so a comparison against an unfolded body still meets it.
                (Subterm::Apply(apply), other) if apply.head.spine_rec_proj().is_some() => {
                    let head = applied_head(&apply.head).clone();
                    match Self::force_folded_call(context, apply)? {
                        Some(forced) => {
                            self.unfolded(Some(&head), None);
                            self.enqueue(type_, forced, other.into());
                            true
                        }
                        None => false,
                    }
                }
                (other, Subterm::Apply(apply)) if apply.head.spine_rec_proj().is_some() => {
                    let head = applied_head(&apply.head).clone();
                    match Self::force_folded_call(context, apply)? {
                        Some(forced) => {
                            self.unfolded(None, Some(&head));
                            self.enqueue(type_, other.into(), forced);
                            true
                        }
                        None => false,
                    }
                }
                (Subterm::TupleType(this), Subterm::TupleType(that)) => {
                    self.compare_tuple_type(context, this, that)?
                }
                (Subterm::Tuple(this), Subterm::Tuple(that)) => {
                    self.compare_tuple(context, this, that, type_.clone())?
                }
                (Subterm::Tuple(tuple), other) => {
                    self.eta_expand_tuple(context, tuple, other.into(), type_.clone())?
                }
                (other, Subterm::Tuple(tuple)) => {
                    self.eta_expand_tuple(context, tuple, other.into(), type_.clone())?
                }
                (Subterm::Proj(this), Subterm::Proj(that)) => self.compare_proj(this, that)?,
                (Subterm::InductType(this), Subterm::InductType(that)) => {
                    self.compare_induct_type(context, this, that)?
                }
                (Subterm::Variant(this), Subterm::Variant(that)) => {
                    self.compare_variant(context, this, that)?
                }
                (Subterm::StructType(this), Subterm::StructType(that)) => {
                    self.compare_struct_type(context, this, that)?
                }
                (Subterm::Struct(this), Subterm::Struct(that)) => {
                    self.compare_struct(context, this, that)?
                }
                (Subterm::Struct(struct_), other) => {
                    self.eta_expand_struct(context, struct_, other.into(), type_.clone())?
                }
                (other, Subterm::Struct(struct_)) => {
                    self.eta_expand_struct(context, struct_, other.into(), type_.clone())?
                }
                (Subterm::Rec(this), Subterm::Rec(that)) => {
                    match (this.as_proj(), that.as_proj()) {
                        (Some(this_index), Some(that_index)) => {
                            let this_term: Term = Subterm::Rec(this.clone()).into();
                            let that_term: Term = Subterm::Rec(that.clone()).into();
                            // Two projections of one group at two universe instances are a level question before they are two groups: the member-wise comparison would commit the levels through the member types' sorts whatever the bodies then decide, an over-commitment the kernel's rule never makes, so the levels are identified only once nothing but levels keeps the sides apart, refused as the kernel refuses them, and parked while an unsolved metavariable is all that keeps the question open. Two groups the skeletons keep apart are one where they are one as binders, the one index of both being what pairs them. Two projections at *different* indices are asked neither: comparing their tails would only report that 0 is not 1.
                            let mut undecided = false;
                            let decided = match this_index == that_index {
                                false => None,
                                true => match level_question(context, &this_term, &that_term)? {
                                    LevelQuestion::Identified => Some(true),
                                    LevelQuestion::GroundUnequal
                                    | LevelQuestion::UnderBinder
                                    | LevelQuestion::Withheld => Some(false),
                                    LevelQuestion::Blocked => {
                                        self.blocked.push(raw_problem(&type_));
                                        continue;
                                    }
                                    LevelQuestion::Distinct => match Self::two_groups(
                                        context,
                                        &self.binders,
                                        &self.pairs,
                                        &this,
                                        &that,
                                    )? {
                                        Outcome::Converts => Some(true),
                                        Outcome::Blocked(_) => {
                                            undecided = true;
                                            None
                                        }
                                        Outcome::Mismatch(_) => None,
                                    },
                                },
                            };

                            // Two members no rule above decides are their two bodies, as the kernel forces both, and that once: met again inside their own unfolding they are apart, or wait where a metavariable left them undecided.
                            let nested =
                                decided.is_none() && self.pairs.hold(&this_term, &that_term);
                            if nested && undecided {
                                self.blocked.push(raw_problem(&type_));
                                continue;
                            }
                            match decided {
                                Some(decided) => decided,
                                None if nested => false,
                                None => {
                                    self.unfolded(Some(&this_term), Some(&that_term));
                                    self.enqueue(
                                        type_,
                                        this.group.member_body(this_index),
                                        that.group.member_body(that_index),
                                    );
                                    true
                                }
                            }
                        }
                        _ => self.compare_rec(context, this, that)?,
                    }
                }
                (Subterm::Rec(rec), other) => {
                    // Opening a projection's tail is the identity on it, so the step available here is the delta one: on to the member's body.
                    let head: Term = Subterm::Rec(rec.clone()).into();
                    let stepped = match rec.as_proj() {
                        Some(index) => rec.group.member_body(index),
                        None => unfold_rec(context, rec)?,
                    };
                    self.unfolded(Some(&head), None);
                    self.enqueue(type_, stepped, other.into());
                    true
                }
                (other, Subterm::Rec(rec)) => {
                    let head: Term = Subterm::Rec(rec.clone()).into();
                    let stepped = match rec.as_proj() {
                        Some(index) => rec.group.member_body(index),
                        None => unfold_rec(context, rec)?,
                    };
                    self.unfolded(None, Some(&head));
                    self.enqueue(type_, other.into(), stepped);
                    true
                }
                // Flex-apply imitation: a stuck application against a nominal type constructor. The unsolved-metavariable-head guard (and the `eta_expand_neutral` fallthrough for every other `Apply`) lives inside `imitate_flex_apply`.
                (Subterm::Apply(apply), Subterm::InductType(rigid)) => {
                    let rigid: Term = Subterm::InductType(rigid).into();
                    self.imitate_flex_apply(
                        context,
                        apply,
                        rigid,
                        &that_raw,
                        raw_problem(&problem.type_),
                    )?
                }
                (Subterm::InductType(rigid), Subterm::Apply(apply)) => {
                    let rigid: Term = Subterm::InductType(rigid).into();
                    self.imitate_flex_apply(
                        context,
                        apply,
                        rigid,
                        &this_raw,
                        raw_problem(&problem.type_),
                    )?
                }
                (Subterm::Apply(apply), Subterm::StructType(rigid)) => {
                    let rigid: Term = Subterm::StructType(rigid).into();
                    self.imitate_flex_apply(
                        context,
                        apply,
                        rigid,
                        &that_raw,
                        raw_problem(&problem.type_),
                    )?
                }
                (Subterm::StructType(rigid), Subterm::Apply(apply)) => {
                    let rigid: Term = Subterm::StructType(rigid).into();
                    self.imitate_flex_apply(
                        context,
                        apply,
                        rigid,
                        &this_raw,
                        raw_problem(&problem.type_),
                    )?
                }
                (
                    Subterm::Apply(apply),
                    Subterm::Intrinsic(
                        rigid @ (Intrinsic::ListType(_)
                        | Intrinsic::CellType(_)
                        | Intrinsic::ChannelType(_)
                        | Intrinsic::IoType(_)),
                    ),
                ) => {
                    let rigid: Term = Subterm::Intrinsic(rigid).into();
                    self.imitate_flex_apply(
                        context,
                        apply,
                        rigid,
                        &that_raw,
                        raw_problem(&problem.type_),
                    )?
                }
                (
                    Subterm::Intrinsic(
                        rigid @ (Intrinsic::ListType(_)
                        | Intrinsic::CellType(_)
                        | Intrinsic::ChannelType(_)
                        | Intrinsic::IoType(_)),
                    ),
                    Subterm::Apply(apply),
                ) => {
                    let rigid: Term = Subterm::Intrinsic(rigid).into();
                    self.imitate_flex_apply(
                        context,
                        apply,
                        rigid,
                        &this_raw,
                        raw_problem(&problem.type_),
                    )?
                }
                (this_n, that_n) => {
                    self.eta_expand_neutral(context, this_n.into(), that_n.into(), type_)?
                }
            };

            if !ok {
                // A structural mismatch where a side is stuck on an unsolved metavariable is undecided, not provably unequal: solving the metavariable may fold an intrinsic (`?m - 1` against `0` folds once `?m := 1` lands) or a match (`Below(?c, …)` reduces to `True` once `?c` is pinned and the arm refinement applies), which no structural rule anticipates. Park the problem instead of failing — rigid-head disagreements with no metavariable in a blocking position, which no solution can repair, still mismatch here.
                if blocked_on_unsolved_metavar(context, &problem.this)
                    || blocked_on_unsolved_metavar(context, &problem.that)
                {
                    self.blocked.push(raw_problem(&problem.type_));
                    continue;
                }
                // Two `Bool` terms the truth table declined are a mismatch nothing compared, and the report says so — ahead of their operands being unpaired, which is only what the table's silence left them.
                if past_bool_cap(context, &problem.this, &problem.that)? {
                    self.declined = Some(Declined::PastCap);
                }
                return Ok(false);
            }
        }

        Ok(true)
    }
}

/// Whether `type_`, a goal's type in weak-head normal form, is a type former. A literal's eta is refused at one that is not the literal's own, which each caller tells apart first: the two sides are not of one type there, and a literal with no field would convert with anything. A sort, an unsolved metavariable and a neutral type say nothing of the literal's type and keep the expansion — a sort is what a child nothing types is compared at. The kernel refuses the same goals by the same classifier.
fn another_former(context: &Context, type_: &Term) -> bool {
    type_.is_type_former(&context.syntax())
}

/// Whether `term`, in weak-head normal form, is a neutral inhabitant, which is all a literal's eta is taken against: every term whose head does not show its type ([`Subterm::shows_its_type`], `curios-core`). The kernel takes the rule against the same set, by the same classifier, and says why it is the set.
fn neutral(context: &Context, term: &Term) -> bool {
    !term.shows_its_type(&context.syntax())
}

/// Whether a struct literal holds a field that is data: `declared` is its type's fields, each read at the literal's own fields before it. A field whose type still waits on a metavariable is counted as data, so a literal is refused its eta only where every field is known to be a proof.
fn holds_data<B: Bound>(
    context: &mut Context,
    binders: &Binders,
    declared: Telescope<B>,
    fields: &[Term],
) -> Result<bool, ReduceError> {
    let mut cursor = declared.cursor();
    for field in fields {
        let Some((_, type_)) = cursor.entry() else {
            return Ok(false);
        };
        cursor.advance(field.clone());

        let type_ = match type_.has_metavar() {
            true => zonk_solved_term_metas(context, &type_),
            false => type_,
        };
        if type_.has_metavar()
            || !Sort::of_under(context, binders, &mut Vec::new(), &type_)?
                .is_prop_to(context, "holds_data")
        {
            return Ok(true);
        }
    }

    Ok(false)
}

/// What a type directs between two terms, asked of the type a lookup reads where the problem carries none: two terms a lookup types at one proposition are equal. The two looked-up types are compared, so it is the typed rule with its type looked up; `curios-cert`'s rule of the same name says why neither side is expanded — eta between two neutrals decides nothing irrelevance does not, and the goal it would pose at a looked-up type poses its two heads again, which the same lookup types.
///
/// Each side and each looked-up type is read through the solutions already committed, so the verdict does not follow whether an implicit was written or solved. Nothing here solves: one that still mentions an unsolved metavariable is left to the rules that do. A side is typed by [`looked_up`], under the binders the problem is posed under.
fn by_their_own_type(
    context: &mut Context,
    binders: &Binders,
    pairs: &Pairs,
    this: &Term,
    that: &Term,
) -> Result<bool, ReduceError> {
    let through = |context: &Context, term: &Term| match term.has_metavar() {
        true => zonk_solved_term_metas(context, term),
        false => term.clone(),
    };
    let (Some(this_type), Some(that_type)) = (
        looked_up(context, binders, this)?,
        looked_up(context, binders, that)?,
    ) else {
        return Ok(false);
    };

    // The type first, and the sides only where it decides: nearly every pair here is two types at a sort, which this leaves at the cost of two lookups.
    let this_type = through(context, &this_type);
    if this_type.has_metavar()
        || !Sort::of_under(context, binders, &mut Vec::new(), &this_type)?
            .is_prop_to(context, "by_their_own_type")
    {
        return Ok(false);
    }
    let that_type = through(context, &that_type);
    if through(context, this).has_metavar()
        || through(context, that).has_metavar()
        || that_type.has_metavar()
    {
        return Ok(false);
    }

    Ok(matches!(
        convert_under(
            context,
            binders,
            pairs,
            &Term::type_ground(),
            &this_type,
            &that_type
        )?,
        Outcome::Converts
    ))
}

/// The type a lookup reads for one side of a pair its position handed no type: a neutral's, off its head's binder or declaration ([`synth_neutral`]), and a constructor's value's off its declaration — the family at the value's parameters and at the index targets its constructor states for its payload. `curios-cert`'s function of the same name reads the same two.
fn looked_up(
    context: &mut Context,
    binders: &Binders,
    term: &Term,
) -> Result<Option<Term>, ReduceError> {
    let Subterm::Variant(Variant {
        name,
        universes,
        params,
        tag,
        payload,
        ..
    }) = &**term
    else {
        return synth_neutral(context, binders, &[], term);
    };
    let Some(declaration) = declared_at(context, name, universes, params)? else {
        return Ok(None);
    };
    let Some(signature) = declaration.instantiate(tag, params) else {
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

/// `name`'s declaration at one occurrence's universes, where the occurrence states as many parameters as it declares.
fn declared_at(
    context: &mut Context,
    name: &Global,
    universes: &Universes,
    params: &[Term],
) -> Result<Option<InductDecl>, ReduceError> {
    let Some(declaration) = context.induct_decl(name).cloned() else {
        return Ok(None);
    };
    if declaration.param_count() != params.len() {
        return Ok(None);
    }

    context
        .instantiate_induct_decl_at(&declaration, universes)
        .map(Some)
        .map_err(ReduceError::Universe)
}

/// What a pair of sides is as a level question, once the term metavariables already solved are materialized: [`Identification`]'s five verdicts, plus a pair that could still become a level question once its remaining metavariables are solved.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LevelQuestion {
    Identified,
    GroundUnequal,
    UnderBinder,
    Withheld,
    Blocked,
    Distinct,
}

/// Classify a pair whose levels no sibling problem will surface — two projections of one `rec` group, bare or at an applied head. The levels inside the two groups are decided here rather than by the walk: identified when the sides differ in nothing else, refused when two of them are unequal ground levels or are withheld, a group's member reproducing its levels under every unfolding so that no value decides the pair past them, parked when an unsolved metavariable is all that keeps the skeletons apart, and left to the caller only when the sides genuinely differ in structure — two groups, which their comparison as binders or the delta step may still relate. No type directs the pair from here, so a curried head keeps every level ([`Curried::Compared`]).
fn level_question(
    context: &mut Context,
    this: &Term,
    that: &Term,
) -> Result<LevelQuestion, ReduceError> {
    let this = zonk_solved_term_metas(context, this);
    let that = zonk_solved_term_metas(context, that);

    match identify_universe_levels(context, &this, &that, Curried::Compared)? {
        Identification::Identified => return Ok(LevelQuestion::Identified),
        Identification::GroundUnequal => return Ok(LevelQuestion::GroundUnequal),
        Identification::UnderBinder => return Ok(LevelQuestion::UnderBinder),
        Identification::Withheld => return Ok(LevelQuestion::Withheld),
        // No curried pair is set apart under the reading asked for, so the second is never answered.
        Identification::Distinct | Identification::Curried(_) => {}
    }

    if !this.any_metavar(&mut |_| true) && !that.any_metavar(&mut |_| true) {
        return Ok(LevelQuestion::Distinct);
    }

    // After zonking, every metavariable still standing is unsolved, so agreement modulo levels with those as wildcards is exactly "these could become one term modulo levels".
    let wildcard = |term: &Term| matches!(&**term, Subterm::Metavar(_));

    Ok(match this.level_differences(&that, wildcard, &()) {
        Some(_) => LevelQuestion::Blocked,
        None => LevelQuestion::Distinct,
    })
}

/// A family opened at the scrutinee's actual indices and the scrutinee — the elimination's own type. An unindexed family binds the scrutinee alone; an indexed one is opened at the indices the scrutinee's type carries, read by inference as a [`Probe`]. `None` where the scrutinee's type has no reading, which conversion answers as not convertible: incomplete, never unsound.
fn family_at_head(
    context: &mut Context,
    motive: &Scope<Many>,
    head: &Term,
) -> Result<Option<Term>, ReduceError> {
    let mut arguments = Vec::with_capacity(motive.arity());
    if motive.arity() > 1 {
        let Some(head_type) = infer(context, head).probed_refusal()? else {
            return Ok(None);
        };
        let Some(head_type) = reduce_forced(context, head_type).probed()? else {
            return Ok(None);
        };
        if let Subterm::InductType(InductType { indices, .. }) = &*head_type {
            arguments.extend(indices.iter().cloned());
        }
    }
    arguments.push(head.clone());
    if arguments.len() != motive.arity() {
        return Ok(None);
    }
    let refs = arguments.iter().collect::<Vec<_>>();
    Ok(Some(motive.open(&refs)))
}
