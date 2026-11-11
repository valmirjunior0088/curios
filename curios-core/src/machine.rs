//! Closed-term evaluation on an explicit stack: the machine that makes a fold cost what it computes rather than what the recursive strategy spends walking it.
//!
//! Both checkers' reducers are recursive functions that re-enter themselves once per operand of a nested intrinsic and once per link of a match tower, and both substitute arguments unreduced. On *closed* terms those are the two habits that manufacture cost the term never asked for: the re-entry puts one native frame — priced at [`Cost::FRAME`] — on the stack per element of a data-shaped walk, and the unreduced substitution threads accumulator chains that make every memo entry linear in how far the walk has come. This module evaluates the same terms with the recursion reified into a [`Frame`] stack and every substituted term taken to a value first, so depth costs the small frame it takes and the chains an unreduced substitution would thread are never built — an accumulator that is *genuinely* a growing closed neutral is the value itself, and the memo below keeps re-encountering one linear.
//!
//! # Why this is representation, not judgment
//!
//! The two checkers duplicate their reduction strategies on purpose: strategy decides which *open* terms convert, and a strategy bug shared by both checkers is a bug neither can catch. A closed term is outside that argument — it has one result, whichever order finds it — so evaluating one is the same kind of thing [`reduce_intrinsic`] is: arithmetic on the representation, shared so the two checkers cannot price the same program differently. The host still owns everything judgment-shaped, through the [`ClosedHost`] seam: what a global unfolds to, and every charge, which lands on the host's own counter through [`Reducer::spend`].
//!
//! # What the machine refuses to see
//!
//! The entry gate is [`accelerable`] — no local frees and no metavariables, two bits cached per node — and nothing else: an arm's case equation is no reason to stand aside. Both checkers record an equation only under a spelling that names a local (`curios_analysis::records_case_equation`) and probe one only at a stuck form that names a local, so no equation answers a term the machine takes, and the hosts run it on a closed term wherever the term stands. That is also what gives a closed term one reduct: evaluated by the machine inside an arm and by the recursive strategy outside it, it would reach the same value spelled two ways, and a remembered reduct, or a solution spelled from one, would follow which of the two was asked first. A term that fails the gate takes the ordinary recursive strategy, unchanged.
//!
//! # How the machine differs from the recursive strategy, and why each difference is safe
//!
//! **Substituted terms are evaluated first.** Arguments, `let` values, and `Induct` payload binds are taken to weak-head values before substitution, where the hosts substitute them unreduced. On a closed term the result is the same by confluence; what changes is cost, which is the point. Two consequences are handled rather than hoped away: a substituend whose evaluation *errors* falls back to its unreduced spelling — the hosts defer such an error until the position is demanded, and the fallback preserves exactly that — and a substituend whose evaluation exhausts the budget propagates, because spend is never refunded. An induction hypothesis is the deliberate exception: it is bound unreduced exactly as the hosts bind it, because evaluating one eagerly would run the whole tail fold whether or not the arm uses it. Once an arm does demand it, its value is recorded in the memo under the hypothesis term, so an arm projecting a tuple-valued hypothesis several times runs the tail fold once rather than once per projection.
//!
//! **A proof is left as written.** A substituend at a binder that states it binds a proof is: an argument, a `let`'s value and an arm's payload at one are a proof, whose value no judgment reads, so they are bound unreduced, and a body that eliminates one demands it where it does, the memo keeping the demand to one evaluation. The binder states it because the machine holds a value and a closed body where it substitutes, and no telescope to read a sort under ([`Scope::binds_proof`]). So is the proof of an operation's bound: no fold reads it ([`Intrinsic::bound`]), so the intrinsic arm, which forces the operands it folds over, never enters it. A type is evaluated as any substituend is, a family among them: conversion compares the reduct it is handed, and two instances of a definition apart in a level are told from one by whether they reach it as written or as what they unfold to.
//!
//! **A witness named at an instance is left as written.** An argument at a `use` position that is a global name at a universe instance is bound as that name, as a monomorphic witness's bare name is by the rule for names: the hosts leave it so, a method projected off it unfolds it where the method is demanded, and evaluated here its dictionary stands wherever an alias over the witness unfolds, in a type a report prints and resolution can no longer restore. The application states the position, since a function literal's binder does not. A family passed at an instance is no witness, and is evaluated as the paragraph above says.
//!
//! **A match that reads nothing of its scrutinee does not evaluate it.** One arm, no catch-all and no payload named ([`Cases::unread`]): the match is its arm's body, since a closed scrutinee can be no other case. A cast along a proof is written in that form, and the elaborator writes a payload as the index that pins it wherever the arm's equations hold the two one, so such a match runs none of its proof. An arm that still names a payload is bound to what the instance the match states says of it ([`Cases::stated`]), which the host reads the payload off ([`ClosedHost::stated_payload`]). The hosts take the same steps on a closed term, and on an open one fire by that instance's indices, which state the payload.
//!
//! **Eta is contracted, with the hosts' own probe.** `(x) => f(x)` steps to `f` exactly as under the strategies, each argument under its binder's mark ([`eta_head`]), through the fresh binder identities [`Reducer::fresh_binder`] mints. Skipping the probe on the theory that conversion decides eta anyway would starve the elaborator's witness keying, which reads rigid heads off weak-head forms and would not find `Option` under `(A) => Option(A)`.
//!
//! **`Induct` arms bind payload values.** The certifier binds an arm to the scrutinee's payload directly; the elaborator binds projections of the original head, guarding annotation holes a reduced payload could carry. A closed, metavariable-free payload carries none, so the machine uses the certifier's rule on both sides.
//!
//! # What a step costs
//!
//! A dispatch spends [`Cost::STEP`], an opening spends the certifier's beta formula, a frame is charged per new *peak* of machine depth at [`machine_frame`] — the few slots a [`Frame`] takes, priced honestly the way [`Cost::FRAME`] prices the native frame the recursive strategy takes — and every write to the memo is charged as the two handles it stores. That difference is the whole yield: a fold's per-element price falls from the native frame row to the transitions and openings the fold actually performs, and what the strategy amortized through its compilation-scoped memos the machine amortizes through its own, since its internal steps bypass the hosts' tables.
//!
//! # What a run leaves behind
//!
//! The memo is the host's to keep ([`ClosedMemo`], [`ClosedHost::closed_memo`]), and a run reads what the runs before it wrote. A closed term has one value at a demand under one definition store, whichever run finds it, so an answer stands for every later run its host makes while that store stands: in both checkers the declaration under judgment, each discarding the memo where it discards its own reducts. A term a run evaluated on the way to something else is then a probe when it is asked for by itself: the two sides of an equation, evaluated where the equation's type is formed and asked again by conversion, cost one evaluation each.

use {
    super::{
        Apply, Argument, Atom, Bound, Carrier, Cases, Cost, Exhaustion, Field, Free, FreeMonoid,
        Func, Global, Intrinsic, Layer, Let, LetBinding, Many, Match, MatchResult, Mention, Nat,
        Proj, Rec, RecGroup, ReduceError, Reducer, Scope, Sharing, Subterm, Telescope, Term,
        Universes, eta_head, instantiate_universes, project_literal, reduce_intrinsic,
    },
    crate::Foreign,
    curios_common::Plicity,
    std::{collections::HashMap, mem},
};

/// What one machine frame costs when it deepens the stack past its previous peak: a [`Frame`] is a few slots of reducer-owned storage, priced on the buffer row. The native-frame analogue is [`Cost::FRAME`], three orders of magnitude larger, and the gap between the two is what this module exists to stop spending.
fn machine_frame() -> Cost {
    Cost::buffer(2)
}

/// What the machine asks of its host: the reduction seam every intrinsic fold already drives — a fresh binder identity for the eta probe among it — plus the judgment the strategy owns, which is what a global unfolds to. Charges land on the host's counter through [`Reducer::spend`], so both checkers price a machine run identically because both run the same machine.
pub trait ClosedHost: Reducer {
    /// What the local `name` unfolds to: the value it was let-bound to, where it was.
    fn closed_body(&self, name: &Free) -> Option<&Term>;

    /// What the declaration `name` unfolds to at the stated universe instance `universes`, only where the instance states its scheme whole ([`UniverseContext::stated_by`](crate::UniverseContext::stated_by)).
    fn closed_body_at(&self, name: &Global, universes: &Universes) -> Option<&Term>;

    /// The payload the one arm of a closed match binds, read off the instance the match states ([`Cases::stated`]) with the scrutinee unevaluated: the payload of `tag`, where `stated` is an instance of a proposition whose indices state every component of it, and `None` where the scrutinee has to be evaluated to bind one. A closed proof of such a family is that constructor, so the indices are asked nothing. The machine reads no declaration, which is why this is the host's.
    fn stated_payload(
        &mut self,
        stated: &Term,
        tag: &Atom,
    ) -> Result<Option<Vec<Term>>, ReduceError>;

    /// Which payloads of the constructor `tag` of the family `family` its declaration states are proofs ([`InductDecl::payload_proofs`](crate::InductDecl::payload_proofs)), in payload order: what an arm firing on one of its values binds unreduced. Empty where the host holds no such entry, which binds every payload reduced and costs what a proof left unmarked does. A mark decides no reduct, so asking it is no read of the family.
    fn payload_proofs(&self, family: &Global, tag: &Atom) -> Vec<bool>;

    /// What this host's closed runs have answered, for as long as a closed term's value stands with it: the machine reads and writes it at every run, and the host discards it where what a name unfolds to may have changed.
    fn closed_memo(&mut self) -> &mut ClosedMemo;
}

/// What closed runs have answered, by the closed term asked: the machine's memo, which its host keeps past the run that filled it.
///
/// A closed term has one value at a demand under one definition store, whichever run finds it, so an entry answers every later run its host makes while that store stands. The second asking of a term a run evaluated on the way to something else — an equation's side, run to form the equation's type and asked again by conversion — is then a probe.
///
/// **An answer has one of two lives**, as a host's reducts do. One whose term and value are both [`lasting`] is the definition store's alone and stands for the declaration, whatever the host's scope does around it. Any other may rest on what a host rewrites in place, a level or a solution reached through a definition still being elaborated, and goes wherever the host's scoped answers go ([`ClosedMemo::clear_scoped`]).
#[derive(Debug, Default)]
pub struct ClosedMemo {
    /// The answers that stand for the declaration.
    lasting: Answers,
    /// The answers that go with the host's scope.
    scoped: Answers,
    /// The canonical node of each [`lasting`] term an answer is filed or asked under, for as long as the answers stand: a value built twice is one key, found by identity rather than compared node by node.
    keys: Sharing,
}

/// A host's scoped answers, set aside while a bracket of its scope runs and handed back where the bracket closes ([`ClosedMemo::set_aside`]).
#[derive(Debug, Default)]
pub struct AsideAnswers(Answers);

impl AsideAnswers {
    /// Forget every answer set aside.
    pub fn clear(&mut self) {
        self.0.clear();
    }

    /// Forget every answer set aside that names `name`, as [`ClosedMemo::retain_without`] does.
    pub fn retain_without(&mut self, name: &Mention) {
        self.0.retain_without(name);
    }
}

/// One life's answers, a table for each demand.
#[derive(Debug, Default)]
struct Answers {
    /// Values produced at the plain demand, keyed on the term as focused — intrinsic reducts among them, see [`Finish::Intrinsic`] for the chain it exists to keep linear. Each insertion is charged before it is made.
    values: HashMap<Term, Term>,
    /// Values produced at the forced demand. Kept apart because a term's value differs by demand exactly where a recursive call is involved: forced, a call is the value its fold computes; plain, it is the folded call itself. A plain request reads [`Answers::values`] alone, and a forced one either table, since a weak-head value that is not a folded spelling is its own forced form and a folded one is never recorded.
    forced: HashMap<Term, Term>,
}

impl Answers {
    fn clear(&mut self) {
        self.values.clear();
        self.forced.clear();
    }

    fn retain_without(&mut self, name: &Mention) {
        self.values.retain(|_, value| !value.mentions_name(name));
        self.forced.retain(|_, value| !value.mentions_name(name));
    }

    fn plain(&self, term: &Term) -> Option<&Term> {
        self.values.get(term)
    }

    fn forced(&self, term: &Term) -> Option<&Term> {
        self.forced.get(term)
    }
}

impl ClosedMemo {
    /// Forget every answer.
    pub fn clear(&mut self) {
        self.lasting.clear();
        self.scoped.clear();
        self.keys = Sharing::new();
    }

    /// Forget every answer that does not stand for the declaration.
    pub fn clear_scoped(&mut self) {
        self.scoped.clear();
    }

    /// Set the scoped answers aside for a bracket of the host's scope, leaving none in their place: what the bracket's runs answer is its own.
    pub fn set_aside(&mut self) -> AsideAnswers {
        AsideAnswers(mem::take(&mut self.scoped))
    }

    /// Hand back what [`ClosedMemo::set_aside`] set aside, where the bracket closes, the bracket's own scoped answers discarded.
    pub fn hand_back(&mut self, aside: AsideAnswers) {
        self.scoped = aside.0;
    }

    /// Forget every answer that names `name`: a name defined since can only move a value that read its absence, and a value that did holds the name.
    pub fn retain_without(&mut self, name: &Mention) {
        self.lasting.retain_without(name);
        self.scoped.retain_without(name);
    }

    /// `term` as an answer is filed and asked under: its canonical node where it is [`lasting`].
    fn key(&self, term: &Term) -> Term {
        match lasting(term) {
            true => self.keys.share(term),
            false => term.clone(),
        }
    }

    fn remembered(&self, term: &Term, demand: Demand) -> Option<&Term> {
        let term = &self.key(term);
        let plain = || self.lasting.plain(term).or_else(|| self.scoped.plain(term));
        match demand {
            Demand::Whnf => plain(),
            Demand::Forced => self
                .lasting
                .forced(term)
                .or_else(|| self.scoped.forced(term))
                .or_else(plain),
        }
    }

    /// Remember `value` as what `key` evaluates to at `demand`, in the life the two give the answer. A value that is not [`its_own_forced_form`] is remembered nowhere, since [`ClosedMemo::remembered`] reads the plain table for a forced probe too and there is no demand such a value could be filed under safely.
    fn remember(&mut self, key: Term, value: Term, demand: Demand) {
        if !its_own_forced_form(&value) {
            return;
        }

        let key = self.key(&key);
        let answers = match lasting(&key) && lasting(&value) {
            true => &mut self.lasting,
            false => &mut self.scoped,
        };
        match demand {
            Demand::Whnf => answers.values.insert(key, value),
            Demand::Forced => answers.forced.insert(key, value),
        };
    }
}

/// Whether `value`, produced at [`Demand::Whnf`], is its own forced form: true of every weak-head value but a folded recursive spelling, since forcing only unfolds recursive heads.
///
/// **One rule, read everywhere a plain answer could reach a forced reader.** The [`ClosedMemo`] files no value this declines, because `remembered` reads the plain table for a forced probe too; a caller reads it to size that write's charge, so nothing is charged for a write that will not happen; and the [`Tap`] reads it to decide whether an operand produced plainly may answer a fold that forces. Stated once because the three are the same fact: a forced reader may be handed a plain answer exactly where forcing would have changed nothing.
fn its_own_forced_form(value: &Term) -> bool {
    value.spine_rec_proj().is_none() && !matches!(&**value, Subterm::Rec(_))
}

/// Whether an answer about `term` can be the definition store's alone: it names no local, no metavariable and no universe metavariable. A host may rewrite a level or unwind a solution in place, and reads a local through its scope, so an answer naming none of them is one no change of scope reaches.
pub fn lasting(term: &Term) -> bool {
    !term.has_local_free() && !term.has_metavar() && !term.has_universe_meta()
}

/// Whether `term` is in the machine's domain: closed, and holding no local free and no metavariable, read off the per-node cached bits in O(1). It is the whole gate, an arm's equations included: see the module documentation.
///
/// Closedness is part of the gate and not an assumption about the caller. The machine evaluates by looking a variable up — a definition for a free one, nothing for a local — and an index with no binder is neither: it denotes whatever the depth a reader meets it at gives it, so the machine would have to answer about a term that is not the one it was handed. A term carrying one is refused where a module enters ([`open_positions_of`](crate::open_positions_of)), and this is what keeps that the only place it has to be: declining it here hands it to the host's own reduction, which stops on it as on any variable it cannot resolve, rather than to a walk that would assert.
pub fn accelerable(term: &Term) -> bool {
    term.closed() && !term.has_local_free() && !term.has_metavar()
}

/// Whether `term` is its own weak-head value in the machine's domain ([`accelerable`]): a former reduction leaves as it is — a sort, a function, tuple, structure or inductive type, a tuple, a structure or a variant — however it is demanded.
///
/// **Its answer is the node.** The machine hands one back as the node it was given, and so does every host's reducer at its door, before any table is asked of it: a table would file it under itself, and asking conses it as a key, which walks a node a declaration has not met however shared it is. Lean 4's kernel stands the same easy cases outside its table (`whnf`'s "Do not cache easy cases"). A lambda is not one, its table entry recording its η probe, and nor is an intrinsic, which reduction may fold.
pub fn weak_head_value(term: &Term) -> bool {
    accelerable(term)
        && matches!(
            &**term,
            Subterm::Sort(_)
                | Subterm::FuncType(_)
                | Subterm::TupleType(_)
                | Subterm::Tuple(_)
                | Subterm::InductType(_)
                | Subterm::Variant(_)
                | Subterm::StructType(_)
                | Subterm::Struct(_)
        )
}

/// How far the caller wants the focus taken: [`Demand::Whnf`] is the plain reducer contract, where a folded recursive application is its own normal form; [`Demand::Forced`] is the eliminator's, where a `rec` head unfolds because a value is required.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Demand {
    Whnf,
    Forced,
}

/// Evaluate a closed `term` to the value `demand` asks for, spending from `host`'s counter.
///
/// The caller is responsible for the gate: `term` must satisfy [`accelerable`], and no case-equation refinement may be in scope. The result is a weak-head value under the host's own reduction rules — computed in constant native depth per element of any data-shaped walk, which is the entire difference.
pub fn reduce_closed<H: ClosedHost>(
    host: &mut H,
    term: Term,
    demand: Demand,
) -> Result<Term, ReduceError> {
    curios_profile::profile!("machine::reduce_closed");
    Machine {
        stack: Vec::new(),
        peak: 0,
    }
    .run(host, term, demand)
}

/// What to do next: evaluate a term at a demand, or hand a finished value to the innermost frame.
enum Step {
    Eval(Term, Demand),
    Value(Term),
}

/// What an [`Args`] run does with its collected values once the last one is in.
enum Finish {
    /// Open a function telescope over the values and continue with the body.
    Beta {
        telescope: Telescope<Term>,
        demand: Demand,
    },
    /// Open an `Induct` arm over the payload values.
    Arm { body: Scope<Many>, demand: Demand },
    /// Fold the intrinsic over the values ([`Machine::fold`]), and remember the result under `key` — the whole intrinsic term as focused — then hand it on at `demand`, the one the intrinsic was evaluated at.
    ///
    /// The memo is what keeps a *closed neutral* accumulator linear. A fold whose combining operation is genuinely stuck — byte arithmetic no rule folds, a runtime-only operation — accumulates a neutral chain one link deeper per element, exactly as it does under the hosts; without the memo the machine would re-walk that chain at every element, and a pathological program would price *worse* here than under the strategy this replaces. With it, a chain link evaluates once and every later encounter is one probe.
    ///
    /// **A forced demand forces what the fold answers.** A fold's answer is a plain form: a list fold whose step hands its accumulator back answers its initial value, which may be a folded recursive call. A match and a projection evaluate their head at [`Demand::Forced`], and the strategy forces what `whnf` answered before it dispatches; handed the folded call, the machine's match stood stuck, and a recursion stepping through one never computed. So an answer that is not its own forced form is evaluated again at the forced demand, on this stack.
    Intrinsic {
        key: Term,
        intrinsic: Intrinsic,
        demand: Demand,
    },
}

/// An intrinsic folded over its operands' values, as a [`Tap`] serves them.
struct Fold {
    /// The whole intrinsic term as focused, which the answer is remembered under.
    key: Term,
    intrinsic: Intrinsic,
    /// The demand the intrinsic was evaluated at.
    demand: Demand,
    /// Each operand's value at the plain demand, by the operand as written.
    values: HashMap<Term, Term>,
    /// The forced value of each operand the fold has read forced whose plain value is a folded recursive call, by the operand as written.
    forced: HashMap<Term, Term>,
}

/// A left-to-right evaluation of a fixed list of terms, then a [`Finish`] over the values.
///
/// `catch` marks the positions the hosts substitute unreduced — beta arguments and arm payload binds — where an evaluation error must fall back to the original spelling rather than surface, because the host would have deferred it until the position was demanded. Intrinsic and foreign operands are strict in the hosts too, so their runs do not catch.
///
/// `written` says which of those positions are bound as they are written, one answer per term and none where no position is: the ones whose binder states that it binds a proof ([`Scope::binds_proof`]), and at a beta step a witness named at an instance. No judgment reads a proof's value, so the run does not compute it; a body that eliminates one demands it where it does, once, through the memo.
struct Args {
    originals: Vec<Term>,
    written: Vec<bool>,
    done: Vec<Term>,
    next: usize,
    item_demand: Demand,
    catch: bool,
    finish: Finish,
}

/// One reified level of the recursive strategy: what the enclosing computation will do with the value under focus.
enum Frame {
    /// An application waiting for its head. `memo_key` is the application itself, recorded with its value when the head exposes a plain function — see the machine's eval arm for why the decision lives here.
    Head {
        arguments: Vec<Argument>,
        demand: Demand,
        memo_key: Option<Term>,
    },
    /// A forced recursive application waiting for its member's body to expose a function.
    RecBody {
        params: Vec<Argument>,
        folded: Term,
        group: RecGroup,
    },
    /// A forced application whose head is itself a folded call — a member whose result is a function, applied past its own parameters — waiting for that call's forced value to apply its own arguments to.
    Spine {
        arguments: Vec<Argument>,
        demand: Demand,
    },
    /// The restuck test of `force`: a reduct that is still neutral — a match, a variable, a metavariable, a projection, or an application no unfolding opens — and still mentions the group achieved nothing, and the folded spelling stays the canonical normal form.
    RecGuard {
        folded: Term,
        group: RecGroup,
    },
    /// A match waiting for its scrutinee.
    MatchK {
        result: MatchResult,
        cases: Cases,
        demand: Demand,
    },
    /// A projection waiting for its head: the slot it reads.
    ProjK {
        field: Field,
        demand: Demand,
    },
    /// Record the value about to be produced under `key` in the memo, in the table of the `demand` it is produced at — pushed under an application, a member selection or a match, so the second occurrence of the same closed call — or the second projection of the same induction hypothesis — is one probe. This is the machine's spelling of the amortization the hosts get from their own memos, which the machine's internal steps bypass.
    Memo {
        key: Term,
        demand: Demand,
    },
    /// A `let` run evaluating its bindings left to right; `released` holds the values already in.
    LetK {
        released: Vec<Term>,
        current: Term,
        bindings: Vec<LetBinding>,
        next: usize,
        tail: Scope<Many>,
        demand: Demand,
    },
    Args(Args),
    /// A fold waiting for the forced value of `operand`, which it read forced and whose plain value is a folded recursive call: filed in the fold's forced table, and the fold run again.
    Forcing {
        fold: Fold,
        operand: Term,
    },
}

/// The [`Reducer`] [`Machine::fold`] hands to [`reduce_intrinsic`]: operand lookups are served from the operands the run just evaluated, so the fold sees them at constant native depth, and anything it conjures beyond them takes the host's ordinary path.
///
/// **Produced at the demand a fold reads them by.** The operands are evaluated at [`Demand::Whnf`], and a plain value is its own forced form for everything but a folded recursive spelling ([`its_own_forced_form`]). That one shape is forced on the machine's stack the first time the fold reads it forced: the `Tap` stops the fold there, naming the operand in `wanted`, and the machine forces its value and folds again with it in `forced`. So the machine forces what the fold reads forced and nothing it reads plainly, as the recursive strategy does. Producing every operand forced and serving either demand from one table handed a fold the forced value of an operand it asked the plain value of, and a fold over the empty list — which is its initial value, read plainly — then answered that value's forced form where the recursive strategy answers the folded call. Handing the folded spelling to the host's `reduce_forced` started a machine of the host's per level on the native stack, so a closed recursion through an operand, `sum(k) + 1`, overflowed it; falling through to the host for *every* forced read cost too: a 32-link arithmetic chain exhausted a billion units.
struct Tap<'a, H: Reducer> {
    host: &'a mut H,
    values: &'a HashMap<Term, Term>,
    forced: &'a HashMap<Term, Term>,
    /// The operand the fold stopped at: read forced, its plain value a folded recursive spelling, and its forced value not yet in `forced`.
    wanted: Option<Term>,
}

impl<H: Reducer> Reducer for Tap<'_, H> {
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
        match self.values.get(&term) {
            Some(value) => Ok(value.clone()),
            None => self.host.reduce(term),
        }
    }

    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
        let Some(value) = self.values.get(&term) else {
            return self.host.reduce_forced(term);
        };
        if its_own_forced_form(value) {
            return Ok(value.clone());
        }
        match self.forced.get(&term) {
            Some(forced) => Ok(forced.clone()),
            None => {
                self.wanted = Some(term);
                // Exhaustion, which every fold and every probe hands on, so the fold stops at this read; the machine reads `wanted` first, and the error never leaves it.
                Err(ReduceError::exhausted(0, Cost::term(0)))
            }
        }
    }

    fn spend(&mut self, cost: Cost) -> Result<(), ReduceError> {
        self.host.spend(cost)
    }

    fn fresh_binder(&mut self, hint: Option<&str>) -> Free {
        self.host.fresh_binder(hint)
    }
}

struct Machine {
    stack: Vec<Frame>,
    peak: usize,
}

impl Machine {
    fn run<H: ClosedHost>(
        mut self,
        host: &mut H,
        term: Term,
        demand: Demand,
    ) -> Result<Term, ReduceError> {
        let mut step = Step::Eval(term, demand);

        loop {
            let outcome = match step {
                Step::Eval(term, demand) => self.eval(host, term, demand),
                Step::Value(value) => match self.stack.pop() {
                    None => return Ok(value),
                    Some(frame) => self.apply(host, frame, value),
                },
            };

            step = match outcome {
                Ok(next) => next,
                Err(error) => self.catch(error)?,
            };
        }
    }

    /// Resume from an evaluation error at the nearest catching frame, or propagate it.
    ///
    /// The frames between the error site and the catcher are discarded wholesale: the host would have substituted the whole original spelling unreduced, so everything inside it is deferred together. Exhaustion always propagates — the budget never refunds, so there is nothing to resume with.
    fn catch(&mut self, error: ReduceError) -> Result<Step, ReduceError> {
        if error.is_exhausted() {
            return Err(error);
        }

        while let Some(frame) = self.stack.pop() {
            match frame {
                Frame::Args(args) if args.catch => {
                    let original = args.originals[args.next - 1].clone();
                    self.stack.push(Frame::Args(args));
                    return Ok(Step::Value(original));
                }
                // A `let` value the hosts would have substituted unreduced: resume with the released spelling, deferring the error to whatever demands the binding.
                Frame::LetK {
                    released,
                    current,
                    bindings,
                    next,
                    tail,
                    demand,
                } => {
                    let original = current.clone();
                    self.stack.push(Frame::LetK {
                        released,
                        current,
                        bindings,
                        next,
                        tail,
                        demand,
                    });
                    return Ok(Step::Value(original));
                }
                _ => {}
            }
        }

        Err(error)
    }

    /// Push a frame, charging [`machine_frame`] when it deepens the stack past its previous peak — the machine-frame reading of the rule [`Cost::FRAME`] states for native levels.
    fn push<H: Reducer>(&mut self, host: &mut H, frame: Frame) -> Result<(), ReduceError> {
        if self.stack.len() + 1 > self.peak {
            self.peak = self.stack.len() + 1;
            host.spend(machine_frame())?;
        }

        self.stack.push(frame);
        Ok(())
    }

    fn eval<H: ClosedHost>(
        &mut self,
        host: &mut H,
        term: Term,
        demand: Demand,
    ) -> Result<Step, ReduceError> {
        host.spend(Cost::STEP)?;

        #[cfg(test)]
        if std::env::var_os("CURIOS_MACHINE_TRACE").is_some() {
            eprintln!("eval depth={} {term:?}", self.stack.len());
        }

        // A projection is the fixed point of `rec` unfolding: opening its tail yields the same term, so at a plain demand it *is* its own weak-head form, decided from the shape.
        if matches!(demand, Demand::Whnf) && term.as_rec_proj().is_some() {
            return Ok(Step::Value(term));
        }

        // A weak-head value is its own answer, handed back as the node it was given and asked of no table.
        if weak_head_value(&term) {
            return Ok(Step::Value(term));
        }

        // A plain request reads what plain requests produced; a forced one may read either — see [`Machine::forced`]. Keyed on the term alone, a forced call's or a forced match's value answered a later plain request for the same term, where the strategy answers the folded call.
        if let Some(value) = host.closed_memo().remembered(&term, demand) {
            return Ok(Step::Value(value.clone()));
        }

        // The forced demand steps into the member's body, and the value is remembered under the projection so a recursive member's body is opened and eta-probed once rather than once per call.
        if let Some((group, index)) = term.as_rec_proj() {
            let body = group.member_body(index);
            let key = term.clone();
            self.push(
                host,
                Frame::Memo {
                    key,
                    demand: Demand::Forced,
                },
            )?;

            return Ok(Step::Eval(body, Demand::Forced));
        }

        match Term::unwrap_or_clone(term) {
            Subterm::Var(var) => match var.as_free().and_then(|free| host.closed_body(free)) {
                Some(body) => Ok(Step::Eval(body.clone(), demand)),
                None => Ok(Step::Value(Term::var(var))),
            },

            aps @ Subterm::Apply(_) => {
                // The application is remembered whole so that, if its head turns out to be a plain function, its value can be recorded under it — the same closed call, `step` at the same byte and state, then evaluates once and probes thereafter. Whether to record is the head frame's decision, because only there is the head known: a member selection's calls never repeat within a run (their fold argument strictly shrinks), and a folded spelling must never be served to a demand that would unfold it.
                let key = Term::from(aps);
                let Subterm::Apply(Apply { head, arguments }) = Term::unwrap_or_clone(key.clone())
                else {
                    unreachable!("rebuilt from the same value")
                };
                self.push(
                    host,
                    Frame::Head {
                        arguments,
                        demand,
                        memo_key: Some(key),
                    },
                )?;
                Ok(Step::Eval(head, Demand::Whnf))
            }

            mtc @ Subterm::Match(_) => {
                // The match is remembered whole, for the induction hypothesis above all: `dispatch` binds it unreduced, as this very term, and an arm that projects a tuple-valued hypothesis three times demands the same term three times. Recorded under the key on the way out, the first demand runs the tail fold and the rest are probes; without the record each projection ran the fold again, and a fold whose arm projected its hypothesis `k` times cost `k` to the power of its length.
                let key = Term::from(mtc);
                let Subterm::Match(Match {
                    head,
                    result,
                    cases,
                }) = Term::unwrap_or_clone(key.clone())
                else {
                    unreachable!("rebuilt from the same value")
                };
                // A match with one arm and no catch-all, naming none of its scrutinee's payload, is its arm's body: on a closed term the scrutinee can be no other case, so nothing evaluates it, a proof least of all.
                if let Some(body) = cases.unread() {
                    return Ok(Step::Eval(body, demand));
                }
                // An arm that names its payload is bound to what the instance the match states says of it, where it states one the host reads a payload off: the scrutinee is no more evaluated than above.
                if let Some((tag, arm)) = cases.sole_arm()
                    && let Some(stated) = cases.stated()
                    && let Some(payload) = host.stated_payload(stated, tag)?
                    && payload.len() == arm.arity()
                {
                    let refs = payload.iter().collect::<Vec<_>>();
                    return Ok(Step::Eval(arm.open(&refs), demand));
                }
                self.push(host, Frame::Memo { key, demand })?;
                self.push(
                    host,
                    Frame::MatchK {
                        result,
                        cases,
                        demand,
                    },
                )?;
                Ok(Step::Eval(head, Demand::Forced))
            }

            Subterm::Proj(Proj { head, field }) => match field.slot() {
                Some(_) => {
                    self.push(host, Frame::ProjK { field, demand })?;
                    Ok(Step::Eval(head, Demand::Forced))
                }
                // A label or a written position that survived to reduction names no slot yet and stays stuck, exactly as it does in both hosts.
                None => Ok(Step::Value(Term::from(Subterm::Proj(Proj { head, field })))),
            },

            // A block whose tail binds other than one binder per binding opens over nothing: it is its own value, and nothing is charged, since nothing was built.
            Subterm::Let(let_) if !let_.opens() => Ok(Step::Value(Term::from(Subterm::Let(let_)))),

            Subterm::Let(Let { bindings, tail }) => {
                // The certifier's zeta charge: one values vector, a ref vector per binding (triangular), and one substitution pass per binding.
                let count = bindings.len() as u64;
                host.spend(
                    Cost::collection(count)
                        .saturating_add(Cost::buffer(
                            count.saturating_mul(count.saturating_add(1)) / 2,
                        ))
                        .saturating_add(Cost::term(1).saturating_mul(count)),
                )?;

                self.let_advance(
                    host,
                    Vec::with_capacity(bindings.len()),
                    bindings,
                    0,
                    tail,
                    demand,
                )
            }

            Subterm::Rec(rec) => match demand {
                // A `rec` block is weak-head normal; only a demand for its value opens the tail, and a tail that binds other than one binder per member opens over nothing, so the block is its own value at either demand.
                Demand::Whnf => Ok(Step::Value(Term::from(Subterm::Rec(rec)))),
                Demand::Forced if !rec.opens() => Ok(Step::Value(Term::from(Subterm::Rec(rec)))),
                Demand::Forced => Ok(Step::Eval(unfold_rec(rec), Demand::Forced)),
            },

            Subterm::Intrinsic(intrinsic) => {
                // The operands the operation computes with, and not the proof of its bound: no fold reads it ([`Intrinsic::bound`]), and forced, a certificate that is a lemma proved by induction runs its induction at every closed call.
                let mut operands = Vec::new();
                intrinsic.any_computed(&mut |operand| {
                    operands.push(operand.clone());
                    false
                });

                let count = operands.len();
                self.args(
                    host,
                    Args {
                        originals: operands,
                        written: Vec::new(),
                        done: Vec::with_capacity(count),
                        next: 0,
                        // At the plain demand, so a plain read is served what it asked for; the [`Tap`] has a forced read of a folded call produced on this stack. The hosts' folds reduce their operands strictly, so an operand's error surfaces here as it does there.
                        item_demand: Demand::Whnf,
                        catch: false,
                        finish: Finish::Intrinsic {
                            key: Term::from(Subterm::Intrinsic(intrinsic.clone())),
                            intrinsic,
                            demand,
                        },
                    },
                )
            }

            // A host call is its declaration and its operands, and reduction leaves both as they were posed — which is how conversion reads one and how the recursive strategy leaves it, its loop having no arm for this node. Running the operands here reduced terms the strategy stands on, so one closed term had two spellings; the confluence this module argues eager evaluation from is about a position a value is *substituted* into, and a node reduction stops on is not one, nothing being substituted and both terms being the final answer.
            Subterm::Foreign(Foreign {
                function,
                declared,
                arguments: operands,
            }) => Ok(Step::Value(Term::from(Subterm::Foreign(Foreign {
                function,
                declared,
                arguments: operands,
            })))),

            Subterm::Instance(instance) => {
                let universes = instance.universes;
                let name = instance.name;
                let Some(reduct) = host.closed_body_at(&name, &universes).cloned() else {
                    return Ok(Step::Value(Term::instance(name, universes)));
                };
                // A stored body holding a projection needs no special case: a group binds no universe, so the levels its members spell are this declaration's and one walk substitutes them with every other level the instance states.
                let reduct =
                    instantiate_universes(&reduct, &universes).map_err(ReduceError::Universe)?;

                Ok(Step::Eval(reduct, demand))
            }

            // The entry gate excludes metavariables, but a definition *body* the machine unfolds mid-run may still carry solved ones — the elaborator zonks its store at module boundaries, not per definition. Resolution is the host strategy's judgment, so the node bails to it; a metavariable the host leaves standing is genuinely unsolved and is its own normal form, as it is in both hosts.
            metavar @ Subterm::Metavar(_) => {
                let resolved = host.reduce(Term::from(metavar))?;

                match &*resolved {
                    Subterm::Metavar(_) => Ok(Step::Value(resolved)),
                    _ => Ok(Step::Eval(resolved, demand)),
                }
            }

            // Eta for functions, exactly the hosts' probe ([`eta_head`]): `(x) => f(x)` is `f` provided each argument stands under its binder's mark and `f` mentions none of the binders. This is not a normalization nicety — the elaborator's witness keying reads rigid heads off weak-head forms, so `(A) => Option(A)` must contract to `Option` here as it does under the strategy.
            Subterm::Func(func) => {
                let arity = func.telescope.len();
                host.spend(
                    Cost::collection(arity as u64)
                        .saturating_mul(3)
                        .saturating_add(Cost::term(1).saturating_mul(arity as u64)),
                )?;

                let binders = (0..arity)
                    .map(|_| host.fresh_binder(None))
                    .collect::<Vec<_>>();

                match eta_head(&func, &binders) {
                    Some(head) => Ok(Step::Eval(head, demand)),
                    None => {
                        // Remember the probe's negative outcome under the function itself, so a body reached again — a plain definition's, unfolded at every call — pays the eta probe once.
                        let value = Term::from(Subterm::Func(func));
                        host.spend(Cost::buffer(2))?;
                        host.closed_memo()
                            .remember(value.clone(), value.clone(), Demand::Whnf);
                        Ok(Step::Value(value))
                    }
                }
            }

            // What is left is a transient, which no reducer is handed: left as it is.
            value => Ok(Step::Value(Term::from(value))),
        }
    }

    fn apply<H: ClosedHost>(
        &mut self,
        host: &mut H,
        frame: Frame,
        value: Term,
    ) -> Result<Step, ReduceError> {
        match frame {
            Frame::Head {
                arguments,
                demand,
                memo_key,
            } => {
                // Recorded once here, for every shape the head turns out to have, rather than by each arm below deciding for itself. Deferring the decision to the arms would rest on two grounds, and only one holds: a folded spelling must indeed never be served to a demand that would unfold it, which [`Frame::Memo`] itself enforces by refusing to record a rec-shaped value — but *a member selection's calls never repeat within a run* is false. `let (a, b) = go(…)` is projection sugar, so one call is demanded once per component, and with the recursive arm left unrecorded each level would double the work: a fold returning a pair would cost exponentially more than its single-value twin.
                if let Some(key) = memo_key {
                    self.push(host, Frame::Memo { key, demand })?;
                }

                if value.as_rec_proj().is_some() {
                    return match demand {
                        // A folded recursive application is the canonical weak-head normal form.
                        Demand::Whnf => Ok(Step::Value(Term::from(Subterm::Apply(Apply {
                            head: value,
                            arguments,
                        })))),
                        Demand::Forced => {
                            let (group, index) = value.as_rec_proj().expect("checked above");
                            let (group, index) = (group.clone(), index);
                            let params = arguments.clone();
                            let folded = Term::from(Subterm::Apply(Apply {
                                head: value,
                                arguments,
                            }));
                            let body = group.member_body(index);
                            self.push(
                                host,
                                Frame::RecBody {
                                    params,
                                    folded,
                                    group,
                                },
                            )?;
                            Ok(Step::Eval(body, Demand::Forced))
                        }
                    };
                }

                match Term::unwrap_or_clone(value) {
                    // `expose_rec_tail`: a `rec` block at an application head strips to its tail even under plain reduction. One whose tail binds other than one binder per member strips to nothing and falls to the stuck application below, as an unsaturated function does.
                    Subterm::Rec(rec) if rec.opens() => {
                        // The memo frame is already on the stack from this frame's entry, so the re-pushed head carries no key of its own — one record per application, whatever chain of heads it strips through.
                        self.push(
                            host,
                            Frame::Head {
                                arguments,
                                demand,
                                memo_key: None,
                            },
                        )?;
                        Ok(Step::Eval(unfold_rec(rec), Demand::Whnf))
                    }
                    Subterm::Func(Func { telescope, .. }) if telescope.len() == arguments.len() => {
                        self.beta(host, telescope, arguments, demand)
                    }
                    // A head evaluated at the plain demand comes back a folded call when the application is a member's call applied past its own parameters. A forced demand reaches through it, as the strategies' `unfold_rec_apply` does: force the call, and apply what it becomes to these arguments. A plain demand stops at the folded spine, its normal form.
                    head if matches!(demand, Demand::Forced) && head.spine_rec_proj().is_some() => {
                        self.push(host, Frame::Spine { arguments, demand })?;
                        Ok(Step::Eval(head.into(), Demand::Forced))
                    }
                    head => Ok(Step::Value(Term::from(Subterm::Apply(Apply {
                        head: head.into(),
                        arguments,
                    })))),
                }
            }

            Frame::Spine { arguments, demand } => match Term::unwrap_or_clone(value) {
                Subterm::Func(Func { telescope, .. }) if telescope.len() == arguments.len() => {
                    self.beta(host, telescope, arguments, demand)
                }
                // The call did not expose a saturated function — it restuck, and `force` keeps it folded — so its application stays the normal form. Nothing is forced again, which is what keeps a restuck spine from looping here.
                head => Ok(Step::Value(Term::from(Subterm::Apply(Apply {
                    head: head.into(),
                    arguments,
                })))),
            },

            Frame::RecBody {
                params,
                folded,
                group,
            } => match Term::unwrap_or_clone(value) {
                Subterm::Func(Func { telescope, .. }) if telescope.len() == params.len() => {
                    self.push(host, Frame::RecGuard { folded, group })?;
                    self.beta(host, telescope, params, Demand::Forced)
                }
                // The member's body did not expose a saturated function: the folded spelling stays the normal form, exactly as `force` keeps it.
                _ => Ok(Step::Value(folded)),
            },

            Frame::RecGuard { folded, group } => {
                // An application reaching here is one no unfolding opened, which `force` reads as stuck as it reads a match on a variable: `g(member(2))` under an opaque `g` is the folded `member(3)` under both.
                let neutral = matches!(
                    &*value,
                    Subterm::Match(_)
                        | Subterm::Var(_)
                        | Subterm::Instance(_)
                        | Subterm::Metavar(_)
                        | Subterm::Proj(_)
                        | Subterm::Apply(_)
                );

                match neutral && value.mentions_rec_member(&group) {
                    true => Ok(Step::Value(folded)),
                    false => Ok(Step::Value(value)),
                }
            }

            Frame::MatchK {
                result,
                cases,
                demand,
            } => self.dispatch(host, result, cases, value, demand),

            Frame::ProjK { field, demand } => match project_literal(&field, value) {
                Ok(component) => Ok(Step::Eval(component, demand)),
                Err(head) => Ok(Step::Value(Term::from(Subterm::Proj(Proj { head, field })))),
            },

            Frame::LetK {
                mut released,
                current: _,
                bindings,
                next,
                tail,
                demand,
            } => {
                released.push(value);
                self.let_advance(host, released, bindings, next, tail, demand)
            }

            Frame::Memo { key, demand } => {
                // One entry of two `Term` handles, on the buffer row, charged before the write and only where there will be one — what is recorded at all is [`ClosedMemo::records`]'s, which [`ClosedMemo::remember`] holds to whatever a caller charged.
                if its_own_forced_form(&value) {
                    host.spend(Cost::buffer(2))?;
                    host.closed_memo().remember(key, value.clone(), demand);
                }

                Ok(Step::Value(value))
            }

            Frame::Args(mut args) => {
                args.done.push(value);
                self.args(host, args)
            }

            Frame::Forcing { mut fold, operand } => {
                fold.forced.insert(operand, value);
                self.fold(host, fold)
            }
        }
    }

    /// Charge and perform a beta step: open `telescope` over the *values* of `params`, evaluated left to right with the error fallback substitution positions get. A proof, and a witness named at an instance, are bound as written.
    fn beta<H: ClosedHost>(
        &mut self,
        host: &mut H,
        telescope: Telescope<Term>,
        params: Vec<Argument>,
        demand: Demand,
    ) -> Result<Step, ReduceError> {
        host.spend(
            Cost::collection(params.len() as u64)
                .saturating_add(Cost::term(1).saturating_mul(params.len() as u64)),
        )?;

        let count = params.len();
        let written = telescope
            .proofs()
            .into_iter()
            .zip(&params)
            .map(|(proof, argument)| {
                proof
                    || argument.plicity == Plicity::Witness
                        && matches!(&*argument.term, Subterm::Instance(_))
            })
            .collect();
        self.args(
            host,
            Args {
                originals: params.into_iter().map(|argument| argument.term).collect(),
                written,
                done: Vec::with_capacity(count),
                next: 0,
                item_demand: Demand::Whnf,
                catch: true,
                finish: Finish::Beta { telescope, demand },
            },
        )
    }

    /// Advance an [`Args`] run: evaluate the next pending term, or finish over the collected values.
    ///
    /// **A global name in a catching position stays a name.** The hosts substitute a beta argument or an arm payload as written and unfold a name only where it is demanded; evaluating the name here and substituting its *value* is call-by-value on definitions, and on a function-valued global it inlines the definition's whole web once per occurrence — `both(r11, anyof(r11, r11))` came back holding `r11`'s value twice, that value holding `r10`'s twice, a graph whose tree is `2^n` — which the host's unfold memo then retained and the host's own beta later opened as a tree. Everything else in a catching position is still evaluated, which is what keeps a data-shaped walk at constant native depth; a name costs the host one unfold where it is demanded, served by the memo thereafter.
    fn args<H: ClosedHost>(&mut self, host: &mut H, mut args: Args) -> Result<Step, ReduceError> {
        while args.next < args.originals.len() {
            let next = args.originals[args.next].clone();
            let demand = args.item_demand;
            args.next += 1;
            let written = args.written.get(args.next - 1).copied().unwrap_or(false);
            // A declaration at no instance is a name as a variable is; one at levels is evaluated, which is what tells two instances of a family apart in a level.
            let named = match &*next {
                Subterm::Var(_) => true,
                Subterm::Instance(instance) => instance.universes.is_empty(),
                _ => false,
            };
            if args.catch && (written || named) {
                args.done.push(next);
                continue;
            }
            self.push(host, Frame::Args(args))?;
            return Ok(Step::Eval(next, demand));
        }

        match args.finish {
            Finish::Beta { telescope, demand } => {
                let refs = args.done.iter().collect::<Vec<_>>();
                Ok(Step::Eval(telescope.open(&refs), demand))
            }
            Finish::Arm { body, demand } => {
                let refs = args.done.iter().collect::<Vec<_>>();
                Ok(Step::Eval(body.open(&refs), demand))
            }
            Finish::Intrinsic {
                key,
                intrinsic,
                demand,
            } => {
                let values = args
                    .originals
                    .into_iter()
                    .zip(args.done)
                    .collect::<HashMap<_, _>>();
                self.fold(
                    host,
                    Fold {
                        key,
                        intrinsic,
                        demand,
                        values,
                        forced: HashMap::new(),
                    },
                )
            }
        }
    }

    /// Fold `fold`'s intrinsic through [`reduce_intrinsic`], serving it the operands' values through a [`Tap`]; where it stops at an operand to read forced, force that operand's value on this stack and fold again.
    fn fold<H: ClosedHost>(&mut self, host: &mut H, fold: Fold) -> Result<Step, ReduceError> {
        let (answer, wanted) = {
            let mut tap = Tap {
                host,
                values: &fold.values,
                forced: &fold.forced,
                wanted: None,
            };
            let answer = reduce_intrinsic(&mut tap, &fold.intrinsic);
            (answer, tap.wanted)
        };
        if let Some(operand) = wanted {
            let value = fold.values[&operand].clone();
            self.push(host, Frame::Forcing { fold, operand })?;
            return Ok(Step::Eval(value, Demand::Forced));
        }

        let Fold { key, demand, .. } = fold;
        let value = key.rebuilt(answer?);
        // Two entries of two `Term` handles each, on the buffer row, charged before they are made and only where there will be any. The self-entry is what makes a *rebuilt* neutral — a chain link — recognizable as already evaluated when it comes back around as somebody's operand. A fold whose result is a folded recursive spelling records neither, which the operands being produced at the plain demand now makes reachable.
        if its_own_forced_form(&value) {
            host.spend(Cost::buffer(4))?;
            let memo = host.closed_memo();
            memo.remember(value.clone(), value.clone(), Demand::Whnf);
            memo.remember(key, value.clone(), Demand::Whnf);

            return Ok(Step::Value(value));
        }

        match demand {
            Demand::Whnf => Ok(Step::Value(value)),
            // Remembered under the intrinsic at the forced demand, as a match's value is under the match.
            Demand::Forced => {
                self.push(
                    host,
                    Frame::Memo {
                        key,
                        demand: Demand::Forced,
                    },
                )?;
                Ok(Step::Eval(value, Demand::Forced))
            }
        }
    }

    /// Advance a `let` run: release the next binding against the values before it and evaluate it, or open the tail over all of them.
    fn let_advance<H: ClosedHost>(
        &mut self,
        host: &mut H,
        released: Vec<Term>,
        bindings: Vec<LetBinding>,
        next: usize,
        tail: Scope<Many>,
        demand: Demand,
    ) -> Result<Step, ReduceError> {
        let (mut released, mut next) = (released, next);
        while next < bindings.len() {
            let current = {
                let refs = released.iter().collect::<Vec<_>>();
                bindings[next].value().release(&refs)
            };
            // A proof is bound as it is written, as an argument at such a binder is ([`Args`]).
            if tail.binds_proof(next) {
                released.push(current);
                next += 1;
                continue;
            }

            self.push(
                host,
                Frame::LetK {
                    released,
                    current: current.clone(),
                    bindings,
                    next: next + 1,
                    tail,
                    demand,
                },
            )?;
            return Ok(Step::Eval(current, Demand::Whnf));
        }

        let refs = released.iter().collect::<Vec<_>>();
        Ok(Step::Eval(tail.open(&refs), demand))
    }

    /// Iota: dispatch a match on its scrutinee's value, one arm per [`Cases`] family, mirroring the hosts' rules with payload values bound directly.
    fn dispatch<H: ClosedHost>(
        &mut self,
        host: &mut H,
        result: MatchResult,
        cases: Cases,
        scrutinee: Term,
        demand: Demand,
    ) -> Result<Step, ReduceError> {
        match cases {
            Cases::Bool {
                false_case,
                true_case,
            } => match scrutinee.as_bool() {
                Some(false) => Ok(Step::Eval(false_case, demand)),
                Some(true) => Ok(Step::Eval(true_case, demand)),
                None => Ok(Step::Value(Term::from(Subterm::Match(Match {
                    head: scrutinee,
                    result,
                    cases: Cases::Bool {
                        false_case,
                        true_case,
                    },
                })))),
            },

            Cases::Switch { cases, default } => {
                let (value, inner) = Nat::decompose(&scrutinee);

                match Nat::is_zero(&inner) {
                    true => Ok(Step::Eval(
                        cases
                            .iter()
                            .find(|(key, _)| key == &value)
                            .map(|(_, body)| body)
                            .unwrap_or(&default)
                            .clone(),
                        demand,
                    )),
                    false => Ok(Step::Value(Term::from(Subterm::Match(Match {
                        head: scrutinee,
                        result,
                        cases: Cases::Switch { cases, default },
                    })))),
                }
            }

            Cases::Induct {
                cases,
                default,
                stated,
            } => {
                if let Subterm::Variant(ctor) = &*scrutinee {
                    if let Some((_, arm)) = cases
                        .iter()
                        .find(|(candidate, _)| candidate == &ctor.tag)
                        .filter(|(_, arm)| arm.arity() == ctor.payload.len())
                    {
                        let written = host.payload_proofs(&ctor.name, &ctor.tag);
                        return self.args(
                            host,
                            Args {
                                originals: ctor.payload.clone(),
                                written,
                                done: Vec::with_capacity(ctor.payload.len()),
                                next: 0,
                                item_demand: Demand::Whnf,
                                catch: true,
                                finish: Finish::Arm {
                                    body: arm.body().clone(),
                                    demand,
                                },
                            },
                        );
                    }

                    if let Some(default) = default {
                        return Ok(Step::Eval(default, demand));
                    }
                }

                Ok(Step::Value(Term::from(Subterm::Match(Match {
                    head: scrutinee,
                    result,
                    cases: Cases::Induct {
                        cases,
                        default,
                        stated,
                    },
                }))))
            }

            Cases::FreeMonoid { carrier } => {
                let layer = match &carrier {
                    Carrier::Nat { .. } => FreeMonoid::Unary,
                    Carrier::Bytes { .. } => FreeMonoid::Bytes,
                    Carrier::List { .. } => FreeMonoid::List,
                }
                .uncons(Term::unwrap_or_clone(scrutinee));

                match layer {
                    Layer::Empty => Ok(Step::Eval(
                        match carrier {
                            Carrier::Nat { empty_case, .. }
                            | Carrier::Bytes { empty_case, .. }
                            | Carrier::List { empty_case, .. } => empty_case,
                        },
                        demand,
                    )),
                    Layer::Cons { head, tail } => {
                        // The induction hypothesis is bound unreduced, exactly as the hosts bind it: evaluating it eagerly would run the whole tail fold whether or not the arm uses it. An arm that does use it demands it through a frame, at machine depth.
                        let hypothesis: Term = Subterm::Match(Match {
                            head: tail.clone(),
                            result: result.clone(),
                            cases: Cases::FreeMonoid {
                                carrier: carrier.clone(),
                            },
                        })
                        .into();

                        Ok(Step::Eval(
                            match &carrier {
                                Carrier::Nat { cons_case, .. } => {
                                    cons_case.open(&[&tail, &hypothesis])
                                }
                                Carrier::Bytes { cons_case, .. }
                                | Carrier::List { cons_case, .. } => cons_case.open(&[
                                    head.as_ref()
                                        .expect("a Bytes/List cons layer carries a head"),
                                    &tail,
                                    &hypothesis,
                                ]),
                            },
                            demand,
                        ))
                    }
                    Layer::Stuck(stuck) => Ok(Step::Value(Term::from(Subterm::Match(Match {
                        head: stuck.into(),
                        result,
                        cases: Cases::FreeMonoid { carrier },
                    })))),
                }
            }
        }
    }
}

/// Open a `rec` group's tail over its members — a pure binder operation, shared verbatim with both hosts' spelling of the same step.
///
/// For a block whose tail binds one binder per member ([`Rec::opens`]), which every caller here asks before it opens one: the opening is `Scope::open`, whose assert is the only reader of that count. It says nothing of a member scope's own count, which a projection's reduct opens through `RecGroup::member_body` without passing here at all.
fn unfold_rec(rec: Rec) -> Term {
    let members = rec.group.members();
    let refs = members.iter().collect::<Vec<_>>();

    rec.tail.open(&refs)
}

#[cfg(test)]
mod tests;
