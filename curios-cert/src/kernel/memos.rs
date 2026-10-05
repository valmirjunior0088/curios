//! The evaluation memos: what a term weak-head reduces to, the sort a type inhabits, the type inferred for a term, the type a term was checked at and whether two terms converted — each for the rest of the declaration where it is a function of the declaration alone, and otherwise for as long as what it read of the scope stands.
//!
//! This is not the kind of store the [`kernel`](super) module documentation warns about. A metavariable heap or a refinement layer *injects* answers a term alone could not produce; a memo replays the kernel's own pure function of `(term, definitions)`, computed once. Refusing even that multiplies the cost of a whole-prelude re-check, spent re-deriving the same prelude spines for every recursive group the totality gate walks; `curios-prelude-archive`'s `kernel_memo_parity` runs that walk both ways.
//!
//! Five invariants make it an evaluation strategy rather than a second source of truth, and all five live here.
//!
//! **Keys are valid by construction.** [`Memos::invalidate`] is called whenever a name is *overwritten*, so validity never rests on an append-only assumption. A whnf entry for a *local-free* term — every free variable a definition name — has a scope-independent key: whnf reads no local (locals carry no values by design) and no case equation, so the reduct is a function of the definition store alone and no key can dangle a retracted binder. A whnf entry for a *local-bearing* term is a function of one thing more, the case equations in force, and its key cannot say which were; so those entries live in tables of their own that [`Memos::begin_equations`] clears wherever that set changes — an equation assumed, a bracket retracting one, a settlement withholding and restoring them — and within one such span the reduct is as fixed as a closed term's is within a declaration. The binders a key names are minted once and never recur, so a stale entry is unreachable rather than wrong. Which table a term belongs to is decided here, on both the store and the lookup, rather than at the call sites that would have to remember it.
//!
//! **A judgment that reads the scope lives as long as what it read.** A sort rests on more of the scope than a reduct does: on the equations in force, which decide what a type reduces to, and on the type each local stands at, since a neutral's sort is read off its binder. So the scoped sorts are cleared where an equation moves, as the local-bearing reducts are, and where an arm re-assumes a local at its specialized type; and each is filed with the binders in scope when it was read — a [`Prefix`] — and answers only while they stand, because a stale sort is *wrong* where a stale reduct is merely unreachable: a name handed in from outside can be assumed again, at another type, once its first binder is closed. A local-free type's sort is the declaration's inside an arm as outside one, for the reason its reduct is: classifying it reduces local-free terms and terms naming only the binders the classification itself opens, and no case equation is about either — an equation's subject names a local of the walk, and reduction never introduces one.
//!
//! An inferred type and a check read the same things and are filed the same way: a term naming a local is typed off the binders in scope, under the equations in force. One rule is stricter than a sort's. A local-free term typed while a case equation is in force is filed with the scope's answers and not the declaration's — the table declined such a term outright before it kept anything read off the scope, and the stricter filing costs one typing per arm. And one refusal is theirs alone: a term naming a member of a group whose body is being checked is neither filed nor answered, for the reason [`Kernel::infer_hit`](super::Kernel::infer_hit) gives. A comparison's verdict is filed as a typing is, by its type and its two sides, and only where it was reached with no goal in progress assumed.
//!
//! **They are consulted at every reduction level, not only at the crate's boundary.** `whnf_within` probes on entry and stores on exit, so a scrutinee, an application's head, each turn of `force`'s loop and every other internal call is served by the same table. The elaborator's reducer does the same; probing only at the two [`Reducer`](curios_core::Reducer) methods would leave every internal call re-deriving what the table already holds. Reaching every level is safe because the admission test is one test: [`Memos::whnf`] gates the lookup exactly as [`Memos::store_whnf`] gates the store. A sort is probed the same way, at every field and domain [`Sort::of`] classifies, which is what makes a type's sort cost its graph.
//!
//! **Every table lives exactly as long as a budget does.** [`Memos::begin_declaration`] clears them wherever `Spend::restore_budget` fires, and that alignment is what lets a hit on them be free. The paragraph above argues an entry's *content* is semantically determined — "the reduct is a function of the definition store alone" — but its *presence* is historical in any table that outlives the budget: a free hit on one would make what one declaration spends depend on which declarations were walked before it, which is the property the per-declaration budget exists to hold. Clearing removes the dependence rather than asking anyone to trust a cache policy, and it measured free on every probe. No table here outlives a declaration: a name-keyed table of definition unfolds that did, charging on a hit what its first computation spent, would count that computation's own free hits, so the declaration that unfolded a name first would decide what every later one is charged for it — and the closed machine reads a body directly, so it needs none. `documentation/design/soundness/a-reduction-step-costs-what-it-builds.md` states the rule for both checkers.
//!
//! **A store cannot charge.** [`Memos`] hands back a [`Replay`] and nothing else; applying one to the entropy counter is [`Spend::charge_nothing`](super::Spend::charge_nothing)'s job. A component that could both remember and charge is one that could charge twice. The tables of sorts, of inferred types and of checks hand back the answer alone, because a hit on any of them has nothing to apply: it spends nothing and mints nothing, for the reason [`Kernel::infer_hit`](super::Kernel::infer_hit) gives.

use {
    super::{Prefix, Replay, Sort},
    curios_analysis::Erased,
    curios_core::{Bound, Term},
    std::{collections::HashMap, hash::Hash},
};

/// How long a remembered answer stands.
#[derive(Clone, Copy)]
enum Life {
    /// For the declaration in progress: the answer is a function of the terms it is about and the declaration alone.
    Declaration,
    /// For as long as the scope it was read off stands: the equations in force, the binders in scope and the type each stands at.
    Scope,
}

/// One judgment's answers, under the two lives an answer about terms with no loose index has.
struct Lives<K, V> {
    /// The answers that are a function of the declaration alone.
    declaration: HashMap<K, V>,
    /// The answers read off the scope, each beside the binders it was read under.
    scope: HashMap<K, (Prefix, V)>,
}

impl<K: Eq + Hash, V: Clone> Lives<K, V> {
    fn new() -> Self {
        Self {
            declaration: HashMap::new(),
            scope: HashMap::new(),
        }
    }

    /// The remembered answer at `key` under `life`, with the binders it was read under where it was read off the scope.
    fn get(&self, key: &K, life: Life) -> Option<(Option<Prefix>, V)> {
        match life {
            Life::Declaration => self.declaration.get(key).map(|value| (None, value.clone())),
            Life::Scope => self
                .scope
                .get(key)
                .map(|(prefix, value)| (Some(*prefix), value.clone())),
        }
    }

    /// Remember the answer at `key`, in the table [`Lives::get`] reads it from under the same `life`, read under the binders `prefix` holds.
    fn insert(&mut self, key: K, life: Life, prefix: Prefix, value: V) {
        match life {
            Life::Declaration => {
                self.declaration.insert(key, value);
            }
            Life::Scope => {
                self.scope.insert(key, (prefix, value));
            }
        }
    }

    fn begin_declaration(&mut self) {
        self.declaration.clear();
        self.scope.clear();
    }

    fn begin_equations(&mut self) {
        self.scope.clear();
    }
}

pub(super) struct Memos {
    /// Whether the memos are consulted at all. On by default; `Kernel::uncached` exists so a test can assert that switching them off changes no *semantic* verdict — the property that makes them an evaluation strategy rather than a store.
    enabled: bool,
    /// Weak-head reducts of local-free terms, per entry point: plain, and rec-forced. Free on a hit, and cleared at every declaration boundary — see the module documentation for why those two go together, and for why `whnf` reaches this at every level rather than only at the crate's edge.
    whnf: HashMap<Term, Replay>,
    forced: HashMap<Term, Replay>,
    /// The same two tables for *local-bearing* terms, whose reducts are a function of the definition store **and the case equations in force**. The key cannot carry the second, so the tables live only as long as that set does: [`Memos::begin_equations`] clears them wherever an equation is assumed, retracted, withheld or restored, and [`Memos::begin_declaration`] with the rest. Within one such span a term's reduct is as fixed as a closed term's is within a declaration, and the web of definitions the index inversion forces at `Eq()(top(n), 0)` — each naming the one before it twice, a local in every one — would be re-derived `2^n` times without exactly this.
    local: HashMap<Term, Replay>,
    local_forced: HashMap<Term, Replay>,
    /// The sort of each type classified: [`Sort::of`]'s own answer. A type is a graph — a record of two fields at one type holds one node twice — and its sort asks each field's, so remembered by type a shared field is classified once where recomputing classifies it once per path. Free on a hit; a sort alone is kept, as a type alone is.
    sorts: Lives<Term, Sort>,
    /// Which erased half a position at each type belongs to — a type, a proof, or neither — which is its sort read once more: what `Kernel::record_checked` asks at every position it records, so that the question costs one answer per type and not one per position.
    halves: Lives<Term, Option<Erased>>,
    /// The type inferred for each term: `infer`'s own answer, remembered as the reducts are. A reduct is a graph whose tree can be exponential in its depth, and so is a body whose `let`s the kernel substituted — every line a term over the function's parameters; typing walks what it meets, and remembered by term a subterm shared across that tree is typed once. Free on a hit. A type alone is kept, not a [`Replay`], because a hit replays nothing — see [`Kernel::infer_hit`](super::Kernel::infer_hit) for why, and for the terms it is refused.
    types: Lives<Term, Term>,
    /// Each term checked, beside the type it was checked at. The rules that introduce a function and a record, and the one that descends a `let`, check a term without inferring it, so a tuple whose two fields are one node is checked against its record once per path unless the check itself is remembered. Only that a term checked is kept: a refusal ends its item's check.
    checked: Lives<(Term, Term), ()>,
    /// The verdict of each comparison decided with no goal in progress assumed, at its type: `convert`'s own answer, both ways. Two terms that are equal graphs built apart are compared along every path that reaches a pair of their nodes, and remembered by pair each is compared once. A verdict that rested on the recurrence rule is true of the goals that were in progress when it was reached, and is not kept — see `convert`'s `History`.
    converted: Lives<(Term, Term, Term), bool>,
}

impl Memos {
    pub(super) fn new(enabled: bool) -> Self {
        Self {
            enabled,
            whnf: HashMap::new(),
            forced: HashMap::new(),
            local: HashMap::new(),
            local_forced: HashMap::new(),
            sorts: Lives::new(),
            halves: Lives::new(),
            types: Lives::new(),
            checked: Lives::new(),
            converted: Lives::new(),
        }
    }

    /// The remembered weak-head reduct of `term` at the given entry point, still to be applied — its identities minted, and its steps deliberately not spent.
    pub(super) fn whnf(&self, term: &Term, forced: bool) -> Option<Replay> {
        if !self.enabled {
            return None;
        }

        // Dispatched by whether the term mentions a local, on the lookup as on the store, so a closed term is never answered from the scoped tables nor a local-bearing one from the tables that outlive a scope.
        match (term.has_local_free(), forced) {
            (false, false) => self.whnf.get(term).cloned(),
            (false, true) => self.forced.get(term).cloned(),
            (true, false) => self.local.get(term).cloned(),
            (true, true) => self.local_forced.get(term).cloned(),
        }
    }

    /// Remember `term`'s weak-head reduct at the given entry point, and its consumption.
    pub(super) fn store_whnf(&mut self, term: Term, forced: bool, replay: Replay) {
        if !self.enabled {
            return;
        }

        match (term.has_local_free(), forced) {
            (false, false) => self.whnf.insert(term, replay),
            (false, true) => self.forced.insert(term, replay),
            (true, false) => self.local.insert(term, replay),
            (true, true) => self.local_forced.insert(term, replay),
        };
    }

    /// The remembered sort of `type_`, with the binders it was read under where it was read off the scope, which the caller asks the scope about before taking the answer.
    pub(super) fn sort(&self, type_: &Term) -> Option<(Option<Prefix>, Sort)> {
        self.sorts.get(type_, self.classified(type_)?)
    }

    /// Remember `type_`'s sort, read under the binders `prefix` holds.
    pub(super) fn store_sort(&mut self, type_: Term, prefix: Prefix, sort: Sort) {
        if let Some(life) = self.classified(&type_) {
            self.sorts.insert(type_, life, prefix, sort);
        }
    }

    /// The remembered erased half of a position at `type_`, as [`Memos::sort`] hands back a sort.
    pub(super) fn half(&self, type_: &Term) -> Option<(Option<Prefix>, Option<Erased>)> {
        self.halves.get(type_, self.classified(type_)?)
    }

    /// Remember the erased half of a position at `type_`, read under the binders `prefix` holds.
    pub(super) fn store_half(&mut self, type_: Term, prefix: Prefix, half: Option<Erased>) {
        if let Some(life) = self.classified(&type_) {
            self.halves.insert(type_, life, prefix, half);
        }
    }

    /// The remembered type of `term`, `in_arm` saying whether a case equation is in force, as [`Memos::sort`] hands back a sort.
    pub(super) fn infer(&self, term: &Term, in_arm: bool) -> Option<(Option<Prefix>, Term)> {
        self.types.get(term, self.judged(&[term], in_arm)?)
    }

    /// Remember `term`'s type, in the table [`Memos::infer`] reads it from under the same `in_arm`.
    pub(super) fn store_infer(&mut self, term: Term, in_arm: bool, prefix: Prefix, type_: Term) {
        if let Some(life) = self.judged(&[&term], in_arm) {
            self.types.insert(term, life, prefix, type_);
        }
    }

    /// Whether `term` is remembered to check at `expected`: `Some`, with the binders that was read under where it was read off the scope.
    pub(super) fn checked(
        &self,
        term: &Term,
        expected: &Term,
        in_arm: bool,
    ) -> Option<(Option<Prefix>, ())> {
        let life = self.judged(&[term, expected], in_arm)?;

        self.checked.get(&(term.clone(), expected.clone()), life)
    }

    /// Remember that `term` checks at `expected`.
    pub(super) fn store_checked(
        &mut self,
        term: &Term,
        expected: &Term,
        in_arm: bool,
        prefix: Prefix,
    ) {
        if let Some(life) = self.judged(&[term, expected], in_arm) {
            self.checked
                .insert((term.clone(), expected.clone()), life, prefix, ());
        }
    }

    /// The remembered verdict of comparing `this` with `that` at `type_`, as [`Memos::infer`] hands back a type.
    pub(super) fn converted(
        &self,
        type_: &Term,
        this: &Term,
        that: &Term,
        in_arm: bool,
    ) -> Option<(Option<Prefix>, bool)> {
        let life = self.judged(&[type_, this, that], in_arm)?;

        self.converted
            .get(&(type_.clone(), this.clone(), that.clone()), life)
    }

    /// Remember the verdict of the comparison `goal` — its type, then its two sides.
    pub(super) fn store_converted(
        &mut self,
        goal: (Term, Term, Term),
        in_arm: bool,
        prefix: Prefix,
        verdict: bool,
    ) {
        if let Some(life) = self.judged(&[&goal.0, &goal.1, &goal.2], in_arm) {
            self.converted.insert(goal, life, prefix, verdict);
        }
    }

    /// The life of what [`Sort::of`] reads off `type_`: the declaration's where the type is local-free, inside an arm as outside one, and the scope's otherwise. `None` where nothing is remembered — the memos off, or a loose index a binder outside the type gives meaning to.
    fn classified(&self, type_: &Term) -> Option<Life> {
        self.judged(&[type_], false)
    }

    /// The life of a typing judgment over `terms`, `in_arm` saying whether a case equation is in force: the declaration's where every term is local-free and the judgment is made outside every arm, and the scope's otherwise. `None` as for [`Memos::classified`].
    fn judged(&self, terms: &[&Term], in_arm: bool) -> Option<Life> {
        if !self.enabled || terms.iter().any(|term| term.reach() != 0) {
            return None;
        }

        Some(
            match !in_arm && terms.iter().all(|term| !term.has_local_free()) {
                true => Life::Declaration,
                false => Life::Scope,
            },
        )
    }

    /// Discard every remembered reduct, sort, type, check and verdict. Called wherever the budget is restored, which is what makes a hit on them free rather than order-dependent.
    pub(super) fn begin_declaration(&mut self) {
        self.whnf.clear();
        self.forced.clear();
        self.sorts.begin_declaration();
        self.halves.begin_declaration();
        self.types.begin_declaration();
        self.checked.begin_declaration();
        self.converted.begin_declaration();
        self.begin_equations();
    }

    /// Discard what was read off the scope: the local-bearing reducts, and the scoped sorts, types, checks and verdicts. Called wherever the set of case equations in force changes, which is the one thing besides the definition store a local-bearing reduct is a function of, and wherever a local is re-typed, which the others are a function of besides.
    pub(super) fn begin_equations(&mut self) {
        self.local.clear();
        self.local_forced.clear();
        self.sorts.begin_equations();
        self.halves.begin_equations();
        self.types.begin_equations();
        self.checked.begin_equations();
        self.converted.begin_equations();
    }

    /// Discard the typings read off the scope, and nothing else. Called wherever what the enclosing arms established for the call recorder changes: a group typed inside a term closes under it, so a type or a check remembered on one side of that change does not answer on the other. A reduct and a sort read none of it. Called too where an arm binds a `let`-bound local again: a typing of a term naming it read what it stood for, and a reduct, a sort and a verdict are keyed by value and name none.
    pub(super) fn begin_sizes(&mut self) {
        self.types.begin_equations();
        self.checked.begin_equations();
    }

    /// Discard every remembered reduct. Called when a definition is overwritten, which is the one event that can invalidate one.
    pub(super) fn invalidate(&mut self) {
        self.begin_declaration();
    }
}
