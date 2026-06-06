use {
    super::{Bound, Term},
    crate::Span,
    std::{
        collections::HashMap,
        time::{Duration, Instant},
    },
};

/// One metavariable's record in the [`MetaStore`]. Everything here is frozen at
/// birth except `solution`, which transitions `None -> Some(_)` exactly once.
#[derive(Debug)]
pub struct MetaEntry {
    /// Γ frozen at birth: the local assumption context in binding order, with
    /// birth-time types. Drives the scope check and re-validation (§7.3–§7.4).
    pub telescope: Vec<(String, Term)>,
    /// The metavariable's type — the `expected` it was checked against at birth.
    pub result: Term,
    /// `None` while unsolved; `Some(t)` once solved. `t`'s free `Var`s are a
    /// subset of `telescope`'s names.
    pub solution: Option<Term>,
    /// Birth-site span, for `cannot_infer` / unsolved-hole errors.
    pub span: Option<Span>,
}

/// Flat, frame-independent store of metavariable records, indexed by
/// `Metavar::id`. Its contents are monotonic facts about the program being
/// elaborated, not lexically-scoped bindings — so `enter_frame`/`leave_frame`
/// never touch it.
#[derive(Debug, Default)]
pub struct MetaStore {
    entries: Vec<MetaEntry>,
}

#[derive(Debug)]
pub struct Context {
    entropy: usize,
    deadline: Instant,
    reductions: HashMap<Term, Term>,
    assumptions: Vec<HashMap<String, Term>>,
    definitions: Vec<HashMap<String, Term>>,
    projections: Vec<HashMap<(Term, usize), Term>>,
    // Counterfactual match-arm refinements (`refine_head`), kept parallel to
    // `definitions`/`projections` but suppressible: re-validation of a
    // metavariable solution (§7.4) must keep stable definitions yet ignore these.
    refinements: Vec<HashMap<String, Term>>,
    refinement_projections: Vec<HashMap<(Term, usize), Term>>,
    suppress_refinements: bool,
    // The local assumption context in binding order (a companion to
    // `assumptions`, which is keyed by name and loses order). `assume` appends;
    // frames are delimited by `local_marks`.
    local: Vec<(String, Term)>,
    local_marks: Vec<usize>,
    metas: MetaStore,
}

// Safety: `Term` keys contain `OnceCell` fields for caching, which triggers Clippy's
// interior mutability warning. However, the logical value is fully immutable, and the
// hash/equality check remains stable.
#[allow(clippy::mutable_key_type)]
impl Context {
    // The deadline is set once at construction and shared across every
    // `reduce`/`convert`/`infer`/`erase` call that uses this context, so the
    // timeout bounds total work, not per-call work.
    pub fn new(timeout: Duration) -> Self {
        Self {
            entropy: 0,
            deadline: Instant::now() + timeout,
            reductions: HashMap::new(),
            assumptions: vec![HashMap::new()],
            definitions: vec![HashMap::new()],
            projections: vec![HashMap::new()],
            refinements: vec![HashMap::new()],
            refinement_projections: vec![HashMap::new()],
            suppress_refinements: false,
            local: Vec::new(),
            local_marks: Vec::new(),
            metas: MetaStore::default(),
        }
    }

    pub fn fresh(&mut self, hint: Option<&str>) -> String {
        let counter = self.entropy;
        self.entropy += 1;

        match hint {
            Some(h) => format!("{h}#{counter}"),
            None => format!("#{counter}"),
        }
    }

    pub fn deadline(&self) -> Instant {
        self.deadline
    }

    pub fn get_or_init_reduced<E>(
        &mut self,
        term: Term,
        compute: impl FnOnce(&mut Self, Term) -> Result<Term, E>,
    ) -> Result<Term, E> {
        if let Some(cached) = self.reductions.get(&term) {
            return Ok(cached.clone());
        }

        let result = compute(self, term.clone())?;

        if term.closed() {
            self.reductions.insert(term, result.clone());
        }

        Ok(result)
    }

    fn enter_frame(&mut self) {
        self.assumptions.push(HashMap::new());
        self.definitions.push(HashMap::new());
        self.projections.push(HashMap::new());
        self.refinements.push(HashMap::new());
        self.refinement_projections.push(HashMap::new());
        self.local_marks.push(self.local.len());
    }

    fn leave_frame(&mut self) {
        self.assumptions.pop().unwrap();
        let definitions = self.definitions.pop().unwrap();
        let projections = self.projections.pop().unwrap();
        let refinements = self.refinements.pop().unwrap();
        let refinement_projections = self.refinement_projections.pop().unwrap();
        self.local.truncate(self.local_marks.pop().unwrap());

        if !definitions.is_empty()
            || !projections.is_empty()
            || !refinements.is_empty()
            || !refinement_projections.is_empty()
        {
            self.reductions.clear();
        }
    }

    pub fn with_frame<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.enter_frame();
        let result = f(self);
        self.leave_frame();

        result
    }

    pub fn assume<A>(&mut self, label: A, type_: &Term)
    where
        A: Into<String>,
    {
        let label = label.into();
        self.local.push((label.clone(), type_.clone()));
        self.assumptions
            .last_mut()
            .unwrap()
            .insert(label, type_.clone());
    }

    /// The local assumption context in binding order — the Γ a metavariable
    /// freezes at birth (§5). Includes every `assume`d binder currently in
    /// scope, across all open frames.
    pub fn local_context(&self) -> &[(String, Term)] {
        &self.local
    }

    pub fn assumption(&self, label: &str) -> Option<&Term> {
        self.assumptions
            .iter()
            .rev()
            .find_map(|assumptions| assumptions.get(label))
    }

    pub fn define<A>(&mut self, label: A, term: &Term)
    where
        A: Into<String>,
    {
        self.definitions
            .last_mut()
            .unwrap()
            .insert(label.into(), term.clone());

        self.reductions.clear();
    }

    pub fn definition(&self, label: &str) -> Option<&Term> {
        self.definitions
            .iter()
            .rev()
            .find_map(|definitions| definitions.get(label))
    }

    pub fn define_assuming<A>(&mut self, label: A, type_: &Term, term: &Term)
    where
        A: Into<String>,
    {
        let label = label.into();
        self.assume(label.as_str(), type_);
        self.define(label, term);
    }

    pub fn define_projection(&mut self, base: Term, index: usize, value: Term) {
        self.projections
            .last_mut()
            .unwrap()
            .insert((base, index), value);

        self.reductions.clear();
    }

    pub fn projection(&self, base: &Term, index: usize) -> Option<&Term> {
        self.projections
            .iter()
            .rev()
            .find_map(|p| p.get(&(base.clone(), index)))
    }

    // === Refinements ========================================================

    /// Register a counterfactual match-arm refinement of a variable. Unlike
    /// `define`, this lives in a suppressible store so re-validation can ignore
    /// it. Clears the reduction cache, as the variable now reduces differently.
    pub fn refine<A>(&mut self, label: A, term: &Term)
    where
        A: Into<String>,
    {
        self.refinements
            .last_mut()
            .unwrap()
            .insert(label.into(), term.clone());

        self.reductions.clear();
    }

    /// Register a counterfactual refinement of a projection (`refine_head` on a
    /// `Proj` scrutinee). The suppressible analogue of `define_projection`.
    pub fn refine_projection(&mut self, base: Term, index: usize, value: Term) {
        self.refinement_projections
            .last_mut()
            .unwrap()
            .insert((base, index), value);

        self.reductions.clear();
    }

    /// The reduct of a variable: its definition, or — unless refinements are
    /// suppressed — its counterfactual refinement. Labels never appear in both
    /// stores (definitions name `let`/`rec` binders; refinements name assumed
    /// scrutinee heads), so the order between them is immaterial.
    pub fn var_reduct(&self, label: &str) -> Option<&Term> {
        if !self.suppress_refinements
            && let Some(term) = self.refinements.iter().rev().find_map(|r| r.get(label))
        {
            return Some(term);
        }

        self.definition(label)
    }

    /// The reduct of a projection: a stable projection definition, or — unless
    /// suppressed — a counterfactual projection refinement.
    pub fn proj_reduct(&self, base: &Term, index: usize) -> Option<&Term> {
        if !self.suppress_refinements
            && let Some(value) = self
                .refinement_projections
                .iter()
                .rev()
                .find_map(|p| p.get(&(base.clone(), index)))
        {
            return Some(value);
        }

        self.projection(base, index)
    }

    /// Run `f` with refinements suppressed (re-validation, §7.4). Brackets the
    /// region with reduction-cache clears so refinement-applied and
    /// refinement-suppressed reducts never contaminate each other's cache.
    pub fn with_suppressed_refinements<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let previous = self.suppress_refinements;
        self.reductions.clear();
        self.suppress_refinements = true;

        let result = f(self);

        self.suppress_refinements = previous;
        self.reductions.clear();

        result
    }

    // === Metavariable store =================================================

    /// Materialize a metavariable's birth record (§5). The store grows to cover
    /// `id`; births happen exactly once per id (each `_` is distinct and occurs
    /// once).
    pub fn birth_metavar(
        &mut self,
        id: usize,
        telescope: Vec<(String, Term)>,
        result: Term,
        span: Option<Span>,
    ) {
        if id >= self.metas.entries.len() {
            self.metas.entries.resize_with(id + 1, || MetaEntry {
                telescope: Vec::new(),
                result: Term::type_(),
                solution: None,
                span: None,
            });
        }

        self.metas.entries[id] = MetaEntry {
            telescope,
            result,
            solution: None,
            span,
        };
    }

    pub fn metavar_entry(&self, id: usize) -> Option<&MetaEntry> {
        self.metas.entries.get(id)
    }

    pub fn metavar_solution(&self, id: usize) -> Option<&Term> {
        self.metas.entries.get(id).and_then(|e| e.solution.as_ref())
    }

    /// Commit a metavariable's solution. Clears the reduction cache, since a
    /// bare metavariable is `reach == 0` (hence cacheable) and may have cached
    /// as itself while unsolved (§7.2).
    pub fn solve_metavar(&mut self, id: usize, term: Term) {
        if let Some(entry) = self.metas.entries.get_mut(id) {
            entry.solution = Some(term);
            self.reductions.clear();
        }
    }
}
