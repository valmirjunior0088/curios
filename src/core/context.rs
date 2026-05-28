use {
    super::{Bound, Term},
    std::{
        collections::HashMap,
        time::{Duration, Instant},
    },
};

#[derive(Debug)]
pub struct Context {
    entropy: usize,
    deadline: Instant,
    reductions: HashMap<Term, Term>,
    assumptions: Vec<HashMap<String, Term>>,
    definitions: Vec<HashMap<String, Term>>,
    projections: Vec<HashMap<(Term, usize), Term>>,
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
    }

    fn leave_frame(&mut self) {
        self.assumptions.pop().unwrap();
        let definitions = self.definitions.pop().unwrap();
        let projections = self.projections.pop().unwrap();

        if !definitions.is_empty() || !projections.is_empty() {
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
        self.assumptions
            .last_mut()
            .unwrap()
            .insert(label.into(), type_.clone());
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
}
