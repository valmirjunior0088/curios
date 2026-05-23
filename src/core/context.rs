use {
    super::{Term, Type},
    std::{collections::HashMap, time::Duration},
};

#[derive(Debug)]
pub struct Context {
    entropy: usize,
    timeout: Duration,
    assumptions: Vec<HashMap<String, Term>>,
    definitions: Vec<HashMap<String, Term>>,
    witnesses: Vec<HashMap<String, Term>>,
    projections: Vec<HashMap<(Term, usize), Term>>,
}

impl Context {
    pub fn new(timeout: Duration) -> Self {
        Self {
            entropy: 0,
            timeout,
            assumptions: vec![HashMap::new()],
            definitions: vec![HashMap::new()],
            witnesses: vec![HashMap::new()],
            projections: vec![HashMap::new()],
        }
    }

    pub fn fresh(&mut self) -> String {
        let entropy = self.entropy.to_string();
        self.entropy += 1;

        entropy
    }

    pub fn timeout(&self) -> Duration {
        self.timeout
    }

    fn enter_frame(&mut self) {
        self.assumptions.push(HashMap::new());
        self.definitions.push(HashMap::new());
        self.witnesses.push(HashMap::new());
        self.projections.push(HashMap::new());
    }

    fn leave_frame(&mut self) {
        self.assumptions.pop().unwrap();
        self.definitions.pop().unwrap();
        self.witnesses.pop().unwrap();
        self.projections.pop().unwrap();
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

    pub fn seal<A>(&mut self, label: A, witness: &Term)
    where
        A: Into<String>,
    {
        let label = label.into();
        self.assume(label.as_str(), &Type.into());
        self.witnesses
            .last_mut()
            .unwrap()
            .insert(label, witness.clone());
    }

    pub fn witness(&self, label: &str) -> Option<&Term> {
        self.witnesses.iter().rev().find_map(|s| s.get(label))
    }

    pub fn define_proj(&mut self, base: Term, index: usize, value: &Term) {
        self.projections
            .last_mut()
            .unwrap()
            .insert((base, index), value.clone());
    }

    pub fn projection(&self, base: &Term, index: usize) -> Option<&Term> {
        self.projections
            .iter()
            .rev()
            .find_map(|p| p.get(&(base.clone(), index)))
    }
}
