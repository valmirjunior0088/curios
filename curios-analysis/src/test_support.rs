//! Fixtures the crate's own unit tests share.

use {
    crate::Env,
    curios_core::{Free, Global, InductDecl, StructDecl, Term},
    std::{collections::BTreeMap, convert::Infallible},
};

/// The two facts the local-definition readings take from a checker's scope: which names are locals, and what each local definition is.
///
/// Not a checker and not a mock of one, for the totality tests' `Probe`'s reason: these rules ask no judgment, only these two environment queries, and the kernel — whose locals carry no definitions — cannot pose the question reading through them answers. What the elaborator does with the answer is `curios/src/tests/`'s to show.
#[derive(Default)]
pub(crate) struct Scope {
    assumed: Vec<Free>,
    defined: BTreeMap<Free, Term>,
}

impl Scope {
    pub(crate) fn assume(&mut self, name: &Free) {
        self.assumed.push(*name);
    }

    pub(crate) fn define(&mut self, name: &Free, definition: Term) {
        self.defined.insert(*name, definition);
    }
}

impl Env for Scope {
    type Error = Infallible;

    fn force(&mut self, term: &Term) -> Result<Term, Self::Error> {
        Ok(term.clone())
    }

    fn assumption(&self, _: &Free) -> Option<&Term> {
        None
    }

    fn fresh(&mut self, hint: Option<&str>) -> Free {
        Free::local(9_000, hint)
    }

    fn is_local(&self, name: &Free) -> bool {
        self.assumed.contains(name) || self.defined.contains_key(name)
    }

    fn unfold(&self, name: &Free) -> Option<&Term> {
        self.defined.get(name)
    }

    fn induct_decl(&self, _: &Global) -> Option<&InductDecl> {
        None
    }

    fn struct_decl(&self, _: &Global) -> Option<&StructDecl> {
        None
    }
}
