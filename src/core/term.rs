use {
    super::{Arity, Atom, Many, Name, One, Two},
    std::collections::{BTreeMap, BTreeSet, HashSet},
};

pub type Subterm = Box<Term>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scope<A: Arity> {
    arity: A,
    body: Subterm,
}

impl<A: Arity> Scope<A> {
    pub fn close<'a>(arity: A, names: A::Params<'a, str>, body: Term) -> Self {
        let names = names.as_ref();

        assert!(arity.arity() == names.len());

        Self {
            arity,
            body: body.capture(names).into(),
        }
    }

    pub fn arity(&self) -> usize {
        self.arity.arity()
    }

    pub fn open<'a>(self, terms: A::Params<'a, Term>) -> Term {
        let terms = terms.as_ref();

        assert!(self.arity() == terms.len());

        self.body.release(terms)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    Type,
    FuncType(Subterm, Scope<One>),
    Func(Scope<One>),
    Apply(Subterm, Subterm),
    PairType(Subterm, Scope<One>),
    Pair(Subterm, Subterm),
    Split(Subterm, Scope<One>, Scope<Two>),
    AtomType(BTreeSet<Atom>),
    Atom(Atom),
    Switch(Subterm, Scope<One>, BTreeMap<Atom, Subterm>),
    Bind(Vec<(Subterm, Scope<Many>)>, Scope<Many>),
    Name(Name),
}

impl Term {
    pub fn func_type(name: &str, kind: Term, body: Term) -> Self {
        Self::FuncType(kind.into(), Scope::close(One, &[name], body))
    }

    pub fn func(name: &str, body: Term) -> Self {
        Self::Func(Scope::close(One, &[name], body))
    }

    pub fn apply(head: Term, param: Term) -> Self {
        Self::Apply(head.into(), param.into())
    }

    pub fn pair_type(name: &str, kind: Term, body: Term) -> Self {
        Self::PairType(kind.into(), Scope::close(One, &[name], body))
    }

    pub fn pair(left: Term, right: Term) -> Self {
        Self::Pair(left.into(), right.into())
    }

    pub fn split(
        head: Term,
        motive: Scope<One>,
        left_name: &str,
        right_name: &str,
        body: Term,
    ) -> Self {
        Self::Split(
            head.into(),
            motive,
            Scope::close(Two, &[left_name, right_name], body),
        )
    }

    pub fn atom_type<I, A>(atoms: I) -> Self
    where
        I: IntoIterator<Item = A>,
        A: Into<String>,
    {
        Self::AtomType(atoms.into_iter().map(Atom::from).collect())
    }

    pub fn atom<A>(atom: A) -> Self
    where
        A: Into<String>,
    {
        Self::Atom(Atom::from(atom.into()))
    }

    pub fn switch<I>(head: Term, motive: Scope<One>, cases: I) -> Self
    where
        I: IntoIterator<Item = (Atom, Term)>,
    {
        Self::Switch(
            head.into(),
            motive,
            cases
                .into_iter()
                .map(|(atom, body)| (atom, body.into()))
                .collect(),
        )
    }

    pub fn bind(bindings: Vec<(&str, Term, Term)>, body: Term) -> Self {
        let names = bindings
            .iter()
            .map(|(name, _, _)| *name)
            .collect::<Vec<_>>();

        let arity = Many(names.len());

        Self::Bind(
            bindings
                .into_iter()
                .map(|(_, kind, value)| (kind.into(), Scope::close(arity, &names, value)))
                .collect(),
            Scope::close(arity, &names, body),
        )
    }

    pub fn free<A>(free: A) -> Self
    where
        A: Into<String>,
    {
        Self::Name(Name::free(free))
    }

    pub fn free_names(&self) -> HashSet<String> {
        let mut names = HashSet::new();

        Visit::new(|_, name| {
            if let Some(free) = name.as_free() {
                names.insert(free.to_string());
            }

            None
        })
        .visit_term(self.clone());

        names
    }

    fn shift(self, amount: usize) -> Self {
        Visit::new(|depth, name| {
            name.as_bound()
                .filter(|&bound| bound >= depth)
                .map(|bound| Self::Name(Name::bound(bound + amount)))
        })
        .visit_term(self)
    }

    fn capture(self, names: &[&str]) -> Self {
        Visit::new(|depth, name| {
            name.as_free()
                .and_then(|free| {
                    names
                        .iter()
                        .position(|&bound| free == bound)
                        .map(|bound| Self::Name(Name::bound(depth + bound)))
                })
                .or_else(|| {
                    name.as_bound()
                        .filter(|&bound| bound >= depth)
                        .map(|bound| Self::Name(Name::bound(bound + names.len())))
                })
        })
        .visit_term(self)
    }

    fn release(self, terms: &[&Term]) -> Self {
        Visit::new(|depth, name| {
            name.as_bound()
                .and_then(|bound| match bound.checked_sub(depth) {
                    Some(delta) if delta < terms.len() => Some(terms[delta].clone().shift(depth)),
                    Some(_) => Some(Self::Name(Name::bound(bound - terms.len()))),
                    None => None,
                })
        })
        .visit_term(self)
    }
}

struct Visit<F> {
    depth: usize,
    visit: F,
}

impl<F> Visit<F>
where
    F: FnMut(usize, &Name) -> Option<Term>,
{
    fn new(visit: F) -> Self {
        Self { depth: 0, visit }
    }

    fn visit_subterm(&mut self, subterm: Subterm) -> Subterm {
        self.visit_term(*subterm).into()
    }

    fn visit_scope<A: Arity>(&mut self, scope: Scope<A>) -> Scope<A> {
        let Scope { arity, body } = scope;

        self.depth += arity.arity();
        let body = self.visit_subterm(body);
        self.depth -= arity.arity();

        Scope { arity, body }
    }

    fn visit_term(&mut self, term: Term) -> Term {
        match term {
            Term::Type => Term::Type,
            Term::FuncType(term, scope) => {
                Term::FuncType(self.visit_subterm(term), self.visit_scope(scope))
            }
            Term::Func(term) => Term::Func(self.visit_scope(term)),
            Term::Apply(head, param) => {
                Term::Apply(self.visit_subterm(head), self.visit_subterm(param))
            }
            Term::PairType(term, scope) => {
                Term::PairType(self.visit_subterm(term), self.visit_scope(scope))
            }
            Term::Pair(left, right) => {
                Term::Pair(self.visit_subterm(left), self.visit_subterm(right))
            }
            Term::Split(head, motive, scope) => Term::Split(
                self.visit_subterm(head),
                self.visit_scope(motive),
                self.visit_scope(scope),
            ),
            Term::AtomType(atoms) => Term::AtomType(atoms),
            Term::Atom(atom) => Term::Atom(atom),
            Term::Switch(head, motive, cases) => Term::Switch(
                self.visit_subterm(head),
                self.visit_scope(motive),
                cases
                    .into_iter()
                    .map(|(atom, body)| (atom, self.visit_subterm(body)))
                    .collect(),
            ),
            Term::Bind(head, body) => Term::Bind(
                head.into_iter()
                    .map(|(kind, value)| (self.visit_subterm(kind), self.visit_scope(value)))
                    .collect(),
                self.visit_scope(body),
            ),
            Term::Name(name) => (self.visit)(self.depth, &name).unwrap_or(Term::Name(name)),
        }
    }
}

#[cfg(test)]
mod tests {
    use {super::*, std::collections::HashSet};

    #[test]
    fn close_open_substitutes_free_name() {
        let term = Scope::close(One, &["x"], Term::free("x")).open(&[&Term::free("y")]);

        let name = match term {
            Term::Name(name) => name,
            term => panic!("unexpected `{term:?}`"),
        };

        assert_eq!(name, Name::free("y"));
    }

    #[test]
    fn close_open_preserves_nested_bind() {
        let term =
            Scope::close(One, &["x"], Term::func("y", Term::free("x"))).open(&[&Term::free("z")]);

        let body = match term {
            Term::Func(body) => body,
            term => panic!("unexpected `{term:?}`"),
        };

        let name = match body.open(&[&Term::free("w")]) {
            Term::Name(name) => name,
            term => panic!("unexpected `{term:?}`"),
        };

        assert_eq!(name, Name::free("z"));
    }

    #[test]
    fn free_names_collects_only_free_names() {
        let term = Term::func(
            "x",
            Term::pair(
                Term::free("x"),
                Term::bind(
                    vec![("y", Term::Type, Term::free("z"))],
                    Term::pair(Term::free("y"), Term::free("w")),
                ),
            ),
        );

        assert_eq!(
            term.free_names(),
            HashSet::from([String::from("w"), String::from("z")])
        );
    }
}
