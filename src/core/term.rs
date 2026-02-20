use super::{Arity, Atom, Many, Name, One, Two};

pub type Subterm = Box<Term>;

#[derive(Clone, Debug)]
pub struct Scope<A: Arity> {
    arity: A,
    body: Subterm,
}

impl<A: Arity> Scope<A> {
    pub fn open<'a>(self, terms: A::Params<'a, Term>) -> Term {
        let terms = terms.as_ref();

        if self.arity.arity() != terms.len() {
            panic!("mismatched arity");
        }

        self.body.release(terms)
    }
}

#[derive(Clone, Debug)]
pub enum Term {
    Type,
    FuncType(Subterm, Scope<One>),
    Func(Scope<One>),
    Apply(Subterm, Subterm),
    PairType(Subterm, Scope<One>),
    Pair(Subterm, Subterm),
    Split(Subterm, Scope<Two>),
    AtomType(Vec<Atom>),
    Atom(Atom),
    Match(Subterm, Vec<(Atom, Subterm)>),
    LetRec(Vec<(Name, Subterm, Scope<One>)>, Scope<Many>),
    Name(Name),
}

impl Term {
    pub fn close<'a, A: Arity>(self, arity: A, names: A::Params<'a, str>) -> Scope<A> {
        let names = names.as_ref();

        if arity.arity() != names.len() {
            panic!("mismatched arity");
        }

        Scope {
            arity,
            body: Box::new(self.capture(names)),
        }
    }

    pub fn reduce(self) -> Self {
        match self {
            Self::Apply(head, param) => match head.reduce() {
                Self::Func(body) => body.open([&param]).reduce(),
                head => Self::Apply(head.into(), param),
            },
            Self::Split(head, scope) => match head.reduce() {
                Self::Pair(left, right) => scope.open([&left, &right]).reduce(),
                head => Self::Split(head.into(), scope),
            },
            Self::Match(head, mut cases) => match head.reduce() {
                Self::Atom(atom) => {
                    if let Some(index) = cases.iter().position(|(case, _)| &atom == case) {
                        cases.remove(index).1.reduce()
                    } else {
                        Self::Match(Box::new(Self::Atom(atom)), cases)
                    }
                }
                head => Self::Match(Box::new(head), cases),
            },
            term => term,
        }
    }

    fn shift(self, amount: usize) -> Self {
        Visitor::new(|depth, name| {
            name.as_bound()
                .filter(|&index| index >= depth)
                .map(|index| Self::Name(Name::bound(index + amount)))
        })
        .visit_term(self)
    }

    fn capture(self, names: &[&str]) -> Self {
        Visitor::new(|depth, name| {
            name.as_free()
                .and_then(|free| {
                    names
                        .iter()
                        .position(|&bound| free == bound)
                        .map(|index| Self::Name(Name::bound(depth + index)))
                })
                .or_else(|| {
                    name.as_bound()
                        .filter(|&index| index >= depth)
                        .map(|index| Self::Name(Name::bound(index + names.len())))
                })
        })
        .visit_term(self)
    }

    fn release(self, terms: &[&Term]) -> Self {
        Visitor::new(|depth, name| {
            name.as_bound()
                .and_then(|index| match index.checked_sub(depth) {
                    Some(delta) if delta < terms.len() => Some(terms[delta].clone().shift(depth)),
                    Some(_) => Some(Self::Name(Name::bound(index - terms.len()))),
                    None => None,
                })
        })
        .visit_term(self)
    }
}

impl<A> From<A> for Term
where
    A: Into<String>,
{
    fn from(free: A) -> Self {
        Self::Name(Name::from(free))
    }
}

struct Visitor<F> {
    depth: usize,
    f: F,
}

impl<F> Visitor<F>
where
    F: FnMut(usize, &Name) -> Option<Term>,
{
    fn new(f: F) -> Self {
        Self { depth: 0, f }
    }

    fn visit_node(&mut self, node: Subterm) -> Subterm {
        self.visit_term(*node).into()
    }

    fn visit_scope<A: Arity>(&mut self, scope: Scope<A>) -> Scope<A> {
        let Scope { arity, body } = scope;

        self.depth += arity.arity();
        let body = self.visit_node(body).into();
        self.depth -= arity.arity();

        Scope { arity, body }
    }

    fn visit_term(&mut self, term: Term) -> Term {
        match term {
            Term::Type => Term::Type,
            Term::FuncType(term, scope) => {
                Term::FuncType(self.visit_node(term), self.visit_scope(scope))
            }
            Term::Func(term) => Term::Func(self.visit_scope(term)),
            Term::Apply(head, param) => Term::Apply(self.visit_node(head), self.visit_node(param)),
            Term::PairType(term, scope) => {
                Term::PairType(self.visit_node(term), self.visit_scope(scope))
            }
            Term::Pair(left, right) => Term::Pair(self.visit_node(left), self.visit_node(right)),
            Term::Split(head, scope) => Term::Split(self.visit_node(head), self.visit_scope(scope)),
            Term::AtomType(atoms) => Term::AtomType(atoms),
            Term::Atom(atom) => Term::Atom(atom),
            Term::Match(head, cases) => Term::Match(
                self.visit_node(head),
                cases
                    .into_iter()
                    .map(|(atom, body)| (atom, self.visit_node(body)))
                    .collect(),
            ),
            Term::LetRec(head, body) => Term::LetRec(
                head.into_iter()
                    .map(|(name, kind, value)| {
                        (name, self.visit_node(kind), self.visit_scope(value))
                    })
                    .collect(),
                self.visit_scope(body),
            ),
            Term::Name(name) => (self.f)(self.depth, &name).unwrap_or(Term::Name(name)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn close_open_substitutes_free_name() {
        let term = Term::from("x").close(One, ["x"]).open([&Term::from("y")]);

        let name = match term {
            Term::Name(name) => name,
            term => panic!("unexpected `{term:?}`"),
        };

        assert_eq!(name, Name::from("y"));
    }

    #[test]
    fn close_open_preserves_nested_bind() {
        let term = Term::Func(Term::from("x").close(One, ["y"]))
            .close(One, ["x"])
            .open([&Term::from("z")]);

        let body = match term {
            Term::Func(body) => body,
            term => panic!("unexpected `{term:?}`"),
        };

        let name = match body.open([&Term::from("w")]) {
            Term::Name(name) => name,
            term => panic!("unexpected `{term:?}`"),
        };

        assert_eq!(name, Name::from("z"));
    }
}
