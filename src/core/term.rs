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
    pub fn close<'a>(arity: A, labels: A::Params<'a, str>, body: Term) -> Self {
        assert!(
            arity.arity() == labels.as_ref().len(),
            "scope arity mismatch in `close`: expected {}, got {}",
            arity.arity(),
            labels.as_ref().len()
        );

        Self {
            arity,
            body: body.capture(labels.as_ref()).into(),
        }
    }

    pub fn arity(&self) -> usize {
        self.arity.arity()
    }

    pub fn open<'a>(self, terms: A::Params<'a, Term>) -> Term {
        assert!(
            self.arity() == terms.as_ref().len(),
            "scope arity mismatch in `open`: expected {}, got {}",
            self.arity(),
            terms.as_ref().len()
        );

        self.body.release(terms.as_ref())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    Type,
    FuncType {
        input: Subterm,
        output: Scope<One>,
    },
    Func {
        body: Scope<One>,
    },
    Apply {
        head: Subterm,
        param: Subterm,
    },
    PairType {
        input: Subterm,
        output: Scope<One>,
    },
    Pair {
        first: Subterm,
        second: Subterm,
    },
    Split {
        head: Subterm,
        motive: Scope<One>,
        tail: Scope<Two>,
    },
    AtomType {
        atoms: BTreeSet<Atom>,
    },
    Atom {
        atom: Atom,
    },
    Match {
        head: Subterm,
        motive: Scope<One>,
        cases: BTreeMap<Atom, Subterm>,
    },
    Let {
        type_: Subterm,
        body: Subterm,
        tail: Scope<One>,
    },
    LetRec {
        items: Vec<(Scope<Many>, Scope<Many>)>,
        tail: Scope<Many>,
    },
    Name {
        name: Name,
    },
}

impl Term {
    pub fn func_type(label: &str, input: Term, output: Term) -> Self {
        Self::FuncType {
            input: input.into(),
            output: Scope::close(One, &[label], output),
        }
    }

    pub fn func(label: &str, body: Term) -> Self {
        Self::Func {
            body: Scope::close(One, &[label], body),
        }
    }

    pub fn apply<I>(head: Term, params: I) -> Self
    where
        I: IntoIterator<Item = Term>,
    {
        params.into_iter().fold(head, |head, param| Self::Apply {
            head: head.into(),
            param: param.into(),
        })
    }

    pub fn pair_type(label: &str, input: Term, output: Term) -> Self {
        Self::PairType {
            input: input.into(),
            output: Scope::close(One, &[label], output),
        }
    }

    pub fn pair(first: Term, second: Term) -> Self {
        Self::Pair {
            first: first.into(),
            second: second.into(),
        }
    }

    pub fn split(
        head: Term,
        motive_label: &str,
        motive: Term,
        first_label: &str,
        second_label: &str,
        tail: Term,
    ) -> Self {
        Self::Split {
            head: head.into(),
            motive: Scope::close(One, &[motive_label], motive),
            tail: Scope::close(Two, &[first_label, second_label], tail),
        }
    }

    pub fn atom_type<I, A>(atoms: I) -> Self
    where
        I: IntoIterator<Item = A>,
        A: Into<String>,
    {
        Self::AtomType {
            atoms: atoms.into_iter().map(Atom::from).collect(),
        }
    }

    pub fn atom<A>(atom: A) -> Self
    where
        A: Into<String>,
    {
        Self::Atom {
            atom: Atom::from(atom.into()),
        }
    }

    pub fn match_<I>(head: Term, motive_label: &str, motive: Term, cases: I) -> Self
    where
        I: IntoIterator<Item = (Atom, Term)>,
    {
        Self::Match {
            head: head.into(),
            motive: Scope::close(One, &[motive_label], motive),
            cases: cases
                .into_iter()
                .map(|(atom, body)| (atom, body.into()))
                .collect(),
        }
    }

    pub fn let_(label: &str, type_: Term, body: Term, tail: Term) -> Self {
        Self::Let {
            type_: type_.into(),
            body: body.into(),
            tail: Scope::close(One, &[label], tail),
        }
    }

    pub fn let_rec(items: Vec<(&str, Term, Term)>, tail: Term) -> Self {
        let labels = items.iter().map(|&(label, _, _)| label).collect::<Vec<_>>();

        Self::LetRec {
            items: items
                .into_iter()
                .map(|(_, type_, value)| {
                    (
                        Scope::close(Many(labels.len()), &labels, type_),
                        Scope::close(Many(labels.len()), &labels, value),
                    )
                })
                .collect(),
            tail: Scope::close(Many(labels.len()), &labels, tail),
        }
    }

    pub fn label<A>(label: A) -> Self
    where
        A: Into<String>,
    {
        Self::Name {
            name: Name::label(label),
        }
    }

    pub fn collect(&self) -> HashSet<String> {
        let mut names = HashSet::new();

        Visit::new(|_, name| {
            if let Some(label) = name.as_label() {
                names.insert(label.to_string());
            }

            None
        })
        .visit_term(self.clone());

        names
    }

    fn shift(self, amount: usize) -> Self {
        Visit::new(|depth, name| {
            name.as_index()
                .filter(|&index| index >= depth)
                .map(|index| Self::Name {
                    name: Name::index(index + amount),
                })
        })
        .visit_term(self)
    }

    fn capture(self, labels: &[&str]) -> Self {
        Visit::new(|depth, name| {
            name.as_label()
                .and_then(|label| {
                    labels
                        .iter()
                        .position(|&candidate_label| label == candidate_label)
                        .map(|index| Self::Name {
                            name: Name::index(depth + index),
                        })
                })
                .or_else(|| {
                    name.as_index()
                        .filter(|&index| index >= depth)
                        .map(|index| Self::Name {
                            name: Name::index(index + labels.len()),
                        })
                })
        })
        .visit_term(self)
    }

    fn release(self, terms: &[&Term]) -> Self {
        Visit::new(|depth, name| {
            name.as_index().and_then(|index| {
                index
                    .checked_sub(depth)
                    .map(|delta| match delta < terms.len() {
                        true => terms[delta].clone().shift(depth),
                        false => Self::Name {
                            name: Name::index(index - terms.len()),
                        },
                    })
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
            Term::FuncType { input, output } => Term::FuncType {
                input: self.visit_subterm(input),
                output: self.visit_scope(output),
            },
            Term::Func { body } => Term::Func {
                body: self.visit_scope(body),
            },
            Term::Apply { head, param } => Term::Apply {
                head: self.visit_subterm(head),
                param: self.visit_subterm(param),
            },
            Term::PairType { input, output } => Term::PairType {
                input: self.visit_subterm(input),
                output: self.visit_scope(output),
            },
            Term::Pair { first, second } => Term::Pair {
                first: self.visit_subterm(first),
                second: self.visit_subterm(second),
            },
            Term::Split { head, motive, tail } => Term::Split {
                head: self.visit_subterm(head),
                motive: self.visit_scope(motive),
                tail: self.visit_scope(tail),
            },
            Term::AtomType { atoms } => Term::AtomType { atoms },
            Term::Atom { atom } => Term::Atom { atom },
            Term::Match {
                head,
                motive,
                cases,
            } => Term::Match {
                head: self.visit_subterm(head),
                motive: self.visit_scope(motive),
                cases: cases
                    .into_iter()
                    .map(|(atom, body)| (atom, self.visit_subterm(body)))
                    .collect(),
            },
            Term::Let { type_, body, tail } => Term::Let {
                type_: self.visit_subterm(type_),
                body: self.visit_subterm(body),
                tail: self.visit_scope(tail),
            },
            Term::LetRec { items, tail } => Term::LetRec {
                items: items
                    .into_iter()
                    .map(|(type_, value)| (self.visit_scope(type_), self.visit_scope(value)))
                    .collect(),
                tail: self.visit_scope(tail),
            },
            Term::Name { name } => (self.visit)(self.depth, &name).unwrap_or(Term::Name { name }),
        }
    }
}

#[cfg(test)]
mod tests {
    use {super::*, std::collections::HashSet};

    #[test]
    fn close_open_substitutes_label_name() {
        let term = Scope::close(One, &["x"], Term::label("x")).open(&[&Term::label("y")]);

        let name = match term {
            Term::Name { name } => name,
            term => panic!("unexpected `{term:?}`"),
        };

        assert_eq!(name, Name::label("y"));
    }

    #[test]
    fn close_open_preserves_nested_bind() {
        let term =
            Scope::close(One, &["x"], Term::func("y", Term::label("x"))).open(&[&Term::label("z")]);

        let body = match term {
            Term::Func { body } => body,
            term => panic!("unexpected `{term:?}`"),
        };

        let name = match body.open(&[&Term::label("w")]) {
            Term::Name { name } => name,
            term => panic!("unexpected `{term:?}`"),
        };

        assert_eq!(name, Name::label("z"));
    }

    #[test]
    fn collect_ignores_index_names() {
        let term = Term::func(
            "x",
            Term::pair(
                Term::label("x"),
                Term::let_rec(
                    vec![("y", Term::Type, Term::label("z"))],
                    Term::pair(Term::label("y"), Term::label("w")),
                ),
            ),
        );

        assert_eq!(
            term.collect(),
            HashSet::from([String::from("w"), String::from("z")])
        );
    }
}
