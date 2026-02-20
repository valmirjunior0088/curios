use super::{Atom, Name};

#[derive(Clone, Debug)]
pub struct Bind<const N: usize> {
    body: Box<Term>,
}

impl<const N: usize> Bind<N> {
    pub fn open(self, terms: [&Term; N]) -> Term {
        (*self.body).release(0, terms)
    }

    fn apply<F>(self, depth: usize, f: F) -> Bind<N>
    where
        F: Fn(usize, &Name) -> Option<Term> + Copy,
    {
        Bind {
            body: Box::new((*self.body).apply(depth + N, f)),
        }
    }

    fn shift(self, depth: usize, amount: usize) -> Bind<N> {
        Bind {
            body: Box::new((*self.body).shift(depth + N, amount)),
        }
    }

    fn capture<const M: usize>(self, depth: usize, names: [&str; M]) -> Bind<N> {
        Bind {
            body: Box::new((*self.body).capture(depth + N, names)),
        }
    }

    fn release<const M: usize>(self, depth: usize, terms: [&Term; M]) -> Bind<N> {
        Bind {
            body: Box::new((*self.body).release(depth + N, terms)),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Term {
    Type,

    FuncType(Bind<0>, Bind<1>),
    Func(Bind<1>),
    Apply(Bind<0>, Bind<0>),

    PairType(Bind<0>, Bind<1>),
    Pair(Bind<0>, Bind<0>),
    Split(Bind<0>, Bind<2>),

    AtomType(Vec<Atom>),
    Atom(Atom),
    Match(Bind<0>, Vec<(Atom, Bind<0>)>),

    Bind(Name, Bind<0>, Bind<1>),

    Name(Name),
}

impl Term {
    pub fn close<const N: usize>(self, names: [&str; N]) -> Bind<N> {
        Bind {
            body: Box::new(self.capture(0, names)),
        }
    }

    fn apply<F>(self, depth: usize, f: F) -> Self
    where
        F: Fn(usize, &Name) -> Option<Term> + Copy,
    {
        match self {
            Self::Type => Self::Type,
            Self::FuncType(source, target) => {
                Self::FuncType(source.apply(depth, f), target.apply(depth, f))
            }
            Self::Func(function) => Self::Func(function.apply(depth, f)),
            Self::Apply(function, argument) => {
                Self::Apply(function.apply(depth, f), argument.apply(depth, f))
            }
            Self::PairType(first, second) => {
                Self::PairType(first.apply(depth, f), second.apply(depth, f))
            }
            Self::Pair(first, second) => Self::Pair(first.apply(depth, f), second.apply(depth, f)),
            Self::Split(pair, target) => Self::Split(pair.apply(depth, f), target.apply(depth, f)),
            Self::AtomType(atoms) => Self::AtomType(atoms),
            Self::Atom(atom) => Self::Atom(atom),
            Self::Match(scrutinee, branches) => Self::Match(
                scrutinee.apply(depth, f),
                branches
                    .into_iter()
                    .map(|(atom, body)| (atom, body.apply(depth, f)))
                    .collect(),
            ),
            Self::Bind(name, value, body) => {
                Self::Bind(name, value.apply(depth, f), body.apply(depth, f))
            }
            Self::Name(name) => f(depth, &name).unwrap_or(Self::Name(name)),
        }
    }

    fn shift(self, depth: usize, amount: usize) -> Self {
        self.apply(depth, |depth, name| {
            name.as_bound()
                .filter(|&index| index >= depth)
                .map(|index| Self::Name(Name::bound(index + amount)))
        })
    }

    fn capture<const N: usize>(self, depth: usize, names: [&str; N]) -> Self {
        self.apply(depth, |depth, name| {
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
                        .map(|index| Self::Name(Name::bound(index + N)))
                })
        })
    }

    fn release<const N: usize>(self, depth: usize, terms: [&Term; N]) -> Self {
        self.apply(depth, |depth, name| {
            name.as_bound()
                .and_then(|index| match index.checked_sub(depth) {
                    Some(delta) if delta < N => Some(terms[delta].clone().shift(0, depth)),
                    Some(_) => Some(Self::Name(Name::bound(index - N))),
                    None => None,
                })
        })
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn close_open_substitutes_free_name() {
        let term = Term::from("x").close(["x"]).open([&Term::from("y")]);

        let name = match term {
            Term::Name(name) => name,
            term => panic!("unexpected `{term:?}`"),
        };

        assert_eq!(name, Name::from("y"));
    }

    #[test]
    fn close_open_preserves_nested_bind() {
        let term = Term::Func(Term::from("x").close(["y"]))
            .close(["x"])
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
