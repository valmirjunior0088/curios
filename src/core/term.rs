use {
    super::{Arity, Atom, Many, Name, One, Two},
    std::collections::{BTreeMap, BTreeSet, HashSet},
};

pub type Subterm = Box<Term>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Intrinsic {
    IntType,
    Int(i32),
    IntEql(Subterm, Subterm),
    IntAdd(Subterm, Subterm),
    IntSub(Subterm, Subterm),
    IntMul(Subterm, Subterm),
    FltType,
    Flt(u32),
    FltAdd(Subterm, Subterm),
    FltSub(Subterm, Subterm),
    FltMul(Subterm, Subterm),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scope<A: Arity> {
    arity: A,
    body: Subterm,
}

impl<A> Scope<A>
where
    A: Arity,
{
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

    pub fn open<'a>(&self, terms: A::Params<'a, Term>) -> Term {
        assert!(
            self.arity() == terms.as_ref().len(),
            "scope arity mismatch in `open`: expected {}, got {}",
            self.arity(),
            terms.as_ref().len()
        );

        self.body.release(terms.as_ref())
    }

    pub fn collect(&self) -> HashSet<String> {
        self.body.collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    Type,
    Intrinsic {
        intrinsic: Intrinsic,
    },
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
    pub fn type_() -> Self {
        Self::Type
    }

    pub fn int_type() -> Self {
        Intrinsic::IntType.into()
    }

    pub fn int(value: i32) -> Self {
        Intrinsic::Int(value).into()
    }

    pub fn int_eql<F, S>(first: F, second: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Intrinsic::IntEql(first.into().into(), second.into().into()).into()
    }

    pub fn int_add<F, S>(first: F, second: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Intrinsic::IntAdd(first.into().into(), second.into().into()).into()
    }

    pub fn int_sub<F, S>(first: F, second: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Intrinsic::IntSub(first.into().into(), second.into().into()).into()
    }

    pub fn int_mul<F, S>(first: F, second: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Intrinsic::IntMul(first.into().into(), second.into().into()).into()
    }

    pub fn flt_type() -> Self {
        Intrinsic::FltType.into()
    }

    pub fn flt(value: f32) -> Self {
        Intrinsic::Flt(value.to_bits()).into()
    }

    pub fn flt_add<F, S>(first: F, second: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Intrinsic::FltAdd(first.into().into(), second.into().into()).into()
    }

    pub fn flt_sub<F, S>(first: F, second: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Intrinsic::FltSub(first.into().into(), second.into().into()).into()
    }

    pub fn flt_mul<F, S>(first: F, second: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Intrinsic::FltMul(first.into().into(), second.into().into()).into()
    }

    pub fn func_type<L, I, O>(label: L, input: I, output: O) -> Self
    where
        L: Into<String>,
        I: Into<Term>,
        O: Into<Term>,
    {
        let label = label.into();

        Self::FuncType {
            input: input.into().into(),
            output: Scope::close(One, &[label.as_str()], output.into()),
        }
    }

    pub fn func<L, B>(label: L, body: B) -> Self
    where
        L: Into<String>,
        B: Into<Term>,
    {
        let label = label.into();

        Self::Func {
            body: Scope::close(One, &[label.as_str()], body.into()),
        }
    }

    pub fn apply<H, I, P>(head: H, params: I) -> Self
    where
        H: Into<Term>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
    {
        params
            .into_iter()
            .fold(head.into(), |head, param| Self::Apply {
                head: head.into(),
                param: param.into().into(),
            })
    }

    pub fn pair_type<L, I, O>(label: L, input: I, output: O) -> Self
    where
        L: Into<String>,
        I: Into<Term>,
        O: Into<Term>,
    {
        let label = label.into();

        Self::PairType {
            input: input.into().into(),
            output: Scope::close(One, &[label.as_str()], output.into()),
        }
    }

    pub fn pair<F, S>(first: F, second: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::Pair {
            first: first.into().into(),
            second: second.into().into(),
        }
    }

    pub fn split<H, ML, M, FL, SL, T>(
        head: H,
        motive_label: ML,
        motive: M,
        first_label: FL,
        second_label: SL,
        tail: T,
    ) -> Self
    where
        H: Into<Term>,
        ML: Into<String>,
        M: Into<Term>,
        FL: Into<String>,
        SL: Into<String>,
        T: Into<Term>,
    {
        let motive_label = motive_label.into();
        let first_label = first_label.into();
        let second_label = second_label.into();

        Self::Split {
            head: head.into().into(),
            motive: Scope::close(One, &[motive_label.as_str()], motive.into()),
            tail: Scope::close(
                Two,
                &[first_label.as_str(), second_label.as_str()],
                tail.into(),
            ),
        }
    }

    pub fn atom_type<I, A>(atoms: I) -> Self
    where
        I: IntoIterator<Item = A>,
        A: Into<Atom>,
    {
        Self::AtomType {
            atoms: atoms.into_iter().map(Into::into).collect(),
        }
    }

    pub fn atom<A>(atom: A) -> Self
    where
        A: Into<Atom>,
    {
        Self::Atom { atom: atom.into() }
    }

    pub fn match_<H, L, M, I, A, B>(head: H, motive_label: L, motive: M, cases: I) -> Self
    where
        H: Into<Term>,
        L: Into<String>,
        M: Into<Term>,
        I: IntoIterator<Item = (A, B)>,
        A: Into<Atom>,
        B: Into<Term>,
    {
        let motive_label = motive_label.into();

        Self::Match {
            head: head.into().into(),
            motive: Scope::close(One, &[motive_label.as_str()], motive.into()),
            cases: cases
                .into_iter()
                .map(|(atom, body)| (atom.into(), body.into().into()))
                .collect(),
        }
    }

    pub fn let_<L, T, B, U>(label: L, type_: T, body: B, tail: U) -> Self
    where
        L: Into<String>,
        T: Into<Term>,
        B: Into<Term>,
        U: Into<Term>,
    {
        let label = label.into();

        Self::Let {
            type_: type_.into().into(),
            body: body.into().into(),
            tail: Scope::close(One, &[label.as_str()], tail.into()),
        }
    }

    pub fn let_rec<I, L, T, U, V>(items: I, tail: V) -> Self
    where
        I: IntoIterator<Item = (L, T, U)>,
        L: Into<String>,
        T: Into<Term>,
        U: Into<Term>,
        V: Into<Term>,
    {
        let items = items
            .into_iter()
            .map(|(label, type_, value)| (label.into(), type_.into(), value.into()))
            .collect::<Vec<_>>();

        let labels = items
            .iter()
            .map(|(label, _, _)| label.clone())
            .collect::<Vec<_>>();

        let labels = labels
            .iter()
            .map(|label| label.as_str())
            .collect::<Vec<_>>();

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
            tail: Scope::close(Many(labels.len()), &labels, tail.into()),
        }
    }

    pub fn label<A>(label: A) -> Self
    where
        A: Into<String>,
    {
        Name::label(label).into()
    }

    fn index(index: usize) -> Self {
        Name::index(index).into()
    }

    pub fn collect(&self) -> HashSet<String> {
        let mut names = HashSet::new();

        Visit::new(|_, name| {
            if let Some(label) = name.as_label() {
                names.insert(label.to_string());
            }

            None
        })
        .visit_term(self);

        names
    }

    fn shift(&self, amount: usize) -> Self {
        Visit::new(|depth, name| {
            name.as_index()
                .filter(|&index| index >= depth)
                .map(|index| Self::index(index + amount))
        })
        .visit_term(self)
    }

    fn capture(&self, labels: &[&str]) -> Self {
        Visit::new(|depth, name| {
            name.as_label()
                .and_then(|label| {
                    labels
                        .iter()
                        .position(|&candidate_label| label == candidate_label)
                        .map(|index| Self::index(depth + index))
                })
                .or_else(|| {
                    name.as_index()
                        .filter(|&index| index >= depth)
                        .map(|index| Self::index(index + labels.len()))
                })
        })
        .visit_term(self)
    }

    fn release(&self, terms: &[&Term]) -> Self {
        Visit::new(|depth, name| {
            name.as_index().and_then(|index| {
                index
                    .checked_sub(depth)
                    .map(|delta| match delta < terms.len() {
                        true => terms[delta].shift(depth),
                        false => Self::index(index - terms.len()),
                    })
            })
        })
        .visit_term(self)
    }
}

impl From<&Term> for Term {
    fn from(term: &Term) -> Self {
        term.clone()
    }
}

impl From<Intrinsic> for Term {
    fn from(intrinsic: Intrinsic) -> Self {
        Self::Intrinsic { intrinsic }
    }
}

impl From<Name> for Term {
    fn from(name: Name) -> Self {
        Self::Name { name }
    }
}

impl From<&Name> for Term {
    fn from(name: &Name) -> Self {
        name.into()
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

    fn visit_subterm(&mut self, subterm: &Subterm) -> Subterm {
        self.visit_term(subterm).into()
    }

    fn visit_intrinsic(&mut self, intrinsic: &Intrinsic) -> Intrinsic {
        match intrinsic {
            Intrinsic::IntType => Intrinsic::IntType,
            Intrinsic::Int(value) => Intrinsic::Int(*value),
            Intrinsic::IntEql(first, second) => {
                Intrinsic::IntEql(self.visit_subterm(first), self.visit_subterm(second))
            }
            Intrinsic::IntAdd(first, second) => {
                Intrinsic::IntAdd(self.visit_subterm(first), self.visit_subterm(second))
            }
            Intrinsic::IntSub(first, second) => {
                Intrinsic::IntSub(self.visit_subterm(first), self.visit_subterm(second))
            }
            Intrinsic::IntMul(first, second) => {
                Intrinsic::IntMul(self.visit_subterm(first), self.visit_subterm(second))
            }
            Intrinsic::FltType => Intrinsic::FltType,
            Intrinsic::Flt(bits) => Intrinsic::Flt(*bits),
            Intrinsic::FltAdd(first, second) => {
                Intrinsic::FltAdd(self.visit_subterm(first), self.visit_subterm(second))
            }
            Intrinsic::FltSub(first, second) => {
                Intrinsic::FltSub(self.visit_subterm(first), self.visit_subterm(second))
            }
            Intrinsic::FltMul(first, second) => {
                Intrinsic::FltMul(self.visit_subterm(first), self.visit_subterm(second))
            }
        }
    }

    fn visit_scope<A: Arity>(&mut self, scope: &Scope<A>) -> Scope<A> {
        self.depth += scope.arity.arity();
        let body = self.visit_subterm(&scope.body);
        self.depth -= scope.arity.arity();

        Scope {
            arity: scope.arity,
            body,
        }
    }

    fn visit_term(&mut self, term: &Term) -> Term {
        match term {
            Term::Type => Term::Type,
            Term::Intrinsic { intrinsic } => self.visit_intrinsic(intrinsic).into(),
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
            Term::AtomType { atoms } => Term::AtomType {
                atoms: atoms.clone(),
            },
            Term::Atom { atom } => Term::atom(atom),
            Term::Match {
                head,
                motive,
                cases,
            } => Term::Match {
                head: self.visit_subterm(head),
                motive: self.visit_scope(motive),
                cases: cases
                    .iter()
                    .map(|(atom, body)| (atom.into(), self.visit_subterm(body)))
                    .collect(),
            },
            Term::Let { type_, body, tail } => Term::Let {
                type_: self.visit_subterm(type_),
                body: self.visit_subterm(body),
                tail: self.visit_scope(tail),
            },
            Term::LetRec { items, tail } => Term::LetRec {
                items: items
                    .iter()
                    .map(|(type_, value)| (self.visit_scope(type_), self.visit_scope(value)))
                    .collect(),
                tail: self.visit_scope(tail),
            },
            Term::Name { name } => {
                (self.visit)(self.depth, name).unwrap_or_else(|| name.clone().into())
            }
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
