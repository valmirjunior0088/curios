use {
    super::{Name, Prim},
    std::{
        collections::{BTreeMap, BTreeSet},
        ops::Deref,
    },
};

#[derive(Debug)]
pub struct Term {
    inner: Box<Subterm>,
}

impl Term {
    pub fn into_subterm(self) -> Subterm {
        *self.inner
    }

    pub fn as_subterm(&self) -> &Subterm {
        &self.inner
    }

    pub fn as_subterm_mut(&mut self) -> &mut Subterm {
        &mut self.inner
    }

    /// Free names of this term, treating a nested `Func` as contributing its
    /// precomputed `captures` (the closure's free variables) rather than
    /// descending into its body. Used to build the `rec` dependency graph in
    /// `to_cont` and to compute a closure's captures during erasure — reading the
    /// *erased* body's free names is what keeps an erased-only reference from
    /// being threaded as a runtime capture.
    pub fn free_names(&self) -> BTreeSet<String> {
        let mut names = BTreeSet::new();

        match &**self {
            Subterm::Erased | Subterm::Unreachable | Subterm::Atom(_) => {}
            Subterm::Name(name) => {
                names.insert(name.as_str().to_owned());
            }
            Subterm::Func(func) => names.extend(func.captures.iter().map(|c| c.name.clone())),
            Subterm::Apply(apply) => {
                names.extend(apply.head.free_names());
                apply
                    .params
                    .iter()
                    .for_each(|param| names.extend(param.free_names()));
            }
            Subterm::Tuple(tuple) => tuple
                .fields
                .iter()
                .for_each(|field| names.extend(field.free_names())),
            Subterm::Proj(proj) => names.extend(proj.head.free_names()),
            Subterm::Match(m) => {
                names.extend(m.head.free_names());
                m.cases
                    .iter()
                    .for_each(|case| names.extend(case.free_names()));
            }
            Subterm::NatMatch(NatMatch::Induction {
                head,
                zero_case,
                pred,
                ih,
                succ_case,
            }) => {
                names.extend(head.free_names());
                names.extend(zero_case.free_names());

                let mut succ = succ_case.free_names();
                succ.remove(pred);
                succ.remove(ih);
                names.extend(succ);
            }
            Subterm::NatMatch(NatMatch::Dispatch {
                head,
                cases,
                default,
            }) => {
                names.extend(head.free_names());
                cases
                    .iter()
                    .for_each(|(_, case)| names.extend(case.free_names()));
                names.extend(default.free_names());
            }
            Subterm::Let(let_) => {
                names.extend(let_.body.free_names());

                let mut tail = let_.tail.free_names();
                tail.remove(&let_.name);
                names.extend(tail);
            }
            Subterm::Rec(rec) => {
                let mut inner = BTreeSet::new();
                rec.items
                    .iter()
                    .for_each(|item| inner.extend(item.free_names()));
                inner.extend(rec.tail.free_names());
                rec.names.iter().for_each(|name| {
                    inner.remove(name);
                });
                names.extend(inner);
            }
            Subterm::Prim(prim) => names.extend(prim.free_names()),
        }

        names
    }

    /// Whether this term contains an effectful primitive anywhere — including
    /// under a lambda, since a closure that performs an effect performs it when
    /// *called*, and the caller's evaluation is what this classifies. The
    /// impurity boundary is exactly [`Prim::is_effectful`] (`Host`/`Cell`);
    /// everything in `PurePrim` is pure by construction.
    ///
    /// This is the intra-term half of the purity question shared between
    /// `optm::prune` (which seeds its effect taint with it) and
    /// `optm::worker_wrapper` (whose `MonoidAccumulator` gate rejects an
    /// absorbed context that is not pure). The transitive half — following an
    /// `Apply` to another module item — lives in `optm::call_graph`.
    pub fn contains_effect(&self) -> bool {
        match self.as_subterm() {
            Subterm::Prim(prim) => {
                prim.is_effectful() || prim.operands().iter().any(|t| t.contains_effect())
            }
            Subterm::Func(func) => func.body.contains_effect(),
            Subterm::Apply(apply) => {
                apply.head.contains_effect() || apply.params.iter().any(Term::contains_effect)
            }
            Subterm::Tuple(tuple) => tuple.fields.iter().any(Term::contains_effect),
            Subterm::Proj(proj) => proj.head.contains_effect(),
            Subterm::Match(m) => {
                m.head.contains_effect() || m.cases.iter().any(Term::contains_effect)
            }
            Subterm::NatMatch(NatMatch::Induction {
                head,
                zero_case,
                succ_case,
                ..
            }) => {
                head.contains_effect() || zero_case.contains_effect() || succ_case.contains_effect()
            }
            Subterm::NatMatch(NatMatch::Dispatch {
                head,
                cases,
                default,
            }) => {
                head.contains_effect()
                    || cases.iter().any(|(_, case)| case.contains_effect())
                    || default.contains_effect()
            }
            Subterm::Let(let_) => let_.body.contains_effect() || let_.tail.contains_effect(),
            Subterm::Rec(rec) => {
                rec.items.iter().any(Term::contains_effect) || rec.tail.contains_effect()
            }
            Subterm::Name(_) | Subterm::Atom(_) | Subterm::Erased | Subterm::Unreachable => false,
        }
    }

    /// Whether binding this term performs no action — it is a closure, name, or
    /// atom, allocated without evaluating an effect. `to_cont` lowers such terms
    /// into the flat top-level loop rather than the CPS path, and `optm::prune`
    /// keeps a non-synchronous tainted item for its eager init effect.
    pub fn is_synchronous(&self) -> bool {
        matches!(
            self.as_subterm(),
            Subterm::Func(_)
                | Subterm::Erased
                | Subterm::Unreachable
                | Subterm::Atom(_)
                | Subterm::Name(_)
        )
    }
}

impl Deref for Term {
    type Target = Subterm;

    fn deref(&self) -> &Subterm {
        &self.inner
    }
}

impl From<Subterm> for Term {
    fn from(subterm: Subterm) -> Self {
        Self {
            inner: Box::new(subterm),
        }
    }
}

#[derive(Debug)]
pub enum NatMatch {
    Induction {
        head: Term,
        zero_case: Term,
        pred: String,
        ih: String,
        succ_case: Term,
    },
    Dispatch {
        head: Term,
        cases: BTreeMap<u32, Term>,
        default: Term,
    },
}

/// A captured or parameter binder, plus its specialization-*candidate* flag: true
/// when its (pre-erasure) type was a function, a `Type`, or unit. Computed during
/// type-directed erasure (the last point types are available) and carried to
/// `cont`, glued to the name so the two can never desync. Defaults to
/// non-candidate, so a binder built from a bare name (`"x".into()`) is not one.
#[derive(Debug)]
pub struct Argument {
    pub name: String,
    pub candidate: bool,
}

impl<S: Into<String>> From<S> for Argument {
    fn from(name: S) -> Self {
        Self {
            name: name.into(),
            candidate: false,
        }
    }
}

impl Argument {
    pub fn as_str(&self) -> &str {
        &self.name
    }
}

#[derive(Debug)]
pub struct Func {
    pub captures: Vec<Argument>,
    pub params: Vec<Argument>,
    pub body: Term,
}

#[derive(Debug)]
pub struct Apply {
    pub head: Term,
    pub params: Vec<Term>,
}

#[derive(Debug)]
pub struct Tuple {
    pub fields: Vec<Term>,
}

#[derive(Debug)]
pub struct Proj {
    pub head: Term,
    pub index: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct Atom {
    pub index: usize,
}

#[derive(Debug)]
pub struct Match {
    pub head: Term,
    pub cases: Vec<Term>,
}

#[derive(Debug)]
pub struct Let {
    pub name: String,
    pub body: Term,
    pub tail: Term,
}

#[derive(Debug)]
pub struct Rec {
    pub names: Vec<String>,
    pub items: Vec<Term>,
    pub tail: Term,
}

#[derive(Debug)]
pub enum Subterm {
    Erased,
    Unreachable,
    Prim(Prim),
    NatMatch(NatMatch),
    Func(Func),
    Apply(Apply),
    Tuple(Tuple),
    Proj(Proj),
    Atom(Atom),
    Match(Match),
    Let(Let),
    Rec(Rec),
    Name(Name),
}
