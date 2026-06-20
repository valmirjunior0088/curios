//! De Bruijn machinery for the `core` stage's terms.
//!
//! `Scope`, `Telescope`, `Var`, the `Bound` traversal trait, and the `Visit`
//! driver operate over `core`'s `Term` and `Subterm`. `core` keeps its own
//! `Subterm::traverse` (the big structural match, including its primitives) and
//! plugs it into this machinery by implementing `Bound`.

use {
    super::{Subterm, Term},
    std::{collections::BTreeSet, fmt::Debug, hash::Hash, ops::Deref},
};

// === Arity ===================================================================

pub trait Arity: Copy {
    type Params<'a, T: ?Sized + 'a>: AsRef<[&'a T]>;

    fn arity(&self) -> usize;
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct One;

impl One {
    pub const ARITY: usize = 1;
}

impl Arity for One {
    type Params<'a, T: ?Sized + 'a> = &'a [&'a T; Self::ARITY];

    fn arity(&self) -> usize {
        Self::ARITY
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Two;

impl Two {
    pub const ARITY: usize = 2;
}

impl Arity for Two {
    type Params<'a, T: ?Sized + 'a> = &'a [&'a T; Self::ARITY];

    fn arity(&self) -> usize {
        Self::ARITY
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Three;

impl Three {
    pub const ARITY: usize = 3;
}

impl Arity for Three {
    type Params<'a, T: ?Sized + 'a> = &'a [&'a T; Self::ARITY];

    fn arity(&self) -> usize {
        Self::ARITY
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Many(pub usize);

impl Arity for Many {
    type Params<'a, T: ?Sized + 'a> = &'a [&'a T];

    fn arity(&self) -> usize {
        self.0
    }
}

// === Var =====================================================================

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum VarType {
    Free(String),
    Bound(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    type_: VarType,
}

impl Var {
    pub fn free<A>(label: A) -> Self
    where
        A: Into<String>,
    {
        Self {
            type_: VarType::Free(label.into()),
        }
    }

    pub fn as_free(&self) -> Option<&str> {
        match &self.type_ {
            VarType::Free(label) => Some(label),
            VarType::Bound(_) => None,
        }
    }

    pub fn bound(index: usize) -> Self {
        Self {
            type_: VarType::Bound(index),
        }
    }

    pub fn as_bound(&self) -> Option<usize> {
        match &self.type_ {
            VarType::Free(_) => None,
            &VarType::Bound(index) => Some(index),
        }
    }

    pub fn unwrap(&self) -> &str {
        self.as_free().unwrap()
    }
}

// === Bound ===================================================================

pub trait Bound: Sized + Clone + Eq + Hash + Debug {
    fn traverse<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>;

    /// Number of outer de Bruijn binders this term depends on: `1 + max escaping
    /// bound index`, or `0` if none. A term with `reach <= depth` contains no
    /// bound index `>= depth`, so `shift`/`release` at that depth are the
    /// identity on it.
    fn reach(&self) -> usize;

    /// `true` iff the term has no loose de Bruijn indices — i.e. it's not
    /// floating inside some outer scope.
    fn closed(&self) -> bool {
        self.reach() == 0
    }

    fn shift(&self, amount: usize) -> Self {
        self.traverse(&mut Visit::pruning(|depth, var| {
            var.as_bound()
                .filter(|&index| index >= depth)
                .map(|index| Subterm::Var(Var::bound(index + amount)))
        }))
    }

    fn capture(&self, labels: &[&str]) -> Self {
        self.traverse(&mut Visit::new(|depth, var| {
            var.as_free()
                .and_then(|label| {
                    labels
                        .iter()
                        .position(|&candidate| label == candidate)
                        .map(|index| Subterm::Var(Var::bound(depth + index)))
                })
                .or_else(|| {
                    var.as_bound()
                        .filter(|&index| index >= depth)
                        .map(|index| Subterm::Var(Var::bound(index + labels.len())))
                })
        }))
    }

    fn release(&self, terms: &[&Term]) -> Self {
        self.traverse(&mut Visit::pruning(|depth, var| {
            var.as_bound().and_then(|index| {
                index
                    .checked_sub(depth)
                    .map(|delta| match delta < terms.len() {
                        true => terms[delta].deref().shift(depth),
                        false => Subterm::Var(Var::bound(index - terms.len())),
                    })
            })
        }))
    }

    fn free_vars(&self) -> BTreeSet<String> {
        let mut vars = BTreeSet::new();
        self.traverse(&mut Visit::new(|_, var| {
            if let Some(label) = var.as_free() {
                vars.insert(label.to_string());
            }
            None
        }));
        vars
    }
}

impl Bound for () {
    fn traverse<F>(&self, _: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
    }

    fn reach(&self) -> usize {
        0
    }
}

// === Scope ===================================================================

pub struct Scope<A: Arity, B: Bound = Term> {
    arity: A,
    names: Option<Vec<String>>,
    body: Box<B>,
}

impl<A: Arity + Debug, B: Bound> Debug for Scope<A, B> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Scope")
            .field("arity", &self.arity)
            .field("names", &self.names)
            .field("body", &self.body)
            .finish()
    }
}

impl<A: Arity + Clone, B: Bound> Clone for Scope<A, B> {
    fn clone(&self) -> Self {
        Self {
            arity: self.arity,
            names: self.names.clone(),
            body: self.body.clone(),
        }
    }
}

impl<A: Arity + PartialEq, B: Bound> PartialEq for Scope<A, B> {
    fn eq(&self, other: &Self) -> bool {
        self.arity == other.arity && self.names == other.names && self.body == other.body
    }
}

impl<A: Arity + Eq, B: Bound> Eq for Scope<A, B> {}

impl<A: Arity + Hash, B: Bound> Hash for Scope<A, B> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.arity.hash(state);
        self.names.hash(state);
        self.body.hash(state);
    }
}

impl<A: Arity, B: Bound> Scope<A, B> {
    pub fn close<'a>(arity: A, labels: A::Params<'a, str>, body: B) -> Self {
        assert!(
            arity.arity() == labels.as_ref().len(),
            "scope arity mismatch in `close`: expected {}, got {}",
            arity.arity(),
            labels.as_ref().len()
        );

        Self {
            arity,
            names: Some(labels.as_ref().iter().map(|s| s.to_string()).collect()),
            body: body.capture(labels.as_ref()).into(),
        }
    }

    pub fn arity(&self) -> usize {
        self.arity.arity()
    }

    pub fn body(&self) -> &B {
        &self.body
    }

    pub fn names(&self) -> Option<&[String]> {
        self.names.as_deref()
    }

    pub fn reach(&self) -> usize {
        self.body.reach().saturating_sub(self.arity())
    }

    pub fn open<'a>(&self, terms: A::Params<'a, Term>) -> B {
        assert!(
            self.arity() == terms.as_ref().len(),
            "scope arity mismatch in `open`: expected {}, got {}",
            self.arity(),
            terms.as_ref().len()
        );

        self.body.release(terms.as_ref())
    }

    pub fn constant(arity: A, body: B) -> Self {
        Self {
            arity,
            names: None,
            body: body.into(),
        }
    }

    /// Rebuild this scope with `f` applied to its body, preserving arity and
    /// binder names. The body keeps its de Bruijn structure, so `f` must be a
    /// transformation that does not disturb loose indices — e.g. zonking, which
    /// only replaces closed metavariable nodes by closed solutions.
    pub fn map_body<E>(&self, f: impl FnOnce(&B) -> Result<B, E>) -> Result<Self, E> {
        Ok(Self {
            arity: self.arity,
            names: self.names.clone(),
            body: f(&self.body)?.into(),
        })
    }

    pub fn first_label(&self) -> Option<&str> {
        self.names.as_deref()?.first().map(String::as_str)
    }

    pub fn second_label(&self) -> Option<&str> {
        self.names.as_deref()?.get(1).map(String::as_str)
    }

    pub fn third_label(&self) -> Option<&str> {
        self.names.as_deref()?.get(2).map(String::as_str)
    }

    pub fn label_iter(&self) -> impl Iterator<Item = Option<&str>> {
        (0..self.arity()).map(move |i| {
            self.names
                .as_deref()
                .and_then(|ns| ns.get(i))
                .map(String::as_str)
        })
    }

    pub fn free_vars(&self) -> BTreeSet<String> {
        self.body.free_vars()
    }
}

// === Telescope ===============================================================

pub enum Telescope<B: Bound> {
    Done(Box<B>),
    Cons(Term, Scope<One, Telescope<B>>),
}

impl<B: Bound> Debug for Telescope<B> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Telescope::Done(body) => f.debug_tuple("Done").field(body).finish(),
            Telescope::Cons(ty, rest) => f.debug_tuple("Cons").field(ty).field(rest).finish(),
        }
    }
}

impl<B: Bound> Clone for Telescope<B> {
    fn clone(&self) -> Self {
        match self {
            Telescope::Done(body) => Telescope::Done(body.clone()),
            Telescope::Cons(ty, rest) => Telescope::Cons(ty.clone(), rest.clone()),
        }
    }
}

impl<B: Bound> PartialEq for Telescope<B> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Telescope::Done(a), Telescope::Done(b)) => a == b,
            (Telescope::Cons(ta, ra), Telescope::Cons(tb, rb)) => ta == tb && ra == rb,
            _ => false,
        }
    }
}

impl<B: Bound> Eq for Telescope<B> {}

impl<B: Bound> Hash for Telescope<B> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Telescope::Done(body) => {
                state.write_u8(0);
                body.hash(state);
            }
            Telescope::Cons(ty, rest) => {
                state.write_u8(1);
                ty.hash(state);
                rest.hash(state);
            }
        }
    }
}

impl<B: Bound> Bound for Telescope<B> {
    fn traverse<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        match self {
            Telescope::Cons(ty, rest) => {
                Telescope::Cons(visit.visit_subterm(ty), visit.visit_scope(rest))
            }
            Telescope::Done(body) => Telescope::Done(body.traverse(visit).into()),
        }
    }

    fn reach(&self) -> usize {
        match self {
            Telescope::Cons(ty, rest) => ty.reach().max(rest.reach()),
            Telescope::Done(body) => body.reach(),
        }
    }
}

impl<B: Bound> Telescope<B> {
    pub fn done(body: B) -> Self {
        Telescope::Done(body.into())
    }

    pub fn cons<S, T>(label: S, ty: T, rest: Telescope<B>) -> Self
    where
        S: Into<String>,
        T: Into<Term>,
    {
        let label = label.into();
        Telescope::Cons(ty.into(), Scope::close(One, &[label.as_str()], rest))
    }

    pub fn build<I, S, T>(entries: I, body: B) -> Self
    where
        I: IntoIterator<Item = (S, T)>,
        S: Into<String>,
        T: Into<Term>,
    {
        entries
            .into_iter()
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .fold(Telescope::done(body), |rest, (l, t)| {
                Telescope::cons(l, t, rest)
            })
    }

    pub fn len(&self) -> usize {
        let mut n = 0;
        let mut cur = self;
        while let Telescope::Cons(_, rest) = cur {
            n += 1;
            cur = &rest.body;
        }
        n
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Telescope::Done(_))
    }

    pub fn first_label(&self) -> Option<&str> {
        match self {
            Telescope::Cons(_, rest) => rest.first_label(),
            Telescope::Done(_) => None,
        }
    }

    /// The binder name at each position (`""` when unnamed), walking the spine
    /// without opening — names are structural, no substitution needed.
    pub fn labels(&self) -> Vec<&str> {
        let mut out = Vec::new();
        let mut cur = self;
        while let Telescope::Cons(_, rest) = cur {
            out.push(rest.first_label().unwrap_or_default());
            cur = &rest.body;
        }
        out
    }

    /// Replace the stored binder names along the spine. Pure metadata: the de
    /// Bruijn structure is untouched, so this never changes what binds where —
    /// it restores source labels after a rebuild that had to gensym its
    /// binders (tuple-type labels are part of the type's identity and the
    /// target of `.label` resolution, so they must survive elaboration
    /// verbatim).
    pub fn relabel(self, labels: &[&str]) -> Self {
        match self {
            Telescope::Done(body) => Telescope::Done(body),
            Telescope::Cons(ty, rest) => {
                let (label, rest_labels) = labels.split_first().expect("relabel arity");
                let Scope { arity, body, .. } = rest;
                Telescope::Cons(
                    ty,
                    Scope {
                        arity,
                        names: Some(vec![label.to_string()]),
                        body: Box::new((*body).relabel(rest_labels)),
                    },
                )
            }
        }
    }

    pub fn open(&self, args: &[&Term]) -> B {
        assert!(
            self.len() == args.len(),
            "telescope arity mismatch in `open`: expected {}, got {}",
            self.len(),
            args.len()
        );

        let mut cur = self.clone();
        for arg in args {
            cur = match cur {
                Telescope::Cons(_, rest) => rest.open(&[arg]),
                Telescope::Done(_) => unreachable!(),
            };
        }
        match cur {
            Telescope::Done(body) => *body,
            Telescope::Cons(_, _) => unreachable!(),
        }
    }

    /// Open the telescope across `args`, invoking `f(arg, ty)` at each binder
    /// before substituting that arg into the rest, and return the final `Done`
    /// body. The walk is infallible; the error type `E` belongs to the callback.
    pub fn walk<F, E>(self, args: &[Term], mut f: F) -> Result<B, E>
    where
        F: FnMut(&Term, &Term) -> Result<(), E>,
    {
        assert!(
            self.len() == args.len(),
            "telescope arity mismatch in `walk`: expected {}, got {}",
            self.len(),
            args.len()
        );

        let mut tele = self;
        let mut i = 0;
        loop {
            match tele {
                Telescope::Done(body) => return Ok(*body),
                Telescope::Cons(ty, rest) => {
                    f(&args[i], &ty)?;
                    tele = rest.open(&[&args[i]]);
                    i += 1;
                }
            }
        }
    }

    /// Like [`Telescope::walk`], but each entry is opened with the term `f`
    /// *returns* for that slot rather than the given argument — the rebuilt
    /// rather than the lowered spelling, say — so later entry types and the
    /// body carry the mapped forms. Returns the mapped arguments alongside
    /// the body.
    pub fn walk_map<F, E>(self, args: &[Term], mut f: F) -> Result<(Vec<Term>, B), E>
    where
        F: FnMut(&Term, &Term) -> Result<Term, E>,
    {
        assert!(
            self.len() == args.len(),
            "telescope arity mismatch in `walk_map`: expected {}, got {}",
            self.len(),
            args.len()
        );

        let mut mapped = Vec::with_capacity(args.len());
        let mut tele = self;
        let mut i = 0;
        loop {
            match tele {
                Telescope::Done(body) => return Ok((mapped, *body)),
                Telescope::Cons(ty, rest) => {
                    let term = f(&args[i], &ty)?;
                    tele = rest.open(&[&term]);
                    mapped.push(term);
                    i += 1;
                }
            }
        }
    }

    pub fn nth<F>(self, index: usize, mut sub: F) -> Option<Term>
    where
        F: FnMut(usize) -> Term,
    {
        fn go<B: Bound, F: FnMut(usize) -> Term>(
            tele: Telescope<B>,
            index: usize,
            j: usize,
            sub: &mut F,
        ) -> Option<Term> {
            match tele {
                Telescope::Done(_) => None,
                Telescope::Cons(ty, rest) => {
                    if j == index {
                        Some(ty)
                    } else {
                        go(rest.open(&[&sub(j)]), index, j + 1, sub)
                    }
                }
            }
        }

        go(self, index, 0, &mut sub)
    }
}

// === Visit ===================================================================

/// A term-level pre-hook for [`Visit`]: `Some(replacement)` substitutes the
/// whole node at the current depth.
pub type Rewrite = Box<dyn FnMut(usize, &Term) -> Option<Term>>;

pub struct Visit<F> {
    depth: usize,
    prune: bool,
    visit: F,
    // An optional *term-level* pre-hook, consulted at every recursion point
    // before descending: `Some(replacement)` substitutes the whole node (and
    // is not descended into). Incompatible with pruning, which may skip the
    // very nodes the hook would match.
    rewrite: Option<Rewrite>,
}

impl<F> Visit<F>
where
    F: FnMut(usize, &Var) -> Option<Subterm>,
{
    pub fn new(visit: F) -> Self {
        Self {
            depth: 0,
            prune: false,
            visit,
            rewrite: None,
        }
    }

    /// Like `new`, but lets a `Term::traverse` impl skip (and structurally
    /// share) subtrees the visit provably leaves unchanged. Only sound for
    /// index-monotonic visits whose effect depends solely on bound indices
    /// `>= depth` — i.e. `shift` and `release`. Must NOT be used for `capture`
    /// (rewrites free names) or `free_vars` (must observe every node).
    pub fn pruning(visit: F) -> Self {
        Self {
            depth: 0,
            prune: true,
            visit,
            rewrite: None,
        }
    }

    /// Like `new`, additionally carrying a term-level rewrite hook fired at
    /// every recursion point. Note the root term reaches `traverse` directly,
    /// not through `visit_subterm` — callers must check it themselves.
    pub fn rewriting(visit: F, rewrite: Rewrite) -> Self {
        Self {
            depth: 0,
            prune: false,
            visit,
            rewrite: Some(rewrite),
        }
    }

    pub fn depth(&self) -> usize {
        self.depth
    }

    pub fn prune(&self) -> bool {
        self.prune
    }

    /// Invoke the underlying visit callback on a variable at the current depth.
    pub fn call(&mut self, var: &Var) -> Option<Subterm> {
        (self.visit)(self.depth, var)
    }

    pub fn visit_subterm(&mut self, term: &Term) -> Term {
        if let Some(rewrite) = &mut self.rewrite
            && let Some(replacement) = rewrite(self.depth, term)
        {
            return replacement;
        }

        term.traverse(self)
    }

    pub fn visit_scope<A: Arity, B: Bound>(&mut self, scope: &Scope<A, B>) -> Scope<A, B> {
        self.depth += scope.arity.arity();
        let body = scope.body.traverse(self).into();
        self.depth -= scope.arity.arity();

        Scope {
            arity: scope.arity,
            names: scope.names.clone(),
            body,
        }
    }
}
