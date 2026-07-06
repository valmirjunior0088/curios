//! De Bruijn machinery for the `core` stage's terms.
//!
//! `Scope`, `Telescope`, `Var`, the `Bound` traversal trait, and the `Visit`
//! driver operate over `core`'s `Term` and `Subterm`. `core` keeps its own
//! `Subterm::traverse` (the big structural match, including its primitives) and
//! plugs it into this machinery by implementing `Bound`.

use {
    super::{MetavarId, Subterm, Term},
    std::{collections::BTreeSet, fmt::Debug, hash::Hash, ops::Deref},
};

// === Arity ===================================================================

/// A [`Scope`]'s binder count, lifted to the type level: the fixed arities ([`One`]/[`Two`]/[`Three`]) make `close`/`open` take exactly-sized arrays, so an arity mismatch on the common eliminator shapes is a compile error; [`Many`] defers the check to a runtime assert.
pub trait Arity: Copy {
    /// The parameter-pack shape `close`/`open` accept: a fixed-size array reference for the static arities, a plain slice for [`Many`].
    type Params<'a, T: ?Sized + 'a>: AsRef<[&'a T]>;

    /// The number of binders this arity denotes.
    fn arity(&self) -> usize;
}

/// The static one-binder arity — `let` tails, telescope links, single-scrutinee motives.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct One;

impl One {
    /// The binder count as a constant, so `Params` can be the fixed-size array type `[&T; 1]`.
    pub const ARITY: usize = 1;
}

impl Arity for One {
    type Params<'a, T: ?Sized + 'a> = &'a [&'a T; Self::ARITY];

    fn arity(&self) -> usize {
        Self::ARITY
    }
}

/// The static two-binder arity — the `(pred, ih)` successor arm of the `Nat` eliminator.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Two;

impl Two {
    /// The binder count as a constant, so `Params` can be the fixed-size array type `[&T; 2]`.
    pub const ARITY: usize = 2;
}

impl Arity for Two {
    type Params<'a, T: ?Sized + 'a> = &'a [&'a T; Self::ARITY];

    fn arity(&self) -> usize {
        Self::ARITY
    }
}

/// The static three-binder arity — the `(head, tail, ih)` cons arms of the `Bin`/`Lst` eliminators.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Three;

impl Three {
    /// The binder count as a constant, so `Params` can be the fixed-size array type `[&T; 3]`.
    pub const ARITY: usize = 3;
}

impl Arity for Three {
    type Params<'a, T: ?Sized + 'a> = &'a [&'a T; Self::ARITY];

    fn arity(&self) -> usize {
        Self::ARITY
    }
}

/// A runtime-chosen binder count, for scopes whose arity is data-dependent (inductive-match arms over constructor payloads, `Rec` blocks, motives). `close`/`open` fall back to slices and assert the length instead of getting it checked at compile time.
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

/// A locally-nameless variable: free (a label naming a Γ assumption or global definition) or bound (a de Bruijn index into enclosing [`Scope`]s). The bound form and its accessors are crate-internal — outside code builds free variables and lets the scope machinery convert them.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    type_: VarType,
}

impl Var {
    /// A free variable named `label` — the only form constructible outside the crate; `Scope::close` (via `capture`) is what turns free occurrences into bound indices.
    pub fn free<A>(label: A) -> Self
    where
        A: Into<String>,
    {
        Self {
            type_: VarType::Free(label.into()),
        }
    }

    pub(crate) fn as_free(&self) -> Option<&str> {
        match &self.type_ {
            VarType::Free(label) => Some(label),
            VarType::Bound(_) => None,
        }
    }

    pub(crate) fn bound(index: usize) -> Self {
        Self {
            type_: VarType::Bound(index),
        }
    }

    pub(crate) fn as_bound(&self) -> Option<usize> {
        match &self.type_ {
            VarType::Free(_) => None,
            &VarType::Bound(index) => Some(index),
        }
    }

    pub(crate) fn unwrap(&self) -> &str {
        self.as_free().unwrap()
    }
}

// === Bound ===================================================================

/// A syntactic category the de Bruijn machinery can operate on: anything that can rebuild itself under a variable-visiting [`Visit`] and report its `reach`. Implemented by `Term`/`Subterm` (the big structural match lives in `term.rs`), [`Telescope`], and `()` (a Σ-telescope's trailing payload); everything else here — `shift`, `capture`, `release`, `free_vars` — is derived from `traverse` alone.
pub trait Bound: Sized + Clone + Eq + Hash + Debug {
    /// Rebuild the term, invoking the visit callback at every variable with the binder depth it sits under; a `Some(replacement)` substitutes that variable. The single primitive the rest of the trait is defined from — implementations must route subterms through `Visit::visit_subterm`/`visit_scope` so depth tracking, pruning, and the rewrite hook fire.
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

    /// De Bruijn weakening: add `amount` to every loose bound index (`>= depth`), making room for that many new enclosing binders when a term is moved under them. Index-monotonic, so the traversal prunes by `reach`.
    fn shift(&self, amount: usize) -> Self {
        self.traverse(&mut Visit::pruning(|depth, var| {
            var.as_bound()
                .filter(|&index| index >= depth)
                .map(|index| Subterm::Var(Var::bound(index + amount)))
        }))
    }

    /// The closing half of the locally-nameless discipline: turn free occurrences of `labels` into bound indices (position in `labels`, offset by the current depth) while shifting already-loose indices past the new binders. `Scope::close` is this plus the name bookkeeping. Rewrites *free* names, so it can never be pruned by `reach`.
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

    /// The opening half of the locally-nameless discipline: substitute the outermost `terms.len()` loose bound indices with `terms` (each shifted by the depth it lands under) and re-tighten the loose indices beyond them. `Scope::open` is this plus the arity check; effects depend only on indices `>= depth`, so the traversal prunes by `reach`.
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

    /// The set of free-variable labels occurring anywhere in the term. A pure observation ridden on `traverse` (the callback rewrites nothing), so it must never be pruned — every node has to be seen.
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

/// A body abstracted over `A::arity()` binders, locally nameless: the body stores de Bruijn indices, while `names` remembers the source labels for reopening and printing (`None` for a `constant` scope that never had binders written). Built by `close` (which captures free occurrences of the labels) and eliminated by `open` (which substitutes terms for the indices); entering a `Scope` is the only place a [`Visit`]'s depth changes, so this type is the unit of binding for the whole crate.
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
    pub(crate) fn close<'a>(arity: A, labels: A::Params<'a, str>, body: B) -> Self {
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

    pub(crate) fn arity(&self) -> usize {
        self.arity.arity()
    }

    pub(crate) fn body(&self) -> &B {
        &self.body
    }

    pub(crate) fn names(&self) -> Option<&[String]> {
        self.names.as_deref()
    }

    pub(crate) fn reach(&self) -> usize {
        self.body.reach().saturating_sub(self.arity())
    }

    pub(crate) fn open<'a>(&self, terms: A::Params<'a, Term>) -> B {
        assert!(
            self.arity() == terms.as_ref().len(),
            "scope arity mismatch in `open`: expected {}, got {}",
            self.arity(),
            terms.as_ref().len()
        );

        self.body.release(terms.as_ref())
    }

    pub(crate) fn constant(arity: A, body: B) -> Self {
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
    pub(crate) fn map_body<E>(&self, f: impl FnOnce(&B) -> Result<B, E>) -> Result<Self, E> {
        Ok(Self {
            arity: self.arity,
            names: self.names.clone(),
            body: f(&self.body)?.into(),
        })
    }

    pub(crate) fn first_label(&self) -> Option<&str> {
        self.names.as_deref()?.first().map(String::as_str)
    }

    pub(crate) fn second_label(&self) -> Option<&str> {
        self.names.as_deref()?.get(1).map(String::as_str)
    }

    pub(crate) fn third_label(&self) -> Option<&str> {
        self.names.as_deref()?.get(2).map(String::as_str)
    }

    pub(crate) fn label_iter(&self) -> impl Iterator<Item = Option<&str>> {
        (0..self.arity()).map(move |i| {
            self.names
                .as_deref()
                .and_then(|ns| ns.get(i))
                .map(String::as_str)
        })
    }

    /// Whether the binder at position `index` (0 = first/outermost label) is
    /// referenced anywhere in the body. A bound var refers to this binder iff its
    /// de Bruijn index equals `index` plus the number of binders entered since —
    /// which `Visit` tracks as `depth`. Used by erasure to spot an eliminator
    /// whose induction hypothesis is dead: that arm is a case-split, not a fold.
    pub(crate) fn uses(&self, index: usize) -> bool {
        let mut used = false;
        self.body.traverse(&mut Visit::new(|depth, var: &Var| {
            if var.as_bound() == Some(index + depth) {
                used = true;
            }
            None
        }));
        used
    }
}

// === Telescope ===============================================================

/// A dependent context: a chain of entry types where each `Cons` tail is a one-binder [`Scope`], so every later entry — and the final `Done` payload — may mention the binders before it. Function types, function literals, and tuple types all reuse it and differ only in the payload: a `Term` (the return type or body) for Π/λ, `()` for Σ, where the fields themselves are the point.
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
    pub(crate) fn done(body: B) -> Self {
        Telescope::Done(body.into())
    }

    pub(crate) fn cons<S, T>(label: S, ty: T, rest: Telescope<B>) -> Self
    where
        S: Into<String>,
        T: Into<Term>,
    {
        let label = label.into();
        Telescope::Cons(ty.into(), Scope::close(One, &[label.as_str()], rest))
    }

    /// Build a telescope from `(label, type)` entries in written order, right-folding so each entry's scope closes over everything after it — written order mirrors telescope order.
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

    pub(crate) fn len(&self) -> usize {
        let mut n = 0;
        let mut cur = self;
        while let Telescope::Cons(_, rest) = cur {
            n += 1;
            cur = &rest.body;
        }
        n
    }

    pub(crate) fn is_empty(&self) -> bool {
        matches!(self, Telescope::Done(_))
    }

    /// The binder name at each position (`""` when unnamed), walking the spine
    /// without opening — names are structural, no substitution needed.
    pub(crate) fn labels(&self) -> Vec<&str> {
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
    pub(crate) fn relabel(self, labels: &[&str]) -> Self {
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

    pub(crate) fn open(&self, args: &[&Term]) -> B {
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

    /// Open the leading binders at successive `params` — one binder per param —
    /// returning the residual telescope. Every caller's telescope leads with the
    /// type parameters (constructor payloads, struct fields, inductive indices all
    /// follow them), so a telescope that runs out early is an invariant violation.
    pub(crate) fn open_params(self, params: &[Term]) -> Telescope<B> {
        let mut telescope = self;
        for param in params {
            telescope = match telescope {
                Telescope::Cons(_, rest) => rest.open(&[param]),
                Telescope::Done(_) => unreachable!("telescope must lead with its parameters"),
            };
        }
        telescope
    }

    /// Open the telescope across `args`, invoking `f(arg, ty)` at each binder
    /// before substituting that arg into the rest, and return the final `Done`
    /// body. The walk is infallible; the error type `E` belongs to the callback.
    pub(crate) fn walk<F, E>(self, args: &[Term], mut f: F) -> Result<B, E>
    where
        F: FnMut(usize, &Term, &Term) -> Result<(), E>,
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
                    f(i, &args[i], &ty)?;
                    tele = rest.open(&[&args[i]]);
                    i += 1;
                }
            }
        }
    }

    pub(crate) fn nth<F>(self, index: usize, mut sub: F) -> Option<Term>
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

impl Telescope<Term> {
    /// Whether any metavariable in a function/Π telescope (`Func`/`FuncType`) —
    /// the parameter types and the trailing body/return type — satisfies
    /// `pred`, short-circuiting on the first hit.
    pub(crate) fn any_metavar<F: FnMut(MetavarId) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Telescope::Cons(ty, rest) => ty.any_metavar(pred) || rest.body().any_metavar(pred),
            Telescope::Done(body) => body.any_metavar(pred),
        }
    }
}

impl Telescope<()> {
    /// Whether any metavariable in a Σ telescope (`TupleType`) — only the field
    /// types; its `Done` body is `()` — satisfies `pred`, short-circuiting on
    /// the first hit.
    pub(crate) fn any_metavar<F: FnMut(MetavarId) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Telescope::Cons(ty, rest) => ty.any_metavar(pred) || rest.body().any_metavar(pred),
            // The trailing body is `()`, which holds no metavariables.
            Telescope::Done(_) => false,
        }
    }
}

// === Visit ===================================================================

/// A term-level pre-hook for [`Visit`]: `Some(replacement)` substitutes the
/// whole node at the current depth.
pub(crate) type Rewrite = Box<dyn FnMut(usize, &Term) -> Option<Term>>;

/// The traversal driver threaded through [`Bound::traverse`]: it owns the current binder depth (bumped and restored by `visit_scope` as scopes are crossed), the variable callback, the pruning flag (skip subtrees whose `reach` proves the visit cannot touch them), and an optional term-level rewrite hook. Public constructors are `Visit::pruning` and `Visit::rewriting`; the plain constructor is crate-internal.
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
    pub(crate) fn new(visit: F) -> Self {
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
    pub(crate) fn pruning(visit: F) -> Self {
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
    pub(crate) fn rewriting(visit: F, rewrite: Rewrite) -> Self {
        Self {
            depth: 0,
            prune: false,
            visit,
            rewrite: Some(rewrite),
        }
    }

    pub(crate) fn depth(&self) -> usize {
        self.depth
    }

    pub(crate) fn prune(&self) -> bool {
        self.prune
    }

    /// Invoke the underlying visit callback on a variable at the current depth.
    pub(crate) fn call(&mut self, var: &Var) -> Option<Subterm> {
        (self.visit)(self.depth, var)
    }

    pub(crate) fn visit_subterm(&mut self, term: &Term) -> Term {
        if let Some(rewrite) = &mut self.rewrite
            && let Some(replacement) = rewrite(self.depth, term)
        {
            return replacement;
        }

        term.traverse(self)
    }

    pub(crate) fn visit_scope<A: Arity, B: Bound>(&mut self, scope: &Scope<A, B>) -> Scope<A, B> {
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
