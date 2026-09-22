//! [`Telescope`], the dependent context function types, function literals and tuple types share, and the [`Cursor`] every walk that opens one entry by entry reads it through.

use super::*;

/// A dependent context: a chain of entry types where each `Cons` tail is a one-binder [`Scope`], so every later entry — and the final `Done` payload — may mention the binders before it. Function types, function literals, and tuple types all reuse it and differ only in the payload: a `Term` (the return type or body) for Π/λ, `()` for Σ, where the fields themselves are the point.
#[curios_archive::archived(recursive)]
pub enum Telescope<B: Bound> {
    Done(Box<B>),
    Cons(Term, #[archived_omit_bounds] Scope<One, Telescope<B>>),
}

impl<B: Bound> Telescope<B> {
    pub fn done(body: B) -> Self {
        Telescope::Done(body.into())
    }

    /// Build a telescope from `(binder, type)` entries in written order — written order mirrors telescope order — closing each entry once over the binders before it and the payload over all of them.
    ///
    /// This is the telescope a right fold reaches by closing each entry's scope over everything after it, reached in one walk per entry. The fold closes everything after an entry at every entry, so it walks the tail once per binder: quadratic in a telescope's length, and cubic in the elaborator's, whose entry types carry metavariable spines as long as the binders before them.
    pub fn build<I, T>(entries: I, body: B) -> Self
    where
        I: IntoIterator<Item = (Free, T)>,
        T: Into<Term>,
    {
        let entries = entries
            .into_iter()
            .map(|(binder, ty)| (binder, ty.into()))
            .collect::<Vec<(Free, Term)>>();
        // Beneath `j` one-binder scopes the nearest binder is index 0, so an entry closes over the binders before it innermost first: the last `j` of this list.
        let innermost_first = entries
            .iter()
            .rev()
            .map(|(binder, _)| binder)
            .collect::<Vec<_>>();
        let before = |count: usize| &innermost_first[entries.len() - count..];

        let mut telescope = Telescope::done(body.capture(before(entries.len())));

        for (index, (binder, ty)) in entries.iter().enumerate().rev() {
            let ty = match index {
                0 => ty.clone(),
                _ => ty.capture(before(index)),
            };

            telescope = Telescope::Cons(
                ty,
                Scope {
                    arity: One,
                    names: Some(vec![binder.clone()]),
                    body: Box::new(telescope),
                },
            );
        }

        telescope
    }

    /// A [`Cursor`] at this telescope's first entry: how every walk that opens a telescope entry by entry reads it.
    pub fn cursor(&self) -> Cursor<'_, B> {
        Cursor {
            at: self,
            args: Vec::new(),
        }
    }

    /// Each entry's type, opened at every argument before it, handed to `visit`, and then the payload opened at all of them.
    ///
    /// `visit` is handed the entry's position, its binder's hint and its type, and returns the argument that binder opens at: what a caller elaborated against the type — a dependent tuple, a telescope of arguments whose later types name earlier ones — or a fresh variable it minted from the hint to walk under the binder.
    pub fn walk_producing<F, E>(&self, mut visit: F) -> Result<(Vec<Term>, B), E>
    where
        F: FnMut(usize, Option<&str>, Term) -> Result<Term, E>,
    {
        let mut cursor = self.cursor();

        while let Some((hint, ty)) = cursor.entry() {
            let arg = visit(cursor.args().len(), hint, ty)?;
            cursor.advance(arg);
        }

        let body = cursor.body().expect("a cursor past every entry");
        Ok((cursor.into_args(), body))
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

    /// The final payload beneath every binder, without opening the telescope or substituting for any of its bound variables.
    pub fn terminal(&self) -> &B {
        let mut current = self;
        loop {
            match current {
                Telescope::Done(body) => return body,
                Telescope::Cons(_, rest) => current = &rest.body,
            }
        }
    }

    /// The binder hint at each position (`""` when unnamed), walking the spine without opening — names are structural, no substitution needed.
    pub fn labels(&self) -> Vec<&str> {
        let mut out = Vec::new();
        let mut cur = self;
        while let Telescope::Cons(_, rest) = cur {
            out.push(rest.first_hint().unwrap_or_default());
            cur = &rest.body;
        }
        out
    }

    /// Replace the display hints along the spine, leaving each binder's identity alone. Pure metadata: the de Bruijn structure is untouched and no occurrence changes what it refers to — this restores source labels after a rebuild that had to re-mint its binders (tuple-type labels are part of the type's identity and the target of `.label` resolution, so they must survive elaboration verbatim).
    pub fn relabel(self, labels: &[&str]) -> Self {
        let mut entries = Vec::new();
        let mut labels = labels.iter();
        let mut current = self;
        let body = loop {
            match current {
                Telescope::Done(body) => break body,
                Telescope::Cons(ty, rest) => {
                    let label = labels.next().expect("relabel arity");
                    let names = rest
                        .names
                        .as_ref()
                        .map(|names| names.iter().map(|name| name.relabelled(label)).collect());
                    entries.push((ty, names));
                    current = *rest.body;
                }
            }
        };

        entries
            .into_iter()
            .rev()
            .fold(Telescope::Done(body), |rest, (ty, names)| {
                Telescope::Cons(
                    ty,
                    Scope {
                        arity: One,
                        names,
                        body: Box::new(rest),
                    },
                )
            })
    }

    pub fn open(&self, args: &[&Term]) -> B {
        assert!(
            self.len() == args.len(),
            "telescope arity mismatch in `open`: expected {}, got {}",
            self.len(),
            args.len()
        );

        let mut cursor = self.cursor();
        for &arg in args {
            cursor.advance(arg.clone());
        }
        cursor.body().expect("a cursor past every entry")
    }

    /// Open the leading binders at successive `params` — one binder per param — returning the residual telescope. Every caller's telescope leads with the type parameters (constructor payloads, struct fields, inductive indices all follow them), so a telescope that runs out early is an invariant violation.
    pub fn open_params(self, params: &[Term]) -> Telescope<B> {
        let mut cursor = self.cursor();
        for param in params {
            assert!(
                cursor.entry().is_some(),
                "telescope must lead with its parameters"
            );
            cursor.advance(param.clone());
        }
        cursor.rest()
    }

    /// Open the telescope across `args`, invoking `f(arg, ty)` at each binder with its type opened at the args before it, and return the final `Done` body. The walk is infallible; the error type `E` belongs to the callback.
    pub fn walk<F, E>(self, args: &[Term], mut f: F) -> Result<B, E>
    where
        F: FnMut(usize, &Term, &Term) -> Result<(), E>,
    {
        assert!(
            self.len() == args.len(),
            "telescope arity mismatch in `walk`: expected {}, got {}",
            self.len(),
            args.len()
        );

        self.walk_producing(|index, _, ty| {
            f(index, &args[index], &ty)?;
            Ok(args[index].clone())
        })
        .map(|(_, body)| body)
    }

    /// The type at `index`, with each preceding binder opened at `sub` of its position. The general form; a field telescope read from a value wants [`Telescope::field_type_from`] instead.
    pub fn nth<F>(self, index: usize, mut sub: F) -> Option<Term>
    where
        F: FnMut(usize) -> Term,
    {
        let mut cursor = self.cursor();
        for position in 0..index {
            cursor.entry()?;
            cursor.advance(sub(position));
        }
        cursor.entry().map(|(_, ty)| ty)
    }

    /// The type of field `index` as seen from `value`: every preceding field is opened at its own projection off `value`, so a field type that names an earlier field names *that value's* earlier field rather than a loose binder.
    ///
    /// This is the one answer to "what type does `value.index` have", and every site that asks — inference, sorting, conversion, witness resolution, operator dispatch, and the method wrappers `into_core` generates — reaches it through here. Re-deriving it anywhere else is how the two readings drift: the wrappers once restated a field's written type in a scope binding no sibling, which was well-formed only while no concept had a dependent field telescope.
    pub fn field_type_from(self, value: &Term, index: usize) -> Option<Term> {
        self.nth(index, |j| Term::proj(value.clone(), j))
    }
}

/// A position inside a [`Telescope`]: every entry before it opened at the argument its binder was given, and everything from it on left untouched until it is read.
///
/// Opening a telescope a binder at a time rewrites everything after that binder at each step, which is quadratic in its length, and cubic in the elaborator's, whose entry types carry metavariable spines naming every binder before them. A cursor remembers the arguments instead and opens only what is read: an entry's type at every argument so far, in one walk of that entry, and the payload once, at the end. Every walk that opens a telescope entry by entry reads it through one, which is what keeps a telescope's cost its size rather than its size times its binders.
pub struct Cursor<'a, B: Bound> {
    at: &'a Telescope<B>,
    /// The arguments opened so far, outermost first.
    args: Vec<Term>,
}

impl<'a, B: Bound> Cursor<'a, B> {
    /// The arguments opened so far, outermost first.
    pub fn args(&self) -> &[Term] {
        &self.args
    }

    pub fn into_args(self) -> Vec<Term> {
        self.args
    }

    /// Whether every entry has been passed.
    pub fn is_done(&self) -> bool {
        matches!(self.at, Telescope::Done(_))
    }

    /// The binder hint and type of the entry at this position, the type opened at every argument so far; `None` once every entry is passed.
    pub fn entry(&self) -> Option<(Option<&'a str>, Term)> {
        match self.at {
            Telescope::Done(_) => None,
            Telescope::Cons(ty, rest) => {
                Some((rest.first_hint(), ty.release(&self.innermost_first())))
            }
        }
    }

    /// The binder the entry at this position was closed over, for a caller that renders it; `None` at the end, or for a scope built without names.
    pub(crate) fn binder(&self) -> Option<&'a Free> {
        match self.at {
            Telescope::Cons(_, rest) => rest.binder(0),
            Telescope::Done(_) => None,
        }
    }

    /// Whether anything after the entry at this position names its binder. Read off the unopened remainder, which opening earlier binders does not change.
    pub(crate) fn binder_used(&self) -> bool {
        match self.at {
            Telescope::Cons(_, rest) => rest.uses(0),
            Telescope::Done(_) => false,
        }
    }

    /// Step past the entry at this position, opening its binder at `arg`. Stepping past the end is a caller's arity bug.
    pub fn advance(&mut self, arg: Term) {
        match self.at {
            Telescope::Cons(_, rest) => {
                self.at = &rest.body;
                self.args.push(arg);
            }
            Telescope::Done(_) => panic!("a cursor advanced past the end of its telescope"),
        }
    }

    /// The payload opened at every argument, once every entry is passed; `None` before.
    pub fn body(&self) -> Option<B> {
        match self.at {
            Telescope::Done(body) => Some(body.release(&self.innermost_first())),
            Telescope::Cons(..) => None,
        }
    }

    /// What remains from this position, as a telescope opened at every argument so far — for a caller that hands the rest on rather than reading it entry by entry. It costs the remainder's whole size, which is why reading through [`Cursor::entry`] is preferred.
    pub fn rest(&self) -> Telescope<B> {
        self.at.release(&self.innermost_first())
    }

    /// Beneath `j` one-binder scopes the nearest binder is index 0, so a release takes the arguments innermost first.
    fn innermost_first(&self) -> Vec<&Term> {
        self.args.iter().rev().collect()
    }
}
/// A walk that can step past its current binder at a fresh variable: a [`Cursor`], or two cursors in [`Lockstep`]. What a checker's "mint the binder, assume it, step past it" encapsulates, so one helper serves both shapes.
pub trait Advance {
    /// The hint of the binder about to be stepped past, which a fresh binder is named after.
    fn hint(&self) -> Option<&str>;

    /// Step past the current binder, opening it at `arg`.
    fn advance(&mut self, arg: Term);

    /// Step past the current binder at a fresh one `fresh` mints from its hint, and hand that binder back — so a binder is always named after the entry it stands for.
    fn advance_fresh(&mut self, fresh: impl FnOnce(Option<&str>) -> Free) -> Free {
        let binder = fresh(self.hint());
        self.advance(Term::free_var(&binder));
        binder
    }
}

impl<B: Bound> Advance for Cursor<'_, B> {
    fn hint(&self) -> Option<&str> {
        match self.at {
            Telescope::Cons(_, rest) => rest.first_hint(),
            Telescope::Done(_) => None,
        }
    }

    fn advance(&mut self, arg: Term) {
        Cursor::advance(self, arg);
    }
}

/// Two telescopes walked binder for binder at one shared argument per position, so both dependent tails speak of the same variable — how a comparison or a check lines a telescope up against another. The left one names the binders.
pub struct Lockstep<'a, B: Bound> {
    left: Cursor<'a, B>,
    right: Cursor<'a, B>,
}

/// Where a [`Lockstep`] stands: at a pair of entries, past both telescopes with their payloads, or at a length mismatch.
pub enum Step<'a, B> {
    Entries {
        hint: Option<&'a str>,
        left: Term,
        right: Term,
    },
    Bodies(B, B),
    Mismatch,
}

impl<'a, B: Bound> Lockstep<'a, B> {
    pub fn new(left: &'a Telescope<B>, right: &'a Telescope<B>) -> Self {
        Self {
            left: left.cursor(),
            right: right.cursor(),
        }
    }

    /// Both entry types at this position, each opened at every shared argument so far, or both payloads once both telescopes are passed, or `Mismatch` when one ends first.
    pub fn step(&self) -> Step<'a, B> {
        match (self.left.entry(), self.right.entry()) {
            (Some((hint, left)), Some((_, right))) => Step::Entries { hint, left, right },
            (None, None) => Step::Bodies(
                self.left.body().expect("a cursor past every entry"),
                self.right.body().expect("a cursor past every entry"),
            ),
            _ => Step::Mismatch,
        }
    }
}

impl<B: Bound> Advance for Lockstep<'_, B> {
    fn hint(&self) -> Option<&str> {
        Advance::hint(&self.left)
    }

    fn advance(&mut self, arg: Term) {
        self.left.advance(arg.clone());
        self.right.advance(arg);
    }
}

impl Telescope<Term> {
    /// Whether any metavariable in a function/Π telescope (`Func`/`FuncType`) — the parameter types and the trailing body/return type — satisfies `pred`, short-circuiting on the first hit.
    pub fn any_metavar<F: FnMut(MetavarId) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Telescope::Cons(ty, rest) => ty.any_metavar(pred) || rest.body().any_metavar(pred),
            Telescope::Done(body) => body.any_metavar(pred),
        }
    }

    /// Whether any `Term` in a function/Π telescope (`Func`/`FuncType`) — the parameter types and the trailing body/return type — satisfies `pred`, short-circuiting on the first hit. The telescope leg of `Subterm::any_child_term`: `pred` carries the per-node memoized recursion, so this visits each `Term` exactly once.
    pub(crate) fn any_term<F: FnMut(&Term) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Telescope::Cons(ty, rest) => pred(ty) || rest.body().any_term(pred),
            Telescope::Done(body) => pred(body),
        }
    }

    /// Walk a function/Π telescope (`Func`/`FuncType`): the parameter types and the trailing body/return type. Concrete in `Term` — no collector trait needed. See `Subterm::collect_construction_names`.
    pub fn collect_construction_names(&self, names: &mut BTreeSet<Global>) {
        match self {
            Telescope::Cons(ty, rest) => {
                ty.collect_construction_names(names);
                rest.body().collect_construction_names(names);
            }
            Telescope::Done(body) => body.collect_construction_names(names),
        }
    }
}

impl Telescope<Vec<Term>> {
    /// Whether any metavariable in a constructor signature — the payload domains, or one of the index targets it terminates in — satisfies `pred`, short-circuiting on the first hit.
    pub fn any_metavar<F: FnMut(MetavarId) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Telescope::Cons(ty, rest) => ty.any_metavar(pred) || rest.body().any_metavar(pred),
            Telescope::Done(targets) => targets.iter().any(|target| target.any_metavar(pred)),
        }
    }
}

impl Telescope<Telescope<()>> {
    /// Whether any metavariable in a nested arity telescope — a declaration's parameter domains and, at its terminal, its index or field domains — satisfies `pred`, short-circuiting on the first hit.
    pub fn any_metavar<F: FnMut(MetavarId) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Telescope::Cons(ty, rest) => ty.any_metavar(pred) || rest.body().any_metavar(pred),
            Telescope::Done(inner) => inner.any_metavar(pred),
        }
    }
}

impl Telescope<()> {
    /// Whether any metavariable in a Σ telescope (`TupleType`) — only the field types; its `Done` body is `()` — satisfies `pred`, short-circuiting on the first hit.
    pub fn any_metavar<F: FnMut(MetavarId) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Telescope::Cons(ty, rest) => ty.any_metavar(pred) || rest.body().any_metavar(pred),
            // The trailing body is `()`, which holds no metavariables.
            Telescope::Done(_) => false,
        }
    }

    /// Whether any `Term` in a Σ telescope (`TupleType`) — only the field types; its `Done` body is `()` — satisfies `pred`, short-circuiting on the first hit. See the `Telescope<Term>` counterpart above.
    pub(crate) fn any_term<F: FnMut(&Term) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Telescope::Cons(ty, rest) => pred(ty) || rest.body().any_term(pred),
            // The trailing body is `()`, which holds no terms.
            Telescope::Done(_) => false,
        }
    }

    /// Walk a Σ telescope (`TupleType`): only the field types — its `Done` body is `()`, which contributes no names.
    pub fn collect_construction_names(&self, names: &mut BTreeSet<Global>) {
        if let Telescope::Cons(ty, rest) = self {
            ty.collect_construction_names(names);
            rest.body().collect_construction_names(names);
        }
    }
}

impl<B: Bound> fmt::Debug for Telescope<B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

/// All three derivations walk the spine in a loop rather than one native frame per parameter.
///
/// A telescope's length is its written arity, and "written depth is a bound the default stack tolerates" is the assumption this file already retired for `Let`/`Rec` spines — `Visit::enter_scope`/`Visit::leave_scope` exist for exactly this shape. The spine is the sibling that kept the recursion, which is invisible in authored signatures and unbounded in generated ones.
impl<B: Bound> Bound for Telescope<B> {
    fn traverse<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        // Each entry type is visited under the binders declared *before* it, so a link's own binders are entered after its type and retracted in the reverse order on the way back up — which is the bracket `visit_scope` used to keep on the native stack.
        let mut entries = Vec::new();
        let mut current = self;
        let body = loop {
            match current {
                Telescope::Cons(ty, rest) => {
                    entries.push((visit.visit_subterm(ty), rest.names.clone(), rest.arity()));
                    visit.enter_scope(rest.arity());
                    current = rest.body();
                }
                Telescope::Done(body) => break body.traverse(visit),
            }
        };

        entries
            .into_iter()
            .rev()
            .fold(Telescope::Done(body.into()), |rest, (ty, names, arity)| {
                visit.leave_scope(arity);
                Telescope::Cons(
                    ty,
                    Scope {
                        arity: One,
                        names,
                        body: Box::new(rest),
                    },
                )
            })
    }

    /// `saturating_sub` is monotone, so it distributes over `max` — which is what lets the nested `max(ty.reach(), rest.reach())` be flattened into one pass that discounts each entry by the binders standing before it.
    fn reach(&self) -> usize {
        let (mut reach, mut depth) = (0, 0);
        let mut current = self;
        loop {
            match current {
                Telescope::Cons(ty, rest) => {
                    reach = reach.max(ty.reach().saturating_sub(depth));
                    depth += rest.arity();
                    current = rest.body();
                }
                Telescope::Done(body) => {
                    return reach.max(body.reach().saturating_sub(depth));
                }
            }
        }
    }

    fn has_metavar(&self) -> bool {
        let mut current = self;
        loop {
            match current {
                Telescope::Cons(ty, rest) => match ty.has_metavar() {
                    true => return true,
                    false => current = rest.body(),
                },
                Telescope::Done(body) => return body.has_metavar(),
            }
        }
    }

    fn has_transient(&self) -> bool {
        let mut current = self;
        loop {
            match current {
                Telescope::Cons(ty, rest) => match ty.has_transient() {
                    true => return true,
                    false => current = rest.body(),
                },
                Telescope::Done(body) => return body.has_transient(),
            }
        }
    }
}
