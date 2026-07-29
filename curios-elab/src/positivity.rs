//! Strict positivity modulo polarity: the declaration-time gate that makes
//! `induct` and `struct` sound.
//!
//! An `induct` declaration is a claim that a functor has an initial algebra,
//! and the eliminator it hands back is the statement that the algebra is
//! initial. Strict positivity is a syntactic sufficient condition for that
//! functor being polynomial, hence accessible, hence for the initial algebra
//! existing. Without it, `induct Bad | c(f : (Bad) -> False) end` yields a
//! closed inhabitant of `False` in four lines with no recursion at all.
//!
//! "Modulo polarity" is load-bearing on day one, not a later refinement. A
//! check that only recognizes recursive occurrences in immediate payload
//! positions rejects `/std/Toml`, whose recursion travels
//! `Toml → Map(Toml) → struct field → Option → Node → leaf`. Polarity does not
//! weaken the rule — every functor it admits was polynomial already; it only
//! lets the checker recognize that fact through abstraction.
//!
//! The pass computes two facts over the same walk (`occurrences`). The
//! **parameter polarities** — how each type former uses its *i*th parameter —
//! are what composition consumes, and they persist on [`crate::InductDecl`] and
//! [`crate::StructDecl`] so the prelude's are computed once at archive-build time
//! rather than re-derived per compilation. The **occurrence relation** — at
//! what polarity declaration `D` appears inside declaration `E`, transitively
//! closed — is transient; it exists to answer the acceptance test and is
//! discarded.
//!
//! Acceptance is one predicate: the diagonal of the closed occurrence relation
//! must be [`Polarity::Strict`] or [`Polarity::Unused`] for every declaration
//! — no declaration reaches itself through a non-strict path. That predicate
//! does not change. Every future improvement to the analysis changes how
//! precisely `occurrences` computes, never what the test admits.
//!
//! The pass runs on **zonked** Core, from `elaborate_and_zonk_module` and
//! `elaborate_and_zonk_with_prelude`, as a sibling of `validate_universes` —
//! the other whole-module structural gate on finished Core. Running there
//! rather than inside elaboration buys three things: the telescopes are final,
//! they are meta-free (so an unsolved hole reports as an unsolved hole rather
//! than as an unseeable occurrence), and at the replay site the module in hand
//! is already exactly the user suffix, so the analysis needs no key list and
//! no "already analyzed" filter.
//!
//! Two obligations are deliberately out of scope. A declaration that takes a
//! *type-former* parameter — `induct Mu(F : (Type) -> Type) | fix(F(Mu(F)))
//! end` — cannot be checked from its own body, because `F` is a binder with no
//! known polarity; discharging that needs an inferred per-binder obligation in
//! a side store, never a field on `FuncType`. And termination and productivity
//! for `rec` remain unchecked at both the type and value layers. Nothing in
//! the corpus takes a type-former parameter, so the first is co-scheduled with
//! `Mu`; the second is a separate mechanism, not a refinement of this one.

#[cfg(test)]
mod tests;

use {
    super::{
        Bound, Context, Error, Free, FuncType, Global, InductType, Module, One, Prim, Scope,
        StructType, Subterm, Telescope, Term, TupleType, reduce_forced,
    },
    std::{
        collections::{BTreeMap, BTreeSet},
        fmt,
    },
};

/// How a type former uses one parameter, or at what polarity one declaration
/// occurs inside another.
///
/// Five points, ordered `Unused ⊑ Strict ⊑ Pos ⊑ Mixed` and
/// `Unused ⊑ Neg ⊑ Mixed`, with `Strict` and `Neg` incomparable. Only `Strict`
/// and `Unused` are accepting; the middle grades exist because composition has
/// to distinguish them (`Neg ∘ Neg = Pos`) and because "occurs negatively" and
/// "occurs positively but not strictly" are different user errors.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum Polarity {
    /// The parameter does not occur.
    Unused,
    /// Occurs, never under a function arrow.
    Strict,
    /// Occurs under an even number of arrows.
    Pos,
    /// Occurs under an odd number of arrows.
    Neg,
    /// Occurs both ways, or the analysis cannot tell.
    Mixed,
}

impl Polarity {
    /// Whether an occurrence at this polarity is admissible in a recursive
    /// position — the whole acceptance test, applied to one entry.
    fn accepting(self) -> bool {
        matches!(self, Polarity::Unused | Polarity::Strict)
    }

    /// Least upper bound: how several occurrences of the same target combine.
    fn join(self, other: Polarity) -> Polarity {
        use Polarity::*;
        match (self, other) {
            (Unused, p) | (p, Unused) => p,
            (Mixed, _) | (_, Mixed) => Mixed,
            (p, q) if p == q => p,
            // `Strict ⊑ Pos`, so a strict and a positive occurrence join to
            // positive; every other distinct pair spans the two incomparable
            // branches and lands at the top.
            (Strict, Pos) | (Pos, Strict) => Pos,
            _ => Mixed,
        }
    }

    /// Sign multiplication: a target occurring at `self` inside an argument
    /// whose former uses that argument at `outer` occurs at `outer ∘ self`.
    ///
    /// `Unused` annihilates (the argument is never looked at, so nothing inside
    /// it occurs), `Strict` is the identity, and `Mixed` absorbs every
    /// non-`Unused` argument.
    fn compose(self, inner: Polarity) -> Polarity {
        use Polarity::*;
        match (self, inner) {
            (Unused, _) | (_, Unused) => Unused,
            (Strict, p) | (p, Strict) => p,
            (Mixed, _) | (_, Mixed) => Mixed,
            (Pos, Pos) | (Neg, Neg) => Pos,
            (Pos, Neg) | (Neg, Pos) => Neg,
        }
    }

    /// Descend into a function type's domain: what was positive is now
    /// negative and vice versa. `Strict` becomes `Neg` — a strict occurrence
    /// is a positive one that additionally never crossed an arrow, and it has
    /// now crossed one.
    fn flip(self) -> Polarity {
        use Polarity::*;
        match self {
            Unused => Unused,
            Strict | Pos => Neg,
            Neg => Pos,
            Mixed => Mixed,
        }
    }
}

impl fmt::Display for Polarity {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let described = match self {
            Polarity::Unused => "not at all",
            Polarity::Strict => "strictly positively",
            Polarity::Pos => "positively, but not strictly",
            Polarity::Neg => "negatively",
            Polarity::Mixed => "in a position the checker cannot see through",
        };
        formatter.write_str(described)
    }
}

/// Reject every `induct` and `struct` declaration in `module` that is not
/// strictly positive, and record each surviving declaration's parameter
/// polarities on its registry entry.
///
/// Runs on zonked Core, so the telescopes it reads are final and meta-free.
/// `module` is exactly the declaration set to analyze: the whole program when
/// the prelude is being built, and the user suffix alone when the prelude is
/// replayed. Anything the walk reaches outside it is a prelude declaration,
/// whose vector was computed once at archive-build time and is read back from
/// `context`.
///
/// That split is sound because prelude items are program-independent: they
/// cannot mention user code, so no cycle crosses the boundary and every
/// prelude declaration is a sink of the occurrence relation.
pub fn check_positivity(context: &mut Context, module: &mut Module) -> Result<(), Error> {
    if module.induct_decls.is_empty() && module.struct_decls.is_empty() {
        return Ok(());
    }

    // Opening a telescope mints binders and is the expensive half of the pass,
    // while the fixpoint below re-walks the same pieces several times — so
    // split every declaration exactly once, up front.
    let names = module
        .induct_decls
        .keys()
        .chain(module.struct_decls.keys())
        .cloned()
        .collect::<Vec<_>>();
    let mut split = BTreeMap::new();
    for name in &names {
        split.insert(name.clone(), Split::of(context, module, name));
    }

    let (vectors, closed) = fixpoint(context, &split)?;

    for name in &names {
        let diagonal = closed
            .get(name)
            .and_then(|reached| reached.get(name))
            .copied()
            .unwrap_or(Polarity::Unused);
        if !diagonal.accepting() {
            return Err(reject(context, &split, &vectors, &closed, name, diagonal)?);
        }
    }

    for (name, vector) in vectors.0 {
        if let Some(declaration) = module.induct_decls.get_mut(&name) {
            declaration.polarities = vector;
        } else if let Some(declaration) = module.struct_decls.get_mut(&name) {
            declaration.polarities = vector;
        }
    }

    Ok(())
}

/// One walkable piece of a declaration: a constructor payload binder, a struct
/// field, or an index binder's type — named so a rejection can point at it.
#[derive(Debug)]
struct Part {
    label: String,
    type_: Term,
}

/// A declaration opened for analysis: its parameter binders, and every piece
/// of it the walk consumes.
#[derive(Debug)]
struct Split {
    params: Vec<Free>,
    parts: Vec<Part>,
}

impl Split {
    fn of(context: &mut Context, module: &Module, name: &Global) -> Self {
        if let Some(declaration) = module.induct_decls.get(name) {
            let params = binders(context, &declaration.params);
            let arguments = params.iter().map(Term::free_var).collect::<Vec<_>>();
            let mut parts = Vec::new();

            // The index *binder types* are deliberately not walked. They
            // describe the family's arity, not its carrier: they exist to
            // typecheck the index arguments a constructor targets and store
            // nothing, so what a declaration contains is witnessed entirely by
            // its constructor payloads. `Eq(@A : Type) : (x : A, y : A)` is
            // `Strict` in `A` because `refl(@z : A)` has an `A` payload, and
            // walking `x : A` on top of that would only cost the vector its
            // precision — enough to reject the sound
            // `induct Wit | tied(a : Wit, b : Wit, p : Eq(a, b)) end`.
            //
            // Nor do they need to guard a self-reference. `induct Foo : (x :
            // Foo) -> Type` does not elaborate: `x : Foo` requires `Foo` to
            // already be a type, and it is a family until applied to the very
            // index being declared. The elaborator rejects it on kinding
            // before positivity ever runs.
            //
            // Index *arguments* at an occurrence site are a different position
            // that shares the word, and stay `Mixed` — see `walk`.
            for (tag, constructor) in &declaration.constructors {
                // The terminal is the constructed type, not a payload: it is
                // *always* a recursive occurrence and is skipped, which
                // `labelled` does by ignoring the telescope's `Done`.
                let payload = constructor.telescope.clone().open_params(&arguments);
                for (label, type_) in labelled(context, payload) {
                    parts.push(Part {
                        label: format!("{tag}({label})"),
                        type_,
                    });
                }
            }

            return Self { params, parts };
        }

        let declaration = module
            .struct_decls
            .get(name)
            .expect("every analyzed name is an inductive or a struct");
        let params = binders(context, &declaration.params);
        let arguments = params.iter().map(Term::free_var).collect::<Vec<_>>();
        let parts = labelled(context, declaration.fields.clone().open_params(&arguments))
            .into_iter()
            .map(|(label, type_)| Part { label, type_ })
            .collect();

        Self { params, parts }
    }
}

/// One fresh binder per entry of a parameter telescope, carrying the declared
/// hints so a diagnostic reads in the user's own names.
fn binders(context: &mut Context, params: &Telescope<()>) -> Vec<Free> {
    params
        .labels()
        .into_iter()
        .map(|label| {
            let hint = (!label.is_empty()).then_some(label);
            context.fresh(hint)
        })
        .collect::<Vec<_>>()
}

/// Split an opened telescope into `(label, type)` entries, minting a fresh
/// binder per entry so each later type holds free occurrences rather than
/// dangling de Bruijn indices — the precondition for reducing it at all.
fn labelled<B: Bound>(context: &mut Context, telescope: Telescope<B>) -> Vec<(String, Term)> {
    let mut entries = Vec::new();
    let mut telescope = telescope;
    let mut position = 0;
    while let Telescope::Cons(type_, rest) = telescope {
        let hint = rest.first_hint().filter(|hint| !hint.is_empty());
        let label = hint.map_or_else(|| position.to_string(), str::to_string);
        let binder = context.fresh(hint);
        entries.push((label, type_));
        telescope = rest.open(&[&Term::free_var(&binder)]);
        position += 1;
    }
    entries
}

/// Every declaration's occurrences under the current parameter estimates.
fn sweep(
    context: &mut Context,
    vectors: &Vectors,
    split: &Split,
) -> Result<BTreeMap<Target, Polarity>, Error> {
    let mut found = BTreeMap::new();
    for part in &split.parts {
        let mut walk = Walk {
            context,
            params: split.params.clone(),
            vectors,
            unfolded: BTreeSet::new(),
            found: BTreeMap::new(),
        };
        walk.walk(&part.type_, Polarity::Strict)?;
        for (target, polarity) in walk.found {
            found
                .entry(target)
                .and_modify(|entry: &mut Polarity| *entry = entry.join(polarity))
                .or_insert(polarity);
        }
    }
    Ok(found)
}

/// The two fixpoints, run together over the whole declaration set to
/// stability, then the occurrence relation closed transitively.
///
/// Whole-set rather than declaration-by-declaration because a single ordered
/// pass gets the wrong answer twice over. `Node(V)` mentions itself, so its
/// own vector is an input to computing it; and in the mutual pair
/// `A(X) | a(B(X))` / `B(X) | b(f : (X) -> Nat)`, `A` only learns that it is
/// negative in `X` on the round after `B` does. Seeding at `Unused` and
/// joining upward computes the least fixpoint, which is the right one: an
/// inductive declaration *is* the least fixpoint of its own unfolding.
fn fixpoint(
    context: &mut Context,
    split: &BTreeMap<Global, Split>,
) -> Result<(Vectors, Occurrences), Error> {
    let mut vectors = Vectors(
        split
            .iter()
            .map(|(name, entry)| (name.clone(), vec![Polarity::Unused; entry.params.len()]))
            .collect(),
    );
    let mut direct: Occurrences = BTreeMap::new();

    loop {
        let mut changed = false;
        for (name, entry) in split {
            let found = sweep(context, &vectors, entry)?;

            let mut vector = vec![Polarity::Unused; entry.params.len()];
            let mut edges: BTreeMap<Global, Polarity> = BTreeMap::new();
            for (target, polarity) in found {
                match target {
                    Target::Param(index) => vector[index] = vector[index].join(polarity),
                    Target::Decl(other) => {
                        edges
                            .entry(other)
                            .and_modify(|entry| *entry = entry.join(polarity))
                            .or_insert(polarity);
                    }
                }
            }

            if vectors.0.get(name) != Some(&vector) {
                vectors.0.insert(name.clone(), vector);
                changed = true;
            }
            if direct.get(name) != Some(&edges) {
                direct.insert(name.clone(), edges);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    Ok((vectors, close(&direct)))
}

/// Transitively close the occurrence relation under composition: if `middle`
/// occurs in `owner` at `first` and `target` occurs in `middle` at `second`,
/// then `target` occurs in `owner` at `first ∘ second`.
///
/// This is what removes any need for the check to know which declarations were
/// declared as one mutually recursive group. That grouping lives on
/// `Item::Rec` and not on the registry entry, so asking "who else is in my
/// group" would mean reaching outside the registry; closing the relation
/// instead reaches every member of every cycle by construction.
fn close(direct: &Occurrences) -> Occurrences {
    let mut closed = direct.clone();
    loop {
        let mut changed = false;
        let reached = closed.clone();
        for (owner, edges) in direct {
            for (middle, first) in edges {
                let Some(onward) = reached.get(middle) else {
                    continue;
                };
                for (target, second) in onward {
                    let composed = first.compose(*second);
                    let slot = closed
                        .entry(owner.clone())
                        .or_default()
                        .entry(target.clone())
                        .or_insert(Polarity::Unused);
                    let joined = slot.join(composed);
                    if *slot != joined {
                        *slot = joined;
                        changed = true;
                    }
                }
            }
        }
        if !changed {
            return closed;
        }
    }
}

/// Name the piece of `name` that a non-strict path back to `name` starts from.
///
/// Runs only on the error path, which is why the fixpoint above can stay a
/// plain polarity join with no provenance threaded through it. Re-walking one
/// declaration part by part is cheap here and gives a message that points at
/// real source: `Toml`, rejected through `Toml → Map → Node → Toml`, still
/// names the `table` payload the path leaves from.
fn reject(
    context: &mut Context,
    split: &BTreeMap<Global, Split>,
    vectors: &Vectors,
    closed: &Occurrences,
    name: &Global,
    diagonal: Polarity,
) -> Result<Error, Error> {
    let entry = &split[name];
    for part in &entry.parts {
        let mut walk = Walk {
            context,
            params: entry.params.clone(),
            vectors,
            unfolded: BTreeSet::new(),
            found: BTreeMap::new(),
        };
        walk.walk(&part.type_, Polarity::Strict)?;

        for (target, polarity) in walk.found {
            let Target::Decl(other) = target else {
                continue;
            };
            // A direct self-occurrence stands on its own; anything else has to
            // find its way back, and `closed` already knows how.
            let back = match other == *name {
                true => Polarity::Strict,
                false => match closed.get(&other).and_then(|reached| reached.get(name)) {
                    Some(back) => *back,
                    None => continue,
                },
            };
            let through = polarity.compose(back);
            if !through.accepting() {
                return Ok(Error::not_strictly_positive(
                    name.symbol(),
                    part.label.clone(),
                    part.type_.clone(),
                    through,
                ));
            }
        }
    }

    // The parts were re-walked under the final vectors, so one of them always
    // reproduces the offending path. Fall back to the declaration alone rather
    // than panicking on a diagnostic.
    Ok(Error::not_strictly_positive(
        name.symbol(),
        "this declaration",
        Term::type_ground(),
        diagonal,
    ))
}

/// What an occurrence the walk found refers to.
///
/// The two live in one relation because they come from one traversal: the
/// parameter fixpoint consumes the `Param` entries and the occurrence relation
/// consumes the `Decl` entries, so there is exactly one place in the compiler
/// that knows the per-`Subterm` polarity rules.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Target {
    /// The `index`th parameter binder of the declaration being walked.
    Param(usize),
    /// Another declaration, by qualified name — including the walked
    /// declaration itself, which is how a recursive occurrence is recorded.
    Decl(Global),
}

/// At what polarity each declaration occurs inside each other declaration —
/// `relation[owner][mentioned]`. Transient: it exists to answer the acceptance
/// test and is discarded, which is why nothing persists it the way parameter
/// polarities are persisted.
type Occurrences = BTreeMap<Global, BTreeMap<Global, Polarity>>;

/// Each analyzed declaration's parameter polarities as the fixpoint currently
/// estimates them.
///
/// A name absent from the map is outside the module under analysis — a
/// replayed prelude declaration — and answers from its registry entry, whose
/// vector was computed when the archive was built. Either way an unknown
/// lookup is [`Polarity::Mixed`], never `Unused`.
#[derive(Debug, Default)]
struct Vectors(BTreeMap<Global, Vec<Polarity>>);

impl Vectors {
    fn at(&self, context: &Context, name: &Global, index: usize) -> Polarity {
        if let Some(vector) = self.0.get(name) {
            return vector.get(index).copied().unwrap_or(Polarity::Mixed);
        }
        context
            .induct_decl(name)
            .map(|declaration| declaration.polarity(index))
            .or_else(|| {
                context
                    .struct_decl(name)
                    .map(|declaration| declaration.polarity(index))
            })
            .unwrap_or(Polarity::Mixed)
    }
}

/// One declaration's traversal: where it mentions its own parameters, and
/// where it mentions other declarations.
struct Walk<'a> {
    context: &'a mut Context,
    /// The declaration's parameter binders as this traversal opened them, in
    /// declaration order. `Target::Param(i)` is a free occurrence of
    /// `params[i]`.
    params: Vec<Free>,
    vectors: &'a Vectors,
    /// Definitions already unfolded on this traversal. A type-level `let` can
    /// be mentioned from many positions, and a recursive group can mention
    /// itself; without this the sweep in `blocked` would not terminate.
    unfolded: BTreeSet<Free>,
    found: BTreeMap<Target, Polarity>,
}

impl Walk<'_> {
    fn record(&mut self, target: Target, polarity: Polarity) {
        self.found
            .entry(target)
            .and_modify(|found| *found = found.join(polarity))
            .or_insert(polarity);
    }

    /// Weak-head-reduce `term` when its head could reduce, which is what
    /// unfolds a type-level `let` — `pub let Fiber : Type = () -> Async({})`
    /// mentioned as `struct Job { resume : Fiber, … }`, or `Valid(bytes)`
    /// inside `struct Str` — into the type former it actually names.
    ///
    /// Forces a `rec` head rather than leaving it stuck, because a mutually
    /// recursive `induct` group lowers its *type constructors* into a
    /// top-level `rec`: `Pause` inside `step(pause : Pause, …)` elaborates to
    /// a universe instance of a `RecMember`, and without forcing it the whole
    /// `Pause`/`Async` group reads as `Mixed` and is rejected.
    ///
    /// Declines in two cases, both of which leave the term to be treated as
    /// opaque, which is conservative. A term whose `reach` is non-zero still
    /// sits under enclosing binders, and reduction assumes free occurrences —
    /// it would panic on a dangling de Bruijn index. And a reduction that
    /// exhausts the budget (a type-level `rec` that will not converge)
    /// reports nothing rather than failing the compilation.
    fn forced(&mut self, term: &Term) -> Term {
        let reducible = matches!(
            &**term,
            Subterm::Var(_)
                | Subterm::Apply(_)
                | Subterm::UniverseInst(_)
                | Subterm::Proj(_)
                | Subterm::Match(_)
                | Subterm::Let(_)
                | Subterm::Metavar(_)
                | Subterm::Rec(_)
                | Subterm::RecMember(_)
        );
        if !reducible || term.reach() != 0 {
            return term.clone();
        }
        reduce_forced(self.context, term.clone()).unwrap_or_else(|_| term.clone())
    }

    /// Open one telescope binder against a fresh assumption, so the rest of the
    /// telescope holds free occurrences rather than dangling indices and can be
    /// reduced. The binder is never assumed a type: the walk reads structure,
    /// not sorts.
    fn open<B: Bound>(&mut self, rest: Scope<One, Telescope<B>>) -> Telescope<B> {
        let binder = self.context.fresh(rest.first_hint());
        rest.open(&[&Term::free_var(&binder)])
    }

    /// A function type: each domain flips, the terminal keeps its polarity.
    ///
    /// A nullary `() -> X` is `Done(X)` — an *empty* domain telescope — so this
    /// loop never runs and `X` keeps the polarity it arrived with. That is not
    /// an edge case to tolerate but the rule that admits
    /// `step(pause : Pause, next : () -> Async(A))`: a zero-argument function
    /// is a thunk of its result, so `Async(A) = done(A) | step(Pause × X)` is
    /// the polynomial functor it looks like.
    fn arrow(&mut self, telescope: &Telescope<Term>, polarity: Polarity) -> Result<(), Error> {
        let flipped = polarity.flip();
        let mut telescope = telescope.clone();
        loop {
            match telescope {
                Telescope::Done(terminal) => return self.walk(&terminal, polarity),
                Telescope::Cons(domain, rest) => {
                    self.walk(&domain, flipped)?;
                    telescope = self.open(rest);
                }
            }
        }
    }

    /// A field telescope — a tuple type, a struct's fields, a constructor's
    /// payload. Every component keeps the polarity it arrived with: a product
    /// is as positive as its factors.
    fn fields(&mut self, telescope: &Telescope<()>, polarity: Polarity) -> Result<(), Error> {
        let mut telescope = telescope.clone();
        while let Telescope::Cons(field, rest) = telescope {
            self.walk(&field, polarity)?;
            telescope = self.open(rest);
        }
        Ok(())
    }

    /// A nominal type former's arguments, composed against how that former uses
    /// each one. `Toml/table(Map(Toml))` is admitted here and nowhere else:
    /// `Map` is `Strict` in its parameter, so the `Toml` inside stays strict.
    fn arguments(
        &mut self,
        name: &Global,
        arguments: &[Term],
        polarity: Polarity,
    ) -> Result<(), Error> {
        for (index, argument) in arguments.iter().enumerate() {
            let used = self.vectors.at(self.context, name, index);
            self.walk(argument, polarity.compose(used))?;
        }
        Ok(())
    }

    /// A form the walk cannot see through: descend into every direct child at
    /// `Mixed`.
    ///
    /// `Mixed` is a *result*, never a stop. Halting here would let a recursive
    /// occurrence hidden inside a stuck `match` arm or a lambda body pass
    /// unseen, which is a soundness hole rather than a missed refinement. The
    /// descent reuses `Subterm::any_child_term` so a new term former cannot be
    /// added without this walk following it.
    fn opaque(&mut self, term: &Term) -> Result<(), Error> {
        let subterm: &Subterm = term;
        let mut failure = None;
        subterm.any_child_term(&mut |child| match self.walk(child, Polarity::Mixed) {
            Ok(()) => false,
            Err(error) => {
                failure = Some(error);
                true
            }
        });
        if let Some(error) = failure {
            return Err(error);
        }
        self.blocked(term)
    }

    /// Follow the definitions of an unreducible term's free variables.
    ///
    /// `any_child_term` visits scope bodies closed, so a child that still sits
    /// under a binder is one `forced` refused to reduce — and a definition
    /// mentioned there would otherwise hide whatever its body names. Walking
    /// each such definition's body (which is closed, hence safe to reduce)
    /// at `Mixed` closes that gap. `unfolded` keeps a definition that is
    /// mentioned many times, or that mentions itself, from being followed
    /// twice.
    fn blocked(&mut self, term: &Term) -> Result<(), Error> {
        if term.reach() == 0 {
            return Ok(());
        }
        for name in term.free_vars() {
            if !self.unfolded.insert(name.clone()) {
                continue;
            }
            let Some(body) = self.context.var_reduct_at(&name).cloned() else {
                continue;
            };
            self.walk(&body, Polarity::Mixed)?;
        }
        Ok(())
    }

    /// Where `term` mentions this walk's targets, given that `term` itself sits
    /// at `polarity`.
    fn walk(&mut self, term: &Term, polarity: Polarity) -> Result<(), Error> {
        // Nothing inside a position the enclosing former never inspects can
        // occur at all — `Unused` composes to `Unused` however deep the walk
        // would go.
        if polarity == Polarity::Unused {
            return Ok(());
        }

        let term = self.forced(term);
        match &*term {
            // Sorts and unresolved literals name nothing.
            Subterm::Type(_) | Subterm::Prop | Subterm::NumLit(_) => Ok(()),

            Subterm::Var(var) => {
                if let Some(free) = var.as_free()
                    && let Some(index) = self.params.iter().position(|param| param == free)
                {
                    self.record(Target::Param(index), polarity);
                }
                Ok(())
            }

            Subterm::FuncType(FuncType { telescope, .. }) => self.arrow(telescope, polarity),
            Subterm::TupleType(TupleType { telescope }) => self.fields(telescope, polarity),

            // A nominal occurrence is recorded at the polarity it stands at —
            // `Strict` at the top of a payload, but `Neg` for the `Waker` in
            // `park((Waker) -> {})`. Its arguments are walked *as well*, not
            // instead: `Vec(T, n)` inside `Vec`'s own payload is both a
            // recursive occurrence and a use of `T`.
            Subterm::InductType(InductType {
                name,
                params,
                indices,
                ..
            }) => {
                self.record(Target::Decl(name.clone()), polarity);
                self.arguments(name, params, polarity)?;
                // Indices are not parameters: an inductive is not uniform in
                // them, so nothing can be composed and they stay opaque. The
                // corpus never recurses through one.
                for index in indices {
                    self.walk(index, Polarity::Mixed)?;
                }
                Ok(())
            }

            Subterm::StructType(StructType { name, params, .. }) => {
                self.record(Target::Decl(name.clone()), polarity);
                self.arguments(name, params, polarity)
            }

            Subterm::Prim(prim) => self.prim(prim, polarity, &term),

            Subterm::Apply(_)
            | Subterm::Func(_)
            | Subterm::Tuple(_)
            | Subterm::Variant(_)
            | Subterm::Match(_)
            | Subterm::Struct(_)
            | Subterm::Proj(_)
            | Subterm::Let(_)
            | Subterm::Rec(_)
            | Subterm::RecMember(_)
            | Subterm::UniverseInst(_)
            | Subterm::Metavar(_)
            | Subterm::Infix(_) => self.opaque(&term),
        }
    }

    /// Primitive type formers, matched exhaustively rather than consulted
    /// through a side table — so adding a primitive forces a polarity decision
    /// at compile time instead of inheriting a default.
    fn prim(&mut self, prim: &Prim, polarity: Polarity, term: &Term) -> Result<(), Error> {
        match prim {
            // The nullary type formers name nothing.
            Prim::BoolType
            | Prim::NatType
            | Prim::ByteType
            | Prim::IntType
            | Prim::FltType
            | Prim::BinType(_)
            | Prim::HandleType => Ok(()),

            // `Lst` is covariant: a list of `T`s is a finite product of `T`s,
            // so a strict occurrence in the element stays strict. This is the
            // rule that admits `Json/arr(Lst(Json))` and `Toml/arr(Lst(Toml))`.
            Prim::LstType(element) => self.walk(element, polarity),

            // A cell is read *and* written, so it is invariant in its element.
            Prim::CellType(element) => self.walk(element, Polarity::Mixed),

            // Everything remaining is a value or an operation over values, not
            // a type former. Its operands are terms; whatever they mention,
            // they mention at `Mixed`.
            Prim::Bool(_)
            | Prim::BoolAnd(..)
            | Prim::BoolOr(..)
            | Prim::BoolXor(..)
            | Prim::BoolEql(..)
            | Prim::BoolNeq(..)
            | Prim::Nat(_)
            | Prim::NatEql(..)
            | Prim::NatNeq(..)
            | Prim::NatAdd(..)
            | Prim::NatSub(..)
            | Prim::NatMul(..)
            | Prim::NatLt(..)
            | Prim::NatDiv(..)
            | Prim::NatRem(..)
            | Prim::NatGt(..)
            | Prim::NatLte(..)
            | Prim::NatGte(..)
            | Prim::NatAnd(..)
            | Prim::NatOr(..)
            | Prim::NatXor(..)
            | Prim::NatShl(..)
            | Prim::NatShr(..)
            | Prim::NatRotl(..)
            | Prim::NatRotr(..)
            | Prim::NatClz(..)
            | Prim::NatCtz(..)
            | Prim::NatPopcnt(..)
            | Prim::Byte(_)
            | Prim::ByteToNat(..)
            | Prim::NatToByte(..)
            | Prim::ByteEql(..)
            | Prim::ByteLt(..)
            | Prim::ByteLte(..)
            | Prim::ByteGt(..)
            | Prim::ByteGte(..)
            | Prim::Int(_)
            | Prim::IntEql(..)
            | Prim::IntNeq(..)
            | Prim::IntAdd(..)
            | Prim::IntSub(..)
            | Prim::IntMul(..)
            | Prim::IntDiv(..)
            | Prim::IntRem(..)
            | Prim::IntLt(..)
            | Prim::IntGt(..)
            | Prim::IntLte(..)
            | Prim::IntGte(..)
            | Prim::IntAnd(..)
            | Prim::IntOr(..)
            | Prim::IntXor(..)
            | Prim::IntShl(..)
            | Prim::IntShr(..)
            | Prim::IntRotl(..)
            | Prim::IntRotr(..)
            | Prim::IntClz(..)
            | Prim::IntCtz(..)
            | Prim::IntPopcnt(..)
            | Prim::Flt(_)
            | Prim::FltAdd(..)
            | Prim::FltSub(..)
            | Prim::FltMul(..)
            | Prim::FltDiv(..)
            | Prim::FltRem(..)
            | Prim::FltEql(..)
            | Prim::FltNeq(..)
            | Prim::FltLt(..)
            | Prim::FltGt(..)
            | Prim::FltLte(..)
            | Prim::FltGte(..)
            | Prim::FltMin(..)
            | Prim::FltMax(..)
            | Prim::FltNeg(..)
            | Prim::FltAbs(..)
            | Prim::FltSqrt(..)
            | Prim::FltFloor(..)
            | Prim::FltCeil(..)
            | Prim::FltTrunc(..)
            | Prim::FltNearest(..)
            | Prim::FltCopysign(..)
            | Prim::NatToInt(..)
            | Prim::NatToFlt(..)
            | Prim::IntToNat(..)
            | Prim::IntToFlt(..)
            | Prim::FltToNat(..)
            | Prim::FltToLeBytes(..)
            | Prim::FltOfLeBytes(..)
            | Prim::FltToInt(..)
            | Prim::Bin(..)
            | Prim::BinLen(..)
            | Prim::BinEql(..)
            | Prim::BinGet(..)
            | Prim::BinSlice(..)
            | Prim::BinAppend(..)
            | Prim::BinConcat(..)
            | Prim::Lst(_)
            | Prim::LstLen(..)
            | Prim::LstGet(..)
            | Prim::LstSlice(..)
            | Prim::LstAppend(..)
            | Prim::LstConcat(..)
            | Prim::LstMap(..)
            | Prim::Handle(_)
            | Prim::HandleEql(..)
            | Prim::Foreign(..)
            | Prim::Exit(..)
            | Prim::Cell(..)
            | Prim::CellSet(..)
            | Prim::CellGet(..) => self.opaque(term),
        }
    }
}
