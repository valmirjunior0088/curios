//! Strict positivity modulo polarity: the declaration-time gate that makes `induct` and `struct` sound.
//!
//! An `induct` declaration is a claim that a functor has an initial algebra, and the eliminator it hands back is the statement that the algebra is initial. Strict positivity is a syntactic sufficient condition for that functor being polynomial, hence accessible, hence for the initial algebra existing. Without it, `induct Bad | c(f : (Bad) -> False) end` yields a closed inhabitant of `False` in four lines with no recursion at all.
//!
//! "Modulo polarity" is load-bearing on day one, not a later refinement. A check that only recognizes recursive occurrences in immediate payload positions rejects `/std/Toml`, whose recursion travels `Toml → Map(Toml) → struct field → Option → Node → leaf`. Polarity does not weaken the rule — every functor it admits was polynomial already; it only lets the checker recognize that fact through abstraction.
//!
//! The pass computes two facts over the same walk (`occurrences`). The **parameter polarities** — how each type former uses its *i*th parameter — are what composition consumes, and they persist on [`curios_core::InductDecl`] and [`curios_core::StructDecl`] so the prelude's are computed once at archive-build time rather than re-derived per compilation. The **occurrence relation** — at what polarity declaration `D` appears inside declaration `E`, transitively closed — is transient; it exists to answer the acceptance test and is discarded.
//!
//! Acceptance is one predicate: the diagonal of the closed occurrence relation must be [`Polarity::Strict`] or [`Polarity::Unused`] for every declaration — no declaration reaches itself through a non-strict path. That predicate does not change. Every future improvement to the analysis changes how precisely `occurrences` computes, never what the test admits.
//!
//! # Shared, not duplicated
//!
//! Both checkers run *this* analysis, through [`Env`]. It is a total function of post-zonk declarations, so a second implementation would be a second run of the same function on the same input rather than a second opinion; what each side supplies for itself is reduction, unfolding, and the registry fallback for declarations outside the analyzed set. The analysis is infallible by construction — a reduction the driver refuses leaves the term opaque, which is the conservative direction — so its only failure is the refusal it exists to produce.
//!
//! Two obligations are deliberately out of scope. A declaration that takes a *type-former* parameter — `induct Mu(F : (Type) -> Type) | fix(F(Mu(F))) end` — cannot be checked from its own body, because `F` is a binder with no known polarity; discharging that needs an inferred per-binder obligation in a side store, never a field on `FuncType`. And termination and productivity for `rec` remain unchecked at both the type and value layers. Nothing in the corpus takes a type-former parameter, so the first is co-scheduled with `Mu`; the second is a separate mechanism, not a refinement of this one.

#[cfg(test)]
mod tests;

use {
    crate::{Env, forceable},
    curios_core::{
        Bound, Free, FuncType, Global, InductDecl, InductType, Intrinsic, One, Polarity, Scope,
        StructDecl, StructType, Subterm, Telescope, Term, TupleType,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// Why a declaration set was refused: `name`'s `part` reaches back to `name` at a non-accepting `polarity`. The driver owns the rendering; `type_` is the offending part's type, for a diagnostic that points at real source.
#[derive(Debug, Clone, PartialEq)]
pub struct NotPositive {
    pub name: Global,
    pub part: String,
    pub type_: Term,
    pub polarity: Polarity,
}

/// Analyze every declaration in `inducts` and `structs` for strict positivity modulo polarity, returning each declaration's parameter-polarity vector, or the first refusal in name order.
///
/// The maps are exactly the declaration set to analyze: the whole program when the kernel re-checks or the prelude is being built, and the user suffix alone when the elaborator replays a prelude. Anything the walk reaches outside them answers from the driver's registry ([`Env::induct_decl`] / [`Env::struct_decl`]), whose vector was computed when that declaration was — sound at the replay boundary because prelude items cannot mention user code, so every out-of-set declaration is a sink of the occurrence relation.
pub fn positivity_vectors<E: Env>(
    env: &mut E,
    inducts: &BTreeMap<Global, InductDecl>,
    structs: &BTreeMap<Global, StructDecl>,
    coverage: Coverage,
) -> Result<BTreeMap<Global, Vec<Polarity>>, NotPositive> {
    curios_profile::profile!("positivity_vectors");
    if inducts.is_empty() && structs.is_empty() {
        return Ok(BTreeMap::new());
    }

    // Opening a telescope mints binders and is the expensive half of the pass, while the fixpoint below re-walks the same pieces several times — so split every declaration exactly once, up front.
    let names = inducts
        .keys()
        .chain(structs.keys())
        .cloned()
        .collect::<Vec<_>>();
    let mut split = BTreeMap::new();
    for name in &names {
        split.insert(name.clone(), Split::of(env, inducts, structs, name));
    }

    let (vectors, closed) = fixpoint(env, &split, coverage);

    for name in &names {
        let diagonal = closed
            .get(name)
            .and_then(|reached| reached.get(name))
            .copied()
            .unwrap_or(Polarity::Unused);
        if !diagonal.accepting() {
            return Err(refusal(env, &split, &vectors, &closed, name, diagonal));
        }
    }

    Ok(vectors.computed)
}

/// One walkable piece of a declaration: a constructor payload binder, a struct field, or an index binder's type — named so a rejection can point at it.
#[derive(Debug)]
struct Split {
    params: Vec<Free>,
    parts: Vec<Part>,
}

/// A declaration opened for analysis: its parameter binders, and every piece of it the walk consumes.
#[derive(Debug)]
struct Part {
    label: String,
    type_: Term,
}

impl Split {
    fn of<E: Env>(
        env: &mut E,
        inducts: &BTreeMap<Global, InductDecl>,
        structs: &BTreeMap<Global, StructDecl>,
        name: &Global,
    ) -> Self {
        if let Some(declaration) = inducts.get(name) {
            let params = binders(env, &declaration.arity);
            let arguments = params.iter().map(Term::free_var).collect::<Vec<_>>();
            let mut parts = Vec::new();

            // The index *binder types* are deliberately not walked. They describe the family's arity, not its carrier: they exist to typecheck the index arguments a constructor targets and store nothing, so what a declaration contains is witnessed entirely by its constructor payloads. `Eq(@A : Type) : (x : A, y : A)` is `Strict` in `A` because `refl(@z : A)` has an `A` payload, and walking `x : A` on top of that would only cost the vector its precision — enough to reject the sound `induct Wit | tied(a : Wit, b : Wit, p : Eq(a, b)) end`.
            //
            // Nor do they need to guard a self-reference. `induct Foo : (x : Foo) -> Type` does not elaborate: `x : Foo` requires `Foo` to already be a type, and it is a family until applied to the very index being declared. The elaborator rejects it on kinding before positivity ever runs.
            //
            // Index *arguments* at an occurrence site are a different position that shares the word, and stay `Mixed` — see `walk`.
            for (tag, constructor) in &declaration.constructors {
                // The terminal is the constructed type, not a payload: it is *always* a recursive occurrence and is skipped, which `labelled` does by ignoring the telescope's `Done`.
                let payload = constructor.telescope.clone().open_params(&arguments);
                for (label, type_) in labelled(env, payload) {
                    parts.push(Part {
                        label: format!("{tag}({label})"),
                        type_,
                    });
                }
            }

            return Self { params, parts };
        }

        let declaration = structs
            .get(name)
            .expect("every analyzed name is an inductive or a struct");
        let params = binders(env, &declaration.arity);
        let arguments = params.iter().map(Term::free_var).collect::<Vec<_>>();
        let parts = labelled(env, declaration.fields_at(&arguments))
            .into_iter()
            .map(|(label, type_)| Part { label, type_ })
            .collect();

        Self { params, parts }
    }
}

/// One fresh binder per entry of a parameter telescope, carrying the declared hints so a diagnostic reads in the user's own names.
fn binders<E: Env, B: Bound>(env: &mut E, params: &Telescope<B>) -> Vec<Free> {
    params
        .labels()
        .into_iter()
        .map(|label| {
            let hint = (!label.is_empty()).then_some(label);
            env.fresh(hint)
        })
        .collect::<Vec<_>>()
}

/// Split an opened telescope into `(label, type)` entries, minting a fresh binder per entry so each later type holds free occurrences rather than dangling de Bruijn indices — the precondition for reducing it at all.
fn labelled<E: Env, B: Bound>(env: &mut E, telescope: Telescope<B>) -> Vec<(String, Term)> {
    let mut entries = Vec::new();
    let mut telescope = telescope;
    let mut position = 0;
    while let Telescope::Cons(type_, rest) = telescope {
        let hint = rest.first_hint().filter(|hint| !hint.is_empty());
        let label = hint.map_or_else(|| position.to_string(), str::to_string);
        let binder = env.fresh(hint);
        entries.push((label, type_));
        telescope = rest.open(&[&Term::free_var(&binder)]);
        position += 1;
    }
    entries
}

/// One declaration's occurrences under the current parameter estimates.
fn sweep<E: Env>(env: &mut E, vectors: &Vectors, split: &Split) -> BTreeMap<Target, Polarity> {
    let mut found = BTreeMap::new();
    for part in &split.parts {
        let mut walk = Walk {
            env,
            params: split.params.clone(),
            vectors,
            unfolded: BTreeSet::new(),
            found: BTreeMap::new(),
        };
        walk.walk(&part.type_, Polarity::Strict);
        for (target, polarity) in walk.found {
            found
                .entry(target)
                .and_modify(|entry: &mut Polarity| *entry = entry.join(polarity))
                .or_insert(polarity);
        }
    }
    found
}

/// The two fixpoints, run together over the whole declaration set to stability, then the occurrence relation closed transitively.
///
/// Whole-set rather than declaration-by-declaration because a single ordered pass gets the wrong answer twice over. `Node(V)` mentions itself, so its own vector is an input to computing it; and in the mutual pair `A(X) | a(B(X))` / `B(X) | b(f : (X) -> Nat)`, `A` only learns that it is negative in `X` on the round after `B` does. Seeding at `Unused` and joining upward computes the least fixpoint, which is the right one: an inductive declaration *is* the least fixpoint of its own unfolding.
fn fixpoint<E: Env>(
    env: &mut E,
    split: &BTreeMap<Global, Split>,
    coverage: Coverage,
) -> (Vectors, Occurrences) {
    let mut vectors = Vectors {
        computed: split
            .iter()
            .map(|(name, entry)| (name.clone(), vec![Polarity::Unused; entry.params.len()]))
            .collect(),
        coverage,
    };
    let mut direct: Occurrences = BTreeMap::new();

    loop {
        let mut changed = false;
        for (name, entry) in split {
            let found = sweep(env, &vectors, entry);

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

            if vectors.computed.get(name) != Some(&vector) {
                vectors.computed.insert(name.clone(), vector);
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

    (vectors, close(&direct))
}

/// Transitively close the occurrence relation under composition: if `middle` occurs in `owner` at `first` and `target` occurs in `middle` at `second`, then `target` occurs in `owner` at `first ∘ second`.
///
/// This is what removes any need for the check to know which declarations were declared as one mutually recursive group. That grouping lives on `Item::Rec` and not on the registry entry, so asking "who else is in my group" would mean reaching outside the registry; closing the relation instead reaches every member of every cycle by construction.
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
/// Runs only on the error path, which is why the fixpoint above can stay a plain polarity join with no provenance threaded through it. Re-walking one declaration part by part is cheap here and gives a message that points at real source: `Toml`, rejected through `Toml → Map → Node → Toml`, still names the `table` payload the path leaves from.
fn refusal<E: Env>(
    env: &mut E,
    split: &BTreeMap<Global, Split>,
    vectors: &Vectors,
    closed: &Occurrences,
    name: &Global,
    diagonal: Polarity,
) -> NotPositive {
    let entry = &split[name];
    for part in &entry.parts {
        let mut walk = Walk {
            env,
            params: entry.params.clone(),
            vectors,
            unfolded: BTreeSet::new(),
            found: BTreeMap::new(),
        };
        walk.walk(&part.type_, Polarity::Strict);

        for (target, polarity) in walk.found {
            let Target::Decl(other) = target else {
                continue;
            };
            // A direct self-occurrence stands on its own; anything else has to find its way back, and `closed` already knows how.
            let back = match other == *name {
                true => Polarity::Strict,
                false => match closed.get(&other).and_then(|reached| reached.get(name)) {
                    Some(back) => *back,
                    None => continue,
                },
            };
            let through = polarity.compose(back);
            if !through.accepting() {
                return NotPositive {
                    name: name.clone(),
                    part: part.label.clone(),
                    type_: part.type_.clone(),
                    polarity: through,
                };
            }
        }
    }

    // The parts were re-walked under the final vectors, so one of them always reproduces the offending path. Fall back to the declaration alone rather than panicking on a diagnostic.
    NotPositive {
        name: name.clone(),
        part: "this declaration".to_string(),
        type_: Term::type_ground(),
        polarity: diagonal,
    }
}

/// What an occurrence the walk found refers to.
///
/// The two live in one relation because they come from one traversal: the parameter fixpoint consumes the `Param` entries and the occurrence relation consumes the `Decl` entries, so there is exactly one place in the compiler that knows the per-`Subterm` polarity rules.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Target {
    /// The `index`th parameter binder of the declaration being walked.
    Param(usize),
    /// Another declaration, by qualified name — including the walked declaration itself, which is how a recursive occurrence is recorded.
    Decl(Global),
}

/// At what polarity each declaration occurs inside each other declaration — `relation[owner][mentioned]`. Transient: it exists to answer the acceptance test and is discarded, which is why nothing persists it the way parameter polarities are persisted.
type Occurrences = BTreeMap<Global, BTreeMap<Global, Polarity>>;

/// Whether the declarations handed to the analysis are every declaration the program has, or only a suffix of them.
///
/// The distinction decides what an out-of-set name means. Under [`Coverage::Partial`] — the elaborator at a replay, holding the user suffix while the prelude prefix was analyzed when it was elaborated — a name from outside answers from the registry vector recorded then, which is that pass's own earlier result. Under [`Coverage::Complete`] there is no outside: every declaration is in hand, so a name not among them is one this analysis has no result for, and reading a carried vector would mean believing an answer some *other* pass computed. The kernel therefore takes the conservative value instead, which is the same one an unknown name has always taken.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Coverage {
    /// Every declaration the program has.
    Complete,
    /// A suffix, with the earlier declarations analyzed elsewhere.
    Partial,
}

/// Each analyzed declaration's parameter polarities as the fixpoint currently estimates them.
///
/// A name absent from the map is outside the set under analysis. What that means depends on [`Coverage`]; either way an unknown lookup is [`Polarity::Mixed`], never `Unused`.
#[derive(Debug)]
struct Vectors {
    computed: BTreeMap<Global, Vec<Polarity>>,
    coverage: Coverage,
}

impl Vectors {
    fn at<E: Env>(&self, env: &E, name: &Global, index: usize) -> Polarity {
        if let Some(vector) = self.computed.get(name) {
            return vector.get(index).copied().unwrap_or(Polarity::Mixed);
        }
        if self.coverage == Coverage::Complete {
            return Polarity::Mixed;
        }
        env.induct_decl(name)
            .map(|declaration| declaration.polarity(index))
            .or_else(|| {
                env.struct_decl(name)
                    .map(|declaration| declaration.polarity(index))
            })
            .unwrap_or(Polarity::Mixed)
    }
}

/// One declaration's traversal: where it mentions its own parameters, and where it mentions other declarations.
struct Walk<'a, E: Env> {
    env: &'a mut E,
    /// The declaration's parameter binders as this traversal opened them, in declaration order. `Target::Param(i)` is a free occurrence of `params[i]`.
    params: Vec<Free>,
    vectors: &'a Vectors,
    /// Definitions already unfolded on this traversal. A type-level `let` can be mentioned from many positions, and a recursive group can mention itself; without this the sweep in `blocked` would not terminate.
    unfolded: BTreeSet<Free>,
    found: BTreeMap<Target, Polarity>,
}

impl<E: Env> Walk<'_, E> {
    fn record(&mut self, target: Target, polarity: Polarity) {
        self.found
            .entry(target)
            .and_modify(|found| *found = found.join(polarity))
            .or_insert(polarity);
    }

    /// Weak-head-reduce `term` when its head could reduce, which is what unfolds a type-level `let` — `pub let Fiber : Type = () -> Async({})` mentioned as `struct Job { resume : Fiber, … }`, or `Valid(bytes)` inside `struct Str` — into the type former it actually names.
    ///
    /// Forces a `rec` head rather than leaving it stuck, because a mutually recursive `induct` group lowers its *type constructors* into a top-level `rec`: `Pause` inside `step(pause : Pause, …)` elaborates to a universe instance of a projection, and without forcing it the whole `Pause`/`Async` group reads as `Mixed` and is rejected.
    ///
    /// Declines in two cases, both of which leave the term to be treated as opaque, which is conservative. [`forceable`] owns the first — a head that cannot move, or a term still under enclosing binders. And a reduction the driver refuses (an exhausted budget on a type-level `rec` that will not converge) reports nothing rather than failing the analysis.
    fn forced(&mut self, term: &Term) -> Term {
        if !forceable(term) {
            return term.clone();
        }
        self.env.force(term).unwrap_or_else(|_| term.clone())
    }

    /// Open one telescope binder against a fresh assumption, so the rest of the telescope holds free occurrences rather than dangling indices and can be reduced. The binder is never assumed a type: the walk reads structure, not sorts.
    fn open<B: Bound>(&mut self, rest: Scope<One, Telescope<B>>) -> Telescope<B> {
        let binder = self.env.fresh(rest.first_hint());
        rest.open(&[&Term::free_var(&binder)])
    }

    /// A function type: each domain flips, the terminal keeps its polarity.
    ///
    /// A nullary `() -> X` is `Done(X)` — an *empty* domain telescope — so this loop never runs and `X` keeps the polarity it arrived with. That is not an edge case to tolerate but the rule that admits `step(pause : Pause, next : () -> Async(A))`: a zero-argument function is a thunk of its result, so `Async(A) = done(A) | step(Pause × X)` is the polynomial functor it looks like.
    fn arrow(&mut self, telescope: &Telescope<Term>, polarity: Polarity) {
        let flipped = polarity.flip();
        let mut telescope = telescope.clone();
        loop {
            match telescope {
                Telescope::Done(terminal) => return self.walk(&terminal, polarity),
                Telescope::Cons(domain, rest) => {
                    self.walk(&domain, flipped);
                    telescope = self.open(rest);
                }
            }
        }
    }

    /// A field telescope — a tuple type, a struct's fields, a constructor's payload. Every component keeps the polarity it arrived with: a product is as positive as its factors.
    fn fields(&mut self, telescope: &Telescope<()>, polarity: Polarity) {
        let mut telescope = telescope.clone();
        while let Telescope::Cons(field, rest) = telescope {
            self.walk(&field, polarity);
            telescope = self.open(rest);
        }
    }

    /// A nominal type former's arguments, composed against how that former uses each one. `Toml/table(Map(Toml))` is admitted here and nowhere else: `Map` is `Strict` in its parameter, so the `Toml` inside stays strict.
    fn arguments(&mut self, name: &Global, arguments: &[Term], polarity: Polarity) {
        for (index, argument) in arguments.iter().enumerate() {
            let used = self.vectors.at(self.env, name, index);
            self.walk(argument, polarity.compose(used));
        }
    }

    /// A form the walk cannot see through: descend into every direct child at `Mixed`.
    ///
    /// `Mixed` is a *result*, never a stop. Halting here would let a recursive occurrence hidden inside a stuck `match` arm or a lambda body pass unseen, which is a soundness hole rather than a missed refinement. The descent reuses `Subterm::any_child_term` so a new term former cannot be added without this walk following it.
    fn opaque(&mut self, term: &Term) {
        let subterm: &Subterm = term;
        subterm.any_child_term(&mut |child| {
            self.walk(child, Polarity::Mixed);
            false
        });
        self.blocked(term);
    }

    /// Follow the definitions of an unreducible term's free variables.
    ///
    /// `any_child_term` visits scope bodies closed, so a child that still sits under a binder is one `forced` refused to reduce — and a definition mentioned there would otherwise hide whatever its body names. Walking each such definition's body (which is closed, hence safe to reduce) at `Mixed` closes that gap. `unfolded` keeps a definition that is mentioned many times, or that mentions itself, from being followed twice.
    fn blocked(&mut self, term: &Term) {
        if term.reach() == 0 {
            return;
        }
        for name in term.free_vars() {
            if !self.unfolded.insert(name.clone()) {
                continue;
            }
            let Some(body) = self.env.unfold(&name).cloned() else {
                continue;
            };
            self.walk(&body, Polarity::Mixed);
        }
    }

    /// Where `term` mentions this walk's targets, given that `term` itself sits at `polarity`.
    fn walk(&mut self, term: &Term, polarity: Polarity) {
        // Nothing inside a position the enclosing former never inspects can occur at all — `Unused` composes to `Unused` however deep the walk would go.
        if polarity == Polarity::Unused {
            return;
        }

        let term = self.forced(term);
        match &*term {
            // Sorts name nothing.
            Subterm::Type(_) | Subterm::Prop => {}

            Subterm::Var(var) => {
                if let Some(free) = var.as_free()
                    && let Some(index) = self.params.iter().position(|param| param == free)
                {
                    self.record(Target::Param(index), polarity);
                }
            }

            Subterm::FuncType(FuncType { telescope, .. }) => self.arrow(telescope, polarity),
            Subterm::TupleType(TupleType { telescope }) => self.fields(telescope, polarity),

            // A nominal occurrence is recorded at the polarity it stands at — `Strict` at the top of a payload, but `Neg` for the `Waker` in `park((Waker) -> {})`. Its arguments are walked *as well*, not instead: `Vec(T, n)` inside `Vec`'s own payload is both a recursive occurrence and a use of `T`.
            Subterm::InductType(InductType {
                name,
                params,
                indices,
                ..
            }) => {
                self.record(Target::Decl(name.clone()), polarity);
                self.arguments(name, params, polarity);
                // Indices are not parameters: an inductive is not uniform in them, so nothing can be composed and they stay opaque. The corpus never recurses through one.
                for index in indices {
                    self.walk(index, Polarity::Mixed);
                }
            }

            Subterm::StructType(StructType { name, params, .. }) => {
                self.record(Target::Decl(name.clone()), polarity);
                self.arguments(name, params, polarity);
            }

            Subterm::Intrinsic(intrinsic) => self.intrinsic(intrinsic, polarity, &term),

            // A host call is opaque for the same reason an effectful intrinsic is: its result comes from outside the program, so nothing about the declaration being checked can be read out of it.
            Subterm::Foreign(..) => self.opaque(&term),

            Subterm::Apply(_)
            | Subterm::Func(_)
            | Subterm::Tuple(_)
            | Subterm::Variant(_)
            | Subterm::Match(_)
            | Subterm::Struct(_)
            | Subterm::Proj(_)
            | Subterm::Let(_)
            | Subterm::Rec(_)
            | Subterm::UniverseInst(_)
            | Subterm::Metavar(_)
            | Subterm::Transient(_) => self.opaque(&term),
        }
    }

    /// Intrinsic type formers, matched exhaustively rather than consulted through a side table — so adding an intrinsic forces a polarity decision at compile time instead of inheriting a default.
    fn intrinsic(&mut self, intrinsic: &Intrinsic, polarity: Polarity, term: &Term) {
        match intrinsic {
            // The nullary type formers name nothing.
            Intrinsic::BoolType
            | Intrinsic::NatType
            | Intrinsic::ByteType
            | Intrinsic::IntType
            | Intrinsic::FltType
            | Intrinsic::BinType(_)
            | Intrinsic::HandleType => {}

            // `List` is covariant: a list of `T`s is a finite product of `T`s, so a strict occurrence in the element stays strict. This is the rule that admits `Json/arr(List(Json))` and `Toml/arr(List(Toml))`.
            Intrinsic::ListType(element) => self.walk(element, polarity),

            // A cell is read *and* written, so it is invariant in its element.
            Intrinsic::CellType(element) => self.walk(element, Polarity::Mixed),

            // `Io` is covariant, and by the same reading as `List` rather than by analogy to `Cell`.
            //
            // A description of a `T` is operationally the delayed `T` its thunk erasure makes it — `{} -> T`, whose codomain is a strictly positive position. What decides the polarity is what the eliminations can extract, and `Io`'s are *weaker* than `List`'s: `List/at` hands back a `T` outright, while `bind` never exposes the `A` except inside another `Io`, and there is no `Io(T) -> T` for any other rule to lean on. A carrier whose only reader is stricter than a covariant carrier's cannot be less positive than it.
            //
            // This is the rule that admits a suspension whose continuation is computed by performing an effect — `Step(A) | step(Pause, next: Io(Step(A)))`, which is what `/std/Async` is once its hand-rolled delay is replaced by the typed one.
            Intrinsic::IoType(result) => self.walk(result, polarity),

            // Everything remaining is a value or an operation over values, not a type former. Its operands are terms; whatever they mention, they mention at `Mixed`.
            Intrinsic::Bool(_)
            | Intrinsic::BoolAnd(..)
            | Intrinsic::BoolOr(..)
            | Intrinsic::BoolXor(..)
            | Intrinsic::BoolEql(..)
            | Intrinsic::BoolNeq(..)
            | Intrinsic::Nat(_)
            | Intrinsic::NatEql(..)
            | Intrinsic::NatNeq(..)
            | Intrinsic::NatAdd(..)
            | Intrinsic::NatSub(..)
            | Intrinsic::NatMul(..)
            | Intrinsic::NatLt(..)
            | Intrinsic::NatDiv(..)
            | Intrinsic::NatRem(..)
            | Intrinsic::NatGt(..)
            | Intrinsic::NatLte(..)
            | Intrinsic::NatGte(..)
            | Intrinsic::NatAnd(..)
            | Intrinsic::NatOr(..)
            | Intrinsic::NatXor(..)
            | Intrinsic::NatShl(..)
            | Intrinsic::NatShr(..)
            | Intrinsic::NatRotl(..)
            | Intrinsic::NatRotr(..)
            | Intrinsic::NatClz(..)
            | Intrinsic::NatCtz(..)
            | Intrinsic::NatPopcnt(..)
            | Intrinsic::Byte(_)
            | Intrinsic::ByteToNat(..)
            | Intrinsic::NatToByte(..)
            | Intrinsic::ByteEql(..)
            | Intrinsic::ByteLt(..)
            | Intrinsic::ByteLte(..)
            | Intrinsic::ByteGt(..)
            | Intrinsic::ByteGte(..)
            | Intrinsic::Int(_)
            | Intrinsic::IntEql(..)
            | Intrinsic::IntNeq(..)
            | Intrinsic::IntAdd(..)
            | Intrinsic::IntSub(..)
            | Intrinsic::IntMul(..)
            | Intrinsic::IntDiv(..)
            | Intrinsic::IntRem(..)
            | Intrinsic::IntLt(..)
            | Intrinsic::IntGt(..)
            | Intrinsic::IntLte(..)
            | Intrinsic::IntGte(..)
            | Intrinsic::IntAnd(..)
            | Intrinsic::IntOr(..)
            | Intrinsic::IntXor(..)
            | Intrinsic::IntShl(..)
            | Intrinsic::IntShr(..)
            | Intrinsic::IntRotl(..)
            | Intrinsic::IntRotr(..)
            | Intrinsic::IntClz(..)
            | Intrinsic::IntCtz(..)
            | Intrinsic::IntPopcnt(..)
            | Intrinsic::Flt(_)
            | Intrinsic::FltAdd(..)
            | Intrinsic::FltSub(..)
            | Intrinsic::FltMul(..)
            | Intrinsic::FltDiv(..)
            | Intrinsic::FltRem(..)
            | Intrinsic::FltEql(..)
            | Intrinsic::FltNeq(..)
            | Intrinsic::FltLt(..)
            | Intrinsic::FltGt(..)
            | Intrinsic::FltLte(..)
            | Intrinsic::FltGte(..)
            | Intrinsic::FltMin(..)
            | Intrinsic::FltMax(..)
            | Intrinsic::FltNeg(..)
            | Intrinsic::FltAbs(..)
            | Intrinsic::FltSqrt(..)
            | Intrinsic::FltFloor(..)
            | Intrinsic::FltCeil(..)
            | Intrinsic::FltTrunc(..)
            | Intrinsic::FltNearest(..)
            | Intrinsic::FltCopysign(..)
            | Intrinsic::NatToInt(..)
            | Intrinsic::NatToFlt(..)
            | Intrinsic::IntToNat(..)
            | Intrinsic::IntToFlt(..)
            | Intrinsic::FltToNat(..)
            | Intrinsic::FltToLeBytes(..)
            | Intrinsic::FltOfLeBytes(..)
            | Intrinsic::FltToInt(..)
            | Intrinsic::Bin(..)
            | Intrinsic::BinLen(..)
            | Intrinsic::BinEql(..)
            | Intrinsic::BinGet(..)
            | Intrinsic::BinSlice(..)
            | Intrinsic::BinAppend(..)
            | Intrinsic::BinConcat(..)
            | Intrinsic::List(..)
            | Intrinsic::ListLen(..)
            | Intrinsic::ListGet(..)
            | Intrinsic::ListSlice(..)
            | Intrinsic::ListAppend(..)
            | Intrinsic::ListConcat(..)
            | Intrinsic::ListMap(..)
            | Intrinsic::Handle(_)
            | Intrinsic::HandleEql(..)
            | Intrinsic::ProcExit(..)
            | Intrinsic::Cell(..)
            | Intrinsic::CellSet(..)
            | Intrinsic::CellGet(..)
            | Intrinsic::IoPure(..)
            | Intrinsic::IoBind(..) => self.opaque(term),
        }
    }
}
