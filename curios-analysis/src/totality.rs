//! Size-change termination: which `rec` groups descend.
//!
//! Curios keeps general recursion. What it cannot keep is general recursion in the places erasure *removes*, because a divergent type breaks type formation and a divergent proof proves anything, while a program that loops is only a program that loops. This module supplies the fact both checkers need: whether a recursive group terminates on every call path.
//!
//! A group is accepted by the size-change principle (Lee–Jones–Ben-Amram, in the shape Abel's `foetus` gave it for dependent types). Each recursive call contributes a **call matrix** grading every callee argument against every caller parameter as strictly smaller, equal, or unrelated. The matrices are closed transitively under composition, and the group is accepted when every **idempotent** matrix in that closure carries a strict decrease on its diagonal — an idempotent matrix describes a call path that can repeat forever, so a decrease on it is a decrease that cannot be sustained.
//!
//! Size-change rather than structural recursion because the corpus needs it. `/std/BigNat/add/raw` descends on *either* of two `Bits` arguments depending on the arm, `add/raw_assoc` does the same over three, and `add/raw_comm` needs the mutual closure across two members. A rule keyed to one designated argument rejects all of them, and a fold cannot express them either, because a fold cannot short-circuit.
//!
//! **Match refinement is part of the size order, not an optimization.** A parameter scrutinized by an enclosing arm is expanded through that arm's constructor before it is compared, so in `add/raw`'s empty-`x` arm the literal argument `b[]` grades *equal* to `x` rather than unknown. Without that, the two nil-arm matrices compose to an all-unknown matrix, which is idempotent with no decrease anywhere on its diagonal, and `add/raw` is rejected on a call path that cannot actually occur.
//!
//! # Shared, not duplicated
//!
//! Both checkers run *this* engine, through [`Env`]: it is a total function of post-zonk terms, so a second implementation would be a second run of the same function on the same input rather than a second opinion. What differs is the obligation each driver hangs on the verdict. The elaborator's is positional and whole-module — obligations (T) and (V), seeded from what elaboration settled, turning a `Partial` classification into a rejection only where erasure deletes. The kernel's is local and self-derivable: a `rec` member whose declared type is a proof or yields a sort must descend, because assuming it at that type otherwise certifies `rec f : False = f`. Rejection by the *engine* is a classification, not an error — corecursive and productive definitions classify `Partial` and stay usable everywhere erasure keeps them.

mod guard;
use guard::*;

mod matrix;
use matrix::*;

mod shape;
use shape::*;

#[cfg(test)]
mod tests;

use {
    crate::{Env, forceable},
    curios_core::{
        Apply, Arity, Atom, Carrier, Cases, Free, FreeMonoid, Func, FuncType, InductType,
        Intrinsic, Layer, Let, Many, Match, Nat, One, Proj, Rec, RecGroup, Scope, Struct,
        StructType, Subterm, Telescope, Term, Three, Totality, Tuple, TupleType, Two, UniverseInst,
        Variant,
    },
    num_bigint::BigUint,
    std::{
        collections::{BTreeMap, BTreeSet},
        iter::once,
    },
};

/// Whether shape reading may force `term`.
///
/// [`forceable`] answers the two halves every analysis on this seam shares — whether the head could move, and whether the term is closed enough to reduce. This adds totality's own, and it is the whole of what replaced a step count: **a `rec` head is refused.** The group under analysis is the one whose termination is being decided, and a group already in scope may be legitimately partial — `check_rec_group` demands descent only where a member is erased, so `/std/Async`'s scheduler and `/std/Json/decode` classify `Partial` and are still defined. Unfolding either can spin, and refusing makes the read stop on its own terms instead of by exhausting the reduction budget.
///
/// Measured inert: across the corpus not one of 288 load-bearing unfoldings had a `rec` head, so this refuses nothing that was being read and is a determinism guarantee rather than a restriction.
fn readable(term: &Term) -> bool {
    forceable(term) && !matches!(&**term, Subterm::Rec(_))
}

/// How deep a refinement chain may be expanded.
///
/// Each nested `match` on a binder introduced by an outer arm adds one level — `raw_trimmed` reaches three — so this is generous. It exists only so a pathological or cyclic refinement map cannot loop.
const EXPAND_FUEL: usize = 16;

/// Whether every recursive call path in `group` descends.
///
/// This is the whole of size-change termination as this compiler applies it: collect one matrix per call site, close them under composition, and demand a decrease on the diagonal of every idempotent result. A group with no recursive call at all closes to nothing and is accepted, which is how the prelude's call-free `; ih` folds pass — their recursion is the intrinsic eliminator's, already structural by construction.
pub fn group_totality<E: Env>(env: &mut E, group: &RecGroup) -> Totality {
    let mut members = Vec::new();
    for index in 0..group.length() {
        members.push(Member::of(env, group, index));
    }
    let arities = members.iter().map(|member| member.params.len()).collect();

    let mut calls = Vec::new();
    for (index, member) in members.iter().enumerate() {
        let mut walk = Walk {
            env,
            group,
            arities: &arities,
            caller: index,
            params: &member.params,
            refined: BTreeMap::new(),
            nonzero: BTreeSet::new(),
            entered: Vec::new(),
            ops: Vec::new(),
            scopes: Vec::new(),
            calls: Vec::new(),
        };
        walk.walk(&member.body);
        calls.extend(walk.calls);
    }

    match close(calls) {
        Some(closed) => match closed
            .iter()
            .all(|(from, to, matrix)| from != to || !matrix.is_idempotent() || matrix.descends())
        {
            true => Totality::Total,
            false => Totality::Partial,
        },
        // The closure outgrew its bound; claim nothing.
        None => Totality::Partial,
    }
}

/// One member of a group, opened for analysis: the parameter binders the size order is measured against, and the body under them.
struct Member {
    params: Vec<Free>,
    body: Term,
}

impl Member {
    /// Peel the member's leading lambdas, minting one binder per parameter.
    ///
    /// A member with no lambda — `rec inf : F = F/more(inf)`, or any of `/std/Json/decode`'s nullary parsers — has an empty parameter vector, and its self-call therefore contributes a 0×0 matrix. That matrix is idempotent and has no diagonal to descend on, so a nullary self-call is rejected, which is exactly right: nothing about it can get smaller.
    fn of<E: Env>(env: &mut E, group: &RecGroup, index: usize) -> Self {
        let mut params = Vec::new();
        let mut body = group.member_body(index);

        while let Subterm::Func(Func { telescope, .. }) = &*body {
            let mut telescope = telescope.clone();
            loop {
                match telescope {
                    Telescope::Done(inner) => {
                        body = *inner;
                        break;
                    }
                    Telescope::Cons(_, rest) => {
                        let binder = env.fresh(rest.first_hint());
                        params.push(binder.clone());
                        telescope = rest.open(&[&Term::free_var(&binder)]);
                    }
                }
            }
        }

        Self { params, body }
    }
}

/// One member body's traversal: it finds the recursive calls and grades them.
struct Walk<'a, E: Env> {
    env: &'a mut E,
    group: &'a RecGroup,
    arities: &'a Vec<usize>,
    caller: usize,
    params: &'a [Free],
    /// What each binder has been refined to by the arms enclosing the position being walked. Entries are added on the way into an arm and removed on the way out, so a refinement never escapes the branch that established it.
    refined: BTreeMap<Free, Shape>,
    /// The binders an enclosing arm has established are not zero, entered and left exactly like `refined`.
    ///
    /// This is what makes an arithmetic decrease sound rather than merely plausible: `n / k` is below `n` only when `n` is nonzero, and without the guard `rec loop(n : Nat) -> Nat = loop(n / 10)` would be accepted while looping forever at zero.
    nonzero: BTreeSet<Free>,
    /// The nested groups whose bodies the walk is currently inside, entered and left exactly like `refined`. A group reached from within itself would regenerate its own bodies without end, since every member reference materializes as a projection carrying the whole group.
    entered: Vec<RecGroup>,
    /// The work still owed, innermost last. The traversal's depth lives here rather than on the native stack: a member body nests as deep as its source is written, and a program-generating spelling of it is unbounded by anything the walk controls.
    ops: Vec<Op>,
    /// One entry per scope an [`Op::Enter`] has opened and its [`Op::Exit`] has yet to close.
    scopes: Vec<Undo>,
    calls: Vec<(usize, usize, Matrix)>,
}

/// One unit of the walk's pending work — what a native frame used to hold.
///
/// **Arms expand lazily, and that is the correctness argument for the whole stack.** Every op that opens an arm — [`Op::BoolArm`] through [`Op::ElemCons`], and [`Op::RecBodies`], [`Op::OpenMany`], [`Op::TermsRest`], [`Op::UnitsRest`] beside them — does its own guard read, shape read, binder minting, or body materialization when it is *popped*, never when the node that owns it is expanded. Those are effects on the checker driving the analysis: [`Env::force`] spends a reduction budget and [`Env::fresh`] mints an identity, so the order the arms are reached in *is* the order those effects land in. Expanding a node's arms together would batch its reads ahead of the subtrees the recursive walk ran between them, and a differently-ordered spend against a nearly exhausted budget reads a different shape — a verdict change with no cause in the term. One arm per pop makes the two orders identical rather than merely similar.
enum Op {
    /// Read one term: the traversal's own match, scheduling children in place of recursing.
    Walk(Term),
    /// Open a scope, applying what an arm established and recording its exact [`Undo`].
    Enter {
        refine: Option<(Free, Shape)>,
        nonzero: Option<Free>,
        entered: Option<RecGroup>,
    },
    /// Close the innermost open scope.
    Exit,
    /// One arm of a boolean match. The scrutinee is re-read as a guard per arm, because what a guard establishes depends on which way it went.
    BoolArm {
        head: Term,
        scrutinee: Option<Free>,
        taken: bool,
        body: Term,
    },
    /// One enumerated arm of a `Nat` switch, refining the scrutinee to that arm's literal.
    SwitchArm {
        scrutinee: Option<Free>,
        value: u32,
        body: Term,
    },
    /// One arm of an inductive match: the scrutinee is that constructor applied to the arm's own payload binders.
    InductArm {
        scrutinee: Option<Free>,
        tag: Atom,
        body: Scope<Many>,
    },
    /// The cons arm of the `Nat` eliminator, binding the predecessor and the induction hypothesis.
    NatCons {
        scrutinee: Option<Free>,
        cons_case: Scope<Two>,
    },
    /// The cons arm of a `Bin`/`List` eliminator, binding the generator, the tail, and the induction hypothesis.
    ElemCons {
        scrutinee: Option<Free>,
        carriers: Carriers,
        cons_case: Scope<Three>,
    },
    /// A nested group's members from `index` on, one body materialized at a time — each carries the whole group, so materializing them together would hold every one of them at once.
    RecBodies { group: RecGroup, index: usize },
    /// A runtime-arity scope — a motive, a `let` tail, a `rec` tail — to open against fresh binders and walk.
    OpenMany(Scope<Many>),
    /// A `Func`/`FuncType` telescope from its next entry on.
    Terms(Telescope<Term>),
    /// What follows a `Func`/`FuncType` entry, still closed: its binder is minted only after the entry before it has been walked, which is where the recursive walk minted it.
    TermsRest(Scope<One, Telescope<Term>>),
    /// A `TupleType` telescope from its next entry on. It has no terminal to walk — a tuple type's payload is its fields.
    Units(Telescope<()>),
    /// What follows a `TupleType` entry, minted like [`Op::TermsRest`].
    UnitsRest(Scope<One, Telescope<()>>),
}

/// What one [`Op::Enter`] changed, so its [`Op::Exit`] puts back exactly that and nothing else.
///
/// Recorded on the way in rather than recomputed on the way out, because only the entering side can tell the difference that matters: a binder an outer arm had already refined, or already knew nonzero, must be left standing as that outer arm left it.
struct Undo {
    /// The refined binder and what it stood for before, `None` when the scope refined nothing.
    refined: Option<(Free, Option<Shape>)>,
    /// The binder this scope added to `nonzero`, `None` when it added none — an already-known binder included.
    nonzero: Option<Free>,
    /// Whether this scope pushed a group onto `entered`.
    entered: bool,
}

impl<E: Env> Walk<'_, E> {
    /// Record a call to member `callee` with `arguments`, grading each argument against each of the caller's parameters.
    fn call(&mut self, callee: usize, arguments: &[Term]) {
        let columns = self.arities[callee];
        let mut matrix = Matrix::unknown(self.params.len(), columns);

        let expanded = self
            .params
            .iter()
            .map(|param| self.expand(param, EXPAND_FUEL))
            .collect::<Vec<_>>();

        for (column, argument) in arguments.iter().enumerate().take(columns) {
            let shape = self.shape_of(argument);
            for (row, parameter) in expanded.iter().enumerate() {
                matrix.set(row, column, shape.against(parameter));
            }
        }

        self.calls.push((self.caller, callee, matrix));
    }

    /// The value a binder currently stands for, following the refinements the enclosing arms established.
    fn expand(&self, var: &Free, fuel: usize) -> Shape {
        if fuel == 0 {
            return Shape::Atom(var.clone());
        }
        match self.refined.get(var) {
            None => Shape::Atom(var.clone()),
            Some(shape) => self.expand_shape(shape, fuel - 1),
        }
    }

    fn expand_shape(&self, shape: &Shape, fuel: usize) -> Shape {
        match shape {
            Shape::Atom(var) => self.expand(var, fuel),
            // Already relative to a binder; expanding that binder would only lose the identity the claim is stated against.
            Shape::Smaller(below) => Shape::Smaller(below.clone()),
            Shape::Opaque => Shape::Opaque,
            Shape::Node(tag, kids) => Shape::Node(
                tag.clone(),
                kids.iter()
                    .map(|kid| self.expand_shape(kid, fuel))
                    .collect(),
            ),
            // Rebuilt through the constructors so a tail that expands into a run of the same carrier merges back into one canonical run.
            Shape::UnaryRun { count, tail } => {
                Shape::unary_run(count.clone(), self.expand_shape(tail, fuel))
            }
            Shape::ElemRun {
                carrier,
                heads,
                tail,
            } => Shape::elem_run(
                *carrier,
                heads
                    .iter()
                    .map(|head| self.expand_shape(head, fuel))
                    .collect(),
                self.expand_shape(tail, fuel),
            ),
        }
    }

    /// Read a term as a constructor tree.
    ///
    /// A constructor, a free-monoid layer, and a recognised arithmetic decrease read directly; everything else goes to [`Walk::unfolded_shape`], which sees through the definitions standing between a term and its shape. That fallback carries most of what the size order knows — an operator resolves a witness, so even `n - 1` reaches here as a projection — and it is not bounded by a step count; see [`readable`] for what bounds it instead.
    fn shape_of(&mut self, term: &Term) -> Shape {
        match &**term {
            Subterm::Var(var) => {
                if let Some(free) = var.as_free() {
                    return self.expand(free, EXPAND_FUEL);
                }
                Shape::Opaque
            }

            Subterm::Variant(Variant { tag, payload, .. }) => {
                let kids = payload
                    .iter()
                    .map(|argument| self.shape_of(argument))
                    .collect();
                Shape::Node(Tag::Variant(tag.clone()), kids)
            }

            Subterm::Struct(Struct { name, fields, .. }) => {
                let kids = fields.iter().map(|field| self.shape_of(field)).collect();
                Shape::Node(Tag::Struct(name.clone()), kids)
            }

            Subterm::Tuple(Tuple { fields, .. }) => {
                let kids = fields.iter().map(|field| self.shape_of(field)).collect();
                Shape::Node(Tag::Tuple, kids)
            }

            Subterm::Intrinsic(Intrinsic::Bool(value)) => {
                Shape::Node(Tag::Bool(*value), Vec::new())
            }

            Subterm::Intrinsic(Intrinsic::Nat(_)) => self.monoid_shape(FreeMonoid::Unary, term),

            Subterm::Intrinsic(
                Intrinsic::Bin(grain, _)
                | Intrinsic::BinAppend(grain, ..)
                | Intrinsic::BinConcat(grain, ..)
                | Intrinsic::BinSlice(grain, ..),
            ) => self.monoid_shape(FreeMonoid::Bin(*grain), term),

            Subterm::Intrinsic(
                Intrinsic::List(..)
                | Intrinsic::ListAppend(..)
                | Intrinsic::ListConcat(..)
                | Intrinsic::ListSlice(..),
            ) => self.monoid_shape(FreeMonoid::List, term),

            // Arithmetic descent. Both operations are monotone and floor-like on Core's unbounded `Nat` — `NatDiv` folds through `BigUint` division and `NatSub` truncates at zero — so each is below its left operand whenever that operand is nonzero.
            Subterm::Intrinsic(Intrinsic::NatDiv(left, right)) => {
                self.arithmetic_shape(left, right, &BigUint::from(2usize))
            }

            Subterm::Intrinsic(Intrinsic::NatSub(left, right)) => {
                self.arithmetic_shape(left, right, &BigUint::from(1usize))
            }

            _ => self.unfolded_shape(term),
        }
    }

    /// Read `left op right` as a decrease on the binder `left` stands for.
    ///
    /// `least` is the smallest literal right-hand operand that makes the operation strictly decreasing: `2` for division, because `n / 1` is `n`, and `1` for subtraction, because `n - 0` is `n`. A non-literal operand, an operand below `least`, or a left side that is neither the binder nor already a decrease on one, all read as unread — which is what this term read as before the rule existed.
    fn arithmetic_shape(&mut self, left: &Term, right: &Term, least: &BigUint) -> Shape {
        let Some(divisor) = right.as_nat().and_then(|nat| nat.to_big_uint()) else {
            return Shape::Opaque;
        };
        if divisor < *least {
            return Shape::Opaque;
        }
        match self.shape_of(left) {
            // `n` itself, and an arm has ruled out zero.
            Shape::Atom(atom) if self.nonzero.contains(&atom) => Shape::Smaller(atom),
            // Already below `below`, and these operations never grow: dividing or subtracting again keeps it below.
            Shape::Smaller(below) => Shape::Smaller(below),
            _ => Shape::Opaque,
        }
    }

    /// Decode a whole free-monoid prefix into one packed run over the shape of whatever stops the peel.
    ///
    /// The run mirrors the carrier's own representation instead of re-expanding it: a `Nat`'s successor count is read wholesale off the packed spine, and a `Bin`/`List` prefix is peeled breadth-wise into one head vector. One node per layer would recurse — in construction and in every later comparison — as deep as the literal is large, and a `Nat` literal's value is unbounded by the source that spelled it.
    fn monoid_shape(&mut self, carrier: FreeMonoid, term: &Term) -> Shape {
        let carriers = match carrier {
            FreeMonoid::Unary => {
                return match &**term {
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => {
                        Shape::Node(Tag::Empty(Carriers::Unary), Vec::new())
                    }
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(spine, inner))) => {
                        Shape::unary_run(spine.clone(), self.shape_of(inner))
                    }
                    _ => self.unfolded_shape(term),
                };
            }
            FreeMonoid::Bin(_) => Carriers::Bin,
            FreeMonoid::List => Carriers::List,
        };

        let mut heads = Vec::new();
        let mut rest = Term::unwrap_or_clone(term.clone());
        loop {
            match carrier.uncons(rest) {
                Layer::Empty => {
                    break Shape::elem_run(
                        carriers,
                        heads,
                        Shape::Node(Tag::Empty(carriers), Vec::new()),
                    );
                }
                Layer::Cons { head, tail } => {
                    if let Some(head) = head {
                        heads.push(self.shape_of(&head));
                    }
                    rest = Term::unwrap_or_clone(tail);
                }
                Layer::Stuck(stuck) => {
                    let stuck = Term::from(stuck);
                    let tail = match heads.is_empty() {
                        // Nothing peeled: the whole term is what stuck, and dispatching it back through `shape_of` would land right here again — force it instead.
                        true => self.unfolded_shape(&stuck),
                        // The remainder after a peeled prefix is an arbitrary term — a binder, another literal spelling, an application — and gets the full dispatch, exactly as the tail of every peeled layer did when the layers were nested nodes.
                        false => self.shape_of(&stuck),
                    };
                    break Shape::elem_run(carriers, heads, tail);
                }
            }
        }
    }

    /// Unfold weak-head steps until the term reads as a shape, or stops moving.
    ///
    /// Definitions stand between a term and its constructor shape, and no enumeration of *which* closes the set: measured over the corpus, 206 of 288 load-bearing unfoldings are witness projections (an operator resolves a witness, so `n - 1` arrives as `(w).0(n, 1)`), 11 are `/sys` intrinsic wrappers, and 65 are ordinary definitions like `/std/BigNat/mul/small` and `/syn/Str/step`. Unfolding is uniform over all of them because δ and β preserve meaning: a decrease visible after unfolding is a decrease in the term's value.
    ///
    /// There is no step count. Termination rests on what this pass is handed rather than on a budget: `check_rec_group` types every member body *before* asking for a verdict, positivity refuses a negative occurrence, and the universe hierarchy refuses `Type : Type` — so a well-typed rec-free term normalizes. [`readable`] keeps `rec` out, and the kernel's own reduction budget (`kernel::whnf`) remains the backstop for anything that still fails to settle.
    ///
    /// Removing the count changed no verdict in the corpus, and the reason is worth keeping: [`Env::force`] is a full weak-head normalization, so it walks an entire forwarder chain in one call and the count bounded *re-entries* here rather than unfoldings. Measured, 286 of 288 load-bearing unfoldings re-entered once and none more than twice, against a bound of three. What the removal buys is a stated condition in place of a number, not reach.
    fn unfolded_shape(&mut self, term: &Term) -> Shape {
        if !readable(term) {
            return Shape::Opaque;
        }
        let Ok(reduced) = self.env.force(term) else {
            return Shape::Opaque;
        };
        if reduced == *term {
            return Shape::Opaque;
        }
        self.shape_of(&reduced)
    }

    /// Read a boolean scrutinee as a comparison against a literal.
    ///
    /// Neither spelling arrives as an intrinsic: an operator (`n < 10`) resolves a witness and comes through as a projection out of it, and a named comparison (`Nat/lt(n, 10)`) stays an application of a one-line `/sys` wrapper. The same unfolding [`Walk::shape_of`] uses is what exposes the intrinsic under both.
    fn guard(&mut self, head: &Term) -> Option<Guard> {
        if let Some(guard) = Guard::read(head) {
            return Some(guard);
        }
        if !readable(head) {
            return None;
        }
        let reduced = self.env.force(head).ok()?;
        if reduced == *head {
            return None;
        }
        self.guard(&reduced)
    }

    /// Apply what one scope establishes, recording its [`Undo`].
    ///
    /// The three kinds of scoped knowledge open together because they close together: a boolean arm refines its scrutinee *and* rules out zero, and one bracket is one thing to keep balanced instead of three.
    fn enter(
        &mut self,
        refine: Option<(Free, Shape)>,
        nonzero: Option<Free>,
        entered: Option<RecGroup>,
    ) {
        let refined = refine.map(|(binder, shape)| {
            let previous = self.refined.insert(binder.clone(), shape);
            (binder, previous)
        });
        let nonzero = match nonzero {
            Some(atom) if self.nonzero.insert(atom.clone()) => Some(atom),
            _ => None,
        };
        let entered = match entered {
            Some(group) => {
                self.entered.push(group);
                true
            }
            None => false,
        };

        self.scopes.push(Undo {
            refined,
            nonzero,
            entered,
        });
    }

    /// Close the innermost open scope, putting back exactly what its [`Op::Enter`] recorded.
    fn exit(&mut self) {
        let undo = self
            .scopes
            .pop()
            .expect("every Exit is scheduled together with its own Enter");

        if let Some((binder, previous)) = undo.refined {
            match previous {
                Some(previous) => self.refined.insert(binder, previous),
                None => self.refined.remove(&binder),
            };
        }
        if let Some(atom) = undo.nonzero {
            self.nonzero.remove(&atom);
        }
        if undo.entered {
            self.entered
                .pop()
                .expect("an entered group is left by its own Exit");
        }
    }

    /// One fresh binder per position of a scope, carrying the written hints so a shape reads in the user's own names.
    fn binders<A: Arity>(&mut self, scope: &Scope<A>) -> Vec<Free> {
        (0..scope.arity())
            .map(|index| self.env.fresh(scope.hint(index)))
            .collect()
    }

    /// Open a runtime-arity scope — a motive, an inductive arm, a `let` tail, a `rec` tail — against fresh binders.
    ///
    /// The three openers here stay separate on purpose. `Scope<A>::open` takes `A::Params`, a *fixed-size* type per arity, which is what stops a two-binder arm from being opened at three terms; collapsing them would mean a slice-taking opener in `curios-core` that re-admits exactly that mistake, to save three lines apiece.
    fn open_many(&mut self, scope: &Scope<Many>) -> (Vec<Free>, Term) {
        let binders = self.binders(scope);
        let terms = binders.iter().map(Term::free_var).collect::<Vec<_>>();
        let refs = terms.iter().collect::<Vec<_>>();
        let body = scope.open(&refs);
        (binders, body)
    }

    /// Open the `(pred, ih)` arm of the `Nat` eliminator.
    fn open_two(&mut self, scope: &Scope<Two>) -> (Vec<Free>, Term) {
        let binders = self.binders(scope);
        let terms = binders.iter().map(Term::free_var).collect::<Vec<_>>();
        let body = scope.open(&[&terms[0], &terms[1]]);
        (binders, body)
    }

    /// Open the `(head, tail, ih)` arm of the `Bin`/`List` eliminators.
    fn open_three(&mut self, scope: &Scope<Three>) -> (Vec<Free>, Term) {
        let binders = self.binders(scope);
        let terms = binders.iter().map(Term::free_var).collect::<Vec<_>>();
        let body = scope.open(&[&terms[0], &terms[1], &terms[2]]);
        (binders, body)
    }

    /// Find the recursive calls in `body`, tracking the refinements that make their arguments comparable.
    ///
    /// The traversal is a loop over [`Op`]s rather than a recursion because a member body nests as deep as the source that spelled it, and one native frame per level makes a generated term an aborted process instead of a verdict. Scope discipline holds by construction: an arm is scheduled as `Enter`, its body, `Exit`, so no path can drop a refinement or carry one out.
    fn walk(&mut self, body: &Term) {
        self.ops.push(Op::Walk(body.clone()));

        while let Some(op) = self.ops.pop() {
            match op {
                Op::Walk(term) => self.step(&term),

                Op::Enter {
                    refine,
                    nonzero,
                    entered,
                } => self.enter(refine, nonzero, entered),

                Op::Exit => self.exit(),

                // A boolean arm carries no binder, but when the scrutinee compares a binder against a literal the arm still settles whether that binder can be zero — which is what an arithmetic decrease on it needs.
                Op::BoolArm {
                    head,
                    scrutinee,
                    taken,
                    body,
                } => {
                    let atom = self
                        .guard(&head)
                        .filter(|guard| guard.establishes_nonzero(taken))
                        .map(|guard| guard.atom);
                    let shape = Shape::Node(Tag::Bool(taken), Vec::new());
                    self.schedule(arm(refine(scrutinee, shape), atom, body));
                }

                Op::SwitchArm {
                    scrutinee,
                    value,
                    body,
                } => {
                    let literal = Term::intrinsic(Intrinsic::Nat(Nat::new(value)));
                    let shape = self.shape_of(&literal);
                    self.schedule(arm(refine(scrutinee, shape), None, body));
                }

                Op::InductArm {
                    scrutinee,
                    tag,
                    body,
                } => {
                    let (binders, body) = self.open_many(&body);
                    let shape = Shape::Node(
                        Tag::Variant(tag),
                        binders.iter().map(|b| Shape::Atom(b.clone())).collect(),
                    );
                    self.schedule(arm(refine(scrutinee, shape), None, body));
                }

                Op::NatCons {
                    scrutinee,
                    cons_case,
                } => {
                    let (binders, body) = self.open_two(&cons_case);
                    let shape =
                        Shape::unary_run(BigUint::from(1usize), Shape::Atom(binders[0].clone()));
                    self.schedule(arm(refine(scrutinee, shape), None, body));
                }

                Op::ElemCons {
                    scrutinee,
                    carriers,
                    cons_case,
                } => {
                    let (binders, body) = self.open_three(&cons_case);
                    let shape = Shape::elem_run(
                        carriers,
                        vec![Shape::Atom(binders[0].clone())],
                        Shape::Atom(binders[1].clone()),
                    );
                    self.schedule(arm(refine(scrutinee, shape), None, body));
                }

                Op::RecBodies { group, index } => {
                    if index < group.length() {
                        let body = group.member_body(index);
                        self.schedule([
                            Op::Walk(body),
                            Op::RecBodies {
                                group,
                                index: index + 1,
                            },
                        ]);
                    }
                }

                Op::OpenMany(scope) => {
                    let (_, body) = self.open_many(&scope);
                    self.ops.push(Op::Walk(body));
                }

                Op::Terms(telescope) => match telescope {
                    Telescope::Done(terminal) => self.ops.push(Op::Walk(*terminal)),
                    Telescope::Cons(entry, rest) => {
                        self.schedule([Op::Walk(entry), Op::TermsRest(rest)])
                    }
                },

                Op::TermsRest(rest) => {
                    let binder = self.env.fresh(rest.first_hint());
                    self.ops
                        .push(Op::Terms(rest.open(&[&Term::free_var(&binder)])));
                }

                Op::Units(telescope) => {
                    if let Telescope::Cons(entry, rest) = telescope {
                        self.schedule([Op::Walk(entry), Op::UnitsRest(rest)]);
                    }
                }

                Op::UnitsRest(rest) => {
                    let binder = self.env.fresh(rest.first_hint());
                    self.ops
                        .push(Op::Units(rest.open(&[&Term::free_var(&binder)])));
                }
            }
        }

        assert!(
            self.scopes.is_empty(),
            "the walk left a scope open: {} unmatched Enter(s)",
            self.scopes.len()
        );
    }

    /// Schedule `ops` to run in the order written — the stack is LIFO, and every expansion below reads in execution order.
    fn schedule(&mut self, ops: impl IntoIterator<Item = Op, IntoIter: DoubleEndedIterator>) {
        self.ops.extend(ops.into_iter().rev());
    }

    /// Schedule a walk of each of `terms`, in order.
    fn walks(&mut self, terms: impl IntoIterator<Item = Term, IntoIter: DoubleEndedIterator>) {
        self.ops.extend(terms.into_iter().rev().map(Op::Walk));
    }

    /// Read one term, scheduling whatever it contains.
    fn step(&mut self, term: &Term) {
        match &**term {
            // Nothing here can contain a call.
            Subterm::Type(_) | Subterm::Prop | Subterm::Var(_) | Subterm::Metavar(_) => {}

            // A member reference at the head of a spine is a call with those arguments; anywhere else it is a call the analysis cannot grade, which an all-unknown matrix records faithfully. The guard names *this* group alone: a projection of any other group is that group's only appearance in the term, so it falls through to the general `rec` arm and its bodies are walked like any nested group's.
            Subterm::Rec(_)
                if term
                    .as_rec_proj()
                    .is_some_and(|(group, _)| group == self.group) =>
            {
                let (_, index) = term.as_rec_proj().expect("a projection");
                self.call(index, &[]);
            }

            Subterm::Apply(Apply { head, params, .. }) => {
                let (spine_head, arguments) = flatten(term);
                if let Some((group, index)) = spine_head.as_rec_proj()
                    && group == self.group
                {
                    self.call(index, &arguments);
                    self.walks(arguments);
                    return;
                }
                self.walks(once(head.clone()).chain(params.iter().cloned()));
            }

            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => {
                let mut ops = vec![Op::Walk(head.clone()), Op::OpenMany(motive.clone())];
                arms(head, cases, &mut ops);
                self.schedule(ops);
            }

            Subterm::Func(Func { telescope, .. })
            | Subterm::FuncType(FuncType { telescope, .. }) => {
                self.ops.push(Op::Terms(telescope.clone()))
            }

            Subterm::TupleType(TupleType { telescope }) => {
                self.ops.push(Op::Units(telescope.clone()));
            }

            Subterm::Tuple(Tuple { fields, .. }) => self.walks(fields.iter().cloned()),

            Subterm::Proj(Proj { head, .. }) | Subterm::UniverseInst(UniverseInst { head, .. }) => {
                self.ops.push(Op::Walk(head.clone()))
            }

            Subterm::InductType(InductType {
                params, indices, ..
            }) => self.walks(params.iter().chain(indices).cloned()),

            Subterm::Variant(Variant {
                params, payload, ..
            }) => self.walks(params.iter().chain(payload).cloned()),

            Subterm::StructType(StructType { params, .. }) => self.walks(params.iter().cloned()),

            Subterm::Struct(Struct { params, fields, .. }) => {
                self.walks(params.iter().chain(fields).cloned())
            }

            // The early-mention net walks *written* (lowered, pre-elaboration) terms, so transients are legitimate here; their children may name definitions.
            Subterm::Transient(transient) => {
                self.walks(transient.subterms().cloned().collect::<Vec<_>>())
            }

            Subterm::Intrinsic(intrinsic) => {
                let mut terms = Vec::new();
                intrinsic.any_term(&mut |child| {
                    terms.push(child.clone());
                    false
                });
                self.walks(terms);
            }

            Subterm::Foreign(_, args) => self.walks(args.iter().cloned()),

            Subterm::Let(Let { bindings, tail }) => {
                let mut ops = Vec::new();
                for binding in bindings {
                    ops.push(Op::Walk(binding.type_().clone()));
                    ops.push(Op::Walk(binding.value().clone()));
                }
                ops.push(Op::OpenMany(tail.clone()));
                self.schedule(ops);
            }

            // An inner group is classified on its own, but its bodies may still call *this* group, and such a call is a real edge of this group's call graph.
            //
            // `entered` is what keeps the descent finite. `RecGroup::member_body` materializes every member reference as a projection carrying the whole group, so a group reached from inside its own bodies would regenerate them without end — which is why a projection of *this* group is answered above rather than descended into, and why any other group is walked at most once per path. It is entered and left exactly like `refined`, so a group met twice in sibling positions is still walked under each one's refinements.
            Subterm::Rec(Rec { group, tail }) => {
                let mut ops = Vec::new();
                if !self.entered.contains(group) {
                    ops.extend([
                        Op::Enter {
                            refine: None,
                            nonzero: None,
                            entered: Some(group.clone()),
                        },
                        Op::RecBodies {
                            group: group.clone(),
                            index: 0,
                        },
                        Op::Exit,
                    ]);
                }
                ops.push(Op::OpenMany(tail.clone()));
                self.schedule(ops);
            }
        }
    }
}

/// Append the ops of a match's arms, in the order the arms are walked.
///
/// Nothing here touches the checker: an arm that has a guard to read, a shape to read, or binders to mint waits for its own op to be popped. See [`Op`] for why the wait is what makes this walk the recursive one it replaces.
fn arms(head: &Term, cases: &Cases, ops: &mut Vec<Op>) {
    let scrutinee = match &**head {
        Subterm::Var(var) => var.as_free().cloned(),
        _ => None,
    };

    match cases {
        Cases::Bool {
            false_case,
            true_case,
        } => {
            for (taken, body) in [(false, false_case), (true, true_case)] {
                ops.push(Op::BoolArm {
                    head: head.clone(),
                    scrutinee: scrutinee.clone(),
                    taken,
                    body: body.clone(),
                });
            }
        }

        Cases::Switch { cases, default } => {
            for (value, body) in cases {
                ops.push(Op::SwitchArm {
                    scrutinee: scrutinee.clone(),
                    value: *value,
                    body: body.clone(),
                });
            }
            // The default arm stands for every value *not* enumerated, so it refines the scrutinee to nothing — but enumerating zero is exactly what rules zero out everywhere else.
            let atom = scrutinee.filter(|_| cases.contains_key(&0));
            ops.extend(arm(None, atom, default.clone()));
        }

        Cases::Induct { cases, default } => {
            for (tag, case) in cases {
                ops.push(Op::InductArm {
                    scrutinee: scrutinee.clone(),
                    tag: tag.clone(),
                    body: case.body.clone(),
                });
            }
            if let Some(default) = default {
                ops.push(Op::Walk(default.clone()));
            }
        }

        // The cons arm binds the generator, the tail, and the induction hypothesis; the scrutinee is the generator consed onto the tail, and the hypothesis is not part of the value's shape.
        Cases::FreeMonoid { carrier } => match carrier {
            Carrier::Nat {
                empty_case,
                cons_case,
            } => {
                ops.extend(empty_arm(&scrutinee, Carriers::Unary, empty_case));
                ops.push(Op::NatCons {
                    scrutinee,
                    cons_case: cons_case.clone(),
                });
            }
            Carrier::Bin {
                empty_case,
                cons_case,
                ..
            } => {
                ops.extend(empty_arm(&scrutinee, Carriers::Bin, empty_case));
                ops.push(Op::ElemCons {
                    scrutinee,
                    carriers: Carriers::Bin,
                    cons_case: cons_case.clone(),
                });
            }
            Carrier::List {
                elem,
                empty_case,
                cons_case,
            } => {
                ops.push(Op::Walk(elem.clone()));
                ops.extend(empty_arm(&scrutinee, Carriers::List, empty_case));
                ops.push(Op::ElemCons {
                    scrutinee,
                    carriers: Carriers::List,
                    cons_case: cons_case.clone(),
                });
            }
        },
    }
}

/// The three ops an arm is: take on what the arm establishes, walk its body, put it back. Written once so no arm can be scheduled without its own [`Op::Exit`].
fn arm(refine: Option<(Free, Shape)>, nonzero: Option<Free>, body: Term) -> [Op; 3] {
    [
        Op::Enter {
            refine,
            nonzero,
            entered: None,
        },
        Op::Walk(body),
        Op::Exit,
    ]
}

/// The identity arm of a free-monoid eliminator, refining the scrutinee to that carrier's empty value — a shape stated without opening anything, so this arm needs no op of its own.
fn empty_arm(scrutinee: &Option<Free>, carriers: Carriers, empty_case: &Term) -> [Op; 3] {
    let shape = Shape::Node(Tag::Empty(carriers), Vec::new());
    arm(refine(scrutinee.clone(), shape), None, empty_case.clone())
}

/// What an arm refines, for a scrutinee that may not be a binder at all — a match on anything else establishes nothing to remember.
fn refine(scrutinee: Option<Free>, shape: Shape) -> Option<(Free, Shape)> {
    scrutinee.map(|scrutinee| (scrutinee, shape))
}

/// An application spine as its head and its arguments in order, so an over-applied or curried call is graded as the one call it is.
fn flatten(term: &Term) -> (Term, Vec<Term>) {
    let mut arguments = Vec::new();
    let mut head = term.clone();
    loop {
        match &*head.clone() {
            Subterm::Apply(Apply {
                head: inner,
                params,
                ..
            }) => {
                let mut prefix = params.clone();
                prefix.extend(arguments);
                arguments = prefix;
                head = inner.clone();
            }
            Subterm::UniverseInst(UniverseInst { head: inner, .. }) => head = inner.clone(),
            _ => return (head, arguments),
        }
    }
}

/// Whether a sort is extractable from this type: reachable by peeling arrows to the codomain, or by projecting a tuple component. A sort in a *parameter* is not extractable — `(A : Type) -> A` denotes a value, not a type.
///
/// **Reduced at every step rather than read.** A declared type is a term like any other, so `U` — a definition whose value is `Type` — yields a sort and says so only once forced, and so does `(n : Nat) -> U`. Reading the spelling let a member escape the descent gate by being declared through an alias, and the gate is what stops a member erasure deletes from being assumed at its own type before its body is checked against it. An alias is not a different claim.
///
/// Each binder is opened over a fresh identity before the walk descends past it, because a scope's body carries loose indices and reducing one would be reducing a term that is not there. Nothing is assumed at those binders: reduction meets an unknown name as a stuck neutral, which is all this needs.
///
/// A reduction that fails answers **yes**. The gate then demands descent, which is the refusing direction, and a declared type that cannot be reduced is not one to take on trust.
pub fn yields_a_sort<E: Env>(env: &mut E, type_: &Term) -> bool {
    // Peeling to a codomain answers the same question about a smaller type, so it iterates here rather than re-entering: an arrow's spine is as long as its type is written, and the answer is the codomain's own.
    let mut type_ = type_.clone();

    loop {
        let Ok(reduced) = env.force(&type_) else {
            return true;
        };

        match &*reduced {
            Subterm::Type(_) | Subterm::Prop => return true,
            Subterm::FuncType(FuncType { telescope, .. }) => {
                let mut telescope = telescope.clone();
                type_ = loop {
                    match telescope {
                        Telescope::Done(body) => break *body,
                        Telescope::Cons(_, rest) => {
                            let binder = env.fresh(rest.first_hint());
                            telescope = rest.open(&[&Term::free_var(&binder)]);
                        }
                    }
                };
            }
            Subterm::TupleType(tuple) => {
                let mut telescope = tuple.telescope.clone();
                return loop {
                    match telescope {
                        Telescope::Done(_) => break false,
                        Telescope::Cons(entry, rest) => {
                            if yields_a_sort(env, &entry) {
                                break true;
                            }

                            let binder = env.fresh(rest.first_hint());
                            telescope = rest.open(&[&Term::free_var(&binder)]);
                        }
                    }
                };
            }
            _ => return false,
        }
    }
}
