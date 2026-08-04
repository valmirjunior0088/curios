//! Size-change termination: which `rec` groups descend.
//!
//! Curios keeps general recursion. What it cannot keep is general recursion in the places erasure *removes*, because a divergent type breaks type formation and a divergent proof proves anything, while a program that loops is only a program that loops. This module supplies the fact both checkers need: whether a recursive group terminates on every call path.
//!
//! A group is accepted by the size-change principle (Lee–Jones–Ben-Amram, in the shape Abel's `foetus` gave it for dependent types). Each recursive call contributes a **call matrix** grading every callee argument against every caller parameter as strictly smaller, equal, or unrelated. The matrices are closed transitively under composition, and the group is accepted when every **idempotent** matrix in that closure carries a strict decrease on its diagonal — an idempotent matrix describes a call path that can repeat forever, so a decrease on it is a decrease that cannot be sustained.
//!
//! Size-change rather than structural recursion because the corpus needs it. `/std/BigNat/add/raw` descends on *either* of two `Bits` arguments depending on the arm, `add/raw_assoc` does the same over three, and `add/raw_comm` needs the mutual closure across two members. A rule keyed to one designated argument rejects all of them, and a fold cannot express them either, because a fold cannot short-circuit.
//!
//! **Match refinement is part of the size order, not an optimization.** A parameter scrutinized by an enclosing arm is expanded through that arm's constructor before it is compared, so in `add/raw`'s empty-`x` arm the literal argument `b\` grades *equal* to `x` rather than unknown. Without that, the two nil-arm matrices compose to an all-unknown matrix, which is idempotent with no decrease anywhere on its diagonal, and `add/raw` is rejected on a call path that cannot actually occur.
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
        Apply, Arity, Atom, Carrier, Cases, Free, FreeMonoid, Func, FuncType, InductArm,
        InductType, Infix, Layer, Let, Many, Match, Nat, Prim, Proj, Rec, RecGroup, Scope, Struct,
        StructType, Subterm, Telescope, Term, Three, Totality, Tuple, TupleType, Two, UniverseInst,
        Variant,
    },
    num_bigint::BigUint,
    std::collections::{BTreeMap, BTreeSet},
};

/// How many times shape reading may unfold a definition before giving up.
///
/// One unfold is what `/std/Bits/cons` needs: it is an ordinary `let`, not a constructor, so `cons(a2, b2)` only exposes its free-monoid layer once the call is unfolded. Two more are slack for a wrapper over a wrapper. The bound exists so a shape read can never become an unbounded reduction.
const UNFOLD_FUEL: usize = 3;

/// How deep a refinement chain may be expanded.
///
/// Each nested `match` on a binder introduced by an outer arm adds one level — `raw_trimmed` reaches three — so this is generous. It exists only so a pathological or cyclic refinement map cannot loop.
const EXPAND_FUEL: usize = 16;

/// Whether every recursive call path in `group` descends.
///
/// This is the whole of size-change termination as this compiler applies it: collect one matrix per call site, close them under composition, and demand a decrease on the diagonal of every idempotent result. A group with no recursive call at all closes to nothing and is accepted, which is how the prelude's call-free `; ih` folds pass — their recursion is the primitive eliminator's, already structural by construction.
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
    calls: Vec<(usize, usize, Matrix)>,
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
            let shape = self.shape_of(argument, UNFOLD_FUEL);
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
        }
    }

    /// Read a term as a constructor tree.
    ///
    /// Definitions are unfolded up to [`UNFOLD_FUEL`] times, because the corpus builds free-monoid conses through ordinary functions rather than constructors: `/std/Bits/cons` is a `let`, so `cons(a2, b2)` is an application, and `add/raw_trimmed`'s descent is invisible until it is unfolded once. Unfolding is weak-head only and bounded, so it cannot become a reduction; a reduction that fails or exhausts the budget leaves the term unread rather than failing the compile.
    fn shape_of(&mut self, term: &Term, fuel: usize) -> Shape {
        match &**term {
            Subterm::Var(var) => {
                if let Some(free) = var.as_free() {
                    return self.expand(free, EXPAND_FUEL);
                }
                Shape::Opaque
            }

            Subterm::Variant(Variant { tag, payload, .. }) => {
                let payload = payload.clone();
                let kids = payload
                    .iter()
                    .map(|argument| self.shape_of(argument, fuel))
                    .collect();
                Shape::Node(Tag::Variant(tag.clone()), kids)
            }

            Subterm::Struct(Struct { name, fields, .. }) => {
                let (name, fields) = (name.clone(), fields.clone());
                let kids = fields
                    .iter()
                    .map(|field| self.shape_of(field, fuel))
                    .collect();
                Shape::Node(Tag::Struct(name), kids)
            }

            Subterm::Tuple(Tuple { fields, .. }) => {
                let fields = fields.clone();
                let kids = fields
                    .iter()
                    .map(|field| self.shape_of(field, fuel))
                    .collect();
                Shape::Node(Tag::Tuple, kids)
            }

            Subterm::Prim(Prim::Bool(value)) => Shape::Node(Tag::Bool(*value), Vec::new()),

            Subterm::Prim(Prim::Nat(_)) => self.monoid_shape(FreeMonoid::Unary, term, fuel),

            Subterm::Prim(
                Prim::Bin(grain, _)
                | Prim::BinAppend(grain, ..)
                | Prim::BinConcat(grain, ..)
                | Prim::BinSlice(grain, ..),
            ) => self.monoid_shape(FreeMonoid::Bin(*grain), term, fuel),

            Subterm::Prim(
                Prim::Lst(..) | Prim::LstAppend(..) | Prim::LstConcat(..) | Prim::LstSlice(..),
            ) => self.monoid_shape(FreeMonoid::Lst, term, fuel),

            // Arithmetic descent. Both operations are monotone and floor-like on Core's unbounded `Nat` — `NatDiv` folds through `BigUint` division and `NatSub` truncates at zero — so each is below its left operand whenever that operand is nonzero.
            Subterm::Prim(Prim::NatDiv(left, right)) => {
                let (left, right) = (left.clone(), right.clone());
                self.arithmetic_shape(&left, &right, &BigUint::from(2usize), fuel)
            }

            Subterm::Prim(Prim::NatSub(left, right)) => {
                let (left, right) = (left.clone(), right.clone());
                self.arithmetic_shape(&left, &right, &BigUint::from(1usize), fuel)
            }

            _ => self.unfolded_shape(term, fuel),
        }
    }

    /// Read `left op right` as a decrease on the binder `left` stands for.
    ///
    /// `least` is the smallest literal right-hand operand that makes the operation strictly decreasing: `2` for division, because `n / 1` is `n`, and `1` for subtraction, because `n - 0` is `n`. A non-literal operand, an operand below `least`, or a left side that is neither the binder nor already a decrease on one, all read as unread — which is what this term read as before the rule existed.
    fn arithmetic_shape(
        &mut self,
        left: &Term,
        right: &Term,
        least: &BigUint,
        fuel: usize,
    ) -> Shape {
        let Some(divisor) = right.as_nat().and_then(|nat| nat.to_big_uint()) else {
            return Shape::Opaque;
        };
        if divisor < *least {
            return Shape::Opaque;
        }
        match self.shape_of(left, fuel) {
            // `n` itself, and an arm has ruled out zero.
            Shape::Atom(atom) if self.nonzero.contains(&atom) => Shape::Smaller(atom),
            // Already below `below`, and these operations never grow: dividing or subtracting again keeps it below.
            Shape::Smaller(below) => Shape::Smaller(below),
            _ => Shape::Opaque,
        }
    }

    /// Decode one free-monoid layer, and the tail beneath it.
    fn monoid_shape(&mut self, carrier: FreeMonoid, term: &Term, fuel: usize) -> Shape {
        let carriers = match carrier {
            FreeMonoid::Unary => Carriers::Unary,
            FreeMonoid::Bin(_) => Carriers::Bin,
            FreeMonoid::Lst => Carriers::Lst,
        };
        match carrier.uncons(Term::unwrap_or_clone(term.clone())) {
            Layer::Empty => Shape::Node(Tag::Empty(carriers), Vec::new()),
            Layer::Cons { head, tail } => {
                // `Nat`'s generator carries no payload, so its cons node has exactly one child and `pred + 1` is one level over `pred`.
                let mut kids = Vec::new();
                if let Some(head) = head {
                    kids.push(self.shape_of(&head, fuel));
                }
                kids.push(self.shape_of(&tail, fuel));
                Shape::Node(Tag::Cons(carriers), kids)
            }
            Layer::Stuck(_) => self.unfolded_shape(term, fuel),
        }
    }

    /// Unfold one weak-head step and re-read, or give up.
    fn unfolded_shape(&mut self, term: &Term, fuel: usize) -> Shape {
        if fuel == 0 || !forceable(term) {
            return Shape::Opaque;
        }
        let Ok(reduced) = self.env.force(term) else {
            return Shape::Opaque;
        };
        if reduced == *term {
            return Shape::Opaque;
        }
        self.shape_of(&reduced, fuel - 1)
    }

    /// Read a boolean scrutinee as a comparison against a literal.
    ///
    /// The operator spellings (`n < 10`) elaborate straight to a primitive, but the named ones (`Nat/lt(n, 10)`) stay applications of a one-line `/sys` wrapper, so the same bounded weak-head unfolding [`Walk::shape_of`] uses is what makes both readable.
    fn guard(&mut self, head: &Term, fuel: usize) -> Option<Guard> {
        if let Some(guard) = Guard::read(head) {
            return Some(guard);
        }
        if fuel == 0 || !forceable(head) {
            return None;
        }
        let reduced = self.env.force(head).ok()?;
        if reduced == *head {
            return None;
        }
        self.guard(&reduced, fuel - 1)
    }

    /// Walk `body` with `atom` additionally known nonzero, restoring the previous knowledge afterwards.
    ///
    /// Entered and left around exactly the same walk as the refinement, so an arm's arithmetic fact has precisely the arm's extent.
    fn under_nonzero(
        &mut self,
        atom: Option<Free>,
        scrutinee: Option<&Free>,
        shape: Shape,
        body: &Term,
    ) {
        let added = match &atom {
            Some(atom) => self.nonzero.insert(atom.clone()),
            None => false,
        };
        self.refine(scrutinee, shape, body);
        if added && let Some(atom) = &atom {
            self.nonzero.remove(atom);
        }
    }

    /// Walk an arm with `scrutinee` refined to `shape`, restoring the previous refinement afterwards.
    fn refine(&mut self, scrutinee: Option<&Free>, shape: Shape, body: &Term) {
        let Some(scrutinee) = scrutinee else {
            self.walk(body);
            return;
        };
        let previous = self.refined.insert(scrutinee.clone(), shape);
        self.walk(body);
        match previous {
            Some(previous) => self.refined.insert(scrutinee.clone(), previous),
            None => self.refined.remove(scrutinee),
        };
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

    /// Open the `(head, tail, ih)` arm of the `Bin`/`Lst` eliminators.
    fn open_three(&mut self, scope: &Scope<Three>) -> (Vec<Free>, Term) {
        let binders = self.binders(scope);
        let terms = binders.iter().map(Term::free_var).collect::<Vec<_>>();
        let body = scope.open(&[&terms[0], &terms[1], &terms[2]]);
        (binders, body)
    }

    /// Find the recursive calls in `term`, tracking the refinements that make their arguments comparable.
    fn walk(&mut self, term: &Term) {
        match &**term {
            // Nothing here can contain a call.
            Subterm::Type(_)
            | Subterm::Prop
            | Subterm::NumLit(_)
            | Subterm::Var(_)
            | Subterm::Metavar(_) => {}

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
                    for argument in &arguments {
                        self.walk(argument);
                    }
                    return;
                }
                self.walk(head);
                for param in params {
                    self.walk(param);
                }
            }

            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => {
                self.walk(head);
                let (_, motive_body) = self.open_many(motive);
                self.walk(&motive_body);
                self.arms(head, cases);
            }

            Subterm::Func(Func { telescope, .. })
            | Subterm::FuncType(FuncType { telescope, .. }) => self.telescope_terms(telescope),

            Subterm::TupleType(TupleType { telescope }) => {
                self.telescope_units(telescope);
            }

            Subterm::Tuple(Tuple { fields, .. }) => {
                for field in fields {
                    self.walk(field);
                }
            }

            Subterm::Proj(Proj { head, .. }) | Subterm::UniverseInst(UniverseInst { head, .. }) => {
                self.walk(head)
            }

            Subterm::InductType(InductType {
                params, indices, ..
            }) => {
                for term in params.iter().chain(indices) {
                    self.walk(term);
                }
            }

            Subterm::Variant(Variant {
                params, payload, ..
            }) => {
                for term in params.iter().chain(payload) {
                    self.walk(term);
                }
            }

            Subterm::StructType(StructType { params, .. }) => {
                for param in params {
                    self.walk(param);
                }
            }

            Subterm::Struct(Struct { params, fields, .. }) => {
                for term in params.iter().chain(fields) {
                    self.walk(term);
                }
            }

            Subterm::Infix(Infix { left, right, .. }) => {
                self.walk(left);
                self.walk(right);
            }

            Subterm::Prim(prim) => {
                let mut terms = Vec::new();
                prim.any_term(&mut |child| {
                    terms.push(child.clone());
                    false
                });
                for child in terms {
                    self.walk(&child);
                }
            }

            Subterm::Let(Let { bindings, tail }) => {
                for binding in bindings {
                    self.walk(binding.type_());
                    self.walk(binding.value());
                }
                let (_, body) = self.open_many(tail);
                self.walk(&body);
            }

            // An inner group is classified on its own, but its bodies may still call *this* group, and such a call is a real edge of this group's call graph.
            //
            // `entered` is what keeps the descent finite. `RecGroup::member_body` materializes every member reference as a projection carrying the whole group, so a group reached from inside its own bodies would regenerate them without end — which is why a projection of *this* group is answered above rather than descended into, and why any other group is walked at most once per path. It is entered and left exactly like `refined`, so a group met twice in sibling positions is still walked under each one's refinements.
            Subterm::Rec(Rec { group, tail }) => {
                if !self.entered.contains(group) {
                    self.entered.push(group.clone());
                    for index in 0..group.length() {
                        let body = group.member_body(index);
                        self.walk(&body);
                    }
                    self.entered.pop();
                }
                let (_, body) = self.open_many(tail);
                self.walk(&body);
            }
        }
    }

    /// Walk each arm under the refinement its constructor establishes.
    fn arms(&mut self, head: &Term, cases: &Cases) {
        let scrutinee = match &**head {
            Subterm::Var(var) => var.as_free().cloned(),
            _ => None,
        };
        let scrutinee = scrutinee.as_ref();

        match cases {
            Cases::Bool {
                false_case,
                true_case,
            } => {
                // A boolean arm carries no binder, but when the scrutinee compares a binder against a literal the arm still settles whether that binder can be zero — which is what an arithmetic decrease on it needs.
                for (taken, body) in [(false, false_case), (true, true_case)] {
                    let atom = self
                        .guard(head, UNFOLD_FUEL)
                        .filter(|guard| guard.establishes_nonzero(taken))
                        .map(|guard| guard.atom);
                    let shape = Shape::Node(Tag::Bool(taken), Vec::new());
                    self.under_nonzero(atom, scrutinee, shape, body);
                }
            }

            Cases::Switch { cases, default } => {
                for (value, body) in cases {
                    let literal = Term::prim(Prim::Nat(Nat::new(*value)));
                    let shape = self.shape_of(&literal, UNFOLD_FUEL);
                    self.refine(scrutinee, shape, body);
                }
                // The default arm stands for every value *not* enumerated, so it refines the scrutinee to nothing — but enumerating zero is exactly what rules zero out everywhere else.
                let atom = scrutinee.filter(|_| cases.contains_key(&0)).cloned();
                let added = match &atom {
                    Some(atom) => self.nonzero.insert(atom.clone()),
                    None => false,
                };
                self.walk(default);
                if added && let Some(atom) = &atom {
                    self.nonzero.remove(atom);
                }
            }

            Cases::Induct { cases, default } => {
                for (tag, arm) in cases {
                    self.induct_arm(scrutinee, tag, arm);
                }
                if let Some(default) = default {
                    self.walk(default);
                }
            }

            Cases::FreeMonoid { carrier } => self.monoid_arms(scrutinee, carrier),
        }
    }

    /// One `Cases::Induct` arm: the scrutinee is that constructor applied to the arm's own payload binders.
    fn induct_arm(&mut self, scrutinee: Option<&Free>, tag: &Atom, arm: &InductArm) {
        let (binders, body) = self.open_many(&arm.body);
        let shape = Shape::Node(
            Tag::Variant(tag.clone()),
            binders.iter().map(|b| Shape::Atom(b.clone())).collect(),
        );
        self.refine(scrutinee, shape, &body);
    }

    /// The two arms of a free-monoid eliminator. The cons arm binds the generator, the tail, and the induction hypothesis; the scrutinee is the generator consed onto the tail, and the hypothesis is not part of the value's shape.
    fn monoid_arms(&mut self, scrutinee: Option<&Free>, carrier: &Carrier) {
        match carrier {
            Carrier::Nat {
                empty_case,
                cons_case,
            } => {
                self.refine(
                    scrutinee,
                    Shape::Node(Tag::Empty(Carriers::Unary), Vec::new()),
                    empty_case,
                );
                let (binders, body) = self.open_two(cons_case);
                let shape = Shape::Node(
                    Tag::Cons(Carriers::Unary),
                    vec![Shape::Atom(binders[0].clone())],
                );
                self.refine(scrutinee, shape, &body);
            }
            Carrier::Bin {
                empty_case,
                cons_case,
                ..
            } => self.monoid_cons(scrutinee, Carriers::Bin, empty_case, cons_case),
            Carrier::Lst {
                elem,
                empty_case,
                cons_case,
            } => {
                self.walk(elem);
                self.monoid_cons(scrutinee, Carriers::Lst, empty_case, cons_case)
            }
        }
    }

    fn monoid_cons(
        &mut self,
        scrutinee: Option<&Free>,
        carriers: Carriers,
        empty_case: &Term,
        cons_case: &Scope<Three>,
    ) {
        self.refine(
            scrutinee,
            Shape::Node(Tag::Empty(carriers), Vec::new()),
            empty_case,
        );
        let (binders, body) = self.open_three(cons_case);
        let shape = Shape::Node(
            Tag::Cons(carriers),
            vec![
                Shape::Atom(binders[0].clone()),
                Shape::Atom(binders[1].clone()),
            ],
        );
        self.refine(scrutinee, shape, &body);
    }

    fn telescope_terms(&mut self, telescope: &Telescope<Term>) {
        let mut telescope = telescope.clone();
        loop {
            match telescope {
                Telescope::Done(terminal) => {
                    self.walk(&terminal);
                    return;
                }
                Telescope::Cons(entry, rest) => {
                    self.walk(&entry);
                    let binder = self.env.fresh(rest.first_hint());
                    telescope = rest.open(&[&Term::free_var(&binder)]);
                }
            }
        }
    }

    fn telescope_units(&mut self, telescope: &Telescope<()>) {
        let mut telescope = telescope.clone();
        while let Telescope::Cons(entry, rest) = telescope {
            self.walk(&entry);
            let binder = self.env.fresh(rest.first_hint());
            telescope = rest.open(&[&Term::free_var(&binder)]);
        }
    }
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
    let Ok(reduced) = env.force(type_) else {
        return true;
    };

    match &*reduced {
        Subterm::Type(_) | Subterm::Prop => true,
        Subterm::FuncType(FuncType { telescope, .. }) => {
            let mut telescope = telescope.clone();
            loop {
                match telescope {
                    Telescope::Done(body) => break yields_a_sort(env, &body),
                    Telescope::Cons(_, rest) => {
                        let binder = env.fresh(rest.first_hint());
                        telescope = rest.open(&[&Term::free_var(&binder)]);
                    }
                }
            }
        }
        Subterm::TupleType(tuple) => {
            let mut telescope = tuple.telescope.clone();
            loop {
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
            }
        }
        _ => false,
    }
}
