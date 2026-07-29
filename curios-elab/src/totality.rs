//! Size-change termination: which `rec` groups descend, and therefore which
//! definitions erasure may safely delete.
//!
//! Curios keeps general recursion. What it cannot keep is general recursion in
//! the places erasure *removes*, because a divergent type breaks type formation
//! and a divergent proof proves anything, while a program that loops is only a
//! program that loops. This module supplies the fact both gates need: whether
//! a recursive group terminates on every call path.
//!
//! A group is accepted by the size-change principle (Lee–Jones–Ben-Amram, in
//! the shape Abel's `foetus` gave it for dependent types). Each recursive call
//! contributes a **call matrix** grading every callee argument against every
//! caller parameter as strictly smaller, equal, or unrelated. The matrices are
//! closed transitively under composition, and the group is accepted when every
//! **idempotent** matrix in that closure carries a strict decrease on its
//! diagonal — an idempotent matrix describes a call path that can repeat
//! forever, so a decrease on it is a decrease that cannot be sustained.
//!
//! Size-change rather than structural recursion because the corpus needs it.
//! `/std/BigNat/add/raw` descends on *either* of two `Bits` arguments depending
//! on the arm, `add/raw_assoc` does the same over three, and `add/raw_comm`
//! needs the mutual closure across two members. A rule keyed to one designated
//! argument rejects all of them, and a fold cannot express them either, because
//! a fold cannot short-circuit.
//!
//! **Match refinement is part of the size order, not an optimization.** A
//! parameter scrutinized by an enclosing arm is expanded through that arm's
//! constructor before it is compared, so in `add/raw`'s empty-`x` arm the
//! literal argument `b\` grades *equal* to `x` rather than unknown. Without
//! that, the two nil-arm matrices compose to an all-unknown matrix, which is
//! idempotent with no decrease anywhere on its diagonal, and `add/raw` is
//! rejected on a call path that cannot actually occur.
//!
//! Rejection is a **classification, not an error**. `/std/Async` is
//! corecursive, `/std/Json/decode`'s parsers are nullary and productive, and
//! `/std/BigNat/convert/to_str_go` recurses on a computed quotient; none passes
//! this check and none needs to. They stay usable everywhere erasure keeps
//! them. Only [`crate::check_type_totality`] and [`crate::check_proof_totality`]
//! turn a `Partial` classification into a rejection, and only for the positions
//! that erase.

mod reach;
use reach::*;

#[cfg(test)]
mod tests;

use {
    super::{
        Apply, Bound, Carrier, Cases, Context, Definition, Error, Free, FreeMonoid, Func, FuncType,
        Global, InductArm, Item, Layer, Let, Match, Module, Nat, Prim, Proj, Rec, RecGroup,
        RecItem, RecMember, Scope, Struct, Subterm, Telescope, Term, Tuple, Variant, is_prop,
        reduce_forced, zonk,
    },
    num_bigint::BigUint,
    std::collections::{BTreeMap, BTreeSet, HashMap},
};

/// How many times shape reading may unfold a definition before giving up.
///
/// One unfold is what `/std/Bits/cons` needs: it is an ordinary `let`, not a
/// constructor, so `cons(a2, b2)` only exposes its free-monoid layer once the
/// call is unfolded. Two more are slack for a wrapper over a wrapper. The bound
/// exists so a shape read can never become an unbounded reduction.
const UNFOLD_FUEL: usize = 3;

/// How deep a refinement chain may be expanded.
///
/// Each nested `match` on a binder introduced by an outer arm adds one level —
/// `raw_trimmed` reaches three — so this is generous. It exists only so a
/// pathological or cyclic refinement map cannot loop.
const EXPAND_FUEL: usize = 16;

/// The largest transitive closure a group's call matrices may reach.
///
/// Closure is worst-case exponential in the number of call sites. The prelude's
/// largest group closes in tens of matrices; a group that blows past this is
/// classified `Partial`, which is the conservative direction.
const CLOSURE_LIMIT: usize = 4096;

/// Whether a definition is known to terminate on every input.
///
/// `Partial` is "not proven total", never "proven divergent": a productive
/// corecursive definition and a genuine infinite loop are both `Partial`, and
/// both remain legal wherever erasure keeps them.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum Totality {
    /// Every recursive group this definition contains descends, it does not
    /// mention [`Prim::Exit`], and neither does anything it reaches.
    Total,
    /// Not proven total. The conservative default: a definition whose
    /// classification is unknown is `Partial`, never `Total`.
    #[default]
    Partial,
}

impl Totality {
    pub fn is_total(self) -> bool {
        matches!(self, Totality::Total)
    }
}

/// Which half of erasure an obligation guards.
///
/// The two obligations run one analysis over one partiality relation, seeded
/// twice. This says which seeding rejected, because the two failures want
/// different advice: a type must be total or type formation may not terminate,
/// a proof must be total or it proves anything.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Erased {
    Type,
    Proof,
}

impl std::fmt::Display for Erased {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Erased::Type => write!(formatter, "type"),
            Erased::Proof => write!(formatter, "proof"),
        }
    }
}

/// How a call argument's size compares to the caller parameter it is graded
/// against.
///
/// Ordered `Unknown ⊏ Same ⊏ Less`, so joining several routes to the same
/// entry keeps the most informative one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Size {
    /// No relation the analysis can establish.
    Unknown,
    /// The argument is the parameter's own expanded value.
    Same,
    /// The argument is a proper subterm of the parameter's expanded value.
    Less,
}

impl Size {
    /// Sequential composition: what one call followed by another establishes.
    ///
    /// `Unknown` annihilates — a link that says nothing breaks the chain.
    /// `Same` is the identity, and `Less` absorbs, because a chain containing
    /// one strict decrease is a strict decrease.
    fn compose(self, other: Size) -> Size {
        match (self, other) {
            (Size::Unknown, _) | (_, Size::Unknown) => Size::Unknown,
            (Size::Less, _) | (_, Size::Less) => Size::Less,
            (Size::Same, Size::Same) => Size::Same,
        }
    }

    /// Least upper bound: two routes between the same pair of positions keep
    /// the stronger claim.
    fn join(self, other: Size) -> Size {
        self.max(other)
    }
}

/// One call's size relation: `entry(row, column)` grades the callee's
/// `column`th argument against the caller's `row`th parameter.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Matrix {
    rows: usize,
    columns: usize,
    entries: Vec<Size>,
}

impl Matrix {
    /// The matrix that claims nothing — what an unanalyzable call contributes.
    fn unknown(rows: usize, columns: usize) -> Self {
        Self {
            rows,
            columns,
            entries: vec![Size::Unknown; rows * columns],
        }
    }

    fn entry(&self, row: usize, column: usize) -> Size {
        self.entries[row * self.columns + column]
    }

    fn set(&mut self, row: usize, column: usize, size: Size) {
        self.entries[row * self.columns + column] = size;
    }

    /// `self` followed by `other`, in the (join, compose) semiring: the best
    /// relation reachable through any intermediate position.
    fn compose(&self, other: &Matrix) -> Option<Matrix> {
        if self.columns != other.rows {
            return None;
        }
        let mut composed = Matrix::unknown(self.rows, other.columns);
        for row in 0..self.rows {
            for column in 0..other.columns {
                let mut best = Size::Unknown;
                for middle in 0..self.columns {
                    best = best.join(self.entry(row, middle).compose(other.entry(middle, column)));
                }
                composed.set(row, column, best);
            }
        }
        Some(composed)
    }

    /// Whether this matrix describes a call path that composes with itself
    /// unchanged — the paths that can repeat forever.
    fn is_idempotent(&self) -> bool {
        self.rows == self.columns && self.compose(self).as_ref() == Some(self)
    }

    /// Whether some parameter strictly decreases along this path.
    fn descends(&self) -> bool {
        (0..self.rows.min(self.columns)).any(|index| self.entry(index, index) == Size::Less)
    }
}

/// The constructor a [`Shape`] node stands for.
///
/// Carriers are kept apart so a `Bin` cons can never be mistaken for an `Lst`
/// cons. Well-typed code could not confuse them, but the size order is only as
/// trustworthy as the identities it compares.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Tag {
    /// An inductive constructor, by tag alone.
    ///
    /// The owning type is deliberately not part of the identity, because a
    /// freshly minted arm binder has no recorded type to read it from. Dropping
    /// it cannot manufacture a false decrease: every leaf of a shape is a
    /// binder identity minted by this very traversal, so two trees compare
    /// equal only when they name the same binders, and a same-named
    /// constructor of a different type would still have to reach the same
    /// binders to be mistaken for one.
    Variant(super::Atom),
    /// A nominal structure literal.
    Struct(Global),
    /// An anonymous tuple.
    Tuple,
    /// One generator of a free-monoid carrier: `Nat`'s successor, a `Bin`
    /// byte, an `Lst` element.
    Cons(Carriers),
    /// A free-monoid carrier's identity: zero, `b\`, `[]`.
    Empty(Carriers),
    /// A boolean literal.
    Bool(bool),
}

/// Which native free monoid a [`Tag::Cons`] or [`Tag::Empty`] belongs to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Carriers {
    Unary,
    Bin,
    Lst,
}

/// A call argument or a parameter, read as a constructor tree over binder
/// atoms — the term on which the size order is a proper-subterm order.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Shape {
    /// A binder the analysis tracks but cannot see inside.
    Atom(Free),
    /// A constructor applied to its arguments.
    Node(Tag, Vec<Shape>),
    /// A `Nat` strictly below the binder it names, established arithmetically
    /// rather than structurally: `n / k` for a literal `k >= 2`, or `n - k` for
    /// a literal `k >= 1`, at a point where `n` is known nonzero.
    ///
    /// It is a claim *about* another shape rather than a shape of its own, so
    /// it is inert everywhere the constructor order is read — never equal to
    /// anything, never a subterm of anything — and is consulted only by
    /// [`Shape::against`].
    Smaller(Free),
    /// Anything else. Never equal to, and never a subterm of, anything —
    /// including another `Opaque`, since two unreadable terms are not thereby
    /// the same term.
    Opaque,
}

impl Shape {
    /// Whether these are the same value. `Opaque` is deliberately unequal to
    /// itself: it means "not read", not "read and found identical".
    fn same_as(&self, other: &Shape) -> bool {
        match (self, other) {
            (Shape::Atom(left), Shape::Atom(right)) => left == right,
            (Shape::Node(left, left_kids), Shape::Node(right, right_kids)) => {
                left == right
                    && left_kids.len() == right_kids.len()
                    && left_kids
                        .iter()
                        .zip(right_kids)
                        .all(|(left, right)| left.same_as(right))
            }
            _ => false,
        }
    }

    /// Whether `self` occurs strictly inside `whole`.
    fn proper_subterm_of(&self, whole: &Shape) -> bool {
        match whole {
            Shape::Node(_, kids) => kids
                .iter()
                .any(|kid| self.same_as(kid) || self.proper_subterm_of(kid)),
            _ => false,
        }
    }

    /// This argument's size against a parameter whose expanded value is
    /// `parameter` — the whole size order, applied to one entry.
    fn against(&self, parameter: &Shape) -> Size {
        // An arithmetic decrease names the binder it is below, so it grades
        // only against a parameter still standing for exactly that binder. A
        // parameter an enclosing arm has refined to a constructor is a
        // different value, and withholding the claim there is the conservative
        // reading rather than a missed case.
        if let Shape::Smaller(below) = self {
            return match parameter {
                Shape::Atom(atom) if atom == below => Size::Less,
                _ => Size::Unknown,
            };
        }
        if self.same_as(parameter) {
            return Size::Same;
        }
        if self.proper_subterm_of(parameter) {
            return Size::Less;
        }
        Size::Unknown
    }
}

/// How a [`Guard`] relates its binder to its literal, always read with the
/// binder on the left.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Relation {
    Lt,
    Lte,
    Gt,
    Gte,
    Eql,
    Neq,
}

impl Relation {
    /// The same relation with the operands exchanged, for a guard written with
    /// its literal first (`10 > n`).
    fn flipped(self) -> Relation {
        match self {
            Relation::Lt => Relation::Gt,
            Relation::Lte => Relation::Gte,
            Relation::Gt => Relation::Lt,
            Relation::Gte => Relation::Lte,
            Relation::Eql => Relation::Eql,
            Relation::Neq => Relation::Neq,
        }
    }
}

/// A boolean scrutinee read as a comparison between a tracked binder and a
/// `Nat` literal — the only shape from which an arm can conclude that the
/// binder is not zero.
struct Guard {
    atom: Free,
    literal: BigUint,
    relation: Relation,
}

impl Guard {
    fn read(term: &Term) -> Option<Guard> {
        let (left, right, relation) = match &**term {
            Subterm::Prim(Prim::NatLt(left, right)) => (left, right, Relation::Lt),
            Subterm::Prim(Prim::NatLte(left, right)) => (left, right, Relation::Lte),
            Subterm::Prim(Prim::NatGt(left, right)) => (left, right, Relation::Gt),
            Subterm::Prim(Prim::NatGte(left, right)) => (left, right, Relation::Gte),
            Subterm::Prim(Prim::NatEql(left, right)) => (left, right, Relation::Eql),
            Subterm::Prim(Prim::NatNeq(left, right)) => (left, right, Relation::Neq),
            _ => return None,
        };

        let atom = |term: &Term| match &**term {
            Subterm::Var(var) => var.as_free().cloned(),
            _ => None,
        };
        let literal = |term: &Term| term.as_nat().and_then(|nat| nat.to_big_uint());

        if let (Some(atom), Some(literal)) = (atom(left), literal(right)) {
            return Some(Guard {
                atom,
                literal,
                relation,
            });
        }
        match (literal(left), atom(right)) {
            (Some(literal), Some(atom)) => Some(Guard {
                atom,
                literal,
                relation: relation.flipped(),
            }),
            _ => None,
        }
    }

    /// Whether the arm in which this guard evaluated to `taken` proves the
    /// binder is not zero.
    ///
    /// Each row is the arm's fact about `atom` followed by what it takes for
    /// that fact to exclude zero: `atom >= k` excludes it only for `k >= 1`,
    /// while `atom > k` excludes it for every `k`.
    fn establishes_nonzero(&self, taken: bool) -> bool {
        let zero = BigUint::from(0usize);
        let one = BigUint::from(1usize);

        match (self.relation, taken) {
            // atom > k, hence atom >= k + 1 >= 1.
            (Relation::Gt, true) | (Relation::Lte, false) => true,
            // atom >= k.
            (Relation::Gte, true) | (Relation::Lt, false) => self.literal >= one,
            // atom == k.
            (Relation::Eql, true) | (Relation::Neq, false) => self.literal >= one,
            // atom != k.
            (Relation::Neq, true) | (Relation::Eql, false) => self.literal == zero,
            // atom < k and atom <= k both admit zero.
            (Relation::Lt, true) | (Relation::Lte, true) => false,
            (Relation::Gt, false) | (Relation::Gte, false) => false,
        }
    }
}

/// Whether every recursive call path in `group` descends.
///
/// This is the whole of size-change termination as this compiler applies it:
/// collect one matrix per call site, close them under composition, and demand
/// a decrease on the diagonal of every idempotent result. A group with no
/// recursive call at all closes to nothing and is accepted, which is how the
/// prelude's call-free `; ih` folds pass — their recursion is the primitive
/// eliminator's, already structural by construction.
pub(crate) fn group_totality(context: &mut Context, group: &RecGroup) -> Totality {
    let mut members = Vec::new();
    for index in 0..group.length() {
        members.push(Member::of(context, group, index));
    }
    let arities = members.iter().map(|member| member.params.len()).collect();

    let mut calls = Vec::new();
    for (index, member) in members.iter().enumerate() {
        let mut walk = Walk {
            context,
            group,
            arities: &arities,
            caller: index,
            params: &member.params,
            refined: BTreeMap::new(),
            nonzero: BTreeSet::new(),
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

/// One member of a group, opened for analysis: the parameter binders the size
/// order is measured against, and the body under them.
struct Member {
    params: Vec<Free>,
    body: Term,
}

impl Member {
    /// Peel the member's leading lambdas, minting one binder per parameter.
    ///
    /// A member with no lambda — `rec inf : F = F/more(inf)`, or any of
    /// `/std/Json/decode`'s nullary parsers — has an empty parameter vector,
    /// and its self-call therefore contributes a 0×0 matrix. That matrix is
    /// idempotent and has no diagonal to descend on, so a nullary self-call is
    /// rejected, which is exactly right: nothing about it can get smaller.
    fn of(context: &mut Context, group: &RecGroup, index: usize) -> Self {
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
                        let binder = context.fresh(rest.first_hint());
                        params.push(binder.clone());
                        telescope = rest.open(&[&Term::free_var(&binder)]);
                    }
                }
            }
        }

        Self { params, body }
    }
}

/// Close the call matrices transitively, or `None` if the closure outgrows
/// [`CLOSURE_LIMIT`].
///
/// The closure is what makes mutual recursion work without the analysis
/// knowing which members were declared together: `raw_comm` calls
/// `raw_swap_step` which calls back, and only the composite path is a cycle.
fn close(calls: Vec<(usize, usize, Matrix)>) -> Option<Vec<(usize, usize, Matrix)>> {
    let mut closed: BTreeSet<(usize, usize, Matrix)> = calls.into_iter().collect();

    loop {
        let mut discovered = Vec::new();
        for (from, middle, first) in &closed {
            for (start, to, second) in &closed {
                if middle != start {
                    continue;
                }
                let Some(composed) = first.compose(second) else {
                    continue;
                };
                let candidate = (*from, *to, composed);
                if !closed.contains(&candidate) {
                    discovered.push(candidate);
                }
            }
        }
        if discovered.is_empty() {
            return Some(closed.into_iter().collect());
        }
        closed.extend(discovered);
        if closed.len() > CLOSURE_LIMIT {
            return None;
        }
    }
}

/// One member body's traversal: it finds the recursive calls and grades them.
struct Walk<'a> {
    context: &'a mut Context,
    group: &'a RecGroup,
    arities: &'a Vec<usize>,
    caller: usize,
    params: &'a [Free],
    /// What each binder has been refined to by the arms enclosing the position
    /// being walked. Entries are added on the way into an arm and removed on
    /// the way out, so a refinement never escapes the branch that established
    /// it.
    refined: BTreeMap<Free, Shape>,
    /// The binders an enclosing arm has established are not zero, entered and
    /// left exactly like `refined`.
    ///
    /// This is what makes an arithmetic decrease sound rather than merely
    /// plausible: `n / k` is below `n` only when `n` is nonzero, and without
    /// the guard `rec loop(n : Nat) -> Nat = loop(n / 10)` would be accepted
    /// while looping forever at zero.
    nonzero: BTreeSet<Free>,
    calls: Vec<(usize, usize, Matrix)>,
}

impl Walk<'_> {
    /// Record a call to member `callee` with `arguments`, grading each
    /// argument against each of the caller's parameters.
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

    /// The value a binder currently stands for, following the refinements the
    /// enclosing arms established.
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
            // Already relative to a binder; expanding that binder would only
            // lose the identity the claim is stated against.
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
    /// Definitions are unfolded up to [`UNFOLD_FUEL`] times, because the
    /// corpus builds free-monoid conses through ordinary functions rather than
    /// constructors: `/std/Bits/cons` is a `let`, so `cons(a2, b2)` is an
    /// application, and `add/raw_trimmed`'s descent is invisible until it is
    /// unfolded once. Unfolding is weak-head only and bounded, so it cannot
    /// become a reduction; a reduction that fails or exhausts the budget
    /// leaves the term unread rather than failing the compile.
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
                Prim::Lst(_) | Prim::LstAppend(..) | Prim::LstConcat(..) | Prim::LstSlice(..),
            ) => self.monoid_shape(FreeMonoid::Lst, term, fuel),

            // Arithmetic descent. Both operations are monotone and floor-like
            // on Core's unbounded `Nat` — `NatDiv` folds through `BigUint`
            // division and `NatSub` truncates at zero — so each is below its
            // left operand whenever that operand is nonzero.
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
    /// `least` is the smallest literal right-hand operand that makes the
    /// operation strictly decreasing: `2` for division, because `n / 1` is `n`,
    /// and `1` for subtraction, because `n - 0` is `n`. A non-literal operand,
    /// an operand below `least`, or a left side that is neither the binder nor
    /// already a decrease on one, all read as unread — which is what this term
    /// read as before the rule existed.
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
            // Already below `below`, and these operations never grow: dividing
            // or subtracting again keeps it below.
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
                // `Nat`'s generator carries no payload, so its cons node has
                // exactly one child and `pred + 1` is one level over `pred`.
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
        let reducible = matches!(
            &**term,
            Subterm::Var(_)
                | Subterm::Apply(_)
                | Subterm::UniverseInst(_)
                | Subterm::Proj(_)
                | Subterm::Match(_)
                | Subterm::Let(_)
                | Subterm::RecMember(_)
        );
        if fuel == 0 || !reducible || term.reach() != 0 {
            return Shape::Opaque;
        }
        let Ok(reduced) = reduce_forced(self.context, term.clone()) else {
            return Shape::Opaque;
        };
        if reduced == *term {
            return Shape::Opaque;
        }
        self.shape_of(&reduced, fuel - 1)
    }

    /// Read a boolean scrutinee as a comparison against a literal.
    ///
    /// The operator spellings (`n < 10`) elaborate straight to a primitive, but
    /// the named ones (`Nat/lt(n, 10)`) stay applications of a one-line `/sys`
    /// wrapper, so the same bounded weak-head unfolding [`Walk::shape_of`] uses
    /// is what makes both readable.
    fn guard(&mut self, head: &Term, fuel: usize) -> Option<Guard> {
        if let Some(guard) = Guard::read(head) {
            return Some(guard);
        }
        let reducible = matches!(
            &**head,
            Subterm::Var(_)
                | Subterm::Apply(_)
                | Subterm::UniverseInst(_)
                | Subterm::Proj(_)
                | Subterm::Match(_)
                | Subterm::Let(_)
                | Subterm::RecMember(_)
        );
        if fuel == 0 || !reducible || head.reach() != 0 {
            return None;
        }
        let reduced = reduce_forced(self.context, head.clone()).ok()?;
        if reduced == *head {
            return None;
        }
        self.guard(&reduced, fuel - 1)
    }

    /// Walk `body` with `atom` additionally known nonzero, restoring the
    /// previous knowledge afterwards.
    ///
    /// Entered and left around exactly the same walk as the refinement, so an
    /// arm's arithmetic fact has precisely the arm's extent.
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

    /// Walk an arm with `scrutinee` refined to `shape`, restoring the previous
    /// refinement afterwards.
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

    /// One fresh binder per position of a scope, carrying the written hints so
    /// a shape reads in the user's own names.
    fn binders<A: super::Arity>(&mut self, scope: &Scope<A>) -> Vec<Free> {
        (0..scope.arity())
            .map(|index| self.context.fresh(scope.hint(index)))
            .collect()
    }

    /// Open a runtime-arity scope — a motive, an inductive arm, a `let` tail,
    /// a `rec` tail — against fresh binders.
    fn open_many(&mut self, scope: &Scope<super::Many>) -> (Vec<Free>, Term) {
        let binders = self.binders(scope);
        let terms = binders.iter().map(Term::free_var).collect::<Vec<_>>();
        let refs = terms.iter().collect::<Vec<_>>();
        let body = scope.open(&refs);
        (binders, body)
    }

    /// Open the `(pred, ih)` arm of the `Nat` eliminator.
    fn open_two(&mut self, scope: &Scope<super::Two>) -> (Vec<Free>, Term) {
        let binders = self.binders(scope);
        let terms = binders.iter().map(Term::free_var).collect::<Vec<_>>();
        let body = scope.open(&[&terms[0], &terms[1]]);
        (binders, body)
    }

    /// Open the `(head, tail, ih)` arm of the `Bin`/`Lst` eliminators.
    fn open_three(&mut self, scope: &Scope<super::Three>) -> (Vec<Free>, Term) {
        let binders = self.binders(scope);
        let terms = binders.iter().map(Term::free_var).collect::<Vec<_>>();
        let body = scope.open(&[&terms[0], &terms[1], &terms[2]]);
        (binders, body)
    }

    /// Find the recursive calls in `term`, tracking the refinements that make
    /// their arguments comparable.
    fn walk(&mut self, term: &Term) {
        match &**term {
            // Nothing here can contain a call.
            Subterm::Type(_)
            | Subterm::Prop
            | Subterm::NumLit(_)
            | Subterm::Var(_)
            | Subterm::Metavar(_) => {}

            // A member reference at the head of a spine is a call with those
            // arguments; anywhere else it is a call the analysis cannot grade,
            // which an all-unknown matrix records faithfully.
            Subterm::RecMember(RecMember { group, index }) => {
                if group == self.group {
                    self.call(*index, &[]);
                }
            }

            Subterm::Apply(Apply { head, params, .. }) => {
                let (spine_head, arguments) = flatten(term);
                if let Subterm::RecMember(RecMember { group, index }) = &*spine_head
                    && group == self.group
                {
                    self.call(*index, &arguments);
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

            Subterm::TupleType(super::TupleType { telescope }) => {
                self.telescope_units(telescope);
            }

            Subterm::Tuple(Tuple { fields, .. }) => {
                for field in fields {
                    self.walk(field);
                }
            }

            Subterm::Proj(Proj { head, .. })
            | Subterm::UniverseInst(super::UniverseInst { head, .. }) => self.walk(head),

            Subterm::InductType(super::InductType {
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

            Subterm::StructType(super::StructType { params, .. }) => {
                for param in params {
                    self.walk(param);
                }
            }

            Subterm::Struct(Struct { params, fields, .. }) => {
                for term in params.iter().chain(fields) {
                    self.walk(term);
                }
            }

            Subterm::Infix(super::Infix { left, right, .. }) => {
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

            // An inner group is classified on its own, but its bodies may still
            // call *this* group, and such a call is a real edge of this group's
            // call graph.
            Subterm::Rec(Rec { group, tail }) => {
                for index in 0..group.length() {
                    let body = group.member_body(index);
                    self.walk(&body);
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
                // A boolean arm carries no binder, but when the scrutinee
                // compares a binder against a literal the arm still settles
                // whether that binder can be zero — which is what an
                // arithmetic decrease on it needs.
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
                // The default arm stands for every value *not* enumerated, so
                // it refines the scrutinee to nothing — but enumerating zero
                // is exactly what rules zero out everywhere else.
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

    /// One `Cases::Induct` arm: the scrutinee is that constructor applied to
    /// the arm's own payload binders.
    fn induct_arm(&mut self, scrutinee: Option<&Free>, tag: &super::Atom, arm: &InductArm) {
        let (binders, body) = self.open_many(&arm.body);
        let shape = Shape::Node(
            Tag::Variant(tag.clone()),
            binders.iter().map(|b| Shape::Atom(b.clone())).collect(),
        );
        self.refine(scrutinee, shape, &body);
    }

    /// The two arms of a free-monoid eliminator. The cons arm binds the
    /// generator, the tail, and the induction hypothesis; the scrutinee is the
    /// generator consed onto the tail, and the hypothesis is not part of the
    /// value's shape.
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
        cons_case: &Scope<super::Three>,
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
                    let binder = self.context.fresh(rest.first_hint());
                    telescope = rest.open(&[&Term::free_var(&binder)]);
                }
            }
        }
    }

    fn telescope_units(&mut self, telescope: &Telescope<()>) {
        let mut telescope = telescope.clone();
        while let Telescope::Cons(entry, rest) = telescope {
            self.walk(&entry);
            let binder = self.context.fresh(rest.first_hint());
            telescope = rest.open(&[&Term::free_var(&binder)]);
        }
    }
}

/// An application spine as its head and its arguments in order, so an
/// over-applied or curried call is graded as the one call it is.
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
            Subterm::UniverseInst(super::UniverseInst { head: inner, .. }) => head = inner.clone(),
            _ => return (head, arguments),
        }
    }
}

/// Every top-level definition's totality.
///
/// Group rejection is only the local part. A definition is also `Partial` if it
/// mentions [`Prim::Exit`] — which erasure drops, so an exit behind a nullary
/// proof never fires — or if it mentions anything already `Partial`. That last
/// clause is a transitive closure, and it is what makes the flag a usable
/// cross-module summary: "this prelude definition is partial" means something
/// partial is in its closure, so a user proof mentioning it inherits the same.
///
/// `inherited` carries the totality of definitions `module` references but does
/// not define — the replayed prelude prefix. Because each of its flags is
/// already a closure, the walk stops at that boundary instead of re-analyzing
/// the standard library on every compilation. Pass an empty map when `module`
/// is the whole program.
/// Classify one definition against the verdicts already recorded, and record it.
///
/// The whole-module pass needs a fixpoint because it sees every item at once.
/// Items arrive here in dependency order, so everything a definition mentions is
/// already classified and a single pass suffices. A name with no verdict is a
/// member of the group currently elaborating, which [`group_totality`] settles
/// for the group as a whole.
pub fn record_definition_totality(context: &mut Context, definition: &Definition, group: Totality) {
    let partial = !group.is_total()
        || definition_is_locally_partial(context, definition)
        || mentioned(definition).iter().any(|name| {
            context
                .definition_totality(name)
                .is_some_and(|totality| !totality.is_total())
        });
    let totality = match partial {
        true => Totality::Partial,
        false => Totality::Total,
    };
    context.record_definition_totality(&definition.name, totality);
}

/// Obligation (T), at the one point a non-productive type-level loop can still
/// be diagnosed: before the type is reduced.
///
/// The whole-module gate runs post-zonk, which is after elaboration has already
/// performed the type-level reduction it exists to make safe. A type that
/// reaches a partial definition and *is productive* survives elaboration and the
/// gate rejects it; one that is not productive spins until the step budget dies,
/// and the program is refused for apparently running out of a resource that no
/// amount of would have helped. This refuses it first, by name, for the reason
/// it is actually wrong.
///
/// It reads the *lowered* type, before elaboration touches it, so the mentions
/// it sees are the written ones. That is why it is an early net rather than a
/// replacement: the post-zonk gate still runs, and still sees everything
/// elaboration produces.
pub fn check_written_type_totality(
    context: &mut Context,
    type_: &Term,
    site: &str,
) -> Result<(), Error> {
    let offender = type_
        .free_vars()
        .into_iter()
        .filter_map(|free| free.as_global().cloned())
        .find(|name| {
            context
                .definition_totality(name)
                .is_some_and(|totality| !totality.is_total())
        });

    match offender {
        None => Ok(()),
        Some(offender) => Err(Error::PartialInErasedPosition {
            erased: Erased::Type,
            site: site.to_string(),
            offender: Some(offender.to_string()),
        }),
    }
}

pub fn classify_module(
    context: &mut Context,
    module: &Module,
    inherited: &BTreeMap<Global, Totality>,
) -> BTreeMap<Global, Totality> {
    let mut local: BTreeMap<Global, bool> = BTreeMap::new();
    let mut mentions: BTreeMap<Global, BTreeSet<Global>> = BTreeMap::new();

    for item in &module.items {
        match item {
            Item::Let(definition) => {
                let partial = definition_is_locally_partial(context, definition);
                local.insert(definition.name.clone(), partial);
                mentions.insert(definition.name.clone(), mentioned(definition));
            }
            Item::Rec(rec) => {
                let rejected = group_totality(context, &rec.group) == Totality::Partial;
                for definition in rec.definitions() {
                    let partial = rejected || definition_is_locally_partial(context, &definition);
                    local.insert(definition.name.clone(), partial);
                    mentions.insert(definition.name.clone(), mentioned(&definition));
                }
            }
        }
    }

    let mut partial: BTreeSet<Global> = local
        .iter()
        .filter(|(_, value)| **value)
        .map(|(name, _)| name.clone())
        .collect();

    // A name neither defined here nor inherited is treated as total. Every
    // `Global` a zonked module mentions is defined either in it or in the
    // prefix it was spliced onto, so that case is unreachable rather than an
    // approximation.
    let reaches_partial = |partial: &BTreeSet<Global>, reached: &BTreeSet<Global>| {
        reached.iter().any(|other| {
            partial.contains(other)
                || inherited
                    .get(other)
                    .is_some_and(|totality| !totality.is_total())
        })
    };

    loop {
        let mut changed = false;
        for (name, reached) in &mentions {
            if partial.contains(name) {
                continue;
            }
            if reaches_partial(&partial, reached) {
                partial.insert(name.clone());
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    local
        .keys()
        .map(|name| {
            let totality = match partial.contains(name) {
                true => Totality::Partial,
                false => Totality::Total,
            };
            (name.clone(), totality)
        })
        .collect()
}

/// Classify `module` and stamp each definition's [`Definition::totality`].
///
/// Runs post-zonk, beside [`crate::check_positivity`], and for the same reason:
/// the terms it reads are final and meta-free here, so no metavariable can be
/// solved to a partial term after the fact.
///
/// This is the only place the flag is written, and both gates read what it
/// wrote rather than re-deriving it — classification reduces, so doing it once
/// per module is the difference between one pass over the group corpus and
/// three.
pub fn record_totality(
    context: &mut Context,
    module: &mut Module,
    inherited: &BTreeMap<Global, Totality>,
) {
    let classified = classify_module(context, module, inherited);
    let of = |name: &Global| classified.get(name).copied().unwrap_or_default();

    for item in &mut module.items {
        match item {
            Item::Let(definition) => definition.totality = of(&definition.name),
            Item::Rec(rec) => {
                for member in &mut rec.definitions {
                    member.totality = of(&member.name);
                }
            }
        }
    }
}

/// The totality [`record_totality`] stamped onto every definition in `module`.
///
/// This is how the replay path inherits the prelude's verdicts: the archive
/// stores the stamped module, so a user compilation reads the flags instead of
/// re-analyzing the standard library.
pub fn recorded_totality(module: &Module) -> BTreeMap<Global, Totality> {
    let mut recorded = BTreeMap::new();
    for item in &module.items {
        match item {
            Item::Let(definition) => {
                recorded.insert(definition.name.clone(), definition.totality);
            }
            Item::Rec(rec) => {
                for member in &rec.definitions {
                    recorded.insert(member.name.clone(), member.totality);
                }
            }
        }
    }
    recorded
}

/// The verdict for every name in play: `module`'s own stamps over `inherited`.
fn settled(module: &Module, inherited: &BTreeMap<Global, Totality>) -> BTreeMap<Global, Totality> {
    let mut settled = inherited.clone();
    settled.extend(recorded_totality(module));
    settled
}

/// Every way the given positions fail to be total, with the site of each.
///
/// Two ways, and a position can fail either: it reaches a definition already
/// classified `Partial`, or it *is* partial with no name to blame — an inline
/// `rec` that does not descend, or a `Prim::Exit`.
fn faults(
    context: &mut Context,
    module: &Module,
    positions: &[Position],
    inherited: &BTreeMap<Global, Totality>,
) -> Vec<(String, Fault)> {
    let reached = reachable(module, seeds(positions));
    let settled = settled(module, inherited);
    let named = offenders(&reached, &settled);

    let mut faults = Vec::new();
    for position in positions {
        if term_is_locally_partial(context, &position.term) {
            faults.push((position.site.clone(), Fault::Inline));
        }
    }
    // One named fault per offending definition, attributed to the first
    // position that names it. The closure is over the union of the seeds, so
    // the exact position is a best effort; which definition is partial is not.
    for name in named {
        let site = positions
            .iter()
            .find(|position| {
                position
                    .term
                    .free_vars()
                    .iter()
                    .filter_map(|free| free.as_global())
                    .any(|global| *global == name)
            })
            .map(|position| position.site.clone())
            .unwrap_or_else(|| "a position reached from here".to_string());
        faults.push((site, Fault::Named(name)));
    }
    faults
}

/// Obligation **(T)**: everything a type position reaches must be total.
///
/// A divergent type breaks type formation, and — through
/// `rec Bad : Type = Sink(Bad)` — ties the negative knot strict positivity
/// exists to forbid without ever writing an `induct`. Runs post-zonk, after
/// [`record_totality`], whose flags it reads.
pub fn check_type_totality(
    context: &mut Context,
    module: &Module,
    inherited: &BTreeMap<Global, Totality>,
) -> Result<(), Error> {
    let mut positions = type_positions(module);
    positions.extend(checked_type_positions(context)?);
    report(context, module, &positions, inherited, Erased::Type)
}

/// The terms elaboration settled *at a sort* — a term whose type is `Type` or
/// `Prop` is itself a type, wherever it was written.
///
/// This is the half [`type_positions`] structurally cannot reach. That walk is a
/// syntactic reading of where types are *written*: an annotation, a motive, a
/// declaration telescope, a definition whose type ends in a sort. A type passed
/// as an *argument* is written nowhere it looks — `annotations` has no case for
/// an application's parameters — so `ignore(@Shape(inf), 5)` handed a partial
/// value to a type-level function with nothing seeded, while the same type in an
/// annotation was rejected.
///
/// The two seedings are **incomparable**, not nested, which a count of each
/// obscured: the walk sees definition bodies whose type ends in a sort, and no
/// typing judgment classifies those as type positions; this sees type arguments,
/// which no syntactic walk can find without re-deriving the head's telescope —
/// the re-derivation that cost (V) six defects. Taking the union costs one pass
/// over already-recorded terms and needs neither.
fn checked_type_positions(context: &mut Context) -> Result<Vec<Position>, Error> {
    let settled = context.checked().to_vec();
    let mut positions = Vec::new();

    for (term, type_, site) in settled {
        let type_ = zonk(context, &type_)?;
        // The term *is* a type exactly when its own type is a sort. A universe
        // instance wraps one without changing that.
        let sort = match &*type_ {
            Subterm::Type(_) | Subterm::Prop => true,
            Subterm::UniverseInst(instance) => {
                matches!(&*instance.head, Subterm::Type(_) | Subterm::Prop)
            }
            _ => false,
        };
        if sort {
            positions.push(Position {
                term: zonk(context, &term)?,
                site: format!("a type in {site}"),
            });
        }
    }

    Ok(positions)
}

/// Obligation **(V)**: everything a `Prop`-checked term reaches must be total.
///
/// A divergent proof proves anything, and erasure deletes it — so an `exit`
/// behind a proposition never fires and the program continues with a forged
/// invariant. Independent of [`check_type_totality`]: neither obligation
/// subsumes the other.
pub fn check_proof_totality(
    context: &mut Context,
    module: &Module,
    inherited: &BTreeMap<Global, Totality>,
) -> Result<(), Error> {
    let positions = checked_proof_positions(context)?;
    report(context, module, &positions, inherited, Erased::Proof)
}

/// Obligation (V)'s seed: every term elaboration settled at a `Prop`-sorted
/// type.
///
/// Drains what [`Context::record_checked`] collected and keeps the
/// propositions. This is the whole of (V)'s seeding. There is no walk, because
/// there is nothing to look for — elaboration already decided, for every term
/// in the program, what type that term has, and the only thing a later pass can
/// do with that question is re-derive it, incompletely.
///
/// Two properties follow from the seed rather than from care. **Coverage** is a
/// consequence of elaboration being a typechecker: a position it never settles
/// is a position the program was never typed at. And the **prelude boundary** is
/// a consequence of replay — the archived prefix is defined into the context
/// rather than elaborated, so it settles nothing and seeds nothing, and its
/// verdicts arrive through [`recorded_totality`] instead.
///
/// Sort-hood is decided here rather than at the hook because a type may still
/// carry unsolved metavariables while elaborating. Post-zonk every solution is
/// materialized, so [`is_prop`](crate::is_prop) is asked once per *distinct*
/// type, and hash-consing keeps that set far smaller than the term count.
// Safety: the memo is keyed on `Term`, which carries `OnceCell` scalar caches
// and so trips Clippy's interior-mutability warning. The logical value is fully
// immutable, and hashing and equality stay stable across those caches filling.
#[allow(clippy::mutable_key_type)]
fn checked_proof_positions(context: &mut Context) -> Result<Vec<Position>, Error> {
    let checked = context.take_checked();
    let mut memo: HashMap<Term, bool> = HashMap::new();
    let mut positions = Vec::new();

    for (term, type_, site) in checked {
        let type_ = zonk(context, &type_)?;
        let prop = match memo.get(&type_) {
            Some(prop) => *prop,
            None => {
                let prop = is_prop(context, &type_)?;
                memo.insert(type_, prop);
                prop
            }
        };
        if prop {
            positions.push(Position {
                term: zonk(context, &term)?,
                site: format!("a proof in {site}"),
            });
        }
    }

    Ok(positions)
}

fn report(
    context: &mut Context,
    module: &Module,
    positions: &[Position],
    inherited: &BTreeMap<Global, Totality>,
    erased: Erased,
) -> Result<(), Error> {
    match faults(context, module, positions, inherited)
        .into_iter()
        .next()
    {
        None => Ok(()),
        Some((site, Fault::Named(name))) => Err(Error::PartialInErasedPosition {
            erased,
            site,
            offender: Some(name.to_string()),
        }),
        Some((site, Fault::Inline)) => Err(Error::PartialInErasedPosition {
            erased,
            site,
            offender: None,
        }),
    }
}

/// Obligation (T), in the one place it cannot wait for the post-zonk pass.
///
/// A member of a partial group may not have a sort in an *extractable*
/// position of its type: its codomain after peeling arrows, or any component
/// reachable by projection. Parameter positions are exempt, so an ordinary
/// partial polymorphic program keeping `@A : Type` is untouched.
///
/// This exists because `rec Bad : Type = (Bad) -> False` overflows the
/// compiler's stack while elaborating its first *use* — reducing `Bad` rebuilds
/// an arrow whose domain is `Bad` — which is long before any whole-module gate
/// runs. Without it the compiler aborts where it should diagnose. The closure
/// form is what makes the language sound; this is what makes it answer.
pub fn check_rec_totality(
    context: &mut Context,
    group: &RecGroup,
    names: &[String],
) -> Result<(), Error> {
    let extractable = group
        .iter()
        .enumerate()
        .filter(|(_, member)| yields_a_sort(member.type_.body()))
        .map(|(index, _)| index)
        .collect::<Vec<_>>();

    if extractable.is_empty() || group_totality(context, group).is_total() {
        return Ok(());
    }

    let site = extractable
        .into_iter()
        .filter_map(|index| names.get(index))
        .map(|name| format!("'{name}'"))
        .collect::<Vec<_>>()
        .join(", ");

    Err(Error::PartialInErasedPosition {
        erased: Erased::Type,
        site: format!("the recursive definition {site}"),
        offender: None,
    })
}

/// [`check_rec_totality`] for a top-level `rec`, which knows its own names.
pub fn check_rec_item_totality(context: &mut Context, rec: &RecItem) -> Result<(), Error> {
    let names = rec
        .definitions()
        .into_iter()
        .map(|definition| definition.name.to_string())
        .collect::<Vec<_>>();
    check_rec_totality(context, &rec.group, &names)
}

/// Whether a sort is extractable from this type: reachable by peeling arrows
/// to the codomain, or by projecting a tuple component. A sort in a *parameter*
/// is not extractable — `(A : Type) -> A` denotes a value, not a type.
fn yields_a_sort(type_: &Term) -> bool {
    match &**type_ {
        Subterm::Type(_) | Subterm::Prop => true,
        Subterm::FuncType(FuncType { telescope, .. }) => {
            let mut telescope = telescope;
            loop {
                match telescope {
                    Telescope::Done(body) => break yields_a_sort(body),
                    Telescope::Cons(_, rest) => telescope = rest.body(),
                }
            }
        }
        Subterm::TupleType(tuple) => {
            let mut telescope = &tuple.telescope;
            loop {
                match telescope {
                    Telescope::Done(_) => break false,
                    Telescope::Cons(entry, rest) => match yields_a_sort(entry) {
                        true => break true,
                        false => telescope = rest.body(),
                    },
                }
            }
        }
        _ => false,
    }
}

/// Whether this term is partial on its own account, with no name to blame: it
/// contains a `rec` group that does not descend, or an exit.
fn term_is_locally_partial(context: &mut Context, term: &Term) -> bool {
    let mut groups = Vec::new();
    let mut exits = false;
    collect_local(term, &mut groups, &mut exits);
    exits
        || groups
            .iter()
            .any(|group| group_totality(context, group) == Totality::Partial)
}

/// Whether this definition is partial on its own account: it mentions an exit,
/// or it contains a local `rec` group that does not descend.
fn definition_is_locally_partial(context: &mut Context, definition: &Definition) -> bool {
    let mut groups = Vec::new();
    let mut exits = false;
    collect_local(&definition.body, &mut groups, &mut exits);
    collect_local(&definition.type_, &mut groups, &mut exits);
    if exits {
        return true;
    }
    groups
        .iter()
        .any(|group| group_totality(context, group) == Totality::Partial)
}

/// The local `rec` groups and `Prim::Exit` occurrences inside a term.
fn collect_local(term: &Term, groups: &mut Vec<RecGroup>, exits: &mut bool) {
    if let Subterm::Rec(Rec { group, .. }) = &**term {
        groups.push(group.clone());
    }
    if let Subterm::Prim(Prim::Exit(..)) = &**term {
        *exits = true;
    }
    let subterm: &Subterm = term;
    subterm.any_child_term(&mut |child| {
        collect_local(child, groups, exits);
        false
    });
}

/// Every top-level name this definition mentions, by free variable.
fn mentioned(definition: &Definition) -> BTreeSet<Global> {
    definition
        .body
        .free_vars()
        .into_iter()
        .chain(definition.type_.free_vars())
        .filter_map(|free| free.as_global().cloned())
        .collect()
}
