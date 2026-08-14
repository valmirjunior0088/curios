use {
    super::{ReduceError, Reducer},
    crate::{
        Intrinsic, Nat, Peel, Subterm, Term, normalize_concat, peel_bin, peel_first_atom,
        peel_first_elem, project_erased_universes,
    },
    curios_base::{Grain, Int, PackedBin, int_rotl, int_rotr, nat_rotl, nat_rotr},
    num_bigint::BigUint,
    num_traits::{One, ToPrimitive, Zero},
    std::cmp::Ordering,
};

/// Read an already-reduced `Nat` term as a concrete `usize` index — `None` when it is still symbolic or too large to fit. The shared decode behind the `Bin`/`List` `get`/`slice` bounds.
fn as_index(term: &Term) -> Option<usize> {
    term.as_nat().and_then(|n| n.to_big_uint()?.to_usize())
}

/// Reduce both operands of a `Bool` binary intrinsic, then either `fold` the two literals or `rebuild` the neutral term. `Bool` has no numeric carrier at the type level, so the fold reads the `true`/`false` constructors directly.
fn reduce_bool_binary(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(bool, bool) -> bool,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    Ok(Subterm::Intrinsic(
        match (left.as_bool(), right.as_bool()) {
            (Some(l), Some(r)) => Intrinsic::Bool(fold(l, r)),
            _ => rebuild(left, right),
        },
    ))
}

fn reduce_byte_binary(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(u8, u8) -> bool,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    Ok(Subterm::Intrinsic(match (&*left, &*right) {
        (Subterm::Intrinsic(Intrinsic::Byte(left)), Subterm::Intrinsic(Intrinsic::Byte(right))) => {
            Intrinsic::Bool(fold(*left, *right))
        }
        _ => rebuild(left, right),
    }))
}

/// Reduce both operands of a `Nat` binary intrinsic, then either `fold` the two literals or `rebuild` the neutral term from the reduced operands.
fn reduce_nat_binary(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(Nat, Nat) -> Option<Intrinsic>,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    let folded = match (left.as_nat(), right.as_nat()) {
        (Some(l), Some(r)) => fold(l, r),
        _ => None,
    };

    Ok(Subterm::Intrinsic(match folded {
        Some(intrinsic) => intrinsic,
        None => rebuild(left, right),
    }))
}

/// Which half of a Euclidean division a fold computes. One enum rather than the pair of closures the other families take: the symbolic laws below build the quotient and the remainder out of the *same* split, so the two halves cannot be parameterized independently.
#[derive(Clone, Copy)]
enum Euclid {
    Quotient,
    Remainder,
}

impl Euclid {
    fn kind(self) -> &'static str {
        match self {
            Euclid::Quotient => "Nat/div",
            Euclid::Remainder => "Nat/rem",
        }
    }

    fn fold(self, left: Nat, right: Nat) -> Option<Nat> {
        match self {
            Euclid::Quotient => left.checked_div(right),
            Euclid::Remainder => left.checked_rem(right),
        }
    }

    fn rebuild(self, left: Term, right: Term) -> Intrinsic {
        match self {
            Euclid::Quotient => Intrinsic::NatDiv(left, right),
            Euclid::Remainder => Intrinsic::NatRem(left, right),
        }
    }
}

/// A statically known upper bound on every value a reduced term can take, or `None` where it has none.
///
/// Every arm is unconditional, which is what lets the callers below turn a bound into a definitional equation. A `Byte` is `0..=255` by its carrier — `Nat/to_byte` wraps and `Byte` is not a wire type, so no embedder can supply one outside the range — and `x % n < n` holds by definition, a zero divisor having already been reported. The remaining arms are monotone in operands whose own bounds this establishes.
///
/// A wrong bound here is a false definitional equation, not a wrong value: see `documentation/soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md`.
fn nat_bound(term: &Term) -> Option<BigUint> {
    let Subterm::Intrinsic(intrinsic) = &**term else {
        return None;
    };

    match intrinsic {
        Intrinsic::Nat(Nat::Zero) => Some(BigUint::zero()),
        Intrinsic::Nat(Nat::Succ(floor, inner)) => Some(floor + nat_bound(inner)?),
        Intrinsic::ByteToNat(_) => Some(BigUint::from(u8::MAX)),
        Intrinsic::NatRem(_, divisor) => {
            let divisor = divisor.as_nat()?.to_big_uint()?;
            (!divisor.is_zero()).then(|| divisor - BigUint::one())
        }
        // Either bound alone is an upper bound, so one suffices; with both, the smaller wins.
        Intrinsic::NatAnd(left, right) => match (nat_bound(left), nat_bound(right)) {
            (Some(left), Some(right)) => Some(left.min(right)),
            (Some(bound), None) | (None, Some(bound)) => Some(bound),
            (None, None) => None,
        },
        Intrinsic::NatAdd(left, right) => Some(nat_bound(left)? + nat_bound(right)?),
        Intrinsic::NatMul(left, right) => Some(nat_bound(left)? * nat_bound(right)?),
        _ => None,
    }
}

/// A reduced summand read as `coefficient · factor` with a literal coefficient, in either operand order. `NatMul` folds two literals, so at most one side is literal by the time this sees it.
fn nat_literal_factor(summand: &Term) -> Option<(BigUint, Term)> {
    let Subterm::Intrinsic(Intrinsic::NatMul(left, right)) = &**summand else {
        return None;
    };

    if let Some(coefficient) = left.as_nat().and_then(|value| value.to_big_uint()) {
        return Some((coefficient, right.clone()));
    }

    right
        .as_nat()
        .and_then(|value| value.to_big_uint())
        .map(|coefficient| (coefficient, left.clone()))
}

/// `coefficient · factor`, dropping a zero product and a unit coefficient rather than emitting `0 · t` or `1 · t` for reduction to clear afterwards.
fn nat_scaled(coefficient: BigUint, factor: Term) -> Term {
    if coefficient.is_zero() {
        return Term::intrinsic(Intrinsic::Nat(Nat::Zero));
    }

    match coefficient.is_one() {
        true => factor,
        false => Term::intrinsic(Intrinsic::nat_mul(
            Term::intrinsic(Intrinsic::Nat(Nat::new(coefficient))),
            factor,
        )),
    }
}

/// Split a reduced dividend against a literal divisor into `(quotient, remainder)`, or `None` where the division is not forced.
///
/// Every summand must be either a literal multiple of `n` — contributing its cofactor to the quotient — or statically bounded. When the bounded summands together with the residual floor stay below `n`, none of them can carry into the next multiple, so the split is exact for every value the symbolic parts take. That is what makes `(256·x + Byte/to_nat(b)) / 256` reduce to `x`.
fn nat_euclid_split(dividend: &Term, divisor: &BigUint) -> Option<(Term, Term)> {
    let (floor, inner) = Nat::decompose(dividend);
    let mut quotient = Vec::new();
    let mut residual = Vec::new();
    let mut ceiling = &floor % divisor;

    for summand in Nat::summands(&inner) {
        match nat_literal_factor(&summand) {
            Some((coefficient, factor)) if (&coefficient % divisor).is_zero() => {
                quotient.push(nat_scaled(coefficient / divisor, factor));
            }
            _ => {
                ceiling += nat_bound(&summand)?;
                residual.push(summand);
            }
        }
    }

    match ceiling < *divisor {
        true => Some((
            Nat::sum_over_floor(quotient, &floor / divisor),
            Nat::sum_over_floor(residual, &floor % divisor),
        )),
        false => None,
    }
}

/// `Nat/div`/`Nat/rem`: partial, like [`reduce_nat_binary`] is not — a divisor that reduces to literal zero is a reported error (the type-level mirror of the runtime trap, following `BinGet`'s pattern), never a Rust panic.
///
/// Past the closed fold, two unconditional laws let a literal divisor see through a symbolic dividend. Writing the dividend as `inner + floor` and the divisor as `n`:
///
/// The *floor law* is the division twin of `NatAdd`'s: `(i + f) / n = f/n + (i + f%n) / n`, and `(i + f) % n = (i + f%n) % n`. Both hold for every `i`, because `f = (f/n)·n + f%n` contributes exactly `f/n` whole divisors whatever `i` is. As with addition the floor only moves outward, and the residual floor `f%n < n` cannot fire the rule a second time.
///
/// The *split* additionally reads the summands, and is the rule that makes a base-256 encoding provably injective; [`nat_euclid_split`] states it and [`nat_bound`] states why the bounds it rests on are unconditional.
///
/// Nothing conditional may be added here. `(a + b)/n = a/n + b/n` is false — `1/2 + 1/2 = 0 ≠ 1` — so a law holding only for some values of a symbolic part would be a false definitional equation, and congruence carries one of those to `False`.
fn reduce_nat_division(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    euclid: Euclid,
) -> Result<Subterm, ReduceError> {
    let span = right.span().or_else(|| left.span());
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    let divisor = right.as_nat().and_then(|divisor| divisor.to_big_uint());
    if divisor.as_ref().is_some_and(BigUint::is_zero) {
        return Err(ReduceError::DivisionByZero {
            kind: euclid.kind(),
            span,
        });
    }

    if let (Some(dividend), Some(by)) = (left.as_nat(), right.as_nat())
        && let Some(folded) = euclid.fold(dividend, by)
    {
        return Ok(Subterm::Intrinsic(Intrinsic::Nat(folded)));
    }

    if let Some(divisor) = &divisor {
        if let Some((quotient, remainder)) = nat_euclid_split(&left, divisor) {
            return Ok(Term::unwrap_or_clone(match euclid {
                Euclid::Quotient => quotient,
                Euclid::Remainder => remainder,
            }));
        }

        // The floor law alone, for a dividend the split could not close: peel the whole divisors the floor certainly carries and leave the rest neutral.
        let (floor, inner) = Nat::decompose(&left);
        if floor >= *divisor {
            let peeled = Term::intrinsic(
                euclid.rebuild(Nat::rebuild(&floor % divisor, inner), right.clone()),
            );

            return Ok(Term::unwrap_or_clone(match euclid {
                Euclid::Quotient => Nat::rebuild(&floor / divisor, peeled),
                Euclid::Remainder => peeled,
            }));
        }
    }

    Ok(Subterm::Intrinsic(euclid.rebuild(left, right)))
}

/// `Int` counterpart of [`reduce_nat_binary`]: fold both literal operands or rebuild the neutral term. The fold is partial for the same reason — the shifts decline a negative or oversized literal shift count (`None`); the total ops just wrap their result in `Some`.
fn reduce_int_binary(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(Int, Int) -> Option<Intrinsic>,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    let folded = match (left.as_int(), right.as_int()) {
        (Some(l), Some(r)) => fold(l, r),
        _ => None,
    };

    Ok(Subterm::Intrinsic(match folded {
        Some(intrinsic) => intrinsic,
        None => rebuild(left, right),
    }))
}

/// `Int/div`/`Int/rem`: like [`reduce_int_binary`], but a divisor that reduces to literal zero is a reported error — mathematically undefined, following `BinGet`'s pattern. The fold itself is exact and total past that: the type level pretends ℤ (see [`Int`]).
fn reduce_int_division(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    kind: &'static str,
    fold: impl FnOnce(Int, Int) -> Option<Int>,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let span = right.span().or_else(|| left.span());
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    if right.as_int().is_some_and(|divisor| divisor.is_zero()) {
        return Err(ReduceError::DivisionByZero { kind, span });
    }

    let folded = match (left.as_int(), right.as_int()) {
        (Some(l), Some(r)) => fold(l, r).map(Intrinsic::Int),
        _ => None,
    };

    Ok(Subterm::Intrinsic(match folded {
        Some(intrinsic) => intrinsic,
        None => rebuild(left, right),
    }))
}

/// `Flt` operations are opaque at the type level: operands reduce, the operation never folds — `FltAdd(1.0, 1.0)` is its own normal form, so `Eq(@Flt, 1.0 + 1.0, 2.0)` is deliberately unprovable. IEEE semantics inside definitional equality is a soundness hazard with no consumer: the corpus proves nothing about floats, and IEEE equality identifies values (`0.0`, `-0.0`) that `FltToLeBytes` observes apart — the exact shape the singleton guard exists to forbid. Runtime-faithful constant folding belongs downstream in `curios-ersd`'s partial evaluator, which is untrusted. The rule this instance establishes: an intrinsic needs a fold here only if a type or a proof can depend on its value.
fn reduce_flt_binary(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    Ok(Subterm::Intrinsic(rebuild(left, right)))
}

/// Reduce the operand of a `Nat` unary intrinsic, then either `fold` the literal or `rebuild` the neutral term from the reduced operand.
fn reduce_nat_unary(
    reducer: &mut impl Reducer,
    inner: &Term,
    fold: impl FnOnce(Nat) -> Option<Intrinsic>,
    rebuild: impl FnOnce(Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let inner = reducer.reduce_forced(inner.clone())?;

    Ok(Subterm::Intrinsic(match inner.as_nat().and_then(fold) {
        Some(intrinsic) => intrinsic,
        None => rebuild(inner),
    }))
}

/// `Int` counterpart of [`reduce_nat_unary`]. The fold's `None` rebuilds the neutral term: with `Int` unbounded at the type level, a conversion of a value the target cannot represent simply stays stuck.
fn reduce_int_unary(
    reducer: &mut impl Reducer,
    inner: &Term,
    fold: impl FnOnce(Int) -> Option<Intrinsic>,
    rebuild: impl FnOnce(Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let inner = reducer.reduce_forced(inner.clone())?;

    Ok(Subterm::Intrinsic(match inner.as_int().and_then(fold) {
        Some(intrinsic) => intrinsic,
        None => rebuild(inner),
    }))
}

/// [`reduce_flt_binary`]'s unary counterpart: opaque at the type level, the operand reduces and the operation always rebuilds.
fn reduce_flt_unary(
    reducer: &mut impl Reducer,
    inner: &Term,
    rebuild: impl FnOnce(Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let inner = reducer.reduce_forced(inner.clone())?;

    Ok(Subterm::Intrinsic(rebuild(inner)))
}

/// The structural outcome of comparing two `Nat`s. The whole comparison family (`eql`/`neq`/`lt`/`lte`/`gt`/`gte`) reads this one result; each op differs only in how it maps the outcome to a `bool`. `Le`/`Ge` record a *non-strict* bound the operands force without pinning equality (e.g. `succ x ≥ 1`), letting `lt`/`gte` decide where `eql` still cannot; `Stuck` is undecidable, and the op's neutral term is rebuilt.
#[derive(Debug, PartialEq)]
enum Comparison {
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
    Stuck,
}

fn from_ordering(ordering: Ordering) -> Comparison {
    match ordering {
        Ordering::Less => Comparison::Lt,
        Ordering::Equal => Comparison::Eq,
        Ordering::Greater => Comparison::Gt,
    }
}

/// The `Nat` eliminator's structural comparison, specialized to the flat `BigUint` successor spine: the floors stand in for peeling successors, so no recursion is needed and two literals decide in one `BigUint` compare (the literal fold folds into the shared-inner shortcut). It decides ONLY where the answer is forced and is `Stuck` otherwise — a sound partial decision procedure, the shared body of the whole comparison family. (The `lt` partner of the `Unary` eliminator's successor peel; for `Bin`/`List` the same `Comparison` shape would recurse via `uncons`.)
///
/// Returns the operands with their shared successor floor peeled off, so an *undecided* comparison still rebuilds a normalized neutral: `cmp(x + m, y + m)` and `cmp(x, y)` reduce to the same term, which conversion needs (e.g. `Lt(a, succ b) ≡ Lt(succ a, succ(succ b))`).
fn compare_nat(
    reducer: &mut impl Reducer,
    left: Term,
    right: Term,
) -> Result<(Comparison, Term, Term), ReduceError> {
    let left = reducer.reduce_forced(left)?;
    let right = reducer.reduce_forced(right)?;
    // Cancel first, so everything below reads the residuals: the shared part decides nothing on its own, and removing it is what lets `cmp(x + a, x + b)` reach `cmp(a, b)` — and `cmp(a + b, b + a)` reach equality — instead of stalling on two inners that differ only by what they share.
    let (left, right) = Nat::cancel_common(&left, &right);

    let (sl, il) = Nat::decompose(&left);
    let (sr, ir) = Nat::decompose(&right);

    // Same inner ⇒ the floors alone decide: `cmp(x + sl, x + sr) = cmp(sl, sr)` (so `lt(pred, succ pred) = true`). Two literals — inner `0` on both sides — also land here: this is the O(1) literal fold. Otherwise, whichever side keeps successors past the shared floor is larger *iff* the other bottomed out at literal zero (`inner ≥ 0`); equal floors with one zero inner give a non-strict bound (`a ≤ b`/`a ≥ b`) the strict/`gte`/`lte` reads still use; anything else is undecidable.
    // Compared up to universe instances for the same reason [`Nat::cancel_common`] matches summands that way: two occurrences of a polymorphic name carry independently fresh instances, and a level is not part of the answer to "are these the same number".
    let outcome = if project_erased_universes(&il) == project_erased_universes(&ir) {
        from_ordering(sl.cmp(&sr))
    } else {
        match sl.cmp(&sr) {
            Ordering::Greater if Nat::is_zero(&ir) => Comparison::Gt,
            Ordering::Less if Nat::is_zero(&il) => Comparison::Lt,
            Ordering::Equal if Nat::is_zero(&il) => Comparison::Le,
            Ordering::Equal if Nat::is_zero(&ir) => Comparison::Ge,
            _ => Comparison::Stuck,
        }
    };

    // A statically bounded operand decides against a literal one where the floors alone cannot: `bound(l) < r` forces `l < r` for every value `l` takes. This is what reduces `x % n < n`, whose left inner is a stuck `NatRem` the structural body has nothing to say about. See `nat_bound` for why each bound holds unconditionally.
    let outcome = match outcome {
        Comparison::Stuck if Nat::is_zero(&ir) => match nat_bound(&il).map(|bound| bound + &sl) {
            Some(bound) if bound < sr => Comparison::Lt,
            _ => Comparison::Stuck,
        },
        Comparison::Stuck if Nat::is_zero(&il) => match nat_bound(&ir).map(|bound| bound + &sr) {
            Some(bound) if bound < sl => Comparison::Gt,
            _ => Comparison::Stuck,
        },
        decided => decided,
    };

    Ok((outcome, left, right))
}

/// Reduce a `Nat` comparison through the shared structural body [`compare_nat`]. `read` projects the outcome to this op's boolean (or `None` when the operands do not decide it), in which case the neutral term is rebuilt from the peeled operands so undecided comparisons land in a normal form.
fn reduce_nat_compare(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    read: impl FnOnce(Comparison) -> Option<bool>,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let (outcome, left, right) = compare_nat(reducer, left.clone(), right.clone())?;

    Ok(match read(outcome) {
        Some(value) => Subterm::Intrinsic(Intrinsic::Bool(value)),
        None => Subterm::Intrinsic(rebuild(left, right)),
    })
}

/// The free-monoid product structure of a reduced carrier value, the view a monoid homomorphism (`len`/`map`) distributes over: a literal run of generators `L` (bytes for `Bin`, elements for `List`), an n-ary `Concat` of operands to recurse on, an `Append` of a base and one appended generator, or an `Opaque` node (a variable / slice) the homomorphism leaves neutral. `Empty` is just `Literal(∅)`.
enum Shape<L> {
    Literal(Vec<L>),
    Concat(Vec<Term>),
    Append(Term, Term),
    Opaque(Term),
}

/// Classify a reduced `Bin` value into its product shape (generators are bytes).
fn bin_shape(grain: Grain, value: Term) -> Shape<u8> {
    match Term::unwrap_or_clone(value) {
        Subterm::Intrinsic(Intrinsic::Bin(found, value)) if found == grain => {
            Shape::Literal(match grain {
                Grain::B => (0..value.bit_length())
                    .map(|index| u8::from(value.bit(index).unwrap()))
                    .collect(),
                Grain::X => value.to_bytes().unwrap(),
            })
        }
        Subterm::Intrinsic(Intrinsic::BinConcat(found, operands)) if found == grain => {
            Shape::Concat(operands)
        }
        Subterm::Intrinsic(Intrinsic::BinAppend(found, base, atom)) if found == grain => {
            Shape::Append(base, atom)
        }
        other => Shape::Opaque(other.into()),
    }
}

/// Classify a reduced `List` value into its product shape (generators are elements).
fn list_shape(value: Term) -> Shape<Term> {
    match Term::unwrap_or_clone(value) {
        Subterm::Intrinsic(Intrinsic::List(_, elems)) => Shape::Literal(elems),
        Subterm::Intrinsic(Intrinsic::ListConcat(_, operands)) => Shape::Concat(operands),
        Subterm::Intrinsic(Intrinsic::ListAppend(_, base, elem)) => Shape::Append(base, elem),
        other => Shape::Opaque(other.into()),
    }
}

/// The shared driver for a free-monoid homomorphism `h` — the one place its distribution law lives, so a carrier physically cannot forget a case. A literal run maps via `literal`; a concatenation recurses `h` over its operands and folds the images with `combine`; an append combines `h(base)` with the appended generator via `append`; an opaque value stays neutral, rebuilt by `node` (which also builds `h(sub)` to recurse). `len` and `map` differ only in those four slots. The built image is reduced, so the homomorphism is eager.
fn reduce_homomorphism<L>(
    reducer: &mut impl Reducer,
    shape: Shape<L>,
    literal: impl Fn(Vec<L>) -> Term,
    combine: impl Fn(Vec<Term>) -> Term,
    append: impl Fn(Term, Term) -> Term,
    node: impl Fn(Term) -> Term,
) -> Result<Subterm, ReduceError> {
    let built = match shape {
        Shape::Literal(run) => literal(run),
        Shape::Concat(operands) => combine(operands.into_iter().map(node).collect()),
        Shape::Append(base, generator) => append(node(base), generator),
        Shape::Opaque(value) => return Ok(Term::unwrap_or_clone(node(value))),
    };

    reducer.reduce(built).map(Term::unwrap_or_clone)
}

/// `Σ` over a run of `Nat` images — the `combine` of the `len` homomorphism into `(ℕ, +, 0)`. `NatAdd`'s successor peeling carries the count out of a symbolic spine.
fn nat_sum(images: Vec<Term>) -> Term {
    images
        .into_iter()
        .rev()
        .fold(Term::intrinsic(Intrinsic::Nat(Nat::Zero)), |acc, image| {
            Term::intrinsic(Intrinsic::nat_add(image, acc))
        })
}

pub fn reduce_intrinsic(
    reducer: &mut impl Reducer,
    intrinsic: &Intrinsic,
) -> Result<Subterm, ReduceError> {
    match intrinsic {
        Intrinsic::BoolType => Ok(Subterm::Intrinsic(Intrinsic::BoolType)),
        Intrinsic::Bool(value) => Ok(Subterm::Intrinsic(Intrinsic::Bool(*value))),
        Intrinsic::BoolAnd(left, right) => {
            reduce_bool_binary(reducer, left, right, |l, r| l && r, Intrinsic::BoolAnd)
        }
        Intrinsic::BoolOr(left, right) => {
            reduce_bool_binary(reducer, left, right, |l, r| l || r, Intrinsic::BoolOr)
        }
        Intrinsic::BoolXor(left, right) => {
            reduce_bool_binary(reducer, left, right, |l, r| l != r, Intrinsic::BoolXor)
        }
        Intrinsic::BoolEql(left, right) => {
            reduce_bool_binary(reducer, left, right, |l, r| l == r, Intrinsic::BoolEql)
        }
        Intrinsic::BoolNeq(left, right) => {
            reduce_bool_binary(reducer, left, right, |l, r| l != r, Intrinsic::BoolNeq)
        }
        Intrinsic::NatType => Ok(Subterm::Intrinsic(Intrinsic::NatType)),
        Intrinsic::Nat(Nat::Zero) => Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero))),
        Intrinsic::Nat(Nat::Succ(spine, inner)) => {
            let inner = reducer.reduce_forced(inner.clone())?;

            Ok(match Term::unwrap_or_clone(inner) {
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(j, tail))) => {
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(spine.clone() + j, tail)))
                }
                inner => {
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(spine.clone(), Term::from(inner))))
                }
            })
        }
        Intrinsic::ByteType => Ok(Subterm::Intrinsic(Intrinsic::ByteType)),
        Intrinsic::Byte(value) => Ok(Subterm::Intrinsic(Intrinsic::Byte(*value))),
        Intrinsic::ByteToNat(inner) => {
            let inner = reducer.reduce_forced(inner.clone())?;
            Ok(Subterm::Intrinsic(match &*inner {
                Subterm::Intrinsic(Intrinsic::Byte(value)) => {
                    Intrinsic::Nat(Nat::new(usize::from(*value)))
                }
                _ => Intrinsic::ByteToNat(inner),
            }))
        }
        Intrinsic::NatToByte(inner) => {
            let inner = reducer.reduce_forced(inner.clone())?;
            if let Subterm::Intrinsic(Intrinsic::ByteToNat(byte)) = &*inner {
                return reducer.reduce(byte.clone()).map(Term::unwrap_or_clone);
            }

            Ok(Subterm::Intrinsic(
                match inner.as_nat().and_then(|value| {
                    let value = value.to_big_uint()?;
                    Some((value.to_u32()? & 0xff) as u8)
                }) {
                    Some(value) => Intrinsic::Byte(value),
                    None => Intrinsic::NatToByte(inner),
                },
            ))
        }
        Intrinsic::ByteEql(l, r) => {
            reduce_byte_binary(reducer, l, r, |l, r| l == r, Intrinsic::ByteEql)
        }
        Intrinsic::ByteLt(l, r) => {
            reduce_byte_binary(reducer, l, r, |l, r| l < r, Intrinsic::ByteLt)
        }
        Intrinsic::ByteLte(l, r) => {
            reduce_byte_binary(reducer, l, r, |l, r| l <= r, Intrinsic::ByteLte)
        }
        Intrinsic::ByteGt(l, r) => {
            reduce_byte_binary(reducer, l, r, |l, r| l > r, Intrinsic::ByteGt)
        }
        Intrinsic::ByteGte(l, r) => {
            reduce_byte_binary(reducer, l, r, |l, r| l >= r, Intrinsic::ByteGte)
        }
        Intrinsic::NatEql(left, right) => reduce_nat_compare(
            reducer,
            left,
            right,
            |c| match c {
                Comparison::Eq => Some(true),
                Comparison::Lt | Comparison::Gt => Some(false),
                Comparison::Le | Comparison::Ge | Comparison::Stuck => None,
            },
            Intrinsic::nat_eql,
        ),
        // Handles are opaque runtime tokens with no compile-time literal form, so this only ever reduces its operands and rebuilds — it never folds.
        Intrinsic::HandleEql(left, right) => {
            reduce_nat_binary(reducer, left, right, |_, _| None, Intrinsic::HandleEql)
        }
        Intrinsic::NatNeq(left, right) => reduce_nat_compare(
            reducer,
            left,
            right,
            |c| match c {
                Comparison::Eq => Some(false),
                Comparison::Lt | Comparison::Gt => Some(true),
                Comparison::Le | Comparison::Ge | Comparison::Stuck => None,
            },
            Intrinsic::nat_neq,
        ),
        // Addition combines the literal successor floors and recurses on the symbolic tails: `(il + sl) + (ir + sr) = (il + ir) + (sl + sr)`. A zero tail drops by the unit law; two non-zero tails stay as the neutral `add`. Lifting the combined floor back out with `rebuild` is what makes the unit laws and successor peeling *definitional* — `Nat/add(j + 1, m)` normalises to `(Nat/add(j, m)) + 1` — so an indexed constructor's target meets the motive's expected index without unification. The floor only ever moves outward, so the rewrite terminates.
        Intrinsic::NatAdd(left, right) => {
            let left = reducer.reduce_forced(left.clone())?;
            let right = reducer.reduce_forced(right.clone())?;
            let (sl, il) = Nat::decompose(&left);
            let (sr, ir) = Nat::decompose(&right);

            let inner = match (Nat::is_zero(&il), Nat::is_zero(&ir)) {
                (false, false) => Term::intrinsic(Intrinsic::nat_add(il, ir)),
                (true, _) => ir,
                (_, true) => il,
            };
            Ok(Term::unwrap_or_clone(Nat::rebuild(sl + sr, inner)))
        }
        // `(il + sl) - k` for a literal subtrahend `k`: when the floor covers it (`sl ≥ k`) the borrow stays within the floor and the tail `il ≥ 0` is untouched, so the result is `il + (sl - k)`. The subtraction twin of the addition floor law (and it gives `x - 0 = x` for any `x`, the unit law `NatAdd` already has): it turns the `succ e - 1` bounds the cons-slice rule produces back into `e`, so a slice over a symbolic cons keeps reducing instead of stalling on a stuck `Nat/sub`. Both-literal subtraction with `k` overshooting the floor truncates to zero; anything else stays neutral.
        Intrinsic::NatSub(left, right) => {
            let left = reducer.reduce_forced(left.clone())?;
            let right = reducer.reduce_forced(right.clone())?;
            // The same cancellation the comparisons take, and for the same law: a borrow never reaches what both sides carry, so `(x + a) - (x + b)` is `a - b` and the floor law below gets to see a literal subtrahend where it would otherwise have seen a sum.
            let (left, right) = Nat::cancel_common(&left, &right);
            let (sl, il) = Nat::decompose(&left);
            let (k, ir) = Nat::decompose(&right);

            if Nat::is_zero(&ir) {
                if sl >= k {
                    return Ok(Term::unwrap_or_clone(Nat::rebuild(sl - k, il)));
                }
                if Nat::is_zero(&il) {
                    return Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)));
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::nat_sub(left, right)))
        }
        // Multiplication distributes a literal factor over the other operand's successor floor: `(it + st) · c = (it · c) + (st · c)`. The literal floors multiply out; the symbolic tail rides as a neutral `mul` (or drops when it is zero, which folds two literals). The multiplicative twin of `NatAdd`'s floor law — it lets `n · k` extract `k` past a symbolic `n` (`(x + 1) · 2 = x · 2 + 2`) the same way `n + k` does. Whichever side is the literal drives; two symbolic operands have no literal factor, so the product stays neutral.
        Intrinsic::NatMul(left, right) => {
            let left = reducer.reduce_forced(left.clone())?;
            let right = reducer.reduce_forced(right.clone())?;
            let (sl, il) = Nat::decompose(&left);
            let (sr, ir) = Nat::decompose(&right);

            if Nat::is_zero(&ir) {
                // right is the literal `sr`: distribute over the left floor.
                let inner = match Nat::is_zero(&il) {
                    true => il,
                    false => Term::intrinsic(Intrinsic::nat_mul(il, right.clone())),
                };
                return Ok(Term::unwrap_or_clone(Nat::rebuild(sl * sr, inner)));
            }
            if Nat::is_zero(&il) {
                // left is the literal `sl`, right symbolic: distribute over the right floor.
                let inner = Term::intrinsic(Intrinsic::nat_mul(left.clone(), ir));
                return Ok(Term::unwrap_or_clone(Nat::rebuild(sl * sr, inner)));
            }
            Ok(Subterm::Intrinsic(Intrinsic::nat_mul(left, right)))
        }
        Intrinsic::NatLt(left, right) => reduce_nat_compare(
            reducer,
            left,
            right,
            |c| match c {
                Comparison::Lt => Some(true),
                Comparison::Eq | Comparison::Gt | Comparison::Ge => Some(false),
                Comparison::Le | Comparison::Stuck => None,
            },
            Intrinsic::nat_lt,
        ),
        Intrinsic::NatDiv(left, right) => {
            reduce_nat_division(reducer, left, right, Euclid::Quotient)
        }
        Intrinsic::NatRem(left, right) => {
            reduce_nat_division(reducer, left, right, Euclid::Remainder)
        }
        Intrinsic::NatGt(left, right) => reduce_nat_compare(
            reducer,
            left,
            right,
            |c| match c {
                Comparison::Gt => Some(true),
                Comparison::Eq | Comparison::Lt | Comparison::Le => Some(false),
                Comparison::Ge | Comparison::Stuck => None,
            },
            Intrinsic::nat_gt,
        ),
        Intrinsic::NatLte(left, right) => reduce_nat_compare(
            reducer,
            left,
            right,
            |c| match c {
                Comparison::Lt | Comparison::Eq | Comparison::Le => Some(true),
                Comparison::Gt => Some(false),
                Comparison::Ge | Comparison::Stuck => None,
            },
            Intrinsic::nat_lte,
        ),
        Intrinsic::NatGte(left, right) => reduce_nat_compare(
            reducer,
            left,
            right,
            |c| match c {
                Comparison::Gt | Comparison::Eq | Comparison::Ge => Some(true),
                Comparison::Lt => Some(false),
                Comparison::Le | Comparison::Stuck => None,
            },
            Intrinsic::nat_gte,
        ),
        // Bitwise ops fold on the unbounded ℕ the type level pretends: `and`, `or`, `xor` on the infinite binary expansion, `shl` as `· 2^n` and `shr` as `⌊·/2^n⌋`. The runtime's 31-bit carrier (truncating `shl`, logical `shr`) is imposed only in the backend, never here.
        Intrinsic::NatAnd(left, right) => reduce_nat_binary(
            reducer,
            left,
            right,
            |l, r| l.checked_bitand(r).map(Intrinsic::Nat),
            Intrinsic::NatAnd,
        ),
        Intrinsic::NatOr(left, right) => reduce_nat_binary(
            reducer,
            left,
            right,
            |l, r| l.checked_bitor(r).map(Intrinsic::Nat),
            Intrinsic::NatOr,
        ),
        Intrinsic::NatXor(left, right) => reduce_nat_binary(
            reducer,
            left,
            right,
            |l, r| l.checked_bitxor(r).map(Intrinsic::Nat),
            Intrinsic::NatXor,
        ),
        Intrinsic::NatShl(left, right) => reduce_nat_binary(
            reducer,
            left,
            right,
            |l, r| l.checked_shl(r).map(Intrinsic::Nat),
            Intrinsic::NatShl,
        ),
        Intrinsic::NatShr(left, right) => reduce_nat_binary(
            reducer,
            left,
            right,
            |l, r| l.checked_shr(r).map(Intrinsic::Nat),
            Intrinsic::NatShr,
        ),
        // The rotation and bit-count operations are 32-bit-carrier notions: they fold only a literal that fits the u32 view (the erased carrier) and stay neutral otherwise, like every other declined fold.
        Intrinsic::NatRotl(left, right) => reduce_nat_binary(
            reducer,
            left,
            right,
            |l, r| {
                let l = l.to_big_uint()?.to_u32()?;
                let r = r.to_big_uint()?.to_u32()?;
                Some(Intrinsic::Nat(Nat::new(nat_rotl(l, r) as usize)))
            },
            Intrinsic::NatRotl,
        ),
        Intrinsic::NatRotr(left, right) => reduce_nat_binary(
            reducer,
            left,
            right,
            |l, r| {
                let l = l.to_big_uint()?.to_u32()?;
                let r = r.to_big_uint()?.to_u32()?;
                Some(Intrinsic::Nat(Nat::new(nat_rotr(l, r) as usize)))
            },
            Intrinsic::NatRotr,
        ),
        Intrinsic::NatClz(inner) => reduce_nat_unary(
            reducer,
            inner,
            |n| {
                Some(Intrinsic::Nat(Nat::new(
                    n.to_big_uint()?.to_u32()?.leading_zeros() as usize,
                )))
            },
            Intrinsic::NatClz,
        ),
        Intrinsic::NatCtz(inner) => reduce_nat_unary(
            reducer,
            inner,
            |n| {
                Some(Intrinsic::Nat(Nat::new(
                    n.to_big_uint()?.to_u32()?.trailing_zeros() as usize,
                )))
            },
            Intrinsic::NatCtz,
        ),
        Intrinsic::NatPopcnt(inner) => reduce_nat_unary(
            reducer,
            inner,
            |n| {
                Some(Intrinsic::Nat(Nat::new(
                    n.to_big_uint()?.to_u32()?.count_ones() as usize,
                )))
            },
            Intrinsic::NatPopcnt,
        ),
        Intrinsic::IntType => Ok(Subterm::Intrinsic(Intrinsic::IntType)),
        Intrinsic::Int(value) => Ok(Subterm::Intrinsic(Intrinsic::Int(value.clone()))),
        Intrinsic::IntEql(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Bool(left == right)),
            Intrinsic::IntEql,
        ),
        Intrinsic::IntNeq(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Bool(left != right)),
            Intrinsic::IntNeq,
        ),
        Intrinsic::IntAdd(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Int(left + right)),
            Intrinsic::IntAdd,
        ),
        Intrinsic::IntSub(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Int(left - right)),
            Intrinsic::IntSub,
        ),
        Intrinsic::IntMul(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Int(left * right)),
            Intrinsic::IntMul,
        ),
        Intrinsic::IntDiv(left, right) => reduce_int_division(
            reducer,
            left,
            right,
            "Int/div",
            Int::checked_div,
            Intrinsic::IntDiv,
        ),
        Intrinsic::IntRem(left, right) => reduce_int_division(
            reducer,
            left,
            right,
            "Int/rem",
            Int::checked_rem,
            Intrinsic::IntRem,
        ),
        Intrinsic::IntLt(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Bool(left < right)),
            Intrinsic::IntLt,
        ),
        Intrinsic::IntGt(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Bool(left > right)),
            Intrinsic::IntGt,
        ),
        Intrinsic::IntLte(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Bool(left <= right)),
            Intrinsic::IntLte,
        ),
        Intrinsic::IntGte(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Bool(left >= right)),
            Intrinsic::IntGte,
        ),
        // Bitwise ops fold on the unbounded ℤ the type level pretends: `and`, `or`, `xor` on the infinite two's-complement expansion, `shl` as `· 2^n` and `shr` as the arithmetic `⌊·/2^n⌋`. The runtime's signed 31-bit carrier (truncating `shl`, `shr_s`) is imposed only in the backend, never here.
        Intrinsic::IntAnd(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Int(left & right)),
            Intrinsic::IntAnd,
        ),
        Intrinsic::IntOr(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Int(left | right)),
            Intrinsic::IntOr,
        ),
        Intrinsic::IntXor(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Int(left ^ right)),
            Intrinsic::IntXor,
        ),
        Intrinsic::IntShl(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| left.checked_shl(right).map(Intrinsic::Int),
            Intrinsic::IntShl,
        ),
        Intrinsic::IntShr(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| left.checked_shr(right).map(Intrinsic::Int),
            Intrinsic::IntShr,
        ),
        // 32-bit-carrier rotations and bit counts over the i32 view; a literal outside it stays neutral.
        Intrinsic::IntRotl(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |l, r| Some(Intrinsic::Int(Int::new(int_rotl(l.to_i32()?, r.to_i32()?)))),
            Intrinsic::IntRotl,
        ),
        Intrinsic::IntRotr(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |l, r| Some(Intrinsic::Int(Int::new(int_rotr(l.to_i32()?, r.to_i32()?)))),
            Intrinsic::IntRotr,
        ),
        Intrinsic::IntClz(inner) => reduce_int_unary(
            reducer,
            inner,
            |n| {
                Some(Intrinsic::Int(Int::new(
                    (n.to_i32()? as u32).leading_zeros() as i32,
                )))
            },
            Intrinsic::IntClz,
        ),
        Intrinsic::IntCtz(inner) => reduce_int_unary(
            reducer,
            inner,
            |n| {
                Some(Intrinsic::Int(Int::new(
                    (n.to_i32()? as u32).trailing_zeros() as i32,
                )))
            },
            Intrinsic::IntCtz,
        ),
        Intrinsic::IntPopcnt(inner) => reduce_int_unary(
            reducer,
            inner,
            |n| {
                Some(Intrinsic::Int(Int::new(
                    (n.to_i32()? as u32).count_ones() as i32
                )))
            },
            Intrinsic::IntPopcnt,
        ),
        Intrinsic::FltType => Ok(Subterm::Intrinsic(Intrinsic::FltType)),
        Intrinsic::Flt(flt) => Ok(Subterm::Intrinsic(Intrinsic::Flt(*flt))),
        Intrinsic::FltAdd(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltAdd)
        }
        Intrinsic::FltSub(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltSub)
        }
        Intrinsic::FltMul(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltMul)
        }
        Intrinsic::FltDiv(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltDiv)
        }
        // `%` on `f32` is C `fmod`: `x - trunc(x / y) * y`, sign of the dividend — the same value the `cont -> wasm` expansion computes.
        Intrinsic::FltRem(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltRem)
        }
        Intrinsic::FltMin(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltMin)
        }
        Intrinsic::FltMax(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltMax)
        }
        Intrinsic::FltCopysign(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltCopysign)
        }
        Intrinsic::FltEql(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltEql)
        }
        Intrinsic::FltNeq(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltNeq)
        }
        Intrinsic::FltLt(left, right) => reduce_flt_binary(reducer, left, right, Intrinsic::FltLt),
        Intrinsic::FltGt(left, right) => reduce_flt_binary(reducer, left, right, Intrinsic::FltGt),
        Intrinsic::FltLte(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltLte)
        }
        Intrinsic::FltGte(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltGte)
        }
        Intrinsic::FltNeg(inner) => reduce_flt_unary(reducer, inner, Intrinsic::FltNeg),
        Intrinsic::FltAbs(inner) => reduce_flt_unary(reducer, inner, Intrinsic::FltAbs),
        Intrinsic::FltSqrt(inner) => reduce_flt_unary(reducer, inner, Intrinsic::FltSqrt),
        Intrinsic::FltFloor(inner) => reduce_flt_unary(reducer, inner, Intrinsic::FltFloor),
        Intrinsic::FltCeil(inner) => reduce_flt_unary(reducer, inner, Intrinsic::FltCeil),
        Intrinsic::FltTrunc(inner) => reduce_flt_unary(reducer, inner, Intrinsic::FltTrunc),
        Intrinsic::FltNearest(inner) => reduce_flt_unary(reducer, inner, Intrinsic::FltNearest),
        Intrinsic::FltToLeBytes(inner) => reduce_flt_unary(reducer, inner, Intrinsic::FltToLeBytes),
        Intrinsic::FltOfLeBytes(inner) => {
            let inner = reducer.reduce_forced(inner.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::FltOfLeBytes(inner)))
        }
        // The conversions preserve the number, never the bits — a bit view belongs to explicit `Bin` casts. `Nat/to_int` is total: ℕ embeds in ℤ, and both are unbounded here. The runtime's carrier-range traps stay where they always were, at the `into_wasm` boundary.
        Intrinsic::NatToInt(inner) => reduce_nat_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Int(Int::new(v.to_big_uint()?))),
            Intrinsic::NatToInt,
        ),
        // Opaque at the type level, like every `Flt` operation: constructing a float *is* float semantics.
        Intrinsic::NatToFlt(inner) => {
            reduce_nat_unary(reducer, inner, |_| None, Intrinsic::NatToFlt)
        }
        // `Int/to_nat` of a negative literal is a value no natural holds — reported like a zero divisor, never wrapped. A symbolic operand rebuilds the neutral term.
        Intrinsic::IntToNat(inner) => {
            let span = inner.span();
            let inner = reducer.reduce_forced(inner.clone())?;
            match inner.as_int() {
                Some(value) => match value.to_big_uint() {
                    Some(number) => Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::new(number)))),
                    None => Err(ReduceError::IntToNatNegative { value, span }),
                },
                None => Ok(Subterm::Intrinsic(Intrinsic::IntToNat(inner))),
            }
        }
        Intrinsic::IntToFlt(inner) => {
            reduce_int_unary(reducer, inner, |_| None, Intrinsic::IntToFlt)
        }
        Intrinsic::FltToNat(inner) => reduce_flt_unary(reducer, inner, Intrinsic::FltToNat),
        Intrinsic::FltToInt(inner) => {
            let inner = reducer.reduce_forced(inner.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::FltToInt(inner)))
        }
        Intrinsic::BinType(Grain::X) => Ok(Subterm::Intrinsic(Intrinsic::BinType(Grain::X))),
        Intrinsic::Bin(Grain::X, bytes) => {
            Ok(Subterm::Intrinsic(Intrinsic::Bin(Grain::X, bytes.clone())))
        }
        Intrinsic::BinLen(Grain::X, bin) => {
            let bin = reducer.reduce_forced(bin.clone())?;
            reduce_homomorphism(
                reducer,
                bin_shape(Grain::X, bin),
                |run| Term::intrinsic(Intrinsic::Nat(Nat::new(run.len()))),
                nat_sum,
                |base_len, _| {
                    Term::intrinsic(Intrinsic::nat_add(
                        Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                        base_len,
                    ))
                },
                |sub| Term::intrinsic(Intrinsic::bin_len(Grain::X, sub)),
            )
        }
        Intrinsic::BinEql(Grain::X, left, right) => {
            let left = reducer.reduce_forced(left.clone())?;
            let right = reducer.reduce_forced(right.clone())?;

            // Reflexivity: any value equals itself. Catches a shared variable, which the peel below cannot — a bare variable is not a `Bin`-valued intrinsic.
            if left == right {
                return Ok(Subterm::Intrinsic(Intrinsic::Bool(true)));
            }

            // Structural decision via the free-monoid peel (`core::spine`): a peeled-equal pair is `true`, a definite byte or length clash is `false` (so `eql([1] ++ x, [2] ++ x) = false` regardless of `x`). Anything the peel leaves undecided stays neutral — the same conservative seam conversion reads, so the fold only ever strengthens, never weakens.
            if let (Subterm::Intrinsic(l), Subterm::Intrinsic(r)) = (&*left, &*right) {
                match peel_bin(l, r) {
                    Some(Peel::Equal) => return Ok(Subterm::Intrinsic(Intrinsic::Bool(true))),
                    Some(Peel::Clash) => return Ok(Subterm::Intrinsic(Intrinsic::Bool(false))),
                    Some(Peel::Continue(..)) | Some(Peel::Stuck) | None => {}
                }
            }

            Ok(Subterm::Intrinsic(Intrinsic::bin_eql(
                Grain::X,
                Term::unwrap_or_clone(left),
                Term::unwrap_or_clone(right),
            )))
        }
        Intrinsic::BinGet(Grain::X, bin, index) => {
            let bin = reducer.reduce_forced(bin.clone())?;
            let index_reduced = reducer.reduce_forced(index.clone())?;
            let i = as_index(&index_reduced);
            // A concrete index into a literal run.
            if let (Subterm::Intrinsic(Intrinsic::Bin(Grain::X, bytes)), Some(i)) = (&*bin, i) {
                return match bytes.byte(i) {
                    Some(byte) => Ok(Subterm::Intrinsic(Intrinsic::Byte(byte))),
                    None => Err(ReduceError::BinGetOutOfBounds {
                        len: bytes.len(Grain::X),
                        index: i,
                        span: index.span(),
                    }),
                };
            }
            // The cons head's byte: `get(append(x[], byte), 0) = byte` — the base case of the cons-peel below, and the partner of `BinSlice`'s rules.
            if let Subterm::Intrinsic(Intrinsic::BinAppend(Grain::X, base, byte)) = &*bin
                && let Subterm::Intrinsic(Intrinsic::Bin(Grain::X, b)) = &**base
                && b.is_empty()
                && let Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) = &*index_reduced
            {
                return reducer.reduce(byte.clone()).map(Term::unwrap_or_clone);
            }
            // A get over a cons spine peels one byte per `0`/`succ` index step: `get(cons(h, t), 0) = h`   and   `get(cons(h, t), succ k) = get(t, k)`.
            if let Some((head, tail)) = peel_first_atom(Grain::X, &bin) {
                match &*index_reduced {
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => {
                        let zero = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_get(Grain::X, head, zero)))
                            .map(Term::unwrap_or_clone);
                    }
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))) => {
                        let one = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
                        let prev = Term::intrinsic(Intrinsic::nat_sub(index_reduced.clone(), one));
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_get(Grain::X, tail, prev)))
                            .map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::bin_get(
                Grain::X,
                bin,
                index_reduced,
            )))
        }
        Intrinsic::BinSlice(Grain::X, bin, start, end) => {
            let bin = reducer.reduce_forced(bin.clone())?;
            let start_reduced = reducer.reduce_forced(start.clone())?;
            let end_reduced = reducer.reduce_forced(end.clone())?;
            // The full slice is the identity: `slice(b, 0, len b) = b`. Sound even for a symbolic `b` — `0..len` is always in range, never trapping — and the runtime partner of `core::spine`'s window-collapse: it lets a bare full-window `BinSlice` reduce to its base, so a `Bin/slice` over the whole value costs no copy and converts against the base directly.
            if matches!(
                &*start_reduced,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero))
            ) && matches!(&*end_reduced, Subterm::Intrinsic(Intrinsic::BinLen(Grain::X, whole)) if *whole == bin)
            {
                return Ok(Term::unwrap_or_clone(bin));
            }
            // The empty slice is empty: `slice(b, i, i) = x[]`. The dual of the full-window identity and equally sound — an empty range yields no bytes regardless of `b`, and never equates two distinct literals. It lets a codepoint take collapse its zero-width base (`take 0`) to the empty string even over a symbolic cons.
            if start_reduced == end_reduced {
                return Ok(Subterm::Intrinsic(Intrinsic::Bin(
                    Grain::X,
                    PackedBin::empty(),
                )));
            }
            let s = as_index(&start_reduced);
            let e = as_index(&end_reduced);
            // A concrete slice of a literal run.
            if let (Subterm::Intrinsic(Intrinsic::Bin(Grain::X, bytes)), Some(s), Some(e)) =
                (&*bin, s, e)
            {
                return match bytes.slice(Grain::X, s, e) {
                    Some(slice) => Ok(Subterm::Intrinsic(Intrinsic::Bin(Grain::X, slice))),
                    None => Err(ReduceError::BinSliceOutOfRange {
                        len: bytes.len(Grain::X),
                        start: s,
                        end: e,
                        span: start.span().or_else(|| end.span()),
                    }),
                };
            }
            // A slice over a cons spine peels one byte per `0`/`succ` boundary step — the reduction partner of the `Utf8` cons the validity proofs walk:  `slice(cons(h, t), 0, succ e) = h ++ slice(t, 0, e)`  and `slice(cons(h, t), succ s, e) = slice(t, s, e - 1)`.
            if let Some((head, tail)) = peel_first_atom(Grain::X, &bin) {
                let dec = |n: &Term| {
                    let one = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
                    Term::intrinsic(Intrinsic::nat_sub(n.clone(), one))
                };
                match (&*start_reduced, &*end_reduced) {
                    (
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)),
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))),
                    ) => {
                        let zero = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
                        let rest = Term::intrinsic(Intrinsic::bin_slice(
                            Grain::X,
                            tail,
                            zero,
                            dec(&end_reduced),
                        ));
                        let consed = Term::intrinsic(Intrinsic::bin_concat(Grain::X, [head, rest]));
                        return reducer.reduce(consed).map(Term::unwrap_or_clone);
                    }
                    (Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))), _) => {
                        let sliced = Term::intrinsic(Intrinsic::bin_slice(
                            Grain::X,
                            tail,
                            dec(&start_reduced),
                            dec(&end_reduced),
                        ));
                        return reducer.reduce(sliced).map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::bin_slice(
                Grain::X,
                bin,
                start_reduced,
                end_reduced,
            )))
        }
        Intrinsic::BinAppend(Grain::X, bin, byte) => {
            let bin = reducer.reduce_forced(bin.clone())?;
            let byte = reducer.reduce_forced(byte.clone())?;
            // A concrete byte is taken mod 256 — its low 8 bits — matching the runtime's packed-`i8` store and the optimizer's `as u8`. A symbolic operand has no `as_nat`, so it stays stuck rather than truncating.
            let n = match &*byte {
                Subterm::Intrinsic(Intrinsic::Byte(byte)) => Some(*byte),
                _ => None,
            };
            Ok(match (Term::unwrap_or_clone(bin), n) {
                (Subterm::Intrinsic(Intrinsic::Bin(Grain::X, bytes)), Some(n)) => {
                    Subterm::Intrinsic(Intrinsic::Bin(Grain::X, bytes.append_byte(n).unwrap()))
                }
                (bin, _) => Subterm::Intrinsic(Intrinsic::bin_append(Grain::X, bin, byte)),
            })
        }
        Intrinsic::BinConcat(grain, operands) => {
            let grain = *grain;
            let reduced: Vec<Term> = operands
                .iter()
                .map(|e| reducer.reduce_forced(e.clone()))
                .collect::<Result<_, _>>()?;
            // Normalise by the monoid unit/associativity laws — drop the empty identity (so `concat(x[], a)`/`concat(a, x[])` collapse to `a`), fuse an all-literal survivor set with `PackedBin::concat`, collapse a lone operand. Grain-generic: both carriers fuse in the packed representation. The definitional partner of `peel_bin`'s `x[]`-handling (`core::spine`); see `normalize_concat`.
            Ok(normalize_concat(
                reduced,
                |operand: &Term| match &**operand {
                    Subterm::Intrinsic(Intrinsic::Bin(found, bytes)) if *found == grain => {
                        Some(bytes)
                    }
                    _ => None,
                },
                |runs| Subterm::Intrinsic(Intrinsic::Bin(grain, PackedBin::concat(runs))),
                |kept| Subterm::Intrinsic(Intrinsic::BinConcat(grain, kept)),
            ))
        }
        Intrinsic::BinType(Grain::B) => Ok(Subterm::Intrinsic(Intrinsic::BinType(Grain::B))),
        Intrinsic::Bin(Grain::B, bits) => {
            Ok(Subterm::Intrinsic(Intrinsic::Bin(Grain::B, bits.clone())))
        }
        Intrinsic::BinLen(Grain::B, bin) => {
            let bin = reducer.reduce_forced(bin.clone())?;
            reduce_homomorphism(
                reducer,
                bin_shape(Grain::B, bin),
                |run| Term::intrinsic(Intrinsic::Nat(Nat::new(run.len()))),
                nat_sum,
                |base_len, _| {
                    Term::intrinsic(Intrinsic::nat_add(
                        Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                        base_len,
                    ))
                },
                |sub| Term::intrinsic(Intrinsic::bin_len(Grain::B, sub)),
            )
        }
        Intrinsic::BinEql(Grain::B, left, right) => {
            let left = reducer.reduce_forced(left.clone())?;
            let right = reducer.reduce_forced(right.clone())?;
            if left == right {
                return Ok(Subterm::Intrinsic(Intrinsic::Bool(true)));
            }
            if let (Subterm::Intrinsic(l), Subterm::Intrinsic(r)) = (&*left, &*right) {
                match peel_bin(l, r) {
                    Some(Peel::Equal) => return Ok(Subterm::Intrinsic(Intrinsic::Bool(true))),
                    Some(Peel::Clash) => return Ok(Subterm::Intrinsic(Intrinsic::Bool(false))),
                    Some(Peel::Continue(..)) | Some(Peel::Stuck) | None => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::BinEql(Grain::B, left, right)))
        }
        Intrinsic::BinGet(Grain::B, bin, index) => {
            let span = index.span();
            let bin = reducer.reduce_forced(bin.clone())?;
            let index_reduced = reducer.reduce_forced(index.clone())?;
            if let (Subterm::Intrinsic(Intrinsic::Bin(Grain::B, bits)), Some(index)) =
                (&*bin, as_index(&index_reduced))
            {
                return bits
                    .bit(index)
                    .map(|bit| Subterm::Intrinsic(Intrinsic::Bool(bit)))
                    .ok_or_else(|| ReduceError::BinGetOutOfBounds {
                        len: bits.bit_length(),
                        index,
                        span,
                    });
            }
            // The cons head's bit: `get(append(b[], bit), 0) = bit` — the base case of the cons-peel below, and the partner of `BinSlice`'s rules. Without it the peel's symbolic head chunk is this same `append(b[], bit)`, so the `0`-index step would rebuild the redex it came from until the budget exhausted.
            if let Subterm::Intrinsic(Intrinsic::BinAppend(Grain::B, base, bit)) = &*bin
                && let Subterm::Intrinsic(Intrinsic::Bin(Grain::B, b)) = &**base
                && b.is_empty()
                && let Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) = &*index_reduced
            {
                return reducer.reduce(bit.clone()).map(Term::unwrap_or_clone);
            }
            if let Some((head, tail)) = peel_first_atom(Grain::B, &bin) {
                match &*index_reduced {
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => {
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_get(
                                Grain::B,
                                head,
                                Term::intrinsic(Intrinsic::Nat(Nat::Zero)),
                            )))
                            .map(Term::unwrap_or_clone);
                    }
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))) => {
                        let prev = Term::intrinsic(Intrinsic::nat_sub(
                            index_reduced.clone(),
                            Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                        ));
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_get(Grain::B, tail, prev)))
                            .map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::BinGet(
                Grain::B,
                bin,
                index_reduced,
            )))
        }
        Intrinsic::BinSlice(Grain::B, bin, start, end) => {
            let span = start.span().or_else(|| end.span());
            let bin = reducer.reduce_forced(bin.clone())?;
            let start_reduced = reducer.reduce_forced(start.clone())?;
            let end_reduced = reducer.reduce_forced(end.clone())?;
            if matches!(
                &*start_reduced,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero))
            ) && matches!(&*end_reduced, Subterm::Intrinsic(Intrinsic::BinLen(Grain::B, whole)) if *whole == bin)
            {
                return Ok(Term::unwrap_or_clone(bin));
            }
            if start_reduced == end_reduced {
                return Ok(Subterm::Intrinsic(Intrinsic::Bin(
                    Grain::B,
                    PackedBin::empty(),
                )));
            }
            if let (Subterm::Intrinsic(Intrinsic::Bin(Grain::B, bits)), Some(start), Some(end)) =
                (&*bin, as_index(&start_reduced), as_index(&end_reduced))
            {
                return bits
                    .slice(Grain::B, start, end)
                    .map(|bits| Subterm::Intrinsic(Intrinsic::Bin(Grain::B, bits)))
                    .ok_or_else(|| ReduceError::BinSliceOutOfRange {
                        len: bits.bit_length(),
                        start,
                        end,
                        span,
                    });
            }
            if let Some((head, tail)) = peel_first_atom(Grain::B, &bin) {
                let dec = |n: &Term| {
                    Term::intrinsic(Intrinsic::nat_sub(
                        n.clone(),
                        Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                    ))
                };
                match (&*start_reduced, &*end_reduced) {
                    (
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)),
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))),
                    ) => {
                        let rest = Term::intrinsic(Intrinsic::bin_slice(
                            Grain::B,
                            tail,
                            Term::intrinsic(Intrinsic::Nat(Nat::Zero)),
                            dec(&end_reduced),
                        ));
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_concat(
                                Grain::B,
                                [head, rest],
                            )))
                            .map(Term::unwrap_or_clone);
                    }
                    (Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))), _) => {
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_slice(
                                Grain::B,
                                tail,
                                dec(&start_reduced),
                                dec(&end_reduced),
                            )))
                            .map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::BinSlice(
                Grain::B,
                bin,
                start_reduced,
                end_reduced,
            )))
        }
        Intrinsic::BinAppend(Grain::B, bin, bit) => {
            let bin = reducer.reduce_forced(bin.clone())?;
            let bit = reducer.reduce_forced(bit.clone())?;
            Ok(Subterm::Intrinsic(match (&*bin, bit.as_bool()) {
                (Subterm::Intrinsic(Intrinsic::Bin(Grain::B, bits)), Some(bit)) => {
                    Intrinsic::Bin(Grain::B, bits.append_bit(bit))
                }
                _ => Intrinsic::BinAppend(Grain::B, bin, bit),
            }))
        }
        Intrinsic::ListType(elem) => {
            let elem = reducer.reduce(elem.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::list_type(elem)))
        }
        Intrinsic::List(elem, elems) => {
            let elem = reducer.reduce(elem.clone())?;
            let elems = elems
                .iter()
                .map(|e| reducer.reduce(e.clone()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Subterm::Intrinsic(Intrinsic::List(elem, elems)))
        }
        Intrinsic::ListLen(type_, list) => {
            let type_ = reducer.reduce(type_.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            reduce_homomorphism(
                reducer,
                list_shape(list),
                |run| Term::intrinsic(Intrinsic::Nat(Nat::new(run.len()))),
                nat_sum,
                |base_len, _| {
                    Term::intrinsic(Intrinsic::nat_add(
                        Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                        base_len,
                    ))
                },
                |sub| Term::intrinsic(Intrinsic::list_len(type_.clone(), sub)),
            )
        }
        Intrinsic::ListGet(type_, list, index) => {
            let type_ = reducer.reduce(type_.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            let index_reduced = reducer.reduce_forced(index.clone())?;
            let i = as_index(&index_reduced);
            // A concrete index into a literal run.
            if let (Subterm::Intrinsic(Intrinsic::List(_, elems)), Some(i)) = (&*list, i) {
                let len = elems.len();
                return match elems.get(i).cloned().map(Term::unwrap_or_clone) {
                    Some(elem) => Ok(elem),
                    None => Err(ReduceError::ListGetOutOfBounds {
                        len,
                        index: i,
                        span: index.span(),
                    }),
                };
            }
            // A get over a cons spine peels one element per `0`/`succ` index step, the `List` twin of `BinGet`'s byte peel: `get(cons(h, t), 0) = h`   and   `get(cons(h, t), succ k) = get(t, k)`.
            if let Some((head, tail)) = peel_first_elem(&list) {
                match &*index_reduced {
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => {
                        return Ok(Term::unwrap_or_clone(head));
                    }
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))) => {
                        let one = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
                        let prev = Term::intrinsic(Intrinsic::nat_sub(index_reduced.clone(), one));
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::list_get(type_, tail, prev)))
                            .map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::list_get(
                type_,
                list,
                index_reduced,
            )))
        }
        Intrinsic::ListSlice(type_, list, start, end) => {
            let type_ = reducer.reduce(type_.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            let start_reduced = reducer.reduce_forced(start.clone())?;
            let end_reduced = reducer.reduce_forced(end.clone())?;
            // The full slice is the identity: `slice(a, 0, len a) = a`. Sound even for a symbolic `a` — `0..len` is always in range — the `List` twin of `BinSlice`'s full-window identity, letting a full-length `List/slice` reduce to its base instead of copying.
            if matches!(
                &*start_reduced,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero))
            ) && matches!(&*end_reduced, Subterm::Intrinsic(Intrinsic::ListLen(_, whole)) if *whole == list)
            {
                return Ok(Term::unwrap_or_clone(list));
            }
            // The empty slice is empty: `slice(a, i, i) = []`. Sound for a symbolic `a` — an empty range yields no elements regardless — and the base case the cons peel below bottoms out on (the `List` twin of `BinSlice`'s empty-slice identity).
            if start_reduced == end_reduced {
                return Ok(Subterm::Intrinsic(Intrinsic::List(
                    type_.clone(),
                    Vec::new(),
                )));
            }
            let s = as_index(&start_reduced);
            let e = as_index(&end_reduced);
            // A concrete slice of a literal run.
            if let (Subterm::Intrinsic(Intrinsic::List(_, elems)), Some(s), Some(e)) =
                (&*list, s, e)
            {
                return match elems.get(s..e) {
                    Some(slice) => Ok(Subterm::Intrinsic(Intrinsic::List(
                        type_.clone(),
                        slice.to_vec(),
                    ))),
                    None => Err(ReduceError::ListSliceOutOfRange {
                        len: elems.len(),
                        start: s,
                        end: e,
                        span: start.span().or_else(|| end.span()),
                    }),
                };
            }
            // A slice over a cons spine peels one element per `0`/`succ` boundary step, the `List` twin of `BinSlice`'s byte peel: `slice(cons(h, t), 0, succ e) = [h] ++ slice(t, 0, e)`  and `slice(cons(h, t), succ s, e) = slice(t, s, e - 1)`.
            if let Some((head, tail)) = peel_first_elem(&list) {
                let dec = |n: &Term| {
                    let one = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
                    Term::intrinsic(Intrinsic::nat_sub(n.clone(), one))
                };
                match (&*start_reduced, &*end_reduced) {
                    (
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)),
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))),
                    ) => {
                        let zero = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
                        let rest = Term::intrinsic(Intrinsic::list_slice(
                            type_.clone(),
                            tail,
                            zero,
                            dec(&end_reduced),
                        ));
                        let head_singleton: Term =
                            Subterm::Intrinsic(Intrinsic::List(type_.clone(), vec![head])).into();
                        let consed =
                            Term::intrinsic(Intrinsic::list_concat(type_, [head_singleton, rest]));
                        return reducer.reduce(consed).map(Term::unwrap_or_clone);
                    }
                    (Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))), _) => {
                        let sliced = Term::intrinsic(Intrinsic::list_slice(
                            type_,
                            tail,
                            dec(&start_reduced),
                            dec(&end_reduced),
                        ));
                        return reducer.reduce(sliced).map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::list_slice(
                type_,
                list,
                start_reduced,
                end_reduced,
            )))
        }
        Intrinsic::ListAppend(type_, list, elem) => {
            let type_ = reducer.reduce(type_.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            let elem = reducer.reduce(elem.clone())?;
            Ok(match Term::unwrap_or_clone(list) {
                Subterm::Intrinsic(Intrinsic::List(list_elem, mut elems)) => {
                    elems.push(elem);
                    Subterm::Intrinsic(Intrinsic::List(list_elem, elems))
                }
                list => Subterm::Intrinsic(Intrinsic::list_append(type_, list, elem)),
            })
        }
        Intrinsic::ListConcat(type_, operands) => {
            let type_ = reducer.reduce(type_.clone())?;
            let reduced: Vec<Term> = operands
                .iter()
                .map(|e| reducer.reduce_forced(e.clone()))
                .collect::<Result<_, _>>()?;
            // The `List` twin of `BinConcat` normalisation: drop the empty list (so `concat([], a)`/`concat(a, [])` collapse to `a`), fuse an all-literal survivor set into one flattened literal, collapse a lone operand — the definitional partner of `peel_arr`'s `[]`-handling (`core::spine`); see `normalize_concat`.
            fn literal(operand: &Term) -> Option<&Vec<Term>> {
                match &**operand {
                    Subterm::Intrinsic(Intrinsic::List(_, elems)) => Some(elems),
                    _ => None,
                }
            }
            Ok(normalize_concat(
                reduced,
                literal,
                |runs| {
                    Subterm::Intrinsic(Intrinsic::List(
                        type_.clone(),
                        runs.into_iter().flatten().cloned().collect(),
                    ))
                },
                |kept| Subterm::Intrinsic(Intrinsic::list_concat(type_.clone(), kept)),
            ))
        }
        // `map`: the eliminator homomorphism. The literal case applies `f` elementwise; the spine cases distribute (`map f (concat segs) = concat (map f segs)`, `map f (append b x) = append (map f b) (f x)`) — the same normal form a structural `foldr (\x ih. f x :: ih) []` produces, so map-based proofs still reduce. A symbolic list stays neutral (the `Opaque` case), so there is no unfold of a variable.
        Intrinsic::ListMap(a, b, list, f) => {
            let a = reducer.reduce(a.clone())?;
            let b = reducer.reduce(b.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            let f = reducer.reduce(f.clone())?;
            reduce_homomorphism(
                reducer,
                list_shape(list),
                |elems| {
                    Term::intrinsic(Intrinsic::List(
                        b.clone(),
                        elems
                            .into_iter()
                            .map(|x| Term::apply(f.clone(), [x]))
                            .collect(),
                    ))
                },
                |images| Term::intrinsic(Intrinsic::list_concat(b.clone(), images)),
                |base_map, generator| {
                    Term::intrinsic(Intrinsic::list_append(
                        b.clone(),
                        base_map,
                        Term::apply(f.clone(), [generator]),
                    ))
                },
                |sub| Term::intrinsic(Intrinsic::list_map(a.clone(), b.clone(), sub, f.clone())),
            )
        }
        // The handle type and handle tokens are inert values, like `Nat`/`Nat(_)`.
        Intrinsic::HandleType => Ok(Subterm::Intrinsic(Intrinsic::HandleType)),
        Intrinsic::Handle(token) => Ok(Subterm::Intrinsic(Intrinsic::Handle(*token))),
        // Every operation the host performs is an `Io`, which is to say a *description*: it denotes one inert value here and becomes a host call only at erasure, where the entrypoint boundary forces the program's description exactly once.
        //
        // These arms used to refuse instead, and the refusal was the type-level half of the effect discipline: a spelling that does not fix a value must not reach a type. It is now the typing that keeps them out — a term of non-`Io` type cannot perform an effect, and an `Io` supports no elimination through which one could reach a type position. So the operands reduce, the node rebuilds, and nothing else follows.
        Intrinsic::ProcExit(code) => {
            let code = reducer.reduce(code.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::ProcExit(code)))
        }
        Intrinsic::CellType(elem) => {
            let elem = reducer.reduce(elem.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::cell_type(elem)))
        }
        Intrinsic::Cell(type_, init) => {
            let type_ = reducer.reduce(type_.clone())?;
            let init = reducer.reduce(init.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::Cell(type_, init)))
        }
        Intrinsic::CellSet(type_, cell, value) => {
            let type_ = reducer.reduce(type_.clone())?;
            let cell = reducer.reduce(cell.clone())?;
            let value = reducer.reduce(value.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::CellSet(type_, cell, value)))
        }
        Intrinsic::CellGet(type_, cell) => {
            let type_ = reducer.reduce(type_.clone())?;
            let cell = reducer.reduce(cell.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::CellGet(type_, cell)))
        }
        Intrinsic::IoType(result) => {
            let result = reducer.reduce(result.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::io_type(result)))
        }
        // A description is an inert value: its operands reduce and the node rebuilds, and no monad law fires. `bind(pure(x), f)` is deliberately *not* definitionally `f(x)` — an `Io` supports no proof for a law to be useful about, and admitting one would make conversion decide when an effect happens.
        Intrinsic::IoPure(type_, value) => {
            let type_ = reducer.reduce(type_.clone())?;
            let value = reducer.reduce(value.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::io_pure(type_, value)))
        }
        Intrinsic::IoBind(from, to, action, f) => {
            let from = reducer.reduce(from.clone())?;
            let to = reducer.reduce(to.clone())?;
            let action = reducer.reduce(action.clone())?;
            let f = reducer.reduce(f.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::io_bind(from, to, action, f)))
        }
    }
}

#[cfg(test)]
mod tests {
    use {
        super::{
            Comparison, Reducer, compare_nat, from_ordering, nat_bound, nat_euclid_split,
            reduce_intrinsic,
        },
        crate::{Free, Intrinsic, Nat, One, ReduceError, Scope, Subterm, Term},
        curios_base::{Grain, Int, PackedBin},
        num_bigint::BigUint,
    };

    /// A reducer that reduces nothing. Every operand below is already a literal — a weak-head normal form — so no strategy is involved, and running the comparison body against an inert reducer says exactly that: the outcome is decided by the structural compare, not by anything unfolded.
    struct Inert;

    impl Reducer for Inert {
        fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
            Ok(term)
        }

        fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
            Ok(term)
        }
    }

    fn lit(n: u32) -> Term {
        Term::intrinsic(Intrinsic::Nat(Nat::new(n as usize)))
    }

    fn sym(index: u32, hint: &'static str) -> Term {
        Term::free_var(&Free::local(index, Some(hint)))
    }

    fn add(left: Term, right: Term) -> Term {
        Term::intrinsic(Intrinsic::nat_add(left, right))
    }

    fn occurrences(term: &Term, wanted: &Term) -> usize {
        Nat::summands(term)
            .iter()
            .filter(|summand| *summand == wanted)
            .count()
    }

    // What the cancellation buys the comparison family: a shared addend decides nothing, so removing it lets a stuck comparison stall on the operands that actually differ. `Le(x + a, x + b)` becoming `Le(a, b)` is what makes a decided proposition usable under a binder rather than only at literals.
    #[test]
    fn compare_nat_sees_through_a_shared_addend() {
        let (x, a, b) = (sym(0, "x"), sym(1, "a"), sym(2, "b"));

        let (outcome, left, right) = compare_nat(
            &mut Inert,
            add(x.clone(), a.clone()),
            add(x.clone(), b.clone()),
        )
        .expect("reduces");

        assert_eq!(
            outcome,
            Comparison::Stuck,
            "two distinct symbols decide nothing"
        );
        assert_eq!(
            occurrences(&left, &x),
            0,
            "the shared `x` is gone from the left"
        );
        assert_eq!(occurrences(&right, &x), 0, "and from the right");
        assert_eq!(occurrences(&left, &a), 1);
        assert_eq!(occurrences(&right, &b), 1);
    }

    // Commutativity of `+` becomes definitional for the whole comparison family, which is the larger half of what cancellation buys: nothing else in the reducer normalises the order of a sum's summands.
    #[test]
    fn compare_nat_decides_a_commuted_sum_equal() {
        let (a, b) = (sym(0, "a"), sym(1, "b"));

        let (outcome, _, _) = compare_nat(
            &mut Inert,
            add(a.clone(), b.clone()),
            add(b.clone(), a.clone()),
        )
        .expect("reduces");

        assert_eq!(
            outcome,
            Comparison::Eq,
            "`a + b` and `b + a` are the same number"
        );
    }

    // The bound every indexed loop in the standard library needs: walking `i` up to `n` under an invariant `i + k = n` asks for `i < i + kp + 1` at each step. Before cancellation that was three lemma applications in the prelude (`add_r`, `succ_of_lte`, and the transport); the comparison now decides it outright.
    #[test]
    fn compare_nat_decides_the_bound_an_indexed_loop_walks_under() {
        let (i, kp) = (sym(0, "i"), sym(1, "kp"));
        let ceiling = Nat::rebuild(1u32.into(), add(i.clone(), kp.clone()));

        let (outcome, _, _) = compare_nat(&mut Inert, i.clone(), ceiling).expect("reduces");

        assert_eq!(
            outcome,
            Comparison::Lt,
            "`i < i + kp + 1` holds for every `kp`"
        );
    }

    // Regression: `get(append(b[], x), 0)` must reduce to `x` through its own base-case arm — the cons peel's symbolic head chunk IS `append(b[], x)`, so without that arm the rewrite rebuilt the redex it came from until the step budget exhausted.
    #[test]
    fn bit_get_of_a_symbolic_cons_head_is_the_bit() {
        let bit = Term::free_var(&Free::local(0, Some("bit")));
        let empty: Term = Subterm::Intrinsic(Intrinsic::Bin(Grain::B, PackedBin::empty())).into();
        let cons = Term::intrinsic(Intrinsic::bin_append(Grain::B, empty, bit.clone()));
        let zero = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
        let get = Intrinsic::bin_get(Grain::B, cons, zero);

        let reduced = reduce_intrinsic(&mut Inert, &get).expect("reduces");

        assert_eq!(Term::from(reduced), bit);
    }

    // Soundness gate: the conversions preserve the number. `Nat/to_int` folds every literal — ℕ embeds in ℤ, both unbounded here — and `Int/to_nat` folds a non-negative to the same value and reports a negative like a zero divisor, never wrapping bits.
    #[test]
    fn conversion_folds_preserve_the_number() {
        for n in [
            0u64,
            1,
            0x3FFF_FFFF,
            0x4000_0000,
            0x7FFF_FFFF,
            0x8000_0000,
            0xFFFF_FFFF,
            0x1_0000_0000,
        ] {
            let nat = Term::intrinsic(Intrinsic::Nat(Nat::new(n)));
            let reduced = reduce_intrinsic(&mut Inert, &Intrinsic::NatToInt(nat)).expect("reduces");
            assert_eq!(
                reduced,
                Subterm::Intrinsic(Intrinsic::Int(Int::new(n))),
                "Nat/to_int changed the number on {n}",
            );
        }
        for i in [0i64, 1, 0x3FFF_FFFF, 0x7FFF_FFFF, 0x1_0000_0000] {
            let int = Term::intrinsic(Intrinsic::Int(Int::new(i)));
            let reduced = reduce_intrinsic(&mut Inert, &Intrinsic::IntToNat(int)).expect("reduces");
            assert_eq!(
                reduced,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(i as u64))),
                "Int/to_nat changed the number on {i}",
            );
        }
        for i in [-1i64, -0x4000_0000, i32::MIN as i64, i64::MIN] {
            let int = Term::intrinsic(Intrinsic::Int(Int::new(i)));
            let reduced = reduce_intrinsic(&mut Inert, &Intrinsic::IntToNat(int));
            assert!(
                matches!(reduced, Err(ReduceError::IntToNatNegative { .. })),
                "Int/to_nat failed to report the negative {i}",
            );
        }
    }

    // Soundness gate: the structural body agrees with the host ordering on every pair of literals — the decidable closed case where the two routes into a `Comparison` (the shared-inner shortcut vs. the host `cmp`) must coincide.
    #[test]
    fn compare_nat_agrees_with_literal_ordering() {
        let mut reducer = Inert;
        let samples = [0u32, 1, 2, 5, 42, 128, 255, 256, 1000];
        for &m in &samples {
            for &n in &samples {
                assert_eq!(
                    compare_nat(&mut reducer, lit(m), lit(n))
                        .expect("reduces")
                        .0,
                    from_ordering(m.cmp(&n)),
                    "compare_nat disagreed with the literal ordering on ({m}, {n})",
                );
            }
        }
    }

    /// A reducer that folds intrinsics all the way down, for the gates that must *evaluate* a rebuilt term rather than inspect its shape. `reduce_intrinsic` already reduces its own operands through this seam, so one pass suffices for the terms below.
    struct Folding;

    impl Reducer for Folding {
        fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
            match &*term {
                Subterm::Intrinsic(intrinsic) => Ok(reduce_intrinsic(self, intrinsic)?.into()),
                _ => Ok(term),
            }
        }

        fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
            self.reduce(term)
        }
    }

    fn symbol(index: u32, hint: &'static str) -> Term {
        Term::free_var(&Free::local(index, Some(hint)))
    }

    fn to_nat_of(term: Term) -> Term {
        Term::intrinsic(Intrinsic::ByteToNat(term))
    }

    fn scaled(coefficient: u32, factor: Term) -> Term {
        Term::intrinsic(Intrinsic::nat_mul(lit(coefficient), factor))
    }

    fn plus(left: Term, right: Term) -> Term {
        Term::intrinsic(Intrinsic::nat_add(left, right))
    }

    fn fold(term: Term) -> Term {
        Folding.reduce(term).expect("reduces")
    }

    // Soundness gate: `nat_bound` must never under-report, because the division split and the comparison body both turn a bound into a definitional equation — an under-report there is a false equation, not merely a wrong value. Every closed instantiation of each bounded shape must land at or below the bound the oracle states for the shape itself.
    #[test]
    fn nat_bound_upper_bounds_every_closed_instantiation() {
        let byte_shape = to_nat_of(symbol(0, "b"));
        let byte_bound = nat_bound(&byte_shape).expect("a Byte carries a bound");
        for byte in [0u8, 1, 17, 128, 254, 255] {
            let value = fold(to_nat_of(Term::intrinsic(Intrinsic::Byte(byte))));
            let value = value
                .as_nat()
                .expect("closed")
                .to_big_uint()
                .expect("literal");
            assert!(
                value <= byte_bound,
                "Byte/to_nat({byte}) exceeded its bound"
            );
        }

        for divisor in [1u32, 2, 7, 256, 1000] {
            let shape = Term::intrinsic(Intrinsic::NatRem(symbol(0, "x"), lit(divisor)));
            let bound = nat_bound(&shape).expect("a remainder carries a bound");
            for dividend in [0u32, 1, 5, 255, 999, 100_000] {
                let value = fold(Term::intrinsic(Intrinsic::NatRem(
                    lit(dividend),
                    lit(divisor),
                )));
                let value = value
                    .as_nat()
                    .expect("closed")
                    .to_big_uint()
                    .expect("literal");
                assert!(value <= bound, "{dividend} % {divisor} exceeded its bound");
            }
        }
    }

    /// `term` with the free variable `binder` replaced by `value`: close over the binder, then open at the value. Comparing *values* rather than shapes is what this gate needs — `4 · (3 · x)` and `12 · x` are the same number, and reduction does not re-associate nested literal factors.
    fn at(term: Term, binder: &Free, value: Term) -> Term {
        Scope::close(One, &[binder], term).open(&[&value])
    }

    // Soundness gate: whatever the split returns must satisfy the Euclidean specification — `n·quotient + remainder` equals the dividend at every instantiation, and the remainder is provably below `n`. Those two together *are* the definition of division, so a split passing both cannot be a false equation whatever its symbolic parts take.
    #[test]
    fn nat_euclid_split_is_a_euclidean_division() {
        let count = Free::local(0, Some("x"));
        let byte = Free::local(1, Some("b"));
        let x = Term::free_var(&count);
        let digit = to_nat_of(Term::free_var(&byte));

        let cases = [
            (fold(plus(scaled(256, x.clone()), digit.clone())), 256u32),
            (fold(plus(scaled(256, x.clone()), lit(700))), 256),
            (fold(scaled(12, x.clone())), 4),
            (fold(plus(scaled(1024, x.clone()), digit.clone())), 256),
        ];

        for (dividend, divisor) in cases {
            let n = BigUint::from(divisor);
            let (quotient, remainder) =
                nat_euclid_split(&dividend, &n).expect("these dividends split");

            assert!(
                nat_bound(&remainder).expect("a split remainder is bounded") < n,
                "the split remainder was not below {divisor}",
            );

            let rebuilt = plus(scaled(divisor, quotient), remainder);
            for sample in [0u32, 1, 7, 255, 1000] {
                let close = |term: Term| {
                    let term = at(term, &count, lit(sample));
                    fold(at(term, &byte, Term::intrinsic(Intrinsic::Byte(201))))
                };

                assert_eq!(
                    close(rebuilt.clone()),
                    close(dividend.clone()),
                    "n·quotient + remainder disagreed with the dividend at {divisor}, x = {sample}",
                );
            }
        }
    }

    // The rule the base-256 encodings need: a digit whose carrier bounds it below the divisor cannot carry, so the scaled symbol divides out exactly and the digit is the whole remainder.
    #[test]
    fn a_bounded_digit_divides_out_of_a_scaled_symbol() {
        let x = symbol(0, "x");
        let digit = to_nat_of(symbol(1, "b"));
        let dividend = fold(plus(scaled(256, x.clone()), digit.clone()));

        assert_eq!(
            fold(Term::intrinsic(Intrinsic::NatDiv(
                dividend.clone(),
                lit(256)
            ))),
            x,
        );
        assert_eq!(
            fold(Term::intrinsic(Intrinsic::NatRem(dividend, lit(256)))),
            digit,
        );
    }

    // The refusals that keep the rule sound: a coefficient the divisor does not divide could carry, and an unbounded summand could be anything at all. Both must stay neutral rather than fold.
    #[test]
    fn an_uncertain_summand_leaves_the_division_neutral() {
        let x = symbol(0, "x");
        let unbounded = plus(scaled(256, x.clone()), symbol(1, "y"));
        let indivisible = plus(scaled(100, x.clone()), to_nat_of(symbol(1, "b")));

        for dividend in [fold(unbounded), fold(indivisible)] {
            let divided = fold(Term::intrinsic(Intrinsic::NatDiv(
                dividend.clone(),
                lit(256),
            )));
            assert!(
                matches!(&*divided, Subterm::Intrinsic(Intrinsic::NatDiv(..))),
                "a division that is not forced folded anyway: {divided:?}",
            );
        }
    }

    // A bounded operand decides a comparison the floors cannot: `x % n` is a stuck remainder the structural body has nothing to say about, yet it is below `n` for every `x`.
    #[test]
    fn a_bounded_operand_decides_a_comparison_against_a_literal() {
        let mut reducer = Folding;
        let remainder = Term::intrinsic(Intrinsic::NatRem(symbol(0, "x"), lit(256)));

        assert_eq!(
            compare_nat(&mut reducer, remainder.clone(), lit(256))
                .expect("reduces")
                .0,
            Comparison::Lt,
        );
        assert_eq!(
            compare_nat(&mut reducer, remainder, lit(200))
                .expect("reduces")
                .0,
            Comparison::Stuck,
        );
    }
}
