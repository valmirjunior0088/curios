use {
    super::{ReduceError, Reducer},
    crate::{
        Cost, FUSION_CAP, Intrinsic, Located, Nat, Peel, Piece, Subterm, Term, bin_locate,
        bin_measure, bin_window, list_locate, list_measure, list_window, normalize_concat,
        peel_bin, peel_first_atom, peel_first_elem, project_erased_universes,
    },
    curios_num::{Integer, Natural, int_rotl, int_rotr, nat_rotl, nat_rotr},
    curios_utilities::{Grain, PackedBin},
    std::cmp::Ordering,
};

/// Read an already-reduced `Nat` term as a concrete `usize` index — `None` when it is still symbolic or too large to fit. The shared decode behind the `Bin`/`List` `get`/`slice` bounds.
fn as_index(term: &Term) -> Option<usize> {
    term.as_nat().and_then(|n| n.to_natural()?.to_usize())
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

/// What a packed value of `bits` logical bits costs, in its grain's own row of the price list.
///
/// One function rather than two call sites choosing a row, because the two rows differ by a factor of eight and picking the wrong one undercharges by that factor at the byte grain.
fn packed_bound(grain: Grain, bits: u64) -> Cost {
    match grain {
        Grain::X => Cost::packed_bytes(bits / 8),
        Grain::B => Cost::packed_bits(bits),
    }
}

/// What a closed binary fold on two big numbers may construct, charged before it runs.
///
/// Every operation routed through [`reduce_nat_binary`] and [`reduce_int_binary`] has a result no wider than `left + right + 1` bits: a sum is at most one bit past the wider operand, a product is exactly the two widths together, a quotient or remainder is no wider than its dividend, and a bitwise operation is no wider than the wider operand. One conservative bound rather than six exact ones, because the price list permits overcharging and forbids the opposite — six formulas would be six chances to get the direction wrong for a saving no program would notice.
///
/// **The shifts are deliberately not routed through those two**, and that is the whole reason this is a named function with a doc rather than an expression. See [`shift_bound`].
fn operand_bound(left: u64, right: u64) -> Cost {
    Cost::big_number(left.saturating_add(right).saturating_add(1))
}

/// What a closed shift may construct: the value's width plus the shift *amount*.
///
/// The one fold in the roster whose result size is not bounded by its operands' sizes, and it is reachable from three lines of surface Curios with no loop in them — `Nat/shl(1, 400000000)` builds fifty megabytes of magnitude while a transition counter sees a single step. The charge is computed from the amount before `num-bigint` is asked for anything, so the refusal happens instead of the allocation rather than after it.
///
/// `amount` is `None` when the second operand is symbolic or does not fit a `u64`, and then this charges **nothing** — because the fold declines in exactly that case and constructs nothing to charge for. It is read as a `u64` rather than a `usize` for the reason [`Natural::to_u64`](curios_num::Natural::to_u64) states: a charge that differed between the native and wasm32 targets would break the promise that a program compiling in the playground compiles at the command line.
///
/// Pricing first also closes a target divergence in the *fold*, which reads its amount through `to_usize` and therefore folds natively what it leaves neutral on wasm32. Any amount large enough for the two to disagree prices far past any budget, so both targets refuse before either reaches the shift.
fn shift_bound(value: u64, amount: Option<u64>) -> Cost {
    match amount {
        Some(amount) => Cost::big_number(value.saturating_add(amount)),
        None => Cost::NOTHING,
    }
}

/// Reduce both operands of a `Nat` binary intrinsic, then either `fold` the two literals or `rebuild` the neutral term from the reduced operands.
///
/// The fold is charged [`operand_bound`] before it runs, so every operation reaching here must have a result bounded by its operands' widths. `Nat/shl` does not and is folded by [`reduce_nat_shl`] instead.
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
        (Some(l), Some(r)) => {
            reducer.spend(operand_bound(l.bits(), r.bits()))?;

            fold(l, r)
        }
        _ => None,
    };

    Ok(Subterm::Intrinsic(match folded {
        Some(intrinsic) => intrinsic,
        None => rebuild(left, right),
    }))
}

/// `Nat/shl`, folded under [`shift_bound`] rather than [`operand_bound`].
fn reduce_nat_shl(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    let folded = match (left.as_nat(), right.as_nat()) {
        (Some(value), Some(amount)) => {
            reducer.spend(shift_bound(value.bits(), amount.to_u64()))?;

            value.checked_shl(amount).map(Intrinsic::Nat)
        }
        _ => None,
    };

    Ok(Subterm::Intrinsic(match folded {
        Some(intrinsic) => intrinsic,
        None => Intrinsic::NatShl(left, right),
    }))
}

/// `Int/shl`, the signed twin of [`reduce_nat_shl`].
fn reduce_int_shl(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    let folded = match (left.as_int(), right.as_int()) {
        (Some(value), Some(amount)) => {
            reducer.spend(shift_bound(
                value.bits(),
                amount.to_natural().and_then(|amount| amount.to_u64()),
            ))?;

            value.checked_shl(amount).map(Intrinsic::Int)
        }
        _ => None,
    };

    Ok(Subterm::Intrinsic(match folded {
        Some(intrinsic) => intrinsic,
        None => Intrinsic::IntShl(left, right),
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

    /// The neutral rebuild carries the *original* proof through unreduced. Its proposition is stated over the operands, which have only been reduced, so the two are convertible and the same proof still inhabits the rebuilt bound — reduction never has to derive one. Leaving it unreduced is deliberate besides: a bound's normal form is unobservable under proof irrelevance, and reducing into it would unfold whatever the caller proved it with, at every division this passes.
    fn rebuild(self, left: Term, right: Term, non_zero: Term) -> Intrinsic {
        match self {
            Euclid::Quotient => Intrinsic::NatDiv {
                dividend: left,
                divisor: right,
                non_zero,
            },
            Euclid::Remainder => Intrinsic::NatRem {
                dividend: left,
                divisor: right,
                non_zero,
            },
        }
    }
}

/// A statically known upper bound on every value a reduced term can take, or `None` where it has none.
///
/// Every arm is unconditional, which is what lets the callers below turn a bound into a definitional equation. A `Byte` is `0..=255` by its carrier — `Nat/to_byte` wraps and `Byte` is not a wire type, so no embedder can supply one outside the range — and `x % n < n` holds by definition, a zero divisor having already been reported. The remaining arms are monotone in operands whose own bounds this establishes.
///
/// A wrong bound here is a false definitional equation, not a wrong value: see `documentation/soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md`.
fn nat_bound(term: &Term) -> Option<Natural> {
    let Subterm::Intrinsic(intrinsic) = &**term else {
        return None;
    };

    match intrinsic {
        Intrinsic::Nat(Nat::Zero) => Some(Natural::zero()),
        Intrinsic::Nat(Nat::Succ(floor, inner)) => Some(floor + nat_bound(inner)?),
        Intrinsic::ByteToNat(_) => Some(Natural::from(u8::MAX)),
        Intrinsic::NatRem { divisor, .. } => {
            let divisor = divisor.as_nat()?.to_natural()?;
            (!divisor.is_zero()).then(|| divisor - Natural::one())
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
fn nat_literal_factor(summand: &Term) -> Option<(Natural, Term)> {
    let Subterm::Intrinsic(Intrinsic::NatMul(left, right)) = &**summand else {
        return None;
    };

    if let Some(coefficient) = left.as_nat().and_then(|value| value.to_natural()) {
        return Some((coefficient, right.clone()));
    }

    right
        .as_nat()
        .and_then(|value| value.to_natural())
        .map(|coefficient| (coefficient, left.clone()))
}

/// `coefficient · factor`, dropping a zero product and a unit coefficient rather than emitting `0 · t` or `1 · t` for reduction to clear afterwards.
fn nat_scaled(coefficient: Natural, factor: Term) -> Term {
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
fn nat_euclid_split(dividend: &Term, divisor: &Natural) -> Option<(Term, Term)> {
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
    non_zero: &Term,
    euclid: Euclid,
) -> Result<Subterm, ReduceError> {
    let span = right.span().or_else(|| left.span());
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    let divisor = right.as_nat().and_then(|divisor| divisor.to_natural());
    if divisor.as_ref().is_some_and(Natural::is_zero) {
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
            let peeled = Term::intrinsic(euclid.rebuild(
                Nat::rebuild(&floor % divisor, inner),
                right.clone(),
                non_zero.clone(),
            ));

            return Ok(Term::unwrap_or_clone(match euclid {
                Euclid::Quotient => Nat::rebuild(&floor / divisor, peeled),
                Euclid::Remainder => peeled,
            }));
        }
    }

    Ok(Subterm::Intrinsic(euclid.rebuild(
        left,
        right,
        non_zero.clone(),
    )))
}

/// `Int` counterpart of [`reduce_nat_binary`]: fold both literal operands or rebuild the neutral term. The fold is partial for the same reason — the shifts decline a negative or oversized literal shift count (`None`); the total ops just wrap their result in `Some`.
fn reduce_int_binary(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(Integer, Integer) -> Option<Intrinsic>,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    let folded = match (left.as_int(), right.as_int()) {
        (Some(l), Some(r)) => {
            reducer.spend(operand_bound(l.bits(), r.bits()))?;

            fold(l, r)
        }
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
    fold: impl FnOnce(Integer, Integer) -> Option<Integer>,
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
///
/// One fact escapes the opacity without breaching it, and `free_monoid::bin_measure` is where: `Bin/len(Flt/to_le_bytes(x))` is `4` for every `x`. That is the arity of the operation's result rather than anything about the float — it folds no value, distinguishes no `0.0` from `-0.0`, and is what makes `Flt/of_le_bytes`'s length precondition dischargeable over the operation it inverts.
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
    fold: impl FnOnce(Integer) -> Option<Intrinsic>,
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

/// The `Nat` eliminator's structural comparison, specialized to the flat `Natural` successor spine: the floors stand in for peeling successors, so no recursion is needed and two literals decide in one `Natural` compare (the literal fold folds into the shared-inner shortcut). It decides ONLY where the answer is forced and is `Stuck` otherwise — a sound partial decision procedure, the shared body of the whole comparison family. (The `lt` partner of the `Unary` eliminator's successor peel; for `Bin`/`List` the same `Comparison` shape would recurse via `uncons`.)
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
///
/// **The literal arm materializes the whole run**, one `u8` per generator — which at the bit grain is a byte per *bit*, eight times the value's own width. An operation whose result is a single `Nat` therefore allocates its entire subject to compute it, and that is why this takes a reducer: the buffer is charged before it is filled. `Bin/len` no longer reaches here for a wholly-literal value, which answers from the free monoid's measure instead, but every symbolic shape still falls through to the homomorphism and still pays this.
fn bin_shape(
    reducer: &mut impl Reducer,
    grain: Grain,
    value: Term,
) -> Result<Shape<u8>, ReduceError> {
    Ok(match Term::unwrap_or_clone(value) {
        Subterm::Intrinsic(Intrinsic::Bin(found, value)) if found == grain => {
            reducer.spend(Cost::buffer(value.len(grain) as u64))?;

            Shape::Literal(match grain {
                Grain::B => (0..value.bit_length())
                    .map(|index| u8::from(value.bit(index).unwrap()))
                    .collect(),
                Grain::X => value.to_bytes().unwrap(),
            })
        }
        Subterm::Intrinsic(Intrinsic::BinConcat {
            grain: found,
            operands,
        }) if found == grain => Shape::Concat(operands),
        Subterm::Intrinsic(Intrinsic::BinAppend {
            grain: found,
            bin: base,
            element: atom,
        }) if found == grain => Shape::Append(base, atom),
        other => Shape::Opaque(other.into()),
    })
}

/// Classify a reduced `List` value into its product shape (generators are elements).
///
/// No charge: every arm hands back storage the value already held. The literal arm moves the element vector out of a uniquely-held node, or clones its reference slots out of a shared one — which is the sharing case, since the elements themselves are reference-count bumps rather than rebuilt terms.
fn list_shape(value: Term) -> Shape<Term> {
    match Term::unwrap_or_clone(value) {
        Subterm::Intrinsic(Intrinsic::List {
            element: _,
            items: elems,
        }) => Shape::Literal(elems),
        Subterm::Intrinsic(Intrinsic::ListConcat {
            element: _,
            operands,
        }) => Shape::Concat(operands),
        Subterm::Intrinsic(Intrinsic::ListAppend {
            element: _,
            list: base,
            item: elem,
        }) => Shape::Append(base, elem),
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
        Shape::Concat(operands) => {
            // One rebuilt image node per operand, collected into one vector — the homomorphism's whole allocation, and the only arm of the four that scales with anything.
            reducer.spend(
                Cost::collection(operands.len() as u64)
                    .saturating_add(Cost::term(1).saturating_mul(operands.len() as u64)),
            )?;

            combine(operands.into_iter().map(node).collect())
        }
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
                    let value = value.to_natural()?;
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
        Intrinsic::NatDiv {
            dividend,
            divisor,
            non_zero,
        } => reduce_nat_division(reducer, dividend, divisor, non_zero, Euclid::Quotient),
        Intrinsic::NatRem {
            dividend,
            divisor,
            non_zero,
        } => reduce_nat_division(reducer, dividend, divisor, non_zero, Euclid::Remainder),
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
        Intrinsic::NatShl(left, right) => reduce_nat_shl(reducer, left, right),
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
                let l = l.to_natural()?.to_u32()?;
                let r = r.to_natural()?.to_u32()?;
                Some(Intrinsic::Nat(Nat::new(nat_rotl(l, r) as usize)))
            },
            Intrinsic::NatRotl,
        ),
        Intrinsic::NatRotr(left, right) => reduce_nat_binary(
            reducer,
            left,
            right,
            |l, r| {
                let l = l.to_natural()?.to_u32()?;
                let r = r.to_natural()?.to_u32()?;
                Some(Intrinsic::Nat(Nat::new(nat_rotr(l, r) as usize)))
            },
            Intrinsic::NatRotr,
        ),
        Intrinsic::NatClz(inner) => reduce_nat_unary(
            reducer,
            inner,
            |n| {
                Some(Intrinsic::Nat(Nat::new(
                    n.to_natural()?.to_u32()?.leading_zeros() as usize,
                )))
            },
            Intrinsic::NatClz,
        ),
        Intrinsic::NatCtz(inner) => reduce_nat_unary(
            reducer,
            inner,
            |n| {
                Some(Intrinsic::Nat(Nat::new(
                    n.to_natural()?.to_u32()?.trailing_zeros() as usize,
                )))
            },
            Intrinsic::NatCtz,
        ),
        Intrinsic::NatPopcnt(inner) => reduce_nat_unary(
            reducer,
            inner,
            |n| {
                Some(Intrinsic::Nat(Nat::new(
                    n.to_natural()?.to_u32()?.count_ones() as usize,
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
        Intrinsic::IntDiv {
            dividend,
            divisor,
            non_zero,
        } => reduce_int_division(
            reducer,
            dividend,
            divisor,
            "Int/div",
            Integer::checked_div,
            |dividend, divisor| Intrinsic::IntDiv {
                dividend,
                divisor,
                non_zero: non_zero.clone(),
            },
        ),
        Intrinsic::IntRem {
            dividend,
            divisor,
            non_zero,
        } => reduce_int_division(
            reducer,
            dividend,
            divisor,
            "Int/rem",
            Integer::checked_rem,
            |dividend, divisor| Intrinsic::IntRem {
                dividend,
                divisor,
                non_zero: non_zero.clone(),
            },
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
        Intrinsic::IntShl(left, right) => reduce_int_shl(reducer, left, right),
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
            |l, r| {
                Some(Intrinsic::Int(Integer::from(int_rotl(
                    l.to_i32()?,
                    r.to_i32()?,
                ))))
            },
            Intrinsic::IntRotl,
        ),
        Intrinsic::IntRotr(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |l, r| {
                Some(Intrinsic::Int(Integer::from(int_rotr(
                    l.to_i32()?,
                    r.to_i32()?,
                ))))
            },
            Intrinsic::IntRotr,
        ),
        Intrinsic::IntClz(inner) => reduce_int_unary(
            reducer,
            inner,
            |n| {
                Some(Intrinsic::Int(Integer::from(
                    (n.to_i32()? as u32).leading_zeros() as i32,
                )))
            },
            Intrinsic::IntClz,
        ),
        Intrinsic::IntCtz(inner) => reduce_int_unary(
            reducer,
            inner,
            |n| {
                Some(Intrinsic::Int(Integer::from(
                    (n.to_i32()? as u32).trailing_zeros() as i32,
                )))
            },
            Intrinsic::IntCtz,
        ),
        Intrinsic::IntPopcnt(inner) => reduce_int_unary(
            reducer,
            inner,
            |n| {
                Some(Intrinsic::Int(Integer::from(
                    (n.to_i32()? as u32).count_ones() as i32,
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
        Intrinsic::FltOfLeBytes { bin, four_bytes } => {
            let bin = reducer.reduce_forced(bin.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::FltOfLeBytes {
                bin,
                four_bytes: four_bytes.clone(),
            }))
        }
        // The conversions preserve the number, never the bits — a bit view belongs to explicit `Bin` casts. `Nat/to_int` is total: ℕ embeds in ℤ, and both are unbounded here. The runtime's carrier-range traps stay where they always were, at the `into_wasm` boundary.
        Intrinsic::NatToInt(inner) => reduce_nat_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Int(Integer::from(v.to_natural()?))),
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
                Some(value) => match value.to_natural() {
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
            // The measure answers a wholly-literal spine by folding it, without rebuilding a `Bin/len` per operand and handing each back to the reducer — which is what made a length over a deep concatenation cost a re-walk of every sub-spine. It agrees with the homomorphism below by construction on the shapes it accepts (a literal run's length, summed over a concatenation's operands) and declines everything else, so every other value reduces exactly as it did.
            if let Some(total) = bin_measure(Grain::X, &bin) {
                return Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::new(total))));
            }
            let shape = bin_shape(reducer, Grain::X, bin)?;

            reduce_homomorphism(
                reducer,
                shape,
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
        Intrinsic::BinGet {
            grain: Grain::X,
            bin,
            index,
        } => {
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
            if let Subterm::Intrinsic(Intrinsic::BinAppend {
                grain: Grain::X,
                bin: base,
                element: byte,
            }) = &*bin
                && let Subterm::Intrinsic(Intrinsic::Bin(Grain::X, b)) = &**base
                && b.is_empty()
                && let Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) = &*index_reduced
            {
                return reducer.reduce(byte.clone()).map(Term::unwrap_or_clone);
            }
            // A get over a cons spine peels one byte per `0`/`succ` index step: `get(cons(h, t), 0) = h`   and   `get(cons(h, t), succ k) = get(t, k)`.
            // Locate the index by the operands' own lengths rather than peeling one generator at a time. A peel walks the whole spine to expose one generator and rebuilds the rest, so reading an index costs a pass per generator ahead of it; the measure reaches the operand holding it in one pass and indexes within that operand alone. `None` means some operand's length is not statically known, which is what the peel below is for.
            if let Some(i) = i {
                match bin_locate(Grain::X, &bin, i) {
                    Some(Located::At(operand, local)) => {
                        let local = Term::intrinsic(Intrinsic::Nat(Nat::new(local)));
                        let operand = operand.clone();
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_get(
                                Grain::X,
                                operand,
                                local,
                            )))
                            .map(Term::unwrap_or_clone);
                    }
                    Some(Located::Past(len)) => {
                        return Err(ReduceError::BinGetOutOfBounds {
                            len,
                            index: i,
                            span: index.span(),
                        });
                    }
                    None => {}
                }
            }
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
        Intrinsic::BinSlice {
            grain: Grain::X,
            bin,
            start,
            end,
        } => {
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
            // Locate the window by the operands' own lengths. Every operand it covers whole is handed back untouched and shares its payload; only the two at the edges are narrowed, and everything outside the window is dropped without being read.
            if let (Some(s), Some(e)) = (s, e) {
                match bin_window(Grain::X, &bin, s, e) {
                    Some(Ok(pieces)) => {
                        let parts: Vec<Term> = pieces
                            .into_iter()
                            .map(|piece| match piece {
                                Piece::Whole(operand) => operand.clone(),
                                Piece::Part(operand, lo, hi) => {
                                    Term::intrinsic(Intrinsic::bin_slice(
                                        Grain::X,
                                        operand.clone(),
                                        Term::intrinsic(Intrinsic::Nat(Nat::new(lo))),
                                        Term::intrinsic(Intrinsic::Nat(Nat::new(hi))),
                                    ))
                                }
                            })
                            .collect();
                        reducer.spend(Cost::collection(parts.len() as u64))?;

                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_concat(Grain::X, parts)))
                            .map(Term::unwrap_or_clone);
                    }
                    Some(Err(len)) => {
                        return Err(ReduceError::BinSliceOutOfRange {
                            len,
                            start: s,
                            end: e,
                            span: start.span().or_else(|| end.span()),
                        });
                    }
                    None => {}
                }
            }
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
        Intrinsic::BinAppend {
            grain: Grain::X,
            bin,
            element: byte,
        } => {
            let bin = reducer.reduce_forced(bin.clone())?;
            let byte = reducer.reduce_forced(byte.clone())?;
            // A concrete byte is taken mod 256 — its low 8 bits — matching the runtime's packed-`i8` store and the optimizer's `as u8`. A symbolic operand has no `as_nat`, so it stays stuck rather than truncating.
            let n = match &*byte {
                Subterm::Intrinsic(Intrinsic::Byte(byte)) => Some(*byte),
                _ => None,
            };
            Ok(match (Term::unwrap_or_clone(bin), n) {
                (Subterm::Intrinsic(Intrinsic::Bin(Grain::X, bytes)), Some(n)) => {
                    // Twice the whole rebuilt value: `append_byte` copies the base out with `to_bytes` and then copies the extended run into a fresh buffer. Appending one byte therefore costs the length of everything appended so far, twice — which is the shape that makes a naive accumulation quadratic, and the reason it is charged rather than treated as an increment.
                    reducer.spend(
                        packed_bound(Grain::X, bytes.bit_length() as u64 + 8).saturating_mul(2),
                    )?;

                    Subterm::Intrinsic(Intrinsic::Bin(Grain::X, bytes.append_byte(n).unwrap()))
                }
                (bin, _) => Subterm::Intrinsic(Intrinsic::bin_append(Grain::X, bin, byte)),
            })
        }
        Intrinsic::BinConcat { grain, operands } => {
            let grain = *grain;
            let reduced: Vec<Term> = operands
                .iter()
                .map(|e| reducer.reduce_forced(e.clone()))
                .collect::<Result<_, _>>()?;
            // Normalise by the monoid unit/associativity laws — drop the empty identity (so `concat(x[], a)`/`concat(a, x[])` collapse to `a`), fuse an all-literal survivor set with `PackedBin::concat`, collapse a lone operand. Grain-generic: both carriers fuse in the packed representation. The definitional partner of `peel_bin`'s `x[]`-handling (`core::spine`); see `normalize_concat`.
            //
            // A run past `FUSION_CAP` declines to lend itself, so the concatenation keeps its node instead of copying both operands into a third. Measured in the grain's own generators, which is what makes one constant serve both: a bit-grain operand is capped at 64 bits and a byte-grain one at 64 bytes, and the corpus reaches neither.
            // The reduced operand vector, and the survivor vector the normalizer filters out of it — two collections whose length is the operand count, charged together before either exists.
            reducer.spend(Cost::collection(reduced.len() as u64).saturating_mul(2))?;

            normalize_concat(
                reduced,
                |operand: &Term| match &**operand {
                    Subterm::Intrinsic(Intrinsic::Bin(found, bytes))
                        if *found == grain && bytes.len(grain) <= FUSION_CAP =>
                    {
                        Some(bytes)
                    }
                    _ => None,
                },
                |runs| {
                    // Twice the fused payload, per the price list's last paragraph: `PackedBin::concat` fills a `Vec<u8>` and then converts it into an `Arc<[u8]>`, which allocates a second buffer of the same length. The operation costs two payloads even though one survives.
                    let bits = runs
                        .iter()
                        .map(|run| run.bit_length() as u64)
                        .fold(0u64, u64::saturating_add);
                    reducer.spend(packed_bound(grain, bits).saturating_mul(2))?;

                    Ok(Subterm::Intrinsic(Intrinsic::Bin(
                        grain,
                        PackedBin::concat(runs),
                    )))
                },
                |kept| {
                    Subterm::Intrinsic(Intrinsic::BinConcat {
                        grain,
                        operands: kept,
                    })
                },
            )
        }
        Intrinsic::BinType(Grain::B) => Ok(Subterm::Intrinsic(Intrinsic::BinType(Grain::B))),
        Intrinsic::Bin(Grain::B, bits) => {
            Ok(Subterm::Intrinsic(Intrinsic::Bin(Grain::B, bits.clone())))
        }
        Intrinsic::BinLen(Grain::B, bin) => {
            let bin = reducer.reduce_forced(bin.clone())?;
            if let Some(total) = bin_measure(Grain::B, &bin) {
                return Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::new(total))));
            }
            let shape = bin_shape(reducer, Grain::B, bin)?;

            reduce_homomorphism(
                reducer,
                shape,
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
        Intrinsic::BinGet {
            grain: Grain::B,
            bin,
            index,
        } => {
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
            if let Subterm::Intrinsic(Intrinsic::BinAppend {
                grain: Grain::B,
                bin: base,
                element: bit,
            }) = &*bin
                && let Subterm::Intrinsic(Intrinsic::Bin(Grain::B, b)) = &**base
                && b.is_empty()
                && let Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) = &*index_reduced
            {
                return reducer.reduce(bit.clone()).map(Term::unwrap_or_clone);
            }
            // Locate the index by the operands' own lengths rather than peeling one generator at a time. A peel walks the whole spine to expose one generator and rebuilds the rest, so reading an index costs a pass per generator ahead of it; the measure reaches the operand holding it in one pass and indexes within that operand alone. `None` means some operand's length is not statically known, which is what the peel below is for.
            if let Some(i) = as_index(&index_reduced) {
                match bin_locate(Grain::B, &bin, i) {
                    Some(Located::At(operand, local)) => {
                        let local = Term::intrinsic(Intrinsic::Nat(Nat::new(local)));
                        let operand = operand.clone();
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_get(
                                Grain::B,
                                operand,
                                local,
                            )))
                            .map(Term::unwrap_or_clone);
                    }
                    Some(Located::Past(len)) => {
                        return Err(ReduceError::BinGetOutOfBounds {
                            len,
                            index: i,
                            span,
                        });
                    }
                    None => {}
                }
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
            Ok(Subterm::Intrinsic(Intrinsic::BinGet {
                grain: Grain::B,
                bin,
                index: index_reduced,
            }))
        }
        Intrinsic::BinSlice {
            grain: Grain::B,
            bin,
            start,
            end,
        } => {
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
            // Locate the window by the operands' own lengths. Every operand it covers whole is handed back untouched and shares its payload; only the two at the edges are narrowed, and everything outside the window is dropped without being read.
            if let (Some(s), Some(e)) = (as_index(&start_reduced), as_index(&end_reduced)) {
                match bin_window(Grain::B, &bin, s, e) {
                    Some(Ok(pieces)) => {
                        let parts: Vec<Term> = pieces
                            .into_iter()
                            .map(|piece| match piece {
                                Piece::Whole(operand) => operand.clone(),
                                Piece::Part(operand, lo, hi) => {
                                    Term::intrinsic(Intrinsic::bin_slice(
                                        Grain::B,
                                        operand.clone(),
                                        Term::intrinsic(Intrinsic::Nat(Nat::new(lo))),
                                        Term::intrinsic(Intrinsic::Nat(Nat::new(hi))),
                                    ))
                                }
                            })
                            .collect();
                        reducer.spend(Cost::collection(parts.len() as u64))?;

                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_concat(Grain::B, parts)))
                            .map(Term::unwrap_or_clone);
                    }
                    Some(Err(len)) => {
                        return Err(ReduceError::BinSliceOutOfRange {
                            len,
                            start: s,
                            end: e,
                            span,
                        });
                    }
                    None => {}
                }
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
            Ok(Subterm::Intrinsic(Intrinsic::BinSlice {
                grain: Grain::B,
                bin,
                start: start_reduced,
                end: end_reduced,
            }))
        }
        Intrinsic::BinAppend {
            grain: Grain::B,
            bin,
            element: bit,
        } => {
            let bin = reducer.reduce_forced(bin.clone())?;
            let bit = reducer.reduce_forced(bit.clone())?;
            let appended = match (&*bin, bit.as_bool()) {
                (Subterm::Intrinsic(Intrinsic::Bin(Grain::B, bits)), Some(bit)) => {
                    // `append_bit` rebuilds the whole value through `from_bits`, which materializes a `bool` per bit — eight units of scratch for every one the result holds — before packing it and copying that into a fresh buffer. The value row plus a buffer eight times its width is what that comes to.
                    let width = bits.bit_length() as u64 + 1;
                    reducer.spend(
                        packed_bound(Grain::B, width)
                            .saturating_mul(2)
                            .saturating_add(Cost::buffer(width)),
                    )?;

                    Intrinsic::Bin(Grain::B, bits.append_bit(bit))
                }
                _ => Intrinsic::BinAppend {
                    grain: Grain::B,
                    bin,
                    element: bit,
                },
            };

            Ok(Subterm::Intrinsic(appended))
        }
        Intrinsic::ListType(elem) => {
            let elem = reducer.reduce(elem.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::list_type(elem)))
        }
        Intrinsic::List {
            element: elem,
            items: elems,
        } => {
            let elem = reducer.reduce(elem.clone())?;
            reducer.spend(Cost::collection(elems.len() as u64))?;
            let elems = elems
                .iter()
                .map(|e| reducer.reduce(e.clone()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Subterm::Intrinsic(Intrinsic::List {
                element: elem,
                items: elems,
            }))
        }
        Intrinsic::ListLen {
            element: type_,
            list,
        } => {
            let type_ = reducer.reduce(type_.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            if let Some(total) = list_measure(&list) {
                return Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::new(total))));
            }
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
        Intrinsic::ListGet {
            element: type_,
            list,
            index,
        } => {
            let type_ = reducer.reduce(type_.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            let index_reduced = reducer.reduce_forced(index.clone())?;
            let i = as_index(&index_reduced);
            // A concrete index into a literal run.
            if let (
                Subterm::Intrinsic(Intrinsic::List {
                    element: _,
                    items: elems,
                }),
                Some(i),
            ) = (&*list, i)
            {
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
            // The `List` twin of `BinGet`'s locator: reach the segment holding the index by the segments' own lengths, then index within it, rather than peeling one element at a time.
            if let Some(i) = i {
                match list_locate(&list, i) {
                    Some(Located::At(operand, local)) => {
                        let local = Term::intrinsic(Intrinsic::Nat(Nat::new(local)));
                        let operand = operand.clone();
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::list_get(type_, operand, local)))
                            .map(Term::unwrap_or_clone);
                    }
                    Some(Located::Past(len)) => {
                        return Err(ReduceError::ListGetOutOfBounds {
                            len,
                            index: i,
                            span: index.span(),
                        });
                    }
                    None => {}
                }
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
        Intrinsic::ListSlice {
            element: type_,
            list,
            start,
            end,
        } => {
            let type_ = reducer.reduce(type_.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            let start_reduced = reducer.reduce_forced(start.clone())?;
            let end_reduced = reducer.reduce_forced(end.clone())?;
            // The full slice is the identity: `slice(a, 0, len a) = a`. Sound even for a symbolic `a` — `0..len` is always in range — the `List` twin of `BinSlice`'s full-window identity, letting a full-length `List/slice` reduce to its base instead of copying.
            if matches!(
                &*start_reduced,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero))
            ) && matches!(&*end_reduced, Subterm::Intrinsic(Intrinsic::ListLen { element: _, list: whole }) if *whole == list)
            {
                return Ok(Term::unwrap_or_clone(list));
            }
            // The empty slice is empty: `slice(a, i, i) = []`. Sound for a symbolic `a` — an empty range yields no elements regardless — and the base case the cons peel below bottoms out on (the `List` twin of `BinSlice`'s empty-slice identity).
            if start_reduced == end_reduced {
                return Ok(Subterm::Intrinsic(Intrinsic::List {
                    element: type_.clone(),
                    items: Vec::new(),
                }));
            }
            let s = as_index(&start_reduced);
            let e = as_index(&end_reduced);
            // A concrete slice of a literal run.
            if let (
                Subterm::Intrinsic(Intrinsic::List {
                    element: _,
                    items: elems,
                }),
                Some(s),
                Some(e),
            ) = (&*list, s, e)
            {
                return match elems.get(s..e) {
                    Some(slice) => {
                        reducer.spend(Cost::collection(slice.len() as u64))?;

                        Ok(Subterm::Intrinsic(Intrinsic::List {
                            element: type_.clone(),
                            items: slice.to_vec(),
                        }))
                    }
                    None => Err(ReduceError::ListSliceOutOfRange {
                        len: elems.len(),
                        start: s,
                        end: e,
                        span: start.span().or_else(|| end.span()),
                    }),
                };
            }
            // The `List` twin of `BinSlice`'s locator: the window's segments, each already narrowed to its overlap.
            if let (Some(s), Some(e)) = (as_index(&start_reduced), as_index(&end_reduced)) {
                match list_window(&list, s, e) {
                    Some(Ok(pieces)) => {
                        let parts: Vec<Term> = pieces
                            .into_iter()
                            .map(|piece| match piece {
                                Piece::Whole(operand) => operand.clone(),
                                Piece::Part(operand, lo, hi) => {
                                    Term::intrinsic(Intrinsic::list_slice(
                                        type_.clone(),
                                        operand.clone(),
                                        Term::intrinsic(Intrinsic::Nat(Nat::new(lo))),
                                        Term::intrinsic(Intrinsic::Nat(Nat::new(hi))),
                                    ))
                                }
                            })
                            .collect();
                        reducer.spend(Cost::collection(parts.len() as u64))?;

                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::list_concat(type_, parts)))
                            .map(Term::unwrap_or_clone);
                    }
                    Some(Err(len)) => {
                        return Err(ReduceError::ListSliceOutOfRange {
                            len,
                            start: s,
                            end: e,
                            span: start.span().or_else(|| end.span()),
                        });
                    }
                    None => {}
                }
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
                        let head_singleton: Term = Subterm::Intrinsic(Intrinsic::List {
                            element: type_.clone(),
                            items: vec![head],
                        })
                        .into();
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
        Intrinsic::ListAppend {
            element: type_,
            list,
            item: elem,
        } => {
            let type_ = reducer.reduce(type_.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            let elem = reducer.reduce(elem.clone())?;
            let appended = match Term::unwrap_or_clone(list) {
                Subterm::Intrinsic(Intrinsic::List {
                    element: list_elem,
                    items: mut elems,
                }) => {
                    // Growing a vector reallocates it, so the whole extended run is charged rather than the one slot appended — the same reason `BinAppend` charges its whole rebuilt value.
                    reducer.spend(Cost::collection(elems.len() as u64 + 1))?;
                    elems.push(elem);

                    Subterm::Intrinsic(Intrinsic::List {
                        element: list_elem,
                        items: elems,
                    })
                }
                list => Subterm::Intrinsic(Intrinsic::list_append(type_, list, elem)),
            };

            Ok(appended)
        }
        Intrinsic::ListConcat {
            element: type_,
            operands,
        } => {
            let type_ = reducer.reduce(type_.clone())?;
            let reduced: Vec<Term> = operands
                .iter()
                .map(|e| reducer.reduce_forced(e.clone()))
                .collect::<Result<_, _>>()?;
            // The `List` twin of `BinConcat` normalisation: drop the empty list (so `concat([], a)`/`concat(a, [])` collapse to `a`), fuse an all-literal survivor set into one flattened literal, collapse a lone operand — the definitional partner of `peel_arr`'s `[]`-handling (`core::spine`); see `normalize_concat`.
            // A run past `FUSION_CAP` declines to lend itself, exactly as on the `Bin` side, so a growing accumulation stops flattening its element vector into a longer one every step.
            fn literal(operand: &Term) -> Option<&Vec<Term>> {
                match &**operand {
                    Subterm::Intrinsic(Intrinsic::List {
                        element: _,
                        items: elems,
                    }) if elems.len() <= FUSION_CAP => Some(elems),
                    _ => None,
                }
            }
            reducer.spend(Cost::collection(reduced.len() as u64).saturating_mul(2))?;

            normalize_concat(
                reduced,
                literal,
                |runs| {
                    // One flattened vector of every operand's elements, each a retained reference rather than a rebuilt term — so this is the collection row and not the term row, and the elements it clones are reference-count bumps.
                    let slots = runs
                        .iter()
                        .map(|run| run.len() as u64)
                        .fold(0u64, u64::saturating_add);
                    reducer.spend(Cost::collection(slots))?;

                    Ok(Subterm::Intrinsic(Intrinsic::List {
                        element: type_.clone(),
                        items: runs.into_iter().flatten().cloned().collect(),
                    }))
                },
                |kept| Subterm::Intrinsic(Intrinsic::list_concat(type_.clone(), kept)),
            )
        }
        // `map`: the eliminator homomorphism. The literal case applies `f` elementwise; the spine cases distribute (`map f (concat segs) = concat (map f segs)`, `map f (append b x) = append (map f b) (f x)`) — the same normal form a structural `foldr (\x ih. f x :: ih) []` produces, so map-based proofs still reduce. A symbolic list stays neutral (the `Opaque` case), so there is no unfold of a variable.
        Intrinsic::ListMap {
            from: a,
            to: b,
            list,
            function: f,
        } => {
            let a = reducer.reduce(a.clone())?;
            let b = reducer.reduce(b.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            let f = reducer.reduce(f.clone())?;
            reduce_homomorphism(
                reducer,
                list_shape(list),
                |elems| {
                    Term::intrinsic(Intrinsic::List {
                        element: b.clone(),
                        items: elems
                            .into_iter()
                            .map(|x| Term::apply(f.clone(), [x]))
                            .collect(),
                    })
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
        Intrinsic::Cell {
            element: type_,
            initial: init,
        } => {
            let type_ = reducer.reduce(type_.clone())?;
            let init = reducer.reduce(init.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::Cell {
                element: type_,
                initial: init,
            }))
        }
        Intrinsic::CellSet {
            element: type_,
            cell,
            value,
        } => {
            let type_ = reducer.reduce(type_.clone())?;
            let cell = reducer.reduce(cell.clone())?;
            let value = reducer.reduce(value.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::CellSet {
                element: type_,
                cell,
                value,
            }))
        }
        Intrinsic::CellGet {
            element: type_,
            cell,
        } => {
            let type_ = reducer.reduce(type_.clone())?;
            let cell = reducer.reduce(cell.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::CellGet {
                element: type_,
                cell,
            }))
        }
        Intrinsic::IoType(result) => {
            let result = reducer.reduce(result.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::io_type(result)))
        }
        // A description is an inert value: its operands reduce and the node rebuilds, and no monad law fires. `bind(pure(x), f)` is deliberately *not* definitionally `f(x)` — an `Io` supports no proof for a law to be useful about, and admitting one would make conversion decide when an effect happens.
        Intrinsic::IoPure {
            result: type_,
            value,
        } => {
            let type_ = reducer.reduce(type_.clone())?;
            let value = reducer.reduce(value.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::io_pure(type_, value)))
        }
        Intrinsic::IoBind {
            from,
            to,
            action,
            continuation: f,
        } => {
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
        crate::{
            Category, Cost, Free, Intrinsic, Nat, One, Peel, ReduceError, Scope, Subterm, Term,
            peel_bin, peel_list, peel_nat,
        },
        curios_num::{Integer, Natural},
        curios_utilities::{Grain, PackedBin},
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

        /// Unbudgeted: these fixtures are about what a fold *decides*, and a limit would only decide it a second time.
        fn spend(&mut self, _cost: Cost) -> Result<(), ReduceError> {
            Ok(())
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

    // The bound every indexed loop in the standard library needs: walking `i` up to `n` under an invariant `i + k = n` asks for `i < i + kp + 1` at each step. Before cancellation that was three lemma applications in the prelude (`add_r`, `succ_of_ind`, and the transport); the comparison now decides it outright.
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
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(n))),
                "Nat/to_int changed the number on {n}",
            );
        }
        for i in [0i64, 1, 0x3FFF_FFFF, 0x7FFF_FFFF, 0x1_0000_0000] {
            let int = Term::intrinsic(Intrinsic::Int(Integer::from(i)));
            let reduced = reduce_intrinsic(&mut Inert, &Intrinsic::IntToNat(int)).expect("reduces");
            assert_eq!(
                reduced,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(i as u64))),
                "Int/to_nat changed the number on {i}",
            );
        }
        for i in [-1i64, -0x4000_0000, i32::MIN as i64, i64::MIN] {
            let int = Term::intrinsic(Intrinsic::Int(Integer::from(i)));
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

        /// Unbudgeted, for [`Inert`]'s reason.
        fn spend(&mut self, _cost: Cost) -> Result<(), ReduceError> {
            Ok(())
        }
    }

    /// [`Folding`] under a budget, for the gates whose subject is a *charge* rather than a value.
    struct Budgeted {
        remaining: u64,
    }

    impl Reducer for Budgeted {
        fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
            match &*term {
                Subterm::Intrinsic(intrinsic) => Ok(reduce_intrinsic(self, intrinsic)?.into()),
                _ => Ok(term),
            }
        }

        fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
            self.reduce(term)
        }

        fn spend(&mut self, cost: Cost) -> Result<(), ReduceError> {
            if cost.is_refused() {
                return Err(ReduceError::exhausted(self.remaining, cost));
            }

            match self.remaining.checked_sub(cost.get()) {
                Some(remaining) => {
                    self.remaining = remaining;
                    Ok(())
                }
                None => Err(ReduceError::exhausted(self.remaining, cost)),
            }
        }
    }

    /// A shift's result is `bits(value) + amount` wide and the amount is a *value*, so no operand size bounds it. The charge is computed from the amount and refused before `num-bigint` is asked for anything — which is the difference between a diagnostic and an allocation the process may not survive.
    ///
    /// The two arms differ only in the shift amount, and the affordable one establishes that the refusal is about size rather than about the operation.
    #[test]
    fn an_oversized_shift_is_refused_before_it_is_built() {
        let shift = |amount: usize| {
            Term::intrinsic(Intrinsic::NatShl(
                lit(1),
                Term::intrinsic(Intrinsic::Nat(Nat::new(amount))),
            ))
        };

        let mut reducer = Budgeted { remaining: 1_000 };
        assert_eq!(
            reducer.reduce(shift(40)),
            Ok(Term::intrinsic(Intrinsic::Nat(Nat::new(1usize << 40_u32))))
        );

        let mut reducer = Budgeted { remaining: 1_000 };
        assert_eq!(
            reducer.reduce(shift(1 << 30)),
            Err(ReduceError::Exhausted {
                category: Category::Limbs,
                remaining: 1_000,
                // The value's own width plus the amount: `1` is one bit wide, so the charge is a bit past the shift itself.
                attempted: Cost::big_number(1 + (1 << 30)).get(),
            }),
            "the refusal names the row it was refused on, and the size it was refused at"
        );
    }

    /// The refusal is target-independent, which a shift priced through `usize` would not be: `usize` is 32 bits on wasm32 and 64 natively, so an amount between the two would be folded on one target and left neutral on the other. Any such amount prices at 2^32 bits or more — sixty-seven million units before the value's own width — which no shippable budget affords, so both targets refuse.
    ///
    /// The budget below is a thousand times the shipped default and still refuses, which is the margin that makes "no shippable budget" a claim rather than a hope. **It is also a live regression guard**: with the charge removed, this test does not fail, it *aborts* — `memory allocation of 2305843009213693960 bytes failed`, which is what the fold does when nothing stops it.
    #[test]
    fn a_shift_amount_past_a_host_index_is_refused_rather_than_folded() {
        let huge = Term::intrinsic(Intrinsic::NatShl(
            lit(1),
            Term::intrinsic(Intrinsic::Nat(Nat::new(Natural::from(u64::MAX)))),
        ));

        let mut reducer = Budgeted {
            remaining: 1_000_000_000,
        };
        assert_eq!(
            reducer.reduce(huge),
            Err(ReduceError::Exhausted {
                category: Category::Limbs,
                remaining: 1_000_000_000,
                attempted: Cost::big_number(u64::MAX).get(),
            })
        );
    }

    /// What reducing `term` costs a fresh [`Budgeted`], for the gates whose subject is a charge.
    fn charged(term: Term) -> u64 {
        const AMPLE: u64 = 1_000_000_000;
        let mut reducer = Budgeted { remaining: AMPLE };
        reducer.reduce(term).expect("the subject reduces");

        AMPLE - reducer.remaining
    }

    /// Bytes are not the only protected payload. Each subject below builds a result whose logical size its operands decide, and each is charged **at least** that size — so a carrier priced as a constant would show up here as a charge that does not cover what it built.
    ///
    /// At least, rather than exactly: a fold pays for traversing its operands as well as for its result, and the price list is an upper bound rather than an equality. The lower bound is the half that matters, because undercharging is the direction that loses the property.
    ///
    /// The `Bin` subject appends rather than concatenates, deliberately: `FUSION_CAP` stops a concatenation fusing past 64 generators, so past that it builds *nothing* and correctly charges nothing. An append has no cap and rebuilds its whole value, which is the shape whose price has to scale.
    #[test]
    fn every_payload_carrier_is_charged_for_at_least_what_it_builds() {
        const BITS: usize = 4_096;
        const DIGITS: usize = 1_000;
        const ELEMENTS: usize = 32;

        let appended_bits = Term::intrinsic(Intrinsic::BinAppend {
            grain: Grain::B,
            bin: Term::intrinsic(Intrinsic::Bin(
                Grain::B,
                PackedBin::from_bits((0..BITS).map(|index| index % 2 == 0)),
            )),
            element: Term::intrinsic(Intrinsic::Bool(true)),
        });

        let wide = Integer::from(
            Natural::parse_bytes(&vec![b'9'; DIGITS], 10).expect("a decimal numeral"),
        );
        let product = Term::intrinsic(Intrinsic::IntMul(
            Term::intrinsic(Intrinsic::Int(wide.clone())),
            Term::intrinsic(Intrinsic::Int(wide)),
        ));

        let joined = Term::intrinsic(Intrinsic::ListConcat {
            element: nat_type(),
            operands: vec![run_of(ELEMENTS), run_of(ELEMENTS)],
        });

        // A decimal digit is a little over three and a third bits; rounding down keeps this a lower bound.
        let digit_bits = (DIGITS as u64 * 33) / 10;

        for (carrier, charge, built) in [
            ("bits", charged(appended_bits), BITS as u64 / 64),
            ("integer", charged(product), digit_bits * 2 / 64),
            ("list", charged(joined), ELEMENTS as u64 * 2),
        ] {
            assert!(
                charge >= built,
                "{carrier}: charged {charge} for a result of {built} units"
            );
        }
    }

    /// A window over a value builds no payload — it takes a reference count on the buffer somebody else owns — so slicing a large run costs about what slicing a small one costs. This is the sharing half of the audit's central distinction, and the half a price list gets wrong by charging for every value it touches rather than every value it builds.
    #[test]
    fn a_window_charges_for_no_payload_it_did_not_build() {
        let slice = |n: usize| {
            Term::intrinsic(Intrinsic::BinSlice {
                grain: Grain::X,
                bin: Term::intrinsic(Intrinsic::Bin(
                    Grain::X,
                    PackedBin::from_bytes(vec![7u8; n]),
                )),
                start: Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
                end: Term::intrinsic(Intrinsic::Nat(Nat::new(4usize))),
            })
        };

        assert_eq!(charged(slice(8)), charged(slice(8_000)));
    }

    /// A literal run of `n` naturals.
    fn run_of(n: usize) -> Term {
        Term::intrinsic(Intrinsic::List {
            element: nat_type(),
            items: (0..n).map(|i| lit(i as u32)).collect(),
        })
    }

    fn nat_type() -> Term {
        Term::intrinsic(Intrinsic::NatType)
    }

    /// A stand-in for a discharged bound. Reduction never inspects one — proof irrelevance makes its value unobservable, and these tests are about the fold laws rather than the obligation — so every bounded operation below states it with the same name.
    fn qed() -> Term {
        symbol(9_999, "qed")
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
                .to_natural()
                .expect("literal");
            assert!(
                value <= byte_bound,
                "Byte/to_nat({byte}) exceeded its bound"
            );
        }

        for divisor in [1u32, 2, 7, 256, 1000] {
            let shape = Term::intrinsic(Intrinsic::NatRem {
                dividend: symbol(0, "x"),
                divisor: lit(divisor),
                non_zero: qed(),
            });
            let bound = nat_bound(&shape).expect("a remainder carries a bound");
            for dividend in [0u32, 1, 5, 255, 999, 100_000] {
                let value = fold(Term::intrinsic(Intrinsic::NatRem {
                    dividend: lit(dividend),
                    divisor: lit(divisor),
                    non_zero: qed(),
                }));
                let value = value
                    .as_nat()
                    .expect("closed")
                    .to_natural()
                    .expect("literal");
                assert!(value <= bound, "{dividend} % {divisor} exceeded its bound");
            }
        }
    }

    /// `term` with the free variable `binder` replaced by `value`: close over the binder, then open at the value. Comparing *values* rather than shapes is what this gate needs — `4 · (3 · x)` and `12 · x` are the same number, and reduction does not re-associate nested literal factors.
    fn at(term: Term, binder: &Free, value: Term) -> Term {
        Scope::close(One, &[binder], term).open(&[&value])
    }

    fn as_nat(term: &Term) -> Nat {
        match &**term {
            Subterm::Intrinsic(Intrinsic::Nat(nat)) => nat.clone(),
            _ => unreachable!("a folded `Nat` carrying a successor floor"),
        }
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
            let n = Natural::from(divisor);
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

    // Soundness gate for the peel's own verdicts over values, which nothing stated before this: `Nat::cancel_common` decides all three, and the perimeter grades the law behind them argued in code comments only.
    //
    // Each verdict is believed by a different consumer, so each has its own obligation. A `Peel::Equal` reaches conversion as a definitional equation, and congruence carries a false one to `False`. A `Peel::Clash` reaches inversion as *impossible*, which excuses an omitted arm — the vacuous-elimination route. `Peel::Continue` is the one with no property stated anywhere, and it needs the strongest: the caller compares the residuals and reports *their* verdict as the original pair's, so the residuals must be equi-satisfiable with the pair they replaced, not merely implied by it. A residual pair that disagreed where the originals agreed would turn a later clash into a clash on the originals.
    //
    // So each verdict is checked against ground truth at every closed instantiation of its symbols, which is the only thing that can distinguish a valid equation from a plausible one. The grid reaches what cancelling *summands* newly decides rather than the successor spine alone: a commuted sum, a summand carried at multiplicity two, a floor surviving over shared summands, and two spellings of one number that share no summand syntactically. `Peel::Stuck` is asserted unreachable here, which is the claim `peel_nat`'s documentation makes and no fixture held.
    #[test]
    fn every_nat_peel_verdict_holds_at_every_closed_instantiation() {
        let (first, second) = (Free::local(0, Some("x")), Free::local(1, Some("y")));
        let (x, y) = (Term::free_var(&first), Term::free_var(&second));

        let value_at = |term: &Term, a: u32, b: u32| {
            let closed = fold(at(at(term.clone(), &first, lit(a)), &second, lit(b)));
            let (floor, inner) = Nat::decompose(&closed);
            assert!(Nat::is_zero(&inner), "a closed Nat folds to a literal");
            floor
        };

        let cases = [
            (
                "x + y + 1 ~ y + x + 1",
                fold(plus(plus(x.clone(), y.clone()), lit(1))),
                fold(plus(plus(y.clone(), x.clone()), lit(1))),
            ),
            (
                "x + 2 ~ x + 1",
                fold(plus(x.clone(), lit(2))),
                fold(plus(x.clone(), lit(1))),
            ),
            (
                "x + 1 ~ y + 1",
                fold(plus(x.clone(), lit(1))),
                fold(plus(y.clone(), lit(1))),
            ),
            (
                "x + x + 1 ~ x + 1",
                fold(plus(plus(x.clone(), x.clone()), lit(1))),
                fold(plus(x.clone(), lit(1))),
            ),
            (
                "x + y + 3 ~ y + 1",
                fold(plus(plus(x.clone(), y.clone()), lit(3))),
                fold(plus(y.clone(), lit(1))),
            ),
            (
                "x + x + y + 1 ~ x + y + 1",
                fold(plus(plus(plus(x.clone(), x.clone()), y.clone()), lit(1))),
                fold(plus(plus(x.clone(), y.clone()), lit(1))),
            ),
            ("0 ~ x + 1", lit(0), fold(plus(x.clone(), lit(1)))),
            (
                "2·x + 1 ~ x + x + 1",
                fold(plus(scaled(2, x.clone()), lit(1))),
                fold(plus(plus(x.clone(), x.clone()), lit(1))),
            ),
        ];

        let (mut equal, mut clash, mut carried) = (0, 0, 0);

        for (label, left, right) in cases {
            let peel = peel_nat(&as_nat(&left), &as_nat(&right));

            match &peel {
                Peel::Equal => equal += 1,
                Peel::Clash => clash += 1,
                Peel::Continue(..) => carried += 1,
                Peel::Stuck => {}
            }

            for a in [0u32, 1, 2, 5] {
                for b in [0u32, 1, 2, 5] {
                    let agree = value_at(&left, a, b) == value_at(&right, a, b);

                    match &peel {
                        Peel::Equal => assert!(
                            agree,
                            "`{label}` was decided equal but differs at x = {a}, y = {b}"
                        ),
                        Peel::Clash => assert!(
                            !agree,
                            "`{label}` was decided impossible but holds at x = {a}, y = {b}"
                        ),
                        Peel::Continue(residual_left, residual_right) => assert_eq!(
                            value_at(residual_left, a, b) == value_at(residual_right, a, b),
                            agree,
                            "`{label}`'s residuals disagree with the pair they replaced at x = {a}, y = {b}",
                        ),
                        Peel::Stuck => unreachable!("`{label}`: peeling a `Nat` never declines"),
                    }
                }
            }
        }

        // Every verdict above holds vacuously of a grid that reaches only one of them, and `Continue` is the one a shape falls to when nothing fires — so a grid that decided nothing would pass while checking nothing. This is the count that says otherwise, and it is an assertion rather than a comment because the perimeter's own record is that inert rules are what hide defects.
        assert_eq!(
            (equal, clash, carried),
            (1, 3, 4),
            "the grid stopped reaching every peel verdict",
        );
    }

    // What matching summands *up to universe instances* actually decides, stated over a term rather than in a comment. Two occurrences of one polymorphic name carry independently minted instances, so `Nat::cancel_common` and `compare_nat` both key through `project_erased_universes` to stop a bound mentioning one from stalling forever — and the equation that buys is this one: two summands the ordinary structural comparison would refuse, because it reaches the differing levels, are cancelled against each other and the sums decide equal.
    //
    // **The premise is that no `Nat` value can depend on a level, and the projection is unsound the moment that stops holding.** The same projection, read as a quotient by definitional equality, is what let the certifier's refinement key certify a coercion between distinct types, because a *type* can depend on a level: `Type u` embeds one in a term, so `wrap(Type u)` is genuinely two values at two instances (`curios-cert`'s `recheck::tests::a_case_equation_does_not_refine_an_occurrence_at_another_universe_instance`). What stops the same argument here is narrower than the comment beside the code claims: not that instances are erased before anything runs, but that Core offers no elimination from a type or a level into a `Nat`, so the two summands below denote one number however they are spelled. Adding one — any intrinsic reading a level or a sort as a count — makes this fixture a witness rather than a record.
    //
    // The control is the second pair, which shares that shape in every respect except the one that matters: distinct *arguments* under one instance must not cancel, or the key would be collapsing terms wholesale rather than levels.
    #[test]
    fn summands_cancel_across_a_universe_instance_and_not_across_an_argument() {
        let instanced =
            |level: u32| Term::universe_inst(symbol(0, "g"), vec![crate::Level::constant(level)]);

        let peel = peel_nat(
            &as_nat(&fold(plus(instanced(0), lit(1)))),
            &as_nat(&fold(plus(instanced(1), lit(1)))),
        );

        assert!(
            matches!(peel, Peel::Equal),
            "`g<0> + 1` and `g<1> + 1` differ only in a level, which is not part of a number",
        );

        let applied = |argument: Term| Term::apply(instanced(0), vec![argument]);

        let peel = peel_nat(
            &as_nat(&fold(plus(applied(symbol(1, "x")), lit(1)))),
            &as_nat(&fold(plus(applied(symbol(2, "y")), lit(1)))),
        );

        assert!(
            matches!(peel, Peel::Continue(..)),
            "`g<0>(x) + 1` and `g<0>(y) + 1` are undecided, not one number",
        );
    }

    // The rule the base-256 encodings need: a digit whose carrier bounds it below the divisor cannot carry, so the scaled symbol divides out exactly and the digit is the whole remainder.
    #[test]
    fn a_bounded_digit_divides_out_of_a_scaled_symbol() {
        let x = symbol(0, "x");
        let digit = to_nat_of(symbol(1, "b"));
        let dividend = fold(plus(scaled(256, x.clone()), digit.clone()));

        assert_eq!(
            fold(Term::intrinsic(Intrinsic::NatDiv {
                dividend: dividend.clone(),
                divisor: lit(256),
                non_zero: qed(),
            })),
            x,
        );
        assert_eq!(
            fold(Term::intrinsic(Intrinsic::NatRem {
                dividend,
                divisor: lit(256),
                non_zero: qed(),
            })),
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
            let divided = fold(Term::intrinsic(Intrinsic::NatDiv {
                dividend: dividend.clone(),
                divisor: lit(256),
                non_zero: qed(),
            }));
            assert!(
                matches!(&*divided, Subterm::Intrinsic(Intrinsic::NatDiv { .. })),
                "a division that is not forced folded anyway: {divided:?}",
            );
        }
    }

    // A bounded operand decides a comparison the floors cannot: `x % n` is a stuck remainder the structural body has nothing to say about, yet it is below `n` for every `x`.
    #[test]
    fn a_bounded_operand_decides_a_comparison_against_a_literal() {
        let mut reducer = Folding;
        let remainder = Term::intrinsic(Intrinsic::NatRem {
            dividend: symbol(0, "x"),
            divisor: lit(256),
            non_zero: qed(),
        });

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

    fn run_bytes(run: &[u8]) -> Term {
        Term::intrinsic(Intrinsic::Bin(
            Grain::X,
            PackedBin::from_bytes(run.to_vec()),
        ))
    }

    /// The same five bytes, spelled four ways: whole, split once, split twice, and left-nested the way an accumulation builds one.
    fn groupings(whole: &[u8]) -> Vec<Term> {
        let nest = |left: Term, right: Term| {
            Term::intrinsic(Intrinsic::BinConcat {
                grain: Grain::X,
                operands: vec![left, right],
            })
        };

        vec![
            run_bytes(whole),
            nest(run_bytes(&whole[..2]), run_bytes(&whole[2..])),
            Term::intrinsic(Intrinsic::BinConcat {
                grain: Grain::X,
                operands: vec![
                    run_bytes(&whole[..1]),
                    run_bytes(&whole[1..3]),
                    run_bytes(&whole[3..]),
                ],
            }),
            nest(
                nest(run_bytes(&whole[..1]), run_bytes(&whole[1..2])),
                run_bytes(&whole[2..]),
            ),
        ]
    }

    /// **What `Bin/len` now answers from has to agree with what the run actually is.** The measure replaced computing a length by rebuilding a `Bin/len` per operand and handing each back to the reducer; a length is a definitional equation, so a measure that disagreed with the run would be a false one, and congruence carries a false equation to `False`. Ground truth here is the fused literal's own byte count, so this pins the measure to the representation rather than to itself — and it varies the grouping, including the left-nested shape an accumulation builds, because grouping is exactly what the measure must not be able to see.
    #[test]
    fn a_length_does_not_depend_on_how_its_run_is_grouped() {
        let whole: &[u8] = &[0x30, 0x31, 0x32, 0x33, 0x34];

        for spelling in groupings(whole) {
            let length = reduce_intrinsic(&mut Folding, &Intrinsic::BinLen(Grain::X, spelling))
                .expect("a length over a literal run reduces");

            assert_eq!(
                length,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(whole.len()))),
                "every grouping of the same run is the same length",
            );
        }
    }

    /// The control the test above would be worthless without: an operand the measure cannot read must send the length back to the homomorphism rather than be silently skipped. Skipping one is the sharp failure — it would report `2` for a value at least two bytes long, which is a false equation in the admitting direction.
    #[test]
    fn an_unmeasurable_operand_is_not_skipped() {
        let spine = Term::intrinsic(Intrinsic::BinConcat {
            grain: Grain::X,
            operands: vec![run_bytes(&[0x30, 0x31]), symbol(0, "b")],
        });

        let length = reduce_intrinsic(&mut Folding, &Intrinsic::BinLen(Grain::X, spine))
            .expect("a length over a symbolic tail reduces to a neutral sum");

        assert_ne!(
            length,
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(2usize))),
            "the literal prefix is not the whole length",
        );
    }

    /// **A window located by operand lengths has to be the window.** `Bin/slice` now reaches its result by measuring the operands and narrowing the two at the edges, rather than peeling one byte at a time; the two must agree for every grouping, or slicing would depend on how a value was spelled.
    #[test]
    fn a_window_over_a_spine_is_the_window_over_its_run() {
        let whole: &[u8] = &[0x30, 0x31, 0x32, 0x33, 0x34];

        for spelling in groupings(whole) {
            for (start, end) in [(0usize, 5usize), (0, 2), (1, 4), (2, 5), (3, 3), (4, 5)] {
                let window = Intrinsic::bin_slice(
                    Grain::X,
                    spelling.clone(),
                    lit(start as u32),
                    lit(end as u32),
                );

                let sliced = reduce_intrinsic(&mut Folding, &window).expect("a window reduces");

                assert_eq!(
                    sliced,
                    Subterm::Intrinsic(Intrinsic::Bin(
                        Grain::X,
                        PackedBin::from_bytes(whole[start..end].to_vec()),
                    )),
                    "the window {start}..{end} is the same bytes however the run is grouped",
                );
            }
        }
    }

    /// The index twin of the window test: reading a byte must not depend on the grouping either.
    #[test]
    fn an_index_into_a_spine_reads_the_same_byte() {
        let whole: &[u8] = &[0x30, 0x31, 0x32, 0x33, 0x34];

        for spelling in groupings(whole) {
            for (index, expected) in whole.iter().enumerate() {
                let read = Intrinsic::bin_get(Grain::X, spelling.clone(), lit(index as u32));
                let byte = reduce_intrinsic(&mut Folding, &read).expect("an index reduces");

                assert_eq!(
                    byte,
                    Subterm::Intrinsic(Intrinsic::Byte(*expected)),
                    "index {index} is the same byte however the run is grouped",
                );
            }
        }
    }

    /// The `List` twin of the three tests above, in one: its carrier flattens element vectors where `Bin` copies packed bytes, and the walks are written separately, so agreement on one is not agreement on the other. Elements are symbols, compared syntactically — which is all the property needs, since regrouping never changes an element, only which run it sits in.
    fn list_of(elements: &[u32]) -> Term {
        Term::intrinsic(Intrinsic::List {
            element: symbol(1000, "T"),
            items: elements.iter().map(|n| symbol(*n, "e")).collect(),
        })
    }

    #[test]
    fn a_list_length_window_and_index_do_not_depend_on_grouping() {
        let whole: &[u32] = &[1, 2, 3, 4, 5];
        let elem = symbol(1000, "T");
        let concat = |parts: Vec<Term>| {
            Term::intrinsic(Intrinsic::ListConcat {
                element: elem.clone(),
                operands: parts,
            })
        };

        let groupings = [
            list_of(whole),
            concat(vec![list_of(&whole[..2]), list_of(&whole[2..])]),
            concat(vec![
                concat(vec![list_of(&whole[..1]), list_of(&whole[1..2])]),
                list_of(&whole[2..]),
            ]),
        ];

        for spelling in groupings {
            let length = reduce_intrinsic(
                &mut Folding,
                &Intrinsic::ListLen {
                    element: elem.clone(),
                    list: spelling.clone(),
                },
            )
            .expect("a length over an element run reduces");

            assert_eq!(
                length,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(whole.len()))),
                "every grouping of the same run is the same length",
            );

            for (start, end) in [(0usize, 5usize), (1, 4), (2, 5), (3, 3)] {
                let window = Intrinsic::list_slice(
                    elem.clone(),
                    spelling.clone(),
                    lit(start as u32),
                    lit(end as u32),
                );
                let sliced = reduce_intrinsic(&mut Folding, &window).expect("a window reduces");

                assert_eq!(
                    sliced,
                    Term::unwrap_or_clone(list_of(&whole[start..end])),
                    "the window {start}..{end} is the same elements however the run is grouped",
                );
            }

            for (index, expected) in whole.iter().enumerate() {
                let read = Intrinsic::list_get(elem.clone(), spelling.clone(), lit(index as u32));
                let element = reduce_intrinsic(&mut Folding, &read).expect("an index reduces");

                assert_eq!(
                    element,
                    Term::unwrap_or_clone(symbol(*expected, "e")),
                    "index {index} is the same element however the run is grouped",
                );
            }
        }
    }

    /// A literal `List` run of `Nat` literals — [`list_of`]'s twin for the grids below, whose ground truth needs closed elements rather than symbols.
    fn nat_list(elements: &[u32]) -> Term {
        Term::intrinsic(Intrinsic::List {
            element: symbol(1000, "T"),
            items: elements.iter().map(|n| lit(*n)).collect(),
        })
    }

    /// Fold a closed `Bin` term to its literal run — the ground truth the peel grids compare, so it must land on a literal or the instantiation was not closed.
    fn bin_value(term: Term) -> Term {
        let folded = fold(term);
        assert!(
            matches!(
                &*folded,
                Subterm::Intrinsic(Intrinsic::Bin(Grain::X, _))
                    | Subterm::Intrinsic(Intrinsic::Byte(_))
            ),
            "a closed instantiation folds to a literal, got {folded:?}",
        );
        folded
    }

    /// Fold a closed `List` term to its element run, folding each element too — a fused literal keeps its elements as written, so `[2 + 3]` and `[5]` are one value and must compare as one.
    fn list_value(term: Term) -> Vec<Term> {
        let folded = fold(term);
        match &*folded {
            Subterm::Intrinsic(Intrinsic::List {
                element: _,
                items: elems,
            }) => elems.iter().map(|elem| fold(elem.clone())).collect(),
            other => unreachable!("a closed List folds to a literal run, got {other:?}"),
        }
    }

    // Soundness gate for the `Bin` peel's verdicts over values — the `Bin` half of what `every_nat_peel_verdict_holds_at_every_closed_instantiation` states for `Nat`, written because the perimeter graded these laws argued in code comments only. The obligations are the same three. A `Peel::Equal` reaches conversion as a definitional equation, and congruence carries a false one to `False`. A `Peel::Clash` reaches inversion as *impossible*, which excuses an omitted arm — the vacuous-elimination route. A `Peel::Continue`'s residuals must be equi-satisfiable with the pair they replaced, since the caller compares the residuals and reports their verdict as the originals'. `Peel::Stuck` promises nothing and is only tallied.
    //
    // The shapes reach the laws the code comments assert and nothing else stated: symbolic chunks cancelling by syntactic equality with a byte clash surviving past them, window fusion across a shared seam (`slice(w, s, m) ++ slice(w, m, e) = slice(w, s, e)`), the empty-window drop (`slice(w, i, i)` vanishing), append-as-concatenation (`append(b, c) = b ++ append(x[], c)`), and a near-miss control beside each: windows meeting at no seam must not fuse, and a one-byte symbolic cons against the identity stays undecided. Ground truth is the folded value at every closed instantiation of the symbols — instantiations respect `/sys/slice`'s `s <= e <= len(b)` preconditions, since a program outside them cannot be written, and that typing fact is exactly what makes the window laws unconditional.
    //
    // Mutation-checked: fusing two windows of one base without the seam check (`*seam == lo` dropped from `push`) turns the no-seam control into a false `Equal` and this grid fails it at the first anchor whose seam bytes differ. The tally is the anti-inertness assertion the perimeter asks of a sole-reach fixture: `Stuck` is where a pair falls when nothing fires, so a grid that decided nothing would otherwise pass while checking nothing.
    #[test]
    fn every_bin_peel_verdict_holds_at_every_closed_instantiation() {
        let bin_left = Free::local(0, Some("x"));
        let bin_right = Free::local(1, Some("y"));
        let byte_free = Free::local(2, Some("c"));
        let anchor_free = Free::local(3, Some("w"));
        let x = Term::free_var(&bin_left);
        let y = Term::free_var(&bin_right);
        let w = Term::free_var(&anchor_free);
        let c = Term::free_var(&byte_free);

        let cat = |parts: Vec<Term>| {
            Term::intrinsic(Intrinsic::BinConcat {
                grain: Grain::X,
                operands: parts,
            })
        };
        let window = |lo: u32, hi: u32| {
            Term::intrinsic(Intrinsic::bin_slice(Grain::X, w.clone(), lit(lo), lit(hi)))
        };
        let chunk = Term::intrinsic(Intrinsic::bin_append(Grain::X, run_bytes(&[]), c.clone()));

        let cases = [
            (
                "x ++ x[05] ~ x ++ x[05]",
                cat(vec![x.clone(), run_bytes(&[5])]),
                cat(vec![x.clone(), run_bytes(&[5])]),
            ),
            (
                "x[05] ++ x ~ x[09] ++ x",
                cat(vec![run_bytes(&[5]), x.clone()]),
                cat(vec![run_bytes(&[9]), x.clone()]),
            ),
            (
                "x ++ x[05] ~ x ++ x[09]",
                cat(vec![x.clone(), run_bytes(&[5])]),
                cat(vec![x.clone(), run_bytes(&[9])]),
            ),
            (
                "x[05] ++ x ~ x[05] ++ y",
                cat(vec![run_bytes(&[5]), x.clone()]),
                cat(vec![run_bytes(&[5]), y.clone()]),
            ),
            (
                "x[0509] ~ x[05] ++ x",
                run_bytes(&[5, 9]),
                cat(vec![run_bytes(&[5]), x.clone()]),
            ),
            (
                "x[05] ++ x ~ x ++ x[05]",
                cat(vec![run_bytes(&[5]), x.clone()]),
                cat(vec![x.clone(), run_bytes(&[5])]),
            ),
            (
                "append(x, c) ~ x ++ append(x[], c)",
                Term::intrinsic(Intrinsic::bin_append(Grain::X, x.clone(), c.clone())),
                cat(vec![x.clone(), chunk.clone()]),
            ),
            (
                "slice(w, 0, 2) ++ slice(w, 2, 4) ~ slice(w, 0, 4)",
                cat(vec![window(0, 2), window(2, 4)]),
                window(0, 4),
            ),
            (
                "slice(w, 0, 2) ++ slice(w, 3, 4) ~ slice(w, 0, 4)",
                cat(vec![window(0, 2), window(3, 4)]),
                window(0, 4),
            ),
            (
                "slice(w, 1, 1) ++ x ~ x ++ slice(w, 2, 2)",
                cat(vec![window(1, 1), x.clone()]),
                cat(vec![x.clone(), window(2, 2)]),
            ),
            (
                "append(x[], c) ++ x ~ append(x[], c) ++ y",
                cat(vec![chunk.clone(), x.clone()]),
                cat(vec![chunk.clone(), y.clone()]),
            ),
            ("append(x[], c) ~ x[]", chunk.clone(), run_bytes(&[])),
        ];

        let as_intrinsic = |term: &Term| match &**term {
            Subterm::Intrinsic(intrinsic) => intrinsic.clone(),
            other => unreachable!("every side of the grid is an intrinsic, got {other:?}"),
        };

        let runs: [&[u8]; 5] = [&[], &[5], &[9], &[9, 8], &[1, 1]];
        let anchors: [&[u8]; 2] = [&[9, 8, 7, 6], &[9, 8, 7, 7, 3]];

        let (mut equal, mut clash, mut carried, mut stuck) = (0, 0, 0, 0);

        for (label, left, right) in &cases {
            let peel =
                peel_bin(&as_intrinsic(left), &as_intrinsic(right)).expect("two Bin values peel");

            match &peel {
                Peel::Equal => equal += 1,
                Peel::Clash => clash += 1,
                Peel::Continue(..) => carried += 1,
                Peel::Stuck => stuck += 1,
            }

            for left_run in runs {
                for right_run in runs {
                    for byte_value in [0u8, 7, 255] {
                        for anchor in anchors {
                            let close = |term: &Term| {
                                let term = at(term.clone(), &bin_left, run_bytes(left_run));
                                let term = at(term, &bin_right, run_bytes(right_run));
                                let term = at(
                                    term,
                                    &byte_free,
                                    Term::intrinsic(Intrinsic::Byte(byte_value)),
                                );
                                bin_value(at(term, &anchor_free, run_bytes(anchor)))
                            };

                            let agree = close(left) == close(right);

                            match &peel {
                                Peel::Equal => assert!(
                                    agree,
                                    "`{label}` was decided equal but differs at x = {left_run:?}, y = {right_run:?}, c = {byte_value}, w = {anchor:?}",
                                ),
                                Peel::Clash => assert!(
                                    !agree,
                                    "`{label}` was decided impossible but holds at x = {left_run:?}, y = {right_run:?}, c = {byte_value}, w = {anchor:?}",
                                ),
                                Peel::Continue(residual_left, residual_right) => assert_eq!(
                                    close(residual_left) == close(residual_right),
                                    agree,
                                    "`{label}`'s residuals disagree with the pair they replaced at x = {left_run:?}, y = {right_run:?}, c = {byte_value}, w = {anchor:?}",
                                ),
                                Peel::Stuck => {}
                            }
                        }
                    }
                }
            }
        }

        assert_eq!(
            (equal, clash, carried, stuck),
            (4, 2, 3, 3),
            "the grid stopped reaching every peel verdict",
        );
    }

    // The `List` half of the grid above, separate because the two carriers differ exactly where a copied rule would be wrong: `List` literals hold *terms*, so two leading runs whose heads differ syntactically are NOT a clash — the elements may still be convertible — while a leftover run against the exhausted identity is still a definite length clash whatever its elements are. The first shape pins that difference over values: `[a + b]` and `[b + a]` denote one list at every instantiation, so the `Bin` byte-disagreement rule applied here would be a false impossibility, which is the vacuous-elimination route to `False`. Mutation-checked: clashing two differing literal heads the way `peel_bin` does fails that shape at its first instantiation. The other shapes and the tally mirror the `Bin` grid's obligations: append-as-concatenation with a symbolic element, window fusion over the element carrier, a genuine length clash, and residual equi-satisfiability.
    #[test]
    fn every_list_peel_verdict_holds_at_every_closed_instantiation() {
        let list_left = Free::local(0, Some("xs"));
        let list_right = Free::local(1, Some("ys"));
        let nat_a = Free::local(2, Some("a"));
        let nat_b = Free::local(3, Some("b"));
        let anchor_free = Free::local(4, Some("ws"));
        let xs = Term::free_var(&list_left);
        let ys = Term::free_var(&list_right);
        let a = Term::free_var(&nat_a);
        let b = Term::free_var(&nat_b);
        let ws = Term::free_var(&anchor_free);

        let elem = symbol(1000, "T");
        let cat = |parts: Vec<Term>| {
            Term::intrinsic(Intrinsic::ListConcat {
                element: elem.clone(),
                operands: parts,
            })
        };
        let one = |element: Term| {
            Term::intrinsic(Intrinsic::List {
                element: elem.clone(),
                items: vec![element],
            })
        };
        let window = |lo: u32, hi: u32| {
            Term::intrinsic(Intrinsic::list_slice(
                elem.clone(),
                ws.clone(),
                lit(lo),
                lit(hi),
            ))
        };

        let cases = [
            (
                "[a + b] ++ xs ~ [b + a] ++ xs",
                cat(vec![one(plus(a.clone(), b.clone())), xs.clone()]),
                cat(vec![one(plus(b.clone(), a.clone())), xs.clone()]),
            ),
            (
                "xs ++ [7] ~ xs ++ []",
                cat(vec![xs.clone(), nat_list(&[7])]),
                cat(vec![xs.clone(), nat_list(&[])]),
            ),
            (
                "[7] ++ xs ~ [7] ++ ys",
                cat(vec![nat_list(&[7]), xs.clone()]),
                cat(vec![nat_list(&[7]), ys.clone()]),
            ),
            (
                "append(xs, a) ~ xs ++ [a]",
                Term::intrinsic(Intrinsic::list_append(elem.clone(), xs.clone(), a.clone())),
                cat(vec![xs.clone(), one(a.clone())]),
            ),
            (
                "slice(ws, 0, 2) ++ slice(ws, 2, 4) ~ slice(ws, 0, 4)",
                cat(vec![window(0, 2), window(2, 4)]),
                window(0, 4),
            ),
            (
                "[7, 8] ~ [7] ++ xs",
                nat_list(&[7, 8]),
                cat(vec![nat_list(&[7]), xs.clone()]),
            ),
        ];

        let as_intrinsic = |term: &Term| match &**term {
            Subterm::Intrinsic(intrinsic) => intrinsic.clone(),
            other => unreachable!("every side of the grid is an intrinsic, got {other:?}"),
        };

        let runs: [&[u32]; 4] = [&[], &[8], &[7, 8], &[1, 2]];
        let anchors: [&[u32]; 2] = [&[9, 8, 7, 6], &[6, 6, 5, 4, 3]];

        let (mut equal, mut clash, mut carried, mut stuck) = (0, 0, 0, 0);

        for (label, left, right) in &cases {
            let peel =
                peel_list(&as_intrinsic(left), &as_intrinsic(right)).expect("two List values peel");

            match &peel {
                Peel::Equal => equal += 1,
                Peel::Clash => clash += 1,
                Peel::Continue(..) => carried += 1,
                Peel::Stuck => stuck += 1,
            }

            for left_run in runs {
                for right_run in runs {
                    for first in [0u32, 1, 2] {
                        for second in [0u32, 1, 2] {
                            for anchor in anchors {
                                let close = |term: &Term| {
                                    let term = at(term.clone(), &list_left, nat_list(left_run));
                                    let term = at(term, &list_right, nat_list(right_run));
                                    let term = at(term, &nat_a, lit(first));
                                    let term = at(term, &nat_b, lit(second));
                                    list_value(at(term, &anchor_free, nat_list(anchor)))
                                };

                                let agree = close(left) == close(right);

                                match &peel {
                                    Peel::Equal => assert!(
                                        agree,
                                        "`{label}` was decided equal but differs at xs = {left_run:?}, ys = {right_run:?}, a = {first}, b = {second}",
                                    ),
                                    Peel::Clash => assert!(
                                        !agree,
                                        "`{label}` was decided impossible but holds at xs = {left_run:?}, ys = {right_run:?}, a = {first}, b = {second}",
                                    ),
                                    Peel::Continue(residual_left, residual_right) => assert_eq!(
                                        close(residual_left) == close(residual_right),
                                        agree,
                                        "`{label}`'s residuals disagree with the pair they replaced at xs = {left_run:?}, ys = {right_run:?}, a = {first}, b = {second}",
                                    ),
                                    Peel::Stuck => {}
                                }
                            }
                        }
                    }
                }
            }
        }

        assert_eq!(
            (equal, clash, carried, stuck),
            (2, 1, 2, 1),
            "the grid stopped reaching every peel verdict",
        );
    }

    /// Fold a closed term to the value the commutation grid compares: a `List` literal folds its elements too, since a fused run keeps them as written, and every other carrier's closed fold is already the value.
    fn closed_value(term: Term) -> Term {
        let folded = fold(term);
        match &*folded {
            Subterm::Intrinsic(Intrinsic::List {
                element: elem,
                items: elems,
            }) => Term::intrinsic(Intrinsic::List {
                element: elem.clone(),
                items: elems.iter().map(|element| fold(element.clone())).collect(),
            }),
            _ => folded,
        }
    }

    // Soundness gate for the open-term reduction laws the code comments beside the folds assert and nothing else stated: the subtraction borrow within the floor, the literal-factor distribution of `·` on either side, the full-window collapse `slice(b, 0, len(b)) = b`, the empty window `slice(b, i, i) = x[]` over a symbolic base and bound, the cons peels of `get` and `slice` over a symbolic tail and symbolic bounds, and the `len`/`map` homomorphisms over an append and a concatenation. Each case states the law's own reduct and holds the pair to two obligations. The open fold must land on exactly that reduct — so the law demonstrably fired, and where its comment claims, which is what keeps a case from passing vacuously when a rule stops firing. And the original and the reduct must agree as values at every closed instantiation, which is what a definitional equation promises and the only thing a false one fails. Instantiations respect the operations' `/sys` preconditions (`i < len` for `get`, `s <= e <= len` for `slice`), since a program outside them cannot be written.
    //
    // `map`'s ground truth is structural rather than numeric: the mapped function stays a free symbol, so both sides fold to element runs of identical stuck applications, and their agreement says no element was dropped, duplicated or reordered — which is the whole of what the distribution law claims. Mutation-checked: misstating the append measure (`nat_add(2, base)` for `nat_add(1, base)` in the `BinLen` slot) fails the length case on both obligations at once.
    #[test]
    fn every_open_fold_law_preserves_the_value_at_every_closed_instantiation() {
        let nat_x = Free::local(0, Some("x"));
        let nat_y = Free::local(1, Some("y"));
        let bin_base = Free::local(2, Some("b"));
        let bin_tail = Free::local(3, Some("t"));
        let byte_free = Free::local(4, Some("c"));
        let nat_end = Free::local(5, Some("e"));
        let nat_start = Free::local(6, Some("s"));
        let list_base = Free::local(7, Some("xs"));
        let nat_elem = Free::local(8, Some("a"));
        let fun = Free::local(9, Some("f"));
        let x = Term::free_var(&nat_x);
        let y = Term::free_var(&nat_y);
        let b = Term::free_var(&bin_base);
        let t = Term::free_var(&bin_tail);
        let c = Term::free_var(&byte_free);
        let e = Term::free_var(&nat_end);
        let s = Term::free_var(&nat_start);
        let xs = Term::free_var(&list_base);
        let a = Term::free_var(&nat_elem);
        let f = Term::free_var(&fun);

        let sub = |left: Term, right: Term| Term::intrinsic(Intrinsic::nat_sub(left, right));
        let mul = |left: Term, right: Term| Term::intrinsic(Intrinsic::nat_mul(left, right));
        let cat = |parts: Vec<Term>| {
            Term::intrinsic(Intrinsic::BinConcat {
                grain: Grain::X,
                operands: parts,
            })
        };
        let bin_slice = |base: Term, lo: Term, hi: Term| {
            Term::intrinsic(Intrinsic::bin_slice(Grain::X, base, lo, hi))
        };
        let bin_get =
            |base: Term, index: Term| Term::intrinsic(Intrinsic::bin_get(Grain::X, base, index));
        let bin_len = |base: Term| Term::intrinsic(Intrinsic::bin_len(Grain::X, base));
        let chunk = Term::intrinsic(Intrinsic::bin_append(Grain::X, run_bytes(&[]), c.clone()));
        let elem = symbol(1000, "T");
        let list_append = |base: Term, element: Term| {
            Term::intrinsic(Intrinsic::list_append(elem.clone(), base, element))
        };
        let list_len = |base: Term| Term::intrinsic(Intrinsic::list_len(elem.clone(), base));
        let list_map = |base: Term| {
            Term::intrinsic(Intrinsic::list_map(
                elem.clone(),
                elem.clone(),
                base,
                f.clone(),
            ))
        };
        let byte = |value: u8| Term::intrinsic(Intrinsic::Byte(value));

        let cases = vec![
            (
                "(x + 5) - 3 = x + 2",
                sub(plus(x.clone(), lit(5)), lit(3)),
                plus(x.clone(), lit(2)),
                vec![
                    vec![(&nat_x, lit(0))],
                    vec![(&nat_x, lit(1))],
                    vec![(&nat_x, lit(9))],
                ],
            ),
            (
                "(x + 1) - (x + 2) = 0",
                sub(plus(x.clone(), lit(1)), plus(x.clone(), lit(2))),
                lit(0),
                vec![vec![(&nat_x, lit(0))], vec![(&nat_x, lit(4))]],
            ),
            (
                "(x + y + 3) - (y + 1) = x + 2",
                sub(
                    plus(plus(x.clone(), y.clone()), lit(3)),
                    plus(y.clone(), lit(1)),
                ),
                plus(x.clone(), lit(2)),
                vec![
                    vec![(&nat_x, lit(0)), (&nat_y, lit(0))],
                    vec![(&nat_x, lit(2)), (&nat_y, lit(5))],
                    vec![(&nat_x, lit(7)), (&nat_y, lit(1))],
                ],
            ),
            (
                "(x + 1) * 2 = x * 2 + 2",
                mul(plus(x.clone(), lit(1)), lit(2)),
                plus(mul(x.clone(), lit(2)), lit(2)),
                vec![vec![(&nat_x, lit(0))], vec![(&nat_x, lit(3))]],
            ),
            (
                "3 * (x + 2) = 3 * x + 6",
                mul(lit(3), plus(x.clone(), lit(2))),
                plus(mul(lit(3), x.clone()), lit(6)),
                vec![vec![(&nat_x, lit(0))], vec![(&nat_x, lit(3))]],
            ),
            (
                "slice(b, 0, len(b)) = b",
                bin_slice(b.clone(), lit(0), bin_len(b.clone())),
                b.clone(),
                vec![
                    vec![(&bin_base, run_bytes(&[]))],
                    vec![(&bin_base, run_bytes(&[9, 8, 7]))],
                ],
            ),
            (
                "slice(b, e, e) = x[]",
                bin_slice(b.clone(), e.clone(), e.clone()),
                run_bytes(&[]),
                vec![
                    vec![(&bin_base, run_bytes(&[])), (&nat_end, lit(0))],
                    vec![(&bin_base, run_bytes(&[9, 8, 7])), (&nat_end, lit(2))],
                    vec![(&bin_base, run_bytes(&[9, 8, 7])), (&nat_end, lit(3))],
                ],
            ),
            (
                "slice(cons(c, b), 0, e + 1) = cons(c, x[]) ++ slice(b, 0, e)",
                bin_slice(
                    cat(vec![chunk.clone(), b.clone()]),
                    lit(0),
                    plus(e.clone(), lit(1)),
                ),
                cat(vec![chunk.clone(), bin_slice(b.clone(), lit(0), e.clone())]),
                vec![
                    vec![
                        (&bin_base, run_bytes(&[])),
                        (&byte_free, byte(7)),
                        (&nat_end, lit(0)),
                    ],
                    vec![
                        (&bin_base, run_bytes(&[8, 9])),
                        (&byte_free, byte(255)),
                        (&nat_end, lit(1)),
                    ],
                    vec![
                        (&bin_base, run_bytes(&[8, 9])),
                        (&byte_free, byte(0)),
                        (&nat_end, lit(2)),
                    ],
                ],
            ),
            (
                "slice(cons(c, b), s + 1, e + 1) = slice(b, s, e)",
                bin_slice(
                    cat(vec![chunk.clone(), b.clone()]),
                    plus(s.clone(), lit(1)),
                    plus(e.clone(), lit(1)),
                ),
                bin_slice(b.clone(), s.clone(), e.clone()),
                vec![
                    vec![
                        (&bin_base, run_bytes(&[8, 9])),
                        (&byte_free, byte(7)),
                        (&nat_start, lit(0)),
                        (&nat_end, lit(0)),
                    ],
                    vec![
                        (&bin_base, run_bytes(&[8, 9])),
                        (&byte_free, byte(7)),
                        (&nat_start, lit(0)),
                        (&nat_end, lit(2)),
                    ],
                    vec![
                        (&bin_base, run_bytes(&[8, 9])),
                        (&byte_free, byte(7)),
                        (&nat_start, lit(1)),
                        (&nat_end, lit(2)),
                    ],
                    vec![
                        (&bin_base, run_bytes(&[8, 9])),
                        (&byte_free, byte(7)),
                        (&nat_start, lit(2)),
                        (&nat_end, lit(2)),
                    ],
                ],
            ),
            (
                "get(cons(c, x[]), 0) = c",
                bin_get(chunk.clone(), lit(0)),
                c.clone(),
                vec![
                    vec![(&byte_free, byte(0))],
                    vec![(&byte_free, byte(7))],
                    vec![(&byte_free, byte(255))],
                ],
            ),
            (
                "get(cons(c, b), e + 1) = get(b, e)",
                bin_get(cat(vec![chunk.clone(), b.clone()]), plus(e.clone(), lit(1))),
                bin_get(b.clone(), e.clone()),
                vec![
                    vec![
                        (&bin_base, run_bytes(&[8, 9])),
                        (&byte_free, byte(7)),
                        (&nat_end, lit(0)),
                    ],
                    vec![
                        (&bin_base, run_bytes(&[8, 9])),
                        (&byte_free, byte(7)),
                        (&nat_end, lit(1)),
                    ],
                ],
            ),
            (
                "len(append(b, c)) = len(b) + 1",
                bin_len(Term::intrinsic(Intrinsic::bin_append(
                    Grain::X,
                    b.clone(),
                    c.clone(),
                ))),
                plus(bin_len(b.clone()), lit(1)),
                vec![
                    vec![(&bin_base, run_bytes(&[])), (&byte_free, byte(0))],
                    vec![(&bin_base, run_bytes(&[9, 8])), (&byte_free, byte(255))],
                ],
            ),
            (
                "len(b ++ x[0509] ++ t) = len(b) + len(t) + 2",
                bin_len(cat(vec![b.clone(), run_bytes(&[5, 9]), t.clone()])),
                plus(plus(bin_len(b.clone()), bin_len(t.clone())), lit(2)),
                vec![
                    vec![(&bin_base, run_bytes(&[])), (&bin_tail, run_bytes(&[]))],
                    vec![
                        (&bin_base, run_bytes(&[7])),
                        (&bin_tail, run_bytes(&[1, 2])),
                    ],
                ],
            ),
            (
                "map(append(xs, a), f) = append(map(xs, f), f(a))",
                list_map(list_append(xs.clone(), a.clone())),
                list_append(list_map(xs.clone()), Term::apply(f.clone(), [a.clone()])),
                vec![
                    vec![(&list_base, nat_list(&[])), (&nat_elem, lit(5))],
                    vec![(&list_base, nat_list(&[1, 2])), (&nat_elem, lit(5))],
                ],
            ),
            (
                "len(append(xs, a)) = len(xs) + 1",
                list_len(list_append(xs.clone(), a.clone())),
                plus(list_len(xs.clone()), lit(1)),
                vec![
                    vec![(&list_base, nat_list(&[])), (&nat_elem, lit(5))],
                    vec![(&list_base, nat_list(&[1, 2])), (&nat_elem, lit(5))],
                ],
            ),
        ];

        for (label, term, reduct, samples) in cases {
            assert_eq!(
                fold(term.clone()),
                fold(reduct.clone()),
                "`{label}`: the open fold did not land on the law's stated reduct",
            );

            for (index, sample) in samples.iter().enumerate() {
                let close = |term: &Term| {
                    let mut closed = term.clone();
                    for (binder, value) in sample {
                        closed = at(closed, binder, value.clone());
                    }
                    closed_value(closed)
                };

                assert_eq!(
                    close(&term),
                    close(&reduct),
                    "`{label}` changed its value at closed instantiation {index}",
                );
            }
        }
    }
}
