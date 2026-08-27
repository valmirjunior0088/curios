use {
    super::{ReduceError, Reducer},
    crate::{
        Cost, FUSION_CAP, Intrinsic, Located, Nat, Peel, Piece, Subterm, Term, bin_locate,
        bin_measure, bin_window, list_locate, list_measure, list_window, normalize_concat,
        peel_bin, peel_first_atom, peel_first_elem, project_erased_universes,
    },
    curios_num::{Floating, Integer, Natural},
    curios_utilities::{Grain, PackedBin},
    std::cmp::Ordering,
};

/// Read an already-reduced `Nat` term as a concrete `usize` index — `None` when it is still symbolic or too large to fit. The shared decode behind the `Bin`/`List` `get`/`slice` bounds.
fn as_index(term: &Term) -> Option<usize> {
    term.as_nat().and_then(|n| n.to_natural()?.to_usize())
}

/// Reduce the operands of a `Bool` binary intrinsic as far as a fold could use them, then either `fold` the two literals or `rebuild` the neutral term. `Bool` has no numeric carrier at the type level, so the fold reads the `true`/`false` constructors directly.
///
/// **The right operand is reduced only once the left is a literal.** A fold needs both, so a stuck left settles the verdict whatever the right holds, and reducing the right then is work the answer cannot use. It was reduced regardless, and that made weak-head reduction of a `&&`/`||` tree its *full* normalization: a web of predicate definitions each naming the one before it twice unfolded `2^n` times under any demand on its top, since a local-bearing term is remembered by nothing — the cliff `curios`' `scrutinee_refinement_measurements` records under `proved`. Stopping at the left leaves the right as written, which conversion compares lazily through its own reduction, so no equality decision moves. The `Nat` folds below keep both operands eager because their identity laws (`x + 0`) read the right.
fn reduce_bool_binary(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(bool, bool) -> bool,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let Some(l) = left.as_bool() else {
        return Ok(Subterm::Intrinsic(rebuild(left, right.clone())));
    };

    let right = reducer.reduce_forced(right.clone())?;
    Ok(Subterm::Intrinsic(match right.as_bool() {
        Some(r) => Intrinsic::Bool(fold(l, r)),
        None => rebuild(left, right),
    }))
}

/// A binary fold's laws beside its two-literal case, tried on what that case left neutral: a literal unit on one side yields the other operand, a literal absorbing element yields itself, and two structurally identical operands yield what idempotence or self-cancellation says. Every one is an equation on the carrier's values that holds for every value of its symbolic side, which is what makes it admissible in a fold both checkers share — see `documentation/soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md`. Run after the fold rather than inside it because every binary helper already rebuilds its neutral from the operands it reduced, so the laws read them back off the neutral and the helpers keep one signature; a fold that produced a literal has no operands to read and passes through. `reduce_bool_binary` leaves its right operand as written under a stuck left — deliberately, see `a_stuck_left_operand_leaves_the_right_as_written` — so a `Bool` law sees that operand unreduced; a literal or a repeated binder is visible either way, and a law missed on an unreduced operand is a neutral the next demand reduces, never a wrong answer.
fn then_laws(result: Subterm, laws: impl FnOnce(&Term, &Term) -> Option<Term>) -> Subterm {
    let Subterm::Intrinsic(intrinsic) = &result else {
        return result;
    };
    let operands = intrinsic.operands();
    let [left, right] = operands.as_slice() else {
        return result;
    };
    match laws(left, right) {
        Some(term) => Term::unwrap_or_clone(term),
        None => result,
    }
}

/// `&&` with `unit = true` and `||` with `unit = false`: the other literal absorbs, and a repeated operand is itself.
fn bool_lattice_laws(left: &Term, right: &Term, unit: bool) -> Option<Term> {
    match (left.as_bool(), right.as_bool()) {
        (Some(l), _) => Some(if l == unit {
            right.clone()
        } else {
            left.clone()
        }),
        (_, Some(r)) => Some(if r == unit {
            left.clone()
        } else {
            right.clone()
        }),
        _ => (left == right).then(|| left.clone()),
    }
}

/// `xor`: `false` is the unit, a repeated operand cancels to `false`, and a shared operand cancels through one nesting — `(a ⊕ c) ⊕ c = a` — which is what takes `not(not(b))` back to `b`, `not` being `xor(·, true)`. A literal `true` stays: `xor(b, true)` *is* `not b`, and there is nothing shorter to spell it as.
fn bool_xor_laws(left: &Term, right: &Term) -> Option<Term> {
    if left.as_bool() == Some(false) {
        return Some(right.clone());
    }
    if right.as_bool() == Some(false) {
        return Some(left.clone());
    }
    if left == right {
        return Some(Term::intrinsic(Intrinsic::Bool(false)));
    }
    if let Subterm::Intrinsic(Intrinsic::BoolXor(a, c)) = &**left {
        if c == right {
            return Some(a.clone());
        }
        if a == right {
            return Some(c.clone());
        }
    }
    if let Subterm::Intrinsic(Intrinsic::BoolXor(a, c)) = &**right {
        if c == left {
            return Some(a.clone());
        }
        if a == left {
            return Some(c.clone());
        }
    }
    None
}

/// `==` with `same = true` and `!=` with `same = false`: identical operands decide, a literal equal to `same` yields the other operand, and the opposite literal negates it — as `xor(·, true)`, the spelling `not` already has.
fn bool_eql_laws(left: &Term, right: &Term, same: bool) -> Option<Term> {
    if left == right {
        return Some(Term::intrinsic(Intrinsic::Bool(same)));
    }
    let (literal, other) = match (left.as_bool(), right.as_bool()) {
        (Some(l), _) => (l, right),
        (_, Some(r)) => (r, left),
        _ => return None,
    };
    Some(match literal == same {
        true => other.clone(),
        false => Term::intrinsic(Intrinsic::BoolXor(
            other.clone(),
            Term::intrinsic(Intrinsic::Bool(true)),
        )),
    })
}

/// The bitwise lattice on ℕ: `and` has `0` absorbing and no unit (there is no all-ones natural), `or` and `xor` have `0` as unit; `and` and `or` are idempotent and `xor` self-cancels.
fn nat_bitwise_laws(left: &Term, right: &Term, op: &Intrinsic) -> Option<Term> {
    let zero = || Term::intrinsic(Intrinsic::Nat(Nat::Zero));
    let (left_zero, right_zero) = (Nat::is_zero(left), Nat::is_zero(right));
    match op {
        Intrinsic::NatAnd(..) => {
            if left_zero || right_zero {
                return Some(zero());
            }
            (left == right).then(|| left.clone())
        }
        Intrinsic::NatOr(..) => {
            if left_zero {
                return Some(right.clone());
            }
            if right_zero || left == right {
                return Some(left.clone());
            }
            None
        }
        Intrinsic::NatXor(..) => {
            if left_zero {
                return Some(right.clone());
            }
            if right_zero {
                return Some(left.clone());
            }
            (left == right).then(zero)
        }
        _ => None,
    }
}

/// A shift by `0` is the value, and a shifted `0` is `0` — both hold on the unbounded ℕ the type level folds and on the truncating carrier the runtime imposes, which is why no other shift law is taken here: `shl(x, k) = 2ᵏ · x` holds only on the former.
fn nat_shift_laws(left: &Term, right: &Term) -> Option<Term> {
    if Nat::is_zero(right) || Nat::is_zero(left) {
        return Some(left.clone());
    }
    None
}

/// The ring laws `Int` has literally: `0` is `+`'s unit and `-`'s right unit, `1` is `*`'s unit and `0` its absorber, and `i - i` is `0`. Commutativity is deliberately not here — it needs the summand normal form `Nat` has, which `Int` does not, and a law that fires on one operand order is not a law.
fn int_ring_laws(left: &Term, right: &Term, op: &Intrinsic) -> Option<Term> {
    let is = |term: &Term, value: i32| term.as_int() == Some(Integer::from(value));
    let zero = || Term::intrinsic(Intrinsic::Int(Integer::from(0)));
    match op {
        Intrinsic::IntAdd(..) => {
            if is(left, 0) {
                return Some(right.clone());
            }
            is(right, 0).then(|| left.clone())
        }
        Intrinsic::IntSub(..) => {
            if is(right, 0) {
                return Some(left.clone());
            }
            (left == right).then(zero)
        }
        Intrinsic::IntMul(..) => {
            if is(left, 0) || is(right, 0) {
                return Some(zero());
            }
            if is(left, 1) {
                return Some(right.clone());
            }
            is(right, 1).then(|| left.clone())
        }
        _ => None,
    }
}

/// Identical operands decide `==` and `!=` on any carrier whose equality is the value's: two structurally identical reduced terms denote one value.
fn identity_laws(left: &Term, right: &Term, same: bool) -> Option<Term> {
    (left == right).then(|| Term::intrinsic(Intrinsic::Bool(same)))
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

/// A reduced summand read as `coefficient · factor` with a *literal* coefficient, or `None` for a summand that is not such a product — the reading [`Nat::literal_factor`] takes, minus its unit default, for the callers that need to know whether a literal was there.
fn nat_literal_factor(summand: &Term) -> Option<(Natural, Term)> {
    matches!(&**summand, Subterm::Intrinsic(Intrinsic::NatMul(..)))
        .then(|| Nat::literal_factor(summand))
        .filter(|(_, factor)| factor != summand)
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
                quotient.push(Nat::scaled(coefficient / divisor, factor));
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

    // The unconditional laws a symbolic part cannot falsify: a zero dividend divides to `0` with remainder `0` by any divisor, a dividend divides by `1` to itself with remainder `0`, and a dividend divides by itself to `1` with remainder `0` — the last on the operation's own precondition that the divisor is nonzero, which its proof operand states for every value.
    let zero = || Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero));
    if Nat::is_zero(&left) {
        return Ok(zero());
    }
    if divisor.as_ref().is_some_and(Natural::is_one) {
        return Ok(match euclid {
            Euclid::Quotient => Term::unwrap_or_clone(left),
            Euclid::Remainder => zero(),
        });
    }
    if left == right {
        return Ok(match euclid {
            Euclid::Quotient => Subterm::Intrinsic(Intrinsic::Nat(Nat::new(Natural::one()))),
            Euclid::Remainder => zero(),
        });
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

/// `Flt` operations fold on literal operands by calling the model, `curios_num::Floating` — binary32 computed exactly over unbounded integers and rounded once, rather than whatever the compiler's host computes. There is no decline gate: with exactly one NaN and a runtime held to the same clauses, the model leaves nothing undetermined, so `1.0 + 1.0` is `2.0`, `1.0 / 0.0` is `+inf`, `0.0 / 0.0` is the NaN, and each is true of the running program. A symbolic operand rebuilds the neutral term.
///
/// **Why folding here is not the hazard the opacity this replaced was afraid of.** IEEE equality identifies `0.0` with `-0.0`, which `FltToLeBytes` tells apart — the singleton-forgery shape — but folding `FltEql(0.0, -0.0)` to the `Bool` `true` creates no convertibility: `Eq` still needs `refl`, conversion on literals is bitwise, and scrutinee refinement rewrites the scrutinee term rather than an operand. What *would* be a hazard is a fold the running program can disagree with, and the only thing IEEE and Wasm leave to the implementation is a computed NaN's sign and payload — which the one canonical NaN removes, and which `into_wasm` closes at the two operations that could read those bits.
///
/// The rule the opacity established survives verbatim: an intrinsic needs a fold here only if a type or a proof can depend on its value. `Flt` has moved to the other side of it, because [`/syn/Flt/Finite` and `/syn/Flt/NonNeg`](Intrinsic::signature) are bounds decided by a comparison.
///
/// One fact predates all of it, and `free_monoid::bin_measure` is where: `Bin/len(Flt/to_le_bytes(x))` is `4` for every `x`, symbolic `x` included. That is the arity of the operation's result rather than anything about the float, and it is what makes `Flt/of_le_bytes`'s length precondition dischargeable over the operation it inverts.
fn reduce_flt_binary(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(Floating, Floating) -> Intrinsic,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    let folded = match (left.as_flt(), right.as_flt()) {
        (Some(l), Some(r)) => Some(fold(l, r)),
        _ => None,
    };

    Ok(Subterm::Intrinsic(match folded {
        Some(intrinsic) => intrinsic,
        None => rebuild(left, right),
    }))
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

/// [`reduce_flt_binary`]'s unary counterpart. The fold's `None` rebuilds the neutral term, which is how the two narrowings answer an operand outside the domain their bound states: a well-typed call carries a proof that excludes it, and a term that reaches here without one stays stuck rather than being given a value the model does not define.
fn reduce_flt_unary(
    reducer: &mut impl Reducer,
    inner: &Term,
    fold: impl FnOnce(Floating) -> Option<Intrinsic>,
    rebuild: impl FnOnce(Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let inner = reducer.reduce_forced(inner.clone())?;

    Ok(Subterm::Intrinsic(match inner.as_flt().and_then(fold) {
        Some(intrinsic) => intrinsic,
        None => rebuild(inner),
    }))
}

/// The structural outcome of comparing two `Nat`s. The whole comparison family (`eql`/`neq`/`lt`/`le`/`gt`/`ge`) reads this one result; each op differs only in how it maps the outcome to a `bool`. `Le`/`Ge` record a *non-strict* bound the operands force without pinning equality (e.g. `succ x ≥ 1`), letting `lt`/`ge` decide where `eql` still cannot; `Stuck` is undecidable, and the op's neutral term is rebuilt.
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
    // The comparison cancels like terms across both sides, so a stuck product on either is distributed first, by name.
    let left = Nat::normalize(reducer, left)?;
    let right = Nat::normalize(reducer, right)?;
    // Cancel first, so everything below reads the residuals: the shared part decides nothing on its own, and removing it is what lets `cmp(x + a, x + b)` reach `cmp(a, b)` — and `cmp(a + b, b + a)` reach equality — instead of stalling on two inners that differ only by what they share.
    let (left, right) = Nat::cancel_common(&left, &right);

    let (sl, il) = Nat::decompose(&left);
    let (sr, ir) = Nat::decompose(&right);

    // Same inner ⇒ the floors alone decide: `cmp(x + sl, x + sr) = cmp(sl, sr)` (so `lt(pred, succ pred) = true`). Two literals — inner `0` on both sides — also land here: this is the O(1) literal fold. Otherwise, whichever side keeps successors past the shared floor is larger *iff* the other bottomed out at literal zero (`inner ≥ 0`); equal floors with one zero inner give a non-strict bound (`a ≤ b`/`a ≥ b`) the strict/`ge`/`le` reads still use; anything else is undecidable.
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

/// One piece of a located `Bin` window, as a value.
///
/// Every segment [`bin_segments`](crate::free_monoid) admits is a literal run, so a narrowed edge is narrowed *here* — `PackedBin::slice` is an O(1) window into the same payload — rather than rebuilt as a `BinSlice` node for the next pass to fold into exactly this. Same value, same operation, one round trip earlier, and the window arm then constructs no bounded node at all.
fn bin_piece(grain: Grain, piece: Piece<'_>) -> Term {
    match piece {
        Piece::Whole(operand) => operand.clone(),
        Piece::Part(operand, lo, hi) => match &**operand {
            Subterm::Intrinsic(Intrinsic::Bin(found, run)) if *found == grain => {
                let narrowed = run
                    .slice(grain, lo, hi)
                    .expect("a window's piece lies inside the run it was located in");

                Term::intrinsic(Intrinsic::Bin(grain, narrowed))
            }
            _ => unreachable!("a located window's segments are literal runs"),
        },
    }
}

/// The generator a located index names, read straight out of the literal run holding it.
///
/// Every segment [`bin_segments`](crate::free_monoid) admits is a literal run, so the read is performed here rather than rebuilt as a `BinGet` over that operand for the next pass to fold into exactly this — which is also what keeps the located path from having to *state* a bound it would then have to prove.
fn bin_element(grain: Grain, operand: &Term, local: usize) -> Option<Subterm> {
    let Subterm::Intrinsic(Intrinsic::Bin(found, run)) = &**operand else {
        unreachable!("a located index lies in a literal run");
    };
    debug_assert_eq!(*found, grain, "a located segment shares the value's grain");

    match grain {
        Grain::X => run
            .byte(local)
            .map(|byte| Subterm::Intrinsic(Intrinsic::Byte(byte))),
        Grain::B => run
            .bit(local)
            .map(|bit| Subterm::Intrinsic(Intrinsic::Bool(bit))),
    }
}

/// A window aligned to the seams of a concatenation is the run of operands between those seams: `slice([..xs, ..ys], 0, len(xs)) = xs` and `slice([..xs, ..ys], len(xs), len(ys)) = ys`, over *symbolic* operands — the case the literal-run locators above decline. The seams are found by measuring each operand the way `len` measures it and comparing the running sum with the window's start and end as reduced terms; a symbolic operand contributes its own `len`, and the comparison is structural, so two sums that are definitionally but not syntactically equal decline, which is the refusing direction. Sound for every value of the symbolic operands: a window whose start is exactly a prefix's length and whose end is exactly a longer prefix's length covers exactly the operands between, whatever those lengths are. `None` where no seam matches; the operands of the matched run otherwise, for the caller to concatenate.
fn seam_window(
    reducer: &mut impl Reducer,
    operands: &[Term],
    start: &Term,
    length: &Term,
    measure: impl Fn(&Term) -> Intrinsic,
) -> Result<Option<Vec<Term>>, ReduceError> {
    let end = reducer.reduce_forced(Term::intrinsic(Intrinsic::nat_add(
        start.clone(),
        length.clone(),
    )))?;
    let mut prefix = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
    let mut begin = None;
    for (index, operand) in operands.iter().enumerate() {
        if begin.is_none() && prefix == *start {
            begin = Some(index);
        }
        if let Some(begin) = begin
            && prefix == end
        {
            return Ok(Some(operands[begin..index].to_vec()));
        }
        let measured = reducer.reduce_forced(Term::intrinsic(measure(operand)))?;
        prefix = reducer.reduce_forced(Term::intrinsic(Intrinsic::nat_add(prefix, measured)))?;
    }
    Ok(match begin {
        Some(begin) if prefix == end => Some(operands[begin..].to_vec()),
        _ => None,
    })
}

/// [`bin_piece`] over the element carrier, restoring the element type every `List` value carries.
fn list_piece(element: &Term, piece: Piece<'_>) -> Term {
    match piece {
        Piece::Whole(operand) => operand.clone(),
        Piece::Part(operand, lo, hi) => match &**operand {
            Subterm::Intrinsic(Intrinsic::List { element: _, items }) => {
                Term::intrinsic(Intrinsic::List {
                    element: element.clone(),
                    items: items[lo..hi].to_vec(),
                })
            }
            _ => unreachable!("a located window's segments are literal runs"),
        },
    }
}

pub fn reduce_intrinsic(
    reducer: &mut impl Reducer,
    intrinsic: &Intrinsic,
) -> Result<Subterm, ReduceError> {
    match intrinsic {
        Intrinsic::BoolType => Ok(Subterm::Intrinsic(Intrinsic::BoolType)),
        Intrinsic::Bool(value) => Ok(Subterm::Intrinsic(Intrinsic::Bool(*value))),
        Intrinsic::BoolAnd(left, right) => Ok(then_laws(
            reduce_bool_binary(reducer, left, right, |l, r| l && r, Intrinsic::BoolAnd)?,
            |l, r| bool_lattice_laws(l, r, true),
        )),
        Intrinsic::BoolOr(left, right) => Ok(then_laws(
            reduce_bool_binary(reducer, left, right, |l, r| l || r, Intrinsic::BoolOr)?,
            |l, r| bool_lattice_laws(l, r, false),
        )),
        Intrinsic::BoolXor(left, right) => Ok(then_laws(
            reduce_bool_binary(reducer, left, right, |l, r| l != r, Intrinsic::BoolXor)?,
            bool_xor_laws,
        )),
        Intrinsic::BoolEql(left, right) => Ok(then_laws(
            reduce_bool_binary(reducer, left, right, |l, r| l == r, Intrinsic::BoolEql)?,
            |l, r| bool_eql_laws(l, r, true),
        )),
        Intrinsic::BoolNeq(left, right) => Ok(then_laws(
            reduce_bool_binary(reducer, left, right, |l, r| l != r, Intrinsic::BoolNeq)?,
            |l, r| bool_eql_laws(l, r, false),
        )),
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
        Intrinsic::ByteLe(l, r) => {
            reduce_byte_binary(reducer, l, r, |l, r| l <= r, Intrinsic::ByteLe)
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
            // Through the sum normal form, which is what merges like terms: `x + x` is `2 · x`, and `2 · x + 3 · x` is `5 · x`. Idempotent by construction — `Nat::summands` reads in the order `Nat::from_linear` writes — which is what lets the reducer rebuild a sum it was handed already reduced without changing it.
            Ok(Term::unwrap_or_clone(Nat::sum(&left, &right)))
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
            // A zero minuend: `0 - x = 0` for every `x`, truncation being what makes it so.
            if Nat::is_zero(&left) {
                return Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)));
            }
            // A neutral left that is itself a subtraction reassociates: `(a - b) - c = a - (b + c)` holds for truncated subtraction as it does for the integers, and the right-nested form is the one where a later literal subtrahend meets `a`'s floor. The sum is reduced so the cancellation above sees its summands, and the result re-enters this arm for the laws it may now satisfy.
            if let Subterm::Intrinsic(Intrinsic::NatSub(minuend, subtrahend)) = &*left {
                let subtrahend = reducer.reduce_forced(Term::intrinsic(Intrinsic::nat_add(
                    subtrahend.clone(),
                    right.clone(),
                )))?;
                return reduce_intrinsic(reducer, &Intrinsic::nat_sub(minuend.clone(), subtrahend));
            }
            Ok(Subterm::Intrinsic(Intrinsic::nat_sub(left, right)))
        }
        // Multiplication distributes in full, through `Nat::multiply`: every summand of one operand times every summand of the other, each product a monomial in canonical factor order, the results merged as a linear combination. The literal-factor floor law `(x + 1) · 2 = x · 2 + 2`, the unit and annihilation laws, the nested-factor fold `2 · (3 · x) = 6 · x`, a literal over a symbolic sum, a symbolic factor over a symbolic sum, and `x · y = y · x` are all the one rule; the floor only ever moves outward and a monomial is never nested, so the rewrite terminates.
        Intrinsic::NatMul(left, right) => {
            let left = reducer.reduce_forced(left.clone())?;
            let right = reducer.reduce_forced(right.clone())?;
            // **A product of two symbolic sums is its own weak-head form.** Distribution is the one quadratic step in the `Nat` normal form — every summand of one operand against every summand of the other — and a web of definitions each naming the one before it twice made it build 1 222 222 monomials to keep 25 412, to answer a comparison a head clash settles. A product with a literal or a single symbolic summand on either side distributes here as it always did, in O(summands); only sum × sum stays stuck, and `Nat::normalize` distributes it where a value is asked for by name — see `documentation/design/toolchain/a-sum-is-merged-when-it-is-forced-not-when-it-is-built.md`.
            let symbolic_summands = |term: &Term| Nat::summands(&Nat::decompose(term).1).len();
            if symbolic_summands(&left) > 1 && symbolic_summands(&right) > 1 {
                return Ok(Subterm::Intrinsic(Intrinsic::nat_mul(left, right)));
            }
            reducer.spend(operand_bound(
                left.as_nat().map_or(0, |value| value.bits()),
                right.as_nat().map_or(0, |value| value.bits()),
            ))?;
            Ok(Term::unwrap_or_clone(Nat::multiply(&left, &right)))
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
        Intrinsic::NatLe(left, right) => reduce_nat_compare(
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
        // Bitwise ops fold on the unbounded ℕ the type level pretends: `and`, `or`, `xor` on the infinite binary expansion, `shl` as `· 2^n` and `shr` as `⌊·/2^n⌋`. The runtime's 31-bit carrier (truncating `shl`, logical `shr`) is imposed only in the backend, never here.
        Intrinsic::NatAnd(left, right) => Ok(then_laws(
            reduce_nat_binary(
                reducer,
                left,
                right,
                |l, r| l.checked_bitand(r).map(Intrinsic::Nat),
                Intrinsic::NatAnd,
            )?,
            |l, r| nat_bitwise_laws(l, r, intrinsic),
        )),
        Intrinsic::NatOr(left, right) => Ok(then_laws(
            reduce_nat_binary(
                reducer,
                left,
                right,
                |l, r| l.checked_bitor(r).map(Intrinsic::Nat),
                Intrinsic::NatOr,
            )?,
            |l, r| nat_bitwise_laws(l, r, intrinsic),
        )),
        Intrinsic::NatXor(left, right) => Ok(then_laws(
            reduce_nat_binary(
                reducer,
                left,
                right,
                |l, r| l.checked_bitxor(r).map(Intrinsic::Nat),
                Intrinsic::NatXor,
            )?,
            |l, r| nat_bitwise_laws(l, r, intrinsic),
        )),
        Intrinsic::NatShl(left, right) => Ok(then_laws(
            reduce_nat_shl(reducer, left, right)?,
            nat_shift_laws,
        )),
        Intrinsic::NatShr(left, right) => Ok(then_laws(
            reduce_nat_binary(
                reducer,
                left,
                right,
                |l, r| l.checked_shr(r).map(Intrinsic::Nat),
                Intrinsic::NatShr,
            )?,
            nat_shift_laws,
        )),
        Intrinsic::IntType => Ok(Subterm::Intrinsic(Intrinsic::IntType)),
        Intrinsic::Int(value) => Ok(Subterm::Intrinsic(Intrinsic::Int(value.clone()))),
        Intrinsic::IntEql(left, right) => Ok(then_laws(
            reduce_int_binary(
                reducer,
                left,
                right,
                |left, right| Some(Intrinsic::Bool(left == right)),
                Intrinsic::IntEql,
            )?,
            |l, r| identity_laws(l, r, true),
        )),
        Intrinsic::IntNeq(left, right) => Ok(then_laws(
            reduce_int_binary(
                reducer,
                left,
                right,
                |left, right| Some(Intrinsic::Bool(left != right)),
                Intrinsic::IntNeq,
            )?,
            |l, r| identity_laws(l, r, false),
        )),
        Intrinsic::IntAdd(left, right) => Ok(then_laws(
            reduce_int_binary(
                reducer,
                left,
                right,
                |left, right| Some(Intrinsic::Int(left + right)),
                Intrinsic::IntAdd,
            )?,
            |l, r| int_ring_laws(l, r, intrinsic),
        )),
        Intrinsic::IntSub(left, right) => Ok(then_laws(
            reduce_int_binary(
                reducer,
                left,
                right,
                |left, right| Some(Intrinsic::Int(left - right)),
                Intrinsic::IntSub,
            )?,
            |l, r| int_ring_laws(l, r, intrinsic),
        )),
        Intrinsic::IntMul(left, right) => Ok(then_laws(
            reduce_int_binary(
                reducer,
                left,
                right,
                |left, right| Some(Intrinsic::Int(left * right)),
                Intrinsic::IntMul,
            )?,
            |l, r| int_ring_laws(l, r, intrinsic),
        )),
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
        Intrinsic::IntLe(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Bool(left <= right)),
            Intrinsic::IntLe,
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
        Intrinsic::FltType => Ok(Subterm::Intrinsic(Intrinsic::FltType)),
        Intrinsic::Flt(flt) => Ok(Subterm::Intrinsic(Intrinsic::Flt(*flt))),
        Intrinsic::FltAdd(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l + r),
            Intrinsic::FltAdd,
        ),
        Intrinsic::FltSub(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l - r),
            Intrinsic::FltSub,
        ),
        Intrinsic::FltMul(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l * r),
            Intrinsic::FltMul,
        ),
        Intrinsic::FltDiv(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l / r),
            Intrinsic::FltDiv,
        ),
        // `%` on `f32` is C `fmod`: `x - trunc(x / y) * y`, sign of the dividend — the same value the `cont -> wasm` expansion computes.
        Intrinsic::FltRem(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l % r),
            Intrinsic::FltRem,
        ),
        Intrinsic::FltMin(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l.min(r)),
            Intrinsic::FltMin,
        ),
        Intrinsic::FltMax(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l.max(r)),
            Intrinsic::FltMax,
        ),
        Intrinsic::FltCopysign(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l.copysign(r)),
            Intrinsic::FltCopysign,
        ),
        Intrinsic::FltEql(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Bool(l.eql(r)),
            Intrinsic::FltEql,
        ),
        Intrinsic::FltNeq(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Bool(l.neq(r)),
            Intrinsic::FltNeq,
        ),
        Intrinsic::FltLt(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Bool(l.lt(r)),
            Intrinsic::FltLt,
        ),
        Intrinsic::FltLe(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Bool(l.le(r)),
            Intrinsic::FltLe,
        ),
        Intrinsic::FltNeg(inner) => reduce_flt_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(-v)),
            Intrinsic::FltNeg,
        ),
        Intrinsic::FltAbs(inner) => reduce_flt_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(v.abs())),
            Intrinsic::FltAbs,
        ),
        Intrinsic::FltSqrt(inner) => reduce_flt_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(v.sqrt())),
            Intrinsic::FltSqrt,
        ),
        Intrinsic::FltFloor(inner) => reduce_flt_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(v.floor())),
            Intrinsic::FltFloor,
        ),
        Intrinsic::FltCeil(inner) => reduce_flt_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(v.ceil())),
            Intrinsic::FltCeil,
        ),
        Intrinsic::FltTrunc(inner) => reduce_flt_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(v.trunc())),
            Intrinsic::FltTrunc,
        ),
        Intrinsic::FltNearest(inner) => reduce_flt_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(v.nearest())),
            Intrinsic::FltNearest,
        ),
        // The two reinterpretations, whose round-trip laws are now theorems of the model rather than a postulate: `of_le_bytes(to_le_bytes(x))` is `x` for every `x`, and `to_le_bytes(of_le_bytes(b))` is `b` for every `b` that is not a non-canonical NaN pattern — which every NaN pattern reaching `Floating` is turned into.
        Intrinsic::FltToLeBytes(inner) => reduce_flt_unary(
            reducer,
            inner,
            |v| {
                Some(Intrinsic::Bin(
                    Grain::X,
                    PackedBin::from_bytes(v.to_bits().to_le_bytes().to_vec()),
                ))
            },
            Intrinsic::FltToLeBytes,
        ),
        Intrinsic::FltOfLeBytes { bin, four_bytes } => {
            let bin = reducer.reduce_forced(bin.clone())?;

            let folded = match &*bin {
                Subterm::Intrinsic(Intrinsic::Bin(Grain::X, packed)) => packed
                    .to_bytes()
                    .and_then(|bytes| <[u8; 4]>::try_from(bytes).ok())
                    .map(|bytes| Intrinsic::Flt(Floating::from_bits(u32::from_le_bytes(bytes)))),
                _ => None,
            };

            Ok(Subterm::Intrinsic(match folded {
                Some(intrinsic) => intrinsic,
                None => Intrinsic::FltOfLeBytes {
                    bin,
                    four_bytes: four_bytes.clone(),
                },
            }))
        }
        // The conversions preserve the number, never the bits — a bit view belongs to explicit `Bin` casts. `Nat/to_int` is total: ℕ embeds in ℤ, and both are unbounded here. The runtime's carrier-range traps stay where they always were, at the `into_wasm` boundary.
        Intrinsic::NatToInt(inner) => reduce_nat_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Int(Integer::from(v.to_natural()?))),
            Intrinsic::NatToInt,
        ),
        // Into `Flt` the conversions are total and take no proof: rounding to nearest is the canonical extension of the embedding, forced by the structure the way monus is for `Nat/sub`, and a magnitude past the largest finite value answers the infinity of its sign.
        Intrinsic::NatToFlt(inner) => reduce_nat_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(Floating::of_natural(&v.to_natural()?))),
            Intrinsic::NatToFlt,
        ),
        // `Int/to_nat` of a negative literal is a value no natural holds — reported like a zero divisor, never wrapped. The bound the operation now states does not retire that report: a bound is discharged in the context the call was written in, and an open term reduces under hypotheses that context may not have. A symbolic operand rebuilds the neutral term, carrying the proof it was handed.
        Intrinsic::IntToNat { int, non_neg } => {
            let span = int.span();
            let int = reducer.reduce_forced(int.clone())?;
            match int.as_int() {
                Some(value) => match value.to_natural() {
                    Some(number) => Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::new(number)))),
                    None => Err(ReduceError::IntToNatNegative { value, span }),
                },
                None => Ok(Subterm::Intrinsic(Intrinsic::IntToNat {
                    int,
                    non_neg: non_neg.clone(),
                })),
            }
        }
        Intrinsic::IntToFlt(inner) => reduce_int_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(Floating::of_integer(&v))),
            Intrinsic::IntToFlt,
        ),
        // The two narrowings truncate toward zero and answer the *exact* unbounded natural or integer: `to_nat(3.0e9)` is `3000000000`, a value no runtime carrier holds, refused downstream exactly as an overflowing `Nat` is rather than bent to fit here. Outside the domain each bound states, the model declines and the neutral is rebuilt, carrying the proof it was handed.
        Intrinsic::FltToNat { flt, non_neg } => reduce_flt_unary(
            reducer,
            flt,
            |v| Some(Intrinsic::Nat(Nat::new(v.to_natural()?))),
            |flt| Intrinsic::FltToNat {
                flt,
                non_neg: non_neg.clone(),
            },
        ),
        Intrinsic::FltToInt { flt, finite } => reduce_flt_unary(
            reducer,
            flt,
            |v| Some(Intrinsic::Int(v.to_integer()?)),
            |flt| Intrinsic::FltToInt {
                flt,
                finite: finite.clone(),
            },
        ),
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
            in_range,
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
                        return bin_element(Grain::X, operand, local).ok_or_else(|| {
                            ReduceError::BinGetOutOfBounds {
                                len: local,
                                index: i,
                                span: index.span(),
                            }
                        });
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
                            .reduce(Term::intrinsic(Intrinsic::bin_get(
                                Grain::X,
                                head,
                                zero,
                                in_range.clone(),
                            )))
                            .map(Term::unwrap_or_clone);
                    }
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))) => {
                        let one = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
                        let prev = Term::intrinsic(Intrinsic::nat_sub(index_reduced.clone(), one));
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_get(
                                Grain::X,
                                tail,
                                prev,
                                in_range.clone(),
                            )))
                            .map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::bin_get(
                Grain::X,
                bin,
                index_reduced,
                in_range.clone(),
            )))
        }
        Intrinsic::BinSlice {
            grain: Grain::X,
            bin,
            start,
            length,
            within,
        } => {
            let bin = reducer.reduce_forced(bin.clone())?;
            let start_reduced = reducer.reduce_forced(start.clone())?;
            let length_reduced = reducer.reduce_forced(length.clone())?;
            // The full slice is the identity: `slice(b, 0, len b) = b`. Sound even for a symbolic `b` — a window covering everything is always in range, never trapping — and the runtime partner of `core::spine`'s window-collapse: it lets a bare full-window `BinSlice` reduce to its base, so a `Bin/slice` over the whole value costs no copy and converts against the base directly.
            if matches!(
                &*start_reduced,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero))
            ) && matches!(&*length_reduced, Subterm::Intrinsic(Intrinsic::BinLen(Grain::X, whole)) if *whole == bin)
            {
                return Ok(Term::unwrap_or_clone(bin));
            }
            // The empty slice is empty: `slice(b, i, 0) = x[]`. The dual of the full-window identity and equally sound — a zero-length window yields no bytes regardless of `b` or `i`, and never equates two distinct literals. It lets a codepoint take collapse its zero-width base (`take 0`) to the empty string even over a symbolic cons. Reading a *count* is what makes this one test rather than a comparison of two subjects.
            if Nat::is_zero(&length_reduced) {
                return Ok(Subterm::Intrinsic(Intrinsic::Bin(
                    Grain::X,
                    PackedBin::empty(),
                )));
            }
            let s = as_index(&start_reduced);
            let n = as_index(&length_reduced);
            // A concrete slice of a literal run.
            if let (Subterm::Intrinsic(Intrinsic::Bin(Grain::X, bytes)), Some(s), Some(n)) =
                (&*bin, s, n)
            {
                return match s.checked_add(n).and_then(|e| bytes.slice(Grain::X, s, e)) {
                    Some(slice) => Ok(Subterm::Intrinsic(Intrinsic::Bin(Grain::X, slice))),
                    None => Err(ReduceError::BinSliceOutOfRange {
                        len: bytes.len(Grain::X),
                        start: s,
                        length: n,
                        span: start.span().or_else(|| length.span()),
                    }),
                };
            }
            // Locate the window by the operands' own lengths. Every operand it covers whole is handed back untouched and shares its payload; only the two at the edges are narrowed, and everything outside the window is dropped without being read.
            //
            // Every segment `bin_segments` admits is a literal run, so a narrowed edge is narrowed *here* rather than rebuilt as a `BinSlice` for the next pass to fold — `PackedBin::slice` is an O(1) window into the same payload, so this is the same value by the same operation, one round trip earlier. It also leaves this arm constructing no bounded node at all, which is what keeps a bound off the reducer once these accessors carry one.
            if let (Some(s), Some(n)) = (s, n) {
                match bin_window(Grain::X, &bin, s, n) {
                    Some(Ok(pieces)) => {
                        let parts = pieces
                            .into_iter()
                            .map(|piece| bin_piece(Grain::X, piece))
                            .collect::<Vec<Term>>();
                        reducer.spend(Cost::collection(parts.len() as u64))?;

                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_concat(Grain::X, parts)))
                            .map(Term::unwrap_or_clone);
                    }
                    Some(Err(len)) => {
                        return Err(ReduceError::BinSliceOutOfRange {
                            len,
                            start: s,
                            length: n,
                            span: start.span().or_else(|| length.span()),
                        });
                    }
                    None => {}
                }
            }
            // A window on the seams of a symbolic concatenation — see `seam_window`.
            if let Subterm::Intrinsic(Intrinsic::BinConcat {
                grain: Grain::X,
                operands,
            }) = &*bin
                && let Some(run) = seam_window(
                    reducer,
                    operands,
                    &start_reduced,
                    &length_reduced,
                    |operand| Intrinsic::bin_len(Grain::X, operand.clone()),
                )?
            {
                return reducer
                    .reduce(Term::intrinsic(Intrinsic::bin_concat(Grain::X, run)))
                    .map(Term::unwrap_or_clone);
            }
            // A slice over a cons spine peels one byte per `0`/`succ` boundary step — the reduction partner of the `Utf8` cons the validity proofs walk:  `slice(cons(h, t), 0, succ n) = h ++ slice(t, 0, n)`  and  `slice(cons(h, t), succ s, n) = slice(t, s, n)`.
            //
            // Advancing the start no longer touches the length, which is the reparameterisation paying for itself: the count is invariant under peeling the base, so nothing about the window has to be recomputed to move it.
            if let Some((head, tail)) = peel_first_atom(Grain::X, &bin) {
                let dec = |n: &Term| {
                    let one = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
                    Term::intrinsic(Intrinsic::nat_sub(n.clone(), one))
                };
                match (&*start_reduced, &*length_reduced) {
                    (
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)),
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))),
                    ) => {
                        let zero = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
                        let rest = Term::intrinsic(Intrinsic::bin_slice(
                            Grain::X,
                            tail,
                            zero,
                            dec(&length_reduced),
                            within.clone(),
                        ));
                        let consed = Term::intrinsic(Intrinsic::bin_concat(Grain::X, [head, rest]));
                        return reducer.reduce(consed).map(Term::unwrap_or_clone);
                    }
                    (Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))), _) => {
                        let sliced = Term::intrinsic(Intrinsic::bin_slice(
                            Grain::X,
                            tail,
                            dec(&start_reduced),
                            length_reduced.clone(),
                            within.clone(),
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
                length_reduced,
                within.clone(),
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
            in_range,
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
                        return bin_element(Grain::B, operand, local).ok_or_else(|| {
                            ReduceError::BinGetOutOfBounds {
                                len: local,
                                index: i,
                                span: index.span(),
                            }
                        });
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
                                in_range.clone(),
                            )))
                            .map(Term::unwrap_or_clone);
                    }
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))) => {
                        let prev = Term::intrinsic(Intrinsic::nat_sub(
                            index_reduced.clone(),
                            Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                        ));
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_get(
                                Grain::B,
                                tail,
                                prev,
                                in_range.clone(),
                            )))
                            .map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::BinGet {
                grain: Grain::B,
                bin,
                index: index_reduced,
                in_range: in_range.clone(),
            }))
        }
        Intrinsic::BinSlice {
            grain: Grain::B,
            bin,
            start,
            length,
            within,
        } => {
            let span = start.span().or_else(|| length.span());
            let bin = reducer.reduce_forced(bin.clone())?;
            let start_reduced = reducer.reduce_forced(start.clone())?;
            let length_reduced = reducer.reduce_forced(length.clone())?;
            if matches!(
                &*start_reduced,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero))
            ) && matches!(&*length_reduced, Subterm::Intrinsic(Intrinsic::BinLen(Grain::B, whole)) if *whole == bin)
            {
                return Ok(Term::unwrap_or_clone(bin));
            }
            if Nat::is_zero(&length_reduced) {
                return Ok(Subterm::Intrinsic(Intrinsic::Bin(
                    Grain::B,
                    PackedBin::empty(),
                )));
            }
            if let (Subterm::Intrinsic(Intrinsic::Bin(Grain::B, bits)), Some(start), Some(count)) =
                (&*bin, as_index(&start_reduced), as_index(&length_reduced))
            {
                return start
                    .checked_add(count)
                    .and_then(|end| bits.slice(Grain::B, start, end))
                    .map(|bits| Subterm::Intrinsic(Intrinsic::Bin(Grain::B, bits)))
                    .ok_or_else(|| ReduceError::BinSliceOutOfRange {
                        len: bits.bit_length(),
                        start,
                        length: count,
                        span,
                    });
            }
            // Locate the window by the operands' own lengths. Every operand it covers whole is handed back untouched and shares its payload; only the two at the edges are narrowed, and everything outside the window is dropped without being read.
            if let (Some(s), Some(n)) = (as_index(&start_reduced), as_index(&length_reduced)) {
                match bin_window(Grain::B, &bin, s, n) {
                    Some(Ok(pieces)) => {
                        let parts = pieces
                            .into_iter()
                            .map(|piece| bin_piece(Grain::B, piece))
                            .collect::<Vec<Term>>();
                        reducer.spend(Cost::collection(parts.len() as u64))?;

                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_concat(Grain::B, parts)))
                            .map(Term::unwrap_or_clone);
                    }
                    Some(Err(len)) => {
                        return Err(ReduceError::BinSliceOutOfRange {
                            len,
                            start: s,
                            length: n,
                            span,
                        });
                    }
                    None => {}
                }
            }
            // A window on the seams of a symbolic concatenation — see `seam_window`.
            if let Subterm::Intrinsic(Intrinsic::BinConcat {
                grain: Grain::B,
                operands,
            }) = &*bin
                && let Some(run) = seam_window(
                    reducer,
                    operands,
                    &start_reduced,
                    &length_reduced,
                    |operand| Intrinsic::bin_len(Grain::B, operand.clone()),
                )?
            {
                return reducer
                    .reduce(Term::intrinsic(Intrinsic::bin_concat(Grain::B, run)))
                    .map(Term::unwrap_or_clone);
            }
            if let Some((head, tail)) = peel_first_atom(Grain::B, &bin) {
                let dec = |n: &Term| {
                    Term::intrinsic(Intrinsic::nat_sub(
                        n.clone(),
                        Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                    ))
                };
                match (&*start_reduced, &*length_reduced) {
                    (
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)),
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))),
                    ) => {
                        let rest = Term::intrinsic(Intrinsic::bin_slice(
                            Grain::B,
                            tail,
                            Term::intrinsic(Intrinsic::Nat(Nat::Zero)),
                            dec(&length_reduced),
                            within.clone(),
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
                                length_reduced.clone(),
                                within.clone(),
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
                length: length_reduced,
                within: within.clone(),
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
            // `len(map(xs, f)) = len(xs)`: a map is elementwise, so the measure passes through it whatever `f` does.
            if let Subterm::Intrinsic(Intrinsic::ListMap {
                from, list: inner, ..
            }) = &*list
            {
                return reduce_intrinsic(
                    reducer,
                    &Intrinsic::list_len(from.clone(), inner.clone()),
                );
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
            in_range,
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
                            .reduce(Term::intrinsic(Intrinsic::list_get(
                                type_,
                                operand,
                                local,
                                in_range.clone(),
                            )))
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
                            .reduce(Term::intrinsic(Intrinsic::list_get(
                                type_,
                                tail,
                                prev,
                                in_range.clone(),
                            )))
                            .map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::list_get(
                type_,
                list,
                index_reduced,
                in_range.clone(),
            )))
        }
        Intrinsic::ListSlice {
            element: type_,
            list,
            start,
            length,
            within,
        } => {
            let type_ = reducer.reduce(type_.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            let start_reduced = reducer.reduce_forced(start.clone())?;
            let length_reduced = reducer.reduce_forced(length.clone())?;
            // The full slice is the identity: `slice(a, 0, len a) = a`. Sound even for a symbolic `a` — a window covering everything is always in range — the `List` twin of `BinSlice`'s full-window identity, letting a full-length `List/slice` reduce to its base instead of copying.
            if matches!(
                &*start_reduced,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero))
            ) && matches!(&*length_reduced, Subterm::Intrinsic(Intrinsic::ListLen { element: _, list: whole }) if *whole == list)
            {
                return Ok(Term::unwrap_or_clone(list));
            }
            // The empty slice is empty: `slice(a, i, 0) = []`. Sound for a symbolic `a` — a zero-length window yields no elements regardless — and the base case the cons peel below bottoms out on (the `List` twin of `BinSlice`'s empty-slice identity).
            if Nat::is_zero(&length_reduced) {
                return Ok(Subterm::Intrinsic(Intrinsic::List {
                    element: type_.clone(),
                    items: Vec::new(),
                }));
            }
            let s = as_index(&start_reduced);
            let n = as_index(&length_reduced);
            // A concrete slice of a literal run.
            if let (
                Subterm::Intrinsic(Intrinsic::List {
                    element: _,
                    items: elems,
                }),
                Some(s),
                Some(n),
            ) = (&*list, s, n)
            {
                return match s.checked_add(n).and_then(|e| elems.get(s..e)) {
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
                        length: n,
                        span: start.span().or_else(|| length.span()),
                    }),
                };
            }
            // The `List` twin of `BinSlice`'s locator: the window's segments, each already narrowed to its overlap, and — since every segment is a literal run — narrowed here rather than rebuilt as a `ListSlice` node for the next pass to fold.
            if let (Some(s), Some(n)) = (s, n) {
                match list_window(&list, s, n) {
                    Some(Ok(pieces)) => {
                        let parts = pieces
                            .into_iter()
                            .map(|piece| list_piece(&type_, piece))
                            .collect::<Vec<Term>>();
                        reducer.spend(Cost::collection(parts.len() as u64))?;

                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::list_concat(type_, parts)))
                            .map(Term::unwrap_or_clone);
                    }
                    Some(Err(len)) => {
                        return Err(ReduceError::ListSliceOutOfRange {
                            len,
                            start: s,
                            length: n,
                            span: start.span().or_else(|| length.span()),
                        });
                    }
                    None => {}
                }
            }
            // A window on the seams of a symbolic concatenation — see `seam_window`.
            if let Subterm::Intrinsic(Intrinsic::ListConcat { operands, .. }) = &*list
                && let Some(run) = seam_window(
                    reducer,
                    operands,
                    &start_reduced,
                    &length_reduced,
                    |operand| Intrinsic::list_len(type_.clone(), operand.clone()),
                )?
            {
                return reducer
                    .reduce(Term::intrinsic(Intrinsic::list_concat(type_, run)))
                    .map(Term::unwrap_or_clone);
            }
            // A slice over a cons spine peels one element per `0`/`succ` boundary step, the `List` twin of `BinSlice`'s element peel: `slice(cons(h, t), 0, succ n) = [h] ++ slice(t, 0, n)`  and  `slice(cons(h, t), succ s, n) = slice(t, s, n)` — the count riding through the second untouched.
            if let Some((head, tail)) = peel_first_elem(&list) {
                let dec = |n: &Term| {
                    let one = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
                    Term::intrinsic(Intrinsic::nat_sub(n.clone(), one))
                };
                match (&*start_reduced, &*length_reduced) {
                    (
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)),
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))),
                    ) => {
                        let zero = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
                        let rest = Term::intrinsic(Intrinsic::list_slice(
                            type_.clone(),
                            tail,
                            zero,
                            dec(&length_reduced),
                            within.clone(),
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
                            length_reduced.clone(),
                            within.clone(),
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
                length_reduced,
                within.clone(),
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
mod compare_tests;
#[cfg(test)]
mod cost_tests;
#[cfg(test)]
mod free_monoid_tests;
#[cfg(test)]
mod laws_tests;
#[cfg(test)]
mod nat_tests;
#[cfg(test)]
mod test_support;
