mod compare;
use compare::*;

mod cost;
use cost::*;

mod free_monoid;
use free_monoid::*;

mod laws;
use laws::*;

mod nat;
use nat::*;

mod int;
use int::*;

mod scalar;
use scalar::*;

mod truth;
pub use truth::*;

use {
    super::{ReduceError, Reducer},
    crate::{
        Cost, FUSION_CAP, FreeMonoid, Func, Intrinsic, Nat, Peel, Subterm, Telescope, Term,
        int_cancel_common, int_negate, int_of_nat, int_preimage, int_product, int_split_by_sign,
        int_sum, int_terms, normalize_concat, peel_bin, peel_first_atom, peel_first_elem,
        project_erased_universes,
    },
    curios_num::{Binary, Floating, Grain, Integer, Natural},
};

/// A `&&` or `||` tree with every leaf forced and the tree re-nested to the left, asked for by name where a comparison needs one set of leaves against another — the converters' rule for two conjunctions or two disjunctions, the twin of `Nat::normalize` for a stuck product. `None` for any other intrinsic.
///
/// The fold leaves a stuck connective's right operand as written, on the record `reduce_bool_binary` keeps of the `&&`/`||` cliff, so a leaf as the fold left it may be a spelling that reduces to a subtree — a witness projection standing for `c && d`. Forcing each leaf and descending is what makes the leaf set the value's rather than the spelling's, and it is paid once per comparison of two trees rather than at every reduction of one, which is the whole of the difference from the cliff. The rebuilt tree is reduced again, so a leaf that forced to a literal meets the lattice laws where it stands.
pub fn normalize_bool(
    reducer: &mut impl Reducer,
    intrinsic: &Intrinsic,
) -> Result<Option<Term>, ReduceError> {
    let (conjunction, left, right) = match intrinsic {
        Intrinsic::BoolAnd(left, right) => (true, left, right),
        Intrinsic::BoolOr(left, right) => (false, left, right),
        _ => return Ok(None),
    };
    let rebuild = |left: Term, right: Term| match conjunction {
        true => Intrinsic::BoolAnd(left, right),
        false => Intrinsic::BoolOr(left, right),
    };

    let mut leaves = Vec::new();
    let mut pending = vec![right.clone(), left.clone()];
    while let Some(term) = pending.pop() {
        let forced = reducer.reduce_forced(term)?;
        match (&*forced, conjunction) {
            (Subterm::Intrinsic(Intrinsic::BoolAnd(left, right)), true)
            | (Subterm::Intrinsic(Intrinsic::BoolOr(left, right)), false) => {
                pending.push(right.clone());
                pending.push(left.clone());
            }
            _ => leaves.push(forced),
        }
    }
    reducer.spend(Cost::collection(leaves.len() as u64))?;

    let tree = leaves
        .into_iter()
        .reduce(|acc, leaf| Term::intrinsic(rebuild(acc, leaf)))
        .expect("a connective has two operands, so at least two leaves");
    reducer.reduce_forced(tree).map(Some)
}

/// Whether a function is the identity: one binder, whose body is that binder once it is weak-head reduced, so `(v) => v + 0` and `(v) => ((w) => w)(v)` are the identity as `(v) => v` is. This is beta and the folds under the binder, never extensionality: a function that is the identity only *pointwise* — `(v) => match v | 0 => 0 | k + 1 => k + 1 end` — has a stuck match for a body, and stays a function nothing here recognises.
///
/// The body is read as conversion would read it, which is what makes the test agree with what both checkers already say of the function itself: `(v) => v + 0` converts with `(v) => v`, so a `map` by one that stayed stuck while a `map` by the other collapsed was a test of the *spelling* where the value was meant. The lambda as written is asked first, since it costs nothing; past it the binder is opened on a fresh identity, charged as the closed machine's eta probe charges the same opening, and the body reduced to weak-head form and not forced — a body whose head is a recursive call is no binder, and unfolding it to find that out would be paid at every `map`.
fn is_identity(reducer: &mut impl Reducer, function: &Term) -> Result<bool, ReduceError> {
    let Subterm::Func(Func { telescope, .. }) = &**function else {
        return Ok(false);
    };
    let Telescope::Cons(_, rest) = telescope else {
        return Ok(false);
    };
    let Telescope::Done(body) = rest.body() else {
        return Ok(false);
    };
    if matches!(&***body, Subterm::Var(var) if var.as_bound() == Some(0)) {
        return Ok(true);
    }

    reducer.spend(
        Cost::collection(1)
            .saturating_mul(3)
            .saturating_add(Cost::term(1)),
    )?;
    let binder = reducer.fresh_binder(None);
    let body = reducer.reduce(telescope.open(&[&Term::free_var(&binder)]))?;

    Ok(matches!(&*body, Subterm::Var(var) if var.unwrap() == &binder))
}

/// Two stuck comparisons spelled across the family, aligned to one spelling so the congruence can compare them: a negated comparison — `Bool/not` is `xor(_, true)` once unfolded — becomes its dual, `not(a < b)` reading `b <= a` and `not(a == b)` reading `a != b`, and a `<=` meeting a `<` on the other side becomes `<` of the successor, since `a <= b` and `a < b + 1` are one relation on `Nat` and on `Int`. `None` when neither side moved.
///
/// Asked for by name in both converters beside [`normalize_bool`], and **probe-side only**, on the record `documentation/design/toolchain/a-comparison-is-spelled-one-way-when-it-is-stuck.md` keeps: a guard's refinement is keyed on the guard's written spelling, so a fold that respelled a comparison would take every later occurrence past it, where a probe respelled inside the judgment leaves every recorded key as written. Total orders only: on `Flt` every ordered comparison against a NaN is false in both directions, so its negation is not the mirror, and the negation of an `Flt` comparison stays a leaf.
pub fn align_comparisons(
    reducer: &mut impl Reducer,
    this: &Intrinsic,
    that: &Intrinsic,
) -> Result<Option<(Intrinsic, Intrinsic)>, ReduceError> {
    let this_dual = dual_of_negated(reducer, this)?;
    let that_dual = dual_of_negated(reducer, that)?;
    let moved = this_dual.is_some() || that_dual.is_some();
    let this = this_dual.unwrap_or_else(|| this.clone());
    let that = that_dual.unwrap_or_else(|| that.clone());

    let one = || Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
    let plus_one = || Term::intrinsic(Intrinsic::Int(Integer::from(1i32)));
    let aligned = match (&this, &that) {
        (Intrinsic::NatLe(a, b), Intrinsic::NatLt(..)) => Some((
            Intrinsic::nat_lt(
                a.clone(),
                Term::intrinsic(Intrinsic::nat_add(b.clone(), one())),
            ),
            that.clone(),
        )),
        (Intrinsic::NatLt(..), Intrinsic::NatLe(c, d)) => Some((
            this.clone(),
            Intrinsic::nat_lt(
                c.clone(),
                Term::intrinsic(Intrinsic::nat_add(d.clone(), one())),
            ),
        )),
        (Intrinsic::IntLe(a, b), Intrinsic::IntLt(..)) => Some((
            Intrinsic::IntLt(
                a.clone(),
                Term::intrinsic(Intrinsic::IntAdd(b.clone(), plus_one())),
            ),
            that.clone(),
        )),
        (Intrinsic::IntLt(..), Intrinsic::IntLe(c, d)) => Some((
            this.clone(),
            Intrinsic::IntLt(
                c.clone(),
                Term::intrinsic(Intrinsic::IntAdd(d.clone(), plus_one())),
            ),
        )),
        _ => None,
    };
    let (this, that, moved) = match aligned {
        Some((this, that)) => {
            reducer.spend(Cost::term(2))?;
            (this, that, true)
        }
        None => (this, that, moved),
    };

    // An `Int` comparison of widened naturals meets the `Nat` comparison of their preimages — `Nat/to_int(m) < Nat/to_int(n)` is `m < n` — because ℕ → ℤ preserves and reflects order. Probe-side, for the reason the split below is.
    if let Some(pulled) = nat_comparison_of_int(&this, &that) {
        reducer.spend(Cost::term(2))?;
        return Ok(Some((pulled, that)));
    }
    if let Some(pulled) = nat_comparison_of_int(&that, &this) {
        reducer.spend(Cost::term(2))?;
        return Ok(Some((this, pulled)));
    }

    // Two `Int` comparisons of one relation meet through their difference, which only a split taken whether or not anything cancels can see: `0 < j - i` is `i < j`, and `-i < -j` is `j < i`. See `int_split_by_sign` for why this is the judgment's spelling and never the fold's.
    if std::mem::discriminant(&this) == std::mem::discriminant(&that)
        && let (Some(this_split), Some(that_split)) =
            (int_split_comparison(&this), int_split_comparison(&that))
        && (this_split != this || that_split != that)
    {
        reducer.spend(Cost::term(2))?;
        return Ok(Some((this_split, that_split)));
    }
    Ok(moved.then_some((this, that)))
}

/// `int`, an `Int` ordering or equality, as the `Nat` comparison of the same relation over its preimages, when `nat` is that relation and both of `int`'s sides split by sign into widened naturals; `None` otherwise.
fn nat_comparison_of_int(int: &Intrinsic, nat: &Intrinsic) -> Option<Intrinsic> {
    let (a, b, rebuild): (_, _, fn(Term, Term) -> Intrinsic) = match (int, nat) {
        (Intrinsic::IntLt(a, b), Intrinsic::NatLt(..)) => (a, b, Intrinsic::NatLt),
        (Intrinsic::IntLe(a, b), Intrinsic::NatLe(..)) => (a, b, Intrinsic::NatLe),
        (Intrinsic::IntEql(a, b), Intrinsic::NatEql(..)) => (a, b, Intrinsic::NatEql),
        (Intrinsic::IntNeq(a, b), Intrinsic::NatNeq(..)) => (a, b, Intrinsic::NatNeq),
        _ => return None,
    };
    let (left, right) = int_split_by_sign(a, b);
    Some(rebuild(int_preimage(&left)?, int_preimage(&right)?))
}

/// An `Int` ordering or equality with its operands split by sign, through `int_split_by_sign`; `None` for any other intrinsic.
fn int_split_comparison(comparison: &Intrinsic) -> Option<Intrinsic> {
    Some(match comparison {
        Intrinsic::IntLt(a, b) => {
            let (left, right) = int_split_by_sign(a, b);
            Intrinsic::IntLt(left, right)
        }
        Intrinsic::IntLe(a, b) => {
            let (left, right) = int_split_by_sign(a, b);
            Intrinsic::IntLe(left, right)
        }
        Intrinsic::IntEql(a, b) => {
            let (left, right) = int_split_by_sign(a, b);
            Intrinsic::IntEql(left, right)
        }
        Intrinsic::IntNeq(a, b) => {
            let (left, right) = int_split_by_sign(a, b);
            Intrinsic::IntNeq(left, right)
        }
        _ => return None,
    })
}

/// The dual of a negated comparison: an `xor` with a `true` operand whose other operand forces to an ordered or equality comparison on a total order, read as the comparison that is true exactly when it is false. `None` for anything else, the `Flt` comparisons included.
fn dual_of_negated(
    reducer: &mut impl Reducer,
    intrinsic: &Intrinsic,
) -> Result<Option<Intrinsic>, ReduceError> {
    let Intrinsic::BoolXor(left, right) = intrinsic else {
        return Ok(None);
    };
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;
    let negated = match (left.as_bool(), right.as_bool()) {
        (Some(true), _) => right,
        (_, Some(true)) => left,
        _ => return Ok(None),
    };
    let Subterm::Intrinsic(comparison) = &*negated else {
        return Ok(None);
    };
    let Some(dual) = dual_comparison(comparison) else {
        return Ok(None);
    };
    reducer.spend(Cost::term(1))?;
    Ok(Some(dual))
}

/// The comparison that is true exactly when `comparison` is false, on a total order: `a < b` against `b <= a`, `a == b` against `a != b`, and on `Bool` an equality against the `xor` its inequality lowers to. `None` for anything else — a `Flt` comparison, whose negation against a NaN is not the mirror, or a `xor` with a literal operand, which is a negation and not a comparison.
///
/// Two readers: [`align_comparisons`], which spells a negated probe as its dual, and both reducers' refinement probes, which ask a case equation recorded on a guard's written spelling under the guard's dual as well — the false arm of `n < m` is the fact `m <= n`, read the other way.
pub fn dual_comparison(comparison: &Intrinsic) -> Option<Intrinsic> {
    let dual = match comparison {
        Intrinsic::NatLt(a, b) => Intrinsic::NatLe(b.clone(), a.clone()),
        Intrinsic::NatLe(a, b) => Intrinsic::NatLt(b.clone(), a.clone()),
        Intrinsic::NatEql(a, b) => Intrinsic::NatNeq(a.clone(), b.clone()),
        Intrinsic::NatNeq(a, b) => Intrinsic::NatEql(a.clone(), b.clone()),
        Intrinsic::IntLt(a, b) => Intrinsic::IntLe(b.clone(), a.clone()),
        Intrinsic::IntLe(a, b) => Intrinsic::IntLt(b.clone(), a.clone()),
        Intrinsic::IntEql(a, b) => Intrinsic::IntNeq(a.clone(), b.clone()),
        Intrinsic::IntNeq(a, b) => Intrinsic::IntEql(a.clone(), b.clone()),
        // `!=` on `Bool` lowers to `xor` at the `/sys` row, so the dual of an equality is the `xor` its inequality is, and a negated `xor` of two operands is their equality; a `xor` with a literal operand is a negation itself and not a comparison.
        Intrinsic::BoolEql(a, b) | Intrinsic::BoolNeq(a, b)
            if a.as_bool().is_none() && b.as_bool().is_none() =>
        {
            match comparison {
                Intrinsic::BoolEql(..) => Intrinsic::BoolXor(a.clone(), b.clone()),
                _ => Intrinsic::BoolEql(a.clone(), b.clone()),
            }
        }
        Intrinsic::BoolXor(a, b) if a.as_bool().is_none() && b.as_bool().is_none() => {
            Intrinsic::BoolEql(a.clone(), b.clone())
        }
        _ => return None,
    };
    Some(dual)
}

/// The comparison that is true exactly when `comparison` is, spelled across the `<`/`<=` seam: `a < b` is `a + 1 <= b` on `Nat` and on `Int`, where the successor is exact in both directions. `None` where no such spelling exists — a `Nat` `<=` whose left operand carries no successor floor to peel, which is where truncation would otherwise invent one, and every comparison that is not an ordering on those two carriers.
///
/// Read by both reducers' refinement probes, and by them alone: a guard is recorded on its written spelling, so `match i < len(l)` records `i < len(l)` and an obligation reaching the probe as `i + 1 <= len(l)` misses it over a spelling rather than over a fact. [`align_comparisons`] settles the same seam for *conversion*, by the mirror identity `a <= b` ⟺ `a < b + 1`; the two are separate because a probe must produce the key a guard actually recorded, while a congruence needs only one spelling both sides reach.
///
/// **The literal is carried across unchanged**, where [`dual_comparison`]'s is negated: these two comparisons have one truth value rather than opposite ones, so the arm that refined the guard refines this obligation to the same `Bool`.
pub fn successor_comparison(comparison: &Intrinsic) -> Option<Intrinsic> {
    let one = || Term::intrinsic(Intrinsic::Int(Integer::from(1i32)));
    let succ = |term: &Term| {
        let (floor, inner) = Nat::decompose(term);
        Nat::rebuild(floor + Natural::from(1u32), inner)
    };

    // **Every arm adds**, on whichever side keeps the step exact: `a < b` is `a + 1 <= b`, and `a <= b` is `a < b + 1`. Reading either identity backwards would spell a *predecessor*, which on `Nat` exists only where a successor floor is already standing — so adding is what lets this reach every comparison rather than the ones whose operand happens to carry one.
    let (left, right, nat) = match comparison {
        Intrinsic::NatLt(a, b) => (succ(a), b.clone(), true),
        Intrinsic::NatLe(a, b) => (a.clone(), succ(b), true),
        Intrinsic::IntLt(a, b) => (int_sum(a, &one()), b.clone(), false),
        Intrinsic::IntLe(a, b) => (a.clone(), int_sum(b, &one()), false),
        _ => return None,
    };

    // **The shared floor comes off, because the fold this spelling has to meet has already taken it off.** `compare_nat` cancels what both operands carry in common, so a bound `i + 1 <= len + 64` is stuck as `i <= len + 63` while the guard `i < len + 64` is stuck as itself: adding the step back without cancelling would build a spelling the reducer never produces, and the probe would miss on every comparison whose operands share a floor. The cancellation is the fold's own function and is pure, so it runs here rather than by re-entering reduction — which a probe running inside reduction cannot do.
    let (left, right) = match nat {
        true => Nat::cancel_common(&left, &right),
        false => int_cancel_common(&left, &right),
    };

    let spelling = match comparison {
        Intrinsic::NatLt(..) => Intrinsic::nat_lte(left, right),
        Intrinsic::NatLe(..) => Intrinsic::nat_lt(left, right),
        Intrinsic::IntLt(..) => Intrinsic::IntLe(left, right),
        _ => Intrinsic::IntLt(left, right),
    };
    Some(spelling)
}

/// The three pointwise rows fold by one rule: two literal runs of one length combine in the packed representation, and anything else keeps the node it was written as.
///
/// **Two literals of different lengths decline to fold rather than trapping.** The bound in the type is what forbids the pair, and the checker is what enforces it; a reducer that answered here would be deciding a proposition it may not construct — the defect `Intrinsic::signature`'s module documentation records the bound fields as existing to remove — and one that panicked would turn an ill-typed term into a crash instead of a report. Declining costs reductions and never an answer, which is `FreeMonoid::single_generator`'s trade at a different seam.
fn reduce_bin_pointwise(
    reducer: &mut impl Reducer,
    grain: Grain,
    left: &Term,
    right: &Term,
    same_length: &Term,
    combine: impl Fn(&Binary, &Binary) -> Binary,
    rebuild: impl FnOnce(Grain, Term, Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;
    let same_length = reducer.reduce(same_length.clone())?;

    if let (
        Subterm::Intrinsic(Intrinsic::Bin(found_left, run_left)),
        Subterm::Intrinsic(Intrinsic::Bin(found_right, run_right)),
    ) = (&*left, &*right)
        && *found_left == grain
        && *found_right == grain
        && run_left.bit_length() == run_right.bit_length()
    {
        // Three payloads: each operand normalizes into a buffer of its own, and the combined run collects into a third.
        reducer.spend(packed_bound(grain, run_left.bit_length() as u64).saturating_mul(3))?;

        return Ok(Subterm::Intrinsic(Intrinsic::Bin(
            grain,
            combine(run_left, run_right),
        )));
    }

    Ok(Subterm::Intrinsic(rebuild(grain, left, right, same_length)))
}

pub fn reduce_intrinsic(
    reducer: &mut impl Reducer,
    intrinsic: &Intrinsic,
) -> Result<Subterm, ReduceError> {
    match intrinsic {
        Intrinsic::BoolType => Ok(Subterm::Intrinsic(Intrinsic::BoolType)),
        Intrinsic::Bool(value) => Ok(Subterm::Intrinsic(Intrinsic::Bool(*value))),
        Intrinsic::BoolAnd(left, right) => Ok(then_laws(
            reduce_bool_binary(
                reducer,
                left,
                right,
                Right::AsWritten,
                |l, r| l && r,
                Intrinsic::BoolAnd,
            )?,
            |l, r| bool_lattice_laws(l, r, true),
        )),
        Intrinsic::BoolOr(left, right) => Ok(then_laws(
            reduce_bool_binary(
                reducer,
                left,
                right,
                Right::AsWritten,
                |l, r| l || r,
                Intrinsic::BoolOr,
            )?,
            |l, r| bool_lattice_laws(l, r, false),
        )),
        Intrinsic::BoolXor(left, right) => Ok(then_laws(
            reduce_bool_binary(
                reducer,
                left,
                right,
                Right::Reduced,
                |l, r| l != r,
                Intrinsic::BoolXor,
            )?,
            bool_xor_laws,
        )),
        Intrinsic::BoolEql(left, right) => Ok(then_laws(
            reduce_bool_binary(
                reducer,
                left,
                right,
                Right::Reduced,
                |l, r| l == r,
                Intrinsic::BoolEql,
            )?,
            |l, r| bool_eql_laws(l, r, true),
        )),
        Intrinsic::BoolNeq(left, right) => Ok(then_laws(
            reduce_bool_binary(
                reducer,
                left,
                right,
                Right::Reduced,
                |l, r| l != r,
                Intrinsic::BoolNeq,
            )?,
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
        // Inversion of the constructor, not an equation about arithmetic: `Nat/to_byte` states `nat < 256`, so the byte it builds *is* that number and reading it back is the number again. This is the half that makes `Byte` transparent to the bounds oracle — a bound established in `Nat` survives the round trip, where before the trip erased it.
        Intrinsic::ByteToNat(inner) => {
            let inner = reducer.reduce_forced(inner.clone())?;
            Ok(Subterm::Intrinsic(match &*inner {
                Subterm::Intrinsic(Intrinsic::Byte(value)) => {
                    Intrinsic::Nat(Nat::new(usize::from(*value)))
                }
                Subterm::Intrinsic(Intrinsic::NatToByte { nat, .. }) => {
                    return reducer.reduce(nat.clone()).map(Term::unwrap_or_clone);
                }
                _ => Intrinsic::ByteToNat(inner),
            }))
        }
        // A closed operand past the carrier is *refused* rather than masked. Masking made this total by changing a value, which is the one thing a narrowing may not do, and it was the only such row on a numeric carrier; the `below` field is what replaces it, so a program that cannot prove its operand small no longer compiles rather than silently computing a different byte.
        Intrinsic::NatToByte { nat, below } => {
            let nat = reducer.reduce_forced(nat.clone())?;
            if let Subterm::Intrinsic(Intrinsic::ByteToNat(byte)) = &*nat {
                return reducer.reduce(byte.clone()).map(Term::unwrap_or_clone);
            }

            let span = nat.span();
            match nat.as_nat().map(|value| value.to_natural()) {
                Some(Some(value)) => {
                    match u32::try_from(&value)
                        .ok()
                        .and_then(|value| u8::try_from(value).ok())
                    {
                        Some(value) => Ok(Subterm::Intrinsic(Intrinsic::Byte(value))),
                        None => Err(ReduceError::NatToByteAbove { value, span }),
                    }
                }
                _ => Ok(Subterm::Intrinsic(Intrinsic::NatToByte {
                    nat,
                    below: below.clone(),
                })),
            }
        }
        Intrinsic::NatEql(left, right) => reduce_nat_compare(
            reducer,
            left,
            right,
            |c| match c {
                Comparison::Eq => Some(true),
                Comparison::Lt | Comparison::Gt | Comparison::Ne => Some(false),
                Comparison::Le | Comparison::Ge | Comparison::Stuck => None,
            },
            Intrinsic::nat_eql,
        ),
        Intrinsic::NatNeq(left, right) => reduce_nat_compare(
            reducer,
            left,
            right,
            |c| match c {
                Comparison::Eq => Some(false),
                Comparison::Lt | Comparison::Gt | Comparison::Ne => Some(true),
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
                Comparison::Le | Comparison::Ne | Comparison::Stuck => None,
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
                Comparison::Ge | Comparison::Ne | Comparison::Stuck => None,
            },
            Intrinsic::nat_lte,
        ),
        // Bitwise ops fold on unbounded ℕ, which the running program computes too: `and`, `or`, `xor` on the infinite binary expansion, `shl` as `· 2^n` and `shr` as `⌊·/2^n⌋`. The backend's i31 is a fast path that grows into a boxed magnitude, never a width that truncates.
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
        Intrinsic::NatShl(left, right) => {
            let shifted = then_laws(reduce_nat_shl(reducer, left, right)?, nat_shift_laws);
            let shifted = then_coefficient(reducer, shifted, |coefficient, value| {
                Term::intrinsic(Intrinsic::nat_mul(
                    Term::intrinsic(Intrinsic::Nat(Nat::new(coefficient))),
                    value,
                ))
            })?;
            then_power(
                reducer,
                shifted,
                Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                Intrinsic::NatShl,
                |coefficient, value, power| {
                    Term::intrinsic(Intrinsic::nat_mul(
                        Term::intrinsic(Intrinsic::nat_mul(
                            Term::intrinsic(Intrinsic::Nat(Nat::new(coefficient))),
                            value,
                        )),
                        power,
                    ))
                },
            )
        }
        Intrinsic::NatShr(left, right) => {
            let shifted = then_laws(
                reduce_nat_binary(
                    reducer,
                    left,
                    right,
                    |l, r| l.checked_shr(r).map(Intrinsic::Nat),
                    Intrinsic::NatShr,
                )?,
                nat_shift_laws,
            );
            then_split_shift(reducer, shifted, Intrinsic::NatShr)
        }
        Intrinsic::IntType => Ok(Subterm::Intrinsic(Intrinsic::IntType)),
        Intrinsic::Int(value) => Ok(Subterm::Intrinsic(Intrinsic::Int(value.clone()))),
        // The signed comparisons read through the group's difference, as the `Nat` family reads through cancellation: `compare_int` moves what both sides share to one side by sign, and a pair whose residuals are two constants decides.
        Intrinsic::IntEql(left, right) => reduce_int_compare(
            reducer,
            left,
            right,
            |c| match c {
                Comparison::Eq => Some(true),
                Comparison::Lt | Comparison::Gt | Comparison::Ne => Some(false),
                Comparison::Le | Comparison::Ge | Comparison::Stuck => None,
            },
            Intrinsic::IntEql,
        ),
        Intrinsic::IntNeq(left, right) => reduce_int_compare(
            reducer,
            left,
            right,
            |c| match c {
                Comparison::Eq => Some(false),
                Comparison::Lt | Comparison::Gt | Comparison::Ne => Some(true),
                Comparison::Le | Comparison::Ge | Comparison::Stuck => None,
            },
            Intrinsic::IntNeq,
        ),
        // Addition, subtraction and negation all land in the signed sum normal form (`int_sum`): constants fold, like monomials merge by coefficient, and a subtraction is the sum with the subtrahend's coefficients negated, so `i - i` is `0` and `(i + 1) - 1` is `i` for a symbolic `i`, as `Nat`'s form decides them for its own carrier.
        Intrinsic::IntAdd(left, right) => {
            let left = reducer.reduce_forced(left.clone())?;
            let right = reducer.reduce_forced(right.clone())?;
            if let (Some(l), Some(r)) = (left.as_int(), right.as_int()) {
                reducer.spend(operand_bound(l.bits(), r.bits()))?;
            }
            Ok(Term::unwrap_or_clone(int_sum(&left, &right)))
        }
        Intrinsic::IntSub(left, right) => {
            let left = reducer.reduce_forced(left.clone())?;
            let right = reducer.reduce_forced(right.clone())?;
            if let (Some(l), Some(r)) = (left.as_int(), right.as_int()) {
                reducer.spend(operand_bound(l.bits(), r.bits()))?;
            }
            Ok(Term::unwrap_or_clone(int_sum(&left, &int_negate(&right))))
        }
        // Distributes past a constant or a single monomial and stays stuck between two symbolic sums, the line `NatMul` draws; `int_normalize` crosses it where a comparison asks.
        Intrinsic::IntMul(left, right) => {
            let left = reducer.reduce_forced(left.clone())?;
            let right = reducer.reduce_forced(right.clone())?;
            if let (Some(l), Some(r)) = (left.as_int(), right.as_int()) {
                reducer.spend(operand_bound(l.bits(), r.bits()))?;
            }
            let (_, summands_left) = int_terms(&left);
            let (_, summands_right) = int_terms(&right);
            reducer.spend(Cost::collection(
                (summands_left.len() as u64 + 1).saturating_mul(summands_right.len() as u64 + 1),
            ))?;
            Ok(Term::unwrap_or_clone(int_product(&left, &right)))
        }
        Intrinsic::IntDiv {
            dividend,
            divisor,
            non_zero,
        } => reduce_int_division(
            reducer,
            dividend,
            divisor,
            "Int/div",
            |dividend: Integer, divisor: Integer| dividend.div(&divisor).ok(),
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
            |dividend: Integer, divisor: Integer| dividend.rem(&divisor).ok(),
            |dividend, divisor| Intrinsic::IntRem {
                dividend,
                divisor,
                non_zero: non_zero.clone(),
            },
        ),
        Intrinsic::IntLt(left, right) => reduce_int_compare(
            reducer,
            left,
            right,
            |c| match c {
                Comparison::Lt => Some(true),
                Comparison::Eq | Comparison::Gt | Comparison::Ge => Some(false),
                Comparison::Le | Comparison::Ne | Comparison::Stuck => None,
            },
            Intrinsic::IntLt,
        ),
        Intrinsic::IntLe(left, right) => reduce_int_compare(
            reducer,
            left,
            right,
            |c| match c {
                Comparison::Lt | Comparison::Eq | Comparison::Le => Some(true),
                Comparison::Gt => Some(false),
                Comparison::Ge | Comparison::Ne | Comparison::Stuck => None,
            },
            Intrinsic::IntLe,
        ),
        // Bitwise ops fold on unbounded ℤ, which the running program computes too: `and`, `or`, `xor` on the infinite two's-complement expansion, `shl` as `· 2^n` and `shr` as the arithmetic `⌊·/2^n⌋`. The backend's signed i31 is a fast path that grows into a boxed magnitude, never a width that truncates.
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
        Intrinsic::IntShl(left, right) => {
            let shifted = reduce_int_shift(
                reducer,
                left,
                right,
                shift_bound,
                |value, amount| value.shl_within(&amount, u64::MAX),
                Intrinsic::IntShl,
            )?;
            let shifted = then_coefficient(reducer, shifted, |coefficient, value| {
                Term::intrinsic(Intrinsic::IntMul(
                    Term::intrinsic(Intrinsic::Int(Integer::from(coefficient))),
                    value,
                ))
            })?;
            then_power(
                reducer,
                shifted,
                Term::intrinsic(Intrinsic::Int(Integer::from(1i32))),
                Intrinsic::IntShl,
                |coefficient, value, power| {
                    Term::intrinsic(Intrinsic::IntMul(
                        Term::intrinsic(Intrinsic::IntMul(
                            Term::intrinsic(Intrinsic::Int(Integer::from(coefficient))),
                            value,
                        )),
                        power,
                    ))
                },
            )
        }
        Intrinsic::IntShr(left, right) => {
            let shifted = reduce_int_shift(
                reducer,
                left,
                right,
                |value, amount| {
                    operand_bound(
                        value,
                        amount.map_or(0, |amount| u64::from(u64::BITS - amount.leading_zeros())),
                    )
                },
                |value, amount| Some(&value >> &amount),
                Intrinsic::IntShr,
            )?;
            then_split_shift(reducer, shifted, Intrinsic::IntShr)
        }
        Intrinsic::FltType => Ok(Subterm::Intrinsic(Intrinsic::FltType)),
        Intrinsic::Flt(flt) => Ok(Subterm::Intrinsic(Intrinsic::Flt(*flt))),
        Intrinsic::FltAdd(rounding, left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l.sum(r, *rounding)),
            |l, r| Intrinsic::FltAdd(*rounding, l, r),
        ),
        Intrinsic::FltSub(rounding, left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l.difference(r, *rounding)),
            |l, r| Intrinsic::FltSub(*rounding, l, r),
        ),
        Intrinsic::FltMul(rounding, left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l.product(r, *rounding)),
            |l, r| Intrinsic::FltMul(*rounding, l, r),
        ),
        Intrinsic::FltDiv(rounding, left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l.quotient(r, *rounding)),
            |l, r| Intrinsic::FltDiv(*rounding, l, r),
        ),
        // `%` is C's `fmod` over binary64: the exact remainder `x - trunc(x / y) * y`, sign of the dividend, never a rounding — the value `curios-emit`'s `$flt/rem` helper computes.
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
        Intrinsic::FltSqrt(rounding, inner) => reduce_flt_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(v.sqrt(*rounding))),
            |inner| Intrinsic::FltSqrt(*rounding, inner),
        ),
        Intrinsic::FltRoundIntegral(rounding, inner) => reduce_flt_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(v.round_integral(*rounding))),
            |inner| Intrinsic::FltRoundIntegral(*rounding, inner),
        ),
        Intrinsic::FltFma(rounding, a, b, c) => reduce_flt_ternary(
            reducer,
            (a, b, c),
            |a, b, c| Intrinsic::Flt(a.fma(b, c, *rounding)),
            |a, b, c| Intrinsic::FltFma(*rounding, a, b, c),
        ),
        // The two reinterpretations, whose round-trip laws are theorems of the model rather than a postulate: every one of the 2⁶⁴ bit patterns is a distinct float, so `of_le_bytes(to_le_bytes(x))` is `x` for every `x` and `to_le_bytes(of_le_bytes(b))` is `b` for every eight-byte `b`.
        Intrinsic::FltToLeBytes(inner) => reduce_flt_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Bin(Grain::X, v.to_le_bytes())),
            Intrinsic::FltToLeBytes,
        ),
        Intrinsic::FltOfLeBytes { bin, eight_bytes } => {
            let bin = reducer.reduce_forced(bin.clone())?;

            // Inversion of the constructor: decoding what `Flt/to_le_bytes` wrote is the float it was given, NaNs and both zeros included. The other direction holds of the model as well, since no pattern is merged, and folds on a literal; a symbolic one is not inverted here.
            if let Subterm::Intrinsic(Intrinsic::FltToLeBytes(flt)) = &*bin {
                return reducer.reduce(flt.clone()).map(Term::unwrap_or_clone);
            }

            let folded = match &*bin {
                Subterm::Intrinsic(Intrinsic::Bin(Grain::X, packed)) => {
                    Floating::of_le_bytes(packed).ok().map(Intrinsic::Flt)
                }
                _ => None,
            };

            Ok(Subterm::Intrinsic(match folded {
                Some(intrinsic) => intrinsic,
                None => Intrinsic::FltOfLeBytes {
                    bin,
                    eight_bytes: eight_bytes.clone(),
                },
            }))
        }
        // The conversions preserve the number, never the bits — a bit view belongs to explicit `Bin` casts. `Nat/to_int` is total: ℕ embeds in ℤ, and both are unbounded here. The runtime's carrier-range traps stay where they always were, at the `into_wasm` boundary.
        Intrinsic::NatToInt(inner) => {
            let inner = reducer.reduce_forced(inner.clone())?;

            // Inversion of the constructor, as `ByteToNat`'s arm states for its own pair: `Int/to_nat` demands `0 <= int`, so the natural it builds *is* that number and widening it back is the number again. Without it a bound established in `Int` is erased by the round trip, which is what `/std/Map`'s `Key(Int)` has to reconstruct an operand across.
            if let Subterm::Intrinsic(Intrinsic::IntToNat { int, .. }) = &*inner {
                return reducer.reduce(int.clone()).map(Term::unwrap_or_clone);
            }

            // Pushed through `Nat`'s normal form — a literal folds, a floor becomes the constant, a sum and a product widen summand by summand — since the widening is a semiring homomorphism; `int_of_nat` states it.
            Ok(Term::unwrap_or_clone(int_of_nat(&inner)))
        }
        // Into `Flt` the conversions are total and take no proof: rounding is the canonical extension of the embedding, forced by the structure the way monus is for `Nat/sub`, in whichever direction the operation names, and a magnitude past the largest finite value answers what that direction sends an overflow to.
        Intrinsic::NatToFlt(rounding, inner) => reduce_nat_unary(
            reducer,
            inner,
            |v| {
                Some(Intrinsic::Flt(Floating::of_natural(
                    &v.to_natural()?,
                    *rounding,
                )))
            },
            |inner| Intrinsic::NatToFlt(*rounding, inner),
        ),
        // `Int/to_nat` of a negative literal is a value no natural holds — reported like a zero divisor, never wrapped. The bound the operation now states does not retire that report: a bound is discharged in the context the call was written in, and an open term reduces under hypotheses that context may not have. A symbolic operand rebuilds the neutral term, carrying the proof it was handed.
        Intrinsic::IntToNat { int, non_neg } => {
            let span = int.span();
            let int = reducer.reduce_forced(int.clone())?;

            // The other half of the inversion: ℕ embeds in ℤ, so a natural widened to `Int` is non-negative and narrows back to itself whatever proof the narrowing was handed — and so does any non-negative combination of widened naturals, which is the image of its preimage. `int_preimage` reads it; a single widened atom is its one-summand case.
            if let Subterm::Intrinsic(Intrinsic::NatToInt(nat)) = &*int {
                return reducer.reduce(nat.clone()).map(Term::unwrap_or_clone);
            }
            if int.as_int().is_none()
                && let Some(preimage) = int_preimage(&int)
            {
                return Ok(Term::unwrap_or_clone(preimage));
            }

            match int.as_int() {
                Some(value) => match Natural::try_from(&value) {
                    Ok(number) => Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::new(number)))),
                    Err(_) => Err(ReduceError::IntToNatNegative { value, span }),
                },
                None => Ok(Subterm::Intrinsic(Intrinsic::IntToNat {
                    int,
                    non_neg: non_neg.clone(),
                })),
            }
        }
        Intrinsic::IntToFlt(rounding, inner) => reduce_int_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(Floating::of_integer(&v, *rounding))),
            |inner| Intrinsic::IntToFlt(*rounding, inner),
        ),
        // The two narrowings truncate toward zero and answer the *exact* unbounded natural or integer: `to_nat(3.0e9)` is `3000000000`, which the running program holds as a boxed magnitude. Outside the domain each bound states, the model declines and the neutral is rebuilt, carrying the proof it was handed.
        Intrinsic::FltToNat { flt, non_neg } => reduce_flt_unary(
            reducer,
            flt,
            |v| Some(Intrinsic::Nat(Nat::new(v.to_natural().ok()?))),
            |flt| Intrinsic::FltToNat {
                flt,
                non_neg: non_neg.clone(),
            },
        ),
        Intrinsic::FltToInt { flt, finite } => reduce_flt_unary(
            reducer,
            flt,
            |v| Some(Intrinsic::Int(v.to_integer().ok()?)),
            |flt| Intrinsic::FltToInt {
                flt,
                finite: finite.clone(),
            },
        ),
        // The two halves of the exact value, each folding through the one decomposition the model states and declining outside the domain the bound states, as the narrowings do.
        Intrinsic::FltMantissa { flt, finite } => reduce_flt_unary(
            reducer,
            flt,
            |v| Some(Intrinsic::Int(v.to_dyadic().ok()?.0)),
            |flt| Intrinsic::FltMantissa {
                flt,
                finite: finite.clone(),
            },
        ),
        Intrinsic::FltExponent { flt, finite } => reduce_flt_unary(
            reducer,
            flt,
            |v| Some(Intrinsic::Int(Integer::from(v.to_dyadic().ok()?.1))),
            |flt| Intrinsic::FltExponent {
                flt,
                finite: finite.clone(),
            },
        ),
        Intrinsic::BinType(grain) => Ok(Subterm::Intrinsic(Intrinsic::BinType(*grain))),
        Intrinsic::Bin(grain, run) => Ok(Subterm::Intrinsic(Intrinsic::Bin(*grain, run.clone()))),
        Intrinsic::BinLen(grain, bin) => {
            let grain = *grain;
            let bin = reducer.reduce_forced(bin.clone())?;
            // The measure answers a wholly-literal spine by folding it, without rebuilding a `Bin/len` per operand and handing each back to the reducer — which is what made a length over a deep concatenation cost a re-walk of every sub-spine. It agrees with the homomorphism below by construction on the shapes it accepts (a literal run's length, summed over a concatenation's operands) and declines everything else, so every other value reduces exactly as it did.
            if let Some(total) = FreeMonoid::Bin(grain).measure(&bin) {
                return Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::new(total))));
            }
            // `len(replicate(count, x)) = count`, which the measure above answers only once the count is a literal. It has to hold symbolically too, because a fill is how a program builds a run as long as another one: `/std`'s `not` is an `xor` against a fill of its operand's length, and the equal-length bound that guards it has nothing to reduce to without this.
            if let Subterm::Intrinsic(Intrinsic::BinReplicate {
                grain: found,
                count,
                ..
            }) = &*bin
                && *found == grain
            {
                return Ok(Term::unwrap_or_clone(count.clone()));
            }
            // `len(to_bits(b)) = 8 · len(b)` and `len(to_bytes(b)) = len(b) / 8`, which the measure above answers only once the run is literal. Both have to hold symbolically, and the bit-to-byte direction sharpest of all: its own bound is stated over exactly this length, so a run that reached it through a conversion could never discharge one without this.
            //
            // Spelled as shifts rather than a product and a quotient so that neither needs a proof operand — a quotient would want its divisor's, and a reducer that emitted one would be doing the thing the bound fields exist to stop.
            if let Subterm::Intrinsic(Intrinsic::BinReinterp {
                grain: source,
                bin: inner,
                ..
            }) = &*bin
                && source.other() == grain
            {
                // Reduced rather than merely built: the operand's own length is what carries the answer, and a `Bin/len` handed back unreduced is a node nothing downstream re-enters — the bound reading this would find a shift over a stuck measure instead of the count it needs.
                let measured =
                    reducer.reduce(Term::intrinsic(Intrinsic::bin_len(*source, inner.clone())))?;
                let three = Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)));
                let scaled = Term::intrinsic(match source {
                    Grain::X => Intrinsic::NatShl(measured, three),
                    Grain::B => Intrinsic::NatShr(measured, three),
                });

                return Ok(Term::unwrap_or_clone(reducer.reduce(scaled)?));
            }
            let shape = bin_shape(reducer, grain, bin)?;

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
                // A window's length is the count it was cut to: `slice` carries `start + length <= len(base)`, so the count is the measure at every well-typed instance.
                |_, _, length, _| length,
                |sub| Term::intrinsic(Intrinsic::bin_len(grain, sub)),
            )
        }
        Intrinsic::BinEql(grain, left, right) => {
            let grain = *grain;
            let left = reducer.reduce_forced(left.clone())?;
            let right = reducer.reduce_forced(right.clone())?;

            // Reflexivity: any value equals itself. Catches a shared variable before the peel, which would otherwise read it as two identical chunks and say the same.
            if left == right {
                return Ok(Subterm::Intrinsic(Intrinsic::Bool(true)));
            }

            // A bare side — a variable, a projection, anything that is not an intrinsic — is read as the one-chunk spine it is, under the grain this arm already carries: `x[..bs]` is `bs` on values, so the peel's verdict on the wrapped pair is its verdict on the original. What the wrap decides is the clash on a positive residual, `append(bs, k) ~ bs`; the peel needs a head to read the grain off, and a variable has none. The fold's alone: conversion can only refuse such a pair, and inversion solves a bare side as a binder.
            let spine = |side: &Term| match &**side {
                Subterm::Intrinsic(intrinsic) => intrinsic.clone(),
                _ => Intrinsic::BinConcat {
                    grain,
                    operands: vec![side.clone()],
                },
            };

            // Structural decision via the free-monoid peel (`core::spine`): a peeled-equal pair is `true`, a definite generator or length clash is `false` (so `eql([1] ++ x, [2] ++ x) = false` regardless of `x`). Anything the peel leaves undecided stays neutral — the same conservative seam conversion reads, so the fold only ever strengthens, never weakens.
            match peel_bin(&spine(&left), &spine(&right)) {
                Some(Peel::Equal) => return Ok(Subterm::Intrinsic(Intrinsic::Bool(true))),
                Some(Peel::Clash) => return Ok(Subterm::Intrinsic(Intrinsic::Bool(false))),
                Some(Peel::Continue(..)) | Some(Peel::Stuck) | None => {}
            }

            Ok(Subterm::Intrinsic(Intrinsic::BinEql(grain, left, right)))
        }
        Intrinsic::BinGet {
            grain,
            bin,
            index,
            in_range,
        } => {
            let grain = *grain;
            let span = index.span();
            let bin = reducer.reduce_forced(bin.clone())?;
            let index_reduced = reducer.reduce_forced(index.clone())?;
            let i = as_index(&index_reduced);
            // A concrete index into a literal run.
            if let (Subterm::Intrinsic(Intrinsic::Bin(found, run)), Some(i)) = (&*bin, i)
                && *found == grain
            {
                return element_of_run(grain, run, i).ok_or(ReduceError::BinGetOutOfBounds {
                    len: run.len(grain),
                    index: i,
                    span,
                });
            }
            // The cons head's generator: `get(append(x[], k), 0) = k` — the base case of the cons-peel below, and the partner of `BinSlice`'s rules. Without it the peel's symbolic head chunk is this same `append(x[], k)`, so the `0`-index step would rebuild the redex it came from until the budget exhausted.
            if let Subterm::Intrinsic(Intrinsic::BinAppend {
                grain: found,
                bin: base,
                element,
            }) = &*bin
                && *found == grain
                && let Subterm::Intrinsic(Intrinsic::Bin(empty, b)) = &**base
                && *empty == grain
                && b.is_empty()
                && let Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) = &*index_reduced
            {
                return reducer.reduce(element.clone()).map(Term::unwrap_or_clone);
            }
            // A get over a cons spine peels one generator per `0`/`succ` index step: `get(cons(h, t), 0) = h`   and   `get(cons(h, t), succ k) = get(t, k)`.
            // An index is a window of one, so the same two strategies that place a window place an index — see `FreeMonoid::window`. The concrete one narrows the operand holding it down to that single generator; the symbolic one takes a whole operand that already carries exactly one, which is the seam case `get([..p, k], len(p)) = k`.
            match FreeMonoid::Bin(grain).window(
                reducer,
                &bin,
                &index_reduced,
                &Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                |piece| bin_piece(grain, piece),
                |operand| Intrinsic::bin_len(grain, operand.clone()),
            )? {
                Some(Windowed::Parts(parts)) => {
                    if let [only] = parts.as_slice()
                        && let Some(generator) = FreeMonoid::Bin(grain).single_generator(only)
                    {
                        return reducer.reduce(generator).map(Term::unwrap_or_clone);
                    }
                }
                // The index lies inside the last operand: the same `get` over that operand, under the same bound — which the seam walk's cancellation has already turned into a bound on that operand alone.
                Some(Windowed::Inside { operand, start }) => {
                    return reducer
                        .reduce(Term::intrinsic(Intrinsic::bin_get(
                            grain,
                            operand,
                            start,
                            in_range.clone(),
                        )))
                        .map(Term::unwrap_or_clone);
                }
                Some(Windowed::Past { total, start, .. }) => {
                    return Err(ReduceError::BinGetOutOfBounds {
                        len: total,
                        index: start,
                        span,
                    });
                }
                None => {}
            }
            if let Some((head, tail)) = peel_first_atom(grain, &bin) {
                match &*index_reduced {
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => {
                        let zero = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_get(
                                grain,
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
                                grain,
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
                grain,
                bin,
                index_reduced,
                in_range.clone(),
            )))
        }
        Intrinsic::BinSlice {
            grain,
            bin,
            start,
            length,
            within,
        } => {
            let grain = *grain;
            let span = start.span().or_else(|| length.span());
            let bin = reducer.reduce_forced(bin.clone())?;
            let start_reduced = reducer.reduce_forced(start.clone())?;
            let length_reduced = reducer.reduce_forced(length.clone())?;
            // The full slice is the identity: `slice(b, 0, len b) = b`. Sound even for a symbolic `b` — a window covering everything is always in range, never trapping — and the runtime partner of `core::spine`'s window-collapse: it lets a bare full-window `BinSlice` reduce to its base, so a `Bin/slice` over the whole value costs no copy and converts against the base directly.
            if matches!(
                &*start_reduced,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero))
            ) && matches!(&*length_reduced, Subterm::Intrinsic(Intrinsic::BinLen(found, whole)) if *found == grain && *whole == bin)
            {
                return Ok(Term::unwrap_or_clone(bin));
            }
            // The empty slice is empty: `slice(b, i, 0)` is the empty run. The dual of the full-window identity and equally sound — a zero-length window yields no generators regardless of `b` or `i`, and never equates two distinct literals. It lets a codepoint take collapse its zero-width base (`take 0`) to the empty string even over a symbolic cons. Reading a *count* is what makes this one test rather than a comparison of two subjects.
            if Nat::is_zero(&length_reduced) {
                return Ok(Subterm::Intrinsic(Intrinsic::Bin(grain, Binary::empty())));
            }
            let s = as_index(&start_reduced);
            let n = as_index(&length_reduced);
            // A concrete slice of a literal run.
            if let (Subterm::Intrinsic(Intrinsic::Bin(found, run)), Some(s), Some(n)) =
                (&*bin, s, n)
                && *found == grain
            {
                return match s.checked_add(n).and_then(|e| run.slice(grain, s, e)) {
                    Some(slice) => Ok(Subterm::Intrinsic(Intrinsic::Bin(grain, slice))),
                    None => Err(ReduceError::BinSliceOutOfRange {
                        len: run.len(grain),
                        start: s,
                        length: n,
                        span,
                    }),
                };
            }
            // The window, by whichever strategy reaches it — see `FreeMonoid::window`.
            match FreeMonoid::Bin(grain).window(
                reducer,
                &bin,
                &start_reduced,
                &length_reduced,
                |piece| bin_piece(grain, piece),
                |operand| Intrinsic::bin_len(grain, operand.clone()),
            )? {
                Some(Windowed::Parts(parts)) => {
                    return reducer
                        .reduce(Term::intrinsic(Intrinsic::bin_concat(grain, parts)))
                        .map(Term::unwrap_or_clone);
                }
                // The window lies inside the last operand: the same window into that operand, under the same bound.
                Some(Windowed::Inside { operand, start }) => {
                    return reducer
                        .reduce(Term::intrinsic(Intrinsic::bin_slice(
                            grain,
                            operand,
                            start,
                            length_reduced,
                            within.clone(),
                        )))
                        .map(Term::unwrap_or_clone);
                }
                Some(Windowed::Past {
                    total,
                    start,
                    count,
                }) => {
                    return Err(ReduceError::BinSliceOutOfRange {
                        len: total,
                        start,
                        length: count,
                        span,
                    });
                }
                None => {}
            }
            // A slice over a cons spine peels one generator per `0`/`succ` boundary step — the reduction partner of the `x[c, ..t]` cons `/std/Str/Valid`'s proofs walk:  `slice(cons(h, t), 0, succ n) = h ++ slice(t, 0, n)`  and  `slice(cons(h, t), succ s, n) = slice(t, s, n)`.
            //
            // Advancing the start no longer touches the length, which is the reparameterisation paying for itself: the count is invariant under peeling the base, so nothing about the window has to be recomputed to move it.
            if let Some((head, tail)) = peel_first_atom(grain, &bin) {
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
                            grain,
                            tail,
                            zero,
                            dec(&length_reduced),
                            within.clone(),
                        ));
                        let consed = Term::intrinsic(Intrinsic::bin_concat(grain, [head, rest]));
                        return reducer.reduce(consed).map(Term::unwrap_or_clone);
                    }
                    (Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))), _) => {
                        let sliced = Term::intrinsic(Intrinsic::bin_slice(
                            grain,
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
                grain,
                bin,
                start_reduced,
                length_reduced,
                within.clone(),
            )))
        }
        Intrinsic::BinAppend {
            grain,
            bin,
            element,
        } => {
            let grain = *grain;
            let bin = reducer.reduce_forced(bin.clone())?;
            // A concrete generator is a literal of the grain: a `Byte` at X, taken as the runtime's packed-`i8` store and the optimizer's `as u8` take it, a `Bool` at B. A symbolic operand reads as no generator, so it stays stuck rather than truncating.
            let element = reducer.reduce_forced(element.clone())?;
            if let Subterm::Intrinsic(Intrinsic::Bin(found, run)) = &*bin
                && *found == grain
                && let Some(generator) = Generator::read(grain, &element)
            {
                // Twice the whole rebuilt value: an append copies the packed payload out and then copies the extended run into a fresh buffer. Appending one generator therefore costs the length of everything appended so far, twice — which is the shape that makes a naive accumulation quadratic, and the reason it is charged rather than treated as an increment. Charged before the copy, which is why the generator is read first.
                reducer.spend(
                    packed_bound(grain, run.bit_length() as u64 + grain.bits() as u64)
                        .saturating_mul(2),
                )?;

                let appended = generator
                    .appended_to(run)
                    .expect("a literal run of a grain takes that grain's generator");

                return Ok(Subterm::Intrinsic(Intrinsic::Bin(grain, appended)));
            }

            Ok(Subterm::Intrinsic(Intrinsic::bin_append(
                grain, bin, element,
            )))
        }
        Intrinsic::BinConcat { grain, operands } => {
            let grain = *grain;
            let reduced: Vec<Term> = operands
                .iter()
                .map(|e| reducer.reduce_forced(e.clone()))
                .collect::<Result<_, _>>()?;
            // Normalise by the monoid unit/associativity laws — drop the empty identity (so `concat(x[], a)`/`concat(a, x[])` collapse to `a`), fuse an all-literal survivor set with `Binary::concat`, collapse a lone operand. Grain-generic: both carriers fuse in the packed representation. The definitional partner of `peel_bin`'s `x[]`-handling (`core::spine`); see `normalize_concat`.
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
                    // Twice the fused payload, per the price list's last paragraph: `Binary::concat` fills a `Vec<u8>` and then converts it into an `Arc<[u8]>`, which allocates a second buffer of the same length. The operation costs two payloads even though one survives.
                    let bits = runs
                        .iter()
                        .map(|run| run.bit_length() as u64)
                        .fold(0u64, u64::saturating_add);
                    reducer.spend(packed_bound(grain, bits).saturating_mul(2))?;

                    Ok(Subterm::Intrinsic(Intrinsic::Bin(
                        grain,
                        Binary::concat(runs),
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
        Intrinsic::BinReplicate { grain, count, atom } => {
            let grain = *grain;
            let count = reducer.reduce_forced(count.clone())?;
            let atom = reducer.reduce_forced(atom.clone())?;

            if let Some(n) = as_index(&count)
                && let Some(generator) = Generator::read(grain, &atom)
            {
                // The payload the fill materializes, charged before it exists. This is the one construction here whose size an *operand names* rather than an operand carrying it, so a count the program computed is exactly what has to be priced — the budget is the whole of what stands between a fill and the machine.
                reducer.spend(packed_bound(
                    grain,
                    (n as u64).saturating_mul(grain.bits() as u64),
                ))?;

                return Ok(Subterm::Intrinsic(Intrinsic::Bin(
                    grain,
                    generator.replicated(n),
                )));
            }

            Ok(Subterm::Intrinsic(Intrinsic::bin_replicate(
                grain, count, atom,
            )))
        }
        Intrinsic::BinReinterp {
            grain,
            bin,
            aligned,
        } => {
            let grain = *grain;
            let bin = reducer.reduce_forced(bin.clone())?;
            let aligned = reducer.reduce(aligned.clone())?;

            // Reading a run at the other grain and back is the run: regrouping moves no bit, and the alignment the inner reinterpretation demanded is what makes the composite well formed at all. Both directions hold, because each is the other's inverse — unlike the float pair above, where only one side starts from a canonical value.
            if let Subterm::Intrinsic(Intrinsic::BinReinterp {
                grain: inner_grain,
                bin: inner,
                ..
            }) = &*bin
                && inner_grain.other() == grain
            {
                return reducer.reduce(inner.clone()).map(Term::unwrap_or_clone);
            }

            // One condition serves both directions: a byte run's bit length is eight times its count and so always passes, while a bit run's is exactly what the bound at `B` states. A run that fails it declines to fold rather than answering, for `reduce_bin_pointwise`'s reason — the bound is the checker's to enforce, and there is no byte to answer with besides.
            if let Subterm::Intrinsic(Intrinsic::Bin(found, run)) = &*bin
                && *found == grain
                && run.bit_length().is_multiple_of(8)
            {
                let (regrained, cost) = match run.is_x_aligned() {
                    // Nothing moves: the payload is shared and only the grain its length is read in changes.
                    true => (run.clone(), Cost::NOTHING),
                    // A window holding whole bytes at an offset that is not one has the right count of bits and the wrong place to share them from, so this one is repacked rather than retagged.
                    false => (
                        Binary::from_bytes(run.to_packed_bytes()),
                        packed_bound(Grain::X, run.bit_length() as u64),
                    ),
                };
                reducer.spend(cost)?;

                return Ok(Subterm::Intrinsic(Intrinsic::Bin(grain.other(), regrained)));
            }

            Ok(Subterm::Intrinsic(Intrinsic::bin_reinterp(
                grain, bin, aligned,
            )))
        }
        Intrinsic::BinAnd {
            grain,
            left,
            right,
            same_length,
        } => reduce_bin_pointwise(
            reducer,
            *grain,
            left,
            right,
            same_length,
            Binary::and,
            Intrinsic::bin_and,
        ),
        Intrinsic::BinOr {
            grain,
            left,
            right,
            same_length,
        } => reduce_bin_pointwise(
            reducer,
            *grain,
            left,
            right,
            same_length,
            Binary::or,
            Intrinsic::bin_or,
        ),
        Intrinsic::BinXor {
            grain,
            left,
            right,
            same_length,
        } => reduce_bin_pointwise(
            reducer,
            *grain,
            left,
            right,
            same_length,
            Binary::xor,
            Intrinsic::bin_xor,
        ),
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
            if let Some(total) = FreeMonoid::List.measure(&list) {
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
                // The `List` twin of `BinLen`'s window law: the count a window was cut to is its length.
                |_, _, length, _| length,
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
            // An index into a map is the function at the index into its argument: a map is elementwise, and the bound carries over unchanged because `len(map(xs, f))` reduces to `len(xs)` and a bound is a proof. The `get` twin of the `len` law above.
            if let Subterm::Intrinsic(Intrinsic::ListMap {
                from,
                list: inner,
                function,
                ..
            }) = &*list
            {
                let inner_get = Term::intrinsic(Intrinsic::list_get(
                    from.clone(),
                    inner.clone(),
                    index.clone(),
                    in_range.clone(),
                ));
                return reducer
                    .reduce(Term::apply(function.clone(), [inner_get]))
                    .map(Term::unwrap_or_clone);
            }
            // An index is a window of one, so the same two strategies that place a window place an index — see `FreeMonoid::window`. The `List` twin of `BinGet`'s, down to the seam case `get([..p, k], len(p)) = k`.
            match FreeMonoid::List.window(
                reducer,
                &list,
                &index_reduced,
                &Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                |piece| list_piece(&type_, piece),
                |operand| Intrinsic::list_len(type_.clone(), operand.clone()),
            )? {
                Some(Windowed::Parts(parts)) => {
                    if let [only] = parts.as_slice()
                        && let Some(generator) = FreeMonoid::List.single_generator(only)
                    {
                        return reducer.reduce(generator).map(Term::unwrap_or_clone);
                    }
                }
                // The `List` twin of `BinGet`'s: the index lies inside the last operand, so it is the same `get` over that operand under the same bound.
                Some(Windowed::Inside { operand, start }) => {
                    return reducer
                        .reduce(Term::intrinsic(Intrinsic::list_get(
                            type_,
                            operand,
                            start,
                            in_range.clone(),
                        )))
                        .map(Term::unwrap_or_clone);
                }
                Some(Windowed::Past { total, start, .. }) => {
                    return Err(ReduceError::ListGetOutOfBounds {
                        len: total,
                        index: start,
                        span: index.span(),
                    });
                }
                None => {}
            }
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
            // The window, by whichever strategy reaches it — see `FreeMonoid::window`.
            match FreeMonoid::List.window(
                reducer,
                &list,
                &start_reduced,
                &length_reduced,
                |piece| list_piece(&type_, piece),
                |operand| Intrinsic::list_len(type_.clone(), operand.clone()),
            )? {
                Some(Windowed::Parts(parts)) => {
                    return reducer
                        .reduce(Term::intrinsic(Intrinsic::list_concat(type_, parts)))
                        .map(Term::unwrap_or_clone);
                }
                // The `List` twin of `BinSlice`'s: the window lies inside the last operand, so it is the same window into that operand under the same bound.
                Some(Windowed::Inside { operand, start }) => {
                    return reducer
                        .reduce(Term::intrinsic(Intrinsic::list_slice(
                            type_,
                            operand,
                            start,
                            length_reduced,
                            within.clone(),
                        )))
                        .map(Term::unwrap_or_clone);
                }
                Some(Windowed::Past {
                    total,
                    start: from,
                    count,
                }) => {
                    return Err(ReduceError::ListSliceOutOfRange {
                        len: total,
                        start: from,
                        length: count,
                        span: start.span().or_else(|| length.span()),
                    });
                }
                None => {}
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
            // `map` with the identity is the list itself, whatever its shape: a function whose body reduces to its one binder sends every element to itself.
            if is_identity(reducer, &f)? {
                return Ok(Term::unwrap_or_clone(list));
            }
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
                // `map(f, slice(xs, s, n)) = slice(map(f, xs), s, n)`: a map is elementwise, so the window moves inside it unchanged, and so does its bound — `len(map(f, xs))` reduces to `len(xs)`, so the proof that placed the window on `xs` places it on the image, and nothing here derives one.
                |base, start, length, within| {
                    Term::intrinsic(Intrinsic::list_slice(
                        b.clone(),
                        Term::intrinsic(Intrinsic::list_map(a.clone(), b.clone(), base, f.clone())),
                        start,
                        length,
                        within,
                    ))
                },
                |sub| Term::intrinsic(Intrinsic::list_map(a.clone(), b.clone(), sub, f.clone())),
            )
        }
        // A left fold over the free-monoid shape: a literal run applies `f` element by element from the left, a concatenation folds its operands in order threading the accumulator, and an append folds its base and then the appended element — so `fold([h, ..t], z, f) = fold(t, f(h, z), f)` and `fold([..a, ..b], z, f) = fold(b, fold(a, z, f), f)` are definitional. A window or an opaque value stays neutral: the fold's value depends on every element, and none is in hand. Not a homomorphism — the accumulator threads through — so it does not go through `reduce_homomorphism`.
        Intrinsic::ListFold {
            element,
            result,
            list,
            init,
            function,
        } => {
            let element = reducer.reduce(element.clone())?;
            let result = reducer.reduce(result.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            let init = reducer.reduce(init.clone())?;
            let function = reducer.reduce(function.clone())?;
            let fold = |list: Term, init: Term| {
                Term::intrinsic(Intrinsic::list_fold(
                    element.clone(),
                    result.clone(),
                    list,
                    init,
                    function.clone(),
                ))
            };
            let step = |item: Term, acc: Term| Term::apply(function.clone(), [item, acc]);
            match list_shape(list) {
                Shape::Literal(items) => {
                    reducer.spend(Cost::collection(items.len() as u64))?;
                    let folded = items.into_iter().fold(init, |acc, item| step(item, acc));
                    reducer.reduce(folded).map(Term::unwrap_or_clone)
                }
                Shape::Concat(operands) => {
                    reducer.spend(Cost::collection(operands.len() as u64))?;
                    let folded = operands
                        .into_iter()
                        .fold(init, |acc, operand| fold(operand, acc));
                    reducer.reduce(folded).map(Term::unwrap_or_clone)
                }
                Shape::Append(base, item) => reducer
                    .reduce(step(item, fold(base, init)))
                    .map(Term::unwrap_or_clone),
                Shape::Window {
                    base,
                    start,
                    length,
                    within,
                } => Ok(Subterm::Intrinsic(Intrinsic::list_fold(
                    element.clone(),
                    result.clone(),
                    Term::intrinsic(Intrinsic::list_slice(
                        element.clone(),
                        base,
                        start,
                        length,
                        within,
                    )),
                    init,
                    function.clone(),
                ))),
                Shape::Opaque(value) => Ok(Subterm::Intrinsic(Intrinsic::list_fold(
                    element.clone(),
                    result.clone(),
                    value,
                    init,
                    function.clone(),
                ))),
            }
        }
        // The handle type and handle tokens are inert values, like `Nat`/`Nat(_)`.
        Intrinsic::HandleType => Ok(Subterm::Intrinsic(Intrinsic::HandleType)),
        Intrinsic::Handle(token) => Ok(Subterm::Intrinsic(Intrinsic::Handle(*token))),
        // Every operation the host performs is an `Io`, which is to say a *description*: it denotes one inert value here and becomes a host call only at erasure, where the entrypoint boundary forces the program's description exactly once.
        //
        // These arms used to refuse instead, and the refusal was the type-level half of the effect discipline: a spelling that does not fix a value must not reach a type. It is now the typing that keeps them out — a term of non-`Io` type cannot perform an effect, and an `Io` supports no elimination through which one could reach a type position. So the operands reduce, the node rebuilds, and nothing else follows.
        Intrinsic::CellType(element) => Ok(Subterm::Intrinsic(Intrinsic::CellType(
            reducer.reduce(element.clone())?,
        ))),
        Intrinsic::ChannelType(element) => Ok(Subterm::Intrinsic(Intrinsic::ChannelType(
            reducer.reduce(element.clone())?,
        ))),
        Intrinsic::Cell { element } => Ok(Subterm::Intrinsic(Intrinsic::Cell {
            element: reducer.reduce(element.clone())?,
        })),
        Intrinsic::CellFill {
            element,
            cell,
            value,
        } => Ok(Subterm::Intrinsic(Intrinsic::CellFill {
            element: reducer.reduce(element.clone())?,
            cell: reducer.reduce(cell.clone())?,
            value: reducer.reduce(value.clone())?,
        })),
        Intrinsic::CellPoll {
            element,
            cell,
            universes,
        } => Ok(Subterm::Intrinsic(Intrinsic::CellPoll {
            element: reducer.reduce(element.clone())?,
            cell: reducer.reduce(cell.clone())?,
            universes: universes.clone(),
        })),
        Intrinsic::Channel {
            element,
            capacity,
            positive,
        } => Ok(Subterm::Intrinsic(Intrinsic::Channel {
            element: reducer.reduce(element.clone())?,
            capacity: reducer.reduce(capacity.clone())?,
            positive: reducer.reduce(positive.clone())?,
        })),
        Intrinsic::ChannelPush {
            element,
            channel,
            value,
        } => Ok(Subterm::Intrinsic(Intrinsic::ChannelPush {
            element: reducer.reduce(element.clone())?,
            channel: reducer.reduce(channel.clone())?,
            value: reducer.reduce(value.clone())?,
        })),
        Intrinsic::ChannelTake {
            element,
            channel,
            universes,
        } => Ok(Subterm::Intrinsic(Intrinsic::ChannelTake {
            element: reducer.reduce(element.clone())?,
            channel: reducer.reduce(channel.clone())?,
            universes: universes.clone(),
        })),
        Intrinsic::ChannelClose { element, channel } => {
            Ok(Subterm::Intrinsic(Intrinsic::ChannelClose {
                element: reducer.reduce(element.clone())?,
                channel: reducer.reduce(channel.clone())?,
            }))
        }
        Intrinsic::ChannelClosed { element, channel } => {
            Ok(Subterm::Intrinsic(Intrinsic::ChannelClosed {
                element: reducer.reduce(element.clone())?,
                channel: reducer.reduce(channel.clone())?,
            }))
        }
        Intrinsic::ChannelCount { element, channel } => {
            Ok(Subterm::Intrinsic(Intrinsic::ChannelCount {
                element: reducer.reduce(element.clone())?,
                channel: reducer.reduce(channel.clone())?,
            }))
        }
        Intrinsic::ChannelCapacity { element, channel } => {
            Ok(Subterm::Intrinsic(Intrinsic::ChannelCapacity {
                element: reducer.reduce(element.clone())?,
                channel: reducer.reduce(channel.clone())?,
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
mod shift_tests;
#[cfg(test)]
mod test_support;
#[cfg(test)]
mod truth_tests;
