//! What a packed, shifted, summed or distributed result costs, charged before the value is built.
//!
//! **An asymmetry in what two spellings cost can be earned rather than drifted into.** Two formulas that differ may each be right for its own implementation — a bit-grain append that rebuilt its value a `bool` per bit would earn a whole extra buffer that a byte-grain copy of the packed payload does not. Before unifying two prices, check whether the operations differ for a reason — where they do, the reason is the thing to remove, and unifying over it writes the accident into the budget permanently.
//!
//! Each bound is computed from operand *sizes* rather than from the result, so a fold that would allocate more than the budget allows is refused before `num-bigint` is asked for anything — the difference between a diagnostic and a process the allocator does not return from.

use {crate::Cost, curios_algebra::distribution_size, curios_num::Grain};

/// What a packed value of `bits` logical bits costs, in its grain's own row of the price list.
///
/// One function rather than two call sites choosing a row, because the two rows differ by a factor of eight and picking the wrong one undercharges by that factor at the byte grain.
pub(super) fn packed_bound(grain: Grain, bits: u64) -> Cost {
    match grain {
        Grain::X => Cost::packed_bytes(bits / 8),
        Grain::B => Cost::packed_bits(bits),
    }
}

/// What a closed binary fold on two big numbers may construct, charged before it runs.
///
/// Every operation routed through [`reduce_nat_binary`](super::nat::reduce_nat_binary) and [`reduce_int_binary`](super::scalar::reduce_int_binary) has a result no wider than `left + right + 1` bits: a sum is at most one bit past the wider operand, a product is exactly the two widths together, a quotient or remainder is no wider than its dividend, and a bitwise operation is no wider than the wider operand. One conservative bound rather than six exact ones, because the price list permits overcharging and forbids the opposite — six formulas would be six chances to get the direction wrong for a saving no program would notice.
///
/// **The shifts are deliberately not routed through those two**, and that is the whole reason this is a named function with a doc rather than an expression. See [`shift_bound`].
pub(super) fn operand_bound(left: u64, right: u64) -> Cost {
    Cost::big_number(left.saturating_add(right).saturating_add(1))
}

/// What a closed shift may construct: the value's width plus the shift *amount*.
///
/// The one fold in the roster whose result size is not bounded by its operands' sizes, and it is reachable from three lines of surface Curios with no loop in them — `Nat/shl(1, 400000000)` builds fifty megabytes of magnitude while a transition counter sees a single step. The charge is computed from the amount before `num-bigint` is asked for anything, so the refusal happens instead of the allocation rather than after it.
///
/// `amount` is `None` when the second operand is symbolic or does not fit a `u64`, and then this charges **nothing** — because the fold declines in exactly that case and constructs nothing to charge for. It is read as a `u64` rather than a `usize` for the reason the machine narrowings out of [`Natural`](curios_num::Natural) state: a charge that differed between the native and wasm32 targets would break the promise that a program compiling in the playground compiles at the command line.
///
/// Pricing first also closes a target divergence in the *fold*, which reads its amount through `to_usize` and therefore folds natively what it leaves neutral on wasm32. Any amount large enough for the two to disagree prices far past any budget, so both targets refuse before either reaches the shift.
pub(super) fn shift_bound(value: u64, amount: Option<u64>) -> Cost {
    match amount {
        Some(amount) => Cost::big_number(value.saturating_add(amount)),
        None => Cost::NOTHING,
    }
}

/// What a sum of `summands` symbolic summands costs to write back in normal form: the collection they are read into and a node a summand, the idiom a distributed product is charged by.
///
/// A fold writes the whole sum again wherever one is added to or cancelled against, so what it builds is as wide as its operands, whatever the step added. Nothing for a sum with no symbolic summand, which is a literal and [`operand_bound`]'s.
pub(crate) fn sum_bound(summands: usize) -> Cost {
    match summands as u64 {
        0 => Cost::NOTHING,
        summands => {
            Cost::collection(summands).saturating_add(Cost::term(2).saturating_mul(summands))
        }
    }
}

/// A reduced term's footprint in the sum normal form: its summands, a literal part counted as one over no factor, the factors they hold between them, and the bits of their coefficients. What a fold that writes the form back is charged by, counted before anything is written.
pub(crate) struct Footprint {
    pub(crate) summands: usize,
    pub(crate) factors: usize,
    pub(crate) bits: u64,
}

/// What a product of two operands costs to write back distributed: the collection its monomials are read into, a node for each monomial and for each factor it holds, and the limbs of its coefficients.
///
/// Every summand of one operand meets every summand of the other, so an operand's factors and its coefficients are written once for each summand across from it: a monomial is as long as the two it is the product of and its coefficient as wide as theirs together, which a count of monomials alone prices as one node whatever its degree. Nothing for a product with no factor, which is a literal and [`operand_bound`]'s.
pub(crate) fn product_bound(left: &Footprint, right: &Footprint) -> Cost {
    let across = |own: u64, other: &Footprint| own.saturating_mul(other.summands as u64);
    let products = distribution_size(left.summands, right.summands);
    let factors =
        across(left.factors as u64, right).saturating_add(across(right.factors as u64, left));
    let bits = across(left.bits, right).saturating_add(across(right.bits, left));

    match factors {
        0 => Cost::NOTHING,
        factors => Cost::collection(products)
            .saturating_add(Cost::term(2).saturating_mul(products.saturating_add(factors)))
            .saturating_add(Cost::big_number(bits)),
    }
}

/// What the image of a normal form under the embedding of one carrier in the other costs to write, the widening's and the narrowing's alike: the collection its summands are read into, a node for each summand, two for each factor — the factor's image and its place in the monomial — and the limbs of its coefficients.
///
/// A monomial's image is one spine whatever its degree, so the bound is linear in the factors, which a count of summands prices as one node each. Nothing for a term with no factor, which is a literal.
pub(crate) fn image_bound(operand: &Footprint) -> Cost {
    let summands = operand.summands as u64;

    match operand.factors as u64 {
        0 => Cost::NOTHING,
        factors => Cost::collection(summands)
            .saturating_add(
                Cost::term(2).saturating_mul(summands.saturating_add(factors.saturating_mul(2))),
            )
            .saturating_add(Cost::big_number(operand.bits)),
    }
}
