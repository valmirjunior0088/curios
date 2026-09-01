//! What a packed or shifted result costs, charged before the value is built.
//!
//! Each bound is computed from operand *sizes* rather than from the result, so a fold that would allocate more than the budget allows is refused before `num-bigint` is asked for anything — the difference between a diagnostic and a process the allocator does not return from.

use {crate::Cost, curios_utilities::Grain};

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
/// `amount` is `None` when the second operand is symbolic or does not fit a `u64`, and then this charges **nothing** — because the fold declines in exactly that case and constructs nothing to charge for. It is read as a `u64` rather than a `usize` for the reason [`Natural::to_u64`](curios_num::Natural::to_u64) states: a charge that differed between the native and wasm32 targets would break the promise that a program compiling in the playground compiles at the command line.
///
/// Pricing first also closes a target divergence in the *fold*, which reads its amount through `to_usize` and therefore folds natively what it leaves neutral on wasm32. Any amount large enough for the two to disagree prices far past any budget, so both targets refuse before either reaches the shift.
pub(super) fn shift_bound(value: u64, amount: Option<u64>) -> Cost {
    match amount {
        Some(amount) => Cost::big_number(value.saturating_add(amount)),
        None => Cost::NOTHING,
    }
}
