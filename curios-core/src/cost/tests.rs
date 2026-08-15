use super::*;

/// The rounding is upward in every formula, because a charge that rounds down undercharges — which is the one direction the specification forbids.
#[test]
fn every_payload_formula_rounds_up() {
    // A single byte still costs a whole unit, and eight bytes still cost one.
    assert_eq!(
        Cost::packed_bytes(1).get(),
        Cost::packed_bytes(8).get(),
        "one byte and eight bytes are one unit either way"
    );
    assert_eq!(Cost::packed_bytes(9).get(), Cost::packed_bytes(8).get() + 1);

    assert_eq!(Cost::packed_bits(1).get(), Cost::packed_bits(64).get());
    assert_eq!(Cost::packed_bits(65).get(), Cost::packed_bits(64).get() + 1);

    assert_eq!(Cost::big_number(1).get(), Cost::big_number(64).get());
    assert_eq!(Cost::big_number(65).get(), Cost::big_number(64).get() + 1);
}

/// An empty payload is not free: the value still exists, and a program building a million empty ones has built a million values.
#[test]
fn an_empty_payload_still_costs_its_header() {
    for empty in [
        Cost::packed_bytes(0),
        Cost::packed_bits(0),
        Cost::big_number(0),
        Cost::collection(0),
        Cost::buffer(0),
        Cost::term(0),
    ] {
        assert!(empty.get() > Cost::NOTHING.get(), "{empty:?}");
    }
}

/// Payload is linear in its length, so a formula cannot quietly become sublinear in the size it is meant to bound.
#[test]
fn payload_is_linear_in_length() {
    let one = Cost::packed_bytes(8 * 1_000).get() - Cost::packed_bytes(0).get();
    let two = Cost::packed_bytes(8 * 2_000).get() - Cost::packed_bytes(0).get();

    assert_eq!(two, one * 2);
}

/// Overflow lands on a refusal rather than wrapping to a small number a budget would happily afford. This is the direction that matters: the wrapped value is what would be handed to an allocator.
#[test]
fn overflow_saturates_into_a_refusal() {
    let huge = Cost::units(u64::MAX - 1);

    assert!(huge.saturating_add(Cost::STEP).is_refused());
    assert!(huge.saturating_add(huge).is_refused());
    assert!(huge.saturating_mul(2).is_refused());
    assert!(Cost::REFUSED.is_refused());
    assert!(!Cost::units(u64::MAX - 1).is_refused());
}

/// A bit length near the top of the range is priced without wrapping, which is the path a shift amount reaches.
#[test]
fn a_magnitude_near_the_top_of_the_range_is_priced_not_wrapped() {
    let priced = Cost::big_number(u64::MAX);

    assert!(!priced.is_refused());
    assert!(priced.get() >= u64::MAX / 64);
}

/// Summing charges is the same saturating addition, so a fold over many operands cannot escape the rule the pairwise operation states.
#[test]
fn summing_charges_saturates_too() {
    let refused: Cost = [Cost::units(u64::MAX - 1), Cost::units(2)]
        .into_iter()
        .sum();

    assert!(refused.is_refused());

    let plain: Cost = [Cost::units(3), Cost::units(4)].into_iter().sum();
    assert_eq!(plain.get(), 7);
}

/// A composite charge names the row that made it expensive, so a refusal points at the payload rather than at the header that happened to be added first.
#[test]
fn the_larger_contributor_keeps_its_category() {
    let header = Cost::collection(1);
    let payload = Cost::packed_bytes(8 * 10_000);

    assert_eq!(header.saturating_add(payload).category(), Category::Payload);
    assert_eq!(payload.saturating_add(header).category(), Category::Payload);
    // Ties keep the left, which is what makes the rule deterministic rather than merely reasonable.
    assert_eq!(
        Cost::term(0).saturating_add(Cost::term(0)).category(),
        Category::Reconstruction
    );
}

/// Multiplying keeps the row: two payloads are still payload, which is what `PackedBin::concat`'s double charge has to report.
#[test]
fn multiplying_keeps_the_category() {
    let doubled = Cost::packed_bits(64).saturating_mul(2);

    assert_eq!(doubled.category(), Category::Payload);
    assert_eq!(doubled.get(), Cost::packed_bits(64).get() * 2);
}
