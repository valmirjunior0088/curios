use super::*;

/// The frame bill is the peak times the frame price, because the row charges once per new peak. A change to either side of that identity has to move this test.
#[test]
fn depth_costs_its_peak_times_the_frame() {
    let consumption = Consumption::new(10_000, 3);

    assert_eq!(consumption.frame_units(), 3 * Cost::FRAME.get());
    assert_eq!(consumption.other_units(), 10_000 - 3 * Cost::FRAME.get());
}

/// A refused judgment stops partway through a charge, so its two counters can disagree; the remainder floors at zero rather than wrapping into an enormous number.
#[test]
fn a_partial_judgment_reports_no_negative_remainder() {
    let refused = Consumption::new(1, 400);

    assert!(refused.frame_units() > refused.units());
    assert_eq!(refused.other_units(), 0);
}

/// The fold reports the heaviest declaration *with its own* depth, not the heaviest units beside the deepest peak from somewhere else.
#[test]
fn the_heavier_judgment_carries_its_own_depth() {
    let heavy_shallow = Consumption::new(9_000, 2);
    let light_deep = Consumption::new(100, 900);

    assert_eq!(heavy_shallow.heavier_of(light_deep), heavy_shallow);
    assert_eq!(light_deep.heavier_of(heavy_shallow), heavy_shallow);
    // Ties keep the left, which is what makes the fold deterministic over a walk order.
    let tie = Consumption::new(9_000, 7);
    assert_eq!(heavy_shallow.heavier_of(tie), heavy_shallow);
}

/// The identity a whole-module fold starts from spends nothing and reaches nowhere.
#[test]
fn nothing_consumed_is_the_fold_identity() {
    let nothing = Consumption::default();

    assert_eq!(nothing.units(), 0);
    assert_eq!(nothing.frame_units(), 0);
    assert_eq!(nothing.heavier_of(Consumption::new(5, 1)).units(), 5);
}
