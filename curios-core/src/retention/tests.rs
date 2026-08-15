use super::*;

/// An allowance that cannot cover a charge is left exactly as it was, so a declined insertion costs nothing and a later smaller one can still succeed. A quota that spent-then-declined would starve itself on the first oversized entry.
#[test]
fn a_declined_insertion_leaves_the_allowance_untouched() {
    let mut retention = Retention::new(100);

    assert!(retention.admits(Cost::units(60)));
    assert_eq!(retention.spent(), 60);

    assert!(!retention.admits(Cost::units(60)));
    assert_eq!(retention.spent(), 60);

    assert!(retention.admits(Cost::units(40)));
    assert_eq!(retention.spent(), 100);
}

/// Exhaustion is permanent within a compilation: there is no boundary that restores it, which is the whole difference between this counter and the work budget.
#[test]
fn exhaustion_is_permanent() {
    let mut retention = Retention::new(10);

    assert!(retention.admits(Cost::units(10)));
    assert!(!retention.admits(Cost::units(1)));
    assert!(!retention.admits(Cost::NOTHING.saturating_add(Cost::units(1))));
    assert_eq!(retention.spent(), 10);
}

/// A charge that saturated while being computed is declined rather than compared, exactly as the work budget declines one.
#[test]
fn a_saturated_charge_is_declined() {
    let mut retention = Retention::new(u64::MAX);

    assert!(!retention.admits(Cost::REFUSED));
    assert_eq!(retention.spent(), 0);
}
