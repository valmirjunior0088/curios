use {super::Bound, crate::Lattice, curios_num::Natural};

fn of(value: u64) -> Bound {
    Bound::of(&Natural::from(value))
}

/// The laws `Solver::solve` terminates on: bottom is the identity, and join commutes, associates and is idempotent.
#[test]
fn join_obeys_the_lattice_laws() {
    let samples = [
        Bound::bottom(),
        of(1),
        of(300),
        Bound::WORD,
        Bound::LENGTH,
        Bound::UNBOUNDED,
    ];
    for &a in &samples {
        assert_eq!(a.wider(Bound::bottom()), a);
        assert_eq!(a.wider(a), a);
        for &b in &samples {
            assert_eq!(a.wider(b), b.wider(a));
            for &c in &samples {
                assert_eq!(a.wider(b).wider(c), a.wider(b.wider(c)));
            }
        }
    }
}

/// A word holds a natural below `2³⁰` and nothing at or past it, the i31's non-negative half.
#[test]
fn a_word_holds_exactly_what_lies_below_two_to_the_thirty() {
    assert!(of((1 << 30) - 1).is_word());
    assert!(!of(1 << 30).is_word());
    assert!(Bound::BYTE.is_word() && Bound::BIT.is_word());
    assert!(!Bound::LENGTH.is_word() && !Bound::UNBOUNDED.is_word());
}

/// A bound past `2³²` is no bound, so a value raised a bit a move settles unbounded within 33 moves.
#[test]
fn a_growing_bound_settles_unbounded_within_the_lattices_height() {
    let mut bound = Bound::bottom();
    let mut moves = 0;
    while bound != Bound::UNBOUNDED {
        bound = bound.wider(bound.raised(1));
        moves += 1;
    }
    assert_eq!(moves, 33);
    assert_eq!(of(u64::from(u32::MAX)), Bound::LENGTH);
    assert_eq!(of(1 << 32), Bound::UNBOUNDED);
}

/// Scaling and dividing by powers of two move a bound by their bits, and a product's bound is the sum of its factors'.
#[test]
fn arithmetic_on_bounds_tracks_the_powers_of_two() {
    assert_eq!(of(255).raised(2), of(1023));
    assert_eq!(of(1023).lowered(2), of(255));
    assert_eq!(of(3).lowered(5), Bound::bottom());
    assert_eq!(of(255).times(of(255)), of(65535));
    assert_eq!(of(255).narrower(Bound::UNBOUNDED), of(255));
    assert_eq!(Bound::UNBOUNDED.times(of(1)), Bound::UNBOUNDED);
}

/// What lies below a value lies below the least power of two at or past it, so a remainder by `2³⁰` is a word where the literal itself is not.
#[test]
fn what_lies_below_a_value_lies_below_the_power_of_two_at_or_past_it() {
    let under = |value: u64| Bound::under(&Natural::from(value));
    assert_eq!(under(1 << 30), Bound::WORD);
    assert!(under(1 << 30).is_word() && !of(1 << 30).is_word());
    assert_eq!(under(1_000_003), of(1_000_003));
    assert_eq!(under(1), Bound::bottom());
    assert_eq!(under(0), Bound::bottom());
}
