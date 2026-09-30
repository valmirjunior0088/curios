//! The search, over forms built directly: what it refutes and with which multipliers, what it satisfies and with which assignment, what it refuses to cut, what exhausts it, and that it answers alike every time; where it opens a case split and what that costs; and the rationals its assignments are written in.

use {
    super::{Arms, Certificate, DERIVED_ROWS, Plan, Rational, Search, plan, refute},
    curios_algebra::{Atom, LinearForm, Monomial},
    curios_num::{Integer, Natural},
};

/// `Σ coefficient · atom + constant <= 0` over atoms `0..`, each ranked by its index.
fn form(terms: &[(i64, u32)], constant: i64) -> LinearForm {
    LinearForm {
        constant: Integer::from(constant),
        terms: terms
            .iter()
            .map(|&(coefficient, atom)| {
                (
                    Integer::from(coefficient),
                    Monomial::product(vec![Atom::new(atom, u64::from(atom))]),
                )
            })
            .collect(),
        nonnegative: Vec::new(),
    }
}

fn search(forms: &[LinearForm]) -> Search {
    let mut budget = DERIVED_ROWS;
    refute(forms, &mut budget)
}

fn multipliers(values: &[u32]) -> Vec<Natural> {
    values.iter().map(|&value| Natural::from(value)).collect()
}

/// Whether `form <= 0` holds at `assignment`.
fn holds(form: &LinearForm, assignment: &[(Monomial, Rational)]) -> bool {
    let value = form.terms.iter().fold(
        Rational::from(form.constant.clone()),
        |sum, (coefficient, monomial)| {
            let value = assignment
                .iter()
                .find(|(unknown, _)| unknown == monomial)
                .map(|(_, value)| value.clone())
                .unwrap_or_default();
            sum.plus(&value.times(coefficient))
        },
    );
    value <= Rational::default()
}

/// Whether the combination of `forms` by `multipliers` has no monomial left and a positive constant.
fn is_false_constant(forms: &[LinearForm], multipliers: &[Natural]) -> bool {
    let mut constant = Integer::from(0);
    let mut terms: Vec<(Monomial, Integer)> = Vec::new();
    for (form, multiplier) in forms.iter().zip(multipliers) {
        let multiplier = Integer::from(multiplier.clone());
        constant = constant + form.constant.clone() * multiplier.clone();
        for (coefficient, monomial) in &form.terms {
            let scaled = coefficient.clone() * multiplier.clone();
            match terms.iter_mut().find(|(known, _)| known == monomial) {
                Some((_, sum)) => *sum = sum.clone() + scaled,
                None => terms.push((monomial.clone(), scaled)),
            }
        }
    }
    terms.iter().all(|(_, coefficient)| coefficient.is_zero()) && constant > Integer::from(0)
}

#[test]
fn a_chain_of_bounds_against_its_negation_is_refuted_by_each_once() {
    // x <= y, y <= z, and z + 1 <= x.
    let forms = [
        form(&[(1, 0), (-1, 1)], 0),
        form(&[(1, 1), (-1, 2)], 0),
        form(&[(1, 2), (-1, 0)], 1),
    ];
    match search(&forms) {
        Search::Refuted(found) => assert_eq!(found, multipliers(&[1, 1, 1])),
        _ => panic!("the chain is contradictory"),
    }
}

#[test]
fn a_fact_scaled_by_a_literal_enters_the_certificate_with_that_multiplier() {
    // a <= 15, b <= 15, and the negated goal 256 <= 16a + b.
    let forms = [
        form(&[(1, 0)], -15),
        form(&[(1, 1)], -15),
        form(&[(-16, 0), (-1, 1)], 256),
    ];
    match search(&forms) {
        Search::Refuted(found) => assert_eq!(found, multipliers(&[16, 1, 1])),
        _ => panic!("two hex digits stay under a byte"),
    }
}

#[test]
fn every_multiplier_of_a_certificate_is_needed() {
    // The byte certificate, each multiplier lowered by one in turn: the combination is no longer a false constant. The grid checks that both checkers refuse the proof a corrupted certificate stands for.
    let forms = [
        form(&[(1, 0)], -15),
        form(&[(1, 1)], -15),
        form(&[(-16, 0), (-1, 1)], 256),
    ];
    let Search::Refuted(found) = search(&forms) else {
        panic!("refuted");
    };
    assert!(is_false_constant(&forms, &found));
    for index in 0..found.len() {
        let mut corrupted = found.clone();
        corrupted[index] = corrupted[index].clone() - Natural::from(1u32);
        assert!(
            !is_false_constant(&forms, &corrupted),
            "multiplier {index} lowered still refutes"
        );
    }
}

#[test]
fn a_consistent_system_is_satisfied_by_an_assignment_that_holds_every_row() {
    // 1 <= x and x <= y.
    let forms = [form(&[(-1, 0)], 1), form(&[(1, 0), (-1, 1)], 0)];
    let Search::Satisfied(assignment) = search(&forms) else {
        panic!("the system has solutions");
    };
    for form in &forms {
        assert!(
            holds(form, &assignment),
            "the assignment satisfies every row"
        );
    }
}

#[test]
fn a_contradiction_only_the_integers_see_is_not_refuted() {
    // 2x <= 3 and 3 <= 2x: x = 3/2 over the rationals, and no integer. Refuting it needs a cut, which the procedure does not make.
    let forms = [form(&[(2, 0)], -3), form(&[(-2, 0)], 3)];
    let Search::Satisfied(assignment) = search(&forms) else {
        panic!("the rationals satisfy it");
    };
    assert_eq!(assignment[0].1, Rational::of(3, 2));
}

#[test]
fn exhausting_the_cap_selects_nothing() {
    // `±x ± i·y <= 0` for `i` in `1..=35`: seventy rows bound each unknown above and seventy below, no two alike, so eliminating either first derives 4 900 rows — past the cap, on a system `x = y = 0` satisfies. An unknown bounded on one side only is dropped for free, and two rows with one left side are merged, so neither shape would exhaust anything.
    let mut forms = Vec::new();
    for i in 1..=35 {
        for (x, y) in [(1, i), (-1, i), (1, -i), (-1, -i)] {
            forms.push(form(&[(x, 0), (y, 1)], 0));
        }
    }
    assert!(matches!(search(&forms), Search::Exhausted));
}

#[test]
fn the_same_rows_give_the_same_certificate_every_time() {
    let forms = [
        form(&[(1, 0), (-1, 1)], 0),
        form(&[(1, 1), (-1, 2)], 0),
        form(&[(2, 2), (-2, 0)], 1),
    ];
    let Search::Refuted(first) = search(&forms) else {
        panic!("contradictory");
    };
    for _ in 0..8 {
        let Search::Refuted(again) = search(&forms) else {
            panic!("contradictory");
        };
        assert_eq!(again, first);
    }
}

/// `b + (a - b) <= a` from `b <= a`, over `a`, `b` and the truncated difference `t`: the facts, the negated goal `a + 1 <= b + t`, and the cases `a - b` defines — `b <= a` with `b + t = a`, and `a < b` with `t <= 0`.
fn truncated() -> (Vec<LinearForm>, LinearForm, Vec<Arms>) {
    let facts = vec![form(&[(-1, 0), (1, 1)], 0)];
    let negated = form(&[(1, 0), (-1, 1), (-1, 2)], 1);
    let arms = Arms {
        holds: vec![
            form(&[(-1, 0), (1, 1)], 0),
            form(&[(-1, 0), (1, 1), (1, 2)], 0),
            form(&[(1, 0), (-1, 1), (-1, 2)], 0),
        ],
        fails: vec![form(&[(1, 0), (-1, 1)], 1), form(&[(1, 2)], 0)],
    };
    (facts, negated, vec![arms])
}

#[test]
fn a_split_is_opened_only_where_the_search_without_it_found_an_assignment() {
    let (facts, negated, splits) = truncated();
    let mut budget = DERIVED_ROWS;
    assert!(matches!(
        plan(&facts, Some(&negated), &[], &mut budget),
        Plan::Satisfied(_)
    ));

    let mut budget = DERIVED_ROWS;
    let Plan::Certified(Certificate::Split { holds, fails }) =
        plan(&facts, Some(&negated), &splits, &mut budget)
    else {
        panic!("each case refutes the negated goal");
    };
    for (certificate, arm) in [(holds, &splits[0].holds), (fails, &splits[0].fails)] {
        let Certificate::Leaf(multipliers) = *certificate else {
            panic!("one split is enough");
        };
        let forms = facts
            .iter()
            .chain(arm)
            .chain([&negated])
            .cloned()
            .collect::<Vec<_>>();
        assert!(is_false_constant(&forms, &multipliers));
    }

    // Facts that refute the negated goal without a case open none.
    let refuting = vec![facts[0].clone(), form(&[(1, 0), (-1, 1)], 1)];
    let mut budget = DERIVED_ROWS;
    assert!(matches!(
        plan(&refuting, Some(&negated), &splits, &mut budget),
        Plan::Certified(Certificate::Leaf(_))
    ));
}

#[test]
fn a_budget_runs_across_every_case() {
    let (facts, negated, splits) = truncated();
    let spent = |extra: &[LinearForm]| {
        let forms = facts
            .iter()
            .chain(extra)
            .chain([&negated])
            .cloned()
            .collect::<Vec<_>>();
        let mut budget = DERIVED_ROWS;
        refute(&forms, &mut budget);
        DERIVED_ROWS - budget
    };
    let each = spent(&[]) + spent(&splits[0].holds) + spent(&splits[0].fails);
    assert!(each > 0, "every case derives rows");

    let mut budget = DERIVED_ROWS;
    plan(&facts, Some(&negated), &splits, &mut budget);
    assert_eq!(DERIVED_ROWS - budget, each);
}

#[test]
fn a_rational_is_kept_in_lowest_terms_with_a_positive_denominator() {
    assert_eq!(Rational::of(6, -4), Rational::of(-3, 2));
    assert_eq!(Rational::of(6, -4).to_string(), "-3/2");
    assert_eq!(Rational::of(4, 2).to_string(), "2");
}

#[test]
fn a_rational_rounds_toward_the_side_asked() {
    assert_eq!(Rational::of(-3, 2).floor(), Integer::from(-2));
    assert_eq!(Rational::of(-3, 2).ceil(), Integer::from(-1));
    assert_eq!(Rational::of(3, 2).floor(), Integer::from(1));
    assert_eq!(Rational::of(3, 2).ceil(), Integer::from(2));
}

#[test]
fn the_value_chosen_is_zero_where_it_fits_and_the_nearest_integer_where_one_does() {
    let (minus_one, one) = (Rational::of(-1, 1), Rational::of(1, 1));
    assert_eq!(
        Rational::choose(Some(minus_one), Some(one)),
        Rational::default()
    );
    assert_eq!(
        Rational::choose(Some(Rational::of(3, 2)), None),
        Rational::of(2, 1)
    );
    assert_eq!(
        Rational::choose(None, Some(Rational::of(-3, 2))),
        Rational::of(-2, 1)
    );
    let tight = Rational::of(3, 2);
    assert_eq!(
        Rational::choose(Some(tight.clone()), Some(tight)),
        Rational::of(3, 2)
    );
}
