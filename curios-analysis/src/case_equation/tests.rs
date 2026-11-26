//! The recording rule on each kind of spelling it is asked about, and the arm a stuck dispatch takes by what its keys' tests are decided as.

use {
    super::*,
    curios_common::Qualifier,
    curios_core::{Free, Global, Intrinsic, Keyed, Keys, Nat, Term, Universes},
    std::convert::Infallible,
};

fn nat(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

#[test]
fn a_spelling_that_mentions_a_local_carries_an_equation() {
    let n = Free::local(0, Some("n"));

    assert!(records_case_equation(&Term::intrinsic(Intrinsic::nat_lt(
        Term::free_var(&n),
        nat(2)
    ))));
}

#[test]
fn a_closed_spelling_carries_none() {
    assert!(!records_case_equation(&Term::intrinsic(Intrinsic::nat_lt(
        nat(3),
        nat(2)
    ))));
}

#[test]
fn a_number_or_a_truth_value_holds_no_type_and_a_list_or_a_value_of_no_known_carrier_may() {
    assert!(holds_no_type(&nat(0)));
    assert!(holds_no_type(&Term::intrinsic(Intrinsic::Bool(false))));
    assert!(!holds_no_type(&Term::intrinsic(Intrinsic::List {
        element: Term::intrinsic(Intrinsic::NatType),
        items: Vec::new(),
    })));
    assert!(!holds_no_type(&Term::free_var(&Free::local(0, Some("v")))));
}

// A byte's dispatch keyed at `0` and `7` stuck at `h`, its tests decided as each case states them, in key order.
#[test]
fn a_stuck_dispatch_takes_the_arm_its_keys_tests_decide_and_stays_stuck_where_one_is_undecided() {
    let head = Term::free_var(&Free::local(0, Some("h")));
    let keys = Keys::Byte(Keyed::new(vec![(0, nat(10)), (7, nat(17))]).expect("ascending keys"));
    let default = nat(1);
    let tests = catch_all_tests(&head, &keys).collect::<Vec<_>>();
    let taken = |answers: [Option<bool>; 2]| {
        tested_arm(&keys, &default, &head, |test| {
            let position = tests.iter().position(|each| *each == test);
            Ok::<_, Infallible>(answers[position.expect("a test of one of the keys")])
        })
        .expect("no decision fails")
    };

    assert_eq!(taken([Some(false), Some(false)]), Some(nat(1)));
    assert_eq!(taken([None, Some(true)]), Some(nat(17)));
    assert_eq!(taken([Some(false), None]), None);
}

#[test]
fn a_top_level_name_carries_none() {
    let flag = Global::Authored(Qualifier::from(["flag"]));

    assert!(!records_case_equation(&Term::instance(
        flag,
        Universes::default()
    )));
    assert!(!records_case_equation(&Term::proj(
        Term::instance(flag, Universes::default()),
        0
    )));
}
