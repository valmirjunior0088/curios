//! Every `Int` intrinsic's emitted shape: the i31 fast path it computes in, the one result it boxes past it, and a remainder held in a word.

use super::test_support::*;

// --- Int ------------------------------------------------------------------

#[test]
fn int_add_keeps_an_i31_fast_path_and_grows_past_it() {
    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::IntAdd,
        vec![int(3), int(-4)],
    ));
    assert_contains(&wat, "i32.add");
    assert_contains(&wat, "call $big/of_i64");
    assert_contains(&wat, "call $big/add");
    assert_refuses_nothing(&wat);
}

#[test]
fn int_mul_widens_to_i64_and_grows_past_the_i31() {
    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::IntMul,
        vec![int(3), int(-4)],
    ));
    assert_contains(&wat, "i64.mul");
    assert_contains(&wat, "call $big/mul");
    assert_refuses_nothing(&wat);
}

/// `-2³⁰ / -1` is the one quotient that outgrows its dividend, so the signed fast path checks its result as a sum does. The only refusal reachable is the magnitude division's zero-divisor guard, which the evidence every division carries makes the compiler's fault.
#[test]
fn int_div_is_signed_and_boxes_the_one_quotient_past_the_i31() {
    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::IntDiv,
        vec![int(-9), int(2)],
    ));
    assert_contains(&wat, "i32.div_s");
    assert_contains(&wat, "call $big/of_i64");
    assert_contains(&wat, "call $big/div");
    assert_eq!(
        refusals_besides_the_exit(&wat),
        count(&wat, "call $refuse/invariant")
    );
}

#[test]
fn int_lt_compares_signed() {
    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::IntLt,
        vec![int(-3), int(4)],
    ));
    assert_contains(&wat, "i32.lt_s");
    assert_contains(&wat, "call $big/cmp");
    assert_refuses_nothing(&wat);
}

// An `Int`'s magnitude is the operand or its negation: of an i31 by a machine comparison, the one negation that leaves the i31 boxed as a sum's result is, and of a boxed value by `big/cmp` against zero over `big/add` under its flip. Nothing is refused.
#[test]
fn an_int_s_magnitude_selects_the_operand_or_its_negation_and_boxes_the_one_past_the_i31() {
    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::IntAbs,
        vec![int(-7)],
    ));
    assert_contains(&wat, "i32.sub");
    assert_contains(&wat, "i32.ge_s");
    assert_contains(&wat, "select");
    assert_contains(&wat, "call $big/of_i64");
    assert_contains(&wat, "call $big/add");
    assert_contains(&wat, "call $big/cmp");
    assert_refuses_nothing(&wat);
}

// An `Int`'s sign is an i31 whatever its operand's size: two machine comparisons of an i31, and `big/cmp` against zero of a boxed value, with nothing boxed and nothing refused.
#[test]
fn an_int_s_sign_is_two_machine_comparisons_and_big_cmp_past_the_i31() {
    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::IntSign,
        vec![int(-7)],
    ));
    assert_contains(&wat, "i32.gt_s");
    assert_contains(&wat, "i32.lt_s");
    assert_contains(&wat, "call $big/cmp");
    assert_absent(&wat, "call $big/of_i64");
    assert_refuses_nothing(&wat);
}

/// A remainder by a small literal lies below it, so the representation analysis holds it in a word: the fast path's `i32` is the value, a helper's answer is narrowed rather than boxed, and the exit reads the word with no narrowing of its own.
#[test]
fn a_remainder_by_a_small_literal_is_held_in_a_word() {
    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::NatRem,
        vec![nat(0x8000_0000), nat(7)],
    ));
    assert_contains(&wat, "i32.rem_u");
    assert_absent(&wat, "call $big/word");
    assert_eq!(
        refusals_besides_the_exit(&wat),
        count(&wat, "call $refuse/invariant")
    );
}
