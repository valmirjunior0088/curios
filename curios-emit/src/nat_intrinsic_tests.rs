//! Every `Nat` intrinsic's emitted shape: the i31 fast path it computes in, the helper it grows through past it, and a literal or shift count past the i31.

use {
    super::test_support::*,
    curios_num::{Natural, Rounding},
};

// --- Nat ------------------------------------------------------------------

#[test]
fn nat_add_keeps_an_i31_fast_path_and_grows_past_it() {
    // A sum the representation analysis bounds below `2³⁰` rides a word with no overflow arm, so the operands are ones whose sum is `2³⁰`.
    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::NatAdd,
        vec![nat(1 << 29), nat(1 << 29)],
    ));
    assert_contains(&wat, "i32.add");
    // A sum leaving the i31 is boxed rather than refused, and a boxed operand takes the helper.
    assert_contains(&wat, "call $big/of_i64");
    assert_contains(&wat, "call $big/add");
    assert_refuses_nothing(&wat);
}

#[test]
fn nat_sub_is_monus_on_the_fast_path_and_nat_sub_past_it() {
    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::NatSub,
        vec![nat(3), nat(4)],
    ));
    assert_contains(&wat, "i32.sub");
    assert_contains(&wat, "select");
    assert_contains(&wat, "call $nat/sub");
    assert_refuses_nothing(&wat);
}

#[test]
fn nat_mul_widens_to_i64_and_grows_past_the_i31() {
    // As for the sum: a product its operands bound below `2³⁰` rides a word, so these multiply to `2³⁰`.
    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::NatMul,
        vec![nat(1 << 15), nat(1 << 15)],
    ));
    assert_contains(&wat, "i64.mul");
    assert_contains(&wat, "call $big/of_i64");
    assert_contains(&wat, "call $big/mul");
    assert_refuses_nothing(&wat);
}

#[test]
fn nat_div_and_rem_are_unsigned_on_the_fast_path() {
    let div = wat(&intrinsic_main(
        curios_cps::Intrinsic::NatDiv,
        vec![nat(9), nat(2)],
    ));
    assert_contains(&div, "i32.div_u");
    assert_contains(&div, "call $big/div");

    let rem = wat(&intrinsic_main(
        curios_cps::Intrinsic::NatRem,
        vec![nat(9), nat(2)],
    ));
    assert_contains(&rem, "i32.rem_u");
    assert_contains(&rem, "call $big/rem");
}

#[test]
fn nat_lt_compares_the_i31_signed_and_boxed_values_through_big_cmp() {
    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::NatLt,
        vec![nat(3), nat(4)],
    ));
    assert_contains(&wat, "i32.lt_s");
    assert_contains(&wat, "call $big/cmp");
    assert_refuses_nothing(&wat);
}

// A `min` and a `max` select one of their two operands as it stands: two i31s by a signed machine comparison, and anything else by `big/cmp` against zero, with nothing boxed and nothing refused, on `Nat` and on `Int`.
#[test]
fn a_min_and_a_max_select_an_operand_by_a_signed_comparison_and_big_cmp_past_the_i31() {
    for (intrinsic, keeps) in [
        (curios_cps::Intrinsic::NatMin, "i32.le_s"),
        (curios_cps::Intrinsic::NatMax, "i32.ge_s"),
        (curios_cps::Intrinsic::IntMin, "i32.le_s"),
        (curios_cps::Intrinsic::IntMax, "i32.ge_s"),
    ] {
        let wat = wat(&intrinsic_main(intrinsic, vec![nat(3), nat(4)]));
        assert_contains(&wat, keeps);
        assert_contains(&wat, "select");
        assert_contains(&wat, "call $big/cmp");
        assert_refuses_nothing(&wat);
    }
}

// A power is `big/pow` at every size, its exponent crossing as a 64-bit count: the helper squares and multiplies through `big/mul`, and refuses only a count past every limb array.
#[test]
fn a_power_is_squared_and_multiplied_by_big_pow_under_a_wide_count() {
    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::NatPow,
        vec![nat(3), nat(4)],
    ));
    assert_contains(&wat, "call $big/pow");
    assert_contains(&wat, "call $big/mul");
    assert_contains(&wat, "i64.shr_u");
    assert_eq!(
        refusals_besides_the_exit(&wat),
        count(&wat, "call $refuse/capacity")
    );
}

#[test]
fn nat_and_combines_i31s_as_words_and_boxed_values_in_twos_complement() {
    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::NatAnd,
        vec![nat(6), nat(3)],
    ));
    assert_contains(&wat, "i32.and");
    assert_contains(&wat, "call $big/bitwise");
    assert_refuses_nothing(&wat);
}

#[test]
fn nat_to_flt_converts_an_i31_and_rounds_a_boxed_value() {
    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::NatToFlt(Rounding::TiesToEven),
        vec![nat(7)],
    ));
    assert_contains(&wat, "f64.convert_i32_s");
    assert_contains(&wat, "call $big/to_f64");
}

/// `Nat` and `Int` share one runtime form, so the conversion reads no value and calls no helper.
#[test]
fn nat_to_int_is_the_identity() {
    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::NatToInt,
        vec![nat(7)],
    ));
    assert_absent(&wat, "call $big/add");
    assert_absent(&wat, "call $big/of_i64");
    assert_refuses_nothing(&wat);
}

/// A literal past the i31 is a boxed magnitude built from its limbs.
#[test]
fn a_nat_literal_past_the_i31_is_built_from_a_limb_segment() {
    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::NatAdd,
        vec![nat(0x8000_0000), nat(1)],
    ));
    assert_contains(&wat, "array.new_data $words");
    assert_contains(&wat, "struct.new $big");
    assert_refuses_nothing(&wat);
}

/// A count past one word is handed to the shift as an `i64`, never through the position's saturating `big/word`. Both carriers share this path.
#[test]
fn a_boxed_shift_count_crosses_as_a_wide_count_in_both_carriers() {
    for operation in [
        curios_cps::Intrinsic::NatShl,
        curios_cps::Intrinsic::NatShr,
        curios_cps::Intrinsic::IntShl,
        curios_cps::Intrinsic::IntShr,
    ] {
        let count = curios_cps::Atom::Literal(curios_cps::Literal::Nat(Natural::from(1u64 << 32)));
        let value = match operation {
            curios_cps::Intrinsic::IntShl | curios_cps::Intrinsic::IntShr => int(-1),
            _ => nat(1),
        };
        let mut module = curios_cps::Module::new();
        let result = module.add_value(Some("result".into()));
        // No exit-code conversion: every big/word call would belong to the shift's count.
        let end = module.add_node(curios_cps::Node::Unreachable);
        let body = module.add_node(curios_cps::Node::LetIntrinsic {
            result,
            op: operation,
            args: vec![value, count],
            next: end,
        });
        let return_cont = module.reserve_continuation();
        let main = module.add_function(curios_cps::Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
            droppable: false,
        });
        module.set_entry(main);
        let wat = wat(&module);
        assert_contains(&wat, "call $big/count");
        assert_contains(&wat, "(param $count i64)");
        assert_absent(&wat, "call $big/word");
        if matches!(
            operation,
            curios_cps::Intrinsic::NatShl | curios_cps::Intrinsic::IntShl
        ) {
            assert_contains(&wat, "call $refuse/capacity");
        } else {
            assert_absent(&wat, "call $refuse/capacity");
        }
    }
}
