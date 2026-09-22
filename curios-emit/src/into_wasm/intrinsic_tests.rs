//! Every scalar intrinsic's emitted shape: the fast path it computes in, the helper it grows through, and the guard it refuses through.

//! Backend lowering coverage: build a [`curios_cont::Module`](curios_cont::Module) directly, lower it with [`into_wasm`](crate::into_wasm), and assert the *shape* of the emitted wasm (its WAT text). These are the shape half of a split: the fixtures that once built the old region API and *executed* the module became shape inspection here, and end-to-end semantics in `curios/src/tests/codegen` and the native `.crs` corpus. `into_wasm` performs no optimization, so a `LetIntrinsic` over literal operands lowers one-for-one without constant folding, and the emitted instruction is exactly what codegen chose.

use super::test_support::*;

/// A `Nat` or `Int` operation grows rather than refusing: the module reaches no refusal helper at all.
#[track_caller]
fn assert_refuses_nothing(wat: &str) {
    assert_absent(wat, "call $refuse/");
}

// --- Nat ------------------------------------------------------------------

#[test]
fn nat_add_keeps_an_i31_fast_path_and_grows_past_it() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::NatAdd,
        vec![nat(3), nat(4)],
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
        curios_cont::Intrinsic::NatSub,
        vec![nat(3), nat(4)],
    ));
    assert_contains(&wat, "i32.sub");
    assert_contains(&wat, "select");
    assert_contains(&wat, "call $nat/sub");
    assert_refuses_nothing(&wat);
}

#[test]
fn nat_mul_widens_to_i64_and_grows_past_the_i31() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::NatMul,
        vec![nat(3), nat(4)],
    ));
    assert_contains(&wat, "i64.mul");
    assert_contains(&wat, "call $big/of_i64");
    assert_contains(&wat, "call $big/mul");
    assert_refuses_nothing(&wat);
}

#[test]
fn nat_div_and_rem_are_unsigned_on_the_fast_path() {
    let div = wat(&intrinsic_main(
        curios_cont::Intrinsic::NatDiv,
        vec![nat(9), nat(2)],
    ));
    assert_contains(&div, "i32.div_u");
    assert_contains(&div, "call $big/div");

    let rem = wat(&intrinsic_main(
        curios_cont::Intrinsic::NatRem,
        vec![nat(9), nat(2)],
    ));
    assert_contains(&rem, "i32.rem_u");
    assert_contains(&rem, "call $big/rem");
}

#[test]
fn nat_lt_compares_the_i31_signed_and_boxed_values_through_big_cmp() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::NatLt,
        vec![nat(3), nat(4)],
    ));
    assert_contains(&wat, "i32.lt_s");
    assert_contains(&wat, "call $big/cmp");
    assert_refuses_nothing(&wat);
}

#[test]
fn nat_and_combines_i31s_as_words_and_boxed_values_in_twos_complement() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::NatAnd,
        vec![nat(6), nat(3)],
    ));
    assert_contains(&wat, "i32.and");
    assert_contains(&wat, "call $big/bitwise");
    assert_refuses_nothing(&wat);
}

#[test]
fn nat_to_flt_converts_an_i31_and_rounds_a_boxed_value() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::NatToFlt,
        vec![nat(7)],
    ));
    assert_contains(&wat, "f64.convert_i32_s");
    assert_contains(&wat, "call $big/to_f64");
}

/// `Nat` and `Int` share one runtime form, so the conversion reads no value and calls no helper.
#[test]
fn nat_to_int_is_the_identity() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::NatToInt,
        vec![nat(7)],
    ));
    assert_absent(&wat, "call $big/add");
    assert_absent(&wat, "call $big/of_i64");
    assert_refuses_nothing(&wat);
}

/// A literal past the i31 is a boxed magnitude built from its limbs, where it used to be a refusal.
#[test]
fn a_nat_literal_past_the_i31_is_built_from_a_limb_segment() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::NatAdd,
        vec![nat(0x8000_0000), nat(1)],
    ));
    assert_contains(&wat, "array.new_data $limbs");
    assert_contains(&wat, "struct.new $big");
    assert_refuses_nothing(&wat);
}

// --- Int ------------------------------------------------------------------

#[test]
fn int_add_keeps_an_i31_fast_path_and_grows_past_it() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::IntAdd,
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
        curios_cont::Intrinsic::IntMul,
        vec![int(3), int(-4)],
    ));
    assert_contains(&wat, "i64.mul");
    assert_contains(&wat, "call $big/mul");
    assert_refuses_nothing(&wat);
}

/// `-2³⁰ / -1` is the one quotient that outgrows its dividend, so the signed fast path checks its result as a sum does.
#[test]
fn int_div_is_signed_and_boxes_the_one_quotient_past_the_i31() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::IntDiv,
        vec![int(-9), int(2)],
    ));
    assert_contains(&wat, "i32.div_s");
    assert_contains(&wat, "call $big/of_i64");
    assert_contains(&wat, "call $big/div");
    assert_refuses_nothing(&wat);
}

#[test]
fn int_lt_compares_signed() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::IntLt,
        vec![int(-3), int(4)],
    ));
    assert_contains(&wat, "i32.lt_s");
    assert_contains(&wat, "call $big/cmp");
    assert_refuses_nothing(&wat);
}

/// A remainder by a small literal lies below it, so the representation analysis holds it in a word: the fast path's `i32` is the value, and a helper's answer is narrowed rather than boxed.
#[test]
fn a_remainder_by_a_small_literal_is_held_in_a_word() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::NatRem,
        vec![nat(0x8000_0000), nat(7)],
    ));
    assert_contains(&wat, "i32.rem_u");
    assert_contains(&wat, "$result i32)");
    assert_refuses_nothing(&wat);
}

// --- Flt ------------------------------------------------------------------

#[test]
fn flt_add_boxes_into_the_flt_struct() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::FltAdd,
        vec![flt(1.5), flt(2.5)],
    ));
    assert_contains(&wat, "f64.add");
    assert_contains(&wat, "struct.new $flt");
}

#[test]
fn flt_div_divides() {
    assert_contains(
        &wat(&intrinsic_main(
            curios_cont::Intrinsic::FltDiv,
            vec![flt(3.0), flt(2.0)],
        )),
        "f64.div",
    );
}

#[test]
fn flt_to_le_bytes_packs_an_eight_byte_leaf() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::FltToLeBytes,
        vec![flt(1.0)],
    ));
    assert_contains(&wat, "i64.reinterpret_f64");
    assert_contains(&wat, "array.new_fixed");
    assert_contains(&wat, "struct.new $rope/bin/leaf");
}

/// The truncation instruction traps by itself past its own result type, so a guard placed after it never runs where it matters most: the order is the property. What refuses is the domain the evidence states, and a float past the i31 grows through `big/of_f64`.
#[track_caller]
fn assert_guarded_before_truncating(wat: &str) {
    let guard = wat.find("f64.lt").expect("a ceiling comparison");
    let trunc = wat.find("i32.trunc_f64_s").expect("the truncation");
    assert!(guard < trunc, "the domain is decided before truncating");
    assert_contains(wat, "call $refuse/invariant");
    assert_contains(wat, "call $big/of_f64");
}

#[test]
fn flt_to_nat_guards_its_domain_before_truncating() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::FltToNat,
        vec![flt(1.0)],
    ));
    assert_contains(&wat, "f64.ge");
    assert_guarded_before_truncating(&wat);
}

#[test]
fn flt_to_int_guards_its_domain_before_truncating() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::FltToInt,
        vec![flt(1.0)],
    ));
    assert_contains(&wat, "f64.gt");
    assert_guarded_before_truncating(&wat);
}
