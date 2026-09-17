//! Every scalar intrinsic's emitted shape: the carrier it computes in, and the guard it traps through.

//! Backend lowering coverage: build a [`curios_cont::Module`](curios_cont::Module) directly, lower it with [`into_wasm`](crate::into_wasm), and assert the *shape* of the emitted wasm (its WAT text). These are the shape half of a split: the fixtures that once built the old region API and *executed* the module became shape inspection here, and end-to-end semantics in `curios/src/tests/codegen` and the native `.crs` corpus. `into_wasm` performs no optimization, so a `LetIntrinsic` over literal operands lowers one-for-one without constant folding, and the emitted instruction is exactly what codegen chose.

use super::test_support::*;

// --- Nat ------------------------------------------------------------------

#[test]
fn nat_add_guards_the_i31_carrier() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::NatAdd,
        vec![nat(3), nat(4)],
    ));
    assert_contains(&wat, "i32.add");
    // Overflow past bit 31 traps through the special label.
    assert_traps(&wat);
}

#[test]
fn nat_sub_is_saturating_monus_without_a_guard() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::NatSub,
        vec![nat(3), nat(4)],
    ));
    assert_contains(&wat, "i32.sub");
    assert_contains(&wat, "select");
    assert_total(&wat);
}

#[test]
fn nat_mul_widens_to_i64_and_guards() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::NatMul,
        vec![nat(3), nat(4)],
    ));
    assert_contains(&wat, "i64.mul");
    assert_traps(&wat);
}

#[test]
fn nat_div_is_unsigned_and_rem_unsigned() {
    assert_contains(
        &wat(&intrinsic_main(
            curios_cont::Intrinsic::NatDiv,
            vec![nat(9), nat(2)],
        )),
        "i32.div_u",
    );
    assert_contains(
        &wat(&intrinsic_main(
            curios_cont::Intrinsic::NatRem,
            vec![nat(9), nat(2)],
        )),
        "i32.rem_u",
    );
}

#[test]
fn nat_lt_compares_unsigned() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::NatLt,
        vec![nat(3), nat(4)],
    ));
    assert_contains(&wat, "i32.lt_u");
    assert_total(&wat);
}

#[test]
fn nat_and_is_bitwise_and_total() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::NatAnd,
        vec![nat(6), nat(3)],
    ));
    assert_contains(&wat, "i32.and");
    assert_total(&wat);
}

#[test]
fn nat_to_flt_converts_unsigned() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::NatToFlt,
        vec![nat(7)],
    ));
    assert_contains(&wat, "f64.convert_i32_u");
}

// --- Int ------------------------------------------------------------------

#[test]
fn int_add_guards_the_signed_carrier() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::IntAdd,
        vec![int(3), int(-4)],
    ));
    assert_contains(&wat, "i32.add");
    assert_traps(&wat);
}

#[test]
fn int_mul_widens_to_i64_and_guards() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::IntMul,
        vec![int(3), int(-4)],
    ));
    assert_contains(&wat, "i64.mul");
    assert_traps(&wat);
}

#[test]
fn int_div_is_signed_and_guarded() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::IntDiv,
        vec![int(-9), int(2)],
    ));
    assert_contains(&wat, "i32.div_s");
    assert_traps(&wat);
}

#[test]
fn int_lt_compares_signed() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::IntLt,
        vec![int(-3), int(4)],
    ));
    assert_contains(&wat, "i32.lt_s");
    assert_total(&wat);
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

#[test]
fn flt_to_int_truncates_and_guards_the_range() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::FltToInt,
        vec![flt(1.0)],
    ));
    assert_contains(&wat, "i32.trunc_f64_s");
    assert_traps(&wat);
}
