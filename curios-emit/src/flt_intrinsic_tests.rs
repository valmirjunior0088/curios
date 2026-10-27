//! Every `Flt` intrinsic's emitted shape: the struct it boxes into, a NaN through the model's rule, its bytes, and the guard a conversion refuses through before truncating.

use {super::test_support::*, curios_num::Rounding};

// --- Flt ------------------------------------------------------------------

#[test]
fn flt_add_boxes_into_the_flt_struct() {
    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::FltAdd(Rounding::TiesToEven),
        vec![flt(1.5), flt(2.5)],
    ));
    assert_contains(&wat, "f64.add");
    assert_contains(&wat, "struct.new $flt");
}

/// Every arithmetic answer but a NaN's is the instruction's, and a NaN result is replaced by `flt/nan` of the operands, which the model's NaN rule fixes and the engine does not.
#[test]
fn a_float_operation_answers_a_nan_through_the_model_s_rule() {
    for intrinsic in [
        curios_cps::Intrinsic::FltAdd(Rounding::TiesToEven),
        curios_cps::Intrinsic::FltMin,
        curios_cps::Intrinsic::FltRem,
    ] {
        let wat = wat(&intrinsic_main(intrinsic, vec![flt(1.5), flt(2.5)]));
        assert_contains(&wat, "f64.ne");
        assert_contains(&wat, "call $flt/nan");
        assert_contains(&wat, "(func $flt/nan");
    }

    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::FltSqrt(Rounding::TiesToEven),
        vec![flt(2.0)],
    ));
    assert_contains(&wat, "call $flt/nan");
}

/// The sign operations and the byte conversion touch bits alone in WebAssembly as in IEEE, a NaN's included, so nothing checks or rewrites what they produce.
#[test]
fn a_sign_operation_and_the_bytes_carry_a_nan_as_it_is() {
    for (intrinsic, operands) in [
        (
            curios_cps::Intrinsic::FltCopysign,
            vec![flt(1.0), flt(-2.0)],
        ),
        (curios_cps::Intrinsic::FltNeg, vec![flt(1.0)]),
        (curios_cps::Intrinsic::FltToLeBytes, vec![flt(1.0)]),
    ] {
        let wat = wat(&intrinsic_main(intrinsic, operands));
        assert_absent(&wat, "flt/nan");
        assert_absent(&wat, "select");
    }
}

#[test]
fn flt_div_divides() {
    assert_contains(
        &wat(&intrinsic_main(
            curios_cps::Intrinsic::FltDiv(Rounding::TiesToEven),
            vec![flt(3.0), flt(2.0)],
        )),
        "f64.div",
    );
}

#[test]
fn flt_to_le_bytes_packs_an_eight_byte_leaf() {
    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::FltToLeBytes,
        vec![flt(1.0)],
    ));
    assert_contains(&wat, "i64.reinterpret_f64");
    assert_contains(&wat, "array.new_fixed");
    assert_contains(&wat, "struct.new $rope/bytes/leaf");
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
        curios_cps::Intrinsic::FltToNat,
        vec![flt(1.0)],
    ));
    assert_contains(&wat, "f64.ge");
    assert_guarded_before_truncating(&wat);
}

#[test]
fn flt_decomposition_refuses_a_non_number_before_reading_the_fields() {
    for part in [
        curios_cps::Intrinsic::FltMantissa,
        curios_cps::Intrinsic::FltExponent,
    ] {
        let wat = wat(&intrinsic_main(part, vec![flt(1.0)]));
        let read = wat.find("i64.reinterpret_f64").expect("the bits read");
        let guard = wat.find("call $refuse/invariant").expect("the refusal");
        assert!(
            read < guard,
            "the field is read before the refusal decides on it"
        );
    }
}

#[test]
fn flt_to_int_guards_its_domain_before_truncating() {
    let wat = wat(&intrinsic_main(
        curios_cps::Intrinsic::FltToInt,
        vec![flt(1.0)],
    ));
    assert_contains(&wat, "f64.gt");
    assert_guarded_before_truncating(&wat);
}
