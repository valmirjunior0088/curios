use super::run;

#[test]
fn bigint_add_crosses_zero_in_both_directions() {
    // Mixed-sign `add` routes through `pos_sub`, which reads the sign straight
    // off `cmp`: the same magnitudes flipped land exactly on the other side of
    // zero, and equal magnitudes land on zero itself — which has its own
    // constructor, so "-0" cannot even be produced.
    let source = r#"
        use /std/{Handle, Str, Lst, BigInt};
        /std/print(Str/join(",", [
            BigInt/to_str(BigInt/add(BigInt/of_int(-70000), BigInt/of_int(+99999))),
            BigInt/to_str(BigInt/add(BigInt/of_int(+70000), BigInt/of_int(-99999))),
            BigInt/to_str(BigInt/add(BigInt/of_int(-70000), BigInt/of_int(+70000)))]))
        "#;
    assert_eq!(run(source), b"+29999,-29999,+0");
}

#[test]
fn bigint_sub_is_total() {
    // Unlike `BigNat/sub`, the signed `sub` never truncates: subtracting a
    // larger value produces the genuine negative difference.
    let source = r#"
        use /std/{Handle, Str, BigInt};
        /std/print(BigInt/to_str(BigInt/sub(BigInt/of_nat(1), BigInt/of_nat(100000000))))
        "#;
    assert_eq!(run(source), b"-99999999");
}

#[test]
fn bigint_mul_multiplies_signs() {
    // The sign of a product is the product of the signs, and the magnitude
    // rides `BigNat`'s numeral past any fixed width: (-99999) * (-99999) and
    // (+99999) * (-99999) differ only in sign.
    let source = r#"
        use /std/{Handle, Str, Lst, BigInt};
        /std/print(Str/join(",", [
            BigInt/to_str(BigInt/mul(BigInt/of_int(-99999), BigInt/of_int(-99999))),
            BigInt/to_str(BigInt/mul(BigInt/of_int(+99999), BigInt/of_int(-99999))),
            BigInt/to_str(BigInt/mul(BigInt/of_int(-99999), BigInt/zero))]))
        "#;
    assert_eq!(run(source), b"+9999800001,-9999800001,+0");
}

#[test]
fn bigint_cmp_orders_across_signs() {
    // `cmp` decides by sign first and only then by magnitude — where the
    // negative stratum orders REVERSED: -2 < -1 even though 2 > 1.
    let source = r#"
        use /std/{Handle, Str, BigInt, Order};
        let show(o : Order) -> Str =
            match o : (_) => Str
            | lt() => "lt"
            | eq() => "eq"
            | gt() => "gt"
            end;
        let m2 = BigInt/of_int(-2);
        let m1 = BigInt/of_int(-1);
        let p1 = BigInt/of_int(+1);
        /std/print(Str/concat(
            Str/concat(show(BigInt/cmp(m2, m1)), show(BigInt/cmp(m1, p1))),
            Str/concat(show(BigInt/cmp(p1, m1)), show(BigInt/cmp(m1, m1)))))
        "#;
    assert_eq!(run(source), b"ltltgteq");
}

#[test]
fn bigint_of_int_crosses_the_i31_boundary() {
    // `of_int` decodes a native `Int` sign-and-magnitude: the largest
    // magnitude whose negation also fits the i31 carrier round-trips through
    // the numeral in both signs and renders Int-style. (The carrier minimum
    // itself is out of reach: `Int/abs(-2^30)` has no i31 representation.)
    let source = r#"
        use /std/{Handle, Str, Lst, BigInt};
        /std/print(Str/join(",", [
            BigInt/to_str(BigInt/of_int(+1073741823)),
            BigInt/to_str(BigInt/of_int(-1073741823)),
            BigInt/to_str(BigInt/of_int(+0))]))
        "#;
    assert_eq!(run(source), b"+1073741823,-1073741823,+0");
}

#[test]
fn bigint_neg_abs_and_parity() {
    // `neg` is an involution that `abs` collapses, and `is_even`/`div2` read
    // the magnitude: -101 is odd and halves toward zero to -50.
    let source = r#"
        use /std/{Handle, Str, Bool, Lst, BigInt};
        let show(b : Bool) -> Str =
            match b : (_) => Str
            | true => "T"
            | false => "F"
            end;
        let n = BigInt/of_int(-101);
        /std/print(Str/join(",", [
            BigInt/to_str(BigInt/neg(n)),
            BigInt/to_str(BigInt/abs(n)),
            BigInt/to_str(BigInt/div2(n)),
            show(BigInt/is_even(n)),
            show(BigInt/is_even(BigInt/div2(n)))]))
        "#;
    assert_eq!(run(source), b"+101,+101,-50,F,T");
}

#[test]
fn bigint_operators_dispatch_through_concepts() {
    // The `/std` facades carry `Add`/`Sub`/`Mul`/`Eql`/`Cmp`/`Show` witnesses
    // for `BigInt`, so the operator syntax and `show` resolve on it like on
    // any native numeric type.
    let source = r#"
        use /std/{Handle, Str, Bool, Lst, BigInt, Show};
        let a = BigInt/of_int(-6);
        let b = BigInt/of_int(+2);
        let show_bool(v : Bool) -> Str =
            match v : (_) => Str
            | true => "T"
            | false => "F"
            end;
        /std/print(Str/join(",", [
            Show/show(a + b),
            Show/show(a - b),
            Show/show(a * b),
            show_bool(a == b),
            show_bool(a < b)]))
        "#;
    assert_eq!(run(source), b"-4,-8,-12,F,T");
}
