//! The arithmetic every peer ships beyond the operators: powers, divisors, logarithms, roots, parity and the `Int` extrema.

use crate::tests::{error, run};

#[test]
fn nat_powers_divisors_logarithms_and_roots_compute() {
    let source = r#"
        use /std/{Nat, Str, List, Bool};
        /std/print(Str/join(",", List/map([
            Nat/pow(2, 10), Nat/pow(7, 0), Nat/pow(0, 3),
            Nat/gcd(48, 18), Nat/gcd(0, 5), Nat/gcd(5, 0), Nat/gcd(0, 0),
            Nat/lcm(4, 6), Nat/lcm(0, 6),
            Nat/log2(1), Nat/log2(1024), Nat/log2(1023), Nat/log2(0),
            Nat/sqrt(0), Nat/sqrt(15), Nat/sqrt(16), Nat/sqrt(1000000)
        ], Nat/to_str)))
        "#;

    assert_eq!(run(source), b"1024,1,0,6,5,5,0,12,0,0,10,9,0,0,3,4,1000");
}

#[test]
fn parity_and_the_int_extrema_and_sign() {
    let source = r#"
        use /std/{Nat, Int, Str, Bool};
        /std/print(Str/join(",", [
            Bool/to_str(Nat/is_even(0)), Bool/to_str(Nat/is_even(7)),
            Int/to_str(Int/min(-3, +2)), Int/to_str(Int/max(-3, +2)),
            Int/to_str(Int/sign(-9)), Int/to_str(Int/sign(+0)), Int/to_str(Int/sign(+4))
        ]))
        "#;

    assert_eq!(run(source), b"true,false,-3,+2,-1,+0,+1");
}

// `gcd` recurses on a remainder, which the totality analysis does not grade, so it is retained as a partial program and refused where a proposition would depend on it. `log2` halves under a guard, which the arithmetic rung reads as a decrease, so the same position accepts it.
#[test]
fn gcd_is_refused_in_a_type_and_log2_is_accepted() {
    let refused = r#"
        use /std/{Nat, Str, True};
        let claim: Nat/Le(Nat/gcd(4, 6), 2) = True/qed();
        /std/print("ok")
        "#;
    assert!(
        error(refused).contains("not known to terminate"),
        "gcd in a proposition is refused as partial"
    );

    let accepted = r#"
        use /std/{Nat, Str, True};
        let claim: Nat/Le(Nat/log2(1024), 10) = True/qed();
        /std/print("ok")
        "#;
    assert_eq!(run(accepted), b"ok");
}
