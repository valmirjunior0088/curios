//! Which arithmetic the totality analysis grades: `sqrt` descends on a guard between two computed values and is refused in a type, `log2` halves under a guard and is accepted, and `gcd` recurses on well-founded `<` and is accepted. What those functions compute is the corpus's `/numeric`.

use crate::tests::{error, run};

// `sqrt`'s iteration descends on a comparison of two computed values, which the totality analysis does not grade, so it is retained as a partial program and refused where a proposition would depend on it. `log2` halves under a guard, which the arithmetic rung reads as a decrease, and `gcd` recurses through `/std/Nat/descend`, whose descent is its bound's, so the same position accepts both.
#[test]
fn sqrt_is_refused_in_a_type_and_log2_and_gcd_are_accepted() {
    let refused = r#"
        use /std/{Nat, Str};
        use /std/Bool/{True, Holds};
        let claim: Holds(Nat/sqrt(16) <= 4) = True/qed();
        /std/print("ok")
        "#;
    assert!(
        error(refused).contains("not known to terminate"),
        "sqrt in a proposition is refused as partial"
    );

    let accepted = r#"
        use /std/{Nat, Str};
        use /std/Bool/{True, Holds};
        let halved: Holds(Nat/log2(1024) <= 10) = True/qed();
        let divided: Holds(Nat/gcd(4, 6) <= 2) = True/qed();
        /std/print("ok")
        "#;
    assert_eq!(run(accepted), b"ok");
}
