//! Which arithmetic the totality analysis grades: `gcd` recurses on a remainder and is refused in a type, `log2` halves under a guard and is accepted. What those functions compute is the corpus's `/numeric`.

use crate::tests::{error, run};

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
