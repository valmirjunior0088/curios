//! What a refusal at an argument tells the reader about the call it sits in.

use crate::tests::error;

// A lambda handed in where the callee takes its list — the argument order every other language uses for `map` — is refused as a lambda against a non-function type, which is true and not what the reader needs. The report names the parameter the lambda filled and the parameter the callee takes a function as.
#[test]
fn a_lambda_in_the_wrong_position_names_the_parameter_it_filled_and_the_one_it_fits() {
    let report = error(
        r#"
        use /std/{Nat, List};
        let bump(xs: List(Nat)) -> List(Nat) = List/map((x) => x + 1, xs);
        /std/print("ok")
        "#,
    );
    assert!(
        report.contains("checked as `a`, the 1st argument of '/sys/List/map'")
            && report.contains("'/sys/List/map' takes a function as `f`, its 2nd argument"),
        "unexpected report:\n{report}"
    );
}
