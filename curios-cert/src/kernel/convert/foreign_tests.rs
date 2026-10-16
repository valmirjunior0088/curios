//! Two host calls: a call is its declaration and its operands, and nothing else holds two apart or one.

use {
    super::test_support::*,
    crate::convert,
    curios_abi::{ForeignFunction, WireResults, WireReturn, WireSignature, WireType},
    curios_core::{Global, Intrinsic, Term},
    curios_utilities::Qualifier,
    std::sync::Arc,
};

/// Two calls one declaration states convert where their operands do, each at the type its row demands of it, whatever reduction has made of the two sides; and two calls of two declarations are apart whatever they are handed.
#[test]
fn two_host_calls_convert_by_their_declaration_and_their_operands() {
    let mut kernel = kernel();
    let x = Term::free_var(&binder(0, "x"));
    let n = binder(1, "n");
    let row = Arc::new(ForeignFunction {
        name: "/probe".to_string(),
        signature: WireSignature {
            params: vec![WireType::Nat],
            returns: WireReturn::Answers(WireResults::single(WireType::Nat)),
        },
    });
    let call = |declared: &str, operand: Term| {
        Term::foreign(
            row.clone(),
            Global::Authored(Qualifier::from([declared])),
            vec![operand],
        )
    };
    let respelled = Term::apply(
        Term::func([(n, nat_type())], Term::free_var(&n)),
        [x.clone()],
    );
    let io = Term::intrinsic(Intrinsic::io_type(nat_type()));

    assert_eq!(
        convert(
            &mut kernel,
            &io,
            &call("probe", x.clone()),
            &call("probe", respelled)
        ),
        Ok(true),
    );
    assert_eq!(
        convert(
            &mut kernel,
            &io,
            &call("probe", x.clone()),
            &call("other", x)
        ),
        Ok(false),
    );
}
