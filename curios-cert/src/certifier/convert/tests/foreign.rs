//! Two host calls: a call is its declaration and its operands, and nothing else holds two apart or one.

use {
    crate::certifier::convert::test_support::*,
    crate::{test_support::*, *},
};

/// The rows `/probe` and `/other`, two declarations of one host function, the definitions their forms imply beside them, and three calls of them over a word `x`: `probe(x)`, `probe` of a redex reducing to `x`, and `other(x)`.
const CALLS: &str = r#"
foreign /probe(Nat) -> Nat;
let /probe: (n: #NatType) -> #IoType(#NatType) = (n: #NatType) => #{node = Foreign} /probe(n);
#{name = "/probe"}
foreign /other(Nat) -> Nat;
let /other: (n: #NatType) -> #IoType(#NatType) = (n: #NatType) => #{node = Foreign} /other(n);
(
    #{node = Foreign} /probe(x),
    #{node = Foreign} /probe(((n: #NatType) => n)(x)),
    #{node = Foreign} /other(x),
)
"#;

/// Two calls one declaration states convert where their operands do, each at the type its row demands of it, whatever reduction has made of the two sides; and two calls of two declarations are apart whatever they are handed.
#[test]
fn two_host_calls_convert_by_their_declaration_and_their_operands() {
    let mut certifier = certifier();
    let [call, respelled, other] =
        <[_; 3]>::try_from(components(&closing(CALLS))).expect("three calls");
    let io = term("#IoType(#NatType)");

    assert_eq!(convert(&mut certifier, &io, &call, &respelled), Ok(true));
    assert_eq!(convert(&mut certifier, &io, &call, &other), Ok(false));
}
