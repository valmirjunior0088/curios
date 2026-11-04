use crate::{Date, Proof, Status, Ticket, Witness, refused};

pub(super) const TICKET: Ticket = Ticket {
    title: "A payload that quantifies over `Prop` is applied at its own family, and the application is graded below its constructor",
    found_on: Date::new(2026, 10, 8),
    status: Status::Fixed,
    witnesses: &[
        Witness {
            what: "a proof by recursion on what a payload returns of the scrutinee, the payload the polymorphic identity",
            proof: Proof::Program(A_PAYLOAD_RETURNS_THE_SCRUTINEE_IT_IS_APPLIED_TO),
            expect: refused![elaborator: curios_elab::Error::UniverseInconsistency { .. }],
        },
        Witness {
            what: "the same recursion on what the payload returns of its constructor built again",
            proof: Proof::Program(A_PAYLOAD_RETURNS_ITS_CONSTRUCTOR_BUILT_AGAIN),
            expect: refused![elaborator: curios_elab::Error::UniverseInconsistency { .. }],
        },
        Witness {
            what: "the recursion on what the payload returns of the scrutinee, as a term",
            proof: Proof::Module(A_PAYLOAD_APPLIED_AT_ITS_OWN_FAMILY),
            expect: refused![kernel: curios_cert::Error::Oversized { .. }],
        },
    ],
};

/// `f` quantified over every proposition, `Bad` among them, so `f(Bad)(b)` was a `Bad`, and an application of a payload is graded a child of its constructor. With `f` the identity it is `b` itself, and the recursion that was judged to descend took the same value for ever. `Bad` is now sized: holding `f`, it sits above the propositions `f` ranges over, and is none of them.
const A_PAYLOAD_RETURNS_THE_SCRUTINEE_IT_IS_APPLIED_TO: &str = r#"
    use /std/Bool/{False};
    induct Bad: Prop
    | c(f: (A: Prop) -> (A) -> A)
    end
    let bad(b: Bad) -> False = match b | c(f) => bad(f(Bad)(b)) end;
    bad(Bad/c((A) => (x) => x))
    "#;

/// The argument names no variable the arm was handed but the payload: what is graded is the application, whatever it is applied to.
const A_PAYLOAD_RETURNS_ITS_CONSTRUCTOR_BUILT_AGAIN: &str = r#"
    use /std/Bool/{False};
    induct Bad: Prop
    | c(f: (A: Prop) -> (A) -> A)
    end
    let bad(b: Bad) -> False = match b | c(f) => bad(f(Bad)(Bad/c(f))) end;
    bad(Bad/c((A) => (x) => x))
    "#;

/// A `Prop`-sorted family carried no size condition, so `Bad` held a payload that quantified over the sort `Bad` was in, and the payload's declared type names `Bad` nowhere, so strict positivity had nothing to refuse. The size order reads an application of a payload as a child of its constructor, on the premise that a payload of a foreign type cannot hand back a value of the family; applied at `Bad` this one did, and at the identity it handed back the scrutinee. The declaration is refused where it is sized: its payload does not fit under `Prop` at level zero.
const A_PAYLOAD_APPLIED_AT_ITS_OWN_FAMILY: &str = r#"
    #{absent = [/Bad/c]} induct /Bad: pub Prop
    | c(f: (A: Prop) -> (x: A) -> A)
    end
    #{node = Rec} (let bad: (b: #{node = InductType} /Bad()) -> /sys/Bool/False =
        (b2: #{node = InductType} /Bad()) =>
            match b2: (x2) => /sys/Bool/False
            | c(f) =>
                bad(f(#{node = InductType} /Bad())(b2))
            end;
    bad(#{node = Variant} /Bad/c((A: Prop) => (x: A) => x)))
    "#;
