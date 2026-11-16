//! A projection in size-change: a record is the node of its fields, so a field of one is strictly below it, whatever binds the record — an arm's payload, a parameter, a binder a summary bounds — and a field of a proof record ranks below the record as each premise of its least derivation does.
//!
//! The rule is the totality decision's, *A proposition's own case is structure, at a least derivation* (`documentation/design/soundness/totality-of-the-erased-program.md`).

use super::test_support::*;

// A tuple an arm takes out of its constructor is a payload, and each of its fields is below it: `size` on `p.0` and `p.1` descends below `e`. The refusal projects a tuple built around `e` itself, which is `e` again.
#[test]
fn a_field_of_a_payload_tuple_is_below_its_value() {
    control(
        r#"
        use /std/{Nat, Bool};

        induct Expr: Type
        | lit(Nat)
        | pair({Expr, Expr})
        end

        let size(e: Expr) -> Nat =
            match e | lit(n) => n | pair(p) => size(p.0) + size(p.1) end;

        let Small(e: Expr) -> Prop = Bool/Holds(size(e) == 0);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("size(p.0)", "size((e, e).0)")])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A structure handed to a sibling as a parameter is a record its fields are below: `eval` hands `eval_two` the payload `q`, below `e`, and `eval_two` calls `eval` on fields of its parameter, below it, so the cycle descends. The refusal has `eval_two` call `eval` on an expression it built around its parameter.
#[test]
fn a_field_of_a_parameters_structure_is_below_the_parameter() {
    control(
        r#"
        use /std/{Nat, Bool};

        struct Two(A: Type): Type {
            first: A,
            second: A,
        }

        induct Expr: Type
        | lit(Nat)
        | both(Two(Expr))
        end

        let eval(e: Expr) -> Nat =
            match e | lit(n) => n | both(q) => eval_two(q) end
        and eval_two(p: Two(Expr)) -> Nat =
            eval(p.first) + eval(p.second);

        let Small(e: Expr) -> Prop = Bool/Holds(eval(e) == 0);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("eval(p.first)", "eval(Expr/both(p))")])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A field of a proof record ranks below the record, as each premise of the least derivation of its proposition does: `ok` recurses on the proof of `Even(m)` the payload `h` holds, below `e`. The refusal hands `ok` the derivation it was matching, rebuilt.
#[test]
fn a_field_of_a_proof_record_is_below_the_record() {
    control(
        r#"
        use /std/{Nat, Bool, Eq};

        induct Even: (Nat) -> Prop
        | zero(): (0)
        | twice(m: Nat, h: {Even(m), Bool/Holds(true)}): (m + 2)
        end

        let ok(@n: Nat, e: Even(n)) -> Bool/Holds(true) =
            match e | zero() => Bool/True/qed() | twice(m, h) => ok(@m, h.0) end;

        let Fine(@n: Nat, e: Even(n)) -> Prop = Eq()(ok(@n, e), ok(@n, e));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("ok(@m, h.0)", "ok(@m + 2, Even/twice(m, h))")])
    .by_elaborator_in("/Fine", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Fine", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// `/std/Json/encode` recurses through `List/map`, on the elements of an array and on the second field of each pair of an object, each element bounded below the list it is drawn from: with a projection read below the binder it projects, the library's encoder is total, and a type may reach it.
#[test]
fn the_librarys_json_encoder_is_reached_by_a_type() {
    control(
        r#"
        use /std/{Json, Str, Bool};

        let Short(j: Json) -> Prop = Bool/Holds(Str/len(Json/encode(j)) == 0);

        /std/print("ok")
        "#,
        b"ok",
    );
}
