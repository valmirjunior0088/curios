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

        let Small(e: Expr) -> /std/Bool/True =
            match size(e) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

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

        let Small(e: Expr) -> /std/Bool/True =
            match eval(e) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

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

// A record a caller builds around its own argument holds that argument as a field: the field is below the record, and the record is below nothing the caller holds. `eval` hands `eval_both` the payload `p`, below `e`, and `eval_both` calls `eval` on its fields, so the cycle descends; the refusal hands `eval_both` the record `(e, p.1)`, whose first field is `e` again.
#[test]
fn a_field_of_a_record_built_around_the_argument_is_no_descent() {
    control(
        r#"
        use /std/{Nat, Bool};

        induct Expr: Type
        | lit(Nat)
        | pair({Expr, Expr})
        end

        let eval(e: Expr) -> Nat =
            match e | lit(n) => n | pair(p) => eval_both(p) end
        and eval_both(b: {Expr, Expr}) -> Nat =
            eval(b.0) + eval(b.1);

        let Small(e: Expr) -> /std/Bool/True =
            match eval(e) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("eval_both(p)", "eval_both((e, p.1))")])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A binder a summary bounds is a record its fields are below, and they are below what the binder stands below: `apply` applies `g` to the record it is handed, so `b` stands where `p` does, below `e`, and `size` on `b.0` and `b.1` descends. The refusal hands `apply` the record `(e, p.1)`, which stands below nothing `size` holds, so `b.0` is `e` again.
#[test]
fn a_field_of_a_binder_a_summary_bounds_is_below_what_the_binder_is_below() {
    control(
        r#"
        use /std/{Nat, Bool};

        induct Expr: Type
        | lit(Nat)
        | pair({Expr, Expr})
        end

        let apply(x: {Expr, Expr}, g: ({Expr, Expr}) -> Nat) -> Nat = g(x);

        let size(e: Expr) -> Nat =
            match e | lit(n) => n | pair(p) => apply(p, (b) => size(b.0) + size(b.1)) end;

        let Small(e: Expr) -> /std/Bool/True =
            match size(e) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("apply(p, ", "apply((e, p.1), ")])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A projection of a record literal is the field it holds, through a chain of them: `Two { first = p.0, both = p }.both.1` is `p.1`, below `e`. The refusal holds `(p.0, e)` at `both`, so the chain reads `e` again.
#[test]
fn a_chain_of_projections_of_record_literals_is_the_field_it_reaches() {
    control(
        r#"
        use /std/{Nat, Bool};

        induct Expr: Type
        | lit(Nat)
        | pair({Expr, Expr})
        end

        struct Two: Type {
            first: Expr,
            both: {Expr, Expr},
        }

        let leaves(e: Expr) -> Nat =
            match e | lit(n) => n | pair(p) => leaves(Two { first = p.0, both = p }.both.1) + leaves(p.0) end;

        let Small(e: Expr) -> /std/Bool/True =
            match leaves(e) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("both = p }", "both = (p.0, e) }")])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A field of a proof record holding the very proposition matched is below the proof: `step(h)` is an arm no least derivation of `Tower` takes, since `base()` derives it in one step, so `climb(h.0)` there is excused and the control stands. The arm a least derivation does take is never excused: the refusal has `base()` hand `climb` the derivation `step((t, qed))` built around `t`, which by irrelevance is `t` again.
#[test]
fn a_field_of_the_proposition_matched_is_below_the_proof_it_was_taken_from() {
    control(
        r#"
        use /std/{Bool, Eq};

        induct Tower: Prop
        | base()
        | step(h: {Tower, Bool/Holds(true)})
        end

        let climb(t: Tower) -> Bool/Holds(true) =
            match t | base() => Bool/True/qed() | step(h) => climb(h.0) end;

        let Fine(t: Tower) -> Prop = Eq()(climb(t), climb(t));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "base() => Bool/True/qed()",
        "base() => climb(Tower/step((t, Bool/True/qed())))",
    )])
    .by_elaborator_in("/climb", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/climb", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

// A case of an expression takes its value apart as a case of a binder does, so the binders it opens are below the value the expression reads at: a payload of a case on `x.next` is below `x`, and `F(y)` descends where it is reached by a type. The refusal hands `F` back `x` itself, which the elaborator raises where `F` is declared, so the certifier's refusal of it is its own suite's (`certifier::calls::tests`, under this name).
#[test]
fn a_case_on_a_field_takes_apart_what_is_below_the_record() {
    control(
        r#"
        use /std/{Option};

        struct S: Type { next: Option(S) }

        let F(x: S) -> Type = match x.next | none() => {} | some(y) => F(y) end;

        let g(x: S, y: F(x)) -> F(x) = y;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| some(y) => F(y) end", "| some(y) => F(x) end")])
    .by_elaborator_in("/F", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    });
}

// A structure that holds itself through its fields alone has no finite value, and a field of one reads strictly below it, so a recursion on its fields descends without end on the open value a type is reduced under: `F(x) = F(x.next)` over `struct S { next: S }` unfolds `x.next.next…` wherever `F(x)` stands in a type, until the budget refuses it. It is refused where it is declared, in each way a path back through fields alone is spelled: a field of its own type, a component of a tuple, a parameter another structure holds at a field, and its own declaration at arguments its field grows, which the walk meets on its first step. The control holds itself through a family at each of those places, and through a list beside them, and stands; the certifier's twin is `recheck::positivity_tests`'s test of this name.
#[test]
fn a_record_holding_itself_through_fields_alone_is_refused() {
    let control = control(
        r#"
        use /std/{List, Option};

        struct S: Type { next: Option(S) }

        struct T: Type { kids: List(T) }

        struct Pair(A: Type): Type { first: A, second: A }

        struct U: Type { pair: Pair(Option(U)) }

        struct Q(A: Type): Type { q: Option(Q({A, A})) }

        /std/print("ok")
        "#,
        b"ok",
    );
    for (from, to, at) in [
        ("next: Option(S)", "next: S", "/S"),
        ("next: Option(S)", "next: {Option(S), S}", "/S"),
        ("pair: Pair(Option(U))", "pair: Pair(U)", "/U"),
        ("q: Option(Q({A, A}))", "q: Q({A, A})", "/Q"),
    ] {
        control
            .refused(&[(from, to)])
            .by_elaborator_in(at, |error| {
                matches!(error, curios_elab::Error::HoldsItself { .. })
            });
    }
}

// `/std/Json/encode` recurses through `List/map`, on the elements of an array and on the second field of each pair of an object, each element bounded below the list it is drawn from: with a projection read below the binder it projects, the library's encoder is total, and a type may reach it.
#[test]
fn the_librarys_json_encoder_is_reached_by_a_type() {
    control(
        r#"
        use /std/{Json, Str, Bool};

        let Short(j: Json) -> /std/Bool/True =
            match Str/length(Json/encode(j)) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    );
}
