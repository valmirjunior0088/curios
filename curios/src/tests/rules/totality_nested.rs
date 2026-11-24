//! The size-change obligation through a witness handed on inside another's application: a family nested two witnesses deep, and one nested through a witness that recurses.
//!
//! The rule is the totality decision's, *A call in a callback is graded where its callee applies it* (`documentation/design/soundness/totality-of-the-erased-program.md`).

use super::test_support::*;

// A witness handed on inside another witness's application is graded where each applies what it is handed: `List`'s witness applies its element witness's method to elements below its lists, and that element witness, `List`'s again, applies the tree's own to elements below those, so the method's call on a list of lists of children descends, and a type reaching `==` on the type is formed. The refusal hands lists built around the arguments themselves.
#[test]
fn a_witness_handed_on_inside_another_descends_where_each_applies_what_it_is_handed() {
    control(
        r#"
        use /std/{Nat, Bool, List};
        use /std/ops/{Eql};

        induct Doc: Type
        | num(Nat)
        | nest(List(List(Doc)))
        end

        satisfy Eql(Doc) {
            eql(a, b) =
                match a
                | num(n) => match b | num(m) => n == m | _ => false end
                | nest(cs) => match b | nest(ds) => cs == ds | _ => false end
                end,
        }

        let Same(a: Doc, b: Doc) -> /std/Bool/True =
            match a == b | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("cs == ds", "[[a]] == [[b]]")])
    .by_elaborator_in("/Same", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Same", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// The witnesses between need not be one container's: a pair's witness applies each component's method to a field below the pair, and an option's to the payload below the option, so a tree's children held as a list of labelled options descend through all three. The refusal hands a list built around the arguments themselves.
#[test]
fn a_witness_handed_on_through_a_pair_and_an_option_descends() {
    control(
        r#"
        use /std/{Nat, Bool, List, Option};
        use /std/ops/{Eql};

        induct Doc: Type
        | num(Nat)
        | nest(List({Nat, Option(Doc)}))
        end

        satisfy Eql(Doc) {
            eql(a, b) =
                match a
                | num(n) => match b | num(m) => n == m | _ => false end
                | nest(cs) => match b | nest(ds) => cs == ds | _ => false end
                end,
        }

        let Same(a: Doc, b: Doc) -> /std/Bool/True =
            match a == b | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("cs == ds", "[(0, Option/some(a))] == [(0, Option/some(b))]")])
    .by_elaborator_in("/Same", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Same", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A witness that hands itself, applied to its own parameters, to a container's publishes how it applies them: `Rose`'s witness applies its element's method to the label below its argument, and to the labels below the children it hands `List`'s witness itself for, so a document whose children are a rose of documents descends through it. The refusal hands a rose built around the arguments themselves.
#[test]
fn a_witness_that_hands_itself_on_publishes_how_it_applies_its_own_premise() {
    control(
        r#"
        use /std/{Nat, Bool, List};
        use /std/ops/{Eql};

        induct Rose(A: Type): Type
        | rose(A, List(Rose(A)))
        end

        satisfy (@A: Type, use Eql(A)) => Eql(Rose(A)) {
            ..
        }

        induct Doc: Type
        | num(Nat)
        | nest(Rose(Doc))
        end

        satisfy Eql(Doc) {
            eql(a, b) =
                match a
                | num(n) => match b | num(m) => n == m | _ => false end
                | nest(c) => match b | nest(d) => c == d | _ => false end
                end,
        }

        let Same(a: Doc, b: Doc) -> /std/Bool/True =
            match a == b | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("c == d", "Rose/rose(a, []) == Rose/rose(b, [])")])
    .by_elaborator_in("/Same", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Same", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// What the witness between applies is read off its own summary and never assumed: `Held`'s witness compares the items it holds, below its arguments, so a document's children held so descend through `List`'s witness and it. The refusal has it compare what the functions it holds answer, values it did not take from its arguments, so the document's method is applied to what nothing places below the document.
#[test]
fn a_witness_between_that_applies_its_premise_to_what_it_built_does_not_descend() {
    control(
        r#"
        use /std/{Nat, Bool, List};
        use /std/ops/{Eql};

        struct Held(A: Type): Type {
            item: A,
            make: ({}) -> A,
        }

        satisfy (@A: Type, use Eql(A)) => Eql(Held(A)) {
            eql(a, b) = a.item == b.item,
        }

        induct Doc: Type
        | num(Nat)
        | nest(List(Held(Doc)))
        end

        satisfy Eql(Doc) {
            eql(a, b) =
                match a
                | num(n) => match b | num(m) => n == m | _ => false end
                | nest(cs) => match b | nest(ds) => cs == ds | _ => false end
                end,
        }

        let Same(a: Doc, b: Doc) -> /std/Bool/True =
            match a == b | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("a.item == b.item", "a.make(()) == b.make(())")])
    .by_elaborator_in("/Same", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Same", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// What a record applied where it was answered is counted where the record is handed on too: `d`'s field lambda applies `f` below its own argument, and `call`, handed the record, applies that field at its own, so `g`, which hands `call` the record `d` answers of it, descends. The refusal has `d`'s plain field hold `f(100)`: read by what the field lambda applies alone, `g` would be known to terminate while answering the record calls `g(100)`.
#[test]
fn a_record_handed_on_states_what_it_applied_where_it_was_answered() {
    control(
        r#"
        use /std/{Nat, Bool};

        struct Handlers: Type {
            y: Nat,
            m: (Nat) -> Nat,
        }

        let d(f: (Nat) -> Nat) -> Handlers =
            Handlers { y = 0, m = (x) => match x | 0 => 0 | n + 1 => f(n) end };

        let call(r: Handlers, t: Nat) -> Nat = r.m(t);

        let g(t: Nat) -> Nat = call(d(g), t);

        let Small(t: Nat) -> /std/Bool/True =
            match g(t) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("y = 0,", "y = f(100),")])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}
