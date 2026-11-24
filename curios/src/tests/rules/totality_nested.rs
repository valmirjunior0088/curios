//! The size-change obligation through a witness handed on inside another's application: a family nested two witnesses deep, and one nested through a witness that recurses.
//!
//! The rule is the totality decision's, *A call in a callback is graded where its callee applies it* (`documentation/design/soundness/totality-of-the-erased-program.md`). The witnesses are `Spell`'s, a concept with one method and no law, so a refusal is derived by editing the method alone.

use super::test_support::*;

// A witness handed on inside another witness's application is graded where each applies what it is handed: `List`'s witness applies its element witness's method to elements below its list, and that element witness, `List`'s again, applies the document's own to elements below those, so the method's call on a list of lists of children descends, and a proof reaching the document's spelling is formed. The refusal hands a list built around the argument itself.
#[test]
fn a_witness_handed_on_inside_another_descends_where_each_applies_what_it_is_handed() {
    control(
        r#"
        use /std/{Nat, Bool, Str, List, Spell};

        induct Doc: Type
        | num(Nat)
        | nest(List(List(Doc)))
        end

        satisfy Spell(Doc) {
            spell(a) = match a | num(n) => Spell/spell(n) | nest(cs) => Spell/spell(cs) end,
        }

        let Spelled(a: Doc) -> /std/Bool/True =
            match Str/length(Spell/spell(a)) == 0
            | true => /std/Bool/True/qed()
            | false => /std/Bool/True/qed()
            end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Spell/spell(cs)", "Spell/spell([[a]])")])
    .by_elaborator_in("/Spelled", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Spelled", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// The witnesses between need not be one container's: a pair's witness applies each component's method to a field below the pair, and an option's to the payload below the option, so a document's children held as a list of labelled options descend through all three. The refusal hands a list built around the argument itself.
#[test]
fn a_witness_handed_on_through_a_pair_and_an_option_descends() {
    control(
        r#"
        use /std/{Nat, Bool, Str, List, Option, Spell};

        induct Doc: Type
        | num(Nat)
        | nest(List({Nat, Option(Doc)}))
        end

        satisfy Spell(Doc) {
            spell(a) = match a | num(n) => Spell/spell(n) | nest(cs) => Spell/spell(cs) end,
        }

        let Spelled(a: Doc) -> /std/Bool/True =
            match Str/length(Spell/spell(a)) == 0
            | true => /std/Bool/True/qed()
            | false => /std/Bool/True/qed()
            end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Spell/spell(cs)", "Spell/spell([(0, Option/some(a))])")])
    .by_elaborator_in("/Spelled", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Spelled", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A witness that hands itself, applied to its own parameters, to a container's publishes how it applies them: `Rose`'s derived witness applies its element's method to the label below its argument, and to the labels below the children it hands `List`'s witness itself for, so a document whose children are a rose of documents descends through it. The refusal hands a rose built around the argument itself.
#[test]
fn a_witness_that_hands_itself_on_publishes_how_it_applies_its_own_premise() {
    control(
        r#"
        use /std/{Nat, Bool, Str, List, Spell};

        induct Rose(A: Type): Type
        | rose(A, List(Rose(A)))
        end

        satisfy (@A: Type, use Spell(A)) => Spell(Rose(A)) {
            ..
        }

        induct Doc: Type
        | num(Nat)
        | nest(Rose(Doc))
        end

        satisfy Spell(Doc) {
            spell(a) = match a | num(n) => Spell/spell(n) | nest(c) => Spell/spell(c) end,
        }

        let Spelled(a: Doc) -> /std/Bool/True =
            match Str/length(Spell/spell(a)) == 0
            | true => /std/Bool/True/qed()
            | false => /std/Bool/True/qed()
            end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Spell/spell(c)", "Spell/spell(Rose/rose(a, []))")])
    .by_elaborator_in("/Spelled", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Spelled", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// What the witness between applies is read off its own summary and never assumed: `Held`'s witness spells the item it holds, below its argument, so a document's children held so descend through `List`'s witness and it. The refusal has it spell what the function it holds answers, a value it did not take from its argument, so the document's method is applied to what nothing places below the document.
#[test]
fn a_witness_between_that_applies_its_premise_to_what_it_built_does_not_descend() {
    control(
        r#"
        use /std/{Nat, Bool, Str, List, Spell};

        struct Held(A: Type): Type {
            item: A,
            make: ({}) -> A,
        }

        satisfy (@A: Type, use Spell(A)) => Spell(Held(A)) {
            spell(a) = Spell/spell(a.item),
        }

        induct Doc: Type
        | num(Nat)
        | nest(List(Held(Doc)))
        end

        satisfy Spell(Doc) {
            spell(a) = match a | num(n) => Spell/spell(n) | nest(cs) => Spell/spell(cs) end,
        }

        let Spelled(a: Doc) -> /std/Bool/True =
            match Str/length(Spell/spell(a)) == 0
            | true => /std/Bool/True/qed()
            | false => /std/Bool/True/qed()
            end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Spell/spell(a.item)", "Spell/spell(a.make(()))")])
    .by_elaborator_in("/Spelled", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Spelled", |error| {
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

// A law that leaves its answers to inference holds the comparison it is over by its name, at the instance it stands at: `seq` is generalized over its element's level, and `seq_law` hands `step` the comparisons of the tails with nothing written, so the proof names `seq` where a copy of its group would be a recursion no summary is filed of. The list's witness then publishes how its law applies its premise's, and a document nested through it has a lawful witness.
#[test]
fn a_family_nested_through_a_law_that_infers_its_answers_descends() {
    control(
        r#"
        use /std/{Nat, Eq, List, Ord, Ordering};

        concept Order(A: Type): pub Type {
            cmp(A, A) -> Ordering,
            law(a: A, b: A) -> Eq()(cmp(b, a), Ordering/flip(cmp(a, b))),
        }

        let step(
            @x: Ordering,
            @y: Ordering,
            @fx: Ordering,
            @fy: Ordering,
            head: Eq()(fx, Ordering/flip(x)),
            rest: (Eq()(x, Ordering/eq())) -> Eq()(fy, Ordering/flip(y)),
        ) -> Eq()(Ordering/then(fx, fy), Ordering/flip(Ordering/then(x, y))) =
            Ordering/then/flips(x, y, fx, fy, head, rest);

        let seq(@T: Type, use Order(T), a: List(T), b: List(T)) -> Ordering =
            match a
            | [] => match b | [] => Ordering/eq() | [_, .._] => Ordering/lt() end
            | [x, ..xs] =>
                match b
                | [] => Ordering/gt()
                | [y, ..ys] => Ordering/then(Order/cmp(x, y), seq(xs, ys))
                end
            end;

        let seq_law(
            @T: Type,
            use Order(T),
            a: List(T),
            b: List(T),
        ) -> Eq()(seq(b, a), Ordering/flip(seq(a, b))) =
            match a
            | [] => match b | [] => Eq/refl() | [_, .._] => Eq/refl() end
            | [x, ..xs] =>
                match b
                | [] => Eq/refl()
                | [y, ..ys] => step(Order/law(x, y), (_) => seq_law(xs, ys))
                end
            end;

        satisfy (@T: Type, use Order(T)) => Order(List(T)) {
            cmp = seq,
            law = seq_law,
        }

        induct Doc: Type
        | num(Nat)
        | arr(List(Doc))
        end

        satisfy Order(Doc) {
            cmp(a, b) =
                match a
                | num(n) => match b | num(m) => Ord/ord(n, m) | arr(_) => Ordering/lt() end
                | arr(xs) => match b | num(_) => Ordering/gt() | arr(ys) => Order/cmp(xs, ys) end
                end,
            law(a, b) =
                match a
                | num(n) => match b | num(m) => Ord/flips(n, m) | arr(_) => Eq/refl() end
                | arr(xs) => match b | num(_) => Eq/refl() | arr(ys) => Order/law(xs, ys) end
                end,
        }

        /std/print("ok")
        "#,
        b"ok",
    );
}

// A law that takes its container's law apart descends as one that answers it: `Doc`'s law matches on `List`'s at its children, and the arm's solution, `List`'s comparison handed `Doc`'s witness, is what inversion concluded and no call the arm makes, so the call through `List`'s law is graded where `List`'s witness applies its premise, below the children, by both checkers. The refusal hands `List`'s law lists built around the arguments themselves.
#[test]
fn a_law_taking_its_containers_law_apart_descends() {
    control(
        r#"
        use /std/{Nat, Bool, Eq, List};

        concept Order(A: Type): pub Type {
            ord(A, A) -> Bool,
            law(a: A, b: A) -> Eq()(ord(a, b), ord(a, b)),
        }

        let seq_ord(@T: Type, use Order(T), a: List(T), b: List(T)) -> Bool =
            match a
            | [] => match b | [] => true | [_, .._] => false end
            | [x, ..xs] => match b | [] => false | [y, ..ys] => Bool/and(Order/ord(x, y), seq_ord(xs, ys)) end
            end;

        let seq_law(@T: Type, use Order(T), a: List(T), b: List(T)) -> Eq()(seq_ord(a, b), seq_ord(a, b)) =
            match a
            | [] => Eq/refl()
            | [x, ..xs] =>
                match b
                | [] => Eq/refl()
                | [y, ..ys] => match Order/law(x, y) | refl() => match seq_law(xs, ys) | refl() => Eq/refl() end end
                end
            end;

        satisfy (@T: Type, use Order(T)) => Order(List(T)) {
            ord = seq_ord,
            law = seq_law,
        }

        induct Doc: Type | num(Nat) | arr(List(Doc)) end

        satisfy Order(Doc) {
            ord(a, b) =
                match a
                | num(n) => match b | num(m) => n == m | arr(_) => false end
                | arr(xs) => match b | num(_) => false | arr(ys) => Order/ord(xs, ys) end
                end,
            law(a, b) =
                match a
                | num(_) => Eq/refl()
                | arr(xs) => match b | num(_) => Eq/refl() | arr(ys) => match Order/law(xs, ys) | refl() => Eq/refl() end end
                end,
        }

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Order/law(xs, ys) | refl()", "Order/law([a], [b]) | refl()")])
    .by_elaborator_in("/witness~/Order(/Doc)", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/witness~/Order(/Doc)", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A witness that recurses through its own method hands its premise on at its own place: `Chain`'s spells its tail by the method it is defining, a call that fills the witness's leading lambdas with its own parameters and then projects the method, so the premise is the callee's parameter there as it is in a call that projects nothing. The witness publishes how it applies its premise, below its argument, and a document whose children are a chain of documents descends through it. The refusal hands a chain built around the argument itself.
#[test]
fn a_witness_that_recurses_through_its_own_method_publishes_how_it_applies_its_premise() {
    control(
        r#"
        use /std/{Nat, Bool, Str, Spell};

        induct Chain(A: Type): Type
        | end_()
        | link(A, Chain(A))
        end

        satisfy (@A: Type, use Spell(A)) => Spell(Chain(A)) {
            spell(a) =
                match a
                | end_() => ""
                | link(x, xs) => Str/concat(Spell/spell(x), Spell/spell(xs))
                end,
        }

        induct Doc: Type
        | num(Nat)
        | nest(Chain(Doc))
        end

        satisfy Spell(Doc) {
            spell(a) = match a | num(n) => Spell/spell(n) | nest(c) => Spell/spell(c) end,
        }

        let Spelled(a: Doc) -> /std/Bool/True =
            match Str/length(Spell/spell(a)) == 0
            | true => /std/Bool/True/qed()
            | false => /std/Bool/True/qed()
            end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Spell/spell(c)", "Spell/spell(Chain/link(a, Chain/end_()))")])
    .by_elaborator_in("/Spelled", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Spelled", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A family nested through a recursive family has a lawful witness: `Chain`'s derived `Eql` recurses through its own method and its own laws, and a forest of chains of forests derives its own through it.
#[test]
fn a_family_nested_through_a_recursive_family_derives_a_lawful_witness() {
    control(
        r#"
        use /std/ops/{Eql};

        induct Chain(A: Type): Type
        | end_()
        | link(A, Chain(A))
        end

        satisfy (@A: Type, use Eql(A)) => Eql(Chain(A)) {
            ..
        }

        induct Forest: Type
        | none()
        | some(Chain(Forest))
        end

        satisfy Eql(Forest) {
            ..
        }

        /std/print("ok")
        "#,
        b"ok",
    );
}

// A premise handed on at another place is no parameter of the callee's there, and the witness publishes nothing of it: `Alt`'s witness spells both its fields and its tail, the tail at its two parameters as the tail's type orders them, so where that type swaps them each premise is handed where the other stands, and a document nested through it is refused, where the same family with its parameters in order descends.
#[test]
fn a_witness_that_hands_its_premise_on_at_another_place_publishes_nothing_of_it() {
    control(
        r#"
        use /std/{Nat, Bool, Str, Spell};

        induct Alt(A: Type, B: Type): Type
        | stop()
        | go(A, B, Alt(A, B))
        end

        satisfy (@A: Type, @B: Type, use Spell(A), use Spell(B)) => Spell(Alt(A, B)) {
            spell(a) =
                match a
                | stop() => ""
                | go(x, y, rest) =>
                    Str/concat(Spell/spell(x), Str/concat(Spell/spell(y), Spell/spell(rest)))
                end,
        }

        induct Doc: Type
        | num(Nat)
        | nest(Alt(Doc, Doc))
        end

        satisfy Spell(Doc) {
            spell(a) = match a | num(n) => Spell/spell(n) | nest(c) => Spell/spell(c) end,
        }

        let Spelled(a: Doc) -> /std/Bool/True =
            match Str/length(Spell/spell(a)) == 0
            | true => /std/Bool/True/qed()
            | false => /std/Bool/True/qed()
            end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| go(A, B, Alt(A, B))", "| go(A, B, Alt(B, A))")])
    .by_elaborator_in("/Spelled", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Spelled", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}
