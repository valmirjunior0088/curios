//! An effect is not a value: what may be a scrutinee, an argument, or inhabit a pure arrow, and what may take a description apart.

use {super::test_support::*, crate::tests::test_support::*, curios_elab::Error};

// The arm rule at its one arm with no case value of its own. A `| _ =>` catch-all binds nothing and refines no index, so the only instance it can be checked at is the scrutinee's — which is the instance the elimination then hands its caller.
//
// This is a disagreement between the two checkers rather than a route to `False`: the elaborator checks the catch-all at the actual scrutinee, and a kernel opening the motive's scrutinee binder at the family *type* `/Three` would refuse this program with `/Three` standing in both term positions of the `Eq`. That direction fails closed — nothing well-typed inhabits an expectation with a type substituted for a value — but the certifier's own judgment is the one that matters, and it would establish nothing about the scrutinee for any catch-all it accepted. No prelude catch-all has a scrutinee-dependent motive, so this fixture is where the rule is asked.
//
// The instance is pinned where the rule lives, in `curios_cert::kernel::infer::eliminate::coverage_tests`: `a_catch_all_sees_its_own_scrutinee` with its control `a_catch_all_at_another_value_of_the_family_is_refused`, which asserts the expectation itself so that not checking the catch-all cannot pass for closing the hole.
#[test]
fn a_catch_all_is_checked_at_its_scrutinee() {
    assert_eq!(run(A_CATCH_ALL_IS_CHECKED_AT_ITS_SCRUTINEE), b"1");
}

// The purity premise every refinement rests on: an arm is reached only when its scrutinee *equals* the case's value, which a term of non-`Io` type does because it denotes one value. An effectful read does not — two occurrences of `Cell/fill(c, true)` around a `Cell/fill(c, false)` would denote two values, refining both would re-read `p : Eq()(Cell/fill(c, true), true)` at `Eq()(false, true)` in the inner arm, and `/std/Bool/false_neq_true` would turn that into `/std/Bool/False` in an arm the run reaches. Its type is `Io(Bool)`, opaque and with no cases, so nothing lets the program treat it as a `Bool`, and the refusal names `Io`. The control forces the read once and matches the `Bool` it yields, which the refusal leaves a description.
#[test]
fn an_effectful_scrutinee_is_not_a_value() {
    control(
        r#"
        use /std/{Cell, Eq, Bool, Str};

        let entry() -> /std/Io({}) =
            let c = Cell/new(@Bool)!;
            let read = Cell/fill(c, true)!;

            let forged : Str =
                match read
                | true =>
                    let p : Eq()(read, true) = Eq/refl();
                    let done = Cell/fill(c, false);
                    match read
                    | true => "second read true"
                    | false => match Bool/false_neq_true(p) end
                    end
                | false => "first read false"
                end;

            /std/print(forged);
        entry()
        "#,
        b"second read true",
    )
    .refused(&[
        ("let read = Cell/fill(c, true)!;", "let read = Cell/fill(c, true);"),
    ])
    .by_elaborator_in(
        "/entry",
        |error| matches!(error, Error::NotBoolType { head_type } if pinned(head_type) == "Io(Bool)"),
    );
}

/// Asserted on the *argument*: the fixture uses `Cell/fill(c, true) : Io(Bool)`, which `f : (Bool) -> Bool` does not take, so the derivation is refused at the first occurrence and never reaches a refinement at all. The control's read is the value `true`.
#[test]
fn an_effect_behind_a_stuck_head_is_not_an_argument() {
    control(
        r#"
        use /std/{Cell, Eq, Bool, Str};

        let entry() -> /std/Io({}) =
            /std/print(
                ((f : (Bool) -> Bool,
                  g : (Bool) -> Bool,
                  h : (x : Bool) -> Eq()(g(x), f(x)),
                  c : Cell(Bool)) =>
                    let read = true;
                    match f(read)
                    | true =>
                        let step(p : Eq()(g(read), true)) -> Str =
                            let done = Cell/fill(c, false);
                            match g(read)
                            | true => "second read true"
                            | false => match Bool/false_neq_true(p) end
                            end;
                        step(h(read))
                    | false => "first read false"
                    end
                )((b) => b, (b) => b, (x) => Eq/refl(), Cell/new(@Bool)!)
            );
        entry()
        "#,
        b"second read true",
    )
    .refused(&[
        ("let read = true;", "let read = Cell/fill(@Bool, c, true);"),
    ])
    .by_elaborator_in("/entry", |error| {
        matches!(error, Error::TypeMismatch { inferred, expected, .. } if pinned(inferred) == "Io(Bool)" && pinned(expected) == "Bool")
    });
}

/// Asserted on the offending *argument*: the refusal is that `(b) => Cell/fill(c, true)` cannot be passed where a `(Bool) -> Bool` is wanted, so the description type has to appear in the diagnostic. A fixture refused anywhere else — at the cell, at `Eq/refl`, at an arm — would not produce that, and this file's rule is that a rule test asserts its own diagnostic. The control's function answers `true`.
#[test]
fn an_effect_cannot_inhabit_a_pure_arrow() {
    control(
        r#"
        use /std/{Cell, Eq, Bool, Str};
        use /std/Bool/{False};

        let forge(f : (Bool) -> Bool, c : Cell(Bool), p : Eq()(f(true), true)) -> Str =
            let done = Cell/fill(c, false);
            match f(true)
            | false =>
                let contradiction : False = Bool/false_neq_true(p);
                match contradiction end
            | true => "no contradiction"
            end;

        let entry() -> /std/Io({}) =
            /std/print(
                ((c : Cell(Bool)) =>
                    ((f : (Bool) -> Bool) =>
                        match f(true)
                        | true => forge(f, c, Eq/refl())
                        | false => "first read false"
                        end
                    )((b) => true)
                )(Cell/new(@Bool)!)
            );
        entry()
        "#,
        b"no contradiction",
    )
    .refused(&[
        ("((b) => true)", "((b) => Cell/fill(c, true))"),
    ])
    .by_elaborator_in("/entry", |error| {
        matches!(error, Error::TypeMismatch { inferred, expected, .. } if pinned(inferred) == "Io(Bool)" && pinned(expected) == "Bool")
    });
}

// The control the four refusals below are read against, and what makes them the only thing in the way: a description of a computation yielding `False` is *constructible*. `exit` is typed `(@A : Type, Byte) -> Io(A)`, so no proposition may be its result — but a carrier with no constructors may, and `bind`'s continuation receives the `Empty` and eliminates it into `False` inside another description. The program compiles and runs, which is what the decision means by an inhabitant of `Io(False)` proving nothing.
#[test]
fn a_description_of_a_contradiction_still_compiles() {
    assert_eq!(run(A_DESCRIPTION_OF_A_CONTRADICTION_STILL_COMPILES), b"ok");
}

// No match case. The carrier has no constructors, which is exactly the shape the zero-arm match eliminates — so the one rule that takes a type apart without naming a case is the first thing to try on a description, and `Io(Empty)` would hand `False` out of the fixture above. The refusal names the head's type, so it is the carrier that is refused and not the arity. Its own control eliminates an `Empty` it is handed.
#[test]
fn a_description_is_no_empty_type() {
    control(
        r#"
        use /std/{Proc};
        use /std/Bool/{False};

        induct Empty : pub Type end

        let boom(e : Empty) -> False =
            match e end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("let boom(e : Empty) -> False", "let boom() -> False"),
        ("match e end;", "match Proc/exit(@Empty, 1) end;"),
    ])
    .by_elaborator_in("/boom", |error| {
        matches!(error, Error::NotAInductType { no_arms: true, .. })
    });
}

// No projection, which is the elimination that meets no guard at all: a field read answers whatever the record holds, so a description that read as a record of its payload would release it with nothing to check. Its own control projects a record it is handed.
#[test]
fn a_description_is_no_record() {
    control(
        r#"
        use /std/{Proc};
        use /std/Bool/{False};

        induct Empty : pub Type end

        let absurd(e : Empty) -> False =
            match e end;

        let boom(r : {Empty, Empty}) -> False =
            absurd(r.0);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        (
            "let boom(r : {Empty, Empty}) -> False",
            "let boom() -> False",
        ),
        ("absurd(r.0);", "absurd(Proc/exit(@Empty, 1).0);"),
    ])
    .by_elaborator_in("/boom", |error| matches!(error, Error::NotATuple { .. }));
}

// An eliminator in any spelling, and the one spelling a program may declare for itself: `Monad/Lift(M, N)`'s `lift` carries an action from one monad into another, and `N` is the caller's to choose. Chosen as a nominal wrapper, the embedding is `(Io(A)) -> Box(A)`, whose only honest body needs the `A` — so the whole discipline reduces to there being no term of `(Io(A)) -> A`, and the refusal stands in the witness where the author would write one. A lambda-shaped identity monad cannot even be keyed, which is why this is stated over a struct. Its own control lifts `Box` into itself.
#[test]
fn a_description_cannot_be_carried_out_of_its_monad() {
    control(
        r#"
        use /std/{Io, Monad, Proc};
        use /std/Bool/{False};

        induct Empty : pub Type end

        struct Box(A : Type) : pub Type { held : A }

        satisfy Monad(Box) {
            pure(@_, a) = Box { a },
            bind(@_, @_, m, f) = f(m.held),
        }

        satisfy Monad/Lift(Box, Box) {
            lift(@_, m) = Box { m.held },
        }

        let carried(@A : Type, a : Box(A)) -> Box(A) =
            Monad/lift(a);

        let boom = carried;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("satisfy Monad/Lift(Box, Box) {\n            lift(@_, m) = Box { m.held },", "satisfy Monad/Lift(Io, Box) {\n            lift(@_, m) = Box { m },"),
        ("a : Box(A)) -> Box(A)", "a : Io(A)) -> Box(A)"),
        ("let boom = carried;", "let boom() -> False =\n            match carried(Proc/exit(@Empty, 1)).held end;"),
    ])
    .by_elaborator_in("/witness~/std/Monad/Lift(Io, /Box)", |error| {
        matches!(error, Error::TypeMismatch { inferred, expected, .. } if pinned(inferred) == "Io(x)" && pinned(expected) == "x")
    });
}

// The carrier is opaque to conversion as well, which is the eliminator a transport would need: held one with what it yields, `Eq/refl()` would inhabit the identification and `Eq/subst` would do the rest, with no elimination of a description written anywhere. It is asserted here rather than in the law grid because it is this rule that rests on it. Its own control equates the description with itself.
#[test]
fn a_description_is_not_what_it_yields() {
    control(
        r#"
        use /std/{Io, Eq};

        induct Empty : pub Type end

        let _one : Eq()(Io(Empty), Io(Empty)) = Eq/refl();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("Eq()(Io(Empty), Io(Empty))", "Eq()(Io(Empty), Empty)"),
    ])
    .by_elaborator_in("/_one", |error| matches!(error, Error::TypeMismatch { inferred, expected, .. } if pinned(inferred) == "/std/Eq/Eq(Type)(Io(/Empty), Io(/Empty))" && pinned(expected) == "/std/Eq/Eq(Type)(Io(/Empty), /Empty)"));
}

// A description is taken apart by no elimination: matched by a family's case, projected, or split by a `Bool` case form. `curios_cert`'s `recheck::intrinsic_tests` puts the module to the kernel under this name. The control takes apart a `Wrap`, a record and a `Bool`.
#[test]
fn a_description_is_taken_apart_by_no_elimination() {
    control(
        r#"
        use /std/{Io, Bool};
        use /std/Bool/{False};

        pub induct Wrap(A: Prop): Type
        | mk(a: A)
        end

        pub let matched(io: Wrap(False)) -> False = match io | mk(a) => a end;
        pub let projected(io: {False, False}) -> False = io.0;
        pub let split(io: Bool) -> Bool = match io | true => true | false => false end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("matched(io: Wrap(False))", "matched(io: Io(False))"),
        ("projected(io: {False, False})", "projected(io: Io(False))"),
        ("split(io: Bool)", "split(io: Io(Bool))"),
    ])
    .by_elaborator_in("/matched", |error| {
        matches!(error, Error::NotAInductType { .. })
    })
    .by_elaborator_in("/projected", |error| {
        matches!(error, Error::NotATuple { .. })
    })
    .by_elaborator_in("/split", |error| matches!(error, Error::NotBoolType { .. }));
}
