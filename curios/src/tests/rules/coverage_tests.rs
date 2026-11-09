//! Coverage: an elimination enumerates its constructors, and a clash is told from an index that may still meet.

use {super::test_support::*, crate::tests::test_support::*, curios_elab::Error};

// Coverage. A missing arm leaves an elimination undefined at that constructor, which is a proof of the motive at an index nothing established. The control writes `none`'s arm.
#[test]
fn an_elimination_must_enumerate_its_constructors() {
    control(
        r#"
        use /std/{Nat, Option};

        let f(o : Option(Nat)) -> Nat =
            match o
            | some(x) => x
            | none() => 0
            end;

        /std/print(Nat/to_str(f(Option/some(5))))
        "#,
        b"5",
    )
    .refused(&[("            | none() => 0\n", "")])
    .by_elaborator_in("/f", |error| {
        matches!(error, Error::MatrixUncovered { indexed: false, .. })
    });
}

// An index pair is impossible only where no value meets it: a truncated difference is zero wherever its subtrahend is at least its minuend, so `x - y` against `0` is no clash. The control's index is a successor, which no value meets zero at.
#[test]
fn a_truncated_difference_against_zero_is_no_clash() {
    control(
        r#"
        use /std/{Nat, Eq};
        use /std/Bool/{False};

        let absurd(x: Nat, y: Nat, p: Eq()(x + 1, 0)) -> False = match p end;

        /std/print(Nat/to_str(0))
        "#,
        b"0",
    )
    .refused(&[("Eq()(x + 1, 0)", "Eq()(x - y, 0)")])
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// The same over the free monoid: a concatenation of chunks is empty wherever every chunk is, so `[..xs, ..xs]` against `[]` is no clash. The control appends an element, which no value leaves empty.
#[test]
fn a_concatenation_of_chunks_against_the_empty_list_is_no_clash() {
    control(
        r#"
        use /std/{Nat, Eq, List};
        use /std/Bool/{False};

        let absurd(xs: List(Nat), p: Eq()([..xs, 1], [])) -> False = match p end;

        /std/print(Nat/to_str(0))
        "#,
        b"0",
    )
    .refused(&[("[..xs, 1]", "[..xs, ..xs]")])
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// `j + j` is `0` at `j = 0` and a successor everywhere else, so neither case's target clashes with it and each arm is written. `curios_cert`'s `recheck::case_tests` puts the module to the kernel under this name, with `j + 1` as the control a clash excuses. The control writes both arms where `j + j` may meet either target.
#[test]
fn an_arm_absent_where_a_sum_may_meet_its_target_is_refused() {
    let refusals = control(
        r#"
        use /std/{Nat};

        induct Ix: (i: Nat) -> Type
        | zero(): (0)
        | succ(k: Nat): (k + 1)
        end

        pub let only_zero(j: Nat, v: Ix(j + j)) -> Nat = match v | zero() => 0 | succ(k) => 1 end;
        pub let only_succ(j: Nat, v: Ix(j + j)) -> Nat = match v | zero() => 2 | succ(k) => k end;
        pub let excused(j: Nat, v: Ix(j + 1)) -> Nat = match v | succ(k) => k end;

        /std/print(Nat/to_str(only_succ(1, Ix/succ(1))))
        "#,
        b"1",
    )
    .refused(&[(" | succ(k) => 1", ""), ("| zero() => 2 ", "")]);

    for at in ["/only_zero", "/only_succ"] {
        refusals.by_elaborator_in(at, |error| {
            matches!(error, Error::MatrixUncovered { indexed: true, .. })
        });
    }
}

// The carriers' algebra concludes a clash only where every value of the scrutinee's variables keeps the two apart, and each index below meets its one case's target at some value: a floorless sum, a product and a truncated difference are each a successor somewhere, a power of a base that may be zero is zero at `a = 0`, and a window, a concatenation of chunks and a fill of unknown count are each empty somewhere. So no arm is excused and each empty match is refused. `curios_cert`'s `recheck::case_tests` puts the module to the kernel under this name, with the controls a clash does excuse beside it. Each control's index clashes with its case's target at every value — `0`, `a * 0` and `0 - b` against a successor, a successor against zero, and a window, a run and a fill each ending in or counting a byte against the empty value — so each empty match is excused.
#[test]
fn an_arm_absent_where_a_carriers_index_may_meet_its_target_is_refused() {
    let refusals = control(
        r#"
        use /std/{Nat, Byte, Bytes};
        use /std/Nat/{pow};
        use /std/Bool/{False, Holds};

        induct Ix: (i: Nat) -> Type
        | succ(k: Nat): (k + 1)
        end

        induct Iz: (i: Nat) -> Type
        | zero(): (0)
        end

        induct By: (b: Bytes) -> Type
        | e(): (x[])
        end

        pub let sum(a: Nat, b: Nat, v: Ix(0)) -> False = match v end;
        pub let product(a: Nat, b: Nat, v: Ix(a * 0)) -> False = match v end;
        pub let difference(a: Nat, b: Nat, v: Ix(0 - b)) -> False = match v end;
        pub let power(a: Nat, b: Nat, v: Iz(pow(a, b + 1) + 1)) -> False = match v end;
        pub let window(
            xs: Bytes,
            s: Nat,
            l: Nat,
            @p: Holds(s + l <= Bytes/len(xs)),
            v: By(x[..Bytes/slice(xs, s, l), 1]),
        ) -> False =
            match v end;
        pub let chunks(xs: Bytes, ys: Bytes, v: By(x[..xs, 1])) -> False = match v end;
        pub let fill(n: Nat, c: Byte, v: By(Bytes/replicate(n + 1, c))) -> False = match v end;

        let forged = sum;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("v: Ix(0))", "v: Ix(a + b))"),
        ("Ix(a * 0)", "Ix(a * b)"),
        ("Ix(0 - b)", "Ix(a - b)"),
        ("pow(a, b + 1) + 1", "pow(a, b + 1)"),
        ("x[..Bytes/slice(xs, s, l), 1]", "Bytes/slice(xs, s, l)"),
        ("x[..xs, 1]", "x[..xs, ..ys]"),
        ("Bytes/replicate(n + 1, c)", "Bytes/replicate(n, c)"),
        (
            "let forged = sum;",
            "let forged: False = sum(1, 0, Ix/succ(0));",
        ),
    ]);

    for at in [
        "/sum",
        "/product",
        "/difference",
        "/power",
        "/window",
        "/chunks",
        "/fill",
    ] {
        refusals.by_elaborator_in(at, |error| {
            matches!(error, Error::MatrixUncovered { indexed: true, .. })
        });
    }
}

// The control the refusals above would be worthless without: where every value keeps the index apart from the target — a successor against zero, a run behind a chunk, an appended byte and a fill of a successor count against the empty value — the empty match is excused, and the program runs.
#[test]
fn an_arm_absent_where_a_carriers_index_clashes_with_its_target_is_excused() {
    assert_eq!(
        run(r#"
            use /std/{Nat, Byte, Bytes};
            use /std/Bool/{False};

            induct Iz: (i: Nat) -> Type
            | zero(): (0)
            end

            induct By: (b: Bytes) -> Type
            | e(): (x[])
            end

            pub let successor(a: Nat, v: Iz(a + 1)) -> False = match v end;
            pub let trailing(xs: Bytes, v: By(x[..xs, 1])) -> False = match v end;
            pub let single(xs: Bytes, c: Byte, v: By(x[..xs, c])) -> False = match v end;
            pub let filled(n: Nat, c: Byte, v: By(Bytes/replicate(n + 1, c))) -> False = match v end;

            /std/print("ok")
            "#),
        b"ok"
    );
}

// Irrelevance holds `[Pr/b()]` and `[Pr/a()]` one list, so `Ix/mk()` inhabits `Ix([Pr/b()])` and `forged` types at `False` wherever `absurd` does. A list's elements are terms and not decided values, so two runs whose heads are two of a proposition's constructors are no clash, and `mk` stays mandatory; `apart`'s `[]` against `[Pr/a()]` is a clash of length, which no element is read for, and is excused. `curios_cert`'s `recheck::case_tests` puts the module to the kernel under this name. The control's elements are values, whose constructors clash.
#[test]
fn two_lists_of_proofs_that_irrelevance_holds_one_do_not_clash() {
    control(
        r#"
        use /std/{List};
        use /std/Bool/{False};

        induct Pr: Type
        | a()
        | b()
        end

        induct Ix: (l: List(Pr)) -> Type
        | mk(): ([Pr/a()])
        end

        pub let absurd(v: Ix([Pr/b()])) -> False = match v end;
        pub let apart(v: Ix([])) -> False = match v end;

        let forged = absurd;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("induct Pr: Type", "induct Pr: Prop"),
        (
            "let forged = absurd;",
            "let forged: False = absurd(Ix/mk());",
        ),
    ])
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// Two `List` literals of one length are as many positions as a tuple's fields, so a definite clash at one element is a clash of the two, and two literals whose lengths differ clash whatever their elements: the empty match is excused at an element, a length and an element beside a shared one. The control is `two_lists_of_proofs_that_irrelevance_holds_one_do_not_clash`, where the elements are two of a proposition's constructors.
#[test]
fn two_list_literals_apart_in_an_element_or_in_length_clash() {
    assert_eq!(
        run(r#"
            use /std/{Nat, Eq};
            use /std/Bool/{False};

            pub let element(e: Eq()([0], [1])) -> False = match e end;
            pub let length(e: Eq()([0], [1, 2])) -> False = match e end;
            pub let beside(x: Nat, e: Eq()([0, x], [1, x])) -> False = match e end;

            /std/print("ok")
            "#),
        b"ok"
    );
}

// Two `List` literals of one length are decided pointwise and two whose lengths differ by their lengths, and neither reads a clash where a value meets the case: two of a proposition's constructors are one value wherever they stand, in a list, in a pair or forced twice, and a chunk, an appended base and a window hold lengths no literal fixes. `curios_cert`'s `recheck::case_tests` puts the module to the kernel under this name. The control's elements are values of a type, whose constructors clash, each list index faces the empty list, which a run and an appended element are longer than, and the symbolic run forced beside a literal byte is two bytes long.
#[test]
fn two_lists_that_meet_their_case_at_some_value_are_no_clash() {
    let refusals = control(
        r#"
        use /std/{List, Nat, Byte, Bytes};
        use /std/Bool/{False, Holds};

        induct Pr: Type
        | a()
        | b()
        end

        induct Ix: (l: List(Pr)) -> Type
        | mk(): ([Pr/a(), Pr/b()])
        end

        induct Mx: (l: List({Pr, Nat})) -> Type
        | mk(): ([(Pr/a(), 0)])
        end

        induct Ln: (l: List(Nat)) -> Type
        | one(): ([])
        end

        induct D: (x: List(Pr), y: List(Pr)) -> Type
        | dup(l: List(Pr)): (l, l)
        end

        induct E: (x: Bytes, y: Bytes) -> Type
        | dup(l: Bytes): (l, l)
        end

        pub let swapped(v: Ix([Pr/b(), Pr/a()])) -> False = match v end;
        pub let mixed(v: Mx([(Pr/b(), 0)])) -> False = match v end;
        pub let open(x: Nat, v: Ln([x])) -> False = match v end;
        pub let spread(x: Nat, xs: List(Nat), v: Ln([x, ..xs])) -> False = match v end;
        pub let appended(x: Nat, xs: List(Nat), v: Ln([..xs, x])) -> False = match v end;
        pub let window(xs: List(Nat), s: Nat, @p: Holds(s + 1 <= List/len(xs)), v: Ln([..List/slice(xs, s, 1), 0])) -> False =
            match v end;
        pub let twice(v: D([Pr/a()], [Pr/b()])) -> False = match v end;
        pub let bytes(c: Byte, v: E(x[c, c], x[1])) -> False = match v end;

        let forged = swapped;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("induct Pr: Type", "induct Pr: Prop"),
        ("| one(): ([])", "| one(): ([0])"),
        ("Ln([..List/slice(xs, s, 1), 0])", "Ln(List/slice(xs, s, 1))"),
        ("E(x[c, c], x[1])", "E(x[c], x[1])"),
        ("let forged = swapped;", "let forged: False = swapped(Ix/mk());"),
    ]);

    for at in [
        "/swapped",
        "/mixed",
        "/open",
        "/spread",
        "/appended",
        "/window",
        "/twice",
        "/bytes",
    ] {
        refusals.by_elaborator_in(at, |error| {
            matches!(error, Error::MatrixUncovered { indexed: true, .. })
        });
    }
}
