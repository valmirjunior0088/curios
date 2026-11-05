//! Coverage: an elimination enumerates its constructors, and a clash is told from an index that may still meet.

use {super::test_support::*, crate::test_support::*, curios_elab::Error};

// Coverage. A missing arm leaves an elimination undefined at that constructor, which is a proof of the motive at an index nothing established.
#[test]
fn an_elimination_must_enumerate_its_constructors() {
    refused(AN_ELIMINATION_MUST_ENUMERATE_ITS_CONSTRUCTORS).by_elaborator_in("/f", |error| {
        matches!(error, Error::MatrixUncovered { indexed: false, .. })
    });
}

// An index pair is impossible only where no value meets it: a truncated difference is zero wherever its subtrahend is at least its minuend, so `x - y` against `0` is no clash.
#[test]
fn a_truncated_difference_against_zero_is_no_clash() {
    refused(
        r#"
        use /std/{Nat, Eq};
        use /std/Bool/{False};

        let absurd(x: Nat, y: Nat, p: Eq()(x - y, 0)) -> False = match p end;

        /std/print(Nat/to_str(0))
        "#,
    )
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// The same over the free monoid: a concatenation of chunks is empty wherever every chunk is, so `[..xs, ..xs]` against `[]` is no clash.
#[test]
fn a_concatenation_of_chunks_against_the_empty_list_is_no_clash() {
    refused(
        r#"
        use /std/{Nat, Eq, List};
        use /std/Bool/{False};

        let absurd(xs: List(Nat), p: Eq()([..xs, ..xs], [])) -> False = match p end;

        /std/print(Nat/to_str(0))
        "#,
    )
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// `j + j` is `0` at `j = 0` and a successor everywhere else, so neither case's target clashes with it and each arm is written. `curios_cert`'s `recheck::case_tests` puts the module to the kernel under this name, with `j + 1` as the control a clash excuses.
#[test]
fn an_arm_absent_where_a_sum_may_meet_its_target_is_refused() {
    let refusals = refused(
        r#"
        use /std/{Nat};

        induct Ix: (i: Nat) -> Type
        | zero(): (0)
        | succ(k: Nat): (k + 1)
        end

        pub let only_zero(j: Nat, v: Ix(j + j)) -> Nat = match v | zero() => 0 end;
        pub let only_succ(j: Nat, v: Ix(j + j)) -> Nat = match v | succ(k) => k end;
        pub let excused(j: Nat, v: Ix(j + 1)) -> Nat = match v | succ(k) => k end;

        /std/print("unreachable")
        "#,
    );

    for at in ["/only_zero", "/only_succ"] {
        refusals.by_elaborator_in(at, |error| {
            matches!(error, Error::MatrixUncovered { indexed: true, .. })
        });
    }
}

// The carriers' algebra concludes a clash only where every value of the scrutinee's variables keeps the two apart, and each index below meets its one case's target at some value: a floorless sum, a product and a truncated difference are each a successor somewhere, a power of a base that may be zero is zero at `a = 0`, and a window, a concatenation of chunks and a fill of unknown count are each empty somewhere. So no arm is excused and each empty match is refused. `curios_cert`'s `recheck::case_tests` puts the module to the kernel under this name, with the controls a clash does excuse beside it.
#[test]
fn an_arm_absent_where_a_carriers_index_may_meet_its_target_is_refused() {
    let refusals = refused(
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

        pub let sum(a: Nat, b: Nat, v: Ix(a + b)) -> False = match v end;
        pub let product(a: Nat, b: Nat, v: Ix(a * b)) -> False = match v end;
        pub let difference(a: Nat, b: Nat, v: Ix(a - b)) -> False = match v end;
        pub let power(a: Nat, b: Nat, v: Iz(pow(a, b + 1))) -> False = match v end;
        pub let window(
            xs: Bytes,
            s: Nat,
            l: Nat,
            @p: Holds(s + l <= Bytes/len(xs)),
            v: By(Bytes/slice(xs, s, l)),
        ) -> False =
            match v end;
        pub let chunks(xs: Bytes, ys: Bytes, v: By(x[..xs, ..ys])) -> False = match v end;
        pub let fill(n: Nat, c: Byte, v: By(Bytes/replicate(n, c))) -> False = match v end;

        let forged: False = sum(1, 0, Ix/succ(0));

        /std/print("FORGED")
        "#,
    );

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

// Irrelevance holds `[Pr/b()]` and `[Pr/a()]` one list, so `Ix/mk()` inhabits `Ix([Pr/b()])` and `forged` types at `False` wherever `absurd` does. A list's elements are terms and not decided values, so two runs whose heads are two of a proposition's constructors are no clash, and `mk` stays mandatory; `apart`'s `[]` against `[Pr/a()]` is a clash of length, which no element is read for, and is excused. `curios_cert`'s `recheck::case_tests` puts the module to the kernel under this name.
#[test]
fn two_lists_of_proofs_that_irrelevance_holds_one_do_not_clash() {
    refused(
        r#"
        use /std/{List};
        use /std/Bool/{False};

        induct Pr: Prop
        | a()
        | b()
        end

        induct Ix: (l: List(Pr)) -> Type
        | mk(): ([Pr/a()])
        end

        pub let absurd(v: Ix([Pr/b()])) -> False = match v end;
        pub let apart(v: Ix([])) -> False = match v end;

        let forged: False = absurd(Ix/mk());

        /std/print("FORGED")
        "#,
    )
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}
