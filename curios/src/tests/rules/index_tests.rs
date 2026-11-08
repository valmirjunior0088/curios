//! Index inversion and K: what forces a binder, and what may not excuse an omitted arm.

use {super::test_support::*, crate::test_support::*, curios_elab::Error};

// The large-elimination guard again, at its *singleton* rung. A one-constructor proposition may eliminate into data only when every payload binder is non-informative — a proposition itself, or *pinned* by the constructor's index targets, as `Eq`'s `refl(@z) : (z, z)` recovers `z`.
//
// Occurring in an index target is not the same as being determined by one. `blur` is constant, so `Loose(0)` is inhabited by `mk(0)` and by `mk(7)` alike, and no index tells them apart — proof irrelevance identifies the two inhabitants while `extract` would observe them apart, and the gap is a closed inhabitant of `False`. Both checkers decide the condition by the shared `pinned_by_targets` walk: a binder counts only when matching a value against the target recovers it, which `blur(a)` never does, where a syntactic occurrence test would read `a` as forced because it *occurs* in `blur(a)` and let this program print "FORGED".
//
// The two ends of the discrimination are covered alongside: the same declaration with target `(0)` is rejected below, and `(a)` is a genuinely forced binder that must stay accepted. The control's target is the binder itself, which forces it.
#[test]
fn a_non_injective_index_target_does_not_force_its_binder() {
    control(
        r#"
        use /std/{Nat, Eq};
        use /std/Bool/{False};

        let blur(a : Nat) -> Nat = 0;

        induct Loose : (n : Nat) -> pub Prop
        | mk(a : Nat) : (a)
        end

        let extract(p : Loose(0)) -> Nat =
            match p : (m, q) => Nat
            | mk(a) => a
            end;

        let same = Loose/mk(0);

        let boom = extract;

        /std/print(Nat/to_str(extract(Loose/mk(0))))
        "#,
        b"0",
    )
    .refused(&[
        ("mk(a : Nat) : (a)", "mk(a : Nat) : (blur(a))"),
        ("let same = Loose/mk(0);", "let same : Eq()(Loose/mk(0), Loose/mk(7)) = Eq/refl();"),
        ("let boom = extract;", "let boom : False =\n            Eq/subst((n : Nat) => match n : (_) => Type | 0 => {} | _ => False end,\n                     Eq/cong(extract, same),\n                     ());"),
    ])
    .by_elaborator_in("/extract", |error| {
        matches!(error, Error::LargeElimOfProp { .. })
    });
}

// Proof irrelevance and index inversion disagree about a `Prop`-valued index, and the disagreement is a closed inhabitant of `False`. Conversion identifies `Two/a()` with `Two/b()` — any two inhabitants of a proposition are equal — so `Ind(Two/a())` and `Ind(Two/b())` are the same type and `coerce` is well typed. Inversion decides a case is impossible by *syntactic* constructor clash (`invert_indices` decomposes constructor forms and clashes on distinct tags, with no sort condition), so it reads `only`'s target `Two/a()` against the actual index `Two/b()` as disjoint and accepts the arm-less elimination as vacuous — at a type conversion just proved inhabited.
//
// Without the rule that refuses it, this source compiles, the compile path certifies `let /bad : False = boom(coerce(only()))`, and running it traps at the `unreachable` the vacuous elimination emits, which is the runtime witness that the impossibility claim is false. The rule: a clash may only be concluded at a position whose type distinguishes its inhabitants, and the shared walk both checkers reach decides that from the declaration's own `result_sort` (`curios-analysis/src/invert.rs`), deriving nothing at all — no clash, no equations — at a `Prop`-valued position.
//
// The refusal is the coverage rule's: with the clash retracted, `only` is an ordinary reachable constructor, and an elimination with no arm for it is missing one it cannot prove absent. The control's `Two` is relevant, so its constructors clash and the elimination is vacuous.
#[test]
fn a_proposition_valued_index_cannot_make_an_elimination_vacuous() {
    control(
        r#"
        use /std/Bool/{False};

        induct Two : pub Type
        | a()
        | b()
        end

        induct Ind : (x : Two) -> pub Type
        | only() : (Two/a())
        end

        let coerce = Ind/only;

        let boom(w : Ind(Two/b())) -> False =
            match w : (x, q) => False
            end;

        let bad = boom;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("induct Two : pub Type", "induct Two : pub Prop"),
        (
            "let coerce = Ind/only;",
            "let coerce(w : Ind(Two/a())) -> Ind(Two/b()) = w;",
        ),
        (
            "let bad = boom;",
            "let bad : False = boom(coerce(Ind/only()));",
        ),
    ])
    .by_elaborator_in("/boom", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// The same disagreement through coverage rather than vacuity, which is why the fix belongs in the shared walk and not at one of its callers: here the elimination has an arm, and it is the *omitted* one that inversion wrongly excuses. `Ind/right()` inhabits `Ind(Two/a())` by the same conversion, so the match falls through every arm it enumerated. The control's `Two` is relevant, so `right` clashes and its arm is excused.
#[test]
fn a_proposition_valued_index_cannot_excuse_an_omitted_arm() {
    control(
        r#"
        use /std/{Nat};

        induct Two : pub Type
        | a()
        | b()
        end

        induct Ind : (x : Two) -> pub Type
        | left() : (Two/a())
        | right() : (Two/b())
        end

        let coerce = Ind/right;

        let f(w : Ind(Two/a())) -> Nat =
            match w : (x, q) => Nat
            | left() => 0
            end;

        /std/print(Nat/to_str(f(Ind/left())))
        "#,
        b"0",
    )
    .refused(&[
        ("induct Two : pub Type", "induct Two : pub Prop"),
        (
            "let coerce = Ind/right;",
            "let coerce(w : Ind(Two/b())) -> Ind(Two/a()) = w;",
        ),
        ("f(Ind/left())", "f(coerce(Ind/right()))"),
    ])
    .by_elaborator_in("/f", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// The same rule one decomposition step down, which is where its implementation could most plausibly have parted from its statement. `invert_indices` decides the `Prop` condition from the *family of the values being compared* rather than from the declared type of the position, and it decides it wherever the recursion reaches rather than only at the top of one: here `Pair` is relevant and the `Two` it carries is not, so the condition has to fire inside a matching constructor's payload. Read off the index domain instead — `Pair : Type`, therefore relevant — or applied only at `top`, `Two/a()` against `Two/b()` would clash at depth as it would at the surface, and both routes below would be closed inhabitants of `False`.
//
// Both are refused, and `coerce` elaborates in each: conversion identifies the two `Ind` instances through the nested proof, so the premise each exploit needs is available and inversion's refusal to clash on it is the only thing in the way. The probes are recorded so the rung is not re-attacked. The control's `Two` is relevant, so the clash at depth makes the elimination vacuous.
#[test]
fn a_nested_proposition_valued_index_cannot_make_an_elimination_vacuous() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{False};

        induct Two : pub Type
        | a()
        | b()
        end

        induct Pair : pub Type
        | mk(n : Nat, p : Two)
        end

        induct Ind : (x : Pair) -> pub Type
        | only() : (Pair/mk(0, Two/a()))
        end

        let coerce = Ind/only;

        let boom(w : Ind(Pair/mk(0, Two/b()))) -> False =
            match w : (x, q) => False
            end;

        let bad = boom;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("induct Two : pub Type", "induct Two : pub Prop"),
        (
            "let coerce = Ind/only;",
            "let coerce(w : Ind(Pair/mk(0, Two/a()))) -> Ind(Pair/mk(0, Two/b())) = w;",
        ),
        (
            "let bad = boom;",
            "let bad : False = boom(coerce(Ind/only()));",
        ),
    ])
    .by_elaborator_in("/boom", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

#[test]
fn a_nested_proposition_valued_index_cannot_excuse_an_omitted_arm() {
    control(
        r#"
        use /std/{Nat};

        induct Two : pub Type
        | a()
        | b()
        end

        induct Pair : pub Type
        | mk(n : Nat, p : Two)
        end

        induct Ind : (x : Pair) -> pub Type
        | left()  : (Pair/mk(0, Two/a()))
        | right() : (Pair/mk(0, Two/b()))
        end

        let coerce = Ind/right;

        let f(w : Ind(Pair/mk(0, Two/a()))) -> Nat =
            match w : (x, q) => Nat
            | left() => 0
            end;

        /std/print(Nat/to_str(f(Ind/left())))
        "#,
        b"0",
    )
    .refused(&[
        ("induct Two : pub Type", "induct Two : pub Prop"),
        (
            "let coerce = Ind/right;",
            "let coerce(w : Ind(Pair/mk(0, Two/b()))) -> Ind(Pair/mk(0, Two/a())) = w;",
        ),
        ("f(Ind/left())", "f(coerce(Ind/right()))"),
    ])
    .by_elaborator_in("/f", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// Coverage's *accepting* rung: an omitted arm excused because its index target genuinely clashes with the scrutinee's. Every fixture above asserts the refusing direction, so a change that made every absent arm mandatory would break nothing else in this file. `AN_EMPTY_PROPOSITION_STILL_ELIMINATES_INTO_DATA` is not this control: a family with no constructors leaves the coverage loop with nothing to iterate, so it exercises the loop's absence rather than its verdict.
//
// It is deliberately the nested shape, which is what discriminates the two refusals above rather than merely sitting beside them: the clash is at the same depth and differs only in that `Pair/mk(0)` and `Pair/mk(1)` are values a program can tell apart.
#[test]
fn a_nested_relevant_clash_still_excuses_an_omitted_arm() {
    assert_eq!(
        run(A_NESTED_RELEVANT_CLASH_STILL_EXCUSES_AN_OMITTED_ARM),
        b"0"
    );
}

// The conflict rule through a *non-linear* target. `Eq`'s `refl(@z) : (z, z)` mentions its binder twice, so against `Eq()(false, true)` no position clashes on its own: each solves `z`, to `false` and to `true`. The deletion rule reconciles a binder forced twice, and where the two forcings definitely clash the case is unreachable — each was reached by injective steps, so the arm needs `false` to be `true`. Stated over a literal, two constructors, a closed `Nat` pair, an open successor, a parameterized family and an equality the program declares itself, since the rule is the walk's and not `Eq`'s.
#[test]
fn a_clash_between_two_forcings_of_one_binder_excuses_the_arm() {
    assert_eq!(
        run(A_CLASH_BETWEEN_TWO_FORCINGS_OF_ONE_BINDER_EXCUSES_THE_ARM),
        b"ok"
    );
}

// The control the rule above is most dangerous without, and the exploit it would be: `Two/a()` and `Two/b()` are distinct constructors, and they are proofs, so `Eq/refl()` inhabits `Eq()(Two/a(), Two/b())` by irrelevance. Reading the two forcings as a clash would make `absurd` total on an inhabited type, and `forged` a closed inhabitant of `False`. Two things stand in the way and either suffices: the forcings *convert* at `Two`, so the deletion rule keeps one before a clash is ever asked for; and the walk that would answer the clash reads the family's sort before its tags.
//
// The two are independent rather than one guard stated twice, as mutation at the unifier itself shows, in `curios-analysis`'s `tests/driven/forcing_tests.rs`: with the sort test removed the proposition's verdict does not move, conversion answering first; with the clash asked before conversion it does not move either, the sort test answering; with both gone the case is reported unreachable and `a_binder_forced_twice_survives_only_when_its_forcings_convert` fails on it. The control's `Two` is relevant, so the two forcings clash and the arm is excused.
#[test]
fn two_proofs_forced_on_one_binder_do_not_clash() {
    control(
        r#"
        use /std/Bool/{False};

        induct Same(@P: Type): (P, P) -> pub Prop | same(@p: P): (p, p) end

        induct Two : pub Type
        | a()
        | b()
        end

        let absurd(h : Same()(Two/a(), Two/b())) -> False =
            match h end;

        let forged = absurd;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("Same(@P: Type)", "Same(@P: Prop)"),
        ("induct Two : pub Type", "induct Two : pub Prop"),
        (
            "let forged = absurd;",
            "let forged : False = absurd(Same/same());",
        ),
    ])
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// The same conflict met from the scrutinee's side. `mk()` targets `(0, 1)` and the scrutinee's indices are `(n, n)`, one variable twice, so no position clashes on its own and the unifier's first direction, which reads a variable as what may yet be the target, decides nothing. Its second direction solves `n` to each target, and the two forcings clash as two forcings of one binder do: no value of `Two(n, n)` is `mk()`, whatever `n` is. Both checkers ask both directions of an absent arm, by the call a written arm is specialized by, so an arm may be left out exactly where the kernel would type none of it — a goal the case would refine among them, where a written arm has no body, and a clash that stands behind a position the first direction gave up on.
#[test]
fn a_variable_of_the_indices_forced_two_ways_excuses_the_arm() {
    assert_eq!(
        run(A_VARIABLE_OF_THE_INDICES_FORCED_TWO_WAYS_EXCUSES_THE_ARM),
        b"ok"
    );
}

// The control that direction is most dangerous without, and the exploit it would be. `Two/a()` and `Two/b()` are proofs, so `Apart/apart()` inhabits `Apart(x, x)` for every `x` by irrelevance; reading the two forcings of `x` as a clash would make `absurd` total on an inhabited type and `forged` a closed inhabitant of `False`. The forcings convert at `Two`, so the deletion rule keeps one before a clash is asked for, and the walk that would answer the clash reads the family's sort before its tags. The second fixture puts the proofs one constructor down, under a relevant `Wrap`, where the two forcings convert through their payloads. Each control's `Two` is relevant, so the two forcings clash and the arm is excused.
#[test]
fn two_proofs_forced_on_one_variable_of_the_indices_do_not_clash() {
    control(
        r#"
        use /std/Bool/{False};

        induct Two : pub Type
        | a()
        | b()
        end

        induct Apart : (x : Two, y : Two) -> pub Type
        | apart() : (Two/a(), Two/b())
        end

        let absurd(x : Two, h : Apart(x, x)) -> False =
            match h end;

        let forged = absurd;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("induct Two : pub Type", "induct Two : pub Prop"),
        (
            "let forged = absurd;",
            "let forged : False = absurd(Two/a(), Apart/apart());",
        ),
    ])
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
    control(
        r#"
        use /std/Bool/{False};

        induct Two : pub Type
        | a()
        | b()
        end

        induct Wrap : pub Type
        | wrap(p : Two)
        end

        induct Apart : (x : Wrap, y : Wrap) -> pub Type
        | apart() : (Wrap/wrap(Two/a()), Wrap/wrap(Two/b()))
        end

        let absurd(w : Wrap, h : Apart(w, w)) -> False =
            match h end;

        let forged = absurd;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("induct Two : pub Type", "induct Two : pub Prop"),
        (
            "let forged = absurd;",
            "let forged : False = absurd(Wrap/wrap(Two/a()), Apart/apart());",
        ),
    ])
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// A variable forced twice to one value is forced once. `Two(m, m + 1)` against `(0, 1)` solves `m` to `0` from either position, so the case is reachable, at `m` zero, and its arm stays mandatory: excused, `absurd(0, Two/mk())` would be a closed inhabitant of `False`. The control forces `m` to `0` and to minus one, which clash.
#[test]
fn a_variable_of_the_indices_forced_to_one_value_twice_does_not_clash() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{False};

        induct Two : (a : Nat, b : Nat) -> Type
        | mk() : (0, 1)
        end

        let absurd(m : Nat, x : Two(m, m + 2)) -> False =
            match x end;

        let forged = absurd;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("Two(m, m + 2)", "Two(m, m + 1)"),
        (
            "let forged = absurd;",
            "let forged : False = absurd(0, Two/mk());",
        ),
    ])
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// Short of a definite clash the pair is dropped and the arm stays mandatory. A variable may yet be `red()`. The control's two colours are both closed, and clash.
#[test]
fn an_open_forcing_does_not_clash() {
    control(
        r#"
        use /std/{Eq};
        use /std/Bool/{False};

        induct Color : pub Type
        | red()
        | green()
        end

        let absurd(c : Color, h : Eq()(Color/green(), Color/red())) -> False =
            match h end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Eq()(Color/green(), Color/red())", "Eq()(c, Color/red())")])
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// An opaque function may send `0` and `1` to one value, so its two applications neither convert nor clash. The control's successor of an application clashes with zero.
#[test]
fn two_applications_of_one_opaque_function_do_not_clash() {
    control(
        r#"
        use /std/{Eq, Nat};
        use /std/Bool/{False};

        let absurd(f : (Nat) -> Nat, h : Eq()(f(0) + 1, 0)) -> False =
            match h end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Eq()(f(0) + 1, 0)", "Eq()(f(0), f(1))")])
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// `x * 2 + 1` is never `y * 2`, and the fold says so: a comparison of the two reduces to `false` by the gcd of their coefficients. That verdict is deliberately not the peel's `Clash`, so inversion does not read it as impossibility — `documentation/design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md` defers it as a row of its own — and this rule must not be the way it arrives. The control's successor clashes with zero.
#[test]
fn a_parity_disagreement_is_not_a_clash() {
    control(
        r#"
        use /std/{Eq, Nat};
        use /std/Bool/{False};

        let absurd(x : Nat, y : Nat, h : Eq()(x * 2 + 1, 0)) -> False =
            match h end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Eq()(x * 2 + 1, 0)", "Eq()(x * 2 + 1, y * 2)")])
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// The cancellation's own clash, at the arm whose guard is the whole of its soundness. ℕ is cancellative, so the peel both checkers read concludes impossibility only where one side cancels away and the other keeps a positive *constant*; a truncated difference is an opaque summand, and `a - 1` is `0` at `a` zero and at `a` one. Read as a positive floor — the summand answering where the constant is asked about — the arm would be excused and `absurd(0, Eq/refl())` would be a closed inhabitant of `False`. The control's successor clashes with zero.
#[test]
fn a_truncated_difference_does_not_clash_with_zero() {
    control(
        r#"
        use /std/{Eq, Nat};
        use /std/Bool/{False};

        let absurd(a : Nat, h : Eq()(a + 1, 0)) -> False =
            match h end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Eq()(a + 1, 0)", "Eq()(a - 1, 0)")])
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// The one place the two numeric carriers' cancellations must part. ℕ's clash rests on every summand being at least zero, so a positive constant facing an emptied side is impossible — which is `a_clash_between_two_forcings_of_one_binder_excuses_the_arm`'s `absurd_successor`, and must stay accepted. An `Int` summand may be negative, so `i + +1` is `+0` at `i` minus one and ℤ's rule concludes impossibility only between two constants. ℕ's rule read at ℤ would excuse this arm and prove `False` at `absurd(-1, Eq/refl())`. The control's two `Int` constants clash.
#[test]
fn an_int_successor_does_not_clash_with_zero() {
    control(
        r#"
        use /std/{Eq, Int};
        use /std/Bool/{False};

        let absurd(i : Int, h : Eq()(+1, +0)) -> False =
            match h end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Eq()(+1, +0)", "Eq()(i + +1, +0)")])
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// The same question at a sequence carrier, where the strip decides a residual against the empty value by whether anything left in it has a positive length. A window has none to state: `Bytes/slice(b, 0, n)` is the empty value at `n` zero, so a run or a single element clashes with `x[]` and a window does not. Counted positive, the arm would be excused and `absurd(x[], 0, Eq/refl())` would prove `False`. The control's window is followed by a byte, a positive segment that clashes with `x[]`.
#[test]
fn a_window_that_may_be_empty_does_not_clash_with_the_empty_value() {
    control(
        r#"
        use /std/{Eq, Nat, Bytes};
        use /std/Bool/{False, Holds};

        let absurd(
            b : Bytes,
            n : Nat,
            @ok : Holds(0 + n <= Bytes/len(b)),
            h : Eq()(x[..Bytes/slice(b, 0, n), 0x01], x[]),
        ) -> False =
            match h end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "Eq()(x[..Bytes/slice(b, 0, n), 0x01], x[])",
        "Eq()(Bytes/slice(b, 0, n), x[])",
    )])
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// The strip where a chunk faces a run: `x[..b, 0x01]` opens with an opaque operand and `x[0x01, ..c]` with a byte, so the two share no certain prefix and the pair is handed on rather than clashed. They are one value wherever `b` and `c` are both empty, which is what makes a clash here a `False` rather than imprecision. The control's two values each open with a byte, and the bytes differ.
#[test]
fn a_value_extended_at_one_end_does_not_clash_with_one_extended_at_the_other() {
    control(
        r#"
        use /std/{Eq, Bytes};
        use /std/Bool/{False};

        let absurd(b : Bytes, c : Bytes, h : Eq()(x[0x02, ..b], x[0x01, ..c])) -> False =
            match h end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("x[0x02, ..b]", "x[..b, 0x01]")])
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// The strip's accepting rung, as `an_unmentioned_payload_binder_is_not_forced` is the pinning rule's: a byte appended onto an opaque tail has a positive length wherever that tail stands, so it does clash with the empty value and the arm is excused. Without it a strip that read every sequence residual as undecided would satisfy the two refusals above and nothing else here would notice.
#[test]
fn a_positive_segment_still_clashes_with_the_empty_value() {
    assert_eq!(
        run(A_POSITIVE_SEGMENT_STILL_CLASHES_WITH_THE_EMPTY_VALUE),
        b"ok"
    );
}

// The lower end of that discrimination: drop `a` from the index target and the guard fires. Without this, a fix could "close" the hole above by rejecting every indexed proposition and nothing here would notice. The control's target is the binder itself, which forces it.
#[test]
fn an_unmentioned_payload_binder_is_not_forced() {
    control(
        r#"
        use /std/{Nat};

        induct Tight : (n : Nat) -> pub Prop
        | mk(a : Nat) : (a)
        end

        let extract(p : Tight(7)) -> Nat =
            match p : (m, q) => Nat
            | mk(a) => a
            end;

        /std/print(Nat/to_str(extract(Tight/mk(7))))
        "#,
        b"7",
    )
    .refused(&[
        ("mk(a : Nat) : (a)", "mk(a : Nat) : (0)"),
        ("extract(p : Tight(7))", "extract(p : Tight(0))"),
    ])
    .by_elaborator_in("/extract", |error| {
        matches!(error, Error::LargeElimOfProp { .. })
    });
}

// The singleton rung's side condition at its other half: a payload that is *itself a type*. `mk(A : Type)` pins nothing, so eliminating a `Box` recovers the type it was built with while irrelevance says every `Box` is the same one — `Eq/cong` then equates any two types and `Eq/subst` transports `0` into `False`.
//
// The elaborator refuses at `unbox`, so the items after it never elaborate; they document the route rather than being checked. The certifier is where this rule needs backing up: `carries_information` counts a component whose type is a universe as carrying information, since erasure deleting a type either way is what the runtime observes, not what conversion observes.
//
// No surface program reaches that gate. The executable guarantee therefore lives beside the rule: `curios_cert::recheck::tests::a_derivation_through_a_type_carrying_proposition_is_refused` holds the whole derivation shut, and the two singleton fixtures in `curios_cert::kernel::infer::eliminate::large_elimination_tests` pin the predicate at both halves of the clause that admitted it. The control's `Box` is relevant, and the same derivation at one type stands.
#[test]
fn a_singleton_carrying_a_type_does_not_eliminate_into_a_type() {
    control(
        r#"
        use /std/{Eq, Nat};
        use /std/Bool/{False};

        induct Box : pub Type
        | mk(A : Type)
        end

        let unbox(b : Box) -> Type =
            match b : (_) => Type
            | mk(A) => A
            end;

        let boxes_equal(A : Type) -> Eq()(Box/mk(A), Box/mk(A)) = Eq/refl();

        let types_equal(A : Type) -> Eq()(A, A) =
            Eq/cong(unbox, boxes_equal(A));

        let kept : Nat =
            Eq/subst((t : Type) => t, types_equal(Nat), 0);

        /std/print(Nat/to_str(kept))
        "#,
        b"0",
    )
    .refused(&[
        ("induct Box : pub Type", "induct Box : pub Prop"),
        (
            "boxes_equal(A : Type) -> Eq()(Box/mk(A), Box/mk(A))",
            "boxes_equal(A : Type, B : Type) -> Eq()(Box/mk(A), Box/mk(B))",
        ),
        (
            "types_equal(A : Type) -> Eq()(A, A)",
            "types_equal(A : Type, B : Type) -> Eq()(A, B)",
        ),
        ("boxes_equal(A))", "boxes_equal(A, B))"),
        ("let kept : Nat", "let bad : False"),
        ("types_equal(Nat)", "types_equal(Nat, False)"),
        ("Nat/to_str(kept)", "\"FORGED\""),
    ])
    .by_elaborator_in("/unbox", |error| {
        matches!(error, Error::LargeElimOfProp { .. })
    });
}

// An arm learns an outer variable from inside an index, as the kernel's arm rule does. `one()` targets `1` and the scrutinee's actual index is `n + 1`, so inside the arm `n` is `0`, and `Eq/refl()` inhabits `Eq()(n, 0)`. Both checkers solve an arm's index equations with the shared `curios_analysis::solve_indices`, which reaches the `n` inside `n + 1` where binding an index only when it is itself a variable would not. The refusal is the same arm claiming `Eq()(n, 1)`, which learning `n := 0` must refuse, its goal stated as the arm sees it, `Eq()(0, 1)`.
#[test]
fn an_arm_learns_an_outer_variable_inside_an_index() {
    control(
        r#"
        use /std/{Nat, Eq};

        induct One : (k : Nat) -> Type
        | one() : (1)
        end

        let zero_below(n : Nat, x : One(n + 1)) -> Eq()(n, 0) =
            match x | one() => Eq/refl() end;

        let _ = zero_below(0, One/one());
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("let zero_below(n : Nat, x : One(n + 1)) -> Eq()(n, 0) =", "let one_below(n : Nat, x : One(n + 1)) -> Eq()(n, 1) ="),
        ("        let _ = zero_below(0, One/one());\n", ""),
    ])
    .by_elaborator_in(
        "/one_below",
        |error| matches!(error, Error::TypeMismatch { expected, .. } if pinned(expected) == "/std/Eq/Eq(Nat)(0, 1)"),
    );
}

// An arm whose index equations clash re-types no local, in either checker. `mk()` targets `(0, 1)` and the scrutinee's indices are `(n, n)`, so the case is unreachable and its written arm is checked as written: its goal is the ambient `Eq()(n, 7)` taken at the case, `Eq()(0, 7)`, and `h` keeps its type `Eq()(n, 7)`. Both re-type by the shared `curios_analysis::retyped` over the solution the case actually has, which a clash leaves empty, so the refusal is the elaborator's; re-typing by the goal's own substitution would re-type `h` to `Eq()(0, 7)`, accept the arm, and hand the kernel a program it refuses.
#[test]
fn an_unreachable_arm_retypes_no_local() {
    control(
        r#"
        use /std/{Nat, Eq};

        induct Two : (a : Nat, b : Nat) -> Type
        | mk() : (0, 1)
        end

        let f(n : Nat, h : Eq()(n, 7), x : Two(n, n + 1)) -> Eq()(n, 7) =
            match x | mk() => h end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("Two(n, n + 1)", "Two(n, n)"),
    ])
    .by_elaborator_in(
        "/f",
        |error| matches!(error, Error::TypeMismatch { expected, .. } if pinned(expected) == "/std/Eq/Eq(Nat)(0, 7)"),
    );
}

// Inversion takes an equation only where it is deduced, and a factor both indices share leaves the others' equation unknown: `x * a` against `x * b` holds at `x = 0` whatever `a` and `b` are, so reading `a = b` off it would prove `Eq()(0, 1)`. The control inverts an equation of the operands themselves, which forces it.
#[test]
fn a_factor_both_indices_share_forces_no_equation_of_the_others() {
    control(
        r#"
        use /std/{Nat, Eq};

        let cancel(x: Nat, a: Nat, b: Nat, p: Eq()(a, b)) -> Eq()(a, b) =
            match p | refl() => Eq/refl() end;

        /std/print(Nat/to_str(0))
        "#,
        b"0",
    )
    .refused(&[("p: Eq()(a, b))", "p: Eq()(x * a, x * b))")])
    .by_elaborator_in("/cancel", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

// A rounded sum cancels nothing: `1.0 + 1.0e300` and `2.0 + 1.0e300` round to one float, so a summand two float sums share leaves the others' equation unknown, and reading `a = c` off it would prove `Eq()(1.0, 2.0)`. Pairing the shared summand is conversion's sufficient condition, which inversion never reads; nor is a fused multiply-add's, whose two values at a zero factor are the addend whatever the factors beside it are. Each control inverts an equation of the operands themselves, which forces it.
#[test]
fn an_operand_two_float_operations_share_forces_no_equation_of_the_others() {
    control(
        r#"
        use /std/{Flt, Eq};

        let cancel(a: Flt, b: Flt, c: Flt, p: Eq()(a, c)) -> Eq()(a, c) =
            match p | refl() => Eq/refl() end;

        let _forged: Eq()(1.0, 1.0) = cancel(1.0, 1.0e300, 1.0, Eq/refl());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("p: Eq()(a, c))", "p: Eq()(a + b, c + b))"),
        ("let _forged: Eq()(1.0, 1.0) = cancel(1.0, 1.0e300, 1.0, Eq/refl());", "let _forged: Eq()(1.0, 2.0) = cancel(1.0, 1.0e300, 2.0, Eq/refl());"),
    ])
    .by_elaborator_in(
        "/cancel",
        |error| matches!(error, Error::TypeMismatch { expected, .. } if pinned(expected) == "/std/Eq/Eq(Flt)(a, c)"),
    );
    control(
        r#"
        use /std/{Flt, Eq};

        let cancel(a: Flt, b: Flt, c: Flt, d: Flt, p: Eq()(b, d)) -> Eq()(b, d) =
            match p | refl() => Eq/refl() end;

        let _forged: Eq()(1.0, 1.0) = cancel(0.0, 1.0, 3.0, 1.0, Eq/refl());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("p: Eq()(b, d))", "p: Eq()(Flt/fma(a, b, c), Flt/fma(a, d, c)))"),
        ("let _forged: Eq()(1.0, 1.0) = cancel(0.0, 1.0, 3.0, 1.0, Eq/refl());", "let _forged: Eq()(1.0, 2.0) = cancel(0.0, 1.0, 3.0, 2.0, Eq/refl());"),
    ])
    .by_elaborator_in(
        "/cancel",
        |error| matches!(error, Error::TypeMismatch { expected, .. } if pinned(expected) == "/std/Eq/Eq(Flt)(b, d)"),
    );
}
