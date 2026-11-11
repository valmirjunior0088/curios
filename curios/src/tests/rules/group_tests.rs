//! A recursive group's members as its signatures and bodies read them: each by value once its turn finds it known to terminate, and standing for nothing until then, both checkers alike.
//!
//! The rule is `curios-cert`'s README, *A member is read by value once it is known to terminate*. The certifier's half is `curios_cert`'s `recheck::recursion_tests`, under the same names.

use {super::test_support::*, crate::tests::test_support::*, curios_elab::Error};

// `Flip` calls nothing, so its turn finds it known, and `claimed`, beside it in a local group, states `Flip(false)` in its own body, which is the `True` the control states as `stated`'s type, where its body is checked. Both run.
#[test]
fn a_member_known_to_terminate_is_read_by_value_in_its_groups_bodies() {
    let read_in_a_body = r#"
        use /std/{Bool, print};
        use /std/Bool/{True, False, Holds};

        pub let outer(_: Bool) -> Holds(true) =
            let Flip(b: Bool) -> Prop = match b | false => True | true => False end
            and claimed: Holds(true) =
                let _: Flip(false) = True/qed();
                True/qed();
            claimed;

        print("ok")
        "#;
    assert_eq!(run(read_in_a_body), b"ok");

    let stated_in_a_signature = r#"
        use /std/{Bool, print};
        use /std/Bool/{True, False, Holds};

        pub let outer(_: Bool) -> Holds(true) =
            let Flip(b: Bool) -> Prop = match b | false => True | true => False end
            and stated: Flip(false) = True/qed();
            let _ = stated;
            True/qed();

        print("ok")
        "#;
    assert_eq!(run(stated_in_a_signature), b"ok");
}

// `b` and `c` are values, so their group owes no descent, and `b` calls nothing, so its turn finds it known: `c`'s body reads `Holds(b)` as the `True` `qed` inhabits.
#[test]
fn a_member_of_a_group_that_owes_no_descent_is_read_once_known_to_terminate() {
    let source = r#"
        use /std/{Bool, print};
        use /std/Bool/{True, Holds};

        pub let outer(_: Bool) -> Bool =
            let b: Bool = true
            and c: Bool =
                let _: Holds(b) = True/qed();
                false;
            c;

        print("ok")
        "#;
    assert_eq!(run(source), b"ok");
}

// `b(n)` calls itself at `n`, nothing below it, so its turn withholds it, and `c`'s body reads `Holds(b(0))` over a `b` that stands for nothing, though `b(0)` is `true`: an equation of a member that need not terminate is no fact. The control's `b` calls itself at the predecessor, so its turn finds it known.
#[test]
fn a_member_on_a_cycle_that_does_not_descend_stands_for_nothing_in_its_bodies() {
    control(
        r#"
        use /std/{Bool, Nat, print};
        use /std/Bool/{True, Holds};

        pub let outer(_: Nat) -> Bool =
            let b(n: Nat) -> Bool = match n | 0 => true | k + 1 => b(k) end
            and c: Bool =
                let _: Holds(b(0)) = True/qed();
                false;
            c;

        print("")
        "#,
        b"",
    )
    .refused(&[
        ("| k + 1 => b(k) end", "| _ + 1 => b(n) end"),
    ])
    .by_elaborator_in("/outer", |error| {
        matches!(error, Error::TypeMismatch { inferred, expected, .. } if pinned(inferred) == "/sys/Bool/True" && pinned(expected) == "/sys/Bool/Holds(b(0))")
    });
}

// `b` calls itself with nothing below, so its turn withholds it and nothing unfolds it: the group, whose `b` is a type, is named for not descending before `c`'s body would unfold `b` into a list of itself without end. The control declares `b` apart, a list of `Nat`.
#[test]
fn a_group_listing_itself_is_refused_for_not_descending() {
    control(
        r#"
        use /std/{List, Nat, print};

        pub let outer(n: Nat) -> Nat =
            let b: Type = List(Nat);
            let c: b = [];
            n;

        print("")
        "#,
        b"",
    )
    .refused(&[(
        "let b: Type = List(Nat);\n            let c: b = [];",
        "let b: Type = List(b)\n            and c: b = [];",
    )])
    .by_elaborator_in("/outer", |error| {
        matches!(error, Error::PartialInErasedPosition { .. })
    });
}

// `f` and `g` each read the other's value at an argument below their own, so each member is known at its turn, `f` before `g`'s body is reached: `f`'s reading of `g` waits on `g`'s slot until `g`'s turn finds the two descend. The certifier takes every turn before typing a body, and the run puts the program to it whole, so this one has no Core text twin.
#[test]
fn two_members_reading_each_other_below_their_arguments_stand() {
    let source = r#"
        use /std/{Nat, print};
        use /std/Bool/{True, Holds};

        pub let outer(n: Nat) -> Nat =
            let T(m: Nat) -> Type = match m | 0 => Nat | k + 1 => T(k) end
            and f(m: Nat) -> {Nat, Nat} = (m, match m | 0 => 0 | k + 1 => let _: Holds(g(k).0 == k) = True/qed(); g(k).1 end)
            and g(m: Nat) -> {Nat, Nat} = (m, match m | 0 => 0 | k + 1 => let _: Holds(f(k).0 == k) = True/qed(); f(k).1 end);
            f(n).1 + 42;

        print(Nat/to_str(outer(3)))
        "#;
    assert_eq!(run(source), b"42");
}
