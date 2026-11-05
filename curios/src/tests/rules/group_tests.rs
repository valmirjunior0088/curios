//! A recursive group's members as its bodies read them: standing for nothing, so no body reads a value of its group, where a signature reads the group folded.
//!
//! The rule is `curios-cert`'s README, *A `let` is bound for typing and read by value*. The kernel's half is `curios_cert`'s `recheck::recursion_tests::a_sibling_stands_for_nothing_in_its_groups_bodies`, under the same name.

use {super::test_support::*, crate::test_support::*, curios_elab::Error};

// `claimed`, beside `Flip` in a local group, states `Flip(false)` in its own body, where the members stand for nothing, so it is no `True` for `qed` to inhabit. The control states the same proof as `stated`'s type, where the signature reads its group folded, and runs.
#[test]
fn a_sibling_stands_for_nothing_in_its_groups_bodies() {
    let read_in_a_body = r#"
        use /std/{Bool, print};
        use /std/Bool/{True, False, Holds};

        pub let outer(_: Bool) -> Holds(true) =
            let Flip(b: Bool) -> Prop = match b | false => True | true => False end
            and claimed: Holds(true) =
                let _: Flip(false) = True/qed();
                True/qed();
            claimed;

        print("")
        "#;
    refused(read_in_a_body).by_elaborator_in("/outer", |error| {
        matches!(error, Error::TypeMismatch { inferred, expected, .. } if pinned(inferred) == "/sys/Bool/True" && pinned(expected) == "Flip(false)")
    });

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
