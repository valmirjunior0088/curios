//! A nominal declaration's item as its entries make it: one cycle of what its members' entries reach, as a group of definitions is one cycle of what their terms reach.
//!
//! The rule is `documentation/language.md`'s *Recursive groups*: a top-level group whose members do not all reach one another is refused, naming the parts it falls into.

use {super::test_support::*, curios_elab::Error};

// `A` and `B` hold each other through their payloads, so they are one group; the refusal drops `B`'s reach of `A`, and the two fall apart, each to be declared alone.
#[test]
fn a_nominal_group_whose_members_do_not_all_reach_one_another_is_refused() {
    control(
        r#"
        induct A: pub Type
        | a(b: B)
        | none()
        and B: pub Type
        | b(a: A)
        | nothing()
        end

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| b(a: A)\n", "")])
    .by_elaborator_in("/A", |error| matches!(error, Error::GroupApart { .. }));
}
