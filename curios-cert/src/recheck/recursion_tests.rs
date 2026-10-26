//! A recursive member is certified only with its group.

use {
    super::test_support::*,
    crate::{Error, Globals},
    curios_analysis::test_support::SYNTAX,
    curios_core::{Free, Global, Intrinsic, Many, Module, RecGroup, RecMemberScopes, Scope, Term},
    curios_utilities::Qualifier,
    std::collections::BTreeSet,
};

/// `rec f : Absurd = f` reached as a member selection, which must be refused by the same rule the block spelling is refused by.
///
/// A `rec` group is checked by [`infer`](crate::infer)'s `Rec` arm: every member's declared type is verified to be a type, every body is checked against it, and a group with a proof-typed or type-yielding member must descend, since erasure deletes such a member wholesale and a non-descending one proves anything. A selection is that same node with a tail that picks one member ([`Term::rec_proj`]), so it reaches that arm and is held to that rule.
///
/// A selection with a node of its own — carrying its own copy of the group, well-formed standing alone and so gated by no scope — would need an arm of its own, and one answering `group.member_type(index)` outright would check nothing. The second module below is the sharper case: its group never recurses at all, so no totality rule is in play, and what would go unchecked is simply whether the body inhabits the type the group claims for it. `Absurd` is a proposition with no constructors, so either module would be a closed inhabitant of one.
///
/// (V) seeds from the kernel's own typing, and one node for both spellings is one node for that walk to recognize as a recursion, where a second node would present its member scopes as a bound variable.
///
/// Reachable from no surface program — `curios-elab` builds a checked node — which is why this belongs here rather than in `curios/src/tests`, and why nothing in the corpus could have found it.
///
/// The control is the same construction at a *legal* group, and it is not decoration. General recursion at a relevant type is the language's design, and the group is self-referential, so a rule that certified the group at every selection *by opening its bodies over the group itself* would re-enter the check it is already inside and never terminate. It must stay accepted, and it must stay accepted quickly.
#[test]
fn a_recursive_member_is_certified_only_with_its_group() {
    for (label, body, refusal) in [
        (
            "a non-descending proof",
            Term::free_var(&member()),
            "does not descend",
        ),
        (
            "a body that is not of the declared type",
            Term::tuple(Vec::<Term>::new()),
            "expected",
        ),
    ] {
        let verdicts = fixture_verdicts(
            &selection_module(body),
            1_000_000,
            &Globals::default(),
            SYNTAX,
        );

        assert!(
            verdicts
                .iter()
                .any(|verdict| verdict.error.to_string().contains(refusal)),
            "{label}: the kernel certified a closed inhabitant of an empty proposition: {verdicts:?}",
        );
    }
}

/// The control: a legal group reached through the same selection stays accepted.
///
/// `rec f : (Nat) -> Nat = (n) => f(n)` does not descend, and that is legal — a program that loops is a program rather than an unsoundness, and the obligations bite only where erasure deletes. Refusing every member selection would close the witness above and take this with it.
#[test]
fn a_member_of_a_legal_group_is_still_accepted() {
    let f = member();
    let n = Free::local(901, Some("n"));
    let nat = Term::intrinsic(Intrinsic::NatType);

    let group = RecGroup::new(vec![RecMemberScopes {
        type_: Scope::close(
            Many(1),
            &[&f],
            Term::func_type([(n, nat.clone())], nat.clone()),
        ),
        body: Scope::close(
            Many(1),
            &[&f],
            Term::func(
                [(n, nat.clone())],
                Term::apply(Term::free_var(&f), [Term::free_var(&n)]),
            ),
        ),
    }]);

    let module = Module {
        mounts: Vec::new(),
        items: vec![authored_partial(
            &Global::Authored(Qualifier::from(["ok"])),
            Term::func_type([(n, nat.clone())], nat),
            Term::rec_proj(group, 0),
        )]
        .into(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    };

    assert_eq!(
        fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX),
        Vec::new(),
        "general recursion at a relevant type is legal however it is spelled",
    );
}

/// A member handed bare to an intrinsic operation is a call of it: `rec f : Absurd = fold([], f, (x, acc) => acc)` is `f` by one reduction, the empty list's fold handing back its initial value, so the group recurs on nothing. The fold applies no argument to the member, and the occurrence is held to descend as a selection's is.
#[test]
fn a_member_handed_bare_to_a_list_fold_does_not_descend() {
    let nat = Term::intrinsic(Intrinsic::NatType);
    let absurd = Term::induct_type(
        Global::Authored(Qualifier::from(["Absurd"])),
        Vec::<Term>::new(),
        Vec::<Term>::new(),
    );
    let (x, acc) = (Free::local(902, Some("x")), Free::local(903, Some("acc")));
    let handed_back = Term::intrinsic(Intrinsic::ListFold {
        element: nat.clone(),
        result: absurd.clone(),
        list: Term::intrinsic(Intrinsic::List {
            element: nat.clone(),
            items: Vec::new(),
        }),
        init: Term::free_var(&member()),
        function: Term::func([(x, nat), (acc, absurd)], Term::free_var(&acc)),
    });

    let verdicts = fixture_verdicts(
        &selection_module(handed_back),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );
    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::NotDescending { .. })),
        "the kernel certified a closed inhabitant of an empty proposition: {verdicts:?}",
    );
}
