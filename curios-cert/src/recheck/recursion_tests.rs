//! A recursive member is certified only with its group.

use {
    super::test_support::*,
    crate::{Error, Globals},
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Free, Global, Intrinsic, Level, Many, Module, Nat, RecGroup, RecMemberScopes, Scope, Term,
    },
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

/// `let bad : Nat = <body>`, the one item of a module built by hand.
fn bad_module(body: Term) -> Module {
    Module {
        items: vec![authored_partial(
            &Global::Authored(Qualifier::from(["bad"])),
            Term::intrinsic(Intrinsic::NatType),
            body,
        )]
        .into(),
        ..Module::default()
    }
}

/// A group whose member scopes bind more than its members is refused, not aborted on.
///
/// `RecGroup::new` takes its members without relating their scopes' arity to their number, and `member_type` and `member_body` open each scope over one term per member, so without the boundary check `Scope::open`'s assert is the first thing that reads either count. A `rec` group is a Core term a module built by hand carries, and the board requires the implementation to fail closed.
#[test]
fn a_group_whose_member_scopes_bind_more_than_its_members_is_refused() {
    let (f, g) = (member(), Free::local(905, Some("g")));
    let group = RecGroup::new(vec![RecMemberScopes {
        type_: Scope::close(Many(2), &[&f, &g], Term::intrinsic(Intrinsic::NatType)),
        body: Scope::close(Many(2), &[&f, &g], Term::free_var(&f)),
    }]);
    let module = bad_module(Term::rec_proj(group, 0));

    assert!(
        !fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX).is_empty(),
        "the walk must refuse a group whose member scopes and members disagree in number",
    );
}

/// A projection past a tuple's components is refused, not aborted on.
///
/// This is the shape the README names outright, and the count it is refused by at the boundary is `Counted::Components`. It is the control the two counts above are read against: one count of a hand-built term was checked before this commit and two were not.
#[test]
fn a_projection_past_a_tuples_components_is_refused() {
    let module = bad_module(Term::proj(
        Term::tuple([
            Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
        ]),
        7,
    ));

    assert!(
        !fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX).is_empty(),
        "the walk must refuse a projection past the components of what it projects from",
    );
}

/// `held : (y : (Type 1) -> Type 1) -> Type 1 = (y) => rec m : Type 1 = y(m) end.0`: the one shape whose projection survives forcing with its declared type a sort.
///
/// `Sort::of` reads a member's declared type only through `sort_of_neutral`, which `reduce_forced` reaches only for a projection `force` left standing — and `force` keeps the folded spelling exactly where the member's body reduces to a neutral *mentioning the group*, which is what the opaque head `y` makes of `y(m)`. The mention is the group's only call, recorded with no arguments and graded unknown, so the group does not descend; and the member's declared type yields a sort, so `check_group`'s local gate is owed a descent. The two halves are the same self-mention, which is why nothing escapes both: `yields_a_sort` peels arrows and projects tuple fields, so an applied or a projected member is inside the gate too.
fn surviving_projection_module() -> Module {
    let sort = Term::type_at(Level::zero().succ().expect("a level has a successor"));
    let opaque = Free::local(950, Some("y"));
    let member = Free::local(951, Some("m"));
    let members = [&member];
    let group = Term::rec_proj(
        RecGroup::new(vec![RecMemberScopes {
            type_: Scope::close(Many(1), &members, sort.clone()),
            body: Scope::close(
                Many(1),
                &members,
                Term::apply(Term::free_var(&opaque), [Term::free_var(&member)]),
            ),
        }]),
        0,
    );
    let opaque_type = Term::func_type([(Free::local(952, Some("t")), sort.clone())], sort.clone());

    Module {
        items: vec![authored(
            &Global::Authored(Qualifier::from(["held"])),
            Term::func_type([(opaque, opaque_type.clone())], sort),
            Term::func([(opaque, opaque_type)], group),
        )]
        .into(),
        ..Module::default()
    }
}

/// A member whose declared type is a sort cannot both keep its folded spelling under forcing and descend.
#[test]
fn a_member_whose_projection_survives_forcing_is_owed_a_descent() {
    let verdicts = fixture_verdicts(
        &surviving_projection_module(),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::NotDescending { .. })),
        "a group whose member names itself where erasure deletes it was admitted: {verdicts:?}",
    );
}
