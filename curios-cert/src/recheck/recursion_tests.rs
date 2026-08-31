//! A recursive member is certified only with its group.

//! What the walk derives for itself rather than reading off the module.
//!
//! It also holds the hand-built adversarial modules. A refusal the elaborator reaches first leaves no module behind, so a rule where `curios-elab` is the stricter of the two cannot be put to this crate by any surface program — `Expect::NotAsked` in `curios/src/tests/perimeter.rs` records exactly that gap. Reaching it means constructing the finished module here and asking `recheck_module_verdicts` directly.

use {
    crate::Globals,
    curios_core::{
        Entrypoint, Free, Global, Intrinsic, Many, Module, RecGroup, RecMemberScopes, Scope, Term,
    },
    curios_utilities::Qualifier,
    std::collections::{BTreeMap, BTreeSet},
};

use super::test_support::*;

/// `rec f : Absurd = f` reached as a member selection, which must be refused by the same rule the block spelling is refused by.
///
/// A `rec` group is checked by [`infer`](crate::infer)'s `Rec` arm: every member's declared type is verified to be a type, every body is checked against it, and a group with a proof-typed or type-yielding member must descend, since erasure deletes such a member wholesale and a non-descending one proves anything. A selection is that same node with a tail that picks one member ([`Term::rec_proj`]), so it reaches that arm and is held to that rule.
///
/// It did not always. The selection used to be `Subterm::RecMember`, a node carrying its own copy of the group — well-formed standing alone, and so gated by no scope — and its arm answered `group.member_type(index)` outright, checking nothing. Both modules below were certified with **zero refusals** while that was the representation, and `check` accepted the selection against `Absurd` directly. The second is the sharper of the two: its group never recurses at all, so no totality rule is even in play, and what went unchecked was simply whether the body inhabits the type the group claims for it. `Absurd` is a proposition with no constructors, so either module was a closed inhabitant of one.
///
/// Neither obligation caught it either. (V) seeds from the kernel's own typing, and `locally_partial` asked `group_totality` for a `Subterm::Rec` node only — so the walk descended into a selection's member scopes and found a bound variable rather than a recursion. Unifying the two spellings closed that too, without a second rule: there is now one node for the walk to recognize.
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
            crate::SYNTAX,
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
            Term::func_type([(n.clone(), nat.clone())], nat.clone()),
        ),
        body: Scope::close(
            Many(1),
            &[&f],
            Term::func(
                [(n.clone(), nat.clone())],
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
        )],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    };

    assert_eq!(
        fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX),
        Vec::new(),
        "general recursion at a relevant type is legal however it is spelled",
    );
}
