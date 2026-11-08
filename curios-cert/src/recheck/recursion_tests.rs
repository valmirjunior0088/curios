//! A recursive member is certified only with its group.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::test_support::*,
    curios_common::{Qualifier, fill},
    curios_core::{
        Definitions, Entrypoint, Free, Global, Intrinsic, Item, Let, LetBinding, Level, Many, Nat,
        Program, Rec, RecGroup, RecItem, RecMemberScopes, Scope, Subterm, Term, Universes,
        test_support::*,
    },
    curios_surface::Spelling,
    std::collections::BTreeMap,
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
            verdicts.iter().any(|verdict| verdict
                .error
                .rendered(&Spelling::default())
                .contains(refusal)),
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

    let group = RecGroup::closing(vec![(
        f,
        Term::func_type([(n, nat.clone())], nat.clone()),
        Term::func(
            [(n, nat.clone())],
            Term::apply(Term::free_var(&f), [Term::free_var(&n)]),
        ),
    )]);

    let definitions = Definitions {
        items: vec![authored(
            &Global::Authored(Qualifier::from(["ok"])),
            Term::func_type([(n, nat.clone())], nat),
            Term::rec_proj(group, 0),
        )]
        .into(),
    };

    assert_eq!(
        fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX),
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
fn bad_definitions(body: Term) -> Definitions {
    Definitions {
        items: vec![authored(
            &Global::Authored(Qualifier::from(["bad"])),
            Term::intrinsic(Intrinsic::NatType),
            body,
        )]
        .into(),
    }
}

/// A group whose member scopes bind more than its members is refused, not aborted on.
///
/// `RecGroup::new` takes its members without relating their scopes' arity to their number, and `member_type` and `member_body` open each scope over one term per member, so without the boundary check `Scope::open`'s assert is the first thing that reads either count. A `rec` group is a Core term a module built by hand carries, and the board requires the implementation to fail closed.
#[test]
fn a_group_whose_member_scopes_bind_more_than_its_members_is_refused() {
    let (f, g) = (member(), Free::local(905, Some("g")));
    // No constructor builds this, which is the point: `rec_group_of_scopes` is the fixture door for the shape a restored archive could carry.
    let group = rec_group_of_scopes(vec![RecMemberScopes {
        type_: Scope::close(Many(2), &[&f, &g], Term::intrinsic(Intrinsic::NatType)),
        body: Scope::close(Many(2), &[&f, &g], Term::free_var(&f)),
    }]);
    let definitions = bad_definitions(Term::rec_proj(group, 0));

    assert!(
        !fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX).is_empty(),
        "the walk must refuse a group whose member scopes and members disagree in number",
    );
}

/// A group's tail binds one binder per member, and a tail that binds more is refused rather than aborted on.
///
/// `Rec` holds its `group` and its `tail` side by side, and the tail's arity is a fact about a sibling field — the members the group holds — that nothing in the representation relates it to. `check_group` read the member scopes' arities against the member count and nothing of the tail, and `infer`'s `Rec` arm then opens the tail over one term per member, so `Scope::open`'s assert was the first thing to read the count and the process died where the board requires a refusal. Mutation-checked: without the tail's clause in `check_group` this walk aborts in `Scope::open` instead of returning a verdict.
#[test]
fn a_group_whose_tail_binds_more_than_its_members_is_refused() {
    let (f, g) = (member(), Free::local(906, Some("g")));
    let group = RecGroup::closing(vec![(
        f,
        Term::intrinsic(Intrinsic::NatType),
        Term::free_var(&f),
    )]);
    let tail = Scope::close(Many(2), &[&f, &g], Term::free_var(&f));
    let definitions = bad_definitions(Term::from(Subterm::Rec(Rec { group, tail })));

    let verdicts = fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX);
    assert!(
        verdicts.iter().any(|verdict| matches!(
            verdict.error,
            Error::Arity {
                counted: Counted::GroupBinders,
                expected: 1,
                actual: 2,
            }
        )),
        "the walk must refuse a group whose tail binds more binders than the group has members: {verdicts:?}",
    );
}

/// A `let` block's tail binds one binder per binding, and a tail that binds more is refused rather than aborted on.
///
/// The same unrelated pair as the group's above, in the other block form: `Let` holds its `bindings` and its `tail` side by side, and `under_bindings` opens the tail over one binder per binding. The count is a block's rather than a group's, which is what `Counted::BlockBinders` names. Mutation-checked: without the clause in `under_bindings` this walk aborts in `Scope::open`.
#[test]
fn a_let_whose_tail_binds_more_than_its_bindings_is_refused() {
    let (x, y) = (Free::local(907, Some("x")), Free::local(908, Some("y")));
    let bindings = vec![LetBinding::new(
        Term::intrinsic(Intrinsic::NatType),
        Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
    )];
    let tail = Scope::close(Many(2), &[&x, &y], Term::free_var(&x));
    let definitions = bad_definitions(Term::from(Subterm::Let(Let { bindings, tail })));

    let verdicts = fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX);
    assert!(
        verdicts.iter().any(|verdict| matches!(
            verdict.error,
            Error::Arity {
                counted: Counted::BlockBinders,
                expected: 1,
                actual: 2,
            }
        )),
        "the walk must refuse a `let` whose tail binds more binders than the block has bindings: {verdicts:?}",
    );
}

/// A projection past a tuple's components is refused, not aborted on.
///
/// This is the shape the README names outright, and the count it is refused by at the boundary is `Counted::Components`. It is the control the two counts above are read against: one count of a hand-built term was checked before this commit and two were not.
#[test]
fn a_projection_past_a_tuples_components_is_refused() {
    let definitions = bad_definitions(Term::proj(
        Term::tuple([
            Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
        ]),
        7,
    ));

    assert!(
        !fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX).is_empty(),
        "the walk must refuse a projection past the components of what it projects from",
    );
}

/// `held : (y : (Type 1) -> Type 1) -> Type 1 = (y) => rec m : Type 1 = y(m) end.0`: the one shape whose projection survives forcing with its declared type a sort.
///
/// `Sort::of` reads a member's declared type only through `sort_of_neutral`, which `reduce_forced` reaches only for a projection `force` left standing — and `force` keeps the folded spelling exactly where the member's body reduces to a neutral *mentioning the group*, which is what the opaque head `y` makes of `y(m)`. The mention is the group's only call, recorded with no arguments and graded unknown, so the group does not descend; and the member's declared type yields a sort, so `check_group`'s local gate is owed a descent. The two halves are the same self-mention, which is why nothing escapes both: `yields_a_sort` peels arrows and projects tuple fields, so an applied or a projected member is inside the gate too.
fn surviving_projection_definitions() -> Definitions {
    let sort = Term::type_at(Level::zero().succ().expect("a level has a successor"));
    let opaque = Free::local(950, Some("y"));
    let member = Free::local(951, Some("m"));
    let group = Term::rec_proj(
        RecGroup::closing(vec![(
            member,
            sort.clone(),
            Term::apply(Term::free_var(&opaque), [Term::free_var(&member)]),
        )]),
        0,
    );
    let opaque_type = Term::func_type([(Free::local(952, Some("t")), sort.clone())], sort.clone());

    Definitions {
        items: vec![authored(
            &Global::Authored(Qualifier::from(["held"])),
            Term::func_type([(opaque, opaque_type.clone())], sort),
            Term::func([(opaque, opaque_type)], group),
        )]
        .into(),
    }
}

/// A member whose declared type is a sort cannot both keep its folded spelling under forcing and descend.
#[test]
fn a_member_whose_projection_survives_forcing_is_owed_a_descent() {
    let verdicts = fixture_verdicts(
        &surviving_projection_definitions(),
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

/// A member named at its instance inside its own group is refused as undeclared, never read as the declaration the walk is about to define.
///
/// A group's members are locals while its bodies are checked, and an instance names a declaration ([`curios_core::Instance::name`]) — the two are the one name's two meanings here, and the group's binder captures only the first. So the second must reach no scheme: read as the declaration, the occurrence would be no call the recorder sees, the group would close with no edge, and `rec loop : Absurd = loop` spelled at its instance would certify a closed inhabitant of an empty proposition. `Error::Undeclared` is the refusal that keeps that shut, and it is what makes the spelling unavailable rather than merely unused.
///
/// The control is the same group with its member spelled by the local the group binds, refused for the descent it owes.
#[test]
fn a_member_named_at_its_instance_inside_its_group_is_undeclared() {
    let name = Global::Authored(Qualifier::from(["loop"]));
    let local = Free::local(905, Some("loop"));
    let instanced = Term::instance(name, Universes::default());
    let at_its_instance = grouped_at_absurd(name, local, instanced);
    let by_its_local = grouped_at_absurd(name, local, Term::free_var(&local));

    let verdicts = fixture_verdicts(&at_its_instance, 1_000_000, &Globals::default(), SYNTAX);
    let undeclared = |verdict: &Verdict| matches!(verdict.error, Error::Undeclared(undeclared) if undeclared == name);
    assert!(
        verdicts.iter().any(undeclared),
        "a member named at its instance inside its own group was read as something: {verdicts:?}",
    );

    let control = fixture_verdicts(&by_its_local, 1_000_000, &Globals::default(), SYNTAX);
    assert!(
        control
            .iter()
            .any(|verdict| matches!(verdict.error, Error::NotDescending { .. })),
        "the control stopped refusing: a group of one at an empty proposition owes a descent: {control:?}",
    );
}

/// `rec loop : Absurd = <body>` as a module's one group, closed with `loop`, with `Absurd` an empty proposition: the member is declared under `name` and bound to `local` inside.
fn grouped_at_absurd(name: Global, local: Free, body: Term) -> Program {
    let family = Global::Authored(Qualifier::from(["Absurd"]));
    let absurd = Term::induct_type(family, Vec::<Term>::new(), Vec::<Term>::new());
    let Item::Let(definition) = authored(&name, absurd, body) else {
        unreachable!("`authored` builds a `let`");
    };

    Program {
        definitions: declaring(
            Definitions {
                items: vec![Item::Rec(RecItem::new(vec![(local, *definition)]))].into(),
            },
            BTreeMap::from([(family, proposition(Vec::new()))]),
            [],
            [],
        ),
        entrypoint: Entrypoint {
            body: Term::instance(name, Universes::default()),
            type_: Some(Term::induct_type(
                family,
                Vec::<Term>::new(),
                Vec::<Term>::new(),
            )),
        },
    }
}

/// An indirection does not launder a recursive occurrence past the call recorder: `rec f : Absurd = let g : Absurd = f; g` and `rec f : Absurd = ((g : Absurd) => g)(f)` are refused as `rec f : Absurd = f` is.
///
/// A group's calls are the ones the kernel types, and [`Calls::record_call`](crate::Kernel) takes a head that *is* a member local. Neither spelling applies one: a `let` is bound and not substituted ([`under_bindings`](crate::infer)), so the tail's head is the alias, and a lambda applied on the spot is typed with its binder standing for the argument, so the body's head is that binder. A group whose calls were all spelled this way would close with no edge at all, and a group with no edges descends — which would certify `rec f : Absurd = f` under another spelling.
///
/// What catches both is the bare occurrence the indirection has to pass through — the `let`'s value, checked where the `let` binds it, and the redex's argument, checked against its domain. `infer`'s `Var` rule records a member named anywhere but at an application's head as a call with no arguments, which grades as unknown throughout, so the verdict is the same as the direct spelling's.
///
/// Mutation-checked: with that `record_call` removed, both modules certify a closed inhabitant of an empty proposition and this test fails — as the direct spellings' tests in this module do, which is the point: the indirection is held by the same rule and by no second one.
#[test]
fn an_indirection_does_not_launder_a_member_past_the_call_recorder() {
    let absurd = || {
        Term::induct_type(
            Global::Authored(Qualifier::from(["Absurd"])),
            Vec::<Term>::new(),
            Vec::<Term>::new(),
        )
    };
    let taken = Free::local(904, Some("g"));
    let aliased = Term::let_(
        &taken,
        absurd(),
        Term::free_var(&member()),
        Term::free_var(&taken),
    );
    let passed = Term::apply(
        Term::func([(taken, absurd())], Term::free_var(&taken)),
        [Term::free_var(&member())],
    );

    for (label, body) in [("a `let`'s value", aliased), ("a redex's argument", passed)] {
        let verdicts = fixture_verdicts(
            &selection_module(body),
            1_000_000,
            &Globals::default(),
            SYNTAX,
        );

        assert!(
            verdicts
                .iter()
                .any(|verdict| matches!(verdict.error, Error::NotDescending { .. })),
            "{label}: a member reached through an indirection closed its group with no call, and the kernel certified a closed inhabitant of an empty proposition: {verdicts:?}",
        );
    }
}

/// A member named only in its own motive: `g`'s arms answer `qed` and call nothing, while the motive `Const(g(m))` names `g` at the motive's binder, a type the elimination is typed at.
const A_MEMBER_NAMED_IN_ITS_OWN_MOTIVE: &str = r#"
let /Const : (
    p:
        #{node = InductType}
        /std/Bool/True()
) -> Prop =
    (
        p:
            #{node = InductType}
            /std/Bool/True()
    ) =>
        #{node = InductType}
        /std/Bool/True();
#{node = Rec}
let /g : (n: #NatType) ->
    #{node = InductType}
    /std/Bool/True() =
    (n: #NatType) =>
        match n: (m) => /Const(/g(m))
        | 0 =>
            #{node = Variant}
            /std/Bool/True/qed()
        | _ + 1; x =>
            #{node = Variant}
            /std/Bool/True/qed()
        end;
/std/Bool/False
"#;

// A type is typed as written, so the motive's `g(m)` is an application the walk types and records, at an argument nothing is below: a group whose calls were looked for in its arms alone would read none here and descend. `curios`'s `tests::rules::totality_descent_tests` puts the program to the elaborator under this name.
//
// Mutation-checked at the `Apply` rule of `infer_within`: with no call recorded for an application whose head is a member, `g` descends and is not refused.
#[test]
fn a_member_named_only_in_its_own_motive_is_a_call_its_group_must_descend_on() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_MEMBER_NAMED_IN_ITS_OWN_MOTIVE}",
        PROPOSITIONS,
        A_MEMBER_NAMED_IN_ITS_OWN_MOTIVE
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("g"))
                && matches!(verdict.error, Error::NotDescending { .. })),
        "{verdicts:?}"
    );
}

/// A lambda applied on the spot to the caller's own argument, whose binder an arm then splits, and whose call hands that binder back: `again(x)` is `again(n)` wherever its arm is taken.
const A_CALL_HANDING_BACK_A_LAMBDAS_BINDER: &str = r#"
#{node = Rec}
let /again : (n: #NatType) ->
    #{node = InductType}
    /std/Bool/True() =
    (n: #NatType) =>
        ((x: #NatType) =>
            #{
                result = Ambient(
                    #{node = InductType}
                    /std/Bool/True())}
            match x
            | 0 =>
                #{node = Variant}
                /std/Bool/True/qed()
            | k + 1; y =>
                /again(x)
            end)(n);
/std/Bool/False
"#;

// The binder stands for `n` and the arm's case for the binder, so whichever of the two facts the size order holds, the call's argument is the parameter it stands at and nothing below it. `curios`'s `tests::rules::totality_descent_tests` puts the program to the elaborator under this name.
#[test]
fn a_call_handing_back_a_lambdas_binder_does_not_descend() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_CALL_HANDING_BACK_A_LAMBDAS_BINDER}",
        PROPOSITIONS,
        A_CALL_HANDING_BACK_A_LAMBDAS_BINDER
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("again"))
                && matches!(verdict.error, Error::NotDescending { .. })),
        "{verdicts:?}"
    );
}

/// `loop(0)` typed in a sibling's signature, where no body of the group is under check, and again as `loop`'s own body, where it is the one call `loop` makes, at a proposition. `W` holds the call at a parameter of `False`, so `witness`'s written domain and its folded one are one by irrelevance and nothing unfolds `loop`.
const A_CALL_TYPED_FIRST_IN_A_SIBLINGS_SIGNATURE: &str = r#"
induct /W(
    p:
        #{node = InductType}
        /std/Bool/False()
): Type
| mk()
end
#{node = Rec}
let /loop : (n: #NatType) ->
    #{node = InductType}
    /std/Bool/False() =
    (n: #NatType) =>
        /loop(0)
and /witness : (
    x:
        #{node = InductType}
        /W(/loop(0))
) -> #NatType =
    (
        x:
            #{node = InductType}
            /W(/loop(0))
    ) =>
        0;
/loop(5)
"#;

// The signature's typing of `loop(0)` is remembered, the group having no body under check there, and is never the answer in `loop`'s body, where a term naming a member is typed at every occurrence: the call is recorded and `loop` does not descend.
//
// Mutation-checked at `Kernel::infer_hit` and `Kernel::check_hit`: answering a term that names a member of a group whose body is being checked, the body's `loop(0)` is answered from the signature, no call is recorded, the group closes total and `loop(5)` is admitted at `False`.
#[test]
fn a_call_typed_first_in_a_siblings_signature_is_recorded_in_the_body() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_CALL_TYPED_FIRST_IN_A_SIBLINGS_SIGNATURE}",
        PROPOSITIONS,
        A_CALL_TYPED_FIRST_IN_A_SIBLINGS_SIGNATURE
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("loop"))
                && matches!(verdict.error, Error::NotDescending { .. })),
        "{verdicts:?}"
    );
}

/// A call below its parameter only to a judgment's reduction: `k + f(p) - f(q)` is `k` where the kernel's conversion holds `f(p)` and `f(q)` one atom, `{}` having one inhabitant, and stuck to plain reduction, which asks nothing. The annotation on `a` puts the argument to a judgment's reduction in the arm before the call is graded.
const A_CALL_BELOW_ITS_PARAMETER_ONLY_TO_A_JUDGMENT: &str = r#"
((
    f: (z: {}) -> #NatType,
    p: {},
    q: {}
) =>
    #{node = Rec}
    (let loop: (n: #NatType) -> Type =
        (n: #NatType) =>
            #{result = Ambient(Type)}
            match n
            | 0 =>
                #NatType
            | k + 1; ih =>
                let a: /std/Bool/Holds(#NatEql(#NatSub(#NatAdd(k, f(p)), f(q)), k)) =
                    #{node = Variant}
                    /std/Bool/True/qed();
                loop(#NatSub(#NatAdd(k, f(p)), f(q)))
            end;
    loop(3)))(
    (z: {}) => 0,
    (),
    ()
)
"#;

// A call is graded by plain reduction, whose local-bearing reducts are filed apart from a judgment's: the reduct the annotation's comparison took is no answer to the grading, the call is unknown, and the member, a type, does not descend.
#[test]
fn a_call_below_its_parameter_only_to_a_judgment_does_not_descend() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_CALL_BELOW_ITS_PARAMETER_ONLY_TO_A_JUDGMENT}",
        PROPOSITIONS,
        A_CALL_BELOW_ITS_PARAMETER_ONLY_TO_A_JUDGMENT
    ));

    assert!(
        verdicts.iter().any(|verdict| verdict.name.is_none()
            && matches!(verdict.error, Error::NotDescending { .. })),
        "{verdicts:?}"
    );
}

/// `Flip(b)` is `True` at `false` and `False` at `true`, and `claimed`, beside it in a group a definition's body holds, states `Flip(false)` in its own body; `control` states the same proof as `stated`'s type, which its body is checked at.
const A_SIBLING_READ_IN_A_BODY: &str = r#"
let /outer : /std/Bool/Holds(true) =
    #{node = Rec}
    (let Flip: (b: #BoolType) -> Prop =
        (b: #BoolType) =>
            #{result = Ambient(Prop)}
            match b
            | false =>
                #{node = InductType}
                /std/Bool/True()
            | true =>
                #{node = InductType}
                /std/Bool/False()
            end
    and claimed: /std/Bool/Holds(true) =
        let p: Flip(false) =
            #{node = Variant}
            /std/Bool/True/qed();
        #{node = Variant}
        /std/Bool/True/qed();
    claimed);
let /control : /std/Bool/Holds(true) =
    #{node = Rec}
    (let Flop: (b: #BoolType) -> Prop =
        (b: #BoolType) =>
            #{result = Ambient(Prop)}
            match b
            | false =>
                #{node = InductType}
                /std/Bool/True()
            | true =>
                #{node = InductType}
                /std/Bool/False()
            end
    and stated: Flop(false) =
        #{node = Variant}
        /std/Bool/True/qed();
    #{node = Variant}
    /std/Bool/True/qed());
/std/Bool/False
"#;

// A member known to terminate is read by value in its group's bodies, as its tail reads it: `Flip` calls nothing, so its turn finds it known, and `claimed` states `Flip(false)` in its own body, which is `True` there as it is where `stated`'s body is checked, and both stand. `curios`'s `tests::rules::group_tests` puts the program to the elaborator under this name.
//
// Mutation-checked at `check_group`: with no member read by value, `/outer` and `/control` are refused `Mismatch`.
#[test]
fn a_member_known_to_terminate_is_read_by_value_in_its_groups_bodies() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_SIBLING_READ_IN_A_BODY}",
        PROPOSITIONS,
        A_SIBLING_READ_IN_A_BODY
    ));

    assert!(
        verdicts.iter().all(|verdict| verdict.name.is_none()),
        "{verdicts:?}"
    );
}

/// `f`'s body restates its binder at `T(0)` from its signature, as the elaborator writes a lambda checked at one, and its signature reads `T(0)` folded: read as standing for nothing in the body, `T` left `T(0)` stuck there, where the signature's reduces to `Nat`.
const A_LAMBDA_TYPED_AT_A_SIBLING: &str = r#"
let /outer : (n: #NatType) -> #NatType =
    (n: #NatType) =>
        #{node = Rec}
        (let T: (m: #NatType) -> Type =
            (m: #NatType) =>
                #{result = Ambient(Type)}
                match m
                | 0 =>
                    #NatType
                | k + 1; x =>
                    T(k)
                end
        and f: (x: T(0)) -> T(0) =
            (x: T(0)) => x;
        f(n));
/std/Bool/False
"#;

// A body is checked at its signature as the group's bodies read it, so a binder it restates at a sibling is the type its signature reads. `curios`'s `tests::recursion::a_member_typed_at_a_sibling_reads_it_by_value` runs the program both ways it is written.
//
// Mutation-checked at `check_group`: with each member assumed, and its body checked, at its signature read through the folded group, and no member read by value, `/outer` is refused `Mismatch`, `T(0)` at the local against `T(0)` at the group.
#[test]
fn a_lambda_typed_at_a_sibling_stands() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_LAMBDA_TYPED_AT_A_SIBLING}",
        PROPOSITIONS,
        A_LAMBDA_TYPED_AT_A_SIBLING
    ));

    assert!(
        verdicts.iter().all(|verdict| verdict.name.is_none()),
        "{verdicts:?}"
    );
}

/// No member of `b` and `c` is a proof or yields a sort, so the group owes no descent, and `c`'s body states `Holds(b)`.
const A_VALUE_READING_A_SIBLING: &str = r#"
let /read : #BoolType =
    #{node = Rec}
    (let b: #BoolType = true
    and c: #BoolType =
        let p: /std/Bool/Holds(b) =
            #{node = Variant}
            /std/Bool/True/qed();
        false;
    c);
/std/Bool/False
"#;

// A group that owes no descent reads a member by value once its turn finds it known too: `b` calls nothing, so `Holds(b)` is the `True` `qed` inhabits. `curios`'s `tests::rules::group_tests` puts the program to the elaborator under this name.
//
// Mutation-checked at `check_group`: with no member read by value, `/read` is refused `Mismatch`.
#[test]
fn a_member_of_a_group_that_owes_no_descent_is_read_once_known_to_terminate() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_VALUE_READING_A_SIBLING}",
        PROPOSITIONS,
        A_VALUE_READING_A_SIBLING
    ));

    assert!(
        verdicts.iter().all(|verdict| verdict.name.is_none()),
        "{verdicts:?}"
    );
}

/// `b(n)` calls itself at `n`, nothing below it, and `c`'s body states `Holds(b(0))`, which `b(0)`, `true`, would make `True`.
const A_MEMBER_WITHHELD: &str = r#"
let /withheld : #BoolType =
    #{node = Rec}
    (let b: (n: #NatType) -> #BoolType =
        (n: #NatType) =>
            #{result = Ambient(#BoolType)}
            match n
            | 0 =>
                true
            | k + 1; x =>
                b(n)
            end
    and c: #BoolType =
        let p: /std/Bool/Holds(b(0)) =
            #{node = Variant}
            /std/Bool/True/qed();
        false;
    c);
/std/Bool/False
"#;

// A member not known to terminate stands for nothing in its group's bodies: `b`'s turn withholds it, and an equation of a member that need not terminate is no fact, so `Holds(b(0))` is no `True` for `qed` to inhabit, though `b(0)` is `true`. `curios`'s `tests::rules::group_tests` puts the program to the elaborator under this name.
//
// Mutation-checked at `check_group`: with every member read by value whatever its turn finds, `/withheld` stands.
#[test]
fn a_member_on_a_cycle_that_does_not_descend_stands_for_nothing_in_its_bodies() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_MEMBER_WITHHELD}",
        PROPOSITIONS,
        A_MEMBER_WITHHELD
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("withheld"))
                && matches!(verdict.error, Error::Mismatch { .. })),
        "{verdicts:?}"
    );
}

/// `b` is a list of itself, and `c` a value of `b`.
const A_GROUP_LISTING_ITSELF: &str = r#"
let /listing : #NatType =
    #{node = Rec}
    (let b: Type = #ListType(b)
    and c: b = #List(b, [])
    and n: #NatType = 0;
    n);
/std/Bool/False
"#;

// A group that owes descent is named where it does not, rather than refused for its budget: `b` calls itself with nothing below, so its turn withholds it and nothing unfolds it, and `c`'s body, which reads `b` by value to check `[]` at it, is refused only after the group's calls have named it. `curios`'s `tests::rules::group_tests` puts the program to the elaborator under this name.
//
// Mutation-checked at `check_group`: with every member read by value whatever its turn finds, `c`'s body unfolds `b` into a list of itself without end and `/listing` is refused for its budget.
#[test]
fn a_group_listing_itself_is_refused_for_not_descending() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_GROUP_LISTING_ITSELF}",
        PROPOSITIONS,
        A_GROUP_LISTING_ITSELF
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("listing"))
                && matches!(verdict.error, Error::NotDescending { .. })),
        "{verdicts:?}"
    );
}

/// `g` and `h` each state `h(0)` in a match's motive, a call at an argument nothing is below, so `h`'s own keeps the group from descending.
const A_CALL_TWO_MOTIVES_STATE: &str = r#"
let /Const : (
    p:
        #{node = InductType}
        /std/Bool/True()
) -> Prop =
    (
        p:
            #{node = InductType}
            /std/Bool/True()
    ) =>
        #{node = InductType}
        /std/Bool/True();
#{node = Rec}
let /g : (n: #NatType) ->
    #{node = InductType}
    /std/Bool/True() =
    (n: #NatType) =>
        match n: (m) => /Const(/h(0))
        | 0 =>
            #{node = Variant}
            /std/Bool/True/qed()
        | _ + 1; x =>
            #{node = Variant}
            /std/Bool/True/qed()
        end
and /h : (n: #NatType) ->
    #{node = InductType}
    /std/Bool/True() =
    (n: #NatType) =>
        match n: (m) => /Const(/h(0))
        | 0 =>
            #{node = Variant}
            /std/Bool/True/qed()
        | _ + 1; x =>
            #{node = Variant}
            /std/Bool/True/qed()
        end;
/std/Bool/False
"#;

// A group whose members are read by value in its bodies still spells every term it types with its members' locals, so a call two bodies state is typed, and recorded, in each: no remembered typing answers for a term naming a member. `h`'s own call withholds it at its turn, and the verdict its typed calls close to names the group.
//
// Mutation-checked at `Calls::names_member`: with a remembered typing answering for a term that names a member, `h`'s typing of its motive is answered by the one `g`'s filled, the call is never recorded, and the group stands.
#[test]
fn a_call_two_motives_state_is_recorded_in_each_body() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_CALL_TWO_MOTIVES_STATE}",
        PROPOSITIONS,
        A_CALL_TWO_MOTIVES_STATE
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("g"))
                && matches!(verdict.error, Error::NotDescending { .. })),
        "{verdicts:?}"
    );
}

/// `S` is indexed by a proof of `W`, and its one payload is its index, so a match on an `S` proof fires with the proof unreduced and reads `v` off the index, and the size order grades `v` below `s` though it is `i` and no part of `s`. `h`'s `base` arm hands `g` the base again with a proof built of it, so `h(base)` is `h(base)` for ever.
const A_PAYLOAD_A_CAST_READS_OFF_A_PROOFS_INDEX: &str = r#"
induct /W: pub Prop
| base()
end
induct /S: (
    w:
        #{node = InductType}
        /W()
) -> pub Prop
| mk(
    v:
        #{node = InductType}
        /W()
): (v)
end
#{node = Rec, proof = true}
let /g : (
    i:
        #{node = InductType}
        /W(),
    s:
        #{node = InductType}
        /S()(i)
) ->
    #{node = InductType}
    /std/Bool/False() =
    (
        i:
            #{node = InductType}
            /W(),
        s:
            #{node = InductType}
            /S()(i)
    ) =>
        #{
            result = Ambient(
                #{node = InductType}
                /std/Bool/False())}
        match s
        | mk(v) =>
            /h(v)
        end
and /h : (
    w:
        #{node = InductType}
        /W()
) ->
    #{node = InductType}
    /std/Bool/False() =
    (
        w:
            #{node = InductType}
            /W()
    ) =>
        #{
            result = Ambient(
                #{node = InductType}
                /std/Bool/False())}
        match w
        | base() =>
            /g(w, /S/mk(w))
        end;
/h(/W/base())
"#;

// Reading the cast's payload below the proof is sound — a proof of `S(i)` ranks above `i` at its least derivation, `mk` over a derivation of `i`'s proposition — so what refuses the loop is the other half: nothing in the size order reads `S/mk(w)` at or below `w`, so the base arm's call starts no thread back to `s` and the group does not descend. `curios`'s `tests::rules::totality_tests` puts the program to both checkers under this name.
#[test]
fn a_payload_a_cast_reads_off_a_proofs_index_is_no_descent() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_PAYLOAD_A_CAST_READS_OFF_A_PROOFS_INDEX}",
        PROPOSITIONS,
        A_PAYLOAD_A_CAST_READS_OFF_A_PROOFS_INDEX
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("g"))
                && matches!(verdict.error, Error::NotDescending { .. })),
        "{verdicts:?}"
    );
}

/// `S` is wholly pinned, so a match on a proof of it into `Type` fires with the proof unreduced and reads `m` off the index: `m` is `n`, graded below `p`, and the call hands `X` back a proof at the same index, which a type reaching `X` would unfold for ever.
const A_TYPE_RECURSING_THROUGH_A_CAST_ON_THE_INDEX_IT_READS: &str = r#"
induct /S: (#NatType) -> pub Prop
| mk(n: #NatType): (n)
end
#{node = Rec}
let /X : (
    n: #NatType,
    p:
        #{node = InductType}
        /S()(n)
) -> Type =
    (
        n: #NatType,
        p:
            #{node = InductType}
            /S()(n)
    ) =>
        #{result = Ambient(Type)}
        match p
        | mk(m) =>
            /X(m, /S/mk(m))
        end;
/std/Bool/True/qed()
"#;

// The one way a proof reaches a type is the cast, and a recursion graded below the proof it casts along must still descend to be put at (T): `m` is below `p`, `S/mk(m)` is nothing below `p`, and `m` stands for `n` only by the arm's index solution, which refines for typing alone. `curios`'s `tests::rules::totality_tests` puts the program to the elaborator under this name.
#[test]
fn a_type_recursing_through_a_cast_on_the_index_it_reads_does_not_descend() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_TYPE_RECURSING_THROUGH_A_CAST_ON_THE_INDEX_IT_READS}",
        PROPOSITIONS,
        A_TYPE_RECURSING_THROUGH_A_CAST_ON_THE_INDEX_IT_READS
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("X"))
                && matches!(verdict.error, Error::NotDescending { .. })),
        "{verdicts:?}"
    );
}

/// `f` splits `x`, a lambda's binder applied on the spot to `w2`, inside the arm that refined `w2` to `up(u)`: the split refines `w2` again through what `x` stands for, and its `base` case is the dead `base` of a value known to be `up(u)`. `${dead}` is that case's body and `${live}` the `up(j)` case's.
const A_GROUP_SPLITTING_AN_APPLIED_LAMBDAS_BINDER: &str = r#"
#{absent = [/W/base, /W/up]}
induct /W: pub Prop
| base()
| up(w: #{node = InductType} /W())
end

#{node = Rec}
(let f: (w: #{node = InductType} /W(), b: /std/Bool/Holds(false)) -> /std/Bool/False =
    (w2: #{node = InductType} /W(), b2: /std/Bool/Holds(false)) =>
        ((x: #{node = InductType} /W()) =>
            match w2: (y) => /std/Bool/False
            | base() => match b2: (z) => /std/Bool/False end
            | up(u) =>
                match x: (y2) => /std/Bool/False
                | base() => ${dead}
                | up(j) => ${live}
                end
            end)(w2);
f(#{node = Variant} /W/base(), #{node = Variant} /std/Bool/True/qed()))
"#;

// A dead case of an applied lambda's binder is no descent: what the split makes of `w2` there is a shape of the value the outer arm already knew, so the repeating `f(w2, b2)` is graded at `w2` itself and refused. The control hands the live case's payload on, `f(j, b2)`, and its group stands.
#[test]
fn a_dead_case_of_an_applied_lambdas_binder_is_no_descent() {
    let group = |dead: &str, live: &str| {
        core_text_verdicts(&fill!(
            "${PROPOSITIONS}\n${GROUP}",
            PROPOSITIONS,
            GROUP = fill!(A_GROUP_SPLITTING_AN_APPLIED_LAMBDAS_BINDER, dead, live)
        ))
    };

    let attack = group("f(w2, b2)", "match b2: (z2) => /std/Bool/False end");
    assert!(
        attack
            .iter()
            .any(|verdict| matches!(verdict.error, Error::NotDescending { .. })),
        "{attack:?}"
    );

    let control = group("match b2: (z3) => /std/Bool/False end", "f(j, b2)");
    assert!(
        !control.iter().any(|verdict| matches!(
            verdict.error,
            Error::NotDescending { .. } | Error::NotTotal { .. }
        )),
        "{control:?}"
    );
}

/// `q` calls itself at `n` from the body of a group inside its own body: an outer member's call made through an inner group.
const A_CALL_THROUGH_AN_INNER_GROUP: &str = r#"
let /through_inner : /std/Bool/False =
    #{node = Rec}
    (let q: (n: #NatType) -> /std/Bool/False =
        (n: #NatType) =>
            #{node = Rec}
            (let g: (m: #NatType) -> /std/Bool/False =
                (m: #NatType) =>
                    #{result = Ambient(/std/Bool/False)} match m | 0 => q(n) | k + 1; x => g(k) end;
            g(0));
    q(0));
/std/Bool/False
"#;

// A call an outer member's name makes from an inner group's body is recorded against the outer member and graded under its parameters: `q(n)` at `n` does not descend, whatever the inner group does.
#[test]
fn a_call_an_inner_group_makes_to_an_outer_member_does_not_descend() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_CALL_THROUGH_AN_INNER_GROUP}",
        PROPOSITIONS,
        A_CALL_THROUGH_AN_INNER_GROUP
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("through_inner"))
                && matches!(verdict.error, Error::NotDescending { .. })),
        "{verdicts:?}"
    );
}

/// `A` is known at its turn, its call to `B` being no call while `B` has not taken its turn, and `B` is withheld at its own; `omega` reads `A` by value and applies a proof to itself.
const A_KNOT_ACROSS_TURNS: &str = r#"
let /knot : /std/Bool/False =
    #{node = Rec}
    (let A: Prop = (x: B) -> /std/Bool/False
    and B: Prop = A
    and omega: (a: A) -> /std/Bool/False = (a: A) => a(a);
    omega(omega));
/std/Bool/False
"#;

// A member known at its turn only for the members before it is still on a cycle the group does not descend on: the verdict its typed calls close to names the group, and the error held back for a body is returned after it, so the group is refused whichever of the two a body meets first.
#[test]
fn a_knot_across_turns_is_refused_for_not_descending() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_KNOT_ACROSS_TURNS}",
        PROPOSITIONS,
        A_KNOT_ACROSS_TURNS
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("knot"))
                && matches!(verdict.error, Error::NotDescending { .. })),
        "{verdicts:?}"
    );
}
