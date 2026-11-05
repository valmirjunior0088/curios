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

/// `Flip(b)` is `True` at `false` and `False` at `true`, and `claimed`, beside it in a group a definition's body holds, states `Flip(false)` in its own body; `control` states the same proof as `stated`'s type.
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

// A group's bodies are checked over members that stand for nothing, so no body reads a value of its group, a sibling's included: `claimed` is refused where it states `Flip(false)`, and the same proof stated as `stated`'s type, where a signature reads its group folded, stands. `curios`'s `tests::rules::group_tests` puts the program to the elaborator under this name.
//
// Mutation-checked at `check_group`: with each body checked over the members bound to their folded selections, as `infer`'s `Rec` arm binds them in the tail, `/outer` stands.
#[test]
fn a_sibling_stands_for_nothing_in_its_groups_bodies() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_SIBLING_READ_IN_A_BODY}",
        PROPOSITIONS,
        A_SIBLING_READ_IN_A_BODY
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("outer"))
                && matches!(verdict.error, Error::Mismatch { .. })),
        "{verdicts:?}"
    );
    assert!(
        verdicts
            .iter()
            .all(|verdict| verdict.name != Some(global("control"))),
        "{verdicts:?}"
    );
}

/// `S` is indexed by a proof of `W` and `W` holds an `S`, a family indexed by a family of its own group; `S`'s one payload is its index, so a match on an `S` proof fires with the proof unreduced and reads `v` off the index, and the size order grades `v` below `s` though it is `i` and no part of `s`. `h`'s `base` arm hands `g` the base again with a proof built of it, so `h(base)` is `h(base)` for ever.
const A_PAYLOAD_A_CAST_READS_OFF_A_PROOFS_INDEX: &str = r#"
induct /W: pub Prop
| base()
| wrap(
    x:
        #{node = InductType}
        /W(),
    s:
        #{node = InductType}
        /S()(x)
)
end
and induct /S: (
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
        | wrap(x, s2) =>
            /g(x, s2)
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
