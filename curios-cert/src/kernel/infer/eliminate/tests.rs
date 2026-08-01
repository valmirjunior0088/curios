use {
    crate::{Kernel, KernelError, infer},
    curios_base::{Plicity, Qualifier, RootId},
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Many, Prim, Scope, Telescope, Term,
        UniverseContext,
    },
};

fn kernel() -> Kernel {
    let mut kernel = Kernel::new(100_000);
    kernel.set_local_floor(1_000);
    kernel
}

fn binder(index: u32, hint: &str) -> Free {
    Free::local(index, Some(hint))
}

fn nat(n: usize) -> Term {
    Term::prim(Prim::Nat(curios_core::Nat::new(n)))
}

fn nat_type() -> Term {
    Term::prim(Prim::NatType)
}

/// One constructor of a test family: its tag, the payload binder it carries if any, and the index target this case aims at.
struct Case {
    tag: &'static str,
    payload: Option<(Free, Term)>,
    index: Term,
}

/// A constructor carrying nothing, aimed at `index`.
fn nullary(tag: &'static str, index: Term) -> Case {
    Case {
        tag,
        payload: None,
        index,
    }
}

/// A constructor carrying `binder : type_`, aimed at `index`.
fn carrying(tag: &'static str, binder: Free, type_: Term, index: Term) -> Case {
    Case {
        tag,
        payload: Some((binder, type_)),
        index,
    }
}

/// Declare a one-index family from its constructors.
fn declare(kernel: &mut Kernel, path: &str, result_sort: Term, constructors: Vec<Case>) -> Global {
    let family = Global::Authored(Qualifier::from([path]));

    let entries = constructors
        .into_iter()
        .map(|case| {
            let targets = vec![case.index];
            let (telescope, plicities) = match case.payload {
                Some((field, type_)) => (
                    Telescope::build([(field, type_)], targets),
                    vec![Plicity::Explicit],
                ),
                None => (Telescope::done(targets), Vec::new()),
            };

            (
                Atom::from(case.tag),
                InductParam {
                    telescope,
                    plicities,
                },
            )
        })
        .collect();

    kernel.declare_induct(
        &family,
        &InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::done(())),
            constructors: entries,
            result_sort,
            module: Qualifier::from([path]),
            root: RootId::Entry,
            rep_public: true,
            polarities: Vec::new(),
        },
    );

    family
}

/// `match subject : (i, s) => motive | tag(binders) => body ... end`, over a scrutinee assumed at `family(index)`.
fn eliminate(
    kernel: &mut Kernel,
    family: &Global,
    index: Term,
    motive: Term,
    arms: Vec<(&str, Vec<Free>, Term)>,
) -> Term {
    let subject = binder(50, "subject");
    kernel.assume(
        &subject,
        &Term::induct_type(family.clone(), Vec::<Term>::new(), [index]),
    );

    let motive = Scope::close(Many(2), &[&binder(51, "i"), &binder(52, "s")], motive);

    Term::induct_match_scoped_marked(
        Term::free_var(&subject),
        motive,
        arms.into_iter().map(|(tag, binders, body)| {
            (
                tag,
                binders
                    .into_iter()
                    .map(|b| (Plicity::Explicit, b))
                    .collect::<Vec<_>>(),
                body,
            )
        }),
        None,
    )
}

/// A successor over `tail`, in the successor-floor form reduction keeps.
fn succ(tail: Term) -> Term {
    Term::prim(Prim::Nat(curios_core::Nat::Succ(1u32.into(), tail)))
}

/// An opaque `P : (n : Nat, x : family(n)) -> Type`, for observing which *scrutinee* a term checks at as well as which index.
///
/// [`opaque_family`] below can only witness the index half of a motive, and a catch-all refines no index — so nothing built from it can tell the scrutinee instances apart.
fn scrutinee_family(kernel: &mut Kernel, name: Free, family: &Global) -> Term {
    let index = binder(92, "n");
    kernel.declare(
        &name,
        &Term::func_type(
            [
                (index.clone(), nat_type()),
                (
                    binder(93, "x"),
                    Term::induct_type(family.clone(), Vec::<Term>::new(), [Term::free_var(&index)]),
                ),
            ],
            Term::type_ground(),
        ),
        &UniverseContext::default(),
    );

    Term::free_var(&name)
}

/// An opaque `P : (Nat) -> Type`, for observing which instance a term checks at.
fn opaque_family(kernel: &mut Kernel, name: Free) -> Term {
    kernel.declare(
        &name,
        &Term::func_type([(binder(90, "n"), nat_type())], Term::type_ground()),
        &UniverseContext::default(),
    );

    Term::free_var(&name)
}

/// The arm rule specializes the *context*, not just the motive. Matching `s(p) : Ix(p + 1)` against a scrutinee typed `Ix(b)` forces `b ≡ p + 1`, so a hypothesis at `P(b)` inhabits the arm's expectation `P(p + 1)`.
///
/// This is the shape `/std/Nat/Lte/trans` needs: the equation refines an *outer* binder, which no motive opening reaches and no arm binder owns.
#[test]
fn a_forced_index_equation_refines_an_outer_hypothesis() {
    let mut kernel = kernel();
    let b = binder(0, "b");
    let payload = binder(1, "p");
    let w = binder(3, "w");
    let arm_binder = binder(10, "p");

    let p = opaque_family(&mut kernel, binder(2, "P"));
    kernel.assume(&b, &nat_type());
    kernel.assume(&w, &Term::apply(p.clone(), [Term::free_var(&b)]));

    let family = declare(
        &mut kernel,
        "Ix",
        Term::type_ground(),
        vec![carrying(
            "s",
            payload.clone(),
            nat_type(),
            succ(Term::free_var(&payload)),
        )],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        Term::free_var(&b),
        Term::apply(p.clone(), [Term::free_var(&binder(51, "i"))]),
        vec![("s", vec![arm_binder], Term::free_var(&w))],
    );

    assert_eq!(
        infer(&mut kernel, &term),
        Ok(Term::apply(p, [Term::free_var(&b)])),
    );
}

/// A forced equation whose solution mentions a refinable variable is refused rather than substituted: `Cyc(b)` matched against a case aimed at `b + 1` would solve `b := b + 1`, and the occurs check must leave the arm unspecialized instead. The hypothesis therefore stays at `P(b)` and fails the expectation `P(b + 1)` — a refusal, never a silent cycle.
#[test]
fn a_cyclic_index_equation_refines_nothing() {
    let mut kernel = kernel();
    let b = binder(0, "b");
    let w = binder(3, "w");

    let p = opaque_family(&mut kernel, binder(2, "P"));
    kernel.assume(&b, &nat_type());
    kernel.assume(&w, &Term::apply(p.clone(), [Term::free_var(&b)]));

    let family = declare(
        &mut kernel,
        "Cyc",
        Term::type_ground(),
        vec![nullary("mk", succ(Term::free_var(&b)))],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        Term::free_var(&b),
        Term::apply(p, [Term::free_var(&binder(51, "i"))]),
        vec![("mk", Vec::new(), Term::free_var(&w))],
    );

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// A variable `Bool` scrutinee stands refined to the literal in each arm — the zero-index instance of the same specialization.
#[test]
fn a_bool_arm_sees_its_scrutinee_at_the_literal() {
    let mut kernel = kernel();
    let h = binder(0, "h");
    let w = binder(3, "w");
    let motive_binder = binder(51, "x");

    let p = binder(2, "P");
    kernel.declare(
        &p,
        &Term::func_type(
            [(binder(90, "x"), Term::prim(Prim::BoolType))],
            Term::type_ground(),
        ),
        &UniverseContext::default(),
    );
    let p = Term::free_var(&p);

    kernel.assume(&h, &Term::prim(Prim::BoolType));
    kernel.assume(&w, &Term::apply(p.clone(), [Term::free_var(&h)]));

    let term = Term::bool_match(
        Term::free_var(&h),
        Some(&motive_binder),
        Term::apply(p.clone(), [Term::free_var(&motive_binder)]),
        Term::free_var(&w),
        Term::free_var(&w),
    );

    assert_eq!(
        infer(&mut kernel, &term),
        Ok(Term::apply(p, [Term::free_var(&h)])),
    );
}

/// An absent arm whose targets clash with the scrutinee's indices is legitimately absent: no value of that constructor can carry them. `s(p)` aims at `p + 1`, the scrutinee sits at `0`, and the `s` arm may be omitted.
#[test]
fn a_clashing_absent_arm_is_legitimately_absent() {
    let mut kernel = kernel();
    let z_payload = binder(1, "p");

    let family = declare(
        &mut kernel,
        "Ix",
        Term::type_ground(),
        vec![
            nullary("z", nat(0)),
            carrying(
                "s",
                z_payload.clone(),
                nat_type(),
                succ(Term::free_var(&z_payload)),
            ),
        ],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        nat_type(),
        vec![("z", Vec::new(), nat(1))],
    );

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
}

/// An absent arm the unifier cannot rule out is a refusal: at a *variable* index nothing clashes, so every constructor must have an arm or a catch-all. Undecided is not absent.
#[test]
fn an_undecided_absent_arm_is_refused() {
    let mut kernel = kernel();
    let b = binder(0, "b");
    let payload = binder(1, "p");
    let arm_binder = binder(10, "p");

    kernel.assume(&b, &nat_type());

    let family = declare(
        &mut kernel,
        "Ix",
        Term::type_ground(),
        vec![
            nullary("z", nat(0)),
            carrying(
                "s",
                payload.clone(),
                nat_type(),
                succ(Term::free_var(&payload)),
            ),
        ],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        Term::free_var(&b),
        nat_type(),
        vec![("s", vec![arm_binder], nat(0))],
    );

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(KernelError::MissingArm { .. }),
    ));
}

/// A catch-all stands for every constructor not enumerated, so nothing further is owed for the absent arms.
#[test]
fn a_catch_all_covers_absent_arms() {
    let mut kernel = kernel();
    let b = binder(0, "b");
    let payload = binder(1, "p");
    let subject = binder(50, "subject");

    kernel.assume(&b, &nat_type());

    let family = declare(
        &mut kernel,
        "Ix",
        Term::type_ground(),
        vec![
            nullary("z", nat(0)),
            carrying(
                "s",
                payload.clone(),
                nat_type(),
                succ(Term::free_var(&payload)),
            ),
        ],
    );

    kernel.assume(
        &subject,
        &Term::induct_type(family, Vec::<Term>::new(), [Term::free_var(&b)]),
    );
    let motive = Scope::close(Many(2), &[&binder(51, "i"), &binder(52, "s")], nat_type());
    let term = Term::induct_match_scoped_marked(
        Term::free_var(&subject),
        motive,
        Vec::<(&str, Vec<(Plicity, Free)>, Term)>::new(),
        Some(nat(7)),
    );

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
}

/// A catch-all is checked at the motive's *scrutinee* instance — the instance the elimination is then given.
///
/// Every other arm has a case value of its own to be checked at, and a catch-all is the one arm that does not: it binds nothing and refines no index, so the only value it can stand for is the scrutinee. `infer` reads the elimination's type off `motive.open([indices, head])`, so an arm checked at any other instance proves something other than what the elimination hands its caller.
///
/// While the hole was open the scrutinee binder was opened at the family *type* `Ix(b)` instead — a type substituted where a value of that type belongs — so this hypothesis at `P(b, subject)` was refused against an expectation of `P(b, Ix(b))`. That direction fails closed, because nothing well-typed inhabits the expectation it manufactures; the direction that does not is the certifier's, which established nothing whatever about the scrutinee for any catch-all it accepted. Reachable from source: `match t : (q) => Eq(q, q) | a() => Eq/refl() | _ => Eq/refl() end` compiled, elaborated, and was then refused by the kernel with `/Three` standing in both term positions of the `Eq`. It is inert across the fixed prelude — every catch-all there has a motive that ignores its scrutinee — which is why a rule graded *probed* had never been asked this question.
///
/// [`a_catch_all_at_another_value_of_the_family_is_refused`] is the paired control: closing this by not checking the catch-all at all would satisfy the test above and nothing else.
#[test]
fn a_catch_all_sees_its_own_scrutinee() {
    let mut kernel = kernel();
    let b = binder(0, "b");
    let payload = binder(1, "p");
    let subject = binder(50, "subject");
    let hypothesis = binder(3, "w");

    kernel.assume(&b, &nat_type());

    let family = declare(
        &mut kernel,
        "Ix",
        Term::type_ground(),
        vec![
            nullary("z", nat(0)),
            carrying(
                "s",
                payload.clone(),
                nat_type(),
                succ(Term::free_var(&payload)),
            ),
        ],
    );
    let p = scrutinee_family(&mut kernel, binder(2, "P"), &family);

    kernel.assume(
        &subject,
        &Term::induct_type(family, Vec::<Term>::new(), [Term::free_var(&b)]),
    );
    let at_the_scrutinee = Term::apply(p.clone(), [Term::free_var(&b), Term::free_var(&subject)]);
    kernel.assume(&hypothesis, &at_the_scrutinee);

    let term = Term::induct_match_scoped_marked(
        Term::free_var(&subject),
        Scope::close(
            Many(2),
            &[&binder(51, "i"), &binder(52, "s")],
            Term::apply(
                p,
                [
                    Term::free_var(&binder(51, "i")),
                    Term::free_var(&binder(52, "s")),
                ],
            ),
        ),
        Vec::<(&str, Vec<(Plicity, Free)>, Term)>::new(),
        Some(Term::free_var(&hypothesis)),
    );

    assert_eq!(infer(&mut kernel, &term), Ok(at_the_scrutinee));
}

/// The control for [`a_catch_all_sees_its_own_scrutinee`]: a catch-all body at *another* inhabitant of the family is refused, and refused against the scrutinee's instance.
///
/// `other` sits at the same type as `subject` and is a different variable, so `P(b, other)` and `P(b, subject)` are distinct types with nothing to convert them. Asserting the expectation rather than merely the refusal is what makes this a control: a kernel that stopped checking the catch-all would accept this, and one that checked it at some other instance would name that instance here.
#[test]
fn a_catch_all_at_another_value_of_the_family_is_refused() {
    let mut kernel = kernel();
    let b = binder(0, "b");
    let payload = binder(1, "p");
    let subject = binder(50, "subject");
    let other = binder(53, "other");
    let hypothesis = binder(3, "w");

    kernel.assume(&b, &nat_type());

    let family = declare(
        &mut kernel,
        "Ix",
        Term::type_ground(),
        vec![
            nullary("z", nat(0)),
            carrying(
                "s",
                payload.clone(),
                nat_type(),
                succ(Term::free_var(&payload)),
            ),
        ],
    );
    let p = scrutinee_family(&mut kernel, binder(2, "P"), &family);

    let family_at_b = Term::induct_type(family, Vec::<Term>::new(), [Term::free_var(&b)]);
    kernel.assume(&subject, &family_at_b);
    kernel.assume(&other, &family_at_b);
    kernel.assume(
        &hypothesis,
        &Term::apply(p.clone(), [Term::free_var(&b), Term::free_var(&other)]),
    );

    let term = Term::induct_match_scoped_marked(
        Term::free_var(&subject),
        Scope::close(
            Many(2),
            &[&binder(51, "i"), &binder(52, "s")],
            Term::apply(
                p.clone(),
                [
                    Term::free_var(&binder(51, "i")),
                    Term::free_var(&binder(52, "s")),
                ],
            ),
        ),
        Vec::<(&str, Vec<(Plicity, Free)>, Term)>::new(),
        Some(Term::free_var(&hypothesis)),
    );

    let Err(KernelError::Mismatch { expected, .. }) = infer(&mut kernel, &term) else {
        panic!("expected the catch-all to be refused at the scrutinee's instance");
    };
    assert_eq!(
        *expected,
        Term::apply(p, [Term::free_var(&b), Term::free_var(&subject)]),
    );
}

/// A proposition whose single constructor's payload is *pinned* by its index target — the `Eq`/`refl` shape. Matching `(z)` against a value recovers `z`, so eliminating tells a program nothing it did not already know, and the large elimination is admitted. Without this, `Eq/subst` is unstatable.
#[test]
fn a_singleton_whose_index_pins_its_payload_eliminates_into_a_type() {
    let mut kernel = kernel();
    let z = binder(0, "z");
    let arm_binder = binder(10, "z");

    let family = declare(
        &mut kernel,
        "Pinned",
        Term::prop(),
        vec![carrying("refl", z.clone(), nat_type(), Term::free_var(&z))],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        nat_type(),
        vec![(
            "refl",
            vec![arm_binder.clone()],
            Term::free_var(&arm_binder),
        )],
    );

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
}

/// The same shape with a *non-injective* index target. `blur(a)` mentions `a`, but knowing `blur(a)` recovers nothing — `blur` need not be injective, and in the program this rule exists for it is the constant zero. So the payload is not pinned, the proposition carries something a program could read back, and eliminating it into a relevant type is refused.
///
/// Reading occurrence as determination is exactly the defect this rule exists to avoid: it admits this elimination, and a closed inhabitant of `False` follows from it.
#[test]
fn a_singleton_whose_index_merely_mentions_its_payload_does_not() {
    let mut kernel = kernel();
    let a = binder(0, "a");
    let blur = binder(1, "blur");
    let arm_binder = binder(10, "a");

    kernel.declare(
        &blur,
        &Term::func_type([(binder(2, "n"), nat_type())], nat_type()),
        &UniverseContext::default(),
    );

    let family = declare(
        &mut kernel,
        "Loose",
        Term::prop(),
        vec![carrying(
            "mk",
            a.clone(),
            nat_type(),
            Term::apply(Term::free_var(&blur), [Term::free_var(&a)]),
        )],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        nat_type(),
        vec![("mk", vec![arm_binder.clone()], Term::free_var(&arm_binder))],
    );

    assert_eq!(
        infer(&mut kernel, &term),
        Err(KernelError::LargeElimination(family)),
    );
}

/// The same shape with a payload that is *itself a type*. `mk(a : Type 0)` pins nothing — the index target is the constant `0` — so eliminating into `Type` hands a program the type the proposition was built with, while irrelevance says every inhabitant is the same one. `mk({})` and `mk(False)` are convertible at a `Prop`-sorted family, `refl` therefore proves `Eq(mk({}), mk(False))`, congruence through this eliminator carries that to `Eq({}, False)`, and `subst` turns `()` into a closed inhabitant of `False`.
///
/// The rule refused a `Nat` payload and admitted this one: `carries_information` reported a component whose *type is a universe* as carrying nothing, reasoning that erasure deletes a type either way. Erasure governs what the runtime observes; irrelevance is a claim about definitional equality, and conversion reads this component back in full. No `.crs` file could reach the hole while it was open — `curios-elab`'s `singleton_eliminable` requires each binder to be `Prop`-sorted or index-pinned, and refused every surface spelling — but `recheck_module_verdicts` certified the hand-built derivation with zero refusals, which is what `crate::recheck::tests::a_derivation_through_a_type_carrying_proposition_is_refused` now holds shut.
///
/// `a_singleton_whose_index_pins_its_payload_eliminates_into_a_type` is the control: a payload the targets do recover must stay eliminable, or `Eq/subst` becomes unstatable.
#[test]
fn a_singleton_carrying_a_type_does_not_eliminate_into_a_type() {
    let mut kernel = kernel();
    let a = binder(0, "a");
    let arm_binder = binder(10, "a");

    let family = declare(
        &mut kernel,
        "TypeBox",
        Term::prop(),
        vec![carrying("mk", a, Term::type_ground(), nat(0))],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        Term::type_ground(),
        vec![("mk", vec![arm_binder.clone()], Term::free_var(&arm_binder))],
    );

    assert_eq!(
        infer(&mut kernel, &term),
        Err(KernelError::LargeElimination(family)),
    );
}

/// The other half of the same clause: a payload whose type is `Prop` holds a proposition *as data* rather than proving one. `Sort::of(Prop)` is `Type 0`, so the component is informative for exactly the reason above — `mk(True)` and `mk(False)` are convertible at a `Prop`-sorted family while this eliminator tells them apart, and transporting a proof of `True` along the resulting equality inhabits `False`.
///
/// Worth its own fixture: the clause that admitted these named both `Subterm::Type(_)` and `Subterm::Prop`, so a fix dropping only the first would leave this open.
#[test]
fn a_singleton_carrying_a_proposition_does_not_eliminate_into_a_type() {
    let mut kernel = kernel();
    let a = binder(0, "a");
    let arm_binder = binder(10, "a");

    let family = declare(
        &mut kernel,
        "PropBox",
        Term::prop(),
        vec![carrying("mk", a, Term::prop(), nat(0))],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        Term::type_ground(),
        vec![("mk", vec![arm_binder.clone()], Term::free_var(&arm_binder))],
    );

    assert_eq!(
        infer(&mut kernel, &term),
        Err(KernelError::LargeElimination(family)),
    );
}

/// An empty proposition eliminates into anything: there is nothing to have received, so nothing to extract.
#[test]
fn an_empty_proposition_eliminates_into_a_type() {
    let mut kernel = kernel();

    let family = declare(&mut kernel, "Absurd", Term::prop(), Vec::new());
    let term = eliminate(&mut kernel, &family, nat(0), nat_type(), Vec::new());

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
}

/// Two constructors mean the value says *which*, which is information a relevant result could branch on.
#[test]
fn a_proposition_with_two_constructors_does_not_eliminate_into_a_type() {
    let mut kernel = kernel();
    let (left, right) = (binder(10, "l"), binder(11, "r"));

    let family = declare(
        &mut kernel,
        "Two",
        Term::prop(),
        vec![nullary("here", nat(0)), nullary("there", nat(0))],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        nat_type(),
        vec![("here", Vec::new(), nat(1)), ("there", Vec::new(), nat(2))],
    );
    let _ = (left, right);

    assert_eq!(
        infer(&mut kernel, &term),
        Err(KernelError::LargeElimination(family)),
    );
}

/// The guard is about *relevance*, not about propositions as such: eliminating a proposition into another proposition is always fine, since irrelevance makes the result indistinguishable either way.
#[test]
fn a_proposition_eliminates_into_a_proposition_however_many_constructors() {
    let mut kernel = kernel();

    let target = declare(
        &mut kernel,
        "Target",
        Term::prop(),
        vec![nullary("only", nat(0))],
    );
    let target_type = Term::induct_type(target.clone(), Vec::<Term>::new(), [nat(0)]);

    let family = declare(
        &mut kernel,
        "Two",
        Term::prop(),
        vec![nullary("here", nat(0)), nullary("there", nat(0))],
    );

    let proof = Term::variant(target, Vec::<Term>::new(), "only", Vec::<Term>::new());
    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        target_type.clone(),
        vec![
            ("here", Vec::new(), proof.clone()),
            ("there", Vec::new(), proof),
        ],
    );

    assert_eq!(infer(&mut kernel, &term), Ok(target_type));
}

/// An arm is checked against the motive at *its own* constructor's index target, so a body of the wrong type is refused even where the elimination's overall type is fine.
#[test]
fn an_arm_body_of_the_wrong_type_is_refused() {
    let mut kernel = kernel();

    let family = declare(
        &mut kernel,
        "Flag",
        Term::type_ground(),
        vec![nullary("on", nat(0)), nullary("off", nat(0))],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        nat_type(),
        vec![
            ("on", Vec::new(), nat(1)),
            ("off", Vec::new(), Term::prim(Prim::Bool(true))),
        ],
    );

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// An arm binding the wrong number of payload components is refused: the count is the constructor's, not the arm's to choose.
#[test]
fn an_arm_of_the_wrong_payload_arity_is_refused() {
    let mut kernel = kernel();
    let value = binder(0, "value");

    let family = declare(
        &mut kernel,
        "Wrapped",
        Term::type_ground(),
        vec![carrying("mk", value, nat_type(), nat(0))],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        nat_type(),
        vec![("mk", Vec::new(), nat(1))],
    );

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(KernelError::Arity { .. }),
    ));
}
