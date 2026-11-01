use {
    crate::{Date, Proof, Status, Ticket, Witness, declared, false_type, refused},
    curios_core::{
        Definition, DefinitionKind, Free, Global, Instance, InstanceHead, Item, Level, Many,
        Module, Nominal, RecGroup, RecMemberScopes, Scope, StructDecl, StructType, Subterm,
        Telescope, Term, Totality, UniverseConstraint, UniverseConstraintKind,
        UniverseConstraintOrigin, UniverseContext, UniverseParam, Universes,
    },
    curios_utilities::Qualifier,
    std::cell::Cell,
};

pub(super) const TICKET: Ticket = Ticket {
    title: "A group's instance is held to the group's constraints and its bodies were checked under the enclosing declaration's",
    found_on: Date::new(2026, 10, 30),
    status: Status::Fixed,
    witnesses: &[Witness {
        what: "Hurkens' paradox over the products of a group stating no constraint, instantiated at `{0, 0, 1}` inside a declaration that assumes `u + 1 ≤ w` and `v ≤ w`",
        proof: Proof::Module(a_group_checked_under_the_enclosing_constraints),
        expect: refused![kernel: curios_cert::Error::Mismatch { .. } in "/paradox"],
    }],
};

/// A group of four products over `Type u` or a set at `Type v`, each declared at `Type w` and the group stating three levels and no constraint, read at `{0, 0, 1}` inside `paradox@{u, w, v | u + 1 ≤ w, v ≤ w}`: four products over `Type 0` and over sets at `Type 1`, each at `Type 0`, the impredicative ones Hurkens' paradox needs, over which it closes with `False`.
///
/// The instance rule certifies the group at its generic spelling where the instance stands, under whatever constraints the enclosing declaration assumed, and holds the instance to the group's own context alone. The group's levels and the declaration's are both `UniverseParam` by position, so a product over `Type u` or over a set at `Type v` is admitted at `Type w` by the declaration's constraints, and the instance `{0, 0, 1}` meets the group's empty context.
fn a_group_checked_under_the_enclosing_constraints() -> (Module, Term) {
    let (u, w, v) = (
        Level::param(UniverseParam(0)),
        Level::param(UniverseParam(1)),
        Level::param(UniverseParam(2)),
    );
    let counter = Cell::new(1000);
    let fresh = |hint: &'static str| {
        counter.set(counter.get() + 1);
        Free::local(counter.get(), Some(hint))
    };
    let var = Term::free_var;
    let small = Term::type_at(u.clone());
    let large = Term::type_at(w.clone());
    let sized = Term::type_at(v.clone());

    // bottom: Type w = (A: Type u) -> A.
    // universe: Type w = (X: Type u, f: (℘℘X) -> X) -> ℘℘X, ℘S = (S) -> Type u.
    // over: (S: Type v, F: (℘S) -> Type u) -> Type w = (S, F) => (p: ℘S) -> F(p).
    // pi: (S: Type v, F: (S) -> Type u) -> Type w = (S, F) => (x: S) -> F(x).
    let (bottom, universe, over, pi) = (
        fresh("bottom"),
        fresh("universe"),
        fresh("over"),
        fresh("pi"),
    );
    let power = |set: Term, sort: &Term| Term::func_type([(fresh("s"), set)], sort.clone());
    let a = fresh("A");
    let bottom_body = Term::func_type([(a, small.clone())], var(&a));
    let (x, f) = (fresh("X"), fresh("f"));
    let ppx = power(power(var(&x), &small), &small);
    let universe_body = Term::func_type(
        [
            (x, small.clone()),
            (f, Term::func_type([(fresh("t"), ppx.clone())], var(&x))),
        ],
        ppx,
    );
    let family =
        |set: &Free| Term::func_type([(fresh("p"), power(var(set), &small))], small.clone());
    let set = fresh("S");
    let over_type = Term::func_type(
        [(set, sized.clone()), (fresh("F"), family(&set))],
        large.clone(),
    );
    let (set, big_f, p) = (fresh("S"), fresh("F"), fresh("p"));
    let over_body = Term::func(
        [(set, sized.clone()), (big_f, family(&set))],
        Term::func_type(
            [(p, power(var(&set), &small))],
            Term::apply(var(&big_f), [var(&p)]),
        ),
    );
    let set = fresh("S");
    let fibre = |set: &Free| Term::func_type([(fresh("x"), var(set))], small.clone());
    let pi_type = Term::func_type(
        [(set, sized.clone()), (fresh("F"), fibre(&set))],
        large.clone(),
    );
    let (set, big_f, x) = (fresh("S"), fresh("F"), fresh("x"));
    let pi_body = Term::func(
        [(set, sized.clone()), (big_f, fibre(&set))],
        Term::func_type([(x, var(&set))], Term::apply(var(&big_f), [var(&x)])),
    );
    let members = [&bottom, &universe, &over, &pi];
    let scoped = |term: Term| Scope::close(Many(4), &members, term);
    let group = RecGroup::new(vec![
        RecMemberScopes {
            type_: scoped(large.clone()),
            body: scoped(bottom_body),
        },
        RecMemberScopes {
            type_: scoped(large.clone()),
            body: scoped(universe_body),
        },
        RecMemberScopes {
            type_: scoped(over_type),
            body: scoped(over_body),
        },
        RecMemberScopes {
            type_: scoped(pi_type),
            body: scoped(pi_body),
        },
    ])
    .with_universe_context(UniverseContext {
        parameter_count: 3,
        quality_count: 0,
        constraints: Vec::new(),
    });
    let one = Level::zero().succ().expect("level has a successor");
    let collapsed = |index: usize| {
        Term::from(Subterm::Instance(Instance {
            head: InstanceHead::RecProj(group.clone(), index),
            universes: vec![Level::zero(), Level::zero(), one.clone()].into(),
        }))
    };

    let boxed = Global::Authored(Qualifier::from(["Boxed"]));
    let boxed_decl = StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::build([(fresh("proof"), false_type())], ())),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    let boxed_type: Term = Subterm::StructType(StructType {
        name: boxed,
        universes: Universes::default(),
        params: Vec::new(),
    })
    .into();

    let looped = hurkens(
        &fresh,
        &collapsed(0),
        &collapsed(1),
        &collapsed(2),
        &collapsed(3),
    );
    let body = Term::proj_struct(Term::apply(looped, [boxed_type]), boxed, 0);

    let paradox = Global::Authored(Qualifier::from(["paradox"]));
    let cumulativity = UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity);
    let definition = Item::Let(Box::new(Definition {
        name: paradox,
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext {
            parameter_count: 3,
            quality_count: 0,
            constraints: vec![
                UniverseConstraint {
                    lower: u.succ().expect("level has a successor"),
                    upper: w.clone(),
                    origin: cumulativity.clone(),
                },
                UniverseConstraint {
                    lower: v,
                    upper: w,
                    origin: cumulativity,
                },
            ],
        },
        island: Qualifier::default(),
        totality: Totality::Total,
        proof: false,
        value_sort: None,
        variances: Vec::new(),
        field_variances: Vec::new(),
        parameter_field: None,
        type_: false_type(),
        body,
    }));

    let module = Module {
        items: vec![declared(boxed, Nominal::Struct(boxed_decl)), definition].into(),
        ..Module::default()
    };

    (
        module,
        Term::instance_of(&Free::from(&paradox), vec![Level::zero(), one.clone(), one]),
    )
}

/// Hurkens' paradox with `Type 0` its two sorts, over the products a collapsed group stands at `Type 0`: `bottom`, `(A: Type 0) -> A`; `small_universe`, its `U`; `over`, the product of a family over `℘S`; and `pi`, the product of a family over `S`. A term of `bottom`, its binders minted by `fresh`.
fn hurkens(
    fresh: &impl Fn(&'static str) -> Free,
    bottom: &Term,
    small_universe: &Term,
    over: &Term,
    pi: &Term,
) -> Term {
    let var = Term::free_var;
    let star = Term::type_ground();
    let not = |phi: Term| Term::func_type([(fresh("h"), phi)], bottom.clone());
    let power = |set: Term| Term::func_type([(fresh("s"), set)], star.clone());
    // U as the product it unfolds to, which a binder of it is typed at: a neutral is classified by its head's type, read without unfolding a member.
    let universe = {
        let (x, f) = (fresh("X"), fresh("f"));
        let ppx = power(power(var(&x)));
        Term::func_type(
            [
                (x, star.clone()),
                (f, Term::func_type([(fresh("t"), ppx.clone())], var(&x))),
            ],
            ppx,
        )
    };
    let pu = power(universe.clone());
    let ppu = power(pu.clone());
    // over(U, (p) => body(p)), the product over ℘U at `Type 0`.
    let forall = |body: &dyn Fn(&Free) -> Term| {
        let p = fresh("p");
        Term::apply(
            over.clone(),
            [universe.clone(), Term::func([(p, pu.clone())], body(&p))],
        )
    };
    // pi(U, (x) => body(x)), the product over U at `Type 0`.
    let every = |body: &dyn Fn(&Free) -> Term| {
        let x = fresh("x");
        Term::apply(
            pi.clone(),
            [
                universe.clone(),
                Term::func([(x, universe.clone())], body(&x)),
            ],
        )
    };

    // τ t = (X, f) => (p) => t((x) => p(f(x(X, f)))).
    let tau = {
        let (t, x, f, p, y) = (fresh("t"), fresh("X"), fresh("f"), fresh("p"), fresh("x"));
        let ppx = power(power(var(&x)));
        Term::func(
            [(t, ppu.clone())],
            Term::func(
                [
                    (x, star.clone()),
                    (f, Term::func_type([(fresh("t"), ppx)], var(&x))),
                ],
                Term::func(
                    [(p, power(var(&x)))],
                    Term::apply(
                        var(&t),
                        [Term::func(
                            [(y, universe.clone())],
                            Term::apply(
                                var(&p),
                                [Term::apply(
                                    var(&f),
                                    [Term::apply(var(&y), [var(&x), var(&f)])],
                                )],
                            ),
                        )],
                    ),
                ),
            ),
        )
    };
    let tau_at = |t: Term| Term::apply(tau.clone(), [t]);

    // σ s = s(U, (t) => τ t).
    let sigma = {
        let (s, t) = (fresh("s"), fresh("t"));
        Term::func(
            [(s, universe.clone())],
            Term::apply(
                var(&s),
                [
                    small_universe.clone(),
                    Term::func([(t, ppu.clone())], tau_at(var(&t))),
                ],
            ),
        )
    };
    let sigma_at = |s: Term| Term::apply(sigma.clone(), [s]);
    let holds = |s: Term, p: Term| Term::apply(sigma_at(s), [p]);
    let round = |s: Term| tau_at(sigma_at(s));

    // (y) => p(τ(σ y)).
    let shifted = |p: &Free| {
        let y = fresh("y");
        Term::func(
            [(y, universe.clone())],
            Term::apply(var(p), [round(var(&y))]),
        )
    };
    // over((p) => (h: σ s p) -> p(τ(σ s))).
    let claim = |s: &Term| {
        forall(&|p| {
            Term::func_type(
                [(fresh("h"), holds(s.clone(), var(p)))],
                Term::apply(var(p), [round(s.clone())]),
            )
        })
    };

    // Δ = (y) => ¬claim(y).
    let delta = {
        let y = fresh("y");
        Term::func([(y, universe.clone())], not(claim(&var(&y))))
    };

    // Ω = τ((p) => pi(U, (x) => (h: σ x p) -> p x)).
    let omega = {
        let p = fresh("p");
        tau_at(Term::func(
            [(p, pu.clone())],
            every(&|x| {
                Term::func_type(
                    [(fresh("h"), holds(var(x), var(&p)))],
                    Term::apply(var(&p), [var(x)]),
                )
            }),
        ))
    };

    // lem1 = (p, h1) => h1(Ω)((x) => (h2) => h1(τ(σ x))(h2)).
    let lem1 = {
        let (p, h1, x, h2, x1) = (fresh("p"), fresh("h1"), fresh("x"), fresh("h2"), fresh("x"));
        let premise = Term::func_type(
            [(x1, universe.clone())],
            Term::func_type(
                [(fresh("h"), holds(var(&x1), var(&p)))],
                Term::apply(var(&p), [var(&x1)]),
            ),
        );
        Term::func(
            [(p, pu.clone()), (h1, premise)],
            Term::apply(
                Term::apply(var(&h1), [omega.clone()]),
                [Term::func(
                    [(x, universe.clone())],
                    Term::func(
                        [(h2, holds(var(&x), shifted(&p)))],
                        Term::apply(Term::apply(var(&h1), [round(var(&x))]), [var(&h2)]),
                    ),
                )],
            ),
        )
    };

    // lem2 = lem1(Δ, (x) => (h2) => (h3) => h3(Δ)(h2)((p) => (h4) => h3((y) => p(τ(σ y)))(h4))).
    let lem2 = {
        let (x, h2, h3, p, h4) = (
            fresh("x"),
            fresh("h2"),
            fresh("h3"),
            fresh("p"),
            fresh("h4"),
        );
        Term::apply(
            lem1.clone(),
            [
                delta.clone(),
                Term::func(
                    [(x, universe.clone())],
                    Term::func(
                        [(h2, holds(var(&x), delta.clone()))],
                        Term::func(
                            [(h3, claim(&var(&x)))],
                            Term::apply(
                                Term::apply(Term::apply(var(&h3), [delta.clone()]), [var(&h2)]),
                                [Term::func(
                                    [(p, pu.clone())],
                                    Term::func(
                                        [(h4, holds(round(var(&x)), var(&p)))],
                                        Term::apply(
                                            Term::apply(var(&h3), [shifted(&p)]),
                                            [var(&h4)],
                                        ),
                                    ),
                                )],
                            ),
                        ),
                    ),
                ),
            ],
        )
    };

    // lem3 = (p) => (h) => lem1((y) => p(τ(σ y)), h).
    let lem3 = {
        let (p, h) = (fresh("p"), fresh("h"));
        Term::func(
            [(p, pu.clone())],
            Term::func(
                [(h, holds(omega.clone(), var(&p)))],
                Term::apply(lem1.clone(), [shifted(&p), var(&h)]),
            ),
        )
    };

    Term::apply(lem2, [lem3])
}
