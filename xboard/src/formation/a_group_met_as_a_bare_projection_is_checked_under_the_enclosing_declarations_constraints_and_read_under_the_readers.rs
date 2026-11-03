use {
    crate::{Date, Proof, Status, Ticket, Witness, declared, false_type, refused},
    curios_core::{
        Definition, DefinitionKind, Free, Global, Item, Level, Module, Nominal, RecGroup,
        StructDecl, StructType, Subterm, Telescope, Term, Totality, UniverseConstraint,
        UniverseConstraintKind, UniverseConstraintOrigin, UniverseContext, UniverseParam,
        Universes,
    },
    curios_utilities::Qualifier,
    std::cell::Cell,
};

pub(super) const TICKET: Ticket = Ticket {
    title: "A group met as a bare projection is checked under the enclosing declaration's constraints and read under the reader's",
    found_on: Date::new(2026, 11, 2),
    status: Status::Fixed,
    witnesses: &[Witness {
        what: "Hurkens' paradox over the products of a record a group's one member computes, licensed by `carrier@{u, v, w, x | u + 1 ≤ w, v ≤ w, v + 1 ≤ x, w + 1 ≤ x}` and read in `paradox@{u, v, w, x | u ≤ 0, w ≤ 0, 1 ≤ v}`",
        proof: Proof::Module(a_bare_group_read_under_the_readers_constraints),
        expect: refused![kernel: curios_cert::Error::Mismatch { .. } in "paradox"],
    }],
};

/// A group of one member whose body is the record of the four products Hurkens' paradox needs, declared inside `carrier` and projected inside `paradox`.
///
/// The member's terms spell `param 0 .. param 3`, which are the carrier's: `check_group` reads them as the enclosing item's, so the carrier's constraints — `u + 1 ≤ w`, `v ≤ w` — license a product over `Type u` at `Type w` and a product over a set at `Type v` at `Type w`, exactly as a declaration's cumulativity should.
///
/// This was admitted while a group bound universe parameters of its own. The four were then read two ways — as the item's by `check_group`, and as the group's by every scope walk, which shadowed them — so an instance of the carrier left the record's field types spelled in the *reader's* parameters, under the reader's constraints, `u ≤ 0` and `w ≤ 0`, where the four products quantify over `Type 0` and stand at `Type 0`. Those are the impredicative products the paradox needs, and nothing re-certified the group on the reading side: `synth_neutral` instantiated the carrier's scheme, which *is* the bare projection, and `infer`'s projection rule reduced that to the record and read the field type off it, a reduction and not a judgment.
///
/// A group binds no universe now, so the four are the carrier's under every reader and an instance substitutes them with every other level the carrier binds. The reader's `Type 0` claim meets a product that inhabits `Type 1`, and the comparison refuses it.
fn a_bare_group_read_under_the_readers_constraints() -> (Module, Term) {
    let (u, v, w, x) = (
        Level::param(UniverseParam(0)),
        Level::param(UniverseParam(1)),
        Level::param(UniverseParam(2)),
        Level::param(UniverseParam(3)),
    );
    let counter = Cell::new(2000);
    let fresh = |hint: &'static str| {
        counter.set(counter.get() + 1);
        Free::local(counter.get(), Some(hint))
    };
    let var = Term::free_var;
    let small = Term::type_at(u.clone());
    let large = Term::type_at(w.clone());
    let sized = Term::type_at(v.clone());
    let power = |set: Term, sort: &Term| Term::func_type([(fresh("s"), set)], sort.clone());

    // The record the group's member computes:
    //   { bottom: Type w, universe: Type w,
    //     over: (S: Type v, F: (℘S) -> Type u) -> Type w,
    //     pi:   (S: Type v, F: (S) -> Type u) -> Type w }
    let family =
        |set: &Free| Term::func_type([(fresh("p"), power(var(set), &small))], small.clone());
    let fibre = |set: &Free| Term::func_type([(fresh("x"), var(set))], small.clone());
    let set = fresh("S");
    let over_type = Term::func_type(
        [(set, sized.clone()), (fresh("F"), family(&set))],
        large.clone(),
    );
    let set = fresh("S");
    let pi_type = Term::func_type(
        [(set, sized.clone()), (fresh("F"), fibre(&set))],
        large.clone(),
    );
    let products_type = Term::tuple_type([
        (fresh("bottom"), large.clone()),
        (fresh("universe"), large.clone()),
        (fresh("over"), over_type),
        (fresh("pi"), pi_type),
    ]);

    let held = fresh("products");
    let group = RecGroup::closing(vec![(held, Term::type_at(x.clone()), products_type)]);
    let carrier_type = Term::rec_proj(group, 0);

    // The four products themselves, checked against the record's field types under the carrier's constraints.
    let a = fresh("A");
    let bottom_body = Term::func_type([(a, small.clone())], var(&a));
    let (big_x, f) = (fresh("X"), fresh("f"));
    let ppx = power(power(var(&big_x), &small), &small);
    let universe_body = Term::func_type(
        [
            (big_x, small.clone()),
            (f, Term::func_type([(fresh("t"), ppx.clone())], var(&big_x))),
        ],
        ppx,
    );
    let (set, big_f, p) = (fresh("S"), fresh("F"), fresh("p"));
    let over_body = Term::func(
        [(set, sized.clone()), (big_f, family(&set))],
        Term::func_type(
            [(p, power(var(&set), &small))],
            Term::apply(var(&big_f), [var(&p)]),
        ),
    );
    let (set, big_f, big_y) = (fresh("S"), fresh("F"), fresh("x"));
    let pi_body = Term::func(
        [(set, sized.clone()), (big_f, fibre(&set))],
        Term::func_type(
            [(big_y, var(&set))],
            Term::apply(var(&big_f), [var(&big_y)]),
        ),
    );
    let carrier_body = Term::tuple([bottom_body, universe_body, over_body, pi_body]);

    let cumulativity = || UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity);
    let bounded = |lower: Level, upper: Level| UniverseConstraint {
        lower,
        upper,
        origin: cumulativity(),
    };
    let raised = |level: &Level| level.clone().succ().expect("a level has a successor");

    let carrier = Global::Authored(Qualifier::from(["carrier"]));
    let definition =
        |name: Global, universe_context: UniverseContext, type_: Term, body: Term| -> Item {
            Item::Let(Box::new(Definition {
                name,
                kind: DefinitionKind::Authored,
                universe_context,
                island: Qualifier::default(),
                totality: Totality::Total,
                proof: false,
                variances: Vec::new(),
                field_variances: Vec::new(),
                parameter_field: None,
                value_sort: None,
                type_,
                body,
            }))
        };
    let carrier_item = definition(
        carrier,
        UniverseContext {
            parameter_count: 4,
            quality_count: 0,
            constraints: vec![
                bounded(raised(&u), w.clone()),
                bounded(v.clone(), w.clone()),
                bounded(raised(&v), x.clone()),
                bounded(raised(&w), x),
            ],
        },
        carrier_type,
        carrier_body,
    );

    // The reader: the same four spellings, read at an instance the carrier's own constraints are satisfied by. The carrier's body carries no group, so an instance substitutes its levels and the four products arrive over `Type 0`; the record's field types are the group's own and are shadowed, so they arrive spelled in the reader's parameters.
    let one = Level::zero().succ().expect("level has a successor");
    let two = one.clone().succ().expect("level has a successor");
    let read = Term::instance(carrier, vec![Level::zero(), one.clone(), one.clone(), two]);
    let product = |index: usize| Term::proj(read.clone(), index);

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
        &Term::type_ground(),
        &product(0),
        &product(1),
        &product(2),
        &product(3),
    );
    let body = Term::proj_struct(Term::apply(looped, [boxed_type]), boxed, 0);

    // The reader's own context: the sort the record's products quantify at and the sort they stand at are both `Type 0`, and a set stands a rung above. The carrier's `u + 1 ≤ w` says the opposite of the first two, and nothing puts the two sets together — the record arrives here spelled in these four parameters, because the group binds them and an instance of the carrier leaves them alone.
    let reading = UniverseContext {
        parameter_count: 4,
        quality_count: 0,
        constraints: vec![
            bounded(u, Level::zero()),
            bounded(w, Level::zero()),
            bounded(one.clone(), v),
        ],
    };

    let paradox = Global::Authored(Qualifier::from(["paradox"]));
    let paradox_item = definition(paradox, reading, false_type(), body);

    let module = Module {
        items: vec![
            declared(boxed, Nominal::Struct(boxed_decl)),
            carrier_item,
            paradox_item,
        ]
        .into(),
        ..Module::default()
    };

    (
        module,
        Term::instance(
            paradox,
            vec![Level::zero(), one, Level::zero(), Level::zero()],
        ),
    )
}

/// Hurkens' paradox with `star` its sort, over the four products a record hands back: `bottom`, `(A: star) -> A`; `small_universe`, its `U`; `over`, the product of a family over `℘S`; and `pi`, the product of a family over `S`. A term of `bottom`, its binders minted by `fresh`.
fn hurkens(
    fresh: &impl Fn(&'static str) -> Free,
    star: &Term,
    bottom: &Term,
    small_universe: &Term,
    over: &Term,
    pi: &Term,
) -> Term {
    let var = Term::free_var;
    let star = star.clone();
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
    // over(U, (p) => body(p)), the product over ℘U at `star`.
    let forall = |body: &dyn Fn(&Free) -> Term| {
        let p = fresh("p");
        Term::apply(
            over.clone(),
            [universe.clone(), Term::func([(p, pu.clone())], body(&p))],
        )
    };
    // pi(U, (x) => body(x)), the product over U at `star`.
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
