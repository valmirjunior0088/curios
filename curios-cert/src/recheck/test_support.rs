//! The hand-built adversarial modules the recheck suites forge, and the declarations they forge them from.
//!
//! Built by hand because a refusal the elaborator reaches first leaves no module behind, so a rule where `curios-elab` is the stricter of the two cannot be put to this crate by any surface program. Reaching it means constructing the finished module here and asking `certify_definitions` directly.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across `recheck`, and nothing outside it.

use {
    crate::{test_support::*, *},
    curios_abi::{ForeignFunction, WireReturn, WireSignature, WireType},
    curios_analysis::test_support::SYNTAX,
    curios_common::{Candidates, Plicity, Qualifier, SyntaxRegistry},
    curios_core::{
        Atom, Certification, Certified, Definition, DefinitionKind, Definitions, Entrypoint, Free,
        Global, InductDecl, InductParam, Intrinsic, Item, Level, Many, Nat, Nominal, Places,
        Polarity, Program, Reads, RecGroup, RecItem, Scope, StructDecl, StructType, Subterm,
        Telescope, Term, UniverseConstraint, UniverseContext, UniverseParam, Universes, Variance,
        test_support::*,
    },
    std::{
        collections::{BTreeMap, BTreeSet},
        sync::Arc,
    },
};

/// A top-level definition, as `certify_definitions` binds one.
pub fn authored(name: &Global, type_: Term, body: Term) -> Item {
    Item::Let(Box::new(Definition {
        name: *name,
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_,
        body,
    }))
}

/// [`authored`] marked a proof, as the module a walk hands back marks one its declaration wrote at a proposition: what a unit in scope holds.
pub fn authored_proof(name: &Global, type_: Term, body: Term) -> Item {
    match authored(name, type_, body) {
        Item::Let(definition) => Item::Let(Box::new(Definition {
            proof: true,
            ..*definition
        })),
        item => item,
    }
}

/// A nullary `Prop`-sorted family: `False` itself, and the shape `Box` takes but for its payload.
pub fn proposition(constructors: Vec<(Atom, InductParam)>) -> InductDecl {
    InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: constructors.into(),
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

/// `induct Eq(@A : Type 1) : (x : A, y : A) -> Prop 1 | refl(z : A) : (z, z) end` — the equality the forgeries transport along, a proposition at its carrier's level, where a value of the carrier fits.
pub fn equality_declaration() -> InductDecl {
    let type_1 = Term::type_at(Level::zero().succ().expect("level zero has a successor"));
    let carrier = Free::local(20, Some("A"));
    let left = Free::local(21, Some("x"));
    let right = Free::local(22, Some("y"));
    let value = Free::local(23, Some("z"));

    let mut declaration = proposition(vec![(
        Atom::from("refl"),
        InductParam::new(
            Telescope::build(
                [(carrier, type_1.clone()), (value, Term::free_var(&carrier))],
                vec![Term::free_var(&value), Term::free_var(&value)],
            ),
            vec![Plicity::Implicit, Plicity::Explicit],
        ),
    )]);
    declaration.result_sort =
        Term::prop_at(Level::zero().succ().expect("level zero has a successor"));
    declaration.arity = Telescope::build(
        [(carrier, type_1.clone())],
        Telescope::build(
            [
                (left, Term::free_var(&carrier)),
                (right, Term::free_var(&carrier)),
            ],
            (),
        ),
    );

    declaration
}

/// The module the doc comment above describes: three declarations, and the five definitions that close on `False`.
pub fn forgery() -> Program {
    let type_0 = Term::type_ground();

    let false_name = Global::Authored(Qualifier::from(["False"]));
    let box_name = Global::Authored(Qualifier::from(["Box"]));
    let equality_name = Global::Authored(Qualifier::from(["Eq"]));

    let false_type = Term::induct_type(false_name, Vec::<Term>::new(), Vec::<Term>::new());
    let box_type = Term::induct_type(box_name, Vec::<Term>::new(), Vec::<Term>::new());

    let boxed = |carried: Term| Term::variant(box_name, Vec::<Term>::new(), "mk", [carried]);
    let equality = |carrier: Term, left: Term, right: Term| {
        Term::induct_type(equality_name, [carrier], [left, right])
    };
    let reflexivity =
        |carrier: Term, value: Term| Term::variant(equality_name, [carrier], "refl", [value]);

    // induct Box : Prop 1 | mk(a : Type 0) end — at the level its payload fits under, so the declaration stands and the guard is what refuses `unbox`.
    let payload = Free::local(10, Some("a"));
    let mut box_decl = proposition(vec![(
        Atom::from("mk"),
        InductParam::new(
            Telescope::build([(payload, type_0.clone())], Vec::new()),
            vec![Plicity::Explicit],
        ),
    )]);
    box_decl.result_sort = Term::prop_at(Level::zero().succ().expect("level zero has a successor"));

    let equality_decl = equality_declaration();

    // unbox : (Box) -> Type 0 = (b) => match b : (_) => Type 0 | mk(a) => a end
    let unbox_name = Global::Authored(Qualifier::from(["unbox"]));
    let subject = Free::local(30, Some("b"));
    let scrutinee = Free::local(31, Some("s"));
    let opened = Free::local(32, Some("a"));
    let unbox = authored(
        &unbox_name,
        Term::func_type(
            [(Free::local(33, Some("b")), box_type.clone())],
            type_0.clone(),
        ),
        Term::func(
            [(subject, box_type.clone())],
            Term::induct_match_scoped_marked(
                Term::free_var(&subject),
                Scope::close(Many(1), &[&scrutinee], type_0.clone()),
                [(
                    "mk",
                    vec![(Plicity::Explicit, opened)],
                    Term::free_var(&opened),
                )],
                None,
            ),
        ),
    );
    let unboxed =
        |carried: Term| Term::apply(Term::instance(unbox_name, Universes::default()), [carried]);

    // boxes_equal : (A : Type 0, B : Type 0) -> Eq(Box, mk(A), mk(B)) = refl(mk(A))
    let boxes_equal_name = Global::Authored(Qualifier::from(["boxes_equal"]));
    let this = Free::local(40, Some("A"));
    let that = Free::local(41, Some("B"));
    let boxes_equal = authored(
        &boxes_equal_name,
        Term::func_type(
            [(this, type_0.clone()), (that, type_0.clone())],
            equality(
                box_type.clone(),
                boxed(Term::free_var(&this)),
                boxed(Term::free_var(&that)),
            ),
        ),
        Term::func(
            [(this, type_0.clone()), (that, type_0.clone())],
            reflexivity(box_type.clone(), boxed(Term::free_var(&this))),
        ),
    );

    // types_equal : (A : Type 0, B : Type 0) -> Eq(Type 0, A, B)
    //   = match boxes_equal(A, B) : (x, y, _) => Eq(Type 0, unbox(x), unbox(y))
    //     | refl(z) => refl(unbox(z)) end
    let types_equal_name = Global::Authored(Qualifier::from(["types_equal"]));
    let source = Free::local(50, Some("A"));
    let target = Free::local(51, Some("B"));
    let motive_left = Free::local(52, Some("x"));
    let motive_right = Free::local(53, Some("y"));
    let motive_proof = Free::local(54, Some("q"));
    let arm_value = Free::local(55, Some("z"));
    let types_equal = authored(
        &types_equal_name,
        Term::func_type(
            [(source, type_0.clone()), (target, type_0.clone())],
            equality(
                type_0.clone(),
                Term::free_var(&source),
                Term::free_var(&target),
            ),
        ),
        Term::func(
            [(source, type_0.clone()), (target, type_0.clone())],
            Term::induct_match_scoped_marked(
                Term::apply(
                    Term::instance(boxes_equal_name, Universes::default()),
                    [Term::free_var(&source), Term::free_var(&target)],
                ),
                Scope::close(
                    Many(3),
                    &[&motive_left, &motive_right, &motive_proof],
                    equality(
                        type_0.clone(),
                        unboxed(Term::free_var(&motive_left)),
                        unboxed(Term::free_var(&motive_right)),
                    ),
                ),
                [(
                    "refl",
                    vec![(Plicity::Explicit, arm_value)],
                    reflexivity(type_0.clone(), unboxed(Term::free_var(&arm_value))),
                )],
                None,
            ),
        ),
    );

    // cast : (A : Type 0, B : Type 0, v : A) -> B
    //   = (match types_equal(A, B) : (x, y, _) => (x) -> y | refl(z) => (w) => w end)(v)
    let cast_name = Global::Authored(Qualifier::from(["cast"]));
    let from = Free::local(60, Some("A"));
    let into = Free::local(61, Some("B"));
    let carried = Free::local(62, Some("v"));
    let cast_left = Free::local(63, Some("x"));
    let cast_right = Free::local(64, Some("y"));
    let cast_proof = Free::local(65, Some("q"));
    let cast_value = Free::local(66, Some("z"));
    let coerced = Free::local(67, Some("w"));
    let identity = Free::local(68, Some("w"));
    let cast_params = [
        (from, type_0.clone()),
        (into, type_0.clone()),
        (carried, Term::free_var(&from)),
    ];
    let cast = authored(
        &cast_name,
        Term::func_type(cast_params.clone(), Term::free_var(&into)),
        Term::func(
            cast_params,
            Term::apply(
                Term::induct_match_scoped_marked(
                    Term::apply(
                        Term::instance(types_equal_name, Universes::default()),
                        [Term::free_var(&from), Term::free_var(&into)],
                    ),
                    Scope::close(
                        Many(3),
                        &[&cast_left, &cast_right, &cast_proof],
                        Term::func_type(
                            [(coerced, Term::free_var(&cast_left))],
                            Term::free_var(&cast_right),
                        ),
                    ),
                    [(
                        "refl",
                        vec![(Plicity::Explicit, cast_value)],
                        Term::func(
                            [(identity, Term::free_var(&cast_value))],
                            Term::free_var(&identity),
                        ),
                    )],
                    None,
                ),
                [Term::free_var(&carried)],
            ),
        ),
    );

    // forged : False = cast({}, False, ())
    let forged_name = Global::Authored(Qualifier::from(["forged"]));
    let forged = authored(
        &forged_name,
        false_type.clone(),
        Term::apply(
            Term::instance(cast_name, Universes::default()),
            [
                Term::tuple_type_unit(),
                false_type.clone(),
                Term::tuple(Vec::<Term>::new()),
            ],
        ),
    );

    Program {
        definitions: declaring(
            Definitions {
                items: vec![unbox, boxes_equal, types_equal, cast, forged].into(),
            },
            BTreeMap::from([
                (false_name, proposition(Vec::new())),
                (box_name, box_decl),
                (equality_name, equality_decl),
            ]),
            [],
            [],
        ),
        // The program as a whole is closed, at type `False`.
        entrypoint: Entrypoint {
            body: Term::instance(forged_name, Universes::default()),
            type_: Some(false_type),
        },
    }
}

/// `sink : (Nat) -> Nat`, honestly stamped `Partial`: its body projects `rec f : (Nat) -> Nat = (n) => f(n)`, which does not descend.
pub fn diverging_sink() -> Item {
    diverging("sink")
}

/// [`diverging_sink`] under another name.
pub fn diverging(name: &str) -> Item {
    let f = Free::local(920, Some("f"));
    let n = Free::local(921, Some("n"));
    let nat = Term::intrinsic(Intrinsic::NatType);

    let group = RecGroup::closing(vec![(
        f,
        Term::func_type([(n, nat.clone())], nat.clone()),
        Term::func(
            [(n, nat.clone())],
            Term::apply(Term::free_var(&f), [Term::free_var(&n)]),
        ),
    )]);

    authored(
        &Global::Authored(Qualifier::from([name])),
        Term::func_type([(Free::local(922, Some("n")), nat.clone())], nat),
        Term::rec_proj(group, 0),
    )
}

/// `induct Vouched : Prop | qed(u : {})`, the proposition the proof below inhabits.
pub fn vouched_declaration() -> (Global, InductDecl) {
    let name = Global::Authored(Qualifier::from(["Vouched"]));
    let declaration = InductDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("qed"),
            InductParam::new(
                Telescope::build(
                    [(Free::local(925, Some("u")), Term::tuple_type_unit())],
                    Vec::new(),
                ),
                vec![Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    (name, declaration)
}

/// What `proof_carrying_unit`'s proof carries at `Io({})`.
pub enum Carried {
    /// `exit(@{}, 0)`: a host row that diverges.
    Exit,
    /// `Io/pure(())`: nothing that fails to terminate.
    Pure,
    /// `rec wait : Io({}) = wait; wait`: a group that never descends, at a type relevant enough that its own gate lets it stand.
    Wait,
}

/// `induct Held : Prop | qed(u : Io({}))`, with `bad : Held` built from what `carried` names.
pub fn proof_carrying_unit(carried: Carried) -> Definitions {
    let held_name = Global::Authored(Qualifier::from(["Held"]));
    let held = Term::induct_type(held_name, Vec::<Term>::new(), Vec::<Term>::new());

    let declaration = InductDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("qed"),
            InductParam::new(
                Telescope::build(
                    [(
                        Free::local(910, Some("u")),
                        Term::intrinsic(Intrinsic::io_type(Term::tuple_type_unit())),
                    )],
                    Vec::new(),
                ),
                vec![Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let payload = match carried {
        Carried::Exit => Term::foreign(
            Arc::new(ForeignFunction {
                name: "/exit".to_string(),
                signature: WireSignature {
                    params: vec![WireType::Byte],
                    returns: WireReturn::Never,
                },
            }),
            Global::Authored(Qualifier::from(["exit"])),
            vec![Term::tuple_type_unit(), Term::intrinsic(Intrinsic::Byte(0))],
        ),
        Carried::Pure => Term::intrinsic(Intrinsic::io_pure(
            Term::tuple_type_unit(),
            Term::tuple(Vec::<Term>::new()),
        )),
        Carried::Wait => {
            let wait = Free::local(911, Some("wait"));
            Term::rec(
                [(
                    wait,
                    Term::intrinsic(Intrinsic::io_type(Term::tuple_type_unit())),
                    Term::free_var(&wait),
                )],
                Term::free_var(&wait),
            )
        }
    };

    let mut induct_decls = BTreeMap::new();
    induct_decls.insert(held_name, declaration);

    declaring(
        Definitions {
            items: vec![authored(
                &Global::Authored(Qualifier::from(["bad"])),
                held,
                Term::variant(held_name, Vec::<Term>::new(), "qed", [payload]),
            )]
            .into(),
        },
        induct_decls,
        [],
        [],
    )
}

/// `Held : (n : Nat) -> Type 0 | yes() : (0)`, the family whose two instances the forged equation coerces between.
pub fn indexed_family(index: Free, nat_type: Term, zero: Term, result_sort: Term) -> InductDecl {
    InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build([(index, nat_type)], ())),
        constructors: vec![(
            Atom::from("yes"),
            InductParam::new(Telescope::done(vec![zero]), Vec::new()),
        )]
        .into(),
        result_sort,
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

/// `struct Wrap(b : Bool) : Prop { held : match b : Prop | false => .. | true => .. end }` — a structure whose field type is a stuck `match` claiming, through its motive, to be a proposition.
pub fn computed_field_wrapper(false_case: Term, true_case: Term) -> StructDecl {
    let scrutinee = Free::local(60, Some("b"));
    let field = Term::bool_match(
        Term::free_var(&scrutinee),
        None,
        Term::prop(),
        false_case,
        true_case,
    );

    StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::build(
            [(scrutinee, Term::intrinsic(Intrinsic::BoolType))],
            Telescope::build([(Free::local(61, Some("held")), field)], ()),
        ),
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

/// `Wrap(true)`, the instance at which the field type above reduces to its true arm.
pub fn wrap_at_true(wrap_name: &Global) -> Term {
    Subterm::StructType(StructType {
        name: *wrap_name,
        universes: Universes::default(),
        params: vec![Term::intrinsic(Intrinsic::Bool(true))],
    })
    .into()
}

/// `wrapped : Wrap(true) = Wrap(true){held}`.
pub fn wrapped_at_true(wrap_name: &Global, held: Term) -> Item {
    authored(
        &Global::Authored(Qualifier::from(["wrapped"])),
        wrap_at_true(wrap_name),
        Term::struct_(*wrap_name, [Term::intrinsic(Intrinsic::Bool(true))], [held]),
    )
}

/// [`index_forgery`]'s transport, reached through a `Prop`-sorted structure that really carries a `Nat`.
pub fn computed_field_forgery() -> Definitions {
    let type_0 = Term::type_ground();
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let nat = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));

    let false_name = Global::Authored(Qualifier::from(["False"]));
    let equality_name = Global::Authored(Qualifier::from(["Eq"]));
    let held_name = Global::Authored(Qualifier::from(["Held"]));
    let wrap_name = Global::Authored(Qualifier::from(["Wrap"]));

    let false_type = Term::induct_type(false_name, Vec::<Term>::new(), Vec::<Term>::new());
    let held_at = |index: Term| Term::induct_type(held_name, Vec::<Term>::new(), [index]);
    let yes = Term::variant(held_name, Vec::<Term>::new(), "yes", Vec::<Term>::new());
    let wrap = wrap_at_true(&wrap_name);
    let wrapping =
        |held: Term| Term::struct_(wrap_name, [Term::intrinsic(Intrinsic::Bool(true))], [held]);

    let false_decl = proposition(Vec::new());
    let held_decl = indexed_family(
        Free::local(70, Some("n")),
        nat_type.clone(),
        nat(0),
        type_0.clone(),
    );

    // forged : Eq(Wrap(true), Wrap(true){0}, Wrap(true){1}) = refl(Wrap(true), Wrap(true){0})
    let forged_name = Global::Authored(Qualifier::from(["forged"]));
    let forged = authored(
        &forged_name,
        Term::induct_type(
            equality_name,
            [wrap.clone()],
            [wrapping(nat(0)), wrapping(nat(1))],
        ),
        Term::variant(equality_name, [wrap], "refl", [wrapping(nat(0))]),
    );

    // cast : (Held(0)) -> Held(1)
    //   = match forged : (s, t, q) => (Held(s.0)) -> Held(t.0) | refl(z) => (w) => w end
    let cast_name = Global::Authored(Qualifier::from(["cast"]));
    let motive_left = Free::local(80, Some("s"));
    let motive_right = Free::local(81, Some("t"));
    let motive_proof = Free::local(82, Some("q"));
    let arm_value = Free::local(83, Some("z"));
    let carried = Free::local(84, Some("w"));
    let identity = Free::local(85, Some("w"));
    let cast = authored(
        &cast_name,
        Term::func_type([(carried, held_at(nat(0)))], held_at(nat(1))),
        Term::induct_match_scoped_marked(
            Term::instance(forged_name, Universes::default()),
            Scope::close(
                Many(3),
                &[&motive_left, &motive_right, &motive_proof],
                Term::func_type(
                    [(
                        carried,
                        held_at(Term::proj_struct(
                            Term::free_var(&motive_left),
                            wrap_name,
                            0,
                        )),
                    )],
                    held_at(Term::proj_struct(
                        Term::free_var(&motive_right),
                        wrap_name,
                        0,
                    )),
                ),
            ),
            [(
                "refl",
                vec![(Plicity::Explicit, arm_value)],
                Term::func(
                    [(
                        identity,
                        held_at(Term::proj_struct(Term::free_var(&arm_value), wrap_name, 0)),
                    )],
                    Term::free_var(&identity),
                ),
            )],
            None,
        ),
    );

    // held : Held(1) = cast(yes())
    let held_value_name = Global::Authored(Qualifier::from(["held"]));
    let held_value = authored(
        &held_value_name,
        held_at(nat(1)),
        Term::apply(Term::instance(cast_name, Universes::default()), [yes]),
    );

    // boom : False = match held : (n, w) => False end
    let boom_index = Free::local(90, Some("n"));
    let boom_scrutinee = Free::local(91, Some("w"));
    let boom = authored(
        &Global::Authored(Qualifier::from(["boom"])),
        false_type.clone(),
        Term::induct_match_scoped_marked(
            Term::instance(held_value_name, Universes::default()),
            Scope::close(Many(2), &[&boom_index, &boom_scrutinee], false_type.clone()),
            Vec::<(&str, Vec<(Plicity, Free)>, Term)>::new(),
            None,
        ),
    );

    declaring(
        Definitions {
            items: vec![forged, cast, held_value, boom].into(),
        },
        BTreeMap::from([
            (false_name, false_decl),
            (equality_name, equality_declaration()),
            (held_name, held_decl),
        ]),
        BTreeMap::from([(
            wrap_name,
            computed_field_wrapper(nat_type.clone(), nat_type),
        )]),
        [],
    )
}

/// The scheme both universe-occurrence fixtures below are built on: `def A<u, v | u + 1 <= v> : Type v = Type u`.
///
/// Its constraint is load-bearing rather than decorative, which is the whole point of the shape: checking the body needs `Type (u + 1) <= Type v`, and that holds under this hypothesis and under nothing else. So an occurrence of `A` is legitimate exactly where it discharges the constraint at its own levels, and [`Certifier::check_instance`](crate::Certifier) is what does that.
pub fn scheme_context() -> UniverseContext {
    let ordered = UniverseConstraint {
        lower: Level::param(UniverseParam(0))
            .checked_add(1)
            .expect("level admits the offset"),
        upper: Level::param(UniverseParam(1)),
    };

    UniverseContext {
        parameter_count: 2,
        quality_count: 0,
        constraints: vec![ordered],
    }
}

/// The same two parameters with the scheme's constraint *not* declared — the context a user of `A` stands in when it has assumed nothing about its own levels.
pub fn open_context() -> UniverseContext {
    UniverseContext {
        parameter_count: 2,
        quality_count: 0,
        constraints: Vec::new(),
    }
}

/// The scheme above, followed by one `user` that spells an occurrence of it.
///
/// `user` is `(its own universe context, the term it is defined as)`, and its declared type is `Type v` throughout — so the two spellings of the occurrence differ in nothing but whether the instance is stated.
pub fn universe_scheme_definitions(user: Option<(UniverseContext, Term)>) -> Definitions {
    let scheme = Definition {
        name: Global::Authored(Qualifier::from(["A"])),
        kind: DefinitionKind::Authored,
        universe_context: scheme_context(),
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: Term::type_at(Level::param(UniverseParam(1))),
        body: Term::type_at(Level::param(UniverseParam(0))),
    };

    let mut items = vec![Item::Let(Box::new(scheme))];
    if let Some((universe_context, body)) = user {
        items.push(Item::Let(Box::new(Definition {
            name: Global::Authored(Qualifier::from(["user"])),
            kind: DefinitionKind::Authored,
            universe_context,
            island: Qualifier::default(),
            proof: false,
            value_sort: None,
            type_: Term::type_at(Level::param(UniverseParam(1))),
            body,
        })));
    }

    Definitions {
        items: items.into(),
    }
}

/// `A` alone, and `A` at the using item's own two parameters — the two spellings of one occurrence.
pub fn scheme_occurrences() -> (Term, Term) {
    let name = Global::Authored(Qualifier::from(["A"]));
    let bare = Term::instance(name, Universes::default());
    let instance = Term::instance(
        name,
        vec![
            Level::param(UniverseParam(0)),
            Level::param(UniverseParam(1)),
        ],
    );

    (bare, instance)
}

/// A unit a test puts in scope by hand: a module no unit holds, with the places an environment reads it by and the record filed beside it.
pub struct AlreadyJudged {
    definitions: Definitions,
    places: Places,
    certification: Certification,
}

impl AlreadyJudged {
    /// `module` in scope with `certification` as the record of a walk over it.
    pub fn with(definitions: &Definitions, certification: &Certification) -> Self {
        Self {
            definitions: definitions.clone(),
            places: Places::of(definitions),
            certification: certification.clone(),
        }
    }

    /// The environment a walk is handed: everything this unit puts in scope, and nothing else.
    pub fn globals(&self) -> Globals<'_> {
        let mut globals = Globals::default();
        globals.mount(&self.definitions, &self.places, &self.certification);

        globals
    }
}

/// Everything `module` puts in scope, with an empty record — so a walk classifies its items' totality for itself.
pub fn already_judged(definitions: &Definitions) -> AlreadyJudged {
    AlreadyJudged::with(definitions, &Certification::default())
}

/// A module carrying `items` and `induct_decls` and nothing else.
pub fn collision_definitions(
    items: Vec<Item>,
    induct_decls: BTreeMap<Global, InductDecl>,
) -> Definitions {
    declaring(
        Definitions {
            items: items.into(),
        },
        induct_decls,
        [],
        [],
    )
}

/// The definition name the environment and the module both declare.
pub fn shadowed_name() -> Global {
    Global::Authored(Qualifier::from(["shadowed"]))
}

/// The name judged in both configurations, whose body is the only thing that reveals which `shadowed` the walk used.
pub fn reader_name() -> Global {
    Global::Authored(Qualifier::from(["reader"]))
}

/// The family name the environment and the module both declare.
pub fn shadowed_family() -> Global {
    Global::Authored(Qualifier::from(["Shadowed"]))
}

/// `let shadowed : Nat = 0` beside `induct Shadowed : Type 0 end` — the two entries an earlier walk is taken to have judged.
pub fn judged_environment() -> Definitions {
    collision_definitions(
        vec![authored(
            &shadowed_name(),
            Term::intrinsic(Intrinsic::NatType),
            Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
        )],
        BTreeMap::from([(
            shadowed_family(),
            InductDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::done(Telescope::done(())),
                constructors: Vec::new().into(),
                result_sort: Term::type_ground(),
                module: Qualifier::default(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )]),
    )
}

/// `let reader : Nat = shadowed`.
pub fn reader() -> Item {
    authored(
        &reader_name(),
        Term::intrinsic(Intrinsic::NatType),
        Term::instance(shadowed_name(), Universes::default()),
    )
}

/// `let shadowed : Bool = true` beside `let reader : Nat = shadowed`.
pub fn shadowing_items() -> Definitions {
    collision_definitions(
        vec![
            authored(
                &shadowed_name(),
                Term::intrinsic(Intrinsic::BoolType),
                Term::intrinsic(Intrinsic::Bool(true)),
            ),
            reader(),
        ],
        BTreeMap::new(),
    )
}

/// `let Shadowed : Nat = 0`: a definition under the name [`judged_environment`] holds as a registry key and under no definition.
pub fn definition_under_the_family() -> Definitions {
    collision_definitions(
        vec![authored(
            &shadowed_family(),
            Term::intrinsic(Intrinsic::NatType),
            Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
        )],
        BTreeMap::new(),
    )
}

/// `name : Nat = 0`, as a group states a member.
fn constant(name: Global) -> Definition {
    Definition {
        name,
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: Term::intrinsic(Intrinsic::NatType),
        body: Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
    }
}

/// A group of one member, `name : type_ = body`, marked a proof: how a lemma proved by its own recursion is stored.
pub fn proof_group(name: &Global, type_: Term, body: Term) -> Item {
    Item::Rec(RecItem::new(vec![(
        fresh(None),
        Definition {
            proof: true,
            type_,
            body,
            ..constant(*name)
        },
    )]))
}

/// One group of two constants, `shadowed` and `fresh`: the first under a name [`judged_environment`] holds, the second under none.
pub fn group_held_in_part() -> Definitions {
    collision_definitions(
        vec![Item::Rec(RecItem::new(vec![
            (fresh(None), constant(shadowed_name())),
            (
                fresh(None),
                constant(Global::Authored(Qualifier::from(["fresh"]))),
            ),
        ]))],
        BTreeMap::new(),
    )
}

/// One group of two constants, both named `shadowed`.
pub fn group_naming_a_member_twice() -> Definitions {
    collision_definitions(
        vec![Item::Rec(RecItem::new(vec![
            (fresh(None), constant(shadowed_name())),
            (fresh(None), constant(shadowed_name())),
        ]))],
        BTreeMap::new(),
    )
}

/// The name [`statement_declared`] declares once or twice.
pub fn statement_name() -> Global {
    Global::Authored(Qualifier::from(["p"]))
}

/// `p : Prop = Holds` and `thm : p = Holds/qed()`, closed with `thm` stated at `Absurd` — followed, when `twice`, by a second `p : Prop = ((x : Holds) => Absurd)(thm)`.
pub fn statement_declared(twice: bool) -> Program {
    let holds_name = Global::Authored(Qualifier::from(["Holds"]));
    let absurd_name = Global::Authored(Qualifier::from(["Absurd"]));
    let theorem = Global::Authored(Qualifier::from(["thm"]));
    let holds = Term::induct_type(holds_name, Vec::<Term>::new(), Vec::<Term>::new());
    let absurd = Term::induct_type(absurd_name, Vec::<Term>::new(), Vec::<Term>::new());
    let named = |name: &Global| Term::instance(*name, Universes::default());

    let refuted = authored(
        &statement_name(),
        Term::prop(),
        Term::apply(
            Term::func(
                [(Free::local(930, Some("x")), holds.clone())],
                absurd.clone(),
            ),
            [named(&theorem)],
        ),
    );
    let proved = authored(
        &theorem,
        named(&statement_name()),
        Term::variant(holds_name, Vec::<Term>::new(), "qed", Vec::<Term>::new()),
    );
    let stated = authored(&statement_name(), Term::prop(), holds);

    let qed = (
        Atom::from("qed"),
        InductParam::new(Telescope::done(Vec::new()), Vec::new()),
    );

    Program {
        definitions: collision_definitions(
            match twice {
                true => vec![stated, proved, refuted],
                false => vec![stated, proved],
            },
            BTreeMap::from([
                (holds_name, proposition(vec![qed])),
                (absurd_name, proposition(Vec::new())),
            ]),
        ),
        entrypoint: Entrypoint {
            body: named(&theorem),
            type_: Some(absurd),
        },
    }
}

/// The name [`record_negative_in_itself`] declares.
pub fn negative_record_name() -> Global {
    Global::Authored(Qualifier::from(["Bad"]))
}

/// `struct Bad { f : (Bad) -> Absurd }`, closed with `w(Bad { w })` for `w = (b) => b.0(b)` stated at `Absurd` — beside, when `beside_a_family`, an empty `induct Bad : Type 0`.
pub fn record_negative_in_itself(beside_a_family: bool) -> Program {
    let bad = negative_record_name();
    let absurd_name = Global::Authored(Qualifier::from(["Absurd"]));
    let absurd = Term::induct_type(absurd_name, Vec::<Term>::new(), Vec::<Term>::new());
    let bad_type: Term = Subterm::StructType(StructType {
        name: bad,
        universes: Universes::default(),
        params: Vec::new(),
    })
    .into();

    let record = StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::build(
            [(
                Free::local(931, Some("f")),
                Term::func_type(
                    [(Free::local(932, Some("devoured")), bad_type.clone())],
                    absurd.clone(),
                ),
            )],
            (),
        )),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    // An entry is held by its former, so the family beside the record is a second former under the record's name.
    let family = beside_a_family.then(|| {
        Item::Rec(RecItem::new(vec![(
            fresh(None),
            Definition::former(
                bad,
                Qualifier::default(),
                Nominal::Inductive(InductDecl {
                    result_sort: Term::type_ground(),
                    ..proposition(Vec::new())
                }),
            ),
        )]))
    });
    let induct_decls = BTreeMap::from([(absurd_name, proposition(Vec::new()))]);

    let subject = Free::local(933, Some("b"));
    let open = Term::func(
        [(subject, bad_type)],
        Term::apply(
            Term::proj_struct(Term::free_var(&subject), bad, 0),
            [Term::free_var(&subject)],
        ),
    );

    Program {
        definitions: collision_definitions(
            family
                .into_iter()
                .chain([Item::Let(Box::new(Definition::former(
                    bad,
                    Qualifier::default(),
                    Nominal::Struct(record),
                )))])
                .collect(),
            induct_decls,
        ),
        entrypoint: Entrypoint {
            body: Term::apply(
                open.clone(),
                [Term::struct_(bad, Vec::<Term>::new(), [open])],
            ),
            type_: Some(absurd),
        },
    }
}

/// The family [`wrapper_of_its_parameter`] declares.
pub fn wrapper_family() -> Global {
    Global::Authored(Qualifier::from(["Wrapper"]))
}

/// `induct Wrapper(A : Type) | wrap(held : A) end`, or with `negatively` `| wrap(held : (A) -> Nat) end`, the entry stamped `Strict` in `A` either way: elaboration's vector, the truth of the first and a lie of the second.
pub fn wrapper_of_its_parameter(negatively: bool) -> Definitions {
    wrapper_holding(|a| match negatively {
        true => taken(a),
        false => a,
    })
}

/// `(taken: of) -> Nat`.
pub fn taken(of: Term) -> Term {
    Term::func_type(
        [(Free::local(991, Some("taken")), of)],
        Term::intrinsic(Intrinsic::NatType),
    )
}

/// `induct Wrapper(A : Type) | wrap(held : held(A)) end`, the entry stamped `Strict` in `A` whatever `held` makes of it.
pub fn wrapper_holding(held: impl FnOnce(Term) -> Term) -> Definitions {
    let a = Free::local(990, Some("A"));
    let held = held(Term::free_var(&a));

    collision_definitions(
        Vec::new(),
        BTreeMap::from([(
            wrapper_family(),
            InductDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::build([(a, Term::type_ground())], Telescope::done(())),
                constructors: vec![(
                    Atom::from("wrap"),
                    InductParam::new(
                        Telescope::build(
                            [
                                (a, Term::type_ground()),
                                (Free::local(992, Some("held")), held),
                            ],
                            Vec::new(),
                        ),
                        vec![Plicity::Implicit, Plicity::Explicit],
                    ),
                )]
                .into(),
                result_sort: Term::type_ground(),
                module: Qualifier::default(),
                rep_public: true,
                polarities: vec![Polarity::Strict],
                variances: Vec::new(),
            },
        )]),
    )
}

/// The family [`stored_in_the_wrapper`] declares.
pub fn stored_family() -> Global {
    Global::Authored(Qualifier::from(["Outer"]))
}

/// `induct Outer | c(held : Wrapper(Outer)) end`: strictly positive exactly where [`wrapper_family`] is in its parameter.
pub fn stored_in_the_wrapper() -> Definitions {
    let outer = Term::induct_type(stored_family(), Vec::<Term>::new(), Vec::<Term>::new());

    collision_definitions(
        Vec::new(),
        BTreeMap::from([(
            stored_family(),
            InductDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::done(Telescope::done(())),
                constructors: vec![(
                    Atom::from("c"),
                    InductParam::new(
                        Telescope::build(
                            [(
                                Free::local(993, Some("held")),
                                Term::induct_type(wrapper_family(), [outer], Vec::<Term>::new()),
                            )],
                            Vec::new(),
                        ),
                        vec![Plicity::Explicit],
                    ),
                )]
                .into(),
                result_sort: Term::type_ground(),
                module: Qualifier::default(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )]),
    )
}

/// `induct Shadowed : Type 0 | mk(a : payload) end`, carried as a registry entry with no items lowering it.
pub fn shadowing_registry(payload: Term) -> Definitions {
    collision_definitions(
        Vec::new(),
        BTreeMap::from([(
            shadowed_family(),
            InductDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::done(Telescope::done(())),
                constructors: vec![(
                    Atom::from("mk"),
                    InductParam::new(
                        Telescope::build([(Free::local(910, Some("a")), payload)], Vec::new()),
                        vec![Plicity::Explicit],
                    ),
                )]
                .into(),
                result_sort: Term::type_ground(),
                module: Qualifier::default(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )]),
    )
}

/// The record [`two_registry_entries`] declares beside [`shadowed_family`].
pub fn kept_record() -> Global {
    Global::Authored(Qualifier::from(["Kept"]))
}

/// What [`two_registry_entries`] varies of the two entries it declares.
pub enum Varied {
    /// Nothing: the entries as an environment holds them.
    Nothing,
    /// Elaboration's stamps on both: a polarity vector, the owning module and the representation's privacy.
    Stamps,
    /// The family's variance vector.
    Variance,
    /// The type of the record's field.
    Field,
}

/// `induct Shadowed : Type 0 end` and `struct Kept { held : Nat }`, as registry entries with no items lowering them, differing from the unvaried pair in what `varied` names.
pub fn two_registry_entries(varied: Varied) -> Definitions {
    let stamped = matches!(varied, Varied::Stamps);
    let polarities = || match stamped {
        true => vec![Polarity::Mixed],
        false => Vec::new(),
    };
    let owner = || match stamped {
        true => Qualifier::from(["elsewhere"]),
        false => Qualifier::default(),
    };

    let family = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: Vec::new().into(),
        result_sort: Term::type_ground(),
        module: owner(),
        rep_public: !stamped,
        polarities: polarities(),
        variances: match varied {
            Varied::Variance => vec![Variance::Irrelevant],
            _ => Vec::new(),
        },
    };
    let record = StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::build(
            [(
                Free::local(940, Some("held")),
                Term::intrinsic(match varied {
                    Varied::Field => Intrinsic::BoolType,
                    _ => Intrinsic::NatType,
                }),
            )],
            (),
        )),
        result_sort: Term::type_ground(),
        module: owner(),
        rep_public: !stamped,
        polarities: polarities(),
        variances: Vec::new(),
    };

    declaring(
        Definitions {
            ..collision_definitions(Vec::new(), BTreeMap::from([(shadowed_family(), family)]))
        },
        [],
        BTreeMap::from([(kept_record(), record)]),
        [],
    )
}

pub trait Walked {
    fn parts(&self) -> (&Definitions, Option<&Entrypoint>);
}

impl Walked for Definitions {
    fn parts(&self) -> (&Definitions, Option<&Entrypoint>) {
        (self, None)
    }
}

impl Walked for Program {
    fn parts(&self) -> (&Definitions, Option<&Entrypoint>) {
        (&self.definitions, Some(&self.entrypoint))
    }
}

/// The whole-module walk over a fixture: the modules built here are adversarial by design, some carrying what only elaboration leaves, and the certifier's own refusals are the thing under test.
pub fn fixture_verdicts(
    fixture: &impl Walked,
    budget: u64,
    globals: &Globals,
    syntax: SyntaxRegistry,
) -> Vec<Verdict> {
    fixture_certified(fixture, budget, globals, syntax).verdicts
}

/// [`fixture_verdicts`], with the record the walk leaves — for a fixture that mounts what one walk concluded beneath another.
pub fn fixture_certified(
    fixture: &impl Walked,
    budget: u64,
    globals: &Globals,
    syntax: SyntaxRegistry,
) -> Rechecked {
    let (definitions, entry) = fixture.parts();

    Put {
        scope: None,
        globals: Some(globals),
        syntax,
        budget,
    }
    .module(definitions, entry)
}

/// The door this crate's suites put a candidate through, as the soundness board puts its tickets: nothing in scope, the registry they judge under, and their budget.
pub fn put() -> Put<'static> {
    Put {
        scope: None,
        globals: None,
        syntax: SYNTAX,
        budget: BUDGET,
    }
}

pub const BUDGET: u64 = 1_000_000;

pub fn global(symbol: &str) -> Global {
    Global::Authored(Qualifier::from([symbol]))
}

pub fn nat() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

pub fn literal(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

/// `alias : Type = <alias>`, `three : alias = 3`, `identity : (Nat) -> Nat = (x) => <returned>`, and `five : Nat = identity(5)`: `three` unfolds `alias`, and `five` types `identity` and unfolds nothing.
pub fn reading_definitions(alias: Term, returned: impl Fn(&Free) -> Term) -> Definitions {
    let x = Free::local(10, Some("x"));

    Definitions {
        items: vec![
            authored(&global("alias"), Term::type_ground(), alias),
            authored(
                &global("three"),
                Term::instance(global("alias"), Universes::default()),
                literal(3),
            ),
            authored(
                &global("identity"),
                Term::func_type([(x, nat())], nat()),
                Term::func([(x, nat())], returned(&x)),
            ),
            authored(
                &global("five"),
                nat(),
                Term::apply(
                    Term::instance(global("identity"), Universes::default()),
                    [literal(5)],
                ),
            ),
        ]
        .into(),
    }
}

/// The module every fixture here is an edit of: `alias` is `Nat` and `identity` returns its argument.
pub fn written() -> Definitions {
    reading_definitions(nat(), Term::free_var)
}

/// The propositions a module written as Core text here uses, declared where the registry this crate's suites judge under names them: `False`, `True` and `Holds`, the reflection of a decision a bound states.
pub const PROPOSITIONS: &str = r#"
induct /std/Bool/False: pub Prop
end

induct /std/Bool/True: pub Prop
| qed()
end

let /std/Bool/Holds : (b: #BoolType) -> Prop =
    (b: #BoolType) =>
        #{result = Ambient(Prop)}
        match b
        | false =>
            #{node = InductType}
            /std/Bool/False()
        | true =>
            #{node = InductType}
            /std/Bool/True()
        end;
"#;

/// What `text` writes, Core text read with nothing in scope: the module, and the term it closes with where it closes with one.
fn core_text_read(text: &str) -> (Definitions, Option<Entrypoint>) {
    put()
        .read(text)
        .unwrap_or_else(|unread| panic!("{unread:?}"))
}

/// A family of one case over one `Nat`, `F` with `mk(x : Nat)`, and `held : T = 3` whose type eliminates `mk(3)` through the arm `${arm}`.
pub const ARM: &str = r#"
#{absent = [/F/mk]}
induct /F: pub Type
| mk(x: #NatType)
end

let /held: match #{node = Variant} /F/mk(3): (s) => Type | ${arm} => #NatType end =
    3;
"#;

/// What the walk says of `text`, Core text read with nothing in scope and walked as it closes: a module, or a program at the type it states.
pub fn text_certified(text: &str) -> Rechecked {
    let (definitions, entry) = core_text_read(text);

    put().module(&definitions, entry.as_ref())
}

/// The verdicts of [`text_certified`].
pub fn text_verdicts(text: &str) -> Vec<Verdict> {
    text_certified(text).verdicts
}

/// [`text_certified`] with each judgment restored to `budget`.
pub fn text_certified_within(text: &str, budget: u64) -> Rechecked {
    let (definitions, entry) = core_text_read(text);

    Put { budget, ..put() }.module(&definitions, entry.as_ref())
}

/// Whether `verdicts` refuse the declaration named `name` at the root, or the closing term where it is `None`, by an error `refusal` admits.
pub fn refuses(verdicts: &[Verdict], name: Option<&str>, refusal: impl Fn(&Error) -> bool) -> bool {
    verdicts
        .iter()
        .any(|verdict| verdict.name == name.map(global) && refusal(&verdict.error))
}

/// A count refused as `counted`, `actual` where `expected` was.
pub fn miscounted(counted: Counted, expected: usize, actual: usize) -> impl Fn(&Error) -> bool {
    move |error| {
        matches!(error, Error::Arity { counted: found, expected: e, actual: a }
            if *found == counted && *e == expected && *a == actual)
    }
}

/// The module `text` writes, read with nothing in scope and closing with no term: an environment another text is put over.
pub fn core_text_module(text: &str) -> Definitions {
    let (definitions, entry) = core_text_read(text);
    assert!(entry.is_none(), "an environment closes with no term");

    definitions
}

/// What the certifier says of `text`, Core text read with nothing in scope and closing with a term it states no type for, put at `False` as the registry this crate's suites judge under names it, over `globals`.
pub fn core_text_verdicts_over(text: &str, globals: &Globals) -> Vec<Verdict> {
    Put {
        globals: Some(globals),
        ..put()
    }
    .core_text(text)
    .unwrap_or_else(|unread| panic!("{unread:?}"))
    .verdicts
}

/// What the certifier says of `text`, Core text read with nothing in scope and closing with a term it states no type for, put at `False` as the registry this crate's suites judge under names it: how a module built by hand is written, declaring every name it uses.
pub fn core_text_verdicts(text: &str) -> Vec<Verdict> {
    core_text_verdicts_over(text, &Globals::default())
}

/// `module` walked with nothing in scope and no baseline: the walk that files a baseline's record.
pub fn walked(definitions: &Definitions) -> Rechecked {
    let rechecked = fixture_certified(definitions, BUDGET, &Globals::default(), SYNTAX);
    assert_eq!(rechecked.verdicts, Vec::new());

    rechecked
}

/// `module` walked over `baseline` as `certification` records it.
pub fn walked_over(
    definitions: &Definitions,
    baseline: &Definitions,
    certification: &Certification,
) -> Rechecked {
    walked_among(definitions, baseline, certification, None)
}

/// [`walked_over`], taking what it can from what `store` holds.
pub fn walked_among(
    definitions: &Definitions,
    baseline: &Definitions,
    certification: &Certification,
    store: Option<&dyn Candidates>,
) -> Rechecked {
    put().over(definitions, baseline, certification, store)
}

/// `certification` with `symbol`'s entry replaced by `entry`'s answer for it.
pub fn doctored(
    certification: &Certification,
    symbol: &str,
    entry: impl Fn(Certified) -> Certified,
) -> Certification {
    Certification::of(
        certification.iter().map(|(name, totality)| {
            let certified = Certified {
                totality,
                applications: certification
                    .applications(name)
                    .cloned()
                    .unwrap_or_default(),
                reads: certification
                    .reads(name)
                    .cloned()
                    .expect("a record holds the reads of every definition it classifies"),
            };

            match *name == global(symbol) {
                true => (*name, entry(certified)),
                false => (*name, certified),
            }
        }),
        [],
        [],
        [],
    )
}

/// A record of `written` in which `five` is marked: its reads hold `alias`'s signature, which no judgment of `five` reads. A walk that hands the mark back adopted the entry, and one that judged `five` filed its own reads in its place.
pub fn marked() -> Certification {
    doctored(&walked(&written()).certification, "five", |certified| {
        Certified {
            reads: Reads {
                signatures: BTreeSet::from([global("identity"), global("alias")]),
                bodies: BTreeSet::new(),
            },
            ..certified
        }
    })
}

/// `module` with its items held as a version's: a module made of it by replacing an item shares every other item with it, as a recompile's module shares the ones it kept with its baseline's.
pub fn versioned(definitions: Definitions) -> Definitions {
    Definitions {
        items: definitions.items.version(),
    }
}

/// `module`, a version, with the item declaring `name` replaced by `item`.
pub fn replaced(definitions: &Definitions, name: &str, item: Item) -> Definitions {
    let place = Places::of(definitions)
        .place(&global(name))
        .expect("the module declares the name")
        .item();
    let mut items = definitions.items.clone();
    items.replace(place, item);

    Definitions { items }
}

/// `Type 5`, `Type 4` and `Type 0` as hand-built universes.
pub fn type_at(level: u32) -> Term {
    Term::type_at(Level::constant(level))
}

/// `name : type_ = body`, stating the sort `value_sort` its value inhabits ([`curios_core::Definition::value_sort`]): the field a type alias carries and the one `read_digest` leaves out.
pub fn aliased(name: &Global, type_: Term, body: Term, value_sort: Term) -> Item {
    match authored(name, type_, body) {
        Item::Let(definition) => Item::Let(Box::new(Definition {
            value_sort: Some(value_sort),
            ..*definition
        })),
        item => item,
    }
}

/// Three aliases in a chain, each saying the sort its value inhabits: `H : Type 5 = h_body` saying `h`, `K : Type 5 = H` saying `k`, and `L : Type 5 = K` saying `l`.
///
/// The walk states `L`'s value sort by `Sort::of(K)`, which reads `K`'s *published* sort with nothing run, so what it states of `L` is a function of what it stated of `K` and of nothing else about `K`, and nothing the module says of a sort.
pub fn chained_sort_definitions(h_body: Term, h: Term, k: Term, l: Term) -> Definitions {
    Definitions {
        items: vec![
            aliased(&global("H"), type_at(5), h_body, h),
            aliased(
                &global("K"),
                type_at(5),
                Term::instance(global("H"), Universes::default()),
                k,
            ),
            aliased(
                &global("L"),
                type_at(5),
                Term::instance(global("K"), Universes::default()),
                l,
            ),
        ]
        .into(),
    }
}
