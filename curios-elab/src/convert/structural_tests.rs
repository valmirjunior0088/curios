//! Tuples, projections, eta at a known type, and the irrelevance that fires at a computed proposition.

use {
    super::test_support::*,
    crate::*,
    curios_core::{
        Atom, Exhaustion, Free, InductDecl, InductParam, Intrinsic, Many, MetavarId, Scope,
        StructDecl, StructType, Subterm, Telescope, Term, UniverseContext,
    },
    curios_utilities::{Plicity, Qualifier},
};

#[test]
fn convert_tuple_equal() {
    let mut context = context();

    let this = Term::tuple([nat(1), nat(2)]);
    let that = Term::tuple([nat(1), nat(2)]);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn tuple_unequal_field() {
    let mut context = context();

    let this = Term::tuple([nat(1), nat(2)]);
    let that = Term::tuple([nat(1), nat(3)]);

    assert_eq!(conv(&mut context, &this, &that), Ok(false));
}

#[test]
fn proj_same_index_and_head() {
    let mut context = context();
    let r = context.fresh(Some("r"));

    let this = Term::proj(Term::free_var(&r), 0);
    let that = Term::proj(Term::free_var(&r), 0);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn proj_different_index_is_false() {
    let mut context = context();
    let r = context.fresh(Some("r"));

    let this = Term::proj(Term::free_var(&r), 0);
    let that = Term::proj(Term::free_var(&r), 1);

    assert_eq!(conv(&mut context, &this, &that), Ok(false));
}

#[test]
fn eta_tuple_neutral_with_known_type() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));
    let r_binder = context.fresh(Some("r"));
    let s_binder = context.fresh(Some("s"));

    let tuple_type: Term = Term::tuple_type([
        (x, Term::intrinsic(Intrinsic::NatType)),
        (y, Term::intrinsic(Intrinsic::BoolType)),
    ]);

    let r: Term = Term::free_var(&r_binder);
    let s: Term = Term::free_var(&s_binder);

    assert_eq!(convert(&mut context, &tuple_type, &r, &r), Ok(true));

    assert_eq!(convert(&mut context, &tuple_type, &r, &s), Ok(false));
}

#[test]
fn partial_projection_tuple_at_narrow_type() {
    let mut context = context();
    let p = context.fresh(Some("p"));
    let q = context.fresh(Some("q"));
    let x = context.fresh(Some("x"));

    // p = (1, 2), q = (1, 3) — both 2-tuples agreeing on field 0, differing on field 1.
    context.define(&p, &Term::tuple([nat(1), nat(2)]), None);
    context.define(&q, &Term::tuple([nat(1), nat(3)]), None);

    let type_: Term = Term::tuple_type([(x, Term::intrinsic(Intrinsic::NatType))]);

    // this = (p.0), that = (q.0). At the 1-field type both denote (a), so conversion should return true.
    let this: Term = Term::tuple([Term::proj(Term::free_var(&p), 0)]);
    let that: Term = Term::tuple([Term::proj(Term::free_var(&q), 0)]);

    // Reduction does not eta-reduce a tuple (`reduce`'s `does_not_eta_reduce_tuple`), so each side stays a 1-tuple and its field is compared at the 1-field telescope: each `proj(_, 0)` reduces to `1`, and the two convert.
    assert_eq!(convert(&mut context, &type_, &this, &that), Ok(true));
}

#[test]
fn times_out_on_pathological_inputs() {
    let mut context = context();
    let loop_ = context.fresh(Some("loop"));
    let x = context.fresh(Some("x"));
    let z = context.fresh(Some("z"));
    let y = context.fresh(Some("y"));

    context.define(&loop_, &Term::free_var(&loop_), None);

    let this = Term::tuple_type([
        (
            x,
            Term::apply(func([&z], Term::free_var(&z)), [Term::free_var(&loop_)]),
        ),
        (y, Term::free_var(&x)),
    ]);

    let that = Term::tuple_type([(x, Term::free_var(&loop_)), (y, Term::free_var(&x))]);

    assert!(conv(&mut context, &this, &that).is_err_and(|spent| spent.is_exhausted()));
}

#[test]
fn unit_typed_neutrals_in_type_argument() {
    let mut context = context();
    let func = context.fresh(Some("F"));
    let wildcard = context.fresh(Some("_"));
    let r_binder = context.fresh(Some("r"));
    let s_binder = context.fresh(Some("s"));

    // F : (()) -> Type ; r, s : ()   (all neutral assumptions). r ≡ s by η for the empty tuple (unit / proof irrelevance), so F r ≡ F s. `conv` compares at `Type`, exactly as the pipeline does via `expect`.
    context.assume(
        &func,
        &Term::func_type([(wildcard, Term::tuple_type_unit())], Term::type_ground()),
    );
    context.assume(&r_binder, &Term::tuple_type_unit());
    context.assume(&s_binder, &Term::tuple_type_unit());

    let f = Term::free_var(&func);
    let r = Term::free_var(&r_binder);
    let s = Term::free_var(&s_binder);

    let this = Term::apply(f.clone(), [r]); // F r
    let that = Term::apply(f, [s]); // F s

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

/// Unit eta, by the goal's type: at the empty Σ and at a nominal struct that declares no field, any two terms converge whatever their shapes — two stuck applications of two heads, which the structural rule compares head against head and would refuse. It composes with the eta between two neutrals that reaches it: two variables at a record of units converge by their projections, each at its field's type, and two at a function into a unit by their applications. The kernel's twin of this proposition shares the name.
///
/// The rule is the type's: at `Type` two variables stay apart, and so do two at a record or a struct that has a relevant field.
///
/// Mutation-checked: without the check ahead of the structural dispatch the first goal is refused, with a tuple neutral's projections compared at `Type` the record of units' is, and with a struct's fields uncounted the last goal is accepted.
#[test]
fn any_two_terms_converge_at_a_type_with_no_field() {
    let mut context = context();
    let (f, g) = (context.fresh(Some("f")), context.fresh(Some("g")));
    let (u, v) = (
        Term::free_var(&context.fresh(Some("u"))),
        Term::free_var(&context.fresh(Some("v"))),
    );
    let (a, b) = (context.fresh(Some("a")), context.fresh(Some("b")));

    let unit = Term::tuple_type_unit();
    let function_into_unit = Term::func_type([(a, nat_type())], unit.clone());
    context.assume(&f, &function_into_unit);
    context.assume(&g, &function_into_unit);

    let field_less = declare_struct(&mut context, "U", Telescope::done(()));
    let one_field = declare_struct(&mut context, "S", Telescope::build([(a, nat_type())], ()));

    let record_of_units = Term::tuple_type([(a, unit.clone()), (b, unit.clone())]);
    let record_of_a_number = Term::tuple_type([(a, nat_type())]);
    let applied = |head: &Free, argument: usize| Term::apply(Term::free_var(head), [nat(argument)]);

    assert_eq!(
        convert(&mut context, &unit, &applied(&f, 0), &applied(&g, 1)),
        Ok(true)
    );
    assert_eq!(convert(&mut context, &unit, &u, &v), Ok(true));
    assert_eq!(convert(&mut context, &field_less, &u, &v), Ok(true));
    assert_eq!(convert(&mut context, &record_of_units, &u, &v), Ok(true));
    assert_eq!(convert(&mut context, &function_into_unit, &u, &v), Ok(true));

    assert_eq!(
        convert(&mut context, &Term::type_ground(), &u, &v),
        Ok(false)
    );
    assert_eq!(
        convert(&mut context, &record_of_a_number, &u, &v),
        Ok(false)
    );
    assert_eq!(convert(&mut context, &one_field, &u, &v), Ok(false));
}

/// A literal's eta is the goal type's to refuse. At a type former that is not the literal's own the two sides are not of one type, and a literal with no field — whose walk compares nothing — would convert with anything: a lambda, the unit literal and a field-less struct's literal against a neutral at `Nat`, that struct's literal at another struct, and the unit literal against `1`, are refused. Each is still taken at the literal's own type, and at `Type`, which says nothing. The kernel's twin of this proposition shares the name.
///
/// Mutation-checked: without the refusal in `eta_expand_func` the first goal is accepted, without the one in `eta_expand_tuple` the second and the last of the refused are, without the one in `eta_expand_struct` the two between are, and with a sort counted a former the goals at `Type` are refused.
#[test]
fn eta_by_a_literal_is_refused_at_a_type_former_that_is_not_its_own() {
    let mut context = context();
    let (f, x, a) = (
        context.fresh(Some("f")),
        context.fresh(Some("x")),
        context.fresh(Some("a")),
    );
    context.assume(&f, &Term::func_type([(x, nat_type())], nat_type()));
    let field_less = declare_struct(&mut context, "U", Telescope::done(()));
    let one_field = declare_struct(&mut context, "S", Telescope::build([(a, nat_type())], ()));

    let expansion = Term::func(
        [(x, nat_type())],
        Term::apply(
            Term::free_var(&f),
            [Term::intrinsic(Intrinsic::nat_add(
                Term::free_var(&x),
                nat(0),
            ))],
        ),
    );
    let unit = Term::tuple(Vec::<Term>::new());
    let empty = Term::struct_(nominal("U"), Vec::<Term>::new(), Vec::<Term>::new());
    let (function, neutral) = (
        Term::free_var(&f),
        Term::free_var(&context.fresh(Some("n"))),
    );
    let (ground, number) = (Term::type_ground(), nat_type());
    let mut at = |type_: &Term, this: &Term, that: &Term| convert(&mut context, type_, this, that);

    assert_eq!(
        [
            at(&number, &expansion, &function),
            at(&number, &unit, &neutral),
            at(&number, &empty, &neutral),
            at(&one_field, &neutral, &empty),
            at(&number, &unit, &nat(1)),
        ],
        [Ok(false), Ok(false), Ok(false), Ok(false), Ok(false)]
    );
    assert_eq!(
        [
            at(&Term::tuple_type_unit(), &unit, &neutral),
            at(&field_less, &empty, &neutral),
            at(&ground, &expansion, &function),
            at(&ground, &unit, &neutral),
            at(&ground, &neutral, &empty),
        ],
        [Ok(true), Ok(true), Ok(true), Ok(true), Ok(true)]
    );
}

/// A struct type declared with `fields` and no parameter, for the goals that need a nominal type.
fn declare_struct(context: &mut Context, path: &str, fields: Telescope<()>) -> Term {
    context
        .register_struct(
            &nominal(path),
            StructDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(fields),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                plicities: Vec::new(),
            },
        )
        .unwrap();

    Term::from(Subterm::StructType(StructType {
        name: nominal(path),
        universes: Vec::new(),
        params: Vec::new(),
    }))
}

// A struct's fields compare at their declared types, recovered from the registry — so a proof-irrelevant (unit-typed) field equates distinct neutrals, and two structs differing only there are convertible.
#[test]
fn struct_unit_field_is_irrelevant() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let u = context.fresh(Some("u"));
    let r_binder = context.fresh(Some("r"));
    let s_binder = context.fresh(Some("s"));

    // struct Wrap { x : Nat, u : () }
    context
        .register_struct(
            &nominal("Wrap"),
            StructDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(Telescope::build(
                    [
                        (x, Term::intrinsic(Intrinsic::NatType)),
                        (u, Term::tuple_type_unit()),
                    ],
                    (),
                )),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                plicities: Vec::new(),
            },
        )
        .unwrap();

    context.assume(&r_binder, &Term::tuple_type_unit());
    context.assume(&s_binder, &Term::tuple_type_unit());

    let r = Term::free_var(&r_binder);
    let s = Term::free_var(&s_binder);

    // Wrap { 1, r } and Wrap { 1, s } differ only in the unit field's neutral.
    let this = Term::struct_(nominal("Wrap"), Vec::<Term>::new(), [nat(1), r]);
    let that = Term::struct_(nominal("Wrap"), Vec::<Term>::new(), [nat(1), s]);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

// The same discipline at a nominal proposition rather than the unit — the shape the proof-carrying idiom writes, and the proposition the kernel's twin of this test puts to its own copy.
#[test]
fn a_struct_field_at_a_proposition_is_not_read() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let p_field = context.fresh(Some("p"));
    let (p, q) = (context.fresh(Some("p")), context.fresh(Some("q")));

    context
        .register_induct(
            &nominal("P"),
            InductDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(Telescope::done(())),
                constructors: Vec::new(),
                result_sort: Term::prop(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                plicities: Vec::new(),
            },
        )
        .unwrap();
    let proposition = Term::induct_type(nominal("P"), Vec::<Term>::new(), Vec::<Term>::new());
    context
        .register_struct(
            &nominal("Wrap"),
            StructDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(Telescope::build(
                    [
                        (n, Term::intrinsic(Intrinsic::NatType)),
                        (p_field, proposition.clone()),
                    ],
                    (),
                )),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                plicities: Vec::new(),
            },
        )
        .unwrap();
    context.assume(&p, &proposition);
    context.assume(&q, &proposition);

    let this = Term::struct_(
        nominal("Wrap"),
        Vec::<Term>::new(),
        [nat(1), Term::free_var(&p)],
    );
    let that = Term::struct_(
        nominal("Wrap"),
        Vec::<Term>::new(),
        [nat(1), Term::free_var(&q)],
    );
    assert_eq!(conv(&mut context, &this, &that), Ok(true));

    let other = Term::struct_(
        nominal("Wrap"),
        Vec::<Term>::new(),
        [nat(2), Term::free_var(&p)],
    );
    assert_eq!(conv(&mut context, &this, &other), Ok(false));
}

#[test]
fn a_constructor_payload_at_a_proposition_is_not_read() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let p_field = context.fresh(Some("p"));
    let (p, q) = (context.fresh(Some("p")), context.fresh(Some("q")));

    context
        .register_induct(
            &nominal("P"),
            InductDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(Telescope::done(())),
                constructors: Vec::new(),
                result_sort: Term::prop(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                plicities: Vec::new(),
            },
        )
        .unwrap();
    let proposition = Term::induct_type(nominal("P"), Vec::<Term>::new(), Vec::<Term>::new());
    context
        .register_induct(
            &nominal("Wrap"),
            InductDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(Telescope::done(())),
                constructors: Vec::from([(
                    Atom::from("wrap"),
                    InductParam::new(
                        Telescope::build(
                            [
                                (n, Term::intrinsic(Intrinsic::NatType)),
                                (p_field, proposition.clone()),
                            ],
                            Vec::new(),
                        ),
                        vec![Plicity::Explicit, Plicity::Explicit],
                    ),
                )]),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                plicities: Vec::new(),
            },
        )
        .unwrap();
    context.assume(&p, &proposition);
    context.assume(&q, &proposition);

    let this = Term::variant(
        nominal("Wrap"),
        Vec::<Term>::new(),
        "wrap",
        [nat(1), Term::free_var(&p)],
    );
    let that = Term::variant(
        nominal("Wrap"),
        Vec::<Term>::new(),
        "wrap",
        [nat(1), Term::free_var(&q)],
    );
    assert_eq!(conv(&mut context, &this, &that), Ok(true));

    let other = Term::variant(
        nominal("Wrap"),
        Vec::<Term>::new(),
        "wrap",
        [nat(2), Term::free_var(&p)],
    );
    assert_eq!(conv(&mut context, &this, &other), Ok(false));
}

// Likewise a variant's payload compares at its constructor's declared types, so a unit-typed payload field is proof-irrelevant.
#[test]
fn variant_unit_payload_is_irrelevant() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let u = context.fresh(Some("u"));
    let r_binder = context.fresh(Some("r"));
    let s_binder = context.fresh(Some("s"));

    // induct Wrap | wrap(x : Nat, u : ()) end
    context
        .register_induct(
            &nominal("Wrap"),
            InductDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(Telescope::done(())),
                constructors: Vec::from([(
                    Atom::from("wrap"),
                    InductParam::new(
                        Telescope::build(
                            [
                                (x, Term::intrinsic(Intrinsic::NatType)),
                                (u, Term::tuple_type_unit()),
                            ],
                            Vec::new(),
                        ),
                        vec![Plicity::Explicit, Plicity::Explicit],
                    ),
                )]),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                plicities: Vec::new(),
            },
        )
        .unwrap();

    context.assume(&r_binder, &Term::tuple_type_unit());
    context.assume(&s_binder, &Term::tuple_type_unit());

    let r = Term::free_var(&r_binder);
    let s = Term::free_var(&s_binder);

    // wrap(1, r) and wrap(1, s) differ only in the unit payload's neutral.
    let this = Term::variant(nominal("Wrap"), Vec::<Term>::new(), "wrap", [nat(1), r]);
    let that = Term::variant(nominal("Wrap"), Vec::<Term>::new(), "wrap", [nat(1), s]);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

/// Proof irrelevance at a *computed* proposition — a stuck `match` whose motive is `Prop`.
///
/// Across the prelude's elaboration **every firing is at a computed proposition rather than a nominal `Prop` family**: validity predicates over `Bits` and `Nat`, the shape a decision procedure takes. So the tests that name irrelevance at a nominal family — `curios-cert`'s `any_two_inhabitants_of_a_proposition_convert` and its control — cover a shape the corpus never presents, and cover the *kernel's* copy, which no program reaches. This crate's copy is the one that does the work, and this fixture asserts it rather than leaving it to the prelude happening to be written with those predicates.
///
/// What the fixture pins is the mechanism those firings rest on. `Sort::of` classifies a stuck `Match` by reading its **motive**, not its arms — "a type-valued match: its sort is the motive" — so a match that cannot reduce is nonetheless a proposition when its motive says `Prop`, and irrelevance may then discharge a goal at it without examining either side. Two distinct neutrals convert there.
///
/// The control is the identical term with the motive at `Type`. It must not convert, and it is what makes this a test of the *motive* rather than of matches in general: read the arms instead, or default to `Prop` for anything unclassifiable, and the two fixtures stop disagreeing.
#[test]
fn fires_at_a_computed_proposition() {
    let mut context = context();
    let (left, right) = (context.fresh(Some("p")), context.fresh(Some("q")));
    let computed = computed_type(&mut context, Term::prop());

    assert_eq!(
        convert(
            &mut context,
            &computed,
            &Term::free_var(&left),
            &Term::free_var(&right),
        ),
        Ok(true),
    );
}

/// The control for the fixture above: the same stuck `match`, with its motive at `Type`. Irrelevance is a property of the type, and a computed type is no exception.
#[test]
fn does_not_fire_at_a_computed_relevant_type() {
    let mut context = context();
    let (left, right) = (context.fresh(Some("p")), context.fresh(Some("q")));
    let computed = computed_type(&mut context, Term::type_ground());

    assert_eq!(
        convert(
            &mut context,
            &computed,
            &Term::free_var(&left),
            &Term::free_var(&right),
        ),
        Ok(false),
    );
}

/// `match n | 0 => Nat | _ => Nat end` at the given motive, stuck because `n` is a neutral assumption. The arms are deliberately a *relevant* type in both fixtures: what decides the sort is the motive, and picking arms that agree with it would let a rule reading the arms pass too.
fn computed_type(context: &mut Context, motive: Term) -> Term {
    let subject = context.fresh(Some("n"));
    context.assume(&subject, &Term::intrinsic(Intrinsic::NatType));

    let scrutinee = context.fresh(Some("k"));
    Term::switch_scoped(
        Term::free_var(&subject),
        Scope::close(Many(1), &[&scrutinee], motive),
        [(0u32, Term::intrinsic(Intrinsic::NatType))],
        Term::intrinsic(Intrinsic::NatType),
    )
}

/// An intrinsic with no hand-written congruence arm is still compared operand by operand.
///
/// A hand-written table short an operation would answer a *hard* mismatch rather than a postponement, refusing a metavariable standing in one of that operation's operands instead of solving it — and `convert`'s syntactic-identity short circuit would hide that for every spelling that happens to be identical. The rule reads the operands off `Intrinsic::traverse`, so the table cannot be short an operation; `ListMap` is the one put to it here.
#[test]
fn an_intrinsic_without_a_hand_written_arm_solves_a_metavariable_in_its_operand() {
    let mut context = context();
    let nat_type = Term::intrinsic(Intrinsic::NatType);

    let xs = context.fresh(Some("xs"));
    context.assume(
        &xs,
        &Term::intrinsic(Intrinsic::list_type(nat_type.clone())),
    );

    let n = context.fresh(Some("n"));
    let mapper_type = Term::func_type([(n, nat_type.clone())], nat_type.clone());
    let f = context.fresh(Some("f"));
    context.assume(&f, &mapper_type);

    context.birth_metavar(MetavarId(0), Vec::new(), mapper_type);

    let flexible = Term::intrinsic(Intrinsic::list_map(
        nat_type.clone(),
        nat_type.clone(),
        Term::free_var(&xs),
        Term::hole(0),
    ));
    let rigid = Term::intrinsic(Intrinsic::list_map(
        nat_type.clone(),
        nat_type,
        Term::free_var(&xs),
        Term::free_var(&f),
    ));

    assert_eq!(conv(&mut context, &flexible, &rigid), Ok(true));
}

/// Reading the family an indexed match eliminates at its scrutinee is a probe: a scrutinee whose type the budget cannot afford to read propagates the refusal, rather than answering that the two sides do not convert and letting conversion go ahead on that answer.
#[test]
fn a_family_the_budget_cannot_read_at_its_scrutinee_propagates_the_refusal() {
    let mut context = context();
    let loop_ = context.fresh(Some("loop"));
    context.define(&loop_, &Term::free_var(&loop_), None);
    let head = context.fresh(Some("head"));
    context.assume(&head, &Term::free_var(&loop_));
    let index = context.fresh(Some("i"));
    let scrutinee = context.fresh(Some("s"));
    let motive = Scope::close(Many(2), &[&index, &scrutinee], Term::type_ground());

    let read = super::family_at_head(&mut context, &motive, &Term::free_var(&head));

    assert!(read.is_err_and(|spent| spent.is_exhausted()));
}
