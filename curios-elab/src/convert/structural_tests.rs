//! Tuples, projections, eta at a known type, and the irrelevance that fires at a computed proposition.

use {
    super::test_support::*,
    crate::*,
    curios_core::{
        Atom, Cases, Exhaustion, Free, Global, InductDecl, InductParam, Intrinsic, Many, Match,
        MatchResult, MetavarId, Scope, StructDecl, StructType, Subterm, Telescope, Term,
        UniverseContext,
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

/// A nominal struct has no eta by its type, so what eta would decide between two terms at one is read off the type: any two converge at a struct every field of which has one inhabitant — a unit, a proof, a function into a unit, a record of such, another such struct — whatever their shapes. The kernel's twin of this proposition shares the name.
///
/// The refusals are the rule's bounds. One relevant field keeps two variables apart, at whatever depth it sits; and a struct that reaches itself, by its own fields or through another's, answers no where it is met again, which is what ends the walk. A struct nested in its own parameter is met again too, and is judged again: its declaration names no struct, and the parameter is what bounds the walk.
///
/// Mutation-checked: with the struct left to the dispatch the five held goals are refused; with every struct counted the four refused goals are accepted; with every struct met again refused the goal at the struct nested in its own parameter is; with none refused the last two goals spend the budget; and with a declaration read for its own name alone the last one does.
#[test]
fn any_two_terms_converge_at_a_struct_with_one_inhabitant() {
    let mut context = context();
    let (f, g) = (context.fresh(Some("f")), context.fresh(Some("g")));
    let (u, v) = (
        Term::free_var(&context.fresh(Some("u"))),
        Term::free_var(&context.fresh(Some("v"))),
    );
    let applied = |head: &Free, argument: usize| Term::apply(Term::free_var(head), [nat(argument)]);
    let unit = Term::tuple_type_unit;
    let proposition = declare_proposition(&mut context, "P");
    let declared = |context: &mut Context, path: &str, fields: Vec<Term>| {
        let fields = fields
            .into_iter()
            .map(|field| (context.fresh(None), field))
            .collect::<Vec<_>>();

        declare_struct(context, path, Telescope::build(fields, ()))
    };

    let of_a_unit = declared(&mut context, "W", vec![unit()]);
    let of_a_proof = declared(&mut context, "Proved", vec![proposition]);
    let x = context.fresh(Some("x"));
    let (a, b) = (context.fresh(Some("a")), context.fresh(Some("b")));
    let nested = declared(
        &mut context,
        "N",
        vec![
            of_a_unit.clone(),
            Term::func_type([(x, nat_type())], unit()),
            Term::tuple_type([(a, unit()), (b, of_a_proof.clone())]),
        ],
    );
    let of_a_number = declared(&mut context, "V", vec![nat_type(), unit()]);
    let over_a_number = declared(
        &mut context,
        "M",
        vec![of_a_unit.clone(), of_a_number.clone()],
    );
    // A struct over one parameter, named by its path: its declaration names a struct only where a field does.
    let at = |path: &str| {
        let name = nominal(path);

        move |param: Term| {
            Term::from(Subterm::StructType(StructType {
                name,
                universes: Vec::new(),
                params: vec![param],
            }))
        }
    };
    let (n, m) = (context.fresh(Some("n")), context.fresh(Some("m")));
    let over = |context: &mut Context, path: &str, parameter: Term, fields: Vec<Term>| {
        let fields = fields
            .into_iter()
            .map(|field| (context.fresh(None), field))
            .collect::<Vec<_>>();

        context
            .register_struct(
                &nominal(path),
                StructDecl {
                    universe_context: UniverseContext::empty(),
                    arity: Telescope::build([(n, parameter)], Telescope::build(fields, ())),
                    result_sort: Term::type_ground(),
                    module: Qualifier::empty(),
                    rep_public: true,
                    polarities: Vec::new(),
                    plicities: Vec::new(),
                },
            )
            .unwrap();
    };
    let after = |path: &str| Term::func_type([(m, nat_type())], at(path)(Term::free_var(&m)));
    // `struct R(n: Nat) { next: (m: Nat) -> R(m) }`, which reaches itself at another parameter each time.
    over(&mut context, "R", nat_type(), vec![after("R")]);
    // `struct Left(n: Nat) { right: (m: Nat) -> Right(m) }` and `Right`, its mirror: each reaches itself through the other.
    over(&mut context, "Left", nat_type(), vec![after("Right")]);
    over(&mut context, "Right", nat_type(), vec![after("Left")]);
    // `struct Pair(A: Type) { a: A, b: A }`, which names no struct: nested in its own parameter it is met again, and its declaration does not reach itself.
    over(
        &mut context,
        "Pair",
        Term::type_ground(),
        vec![Term::free_var(&n), Term::free_var(&n)],
    );
    let pair = at("Pair");

    assert_eq!(
        [
            convert(&mut context, &of_a_unit, &u, &v),
            convert(&mut context, &of_a_unit, &applied(&f, 0), &applied(&g, 1)),
            convert(&mut context, &of_a_proof, &u, &v),
            convert(&mut context, &nested, &u, &v),
            convert(&mut context, &pair(pair(unit())), &u, &v),
        ],
        [Ok(true), Ok(true), Ok(true), Ok(true), Ok(true)]
    );
    assert_eq!(
        [
            convert(&mut context, &of_a_number, &u, &v),
            convert(&mut context, &over_a_number, &u, &v),
            convert(&mut context, &pair(pair(nat_type())), &u, &v),
            convert(&mut context, &at("R")(nat(0)), &u, &v),
            convert(&mut context, &at("Left")(nat(0)), &u, &v),
        ],
        [Ok(false), Ok(false), Ok(false), Ok(false), Ok(false)]
    );
}

/// Eta by the goal's type is fired ahead of every structural rule, whatever the two sides' shapes: two stuck applications of two heads converge at a record of units, by their projections, and at a function into a unit, by their applications, where the structural rule would set head against head and refuse. At a record with a relevant field the projections are compared, and the two stay apart. The kernel's twin of this proposition shares the name.
///
/// Mutation-checked: with eta by the type left to the dispatch's last arm, the two goals a unit decides are refused.
#[test]
fn eta_by_the_goals_type_is_fired_whatever_the_two_sides_shapes() {
    let mut context = context();
    let (s, t) = (context.fresh(Some("s")), context.fresh(Some("t")));
    let (a, b) = (context.fresh(Some("a")), context.fresh(Some("b")));
    let unit = Term::tuple_type_unit();
    let applied = |head: &Free, argument: usize| Term::apply(Term::free_var(head), [nat(argument)]);
    let (this, that) = (applied(&s, 0), applied(&t, 1));

    let record_of_units = Term::tuple_type([(a, unit.clone()), (b, unit.clone())]);
    let function_into_unit = Term::func_type([(a, nat_type())], unit.clone());
    let record_of_a_number = Term::tuple_type([(a, nat_type()), (b, unit)]);

    assert_eq!(
        [
            convert(&mut context, &record_of_units, &this, &that),
            convert(&mut context, &function_into_unit, &this, &that),
            convert(&mut context, &record_of_a_number, &this, &that),
        ],
        [Ok(true), Ok(true), Ok(false)]
    );
}

/// Two calls of one definition are compared by their spines whatever spells them. `h(p)` against `h(q)`, two proofs of one proposition, converges by the spines before either call unfolds, and so does the same pair where a solved metavariable stands for `h(p)` — the side `Eq/refl()`'s implicit leaves once it is solved.
///
/// The control is the same pair over a relevant family, refused under either spelling. The proposition's half holds by either rule — read as written both calls unfold, and the two proofs are a stuck elimination's scrutinees, which the type a lookup gives them equates — so this holds the verdict under both spellings, and reading through the solutions is what keeps the two calls folded.
#[test]
fn two_calls_of_one_definition_convert_by_their_spines_whatever_spells_them() {
    let judged = |sort: Term| {
        let mut context = context();
        context
            .register_induct(
                &nominal("F"),
                InductDecl {
                    universe_context: UniverseContext::empty(),
                    arity: Telescope::done(Telescope::done(())),
                    constructors: Vec::new(),
                    result_sort: sort,
                    module: Qualifier::empty(),
                    rep_public: true,
                    polarities: Vec::new(),
                    plicities: Vec::new(),
                },
            )
            .unwrap();
        let family = Term::induct_type(nominal("F"), Vec::<Term>::new(), Vec::<Term>::new());
        let h = Free::global(Qualifier::from(["h"]));
        let e = context.fresh(Some("e"));
        let (p, q) = (context.fresh(Some("p")), context.fresh(Some("q")));
        context.assume(&p, &family);
        context.assume(&q, &family);

        // `h(e: F) -> Nat = match e end`, an elimination with no arm, stuck on a variable.
        let eliminated = Term::from(Subterm::Match(Match {
            head: Term::free_var(&e),
            result: MatchResult::Ambient(nat_type()),
            cases: Cases::Induct {
                cases: Vec::new(),
                default: None,
            },
        }));
        context.define_assuming(
            &h,
            &Term::func_type([(e, family.clone())], nat_type()),
            &Term::func([(e, family.clone())], eliminated),
            None,
        );
        let call = |proof: &Free| Term::apply(Term::free_var(&h), [Term::free_var(proof)]);

        context.birth_metavar(MetavarId(0), Vec::new(), nat_type());
        context.solve_metavar(MetavarId(0), call(&p));

        [
            convert(&mut context, &nat_type(), &call(&p), &call(&q)),
            convert(&mut context, &nat_type(), &Term::hole(0), &call(&q)),
        ]
    };

    assert_eq!(judged(Term::prop()), [Ok(true), Ok(true)]);
    assert_eq!(judged(Term::type_ground()), [Ok(false), Ok(false)]);
}

/// Where a child is compared with no type, what a type directs between two neutrals is read off the type a lookup gives both: two proofs of one proposition converge at `Type`, and so do two variables at the empty record, at a record of units, at a function into a unit, at a struct that declares no field, at a struct of a unit and at a recursive definition's call that unfolds to one of these, at whatever depth; and two eliminations of two such proofs, whose scrutinees are compared with no type, are one term. It is what keeps a verdict from following whether a definition's call was unfolded before the pair was posed. The kernel's twin of this proposition shares the name.
///
/// Neither side is expanded, and the refusals say what that keeps apart: two variables at a record with a relevant field, two proofs of two propositions and two numbers.
///
/// A looked-up type is read through the solutions already committed: two proofs whose proposition a solved metavariable spells converge as two at the proposition written do.
///
/// Mutation-checked: without the lookup the ten held goals are refused; with the two looked-up types left uncompared two proofs of two propositions converge; and with each looked-up type read as written the pair a solved metavariable types is refused.
#[test]
fn two_neutrals_converge_where_a_lookup_gives_them_a_type_with_one_inhabitant() {
    let mut context = context();
    let (one, another) = (
        declare_proposition(&mut context, "P"),
        declare_proposition(&mut context, "Q"),
    );
    let field_less = declare_struct(&mut context, "U", Telescope::done(()));
    let unit = Term::tuple_type_unit();
    let (a, b, z) = (
        context.fresh(Some("a")),
        context.fresh(Some("b")),
        context.fresh(Some("z")),
    );
    let of_a_unit = declare_struct(&mut context, "W", Telescope::build([(a, unit.clone())], ()));
    let record = Term::tuple_type([(a, nat_type()), (b, unit.clone())]);
    let units = Term::tuple_type([(a, unit.clone()), (b, unit.clone())]);
    let function = Term::func_type([(z, nat_type())], unit.clone());
    // `rec F : (Nat) -> Type = (n) => match n | 0 => {} | pred + 1 => {a: F(pred)}; F`. `F(0)` unfolds to the unit, and `F(1)` to a record of it.
    let nested = {
        let (f, n, motive) = (
            context.fresh(Some("F")),
            context.fresh(Some("n")),
            context.fresh(Some("m")),
        );
        let (pred, ih) = (context.fresh(Some("pred")), context.fresh(Some("ih")));
        let again = Term::apply(Term::free_var(&f), [Term::free_var(&pred)]);
        let family = Term::func_type([(n, nat_type())], Term::type_ground());
        let body = Term::func(
            [(n, nat_type())],
            Term::nat_match(
                Term::free_var(&n),
                Some(&motive),
                Term::type_ground(),
                unit.clone(),
                &pred,
                &ih,
                Term::tuple_type([(a, again)]),
            ),
        );
        let definition = Term::rec([(f, family, body)], Term::free_var(&f));

        move |depth: usize| Term::apply(definition.clone(), [nat(depth)])
    };
    let (folded, folded_twice) = (nested(0), nested(1));
    context.birth_metavar(MetavarId(0), Vec::new(), Term::prop());
    context.solve_metavar(MetavarId(0), one.clone());

    let mut assumed = |hint: &str, type_: &Term| {
        let name = context.fresh(Some(hint));
        context.assume(&name, type_);

        Term::free_var(&name)
    };
    let (p, q, other) = (
        assumed("p", &one),
        assumed("q", &one),
        assumed("r", &another),
    );
    let (solved_p, solved_q) = (assumed("p", &Term::hole(0)), assumed("q", &Term::hole(0)));
    let (u, v) = (assumed("u", &unit), assumed("v", &unit));
    let (s, t) = (assumed("s", &field_less), assumed("t", &field_less));
    let (x, y) = (assumed("x", &record), assumed("y", &record));
    let (f, g) = (assumed("f", &function), assumed("g", &function));
    let (m, n) = (assumed("m", &nat_type()), assumed("n", &nat_type()));
    let (c, d) = (assumed("c", &units), assumed("d", &units));
    let (w, e) = (assumed("w", &of_a_unit), assumed("e", &of_a_unit));
    let (h, k) = (assumed("h", &folded), assumed("k", &folded));
    let (i, j) = (assumed("i", &folded_twice), assumed("j", &folded_twice));

    let eliminated = |proof: &Term| {
        Term::from(Subterm::Match(Match {
            head: proof.clone(),
            result: MatchResult::Ambient(nat_type()),
            cases: Cases::Induct {
                cases: Vec::new(),
                default: None,
            },
        }))
    };
    let (ground, number) = (Term::type_ground(), nat_type());
    let mut at = |type_: &Term, this: &Term, that: &Term| convert(&mut context, type_, this, that);

    assert_eq!(
        [
            at(&ground, &p, &q),
            at(&ground, &solved_p, &solved_q),
            at(&ground, &u, &v),
            at(&ground, &s, &t),
            at(&ground, &c, &d),
            at(&ground, &f, &g),
            at(&ground, &w, &e),
            at(&ground, &h, &k),
            at(&ground, &i, &j),
            at(&number, &eliminated(&p), &eliminated(&q)),
        ],
        [
            Ok(true),
            Ok(true),
            Ok(true),
            Ok(true),
            Ok(true),
            Ok(true),
            Ok(true),
            Ok(true),
            Ok(true),
            Ok(true)
        ]
    );
    assert_eq!(
        [
            at(&ground, &x, &y),
            at(&ground, &p, &other),
            at(&ground, &m, &n),
        ],
        [Ok(false), Ok(false), Ok(false)]
    );
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

/// A nominal family with no constructor, at `sort` and over `indices`.
fn declare_family(context: &mut Context, path: &str, indices: Telescope<()>, sort: Term) -> Global {
    register(context, path, indices, Vec::new(), sort)
}

/// A nominal family registered with `constructors`.
fn register(
    context: &mut Context,
    path: &str,
    indices: Telescope<()>,
    constructors: Vec<(Atom, InductParam)>,
    sort: Term,
) -> Global {
    context
        .register_induct(
            &nominal(path),
            InductDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(indices),
                constructors,
                result_sort: sort,
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                plicities: Vec::new(),
            },
        )
        .unwrap();

    nominal(path)
}

/// A stuck elimination is typed by its result at its scrutinee: an ambient goal as written, and a family opened at the indices its scrutinee's type carries and then at the scrutinee. So where a child is compared with no type, an elimination at a proposition converges with a variable at it and with an elimination of another scrutinee, one at a record of units with a variable at it, and an indexed family's elimination is read at its scrutinee's index. The kernel's twin of this proposition shares the name.
///
/// The refusals are what the type does not decide: an elimination at a relevant type against a variable at it, and one at a proposition against a variable at another.
///
/// Mutation-checked: with no type read off an elimination the four held goals are refused, and with a family read at its scrutinee alone the indexed one is.
#[test]
fn a_stuck_elimination_is_typed_by_its_result_at_its_scrutinee() {
    let mut context = context();
    let (proposition, another) = (
        declare_proposition(&mut context, "P"),
        declare_proposition(&mut context, "Q"),
    );
    let empty = declare_family(&mut context, "E", Telescope::done(()), Term::type_ground());
    let empty = Term::induct_type(empty, Vec::<Term>::new(), Vec::<Term>::new());
    let unit = Term::tuple_type_unit();
    let (a, b, index) = (
        context.fresh(Some("a")),
        context.fresh(Some("b")),
        context.fresh(Some("n")),
    );
    let units = Term::tuple_type([(a, unit.clone()), (b, unit.clone())]);
    let indexed = declare_family(
        &mut context,
        "Ix",
        Telescope::build([(index, nat_type())], ()),
        Term::type_ground(),
    );

    let assumed = |context: &mut Context, hint: &str, type_: &Term| {
        let name = context.fresh(Some(hint));
        context.assume(&name, type_);

        Term::free_var(&name)
    };
    let (c, d) = (
        assumed(&mut context, "c", &empty),
        assumed(&mut context, "d", &empty),
    );
    let (p, q) = (
        assumed(&mut context, "p", &proposition),
        assumed(&mut context, "q", &another),
    );
    let r = assumed(&mut context, "r", &units);
    let n = assumed(&mut context, "n", &nat_type());
    let s = assumed(
        &mut context,
        "s",
        &Term::induct_type(indexed, Vec::<Term>::new(), [nat(3)]),
    );

    let no_arm = || Cases::Induct {
        cases: Vec::new(),
        default: None,
    };
    let eliminated = |scrutinee: &Term, goal: &Term| {
        Term::from(Subterm::Match(Match {
            head: scrutinee.clone(),
            result: MatchResult::Ambient(goal.clone()),
            cases: no_arm(),
        }))
    };
    // `match s : (i, x) => P end`, whose result is a family over the index and the scrutinee.
    let over_its_index = {
        let (i, x) = (context.fresh(Some("i")), context.fresh(Some("x")));

        Term::from(Subterm::Match(Match {
            head: s.clone(),
            result: MatchResult::Family(Scope::close(Many(2), &[&i, &x], proposition.clone())),
            cases: no_arm(),
        }))
    };
    let ground = Term::type_ground();
    let mut at = |this: &Term, that: &Term| convert(&mut context, &ground, this, that);

    assert_eq!(
        [
            at(&eliminated(&c, &proposition), &p),
            at(&eliminated(&c, &proposition), &eliminated(&d, &proposition)),
            at(&eliminated(&c, &units), &r),
            at(&over_its_index, &p),
        ],
        [Ok(true), Ok(true), Ok(true), Ok(true)]
    );
    assert_eq!(
        [
            at(&eliminated(&c, &nat_type()), &n),
            at(&eliminated(&c, &proposition), &q),
        ],
        [Ok(false), Ok(false)]
    );
}

/// A constructor's value is typed by its declaration: the family at the value's parameters and at the index targets its constructor states. So where a child is compared with no type, a proof a constructor builds converges with a proof a variable names and with one another constructor builds. The kernel's twin of this proposition shares the name.
///
/// The refusals: two constructors of a relevant family, a constructor's value against a variable at it, and a proof against a variable at the family's other index.
///
/// Mutation-checked: with no type read off a constructor's value the three held goals are refused, and with the index targets left out the indexed one is.
#[test]
fn a_constructors_value_is_typed_by_its_declaration() {
    let mut context = context();
    let nullary = |tags: [&str; 2]| {
        Vec::from(tags.map(|tag| {
            (
                Atom::from(tag),
                InductParam::new(Telescope::done(Vec::new()), Vec::new()),
            )
        }))
    };
    // `induct Or : Prop | left() | right()`, the same two at `Type`, and `induct Zero : (n : Nat) -> Prop | zero() : (0)`.
    let either = register(
        &mut context,
        "Or",
        Telescope::done(()),
        nullary(["left", "right"]),
        Term::prop(),
    );
    let two = register(
        &mut context,
        "Two",
        Telescope::done(()),
        nullary(["left", "right"]),
        Term::type_ground(),
    );
    let index = context.fresh(Some("n"));
    let zero = register(
        &mut context,
        "Zero",
        Telescope::build([(index, nat_type())], ()),
        Vec::from([(
            Atom::from("zero"),
            InductParam::new(Telescope::done(vec![nat(0)]), Vec::new()),
        )]),
        Term::prop(),
    );

    let built =
        |name: Global, tag: &str| Term::variant(name, Vec::<Term>::new(), tag, Vec::<Term>::new());
    let assumed = |context: &mut Context, hint: &str, type_: &Term| {
        let name = context.fresh(Some(hint));
        context.assume(&name, type_);

        Term::free_var(&name)
    };
    let unindexed = |name: Global| Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new());
    let p = assumed(&mut context, "p", &unindexed(either));
    let t = assumed(&mut context, "t", &unindexed(two));
    let at_zero = assumed(
        &mut context,
        "z",
        &Term::induct_type(zero, Vec::<Term>::new(), [nat(0)]),
    );
    let at_one = assumed(
        &mut context,
        "o",
        &Term::induct_type(zero, Vec::<Term>::new(), [nat(1)]),
    );

    let ground = Term::type_ground();
    let mut at = |this: &Term, that: &Term| convert(&mut context, &ground, this, that);

    assert_eq!(
        [
            at(&built(either, "left"), &p),
            at(&built(either, "left"), &built(either, "right")),
            at(&built(zero, "zero"), &at_zero),
        ],
        [Ok(true), Ok(true), Ok(true)]
    );
    assert_eq!(
        [
            at(&built(two, "left"), &built(two, "right")),
            at(&built(two, "left"), &t),
            at(&built(zero, "zero"), &at_one),
        ],
        [Ok(false), Ok(false), Ok(false)]
    );
}

/// An arm's binders are opened at the types its position gives them: a constructor's arm at its telescope over the scrutinee's parameters, and a cons arm at its carrier's domains. Two eliminations whose arms differ in which of two bound proofs they hand on are one term, as are two folds over a list of proofs, one handing on the head it peeled and the other a proof in scope. The kernel's twin of this proposition shares the name.
///
/// The control for each is the same pair over a relevant family, where the two binders stay apart; and where no lookup types the scrutinee the arm's binders carry no type, under which the first pair stays apart too.
///
/// Mutation-checked: with no type recorded for an arm's binders the two held goals are refused.
#[test]
fn an_arms_binders_are_opened_at_the_types_their_constructor_gives_them() {
    let judged = |sort: Term| {
        let mut context = context();
        let held = declare_family(&mut context, "F", Telescope::done(()), sort);
        let held = Term::induct_type(held, Vec::<Term>::new(), Vec::<Term>::new());
        // `induct Pair : Type | two(a : F, b : F)`.
        let (a, b) = (context.fresh(Some("a")), context.fresh(Some("b")));
        let pair = register(
            &mut context,
            "Pair",
            Telescope::done(()),
            Vec::from([(
                Atom::from("two"),
                InductParam::new(
                    Telescope::build([(a, held.clone()), (b, held.clone())], Vec::new()),
                    vec![Plicity::Explicit, Plicity::Explicit],
                ),
            )]),
            Term::type_ground(),
        );
        let (typed, untyped) = (context.fresh(Some("t")), context.fresh(Some("u")));
        context.assume(
            &typed,
            &Term::induct_type(pair, Vec::<Term>::new(), Vec::<Term>::new()),
        );
        let in_scope = context.fresh(Some("p"));
        context.assume(&in_scope, &held);
        let list = context.fresh(Some("l"));
        context.assume(&list, &Term::intrinsic(Intrinsic::ListType(held.clone())));

        let (x, y) = (context.fresh(Some("x")), context.fresh(Some("y")));
        let result = Term::tuple_type([(x, held.clone()), (y, nat_type())]);
        // `match t | two(a, b) => (<chosen>, 1) end`.
        let handing_on = |context: &mut Context, scrutinee: &Free, first: bool| {
            let (a, b, m) = (
                context.fresh(Some("a")),
                context.fresh(Some("b")),
                context.fresh(Some("m")),
            );
            let chosen = match first {
                true => a,
                false => b,
            };

            Term::induct_match(
                Term::free_var(scrutinee),
                Some(&m),
                result.clone(),
                [(
                    "two",
                    vec![a, b],
                    Term::tuple([Term::free_var(&chosen), nat(1)]),
                )],
            )
        };
        let pairs = [
            (
                handing_on(&mut context, &typed, true),
                handing_on(&mut context, &typed, false),
            ),
            (
                handing_on(&mut context, &untyped, true),
                handing_on(&mut context, &untyped, false),
            ),
        ];
        // `match l | [] => (p, 0) | h ++ t; ih => (<chosen>, 1) end`.
        let folding = |context: &mut Context, peeled: bool| {
            let (h, t, ih, m) = (
                context.fresh(Some("h")),
                context.fresh(Some("t")),
                context.fresh(Some("ih")),
                context.fresh(Some("m")),
            );
            let chosen = match peeled {
                true => h,
                false => in_scope,
            };

            Term::list_match(
                Term::free_var(&list),
                held.clone(),
                Some(&m),
                result.clone(),
                Term::tuple([Term::free_var(&in_scope), nat(0)]),
                &h,
                &t,
                &ih,
                Term::tuple([Term::free_var(&chosen), nat(1)]),
            )
        };
        let folds = (folding(&mut context, true), folding(&mut context, false));
        let ground = Term::type_ground();

        [
            convert(&mut context, &ground, &pairs[0].0, &pairs[0].1),
            convert(&mut context, &ground, &folds.0, &folds.1),
            convert(&mut context, &ground, &pairs[1].0, &pairs[1].1),
        ]
    };

    assert_eq!(judged(Term::prop()), [Ok(true), Ok(true), Ok(false)]);
    assert_eq!(
        judged(Term::type_ground()),
        [Ok(false), Ok(false), Ok(false)]
    );
}

/// A motive's binders are opened at the types its family gives them: the index domains, and the scrutinee at the family over those binders. The kernel's twin of this proposition shares the name.
///
/// Two stuck eliminations of one scrutinee at a family indexed by a proposition, differing only inside their motives. Each motive body is `Wit(<index binder>, i)`, and `Wit`'s index type is its own parameter, so the index pair is compared at the motive's index binder — which is opened at `Prop`, its real type, and is a proposition there: the pair is discharged by irrelevance, as it is where the motive was typed.
///
/// Over a scrutinee no lookup types the binders carry no type, the index pair is compared at a binder nothing classifies, and the two stay apart. The control beside each is a motive pair that differs only by a beta redex, which converges either way.
///
/// Mutation-checked: with no type recorded for a motive's binders the first goal is refused.
#[test]
fn a_motives_binders_are_opened_at_the_types_its_family_gives_them() {
    let mut context = context();
    // `induct Wit(P : Prop) : (p : P) -> Type`, a family whose index type is its own parameter, and `induct Ix : (R : Prop) -> Type`, one indexed by a proposition.
    let (param, index, indexing) = (
        context.fresh(Some("P")),
        context.fresh(Some("p")),
        context.fresh(Some("R")),
    );
    context
        .register_induct(
            &nominal("Wit"),
            InductDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::build(
                    [(param, Term::prop())],
                    Telescope::build([(index, Term::free_var(&param))], ()),
                ),
                constructors: Vec::new(),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                plicities: Vec::new(),
            },
        )
        .unwrap();
    let indexed = declare_family(
        &mut context,
        "Ix",
        Telescope::build([(indexing, Term::prop())], ()),
        Term::type_ground(),
    );
    let proposition = declare_proposition(&mut context, "Q");
    let (typed, untyped) = (context.fresh(Some("s")), context.fresh(Some("t")));
    context.assume(
        &typed,
        &Term::induct_type(indexed, Vec::<Term>::new(), [proposition]),
    );
    let (u, v) = (context.fresh(Some("u")), context.fresh(Some("v")));

    // `match s : (R, x) => Wit(R, <index>) end`.
    let elimination = |context: &mut Context, scrutinee: &Free, index: Term| {
        let (carried, x) = (context.fresh(Some("R")), context.fresh(Some("x")));
        let body = Term::induct_type(nominal("Wit"), [Term::free_var(&carried)], [index]);

        Term::from(Subterm::Match(Match {
            head: Term::free_var(scrutinee),
            result: MatchResult::Family(Scope::close(Many(2), &[&carried, &x], body)),
            cases: Cases::Induct {
                cases: Vec::new(),
                default: None,
            },
        }))
    };
    let redex = |context: &mut Context, name: &Free| {
        let x = context.fresh(Some("x"));

        Term::apply(
            Term::func([(x, nat_type())], Term::free_var(&x)),
            [Term::free_var(name)],
        )
    };
    let ground = Term::type_ground();
    let mut at = |scrutinee: &Free, reduced: bool| {
        let this = elimination(&mut context, scrutinee, Term::free_var(&u));
        let other = match reduced {
            true => redex(&mut context, &u),
            false => Term::free_var(&v),
        };
        let that = elimination(&mut context, scrutinee, other);

        convert(&mut context, &ground, &this, &that)
    };

    assert_eq!(
        [
            at(&typed, false),
            at(&typed, true),
            at(&untyped, false),
            at(&untyped, true),
        ],
        [Ok(true), Ok(true), Ok(false), Ok(true)]
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

/// A nominal family at `Prop` with no constructor, for the goals that need a base proposition.
fn declare_proposition(context: &mut Context, path: &str) -> Term {
    context
        .register_induct(
            &nominal(path),
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

    Term::induct_type(nominal(path), Vec::<Term>::new(), Vec::<Term>::new())
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
