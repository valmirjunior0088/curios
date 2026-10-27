//! Eta at a function, a pair and a type with no field or one inhabitant — by the goal's type, and by a literal where no type directs it.

use {
    super::test_support::*,
    crate::convert,
    curios_core::{
        Cases, Free, Global, Intrinsic, Match, MatchResult, StructDecl, StructType, Subterm,
        Telescope, Term, UniverseContext, Universes,
    },
    curios_utilities::Qualifier,
};

/// Eta at a function type: `f` and `(x) => f(x)` are the same function, and neither side has to be written in the other's shape for conversion to see it.
#[test]
fn eta_makes_a_function_converge_with_its_expansion() {
    let mut kernel = kernel();
    let f = binder(0, "f");
    let x = binder(1, "x");
    let arrow = Term::func_type([(x, nat_type())], nat_type());

    let expanded = Term::func(
        [(x, nat_type())],
        Term::apply(Term::free_var(&f), [Term::free_var(&x)]),
    );

    assert_eq!(
        convert(&mut kernel, &arrow, &Term::free_var(&f), &expanded),
        Ok(true),
    );
}

/// Eta at a Σ type: `p` and `(p.0, p.1)` are the same pair.
#[test]
fn eta_makes_a_pair_converge_with_its_projections() {
    let mut kernel = kernel();
    let p = binder(0, "p");
    let pair_type = Term::tuple_type([(binder(8, "a"), nat_type()), (binder(9, "b"), nat_type())]);

    let expanded = Term::tuple([
        Term::proj(Term::free_var(&p), 0),
        Term::proj(Term::free_var(&p), 1),
    ]);

    assert_eq!(
        convert(&mut kernel, &pair_type, &Term::free_var(&p), &expanded),
        Ok(true),
    );
}

/// Eta by the lambda, where no type directs it. At `Type` — what a stuck elimination's arm and a projection's head are compared at — a lambda converges with the neutral it expands, the neutral applied to the lambda's own binder. The lambda is no forwarder, `(x) => f(x + 0)`, so `whnf` does not contract it and the rule is what decides. The near miss drops the binder and is refused: the rule compares the body.
///
/// Mutation-checked: without `function_eta`'s arms the first two are refused, and with its body comparison answered `true` the third is accepted.
#[test]
fn eta_by_the_lambda_converges_a_function_with_a_neutral_at_no_type() {
    let mut kernel = kernel();
    let (f, x) = (binder(0, "f"), binder(1, "x"));
    kernel.assume(&f, &Term::func_type([(x, nat_type())], nat_type()));

    let expansion = |argument: Term| {
        Term::func(
            [(x, nat_type())],
            Term::apply(Term::free_var(&f), [argument]),
        )
    };
    let past_a_fold = expansion(Term::intrinsic(Intrinsic::nat_add(
        Term::free_var(&x),
        nat(0),
    )));
    let dropped = expansion(nat(0));
    let (ground, neutral) = (Term::type_ground(), Term::free_var(&f));

    assert_eq!(
        convert(&mut kernel, &ground, &past_a_fold, &neutral),
        Ok(true)
    );
    assert_eq!(
        convert(&mut kernel, &ground, &neutral, &past_a_fold),
        Ok(true)
    );
    assert_eq!(convert(&mut kernel, &ground, &dropped, &neutral), Ok(false));
}

/// Eta by the literal at a record, where no type directs it: a tuple literal converges with the neutral it projects, field by field, and one with its components swapped does not.
///
/// Mutation-checked: without `tuple_eta`'s arms the first two are refused, and with its field comparison answered `true` the third is accepted.
#[test]
fn eta_by_the_literal_converges_a_pair_with_a_neutral_at_no_type() {
    let mut kernel = kernel();
    let p = binder(0, "p");
    let project = |index| Term::proj(Term::free_var(&p), index);
    let expansion = Term::tuple([project(0), project(1)]);
    let swapped = Term::tuple([project(1), project(0)]);
    let (ground, neutral) = (Term::type_ground(), Term::free_var(&p));

    assert_eq!(
        convert(&mut kernel, &ground, &expansion, &neutral),
        Ok(true)
    );
    assert_eq!(
        convert(&mut kernel, &ground, &neutral, &expansion),
        Ok(true)
    );
    assert_eq!(convert(&mut kernel, &ground, &swapped, &neutral), Ok(false));
}

/// The unit literal has no field to compare, so against a neutral its walk is empty and answers `true`: `{}` has one inhabitant, and a neutral of the literal's type is it.
#[test]
fn the_unit_literal_converges_with_a_neutral() {
    let mut kernel = kernel();
    let neutral = Term::free_var(&binder(0, "u"));
    let unit = Term::tuple(Vec::<Term>::new());

    assert_eq!(
        convert(&mut kernel, &Term::tuple_type_unit(), &unit, &neutral),
        Ok(true)
    );
    assert_eq!(
        convert(&mut kernel, &Term::type_ground(), &neutral, &unit),
        Ok(true)
    );
}

/// Unit eta, by the goal's type: at the empty Σ and at a nominal struct that declares no field, any two terms converge without being read — two variables, and two stuck applications of two heads, which no structural rule equates. It composes with the eta that reaches it: two variables at a record of units converge by their projections, and two at a function into a unit by their applications.
///
/// The rule is the type's. At `Type`, what a child with no typed context is compared at, two variables stay apart, and so do two at a record or a struct that has a relevant field.
///
/// Mutation-checked: with the empty Σ left to the structural rules the first goal is refused, without the struct's arm the field-less struct's is, and with that arm taken whatever the declaration's fields the last goal is accepted.
#[test]
fn any_two_terms_converge_at_a_type_with_no_field() {
    let mut kernel = kernel();
    let (u, v) = (
        Term::free_var(&binder(0, "u")),
        Term::free_var(&binder(1, "v")),
    );
    let applied = |head: u32, hint: &str, argument: usize| {
        Term::apply(Term::free_var(&binder(head, hint)), [nat(argument)])
    };
    let field_less = declare_struct(&mut kernel, "U", Telescope::done(()));
    let one_field = declare_struct(
        &mut kernel,
        "S",
        Telescope::build([(binder(8, "a"), nat_type())], ()),
    );

    let unit = Term::tuple_type_unit();
    let record_of_units = Term::tuple_type([
        (binder(8, "a"), Term::tuple_type_unit()),
        (binder(9, "b"), Term::tuple_type_unit()),
    ]);
    let function_into_unit = Term::func_type([(binder(8, "x"), nat_type())], unit.clone());
    let record_of_a_number = Term::tuple_type([(binder(8, "a"), nat_type())]);

    assert_eq!(convert(&mut kernel, &unit, &u, &v), Ok(true));
    assert_eq!(
        convert(&mut kernel, &unit, &applied(2, "f", 0), &applied(3, "g", 1)),
        Ok(true)
    );
    assert_eq!(convert(&mut kernel, &field_less, &u, &v), Ok(true));
    assert_eq!(convert(&mut kernel, &record_of_units, &u, &v), Ok(true));
    assert_eq!(convert(&mut kernel, &function_into_unit, &u, &v), Ok(true));

    assert_eq!(
        convert(&mut kernel, &Term::type_ground(), &u, &v),
        Ok(false)
    );
    assert_eq!(convert(&mut kernel, &record_of_a_number, &u, &v), Ok(false));
    assert_eq!(convert(&mut kernel, &one_field, &u, &v), Ok(false));
}

/// A nominal struct has no eta by its type, so what eta would decide between two terms at one is read off the type: any two converge at a struct every field of which has one inhabitant — a unit, a proof, a function into a unit, a record of such, another such struct — whatever their shapes. The elaborator's twin of this proposition shares the name.
///
/// The refusals are the rule's bounds. One relevant field keeps two variables apart, at whatever depth it sits; and a struct that reaches itself, by its own fields or through another's, answers no where it is met again, which is what ends the walk. A struct nested in its own parameter is met again too, and is judged again: its declaration names no struct, and the parameter is what bounds the walk.
///
/// Mutation-checked: with the struct left to the structural rules the five held goals are refused; with every struct counted the four refused goals are accepted; with every struct met again refused the goal at the struct nested in its own parameter is; with none refused the last two goals spend the budget; and with a declaration read for its own name alone the last one does.
#[test]
fn any_two_terms_converge_at_a_struct_with_one_inhabitant() {
    let mut kernel = kernel();
    let (u, v) = (
        Term::free_var(&binder(0, "u")),
        Term::free_var(&binder(1, "v")),
    );
    let applied = |head: u32, hint: &str, argument: usize| {
        Term::apply(Term::free_var(&binder(head, hint)), [nat(argument)])
    };
    let unit = Term::tuple_type_unit;
    let proposition = declare(&mut kernel, "P", Term::prop());
    let mut declared = |path: &str, fields: Vec<(Free, Term)>| {
        declare_struct(&mut kernel, path, Telescope::build(fields, ()))
    };

    let of_a_unit = declared("W", vec![(binder(8, "u"), unit())]);
    let of_a_proof = declared("Proved", vec![(binder(8, "p"), proposition)]);
    let nested = declared(
        "N",
        vec![
            (binder(8, "w"), of_a_unit.clone()),
            (
                binder(9, "f"),
                Term::func_type([(binder(10, "x"), nat_type())], unit()),
            ),
            (
                binder(11, "r"),
                Term::tuple_type([
                    (binder(12, "a"), unit()),
                    (binder(13, "b"), of_a_proof.clone()),
                ]),
            ),
        ],
    );
    let of_a_number = declared(
        "V",
        vec![(binder(8, "n"), nat_type()), (binder(9, "u"), unit())],
    );
    let over_a_number = declared(
        "M",
        vec![
            (binder(8, "w"), of_a_unit.clone()),
            (binder(9, "v"), of_a_number.clone()),
        ],
    );
    // A struct over one parameter, named by its path: its declaration names a struct only where a field does.
    let at = |path: &str| {
        let name = Global::Authored(Qualifier::from([path]));

        move |param: Term| {
            Term::from(Subterm::StructType(StructType {
                name,
                universes: Universes::default(),
                params: vec![param],
            }))
        }
    };
    let (n, m) = (binder(8, "n"), binder(9, "m"));
    let mut over = |path: &str, parameter: Term, fields: Vec<(Free, Term)>| {
        kernel
            .declare_struct(
                &Global::Authored(Qualifier::from([path])),
                &StructDecl {
                    universe_context: UniverseContext::default(),
                    arity: Telescope::build([(n, parameter)], Telescope::build(fields, ())),
                    result_sort: Term::type_ground(),
                    module: Qualifier::from([path]),
                    rep_public: true,
                    polarities: Vec::new(),
                    variances: Vec::new(),
                },
            )
            .expect("a name is declared once");
    };
    let after = |path: &str| Term::func_type([(m, nat_type())], at(path)(Term::free_var(&m)));
    // `struct R(n: Nat) { next: (m: Nat) -> R(m) }`, which reaches itself at another parameter each time.
    over("R", nat_type(), vec![(binder(10, "next"), after("R"))]);
    // `struct Left(n: Nat) { right: (m: Nat) -> Right(m) }` and `Right`, its mirror: each reaches itself through the other.
    over(
        "Left",
        nat_type(),
        vec![(binder(10, "right"), after("Right"))],
    );
    over(
        "Right",
        nat_type(),
        vec![(binder(10, "left"), after("Left"))],
    );
    // `struct Pair(A: Type) { a: A, b: A }`, which names no struct: nested in its own parameter it is met again, and its declaration does not reach itself.
    over(
        "Pair",
        Term::type_ground(),
        vec![
            (binder(10, "a"), Term::free_var(&n)),
            (binder(11, "b"), Term::free_var(&n)),
        ],
    );
    let pair = at("Pair");

    assert_eq!(
        [
            convert(&mut kernel, &of_a_unit, &u, &v),
            convert(
                &mut kernel,
                &of_a_unit,
                &applied(2, "f", 0),
                &applied(3, "g", 1)
            ),
            convert(&mut kernel, &of_a_proof, &u, &v),
            convert(&mut kernel, &nested, &u, &v),
            convert(&mut kernel, &pair(pair(unit())), &u, &v),
        ],
        [Ok(true), Ok(true), Ok(true), Ok(true), Ok(true)]
    );
    assert_eq!(
        [
            convert(&mut kernel, &of_a_number, &u, &v),
            convert(&mut kernel, &over_a_number, &u, &v),
            convert(&mut kernel, &pair(pair(nat_type())), &u, &v),
            convert(&mut kernel, &at("R")(nat(0)), &u, &v),
            convert(&mut kernel, &at("Left")(nat(0)), &u, &v),
        ],
        [Ok(false), Ok(false), Ok(false), Ok(false), Ok(false)]
    );
}

/// A goal's type is read forced: a member of a declaration group is folded in weak-head form, and every rule a type directs is keyed on the type's own shape. Two variables converge at a group's member that is a struct of a unit, closed or over a variable, the unit, a record of units or a function into a unit, and stay apart at one that is a struct or a record with a relevant field. The elaborator's twin of this proposition shares the name.
///
/// Mutation-checked: with the type read in weak-head form, the five held goals are refused.
#[test]
fn a_goals_type_is_read_forced() {
    let mut kernel = kernel();
    let (u, v) = (
        Term::free_var(&binder(0, "u")),
        Term::free_var(&binder(1, "v")),
    );
    let (a, b, x) = (binder(8, "a"), binder(9, "b"), binder(10, "x"));
    let unit = Term::tuple_type_unit;
    let of_a_unit = declare_struct(&mut kernel, "W", Telescope::build([(a, unit())], ()));
    let of_a_number = declare_struct(&mut kernel, "V", Telescope::build([(a, nat_type())], ()));
    // `struct Tagged(n: Nat) { u: {} }` at a variable: a type with one inhabitant that is not closed.
    let over_a_variable = {
        let name = Global::Authored(Qualifier::from(["Tagged"]));
        kernel
            .declare_struct(
                &name,
                &StructDecl {
                    universe_context: UniverseContext::default(),
                    arity: Telescope::build(
                        [(binder(11, "n"), nat_type())],
                        Telescope::build([(a, unit())], ()),
                    ),
                    result_sort: Term::type_ground(),
                    module: Qualifier::from(["Tagged"]),
                    rep_public: true,
                    polarities: Vec::new(),
                    variances: Vec::new(),
                },
            )
            .expect("a name is declared once");

        Term::from(Subterm::StructType(StructType {
            name,
            universes: Universes::default(),
            params: vec![Term::free_var(&binder(12, "k"))],
        }))
    };
    // `rec First : Type = <body> and Second : Type = {}; First`, the spelling a declaration in a group is referred to by.
    let member = |body: Term| {
        let (first, second) = (binder(20, "First"), binder(21, "Second"));

        Term::rec(
            [
                (first, Term::type_ground(), body),
                (second, Term::type_ground(), Term::tuple_type_unit()),
            ],
            Term::free_var(&first),
        )
    };

    assert_eq!(
        [
            convert(&mut kernel, &member(of_a_unit), &u, &v),
            convert(&mut kernel, &member(over_a_variable), &u, &v),
            convert(&mut kernel, &member(unit()), &u, &v),
            convert(
                &mut kernel,
                &member(Term::tuple_type([(a, unit()), (b, unit())])),
                &u,
                &v
            ),
            convert(
                &mut kernel,
                &member(Term::func_type([(x, nat_type())], unit())),
                &u,
                &v
            ),
        ],
        [Ok(true), Ok(true), Ok(true), Ok(true), Ok(true)]
    );
    assert_eq!(
        [
            convert(&mut kernel, &member(of_a_number), &u, &v),
            convert(
                &mut kernel,
                &member(Term::tuple_type([(a, nat_type()), (b, unit())])),
                &u,
                &v
            ),
        ],
        [Ok(false), Ok(false)]
    );
}

/// Eta by the goal's type is fired ahead of every structural rule, whatever the two sides' shapes: two stuck applications of two heads converge at a record of units, by their projections, and at a function into a unit, by their applications, where a structural comparison would set head against head and refuse. At a record with a relevant field the projections are compared, and the two stay apart. The elaborator's twin of this proposition shares the name.
#[test]
fn eta_by_the_goals_type_is_fired_whatever_the_two_sides_shapes() {
    let mut kernel = kernel();
    let applied = |head: u32, hint: &str, argument: usize| {
        Term::apply(Term::free_var(&binder(head, hint)), [nat(argument)])
    };
    let (this, that) = (applied(0, "s", 0), applied(1, "t", 1));
    let (a, b) = (binder(8, "a"), binder(9, "b"));
    let unit = Term::tuple_type_unit();

    let record_of_units = Term::tuple_type([(a, unit.clone()), (b, unit.clone())]);
    let function_into_unit = Term::func_type([(a, nat_type())], unit.clone());
    let record_of_a_number = Term::tuple_type([(a, nat_type()), (b, unit)]);

    assert_eq!(
        [
            convert(&mut kernel, &record_of_units, &this, &that),
            convert(&mut kernel, &function_into_unit, &this, &that),
            convert(&mut kernel, &record_of_a_number, &this, &that),
        ],
        [Ok(true), Ok(true), Ok(false)]
    );
}

/// A literal's eta is the goal type's to refuse. At a type former that is not the literal's own the two sides are not of one type, and the neutral restriction alone would let a literal with no field — whose walk compares nothing — convert with any neutral: a lambda, the unit literal and a field-less struct's literal against a neutral at `Nat`, and that struct's literal at another struct, are refused. Each is still taken at the literal's own type, and at `Type`, which says nothing. The elaborator's twin of this proposition shares the name.
///
/// Mutation-checked: without the refusal for a lambda and a tuple the first two goals are accepted, without the struct's the next two are, and with a sort counted a former the goals at `Type` are refused.
#[test]
fn eta_by_a_literal_is_refused_at_a_type_former_that_is_not_its_own() {
    let mut kernel = kernel();
    let (f, x) = (binder(0, "f"), binder(1, "x"));
    kernel.assume(&f, &Term::func_type([(x, nat_type())], nat_type()));
    let field_less = declare_struct(&mut kernel, "U", Telescope::done(()));
    let one_field = declare_struct(
        &mut kernel,
        "S",
        Telescope::build([(binder(8, "a"), nat_type())], ()),
    );

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
    let empty = Term::struct_(
        Global::Authored(Qualifier::from(["U"])),
        Vec::<Term>::new(),
        Vec::<Term>::new(),
    );
    let (function, neutral) = (Term::free_var(&f), Term::free_var(&binder(2, "n")));
    let (ground, number) = (Term::type_ground(), nat_type());
    let mut at = |type_: &Term, this: &Term, that: &Term| convert(&mut kernel, type_, this, that);

    assert_eq!(
        [
            at(&number, &expansion, &function),
            at(&number, &unit, &neutral),
            at(&number, &empty, &neutral),
            at(&one_field, &neutral, &empty),
        ],
        [Ok(false), Ok(false), Ok(false), Ok(false)]
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

/// A literal's eta is taken against every neutral inhabitant, a term whose head does not show its type, and against no other. A stuck elimination is one as a variable is: a lambda, a tuple literal and a struct literal each converge with the stuck elimination they expand, where no type directs the rule and at the struct's own type — the lambda past a fold, since one that only forwards reduces to what it forwards — and each near miss beside them — a binder dropped, two components swapped, two fields swapped — is refused. Against a head that shows another type — a number, a lambda, a constructor's value — the rule is not taken, and the unit literal's empty walk equates it with nothing. The elaborator's twin of this proposition shares the name.
///
/// Mutation-checked: with the set narrowed to a variable, a projection and an application the four held goals are refused, and with every head counted a neutral the unit literal converges with a number.
#[test]
fn eta_by_a_literal_is_taken_against_a_neutral_alone() {
    let mut kernel = kernel();
    let x = binder(0, "x");
    let ground = Term::type_ground();
    let empty = declare(&mut kernel, "E", Term::type_ground());
    let c = binder(1, "c");
    kernel.assume(&c, &empty);
    let fields = || [(binder(8, "a"), nat_type()), (binder(9, "b"), nat_type())];
    let record = declare_struct(&mut kernel, "S", Telescope::build(fields(), ()));
    let built = declare(&mut kernel, "D", Term::type_ground());

    // `match c end` at an ambient goal: an elimination with no arm, stuck on a variable.
    let stuck = |goal: Term| {
        Term::from(Subterm::Match(Match {
            head: Term::free_var(&c),
            result: MatchResult::Ambient(goal),
            cases: Cases::Induct {
                cases: Vec::new(),
                default: None,
                stated: None,
            },
        }))
    };
    let function = stuck(Term::func_type([(x, nat_type())], nat_type()));
    let pair = stuck(Term::tuple_type(fields()));
    let structure = stuck(record.clone());
    let literal = |first: usize, second: usize| {
        Term::struct_(
            Global::Authored(Qualifier::from(["S"])),
            Vec::<Term>::new(),
            [
                Term::proj_struct(
                    structure.clone(),
                    Global::Authored(Qualifier::from(["S"])),
                    first,
                ),
                Term::proj_struct(
                    structure.clone(),
                    Global::Authored(Qualifier::from(["S"])),
                    second,
                ),
            ],
        )
    };
    let applied =
        |argument: Term| Term::func([(x, nat_type())], Term::apply(function.clone(), [argument]));
    let projected = |first: usize, second: usize| {
        Term::tuple([
            Term::proj(pair.clone(), first),
            Term::proj(pair.clone(), second),
        ])
    };

    let identity = Term::func([(x, nat_type())], Term::free_var(&x));
    let unit = Term::tuple(Vec::<Term>::new());
    let constructed = Term::variant(
        match &*built {
            Subterm::InductType(family) => family.name,
            _ => unreachable!("a declared family is an inductive type"),
        },
        Vec::<Term>::new(),
        "mk",
        Vec::<Term>::new(),
    );
    let mut at = |type_: &Term, this: &Term, that: &Term| convert(&mut kernel, type_, this, that);

    assert_eq!(
        [
            at(
                &ground,
                &applied(Term::intrinsic(Intrinsic::nat_add(
                    Term::free_var(&x),
                    nat(0),
                ))),
                &function,
            ),
            at(&ground, &projected(0, 1), &pair),
            at(&ground, &literal(0, 1), &structure),
            at(&record, &literal(0, 1), &structure),
        ],
        [Ok(true), Ok(true), Ok(true), Ok(true)]
    );
    assert_eq!(
        [
            at(&ground, &applied(nat(0)), &function),
            at(&ground, &projected(1, 0), &pair),
            at(&ground, &literal(1, 0), &structure),
        ],
        [Ok(false), Ok(false), Ok(false)]
    );
    assert_eq!(
        [
            at(&ground, &identity, &nat(1)),
            at(&ground, &unit, &nat(1)),
            at(&ground, &unit, &identity),
            at(&ground, &literal(0, 1), &constructed),
        ],
        [Ok(false), Ok(false), Ok(false), Ok(false)]
    );
}

/// A struct literal with fewer fields than its declaration must not convert with a neutral inhabitant.
///
/// The eta walk is driven by the literal's fields, so a short literal runs out before the declaration's telescope does, and a vacuous remainder that passed would equate a malformed literal with *any* neutral at the type, in the accepting direction. The walk answers with whether it consumed the whole telescope.
#[test]
fn a_short_struct_literal_does_not_convert_with_a_neutral() {
    let mut kernel = kernel();
    let name = Global::Authored(Qualifier::from(["S"]));
    kernel
        .declare_struct(
            &name,
            &StructDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::done(Telescope::build(
                    [(binder(8, "a"), nat_type()), (binder(9, "b"), nat_type())],
                    (),
                )),
                result_sort: Term::type_ground(),
                module: Qualifier::from(["S"]),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .expect("a name is declared once");

    let type_ = Term::from(Subterm::StructType(StructType {
        name,
        universes: Universes::default(),
        params: Vec::new(),
    }));
    let literal = Term::struct_(name, Vec::<Term>::new(), Vec::<Term>::new());
    let neutral = Term::free_var(&binder(0, "s"));

    assert_eq!(convert(&mut kernel, &type_, &literal, &neutral), Ok(false));
}
