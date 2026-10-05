//! Structural conversion: reflexivity, beta and delta, eta at a function, a pair and a type with no field — by the goal's type, and by a literal where no type directs it — intrinsic congruence, plicity and universe levels.

use {
    super::test_support::*,
    crate::{Error, Kernel, convert},
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Free, FuncType, Global, InstanceHead, Intrinsic, Level, MetavarId, StructDecl, StructType,
        Subterm, Telescope, Term, UniverseContext, Var,
    },
    curios_num::Grain,
    curios_utilities::{Plicity, Qualifier},
};

#[test]
fn a_term_converts_with_itself() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    assert_eq!(
        convert(
            &mut kernel,
            &nat_type(),
            &Term::free_var(&x),
            &Term::free_var(&x)
        ),
        Ok(true),
    );
}

#[test]
fn distinct_literals_do_not_convert() {
    let mut kernel = kernel();

    assert_eq!(
        convert(&mut kernel, &nat_type(), &nat(1), &nat(2)),
        Ok(false)
    );
}

/// Like terms convert and a sum against a literal clashes — decided in the fold, which merges eagerly; this pins that deferring products leaves both untouched, in the trusted checker.
#[test]
fn like_terms_convert_and_a_stuck_sum_clashes_with_a_literal() {
    let mut kernel = kernel();
    let x = binder(0, "x");
    let sum = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), Term::free_var(&x)));
    let scaled = Term::intrinsic(Intrinsic::nat_mul(nat(2), Term::free_var(&x)));
    assert_eq!(
        convert(&mut kernel, &nat_type(), &sum, &scaled),
        Ok(true),
        "x + x converts with 2 · x"
    );
    let stuck = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), nat(1)));
    assert_eq!(
        convert(&mut kernel, &nat_type(), &stuck, &nat(0)),
        Ok(false),
        "x + 1 clashes with 0"
    );
}

/// Conversion is up to computation, so a redex converts with its value.
#[test]
fn beta_equal_terms_convert() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let redex = Term::apply(Term::func([(x, nat_type())], Term::free_var(&x)), [nat(4)]);

    assert_eq!(convert(&mut kernel, &nat_type(), &redex, &nat(4)), Ok(true));
}

#[test]
fn a_definition_converts_with_its_value() {
    let mut kernel = kernel();
    let f = binder(0, "f");
    kernel.define(&f, &nat_type(), &nat(3), &UniverseContext::default());

    assert_eq!(
        convert(&mut kernel, &nat_type(), &Term::free_var(&f), &nat(3)),
        Ok(true),
    );
}

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
                universes: Vec::new(),
                params: vec![param],
            }))
        }
    };
    let (n, m) = (binder(8, "n"), binder(9, "m"));
    let mut over = |path: &str, parameter: Term, fields: Vec<(Free, Term)>| {
        kernel.declare_struct(
            &Global::Authored(Qualifier::from([path])),
            &StructDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::build([(n, parameter)], Telescope::build(fields, ())),
                result_sort: Term::type_ground(),
                module: Qualifier::from([path]),
                rep_public: true,
                polarities: Vec::new(),
                plicities: Vec::new(),
            },
        );
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
        kernel.declare_struct(
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
                plicities: Vec::new(),
            },
        );

        Term::from(Subterm::StructType(StructType {
            name,
            universes: Vec::new(),
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

/// A literal's shape fires eta against a neutral inhabitant alone. Against a canonical form — which only a caller comparing two terms of different types could hand over — neither rule fires, and the unit literal's empty walk equates it with no literal of another type.
#[test]
fn eta_by_a_literal_is_taken_against_a_neutral_alone() {
    let mut kernel = kernel();
    let x = binder(0, "x");
    let ground = Term::type_ground();
    let identity = Term::func([(x, nat_type())], Term::free_var(&x));
    let unit = Term::tuple(Vec::<Term>::new());

    assert_eq!(convert(&mut kernel, &ground, &identity, &nat(1)), Ok(false));
    assert_eq!(convert(&mut kernel, &ground, &unit, &nat(1)), Ok(false));
    assert_eq!(convert(&mut kernel, &ground, &unit, &identity), Ok(false));
}

/// A struct literal with fewer fields than its declaration must not convert with a neutral inhabitant.
///
/// The eta walk is driven by the literal's fields, so a short literal runs out before the declaration's telescope does, and a vacuous remainder that passed would equate a malformed literal with *any* neutral at the type, in the accepting direction. The walk answers with whether it consumed the whole telescope.
#[test]
fn a_short_struct_literal_does_not_convert_with_a_neutral() {
    let mut kernel = kernel();
    let name = Global::Authored(Qualifier::from(["S"]));
    kernel.declare_struct(
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
            plicities: Vec::new(),
        },
    );

    let type_ = Term::from(Subterm::StructType(StructType {
        name,
        universes: Vec::new(),
        params: Vec::new(),
    }));
    let literal = Term::struct_(name, Vec::<Term>::new(), Vec::<Term>::new());
    let neutral = Term::free_var(&binder(0, "s"));

    assert_eq!(convert(&mut kernel, &type_, &literal, &neutral), Ok(false));
}

/// An intrinsic is congruent when it is the same operation on convertible operands — decided generically, so no operation can be omitted from the rule.
#[test]
fn an_intrinsic_is_congruent_in_its_operands() {
    let mut kernel = kernel();
    let n = binder(0, "n");
    let m = binder(1, "m");
    kernel.define(
        &m,
        &nat_type(),
        &Term::free_var(&n),
        &UniverseContext::default(),
    );

    let left = Term::intrinsic(Intrinsic::nat_mul(Term::free_var(&n), nat(3)));
    let right = Term::intrinsic(Intrinsic::nat_mul(Term::free_var(&m), nat(3)));

    assert_eq!(convert(&mut kernel, &nat_type(), &left, &right), Ok(true));
}

#[test]
fn different_operations_do_not_convert() {
    let mut kernel = kernel();
    let n = binder(0, "n");

    let add = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(3)));
    let mul = Term::intrinsic(Intrinsic::nat_mul(Term::free_var(&n), nat(3)));

    assert_eq!(convert(&mut kernel, &nat_type(), &add, &mul), Ok(false));
}

/// Two stuck operations of one kind are each the whole of their side, so the pair has no atoms to class and is compared operand by operand. Were the pair handed back as its own two atoms, classing them would ask the comparison already in progress, which the recurrence rule assumes: mutation-checked against the guard in `curios-core`'s `atoms_of`.
#[test]
fn two_stuck_operations_on_different_operands_do_not_convert() {
    let mut kernel = kernel();
    let n = binder(0, "n");
    let m = binder(1, "m");
    let k = binder(2, "k");

    let left = Term::intrinsic(Intrinsic::NatSub(Term::free_var(&n), Term::free_var(&k)));
    let right = Term::intrinsic(Intrinsic::NatSub(Term::free_var(&m), Term::free_var(&k)));

    assert_eq!(convert(&mut kernel, &nat_type(), &left, &right), Ok(false));
}

/// The free-monoid peel is what decides `n + 2 ≡ m + 2` by comparing `n` with `m` rather than comparing two opaque symbolic sums.
#[test]
fn a_shared_successor_floor_is_peeled_before_comparing() {
    let mut kernel = kernel();
    let n = binder(0, "n");
    let m = binder(1, "m");

    let left = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(2)));
    let right = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&m), nat(2)));

    // Distinct symbolic bases: the peel exposes the real disagreement.
    assert_eq!(convert(&mut kernel, &nat_type(), &left, &right), Ok(false));

    // The same base: equal after the shared floor comes off.
    let same = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(2)));
    assert_eq!(convert(&mut kernel, &nat_type(), &left, &same), Ok(true));
}

/// The heads are one number spelled two ways, so the peel cannot strip them, and a peel handing the nesting back intact would leave shape congruence a two-operand concatenation against a three-operand one. Regrouping in the peel is what lets the pair reach the operand comparison that decides `n + m ≡ m + n`.
#[test]
fn a_nested_concatenation_converts_with_its_flat_spelling_past_unlike_heads() {
    let mut kernel = kernel();
    let g = binder(0, "g");
    let n = binder(1, "n");
    let m = binder(2, "m");
    let t = binder(3, "t");
    let h = binder(4, "h");

    let head = |left: &Free, right: &Free| {
        Term::apply(
            Term::free_var(&g),
            [Term::intrinsic(Intrinsic::nat_add(
                Term::free_var(left),
                Term::free_var(right),
            ))],
        )
    };
    let single = Term::intrinsic(Intrinsic::List {
        element: nat_type(),
        items: vec![Term::free_var(&h)],
    });
    let cat = |operands: Vec<Term>| Term::intrinsic(Intrinsic::list_concat(nat_type(), operands));

    let nested = cat(vec![
        cat(vec![head(&n, &m), Term::free_var(&t)]),
        single.clone(),
    ]);
    let flat = cat(vec![head(&m, &n), Term::free_var(&t), single]);

    assert_eq!(
        convert(&mut kernel, &Term::type_ground(), &nested, &flat),
        Ok(true)
    );
}

/// Plicity is part of a function type's identity: `(A) -> A` and `(@A) -> A` have different calling conventions, and conflating them would let a value be applied through the wrong one.
#[test]
fn plicity_distinguishes_two_function_types() {
    let mut kernel = kernel();
    let a = binder(0, "a");

    let explicit = Term::func_type([(a, nat_type())], nat_type());
    let implicit = Term::from(Subterm::FuncType(FuncType::new(
        match &*explicit {
            Subterm::FuncType(func) => func.telescope.clone(),
            _ => unreachable!("built as a function type"),
        },
        vec![Plicity::Implicit],
    )));

    assert_eq!(
        convert(&mut kernel, &Term::type_ground(), &explicit, &implicit),
        Ok(false),
    );
}

/// Two universes convert only at the same level. Cumulativity is a *subtyping* rule and belongs to checking, not here: conversion is symmetric and levels are not.
#[test]
fn universes_convert_only_at_the_same_level() {
    let mut kernel = kernel();
    let zero = Term::type_ground();
    let one = Term::type_at(Level::zero().succ().expect("level zero succeeds"));

    assert_eq!(convert(&mut kernel, &zero, &zero, &zero), Ok(true));
    assert_eq!(convert(&mut kernel, &zero, &zero, &one), Ok(false));
}

/// A metavariable is elaboration-only syntax, and conversion refuses it rather than comparing ids — the exclusion is the kernel's own, not an inherited guarantee of the zonk traversal. Reflexivity is the one admitted case (the syntactic fast path, sound because it decides nothing about the unknown); any comparison that would have to *look* at a metavariable refuses.
#[test]
fn a_metavariable_does_not_convert_with_anything_else() {
    let mut kernel = kernel();
    let left = Term::hole(MetavarId::from(0usize));
    let right = Term::hole(MetavarId::from(1usize));

    assert!(matches!(
        convert(&mut kernel, &Term::type_ground(), &left, &right),
        Err(Error::NotCore(_)),
    ));
    assert!(matches!(
        convert(&mut kernel, &Term::type_ground(), &left, &nat(0)),
        Err(Error::NotCore(_)),
    ));
}

/// A `Bool` connective against a term that is no intrinsic at all — absorption's shape — never reaches the intrinsic congruence, so the truth table over the two sides' atoms is asked at the head dispatch. The control differs at one assignment and stays apart.
#[test]
fn a_bool_tree_converts_with_the_bare_term_it_equals_at_every_assignment() {
    let mut kernel = kernel();
    let (b, c) = (
        Term::free_var(&binder(0, "b")),
        Term::free_var(&binder(1, "c")),
    );
    let bool_type = Term::intrinsic(Intrinsic::BoolType);
    let or = |left: Term, right: Term| Term::intrinsic(Intrinsic::BoolOr(left, right));
    let absorbed = or(
        b.clone(),
        Term::intrinsic(Intrinsic::BoolAnd(b.clone(), c.clone())),
    );

    assert_eq!(convert(&mut kernel, &bool_type, &absorbed, &b), Ok(true));
    assert_eq!(convert(&mut kernel, &bool_type, &b, &absorbed), Ok(true));
    assert_eq!(
        convert(&mut kernel, &bool_type, &or(b.clone(), c), &b),
        Ok(false)
    );
}

/// Two applications of one `Nat`-valued operation that differ only in a universe instance are one number: `len(n<0>)` and `len(n<1>)` meet before their congruence, which would compare the two instances' levels and refuse. A number never depends on a level, which is the licence the cancellation already reads inside a sum. The control is two different operands at one instance, which stay apart. The elaborator's twin of this proposition shares the name.
#[test]
fn one_operation_at_two_universe_instances_is_one_number() {
    let mut kernel = kernel();
    let length = |operand: &Free, level: u32| {
        Term::intrinsic(Intrinsic::bin_len(
            Grain::X,
            Term::instance(
                InstanceHead::Var(Var::free(*operand)),
                vec![Level::constant(level)],
            ),
        ))
    };
    let (n, m) = (binder(60, "n"), binder(61, "m"));

    assert_eq!(
        convert(&mut kernel, &nat_type(), &length(&n, 0), &length(&n, 1)),
        Ok(true)
    );
    assert_eq!(
        convert(&mut kernel, &nat_type(), &length(&n, 0), &length(&m, 0)),
        Ok(false)
    );
}

/// The levels a result carries are compared before its operands: two polls of one cell whose results are the cell's family at two ground levels are two terms, though every operand agrees. The control is the same poll at one level. The elaborator's twin of this proposition shares the name.
#[test]
fn two_polls_of_one_cell_at_two_ground_levels_do_not_convert() {
    let mut kernel = kernel();
    let cell = Term::free_var(&binder(62, "cell"));
    let poll = |level: u32| {
        Term::intrinsic(Intrinsic::CellPoll {
            element: nat_type(),
            cell: cell.clone(),
            universes: vec![Level::constant(level)],
        })
    };

    assert_eq!(
        convert(&mut kernel, &Term::type_ground(), &poll(0), &poll(1)),
        Ok(false)
    );
    assert_eq!(
        convert(&mut kernel, &Term::type_ground(), &poll(0), &poll(0)),
        Ok(true)
    );
}

/// `f : (Nat, Nat) -> Nat`, `n : Nat` and `m : Nat` assumed, for the comparisons below.
fn over_a_function(mut kernel: Kernel, f: &Free, n: &Free, m: &Free) -> Kernel {
    kernel.assume(
        f,
        &Term::func_type(
            [(binder(10, "a"), nat_type()), (binder(11, "b"), nat_type())],
            nat_type(),
        ),
    );
    kernel.assume(n, &nat_type());
    kernel.assume(m, &nat_type());

    kernel
}

/// Two terms that are equal graphs built apart are compared once per pair of their nodes. Sixty levels of `f(x, x)` over a local function — one tower over `n`, one over a redex that reduces to it — convert in their size, and a tower over another local is refused in its size. At a depth the uncached kernel affords, both give the same verdicts.
///
/// Mutation-checked: with no remembered verdict answered, the sixty-level comparison runs the budget out.
#[test]
fn two_towers_built_apart_are_compared_once_per_pair_of_nodes() {
    let f = binder(0, "f");
    let n = binder(1, "n");
    let m = binder(2, "m");
    let y = binder(3, "y");
    let tower = |base: Term, depth: usize| {
        (0..depth).fold(base, |term, _| {
            Term::apply(Term::free_var(&f), [term.clone(), term])
        })
    };
    let redex = || {
        Term::apply(
            Term::func([(y, nat_type())], Term::free_var(&y)),
            [Term::free_var(&n)],
        )
    };
    let verdicts = |kernel: Kernel, depth: usize| {
        let mut kernel = over_a_function(kernel, &f, &n, &m);

        [
            convert(
                &mut kernel,
                &nat_type(),
                &tower(Term::free_var(&n), depth),
                &tower(redex(), depth),
            ),
            convert(
                &mut kernel,
                &nat_type(),
                &tower(Term::free_var(&n), depth),
                &tower(Term::free_var(&m), depth),
            ),
        ]
    };

    assert_eq!(verdicts(kernel(), 60), [Ok(true), Ok(false)]);
    assert_eq!(
        verdicts(kernel(), 8),
        verdicts(Kernel::uncached(100_000, SYNTAX), 8),
    );
}

/// A remembered verdict lives as long as the equations it was reached under: `n` and `0` are apart before an arm that assumes `n = 0`, one inside it, and apart after it — and the uncached kernel agrees on all three.
///
/// Mutation-checked: with the scoped verdicts left standing where an equation moves, the comparison inside the arm answers what it answered before it.
#[test]
fn a_remembered_verdict_does_not_outlive_the_equations_it_was_reached_under() {
    let sequence = |mut kernel: Kernel| {
        let n = binder(0, "n");
        kernel.assume(&n, &nat_type());
        let compared =
            |kernel: &mut Kernel| convert(kernel, &nat_type(), &Term::free_var(&n), &nat(0));

        let before = compared(&mut kernel);
        let inside = kernel.scoped(|kernel| {
            kernel
                .refine(Term::free_var(&n), nat(0))
                .expect("the equation records");
            compared(kernel)
        });
        let after = compared(&mut kernel);

        [before, inside, after]
    };

    let cached = sequence(kernel());

    assert_eq!(cached, [Ok(false), Ok(true), Ok(false)]);
    assert_eq!(cached, sequence(Kernel::uncached(100_000, SYNTAX)));
}

/// Nor its binder. `p(a)` and `p(b)` are one where `p` takes a proof — its arguments compare at a proposition — and apart where it takes data, and a name assumed again at another type, once its first binder is closed, is compared at the second.
///
/// Mutation-checked: answering a scoped verdict without asking whether its binders stand calls the two applications under the second `h` one.
#[test]
fn a_remembered_verdict_does_not_outlive_its_binder() {
    let mut kernel = kernel();
    let h = binder(0, "h");
    let p = binder(1, "p");
    let a = binder(2, "a");
    let b = binder(3, "b");
    let applied = |argument: &Free| Term::apply(Term::free_var(&p), [Term::free_var(argument)]);
    let mut compared_under = |sort: Term| {
        kernel.scoped(|kernel| {
            kernel.assume(&h, &sort);
            kernel.assume(
                &p,
                &Term::func_type([(binder(4, "x"), Term::free_var(&h))], nat_type()),
            );
            kernel.assume(&a, &Term::free_var(&h));
            kernel.assume(&b, &Term::free_var(&h));

            convert(kernel, &nat_type(), &applied(&a), &applied(&b))
        })
    };

    assert_eq!(compared_under(Term::prop()), Ok(true));
    assert_eq!(compared_under(Term::type_ground()), Ok(false));
}
