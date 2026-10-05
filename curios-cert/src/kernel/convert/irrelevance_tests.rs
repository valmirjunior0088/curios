//! Definitional proof irrelevance: where it fires, where it must not leak, and what a binder's stand-in type decides.

use {
    super::test_support::*,
    crate::{Error, convert},
    curios_core::{
        Atom, Cases, Free, Global, InductDecl, InductParam, Intrinsic, Level, Many, Match,
        MatchResult, Scope, StructDecl, Subterm, Telescope, Term, UniverseContext,
    },
    curios_utilities::{Plicity, Qualifier},
};

/// Proof irrelevance: at a `Prop`-sorted type any two terms convert, *without* either being examined. This is what licenses erasure to drop proofs wholesale.
#[test]
fn any_two_inhabitants_of_a_proposition_convert() {
    let mut kernel = kernel();
    let proposition = declare(&mut kernel, "P", Term::prop());
    let (left, right) = (binder(0, "p"), binder(1, "q"));

    assert_eq!(
        convert(
            &mut kernel,
            &proposition,
            &Term::free_var(&left),
            &Term::free_var(&right),
        ),
        Ok(true),
    );
}

/// **Two applications of one definition are compared by their spines before either is unfolded**, so two proofs passed to it meet irrelevance at the parameter's own type, and neither call is opened to say so. `tests::board`'s row of the same name is the program.
///
/// The control is the same definition over a relevant family: the spines disagree there, the pair unfolds, and the two eliminations stay apart. The proposition's half holds by either rule — unfolded, the two proofs are a stuck elimination's scrutinees, which the type a lookup gives them equates (`two_neutrals_converge_where_a_lookup_gives_them_a_type_with_one_inhabitant`) — so what this holds is the verdict, and that the spines reach it without unfolding is what the comparison costs less by.
#[test]
fn a_definition_applied_to_two_proofs_converts_before_unfolding() {
    let applied_to_two = |sort: Term| {
        let mut kernel = kernel();
        let family = declare(&mut kernel, "F", sort);
        let (h, e) = (binder(60, "h"), binder(61, "e"));
        let eliminated = Term::from(Subterm::Match(Match {
            head: Term::free_var(&e),
            result: MatchResult::Ambient(nat_type()),
            cases: Cases::Induct {
                cases: Vec::new(),
                default: None,
            },
        }));
        kernel.define(
            &h,
            &Term::func_type([(e, family.clone())], nat_type()),
            &Term::func([(e, family.clone())], eliminated),
            &UniverseContext::default(),
        );
        let (left, right) = (binder(62, "p"), binder(63, "q"));
        kernel.assume(&left, &family);
        kernel.assume(&right, &family);

        convert(
            &mut kernel,
            &nat_type(),
            &Term::apply(Term::free_var(&h), [Term::free_var(&left)]),
            &Term::apply(Term::free_var(&h), [Term::free_var(&right)]),
        )
    };

    assert_eq!(applied_to_two(Term::prop()), Ok(true));
    assert_eq!(
        applied_to_two(Term::type_ground()),
        Ok(false),
        "two relevant arguments were identified",
    );
}

/// The spines of two calls of one definition are compared on the pair as posed, ahead of eta by the goal's type, and through a curried spine: at a function type, at a record type, and for the definition's call applied once more, `h(p)(3)`, whose head is the call. `tests::board`'s row of the same name is the programs.
///
/// The control for each is the same definition over a relevant family, where the spines disagree and the pair stays apart. Each held goal holds by either rule: left to eta by the type, both calls unfold and the two proofs are a stuck elimination's scrutinees, which the type a lookup gives them equates. So this holds the verdict at each type, which does not follow where the spine rule sits; trying it first, as the elaborator does, is what keeps the two calls folded.
#[test]
fn two_calls_of_one_definition_convert_by_their_spines_at_any_type() {
    let judged = |sort: Term| {
        let mut kernel = kernel();
        let family = declare(&mut kernel, "F", sort);
        let (e, x) = (binder(61, "e"), binder(64, "x"));
        let (left, right) = (binder(62, "p"), binder(63, "q"));
        kernel.assume(&left, &family);
        kernel.assume(&right, &family);

        // `h(e: F) -> result = match e end`, an elimination with no arm, stuck on a variable.
        let mut define = |index: u32, result: Term| {
            let h = binder(index, "h");
            let eliminated = Term::from(Subterm::Match(Match {
                head: Term::free_var(&e),
                result: MatchResult::Ambient(result.clone()),
                cases: Cases::Induct {
                    cases: Vec::new(),
                    default: None,
                },
            }));
            kernel.define(
                &h,
                &Term::func_type([(e, family.clone())], result),
                &Term::func([(e, family.clone())], eliminated),
                &UniverseContext::default(),
            );

            move |proof: &Free| Term::apply(Term::free_var(&h), [Term::free_var(proof)])
        };

        let arrow = Term::func_type([(x, nat_type())], nat_type());
        let record =
            Term::tuple_type([(binder(65, "a"), nat_type()), (binder(66, "b"), nat_type())]);
        let into_a_function = define(70, arrow.clone());
        let into_a_record = define(71, record.clone());

        [
            convert(
                &mut kernel,
                &arrow,
                &into_a_function(&left),
                &into_a_function(&right),
            ),
            convert(
                &mut kernel,
                &record,
                &into_a_record(&left),
                &into_a_record(&right),
            ),
            convert(
                &mut kernel,
                &nat_type(),
                &Term::apply(into_a_function(&left), [nat(3)]),
                &Term::apply(into_a_function(&right), [nat(3)]),
            ),
        ]
    };

    assert_eq!(judged(Term::prop()), [Ok(true), Ok(true), Ok(true)]);
    assert_eq!(
        judged(Term::type_ground()),
        [Ok(false), Ok(false), Ok(false)]
    );
}

/// A spine's arguments are compared at the types its head assigns under every head a lookup types: past a curried head, `f(0)(p)`, and past a projected one, `r.0(p)`, as past a variable's own.
///
/// The control is the same two spines over a relevant family. Two proofs of one proposition converge compared at `Type` too, by the type a lookup gives each, so this holds the verdict under each head, and the head's telescope is what reaches it without a lookup of every argument.
#[test]
fn a_spines_arguments_are_typed_under_a_curried_and_a_projected_head() {
    let judged = |sort: Term| {
        let mut kernel = kernel();
        let family = declare(&mut kernel, "F", sort);
        let (f, r) = (binder(60, "f"), binder(61, "r"));
        let (left, right) = (binder(62, "p"), binder(63, "q"));
        let (n, e, a, b) = (
            binder(64, "n"),
            binder(65, "e"),
            binder(66, "a"),
            binder(67, "b"),
        );
        kernel.assume(&left, &family);
        kernel.assume(&right, &family);

        // `f : (Nat) -> (F) -> Nat` and `r : {(F) -> Nat, Nat}`.
        let taking_a_proof = Term::func_type([(e, family.clone())], nat_type());
        kernel.assume(
            &f,
            &Term::func_type([(n, nat_type())], taking_a_proof.clone()),
        );
        kernel.assume(
            &r,
            &Term::tuple_type([(a, taking_a_proof), (b, nat_type())]),
        );

        let curried = |proof: &Free| {
            Term::apply(
                Term::apply(Term::free_var(&f), [nat(0)]),
                [Term::free_var(proof)],
            )
        };
        let projected =
            |proof: &Free| Term::apply(Term::proj(Term::free_var(&r), 0), [Term::free_var(proof)]);

        [
            convert(&mut kernel, &nat_type(), &curried(&left), &curried(&right)),
            convert(
                &mut kernel,
                &nat_type(),
                &projected(&left),
                &projected(&right),
            ),
        ]
    };

    assert_eq!(judged(Term::prop()), [Ok(true), Ok(true)]);
    assert_eq!(judged(Term::type_ground()), [Ok(false), Ok(false)]);
}

/// Where a child is compared with no type, what a type directs between two neutrals is read off the type a lookup gives both: two proofs of one proposition converge at `Type`, and so do two variables at the empty record, at a record of units, at a function into a unit, at a struct that declares no field, at a struct of a unit and at a recursive definition's call that unfolds to one of these, at whatever depth; and two eliminations of two such proofs, whose scrutinees are compared with no type, are one term. It is what keeps a verdict from following whether a definition's call was unfolded before the pair was posed. The elaborator's twin of this proposition shares the name.
///
/// Neither side is expanded, and the refusals say what that keeps apart: two variables at a record with a relevant field, two proofs of two propositions and two numbers.
///
/// Mutation-checked: without the lookup the nine held goals are refused; with the two looked-up types left uncompared, two proofs of two propositions converge; and with eta taken at the looked-up type in place of reading it, two variables at a record with a relevant field converge, the projections' heads asking the pair again and the recurrence rule assuming it.
#[test]
fn two_neutrals_converge_where_a_lookup_gives_them_a_type_with_one_inhabitant() {
    let mut kernel = kernel();
    let proposition = declare(&mut kernel, "P", Term::prop());
    let another = declare(&mut kernel, "Q", Term::prop());
    let field_less = declare_struct(&mut kernel, "U", Telescope::done(()));
    let unit = Term::tuple_type_unit();
    let of_a_unit = declare_struct(
        &mut kernel,
        "W",
        Telescope::build([(binder(89, "u"), unit.clone())], ()),
    );
    let record = Term::tuple_type([
        (binder(90, "a"), nat_type()),
        (binder(91, "b"), unit.clone()),
    ]);
    let units = Term::tuple_type([
        (binder(90, "a"), unit.clone()),
        (binder(91, "b"), unit.clone()),
    ]);
    let function = Term::func_type([(binder(92, "x"), nat_type())], unit.clone());
    // `rec F : (Nat) -> Type = (n) => match n | 0 => {} | pred + 1 => {a: F(pred)}; F`. `F(0)` unfolds to the unit, and `F(1)` to a record of it.
    let nested = {
        let (f, n, motive) = (binder(93, "F"), binder(94, "n"), binder(95, "m"));
        let (pred, ih) = (binder(96, "pred"), binder(97, "ih"));
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
                Term::tuple_type([(binder(98, "a"), again)]),
            ),
        );
        let definition = Term::rec([(f, family, body)], Term::free_var(&f));

        move |depth: usize| Term::apply(definition.clone(), [nat(depth)])
    };
    let (folded, folded_twice) = (nested(0), nested(1));

    let mut assumed = |index: u32, hint: &str, type_: &Term| {
        let name = binder(index, hint);
        kernel.assume(&name, type_);

        Term::free_var(&name)
    };
    let (p, q) = (assumed(0, "p", &proposition), assumed(1, "q", &proposition));
    let other = assumed(2, "r", &another);
    let (u, v) = (assumed(3, "u", &unit), assumed(4, "v", &unit));
    let (s, t) = (assumed(5, "s", &field_less), assumed(6, "t", &field_less));
    let (x, y) = (assumed(7, "x", &record), assumed(8, "y", &record));
    let (f, g) = (assumed(9, "f", &function), assumed(10, "g", &function));
    let (a, b) = (assumed(11, "a", &nat_type()), assumed(12, "b", &nat_type()));
    let (c, d) = (assumed(13, "c", &units), assumed(14, "d", &units));
    let (w, z) = (assumed(15, "w", &of_a_unit), assumed(16, "z", &of_a_unit));
    let (h, k) = (assumed(17, "h", &folded), assumed(18, "k", &folded));
    let (i, j) = (
        assumed(19, "i", &folded_twice),
        assumed(20, "j", &folded_twice),
    );

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
    let mut at = |type_: &Term, this: &Term, that: &Term| convert(&mut kernel, type_, this, that);

    assert_eq!(
        [
            at(&ground, &p, &q),
            at(&ground, &u, &v),
            at(&ground, &s, &t),
            at(&ground, &c, &d),
            at(&ground, &f, &g),
            at(&ground, &w, &z),
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
            Ok(true)
        ]
    );
    assert_eq!(
        [
            at(&ground, &x, &y),
            at(&ground, &p, &other),
            at(&ground, &a, &b),
        ],
        [Ok(false), Ok(false), Ok(false)]
    );
}

/// A stuck elimination is typed by its result at its scrutinee: an ambient goal as written, and a family opened at the indices its scrutinee's type carries and then at the scrutinee. So where a child is compared with no type, an elimination at a proposition converges with a variable at it and with an elimination of another scrutinee, one at a record of units with a variable at it, and an indexed family's elimination is read at its scrutinee's index. The elaborator's twin of this proposition shares the name.
///
/// The refusals are what the type does not decide: an elimination at a relevant type against a variable at it, and one at a proposition against a variable at another.
///
/// Mutation-checked: with no type read off an elimination the four held goals are refused, and with a family read at its scrutinee alone the indexed one is.
#[test]
fn a_stuck_elimination_is_typed_by_its_result_at_its_scrutinee() {
    let mut kernel = kernel();
    let proposition = declare(&mut kernel, "P", Term::prop());
    let another = declare(&mut kernel, "Q", Term::prop());
    let empty = declare(&mut kernel, "E", Term::type_ground());
    let unit = Term::tuple_type_unit();
    let units = Term::tuple_type([
        (binder(90, "a"), unit.clone()),
        (binder(91, "b"), unit.clone()),
    ]);
    // `induct Ix : (n : Nat) -> Type`, a family with one index and no constructor.
    let indexed = Global::Authored(Qualifier::from(["Ix"]));
    kernel.declare_induct(
        &indexed,
        &InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::build([(binder(92, "n"), nat_type())], ())),
            constructors: Vec::new(),
            result_sort: Term::type_ground(),
            module: Qualifier::empty(),
            rep_public: true,
            polarities: Vec::new(),
            variances: Vec::new(),
            plicities: Vec::new(),
        },
    );

    let mut assumed = |index: u32, hint: &str, type_: &Term| {
        let name = binder(index, hint);
        kernel.assume(&name, type_);

        Term::free_var(&name)
    };
    let (c, d) = (assumed(0, "c", &empty), assumed(1, "d", &empty));
    let (p, q) = (assumed(2, "p", &proposition), assumed(3, "q", &another));
    let r = assumed(4, "r", &units);
    let n = assumed(5, "n", &nat_type());
    let s = assumed(
        6,
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
        let (i, x) = (binder(93, "i"), binder(94, "x"));

        Term::from(Subterm::Match(Match {
            head: s.clone(),
            result: MatchResult::Family(Scope::close(Many(2), &[&i, &x], proposition.clone())),
            cases: no_arm(),
        }))
    };
    let ground = Term::type_ground();
    let mut at = |this: &Term, that: &Term| convert(&mut kernel, &ground, this, that);

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

/// A constructor's value is typed by its declaration: the family at the value's parameters and at the index targets its constructor states. So where a child is compared with no type, a proof a constructor builds converges with a proof a variable names and with one another constructor builds. The elaborator's twin of this proposition shares the name.
///
/// The refusals: two constructors of a relevant family, a constructor's value against a variable at it, and a proof against a variable at the family's other index.
///
/// Mutation-checked: with no type read off a constructor's value the three held goals are refused, and with the index targets left out the indexed one is.
#[test]
fn a_constructors_value_is_typed_by_its_declaration() {
    let mut kernel = kernel();
    let nullary = |tags: [&str; 2]| {
        Vec::from(tags.map(|tag| {
            (
                Atom::from(tag),
                InductParam::new(Telescope::done(Vec::new()), Vec::new()),
            )
        }))
    };
    let mut family = |path: &str,
                      arity: Telescope<Telescope<()>>,
                      constructors: Vec<(Atom, InductParam)>,
                      result_sort: Term| {
        let name = Global::Authored(Qualifier::from([path]));
        kernel.declare_induct(
            &name,
            &InductDecl {
                universe_context: UniverseContext::default(),
                arity,
                constructors,
                result_sort,
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
                plicities: Vec::new(),
            },
        );

        name
    };
    // `induct Or : Prop | left() | right()`, the same two at `Type`, and `induct Zero : (n : Nat) -> Prop | zero() : (0)`.
    let unindexed = || Telescope::done(Telescope::done(()));
    let either = family("Or", unindexed(), nullary(["left", "right"]), Term::prop());
    let two = family(
        "Two",
        unindexed(),
        nullary(["left", "right"]),
        Term::type_ground(),
    );
    let zero = family(
        "Zero",
        Telescope::done(Telescope::build([(binder(90, "n"), nat_type())], ())),
        Vec::from([(
            Atom::from("zero"),
            InductParam::new(Telescope::done(vec![nat(0)]), Vec::new()),
        )]),
        Term::prop(),
    );

    let built =
        |name: Global, tag: &str| Term::variant(name, Vec::<Term>::new(), tag, Vec::<Term>::new());
    let mut assumed = |index: u32, hint: &str, type_: &Term| {
        let name = binder(index, hint);
        kernel.assume(&name, type_);

        Term::free_var(&name)
    };
    let p = assumed(
        0,
        "p",
        &Term::induct_type(either, Vec::<Term>::new(), Vec::<Term>::new()),
    );
    let t = assumed(
        1,
        "t",
        &Term::induct_type(two, Vec::<Term>::new(), Vec::<Term>::new()),
    );
    let at_zero = assumed(
        2,
        "z",
        &Term::induct_type(zero, Vec::<Term>::new(), [nat(0)]),
    );
    let at_one = assumed(
        3,
        "o",
        &Term::induct_type(zero, Vec::<Term>::new(), [nat(1)]),
    );

    let ground = Term::type_ground();
    let mut at = |this: &Term, that: &Term| convert(&mut kernel, &ground, this, that);

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

/// An arm's binders are opened at the types its position gives them: a constructor's arm at its telescope over the scrutinee's parameters, and a cons arm at its carrier's domains. Two eliminations whose arms differ in which of two bound proofs they hand on are one term, as are two folds over a list of proofs, one handing on the head it peeled and the other a proof in scope. The elaborator's twin of this proposition shares the name.
///
/// The control for each is the same pair over a relevant family, where the two binders stay apart; and where no lookup types the scrutinee the arm keeps the stand-in, under which the first pair stays apart too.
///
/// Mutation-checked: with every arm at the stand-in the two held goals are refused.
#[test]
fn an_arms_binders_are_opened_at_the_types_their_constructor_gives_them() {
    let judged = |sort: Term| {
        let mut kernel = kernel();
        let held = declare(&mut kernel, "F", sort);
        // `induct Pair : Type | two(a : F, b : F)`.
        let pair = Global::Authored(Qualifier::from(["Pair"]));
        kernel.declare_induct(
            &pair,
            &InductDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::done(Telescope::done(())),
                constructors: Vec::from([(
                    Atom::from("two"),
                    InductParam::new(
                        Telescope::build(
                            [
                                (binder(60, "a"), held.clone()),
                                (binder(61, "b"), held.clone()),
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
                variances: Vec::new(),
                plicities: Vec::new(),
            },
        );
        let (typed, untyped) = (binder(0, "t"), binder(1, "u"));
        kernel.assume(
            &typed,
            &Term::induct_type(pair, Vec::<Term>::new(), Vec::<Term>::new()),
        );
        let in_scope = binder(2, "p");
        kernel.assume(&in_scope, &held);
        let list = binder(3, "l");
        kernel.assume(&list, &Term::intrinsic(Intrinsic::ListType(held.clone())));

        // `match t | two(a, b) => (<chosen>, 1) end`, at the ambient `{F, Nat}`.
        let handing_on = |scrutinee: &Free, first: bool| {
            let (a, b, m) = (binder(70, "a"), binder(71, "b"), binder(72, "m"));
            let chosen = match first {
                true => a,
                false => b,
            };

            Term::induct_match(
                Term::free_var(scrutinee),
                Some(&m),
                Term::tuple_type([
                    (binder(73, "x"), held.clone()),
                    (binder(74, "y"), nat_type()),
                ]),
                [(
                    "two",
                    vec![a, b],
                    Term::tuple([Term::free_var(&chosen), nat(1)]),
                )],
            )
        };
        // `match l | [] => (p, 0) | h ++ t; ih => (<chosen>, 1) end`.
        let folding = |peeled: bool| {
            let (h, t, ih, m) = (
                binder(75, "h"),
                binder(76, "t"),
                binder(77, "ih"),
                binder(78, "m"),
            );
            let chosen = match peeled {
                true => h,
                false => in_scope,
            };

            Term::list_match(
                Term::free_var(&list),
                held.clone(),
                Some(&m),
                Term::tuple_type([
                    (binder(73, "x"), held.clone()),
                    (binder(74, "y"), nat_type()),
                ]),
                Term::tuple([Term::free_var(&in_scope), nat(0)]),
                &h,
                &t,
                &ih,
                Term::tuple([Term::free_var(&chosen), nat(1)]),
            )
        };
        let ground = Term::type_ground();

        [
            convert(
                &mut kernel,
                &ground,
                &handing_on(&typed, true),
                &handing_on(&typed, false),
            ),
            convert(&mut kernel, &ground, &folding(true), &folding(false)),
            convert(
                &mut kernel,
                &ground,
                &handing_on(&untyped, true),
                &handing_on(&untyped, false),
            ),
        ]
    };

    assert_eq!(judged(Term::prop()), [Ok(true), Ok(true), Ok(false)]);
    assert_eq!(
        judged(Term::type_ground()),
        [Ok(false), Ok(false), Ok(false)]
    );
}

/// The same two terms at a *relevant* type are not interchangeable. Irrelevance is a property of the type, and this is the direction that would be unsound to get wrong.
#[test]
fn does_not_leak_into_a_relevant_type() {
    let mut kernel = kernel();
    let data = declare(&mut kernel, "D", Term::type_ground());
    let (left, right) = (binder(0, "p"), binder(1, "q"));

    assert_eq!(
        convert(
            &mut kernel,
            &data,
            &Term::free_var(&left),
            &Term::free_var(&right),
        ),
        Ok(false),
    );
}

/// A struct literal's fields compare at the declaration's field telescope, so a field at a proposition is discharged without being read: two literals differing only in a proof are one value. Comparing every field at `Type` would refuse it, and `Str/concat` would associate for the elaborator and not for the kernel.
#[test]
fn a_struct_field_at_a_proposition_is_not_read() {
    let mut kernel = kernel();
    let proposition = declare(&mut kernel, "P", Term::prop());
    let name = curios_core::Global::Authored(Qualifier::from(["Wrap"]));
    kernel.declare_struct(
        &name,
        &StructDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::build(
                [
                    (binder(60, "n"), nat_type()),
                    (binder(61, "p"), proposition.clone()),
                ],
                (),
            )),
            result_sort: Term::type_ground(),
            module: Qualifier::empty(),
            rep_public: true,
            polarities: Vec::new(),
            variances: Vec::new(),
            plicities: Vec::new(),
        },
    );
    let (p, q) = (binder(62, "p"), binder(63, "q"));
    kernel.assume(&p, &proposition);
    kernel.assume(&q, &proposition);

    let this = Term::struct_(name, Vec::<Term>::new(), [nat(1), Term::free_var(&p)]);
    let that = Term::struct_(name, Vec::<Term>::new(), [nat(1), Term::free_var(&q)]);
    assert_eq!(
        convert(&mut kernel, &Term::type_ground(), &this, &that),
        Ok(true),
        "two literals differing only in a proof field did not convert",
    );

    // The control: a relevant field is still compared, so irrelevance did not leak past the proposition.
    let other = Term::struct_(name, Vec::<Term>::new(), [nat(2), Term::free_var(&p)]);
    assert_eq!(
        convert(&mut kernel, &Term::type_ground(), &this, &other),
        Ok(false),
        "a relevant field stopped being compared",
    );
}

/// The constructor twin: a payload compares at the constructor's telescope, so a payload at a proposition is discharged the same way.
#[test]
fn a_constructor_payload_at_a_proposition_is_not_read() {
    let mut kernel = kernel();
    let proposition = declare(&mut kernel, "P", Term::prop());
    let name = curios_core::Global::Authored(Qualifier::from(["Wrap"]));
    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::done(())),
            constructors: Vec::from([(
                Atom::from("wrap"),
                InductParam::new(
                    Telescope::build(
                        [
                            (binder(60, "n"), nat_type()),
                            (binder(61, "p"), proposition.clone()),
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
            variances: Vec::new(),
            plicities: Vec::new(),
        },
    );
    let (p, q) = (binder(62, "p"), binder(63, "q"));
    kernel.assume(&p, &proposition);
    kernel.assume(&q, &proposition);

    let this = Term::variant(
        name,
        Vec::<Term>::new(),
        "wrap",
        [nat(1), Term::free_var(&p)],
    );
    let that = Term::variant(
        name,
        Vec::<Term>::new(),
        "wrap",
        [nat(1), Term::free_var(&q)],
    );
    assert_eq!(
        convert(&mut kernel, &Term::type_ground(), &this, &that),
        Ok(true),
        "two constructions differing only in a proof payload did not convert",
    );

    let other = Term::variant(
        name,
        Vec::<Term>::new(),
        "wrap",
        [nat(2), Term::free_var(&p)],
    );
    assert_eq!(
        convert(&mut kernel, &Term::type_ground(), &this, &other),
        Ok(false),
        "a relevant payload stopped being compared",
    );
}

/// Definitional proof irrelevance at a *computed* proposition — the shape every real firing takes.
///
/// `any_two_inhabitants_of_a_proposition_convert` above uses a nominal `Prop`-sorted family, which is the easy rung: the registry says outright that the type is a proposition. The elaborator's firings are at types that only *compute* to one — a stuck `match` at motive `Prop`, which is how the corpus fixture `/big_nat` states a validity predicate over a `Bool`. Here the rule is inert over every program, because a proof reaching conversion in this crate does so in an untyped child position compared at `Type`; so nothing in any program brings the rule and the shape together, and only a fixture can.
///
/// The shape is worth its own fixture because of what irrelevance trusts. It accepts *without inspecting either term*, and `Sort::of` classifies a stuck `match` by its **motive** rather than by its arms — a claim the term makes about itself, which is exactly why `check_motive` exists to type a motive under its real binders before any rule reads it. At a computed proposition the motive is therefore the whole of what this rule rests on.
///
/// The control is the identical construction with the motive at `Type` and relevant arms in both, which must *not* converge. It separates "reads the motive" from "accepts every stuck match", and without it a rule that skipped the sort test entirely would satisfy the witness.
#[test]
fn fires_at_a_computed_proposition() {
    let mut kernel = kernel();
    let held = declare(&mut kernel, "Held", Term::prop());
    let empty = declare(&mut kernel, "Empty", Term::prop());

    let scrutinee = binder(40, "b");
    kernel.assume(&scrutinee, &Term::intrinsic(Intrinsic::BoolType));

    let computed = Term::bool_match(Term::free_var(&scrutinee), None, Term::prop(), empty, held);

    let (left, right) = (binder(41, "p"), binder(42, "q"));
    kernel.assume(&left, &computed);
    kernel.assume(&right, &computed);

    assert_eq!(
        convert(
            &mut kernel,
            &computed,
            &Term::free_var(&left),
            &Term::free_var(&right),
        ),
        Ok(true),
        "two inhabitants of a proposition the motive computes did not convert",
    );
}

/// The control for the fixture above: the same stuck `match`, with the motive at `Type` and relevant arms, must still distinguish its inhabitants.
#[test]
fn does_not_fire_at_a_computed_relevant_type() {
    let mut kernel = kernel();
    let one = declare(&mut kernel, "One", Term::type_ground());
    let two = declare(&mut kernel, "Two", Term::type_ground());

    let scrutinee = binder(50, "b");
    kernel.assume(&scrutinee, &Term::intrinsic(Intrinsic::BoolType));

    let computed = Term::bool_match(
        Term::free_var(&scrutinee),
        None,
        Term::type_ground(),
        one,
        two,
    );

    let (left, right) = (binder(51, "x"), binder(52, "y"));
    kernel.assume(&left, &computed);
    kernel.assume(&right, &computed);

    assert_eq!(
        convert(
            &mut kernel,
            &computed,
            &Term::free_var(&left),
            &Term::free_var(&right),
        ),
        Ok(false),
        "irrelevance leaked into a relevant type the motive computes",
    );
}

/// **The stand-in a binder is opened at where no lookup types its elimination's scrutinee, held against the types such a binder could really carry.**
///
/// A motive's and an arm's binders are opened at the types their position gives them, read off the scrutinee's looked-up type. Where it has none, `ground_scope` and `motive_binders` open them at one shared set of binders and assume every one of them at `Type`, whatever it really is. The recorded type is not inert: [`synth_neutral`](super::super::sort::synth_neutral) reads the same recorded type through `Kernel::type_of`, so it reaches `Sort::of`, and `Sort::of` is what [`compare`] asks before *every* goal: the proof-irrelevance test at the top of the rule.
///
/// What actually holds the stand-in up is narrower, and is about the value rather than about the readers: `Type` is the least informative answer `Sort::of` can return for a binder. Irrelevance fires on `Sort::Prop` and on nothing else, and eta dispatches on the goal type's own *shape* rather than on the binder's, so a binder recorded at `Type` can only lose the accepting rules, never gain one. This walks one goal at each type the binder could really carry and records what each decides.
///
/// The grid is two side-pairs against four assumed types, because a single pair cannot separate the two things being asked. Distinct sides expose which types *discharge* the goal without comparing — only `Prop` does — and convertible-but-not-identical sides expose which types get as far as comparing at all. The stand-in's row matches the relevant-sort row in both, which is the null: it decides every goal the way a real relevant type decides it.
///
/// **One row is not a forfeiture, and it is the one to carry forward.** A binder whose real type is not a sort at all leaves `Sort::of` with nothing to decode, and the typed opening refuses the whole certification with `NotASort` — while the stand-in classifies it `Type 0` and goes on to accept. There the stand-in is strictly *more* permissive than the truth. Nothing in the stand-in fences that off; what does is a property of its callers, the same shape as the one `struct_eta`'s neutral restriction rests on. A match motive is typed under its real binders by `infer`'s `check_motive` before any comparison grounds it, so a motive using a `Bool`-typed binder as a type never reaches here. Under the typed opening the row does not arise: the binder carries its real type, and the goal is refused as the typed row is.
#[test]
fn a_binders_stand_in_type_decides_a_goal_the_way_a_relevant_type_does() {
    let distinct = || {
        (
            Term::free_var(&binder(70, "u")),
            Term::free_var(&binder(71, "v")),
        )
    };

    // Convertible without being syntactically equal, so the goal survives `compare`'s reflexivity fast path and has to be decided by a rule.
    let convertible = || {
        let x = binder(72, "x");

        (
            Term::apply(Term::func([(x, nat_type())], Term::free_var(&x)), [nat(1)]),
            nat(1),
        )
    };

    let at = |assumed: Term, sides: (Term, Term)| {
        let mut kernel = kernel();
        let hypothesis = binder(73, "h");
        kernel.assume(&hypothesis, &assumed);

        convert(
            &mut kernel,
            &Term::free_var(&hypothesis),
            &sides.0,
            &sides.1,
        )
    };

    let relevant = Term::type_at(Level::constant(3));
    let stand_in = Term::type_ground();
    let not_a_sort = || Err(Error::NotASort(nat_type()));

    // Distinct sides: only a proposition discharges them, and the stand-in is not one.
    assert_eq!(
        at(Term::prop(), distinct()),
        Ok(true),
        "a hypothesis really at `Prop` stopped licensing irrelevance",
    );
    assert_eq!(
        at(relevant.clone(), distinct()),
        Ok(false),
        "a hypothesis at a relevant sort discharged two distinct inhabitants",
    );
    assert_eq!(
        at(stand_in.clone(), distinct()),
        Ok(false),
        "the stand-in discharged a goal a relevant type refuses",
    );
    assert_eq!(
        at(nat_type(), distinct()),
        not_a_sort(),
        "a hypothesis at a non-sort was classified rather than refused",
    );

    // Convertible sides: every sort compares and accepts, and the non-sort still refuses before comparing.
    assert_eq!(
        at(Term::prop(), convertible()),
        Ok(true),
        "a proposition stopped discharging its inhabitants",
    );
    assert_eq!(
        at(relevant, convertible()),
        Ok(true),
        "a relevant sort refused two convertible terms",
    );
    assert_eq!(
        at(stand_in, convertible()),
        Ok(true),
        "the stand-in refused a goal a relevant type accepts",
    );
    assert_eq!(
        at(nat_type(), convertible()),
        not_a_sort(),
        "the non-sort row stopped being the one place the stand-in is the more permissive of the two",
    );
}

/// A motive's binders are opened at the types its family gives them: the index domains, and the scrutinee at the family over those binders.
///
/// Two stuck eliminations of one scrutinee at a family indexed by a proposition, differing only inside their motives. Each motive body is `Wit(<index binder>, i)`, and `Wit`'s index type is its own parameter, so the index pair is compared at the motive's index binder — which is opened at `Prop`, its real type, and is a proposition there: the pair is discharged by irrelevance, as it is where the motive was typed.
///
/// The fallback is the second half: over a scrutinee no lookup types, the binders are opened at the stand-in, the index pair is compared at a binder recorded at `Type`, and the two stay apart, which is the grid's stand-in row above. The control beside each is a motive pair that differs only by a beta redex, which converges under either opening.
///
/// The counterfactual is the last: the name the fallback's binder was opened under, assumed at `Prop` once that binder is closed, is a proposition, and the two bodies compared directly are discharged by irrelevance. So the verdict moves with the binder's type, toward refusal at the stand-in; and a sort read under the closed binder does not answer for the name assumed again.
///
/// Mutation-checked: with every motive binder at the stand-in the first typed goal is refused; and with a remembered sort answering without asking whether its binders stand, the sort the fallback read under its closed binder answers for the name opened again, and a goal after it is refused.
#[test]
fn a_motives_binders_are_opened_at_the_types_its_family_gives_them() {
    let mut kernel = kernel();
    let wit = declare_indexed(&mut kernel, "Wit", Term::prop());
    let proposition = declare(&mut kernel, "Q", Term::prop());
    // `induct Ix : (R : Prop) -> Type`, a family indexed by a proposition.
    let indexed = Global::Authored(Qualifier::from(["Ix"]));
    kernel.declare_induct(
        &indexed,
        &InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::build([(binder(90, "R"), Term::prop())], ())),
            constructors: Vec::new(),
            result_sort: Term::type_ground(),
            module: Qualifier::empty(),
            rep_public: true,
            polarities: Vec::new(),
            variances: Vec::new(),
            plicities: Vec::new(),
        },
    );
    // The names the kernel is not handed sit below the one it is, so no binder it mints meets one of them.
    let (typed, untyped) = (binder(80, "s"), binder(70, "t"));
    kernel.assume(
        &typed,
        &Term::induct_type(indexed, Vec::<Term>::new(), [proposition]),
    );
    let (u, v) = (binder(71, "u"), binder(72, "v"));

    // `match s : (R, x) => Wit(R, <index>) end`.
    let elimination = |scrutinee: &Free, index: Term| {
        let (carried, x) = (binder(84, "R"), binder(85, "x"));
        let body = Term::induct_type(wit, [Term::free_var(&carried)], [index]);

        Term::from(Subterm::Match(Match {
            head: Term::free_var(scrutinee),
            result: MatchResult::Family(Scope::close(Many(2), &[&carried, &x], body)),
            cases: Cases::Induct {
                cases: Vec::new(),
                default: None,
            },
        }))
    };
    let redex = |name: &Free| {
        let x = binder(86, "x");

        Term::apply(
            Term::func([(x, nat_type())], Term::free_var(&x)),
            [Term::free_var(name)],
        )
    };
    let ground = Term::type_ground();
    let mut at = |scrutinee: &Free, this: Term, that: Term| {
        convert(
            &mut kernel,
            &ground,
            &elimination(scrutinee, this),
            &elimination(scrutinee, that),
        )
    };

    // The fallback's goals are put first, so that the first binder the kernel mints — the name after the scrutinee's — is the motive's, opened at the stand-in.
    let fallback = [
        at(&untyped, Term::free_var(&u), Term::free_var(&v)),
        at(&untyped, Term::free_var(&u), redex(&u)),
    ];
    assert_eq!(
        [
            at(&typed, Term::free_var(&u), Term::free_var(&v)),
            at(&typed, Term::free_var(&u), redex(&u)),
        ],
        [Ok(true), Ok(true)]
    );
    assert_eq!(fallback, [Ok(false), Ok(true)]);

    // That binder is closed, and its name is handed in again at `Prop`.
    let carried = binder(81, "R");
    kernel.assume(&carried, &Term::prop());
    let body =
        |index: &Free| Term::induct_type(wit, [Term::free_var(&carried)], [Term::free_var(index)]);

    assert_eq!(
        convert(&mut kernel, &ground, &body(&u), &body(&v)),
        Ok(true)
    );
}

/// A struct's *parameters* compare at the declaration's outer telescope too, so a parameter at a proposition is discharged without being read.
///
/// It is the parameter-side twin of the field rule above: a family's indices, a struct's parameters and a constructor's are all compared at their declared types, so `Wrap(P, p)` and `Wrap(P, q)` are one type exactly as `Eq(@P)(p, q)` and `Eq(@P)(p, p)` are. Nothing in `/std` forces it; it holds because the rule is the same rule.
#[test]
fn a_struct_parameter_at_a_proposition_is_not_read() {
    let mut kernel = kernel();
    let proposition = declare(&mut kernel, "P", Term::prop());
    let name = curios_core::Global::Authored(Qualifier::from(["Held"]));
    kernel.declare_struct(
        &name,
        &StructDecl {
            universe_context: UniverseContext::default(),
            // One parameter at the proposition, and a field that does not mention it.
            arity: Telescope::build(
                [(binder(70, "p"), proposition.clone())],
                Telescope::build([(binder(71, "n"), nat_type())], ()),
            ),
            result_sort: Term::type_ground(),
            module: Qualifier::empty(),
            rep_public: true,
            polarities: vec![curios_core::Polarity::Unused],
            variances: Vec::new(),
            plicities: Vec::new(),
        },
    );
    let (p, q) = (binder(72, "p"), binder(73, "q"));
    kernel.assume(&p, &proposition);
    kernel.assume(&q, &proposition);

    let this = Term::struct_type(name, [Term::free_var(&p)]);
    let that = Term::struct_type(name, [Term::free_var(&q)]);

    assert_eq!(
        convert(&mut kernel, &Term::type_ground(), &this, &that),
        Ok(true),
        "two instances differing only in a proof parameter did not convert",
    );
}
