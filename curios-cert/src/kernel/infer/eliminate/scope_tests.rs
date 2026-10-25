//! What an arm puts in force and where it ends: what its case teaches, the locals its solution re-types, and the spellings settled under them, asked of a term inside a lambda, under a nested arm, beside a `let`, in the arm's sibling and after the arm.

use {
    super::test_support::*,
    crate::{Error, Kernel, infer},
    curios_core::{Free, Global, Intrinsic, Level, Many, Scope, Term, UniverseContext},
    curios_utilities::Plicity,
};

fn bool_type() -> Term {
    Term::intrinsic(Intrinsic::BoolType)
}

/// `match v | true => Nat | false => Bool`: a type `3` inhabits exactly where `v` reduces to `true`.
fn family(v: Term) -> Term {
    Term::bool_match(v, None, Term::type_ground(), bool_type(), nat_type())
}

/// `match c : (_) => Type | false => Nat | true => {}`, the unit type at `unit` and `Nat` at the other case: where a split re-types `p, q` at it, any two of them are one by the type's shape.
fn carried(c: &Free, unit: bool) -> Term {
    let (at_false, at_true) = match unit {
        true => (nat_type(), Term::tuple_type_unit()),
        false => (Term::tuple_type_unit(), nat_type()),
    };
    Term::bool_match(
        Term::free_var(c),
        None,
        Term::type_ground(),
        at_false,
        at_true,
    )
}

/// `family(k(0))`, which the guard below answers only where its spelling was settled as `k(0)`.
fn claimed(k: &Free) -> Term {
    family(Term::apply(Term::free_var(k), [nat(0)]))
}

/// `let u : family(k(0)) = 3; 0`, typed where the split has made the guard `k(0)`.
fn settles(k: &Free) -> Term {
    Term::let_(&binder(60, "u"), claimed(k), nat(3), nat(0))
}

/// `let z : family(k(0)) = 3; 0`, typed where the split no longer stands.
fn asks(k: &Free) -> Term {
    Term::let_(&binder(62, "z"), claimed(k), nat(3), nat(0))
}

/// `let w : Nat = split; asks(k)`.
fn after(k: &Free, split: Term) -> Term {
    Term::let_(&binder(61, "w"), nat_type(), split, asks(k))
}

/// The locals the guard is stated over: the split variable `c`, a second variable `d`, `h : (carried) -> Nat`, `k : (Nat) -> Bool` and `p, q : carried`.
struct Guarded {
    c: Free,
    d: Free,
    h: Free,
    k: Free,
    p: Free,
    q: Free,
}

impl Guarded {
    fn new() -> Self {
        Self {
            c: binder(40, "c"),
            d: binder(41, "d"),
            h: binder(42, "h"),
            k: binder(43, "k"),
            p: binder(44, "p"),
            q: binder(45, "q"),
        }
    }

    /// `match left == right : (_) => Bool | false => k(1) | true => k(0) end`: the guard, `k(0)` exactly where its comparison folds to `true`.
    fn read(&self, left: Term, right: Term) -> Term {
        let tested = |at: usize| Term::apply(Term::free_var(&self.k), [nat(at)]);
        Term::bool_match(
            Term::intrinsic(Intrinsic::NatEql(left, right)),
            None,
            bool_type(),
            tested(1),
            tested(0),
        )
    }

    /// `(c, d, h, k, p, q) => match read : (_) => Nat | false => 0 | true => arm end`.
    fn under(&self, unit: bool, read: Term, arm: Term) -> Term {
        Term::func(
            [
                (self.c, bool_type()),
                (self.d, bool_type()),
                (
                    self.h,
                    Term::func_type([(binder(46, "x"), carried(&self.c, unit))], nat_type()),
                ),
                (
                    self.k,
                    Term::func_type([(binder(47, "n"), nat_type())], bool_type()),
                ),
                (self.p, carried(&self.c, unit)),
                (self.q, carried(&self.c, unit)),
            ],
            Term::bool_match(read, None, nat_type(), nat(0), arm),
        )
    }

    /// The board's unit guard, `h(p) == h(q)`, over `arm`.
    fn guarding(&self, unit: bool, arm: Term) -> Term {
        let measured = |x: &Free| Term::apply(Term::free_var(&self.h), [Term::free_var(x)]);
        self.under(unit, self.read(measured(&self.p), measured(&self.q)), arm)
    }

    /// `match c : (_) => Nat | false => 0 | true => arm end`.
    fn split(&self, arm: Term) -> Term {
        Term::bool_match(Term::free_var(&self.c), None, nat_type(), nat(0), arm)
    }
}

/// A refusal of the forged term by the mismatch the guard's question leaves unanswered after the split.
fn refused_after_the_split(verdict: Result<Term, Error>) -> bool {
    matches!(verdict, Err(Error::Mismatch { .. }))
}

/// A guard settled where a split re-types its operands at the unit type answers a term inside a lambda there, and nothing after the split: `((u : family(k(0))) => 0)(3)` in the split's arm, and `family(k(0))` asked once `c` is open again. The control is the lambda alone, which the split's spelling answers.
///
/// Mutation-checked: with `Scope::restate` restating only the equations whose spellings name a solved variable, reading no local's type or value, the kernel admits the term asked after the split.
#[test]
fn a_guard_settled_in_a_lambda_inside_a_split_answers_nothing_after_the_split() {
    let guarded = Guarded::new();
    let lambda = || {
        guarded.split(Term::apply(
            Term::func([(binder(63, "u"), claimed(&guarded.k))], nat(0)),
            [nat(3)],
        ))
    };

    assert!(infer(&mut kernel(), &guarded.guarding(true, lambda())).is_ok());
    assert!(refused_after_the_split(infer(
        &mut kernel(),
        &guarded.guarding(true, after(&guarded.k, lambda()))
    )));
}

/// The same guard settled under an arm nested in the split, on a second variable the guard does not read: the nested arm restates nothing, and what the split re-typed is read there as in the split itself.
///
/// Mutation-checked as [`a_guard_settled_in_a_lambda_inside_a_split_answers_nothing_after_the_split`] is.
#[test]
fn a_guard_settled_under_an_arm_nested_in_a_split_answers_nothing_after_the_split() {
    let guarded = Guarded::new();
    let nested = || {
        guarded.split(Term::bool_match(
            Term::free_var(&guarded.d),
            None,
            nat_type(),
            nat(0),
            settles(&guarded.k),
        ))
    };

    assert!(infer(&mut kernel(), &guarded.guarding(true, nested())).is_ok());
    assert!(refused_after_the_split(infer(
        &mut kernel(),
        &guarded.guarding(true, after(&guarded.k, nested()))
    )));
}

/// The same guard settled beside a `let` bound under the split, `let r : carried = p`, which the split's solution binds at the unit type.
///
/// Mutation-checked as [`a_guard_settled_in_a_lambda_inside_a_split_answers_nothing_after_the_split`] is.
#[test]
fn a_guard_settled_beside_a_let_bound_under_a_split_answers_nothing_after_the_split() {
    let guarded = Guarded::new();
    let bound = || {
        guarded.split(Term::let_(
            &binder(64, "r"),
            carried(&guarded.c, true),
            Term::free_var(&guarded.p),
            settles(&guarded.k),
        ))
    };

    assert!(infer(&mut kernel(), &guarded.guarding(true, bound())).is_ok());
    assert!(refused_after_the_split(infer(
        &mut kernel(),
        &guarded.guarding(true, after(&guarded.k, bound()))
    )));
}

/// A guard settled in one arm of a split answers nothing in its sibling: the operands are units at `c = false`, the arm the kernel types first, and numbers at `c = true`, where `family(k(0))` is asked.
///
/// Mutation-checked as [`a_guard_settled_in_a_lambda_inside_a_split_answers_nothing_after_the_split`] is.
#[test]
fn a_guard_settled_in_one_arm_of_a_split_answers_nothing_in_the_other() {
    let guarded = Guarded::new();
    let both = |sibling: Term| {
        Term::bool_match(
            Term::free_var(&guarded.c),
            None,
            nat_type(),
            settles(&guarded.k),
            sibling,
        )
    };

    assert!(infer(&mut kernel(), &guarded.guarding(false, both(nat(0)))).is_ok());
    assert!(refused_after_the_split(infer(
        &mut kernel(),
        &guarded.guarding(false, both(asks(&guarded.k)))
    )));
}

/// A guard whose operands meet the re-typed binders only as the arms of a stuck elimination, `h(match d : (_) => carried | true => p | false => p end)` against the same over `q`, where conversion reads the binders' type off the binders themselves: what the split decided of them is the split's alone. The elimination's motive spells the binders' type, so the guard names the split's variable and is restated by the variable it names, before any type is read.
///
/// Mutation-checked: with `Scope::restate` restating no equation, the kernel admits the term asked after the split; restating only the equations whose spellings name a solved variable still refuses it.
#[test]
fn a_guard_over_eliminations_of_re_typed_binders_answers_nothing_after_the_split() {
    let guarded = Guarded::new();
    let measured = |x: &Free| {
        Term::apply(
            Term::free_var(&guarded.h),
            [Term::bool_match(
                Term::free_var(&guarded.d),
                None,
                carried(&guarded.c, true),
                Term::free_var(x),
                Term::free_var(x),
            )],
        )
    };
    let forged = |arm: Term| {
        guarded.under(
            true,
            guarded.read(measured(&guarded.p), measured(&guarded.q)),
            arm,
        )
    };

    assert!(infer(&mut kernel(), &forged(guarded.split(settles(&guarded.k)))).is_ok());
    assert!(refused_after_the_split(infer(
        &mut kernel(),
        &forged(after(&guarded.k, guarded.split(settles(&guarded.k))))
    )));
}

/// `Same : (Type, Type) -> Prop | same(z : Type) : (z, z)`.
fn same_type(kernel: &mut Kernel) -> Global {
    let z = binder(0, "z");
    declare_at(
        kernel,
        "SameType",
        Term::prop(),
        vec![Term::type_ground(), Term::type_ground()],
        vec![carrying_at(
            "same",
            z,
            Term::type_ground(),
            vec![Term::free_var(&z), Term::free_var(&z)],
        )],
    )
}

/// `(A, B : Type, p : Same(former(A), former(B))) => match p : (_, _, _) => Same(A, B) | same(z) => same(A) end`.
fn learned_through(same: Global, former: &dyn Fn(Term) -> Term) -> Term {
    let (a, b, p, z) = (
        binder(20, "A"),
        binder(21, "B"),
        binder(22, "p"),
        binder(23, "z"),
    );
    let at = |left: Term, right: Term| Term::induct_type(same, Vec::<Term>::new(), [left, right]);
    let matched = Term::induct_match_scoped_marked(
        Term::free_var(&p),
        Scope::constant(Many(3), at(Term::free_var(&a), Term::free_var(&b))),
        [(
            "same",
            vec![(Plicity::Explicit, z)],
            Term::variant(same, Vec::<Term>::new(), "same", [Term::free_var(&a)]),
        )],
        None,
    );

    Term::func(
        [
            (a, Term::type_ground()),
            (b, Term::type_ground()),
            (
                p,
                at(former(Term::free_var(&a)), former(Term::free_var(&b))),
            ),
        ],
        matched,
    )
}

/// An equation between two types one former builds teaches an arm nothing of what the former was applied to: inversion decomposes literals, a relevant family's values and tuples, and refuses a type former, so an injective type constructor, the half of Hur's route to `False` that excluded middle or impredicativity completes, is nowhere a case's lesson. Asked of an intrinsic former, `List`, of a declared family, of an opaque global and of a function type; the control is the equation between the two variables themselves, which the arm solves.
#[test]
fn an_equation_between_two_types_one_former_builds_teaches_an_arm_nothing_of_their_arguments() {
    let mut kernel = kernel();
    let same = same_type(&mut kernel);
    let declared = declare_at(
        &mut kernel,
        "Former",
        Term::type_ground(),
        vec![Term::type_ground()],
        vec![],
    );
    let opaque = binder(30, "F");
    kernel
        .declare(
            &opaque,
            &Term::func_type(
                [(binder(31, "x"), Term::type_ground())],
                Term::type_ground(),
            ),
            &UniverseContext::default(),
        )
        .expect("a name is declared once");

    let formers: [(&str, &dyn Fn(Term) -> Term); 4] = [
        ("List", &|t| Term::intrinsic(Intrinsic::ListType(t))),
        ("a declared family", &|t| {
            Term::induct_type(declared, Vec::<Term>::new(), [t])
        }),
        ("an opaque global", &|t| {
            Term::apply(Term::free_var(&opaque), [t])
        }),
        ("a function type", &|t| {
            Term::func_type([(binder(32, "x"), t)], nat_type())
        }),
    ];
    for (label, former) in formers {
        assert!(
            matches!(
                infer(&mut kernel, &learned_through(same, former)),
                Err(Error::Mismatch { .. })
            ),
            "{label}: the arm learned what the former was applied to"
        );
    }
    assert!(infer(&mut kernel, &learned_through(same, &|t| t)).is_ok());
}

/// A type that is a proposition only under an arm is a type there and nowhere else: over `S(c) = match c : (_) => Type 1 | false => Type | true => Prop` and `F(c) = match c : (d) => S(d) | false => Nat | true => Unit`, a binder at `F(c)` with `c` open is refused, its type's sort a stuck elimination, so nothing irrelevance grants a value of it inside the arm reaches a binder, a motive, a field or a result outside it. The control binds one at `c = true`, inside the split.
#[test]
fn a_binder_at_a_type_whose_sort_an_arm_decides_is_refused_outside_the_arm() {
    let mut kernel = kernel();
    let unit = declare_at(
        &mut kernel,
        "Unit",
        Term::prop(),
        vec![],
        vec![nullary_at("unit", vec![])],
    );
    let (c, d, x) = (binder(80, "c"), binder(81, "d"), binder(82, "x"));
    let sort_at = |v: Term| {
        Term::bool_match(
            v,
            None,
            Term::type_at(Level::zero().succ().expect("level zero has a successor")),
            Term::type_ground(),
            Term::prop(),
        )
    };
    let proposition_at = |v: Term| {
        Term::bool_match(
            v,
            Some(&d),
            sort_at(Term::free_var(&d)),
            nat_type(),
            Term::induct_type(unit, Vec::<Term>::new(), Vec::<Term>::new()),
        )
    };

    let outside = Term::func(
        [(c, bool_type()), (x, proposition_at(Term::free_var(&c)))],
        nat(0),
    );
    assert!(matches!(
        infer(&mut kernel, &outside),
        Err(Error::NotASort(_))
    ));

    let inside = Term::func(
        [(c, bool_type())],
        Term::bool_match(
            Term::free_var(&c),
            None,
            nat_type(),
            nat(1),
            Term::apply(
                Term::func([(x, proposition_at(Term::free_var(&c)))], nat(0)),
                [Term::variant(
                    unit,
                    Vec::<Term>::new(),
                    "unit",
                    Vec::<Term>::new(),
                )],
            ),
        ),
    );
    assert!(infer(&mut kernel, &inside).is_ok());
}
