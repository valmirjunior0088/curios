//! Remembered verdicts: a pair of nodes compared once, and a verdict that does not outlive the equations, case or binder it was reached under.

use {
    super::test_support::*,
    crate::{Kernel, convert},
    curios_analysis::test_support::SYNTAX,
    curios_core::{Free, Intrinsic, Telescope, Term},
};

/// `f : (Nat, Nat) -> Nat`, `n : Nat` and `m : Nat` assumed, for the comparisons below.
fn over_a_function<'s>(mut kernel: Kernel<'s>, f: &Free, n: &Free, m: &Free) -> Kernel<'s> {
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

/// A verdict read off a type's *shape* lives no longer than the case that gave the type that shape. `x` and `y` at `match c | true => W | false => Nat end`, a struct of a unit under `c = true` and `Nat` under `c = false`, are apart before an arm that assumes `c = true`, one inside it where the type has one inhabitant, and apart after it. It is the shape-reading counterpart of `a_remembered_verdict_does_not_outlive_the_equations_it_was_reached_under`, which reaches `true` by the equation making its two terms one rather than by the type's shape, so neither covers the other's path. The uncached kernel agrees on all three.
///
/// Mutation-checked: with the scoped verdicts left standing where an equation moves, the comparison inside the arm answers what it answered before it.
#[test]
fn a_one_inhabitant_verdict_does_not_outlive_the_case_that_shaped_its_type() {
    let sequence = |mut kernel: Kernel| {
        let w = declare_struct(
            &mut kernel,
            "W",
            Telescope::build([(binder(1, "u"), Term::tuple_type_unit())], ()),
        );
        let c = binder(0, "c");
        kernel.assume(&c, &Term::intrinsic(Intrinsic::BoolType));
        let shaped = Term::bool_match(Term::free_var(&c), None, Term::type_ground(), nat_type(), w);
        let x = binder(2, "x");
        let y = binder(3, "y");
        kernel.assume(&x, &shaped);
        kernel.assume(&y, &shaped);
        let compared = |kernel: &mut Kernel| {
            convert(kernel, &shaped, &Term::free_var(&x), &Term::free_var(&y))
        };

        let before = compared(&mut kernel);
        let inside = kernel.scoped(|kernel| {
            kernel
                .refine(Term::free_var(&c), Term::intrinsic(Intrinsic::Bool(true)))
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
