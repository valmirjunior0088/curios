//! The calls the kernel records while it types a group's bodies, held to the gate a type-yielding member must pass: each fixture's only recursive call is one the recorder must see, graded under one kind of context it must build.
//!
//! A member whose declared type yields a sort is erased, so it must descend — the local gate `check_group` applies — and a group's calls are the only thing that gate reads. A call the recorder missed would be an edge the closure never saw, and a group with no edges is accepted, so every fixture below that refuses is the evidence that the call is recorded at all, and every one that accepts is the evidence that the context it was graded under was built.

use {
    crate::{Error, Kernel, infer},
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Carrier, Cases, Free, Global, Intrinsic, Nat, Scope, StructDecl, Subterm, Telescope, Term,
        Two, UniverseContext,
    },
    curios_utilities::Qualifier,
};

fn kernel() -> Kernel {
    Kernel::new(100_000, SYNTAX)
}

fn binder(index: u32, hint: &str) -> Free {
    Free::local(index, Some(hint))
}

fn nat(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

/// `f : (Nat) -> Type`, the member every fixture defines.
fn member() -> (Free, Term) {
    let f = binder(0, "f");
    let n = binder(1, "n");

    (f, Term::func_type([(n, nat_type())], Term::type_ground()))
}

/// The group `rec f(n : Nat) -> Type = body`, as the term that states it and names `f` in its tail.
fn group(f: &Free, signature: Term, n: &Free, body: Term) -> Term {
    Term::rec(
        [(*f, signature, Term::func([(*n, nat_type())], body))],
        Term::free_var(f),
    )
}

/// `f(argument)`.
fn call(f: &Free, argument: Term) -> Term {
    Term::apply(Term::free_var(f), [argument])
}

/// The two-arm split of `n` at `Type`: `Nat` when it is zero, `cons(pred)` when it is `pred + 1`.
fn split(n: &Free, cons: impl FnOnce(&Free) -> Term) -> Term {
    let pred = binder(2, "pred");
    let ih = binder(3, "ih");
    let body = cons(&pred);

    Term::match_ambient(
        Term::free_var(n),
        Term::type_ground(),
        Cases::FreeMonoid {
            carrier: Carrier::Nat {
                empty_case: nat_type(),
                cons_case: Scope::close(Two, &[&pred, &ih], body),
            },
        },
    )
}

/// The control every accepting fixture below is read against: a call at the caller's own argument is a cycle with nothing decreasing, and the gate refuses it. Mutation-checked: recording no applied call empties the group's calls, and this is then accepted.
#[test]
fn a_type_calling_itself_at_its_own_argument_is_refused() {
    let (f, signature) = member();
    let n = binder(1, "n");
    let term = group(&f, signature, &n, call(&f, Term::free_var(&n)));

    assert!(matches!(
        infer(&mut kernel(), &term),
        Err(Error::NotDescending { .. }),
    ));
}

/// A `Nat` arm refines its scrutinee to one successor over the predecessor it binds, so a call at the predecessor is below the caller's argument. Mutation-checked: refining the scrutinee from the arm's typing spelling, `pred + 1`, reads nothing below it, and this is refused.
#[test]
fn a_type_descending_through_a_nat_arm_is_accepted() {
    let (f, signature) = member();
    let n = binder(1, "n");
    let term = group(
        &f,
        signature,
        &n,
        split(&n, |pred| call(&f, Term::free_var(pred))),
    );

    assert!(infer(&mut kernel(), &term).is_ok());
}

/// A lambda applied on the spot is read through its binder: `((m) => f(m))(pred)` calls `f` at what `m` stands for, which is below the caller's argument. Mutation-checked: typing the lambda without its binders standing for the arguments leaves `m` a fresh binder nothing is below, and this is refused.
#[test]
fn a_descent_through_an_applied_lambda_is_read_through_its_binder() {
    let (f, signature) = member();
    let n = binder(1, "n");
    let m = binder(4, "m");
    let term = group(
        &f,
        signature,
        &n,
        split(&n, |pred| {
            Term::apply(
                Term::func([(m, nat_type())], call(&f, Term::free_var(&m))),
                [Term::free_var(pred)],
            )
        }),
    );

    assert!(infer(&mut kernel(), &term).is_ok());
}

/// A comparison behind a definition is read as a guard before the arm's case equation is assumed: the arm where `is_zero(n)` is false rules zero out for `n`, so `n - 1` is below it. Mutation-checked: read after the equation, the scrutinee reduces to the arm's own `false` and compares nothing, and this is refused.
#[test]
fn a_guard_behind_a_definition_establishes_what_its_arm_descends_on() {
    let mut kernel = kernel();
    let is_zero = binder(10, "is_zero");
    let x = binder(11, "x");
    kernel.define(
        &is_zero,
        &Term::func_type([(x, nat_type())], Term::intrinsic(Intrinsic::BoolType)),
        &Term::func(
            [(x, nat_type())],
            Term::intrinsic(Intrinsic::NatEql(Term::free_var(&x), nat(0))),
        ),
        &UniverseContext::empty(),
    );

    let (f, signature) = member();
    let n = binder(1, "n");
    let body = Term::match_ambient(
        Term::apply(Term::free_var(&is_zero), [Term::free_var(&n)]),
        Term::type_ground(),
        Cases::Bool {
            false_case: call(
                &f,
                Term::intrinsic(Intrinsic::NatSub(Term::free_var(&n), nat(1))),
            ),
            true_case: nat_type(),
        },
    );
    let term = group(&f, signature, &n, body);

    assert!(infer(&mut kernel, &term).is_ok());
}

/// A call standing only in a nominal value's parameter is recorded, because the parameter is typed: `f(n)` sits in `Box(f(n)) { 3 }`, whose field is projected where it is inferred, so no conversion ever reads the value's type. Mutation-checked: counting a value's parameters without typing them leaves the group with no call, and it is accepted.
#[test]
fn a_call_in_a_nominal_values_parameter_is_recorded() {
    let mut kernel = kernel();
    let boxed = Global::Authored(Qualifier::from(["Box"]));
    kernel.declare_struct(
        &boxed,
        &StructDecl {
            universe_context: UniverseContext::empty(),
            arity: Telescope::build(
                [(binder(20, "A"), Term::type_ground())],
                Telescope::build([(binder(21, "x"), nat_type())], ()),
            ),
            result_sort: Term::type_ground(),
            module: Qualifier::default(),
            rep_public: true,
            polarities: Vec::new(),
            variances: Vec::new(),
            plicities: Vec::new(),
        },
    );

    let (f, signature) = member();
    let n = binder(1, "n");
    let body = Term::let_(
        &binder(22, "k"),
        nat_type(),
        Term::proj(
            Term::struct_(boxed, [call(&f, Term::free_var(&n))], [nat(3)]),
            0,
        ),
        nat_type(),
    );
    let term = group(&f, signature, &n, body);

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(Error::NotDescending { .. }),
    ));
}

/// A term naming a member is typed wherever it stands, whatever the kernel remembers of it. `rec a : Type = List(b), b : Type = List(b)` states one body twice, and the call it holds is `a`'s to `b` in the first and `b`'s to itself in the second — the cycle the gate refuses. Mutation-checked: with a remembered judgment answering for a term that names a member, the second body's call is never recorded, and the group is accepted.
#[test]
fn a_call_is_recorded_in_every_body_that_states_it() {
    let a = binder(0, "a");
    let b = binder(1, "b");
    let body = || Term::intrinsic(Intrinsic::ListType(Term::free_var(&b)));
    let term = Term::rec(
        [
            (a, Term::type_ground(), body()),
            (b, Term::type_ground(), body()),
        ],
        Term::free_var(&a),
    );

    assert!(matches!(
        infer(&mut kernel(), &term),
        Err(Error::NotDescending { .. }),
    ));
}

/// What inferring `term` costs `kernel`, read off the remaining budget on either side.
fn spent_inferring(kernel: &mut Kernel, term: &Term) -> u64 {
    let (before, _) = kernel.consumption();
    infer(kernel, term).expect("is typed");
    let (after, _) = kernel.consumption();

    before - after
}

/// A typing remembered on one side of what an arm establishes for the recorder does not answer on the other. A group typed inside a term closes under what the arms around it established, and the default arm of a `switch` rules zero out for its scrutinee while assuming no equation and opening no binder, so nothing else stands between a term typed outside it and the same term inside. Mutation-checked: without the clear where the arm's knowledge is entered the term is answered inside the bracket for nothing, and without the one where it is retracted, after it.
#[test]
fn a_remembered_type_does_not_outlive_what_an_arm_established() {
    let (f, signature) = member();
    let n = binder(1, "n");
    let stated = group(&f, signature, &n, nat_type());
    let Subterm::Rec(rec) = &*stated else {
        unreachable!("a group")
    };

    let mut kernel = kernel();
    let assumed = binder(5, "rec");
    let k = binder(6, "k");
    kernel.assume(&assumed, &rec.group.member_type(0));
    kernel.assume(&k, &nat_type());
    kernel.open_group(&rec.group, &[assumed]);
    kernel.begin_member(0);

    let term = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&k), nat(1)));
    let first = spent_inferring(&mut kernel, &term);
    let again = spent_inferring(&mut kernel, &term);
    let inside = kernel.scoped(|kernel| {
        kernel
            .assume_nonzero(k)
            .expect("the arm's knowledge enters");
        spent_inferring(kernel, &term)
    });
    let after = spent_inferring(&mut kernel, &term);

    assert!(first > 0, "typing it spends");
    assert_eq!(again, 0, "and a second typing is remembered");
    assert!(inside > 0, "the arm's knowledge is entered");
    assert!(after > 0, "and retracted");
}
