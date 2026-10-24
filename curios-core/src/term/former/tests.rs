//! Which lambdas are a type former's η-form, what each is read back as, and the node an application of one is made as.

use {
    super::{FormerEta, former_eta},
    crate::{Free, Func, Global, Intrinsic, StructType, Subterm, Telescope, Term},
    curios_utilities::Qualifier,
};

fn global(name: &str) -> Global {
    Global::Authored(Qualifier::from([name]))
}

fn local(index: u32, hint: &'static str) -> Free {
    Free::local(u64::from(index), Some(hint))
}

fn nat() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

/// `Pair`'s node, a struct over two parameters.
fn pair(left: Term, right: Term) -> Term {
    Term::from(Subterm::StructType(StructType {
        name: global("Pair"),
        universes: Default::default(),
        params: vec![left, right],
    }))
}

/// `Vec`'s node, a family over a parameter and an index.
fn vec(parameter: Term, index: Term) -> Term {
    Term::induct_type(global("Vec"), [parameter], [index])
}

/// `(A, B) => Pair(A, B)`: the bare former of a struct, one lambda to its one call.
fn pair_eta() -> Term {
    let (a, b) = (local(0, "A"), local(1, "B"));
    let body = pair(Term::free_var(&a), Term::free_var(&b));

    Term::func([(a, Term::type_ground()), (b, Term::type_ground())], body)
}

/// `(T) => (n) => Vec(T)(n)`: the bare former of a family, one lambda to each of its two calls.
fn vec_eta() -> Term {
    let (t, n) = (local(0, "T"), local(1, "n"));
    let body = vec(Term::free_var(&t), Term::free_var(&n));

    Term::func([(t, Term::type_ground())], Term::func([(n, nat())], body))
}

fn telescope(term: &Term) -> &Telescope<Term> {
    match &**term {
        Subterm::Func(Func { telescope }) => telescope,
        other => panic!("a lambda, not {other:?}"),
    }
}

fn read(term: &Term) -> Option<(Option<Global>, Vec<Term>)> {
    former_eta(telescope(term)).map(|former| match former {
        FormerEta::Nominal(name, prefix, _) => (Some(name), prefix),
        FormerEta::Intrinsic(_) => (None, Vec::new()),
    })
}

/// A former short of its application, bare or at its first call, is read as the former, and so is the lambda an imitation builds over the suffix of one call.
#[test]
fn a_former_short_of_its_application_is_read_as_the_former() {
    assert_eq!(read(&pair_eta()), Some((Some(global("Pair")), Vec::new())));
    assert_eq!(read(&vec_eta()), Some((Some(global("Vec")), Vec::new())));

    let b = local(1, "B");
    let right = Term::func([(b, Term::type_ground())], pair(nat(), Term::free_var(&b)));
    assert_eq!(read(&right), Some((Some(global("Pair")), vec![nat()])));

    let t = local(0, "T");
    let list = Term::func(
        [(t, Term::type_ground())],
        Term::intrinsic(Intrinsic::list_type(Term::free_var(&t))),
    );
    assert_eq!(read(&list), Some((None, Vec::new())));
}

/// A lambda that takes binders of a family's two calls at once, or a former's arguments out of their order, is no former's: no call writes it.
#[test]
fn a_lambda_across_two_calls_or_out_of_order_is_no_former() {
    let (t, n) = (local(0, "T"), local(1, "n"));
    let across = Term::func(
        [(t, Term::type_ground()), (n, nat())],
        vec(Term::free_var(&t), Term::free_var(&n)),
    );
    assert_eq!(read(&across), None);

    let (a, b) = (local(0, "A"), local(1, "B"));
    let swapped = Term::func(
        [(a, Term::type_ground()), (b, Term::type_ground())],
        pair(Term::free_var(&b), Term::free_var(&a)),
    );
    assert_eq!(read(&swapped), None);
}

/// An η-form applied over its whole parameter list is made as what the application reduces to: the node, or, for a family at its parameters, the η-form over its indices.
#[test]
fn an_eta_form_applied_over_its_parameters_is_made_as_the_node() {
    assert_eq!(Term::apply(pair_eta(), [nat(), nat()]), pair(nat(), nat()));

    let at_parameter = Term::apply(vec_eta(), [nat()]);
    assert!(read(&at_parameter).is_some());
    let index = Term::free_var(&local(5, "k"));
    assert_eq!(
        Term::apply(at_parameter, [index.clone()]),
        vec(nat(), index)
    );
}

/// A substitution that puts an η-form at a head makes the node where it rebuilds the application: `F(Nat, Nat)` at `F := Pair`.
#[test]
fn a_substitution_that_puts_an_eta_form_at_a_head_makes_the_node() {
    let f = local(3, "F");
    let kind = Term::func_type(
        [
            (local(0, "A"), Term::type_ground()),
            (local(1, "B"), Term::type_ground()),
        ],
        Term::type_ground(),
    );
    let lambda = Term::func([(f, kind)], Term::apply(Term::free_var(&f), [nat(), nat()]));

    assert_eq!(telescope(&lambda).open(&[&pair_eta()]), pair(nat(), nat()));
}

/// A lambda that is no η-form stays applied: the β-step is taken of a former's alone.
#[test]
fn a_lambda_that_is_no_eta_form_stays_applied() {
    let x = local(0, "x");
    let identity = Term::func([(x, nat())], Term::free_var(&x));

    assert!(matches!(
        &*Term::apply(identity, [nat()]),
        Subterm::Apply(_)
    ));
}
