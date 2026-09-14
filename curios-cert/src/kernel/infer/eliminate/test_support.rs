//! The families, motives and opaque type formers the elimination suites eliminate over.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across `eliminate`, and nothing outside it.

use {
    crate::Kernel,
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Intrinsic, Many, Nat, Scope, Telescope, Term,
        UniverseContext,
    },
    curios_utilities::{Plicity, Qualifier},
};

pub(super) fn kernel() -> Kernel {
    let mut kernel = Kernel::new(100_000, crate::SYNTAX);
    kernel.set_local_floor(1_000);
    kernel
}

pub(super) fn binder(index: u32, hint: &str) -> Free {
    Free::local(index, Some(hint))
}

pub(super) fn nat(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

pub(super) fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

/// One constructor of a test family: its tag, the payload binder it carries if any, and the index target this case aims at.
pub(super) struct Case {
    pub(super) tag: &'static str,
    pub(super) payload: Option<(Free, Term)>,
    pub(super) index: Term,
}

/// A constructor carrying nothing, aimed at `index`.
pub(super) fn nullary(tag: &'static str, index: Term) -> Case {
    Case {
        tag,
        payload: None,
        index,
    }
}

/// A constructor carrying `binder : type_`, aimed at `index`.
pub(super) fn carrying(tag: &'static str, binder: Free, type_: Term, index: Term) -> Case {
    Case {
        tag,
        payload: Some((binder, type_)),
        index,
    }
}

/// Declare a one-index family from its constructors.
pub(super) fn declare(
    kernel: &mut Kernel,
    path: &str,
    result_sort: Term,
    constructors: Vec<Case>,
) -> Global {
    let family = Global::Authored(Qualifier::from([path]));

    let entries = constructors
        .into_iter()
        .map(|case| {
            let targets = vec![case.index];
            let (telescope, plicities) = match case.payload {
                Some((field, type_)) => (
                    Telescope::build([(field, type_)], targets),
                    vec![Plicity::Explicit],
                ),
                None => (Telescope::done(targets), Vec::new()),
            };

            (Atom::from(case.tag), InductParam::new(telescope, plicities))
        })
        .collect();

    kernel.declare_induct(
        &family,
        &InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::build(
                [(
                    Free::local(9_000, Some("i")),
                    Term::intrinsic(Intrinsic::NatType),
                )],
                (),
            )),
            constructors: entries,
            result_sort,
            module: Qualifier::from([path]),
            rep_public: true,
            polarities: Vec::new(),
        },
    );

    family
}

/// `match subject : (i, s) => motive | tag(binders) => body ... end`, over a scrutinee assumed at `family(index)`.
pub(super) fn eliminate(
    kernel: &mut Kernel,
    family: &Global,
    index: Term,
    motive: Term,
    arms: Vec<(&str, Vec<Free>, Term)>,
) -> Term {
    let subject = binder(50, "subject");
    kernel.assume(
        &subject,
        &Term::induct_type(family.clone(), Vec::<Term>::new(), [index]),
    );

    let motive = Scope::close(Many(2), &[&binder(51, "i"), &binder(52, "s")], motive);

    Term::induct_match_scoped_marked(
        Term::free_var(&subject),
        motive,
        arms.into_iter().map(|(tag, binders, body)| {
            (
                tag,
                binders
                    .into_iter()
                    .map(|b| (Plicity::Explicit, b))
                    .collect::<Vec<_>>(),
                body,
            )
        }),
        None,
    )
}

/// A successor over `tail`, in the successor-floor form reduction keeps.
pub(super) fn succ(tail: Term) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::Succ(1u32.into(), tail)))
}

/// An opaque `P : (n : Nat, x : family(n)) -> Type`, for observing which *scrutinee* a term checks at as well as which index.
///
/// [`opaque_family`] below can only witness the index half of a motive, and a catch-all refines no index — so nothing built from it can tell the scrutinee instances apart.
pub(super) fn scrutinee_family(kernel: &mut Kernel, name: Free, family: &Global) -> Term {
    let index = binder(92, "n");
    kernel.declare(
        &name,
        &Term::func_type(
            [
                (index.clone(), nat_type()),
                (
                    binder(93, "x"),
                    Term::induct_type(family.clone(), Vec::<Term>::new(), [Term::free_var(&index)]),
                ),
            ],
            Term::type_ground(),
        ),
        &UniverseContext::default(),
    );

    Term::free_var(&name)
}

/// An opaque `P : (Nat) -> Type`, for observing which instance a term checks at.
pub(super) fn opaque_family(kernel: &mut Kernel, name: Free) -> Term {
    kernel.declare(
        &name,
        &Term::func_type([(binder(90, "n"), nat_type())], Term::type_ground()),
        &UniverseContext::default(),
    );

    Term::free_var(&name)
}
