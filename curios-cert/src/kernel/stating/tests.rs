//! What the kernel states of a declaration's binders and payloads: what its own reading of their types answers, whatever the module says.

use {
    super::*,
    curios_analysis::test_support::*,
    curios_common::{Plicity, Qualifier},
    curios_core::{
        Atom, Definition, DefinitionKind, Func, Global, InductDecl, InductParam, Intrinsic, Item,
        Let, Nat, Subterm, Telescope, Term, UniverseContext,
    },
};

fn kernel() -> Kernel<'static> {
    Kernel::new(100_000, SYNTAX)
}

fn binder(index: u64, hint: &str) -> Free {
    Free::local(index, Some(hint))
}

fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

/// `held : Type = body`: an item whose terms the walk states the binders of. Its type is no part of what is under test.
fn defined(body: Term) -> Item {
    Item::Let(Box::new(Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: Term::type_ground(),
        body,
    }))
}

/// The body `item` holds.
fn body(item: &Item) -> &Term {
    match item {
        Item::Let(definition) => &definition.body,
        Item::Rec(_) => panic!("a definition of its own"),
    }
}

/// `function`, a function literal, saying of its binders what `proofs` says.
fn saying(function: Term, proofs: &[bool]) -> Term {
    let Subterm::Func(Func { telescope }) = Term::unwrap_or_clone(function) else {
        panic!("a function literal");
    };

    Term::from(Subterm::Func(Func {
        telescope: telescope.with_proofs(proofs),
    }))
}

/// `term`, a `let` block, saying of its binders what `proofs` says.
fn let_saying(term: Term, proofs: &[bool]) -> Term {
    let Subterm::Let(Let { bindings, tail }) = Term::unwrap_or_clone(term) else {
        panic!("a `let`");
    };

    Term::from(Subterm::Let(Let {
        bindings,
        tail: tail.with_proofs(proofs),
    }))
}

/// A function literal's binders state what the kernel reads of their types, each under the binders before it: `P` at a sort binds no proof, `h` at `P` and `lemma` at a function into `P` bind one, `n` at `Nat` and `F` at a function into a sort none — each the opposite of what the module says.
#[test]
fn a_function_literals_binders_state_what_the_kernel_reads_of_their_types() {
    let (p, h, lemma, n, family) = (
        binder(0, "P"),
        binder(1, "h"),
        binder(2, "lemma"),
        binder(3, "n"),
        binder(4, "F"),
    );
    let function = Term::func(
        [
            (p, Term::prop()),
            (h, Term::free_var(&p)),
            (
                lemma,
                Term::func_type([(binder(5, "k"), nat_type())], Term::free_var(&p)),
            ),
            (n, nat_type()),
            (
                family,
                Term::func_type([(binder(6, "k"), nat_type())], Term::type_ground()),
            ),
        ],
        Term::free_var(&n),
    );

    let stated = stated_marks(
        &mut kernel(),
        &defined(saying(function, &[true, false, false, true, true])),
    );

    let Subterm::Func(Func { telescope }) = &**body(&stated) else {
        panic!("a function literal");
    };
    assert_eq!(telescope.proofs(), [false, true, true, false, false]);
}

/// A `let`'s binders state what the kernel reads of their types, each under the binders around it and before it: in `(P: Prop, h: P) => let q : P = h; let m : Nat = 0; m`, one block of two, `q` binds a proof and `m` none, each the opposite of what the module says.
#[test]
fn a_lets_binders_state_what_the_kernel_reads_of_their_types() {
    let (p, h, q, m) = (
        binder(0, "P"),
        binder(1, "h"),
        binder(2, "q"),
        binder(3, "m"),
    );
    let block = let_saying(
        Term::let_block(
            vec![
                (q, Term::free_var(&p), Term::free_var(&h)),
                (
                    m,
                    nat_type(),
                    Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
                ),
            ],
            Term::free_var(&m),
        ),
        &[false, true],
    );
    let function = Term::func([(p, Term::prop()), (h, Term::free_var(&p))], block);

    let stated = stated_marks(&mut kernel(), &defined(function));

    let Subterm::Func(Func { telescope }) = &**body(&stated) else {
        panic!("a function literal");
    };
    let Subterm::Let(Let { tail, .. }) = &**telescope.terminal() else {
        panic!("a `let`");
    };
    assert_eq!([tail.binds_proof(0), tail.binds_proof(1)], [true, false]);
}

/// A constructor's binders state what the kernel reads of their types, each under the binders before it: `hold(P: Prop, h: P, n: Nat)` binds a proof at `h` alone, the opposite of what the module says of each.
#[test]
fn a_constructors_binders_state_what_the_kernel_reads_of_their_types() {
    let (p, h, n) = (binder(0, "P"), binder(1, "h"), binder(2, "n"));
    let hold = Atom::from("hold");
    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            hold.clone(),
            InductParam {
                telescope: Telescope::build(
                    [(p, Term::prop()), (h, Term::free_var(&p)), (n, nat_type())],
                    Vec::new(),
                )
                .with_marks(&[Plicity::Explicit; 3])
                .with_proofs(&[true, false, true]),
            },
        )]
        .into(),
        result_sort: Term::type_ground(),
        module: Qualifier::from(["Holder"]),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let stated = stated_entry(&mut kernel(), &declaration);

    assert_eq!(
        stated
            .constructor(&hold)
            .expect("the family declares `hold`")
            .telescope
            .proofs(),
        [false, true, false]
    );
}
