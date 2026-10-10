//! The marking walk: which binders it asks, what it stands in for the binders around them, and that a term it changes nothing in comes back the node it was.

use {
    super::ProofBinders,
    crate::{Bound, Free, Func, FuncType, Intrinsic, Let, Quality, Subterm, Term},
    std::collections::BTreeMap,
};

/// A checker that answers yes of a sort and of a variable it entered at `Prop`, and keeps a record of what it was asked: a rule chosen to tell the walk's questions apart, and not the one a checker asks.
#[derive(Default)]
struct Sorts {
    minted: u32,
    entered: Vec<(Free, Option<Term>)>,
    asked: Vec<Term>,
}

impl ProofBinders for Sorts {
    type Refusal = ();

    fn proves(&mut self, type_: &Term) -> Result<bool, ()> {
        self.asked.push(type_.clone());
        let propositions = self
            .entered
            .iter()
            .filter(|(_, type_)| {
                type_
                    .as_ref()
                    .is_some_and(|type_| matches!(&**type_, Subterm::Sort(Quality::Prop, _)))
            })
            .map(|(binder, _)| (*binder, ()))
            .collect::<BTreeMap<_, _>>();

        Ok(match &**type_ {
            Subterm::Sort(..) => true,
            Subterm::Var(var) => var
                .as_free()
                .is_some_and(|free| propositions.contains_key(free)),
            _ => false,
        })
    }

    fn enter(&mut self, hint: Option<&str>, type_: Option<&Term>) -> Free {
        self.minted += 1;
        let binder = Free::local(10_000 + self.minted, hint);
        self.entered.push((binder, type_.cloned()));

        binder
    }

    fn leave(&mut self, count: usize) {
        let kept = self.entered.len() - count;
        self.entered.truncate(kept);
    }
}

fn binder(index: u32, hint: &str) -> Free {
    Free::local(index, Some(hint))
}

fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

fn proofs(function: &Term) -> Vec<bool> {
    match &**function {
        Subterm::Func(Func { telescope }) => telescope.proofs(),
        other => panic!("not a function literal: {other:?}"),
    }
}

/// A function literal's binders say what the checker answered of them, each asked at its type under the binders before it: `(P: Prop, n: Nat, h: P) => n` marks `P`, at a sort, leaves `n`, and marks `h`, whose type is the variable standing for `P`.
#[test]
fn a_function_literals_binders_are_asked_under_the_binders_before_them() {
    let (p, n, h) = (binder(0, "P"), binder(1, "n"), binder(2, "h"));
    let function = Term::func(
        [(p, Term::prop()), (n, nat_type()), (h, Term::free_var(&p))],
        Term::free_var(&n),
    );
    let mut sorts = Sorts::default();

    let stated = function.stating_proofs(&mut sorts).expect("answers");

    assert_eq!(proofs(&stated), [true, false, true]);
    assert_eq!(stated, function, "a mark is no part of the term");
    assert!(sorts.entered.is_empty(), "every binder entered was left");
    assert!(
        matches!(&*sorts.asked[2], Subterm::Var(var) if var.as_free().is_some()),
        "the third binder's type was asked as the variable standing for the first: {:?}",
        sorts.asked[2]
    );
}

/// A `let`'s binders are asked too, each at its type under the bindings before it, and a function literal inside its tail under all of them.
#[test]
fn a_lets_binders_are_asked() {
    let (p, proof, n) = (binder(0, "P"), binder(1, "proof"), binder(2, "n"));
    let inner = Term::func([(n, nat_type())], Term::free_var(&n));
    let block = Term::let_block(
        vec![
            (p, Term::type_ground(), Term::prop()),
            (proof, Term::prop(), Term::prop()),
        ],
        inner,
    );
    let mut sorts = Sorts::default();

    let stated = block.stating_proofs(&mut sorts).expect("answers");

    let Subterm::Let(Let { tail, .. }) = &*stated else {
        panic!("not a `let`: {stated:?}");
    };
    assert_eq!([tail.binds_proof(0), tail.binds_proof(1)], [true, true]);
    assert_eq!(proofs(tail.body()), [false]);
}

/// A function type's binders are entered and not asked: nothing substitutes at one, but a literal further in may name them, and is asked under the variable that stands for each.
#[test]
fn a_function_types_binders_are_entered_and_not_asked() {
    let (p, h) = (binder(0, "P"), binder(1, "h"));
    let inner = Term::func([(h, Term::free_var(&p))], Term::free_var(&h));
    let type_ = Term::func_type([(p, Term::prop())], inner);
    let mut sorts = Sorts::default();

    let stated = type_.stating_proofs(&mut sorts).expect("answers");

    let Subterm::FuncType(FuncType { telescope }) = &*stated else {
        panic!("not a function type: {stated:?}");
    };
    assert_eq!(telescope.proofs(), [false]);
    assert_eq!(proofs(telescope.terminal()), [true]);
    assert_eq!(sorts.asked.len(), 1, "only the literal's binder was asked");
}

/// A term the walk changes nothing in comes back the node it was, so its sharing and what its nodes had cached are kept.
#[test]
fn a_term_with_nothing_to_state_is_handed_back_as_it_was() {
    let n = binder(0, "n");
    let function = Term::func([(n, nat_type())], Term::free_var(&n));
    let applied = Term::apply(
        function,
        [Term::intrinsic(Intrinsic::Nat(crate::Nat::new(3usize)))],
    );

    let stated = applied
        .stating_proofs(&mut Sorts::default())
        .expect("answers");

    assert_eq!(stated.identity(), applied.identity());
}

/// A scope the walk does not read the types of is crossed under a variable of no type, and a literal under it is still asked: the arm below binds a payload, and the function inside it marks its parameter at a sort.
#[test]
fn a_literal_under_a_scope_of_unknown_types_is_still_asked() {
    let (scrutinee, payload, a) = (binder(0, "s"), binder(1, "x"), binder(2, "A"));
    let inner = Term::func([(a, Term::type_ground())], Term::free_var(&payload));
    let matched = Term::bool_match(
        Term::free_var(&scrutinee),
        None,
        nat_type(),
        inner.clone(),
        inner,
    );
    let outer = Term::func([(payload, nat_type())], matched);

    let stated = outer
        .capture(&[&scrutinee])
        .stating_proofs_under(&mut Sorts::default(), &[Term::free_var(&scrutinee)])
        .expect("answers");

    let Subterm::Func(Func { telescope }) = &*stated else {
        panic!("not a function literal: {stated:?}");
    };
    let Subterm::Match(matched) = &**telescope.terminal() else {
        panic!("not a match: {:?}", telescope.terminal());
    };
    let mut literals = Vec::new();
    Subterm::Match(matched.clone()).any_child_term(&mut |child| {
        if matches!(&**child, Subterm::Func(_)) {
            literals.push(proofs(child));
        }
        false
    });
    assert_eq!(literals, [vec![true], vec![true]]);
}
