//! What a stuck `match` reduces to, read once for the walks that read a type through what it computes.
//!
//! Every instance of a `match` reduces it to one of its arms, whose binders the head supplies — or, where definitional K selects a proposition's sole arm, the instance the `match` states — so the arms opened against fresh binders, beside what supplies their binders, bound every reduct. The goal and a list's element type stand in no reduct: an instance that reduces the `match` drops both. Strict positivity and variance read the same parts and record different things of them, so the parts are told apart here and what each walk records stays in that walk.

use {
    crate::Env,
    curios_core::{Arity, Carrier, Cases, Match, Scope, Term},
};

/// A stuck `match` opened for reading: its arms, what supplies their binders, and what annotates it.
pub struct Opened {
    /// Every arm, a default among them, each opened against fresh binders.
    pub arms: Vec<Term>,
    /// Whether some arm reads one of its binders: where none does, what supplies them reaches no reduct and only selects an arm.
    pub reads_a_binder: bool,
    /// The terms an arm's binders are supplied by: the head, and the instance the `match` states.
    pub suppliers: Vec<Term>,
    /// What the `match` states of itself and no reduct holds: its goal, and a list's element type.
    pub annotations: Vec<Term>,
}

/// `matched`'s parts, each arm opened against binders minted by `env`.
///
/// Read only where the `match` stands under no binder the walk has not opened, so what an arm holds can be forced: one with a dangling index is read as written, as every form that cannot be seen through is.
pub fn opened<E: Env>(env: &mut E, matched: &Match) -> Opened {
    let mut opened = Opened {
        arms: Vec::new(),
        reads_a_binder: false,
        suppliers: vec![matched.head.clone()],
        annotations: vec![matched.goal.clone()],
    };

    match &matched.cases {
        Cases::Bool {
            false_case,
            true_case,
        } => opened.arms.extend([false_case.clone(), true_case.clone()]),
        Cases::Switch { cases, default } => {
            opened.arms.extend(cases.iter().map(|(_, arm)| arm.clone()));
            opened.arms.push(default.clone());
        }
        Cases::Induct {
            cases,
            default,
            stated,
        } => {
            for (_, arm) in cases {
                opened.reads_a_binder |= (0..arm.arity()).any(|index| arm.body().uses(index));
                let binders = arm
                    .hint_iter()
                    .map(|hint| Term::free_var(&env.fresh(hint)))
                    .collect::<Vec<_>>();
                opened
                    .arms
                    .push(arm.open(&binders.iter().collect::<Vec<_>>()));
            }
            opened.arms.extend(default.iter().cloned());
            opened.suppliers.extend(stated.iter().cloned());
        }
        Cases::FreeMonoid { carrier } => match carrier {
            Carrier::Nat {
                empty_case,
                cons_case,
            } => {
                opened.reads_a_binder |= cons_case.uses(0);
                let predecessor = fresh(env, cons_case, 0);
                opened.arms.push(empty_case.clone());
                opened.arms.push(cons_case.open(&[&predecessor]));
            }
            Carrier::Bytes {
                empty_case,
                cons_case,
            } => {
                opened.reads_a_binder |= cons_case.uses(0) || cons_case.uses(1);
                let (head, tail) = (fresh(env, cons_case, 0), fresh(env, cons_case, 1));
                opened.arms.push(empty_case.clone());
                opened.arms.push(cons_case.open(&[&head, &tail]));
            }
            Carrier::List {
                elem,
                empty_case,
                cons_case,
            } => {
                opened.reads_a_binder |= cons_case.uses(0) || cons_case.uses(1);
                let (head, tail) = (fresh(env, cons_case, 0), fresh(env, cons_case, 1));
                opened.arms.push(empty_case.clone());
                opened.arms.push(cons_case.open(&[&head, &tail]));
                opened.annotations.push(elem.clone());
            }
        },
    }

    opened
}

/// A fresh binder for the scope's binder at `index`, rendering as the hint it was written under.
fn fresh<E: Env, A: Arity>(env: &mut E, scope: &Scope<A>, index: usize) -> Term {
    Term::free_var(&env.fresh(scope.hint(index)))
}
