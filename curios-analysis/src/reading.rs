//! What a stuck `match` reduces to, which recursive groups are read by their unfolding, and the fixpoint such a group's unfolding is read by, stated once for the walks that read a type through what it computes.
//!
//! Every instance of a `match` reduces it to one of its arms, whose binders the head supplies — or, where definitional K selects a proposition's sole arm, the instance the `match` states — so the arms opened against fresh binders, beside what supplies their binders, bound every reduct. The goal and a list's element type stand in no reduct: an instance that reduces the `match` drops both. Strict positivity and variance read the same parts and record different things of them, so the parts are told apart here and what each walk records stays in that walk.

use {
    crate::{Env, group_totality},
    curios_core::{Arity, Carrier, Cases, Match, RecGroup, Scope, Term},
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

/// Whether a walk reads `group` by the least fixpoint of its unfolding: only where the group terminates ([`group_totality`]). Both checkers unfold only what terminates (`known_to_terminate`), and a walk opening a group that may not would spend its declaration's budget on an unfolding neither checker makes, before the declaration is refused for reaching it; such a group is read as written, as every form a walk cannot see through is. A reduction refused while deciding is the caller's to keep.
pub fn read_by_unfolding<E: Env>(env: &mut E, group: &RecGroup) -> Result<bool, E::Error> {
    Ok(group_totality(env, group)?.is_total())
}

/// The least fixpoint of `step` from `seed`: `step` applied to its own result until it hands back what it was given. A walk reading a recursive group by its unfolding climbs a finite lattice from its bottom, joining each step's reading with the estimate it read under, so the loop ends.
pub fn least_fixpoint<S: PartialEq>(seed: S, mut step: impl FnMut(&S) -> S) -> S {
    let mut current = seed;
    loop {
        let next = step(&current);
        if next == current {
            return current;
        }
        current = next;
    }
}
