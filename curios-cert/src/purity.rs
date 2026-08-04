//! Whether a term's spelling fixes a value — the premise every scrutinee refinement rests on, decided for both checkers here.
//!
//! # Shared, not duplicated
//!
//! An arm may assume its scrutinee *is* the case's value only because the scrutinee denotes one value, and an operation the host performs at run time does not: `Cell/get(c)` reads `true` before a `Cell/set` and `false` after, so recording an equation at that spelling makes one term denote two values and, from there, `Eq(false, true)`. Both checkers need the same answer to the same question about the same finished term, which is the test [`Judge`](crate::Judge) states for a shared analysis rather than a duplicated one.
//!
//! # Why this is not the reducer's question
//!
//! It used to be. Both sides asked their reducer to reduce the scrutinee and read [`ReduceError::EffectAtTypeLevel`](curios_core::ReduceError::EffectAtTypeLevel) off the refusal, on the reasoning that the reducer's refusing arms *are* the list of operations whose spelling does not fix a value. The list was right and the question was wrong: reduction is **weak-head**, so it stops at a stuck head and hands the application back with its arguments untouched. `f(Cell/get(c))` for a `f` nothing can unfold therefore never reaches the primitive, reduces cleanly, and was registered as a spelling that fixes a value — and both checkers agreed, `curios-cert`'s `assume_case_value` recording at its own `whnf` for the same reason. `curios/src/tests/perimeter.rs`'s `an_effect_behind_a_stuck_head_does_not_refine` holds the derivation that followed.
//!
//! So the question is asked of the *term* instead, which is what makes the answer independent of how far anything reduces. The objection the old comment raised against a syntactic test — that every one of these operations reaches source through a `/sys` wrapper, so the scrutinee carries no `Prim` node to find — is an objection to a *shallow* scan. Following a definition to its body is what sees through the wrapper, and it is the whole of what this walk adds.

#[cfg(test)]
mod tests;

use {
    crate::Env,
    curios_core::{Bound, Free, Prim, Subterm, Term},
    std::collections::BTreeSet,
};

/// Whether `term` carries an operation that cannot happen while a program is being checked, so its spelling does not fix a value.
///
/// Answers about everything the term reaches: its own subterms, and the body of every definition any of them names. `entered` makes that terminate on a recursive definition; the driver's [`Env::effect_memo`] is what keeps it affordable, since the caller is every match arm and the closure of one scrutinee can be most of the standard library. Walking it uncached measured a third of the fixed prelude's build.
///
/// Conservative in the one direction that matters. A shape this cannot see through answers *carries an effect*, which withholds a refinement and checks the arm under fewer assumptions — the incomplete direction, and the safe one. The single exception is an unsolved metavariable, which answers from its spine alone; the arm below states what that concession buys and what it leaves open.
pub fn carries_effect<E: Env>(env: &mut E, term: &Term) -> bool {
    walk(env, term, &mut BTreeSet::new(), &mut false)
}

/// `truncated` reports whether this walk *declined* an edge because the name was already on its own path. It is what makes the memo sound under recursion: an answer computed with an edge cut is an answer about less than the definition reaches, so remembering a `false` derived that way would answer a later query about a closure this walk never saw all of. `true` needs no such care — an effect found is found whatever else was skipped.
fn walk<E: Env>(
    env: &mut E,
    term: &Term,
    entered: &mut BTreeSet<Free>,
    truncated: &mut bool,
) -> bool {
    match &**term {
        Subterm::Prim(prim) if performed_by_the_host(prim) => return true,

        // The `/sys` wrapper case: the primitive is in the body, never at the occurrence.
        Subterm::Var(var) if var.as_bound().is_none() => {
            let name = var.unwrap().clone();

            if let Some(known) = env.effect_memo().get(&name).copied() {
                if known {
                    return true;
                }
            } else if !entered.insert(name.clone()) {
                *truncated = true;
            } else if let Some(body) = env.unfold(&name).cloned() {
                let enclosing = std::mem::replace(truncated, false);
                let carries = walk(env, &body, entered, truncated);

                // A name that resolved to nothing is never remembered either: it may be defined later, and a remembered *no* would be about a body that did not exist yet.
                if carries || !*truncated {
                    env.effect_memo().insert(name, carries);
                }

                *truncated = enclosing || *truncated;

                if carries {
                    return true;
                }
            }
        }

        // A *solved* metavariable is an ordinary term and is walked as one. Forcing is what tells the two apart, and it is safe only on a term no enclosing binder still reaches into — reduction assumes free occurrences and would panic on a dangling index rather than refuse.
        //
        // An unsolved one falls through to its spine, which is all there is to read, and that is a deliberate concession rather than an oversight: answering *carries an effect* for an unsolved metavariable withdraws the refinement a concept-dispatched scrutinee needs — `a <= hi` elaborates to the method projected out of a not-yet-resolved witness, `(?w).1(a, hi)` — and `/std/Str/utf8` does not elaborate without it. What that leaves is a metavariable solved to an effectful term *after* this ran; `PERIMETER.md` records it rather than this claiming it closed.
        Subterm::Metavar(_) => {
            if term.reach() == 0
                && let Ok(solution) = env.force(term)
                && !matches!(&*solution, Subterm::Metavar(_))
            {
                return walk(env, &solution, entered, truncated);
            }
        }

        _ => {}
    }

    term.any_child_term(&mut |child| walk(env, child, entered, truncated))
}

/// The operations `reduce_prim` refuses at the type level, which is the list this must agree with: an effect cannot happen while a program is being checked, so no spelling that performs one denotes a value yet.
///
/// `CellType` is deliberately absent — a cell's *type* is an ordinary type former and reduces like one. What refuses is allocating, reading, or writing the cell.
fn performed_by_the_host(prim: &Prim) -> bool {
    matches!(
        prim,
        Prim::Exit(_) | Prim::Foreign(..) | Prim::Cell(..) | Prim::CellSet(..) | Prim::CellGet(..)
    )
}
