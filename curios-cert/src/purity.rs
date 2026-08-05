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
//!
//! # Why a search for effects is not enough
//!
//! Because effectfulness of `f(true)` is not a property of `f(true)`. Searching a term finds every operation the term *reaches*, and a call through a binder reaches nothing: at the moment an arm records its equation, `f` stands for a closure that does not exist yet. So `match f(true)` for a parameter `f` was a scrutinee with no effect anywhere in it or in anything it names, refined by both checkers — and a caller binding `f := (b) => Cell/get(c)` then made that one spelling read `true` before a `Cell/set` and `false` after, which is `Eq(false, true)` again. `curios/src/tests/perimeter.rs`'s `an_effect_behind_a_function_parameter_does_not_refine` holds the derivation.
//!
//! No improvement to the search closes that, which is why [`fixes_no_value`] asks a second question instead: does the walk *read the body of every function this term would call*. A lambda, a `let`, a `match`, a `rec` member and a definition all put the callee where the search already looks; a binder does not. The premise the whole rule rests on is that a spelling fixes a value, and a call whose callee is unknown fixes none.
//!
//! What this costs is the refinement a *pure* opaque head would have licensed — `f(b)` for a parameter `f` no longer teaches its arm anything. That is the incomplete direction, and unavoidable without an effect discipline on the arrow: the function space admits `Cell/get`, so no property of `(Bool) -> Bool` distinguishes the two.

#[cfg(test)]
mod tests;

use {
    crate::Env,
    curios_core::{Apply, Bound, Free, Prim, Proj, Subterm, Term, UniverseInst},
    std::collections::BTreeSet,
};

/// Whether `term`'s spelling fails to fix a value, so no arm may assume it *is* the case's.
///
/// Two ways for that to happen, and the module documentation states why one is not the other. The term reaches an operation the host performs, which cannot happen while a program is being checked. Or the term applies a function whose body this walk does not read, in which case what it computes is whatever a caller supplies.
///
/// Answers about everything the term reaches: its own subterms, and the body of every definition any of them names. `entered` makes that terminate on a recursive definition; the driver's [`Env::effect_memo`] is what keeps it affordable, since the caller is every match arm and the closure of one scrutinee can be most of the standard library. Walking it uncached measured a third of the fixed prelude's build.
///
/// Conservative in the one direction that matters. A shape this cannot see through answers *fixes no value*, which withholds a refinement and checks the arm under fewer assumptions — the incomplete direction, and the safe one. The single exception is an unsolved metavariable, which answers from its spine alone; the arm below states what that concession buys and what it leaves open.
pub fn fixes_no_value<E: Env>(env: &mut E, term: &Term) -> bool {
    walk(env, term, &mut BTreeSet::new(), &mut false)
}

/// Whether an application's head resolves to something whose body [`walk`] reads.
///
/// A lambda carries its body, a `let`, a `match` and a `rec` member carry every body they could select, a record literal carries its fields, and a definition is one [`Env::unfold`] away — so for all of these the effect search above covers the callee, and the application fixes whatever the callee fixes. A **binder** resolves to nothing: it stands for an argument, and an argument is the caller's to choose.
///
/// An unsolved metavariable is admitted, which is the same concession [`walk`] makes at its own arm and not a second one — a solved metavariable is forced there and walked, so it reaches here as its solution. Refusing it withdraws the refinement a concept-dispatched scrutinee needs, and `/std/Str/utf8` does not elaborate without it.
///
/// Total by construction: a head shape nobody anticipated answers *not read*, which withholds a refinement rather than granting one.
fn reads_the_callee<E: Env>(env: &E, head: &Term) -> bool {
    match &**head {
        Subterm::Func(_)
        | Subterm::Let(_)
        | Subterm::Match(_)
        | Subterm::Rec(_)
        | Subterm::Tuple(_)
        | Subterm::Struct(_)
        | Subterm::Metavar(_) => true,
        Subterm::Var(var) => var.as_free().is_some_and(|name| env.unfold(name).is_some()),
        Subterm::Apply(Apply { head, .. })
        | Subterm::Proj(Proj { head, .. })
        | Subterm::UniverseInst(UniverseInst { head, .. }) => reads_the_callee(env, head),
        _ => false,
    }
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

        // The call whose callee is the caller's to choose. Nothing written here carries an effect and nothing it names does either, so the search below answers *pure* and is right about the term while being wrong about the value.
        Subterm::Apply(Apply { head, .. }) if !reads_the_callee(env, head) => return true,

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
