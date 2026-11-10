//! A group's graded calls, and their closure under composition.
//!
//! One matrix per recursive call site grades every callee argument against every caller parameter ([`CallMatrix`], with its [`Size`](curios_core::Size) lattice, `curios-core`'s). The group is accepted when every **idempotent** matrix in the transitive closure carries a strict decrease on its diagonal — an idempotent matrix describes a call path that can repeat forever, so a decrease on it is a decrease that cannot be sustained.
//!
//! Nothing here reads a term. What a call argument *is* is [`Shape`](super::Shape)'s question, and grading one against a parameter is `Shape::against`; this module only composes the answers.

use {curios_core::CallMatrix, std::collections::HashSet};

/// How far a group's transitive closure may outgrow one call for each ordered pair of its members.
///
/// Closure is worst-case exponential in the number of call sites. The prelude's largest group closes in tens of matrices; a group that blows past its bound is classified `Partial`, which is the conservative direction. The bound grows with the group ([`closure_limit`]), since members that all reach each other close to a call for every ordered pair of them whatever they descend on, and this is the room left for the matrices a pair may hold beyond its first.
pub const CLOSURE_LIMIT: usize = 4096;

/// The largest closure a group of `members` may reach: a call for each ordered pair of them, and [`CLOSURE_LIMIT`] beside.
pub fn closure_limit(members: usize) -> usize {
    CLOSURE_LIMIT.saturating_add(members.saturating_mul(members))
}

/// One graded call inside a group: from member `caller` to member `callee`, with the size relation its arguments bear to the caller's parameters.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Call {
    pub caller: usize,
    pub callee: usize,
    pub matrix: CallMatrix,
}

/// Close the call matrices transitively, or `None` if the closure outgrows [`closure_limit`] for the members the calls name.
///
/// The closure is what makes mutual recursion work without the analysis knowing which members were declared together: `/std/Cli/check`'s `problems` calls `beneath` which calls back, and only the composite path is a cycle.
///
/// By generator extension: every product of call matrices is a shorter product followed by its last factor, so extending each discovered element by the *generators* alone reaches the whole closure — `|closure| × |calls|` compositions, not `|closure|²`. The difference matters for a group whose closure dwarfs its calls, as one member handing its parameters on permuted closes to every permutation of them. The set is hashed rather than ordered because its one consumer runs an order-independent `all`.
pub fn close(calls: Vec<Call>) -> Option<Vec<Call>> {
    let members = calls
        .iter()
        .flat_map(|call| [call.caller, call.callee])
        .collect::<HashSet<_>>()
        .len();
    let limit = closure_limit(members);
    let mut closed: HashSet<Call> = HashSet::new();
    let mut frontier: Vec<Call> = Vec::new();
    let mut generators: Vec<Call> = Vec::new();
    for call in calls {
        if closed.insert(call.clone()) {
            frontier.push(call.clone());
            generators.push(call);
        }
    }

    while let Some(first) = frontier.pop() {
        let mut discovered = Vec::new();
        for second in &generators {
            if first.callee == second.caller
                && let Some(matrix) = first.matrix.compose(&second.matrix)
            {
                discovered.push(Call {
                    caller: first.caller,
                    callee: second.callee,
                    matrix,
                });
            }
        }

        for candidate in discovered {
            if closed.insert(candidate.clone()) {
                if closed.len() > limit {
                    return None;
                }
                frontier.push(candidate);
            }
        }
    }

    Some(closed.into_iter().collect())
}
