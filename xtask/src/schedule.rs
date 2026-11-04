//! Which steps a run takes, and in what order.
//!
//! **A run is what was asked and everything it runs after.** Asking for a group asks for every step it holds; a variable the command line gives keeps of those only the steps that use it. Each step brings what it requires and the producer of each file it consumes, and those bring theirs, so the schedule is the whole closure, known before anything runs.
//!
//! **Order is the order things are written, after what they run after.** A step is placed once everything it runs after is, and otherwise in the order the tree reaches it: an asked group's steps as its file writes them, a step's requirements as its `requires` lists them. So the gate runs its subjects in the order it names them, and nothing about a step's place needs stating beyond what it reads.

use {
    crate::{Given, Name, Tree},
    std::collections::BTreeSet,
};

#[cfg(test)]
mod tests;

/// The steps a run of `asked` performs under `given`, by their place in [`Tree::steps`], in the order it performs them.
pub(crate) fn schedule(tree: &Tree, asked: &Name, given: &Given) -> Vec<usize> {
    let given = given.keys().cloned().collect::<BTreeSet<_>>();
    let asked = tree
        .steps_of(asked)
        .into_iter()
        .filter(|step| given.is_subset(&tree.given_to(*step)))
        .collect::<Vec<_>>();

    let mut ordered = Vec::new();
    for step in asked {
        place(tree, step, &mut ordered);
    }

    ordered
}

/// Place `step` after everything it runs after, each in the order `step` names it. The reader refuses a cycle, so the walk ends.
fn place(tree: &Tree, step: usize, ordered: &mut Vec<usize>) {
    if ordered.contains(&step) {
        return;
    }
    for other in tree.prerequisites(step) {
        place(tree, other, ordered);
    }
    ordered.push(step);
}
