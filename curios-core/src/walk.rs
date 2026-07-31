//! The shared iterative walk driver over a term's node DAG.
//!
//! Every read-only analysis that visits a term's nodes shares this one Enter/Exit machine instead of hand-rolling its worklist: an explicit frame stack absorbs the term's depth, so a data-shaped spine of any length is walked at O(1) native depth per link — the property the kernel's decision procedures already guarantee and every analysis here must match. Children are enumerated exclusively through [`Subterm::any_child_term`](super::Subterm::any_child_term), so a new term former flows into every analysis by extending that one fold.
//!
//! The driver deliberately owns no memoization: each analysis keys its memo differently (a per-walk pointer set, a module-lifetime structural map, a cache's own filled bit), so deduplication lives in the `enter` hook, which prunes a settled node by returning [`Enter::Skip`] with its result. The hooks share one `&mut S` state — a single closure environment cannot lend one memo to two `FnMut`s — and the frames hold owned `Term` clones, which is what keeps node addresses stable for pointer-keyed memos across the walk.
//!
//! What stays off this driver, deliberately: `traverse` (a rebuilding visitor with binder-depth tracking), the interpreters (`whnf`, `reduce`, `elaborate` — suspended computations with domain frames), `Term`'s equality (pairwise masking), and `Node`'s drop (ownership dismantling).

#[cfg(test)]
mod tests;

use {
    super::Term,
    std::{convert::Infallible, ops::ControlFlow, vec::Drain},
};

/// The `enter` hook's verdict on one node: walk its children and combine at `exit`, or settle it right now — a memo hit, a pruned subtree, an already-filled cache.
pub enum Enter<R> {
    Descend,
    Skip(R),
}

/// The children's results at an `exit` combine, drained in child order. Dropping unread results is fine — a pre-order analysis with `R = ()` never looks at them.
pub struct ChildResults<'a, R> {
    results: Drain<'a, R>,
}

impl<R> Iterator for ChildResults<'_, R> {
    type Item = R;

    fn next(&mut self) -> Option<R> {
        self.results.next()
    }
}

impl<R> ExactSizeIterator for ChildResults<'_, R> {
    fn len(&self) -> usize {
        self.results.len()
    }
}

/// One pending obligation: visit a node, or combine an already-visited node's child results, which sit as the `usize` newest entries of the results stack.
enum Frame {
    Enter(Term),
    Exit(Term, usize),
}

impl Term {
    /// Walk this term's nodes iteratively: `enter` decides per node whether to descend or settle ([`Enter`]), `exit` combines a descended node's child results into its own, and the root's result comes back. `state` is threaded to both hooks — the memo-and-context bundle an analysis needs visible on both sides.
    pub fn walk<S, R>(
        &self,
        state: &mut S,
        mut enter: impl FnMut(&mut S, &Term) -> Enter<R>,
        exit: impl FnMut(&mut S, &Term, ChildResults<'_, R>) -> R,
    ) -> R {
        match self.try_walk(
            state,
            |state, term| ControlFlow::<Infallible, _>::Continue(enter(state, term)),
            exit,
        ) {
            ControlFlow::Continue(result) => result,
        }
    }

    /// [`Term::walk`] with a short-circuit: an `enter` verdict of `Break` abandons the walk and returns immediately — the `any_*` analyses' exit door.
    pub fn try_walk<S, R, B>(
        &self,
        state: &mut S,
        mut enter: impl FnMut(&mut S, &Term) -> ControlFlow<B, Enter<R>>,
        mut exit: impl FnMut(&mut S, &Term, ChildResults<'_, R>) -> R,
    ) -> ControlFlow<B, R> {
        let mut frames = vec![Frame::Enter(self.clone())];
        let mut results: Vec<R> = Vec::new();

        while let Some(frame) = frames.pop() {
            match frame {
                Frame::Enter(term) => match enter(state, &term)? {
                    Enter::Skip(result) => results.push(result),
                    Enter::Descend => {
                        let mut children = Vec::new();
                        term.as_ref().any_child_term(&mut |child| {
                            children.push(child.clone());
                            false
                        });
                        frames.push(Frame::Exit(term, children.len()));
                        frames.extend(children.into_iter().rev().map(Frame::Enter));
                    }
                },
                Frame::Exit(term, child_count) => {
                    let combined = {
                        let start = results
                            .len()
                            .checked_sub(child_count)
                            .expect("each exit frame owns its child results");
                        let children = ChildResults {
                            results: results.drain(start..),
                        };
                        exit(state, &term, children)
                    };
                    results.push(combined);
                }
            }
        }

        ControlFlow::Continue(results.pop().expect("a walk returns its root's result"))
    }
}
