//! The recursive calls one walk typed — the call sites a group's descent is decided from.
//!
//! Like the erased positions, this is the kernel's *output*, collected during the typing walk because that is the only place the answer is available. A group's members are assumed under fresh locals while its bodies are checked (`check_group`), so a recursive call, as the kernel types it, is an application whose spine head is one of those locals, and every such application the walk types is a call it records. What the walk types is every position it accepts, which is what makes the set fail closed where a separate traversal fails open: a position that traversal never visits contributes no edge, and a group with no edges is accepted.
//!
//! **Graded when recorded.** A call is graded by the shared [`grade`] once its arguments are typed, against the caller's parameters as the member's leading lambdas bound them, under a [`SizeContext`] the arms enclosing the call built: each arm's solutions — the equations its case value and index inversion put in force, which the arm substitutes into its body exactly as they refine what it scrutinizes — its payload binders, the nonzero facts a boolean or switch arm establishes, and, where a lambda is applied on the spot, each of its binders standing for its argument. The context lives in the kernel's scope brackets: `Kernel::scoped` marks it and retracts it with the binders, so an arm's knowledge leaves with the arm on every path.
//!
//! **Closed per group.** When `check_group` has checked every body, the group's calls close to a verdict through the shared [`decide`] — the local gate's, and the one obligations (T) and (V) read for the group, an inline one included. A group the walk never typed has no verdict, and a reader takes that as `Partial`.

#[cfg(test)]
mod tests;

use {
    super::{Error, Kernel},
    curios_analysis::{Call, SizeContext, decide, grade, member_arities, nonzero_by},
    curios_core::{Free, Intrinsic, RecGroup, Subterm, Term, Totality},
    std::collections::{HashMap, HashSet},
};

#[derive(Default)]
pub(super) struct Calls {
    /// What the arms enclosing the position being typed have established.
    context: SizeContext,
    /// The groups whose bodies are being checked, innermost last.
    frames: Vec<Frame>,
    /// Each group this walk checked, with the verdict its calls closed to.
    verdicts: HashMap<RecGroup, Totality>,
    /// How many groups that do not descend this walk has closed — or met again through a memo hit on a term whose typing closed one. A judgment compares it before and after to learn whether such a group was typed inside it, which is how a verdict reaches what encloses the group without anything looking the group up: the group is typed with its enclosing binders opened, and a term that holds it holds it closed.
    partial: usize,
    /// The terms whose typing closed a group that does not descend, so a memo hit on one counts it again.
    partial_terms: HashSet<Term>,
    /// For each group checked, whether each member's signature or body enclosed a group of its own that does not descend.
    members_enclosing: HashMap<RecGroup, Vec<bool>>,
    /// The definitions whose check enclosed a group that does not descend.
    definitions_enclosing: HashSet<Free>,
    /// Whether the next judgment types the head of an application spine, which is part of the call the spine makes rather than a call of its own.
    spine_head: bool,
    /// Whether the lambdas the next check opens are a member body's leading lambdas, whose binders are the member's parameters.
    parameters: bool,
}

/// One group whose bodies are being checked.
struct Frame {
    /// The locals its members are assumed under while the bodies are checked.
    members: Vec<Free>,
    /// Each member's parameter count — the columns a call to it is graded over.
    arities: Vec<usize>,
    /// The member whose body is being checked, and the parameters its leading lambdas have bound so far. `None` before the bodies and after them: a member named in a signature or in the group's tail is not a call from a body.
    caller: Option<(usize, Vec<Free>)>,
    calls: Vec<Call>,
}

/// Where the recorder stood when a scope bracket opened, for [`Calls::retract`] to return to.
pub(super) struct CallsMark {
    context: usize,
    frames: usize,
}

impl Calls {
    pub(super) fn mark(&self) -> CallsMark {
        CallsMark {
            context: self.context.depth(),
            frames: self.frames.len(),
        }
    }

    /// Close whatever the bracket `mark` opened — the arm knowledge entered and the groups opened within it.
    pub(super) fn retract(&mut self, mark: CallsMark) {
        self.context.exit_to(mark.context);
        self.frames.truncate(mark.frames);
    }

    /// Whether the judgment about to run types a spine head, clearing the flag for the ones after it.
    pub(super) fn take_spine_head(&mut self) -> bool {
        std::mem::take(&mut self.spine_head)
    }

    /// Mark the next judgment as typing the head of an application spine.
    pub(super) fn set_spine_head(&mut self) {
        self.spine_head = true;
    }

    /// Whether the check about to run opens a member's leading lambdas, clearing the flag for the ones after it.
    pub(super) fn take_parameters(&mut self) -> bool {
        std::mem::take(&mut self.parameters)
    }

    /// Mark the next check as still opening a member's leading lambdas — the body of one that was.
    pub(super) fn continue_parameters(&mut self) {
        self.parameters = true;
    }

    /// `term`'s typing closed a group that does not descend.
    pub(super) fn remember_partial(&mut self, term: &Term) {
        self.partial_terms.insert(term.clone());
    }

    /// A memo hit on `term`: count again whatever group that does not descend its typing closed.
    pub(super) fn recall_partial(&mut self, term: &Term) {
        if self.partial_terms.contains(term) {
            self.partial += 1;
        }
    }
}

impl Kernel {
    /// Open `group` for recording, its members assumed under `members`.
    pub(super) fn open_group(&mut self, group: &RecGroup, members: &[Free]) {
        let arities = member_arities(self, group);
        self.calls.frames.push(Frame {
            members: members.to_vec(),
            arities,
            caller: None,
            calls: Vec::new(),
        });
    }

    /// Begin checking member `index`'s body: its leading lambdas bind its parameters, and the calls inside are its own.
    pub(super) fn begin_member(&mut self, index: usize) {
        if let Some(frame) = self.calls.frames.last_mut() {
            frame.caller = Some((index, Vec::new()));
        }
        self.calls.parameters = true;
    }

    /// One more parameter of the member whose body is being checked.
    pub(super) fn bind_parameter(&mut self, binder: &Free) {
        if let Some(Frame {
            caller: Some((_, params)),
            ..
        }) = self.calls.frames.last_mut()
        {
            params.push(*binder);
        }
    }

    /// Close the innermost group's recorded calls to a verdict, and remember it for the rest of the walk — with `enclosing`, whether each member's signature or body enclosed a group of its own that does not descend.
    pub(super) fn close_group(&mut self, group: &RecGroup, enclosing: Vec<bool>) -> Totality {
        let Some(frame) = self.calls.frames.last_mut() else {
            return Totality::Partial;
        };
        frame.caller = None;
        let totality = decide(std::mem::take(&mut frame.calls));
        self.calls.verdicts.insert(group.clone(), totality);
        self.calls
            .members_enclosing
            .insert(group.clone(), enclosing);
        // Not while a position is being classified: that types the position's *type*, and a group there is not one the position's term holds.
        if totality != Totality::Total && !self.positions.suppressed() {
            self.calls.partial += 1;
        }

        totality
    }

    /// Record a call to `head` with `arguments`, when `head` is a member of a group whose body is being checked.
    ///
    /// Not while a position is being classified: deciding a position's erased half types that position's *type*, which may name a member — `T(n)`, a sibling computing the type — without being a call any body makes.
    pub(super) fn record_call(&mut self, head: &Free, arguments: &[Term]) -> Result<(), Error> {
        if self.calls.frames.is_empty() || self.positions.suppressed() {
            return Ok(());
        }
        let Some((frame, callee)) =
            self.calls
                .frames
                .iter()
                .enumerate()
                .rev()
                .find_map(|(frame, open)| {
                    let callee = open.members.iter().position(|member| member == head)?;
                    Some((frame, callee))
                })
        else {
            return Ok(());
        };
        let Some((caller, params)) = self.calls.frames[frame].caller.clone() else {
            return Ok(());
        };
        let arity = self.calls.frames[frame].arities[callee];

        // Taken out for the grading and put back: the grader reads the context while reducing through this kernel, and nothing it reduces opens an arm.
        let context = std::mem::take(&mut self.calls.context);
        let call = grade(self, &context, caller, &params, callee, arity, arguments);
        self.calls.context = context;
        self.calls.frames[frame].calls.push(call?);
        Ok(())
    }

    /// Whether some group's body is being checked, so that what an arm establishes may grade a call — and is worth reading.
    ///
    /// Outside every body there is no call to grade, and reading an arm's value or a comparison as a size forces it, and typing a lambda applied on the spot through its binders passes the memo by — which an ordinary match elsewhere in a program should not pay for. That includes the tail a group scopes over, typed while its frame is still open: a recursive `let` opens one over the whole rest of a program, and reading every arm of a program's own dispatch as a size would run the kernel out of binders (`curios`'s `tests::toml::every_document_prints_what_its_table_expects`).
    pub(super) fn recording(&self) -> bool {
        self.calls.frames.iter().any(|frame| frame.caller.is_some())
    }

    /// Enter what one arm establishes into the size context, within the current bracket — which `Kernel::scoped` retracts it with.
    ///
    /// The context is taken out for the entry and put back: reading `value` as a shape reduces through this kernel, which must not see the context mid-change.
    fn enter_size(
        &mut self,
        refine: Option<(Free, &Term)>,
        nonzero: Option<Free>,
        payloads: Vec<Free>,
    ) -> Result<(), Error> {
        if !self.recording() {
            return Ok(());
        }
        let mut context = std::mem::take(&mut self.calls.context);
        let entered = context.enter(self, refine, nonzero, payloads);
        self.calls.context = context;
        entered
    }

    /// Within the current bracket, `binder` stands for `value`.
    pub(super) fn refine_size(&mut self, binder: &Free, value: &Term) -> Result<(), Error> {
        self.enter_size(Some((*binder, value)), None, Vec::new())
    }

    /// What an arm that scrutinizes `scrutinee` at `value` establishes, within its bracket: the scrutinee, where it is a binder, stands for `value`, and each other binder in `solutions` stands for what it was solved to.
    ///
    /// The scrutinee is refined from `value` even when the solutions settle it too, because `value` is the arm's own spelling of the case — which a caller may spell as the size order reads it, where the solution carries the typing's.
    pub(super) fn assume_arm(
        &mut self,
        scrutinee: &Term,
        value: &Term,
        solutions: &[(Free, Term)],
    ) -> Result<(), Error> {
        if !self.recording() {
            return Ok(());
        }
        let scrutinee_binder = match &**scrutinee {
            Subterm::Var(var) => var.as_free().cloned(),
            _ => None,
        };
        for (binder, solved) in solutions {
            if Some(binder) != scrutinee_binder.as_ref() {
                self.refine_size(binder, solved)?;
            }
        }
        if let Some(binder) = &scrutinee_binder {
            self.refine_size(binder, &value.substitute(solutions))?;
        }
        Ok(())
    }

    /// What a boolean arm taken at `value` establishes about the comparison `scrutinee` makes, within its bracket: the binder it rules zero out for.
    ///
    /// Read before the arm assumes its case equation, and it must be: that equation makes `scrutinee` reduce to `value` itself, and a comparison read through it is a literal with nothing left to compare.
    pub(super) fn assume_guard(&mut self, scrutinee: &Term, value: &Term) -> Result<(), Error> {
        if !self.recording() {
            return Ok(());
        }
        if let Subterm::Intrinsic(Intrinsic::Bool(taken)) = &**value
            && let Some(atom) = nonzero_by(self, scrutinee, *taken)?
        {
            self.assume_nonzero(atom)?;
        }
        Ok(())
    }

    /// Within the current bracket, `binder` is not zero.
    pub(super) fn assume_nonzero(&mut self, binder: Free) -> Result<(), Error> {
        self.enter_size(None, Some(binder), Vec::new())
    }

    /// Within the current bracket, `binders` are a constructor's payloads.
    pub(super) fn assume_payloads(&mut self, binders: &[Free]) -> Result<(), Error> {
        self.enter_size(None, None, binders.to_vec())
    }

    /// The verdict `group`'s recorded calls closed to, when this walk checked it.
    pub(crate) fn group_verdict(&self, group: &RecGroup) -> Option<Totality> {
        self.calls.verdicts.get(group).copied()
    }

    /// Whether member `index` of `group` enclosed a group of its own that does not descend — true for a group this walk never checked, which is the refusing direction.
    pub(crate) fn member_encloses_partial(&self, group: &RecGroup, index: usize) -> bool {
        self.calls
            .members_enclosing
            .get(group)
            .and_then(|enclosing| enclosing.get(index).copied())
            .unwrap_or(true)
    }

    /// Whether the definition `name`'s check enclosed a group that does not descend.
    pub(crate) fn definition_encloses_partial(&self, name: &Free) -> bool {
        self.calls.definitions_enclosing.contains(name)
    }

    /// How many groups that do not descend this walk has typed so far — compared before and after a check to learn whether it enclosed one.
    pub(crate) fn partial_groups(&self) -> usize {
        self.calls.partial
    }

    /// The definition `name`'s check enclosed a group that does not descend.
    pub(crate) fn note_enclosing_partial(&mut self, name: &Free) {
        self.calls.definitions_enclosing.insert(*name);
    }
}
