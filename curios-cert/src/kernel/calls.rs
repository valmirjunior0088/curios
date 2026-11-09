//! The recursive calls one walk typed — the call sites a group's descent is decided from.
//!
//! Like the erased positions, this is the kernel's *output*, collected during the typing walk because that is the only place the answer is available. A group's members are assumed under fresh locals while its bodies are checked (`check_group`), so a recursive call, as the kernel types it, is an application whose spine head is one of those locals, and every such application the walk types is a call it records. What the walk types is every position it accepts, which is what makes the set fail closed where a separate traversal fails open: a position that traversal never visits contributes no edge, and a group with no edges is accepted.
//!
//! **Graded when recorded.** A call is graded by the shared [`grade`] once its arguments are typed, against the caller's parameters as the member's leading lambdas bound them, under a [`SizeContext`] the arms enclosing the call built: each arm's own case, where it scrutinizes a binder, and nothing its index inversion solved ([`Kernel::assume_arm`]) — its payload binders, the nonzero facts a boolean or switch arm establishes, and, where a lambda is applied on the spot, each of its binders standing for its argument. The context lives in the kernel's scope brackets: `Kernel::scoped` marks it and retracts it with the binders, so an arm's knowledge leaves with the arm on every path.
//!
//! **Closed per group.** When `check_group` has checked every body, the group's calls close to a verdict through the shared [`decide`] — the local gate's, and the one obligations (T) and (V) read for the group, an inline one included. A group the walk never typed has no verdict, and a reader takes that as `Partial`.

#[cfg(test)]
mod tests;

use {
    super::{Error, Kernel},
    crate::{binder_bounds, decide, grade, handed, member_arities, nonzero_by, refined_by_arm},
    curios_analysis::{Call, SizeContext},
    curios_common::Keyed,
    curios_core::{CallMatrix, Free, Global, Intrinsic, RecGroup, Subterm, Term, Totality},
    std::{
        collections::{HashMap, HashSet},
        mem,
    },
};

#[derive(Default)]
pub(super) struct Calls {
    /// What the arms enclosing the position being typed have established.
    context: SizeContext,
    /// The groups whose bodies are being checked, innermost last.
    frames: Vec<Frame>,
    /// Each group this walk checked, with the verdict its calls closed to.
    verdicts: HashMap<Keyed<RecGroup>, Totality>,
    /// How many groups that do not descend this walk has closed — or met again through a memo hit on a typing that closed one. A judgment compares it before and after to learn whether such a group was typed inside it, which is how a verdict reaches what encloses the group without anything looking the group up: the group is typed with its enclosing binders opened, and a term that holds it holds it closed.
    partial: usize,
    /// For each group checked, whether each member's signature or body enclosed a group of its own that does not descend.
    members_enclosing: HashMap<Keyed<RecGroup>, Vec<bool>>,
    /// The definitions whose check enclosed a group that does not descend.
    definitions_enclosing: HashSet<Global>,
    /// Whether the next judgment types the head of an application spine, which is part of the call the spine makes rather than a call of its own.
    spine_head: bool,
    /// Whether the lambdas the next check opens are a member body's leading lambdas, whose binders are the member's parameters.
    parameters: bool,
    /// What the lambda the next check opens is handed where its callee applies it ([`handed`]): taken by that check alone, as `parameters` is.
    assumed: Option<Vec<CallMatrix>>,
    /// The member the next typing of a bare member local names, with what its callee hands it ([`handed`]): calls graded through each in place of one claiming nothing. Keyed by the member, so no other local's typing takes it.
    handed: Option<(Free, Vec<CallMatrix>)>,
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
    /// What reduction unfolds each member's local to while the group's bodies are checked: its selection out of the members known to terminate, for each of those, and nothing for one withheld ([`Calls::member_value`]).
    values: Vec<Option<Term>>,
}

/// Where the recorder stood when a scope bracket opened, for [`Calls::retract`] to return to.
pub(super) struct CallsMark {
    context: usize,
    frames: usize,
}

/// What the recorder held when a walk that may be refused began, for [`Calls::take_back`] to return to.
pub(super) struct Recorded {
    /// For each frame open then, how many calls it held and how many parameters its caller had bound.
    frames: Vec<(usize, usize)>,
    partial: usize,
    spine_head: bool,
    parameters: bool,
}

impl Calls {
    pub(super) fn mark(&self) -> CallsMark {
        CallsMark {
            context: self.context.depth(),
            frames: self.frames.len(),
        }
    }

    /// Close whatever the bracket `mark` opened — the arm knowledge entered and the groups opened within it — answering whether any knowledge was closed.
    pub(super) fn retract(&mut self, mark: CallsMark) -> bool {
        let established = self.context.depth() > mark.context;
        self.context.exit_to(mark.context);
        self.frames.truncate(mark.frames);

        established
    }

    /// What the recorder holds now, for a walk that may be refused to be taken back to.
    pub(super) fn recorded(&self) -> Recorded {
        Recorded {
            frames: self
                .frames
                .iter()
                .map(|frame| {
                    let bound = frame.caller.as_ref().map_or(0, |(_, bound)| bound.len());
                    (frame.calls.len(), bound)
                })
                .collect(),
            partial: self.partial,
            spine_head: self.spine_head,
            parameters: self.parameters,
        }
    }

    /// Take back what a refused walk recorded since `recorded`: the calls it added to the frames that stood open, the parameters it bound, the groups it counted as not descending, and the two flags, which a refusal leaves as the judgment it interrupted had set them. Asked once the walk's bracket has closed the frames the walk opened ([`Calls::retract`]), so the frames left are the ones `recorded` counted.
    ///
    /// A group's verdict, and the terms whose typing closed a group that does not descend, stay: each is filed under what it is about and is true of it however the walk around it ended.
    pub(super) fn take_back(&mut self, recorded: Recorded) {
        for (frame, (calls, bound)) in self.frames.iter_mut().zip(recorded.frames) {
            frame.calls.truncate(calls);
            if let Some((_, parameters)) = &mut frame.caller {
                parameters.truncate(bound);
            }
        }
        self.partial = recorded.partial;
        self.spine_head = recorded.spine_head;
        self.parameters = recorded.parameters;
    }

    /// Whether the judgment about to run types a spine head, clearing the flag for the ones after it.
    pub(super) fn take_spine_head(&mut self) -> bool {
        mem::take(&mut self.spine_head)
    }

    /// Mark the next judgment as typing the head of an application spine.
    pub(super) fn set_spine_head(&mut self) {
        self.spine_head = true;
    }

    /// Whether the check about to run opens a member's leading lambdas, clearing the flag for the ones after it.
    pub(super) fn take_parameters(&mut self) -> bool {
        mem::take(&mut self.parameters)
    }

    /// Mark the next check as still opening a member's leading lambdas — the body of one that was.
    pub(super) fn continue_parameters(&mut self) {
        self.parameters = true;
    }

    /// What the lambda the check about to run opens is handed, clearing it for the ones after it.
    pub(super) fn take_assumed(&mut self) -> Option<Vec<CallMatrix>> {
        self.assumed.take()
    }

    /// The calls the typing of `member` about to run makes where its callee applies it, clearing them: `None` for any other local.
    pub(super) fn take_handed(&mut self, member: &Free) -> Option<Vec<CallMatrix>> {
        match &self.handed {
            Some((handed, _)) if handed == member => self.handed.take().map(|(_, handed)| handed),
            _ => None,
        }
    }

    /// Hand the argument whose check is about to run what its callee applies it with, `handed`: a lambda takes it as its binders' bounds, a member local as the calls it makes.
    pub(super) fn hand(&mut self, argument: &Term, handed: Vec<CallMatrix>) {
        match &**argument {
            Subterm::Func(_) => self.assumed = Some(handed),
            Subterm::Var(var) => self.handed = var.as_free().map(|member| (*member, handed)),
            _ => {}
        }
    }

    /// Clear whatever the check of an argument left of what was handed to it, whether or not it took it.
    pub(super) fn clear_handed(&mut self) {
        self.assumed = None;
        self.handed = None;
    }

    /// What the local `name` unfolds to in reduction: where it names a member known to terminate of a group whose bodies are being checked, its selection out of the members known to terminate ([`Kernel::read_known`]), and nothing otherwise. A member withheld stands for nothing in its group's bodies.
    pub(super) fn member_value(&self, name: &Free) -> Option<Term> {
        self.frames.iter().rev().find_map(|frame| {
            let index = frame.members.iter().position(|member| member == name)?;
            frame.values.get(index).cloned().flatten()
        })
    }

    /// Whether `term` names a member of a group whose body is being checked. Such a term may hold a call, which is recorded where it is typed and graded against the member whose body states it, so no judgment about it is remembered: [`Kernel::infer_hit`](super::Kernel::infer_hit).
    pub(super) fn names_member(&self, term: &Term) -> bool {
        term.has_local_free()
            && self.frames.iter().any(|frame| {
                frame.caller.is_some()
                    && frame
                        .members
                        .iter()
                        .any(|member| term.mentions_free(member))
            })
    }

    /// A memo hit on a typing that closed a group that does not descend: count the group again, as the typing it stands for did.
    pub(super) fn recall_partial(&mut self) {
        self.partial += 1;
    }
}

impl Kernel<'_> {
    /// Open `group` for recording, its members assumed under `members`, each standing for nothing until [`Kernel::read_known`] reads it by value.
    pub(super) fn open_group(&mut self, group: &RecGroup, members: &[Free]) {
        let arities = member_arities(self, group);
        self.calls.frames.push(Frame {
            members: members.to_vec(),
            arities,
            caller: None,
            calls: Vec::new(),
            values: vec![None; members.len()],
        });
    }

    /// Read the innermost group's members by value in its bodies, each to what `values` holds for it: the members known to terminate to their selections, and the others to nothing ([`Calls::member_value`]).
    ///
    /// What the scope's tables hold of a term naming a member was read while it stood for nothing, so reading it by value discards them, as a local re-typed does; [`Kernel::close_group`] discards them again where the reading ends.
    pub(super) fn read_known(&mut self, values: Vec<Option<Term>>) {
        let Some(frame) = self.calls.frames.last_mut() else {
            return;
        };
        if values.iter().any(Option::is_some) {
            self.memos.begin_equations();
        }
        frame.values = values;
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
        let read_by_value = mem::take(&mut frame.values).iter().any(Option::is_some);
        let totality = decide(mem::take(&mut frame.calls));
        // Past its bodies a member stands for nothing again until the tail binds it, so what the scope's tables read through its value goes.
        if read_by_value {
            self.memos.begin_equations();
        }
        let group = Keyed::new(group.clone());
        self.calls.verdicts.insert(group.clone(), totality);
        self.calls.members_enclosing.insert(group, enclosing);
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
        // A call is graded on what it passes, and a `let`-bound local passes what it stands for; a variable an arm's index inversion solved passes itself, since the size order reads nothing an inversion concluded, as the elaborator's discovery walk grades it.
        let arguments = arguments
            .iter()
            .map(|argument| self.graded(argument))
            .collect::<Vec<_>>();

        // Taken out for the grading and put back: the grader reads the context while reducing through this kernel, and nothing it reduces opens an arm.
        let context = mem::take(&mut self.calls.context);
        let call = grade(self, &context, caller, &params, callee, arity, &arguments);
        self.calls.context = context;
        self.calls.frames[frame].calls.push(call?);
        Ok(())
    }

    /// The member whose body is being checked innermost, with the parameters its leading lambdas bound: what a call it makes is graded against.
    fn recording_caller(&self) -> Option<(usize, Vec<Free>)> {
        self.calls
            .frames
            .iter()
            .rev()
            .find_map(|frame| frame.caller.clone())
    }

    /// What a call of `callee` with `arguments`, typed in a body being checked, hands each argument at a parameter `callee` applies as its summary states ([`handed`]), graded against the parameters of the member whose body it is: `None` at every position outside a body.
    pub(super) fn handed_to(
        &mut self,
        callee: &Global,
        arguments: &[Term],
    ) -> Result<Vec<Option<Vec<CallMatrix>>>, Error> {
        let Some((_, params)) = self
            .recording_caller()
            .filter(|_| self.recording() && !self.positions.suppressed())
        else {
            return Ok(vec![None; arguments.len()]);
        };
        // Asked first and of the record alone, since grading what a call hands reads each argument whole, and most calls are of a definition that summarizes nothing.
        let summarized = self
            .globals
            .applications(callee)
            .is_some_and(|applications| {
                (0..arguments.len()).any(|position| {
                    applications
                        .of(position)
                        .is_some_and(|applied| !applied.is_empty())
                })
            });
        if !summarized {
            return Ok(vec![None; arguments.len()]);
        }
        // Graded on what each argument passes, as a call is ([`Kernel::record_call`]).
        let arguments = arguments
            .iter()
            .map(|argument| self.graded(argument))
            .collect::<Vec<_>>();
        let context = mem::take(&mut self.calls.context);
        let handed = handed(self, &context, &params, callee, &arguments);
        self.calls.context = context;
        handed
    }

    /// Within the current bracket, each of `binders`, a lambda's handed where its callee applies it as `handed` grades, stands to the parameters of the member whose body it is as [`binder_bounds`] reads them.
    pub(super) fn assume_handed(&mut self, binders: &[Free], handed: &[CallMatrix]) {
        let Some((_, params)) = self.recording_caller().filter(|_| self.recording()) else {
            return;
        };
        let bounds = binder_bounds(handed, &params, binders);
        if bounds.is_empty() {
            return;
        }
        self.calls.context.assume(bounds);
        // A typing remembered outside the lambda does not answer inside it, where its binders stand below what the caller was handed.
        self.memos.begin_sizes();
    }

    /// Record the calls a bare `member` local makes where its callee applies it as `handed` grades: one for each application, where the member belongs to the group whose body is being checked innermost and an application hands it all its parameters, and one claiming nothing otherwise.
    pub(super) fn record_handed(
        &mut self,
        member: &Free,
        handed: &[CallMatrix],
    ) -> Result<(), Error> {
        let innermost = self
            .calls
            .frames
            .iter()
            .rposition(|frame| frame.caller.is_some());
        let callee = innermost.and_then(|frame| {
            self.calls.frames[frame]
                .members
                .iter()
                .position(|candidate| candidate == member)
                .map(|callee| (frame, callee))
        });
        let Some((frame, callee)) = callee.filter(|_| !self.positions.suppressed()) else {
            return self.record_call(member, &[]);
        };
        let Some((caller, params)) = self.calls.frames[frame].caller.clone() else {
            return self.record_call(member, &[]);
        };
        let arity = self.calls.frames[frame].arities[callee];
        for matrix in handed {
            let matrix = match matrix.columns() == arity && matrix.rows() == params.len() {
                true => matrix.clone(),
                false => CallMatrix::unknown(params.len(), arity),
            };
            self.calls.frames[frame].calls.push(Call {
                caller,
                callee,
                matrix,
            });
        }
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
        let mut context = mem::take(&mut self.calls.context);
        let entered = context.enter(self, refine, nonzero, payloads);
        self.calls.context = context;
        // A group typed inside a term closes under what the arms around it established, so a typing remembered outside this one does not answer inside it. Most arms assume an equation beside what they establish, which clears as much; the default arm of a `switch` assumes none.
        self.memos.begin_sizes();
        entered
    }

    /// Within the current bracket, `binder` stands for `value`.
    pub(super) fn refine_size(&mut self, binder: &Free, value: &Term) -> Result<(), Error> {
        self.enter_size(Some((*binder, value)), None, Vec::new())
    }

    /// What an arm that scrutinizes `scrutinee` at `value` establishes, within its bracket: the scrutinee, where it is a binder, stands for `value`, the arm's own spelling of the case.
    ///
    /// Nothing the arm's index inversion solved enters the size order, for the reason [`refined_by_arm`] states; what it solves still specializes the arm for typing, which `bind_solutions` binds.
    pub(super) fn assume_arm(&mut self, scrutinee: &Term, value: &Term) -> Result<(), Error> {
        if !self.recording() {
            return Ok(());
        }
        if let Some(binder) = refined_by_arm(scrutinee) {
            self.refine_size(&binder, value)?;
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

    /// File what the walk decided of `group` as it is stated, over every assignment of the sorts its declaration leaves: the verdict and the members' enclosures the obligations read under the group's own spelling, each assignment's being filed under the group at that assignment.
    pub(super) fn file_group(
        &mut self,
        group: &RecGroup,
        totality: Totality,
        enclosing: Vec<bool>,
    ) {
        let group = Keyed::new(group.clone());
        self.calls.verdicts.insert(group.clone(), totality);
        self.calls.members_enclosing.insert(group, enclosing);
    }

    /// The verdict `group`'s recorded calls closed to, when this walk checked it.
    pub(crate) fn group_verdict(&self, group: &RecGroup) -> Option<Totality> {
        self.calls.verdicts.get(&Keyed::new(group.clone())).copied()
    }

    /// Whether member `index` of `group` enclosed a group of its own that does not descend — true for a group this walk never checked, which is the refusing direction.
    pub(crate) fn member_encloses_partial(&self, group: &RecGroup, index: usize) -> bool {
        self.calls
            .members_enclosing
            .get(&Keyed::new(group.clone()))
            .and_then(|enclosing| enclosing.get(index).copied())
            .unwrap_or(true)
    }

    /// Whether the definition `name`'s check enclosed a group that does not descend.
    pub(crate) fn definition_encloses_partial(&self, name: &Global) -> bool {
        self.calls.definitions_enclosing.contains(name)
    }

    /// How many groups that do not descend this walk has typed so far — compared before and after a check to learn whether it enclosed one.
    pub(crate) fn partial_groups(&self) -> usize {
        self.calls.partial
    }

    /// The definition `name`'s check enclosed a group that does not descend.
    pub(crate) fn note_enclosing_partial(&mut self, name: &Global) {
        self.calls.definitions_enclosing.insert(*name);
    }
}
