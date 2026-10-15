//! The erasure of a description that may suspend: the suspension a forced description answers in place of its value, the test after every force a performance goes on from, and the loop that resumes a suspension through its frames.
//!
//! A bind forces its action and tests what it answers. A value goes on into the continuation, whose description is forced in tail position as it was before descriptions could suspend; a suspension goes back out with the continuation appended to its frames. Resuming one runs what it holds first — the rest of the description that suspended, or a suspension nested in it — then applies the frames in order, and wraps the frames still to run around anything that suspends again rather than concatenating them, so a resume costs what it runs and not the depth it resumes into.
//!
//! The resume loop has no definition a description could name, so it is emitted where it is needed: beside each `Io/advance`, and once beside the entry, which hands the host a suspension it can advance without knowing its frames. A program that never builds a suspension never reaches either, since `curios-ersd` takes the value arm of every test in it before anything is optimized.

use {
    super::{Error, Lowering, Retyping},
    curios_core::Global,
};

/// What a cell's poll answers: `/std/Option`'s family, with `some` where the cell is filled and `none` where it is not.
#[derive(Debug, Clone, Copy)]
pub(super) struct Polled {
    family: curios_ersd::FamilyId,
    some: curios_ersd::ConstructorId,
    none: curios_ersd::ConstructorId,
}

impl Lowering<'_> {
    /// Bind `rhs` in the innermost open block and hand back its value.
    fn let_atom(&mut self, rhs: curios_ersd::Rhs) -> curios_ersd::Atom {
        curios_ersd::Atom::Value(self.builder.let_value(None, rhs))
    }

    fn suspension(
        &mut self,
        operation: curios_ersd::SuspensionOperation,
        operands: Vec<curios_ersd::Atom>,
    ) -> curios_ersd::Atom {
        self.let_atom(curios_ersd::Rhs::Suspension {
            operation,
            operands,
        })
    }

    fn applied(
        &mut self,
        callee: curios_ersd::Atom,
        arguments: Vec<curios_ersd::Atom>,
    ) -> curios_ersd::Atom {
        self.let_atom(curios_ersd::Rhs::Apply { callee, arguments })
    }

    /// Force a description: apply the closure it erased to at no arguments.
    fn forced(&mut self, description: curios_ersd::Atom) -> curios_ersd::Atom {
        self.applied(description, Vec::new())
    }

    /// A block of its own, returning what `body` computes in it.
    fn block(&mut self, body: impl FnOnce(&mut Self) -> curios_ersd::Atom) -> curios_ersd::BlockId {
        self.builder.open_block();
        let answer = body(self);
        self.builder
            .seal_block(curios_ersd::Terminator::Return(answer))
    }

    /// Branch on whether `answer` is a suspension, into the arm `suspended` builds or the one `otherwise` builds.
    ///
    /// The test is bound immediately before the switch, and a caller returns the switch as it is: the shape in which `curios-ersd`, finding nothing in the program that can suspend, puts the value arm in the switch's place.
    fn on_suspension(
        &mut self,
        answer: curios_ersd::Atom,
        suspended: impl FnOnce(&mut Self) -> curios_ersd::Atom,
        otherwise: impl FnOnce(&mut Self) -> curios_ersd::Atom,
    ) -> curios_ersd::Atom {
        let test = self.suspension(curios_ersd::SuspensionOperation::Is, vec![answer]);
        let if_false = self.block(otherwise);
        let if_true = self.block(suspended);
        self.let_atom(curios_ersd::Rhs::SwitchBool {
            scrutinee: test,
            if_false,
            if_true,
        })
    }

    /// The handles, the cells and the cancel of `suspension`, read in that order.
    fn waiting(&mut self, suspension: curios_ersd::Atom) -> [curios_ersd::Atom; 3] {
        [
            curios_ersd::SuspensionOperation::On,
            curios_ersd::SuspensionOperation::Filled,
            curios_ersd::SuspensionOperation::Cancel,
        ]
        .map(|operation| self.suspension(operation, vec![suspension]))
    }

    /// The performance of `Io/suspend(on, filled, cancel, rest)`: the suspension itself.
    pub(super) fn perform_suspend(
        &mut self,
        on: curios_ersd::Atom,
        filled: curios_ersd::Atom,
        cancel: curios_ersd::Atom,
        rest: curios_ersd::Atom,
    ) -> curios_ersd::Atom {
        self.suspension(
            curios_ersd::SuspensionOperation::New,
            vec![on, filled, cancel, rest],
        )
    }

    /// The performance of `Io/bind(action, continuation)`: force the action, then force what the continuation makes of its value in tail position — or, where the action suspended, answer that suspension with the continuation as its last frame.
    pub(super) fn perform_bind(
        &mut self,
        action: curios_ersd::Atom,
        continuation: curios_ersd::Atom,
    ) -> curios_ersd::Atom {
        let answer = self.forced(action);
        self.on_suspension(
            answer,
            |lowering| {
                lowering.suspension(
                    curios_ersd::SuspensionOperation::Carry,
                    vec![answer, continuation],
                )
            },
            |lowering| {
                let next = lowering.applied(continuation, vec![answer]);
                lowering.forced(next)
            },
        )
    }

    /// The performance of `Io/advance(action, done, suspended)`: force the action and hand its value to `done`, or the handles, the cells, the cancel and a description resuming it to `suspended`, forcing the description either makes in tail position.
    pub(super) fn perform_advance(
        &mut self,
        action: curios_ersd::Atom,
        done: curios_ersd::Atom,
        suspended: curios_ersd::Atom,
    ) -> curios_ersd::Atom {
        let answer = self.forced(action);
        self.on_suspension(
            answer,
            |lowering| {
                let resume = lowering.resumption();
                let [on, filled, cancel] = lowering.waiting(answer);
                let rest = lowering.description("io/rest", |lowering| {
                    lowering.applied(curios_ersd::Atom::Function(resume), vec![answer])
                });
                let next = lowering.applied(suspended, vec![on, filled, cancel, rest]);
                lowering.forced(next)
            },
            |lowering| {
                let next = lowering.applied(done, vec![answer]);
                lowering.forced(next)
            },
        )
    }

    /// What the entry hands the host for `answer`, the forced program: its value, or a suspension the host advances without knowing its frames or reading its cells. `polled` is how a cell's poll answers.
    pub(super) fn yield_to_host(
        &mut self,
        answer: curios_ersd::Atom,
        polled: Polled,
    ) -> curios_ersd::Atom {
        self.on_suspension(
            answer,
            |lowering| {
                let resume = lowering.resumption();
                let surface = lowering.surfacing(resume, polled);
                lowering.applied(curios_ersd::Atom::Function(surface), vec![answer])
            },
            |_| answer,
        )
    }

    /// The family of `/std/Option` and its two constructors, which a cell's poll answers.
    pub(super) fn polled(&mut self, context: &mut Retyping) -> Result<Polled, Error> {
        let syntax = context.syntax().option;
        Ok(Polled {
            family: self
                .induct_row(context, &Global::Authored(syntax.family.qualifier()))?
                .family,
            some: self.outcome_constructor(context, syntax.family, syntax.some)?,
            none: self.outcome_constructor(context, syntax.family, syntax.none)?,
        })
    }

    /// `answer`, or where it is a suspension, what `surface` makes of it.
    fn surfaced(
        &mut self,
        surface: curios_ersd::FunctionId,
        answer: curios_ersd::Atom,
    ) -> curios_ersd::Atom {
        self.on_suspension(
            answer,
            |lowering| lowering.applied(curios_ersd::Atom::Function(surface), vec![answer]),
            |_| answer,
        )
    }

    /// Whether a cell of `cells` is filled: a fold over them, polling each.
    fn any_filled(&mut self, cells: curios_ersd::Atom, polled: Polled) -> curios_ersd::Atom {
        let element = self.builder.value(None);
        let suffix = self.builder.value(None);
        let accumulator = self.builder.value(None);
        let empty = self.block(|lowering| lowering.boolean(false));
        let block = self.block(|lowering| {
            let poll = lowering.let_atom(curios_ersd::Rhs::Cell {
                operation: curios_ersd::CellOperation::Poll {
                    some: polled.some,
                    none: polled.none,
                },
                operands: vec![curios_ersd::Atom::Value(element)],
            });
            let unfilled = lowering.block(|_| curios_ersd::Atom::Value(accumulator));
            let filled = lowering.block(|lowering| lowering.boolean(true));
            lowering.let_atom(curios_ersd::Rhs::MatchVariant {
                family: polled.family,
                scrutinee: poll,
                arms: vec![curios_ersd::VariantArm {
                    constructor: polled.none,
                    bindings: Vec::new(),
                    block: unfilled,
                }],
                default: Some(filled),
            })
        });
        self.let_atom(curios_ersd::Rhs::FoldSequence {
            grain: curios_ersd::SequenceGrain::List,
            scrutinee: cells,
            empty,
            step: curios_ersd::FoldSequenceStep {
                element,
                suffix,
                accumulator,
                block,
            },
        })
    }

    fn boolean(&mut self, value: bool) -> curios_ersd::Atom {
        curios_ersd::Atom::Constant(self.builder.constant(curios_ersd::Constant::Bool(value)))
    }

    /// Define a description named `name` in the innermost open block, its performance what `body` computes, and hand it back.
    fn description(
        &mut self,
        name: &str,
        body: impl FnOnce(&mut Self) -> curios_ersd::Atom,
    ) -> curios_ersd::Atom {
        let function = self.builder.reserve_function();
        let block = self.block(body);
        self.builder
            .define_function(function, Some(name.to_string()), Vec::new(), block);
        self.builder.mark_description(function);
        self.builder.let_functions(vec![function]);
        curios_ersd::Atom::Function(function)
    }

    /// Define the resume loop in the innermost open block and hand back `resume(suspension)`: run what the suspension holds first, then its frames through `frames(answer, pending)`, which applies each in turn and wraps those still pending around whatever suspends.
    fn resumption(&mut self) -> curios_ersd::FunctionId {
        let resume = self.builder.reserve_function();
        let frames = self.builder.reserve_function();

        let suspension = self.builder.value(None);
        let held = curios_ersd::Atom::Value(suspension);
        let body = self.block(|lowering| {
            let next = lowering.suspension(curios_ersd::SuspensionOperation::Next, vec![held]);
            let test = lowering.suspension(curios_ersd::SuspensionOperation::Is, vec![next]);
            let if_false = lowering.block(|lowering| lowering.forced(next));
            let if_true = lowering.block(|lowering| {
                lowering.applied(curios_ersd::Atom::Function(resume), vec![next])
            });
            let first = lowering.let_atom(curios_ersd::Rhs::SwitchBool {
                scrutinee: test,
                if_false,
                if_true,
            });
            let pending = lowering.suspension(curios_ersd::SuspensionOperation::Frames, vec![held]);
            lowering.applied(curios_ersd::Atom::Function(frames), vec![first, pending])
        });
        self.builder.define_function(
            resume,
            Some("io/resume".to_string()),
            vec![suspension],
            body,
        );

        let answer = self.builder.value(None);
        let pending = self.builder.value(None);
        let body = self.block(|lowering| {
            let answered = curios_ersd::Atom::Value(answer);
            let empty = lowering.block(|_| answered);
            let frame = lowering.builder.value(None);
            let later = lowering.builder.value(None);
            let cons = lowering.block(|lowering| {
                lowering.on_suspension(
                    answered,
                    |lowering| {
                        let [on, filled, cancel] = lowering.waiting(answered);
                        lowering.suspension(
                            curios_ersd::SuspensionOperation::Build,
                            vec![
                                on,
                                filled,
                                cancel,
                                answered,
                                curios_ersd::Atom::Value(pending),
                            ],
                        )
                    },
                    |lowering| {
                        let next =
                            lowering.applied(curios_ersd::Atom::Value(frame), vec![answered]);
                        let resumed = lowering.forced(next);
                        lowering.applied(
                            curios_ersd::Atom::Function(frames),
                            vec![resumed, curios_ersd::Atom::Value(later)],
                        )
                    },
                )
            });
            lowering.let_atom(curios_ersd::Rhs::UnconsSequence {
                grain: curios_ersd::SequenceGrain::List,
                scrutinee: curios_ersd::Atom::Value(pending),
                empty,
                cons: curios_ersd::UnconsSequenceStep {
                    element: frame,
                    suffix: later,
                    block: cons,
                },
            })
        });
        self.builder.define_function(
            frames,
            Some("io/frames".to_string()),
            vec![answer, pending],
            body,
        );

        self.builder.let_functions(vec![resume, frames]);
        resume
    }

    /// Define `surface(suspension)` in the innermost open block: the suspension around no frames, with a rest that resumes it through `resume` and a cancel that performs its own, each surfacing in turn whatever suspension it answers.
    ///
    /// A suspension whose cell is already filled is resumed here instead, and what it answers surfaced in turn: the host reads no cell, and the rest can go on at once, so a yield costs no turn of the host. Nothing of the program runs while the host waits, so a cell still empty stays so.
    fn surfacing(
        &mut self,
        resume: curios_ersd::FunctionId,
        polled: Polled,
    ) -> curios_ersd::FunctionId {
        let surface = self.builder.reserve_function();
        let suspension = self.builder.value(None);
        let held = curios_ersd::Atom::Value(suspension);
        let body = self.block(|lowering| {
            let [on, filled, cancel] = lowering.waiting(held);
            let woken = lowering.any_filled(filled, polled);
            let waiting = lowering.block(|lowering| {
                let rest = lowering.description("io/rest", |lowering| {
                    let answer = lowering.applied(curios_ersd::Atom::Function(resume), vec![held]);
                    lowering.surfaced(surface, answer)
                });
                let cancel = lowering.description("io/cancel", |lowering| {
                    let answer = lowering.forced(cancel);
                    lowering.surfaced(surface, answer)
                });
                let none = lowering.let_atom(curios_ersd::Rhs::Sequence {
                    operation: curios_ersd::SequenceOp::ListBuild,
                    operands: Vec::new(),
                });
                lowering.suspension(
                    curios_ersd::SuspensionOperation::Build,
                    vec![on, filled, cancel, rest, none],
                )
            });
            let ready = lowering.block(|lowering| {
                let answer = lowering.applied(curios_ersd::Atom::Function(resume), vec![held]);
                lowering.surfaced(surface, answer)
            });
            lowering.let_atom(curios_ersd::Rhs::SwitchBool {
                scrutinee: woken,
                if_false: waiting,
                if_true: ready,
            })
        });
        self.builder.define_function(
            surface,
            Some("io/surface".to_string()),
            vec![suspension],
            body,
        );
        self.builder.let_functions(vec![surface]);
        surface
    }
}
