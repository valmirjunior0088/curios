//! The erasure of a description that may suspend: the suspension a forced description answers in place of its value, the test after every force a performance goes on from, and the loop that resumes a suspension through its frames.
//!
//! A bind forces its action and tests what it answers. A value goes on into the continuation, whose description is forced in tail position as it was before descriptions could suspend; a suspension goes back out with the continuation appended to its frames. Resuming one runs what it holds first — the rest of the description that suspended, or a suspension nested in it — then applies the frames in order, and wraps the frames still to run around anything that suspends again rather than concatenating them, so a resume costs what it runs and not the depth it resumes into.
//!
//! The resume loop has no definition a description could name, so it is emitted where it is needed: beside each `Io/advance`, and once beside the entry, which hands the host a suspension it can advance without knowing its frames. A program that never builds a suspension never reaches either, since `curios-anf` takes the value arm of every test in it before anything is optimized.

use {super::Lowering, crate::Error, curios_elab::Retyping};

/// What a cell's poll answers: `/std/Option`'s family, with `some` where the cell is filled and `none` where it is not.
#[derive(Debug, Clone, Copy)]
pub struct Polled {
    pub family: curios_anf::FamilyId,
    pub some: curios_anf::ConstructorId,
    pub none: curios_anf::ConstructorId,
}

impl Lowering<'_> {
    /// Bind `rhs` in the innermost open block and hand back its value.
    pub fn let_atom(&mut self, rhs: curios_anf::Rhs) -> curios_anf::Atom {
        curios_anf::Atom::Value(self.builder.let_value(None, rhs))
    }

    fn suspension(
        &mut self,
        operation: curios_anf::SuspensionOperation,
        operands: Vec<curios_anf::Atom>,
    ) -> curios_anf::Atom {
        self.let_atom(curios_anf::Rhs::Suspension {
            operation,
            operands,
        })
    }

    pub fn applied(
        &mut self,
        callee: curios_anf::Atom,
        arguments: Vec<curios_anf::Atom>,
    ) -> curios_anf::Atom {
        self.let_atom(curios_anf::Rhs::Apply { callee, arguments })
    }

    /// Force a description: apply the closure it erased to at no arguments.
    fn forced(&mut self, description: curios_anf::Atom) -> curios_anf::Atom {
        self.applied(description, Vec::new())
    }

    /// A block of its own, returning what `body` computes in it.
    pub fn block(
        &mut self,
        body: impl FnOnce(&mut Self) -> curios_anf::Atom,
    ) -> curios_anf::BlockId {
        self.builder.open_block();
        let answer = body(self);
        self.builder
            .seal_block(curios_anf::Terminator::Return(answer))
    }

    /// Branch on whether `answer` is a suspension, into the arm `suspended` builds or the one `otherwise` builds.
    ///
    /// The test is bound immediately before the switch, and a caller returns the switch as it is: the shape in which `curios-anf`, finding nothing in the program that can suspend, puts the value arm in the switch's place.
    fn on_suspension(
        &mut self,
        answer: curios_anf::Atom,
        suspended: impl FnOnce(&mut Self) -> curios_anf::Atom,
        otherwise: impl FnOnce(&mut Self) -> curios_anf::Atom,
    ) -> curios_anf::Atom {
        let test = self.suspension(curios_anf::SuspensionOperation::Is, vec![answer]);
        let if_false = self.block(otherwise);
        let if_true = self.block(suspended);
        self.let_atom(curios_anf::Rhs::SwitchBool {
            scrutinee: test,
            if_false,
            if_true,
        })
    }

    /// The handles, the cells and the cancel of `suspension`, read in that order.
    fn waiting(&mut self, suspension: curios_anf::Atom) -> [curios_anf::Atom; 3] {
        [
            curios_anf::SuspensionOperation::On,
            curios_anf::SuspensionOperation::Filled,
            curios_anf::SuspensionOperation::Cancel,
        ]
        .map(|operation| self.suspension(operation, vec![suspension]))
    }

    /// The performance of `Io/suspend(on, filled, cancel, rest)`: the suspension itself.
    pub fn perform_suspend(
        &mut self,
        on: curios_anf::Atom,
        filled: curios_anf::Atom,
        cancel: curios_anf::Atom,
        rest: curios_anf::Atom,
    ) -> curios_anf::Atom {
        self.suspension(
            curios_anf::SuspensionOperation::New,
            vec![on, filled, cancel, rest],
        )
    }

    /// The performance of `Io/bind(action, continuation)`: force the action, then force what the continuation makes of its value in tail position — or, where the action suspended, answer that suspension with the continuation as its last frame.
    pub fn perform_bind(
        &mut self,
        action: curios_anf::Atom,
        continuation: curios_anf::Atom,
    ) -> curios_anf::Atom {
        let answer = self.forced(action);
        self.on_suspension(
            answer,
            |lowering| {
                lowering.suspension(
                    curios_anf::SuspensionOperation::Carry,
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
    pub fn perform_advance(
        &mut self,
        action: curios_anf::Atom,
        done: curios_anf::Atom,
        suspended: curios_anf::Atom,
    ) -> curios_anf::Atom {
        let answer = self.forced(action);
        self.on_suspension(
            answer,
            |lowering| {
                let resume = lowering.resumption();
                let [on, filled, cancel] = lowering.waiting(answer);
                let rest = lowering.description("io/rest", |lowering| {
                    lowering.applied(curios_anf::Atom::Function(resume), vec![answer])
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
    pub fn yield_to_host(&mut self, answer: curios_anf::Atom, polled: Polled) -> curios_anf::Atom {
        self.on_suspension(
            answer,
            |lowering| {
                let resume = lowering.resumption();
                let surface = lowering.surfacing(resume, polled);
                lowering.applied(curios_anf::Atom::Function(surface), vec![answer])
            },
            |_| answer,
        )
    }

    /// The family of `/std/Option` and its two constructors, which a cell's poll answers.
    pub fn polled(&mut self, context: &mut Retyping) -> Result<Polled, Error> {
        let syntax = context.syntax().option;
        Ok(Polled {
            family: self.induct_family(
                context,
                &curios_core::Global::Authored(syntax.family.qualifier()),
            )?,
            some: self.outcome_constructor(context, syntax.family, syntax.some)?,
            none: self.outcome_constructor(context, syntax.family, syntax.none)?,
        })
    }

    /// `answer`, or where it is a suspension, what `surface` makes of it.
    fn surfaced(
        &mut self,
        surface: curios_anf::FunctionId,
        answer: curios_anf::Atom,
    ) -> curios_anf::Atom {
        self.on_suspension(
            answer,
            |lowering| lowering.applied(curios_anf::Atom::Function(surface), vec![answer]),
            |_| answer,
        )
    }

    /// Whether a cell of `cells` is filled: a fold over them, polling each.
    fn any_filled(&mut self, cells: curios_anf::Atom, polled: Polled) -> curios_anf::Atom {
        let element = self.builder.value(None);
        let suffix = self.builder.value(None);
        let accumulator = self.builder.value(None);
        let empty = self.block(|lowering| lowering.boolean(false));
        let block = self.block(|lowering| {
            let poll = lowering.let_atom(curios_anf::Rhs::Cell {
                operation: curios_anf::CellOperation::Poll {
                    some: polled.some,
                    none: polled.none,
                },
                operands: vec![curios_anf::Atom::Value(element)],
            });
            let unfilled = lowering.block(|_| curios_anf::Atom::Value(accumulator));
            let filled = lowering.block(|lowering| lowering.boolean(true));
            lowering.let_atom(curios_anf::Rhs::MatchVariant {
                family: polled.family,
                scrutinee: poll,
                arms: vec![curios_anf::VariantArm {
                    constructor: polled.none,
                    bindings: Vec::new(),
                    block: unfilled,
                }],
                default: Some(filled),
            })
        });
        self.let_atom(curios_anf::Rhs::FoldSequence {
            grain: curios_anf::SequenceGrain::List,
            scrutinee: cells,
            empty,
            step: curios_anf::FoldSequenceStep {
                element,
                suffix,
                accumulator,
                block,
            },
        })
    }

    fn boolean(&mut self, value: bool) -> curios_anf::Atom {
        curios_anf::Atom::Constant(self.builder.constant(curios_anf::Constant::Bool(value)))
    }

    /// Define a description named `name` in the innermost open block, its performance what `body` computes, and hand it back.
    pub fn description(
        &mut self,
        name: &str,
        body: impl FnOnce(&mut Self) -> curios_anf::Atom,
    ) -> curios_anf::Atom {
        let function = self.builder.reserve_function();
        let block = self.block(body);
        self.builder
            .define_function(function, Some(name.to_string()), Vec::new(), block);
        self.builder.mark_description(function);
        self.builder.let_functions(vec![function]);
        curios_anf::Atom::Function(function)
    }

    /// Define the resume loop in the innermost open block and hand back `resume(suspension)`: run what the suspension holds first, then its frames through `frames(answer, pending)`, which applies each in turn and wraps those still pending around whatever suspends.
    fn resumption(&mut self) -> curios_anf::FunctionId {
        let resume = self.builder.reserve_function();
        let frames = self.builder.reserve_function();

        let suspension = self.builder.value(None);
        let held = curios_anf::Atom::Value(suspension);
        let body = self.block(|lowering| {
            let next = lowering.suspension(curios_anf::SuspensionOperation::Next, vec![held]);
            let test = lowering.suspension(curios_anf::SuspensionOperation::Is, vec![next]);
            let if_false = lowering.block(|lowering| lowering.forced(next));
            let if_true = lowering
                .block(|lowering| lowering.applied(curios_anf::Atom::Function(resume), vec![next]));
            let first = lowering.let_atom(curios_anf::Rhs::SwitchBool {
                scrutinee: test,
                if_false,
                if_true,
            });
            let pending = lowering.suspension(curios_anf::SuspensionOperation::Frames, vec![held]);
            lowering.applied(curios_anf::Atom::Function(frames), vec![first, pending])
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
            let answered = curios_anf::Atom::Value(answer);
            let empty = lowering.block(|_| answered);
            let frame = lowering.builder.value(None);
            let later = lowering.builder.value(None);
            let cons = lowering.block(|lowering| {
                lowering.on_suspension(
                    answered,
                    |lowering| {
                        let [on, filled, cancel] = lowering.waiting(answered);
                        lowering.suspension(
                            curios_anf::SuspensionOperation::Build,
                            vec![
                                on,
                                filled,
                                cancel,
                                answered,
                                curios_anf::Atom::Value(pending),
                            ],
                        )
                    },
                    |lowering| {
                        let next = lowering.applied(curios_anf::Atom::Value(frame), vec![answered]);
                        let resumed = lowering.forced(next);
                        lowering.applied(
                            curios_anf::Atom::Function(frames),
                            vec![resumed, curios_anf::Atom::Value(later)],
                        )
                    },
                )
            });
            lowering.let_atom(curios_anf::Rhs::UnconsSequence {
                grain: curios_anf::SequenceGrain::List,
                scrutinee: curios_anf::Atom::Value(pending),
                empty,
                cons: curios_anf::UnconsSequenceStep {
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
        resume: curios_anf::FunctionId,
        polled: Polled,
    ) -> curios_anf::FunctionId {
        let surface = self.builder.reserve_function();
        let suspension = self.builder.value(None);
        let held = curios_anf::Atom::Value(suspension);
        let body = self.block(|lowering| {
            let [on, filled, cancel] = lowering.waiting(held);
            let woken = lowering.any_filled(filled, polled);
            let waiting = lowering.block(|lowering| {
                let rest = lowering.description("io/rest", |lowering| {
                    let answer = lowering.applied(curios_anf::Atom::Function(resume), vec![held]);
                    lowering.surfaced(surface, answer)
                });
                let cancel = lowering.description("io/cancel", |lowering| {
                    let answer = lowering.forced(cancel);
                    lowering.surfaced(surface, answer)
                });
                let none = lowering.let_atom(curios_anf::Rhs::Sequence {
                    operation: curios_anf::SequenceOp::ListBuild,
                    operands: Vec::new(),
                });
                lowering.suspension(
                    curios_anf::SuspensionOperation::Build,
                    vec![on, filled, cancel, rest, none],
                )
            });
            let ready = lowering.block(|lowering| {
                let answer = lowering.applied(curios_anf::Atom::Function(resume), vec![held]);
                lowering.surfaced(surface, answer)
            });
            lowering.let_atom(curios_anf::Rhs::SwitchBool {
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
