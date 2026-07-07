use {
    super::{LowerResult, Work},
    curios_cont::{BlockName, CallTarget, JumpTarget, Tail, ValueName},
};

type ContFn<'a> = Box<dyn FnOnce(&mut Work<'_, '_, '_>, ValueName) -> LowerResult<Tail> + 'a>;

pub(super) struct Cont<'a> {
    func: ContFn<'a>,
}

impl<'a> Cont<'a> {
    pub(super) fn new(
        func: impl FnOnce(&mut Work<'_, '_, '_>, ValueName) -> LowerResult<Tail> + 'a,
    ) -> Self {
        Self {
            func: Box::new(func),
        }
    }

    pub(super) fn call(self, work: &mut Work<'_, '_, '_>, value: ValueName) -> LowerResult<Tail> {
        (self.func)(work, value)
    }

    pub(super) fn jump_to(resume: BlockName) -> Self {
        Self::new(move |_, value| {
            Ok(Tail::Jump(JumpTarget {
                target: resume,
                params: vec![value],
            }))
        })
    }
}

type ContManyFn<'a> = Box<dyn FnOnce(&mut Work<'_, '_, '_>, Vec<ValueName>) -> LowerResult<Tail> + 'a>;

pub(super) struct ContMany<'a> {
    func: ContManyFn<'a>,
}

impl<'a> ContMany<'a> {
    pub(super) fn new(
        func: impl FnOnce(&mut Work<'_, '_, '_>, Vec<ValueName>) -> LowerResult<Tail> + 'a,
    ) -> Self {
        Self {
            func: Box::new(func),
        }
    }

    pub(super) fn call(self, work: &mut Work<'_, '_, '_>, values: Vec<ValueName>) -> LowerResult<Tail> {
        (self.func)(work, values)
    }

    pub(super) fn call_indirect(target: ValueName, resume: BlockName) -> Self {
        Self::new(move |_, params| {
            Ok(Tail::Call(CallTarget::Indirect {
                target,
                params,
                resume,
            }))
        })
    }
}

type RecBodyFn<'a> = Box<dyn FnOnce(&mut Work<'_, '_, '_>, &super::Frame) -> LowerResult<Tail> + 'a>;

pub(super) struct RecBody<'a> {
    func: RecBodyFn<'a>,
}

impl<'a> RecBody<'a> {
    pub(super) fn new(
        func: impl FnOnce(&mut Work<'_, '_, '_>, &super::Frame) -> LowerResult<Tail> + 'a,
    ) -> Self {
        Self {
            func: Box::new(func),
        }
    }

    pub(super) fn call(self, work: &mut Work<'_, '_, '_>, frame: &super::Frame) -> LowerResult<Tail> {
        (self.func)(work, frame)
    }
}
