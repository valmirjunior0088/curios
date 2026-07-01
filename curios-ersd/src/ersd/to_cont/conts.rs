use {super::Work, curios_cont::cont};

type ContFn<'a> = Box<dyn FnOnce(&mut Work<'_, '_, '_>, cont::ValueName) -> cont::Tail + 'a>;

pub struct Cont<'a> {
    func: ContFn<'a>,
}

impl<'a> Cont<'a> {
    pub fn new(
        func: impl FnOnce(&mut Work<'_, '_, '_>, cont::ValueName) -> cont::Tail + 'a,
    ) -> Self {
        Self {
            func: Box::new(func),
        }
    }

    pub fn call(self, work: &mut Work<'_, '_, '_>, value: cont::ValueName) -> cont::Tail {
        (self.func)(work, value)
    }

    pub fn jump_to(resume: cont::BlockName) -> Self {
        Self::new(move |_, value| {
            cont::Tail::Jump(cont::JumpTarget {
                target: resume,
                params: vec![value],
            })
        })
    }
}

type ContManyFn<'a> =
    Box<dyn FnOnce(&mut Work<'_, '_, '_>, Vec<cont::ValueName>) -> cont::Tail + 'a>;

pub struct ContMany<'a> {
    func: ContManyFn<'a>,
}

impl<'a> ContMany<'a> {
    pub fn new(
        func: impl FnOnce(&mut Work<'_, '_, '_>, Vec<cont::ValueName>) -> cont::Tail + 'a,
    ) -> Self {
        Self {
            func: Box::new(func),
        }
    }

    pub fn call(self, work: &mut Work<'_, '_, '_>, values: Vec<cont::ValueName>) -> cont::Tail {
        (self.func)(work, values)
    }

    pub fn call_indirect(target: cont::ValueName, resume: cont::BlockName) -> Self {
        Self::new(move |_, params| {
            cont::Tail::Call(cont::CallTarget::Indirect {
                target,
                params,
                resume,
            })
        })
    }
}

type RecBodyFn<'a> = Box<dyn FnOnce(&mut Work<'_, '_, '_>, &super::Frame) -> cont::Tail + 'a>;

pub struct RecBody<'a> {
    func: RecBodyFn<'a>,
}

impl<'a> RecBody<'a> {
    pub fn new(func: impl FnOnce(&mut Work<'_, '_, '_>, &super::Frame) -> cont::Tail + 'a) -> Self {
        Self {
            func: Box::new(func),
        }
    }

    pub fn call(self, work: &mut Work<'_, '_, '_>, frame: &super::Frame) -> cont::Tail {
        (self.func)(work, frame)
    }
}
