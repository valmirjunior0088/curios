use {
    super::{Frame, FrameEntropy, Lowerer, RegionBuilder},
    crate::cont,
};

pub type Cont<'a> = Box<
    dyn FnOnce(
            &mut Lowerer<'_>,
            &mut FrameEntropy,
            &mut RegionBuilder,
            cont::ValueName,
        ) -> cont::Tail
        + 'a,
>;

pub type ContMany<'a> = Box<
    dyn FnOnce(
            &mut Lowerer<'_>,
            &mut FrameEntropy,
            &mut RegionBuilder,
            Vec<cont::ValueName>,
        ) -> cont::Tail
        + 'a,
>;

pub type RecBody<'a> = Box<
    dyn FnOnce(&mut Lowerer<'_>, &Frame, &mut FrameEntropy, &mut RegionBuilder) -> cont::Tail + 'a,
>;
