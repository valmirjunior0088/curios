use {super::Work, crate::cont};

pub type Cont<'a> = Box<dyn FnOnce(&mut Work<'_, '_, '_>, cont::ValueName) -> cont::Tail + 'a>;

pub type ContMany<'a> =
    Box<dyn FnOnce(&mut Work<'_, '_, '_>, Vec<cont::ValueName>) -> cont::Tail + 'a>;

pub type RecBody<'a> = Box<dyn FnOnce(&mut Work<'_, '_, '_>, &super::Frame) -> cont::Tail + 'a>;
