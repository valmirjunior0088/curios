pub trait Arity {
    type Params<'a, T: ?Sized + 'a>: AsRef<[&'a T]>;

    fn arity(&self) -> usize;
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct One;

impl Arity for One {
    type Params<'a, T: ?Sized + 'a> = [&'a T; 1];

    fn arity(&self) -> usize {
        1
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Two;

impl Arity for Two {
    type Params<'a, T: ?Sized + 'a> = [&'a T; 2];

    fn arity(&self) -> usize {
        2
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Many(pub usize);

impl Arity for Many {
    type Params<'a, T: ?Sized + 'a> = &'a [&'a T];

    fn arity(&self) -> usize {
        self.0
    }
}
