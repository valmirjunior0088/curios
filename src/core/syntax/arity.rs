pub trait Arity: Copy {
    type Params<'a, T: ?Sized + 'a>: AsRef<[&'a T]>;

    fn arity(&self) -> usize;
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct One;

impl One {
    pub const ARITY: usize = 1;
}

impl Arity for One {
    type Params<'a, T: ?Sized + 'a> = &'a [&'a T; Self::ARITY];

    fn arity(&self) -> usize {
        Self::ARITY
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Two;

impl Two {
    pub const ARITY: usize = 2;
}

impl Arity for Two {
    type Params<'a, T: ?Sized + 'a> = &'a [&'a T; Self::ARITY];

    fn arity(&self) -> usize {
        Self::ARITY
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Many(pub usize);

impl Arity for Many {
    type Params<'a, T: ?Sized + 'a> = &'a [&'a T];

    fn arity(&self) -> usize {
        self.0
    }
}
