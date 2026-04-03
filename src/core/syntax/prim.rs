use super::{Subterm, Term};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IntType;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IntPrim {
    Type,
    Value(i32),
    Eql(Subterm, Subterm),
    Add(Subterm, Subterm),
    Sub(Subterm, Subterm),
    Mul(Subterm, Subterm),
}

impl IntPrim {
    pub fn eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::Eql(left.into().into(), right.into().into())
    }

    pub fn add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::Add(left.into().into(), right.into().into())
    }

    pub fn sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::Sub(left.into().into(), right.into().into())
    }

    pub fn mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::Mul(left.into().into(), right.into().into())
    }
}

impl From<IntType> for IntPrim {
    fn from(IntType: IntType) -> Self {
        Self::Type
    }
}

impl From<i32> for IntPrim {
    fn from(value: i32) -> Self {
        Self::Value(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FltType;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FltPrim {
    Type,
    Value(u32),
    Add(Subterm, Subterm),
    Sub(Subterm, Subterm),
    Mul(Subterm, Subterm),
}

impl FltPrim {
    pub fn add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::Add(left.into().into(), right.into().into())
    }

    pub fn sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::Sub(left.into().into(), right.into().into())
    }

    pub fn mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::Mul(left.into().into(), right.into().into())
    }
}

impl From<FltType> for FltPrim {
    fn from(FltType: FltType) -> Self {
        Self::Type
    }
}

impl From<f32> for FltPrim {
    fn from(value: f32) -> Self {
        Self::Value(value.to_bits())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Prim {
    Int(IntPrim),
    Flt(FltPrim),
}

impl From<IntPrim> for Prim {
    fn from(value: IntPrim) -> Self {
        Self::Int(value)
    }
}

impl From<IntType> for Prim {
    fn from(value: IntType) -> Self {
        IntPrim::from(value).into()
    }
}

impl From<i32> for Prim {
    fn from(value: i32) -> Self {
        IntPrim::from(value).into()
    }
}

impl From<FltPrim> for Prim {
    fn from(value: FltPrim) -> Self {
        Self::Flt(value)
    }
}

impl From<FltType> for Prim {
    fn from(value: FltType) -> Self {
        FltPrim::from(value).into()
    }
}

impl From<f32> for Prim {
    fn from(value: f32) -> Self {
        FltPrim::from(value).into()
    }
}
