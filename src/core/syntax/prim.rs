use super::{Subterm, Term};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IntType;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FltType;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Prim {
    IntType,
    Int(i32),
    IntEql(Subterm, Subterm),
    IntAdd(Subterm, Subterm),
    IntSub(Subterm, Subterm),
    IntMul(Subterm, Subterm),
    FltType,
    Flt(u32),
    FltAdd(Subterm, Subterm),
    FltSub(Subterm, Subterm),
    FltMul(Subterm, Subterm),
}

impl Prim {
    pub fn int_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntEql(left.into().into(), right.into().into())
    }

    pub fn int_add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntAdd(left.into().into(), right.into().into())
    }

    pub fn int_sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntSub(left.into().into(), right.into().into())
    }

    pub fn int_mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntMul(left.into().into(), right.into().into())
    }

    pub fn flt_add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltAdd(left.into().into(), right.into().into())
    }

    pub fn flt_sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltSub(left.into().into(), right.into().into())
    }

    pub fn flt_mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltMul(left.into().into(), right.into().into())
    }
}

impl From<IntType> for Prim {
    fn from(IntType: IntType) -> Self {
        Self::IntType
    }
}

impl From<i32> for Prim {
    fn from(value: i32) -> Self {
        Self::Int(value)
    }
}

impl From<FltType> for Prim {
    fn from(FltType: FltType) -> Self {
        Self::FltType
    }
}

impl From<f32> for Prim {
    fn from(value: f32) -> Self {
        Self::Flt(value.to_bits())
    }
}
