use super::{Subterm, Term};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Prim {
    IntType,
    IntValue(i32),
    IntEql(Subterm, Subterm),
    IntAdd(Subterm, Subterm),
    IntSub(Subterm, Subterm),
    IntMul(Subterm, Subterm),
    FltType,
    FltValue(u32),
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
