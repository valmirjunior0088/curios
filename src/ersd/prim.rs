use super::Subterm;

#[derive(Debug)]
pub enum Prim {
    Nat(u32),
    NatEql(Subterm, Subterm),
    NatAdd(Subterm, Subterm),
    NatSub(Subterm, Subterm),
    NatMul(Subterm, Subterm),
    NatLt(Subterm, Subterm),
    Int(i32),
    IntEql(Subterm, Subterm),
    IntAdd(Subterm, Subterm),
    IntSub(Subterm, Subterm),
    IntMul(Subterm, Subterm),
    Flt(f32),
    FltAdd(Subterm, Subterm),
    FltSub(Subterm, Subterm),
    FltMul(Subterm, Subterm),
}

impl From<u32> for Prim {
    fn from(value: u32) -> Self {
        Self::Nat(value)
    }
}

impl From<i32> for Prim {
    fn from(value: i32) -> Self {
        Self::Int(value)
    }
}

impl From<f32> for Prim {
    fn from(value: f32) -> Self {
        Self::Flt(value)
    }
}

