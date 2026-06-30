use {super::Term, num_bigint::BigUint};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Radix {
    Dec,
    Hex,
    Bin,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NatLiteral {
    Number(BigUint, Radix),
    Char(char),
}

impl NatLiteral {
    pub fn number(n: impl Into<BigUint>) -> Self {
        NatLiteral::Number(n.into(), Radix::Dec)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Nat {
    Zero,
    Succ(NatLiteral, Term),
}
