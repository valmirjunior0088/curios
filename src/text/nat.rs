use {super::Term, num_bigint::BigUint};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NatLiteral {
    Number(BigUint),
    Char(char),
}

impl NatLiteral {
    pub fn number(n: impl Into<BigUint>) -> Self {
        NatLiteral::Number(n.into())
    }
}

impl From<char> for NatLiteral {
    fn from(c: char) -> Self {
        NatLiteral::Char(c)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Nat {
    Zero,
    Succ(NatLiteral, Term),
}
