use {
    super::{Prim, Subterm, Term},
    num_bigint::BigUint,
    num_traits::Zero,
    std::fmt,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Nat {
    Zero,
    Succ(BigUint, Term),
}

impl Nat {
    pub fn new(value: impl Into<BigUint>) -> Self {
        let value = value.into();

        if value.is_zero() {
            Nat::Zero
        } else {
            Nat::Succ(value, Subterm::Prim(Prim::Nat(Nat::Zero)).into())
        }
    }

    pub fn to_big_uint(&self) -> Option<BigUint> {
        match self {
            Nat::Zero => Some(BigUint::zero()),
            Nat::Succ(spine, inner) => match inner.as_ref() {
                Subterm::Prim(Prim::Nat(Nat::Zero)) => Some(spine.clone()),
                _ => None,
            },
        }
    }

    pub fn checked_add(self, other: Self) -> Option<Self> {
        Some(Self::new(self.to_big_uint()? + other.to_big_uint()?))
    }

    pub fn checked_sub(self, other: Self) -> Option<Self> {
        let left = self.to_big_uint()?;
        let right = other.to_big_uint()?;

        Some(Self::new(if left >= right {
            left - right
        } else {
            BigUint::zero()
        }))
    }

    pub fn checked_mul(self, other: Self) -> Option<Self> {
        Some(Self::new(self.to_big_uint()? * other.to_big_uint()?))
    }

    /// `None` on a symbolic operand *or* a zero divisor — never a panic; the
    /// reducer reports the zero-divisor case before folding.
    pub fn checked_div(self, other: Self) -> Option<Self> {
        let left = self.to_big_uint()?;
        let right = other.to_big_uint()?;

        (!right.is_zero()).then(|| Self::new(left / right))
    }

    /// `None` on a symbolic operand or a zero divisor, like
    /// [`Nat::checked_div`].
    pub fn checked_rem(self, other: Self) -> Option<Self> {
        let left = self.to_big_uint()?;
        let right = other.to_big_uint()?;

        (!right.is_zero()).then(|| Self::new(left % right))
    }

    pub fn eql(&self, other: &Self) -> Option<bool> {
        Some(self.to_big_uint()? == other.to_big_uint()?)
    }

    pub fn lt(&self, other: &Self) -> Option<bool> {
        Some(self.to_big_uint()? < other.to_big_uint()?)
    }

    pub fn lte(&self, other: &Self) -> Option<bool> {
        Some(self.to_big_uint()? <= other.to_big_uint()?)
    }

    pub fn gt(&self, other: &Self) -> Option<bool> {
        Some(self.to_big_uint()? > other.to_big_uint()?)
    }

    pub fn gte(&self, other: &Self) -> Option<bool> {
        Some(self.to_big_uint()? >= other.to_big_uint()?)
    }
}

impl fmt::Display for Nat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Nat::Zero => write!(f, "0"),
            Nat::Succ(spine, _) => write!(f, "{spine}"),
        }
    }
}
