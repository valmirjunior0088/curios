use std::{
    fmt,
    ops::{Add, Div, Mul, Rem, Sub},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Int {
    value: i32,
}

impl Int {
    const MIN: i64 = -(1 << 30);
    const MAX: i64 = (1 << 30) - 1;

    pub fn new(value: i64) -> Self {
        assert!(
            (Self::MIN..=Self::MAX).contains(&value),
            "Int arithmetic overflow"
        );
        Self {
            value: value as i32,
        }
    }

    pub fn to_i32(self) -> i32 {
        self.value
    }
}

impl Add for Int {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self::new(self.value as i64 + other.value as i64)
    }
}

impl Sub for Int {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self::new(self.value as i64 - other.value as i64)
    }
}

impl Mul for Int {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        Self::new(self.value as i64 * other.value as i64)
    }
}

impl Div for Int {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        Self::new(self.value as i64 / other.value as i64)
    }
}

impl Rem for Int {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        Self::new(self.value as i64 % other.value as i64)
    }
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
