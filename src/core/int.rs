use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Int {
    value: i32,
}

impl Int {
    const MIN: i64 = -(1 << 30);
    const MAX: i64 = (1 << 30) - 1;

    pub fn new(value: i64) -> Self {
        assert!(
            value >= Self::MIN && value <= Self::MAX,
            "Int arithmetic overflow"
        );
        Self {
            value: value as i32,
        }
    }

    pub fn to_i32(self) -> i32 {
        self.value
    }

    pub fn add(self, other: Self) -> Self {
        Self::new(self.value as i64 + other.value as i64)
    }

    pub fn sub(self, other: Self) -> Self {
        Self::new(self.value as i64 - other.value as i64)
    }

    pub fn mul(self, other: Self) -> Self {
        Self::new(self.value as i64 * other.value as i64)
    }

    pub fn div(self, other: Self) -> Self {
        Self::new(self.value as i64 / other.value as i64)
    }

    pub fn rem(self, other: Self) -> Self {
        Self::new(self.value as i64 % other.value as i64)
    }
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
