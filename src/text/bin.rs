#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinLiteral {
    Bytes(Vec<u8>),
    String(String),
}

impl BinLiteral {
    pub fn string(string: impl Into<String>) -> Self {
        BinLiteral::String(string.into())
    }

    pub fn bytes(bytes: impl Into<Vec<u8>>) -> Self {
        BinLiteral::Bytes(bytes.into())
    }
}
