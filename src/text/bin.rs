#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinLiteral {
    Bytes(Vec<u8>),
    String(String),
}

impl<'a> From<&'a str> for BinLiteral {
    fn from(s: &'a str) -> Self {
        BinLiteral::String(s.to_string())
    }
}

impl<'a> From<&'a [u8]> for BinLiteral {
    fn from(bytes: &'a [u8]) -> Self {
        BinLiteral::Bytes(bytes.to_vec())
    }
}
