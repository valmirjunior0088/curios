use {
    super::{encode_ieee754_double, encode_ieee754_single, encode_leb128_signed, encode_uleb128_unsigned},
    std::io::{Result, Write},
};

#[derive(Debug)]
pub struct Buffer<'w, W> {
    writer: &'w mut W,
}

impl<'w, W> Buffer<'w, W>
where
    W: Write,
{
    pub fn new(writer: &'w mut W) -> Self {
        Self { writer }
    }

    pub fn push_byte(&mut self, byte: u8) -> Result<()> {
        self.writer.write_all(&[byte])?;

        Ok(())
    }

    pub fn push_bytes(&mut self, bytes: &[u8]) -> Result<()> {
        self.writer.write_all(bytes)?;

        Ok(())
    }

    pub fn push_leb128_unsigned(&mut self, number: u64) -> Result<()> {
        self.push_bytes(&encode_uleb128_unsigned(number))?;

        Ok(())
    }

    pub fn push_leb128_signed(&mut self, number: i64) -> Result<()> {
        self.push_bytes(&encode_leb128_signed(number))?;

        Ok(())
    }

    pub fn push_ieee754_single(&mut self, number: f32) -> Result<()> {
        self.push_bytes(&encode_ieee754_single(number))?;

        Ok(())
    }

    pub fn push_ieee754_double(&mut self, number: f64) -> Result<()> {
        self.push_bytes(&encode_ieee754_double(number))?;

        Ok(())
    }

    pub fn push_vec_bytes(&mut self, bytes: &[u8]) -> Result<()> {
        self.push_leb128_unsigned(bytes.len() as u64)?;
        self.push_bytes(bytes)?;

        Ok(())
    }
}
