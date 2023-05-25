use std::io::{Bytes, Error, ErrorKind, Read};

#[derive(Debug)]
pub struct Reader<R: Read> {
    input: Bytes<R>,
}

impl<R: Read> Reader<R> {
    pub fn new(input: R) -> Self {
        Self { input: input.bytes() }
    }
}

impl<R: Read> Iterator for Reader<R> {
    type Item = Result<char, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let byte = match self.input.next() {
            Some(Ok(byte)) => byte,
            Some(Err(error)) => return Some(Err(error)),
            None => return None,
        };

        if (byte & 0b10000000) == 0b00000000 {
            return Some(Ok(byte as char));
        }

        let remaining_bytes = match byte {
            byte if (byte & 0b11110000) == 0b11110000 => 3,
            byte if (byte & 0b11100000) == 0b11100000 => 2,
            byte if (byte & 0b11000000) == 0b11000000 => 1,
            _ => return Some(Err(Error::new(ErrorKind::InvalidData, ""))),
        };

        let trailing_bits = 0b11111111 >> (6 - remaining_bytes);
        let mut code_point = (byte & trailing_bits) as u32;

        for _ in 0..remaining_bytes {
            let byte = match self.input.next() {
                Some(Ok(byte)) => byte,
                Some(Err(error)) => return Some(Err(error)),
                None => return Some(Err(Error::new(ErrorKind::UnexpectedEof, ""))),
            };

            if (byte & 0b11000000) != 0b10000000 {
                return Some(Err(Error::new(ErrorKind::InvalidData, "")));
            }

            code_point = (code_point << 6) | (byte & 0b00111111) as u32;
        }

        match char::from_u32(code_point) {
            Some(character) => Some(Ok(character)),
            None => Some(Err(Error::new(ErrorKind::InvalidData, ""))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let expected = "Je tourne en rond\nJe m'endors et je rêve\nおはようございます";
        let output = Reader::new(expected.as_bytes()).collect::<Result<String, _>>();
        assert_eq!(output.unwrap(), expected);
    }
}
