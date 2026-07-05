pub(super) fn encode_uleb128_unsigned(mut number: u64) -> Vec<u8> {
    let mut bytes = Vec::new();

    loop {
        let byte = (number & 0x7f) as u8;
        let next = number >> 7;

        if next == 0 {
            bytes.push(byte);
            break;
        } else {
            bytes.push(byte | 0x80);
            number = next;
        }
    }

    bytes
}

pub(super) fn encode_leb128_signed(mut number: i64) -> Vec<u8> {
    let mut bytes = Vec::new();

    loop {
        let byte = (number & 0x7f) as u8;
        let sign = byte & 0x40;
        let next = number >> 7;

        if (next == 0 && sign == 0) || (next == -1 && sign != 0) {
            bytes.push(byte);
            break;
        } else {
            bytes.push(byte | 0x80);
            number = next;
        }
    }

    bytes
}

pub(super) fn encode_ieee754_single(number: f32) -> Vec<u8> {
    number.to_bits().to_le_bytes().to_vec()
}

pub(super) fn encode_ieee754_double(number: f64) -> Vec<u8> {
    number.to_bits().to_le_bytes().to_vec()
}

pub(super) fn encode_utf8(string: &str) -> Vec<u8> {
    string.as_bytes().to_vec()
}

pub(super) fn encode_rle<T, I>(values: I) -> Vec<(u64, T)>
where
    T: PartialEq,
    I: IntoIterator<Item = T>,
{
    let mut values = values.into_iter();
    let mut counts = Vec::new();

    let mut current = match values.next() {
        Some(current) => current,
        None => return counts,
    };

    let mut count = 1;

    for value in values {
        if current == value {
            count += 1;
        } else {
            counts.push((count, current));
            current = value;
            count = 1;
        }
    }

    counts.push((count, current));

    counts
}
