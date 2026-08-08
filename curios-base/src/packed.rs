//! Shared packed storage for the two intrinsic binary-sequence grains.
//!
//! Values are immutable windows over a shared byte buffer. Bit zero is the least-significant bit of the first byte, so advancing a B tail is O(1).

use std::{hash::Hash, sync::Arc};

/// The closed logical grain of a packed binary sequence.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum Grain {
    /// One logical atom per bit.
    B,
    /// One logical atom per byte.
    X,
}

impl Grain {
    pub const fn bits(self) -> usize {
        match self {
            Self::B => 1,
            Self::X => 8,
        }
    }
}

/// An immutable logical bit window over shared packed bytes.
#[derive(Debug, Clone)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct PackedBin {
    bytes: Arc<[u8]>,
    bit_offset: usize,
    bit_length: usize,
}

impl PackedBin {
    pub fn empty() -> Self {
        Self::from_bytes(Vec::new())
    }

    pub fn from_bytes(bytes: impl Into<Arc<[u8]>>) -> Self {
        let bytes = bytes.into();
        let bit_length = bytes.len() * 8;
        Self {
            bytes,
            bit_offset: 0,
            bit_length,
        }
    }

    pub fn from_bits(bits: impl IntoIterator<Item = bool>) -> Self {
        let bits: Vec<_> = bits.into_iter().collect();
        let mut bytes = vec![0; bits.len().div_ceil(8)];
        for (index, bit) in bits.iter().copied().enumerate() {
            if bit {
                bytes[index / 8] |= 1 << (index % 8);
            }
        }
        Self {
            bytes: bytes.into(),
            bit_offset: 0,
            bit_length: bits.len(),
        }
    }

    pub fn window(&self, bit_offset: usize, bit_length: usize) -> Option<Self> {
        (bit_offset <= self.bit_length && bit_length <= self.bit_length - bit_offset).then(|| {
            Self {
                bytes: self.bytes.clone(),
                bit_offset: self.bit_offset + bit_offset,
                bit_length,
            }
        })
    }

    pub fn bit_length(&self) -> usize {
        self.bit_length
    }
    pub fn bit_offset(&self) -> usize {
        self.bit_offset
    }
    pub fn is_empty(&self) -> bool {
        self.bit_length == 0
    }
    pub fn len(&self, grain: Grain) -> usize {
        debug_assert!(grain == Grain::B || self.is_x_aligned());
        self.bit_length / grain.bits()
    }
    pub fn is_x_aligned(&self) -> bool {
        self.bit_offset.is_multiple_of(8) && self.bit_length.is_multiple_of(8)
    }
    pub fn bit(&self, index: usize) -> Option<bool> {
        (index < self.bit_length).then(|| {
            let index = self.bit_offset + index;
            self.bytes[index / 8] & (1 << (index % 8)) != 0
        })
    }
    pub fn byte(&self, index: usize) -> Option<u8> {
        if !self.is_x_aligned() || index >= self.bit_length / 8 {
            return None;
        }
        self.bytes.get(self.bit_offset / 8 + index).copied()
    }
    pub fn to_packed_bytes(&self) -> Vec<u8> {
        // An aligned window has no padding bits to mask, so its stored bytes already are the packed form.
        if let Some(bytes) = self.as_bytes() {
            return bytes.to_vec();
        }
        let mut out = vec![0; self.bit_length.div_ceil(8)];
        for index in 0..self.bit_length {
            if self.bit(index).unwrap() {
                out[index / 8] |= 1 << (index % 8);
            }
        }
        out
    }
    pub fn to_bytes(&self) -> Option<Vec<u8>> {
        self.is_x_aligned().then(|| {
            self.bytes[self.bit_offset / 8..(self.bit_offset + self.bit_length) / 8].to_vec()
        })
    }
    pub fn as_bytes(&self) -> Option<&[u8]> {
        self.is_x_aligned()
            .then(|| &self.bytes[self.bit_offset / 8..(self.bit_offset + self.bit_length) / 8])
    }

    pub fn slice(&self, grain: Grain, start: usize, end: usize) -> Option<Self> {
        (start <= end && end <= self.len(grain)).then(|| {
            self.window(start * grain.bits(), (end - start) * grain.bits())
                .unwrap()
        })
    }

    pub fn concat<'a>(values: impl IntoIterator<Item = &'a Self>) -> Self {
        let values: Vec<_> = values.into_iter().collect();
        // Aligned operands (every byte string) concatenate by bulk byte copy; only unaligned bit windows need the per-bit repack.
        if values.iter().all(|value| value.is_x_aligned()) {
            let mut bytes =
                Vec::with_capacity(values.iter().map(|value| value.bit_length / 8).sum());
            for value in &values {
                bytes.extend_from_slice(value.as_bytes().unwrap());
            }
            return Self::from_bytes(bytes);
        }
        Self::from_bits(
            values
                .into_iter()
                .flat_map(|value| (0..value.bit_length).map(|index| value.bit(index).unwrap())),
        )
    }

    pub fn append_bit(&self, bit: bool) -> Self {
        Self::from_bits(
            (0..self.bit_length)
                .map(|index| self.bit(index).unwrap())
                .chain([bit]),
        )
    }

    pub fn append_byte(&self, byte: u8) -> Option<Self> {
        self.is_x_aligned().then(|| {
            let mut bytes = self.to_bytes().unwrap();
            bytes.push(byte);
            Self::from_bytes(bytes)
        })
    }
}

impl PartialEq for PackedBin {
    fn eq(&self, other: &Self) -> bool {
        self.bit_length == other.bit_length
            && (0..self.bit_length).all(|index| self.bit(index) == other.bit(index))
    }
}
impl Eq for PackedBin {}
impl Hash for PackedBin {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.bit_length.hash(state);
        // `Vec<u8>` hashes as its slice, so both arms feed the hasher the same logical packed bytes — the aligned arm just skips the allocation.
        match self.as_bytes() {
            Some(bytes) => bytes.hash(state),
            None => self.to_packed_bytes().hash(state),
        }
    }
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        std::{collections::hash_map::DefaultHasher, hash::Hasher},
    };

    fn hash(value: &PackedBin) -> u64 {
        let mut h = DefaultHasher::new();
        value.hash(&mut h);
        h.finish()
    }

    #[test]
    fn windows_compare_logically_and_ignore_padding() {
        for length in 0..=12 {
            for mask in 0..(1usize << length) {
                let bits = (0..length)
                    .map(|i| mask & (1 << i) != 0)
                    .collect::<Vec<_>>();
                let direct = PackedBin::from_bits(bits.iter().copied());
                let framed = PackedBin::from_bits(
                    [true, false, true]
                        .into_iter()
                        .chain(bits.iter().copied())
                        .chain([true, true]),
                );
                let window = framed.window(3, length).unwrap();
                assert_eq!(direct, window);
                assert_eq!(hash(&direct), hash(&window));
            }
        }
    }

    #[test]
    fn first_written_bit_is_least_significant() {
        assert_eq!(
            PackedBin::from_bits([true, false, true]).to_packed_bytes(),
            vec![5]
        );
    }

    #[test]
    fn exhaustive_short_bit_operations_match_vec_model() {
        for length in 0..=10 {
            for mask in 0..(1usize << length) {
                let model = (0..length)
                    .map(|index| mask & (1 << index) != 0)
                    .collect::<Vec<_>>();
                let value = PackedBin::from_bits(model.iter().copied());

                assert_eq!(value.len(Grain::B), model.len());
                for index in 0..=length {
                    assert_eq!(value.bit(index), model.get(index).copied());
                }
                for start in 0..=length {
                    for end in start..=length {
                        let slice = value.slice(Grain::B, start, end).unwrap();
                        let expected = PackedBin::from_bits(model[start..end].iter().copied());
                        assert_eq!(slice, expected);
                        assert_eq!(hash(&slice), hash(&expected));
                    }
                }
                for bit in [false, true] {
                    let mut expected = model.clone();
                    expected.push(bit);
                    assert_eq!(
                        value.append_bit(bit),
                        PackedBin::from_bits(expected.into_iter())
                    );
                }
            }
        }
    }

    #[test]
    fn byte_operations_preserve_alignment_and_match_vec_model() {
        let models = [vec![], vec![0], vec![0xff], vec![1, 2, 3, 4, 5, 6, 7, 8, 9]];
        for model in models {
            let value = PackedBin::from_bytes(model.clone());
            assert!(value.is_x_aligned());
            assert_eq!(value.len(Grain::X), model.len());
            assert_eq!(value.to_bytes().unwrap(), model);
            for start in 0..=model.len() {
                for end in start..=model.len() {
                    let slice = value.slice(Grain::X, start, end).unwrap();
                    assert!(slice.is_x_aligned());
                    assert_eq!(slice.to_bytes().unwrap(), model[start..end]);
                }
            }
            for byte in [0, 1, 0x7f, 0xff] {
                let mut expected = model.clone();
                expected.push(byte);
                assert_eq!(
                    value.append_byte(byte).unwrap().to_bytes().unwrap(),
                    expected
                );
            }
        }

        let unaligned = PackedBin::from_bits([true, false, true]);
        assert!(!unaligned.is_x_aligned());
        assert!(unaligned.append_byte(0).is_none());
        assert!(unaligned.to_bytes().is_none());
    }

    #[test]
    fn aligned_concat_matches_the_bit_path_and_stays_aligned() {
        let left = PackedBin::from_bytes(vec![1, 2, 3]);
        let right = PackedBin::from_bytes(vec![4, 5]);
        let unaligned_twin = PackedBin::from_bits(
            [false; 3]
                .into_iter()
                .chain((0..right.bit_length()).map(|index| right.bit(index).unwrap())),
        )
        .window(3, right.bit_length())
        .unwrap();

        let fast = PackedBin::concat([&left, &right]);
        let slow = PackedBin::concat([&left, &unaligned_twin]);
        assert!(fast.is_x_aligned());
        assert_eq!(fast, slow);
        assert_eq!(hash(&fast), hash(&slow));
        assert_eq!(fast.to_bytes().unwrap(), vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn concat_crosses_byte_boundaries_without_exposing_padding() {
        let left = PackedBin::from_bits([true, false, true, true, false]);
        let middle = PackedBin::from_bits([false, true, true, false, true, false]);
        let right = PackedBin::from_bits([true, true, false]);
        let actual = PackedBin::concat([&left, &middle, &right]);
        let expected = PackedBin::from_bits([
            true, false, true, true, false, false, true, true, false, true, false, true, true,
            false,
        ]);
        assert_eq!(actual, expected);
        assert_eq!(actual.bit_length(), 14);
        assert_eq!(actual.to_packed_bytes().last().unwrap() & 0b1100_0000, 0);
    }
}
