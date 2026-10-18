//! The packed `Binary` a `Bytes` value is, beside the numbers every constant folder shares.
//!
//! Values are immutable windows over a shared byte buffer, their offset and length counted in bytes.

use {
    crate::BufferBytes,
    std::{hash::Hash, sync::Arc},
};

/// An immutable window over shared bytes.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct Binary {
    #[archived_with(BufferBytes)]
    bytes: Arc<[u8]>,
    offset: usize,
    length: usize,
}

impl Binary {
    pub fn empty() -> Self {
        Self::from_bytes(Vec::new())
    }

    pub fn from_bytes(bytes: impl Into<Arc<[u8]>>) -> Self {
        let bytes = bytes.into();
        let length = bytes.len();
        Self {
            bytes,
            offset: 0,
            length,
        }
    }

    /// `count` copies of one byte.
    pub fn replicate(atom: u8, count: usize) -> Self {
        Self::from_bytes(vec![atom; count])
    }

    /// The `length` bytes from `offset`, over this value's buffer.
    pub fn window(&self, offset: usize, length: usize) -> Option<Self> {
        (offset <= self.length && length <= self.length - offset).then(|| Self {
            bytes: self.bytes.clone(),
            offset: self.offset + offset,
            length,
        })
    }

    pub fn len(&self) -> usize {
        self.length
    }
    pub fn offset(&self) -> usize {
        self.offset
    }
    pub fn is_empty(&self) -> bool {
        self.length == 0
    }
    pub fn byte(&self, index: usize) -> Option<u8> {
        self.as_bytes().get(index).copied()
    }
    pub fn as_bytes(&self) -> &[u8] {
        &self.bytes[self.offset..self.offset + self.length]
    }
    pub fn to_bytes(&self) -> Vec<u8> {
        self.as_bytes().to_vec()
    }

    pub fn slice(&self, start: usize, end: usize) -> Option<Self> {
        (start <= end && end <= self.length).then(|| self.window(start, end - start).unwrap())
    }

    pub fn concat<'a>(values: impl IntoIterator<Item = &'a Self>) -> Self {
        let values: Vec<_> = values.into_iter().collect();
        let mut bytes = Vec::with_capacity(values.iter().map(|value| value.length).sum());
        for value in values {
            bytes.extend_from_slice(value.as_bytes());
        }
        Self::from_bytes(bytes)
    }

    /// The pointwise conjunction of two runs of one length.
    pub fn and(&self, other: &Self) -> Self {
        self.pointwise(other, |left, right| left & right)
    }

    /// The pointwise disjunction.
    pub fn or(&self, other: &Self) -> Self {
        self.pointwise(other, |left, right| left | right)
    }

    /// The pointwise difference.
    pub fn xor(&self, other: &Self) -> Self {
        self.pointwise(other, |left, right| left ^ right)
    }

    /// Two runs combined a byte at a time.
    ///
    /// Equal lengths are the caller's to establish rather than this function's to accommodate: `/sys` states the bound in the type and every reduction reaches here through it, so a mismatch is a broken invariant, not an input. Asserted rather than debug-asserted, because a pointwise result over two lengths is not a near miss but a run that is neither operand's, and it should say so where it breaks rather than downstream of a wrong length.
    fn pointwise(&self, other: &Self, combine: impl Fn(u8, u8) -> u8) -> Self {
        assert_eq!(
            self.length, other.length,
            "a pointwise operation needs two runs of one length"
        );

        Self::from_bytes(
            self.as_bytes()
                .iter()
                .zip(other.as_bytes())
                .map(|(left, right)| combine(*left, *right))
                .collect::<Vec<_>>(),
        )
    }

    pub fn append_byte(&self, byte: u8) -> Self {
        let mut bytes = self.to_bytes();
        bytes.push(byte);
        Self::from_bytes(bytes)
    }
}

impl PartialEq for Binary {
    /// Equal bytes, decided as cheaply as the representation allows.
    ///
    /// **Two windows of one buffer at one offset are the same bytes**, so they are equal without a read — and that case is the one the closed machine asks about once per element of a packed walk: every step's tail is a window of the literal's buffer, and the run-scoped memo that keeps the walk linear probes a key holding it, which finds the key it stored and then has to confirm the two are equal. Confirming it byte by byte makes a `Str` literal's check quadratic in wall clock where every counter says linear, and `curios`' `str_literal_cost_measurements` carries the ladder.
    fn eq(&self, other: &Self) -> bool {
        if self.length != other.length {
            return false;
        }
        if Arc::ptr_eq(&self.bytes, &other.bytes) && self.offset == other.offset {
            return true;
        }
        self.as_bytes() == other.as_bytes()
    }
}
impl Eq for Binary {}
/// How many bytes [`Binary`]'s hash reads from a value larger than that. Enough that two values of one length agreeing at all of them are not a shape a program writes by accident, and small enough that hashing is a constant.
const HASH_SAMPLE_BYTES: usize = 32;

impl Hash for Binary {
    /// **Constant-cost, which is what keeps a walk over a literal linear.** A hash is memoized per term node, but *computing* one is not free: a node is hashed when it is built, and peeling a literal builds one node per element, each holding a window one element shorter than the last. Reading every byte of each made that walk quadratic — the shape [`Binary::eq`] records on its own side of the same probe, where a window of one buffer is equal to itself without a read. A cache probe cannot take that shortcut, because a window and a directly-built value of the same bytes are equal and must hash alike, so what this does instead is read a bounded sample.
    ///
    /// The length is hashed whole and at most `HASH_SAMPLE_BYTES` bytes are read, spread across the run with both ends included. Two values of one length that agree at every sampled byte collide, and a collision costs a comparison rather than an answer: [`Binary::eq`] decides the pair, as it decides every probe that reaches it. Where the hash is an *order* rather than a key — `Nat::multiply` sorts a monomial's factors by the structural hash their term carries — a collision leaves two factors in their written order under a stable sort, which is incompleteness and never a wrong equation, exactly as that sort already records for two distinct factors hashing alike.
    ///
    /// Allocation-free, which is what lets a cache probe stay a probe: a lookup that allocated would have to be fallible under a budget that charges construction.
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.length.hash(state);

        let bytes = self.as_bytes();
        match bytes.len() <= HASH_SAMPLE_BYTES {
            true => {
                for &byte in bytes {
                    state.write_u8(byte);
                }
            }
            // Spread over the run with both ends included, so a value that differs from another only in its first or last byte is still told apart by the hash rather than by the comparison behind it.
            false => {
                for step in 0..HASH_SAMPLE_BYTES {
                    state.write_u8(bytes[step * (bytes.len() - 1) / (HASH_SAMPLE_BYTES - 1)]);
                }
            }
        }
    }
}

impl PartialOrd for Binary {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Binary {
    /// Bytewise with the shorter prefix first, which is `/std/Bytes/cmp`: a byte *is* the element, so the agreement falls out rather than being engineered. Allocation-free for [`Binary::hash`]'s reason — this is what an optimizer's key sort walks.
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_bytes().cmp(other.as_bytes())
    }
}

#[cfg(test)]
mod tests;
