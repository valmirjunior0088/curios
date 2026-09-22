use {
    super::*,
    std::{collections::hash_map::DefaultHasher, hash::Hasher},
};

fn hash(value: &Binary) -> u64 {
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
            let direct = Binary::from_bits(bits.iter().copied());
            let framed = Binary::from_bits(
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

/// Counts what a hash reads, so its cost is asserted rather than timed.
#[derive(Default)]
struct Counting(usize);

impl Hasher for Counting {
    fn finish(&self) -> u64 {
        0
    }

    fn write(&mut self, bytes: &[u8]) {
        self.0 += bytes.len();
    }
}

/// Hashing reads a bounded amount however large the value is, which is what keeps a walk over a literal linear: a node is hashed when it is built, and peeling a literal builds one node per element.
#[test]
fn a_value_past_the_sample_is_hashed_at_a_cost_its_length_does_not_move() {
    let read = |bytes: usize| {
        let mut counting = Counting::default();
        Binary::from_bytes(vec![0x5A; bytes]).hash(&mut counting);
        counting.0
    };

    let sampled = read(HASH_SAMPLE_BYTES + 1);
    assert_eq!(sampled, read(1 << 10));
    assert_eq!(sampled, read(1 << 20));
    assert!(
        sampled <= HASH_SAMPLE_BYTES + 2 * size_of::<usize>(),
        "a hash read {sampled} bytes where the sample is {HASH_SAMPLE_BYTES} and two lengths",
    );
}

/// Equal values hash alike past the sample too, which is the obligation the sampling could most easily have broken: the two spellings are an unaligned window and a value built from the bytes directly, and both must read the same sampled bytes.
#[test]
fn a_window_past_the_sample_hashes_as_the_value_it_equals() {
    let bytes = (0..300u32)
        .map(|index| (index * 7 % 251) as u8)
        .collect::<Vec<_>>();
    let direct = Binary::from_bytes(bytes);
    let framed = Binary::from_bits(
        [true, false, true]
            .into_iter()
            .chain((0..direct.bit_length).map(|index| direct.bit(index).unwrap()))
            .chain([true, true]),
    );
    let window = framed.window(3, direct.bit_length).unwrap();

    assert_eq!(direct, window);
    assert_eq!(hash(&direct), hash(&window));
    // The sample reaches both ends, so a value differing only in its last byte is still told apart by the hash rather than by the comparison behind it.
    let mut last = direct.to_bytes().unwrap();
    *last.last_mut().unwrap() ^= 0xFF;
    assert_ne!(hash(&direct), hash(&Binary::from_bytes(last)));
}

/// The three arms of equality answer alike: a window against itself, an aligned window against a fresh buffer of the same bytes, and an aligned window against an unaligned window of the same bits — and a window against the same buffer one offset over is unequal when the bits say so.
#[test]
fn equality_agrees_across_its_arms() {
    let buffer = Binary::from_bytes(vec![0x61, 0x62, 0x63, 0x64, 0x61, 0x62]);
    let tail = buffer.window(16, 32).unwrap();

    assert_eq!(tail, buffer.window(16, 32).unwrap());
    assert_eq!(tail, Binary::from_bytes(vec![0x63, 0x64, 0x61, 0x62]));
    let framed = Binary::from_bits(
        [true, false, false]
            .into_iter()
            .chain((0..32).map(|i| tail.bit(i).unwrap())),
    );
    assert_eq!(tail, framed.window(3, 32).unwrap());

    assert_ne!(tail, buffer.window(8, 32).unwrap());
    assert_ne!(tail, buffer.window(16, 24).unwrap());
    assert_eq!(
        buffer.window(0, 16).unwrap(),
        buffer.window(32, 16).unwrap()
    );
}

#[test]
fn first_written_bit_is_least_significant() {
    assert_eq!(
        Binary::from_bits([true, false, true]).to_packed_bytes(),
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
            let value = Binary::from_bits(model.iter().copied());

            assert_eq!(value.len(Grain::B), model.len());
            for index in 0..=length {
                assert_eq!(value.bit(index), model.get(index).copied());
            }
            for start in 0..=length {
                for end in start..=length {
                    let slice = value.slice(Grain::B, start, end).unwrap();
                    let expected = Binary::from_bits(model[start..end].iter().copied());
                    assert_eq!(slice, expected);
                    assert_eq!(hash(&slice), hash(&expected));

                    // Appending to a *window*, whose bit offset is where an append that reads the packed payload rather than the bits would go wrong. Appending only to offset-zero values leaves that untested.
                    for bit in [false, true] {
                        let mut shifted = model[start..end].to_vec();
                        shifted.push(bit);
                        assert_eq!(
                            slice.append_bit(bit),
                            Binary::from_bits(shifted.into_iter())
                        );
                    }
                }
            }
            for bit in [false, true] {
                let mut expected = model.clone();
                expected.push(bit);
                assert_eq!(
                    value.append_bit(bit),
                    Binary::from_bits(expected.into_iter())
                );
            }
        }
    }
}

#[test]
fn byte_operations_preserve_alignment_and_match_vec_model() {
    let models = [vec![], vec![0], vec![0xff], vec![1, 2, 3, 4, 5, 6, 7, 8, 9]];
    for model in models {
        let value = Binary::from_bytes(model.clone());
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

    let unaligned = Binary::from_bits([true, false, true]);
    assert!(!unaligned.is_x_aligned());
    assert!(unaligned.append_byte(0).is_none());
    assert!(unaligned.to_bytes().is_none());
}

#[test]
fn aligned_concat_matches_the_bit_path_and_stays_aligned() {
    let left = Binary::from_bytes(vec![1, 2, 3]);
    let right = Binary::from_bytes(vec![4, 5]);
    let unaligned_twin = Binary::from_bits(
        [false; 3]
            .into_iter()
            .chain((0..right.bit_length()).map(|index| right.bit(index).unwrap())),
    )
    .window(3, right.bit_length())
    .unwrap();

    let fast = Binary::concat([&left, &right]);
    let slow = Binary::concat([&left, &unaligned_twin]);
    assert!(fast.is_x_aligned());
    assert_eq!(fast, slow);
    assert_eq!(hash(&fast), hash(&slow));
    assert_eq!(fast.to_bytes().unwrap(), vec![1, 2, 3, 4, 5]);
}

#[test]
fn concat_crosses_byte_boundaries_without_exposing_padding() {
    let left = Binary::from_bits([true, false, true, true, false]);
    let middle = Binary::from_bits([false, true, true, false, true, false]);
    let right = Binary::from_bits([true, true, false]);
    let actual = Binary::concat([&left, &middle, &right]);
    let expected = Binary::from_bits([
        true, false, true, true, false, false, true, true, false, true, false, true, true, false,
    ]);
    assert_eq!(actual, expected);
    assert_eq!(actual.bit_length(), 14);
    assert_eq!(actual.to_packed_bytes().last().unwrap() & 0b1100_0000, 0);
}

/// The order agrees with equality on every short bit string, and separates two values one packed byte cannot: `b[1]` and `b[1, 0]` pack alike and are ordered by length, shorter first.
#[test]
fn the_order_agrees_with_equality_and_separates_equal_packings() {
    let short = Binary::from_bits([true]);
    let long = Binary::from_bits([true, false]);
    assert_eq!(short.to_packed_bytes(), long.to_packed_bytes());
    assert!(short < long);

    let values = (0..=9)
        .flat_map(|length| {
            (0..(1usize << length)).map(move |mask| {
                Binary::from_bits((0..length).map(|index| mask & (1 << index) != 0))
            })
        })
        .collect::<Vec<_>>();
    for left in &values {
        for right in &values {
            assert_eq!(
                left.cmp(right).is_eq(),
                left == right,
                "{left:?} against {right:?}"
            );
        }
    }
}

/// The unaligned arm answers what the aligned one does: a window of a framed buffer orders against a directly built value exactly as two directly built values do.
#[test]
fn the_order_answers_alike_across_its_arms() {
    for length in 0..=8 {
        for mask in 0..(1usize << length) {
            let bits = (0..length)
                .map(|index| mask & (1 << index) != 0)
                .collect::<Vec<_>>();
            let direct = Binary::from_bits(bits.iter().copied());
            let framed = Binary::from_bits(
                [true, false, true]
                    .into_iter()
                    .chain(bits.iter().copied())
                    .chain([true, true]),
            );
            let window = framed.window(3, length).unwrap();
            assert!(direct.cmp(&window).is_eq());
            for other in [Binary::from_bits([true]), Binary::from_bytes(vec![7])] {
                assert_eq!(direct.cmp(&other), window.cmp(&other));
                assert_eq!(other.cmp(&direct), other.cmp(&window));
            }
        }
    }
}

/// The byte grain orders as `/std/Bytes/cmp` does — bytewise, the shorter prefix first — so the compiler's order and the language's never disagree about a value both can see.
#[test]
fn the_byte_grain_orders_as_the_language_does() {
    let model = |bytes: Vec<u8>| Binary::from_bytes(bytes);
    assert!(model(vec![1, 2]) < model(vec![1, 3]));
    assert!(model(vec![1]) < model(vec![1, 0]));
    assert!(model(vec![2]) > model(vec![1, 9]));
    assert!(model(vec![]) < model(vec![0]));
}

/// The bit grain's fill masks its own padding, and it is the only constructor here that has to.
///
/// An all-ones byte carries eight set bits whether or not the length claims them, so `replicate(B, 1, 3)` would pack as `0xFF` and compare unequal to the `b[1, 1, 1]` it denotes — the failure this masks away, and the reason the pointwise operations above it need no mask of their own.
#[test]
fn a_replicated_fill_leaves_no_padding_set() {
    for count in 0..=20 {
        for atom in [0u8, 1] {
            let filled = Binary::replicate(Grain::B, atom, count);
            let model = Binary::from_bits((0..count).map(|_| atom != 0));

            assert_eq!(filled, model, "a fill of {atom} at length {count}");
            assert_eq!(hash(&filled), hash(&model));
            assert_eq!(filled.len(Grain::B), count);
        }
    }

    for count in 0..=6 {
        let filled = Binary::replicate(Grain::X, 0xA5, count);
        assert_eq!(filled, Binary::from_bytes(vec![0xA5; count]));
        assert_eq!(filled.len(Grain::X), count);
    }
}

/// Pointwise combination agrees with the bit model at every length across a byte boundary and every pair of operands, including the lengths whose last byte is partial — where a dirty padding bit would be reported by both equality and the hash.
#[test]
fn pointwise_operations_agree_with_the_bit_model() {
    let bits = |length: usize, mask: usize| {
        Binary::from_bits((0..length).map(move |index| mask & (1 << index) != 0))
    };

    for length in 0..=8 {
        for left in 0..(1usize << length) {
            for right in 0..(1usize << length) {
                let (l, r) = (bits(length, left), bits(length, right));

                assert_eq!(l.and(&r), bits(length, left & right));
                assert_eq!(l.or(&r), bits(length, left | right));
                assert_eq!(l.xor(&r), bits(length, left ^ right));
                assert_eq!(hash(&l.xor(&r)), hash(&bits(length, left ^ right)));
            }
        }
    }
}

/// The two arms of the normalization meet: an operand read out of a misaligned window answers what the same bits answer packed from zero, which is what lets `/std`'s `not` be an `xor` against a fill.
#[test]
fn a_misaligned_operand_combines_as_its_packed_twin_does() {
    for length in 0usize..=10 {
        for offset in 0usize..=9 {
            let framed = Binary::from_bits(
                (0..offset + length + 3).map(|index: usize| index.is_multiple_of(3)),
            );
            let window = framed.window(offset, length).unwrap();
            let packed = Binary::from_bits((0..length).map(|i| (i + offset).is_multiple_of(3)));
            let ones = Binary::replicate(Grain::B, 1, length);

            assert_eq!(window, packed);
            assert_eq!(window.xor(&ones), packed.xor(&ones));
            assert_eq!(window.and(&ones), packed);
            assert_eq!(window.or(&ones), ones);
        }
    }
}
