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

/// The three arms of equality answer alike: a window against itself, an aligned window against a fresh buffer of the same bytes, and an aligned window against an unaligned window of the same bits — and a window against the same buffer one offset over is unequal when the bits say so.
#[test]
fn equality_agrees_across_its_arms() {
    let buffer = PackedBin::from_bytes(vec![0x61, 0x62, 0x63, 0x64, 0x61, 0x62]);
    let tail = buffer.window(16, 32).unwrap();

    assert_eq!(tail, buffer.window(16, 32).unwrap());
    assert_eq!(tail, PackedBin::from_bytes(vec![0x63, 0x64, 0x61, 0x62]));
    let framed = PackedBin::from_bits(
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
        true, false, true, true, false, false, true, true, false, true, false, true, true, false,
    ]);
    assert_eq!(actual, expected);
    assert_eq!(actual.bit_length(), 14);
    assert_eq!(actual.to_packed_bytes().last().unwrap() & 0b1100_0000, 0);
}
