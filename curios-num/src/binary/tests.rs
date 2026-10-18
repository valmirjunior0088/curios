use {
    super::*,
    std::{collections::hash_map::DefaultHasher, hash::Hasher},
};

fn hash(value: &Binary) -> u64 {
    let mut h = DefaultHasher::new();
    value.hash(&mut h);
    h.finish()
}

/// `model` with a byte on each side, and the window of it that holds `model` alone.
fn framed(model: &[u8]) -> Binary {
    let buffer = [0xA5]
        .into_iter()
        .chain(model.iter().copied())
        .chain([0x5A, 0xC3])
        .collect::<Vec<_>>();
    Binary::from_bytes(buffer).window(1, model.len()).unwrap()
}

#[test]
fn a_window_equals_and_hashes_as_the_value_it_holds() {
    for length in 0..=6 {
        let model = (0..length)
            .map(|index| index as u8 * 37)
            .collect::<Vec<_>>();
        let direct = Binary::from_bytes(model.clone());
        let window = framed(&model);
        assert_eq!(direct, window);
        assert_eq!(hash(&direct), hash(&window));
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
        sampled <= HASH_SAMPLE_BYTES + size_of::<usize>(),
        "a hash read {sampled} bytes where the sample is {HASH_SAMPLE_BYTES} and the length",
    );
}

/// Equal values hash alike past the sample too, which is the obligation the sampling could most easily have broken: the two spellings are a window of a larger buffer and a value built from the bytes directly, and both must read the same sampled bytes.
#[test]
fn a_window_past_the_sample_hashes_as_the_value_it_equals() {
    let bytes = (0..300u32)
        .map(|index| (index * 7 % 251) as u8)
        .collect::<Vec<_>>();
    let direct = Binary::from_bytes(bytes.clone());
    let window = framed(&bytes);

    assert_eq!(direct, window);
    assert_eq!(hash(&direct), hash(&window));
    // The sample reaches both ends, so a value differing only in its last byte is still told apart by the hash rather than by the comparison behind it.
    let mut last = direct.to_bytes();
    *last.last_mut().unwrap() ^= 0xFF;
    assert_ne!(hash(&direct), hash(&Binary::from_bytes(last)));
}

/// The two arms of equality answer alike: a window against itself, and a window against a fresh buffer or another buffer's window of the same bytes — and a window against the same buffer one offset over is unequal when the bytes say so.
#[test]
fn equality_agrees_across_its_arms() {
    let buffer = Binary::from_bytes(vec![0x61, 0x62, 0x63, 0x64, 0x61, 0x62]);
    let tail = buffer.window(2, 4).unwrap();

    assert_eq!(tail, buffer.window(2, 4).unwrap());
    assert_eq!(tail, Binary::from_bytes(vec![0x63, 0x64, 0x61, 0x62]));
    assert_eq!(tail, framed(&[0x63, 0x64, 0x61, 0x62]));

    assert_ne!(tail, buffer.window(1, 4).unwrap());
    assert_ne!(tail, buffer.window(2, 3).unwrap());
    assert_eq!(buffer.window(0, 2).unwrap(), buffer.window(4, 2).unwrap());
}

#[test]
fn byte_operations_match_the_vec_model() {
    let models = [vec![], vec![0], vec![0xff], vec![1, 2, 3, 4, 5, 6, 7, 8, 9]];
    for model in models {
        let value = Binary::from_bytes(model.clone());
        assert_eq!(value.len(), model.len());
        assert_eq!(value.to_bytes(), model);
        for start in 0..=model.len() {
            for end in start..=model.len() {
                let slice = value.slice(start, end).unwrap();
                assert_eq!(slice.to_bytes(), model[start..end]);
                assert_eq!(slice.byte(0), model[start..end].first().copied());
            }
        }
        assert!(value.slice(0, model.len() + 1).is_none());
        for byte in [0, 1, 0x7f, 0xff] {
            let mut expected = model.clone();
            expected.push(byte);
            assert_eq!(value.append_byte(byte).to_bytes(), expected);
        }
    }
}

#[test]
fn concat_joins_windows_by_their_bytes() {
    let left = Binary::from_bytes(vec![1, 2, 3]);
    let right = framed(&[4, 5]);

    let joined = Binary::concat([&left, &right]);
    assert_eq!(joined, Binary::from_bytes(vec![1, 2, 3, 4, 5]));
    assert_eq!(
        hash(&joined),
        hash(&Binary::from_bytes(vec![1, 2, 3, 4, 5]))
    );
    assert_eq!(joined.offset(), 0);
}

/// The order agrees with equality on every short byte string over a small alphabet, a window and its fresh twin answering alike.
#[test]
fn the_order_agrees_with_equality() {
    let alphabet = [0x00, 0x01, 0xFF];
    let values = (0..=3)
        .flat_map(|length| {
            (0..alphabet.len().pow(length)).map(move |code| {
                (0..length)
                    .map(|place| alphabet[code / alphabet.len().pow(place) % alphabet.len()])
                    .collect::<Vec<u8>>()
            })
        })
        .flat_map(|model| [Binary::from_bytes(model.clone()), framed(&model)])
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

/// Bytes order as `/std/Bytes/cmp` orders them — bytewise, the shorter prefix first — so the compiler's order and the language's never disagree about a value both can see.
#[test]
fn bytes_order_as_the_language_orders_them() {
    let model = |bytes: Vec<u8>| Binary::from_bytes(bytes);
    assert!(model(vec![1, 2]) < model(vec![1, 3]));
    assert!(model(vec![1]) < model(vec![1, 0]));
    assert!(model(vec![2]) > model(vec![1, 9]));
    assert!(model(vec![]) < model(vec![0]));
}

/// Pointwise combination agrees with the byte model at every pair of short operands, a window among them.
#[test]
fn pointwise_operations_agree_with_the_byte_model() {
    let alphabet = [0x00, 0x0F, 0xF0, 0xFF];
    for left in alphabet {
        for right in alphabet {
            let (l, r) = (
                framed(&[left, right]),
                Binary::from_bytes(vec![right, left]),
            );

            assert_eq!(
                l.and(&r),
                Binary::from_bytes(vec![left & right, right & left])
            );
            assert_eq!(
                l.or(&r),
                Binary::from_bytes(vec![left | right, right | left])
            );
            assert_eq!(
                l.xor(&r),
                Binary::from_bytes(vec![left ^ right, right ^ left])
            );
        }
    }
}
