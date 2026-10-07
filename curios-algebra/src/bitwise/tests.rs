use {
    super::*,
    curios_num::{Integer, Natural},
};

/// The values the identities are held at: zero, `-1`, and values on both sides of them, one past 64 bits at each sign.
fn integers() -> Vec<Integer> {
    let wide = Integer::from(1)
        .shl_within(&Natural::from(70u32), u64::MAX)
        .expect("a shift this small is built");
    [-9i32, -2, -1, 0, 1, 2, 6, 255]
        .into_iter()
        .map(Integer::from)
        .chain([wide.clone(), -wide])
        .collect()
}

fn observed(left: &Integer, right: &Integer) -> Pair {
    let ones = Integer::from(-1);
    Pair {
        left_zero: left.is_zero(),
        right_zero: right.is_zero(),
        left_ones: *left == ones,
        right_ones: *right == ones,
        same: left == right,
    }
}

/// What `curios-num` computes.
fn computed(operation: Operation, left: Integer, right: Integer) -> Integer {
    match operation {
        Operation::And => left & right,
        Operation::Or => left | right,
        Operation::Xor => left ^ right,
        _ => unreachable!("a bitwise operation"),
    }
}

fn reduct(reduct: Reduct, left: &Integer, right: &Integer) -> Integer {
    match reduct {
        Reduct::Left => left.clone(),
        Reduct::Right => right.clone(),
        Reduct::Zero => Integer::from(0),
        Reduct::Ones => Integer::from(-1),
    }
}

// Every identity names the value the operation computes, on ℤ's two's-complement expansion: at every pair of values, whatever an identity makes of what the pair was observed to be is what `curios-num` computes. A natural is an integer that is never `-1`, so the grid holds ℕ's identities too.
//
// Mutation-checked: an `or` that takes an all-ones operand for its unit fails at `-9 | -1`.
#[test]
fn every_bitwise_identity_names_the_value_the_operation_computes() {
    for operation in [Operation::And, Operation::Or, Operation::Xor] {
        let mut named = 0;
        for left in integers() {
            for right in integers() {
                if let Some(identity) = operation.bitwise_identity(observed(&left, &right)) {
                    named += 1;
                    assert_eq!(
                        reduct(identity, &left, &right),
                        computed(operation, left.clone(), right.clone()),
                        "{operation:?} at {left} and {right}"
                    );
                }
            }
        }
        assert!(named > 0, "{operation:?} has an identity the grid reaches");
    }
}

// A shift by zero is the value and a shifted zero is zero, at both shifts and both signs, the right shift arithmetic.
#[test]
fn every_shift_identity_names_the_value_the_shift_computes() {
    for value in integers() {
        for count in [0u32, 1, 3, 70] {
            let count = Natural::from(count);
            let pair = Pair {
                left_zero: value.is_zero(),
                right_zero: count.is_zero(),
                ..Pair::default()
            };
            let left = value
                .shl_within(&count, u64::MAX)
                .expect("a shift this small is built");
            let right = &value >> &count;
            for (operation, shifted) in
                [(Operation::ShiftLeft, left), (Operation::ShiftRight, right)]
            {
                if let Some(identity) = operation.shift_identity(pair) {
                    assert_eq!(
                        reduct(identity, &value, &Integer::from(0)),
                        shifted,
                        "{operation:?} of {value} by {count}"
                    );
                }
            }
        }
    }
}
