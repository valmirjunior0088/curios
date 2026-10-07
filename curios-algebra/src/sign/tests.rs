use {super::*, curios_num::Floating};

/// A composition of the three sign operations over values named by position.
#[derive(Clone, Debug)]
enum Composed {
    Value(usize),
    Neg(Box<Composed>),
    Abs(Box<Composed>),
    Copysign(Box<Composed>, Box<Composed>),
}

/// Every composition over `values` values nested at most `depth` deep.
fn compositions(values: usize, depth: usize) -> Vec<Composed> {
    let mut all: Vec<Composed> = (0..values).map(Composed::Value).collect();
    for _ in 0..depth {
        let inner = all.clone();
        all.extend(
            inner
                .iter()
                .cloned()
                .map(|one| Composed::Neg(Box::new(one))),
        );
        all.extend(
            inner
                .iter()
                .cloned()
                .map(|one| Composed::Abs(Box::new(one))),
        );
        for magnitude in &inner {
            for sign in &inner {
                all.push(Composed::Copysign(
                    Box::new(magnitude.clone()),
                    Box::new(sign.clone()),
                ));
            }
        }
    }
    all
}

/// What the model computes.
fn computed(composed: &Composed, values: &[Floating]) -> Floating {
    match composed {
        Composed::Value(at) => values[*at],
        Composed::Neg(inner) => -computed(inner, values),
        Composed::Abs(inner) => computed(inner, values).abs(),
        Composed::Copysign(magnitude, sign) => {
            computed(magnitude, values).copysign(computed(sign, values))
        }
    }
}

fn read(composed: &Composed) -> SignMagnitude<usize> {
    match composed {
        Composed::Value(at) => SignMagnitude::of(*at),
        Composed::Neg(inner) => read(inner).negated(),
        Composed::Abs(inner) => read(inner).absolute(),
        Composed::Copysign(magnitude, sign) => read(magnitude).signed_as(read(sign)),
    }
}

/// What the model computes for a form as it is written.
fn written(form: SignForm<usize>, values: &[Floating]) -> Floating {
    match form {
        SignForm::Bare(value) => values[value],
        SignForm::Negated(value) => -values[value],
        SignForm::Absolute(value) => values[value].abs(),
        SignForm::NegatedAbsolute(value) => -values[value].abs(),
        SignForm::Copied { magnitude, sign } => values[magnitude].copysign(values[sign]),
        SignForm::CopiedNegated { magnitude, sign } => values[magnitude].copysign(-values[sign]),
    }
}

/// Two magnitudes under each sign, one of them a signaling NaN with a payload: what tells any two readings apart, since every operation here moves one bit and reads no other.
const PATTERNS: [u64; 4] = [
    0x3FF8_0000_0000_0000,
    0xBFF8_0000_0000_0000,
    0x7FF4_0000_0000_0123,
    0xFFF4_0000_0000_0123,
];

/// Every assignment of a pattern to each of `values` values.
fn assignments(values: usize) -> Vec<Vec<Floating>> {
    let mut all = vec![Vec::new()];
    for _ in 0..values {
        all = all
            .into_iter()
            .flat_map(|assigned| {
                PATTERNS.map(|bits| {
                    let mut assigned = assigned.clone();
                    assigned.push(Floating::from_bits(bits));
                    assigned
                })
            })
            .collect();
    }
    all
}

// The form a composition is written in is the composition, at every pattern: three levels deep over two values, and two levels over three, the model computes one pattern for the composition and for its form, a NaN's sign and payload included.
//
// Mutation-checked: an `absolute` that keeps the sign it was handed fails at `abs(v)`, and leaves fewer forms than the test below counts.
#[test]
fn a_composition_and_its_form_are_one_pattern_at_every_assignment() {
    for (values, depth) in [(2, 3), (3, 2)] {
        let assignments = assignments(values);
        for composed in compositions(values, depth) {
            let form = read(&composed).form();
            for assigned in &assignments {
                assert_eq!(
                    computed(&composed, assigned).to_bits(),
                    written(form, assigned).to_bits(),
                    "{composed:?} as {form:?} at {:x?}",
                    assigned
                        .iter()
                        .map(|value| value.to_bits())
                        .collect::<Vec<_>>(),
                );
            }
        }
    }
}

// The form is the only one: two compositions written in two forms are two patterns at some assignment, so the form decides the three operations completely and equates nothing a pattern tells apart.
#[test]
fn two_forms_are_two_patterns_at_some_assignment() {
    let assignments = assignments(2);
    let mut forms: Vec<SignForm<usize>> = Vec::new();
    for composed in compositions(2, 3) {
        let form = read(&composed).form();
        if !forms.contains(&form) {
            forms.push(form);
        }
    }
    // Two magnitudes, each under a clear bit, a set one, and either value's as it is or flipped.
    assert_eq!(forms.len(), 12);

    for (at, this) in forms.iter().enumerate() {
        for that in &forms[..at] {
            assert!(
                assignments.iter().any(|assigned| {
                    written(*this, assigned).to_bits() != written(*that, assigned).to_bits()
                }),
                "{this:?} and {that:?} are one pattern at every assignment",
            );
        }
    }
}
