//! Words over the toy alphabet: normalization, window fusion, the prefix strip and its verdicts, offsets and positions.

use {
    crate::{
        Packed, Run, Segment, Stripped, Window, Word, same_position,
        test_support::{Number, Toy, n},
    },
    curios_num::{Binary, Grain},
};

fn run(elements: &[u32]) -> Segment<Toy> {
    Segment::Run(elements.to_vec())
}

fn window(base: &'static str, offset: Number, length: Number, proof: &'static str) -> Segment<Toy> {
    Segment::Window(Window {
        base,
        offset,
        length,
        proof,
    })
}

fn word(toy: &Toy, segments: Vec<Segment<Toy>>) -> Word<Toy> {
    let mut word = Word::default();
    for segment in segments {
        word.push(toy, segment);
    }
    word
}

fn strip(toy: &Toy, left: Vec<Segment<Toy>>, right: Vec<Segment<Toy>>) -> Stripped {
    word(toy, left).strip_common_prefix(toy, &mut word(toy, right))
}

#[test]
fn adjacent_runs_merge_and_empty_segments_vanish() {
    let toy = Toy::default();
    let built = word(
        &toy,
        vec![
            run(&[1, 2]),
            run(&[]),
            window("b", n(0, &["i"]), n(0, &[]), "p"),
            run(&[3]),
        ],
    );

    match built.segments().iter().collect::<Vec<_>>().as_slice() {
        [Segment::Run(elements)] => assert_eq!(*elements, vec![1, 2, 3]),
        _ => panic!("expected one merged run"),
    }
}

#[test]
fn touching_windows_of_one_base_fuse_and_keep_the_later_proof() {
    let toy = Toy::default();
    let built = word(
        &toy,
        vec![
            window("b", n(0, &["s"]), n(2, &[]), "first"),
            window("b", n(2, &["s"]), n(0, &["l"]), "second"),
            window("b", n(2, &["l", "s"]), n(1, &[]), "third"),
        ],
    );

    match built.segments().iter().collect::<Vec<_>>().as_slice() {
        [Segment::Window(fused)] => {
            assert_eq!(fused.offset, n(0, &["s"]));
            assert_eq!(fused.length, n(3, &["l"]));
            assert_eq!(fused.proof, "third");
        }
        _ => panic!("expected one fused window"),
    }
}

#[test]
fn windows_of_two_bases_or_apart_do_not_fuse() {
    let toy = Toy::default();
    let apart = word(
        &toy,
        vec![
            window("b", n(0, &[]), n(2, &[]), "p"),
            window("b", n(3, &[]), n(1, &[]), "q"),
            window("c", n(4, &[]), n(1, &[]), "r"),
        ],
    );
    assert_eq!(apart.segments().len(), 3);
}

#[test]
fn a_common_prefix_strips_to_equal() {
    let toy = Toy::default();
    assert_eq!(
        strip(
            &toy,
            vec![run(&[1]), Segment::Chunk("x"), run(&[2, 3])],
            vec![run(&[1]), Segment::Chunk("x"), run(&[2]), run(&[3])],
        ),
        Stripped::Equal
    );
}

#[test]
fn element_runs_that_disagree_leave_a_residual() {
    let toy = Toy::default();
    assert_eq!(
        strip(&toy, vec![run(&[1, 2])], vec![run(&[1, 3])]),
        Stripped::Residual { peeled: true }
    );
    assert_eq!(
        strip(&toy, vec![Segment::Chunk("x")], vec![Segment::Chunk("y")]),
        Stripped::Residual { peeled: false }
    );
}

#[test]
fn a_positive_residual_against_the_empty_word_is_impossible() {
    let toy = Toy::default();
    assert_eq!(
        strip(
            &toy,
            vec![run(&[1]), Segment::Chunk("x"), Segment::Single("e")],
            vec![run(&[1])],
        ),
        Stripped::Impossible
    );
    assert_eq!(
        strip(
            &toy,
            vec![],
            vec![
                Segment::Chunk("x"),
                window("b", n(0, &[]), n(0, &["l"]), "p")
            ],
        ),
        Stripped::Undecided
    );
}

#[test]
fn windows_over_one_span_strip_whole() {
    let mut toy = Toy::default();
    // `c` is `b` from `s` on, so `c` at `t` is `b` at `s + t`.
    toy.windows.insert("c", ("b", n(0, &["s"])));
    assert_eq!(
        strip(
            &toy,
            vec![window("c", n(0, &["t"]), n(0, &["l"]), "p")],
            vec![window("b", n(0, &["s", "t"]), n(0, &["l"]), "q")],
        ),
        Stripped::Equal
    );
    assert_eq!(
        strip(
            &toy,
            vec![window("c", n(0, &["t"]), n(0, &["l"]), "p")],
            vec![window("b", n(0, &["t"]), n(0, &["l"]), "q")],
        ),
        Stripped::Residual { peeled: false }
    );
}

#[test]
fn an_operand_begins_after_the_measures_before_it() {
    let toy = Toy::default();
    let built = word(
        &toy,
        vec![
            run(&[1, 2]),
            Segment::Single("e"),
            window("b", n(0, &[]), n(0, &["l"]), "p"),
            Segment::Chunk("x"),
            Segment::Chunk("y"),
            Segment::Chunk("x"),
        ],
    );
    assert_eq!(
        built.offsets_of(&toy, &"x"),
        vec![n(3, &["l"]), n(3, &["l", "x", "y"])]
    );
}

#[test]
fn a_position_inside_an_operand_is_its_position_in_the_concatenation() {
    let mut toy = Toy::default();
    toy.concatenations.insert("r", vec!["a", "b", "c"]);
    assert!(same_position(
        &toy,
        (&"r", &n(0, &["a", "i"])),
        (&"b", &n(0, &["i"]))
    ));
    assert!(same_position(
        &toy,
        (&"b", &n(0, &["i"])),
        (&"r", &n(0, &["a", "i"]))
    ));
    assert!(!same_position(
        &toy,
        (&"r", &n(0, &["i"])),
        (&"b", &n(0, &["i"]))
    ));
}

#[test]
fn packed_runs_strip_across_bits_and_clash_where_they_differ() {
    let bits = |pattern: &[bool]| Packed {
        grain: Grain::B,
        bits: Binary::from_bits(pattern.iter().copied()),
    };
    let mut left = bits(&[true, false, true]);
    let right = bits(&[true, false, false, true]);
    assert_eq!(left.shared_prefix(&right), 2);
    left.skip(2);
    assert_eq!(left, bits(&[true]));
    left.extend(bits(&[false]));
    assert_eq!(left, bits(&[true, false]));

    let bytes = |values: &[u8]| Packed {
        grain: Grain::X,
        bits: Binary::from_bytes(values.to_vec()),
    };
    assert_eq!(bytes(&[1, 2, 3]).shared_prefix(&bytes(&[1, 2, 4])), 2);
}
