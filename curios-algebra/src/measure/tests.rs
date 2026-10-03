//! Measures over known and symbolic lengths: a located window, a window on a concatenation's seams, a concatenation's normal form, and a literal's split.

use {
    crate::{
        Concatenated, Cut, Joining, Seam, Span, Split, join, locate, seam_window, split,
        test_support::{Number, Toy, n},
    },
    std::convert::Infallible,
};

#[test]
fn a_located_window_takes_covered_operands_whole_and_narrows_its_edges() {
    assert_eq!(
        locate(&[2, 3, 4], 1, 6),
        Some(Ok(vec![
            Span::Part {
                operand: 0,
                range: 1..2
            },
            Span::Whole(1),
            Span::Part {
                operand: 2,
                range: 0..2
            },
        ]))
    );
    assert_eq!(locate(&[2, 0, 3], 2, 3), Some(Ok(vec![Span::Whole(2)])));
}

#[test]
fn a_window_past_the_end_reports_the_total() {
    assert_eq!(locate(&[2, 3], 4, 2), Some(Err(5)));
    assert_eq!(locate(&[2, 3], usize::MAX, 2), Some(Err(5)));
    assert_eq!(locate(&[usize::MAX, 1], 0, 1), None);
}

fn seam(toy: &Toy, measures: &[Number], start: Number, length: Number) -> Option<Seam<Number>> {
    let operands = (0..measures.len())
        .map(|index| ["a", "b", "c", "d"][index])
        .collect::<Vec<_>>();
    let measure = |operand: &&'static str| {
        let at = operands.iter().position(|held| held == operand).unwrap();
        Ok::<_, Infallible>(measures[at].clone())
    };
    let Ok(found) = seam_window(toy, &operands, &start, &length, measure);
    found
}

#[test]
fn a_window_on_two_seams_is_the_operands_between() {
    let toy = Toy::default();
    let measures = [n(0, &["x"]), n(0, &["y"]), n(0, &["z"])];
    assert_eq!(
        seam(&toy, &measures, n(0, &["x"]), n(0, &["y"])),
        Some(Seam::Parts(1..2))
    );
    assert_eq!(
        seam(&toy, &measures, n(0, &[]), n(0, &["x", "y", "z"])),
        Some(Seam::Parts(0..3))
    );
}

#[test]
fn a_window_inside_the_last_operand_narrows_to_it() {
    let toy = Toy::default();
    let measures = [n(0, &["x"]), n(0, &["y"])];
    assert_eq!(
        seam(&toy, &measures, n(0, &["i", "x"]), n(1, &[])),
        Some(Seam::Inside {
            operand: 1,
            start: n(0, &["i"])
        })
    );
    assert_eq!(
        seam(&toy, &measures, n(0, &["x"]), n(1, &[])),
        Some(Seam::Inside {
            operand: 1,
            start: n(0, &[])
        })
    );
}

#[test]
fn a_window_off_the_seams_declines() {
    let toy = Toy::default();
    let measures = [n(0, &["x"]), n(0, &["y"]), n(0, &["z"])];
    assert_eq!(seam(&toy, &measures, n(0, &["i"]), n(1, &[])), None);
    assert_eq!(
        seam(&toy, &measures, n(0, &[]), n(0, &["x", "i"])),
        None,
        "a window ending inside an operand past the first it began at"
    );
}

#[test]
fn a_concatenation_drops_the_identity_and_fuses_only_all_literal_survivors() {
    assert_eq!(
        join(&[Joining::Empty, Joining::Fusible, Joining::Fusible]),
        Concatenated::Fused(vec![1, 2])
    );
    assert_eq!(
        join(&[Joining::Empty, Joining::Empty]),
        Concatenated::Fused(vec![])
    );
    assert_eq!(
        join(&[Joining::Empty, Joining::Standing, Joining::Empty]),
        Concatenated::Lone(1)
    );
    assert_eq!(
        join(&[Joining::Fusible, Joining::Standing]),
        Concatenated::Kept(vec![0, 1])
    );
}

#[test]
fn a_literal_splits_at_known_lengths_and_a_trailing_unknown_takes_the_rest() {
    assert_eq!(
        split(&[Some(2), None], 5),
        Split {
            ranges: vec![0..2, 2..5],
            cut: Cut::Whole
        }
    );
    assert_eq!(
        split(&[Some(2), Some(1), Some(4)], 5),
        Split {
            ranges: vec![0..2, 2..3],
            cut: Cut::Clash
        }
    );
    assert_eq!(
        split(&[Some(2), Some(2)], 5),
        Split {
            ranges: vec![0..2, 2..4],
            cut: Cut::Clash
        }
    );
    assert_eq!(
        split(&[Some(1), Some(1), None, Some(1)], 5),
        Split {
            ranges: vec![0..1, 1..2],
            cut: Cut::Undetermined
        }
    );
}
