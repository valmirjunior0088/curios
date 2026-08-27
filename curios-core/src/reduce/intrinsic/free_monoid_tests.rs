//! Lengths, windows and indices over a spine, which may not depend on how its run is grouped.

use {
    super::reduce_intrinsic,
    crate::{Free, Intrinsic, Nat, Subterm, Term},
    curios_utilities::{Grain, PackedBin},
};

use super::test_support::*;

// Regression: `get(append(b[], x), 0)` must reduce to `x` through its own base-case arm — the cons peel's symbolic head chunk IS `append(b[], x)`, so without that arm the rewrite rebuilt the redex it came from until the step budget exhausted.
#[test]
fn bit_get_of_a_symbolic_cons_head_is_the_bit() {
    let bit = Term::free_var(&Free::local(0, Some("bit")));
    let empty: Term = Subterm::Intrinsic(Intrinsic::Bin(Grain::B, PackedBin::empty())).into();
    let cons = Term::intrinsic(Intrinsic::bin_append(Grain::B, empty, bit.clone()));
    let zero = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
    let get = Intrinsic::bin_get(Grain::B, cons, zero, qed());

    let reduced = reduce_intrinsic(&mut Inert, &get).expect("reduces");

    assert_eq!(Term::from(reduced), bit);
}

/// **What `Bin/len` now answers from has to agree with what the run actually is.** The measure replaced computing a length by rebuilding a `Bin/len` per operand and handing each back to the reducer; a length is a definitional equation, so a measure that disagreed with the run would be a false one, and congruence carries a false equation to `False`. Ground truth here is the fused literal's own byte count, so this pins the measure to the representation rather than to itself — and it varies the grouping, including the left-nested shape an accumulation builds, because grouping is exactly what the measure must not be able to see.
#[test]
fn a_length_does_not_depend_on_how_its_run_is_grouped() {
    let whole: &[u8] = &[0x30, 0x31, 0x32, 0x33, 0x34];

    for spelling in groupings(whole) {
        let length = reduce_intrinsic(&mut Folding, &Intrinsic::BinLen(Grain::X, spelling))
            .expect("a length over a literal run reduces");

        assert_eq!(
            length,
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(whole.len()))),
            "every grouping of the same run is the same length",
        );
    }
}

/// The control the test above would be worthless without: an operand the measure cannot read must send the length back to the homomorphism rather than be silently skipped. Skipping one is the sharp failure — it would report `2` for a value at least two bytes long, which is a false equation in the admitting direction.
#[test]
fn an_unmeasurable_operand_is_not_skipped() {
    let spine = Term::intrinsic(Intrinsic::BinConcat {
        grain: Grain::X,
        operands: vec![run_bytes(&[0x30, 0x31]), symbol(0, "b")],
    });

    let length = reduce_intrinsic(&mut Folding, &Intrinsic::BinLen(Grain::X, spine))
        .expect("a length over a symbolic tail reduces to a neutral sum");

    assert_ne!(
        length,
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(2usize))),
        "the literal prefix is not the whole length",
    );
}

/// **A window located by operand lengths has to be the window.** `Bin/slice` now reaches its result by measuring the operands and narrowing the two at the edges, rather than peeling one byte at a time; the two must agree for every grouping, or slicing would depend on how a value was spelled.
#[test]
fn a_window_over_a_spine_is_the_window_over_its_run() {
    let whole: &[u8] = &[0x30, 0x31, 0x32, 0x33, 0x34];

    for spelling in groupings(whole) {
        for (start, count) in [(0usize, 5usize), (0, 2), (1, 3), (2, 3), (3, 0), (4, 1)] {
            let window = Intrinsic::bin_slice(
                Grain::X,
                spelling.clone(),
                lit(start as u32),
                lit(count as u32),
                qed(),
            );

            let sliced = reduce_intrinsic(&mut Folding, &window).expect("a window reduces");

            assert_eq!(
                sliced,
                Subterm::Intrinsic(Intrinsic::Bin(
                    Grain::X,
                    PackedBin::from_bytes(whole[start..start + count].to_vec()),
                )),
                "the {count} bytes at {start} are the same however the run is grouped",
            );
        }
    }
}

/// The index twin of the window test: reading a byte must not depend on the grouping either.
#[test]
fn an_index_into_a_spine_reads_the_same_byte() {
    let whole: &[u8] = &[0x30, 0x31, 0x32, 0x33, 0x34];

    for spelling in groupings(whole) {
        for (index, expected) in whole.iter().enumerate() {
            let read = Intrinsic::bin_get(Grain::X, spelling.clone(), lit(index as u32), qed());
            let byte = reduce_intrinsic(&mut Folding, &read).expect("an index reduces");

            assert_eq!(
                byte,
                Subterm::Intrinsic(Intrinsic::Byte(*expected)),
                "index {index} is the same byte however the run is grouped",
            );
        }
    }
}

#[test]
fn a_list_length_window_and_index_do_not_depend_on_grouping() {
    let whole: &[u32] = &[1, 2, 3, 4, 5];
    let elem = symbol(1000, "T");
    let concat = |parts: Vec<Term>| {
        Term::intrinsic(Intrinsic::ListConcat {
            element: elem.clone(),
            operands: parts,
        })
    };

    let groupings = [
        list_of(whole),
        concat(vec![list_of(&whole[..2]), list_of(&whole[2..])]),
        concat(vec![
            concat(vec![list_of(&whole[..1]), list_of(&whole[1..2])]),
            list_of(&whole[2..]),
        ]),
    ];

    for spelling in groupings {
        let length = reduce_intrinsic(
            &mut Folding,
            &Intrinsic::ListLen {
                element: elem.clone(),
                list: spelling.clone(),
            },
        )
        .expect("a length over an element run reduces");

        assert_eq!(
            length,
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(whole.len()))),
            "every grouping of the same run is the same length",
        );

        for (start, count) in [(0usize, 5usize), (1, 3), (2, 3), (3, 0)] {
            let window = Intrinsic::list_slice(
                elem.clone(),
                spelling.clone(),
                lit(start as u32),
                lit(count as u32),
                qed(),
            );
            let sliced = reduce_intrinsic(&mut Folding, &window).expect("a window reduces");

            assert_eq!(
                sliced,
                Term::unwrap_or_clone(list_of(&whole[start..start + count])),
                "the {count} elements at {start} are the same however the run is grouped",
            );
        }

        for (index, expected) in whole.iter().enumerate() {
            let read =
                Intrinsic::list_get(elem.clone(), spelling.clone(), lit(index as u32), qed());
            let element = reduce_intrinsic(&mut Folding, &read).expect("an index reduces");

            assert_eq!(
                element,
                Term::unwrap_or_clone(symbol(*expected, "e")),
                "index {index} is the same element however the run is grouped",
            );
        }
    }
}
