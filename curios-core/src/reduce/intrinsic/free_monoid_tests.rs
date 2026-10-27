//! Lengths, windows, indices and equality over a spine, which may not depend on how its run is grouped.

use {
    super::{reduce_intrinsic, test_support::*},
    crate::{FUSION_CAP, Free, Intrinsic, Nat, Subterm, Term},
    curios_num::Binary,
};

// Regression: `get(append(x[], b), 0)` must reduce to `b` through its own base-case arm — the cons peel's symbolic head chunk IS `append(x[], b)`, so without that arm the rewrite would rebuild the redex it came from until the step budget exhausted.
#[test]
fn a_get_of_a_symbolic_cons_head_is_the_head() {
    let byte = Term::free_var(&Free::local(0, Some("b")));
    let empty: Term = Subterm::Intrinsic(Intrinsic::Bytes(Binary::empty())).into();
    let cons = Term::intrinsic(Intrinsic::bytes_append(empty, byte.clone()));
    let zero = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
    let get = Intrinsic::bytes_get(cons, zero, qed());

    let reduced = reduce_intrinsic(&mut Inert, &get).expect("reduces");

    assert_eq!(Term::from(reduced), byte);
}

/// **What `Bytes/len` answers from has to agree with what the run actually is.** A length is a definitional equation, so a measure that disagreed with the run would be a false one, and congruence carries a false equation to `False`. Ground truth here is the fused literal's own byte count, so this pins the measure to the representation rather than to itself — and it varies the grouping, including the left-nested shape an accumulation builds, because grouping is exactly what the measure must not be able to see.
#[test]
fn a_length_does_not_depend_on_how_its_run_is_grouped() {
    let whole: &[u8] = &[0x30, 0x31, 0x32, 0x33, 0x34];

    for spelling in groupings(whole) {
        let length = reduce_intrinsic(&mut Folding, &Intrinsic::BytesLen(spelling))
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
    let spine = Term::intrinsic(Intrinsic::BytesConcat {
        operands: vec![run_bytes(&[0x30, 0x31]), symbol(0, "b")],
    });

    let length = reduce_intrinsic(&mut Folding, &Intrinsic::BytesLen(spine))
        .expect("a length over a symbolic tail reduces to a neutral sum");

    assert_ne!(
        length,
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(2usize))),
        "the literal prefix is not the whole length",
    );
}

/// **A window located by operand lengths has to be the window.** `Bytes/slice` reaches its result by measuring the operands and narrowing the two at the edges, rather than peeling one byte at a time; the two must agree for every grouping, or slicing would depend on how a value was spelled.
#[test]
fn a_window_over_a_spine_is_the_window_over_its_run() {
    let whole: &[u8] = &[0x30, 0x31, 0x32, 0x33, 0x34];

    for spelling in groupings(whole) {
        for (start, count) in [(0usize, 5usize), (0, 2), (1, 3), (2, 3), (3, 0), (4, 1)] {
            let window = Intrinsic::bytes_slice(
                spelling.clone(),
                lit(start as u32),
                lit(count as u32),
                qed(),
            );

            let sliced = reduce_intrinsic(&mut Folding, &window).expect("a window reduces");

            assert_eq!(
                sliced,
                Subterm::Intrinsic(Intrinsic::Bytes(Binary::from_bytes(
                    whole[start..start + count].to_vec()
                ),)),
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
            let read = Intrinsic::bytes_get(spelling.clone(), lit(index as u32), qed());
            let byte = reduce_intrinsic(&mut Folding, &read).expect("an index reduces");

            assert_eq!(
                byte,
                Subterm::Intrinsic(Intrinsic::Byte(*expected)),
                "index {index} is the same byte however the run is grouped",
            );
        }
    }
}

/// **A window across operands too large to fuse is located across them.** Below [`FUSION_CAP`] a spine of literals fuses into one run before any window reads it, so only runs past the cap reach the window located over several operands: it takes an operand it covers whole as it stands, narrows the two at its edges, and keeps the concatenation, since the covered operand is itself past the cap.
#[test]
fn a_window_across_runs_past_the_fusion_cap_is_the_window_over_their_bytes() {
    let length = FUSION_CAP + 6;
    let runs = (0..3u8)
        .map(|run| {
            (0..length)
                .map(|index| run.wrapping_mul(100).wrapping_add(index as u8))
                .collect::<Vec<u8>>()
        })
        .collect::<Vec<_>>();
    let bytes =
        |values: &[u8]| Term::intrinsic(Intrinsic::Bytes(Binary::from_bytes(values.to_vec())));
    let operands = runs.iter().map(|run| bytes(run)).collect::<Vec<_>>();
    let spine = Term::intrinsic(Intrinsic::BytesConcat {
        operands: operands.clone(),
    });

    let (start, count) = (length - 10, length + 20);
    let window = Intrinsic::bytes_slice(spine, lit(start as u32), lit(count as u32), qed());
    let sliced = reduce_intrinsic(&mut Folding, &window).expect("a window reduces");

    assert_eq!(
        sliced,
        Subterm::Intrinsic(Intrinsic::BytesConcat {
            operands: vec![
                bytes(&runs[0][start..]),
                operands[1].clone(),
                bytes(&runs[2][..10]),
            ],
        }),
        "the ten bytes before the seam, the middle run whole, and the ten after",
    );
}

/// **A pointwise operation over runs too large to fuse is the operation over their bytes.** [`FUSION_CAP`] leaves a concatenation of such runs standing, and it is the same closed value as the run it would have fused to, so the fold reads it as that run whether the other operand is one literal or a concatenation grouped otherwise; one holding a symbolic run keeps its node.
#[test]
fn a_pointwise_operation_over_runs_past_the_fusion_cap_is_the_operation_over_their_bytes() {
    let length = FUSION_CAP + 6;
    let left = (0..2 * length)
        .map(|index| index as u8)
        .collect::<Vec<u8>>();
    let right = (0..2 * length)
        .map(|index| (index as u8).wrapping_mul(7))
        .collect::<Vec<u8>>();
    let spine = |values: &[u8], at: usize| {
        Term::intrinsic(Intrinsic::BytesConcat {
            operands: vec![run_bytes(&values[..at]), run_bytes(&values[at..])],
        })
    };
    let xor = left
        .iter()
        .zip(&right)
        .map(|(left, right)| left ^ right)
        .collect::<Vec<u8>>();

    for (spelled, other) in [
        (spine(&left, length), run_bytes(&right)),
        (spine(&left, length), spine(&right, length + 1)),
    ] {
        let combined = reduce_intrinsic(&mut Folding, &Intrinsic::bytes_xor(spelled, other, qed()))
            .expect("a pointwise operation reduces");

        assert_eq!(
            Term::from(combined),
            run_bytes(&xor),
            "the operation over the runs' bytes, however each operand is grouped",
        );
    }

    let symbolic = Term::intrinsic(Intrinsic::BytesConcat {
        operands: vec![run_bytes(&left[..length]), symbol(1, "tail")],
    });
    let kept = reduce_intrinsic(
        &mut Folding,
        &Intrinsic::bytes_xor(symbolic.clone(), run_bytes(&right), qed()),
    )
    .expect("a pointwise operation reduces");

    assert_eq!(
        kept,
        Subterm::Intrinsic(Intrinsic::bytes_xor(symbolic, run_bytes(&right), qed())),
        "a symbolic run leaves nothing to combine",
    );
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

/// **A seam is found by consuming the distance, and a span that overshoots one declines at once.** The walk cancels each operand's measure off the distance still to cover rather than growing a prefix and comparing whole terms. Stopping at the first overshoot is exact rather than conservative because a prefix only grows: a seam already passed cannot come back, so there is nothing later to find. The control is the same window one byte wider — a span no seam matches, which must stay neutral rather than settle for the nearer seam, since locating a run it does not cover would be a false equation in the admitting direction.
#[test]
fn a_window_on_a_symbolic_seam_is_its_run_and_a_span_past_the_seam_declines() {
    let left = symbol(0, "l");
    let right = symbol(1, "r");
    let spine = Term::intrinsic(Intrinsic::BytesConcat {
        operands: vec![left.clone(), right.clone()],
    });
    let measure = |bytes: &Term| Term::intrinsic(Intrinsic::bytes_len(bytes.clone()));

    let window = Intrinsic::bytes_slice(spine.clone(), measure(&left), measure(&right), qed());
    let located = reduce_intrinsic(&mut Folding, &window).expect("a seam window reduces");

    assert_eq!(
        Term::from(located),
        right,
        "the window from the first seam to the end is the second operand",
    );

    let past = Intrinsic::bytes_slice(spine, measure(&left), plus(measure(&right), lit(1)), qed());
    let declined = reduce_intrinsic(&mut Folding, &past).expect("an unmatched span reduces");

    assert_ne!(
        Term::from(declined),
        right,
        "a span reaching past the last seam locates no run",
    );
}

// A bare side is the one-chunk spine it is: the peel reads a `Bytes` word off a head, and a variable has none, so the equality fold wraps it as one. What that decides is the clash on a positive residual — `append(bs, k)` is one byte longer than `bs` whatever both take — and nothing more: a residual that may be empty stays neutral, as it would inside a concatenation.
#[test]
fn a_spine_against_a_bare_variable_clashes_on_a_positive_residual() {
    let (bs, cs, k) = (sym(0, "bs"), sym(1, "cs"), sym(2, "k"));

    let appended = Term::intrinsic(Intrinsic::bytes_append(bs.clone(), k));
    let folded = fold(Term::intrinsic(Intrinsic::bytes_eql(appended, bs.clone())));
    assert_eq!(
        folded.as_bool(),
        Some(false),
        "`append(bs, k) == bs` is false for every `bs` and `k`"
    );

    let extended = Term::intrinsic(Intrinsic::BytesConcat {
        operands: vec![bs.clone(), cs],
    });
    let folded = fold(Term::intrinsic(Intrinsic::bytes_eql(extended, bs)));
    assert!(
        matches!(&*folded, Subterm::Intrinsic(Intrinsic::BytesEql(..))),
        "`x[..bs, ..cs] == bs` is undecided while `cs` may be empty"
    );
}

// The identity is recognised by what a function's body reduces to under its binder, not by how the lambda is spelled: `(v) => v + 0` is the identity as `(v) => v` is, and a `map` by either is the list. The controls are the two ways a looser test would go wrong — a body that reduces to a *successor* of the binder, and one that reduces to a variable which is not the binder, a constant function no list survives.
#[test]
fn a_map_by_a_function_whose_body_reduces_to_its_binder_is_the_list() {
    let xs = sym(0, "xs");
    let other = sym(1, "w");
    let binder = Free::local(2, Some("v"));
    let v = Term::free_var(&binder);
    let zero = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
    let map_by = |body: Term| {
        Term::intrinsic(Intrinsic::list_map(
            nat_type(),
            nat_type(),
            xs.clone(),
            Term::func([(binder, nat_type())], body),
        ))
    };
    let is_map = |term: &Term| matches!(&**term, Subterm::Intrinsic(Intrinsic::ListMap { .. }));

    assert_eq!(fold(map_by(v.clone())), xs, "the lambda as written");
    assert_eq!(
        fold(map_by(add(v.clone(), zero.clone()))),
        xs,
        "`v + 0` reduces to the binder"
    );
    assert!(
        is_map(&fold(map_by(add(v, lit(1))))),
        "`v + 1` is not the binder"
    );
    assert!(
        is_map(&fold(map_by(add(other, zero)))),
        "a variable that is not the binder is a constant function"
    );
}

/// `x[h, ..t]` as lowering spells a cons: `h` appended to no bytes, then the rest.
fn cons(head: Term, tail: Term) -> Term {
    let empty: Term = Subterm::Intrinsic(Intrinsic::Bytes(Binary::empty())).into();
    Term::intrinsic(Intrinsic::BytesConcat {
        operands: vec![Term::intrinsic(Intrinsic::bytes_append(empty, head)), tail],
    })
}

/// Equality is read past what two runs share, a literal byte or one symbolic byte alike, as `Nat`'s comparisons are read past their common floor: what is left holds exactly when the pair does.
#[test]
fn a_common_prefix_leaves_the_equality_of_what_follows_it() {
    let (a, b, h) = (sym(0, "a"), sym(1, "b"), sym(2, "h"));
    let past = Intrinsic::bytes_eql(a.clone(), b.clone());

    let literal = |tail: Term| {
        Term::intrinsic(Intrinsic::BytesConcat {
            operands: vec![run_bytes(&[1]), tail],
        })
    };
    let folded = fold(Term::intrinsic(Intrinsic::bytes_eql(
        literal(a.clone()),
        literal(b.clone()),
    )));
    assert_eq!(
        folded,
        Term::intrinsic(past.clone()),
        "`x[1, ..a] == x[1, ..b]` is `a == b`"
    );

    let folded = fold(Term::intrinsic(Intrinsic::bytes_eql(
        cons(h.clone(), a),
        cons(h, b),
    )));
    assert_eq!(
        folded,
        Term::intrinsic(past),
        "`x[h, ..a] == x[h, ..b]` is `a == b`"
    );
}

/// Two runs that each open with a byte the peel cannot read are equal exactly when the bytes are and the rest are, the bytes compared as `/std/Byte`'s `==` compares them: the free monoid cancels, and a byte is one generator.
#[test]
fn two_runs_opening_with_a_byte_are_equal_when_the_bytes_and_the_rest_are() {
    let (a, b, h, k) = (sym(0, "a"), sym(1, "b"), sym(2, "h"), sym(3, "k"));

    let folded = fold(Term::intrinsic(Intrinsic::bytes_eql(
        cons(h.clone(), a.clone()),
        cons(k.clone(), b.clone()),
    )));
    let expected = Term::intrinsic(Intrinsic::BoolAnd(
        Term::intrinsic(Intrinsic::nat_eql(to_nat_of(h), to_nat_of(k.clone()))),
        Term::intrinsic(Intrinsic::bytes_eql(a.clone(), b.clone())),
    ));
    assert_eq!(
        folded, expected,
        "`x[h, ..a] == x[k, ..b]` is `h == k && a == b`"
    );

    let opened = Term::intrinsic(Intrinsic::BytesConcat {
        operands: vec![run_bytes(&[2, 3]), a.clone()],
    });
    let folded = fold(Term::intrinsic(Intrinsic::bytes_eql(
        opened,
        cons(k.clone(), b.clone()),
    )));
    let expected = Term::intrinsic(Intrinsic::BoolAnd(
        Term::intrinsic(Intrinsic::nat_eql(lit(2), to_nat_of(k))),
        Term::intrinsic(Intrinsic::bytes_eql(
            Term::intrinsic(Intrinsic::BytesConcat {
                operands: vec![run_bytes(&[3]), a],
            }),
            b,
        )),
    ));
    assert_eq!(folded, expected, "a literal run opens with its first byte");
}

/// The controls: a side that opens with no byte it shows, a bare run, leaves the pair neutral, and so does a pair the peel hands back as it was given — the bare side it wraps comes back bare, and following it would wrap it again without end.
#[test]
fn a_pair_no_byte_opens_stays_neutral() {
    let (a, b, h) = (sym(0, "a"), sym(1, "b"), sym(2, "h"));

    let against_literal = Intrinsic::bytes_eql(a.clone(), run_bytes(&[1]));
    let folded = fold(Term::intrinsic(against_literal.clone()));
    assert_eq!(
        folded,
        Term::intrinsic(against_literal),
        "`a == x[1]` stays as it is"
    );

    let against_cons = Intrinsic::bytes_eql(a, cons(h, b));
    let folded = fold(Term::intrinsic(against_cons.clone()));
    assert!(
        matches!(&*folded, Subterm::Intrinsic(Intrinsic::BytesEql(..))),
        "`a == x[h, ..b]` stays a comparison while `a` shows no byte"
    );
}
