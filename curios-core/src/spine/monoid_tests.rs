//! The free-monoid prefix step `Bytes` and `List` share: how a run is grouped, regrouping a spelling it cannot enter, the clashes it may and may not report, and a fill against the cons it equals.

use {
    super::{test_support::*, *},
    curios_num::Binary,
};

fn bytes(run: impl Into<Vec<u8>>) -> Term {
    Term::intrinsic(Intrinsic::Bytes(Binary::from_bytes(run.into())))
}

fn nats(run: impl IntoIterator<Item = u32>) -> Term {
    let elems = run.into_iter().map(|n| sym(n, "e")).collect();
    Term::intrinsic(Intrinsic::List {
        element: sym(1000, "T"),
        items: elems,
    })
}

// **How a literal run is grouped is invisible to the peel, and that is the premise the fusion cap rests on.** Reduction fuses an all-literal concatenation into one value up to the cap and leaves the `Concat` node standing past it, so a capped spelling and the literal it would have fused to must still decide equal. They do because `crate::words` flattens a concatenation into segments and `curios-algebra`'s `Word::push` merges every pair of adjacent runs, so both groupings reach the same segment list before anything is compared.
//
// A trailing symbolic operand is what makes this a test rather than a tautology: without it both sides are all-literal, reduction fuses each into one value on the way in, and the assertion holds without the peel having decided anything. With it, neither side fuses and the peel is the only thing that can equate them.
#[test]
fn peel_bytes_decides_a_split_literal_run_against_a_whole_one() {
    let tail = sym(0, "b");

    let split = Intrinsic::BytesConcat {
        operands: vec![bytes([0x30, 0x31]), bytes([0x32, 0x33]), tail.clone()],
    };
    let whole = Intrinsic::BytesConcat {
        operands: vec![bytes([0x30, 0x31, 0x32, 0x33]), tail],
    };

    assert!(
        matches!(peel_bytes(&split, &whole), Some(Deduction::Equal)),
        "a run split across operands is the run"
    );
}

// The same premise one level deeper, over the shape the cap actually produces. An accumulation appends to its own result, so what it builds is *left-nested* — `concat(concat(concat(a, b), c), d)` — never a flat operand list. Flattening has to see through that nesting, or the cap would hold for a spelling nobody writes.
#[test]
fn peel_bytes_decides_a_left_nested_concat_against_a_flat_one() {
    let tail = sym(0, "b");
    let nest = |left: Term, right: Term| {
        Term::intrinsic(Intrinsic::BytesConcat {
            operands: vec![left, right],
        })
    };

    let nested = Intrinsic::BytesConcat {
        operands: vec![
            nest(
                nest(bytes([0x30]), bytes([0x31, 0x32])),
                bytes([0x33, 0x34, 0x35]),
            ),
            tail.clone(),
        ],
    };
    let flat = Intrinsic::BytesConcat {
        operands: vec![bytes([0x30, 0x31, 0x32, 0x33, 0x34, 0x35]), tail],
    };

    assert!(
        matches!(peel_bytes(&nested, &flat), Some(Deduction::Equal)),
        "nesting is associativity, and the peel is flat"
    );
}

// The `List` twin. Its literal runs hold *terms* rather than decided bytes, so the elements here are symbols compared syntactically — which is all the premise needs, since regrouping never changes an element, only which run it sits in.
#[test]
fn peel_list_decides_a_split_literal_run_against_a_whole_one() {
    let tail = sym(0, "xs");

    let split = Intrinsic::ListConcat {
        element: sym(1000, "T"),
        operands: vec![nats([1, 2]), nats([3]), nats([4, 5]), tail.clone()],
    };
    let whole = Intrinsic::ListConcat {
        element: sym(1000, "T"),
        operands: vec![nats([1, 2, 3, 4, 5]), tail],
    };

    assert!(
        matches!(peel_list(&split, &whole), Some(Deduction::Equal)),
        "a run split across segments is the run"
    );
}

// The control the three above would be worthless without: the peel must not decide *everything* equal. Regrouping preserves the element order, so a genuine reordering has to clash rather than merge.
#[test]
fn peel_bytes_still_clashes_a_reordered_run() {
    let split = Intrinsic::BytesConcat {
        operands: vec![bytes([0x30]), bytes([0x31])],
    };
    let swapped = bytes([0x31, 0x30]);

    assert!(
        matches!(
            peel_bytes(&split, as_intrinsic(&swapped)),
            Some(Deduction::Impossible)
        ),
        "`0x30 ++ 0x31` is not `0x31 0x30`"
    );
}

// **A nesting the prefix step cannot enter still comes back flat.** Two sides whose leading chunks are unlike — convertible or not, the peel cannot tell — declined as `Undecided` with the nesting intact would let the caller's shape congruence refuse a two-operand concatenation against a three-operand one before comparing a single chunk. Regrouping is the identity on values, so both segment lists ride back on `Equivalent` and the caller's next round compares one operand list against another.
#[test]
fn peel_bytes_regroups_a_nested_concat_whose_leading_chunks_differ() {
    let (x, y, z) = (sym(0, "x"), sym(1, "y"), sym(2, "z"));
    let cat = |operands: Vec<Term>| Intrinsic::BytesConcat { operands };

    let nested = cat(vec![
        Term::intrinsic(cat(vec![x.clone(), bytes([0x30])])),
        y.clone(),
    ]);
    let flat = cat(vec![z, bytes([0x30]), y.clone()]);

    let Some(Deduction::Equivalent((left, right))) = peel_bytes(&nested, &flat) else {
        panic!("a nesting against a flat spelling regroups")
    };
    assert_eq!(left, Term::intrinsic(cat(vec![x, bytes([0x30]), y])));
    assert_eq!(right, Term::intrinsic(flat));
}

// The `List` twin, over the other spelling the segment list absorbs: an append is its base followed by a one-element run, so an append against a concatenation with an unlike head comes back as two concatenations the caller can compare operand by operand.
#[test]
fn peel_list_regroups_an_append_whose_leading_chunk_differs() {
    let (xs, ys, e) = (sym(0, "xs"), sym(1, "ys"), sym(2, "e"));
    let elem = sym(1000, "T");
    let cat = |operands: Vec<Term>| Intrinsic::ListConcat {
        element: elem.clone(),
        operands,
    };
    let single = Term::intrinsic(Intrinsic::List {
        element: elem.clone(),
        items: vec![e.clone()],
    });

    let appended = Intrinsic::list_append(elem.clone(), xs.clone(), e);
    let flat = cat(vec![ys, single.clone()]);

    let Some(Deduction::Equivalent((left, right))) = peel_list(&appended, &flat) else {
        panic!("an append against a flat spelling regroups")
    };
    assert_eq!(left, Term::intrinsic(cat(vec![xs, single])));
    assert_eq!(right, Term::intrinsic(flat));
}

// The termination witness for the two above: an `Equivalent` hands back flat spellings, so the round after it is this pair, and it must decline rather than regroup again. Both sides are already their own segment lists, so nothing peels and nothing regroups.
#[test]
fn peel_bytes_declines_a_flat_pair_whose_leading_chunks_differ() {
    let (x, y, z) = (sym(0, "x"), sym(1, "y"), sym(2, "z"));
    let cat = |operands: Vec<Term>| Intrinsic::BytesConcat { operands };

    let this = cat(vec![x, bytes([0x30]), y.clone()]);
    let that = cat(vec![z, bytes([0x30]), y]);

    assert!(
        matches!(peel_bytes(&this, &that), Some(Deduction::Undecided)),
        "a flat pair with unlike heads is the caller's"
    );
}

#[test]
fn peel_list_declines_a_flat_pair_whose_leading_chunks_differ() {
    let (xs, ys, e) = (sym(0, "xs"), sym(1, "ys"), sym(2, "e"));
    let elem = sym(1000, "T");
    let cat = |operands: Vec<Term>| Intrinsic::ListConcat {
        element: elem.clone(),
        operands,
    };
    let single = Term::intrinsic(Intrinsic::List {
        element: elem.clone(),
        items: vec![e],
    });

    let this = cat(vec![xs, single.clone()]);
    let that = cat(vec![ys, single]);

    assert!(
        matches!(peel_list(&this, &that), Some(Deduction::Undecided)),
        "a flat pair with unlike heads is the caller's"
    );
}

// The identity check reads the whole residual: a literal run behind a symbolic chunk gives the value a positive length whatever the chunk takes, so the pair is impossible — the suffix twin of the prefix clash, which the head-only read decided while leaving this one undecided.
#[test]
fn peel_bytes_clashes_a_positive_run_behind_a_symbolic_chunk_against_the_identity() {
    let x = sym(0, "x");
    let suffixed = Intrinsic::BytesConcat {
        operands: vec![x, bytes([0x05])],
    };

    assert!(
        matches!(
            peel_bytes(&suffixed, as_intrinsic(&bytes([]))),
            Some(Deduction::Impossible)
        ),
        "`x ++ x[05]` is never empty"
    );
}

// A symbolic appended byte is one byte long whatever it is, so an append against its own base leaves a single element against the identity, and that is a clash rather than an opaque chunk of unknown length.
#[test]
fn peel_bytes_clashes_a_symbolic_byte_against_the_identity() {
    let (x, c) = (sym(0, "x"), sym(1, "c"));
    let appended = Intrinsic::bytes_append(x.clone(), c);
    let base = Intrinsic::BytesConcat { operands: vec![x] };

    assert!(
        matches!(peel_bytes(&appended, &base), Some(Deduction::Impossible)),
        "`append(x, c)` is one byte longer than `x`"
    );
}

// A fill is the one packed shape whose leading generator is known without its length being: `replicate(n + 1, a)` certainly starts with `a` however large `n` is. Deciding it against the cons it equals is what lets a proof induct on a fill's count, which nothing else here can reach — a `Window` carries a length and a literal run carries its bytes, while this carries neither and is still not opaque.
#[test]
fn peel_bytes_decides_a_fill_of_a_successor_against_the_cons_it_equals() {
    let count = sym(0, "n");
    let tail = sym(1, "t");

    let fill = |n: Term| {
        Term::intrinsic(Intrinsic::BytesReplicate {
            count: n,
            atom: Term::intrinsic(Intrinsic::Byte(0x30)),
        })
    };

    // `replicate(n + 1, a) ++ t` against `[a] ++ replicate(n, a) ++ t`.
    let filled = Intrinsic::BytesConcat {
        operands: vec![
            fill(Term::intrinsic(Intrinsic::Nat(nat_of(1, count.clone())))),
            tail.clone(),
        ],
    };
    let consed = Intrinsic::BytesConcat {
        operands: vec![bytes([0x30]), fill(count), tail],
    };

    assert!(
        matches!(peel_bytes(&filled, &consed), Some(Deduction::Equal)),
        "a fill of a successor is its atom over a shorter fill"
    );
}

// The other half of the same rule, and the one that keeps it sound: a count carrying no floor might be zero, so the fill might be empty and its leading generator is *not* known. Declining is the refusing direction — the caller falls back to a structural comparison — where clashing would call two equal values unequal.
#[test]
fn peel_bytes_declines_a_fill_whose_count_is_not_a_successor() {
    let count = sym(0, "n");
    let atom = Term::intrinsic(Intrinsic::Byte(0x30));

    let fill = Intrinsic::BytesReplicate {
        count: count.clone(),
        atom: atom.clone(),
    };
    let consed = Intrinsic::BytesConcat {
        operands: vec![
            bytes([0x30]),
            Term::intrinsic(Intrinsic::BytesReplicate { count, atom }),
        ],
    };

    assert!(
        !matches!(peel_bytes(&fill, &consed), Some(Deduction::Impossible)),
        "a fill that might be empty exposes no generator to clash on"
    );
}
