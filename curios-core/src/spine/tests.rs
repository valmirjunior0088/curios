use super::*;

fn sym(index: u32, hint: &'static str) -> Term {
    Term::free_var(&crate::Free::local(index, Some(hint)))
}

fn nat_of(floor: u32, inner: Term) -> Nat {
    Nat::Succ(floor.into(), inner)
}

fn add(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::nat_add(left, right))
}

// The conclusion cancelling summands adds to the peel: two sums that differ only in the order of their addends are the same number, so peeling decides them equal instead of handing a pair to a structural comparison that compares spellings and refuses. Sound because `+` commutes; new, because nothing else in the peel normalises summand order.
#[test]
fn peel_nat_decides_a_commuted_sum_equal() {
    let (x, y) = (sym(0, "x"), sym(1, "y"));

    let peel = peel_nat(
        &nat_of(1, add(x.clone(), y.clone())),
        &nat_of(1, add(y.clone(), x.clone())),
    );

    assert!(
        matches!(peel, Peel::Equal),
        "`x + y + 1` and `y + x + 1` are one number"
    );
}

// The clash the inverter reads as *impossible*, which is what excuses an omitted arm — so it must fire only where the two sides genuinely cannot be equal. A surviving positive floor against nothing is that case: whatever the symbolic residual takes, one side stays strictly larger.
#[test]
fn peel_nat_clashes_a_surviving_floor_against_the_identity() {
    let x = sym(0, "x");

    let peel = peel_nat(&nat_of(2, x.clone()), &nat_of(1, x.clone()));

    assert!(matches!(peel, Peel::Clash), "`x + 2` never equals `x + 1`");
}

// The control against closing the clash above by clashing everything: a shared floor over *distinct* symbols cancels to a pair that may still be equal, so peeling must hand it on rather than decide it.
#[test]
fn peel_nat_continues_where_the_residuals_may_still_agree() {
    let (x, y) = (sym(0, "x"), sym(1, "y"));

    let peel = peel_nat(&nat_of(1, x.clone()), &nat_of(1, y.clone()));

    assert!(
        matches!(peel, Peel::Continue(..)),
        "`x` and `y` are undecided, not unequal"
    );
}

fn bytes(run: impl Into<Vec<u8>>) -> Term {
    Term::intrinsic(Intrinsic::Bin(Grain::X, PackedBin::from_bytes(run.into())))
}

fn bits(run: impl IntoIterator<Item = bool>) -> Term {
    Term::intrinsic(Intrinsic::Bin(Grain::B, PackedBin::from_bits(run)))
}

fn nats(run: impl IntoIterator<Item = u32>) -> Term {
    let elems = run.into_iter().map(|n| sym(n, "e")).collect();
    Term::intrinsic(Intrinsic::List {
        element: sym(1000, "T"),
        items: elems,
    })
}

fn as_intrinsic(term: &Term) -> &Intrinsic {
    match &**term {
        Subterm::Intrinsic(intrinsic) => intrinsic,
        _ => unreachable!("a literal term"),
    }
}

// **How a literal run is grouped is invisible to the peel, and that is the premise the fusion cap rests on.** Reduction fuses an all-literal concatenation into one value today; capping that leaves the `Concat` node standing instead, so a capped spelling and the literal it would have fused to must still decide equal. They do because [`bin_atoms`] flattens a concatenation into segments and [`push`] merges every pair of adjacent literal runs, so both groupings reach the same segment list before anything is compared.
//
// A trailing symbolic operand is what makes this a test rather than a tautology: without it both sides are all-literal, reduction fuses each into one value on the way in, and the assertion holds without the peel having decided anything. With it, neither side fuses and the peel is the only thing that can equate them.
#[test]
fn peel_bin_decides_a_split_literal_run_against_a_whole_one() {
    let tail = sym(0, "b");

    for (grain, split, whole) in [
        (
            Grain::X,
            vec![bytes([0x30, 0x31]), bytes([0x32, 0x33]), tail.clone()],
            vec![bytes([0x30, 0x31, 0x32, 0x33]), tail.clone()],
        ),
        (
            Grain::B,
            vec![
                bits([true, false]),
                bits([true, true]),
                bits([false]),
                tail.clone(),
            ],
            vec![bits([true, false, true, true, false]), tail.clone()],
        ),
    ] {
        let split = Intrinsic::BinConcat {
            grain,
            operands: split,
        };
        let whole = Intrinsic::BinConcat {
            grain,
            operands: whole,
        };

        assert!(
            matches!(peel_bin(&split, &whole), Some(Peel::Equal)),
            "{grain:?}: a run split across operands is the run"
        );
    }
}

// The same premise one level deeper, over the shape the cap actually produces. An accumulation appends to its own result, so what it builds is *left-nested* — `concat(concat(concat(a, b), c), d)` — never a flat operand list. Flattening has to see through that nesting, or the cap would hold for a spelling nobody writes.
#[test]
fn peel_bin_decides_a_left_nested_concat_against_a_flat_one() {
    let tail = sym(0, "b");
    let nest = |left: Term, right: Term| {
        Term::intrinsic(Intrinsic::BinConcat {
            grain: Grain::X,
            operands: vec![left, right],
        })
    };

    let nested = Intrinsic::BinConcat {
        grain: Grain::X,
        operands: vec![
            nest(
                nest(bytes([0x30]), bytes([0x31, 0x32])),
                bytes([0x33, 0x34, 0x35]),
            ),
            tail.clone(),
        ],
    };
    let flat = Intrinsic::BinConcat {
        grain: Grain::X,
        operands: vec![bytes([0x30, 0x31, 0x32, 0x33, 0x34, 0x35]), tail],
    };

    assert!(
        matches!(peel_bin(&nested, &flat), Some(Peel::Equal)),
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
        matches!(peel_list(&split, &whole), Some(Peel::Equal)),
        "a run split across segments is the run"
    );
}

// **A nesting the prefix step cannot enter still comes back flat.** Two sides whose leading chunks are unlike — convertible or not, the peel cannot tell — used to decline as `Stuck` with the nesting intact, and the caller's shape congruence then refused a two-operand concatenation against a three-operand one before comparing a single chunk. Regrouping is the identity on values, so both segment lists ride back on `Continue` and the caller's next round compares one operand list against another.
#[test]
fn peel_bin_regroups_a_nested_concat_whose_leading_chunks_differ() {
    let (x, y, z) = (sym(0, "x"), sym(1, "y"), sym(2, "z"));
    let cat = |operands: Vec<Term>| Intrinsic::BinConcat {
        grain: Grain::X,
        operands,
    };

    let nested = cat(vec![
        Term::intrinsic(cat(vec![x.clone(), bytes([0x30])])),
        y.clone(),
    ]);
    let flat = cat(vec![z, bytes([0x30]), y.clone()]);

    let Some(Peel::Continue(left, right)) = peel_bin(&nested, &flat) else {
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

    let Some(Peel::Continue(left, right)) = peel_list(&appended, &flat) else {
        panic!("an append against a flat spelling regroups")
    };
    assert_eq!(left, Term::intrinsic(cat(vec![xs, single])));
    assert_eq!(right, Term::intrinsic(flat));
}

// The termination witness for the two above: a `Continue` hands back flat spellings, so the round after it is this pair, and it must decline rather than regroup again. Both sides are already their own segment lists, so nothing peels and nothing regroups.
#[test]
fn peel_bin_declines_a_flat_pair_whose_leading_chunks_differ() {
    let (x, y, z) = (sym(0, "x"), sym(1, "y"), sym(2, "z"));
    let cat = |operands: Vec<Term>| Intrinsic::BinConcat {
        grain: Grain::X,
        operands,
    };

    let this = cat(vec![x, bytes([0x30]), y.clone()]);
    let that = cat(vec![z, bytes([0x30]), y]);

    assert!(
        matches!(peel_bin(&this, &that), Some(Peel::Stuck)),
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
        matches!(peel_list(&this, &that), Some(Peel::Stuck)),
        "a flat pair with unlike heads is the caller's"
    );
}

fn and(left: Term, right: Term) -> Intrinsic {
    Intrinsic::BoolAnd(left, right)
}

// `&&` is a semilattice on its leaves, so a commuted, reassociated or repeated conjunction is the same set of leaves and the same value. Decided as a set rather than by spelling the tree one way, which is what the `&&`/`||` cliff was.
#[test]
fn peel_bool_decides_a_commuted_and_reassociated_conjunction_equal() {
    let (x, y, z) = (sym(0, "x"), sym(1, "y"), sym(2, "z"));

    let nested = and(Term::intrinsic(and(x.clone(), y.clone())), z.clone());
    let commuted = and(z, Term::intrinsic(and(y.clone(), x.clone())));
    assert!(
        matches!(peel_bool(&nested, &commuted), Some(Peel::Equal)),
        "`(x && y) && z` and `z && (y && x)` are one value"
    );

    let repeated = and(Term::intrinsic(and(x.clone(), y.clone())), x.clone());
    let once = and(y, x);
    assert!(
        matches!(peel_bool(&repeated, &once), Some(Peel::Equal)),
        "a repeated leaf is the leaf"
    );
}

// The control: unlike leaf sets are undecided, not unequal, since the values may still agree — so the peel declines to the caller's congruence and never clashes.
#[test]
fn peel_bool_declines_unlike_leaf_sets_without_clashing() {
    let (x, y, z) = (sym(0, "x"), sym(1, "y"), sym(2, "z"));

    let this = and(x.clone(), y);
    let that = and(x.clone(), z);
    assert!(
        matches!(peel_bool(&this, &that), Some(Peel::Stuck)),
        "`x && y` against `x && z` is the congruence's"
    );

    let disjunction = Intrinsic::BoolOr(x.clone(), x);
    assert!(
        peel_bool(&this, &disjunction).is_none(),
        "a conjunction against a disjunction is not this peel's"
    );
}

// A symmetric comparison is one value with its operands in either order, and it is decided here rather than respelled at the fold, since a guard's refinement is keyed on its written spelling.
#[test]
fn peel_symmetric_decides_a_swapped_comparison_equal() {
    let (x, y, z) = (sym(0, "x"), sym(1, "y"), sym(2, "z"));

    let this = Intrinsic::nat_eql(x.clone(), y.clone());
    let that = Intrinsic::nat_eql(y.clone(), x.clone());
    assert!(
        matches!(peel_symmetric(&this, &that), Some(Peel::Equal)),
        "`x == y` and `y == x` are one value"
    );

    let unlike = Intrinsic::nat_eql(x.clone(), z);
    assert!(
        matches!(peel_symmetric(&this, &unlike), Some(Peel::Stuck)),
        "`x == y` against `x == z` is the congruence's"
    );

    let ordered = Intrinsic::nat_lt(x.clone(), y.clone());
    let reversed = Intrinsic::nat_lt(y, x);
    assert!(
        peel_symmetric(&ordered, &reversed).is_none(),
        "an order relation is not symmetric and is not this peel's"
    );
}

// The control the three above would be worthless without: the peel must not decide *everything* equal. Regrouping preserves the element order, so a genuine reordering has to clash rather than merge.
#[test]
fn peel_bin_still_clashes_a_reordered_run() {
    let split = Intrinsic::BinConcat {
        grain: Grain::X,
        operands: vec![bytes([0x30]), bytes([0x31])],
    };
    let swapped = bytes([0x31, 0x30]);

    assert!(
        matches!(peel_bin(&split, as_intrinsic(&swapped)), Some(Peel::Clash)),
        "`0x30 ++ 0x31` is not `0x31 0x30`"
    );
}
