use {
    super::*,
    crate::{test_support::*, *},
};

fn sym(index: u32, hint: &'static str) -> Term {
    Term::free_var(&Free::local(u64::from(index), Some(hint)))
}

fn nat(value: u32) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(value)))
}

fn image(natural: &Term) -> Term {
    int_of_nat(&mut Folding, natural).expect("a natural widens")
}

fn add(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::nat_add(left, right))
}

fn int(value: i32) -> Term {
    Term::intrinsic(Intrinsic::Int(Integer::from(value)))
}

/// `comparison` with each of `symbols` replaced by the literal `values` gives it, folded to its `Bool`: [`Folding`] folds intrinsics all the way down.
fn value_at(comparison: &Intrinsic, symbols: &[(u64, &'static str)], values: &[Term]) -> bool {
    let mut term = Term::intrinsic(comparison.clone());
    for ((index, hint), value) in symbols.iter().zip(values) {
        let binder = Free::local(*index, Some(hint));
        term = Scope::close(One, &[&binder], term).open(&[value]);
    }
    match &*Folding.reduce(term).expect("reduces") {
        Subterm::Intrinsic(Intrinsic::Bool(value)) => *value,
        other => panic!("a closed comparison folds to a `Bool`, not {other:?}"),
    }
}

// A view is the difference of the sides, so how the sums were written — nested, commuted, or with a summand on the other side — is not part of it: `x + (y + z) < w + 1` and `(z + y) + x < 1 + w` are one view, and at `Int`, `i + j < k` and `j < k - i` are one too.
#[test]
fn a_view_does_not_depend_on_how_its_sums_are_written() {
    let (x, y, z, w) = (sym(0, "x"), sym(1, "y"), sym(2, "z"), sym(3, "w"));
    let mut views = LinearViews::default();
    let nested = views.view(&Intrinsic::nat_lt(
        add(x.clone(), add(y.clone(), z.clone())),
        Nat::rebuild(1u32.into(), w.clone()),
    ));
    let commuted = views.view(&Intrinsic::nat_lt(
        add(add(z.clone(), y.clone()), x.clone()),
        Nat::rebuild(1u32.into(), w.clone()),
    ));
    assert_eq!(nested, commuted);
    assert!(nested.is_some());

    let (i, j, k) = (sym(0, "i"), sym(1, "j"), sym(2, "k"));
    let mut views = LinearViews::default();
    let summed = views.view(&Intrinsic::IntLt(int_sum(&i, &j), k.clone()));
    let moved = views.view(&Intrinsic::IntLt(j.clone(), int_sum(&k, &int_negate(&i))));
    assert_eq!(summed, moved);
}

// Two comparisons with one view agree at every value their atoms take, and the view says which: `x + y + 1 < z + y` and `x + 1 < z` differ in a summand both sides share, and are one view; the shared `y` is gone from it.
#[test]
fn comparisons_with_one_view_agree_at_every_sampled_valuation() {
    let (x, y, z) = (sym(0, "x"), sym(1, "y"), sym(2, "z"));
    let wide = Intrinsic::nat_lt(
        add(add(x.clone(), y.clone()), nat(1)),
        add(z.clone(), y.clone()),
    );
    let narrow = Intrinsic::nat_lt(add(x.clone(), nat(1)), z.clone());

    let mut views = LinearViews::default();
    assert_eq!(views.view(&wide), views.view(&narrow));

    let symbols = [(0, "x"), (1, "y"), (2, "z")];
    for a in 0..3 {
        for b in 0..3 {
            for c in 0..3 {
                let values = [nat(a), nat(b), nat(c)];
                assert_eq!(
                    value_at(&wide, &symbols, &values),
                    value_at(&narrow, &symbols, &values),
                    "at x = {a}, y = {b}, z = {c}"
                );
            }
        }
    }
}

// The contract's one case the view decides alone: every atom cancelled, and the constant's sign the answer — at `Nat`, at `Int`, and through the embedding, where `Nat/to_int(m)` is the natural `m` read as an integer. A view that keeps an atom decides nothing.
#[test]
fn a_view_decides_exactly_when_every_atom_cancels() {
    let (x, y) = (sym(0, "x"), sym(1, "y"));
    let decided =
        |comparison: Intrinsic| LinearView::of(&comparison).and_then(|view| view.decided());

    assert_eq!(
        decided(Intrinsic::nat_lt(
            add(x.clone(), nat(1)),
            add(x.clone(), nat(2))
        )),
        Some(true)
    );
    assert_eq!(
        decided(Intrinsic::nat_eql(
            add(x.clone(), y.clone()),
            add(y.clone(), x.clone())
        )),
        Some(true)
    );
    assert_eq!(decided(Intrinsic::nat_lt(x.clone(), y.clone())), None);

    let (m, i) = (sym(0, "m"), sym(1, "i"));
    let widened = image(&m);
    assert_eq!(
        decided(Intrinsic::IntLe(
            int_sum(&widened, &i),
            int_sum(&i, &int_sum(&widened, &int(1)))
        )),
        Some(true)
    );
    assert_eq!(decided(Intrinsic::IntLt(i.clone(), int(0))), None);
}

// A widened natural is the natural it widens, read at `Int`: `m < n` and `Nat/to_int(m) < Nat/to_int(n)` are one view, and its atoms are the naturals', known non-negative.
#[test]
fn a_widened_natural_is_read_as_the_natural_it_widens() {
    let (m, n) = (sym(0, "m"), sym(1, "n"));
    let mut views = LinearViews::default();
    let natural = views
        .view(&Intrinsic::nat_lt(m.clone(), n.clone()))
        .expect("a `Nat` ordering");
    let widened = views
        .view(&Intrinsic::IntLt(image(&m), image(&n)))
        .expect("an `Int` ordering");

    assert_eq!(natural, widened);
    assert_eq!(natural.form().nonnegative.len(), 2);
}

// A reader tells the carrier it read a comparison at, which the view alone does not, and spells each atom of the view as the term it was handed out for.
#[test]
fn a_reader_reports_the_carrier_and_spells_each_atom() {
    let (x, y) = (sym(0, "x"), sym(1, "y"));
    let mut views = LinearViews::default();
    let (carrier, view) = views
        .read(&Intrinsic::nat_lt(add(x.clone(), nat(1)), y.clone()))
        .expect("a `Nat` ordering");
    assert_eq!(carrier, curios_algebra::Carrier::Natural);

    let spelled = view
        .form()
        .terms
        .iter()
        .flat_map(|(_, monomial)| monomial.atoms().iter().map(|atom| views.term(atom).clone()))
        .collect::<Vec<_>>();
    assert_eq!(spelled.len(), 2);
    assert!(spelled.contains(&x) && spelled.contains(&y));

    let (carrier, _) = views
        .read(&Intrinsic::IntLe(int(0), image(&x)))
        .expect("an `Int` ordering");
    assert_eq!(carrier, curios_algebra::Carrier::Integer);
}

// A bare truncated difference as one whole side is read across by the adjunction where that is exact, so the comparison and the one the adjunction gives it are one view and agree at every sampled valuation: at most another side, below as the greater side, against the literal zero as the order of its operands, and at any relation across a side at least one. The controls keep the difference an atom — at least, below or equal to a side that may be zero, and a floored difference — so each pair there is two views; and a fact about the atom is read atomic, where the moved reading of `x - y <= x` would hold no atom at all.
#[test]
fn a_bare_difference_is_read_across_its_comparison_where_the_adjunction_is_exact() {
    let (x, y, z) = (sym(0, "x"), sym(1, "y"), sym(2, "z"));
    let difference = Term::intrinsic(Intrinsic::nat_sub(x.clone(), y.clone()));
    let floored = |term: &Term| Nat::rebuild(1u32.into(), term.clone());
    let moved = [
        (
            Intrinsic::NatLe(difference.clone(), z.clone()),
            Intrinsic::NatLe(x.clone(), add(y.clone(), z.clone())),
        ),
        (
            Intrinsic::nat_lt(z.clone(), difference.clone()),
            Intrinsic::nat_lt(add(y.clone(), z.clone()), x.clone()),
        ),
        (
            Intrinsic::nat_eql(difference.clone(), nat(0)),
            Intrinsic::NatLe(x.clone(), y.clone()),
        ),
        (
            Intrinsic::nat_neq(difference.clone(), nat(0)),
            Intrinsic::nat_lt(y.clone(), x.clone()),
        ),
        (
            Intrinsic::nat_lt(difference.clone(), floored(&z)),
            Intrinsic::nat_lt(x.clone(), floored(&add(y.clone(), z.clone()))),
        ),
        (
            Intrinsic::NatLe(floored(&z), difference.clone()),
            Intrinsic::NatLe(floored(&add(y.clone(), z.clone())), x.clone()),
        ),
        (
            Intrinsic::nat_eql(difference.clone(), floored(&z)),
            Intrinsic::nat_eql(x.clone(), floored(&add(y.clone(), z.clone()))),
        ),
    ];
    let kept = [
        (
            Intrinsic::NatLe(z.clone(), difference.clone()),
            Intrinsic::NatLe(add(y.clone(), z.clone()), x.clone()),
        ),
        (
            Intrinsic::nat_lt(difference.clone(), z.clone()),
            Intrinsic::nat_lt(x.clone(), add(y.clone(), z.clone())),
        ),
        (
            Intrinsic::nat_eql(difference.clone(), z.clone()),
            Intrinsic::nat_eql(x.clone(), add(y.clone(), z.clone())),
        ),
        (
            Intrinsic::NatLe(floored(&difference), z.clone()),
            Intrinsic::NatLe(floored(&x), add(y.clone(), z.clone())),
        ),
    ];

    let symbols = [(0, "x"), (1, "y"), (2, "z")];
    for (written, across) in &moved {
        let mut views = LinearViews::default();
        assert_eq!(
            views.view(written),
            views.view(across),
            "{written:?} is one view with {across:?}"
        );
        for a in 0..4 {
            for b in 0..4 {
                for c in 0..4 {
                    let values = [nat(a), nat(b), nat(c)];
                    assert_eq!(
                        value_at(written, &symbols, &values),
                        value_at(across, &symbols, &values),
                        "{written:?} against {across:?} at x = {a}, y = {b}, z = {c}"
                    );
                }
            }
        }
    }
    for (written, across) in &kept {
        let mut views = LinearViews::default();
        assert_ne!(
            views.view(written),
            views.view(across),
            "{written:?} keeps its difference an atom apart from {across:?}"
        );
    }

    let within = Intrinsic::NatLe(difference.clone(), x.clone());
    let mut views = LinearViews::default();
    let (_, atomic) = views.read_atomic(&within).expect("a `Nat` ordering");
    assert!(
        atomic.form().terms.iter().any(|(_, monomial)| monomial
            .atoms()
            .iter()
            .any(|atom| *views.term(atom) == difference)),
        "read atomic, `x - y <= x` holds the difference as an atom"
    );
    let (_, moved) = views.read(&within).expect("a `Nat` ordering");
    assert!(
        moved.form().terms.iter().all(|(_, monomial)| monomial
            .atoms()
            .iter()
            .all(|atom| *views.term(atom) != difference)),
        "read across, it holds none"
    );
}
