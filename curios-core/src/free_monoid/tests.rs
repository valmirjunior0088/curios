use {super::*, crate::Peel};

/// The shape [`FUSION_CAP`] makes reachable: a left-nested concatenation as deep as an accumulation loop is long, since an accumulator past the cap keeps one node per step instead of fusing into one run.
fn deep_bin(grain: Grain, depth: usize) -> Term {
    let leaf = |byte| {
        Term::from(Subterm::Intrinsic(Intrinsic::Bin(
            grain,
            PackedBin::from_bytes(vec![byte]),
        )))
    };

    (0..depth).fold(leaf(0x30), |acc, _| {
        Subterm::Intrinsic(Intrinsic::BinConcat {
            grain,
            operands: vec![acc, leaf(0x31)],
        })
        .into()
    })
}

/// Its `List` twin, over an element type that stays symbolic.
fn deep_list(depth: usize) -> Term {
    let elem = Term::free_var(&crate::Free::local(0, Some("T")));
    let leaf = |index: u32| {
        Term::from(Subterm::Intrinsic(Intrinsic::List {
            element: elem.clone(),
            items: vec![Term::free_var(&crate::Free::local(index, Some("e")))],
        }))
    };

    (0..depth).fold(leaf(1), |acc, _| {
        Subterm::Intrinsic(Intrinsic::ListConcat {
            element: elem.clone(),
            operands: vec![acc, leaf(2)],
        })
        .into()
    })
}

// A hundred thousand levels is what a loop builds, and it used to be unreachable: fusion collapsed every concatenation into one run on the way in, so no walk here ever saw depth. `FUSION_CAP` is what creates it, which is why these tests land *with* the cap rather than after it — a walk that recursed once per level would spend one granted segment and then the process, since `recurse` is taken at each checker's reduction entry point and checked between *its* frames rather than inside a helper it calls. The same depth constant for the same reason as `print::tests::a_deep_term_is_printed_without_overflowing` and `term::tests::deep_terms_compare_without_native_recursion`, which are the two walks that already met data-shaped depth and were made to survive it.
#[test]
fn a_deep_concatenation_peels_its_first_generator() {
    const DEEP: usize = 100_000;

    let (head, _tail) = peel_first_atom(Grain::X, &deep_bin(Grain::X, DEEP))
        .expect("a literal-led concatenation exposes its leading byte");

    assert_eq!(
        head,
        Term::from(Subterm::Intrinsic(Intrinsic::Bin(
            Grain::X,
            PackedBin::from_bytes(vec![0x30])
        ))),
        "the leftmost leaf's byte, found under a hundred thousand levels"
    );
}

#[test]
fn a_deep_list_concatenation_peels_its_first_element() {
    const DEEP: usize = 100_000;

    let (head, _tail) =
        peel_first_elem(&deep_list(DEEP)).expect("a literal-led concatenation exposes its head");

    assert_eq!(
        head,
        Term::free_var(&crate::Free::local(1, Some("e"))),
        "the leftmost leaf's element"
    );
}

// The conversion side of the same depth: `peel_bin`/`peel_list` flatten a value into segments before comparing, so they meet the nesting through a different walk than the destructors above and need their own evidence. Comparing a value against itself is the cheapest thing that forces both sides to flatten in full.
#[test]
fn a_deep_concatenation_flattens_for_conversion() {
    const DEEP: usize = 100_000;

    let bin = deep_bin(Grain::X, DEEP);
    let Subterm::Intrinsic(intrinsic) = &*bin else {
        unreachable!("a concatenation");
    };

    assert!(
        matches!(crate::peel_bin(intrinsic, intrinsic), Some(Peel::Equal)),
        "a value is itself, however deeply it is nested"
    );

    let list = deep_list(DEEP);
    let Subterm::Intrinsic(intrinsic) = &*list else {
        unreachable!("a concatenation");
    };

    assert!(
        matches!(crate::peel_list(intrinsic, intrinsic), Some(Peel::Equal)),
        "the `List` twin"
    );
}
