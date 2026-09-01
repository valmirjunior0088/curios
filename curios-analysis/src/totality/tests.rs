//! The size-change engine's own algebra: composition, join, idempotence, and the shape order.
//!
//! Pure functions of matrices and shapes, needing no checker. The probes that drive `group_totality` through a real `Env` — including the `Walk::walk` position differential `documentation/soundness.md` asks for — live in `curios-analysis/tests/driven.rs`; see that file's header for why they are an integration test rather than a module here.

use {
    super::*,
    curios_core::{Atom, Free, Global, InductDecl, Intrinsic, Rec, StructDecl, Subterm, Term},
    curios_utilities::{Grain, PackedBin},
};

/// An [`Env`] that reduces nothing, for the two probes below that build a [`Walk`] by hand.
///
/// Not a checker and not a mock of one. Those probes assert *traversal* structure — that a peeled prefix keeps its binder tail, and that a deep body walks without native recursion — over fixtures already in normal form, so "reduce to weak-head normal form" is the identity on every term they contain. Anything that needed real reduction would be asking a typing question, and those live in `tests/driven.rs`, driven by the actual kernel.
///
/// They cannot be integration tests: [`Walk`] is an internal whose fields they set directly, and making it constructible from outside would mean publishing every one of them.
#[derive(Default)]
struct Probe {
    minted: std::cell::Cell<u32>,
}

impl Env for Probe {
    type Error = std::convert::Infallible;

    fn force(&mut self, term: &Term) -> Result<Term, Self::Error> {
        Ok(term.clone())
    }

    fn assumption(&self, _: &Free) -> Option<&Term> {
        None
    }

    fn fresh(&mut self, hint: Option<&str>) -> Free {
        let index = self.minted.get();
        self.minted.set(index + 1);
        Free::local(1_000 + index, hint)
    }

    fn unfold(&self, _: &Free) -> Option<&Term> {
        None
    }

    fn induct_decl(&self, _: &Global) -> Option<&InductDecl> {
        None
    }

    fn struct_decl(&self, _: &Global) -> Option<&StructDecl> {
        None
    }
}

/// Build a matrix from a row-major grid of size grades.
fn matrix(rows: &[&[Size]]) -> Matrix {
    let columns = rows.first().map_or(0, |row| row.len());
    Matrix {
        rows: rows.len(),
        columns,
        entries: rows.concat(),
    }
}

const LESS: Size = Size::Less;
const SAME: Size = Size::Same;
const NONE: Size = Size::Unknown;

#[test]
fn composition_annihilates_on_unknown_and_absorbs_on_less() {
    // A chain says nothing as soon as one link says nothing, and says "decreases" as soon as one link decreases.
    for size in [LESS, SAME, NONE] {
        assert_eq!(size.compose(NONE), NONE);
        assert_eq!(NONE.compose(size), NONE);
    }
    assert_eq!(LESS.compose(SAME), LESS);
    assert_eq!(SAME.compose(LESS), LESS);
    assert_eq!(LESS.compose(LESS), LESS);
    assert_eq!(SAME.compose(SAME), SAME);
}

#[test]
fn join_keeps_the_strongest_of_two_routes() {
    assert_eq!(NONE.join(SAME), SAME);
    assert_eq!(SAME.join(LESS), LESS);
    assert_eq!(NONE.join(LESS), LESS);
    assert_eq!(LESS.join(LESS), LESS);
}

#[test]
fn shapes_are_compared_by_the_proper_subterm_order() {
    let head = Free::local(1, None);
    let tail = Free::local(2, None);
    let whole = Shape::elem_run(
        Carriers::Bin,
        vec![Shape::Atom(head.clone())],
        Shape::Atom(tail.clone()),
    );

    assert_eq!(whole.against(&whole), Size::Same);
    assert_eq!(Shape::Atom(tail).against(&whole), Size::Less);
    assert_eq!(Shape::Atom(Free::local(9, None)).against(&whole), NONE);
}

#[test]
fn an_unread_term_is_never_equal_to_another_unread_term() {
    // Two terms the walk could not read are not thereby the same term; treating them as equal would manufacture a `Same` the analysis has no evidence for, and a chain of those is what an idempotent matrix needs.
    assert_eq!(Shape::Opaque.against(&Shape::Opaque), NONE);
}

#[test]
fn a_rebuilt_constructor_is_smaller_than_the_binder_it_rebuilds() {
    // `raw_trimmed(cons(a2, b2), …)` where `y` refined to `cons(yh, yt)` and `yt` refined to `cons(a2, b2)`: the argument is reached structurally, without folding the constructor back to `yt`.
    let (outer, a2, b2) = (
        Free::local(1, None),
        Free::local(2, None),
        Free::local(3, None),
    );
    let inner = Shape::elem_run(Carriers::Bin, vec![Shape::Atom(a2)], Shape::Atom(b2));
    let whole = Shape::elem_run(Carriers::Bin, vec![Shape::Atom(outer)], inner.clone());

    assert_eq!(inner.against(&whole), Size::Less);
}

#[test]
fn a_literal_is_one_packed_run_however_large_its_value() {
    // The size order on literals is decided by count arithmetic on the packed spine — never by expanding the value into a unary chain, whose depth for a `u64`-sized switch literal or call argument is a stack overflow.
    let empty = || Shape::Node(Tag::Empty(Carriers::Unary), Vec::new());
    let huge = Shape::unary_run(Natural::from(u64::MAX), empty());
    let smaller = Shape::unary_run(Natural::from(u64::MAX - 1), empty());

    assert_eq!(huge.against(&huge), Size::Same);
    assert_eq!(smaller.against(&huge), Size::Less);
    assert_eq!(huge.against(&smaller), NONE);
    assert_eq!(empty().against(&huge), Size::Less);
}

#[test]
fn runs_merge_so_a_refined_tail_stays_one_chain() {
    // `match n` refines `n` to a 1-run over `pred`; a nested arm refining `pred` to a 4-run must expand to the single 5-run the chain denotes, or a literal argument could never grade against it.
    let empty = || Shape::Node(Tag::Empty(Carriers::Unary), Vec::new());
    let merged = Shape::unary_run(
        Natural::from(1usize),
        Shape::unary_run(Natural::from(4usize), empty()),
    );

    assert!(merged.same_as(&Shape::unary_run(Natural::from(5usize), empty())));
    assert_eq!(
        Shape::unary_run(Natural::from(3usize), empty()).against(&merged),
        Size::Less
    );
}

#[test]
fn an_arithmetic_decrease_grades_only_against_its_own_binder() {
    // `n / 10` is below `n` and says nothing about anything else. In particular it must not grade against a parameter an enclosing arm has refined to a constructor, because that parameter no longer stands for the binder the decrease was measured from.
    let (n, other) = (Free::local(1, None), Free::local(2, None));
    let smaller = Shape::Smaller(n.clone());

    assert_eq!(smaller.against(&Shape::Atom(n.clone())), Size::Less);
    assert_eq!(smaller.against(&Shape::Atom(other)), NONE);
    assert_eq!(smaller.against(&Shape::Opaque), NONE);

    let refined = Shape::unary_run(Natural::from(1usize), Shape::Atom(n));
    assert_eq!(smaller.against(&refined), NONE);
}

#[test]
fn an_arithmetic_decrease_is_inert_in_the_constructor_order() {
    // It is a claim about a binder, not a value, so it can never be found equal to or inside a tree. Anything else would manufacture a decrease out of a term the walk never read.
    let n = Free::local(1, None);
    let smaller = Shape::Smaller(n.clone());
    let tree = Shape::Node(Tag::Tuple, vec![smaller.clone(), Shape::Atom(n.clone())]);
    let run = Shape::elem_run(Carriers::List, vec![smaller.clone()], Shape::Atom(n));

    assert!(!smaller.same_as(&smaller));
    assert!(!smaller.proper_subterm_of(&tree));
    assert!(!smaller.proper_subterm_of(&run));
}

#[test]
fn a_guard_excludes_zero_only_when_its_arm_does() {
    let guard = |relation, literal: usize| Guard {
        atom: Free::local(1, None),
        literal: Natural::from(literal),
        relation,
    };

    // `/std/Nat/to_str`: the false arm of `n < 10` gives `n >= 10`.
    assert!(guard(Relation::Lt, 10).establishes_nonzero(false));
    // ...but the true arm gives `n < 10`, which admits zero.
    assert!(!guard(Relation::Lt, 10).establishes_nonzero(true));

    // `/std/BigNat/of_nat`: the false arm of `n == 0` gives `n != 0`.
    assert!(guard(Relation::Eql, 0).establishes_nonzero(false));
    assert!(!guard(Relation::Eql, 0).establishes_nonzero(true));

    // `n >= 0` is no information at all, and `n < 0` is unsatisfiable rather than informative — neither may be read as excluding zero.
    assert!(!guard(Relation::Ge, 0).establishes_nonzero(true));
    assert!(!guard(Relation::Lt, 0).establishes_nonzero(false));

    // A strict lower bound excludes zero whatever the literal is.
    assert!(guard(Relation::Gt, 0).establishes_nonzero(true));
    assert!(guard(Relation::Le, 0).establishes_nonzero(false));
}

#[test]
fn a_guard_written_with_its_literal_first_reads_the_same() {
    // `10 > n` is `n < 10`, so the arm that excludes zero is the same one.
    assert_eq!(Relation::Gt.flipped(), Relation::Lt);
    assert_eq!(Relation::Le.flipped(), Relation::Ge);
    assert_eq!(Relation::Eql.flipped(), Relation::Eql);
    assert_eq!(Relation::Neq.flipped(), Relation::Neq);
}

#[test]
fn add_raw_is_accepted_only_because_arms_refine_the_scrutinee() {
    // The three call matrices of `/std/BigNat/add/raw`, over `(x, y, carry)`. In the empty-`x` arm the literal argument `b[]` grades `Same` against `x` *because* the arm refined `x` to `b[]`.
    let refined = [
        matrix(&[
            &[SAME, NONE, NONE],
            &[NONE, LESS, NONE],
            &[NONE, NONE, NONE],
        ]),
        matrix(&[
            &[LESS, NONE, NONE],
            &[NONE, SAME, NONE],
            &[NONE, NONE, NONE],
        ]),
        matrix(&[
            &[LESS, NONE, NONE],
            &[NONE, LESS, NONE],
            &[NONE, NONE, NONE],
        ]),
    ];
    let calls = refined
        .iter()
        .map(|matrix| (0usize, 0usize, matrix.clone()))
        .collect::<Vec<_>>();
    let closed = close(calls).expect("the closure stays small");
    assert!(
        closed
            .iter()
            .all(|(_, _, matrix)| !matrix.is_idempotent() || matrix.descends())
    );

    // Without refinement the two nil arms grade their own argument `Unknown`, and the composite is an idempotent matrix with nothing on its diagonal.
    let unrefined = [
        matrix(&[
            &[NONE, NONE, NONE],
            &[NONE, LESS, NONE],
            &[NONE, NONE, NONE],
        ]),
        matrix(&[
            &[LESS, NONE, NONE],
            &[NONE, NONE, NONE],
            &[NONE, NONE, NONE],
        ]),
    ];
    let composed = unrefined[0]
        .compose(&unrefined[1])
        .expect("square matrices compose");
    assert!(composed.is_idempotent());
    assert!(!composed.descends());
}

#[test]
fn a_nullary_self_call_cannot_descend() {
    // `rec inf : F = F/more(inf)` and `rec Bad : Type = Sink(Bad)` both have an empty parameter vector, so their self-call is a 0x0 matrix: idempotent, with no diagonal to decrease on.
    let empty = Matrix::unknown(0, 0);
    assert!(empty.is_idempotent());
    assert!(!empty.descends());
}
#[test]
fn mutual_recursion_is_caught_only_by_the_closure() {
    // Neither leg is a cycle on its own; only `a → b → a` is, and it descends.
    let forward = matrix(&[&[LESS]]);
    let backward = matrix(&[&[SAME]]);
    let closed = close(vec![(0usize, 1usize, forward), (1usize, 0usize, backward)])
        .expect("the closure stays small");

    let cycles = closed
        .iter()
        .filter(|(from, to, matrix)| from == to && matrix.is_idempotent())
        .collect::<Vec<_>>();
    assert!(!cycles.is_empty());
    assert!(cycles.iter().all(|(_, _, matrix)| matrix.descends()));
}

#[test]
fn a_peeled_prefix_keeps_its_binder_tail() {
    // The `BigNat/add/raw_trimmed` pattern: `b[h, ..t]` rebuilt from arm binders peels its head and sticks on the binder `t`. The remainder must take the full `shape_of` dispatch — a binder reads as its atom — not a force, which cannot move a local and would file the tail as unread, losing the suffix agreement the descent grades by.
    let mut kernel = Probe::default();
    let f = Free::local(1, Some("f"));
    let rec = Term::rec(
        vec![(
            f.clone(),
            Term::intrinsic(Intrinsic::NatType),
            Term::free_var(&f),
        )],
        Term::free_var(&f),
    );
    let Subterm::Rec(Rec { group, .. }) = &*rec else {
        panic!("the fixture changed shape");
    };

    let h = Free::local(2, Some("h"));
    let t = Free::local(3, Some("t"));
    let arities = vec![0];
    let mut walk = Walk {
        env: &mut kernel,
        group,
        arities: &arities,
        caller: 0,
        params: &[],
        refined: BTreeMap::new(),
        nonzero: BTreeSet::new(),
        payloads: BTreeSet::new(),
        entered: Vec::new(),
        scopes: Vec::new(),
        calls: Vec::new(),
    };

    let single = Term::intrinsic(Intrinsic::BinAppend {
        grain: Grain::X,
        bin: Term::intrinsic(Intrinsic::Bin(Grain::X, PackedBin::empty())),
        element: Term::free_var(&h),
    });
    let cons = Term::intrinsic(Intrinsic::BinConcat {
        grain: Grain::X,
        operands: vec![single, Term::free_var(&t)],
    });

    let shape = walk.shape_of(&cons);
    assert!(
        shape.same_as(&Shape::elem_run(
            Carriers::Bin,
            vec![Shape::Atom(h)],
            Shape::Atom(t),
        )),
        "expected a one-element run over the binder tail, got {shape:?}"
    );
}

/// Levels of written nesting in the deep body below.
///
/// The native recursion this replaces overflowed the default test-thread stack between 800 and 1,400 levels, so a regression here aborts the process rather than merely running long.
const DEEP: usize = 100_000;

#[test]
fn a_deeply_nested_body_walks_without_native_recursion() {
    // `(((… f(n) …)))`: a program-generated tuple nest, the mild sibling of a packed literal — bounded by the size of the source that spelled it rather than by a value, and unbounded by anything the walk controls. The nest is inert to the size order (no arm, no refinement, nothing to force), so what it measures is the traversal's own depth, and the graded call at the bottom is the evidence the walk arrived there.
    let mut kernel = Probe::default();
    let f = Free::local(1, Some("f"));
    let rec = Term::rec(
        vec![(
            f.clone(),
            Term::intrinsic(Intrinsic::NatType),
            Term::free_var(&f),
        )],
        Term::free_var(&f),
    );
    let Subterm::Rec(Rec { group, .. }) = &*rec else {
        panic!("the fixture changed shape");
    };

    let n = Free::local(2, Some("n"));
    let mut body = Term::apply(Term::rec_proj(group.clone(), 0), [Term::free_var(&n)]);
    for _ in 0..DEEP {
        body = Term::tuple([body]);
    }

    let params = [n];
    let arities = vec![1];
    let mut walk = Walk {
        env: &mut kernel,
        group,
        arities: &arities,
        caller: 0,
        params: &params,
        refined: BTreeMap::new(),
        nonzero: BTreeSet::new(),
        payloads: BTreeSet::new(),
        entered: Vec::new(),
        scopes: Vec::new(),
        calls: Vec::new(),
    };
    walk.walk(&body);

    // The call passes the parameter itself, so it grades `Same` — a grade only a walk that reached the bottom of the nest can record.
    assert_eq!(walk.calls.len(), 1);
    assert_eq!(walk.calls[0].2.entry(0, 0), SAME);
}

#[test]
fn an_application_of_a_constructor_payload_grades_below_the_constructor() {
    // The accessibility shape: `a` refined by an arm to `intro(w, below)`, and the call argument `below(y, r)`. A function-typed payload is a branching node whose children are its applications, so the application reads as the payload it came from and grades below the constructor for the reason the payload does. The control is the same application with `below` bound anywhere but a constructor pattern — outside the set, the head is a stuck binder the reduce-nothing environment cannot move, and the argument stays unread.
    let mut kernel = Probe::default();
    let f = Free::local(1, Some("f"));
    let rec = Term::rec(
        vec![(
            f.clone(),
            Term::intrinsic(Intrinsic::NatType),
            Term::free_var(&f),
        )],
        Term::free_var(&f),
    );
    let Subterm::Rec(Rec { group, .. }) = &*rec else {
        panic!("the fixture changed shape");
    };

    let a = Free::local(2, Some("a"));
    let w = Free::local(3, Some("w"));
    let below = Free::local(4, Some("below"));
    let y = Free::local(5, Some("y"));
    let r = Free::local(6, Some("r"));
    let intro = Shape::Node(
        Tag::Variant(Atom::from("intro")),
        vec![Shape::Atom(w), Shape::Atom(below.clone())],
    );
    let params = [a.clone()];
    let arities = vec![1];
    let mut walk = Walk {
        env: &mut kernel,
        group,
        arities: &arities,
        caller: 0,
        params: &params,
        refined: BTreeMap::from([(a.clone(), intro)]),
        nonzero: BTreeSet::new(),
        payloads: BTreeSet::from([below.clone()]),
        entered: Vec::new(),
        scopes: Vec::new(),
        calls: Vec::new(),
    };
    let applied = Term::apply(
        Term::free_var(&below),
        [Term::free_var(&y), Term::free_var(&r)],
    );

    let parameter = walk.expand(&a, EXPAND_FUEL);
    assert_eq!(walk.shape_of(&applied).against(&parameter), LESS);

    walk.payloads.clear();
    assert_eq!(walk.shape_of(&applied).against(&parameter), Size::Unknown);
}
