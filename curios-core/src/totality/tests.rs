use {
    super::*,
    crate::{DefinitionKind, UniverseContext},
    curios_base::{Qualifier, RootId},
    std::time::Duration,
};

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
    // A chain says nothing as soon as one link says nothing, and says
    // "decreases" as soon as one link decreases.
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
    let whole = Shape::Node(
        Tag::Cons(Carriers::Bin),
        vec![Shape::Atom(head.clone()), Shape::Atom(tail.clone())],
    );

    assert_eq!(whole.against(&whole), Size::Same);
    assert_eq!(Shape::Atom(tail).against(&whole), Size::Less);
    assert_eq!(Shape::Atom(Free::local(9, None)).against(&whole), NONE);
}

#[test]
fn an_unread_term_is_never_equal_to_another_unread_term() {
    // Two terms the walk could not read are not thereby the same term;
    // treating them as equal would manufacture a `Same` the analysis has no
    // evidence for, and a chain of those is what an idempotent matrix needs.
    assert_eq!(Shape::Opaque.against(&Shape::Opaque), NONE);
}

#[test]
fn a_rebuilt_constructor_is_smaller_than_the_binder_it_rebuilds() {
    // `raw_trimmed(cons(a2, b2), …)` where `y` refined to `cons(yh, yt)` and
    // `yt` refined to `cons(a2, b2)`: the argument is reached structurally,
    // without folding the constructor back to `yt`.
    let (outer, a2, b2) = (
        Free::local(1, None),
        Free::local(2, None),
        Free::local(3, None),
    );
    let inner = Shape::Node(
        Tag::Cons(Carriers::Bin),
        vec![Shape::Atom(a2), Shape::Atom(b2)],
    );
    let whole = Shape::Node(
        Tag::Cons(Carriers::Bin),
        vec![Shape::Atom(outer), inner.clone()],
    );

    assert_eq!(inner.against(&whole), Size::Less);
}

#[test]
fn add_raw_is_accepted_only_because_arms_refine_the_scrutinee() {
    // The three call matrices of `/std/BigNat/add/raw`, over `(x, y, carry)`.
    // In the empty-`x` arm the literal argument `b\` grades `Same` against `x`
    // *because* the arm refined `x` to `b\`.
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

    // Without refinement the two nil arms grade their own argument `Unknown`,
    // and the composite is an idempotent matrix with nothing on its diagonal.
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
    // `rec inf : F = F/more(inf)` and `rec Bad : Type = Sink(Bad)` both have an
    // empty parameter vector, so their self-call is a 0x0 matrix: idempotent,
    // with no diagonal to decrease on.
    let empty = Matrix::unknown(0, 0);
    assert!(empty.is_idempotent());
    assert!(!empty.descends());
}

/// A qualified top-level name, from the path a test writes. Fixture-only.
fn name(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

/// A `let` whose body is a bare reference to `mentions`. Fixture-only: the
/// closure reads free variables, so one reference is the whole signal.
fn mentioning(path: &str, mentions: &str) -> Item {
    Item::Let(Definition {
        name: name(path),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::empty(),
        root: RootId::Entry,
        totality: Totality::default(),
        type_: Term::prim(Prim::NatType),
        body: Term::free_var(&Free::from(&name(mentions))),
    })
}

fn module(items: Vec<Item>) -> Module {
    Module {
        items,
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 0,
        type_: None,
        body: Term::prim(Prim::Nat(Nat::new(0u32))),
    }
}

#[test]
fn a_partial_name_from_outside_the_module_still_taints_what_mentions_it() {
    // The replay path: `caller` is user code and `/std/Async/bind` is a prelude
    // definition this module does not contain. Without the inherited verdict
    // the walk sees an unresolvable name and calls `caller` total, which is the
    // hole that would let a user proof mention a divergent prelude function.
    let mut context = Context::new(Duration::from_secs(5));
    let module = module(vec![mentioning("caller", "prelude_partial")]);

    let inherited = BTreeMap::from([(name("prelude_partial"), Totality::Partial)]);
    let classified = classify_module(&mut context, &module, &inherited);
    assert_eq!(classified[&name("caller")], Totality::Partial);

    // And the verdict is inherited, not assumed: the same module against a
    // total prelude name stays total.
    let inherited = BTreeMap::from([(name("prelude_partial"), Totality::Total)]);
    let classified = classify_module(&mut context, &module, &inherited);
    assert_eq!(classified[&name("caller")], Totality::Total);
}

#[test]
fn inherited_partiality_propagates_through_a_local_chain() {
    // `first → second → outside`. The taint has to cross the module boundary
    // once and then travel the local closure, which is the fixpoint doing work
    // a single ordered pass would miss.
    let mut context = Context::new(Duration::from_secs(5));
    let module = module(vec![
        mentioning("first", "second"),
        mentioning("second", "outside"),
    ]);

    let inherited = BTreeMap::from([(name("outside"), Totality::Partial)]);
    let classified = classify_module(&mut context, &module, &inherited);
    assert_eq!(classified[&name("first")], Totality::Partial);
    assert_eq!(classified[&name("second")], Totality::Partial);
}

#[test]
fn stamping_a_module_is_what_the_next_compilation_reads_back() {
    // `record_totality` writes the flag and `recorded_totality` reads it: the
    // round trip the archive relies on to hand a user program the prelude's
    // verdicts without re-analyzing `/std`.
    let mut context = Context::new(Duration::from_secs(5));
    let mut module = module(vec![mentioning("caller", "prelude_partial")]);
    let inherited = BTreeMap::from([(name("prelude_partial"), Totality::Partial)]);

    // Unstamped reads as partial, which is the fail-closed default rather than
    // a verdict. Stamping against a clean prelude is what makes it total, so
    // the flip is evidence the write landed.
    assert_eq!(
        recorded_totality(&module)[&name("caller")],
        Totality::Partial
    );
    record_totality(&mut context, &mut module, &BTreeMap::new());
    assert_eq!(recorded_totality(&module)[&name("caller")], Totality::Total);

    record_totality(&mut context, &mut module, &inherited);
    assert_eq!(
        recorded_totality(&module)[&name("caller")],
        Totality::Partial
    );
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
