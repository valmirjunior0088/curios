use {
    super::*,
    crate::Kernel,
    curios_base::{Grain, PackedBin, Qualifier},
    curios_core::{Global, MetaId, Metavar},
    std::rc::Rc,
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
    let huge = Shape::unary_run(BigUint::from(u64::MAX), empty());
    let smaller = Shape::unary_run(BigUint::from(u64::MAX - 1), empty());

    assert_eq!(huge.against(&huge), Size::Same);
    assert_eq!(smaller.against(&huge), Size::Less);
    assert_eq!(huge.against(&smaller), NONE);
    assert_eq!(empty().against(&huge), Size::Less);
}

#[test]
fn a_huge_literal_call_argument_grades_without_expansion() {
    // `rec f : (n: Nat) -> Nat = (n) => f(u64::MAX); f` — grading the literal argument must read the packed spine, not peel it: the unary expansion this replaces would loop once per successor, and the value is unbounded by the source that spelled it.
    let mut kernel = Kernel::new(100_000);
    let f = Free::local(1, Some("f"));
    let n = Free::local(2, Some("n"));
    let nat = || Term::intrinsic(Intrinsic::NatType);
    let rec = Term::rec(
        vec![(
            f.clone(),
            Term::func_type([(n.clone(), nat())], nat()),
            Term::func(
                [(n, nat())],
                Term::apply(
                    Term::free_var(&f),
                    [Term::intrinsic(Intrinsic::Nat(Nat::new(u64::MAX)))],
                ),
            ),
        )],
        Term::free_var(&f),
    );
    let Subterm::Rec(Rec { group, .. }) = &*rec else {
        panic!("the fixture changed shape");
    };

    // A constant argument never decreases, so the verdict is `Partial` — promptly.
    assert_eq!(group_totality(&mut kernel, group), Totality::Partial);
}

#[test]
fn a_peeled_prefix_keeps_its_binder_tail() {
    // The `BigNat/add/raw_trimmed` pattern: `b[h, ..t]` rebuilt from arm binders peels its head and sticks on the binder `t`. The remainder must take the full `shape_of` dispatch — a binder reads as its atom — not a force, which cannot move a local and would file the tail as unread, losing the suffix agreement the descent grades by.
    let mut kernel = Kernel::new(100_000);
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
        entered: Vec::new(),
        ops: Vec::new(),
        scopes: Vec::new(),
        calls: Vec::new(),
    };

    let single = Term::intrinsic(Intrinsic::BinAppend(
        Grain::X,
        Term::intrinsic(Intrinsic::Bin(Grain::X, PackedBin::empty())),
        Term::free_var(&h),
    ));
    let cons = Term::intrinsic(Intrinsic::BinConcat(
        Grain::X,
        vec![single, Term::free_var(&t)],
    ));

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
    let mut kernel = Kernel::new(100_000);
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
        entered: Vec::new(),
        ops: Vec::new(),
        scopes: Vec::new(),
        calls: Vec::new(),
    };
    walk.walk(&body);

    // The call passes the parameter itself, so it grades `Same` — a grade only a walk that reached the bottom of the nest can record.
    assert_eq!(walk.calls.len(), 1);
    assert_eq!(walk.calls[0].2.entry(0, 0), SAME);
}

#[test]
fn runs_merge_so_a_refined_tail_stays_one_chain() {
    // `match n` refines `n` to a 1-run over `pred`; a nested arm refining `pred` to a 4-run must expand to the single 5-run the chain denotes, or a literal argument could never grade against it.
    let empty = || Shape::Node(Tag::Empty(Carriers::Unary), Vec::new());
    let merged = Shape::unary_run(
        BigUint::from(1usize),
        Shape::unary_run(BigUint::from(4usize), empty()),
    );

    assert!(merged.same_as(&Shape::unary_run(BigUint::from(5usize), empty())));
    assert_eq!(
        Shape::unary_run(BigUint::from(3usize), empty()).against(&merged),
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

    let refined = Shape::unary_run(BigUint::from(1usize), Shape::Atom(n));
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
        literal: BigUint::from(literal),
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
    assert!(!guard(Relation::Gte, 0).establishes_nonzero(true));
    assert!(!guard(Relation::Lt, 0).establishes_nonzero(false));

    // A strict lower bound excludes zero whatever the literal is.
    assert!(guard(Relation::Gt, 0).establishes_nonzero(true));
    assert!(guard(Relation::Lte, 0).establishes_nonzero(false));
}

#[test]
fn a_guard_written_with_its_literal_first_reads_the_same() {
    // `10 > n` is `n < 10`, so the arm that excludes zero is the same one.
    assert_eq!(Relation::Gt.flipped(), Relation::Lt);
    assert_eq!(Relation::Lte.flipped(), Relation::Gte);
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

/// The member every probe below plants a call to.
fn planted() -> Free {
    Free::local(1, Some("f"))
}

/// Whether the engine finds a nullary self-call planted at one child position.
///
/// The group is `rec f : Nat = <plant(f)>`, whose member takes no lambda and so has an empty parameter vector: a self-call from it is a 0x0 matrix, idempotent with no diagonal, so the verdict is `Partial` exactly when [`Walk::walk`] reached the planted occurrence and `Total` exactly when it did not. Nothing here needs to be well typed — the engine is a total function of post-zonk terms and types nothing — which is what lets one probe cover every child position of every variant.
fn call_is_seen(body: Term) -> bool {
    let mut kernel = Kernel::new(100_000);
    let rec = Term::rec(
        vec![(planted(), Term::intrinsic(Intrinsic::NatType), body)],
        Term::free_var(&planted()),
    );
    let Subterm::Rec(Rec { group, .. }) = &*rec else {
        panic!("the fixture changed shape");
    };

    group_totality(&mut kernel, group) == Totality::Partial
}

/// The differential `documentation/SOUNDNESS.md` asks for: `Walk::walk` visits every child position `Subterm::any_child_term` does, minus a named whitelist.
///
/// This matters more here than anywhere else on the perimeter because this is the only analysis whose blindness *admits*. Every other one refuses when it cannot see — positivity answers `Mixed` at an out-of-set name, `whnf` goes stuck, inversion derives nothing at a `Prop`-valued position, an under-applied call is graded `Matrix::unknown` — while a call site the walk never visits contributes no edge, and a group with no edges is `Total`. Route eight of this row's history was exactly that and nothing else: a projected inner group went unwalked, `rec f(n) -> False = (rec g(m) -> False = f(m); g)(n)` closed to no call whatsoever, both groups classified `Total`, and `f(0)` diverged through `g` while (V) read the verdict.
///
/// The probe needs no instrumentation because the engine types nothing: it is a total function of post-zonk terms, so an ill-typed fixture is a legitimate input. Each row plants a *nullary* self-call at one child position — the member takes no lambda, so a self-call from it is a 0x0 matrix, idempotent with no diagonal — which makes the verdict `Partial` exactly when the walk reached the plant and `Total` exactly when it did not.
///
/// **The whitelist is three, where the prose audit this replaces counted two.** Its two are a `Metavar`'s spine, which `zonk_module` refuses before this pass runs, and a nested `Rec` member's declared type. The third is a separate mechanism the audit names but folds into the second: `Member::of` peels the member body's *leading* lambdas and discards their domain annotations at `Telescope::Cons(_, rest)`, so a call planted there is invisible while the same lambda one node deeper is walked — which is why every row below is wrapped in a tuple, and why the two spellings are asserted against each other at the end.
///
/// All three are type positions, and the argument that they are safe is the audit's: a call in one is consumed by β or read only by typing, never reduced, and an edge the engine misses is dangerous only where it can complete a reduction cycle. That argument is not what this test checks. What it checks is that the set does not quietly grow — a new term-bearing field on an existing variant is absorbed by the `..` in both patterns without a compile error, and would otherwise widen what the engine accepts in silence.
#[test]
fn the_walk_reaches_every_child_position_but_the_three_it_documents() {
    let filler = || Term::intrinsic(Intrinsic::NatType);
    let zero = || Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)));
    let name = || Global::Authored(Qualifier::from(["N"]));
    let binder = |index: u32| Free::local(500 + index, None);
    let call = || Term::free_var(&planted());

    // Every child position `Subterm::any_child_term` visits, and what the walk must say about it.
    let positions: Vec<(&str, Term, bool)> = vec![
        (
            "a universe instance's head",
            Subterm::UniverseInst(UniverseInst {
                head: call(),
                levels: Vec::new(),
            })
            .into(),
            true,
        ),
        (
            "an intrinsic's operand",
            Term::intrinsic(Intrinsic::ListType(call())),
            true,
        ),
        (
            "a lambda's domain",
            Term::func([(binder(1), call())], zero()),
            true,
        ),
        (
            "a lambda's body",
            Term::func([(binder(1), filler())], call()),
            true,
        ),
        (
            "a function type's domain",
            Term::func_type([(binder(1), call())], zero()),
            true,
        ),
        (
            "a function type's codomain",
            Term::func_type([(binder(1), filler())], call()),
            true,
        ),
        ("an application's head", Term::apply(call(), [zero()]), true),
        (
            "an application's argument",
            Term::apply(zero(), [call()]),
            true,
        ),
        (
            "a tuple type's component",
            Term::tuple_type(vec![(binder(1), call())]),
            true,
        ),
        ("a tuple's field", Term::tuple([call()]), true),
        ("a projection's head", Term::proj(call(), 0), true),
        (
            "a nominal type's parameter",
            Term::induct_type(name(), [call()], Vec::<Term>::new()),
            true,
        ),
        (
            "a nominal type's index",
            Term::induct_type(name(), Vec::<Term>::new(), [call()]),
            true,
        ),
        (
            "a constructor's parameter",
            Term::variant(name(), [call()], "mk", Vec::<Term>::new()),
            true,
        ),
        (
            "a constructor's payload",
            Term::variant(name(), Vec::<Term>::new(), "mk", [call()]),
            true,
        ),
        (
            "a structure type's parameter",
            Subterm::StructType(StructType {
                name: name(),
                universes: Vec::new(),
                params: vec![call()],
            })
            .into(),
            true,
        ),
        (
            "a structure literal's parameter",
            Term::struct_(name(), [call()], Vec::<Term>::new()),
            true,
        ),
        (
            "a structure literal's field",
            Term::struct_(name(), Vec::<Term>::new(), [call()]),
            true,
        ),
        (
            "a match's scrutinee",
            Term::bool_match(call(), None, filler(), zero(), zero()),
            true,
        ),
        (
            "a match's motive",
            Term::bool_match(zero(), None, call(), zero(), zero()),
            true,
        ),
        (
            "a boolean arm",
            Term::bool_match(zero(), None, filler(), call(), zero()),
            true,
        ),
        (
            "a let binding's type",
            Term::let_(&binder(1), call(), zero(), zero()),
            true,
        ),
        (
            "a let binding's value",
            Term::let_(&binder(1), filler(), call(), zero()),
            true,
        ),
        (
            "a let's tail",
            Term::let_(&binder(1), filler(), zero(), call()),
            true,
        ),
        (
            "a nested group's member body",
            Term::rec(vec![(binder(1), filler(), call())], zero()),
            true,
        ),
        (
            "a nested group's tail",
            Term::rec(vec![(binder(1), filler(), zero())], call()),
            true,
        ),
        // The two positions the audit names, and the reason each is safe to skip.
        (
            "a metavariable's spine",
            Subterm::Metavar(Metavar {
                id: MetaId::from(0usize),
                spine: Rc::new(vec![call()]),
                origin: None,
            })
            .into(),
            false,
        ),
        (
            "a nested group's member type",
            Term::rec(vec![(binder(1), call(), zero())], zero()),
            false,
        ),
    ];

    // The harness itself, first: a bare self-call must be seen and a call-free body must not, or every row below passes for the wrong reason.
    assert!(call_is_seen(call()), "the bare self-call was not seen");
    assert!(
        !call_is_seen(zero()),
        "a call-free body was read as recursive"
    );

    // Wrapped in a tuple so the member body is never itself a lambda: `Member::of` peels a *leading* lambda and discards its domain annotations, which is a separate divergence with its own row below, and leaving it in the way would hide what `Walk::step` does with every other position.
    for (position, plant, expected) in positions {
        assert_eq!(
            call_is_seen(Term::tuple([plant])),
            expected,
            "{position}: the walk {} the planted self-call",
            if expected { "missed" } else { "reached" },
        );
    }

    // The third divergence, stated as the pair that separates it from `Walk::step`: the same lambda is invisible at the top of a member body and visited one node deeper.
    let lambda = || Term::func([(binder(1), call())], zero());
    assert!(
        !call_is_seen(lambda()),
        "a peeled lambda domain: the walk reached the planted self-call",
    );
    assert!(
        call_is_seen(Term::tuple([lambda()])),
        "a lambda domain below the peel: the walk missed the planted self-call",
    );
}
