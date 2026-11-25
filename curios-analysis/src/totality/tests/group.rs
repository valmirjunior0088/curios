//! A group judged by the closure of its calls: a descent on either argument, a nullary self call, mutual recursion, a closure past its limit, a ring, and the bodies and towers the walk reads or passes over.

use {
    super::test_support::*,
    crate::totality::*,
    curios_core::{Atom, CallMatrix, Free, Intrinsic, Size, Subterm, Term},
    curios_num::Binary,
};

/// A member `f : Nat` whose body is itself, for the probes below to call: the group, and a call of its one member at `argument`.
fn member(argument: Term) -> (RecGroup, Term) {
    let f = Free::local(1, Some("f"));
    let rec = Term::rec(
        vec![(f, Term::intrinsic(Intrinsic::NatType), Term::free_var(&f))],
        Term::free_var(&f),
    );
    let Subterm::Rec(block) = &*rec else {
        panic!("the fixture changed shape");
    };
    let group = block.group();

    (
        group.clone(),
        Term::apply(Term::rec_proj(group.clone(), 0), [argument]),
    )
}

/// A ring of members, each calling the next on its one parameter with a strict descent: its closure holds one call for every ordered pair of them.
fn ring(members: usize) -> Vec<Call> {
    (0..members)
        .map(|member| call(member, (member + 1) % members, matrix(&[&[LESS]])))
        .collect()
}

/// A member's eight parameters handed on permuted, the `row`th graded strictly below by the argument at `to[row]`.
fn permuted(to: [usize; 8]) -> CallMatrix {
    let mut permuted = CallMatrix::unknown(8, 8);
    for (row, column) in to.into_iter().enumerate() {
        permuted.set(row, column, LESS);
    }

    permuted
}

/// Levels of written nesting in the deep body below.
///
/// Far past the depth an unguarded recursion survives on the default test-thread stack, so a walk that stops growing its stack aborts the process rather than merely running long.
const DEEP: usize = 100_000;

/// How many calls a walk of `body` records for the one member of `group`, whose parameter is `n`.
fn calls_in(group: &RecGroup, n: Free, body: &Term) -> usize {
    graded_in(group, n, body).len()
}

/// The calls a walk of `body` records for the one member of `group`, whose parameter is `n`, each as it was graded.
fn graded_in(group: &RecGroup, n: Free, body: &Term) -> Vec<Call> {
    let mut certifier = Unreducing::default();
    let params = [n];
    let parts = one_part();
    let arities = vec![1];
    let mut walk = Walk {
        driver: &mut certifier,
        group: Some(group),
        parts: &parts,
        arities: &arities,
        caller: 0,
        params: &params,
        context: SizeContext::default(),
        entered: Vec::new(),
        walked: HashMap::new(),
        calls: Vec::new(),
        tracked: None,
    };
    walk.walk(&Part {
        shape: parts[0].clone(),
        params: params.to_vec(),
        runs: vec![body.clone()],
        answers: Vec::new(),
        held: None,
    });

    walk.calls
}

/// The parts of a group of one member taking one parameter, whose body is no record.
fn one_part() -> Vec<PartShape> {
    vec![PartShape {
        member: 0,
        path: Vec::new(),
        arity: 1,
    }]
}

/// Sixty `let`s over `first`, each binding `line` of the one before, closed by `tail` of the last.
fn tower(first: Term, line: impl Fn(Term) -> Term, tail: impl FnOnce(Term) -> Term) -> Term {
    let names = (0..=60)
        .map(|index| Free::local(100 + index, Some("x")))
        .collect::<Vec<_>>();
    let nat = Term::intrinsic(Intrinsic::NatType);
    let mut items = vec![(names[0], nat.clone(), first)];
    for index in 1..=60 {
        items.push((
            names[index],
            nat.clone(),
            line(Term::free_var(&names[index - 1])),
        ));
    }

    Term::let_block(items, tail(Term::free_var(&names[60])))
}

#[test]
fn a_descent_on_either_argument_is_accepted_only_because_arms_refine_the_scrutinee() {
    // The three call matrices of an addition of two packed words, `add(x, y, carry)`, which descends on either word by arm. In the empty-`x` arm the literal argument `0x[]` grades `Same` against `x` *because* the arm refined `x` to `0x[]`.
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
        .map(|matrix| call(0, 0, matrix.clone()))
        .collect::<Vec<_>>();
    let closed = close(calls).expect("the closure stays small");
    assert!(
        closed
            .iter()
            .all(|call| !call.matrix.is_idempotent() || call.matrix.descends())
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
    let empty = CallMatrix::unknown(0, 0);
    assert!(empty.is_idempotent());
    assert!(!empty.descends());
}

#[test]
fn mutual_recursion_is_caught_only_by_the_closure() {
    // Neither leg is a cycle on its own; only `a → b → a` is, and it descends.
    let forward = matrix(&[&[LESS]]);
    let backward = matrix(&[&[SAME]]);
    let closed =
        close(vec![call(0, 1, forward), call(1, 0, backward)]).expect("the closure stays small");

    let cycles = closed
        .iter()
        .filter(|call| call.caller == call.callee && call.matrix.is_idempotent())
        .collect::<Vec<_>>();
    assert!(!cycles.is_empty());
    assert!(cycles.iter().all(|call| call.matrix.descends()));
}

// The closure's bound turns a verdict into a refusal and into nothing else: a group whose closure outgrows it is partial whatever it descends on. One member that hands its eight parameters on swapped and rotated, each with a strict descent, closes to every permutation of them, 40 320 matrices of which each that repeats descends, and the bound stops the closure short. Mutation-checked both ways: with the bound lifted the group is judged total, and answering total past the bound certifies it unjudged.
#[test]
fn a_closure_past_its_limit_claims_nothing() {
    let swapped = permuted([1, 0, 2, 3, 4, 5, 6, 7]);
    let rotated = permuted([1, 2, 3, 4, 5, 6, 7, 0]);

    assert_eq!(
        decide(vec![call(0, 0, swapped), call(0, 0, rotated)]),
        Totality::Unfounded
    );
}

// Members that all reach each other close to a call for every ordered pair of them, so the bound leaves a ring of any size its closure: the ring one member past what the fixed part alone holds is judged, and so is one of twice its side. Mutation-checked: under the fixed part alone, either ring is partial.
#[test]
fn a_ring_is_judged_whatever_its_size() {
    let side = CLOSURE_LIMIT.isqrt();

    assert_eq!(decide(ring(side + 1)), Totality::Total);
    assert_eq!(decide(ring(2 * side)), Totality::Total);
}

#[test]
fn a_peeled_prefix_keeps_its_binder_tail() {
    // A cons `0x[h, ..t]` rebuilt from arm binders peels its head and sticks on the binder `t`. The remainder must take the full `shape_of` dispatch — a binder reads as its atom — not a force, which cannot move a local and would file the tail as unread, losing the suffix agreement the descent grades by.
    let mut certifier = Unreducing::default();
    let h = Free::local(2, Some("h"));
    let t = Free::local(3, Some("t"));
    let context = SizeContext::default();
    let mut grader = Grader::new(&mut certifier, &context);

    let single = Term::intrinsic(Intrinsic::BytesAppend {
        bytes: Term::intrinsic(Intrinsic::Bytes(Binary::empty())),
        element: Term::free_var(&h),
    });
    let cons = Term::intrinsic(Intrinsic::BytesConcat {
        operands: vec![single, Term::free_var(&t)],
    });

    let Ok(shape) = grader.shape_of(&cons);
    assert!(
        shape.same_as(&Shape::elem_run(
            Carriers::Bytes,
            vec![Shape::Atom(h)],
            Shape::Atom(t),
        )),
        "expected a one-element run over the binder tail, got {shape:?}"
    );
}

#[test]
fn a_body_nested_deeper_than_an_unguarded_recursion_survives_is_walked() {
    // `(((… f(n) …)))`: a program-generated tuple nest, the mild sibling of a packed literal — bounded by the size of the source that spelled it rather than by a value, and unbounded by anything the walk controls. The nest is inert to the size order (no arm, no refinement, nothing to force), so what it measures is the traversal's own depth, and the graded call at the bottom is the evidence the walk arrived there.
    let mut certifier = Unreducing::default();
    let f = Free::local(1, Some("f"));
    let rec = Term::rec(
        vec![(f, Term::intrinsic(Intrinsic::NatType), Term::free_var(&f))],
        Term::free_var(&f),
    );
    let Subterm::Rec(block) = &*rec else {
        panic!("the fixture changed shape");
    };
    let group = block.group();

    let n = Free::local(2, Some("n"));
    let mut body = Term::apply(Term::rec_proj(group.clone(), 0), [Term::free_var(&n)]);
    for _ in 0..DEEP {
        body = Term::tuple([body]);
    }

    let params = [n];
    let parts = one_part();
    let arities = vec![1];
    let mut walk = Walk {
        driver: &mut certifier,
        group: Some(group),
        parts: &parts,
        arities: &arities,
        caller: 0,
        params: &params,
        context: SizeContext::default(),
        entered: Vec::new(),
        walked: HashMap::new(),
        calls: Vec::new(),
        tracked: None,
    };
    walk.walk(&Part {
        shape: parts[0].clone(),
        params: params.to_vec(),
        runs: vec![body.clone()],
        answers: Vec::new(),
        held: None,
    });

    // The call passes the parameter itself, so it grades `Same` — a grade only a walk that reached the bottom of the nest can record.
    assert_eq!(walk.calls.len(), 1);
    assert_eq!(walk.calls[0].matrix.entry(0, 0), SAME);
}

#[test]
fn an_application_of_a_constructor_payload_grades_below_the_constructor() {
    // The accessibility shape: `a` refined by an arm to `intro(w, below)`, and the call argument `below(y, r)`. A function-typed payload is a branching node whose children are its applications, so the application reads as the payload it came from and grades below the constructor for the reason the payload does. The control is the same application with `below` bound anywhere but a constructor pattern — outside the set, the head is a stuck binder the reduce-nothing environment cannot move, and the argument stays unread.
    let mut certifier = Unreducing::default();
    let a = Free::local(2, Some("a"));
    let w = Free::local(3, Some("w"));
    let below = Free::local(4, Some("below"));
    let y = Free::local(5, Some("y"));
    let r = Free::local(6, Some("r"));
    let intro = Shape::node(
        Tag::Variant(Atom::from("intro")),
        [Shape::Atom(w), Shape::Atom(below)],
    );
    let mut context = SizeContext::default();
    context.open(Some((a, intro.clone())), None, vec![below]);
    let applied = Term::apply(
        Term::free_var(&below),
        [Term::free_var(&y), Term::free_var(&r)],
    );

    let mut grader = Grader::new(&mut certifier, &context);
    let parameter = grader.expand(&a, EXPAND_FUEL);
    let Ok(shape) = grader.shape_of(&applied);
    assert_eq!(shape.against(&parameter), LESS);

    let mut context = SizeContext::default();
    context.open(Some((a, intro)), None, Vec::new());
    let mut grader = Grader::new(&mut certifier, &context);
    let Ok(shape) = grader.shape_of(&applied);
    assert_eq!(shape.against(&parameter), Size::Unknown);
}

/// A tower that holds no call is passed over. The walk reads a `let`'s tail with each binder standing for its value, so sixty lines each pairing the one before with itself hand it a tail of 2⁶⁰ leaves; a term that holds no group holds no call, and the one call beside the tower is all the walk finds.
///
/// Mutation-checked: with every term entered whether or not it holds a group, the walk does not return.
#[test]
fn a_tower_that_holds_no_call_is_passed_over() {
    let n = Free::local(2, Some("n"));
    let (group, call) = member(Term::free_var(&n));
    let body = tower(
        Term::free_var(&n),
        |before| Term::tuple([before.clone(), before]),
        |last| Term::tuple([last, call]),
    );

    assert_eq!(calls_in(&group, n, &body), 1);
}

/// A tower whose every line calls the member is walked once per line: a value is walked where its `let` binds it, and a copy of it the tail holds is passed over, its calls being the ones already recorded graded under a context that only grew.
///
/// Mutation-checked: with a walked value entered again wherever the tail holds it, the walk does not return.
#[test]
fn a_value_walked_where_it_is_bound_is_passed_over_in_the_tail() {
    let n = Free::local(2, Some("n"));
    let (group, call) = member(Term::free_var(&n));
    let Subterm::Apply(apply) = &*call else {
        panic!("the fixture changed shape");
    };
    let head = apply.head.clone();
    let body = tower(
        call.clone(),
        |before| Term::apply(head.clone(), [Term::tuple([before.clone(), before])]),
        |last| last,
    );

    assert_eq!(calls_in(&group, n, &body), 61);
}

/// A tower handed whole to the member is read in its size. The call's argument is graded as a shape, and sixty lines each pairing the one before with itself are a graph of sixty nodes whose tree has 2⁶⁰ leaves: a reader that remembers each term's shape, and a shape that shares what it holds, read a line for a line.
///
/// Mutation-checked: with a term's shape read again wherever the term is met, the walk does not return.
#[test]
fn a_tower_handed_to_the_member_is_read_in_its_size() {
    let n = Free::local(2, Some("n"));
    let (group, call) = member(Term::free_var(&n));
    let Subterm::Apply(apply) = &*call else {
        panic!("the fixture changed shape");
    };
    let head = apply.head.clone();
    let body = tower(
        Term::free_var(&n),
        |before| Term::tuple([before.clone(), before]),
        |last| Term::apply(head.clone(), [last]),
    );

    assert_eq!(calls_in(&group, n, &body), 1);
}

/// A call in a `let`'s value reads the bindings before it, as one in its tail does. `let same = n; let r = f(same); r` hands the member its own parameter, so the call is graded `Same`; a value walked as it is stored names the binding before it by a position nothing stands at, and the call reads as unrelated to every parameter.
///
/// Mutation-checked: with a binding's value walked as stored, the call is graded as unrelated.
#[test]
fn a_call_in_a_lets_value_reads_the_bindings_before_it() {
    let n = Free::local(2, Some("n"));
    let same = Free::local(3, Some("same"));
    let r = Free::local(4, Some("r"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let (group, _) = member(Term::free_var(&n));
    let call = Term::apply(Term::rec_proj(group.clone(), 0), [Term::free_var(&same)]);
    let body = Term::let_block(
        vec![(same, nat.clone(), Term::free_var(&n)), (r, nat, call)],
        Term::free_var(&r),
    );

    let calls = graded_in(&group, n, &body);
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].matrix.entry(0, 0), Size::Same);
}

/// A walked value is passed over in its own `let`'s tail and nowhere else: a sibling position holds the same call under another context, which the value's walk did not grade it under.
///
/// Mutation-checked: with a value kept past the tail it was walked for, a call after it is lost.
#[test]
fn a_walked_value_is_forgotten_past_its_tail() {
    let n = Free::local(2, Some("n"));
    let b = Free::local(3, Some("b"));
    let x = Free::local(4, Some("x"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let (group, call) = member(Term::free_var(&n));
    // The call three times: bound by a `let` in one arm, alone in the other, and again after the match — the last so that the fixture does not rest on which arm is walked first.
    let body = Term::tuple([
        Term::bool_match(
            Term::free_var(&b),
            nat.clone(),
            call.clone(),
            Term::let_(&x, nat, call.clone(), Term::free_var(&x)),
        ),
        call,
    ]);

    assert_eq!(calls_in(&group, n, &body), 3);
}
