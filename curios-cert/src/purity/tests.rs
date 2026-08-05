use {
    crate::{Kernel, fixes_no_value},
    curios_core::{Prim, Term, UniverseContext},
};

fn kernel() -> Kernel {
    let mut kernel = Kernel::new(100_000);
    kernel.set_local_floor(1_000);
    kernel
}

/// A definition whose body reads a cell — the shape every one of these operations actually reaches source in, since `/sys/Cell/get` is a definition and the occurrence carries no `Prim` node of its own.
fn reader(kernel: &mut Kernel) -> Term {
    let read = kernel.fresh(Some("read"));
    let cell = Term::free_var(&kernel.fresh(Some("cell")));

    kernel.define(
        &read,
        &Term::prim(Prim::NatType),
        &Term::prim(Prim::CellGet(Term::prim(Prim::NatType), cell)),
        &UniverseContext::default(),
    );

    Term::free_var(&read)
}

/// The wrapper rung: the name is what must be followed, because the primitive is only ever in the body. This is the objection the reducer-based question was written to answer, and it is the one part of that question this walk keeps.
#[test]
fn an_effect_inside_a_definition_body_is_reached_through_its_name() {
    let mut kernel = kernel();
    let read = reader(&mut kernel);

    assert!(fixes_no_value(&mut kernel, &read));
}

/// A definition with no body of its own, standing for the readable head the two rungs below need: naming it resolves, so the verdict there is about the argument rather than about the callee.
fn callee(kernel: &mut Kernel, hint: &str) -> Term {
    let name = kernel.fresh(Some(hint));

    kernel.define(
        &name,
        &Term::prim(Prim::NatType),
        &Term::prim(Prim::NatType),
        &UniverseContext::default(),
    );

    Term::free_var(&name)
}

/// The rung weak-head reduction cannot reach, and the one the old question got wrong: an application whose head is stuck hands its arguments back untouched, so reducing this term succeeds and reports nothing. Asking the term rather than the reducer is what makes the answer independent of how far it reduces.
///
/// The head is readable, so the head clause is not what decides this — without that the assertion would hold for the wrong reason and the argument would go unread.
#[test]
fn an_effect_in_a_stuck_heads_argument_is_still_reached() {
    let mut kernel = kernel();
    let read = reader(&mut kernel);
    let head = callee(&mut kernel, "f");

    assert!(fixes_no_value(&mut kernel, &Term::apply(head, [read])));
}

/// The control, and it is what keeps the two rungs above from being satisfied by a walk that answers *yes* to everything: a definition chain of the same depth, ending in a value rather than an effect, still refines. Without it a guard that refused every named scrutinee would pass both tests above and silently withdraw every refinement in the standard library.
#[test]
fn a_pure_definition_chain_fixes_a_value() {
    let mut kernel = kernel();

    let inner = kernel.fresh(Some("inner"));
    kernel.define(
        &inner,
        &Term::prim(Prim::NatType),
        &Term::prim(Prim::CellType(Term::prim(Prim::NatType))),
        &UniverseContext::default(),
    );

    let outer = kernel.fresh(Some("outer"));
    kernel.define(
        &outer,
        &Term::prim(Prim::NatType),
        &Term::free_var(&inner),
        &UniverseContext::default(),
    );

    let head = callee(&mut kernel, "f");

    assert!(!fixes_no_value(
        &mut kernel,
        &Term::apply(head, [Term::free_var(&outer)]),
    ));
}

/// The second premise, at the term. Everything written here is pure and everything it names is pure, so the effect search alone answers *fixes a value* — and it is wrong about the value, because the callee is whatever a caller binds to the binder. `curios/src/tests/perimeter.rs`'s `an_effect_behind_a_function_parameter_does_not_refine` is the derivation this refuses.
///
/// Mutation-checked against the control above, which shares its shape and differs only in whether the head resolves: dropping the head clause leaves that one green and fails this.
#[test]
fn a_call_through_a_binder_fixes_no_value() {
    let mut kernel = kernel();
    let binder = Term::free_var(&kernel.fresh(Some("f")));

    assert!(fixes_no_value(
        &mut kernel,
        &Term::apply(binder, [Term::prim(Prim::Bool(true))]),
    ));
}

/// The head clause has to run inside a followed body, not only at the scrutinee. Here the scrutinee's own head is a definition and resolves; what does not is the parameter that definition applies, so a clause reading the outermost head alone would admit `apply(g, x)` and the derivation would come straight back one call deeper.
#[test]
fn a_definition_that_applies_its_parameter_fixes_no_value() {
    let mut kernel = kernel();

    let parameter = kernel.fresh(Some("g"));
    let apply = kernel.fresh(Some("apply"));
    kernel.define(
        &apply,
        &Term::prim(Prim::NatType),
        &Term::apply(Term::free_var(&parameter), [Term::prim(Prim::Bool(true))]),
        &UniverseContext::default(),
    );

    assert!(fixes_no_value(
        &mut kernel,
        &Term::apply(Term::free_var(&apply), [Term::prim(Prim::Bool(true))]),
    ));
}

/// The memo's one hazard, pinned. An edge that closes a cycle is cut, so an answer computed underneath one is about less than the definition actually reaches — and remembering a `false` derived that way would answer a later query about a closure this walk never saw all of. Here `f` reaches `g`, `g` reaches only `f`, and the cell is in `f`: walking `f` therefore computes `g` as *false* on the way through, which must not be remembered, because asking about `g` on its own is asking about a closure that does reach the cell.
///
/// Mutation-checked: remembering every answer regardless leaves the first assertion green and fails the second.
#[test]
fn a_definition_answered_under_a_cycle_is_not_remembered() {
    let mut kernel = kernel();

    let f = kernel.fresh(Some("f"));
    let g = kernel.fresh(Some("g"));
    let cell = Term::free_var(&kernel.fresh(Some("cell")));

    kernel.define(
        &g,
        &Term::prim(Prim::NatType),
        &Term::free_var(&f),
        &UniverseContext::default(),
    );
    kernel.define(
        &f,
        &Term::prim(Prim::NatType),
        &Term::apply(
            Term::free_var(&g),
            [Term::prim(Prim::CellGet(Term::prim(Prim::NatType), cell))],
        ),
        &UniverseContext::default(),
    );

    assert!(fixes_no_value(&mut kernel, &Term::free_var(&f)));
    assert!(fixes_no_value(&mut kernel, &Term::free_var(&g)));
}
