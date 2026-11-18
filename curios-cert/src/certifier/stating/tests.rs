//! What the certifier states of a declaration's binders and payloads: what its own reading of their types answers, whatever the module says.

use {
    super::*,
    crate::test_support::*,
    curios_analysis::test_support::*,
    curios_common::{Qualifier, fill},
    curios_core::{Func, Global, Item, Subterm},
};

fn certifier() -> Certifier<'static> {
    Certifier::new(100_000, SYNTAX)
}

/// `held : Type = ${body}`, under `${context}`: an item whose terms the walk states the binders of. Its type is no part of what is under test.
const HELD: &str = r#"
${context}let /held: Type =
    ${body};
"#;

/// The item [`HELD`] writes at `body`, under no universe context.
fn defined(body: &str) -> Item {
    read_module(&fill!(HELD, context = "", body))
        .items
        .iter()
        .next()
        .cloned()
        .expect("the text declares `held`")
}

/// The body `item` holds.
fn body(item: &Item) -> &Term {
    match item {
        Item::Let(definition) => &definition.body,
        Item::Rec(_) | Item::Nominal(_) => panic!("a definition of its own"),
    }
}

/// The entry of the family named `path` that `text` declares.
fn entry(text: &str, path: &str) -> InductDecl {
    read_module(text).induct_decls()[&Global::Authored(Qualifier::from([path]))].clone()
}

/// A function literal's binders state what the certifier reads of their types, each under the binders before it: `P` at a sort binds no proof, `h` at `P` and `lemma` at a function into `P` bind one, `n` at `Nat` and `F` at a function into a sort none — each the opposite of what the module says.
#[test]
fn a_function_literals_binders_state_what_the_certifier_reads_of_their_types() {
    let stated = stated_marks(
        &mut certifier(),
        &defined(
            "(#{proof = true} P: Prop, h: P, lemma: (k: #NatType) -> P, #{proof = true} n: #NatType, #{proof = true} F: (k: #NatType) -> Type) => n",
        ),
    );

    let Subterm::Func(Func { telescope }) = &**body(&stated) else {
        panic!("a function literal");
    };
    assert_eq!(telescope.proofs(), [false, true, true, false, false]);
}

/// A `let`'s binders state what the certifier reads of their types, each under the binders around it and before it: in `(P: Prop, h: P) => let q : P = h; let m : Nat = 0; m`, one block of two, `q` binds a proof and `m` none, each the opposite of what the module says.
#[test]
fn a_lets_binders_state_what_the_certifier_reads_of_their_types() {
    let stated = stated_marks(
        &mut certifier(),
        &defined("(P: Prop, h: P) => let q: P = h; let #{proof = true} m: #NatType = 0; m"),
    );

    let Subterm::Func(Func { telescope }) = &**body(&stated) else {
        panic!("a function literal");
    };
    let Subterm::Let(let_) = &**telescope.terminal() else {
        panic!("a `let`");
    };
    let tail = let_.tail();
    assert_eq!([tail.binds_proof(0), tail.binds_proof(1)], [true, false]);
}

/// A constructor's binders state what the certifier reads of their types, each under the binders before it: `hold(P: Prop, h: P, n: Nat)` binds a proof at `h` alone, the opposite of what the module says of each.
#[test]
fn a_constructors_binders_state_what_the_certifier_reads_of_their_types() {
    let declaration = entry(
        "induct /Holder: pub Type\n| hold(#{proof = true} P: Prop, h: P, #{proof = true} n: #NatType)\nend",
        "Holder",
    );

    let stated = stated_entry(&mut certifier(), &declaration);

    assert_eq!(
        stated
            .constructor(&"hold".into())
            .expect("the family declares `hold`")
            .proofs(),
        [false, true, false]
    );
}

/// A binder at a sort its declaration leaves to its caller binds no proof: `(A: Sort, a: A) => a` and `box(A: Sort, a: A)` hold a proof at `Prop` and data at `Type`, and a mark is stated once, of the declaration as written, and read at every assignment, so `a` states none in either, the opposite of what the module says.
///
/// Mutation-check: have `as_sort` answer `Prop` for a sort a declaration leaves open, and both state `a` a proof.
#[test]
fn a_binder_at_a_sort_left_to_the_caller_binds_no_proof() {
    let held = read_module(&fill!(
        HELD,
        context = "#{universe_context = {quality_count = 1}}\n",
        body = "(#{proof = true} A: Sort, #{proof = true} a: A) => a"
    ))
    .items
    .iter()
    .next()
    .cloned()
    .expect("the text declares `held`");

    let stated = stated_marks(&mut certifier(), &held);

    let Subterm::Func(Func { telescope }) = &**body(&stated) else {
        panic!("a function literal");
    };
    assert_eq!(telescope.proofs(), [false, false]);

    let declaration = entry(
        "#{universe_context = {quality_count = 1}}\ninduct /Box(A: Sort): pub Type\n| box(#{proof = true} a: A)\nend",
        "Box",
    );

    let stated = stated_entry(&mut certifier(), &declaration);

    assert_eq!(stated.payload_proofs(&"box".into()), Some(vec![false]));
}
