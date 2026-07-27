//! End-to-end coverage for strict positivity modulo polarity.
//!
//! The check exists to make `induct` and `struct` sound: an inductive
//! declaration claims its functor has an initial algebra, and without the gate
//! `induct Bad | c(f : (Bad) -> False) end` inhabits `False`. The lattice
//! itself is unit-tested in `curios-core/src/positivity/tests.rs`; these check
//! what a *user* can observe.
//!
//! Every shape here is one the standard library already relies on. The prelude
//! build exercises them through the from-scratch elaboration path, so it fails
//! loudly on a regression; these run the same shapes through the prelude-replay
//! path a user program actually takes, where the analysis sees only the user
//! suffix and reads the prelude's polarity vectors back from the archive.

use {super::run, curios_runtime::MockHost, std::time::Duration};

fn rejected(source: &str) {
    let (system, _io) = MockHost::builder().build();
    assert!(
        crate::run_text(Duration::from_secs(10), source, system).is_err(),
        "expected the declaration to be rejected",
    );
}

// The rule most likely to be implemented wrongly, and the one that would take
// the whole concurrency stack with it. A nullary `() -> X` is an *empty*
// parameter list, so nothing flips and `X` stays strictly positive — a
// zero-argument function is a thunk of its result. Read as "an occurrence
// under an arrow is negative", this rejects `/std/Async`.
#[test]
fn a_nullary_arrow_codomain_is_a_strictly_positive_payload() {
    let source = r#"
        use /std/{Nat};

        induct Susp(A : Type) : pub Type
        | now(A)
        | later(() -> Susp(A))
        end

        let delayed : Susp(Nat) = Susp/later(() => Susp/now(7));

        match delayed
        | now(_) => /std/print("no")
        | later(_) => /std/print("thunked")
        end
        "#;
    assert_eq!(run(source), b"thunked");
}

// `/std/Pause` and `/std/Async` in miniature: two declarations in one `and`
// block, each reaching the other, with the recursive occurrence behind a
// thunk. The group boundary lives on `Item::Rec` and not on the registry
// entry, so this passes only because the occurrence relation is closed
// transitively rather than by asking who shares a declaration block.
#[test]
fn a_mutually_recursive_group_reaching_itself_through_a_thunk_is_admitted() {
    let source = r#"
        induct Step : Type
        | halt()
        | again(next : () -> Machine)
        and Machine : Type
        | done()
        | more(step : Step, rest : () -> Machine)
        end

        match Machine/more(Step/halt(), () => Machine/done())
        | done() => /std/print("no")
        | more(_, _) => /std/print("mutual")
        end
        "#;
    assert_eq!(run(source), b"mutual");
}

// `Json/arr(Lst(Json))`: a list is a finite product of its element, so the
// primitive is covariant and a strict occurrence stays strict. Without
// polarity on primitives this shape is rejected outright.
#[test]
fn recursion_through_a_covariant_primitive_is_admitted() {
    let source = r#"
        use /std/{Lst};

        induct Tree : pub Type
        | leaf()
        | node(kids : Lst(Tree))
        end

        let branch : Tree = Tree/node(Lst/cons(Tree/leaf(), Lst/nil()));

        match branch
        | leaf() => /std/print("no")
        | node(_) => /std/print("listed")
        end
        "#;
    assert_eq!(run(source), b"listed");
}

// `Json/obj(Lst({Str, Json}))`: polarity has to travel through an anonymous
// tuple type as well as the primitive, since a product is as positive as its
// factors.
#[test]
fn recursion_through_an_anonymous_tuple_is_admitted() {
    let source = r#"
        use /std/{Lst, Nat};

        induct Doc : pub Type
        | atom(Nat)
        | fields(Lst({Nat, Doc}))
        end

        match Doc/fields(Lst/nil())
        | atom(_) => /std/print("no")
        | fields(_) => /std/print("tupled")
        end
        "#;
    assert_eq!(run(source), b"tupled");
}

// `/std/Toml`, whose recursion travels `Toml → Map(Toml) → struct field →
// Option → Node`. This is the shape that makes "modulo polarity" load-bearing
// on day one rather than a later refinement: a check that only recognizes
// recursive occurrences in immediate payload positions rejects it.
#[test]
fn recursion_through_a_struct_into_a_parameterized_inductive_is_admitted() {
    let source = r#"
        use /std/{Nat, Option};

        induct Node(V : Type) : pub Type
        | leaf(key : Nat, value : V)
        | fork(zero : Node(V), one : Node(V))
        end

        struct Table(V : Type) : pub Type {
            size : Nat,
            root : Option(Node(V)),
        }

        induct Value : pub Type
        | num(Nat)
        | table(Table(Value))
        end

        match Value/table(Table { size = 0, root = Option/none() })
        | num(_) => /std/print("no")
        | table(_) => /std/print("three hops")
        end
        "#;
    assert_eq!(run(source), b"three hops");
}

// `Node(V)` mentions itself, so its own parameter vector is an input to
// computing that vector. A single ordered pass reads `V` as `Unused` and then
// composes `Value → Table → Node` through a parameter it wrongly believes is
// never inspected; only running the two fixpoints to stability gets `Strict`.
#[test]
fn a_self_mentioning_parameter_needs_the_fixpoint_to_settle() {
    let source = r#"
        use /std/{Nat};

        induct Rose(A : Type) : pub Type
        | tip(A)
        | fork(left : Rose(A), right : Rose(A))
        end

        induct Holder : pub Type
        | held(Rose(Holder))
        | empty()
        end

        match Holder/held(Rose/tip(@Holder, Holder/empty()))
        | held(_) => /std/print("settled")
        | empty() => /std/print("no")
        end
        "#;
    assert_eq!(run(source), b"settled");
}

// `syn/Str`'s `Utf8`, which recurses at an index computed from its own
// payload. Indices are walked opaquely — an inductive is not uniform in them —
// so what has to survive is the *payload* occurrence, not the index.
#[test]
fn an_indexed_family_recursing_at_a_computed_index_is_admitted() {
    let source = r#"
        use /std/{Nat};

        induct Run : (n : Nat) -> pub Type
        | stop() : (0)
        | more(@m : Nat, rest : Run(m)) : (m + 1)
        end

        let two : Run(2) = Run/more(Run/more(Run/stop()));

        match two : (k, r) => {}
        | more(@_, _) => /std/print("indexed")
        end
        "#;
    assert_eq!(run(source), b"indexed");
}

// An inductive's *index binder types* describe the family's arity, not its
// carrier, so they contribute no polarity of their own — `Eq(@A : Type) : (x :
// A, y : A)` is `Strict` in `A` because `refl(@z : A)` has an `A` payload.
// Walking `x : A` on top of that costs the vector its precision and rejects
// this declaration, which is sound.
//
// Nothing is lost by skipping them: a declaration cannot reach itself there.
// `induct Foo : (x : Foo) -> Type` does not elaborate, because `x : Foo`
// requires `Foo` to already be a type and it is a family until applied to the
// very index being declared.
#[test]
fn recursion_beside_a_propositional_equality_over_the_declaration_is_admitted() {
    let source = r#"
        use /std/{Eq};

        induct Wit : pub Type
        | base()
        | tied(a : Wit, b : Wit, p : Eq(a, b))
        end

        match Wit/tied(Wit/base(), Wit/base(), Eq/refl())
        | base() => /std/print("no")
        | tied(_, _, _) => /std/print("equated")
        end
        "#;
    assert_eq!(run(source), b"equated");
}

// `struct Waker : Type { () -> {} }` inside `/std/Pause`'s `park` payload: a
// genuine negative position, and sound only because `Waker` mentions neither
// member of the group. The check must reject on the *path back*, not on the
// mere presence of a negative occurrence.
#[test]
fn a_negative_occurrence_of_an_unrelated_type_is_admitted() {
    let source = r#"
        struct Waker : pub Type { () -> {} }

        induct Event : pub Type
        | idle()
        | park((Waker) -> {})
        end

        match Event/idle()
        | idle() => /std/print("unrelated")
        | park(_) => /std/print("no")
        end
        "#;
    assert_eq!(run(source), b"unrelated");
}

// The whole reason the gate exists. `Bad` is not the initial algebra of any
// functor — the payload is a function *out of* `Bad` — and admitting it hands
// back an eliminator that inhabits `False` in four lines with no recursion.
#[test]
fn a_negative_occurrence_is_rejected() {
    rejected(
        r#"
        use /syn/{False};

        induct Bad : pub Type
        | c(f : (Bad) -> False)
        end

        /std/print("unreachable")
        "#,
    );
}

// Positive but not strictly positive: two arrows, so the sign flips back. This
// records the impredicative-`Prop` decision as a test rather than as prose —
// with an impredicative `Prop` and a universe hierarchy both present, the
// Coquand–Paulin construction applies, so the merely-positive relaxation other
// systems allow is not available here.
#[test]
fn a_positive_but_not_strictly_positive_occurrence_is_rejected() {
    rejected(
        r#"
        use /syn/{False};

        induct Bad2 : pub Type
        | c(f : ((Bad2) -> False) -> False)
        end

        /std/print("unreachable")
        "#,
    );
}

// The composition case, and the one a check without polarity vectors would
// miss entirely: `Sink` is contravariant in its parameter, so `Trap`'s payload
// puts `Trap` left of an arrow one indirection away. Nothing about `Trap`'s own
// constructor looks negative — the rejection comes from `Sink`'s vector.
#[test]
fn a_negative_occurrence_borrowed_through_another_declaration_is_rejected() {
    rejected(
        r#"
        use /std/{Nat};

        induct Sink(A : Type) : pub Type
        | drain(f : (A) -> Nat)
        end

        induct Trap : pub Type
        | caught(Sink(Trap))
        end

        /std/print("unreachable")
        "#,
    );
}

// A cycle whose negative step is in the *other* member. Neither declaration is
// negative on its own inspection, and the group boundary is not on the registry
// entry, so this is caught only by closing the occurrence relation
// transitively.
#[test]
fn a_negative_cycle_through_a_mutual_group_is_rejected() {
    rejected(
        r#"
        use /syn/{False};

        induct Left : Type
        | wrap(Right)
        and Right : Type
        | back(f : (Left) -> False)
        end

        /std/print("unreachable")
        "#,
    );
}

// A struct is checked on the same footing as an inductive: it is a nominal
// record, so a field that consumes the record it belongs to is the same
// unsoundness wearing different syntax.
#[test]
fn a_negative_struct_field_is_rejected() {
    rejected(
        r#"
        use /syn/{False};

        struct Consume : pub Type {
            run : (Consume) -> False,
        }

        /std/print("unreachable")
        "#,
    );
}

// `Cell` is invariant — it is read *and* written — so nothing recursive may
// travel through one, even though the occurrence looks like a plain payload.
#[test]
fn recursion_through_an_invariant_primitive_is_rejected() {
    rejected(
        r#"
        use /std/{Cell};

        induct Knot : pub Type
        | tie(Cell(Knot))
        end

        /std/print("unreachable")
        "#,
    );
}
