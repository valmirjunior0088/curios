//! Polarity travelling through a type former, which is the whole of what "modulo polarity" buys.
//!
//! A check recognizing recursive occurrences only in immediate payload positions rejects every shape here, `/std/Toml` and `/std/Async` among them. Each row names the former whose polarity vector the occurrence composes against.

use crate::tests::run;

// A description of an `X` is the delayed `X` its thunk erasure makes it, so `X` under `Io` stays strictly positive — the same reading as `List`, and for a stronger reason: `bind` never hands back the result except inside another `Io`, and there is no eliminator that does. This is the rule a suspension whose continuation is computed by performing an effect rests on.
#[test]
fn an_io_payload_is_a_strictly_positive_occurrence() {
    let source = r#"
        use /std/{Nat, Io};

        induct Step(A : Type) : pub Type
        | done(A)
        | more(next : Io(Step(A)))
        end

        let machine : Step(Nat) = Step/more(Io/pure(Step/done(7)));

        match machine
        | done(_) => /std/print("no")
        | more(_) => /std/print("suspended")
        end
        "#;
    assert_eq!(run(source), b"suspended");
}

// The rule most likely to be implemented wrongly, and the one that would take the whole concurrency stack with it. A nullary `() -> X` is an *empty* parameter list, so nothing flips and `X` stays strictly positive — a zero-argument function is a thunk of its result. Read as "an occurrence under an arrow is negative", this rejects `/std/Async`.
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

// `/std/Pause` and `/std/Async` in miniature: two declarations in one `and` block, each reaching the other, with the recursive occurrence behind a thunk. The group boundary lives on `Item::Rec` and not on the registry entry, so this passes only because the occurrence relation is closed transitively rather than by asking who shares a declaration block.
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

// `Json/arr(List(Json))`: a list is a finite product of its element, so the intrinsic is covariant and a strict occurrence stays strict. Without polarity on intrinsics this shape is rejected outright.
#[test]
fn recursion_through_a_covariant_intrinsic_is_admitted() {
    let source = r#"
        use /std/{List};

        induct Tree : pub Type
        | leaf()
        | node(kids : List(Tree))
        end

        let branch : Tree = Tree/node([Tree/leaf()]);

        match branch
        | leaf() => /std/print("no")
        | node(_) => /std/print("listed")
        end
        "#;
    assert_eq!(run(source), b"listed");
}

// `Json/obj(List({Str, Json}))`: polarity has to travel through an anonymous tuple type as well as the intrinsic, since a product is as positive as its factors.
#[test]
fn recursion_through_an_anonymous_tuple_is_admitted() {
    let source = r#"
        use /std/{List, Nat};

        induct Doc : pub Type
        | atom(Nat)
        | fields(List({Nat, Doc}))
        end

        match Doc/fields([])
        | atom(_) => /std/print("no")
        | fields(_) => /std/print("tupled")
        end
        "#;
    assert_eq!(run(source), b"tupled");
}

// `/std/Toml`, whose recursion travels `Toml → Map(Toml) → struct field → Option → Node`. This is the shape that makes "modulo polarity" load-bearing on day one rather than a later refinement: a check that only recognizes recursive occurrences in immediate payload positions rejects it.
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

// `Node(V)` mentions itself, so its own parameter vector is an input to computing that vector. A single ordered pass reads `V` as `Unused` and then composes `Value → Table → Node` through a parameter it wrongly believes is never inspected; only running the two fixpoints to stability gets `Strict`.
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

// `struct Waker : Type { () -> {} }` inside `/std/Pause`'s `park` payload: a genuine negative position, and sound only because `Waker` mentions neither member of the group. The check must reject on the *path back*, not on the mere presence of a negative occurrence.
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

// The control for the fixture above: a type-former parameter is legal, and applying it is legal. What is refused there is the declaration reaching *itself* through a former whose polarity nothing knows — so the diagonal of the occurrence relation is what decides, exactly as the acceptance predicate says. `Apply` never appears inside `F(A)`, so its diagonal stays `Unused` however `F` behaves.
#[test]
fn a_type_former_parameter_not_recursed_through_is_admitted() {
    let source = r#"
        induct Apply(F : (Type) -> Type, A : Type) : pub Type
        | wrap(F(A))
        end

        induct Box(A : Type) : pub Type
        | put(A)
        end

        let boxed : Apply(Box, /std/Nat) = Apply/wrap(Box/put(7));

        match boxed
        | wrap(_) => /std/print("applied")
        end
        "#;
    assert_eq!(run(source), b"applied");
}
