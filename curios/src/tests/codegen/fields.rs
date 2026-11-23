//! Scalar replacement of structural tuples whose runtime-dependent type changes their width.

use {
    crate::test_support::*,
    curios_cps::{FieldGroup, Intrinsic, Node, ValueExpr},
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_native::{ForeignBindings, test_support::*},
    curios_pipeline::{Stage, compile_with_prelude},
    curios_surface::Entrypoint,
};

/// The loop toggles between two- and three-field tuples, reading their common first field under the matching `Bool` arm. Its host-read first field prevents literal-tag specialization from separating the widths before they merge. Scalar replacement removes the projections and the three-field construction; the result still follows the input on both widths and after several backedges.
#[test]
fn a_runtime_dependent_tuple_width_is_split_into_fields() {
    let source = r#"
        use /std/{Bool, Nat, List, Proc};
        let Shape(b: Bool) -> Type =
            match b | true => {Nat, Nat} | false => {Nat, Nat, Nat} end;
        let make(b: Bool, n: Nat) -> Shape(b) =
            match b
            | true => (n, 2)
            | false => (n, 4, 5)
            end;
        let loop(b: Bool, x: Shape(b), n: Nat, fuel: Nat) -> Nat =
            match fuel
            | 0 => match b | true => x.0 | false => x.0 end
            | rest + 1 =>
                let next = Bool/not(b);
                loop(next, make(next, n + 1), n + 1, rest)
            end;
        let entry() -> /std/Io({}) =
            let n = List/length(Proc/args()!);
            let b = Nat/eql(n, 1);
            /std/print(Nat/to_str(loop(b, make(b, n), n, n)));
        entry()
        "#;
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let mut captured = None;
    let (wasm, foreigns) =
        compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |stage| {
            if let Stage::CpsOptm(module) = stage {
                captured = Some(module.clone());
            }
        })
        .expect("the runtime-dependent product compiles");
    let module = captured.expect("the pipeline observes cps-optm");
    let loop_cont = module
        .continuation_ids()
        .find(|&id| module.continuation(id).unwrap().debug_name.as_deref() == Some("/loop"))
        .expect("the recursive loop is contified");
    assert_eq!(
        module.field_groups().get(&loop_cont),
        Some(&vec![FieldGroup { start: 1, width: 1 }]),
        "the tuple parameter travels as its one live field"
    );

    // Owning links include the entry's local continuations but exclude the separately defined library functions: their error handling legitimately reads structural tuples.
    let mut pending = vec![module.function(module.entry().unwrap()).unwrap().body];
    let mut nodes = Vec::new();
    while let Some(id) = pending.pop() {
        let node = module.node(id).unwrap();
        match node {
            Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. } => pending.push(*next),
            Node::LetFun { body, .. } => pending.push(*body),
            Node::LetCont {
                continuations,
                body,
            } => {
                pending.push(*body);
                pending.extend(
                    continuations
                        .iter()
                        .map(|&id| module.continuation(id).unwrap().body),
                );
            }
            _ => {}
        }
        nodes.push(node);
    }
    assert!(
        nodes.iter().all(|node| !matches!(
            node,
            Node::LetIntrinsic {
                op: Intrinsic::TupleGet(_),
                ..
            }
        )),
        "the entry and its loop read fields directly: {module}"
    );
    assert!(
        nodes.iter().all(|node| !matches!(
            node,
            Node::LetValue { value: ValueExpr::Tuple(fields), .. } if fields.len() == 3
        )),
        "neither entering nor iterating the loop materializes the wider tuple: {module}"
    );

    let compiled = to_compiled(&wasm, foreigns).expect("the scalarized program compiles");
    for (args, expected) in [
        (&[][..], b"0".as_slice()),
        (&["program"][..], b"2".as_slice()),
        (&["program", "argument"][..], b"4".as_slice()),
    ] {
        let (system, io) = MockHost::builder().args(args.iter().copied()).build();
        assert_eq!(compiled.run(system, ForeignBindings::empty()), Ok(0));
        assert_eq!(io.output(), expected);
    }
}

/// An exit arm that builds a value nothing reads leaves the loop its window: the unread `0x[h, ..t]` goes as a total operation does, so no arm reads the suffix as a value and the loop carries `(base, end, length)` instead of cutting a view at every byte. Mutation-checked: with an unread allocation kept, the arm reads `t` whole and the loop takes the bytes as one parameter.
#[test]
fn an_unread_construction_in_an_exit_arm_leaves_the_loop_its_window() {
    let source = r#"
        use /std/{Nat, Bytes, List, Option, Proc};
        let walk(b: Bytes, k: Nat) -> Nat =
            match b
            | 0x[] => k
            | 0x[h, ..t] =>
                match h == 46
                | true => let _ = 0x[h, ..t]; k
                | false => walk(t, k + 1)
                end
            end;
        let entry() -> /std/Io({}) =
            let arguments = Proc/args()!;
            let bytes = Option/unwrap_or(List/try_get(arguments, 0), 0x[]);
            /std/print(Nat/to_str(walk(bytes, 0)));
        entry()
        "#;
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let mut captured = None;
    let (wasm, foreigns) =
        compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |stage| {
            if let Stage::CpsOptm(module) = stage {
                captured = Some(module.clone());
            }
        })
        .expect("the walk compiles");
    let module = captured.expect("the pipeline observes cps-optm");
    let walk = module
        .continuation_ids()
        .find(|&id| module.continuation(id).unwrap().debug_name.as_deref() == Some("/walk"))
        .expect("the recursive walk is contified");
    assert_eq!(
        module.field_groups().get(&walk),
        Some(&vec![FieldGroup { start: 0, width: 3 }]),
        "the bytes travel as their window: {module}"
    );

    let compiled = to_compiled(&wasm, foreigns).expect("the walk compiles to a program");
    for (argument, expected) in [("abc", b"3".as_slice()), ("ab.c", b"2".as_slice())] {
        let (system, io) = MockHost::builder().args([argument]).build();
        assert_eq!(compiled.run(system, ForeignBindings::empty()), Ok(0));
        assert_eq!(io.output(), expected);
    }
}
