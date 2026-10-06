//! Scalar replacement of structural tuples whose runtime-dependent type changes their width.

use {
    crate::{tests::Compiled, to_cwasm},
    curios_cont::{FieldGroup, Intrinsic, Node, ValueExpr},
    curios_pipeline::{DEFAULT_STEP_BUDGET, Stage, compile_with_prelude},
    curios_runtime::MockHost,
    curios_text::{Entrypoint, RootSource},
};

/// The loop toggles between two- and three-field tuples, reading their common first field under the matching `Bool` arm. Its host-read first field prevents literal-tag specialization from separating the widths before they merge. Scalar replacement removes the projections and the three-field construction; the result still follows the input on both widths and after several backedges.
#[test]
fn a_runtime_dependent_tuple_width_is_split_into_fields() {
    let source = r#"
        use /std/{Bool, Nat, List, proc};
        let Shape(b: Bool) -> Type =
            match b | true => {Nat, Nat} | false => {Nat, Nat, Nat} end;
        let make(b: Bool, n: Nat) -> Shape(b) =
            match b: (tag) => Shape(tag)
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
        let n = List/len(proc/args!);
        let b = Nat/eql(n, 1);
        /std/print(Nat/to_str(loop(b, make(b, n), n, n)))
        "#;
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let mut captured = None;
    let (wasm, _foreigns) = compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| {
            if let Stage::ContOptm(module) = stage {
                captured = Some(module.clone());
            }
        },
    )
    .expect("the runtime-dependent product compiles");
    let module = captured.expect("the pipeline observes cont-optm");
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

    let compiled = Compiled {
        cwasm: to_cwasm(&wasm).expect("the scalarized program precompiles"),
    };
    for (args, expected) in [
        (&[][..], b"0".as_slice()),
        (&["program"][..], b"2".as_slice()),
        (&["program", "argument"][..], b"4".as_slice()),
    ] {
        let (system, io) = MockHost::builder().args(args.iter().copied()).build();
        assert_eq!(compiled.run(system), Ok(0));
        assert_eq!(io.output(), expected);
    }
}
