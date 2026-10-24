//! Reconstruction disappears before lowering, and recursive transfers remain tail calls or loop backedges through raw and optimized Wasm.

use {
    super::functions,
    curios_abi::ForeignStore,
    curios_native::{Compiled, ForeignBindings, MockHost},
    curios_pipeline::{DEFAULT_BUDGET, Stage, compile_with_prelude},
    curios_text::{Entrypoint, RootSource},
    curios_utilities::fill,
    curios_wasm::to_bytes,
    std::collections::BTreeSet,
};

const HEADER: &str =
    "use /std/{Nat, Io, Cell, Option, List, Process, proved}; use /std/Bool/{Holds};";
const EXACT: &str = r#"
let walk(n: Nat, seed: Nat) -> {Nat, Nat} =
    match n
    | 0 => (seed, seed + 13)
    | k + 1 => let r = walk(k, (seed + 1) % 16777213); (r.0, r.1)
    end;
"#;
const PROOF: &str = r#"
let walk(n: Nat, seed: Nat) -> {at: Nat, low: Holds(0 <= at)} =
    match n
    | 0 => (at = seed, low = proved())
    | k + 1 => let r = walk(k, (seed + 1) % 16777213); (at = r.at, low = proved())
    end;
"#;
const OPTIONAL: &str = r#"
let walk(n: Nat, seed: Nat) -> Option({at: Nat, seen: Nat, low: Holds(0 <= at)}) =
    match n
    | 0 => Option/some((at = seed, seen = seed + 13, low = proved()))
    | k + 1 => match k
        | 0 => Option/none()
        | j + 1 => match walk(j, (seed + 2) % 16777213)
            | some(hit) => Option/some((at = hit.at, seen = hit.seen, low = proved()))
            | none() => Option/none()
            end
        end
    end;
"#;

struct Fixture {
    erased: curios_ersd::Module,
    cont: curios_cont::Module,
    wasm: curios_wasm::Module,
    foreigns: ForeignStore,
}

fn compile(source: &str) -> Fixture {
    let entrypoint = source.parse::<Entrypoint>().expect("fixture parses");
    let mut erased = None;
    let mut cont = None;
    let (wasm, foreigns) = compile_with_prelude(
        DEFAULT_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| match stage {
            Stage::ErsdOptm(module) => erased = Some(module.clone()),
            Stage::ContOptm(module) => cont = Some(module.clone()),
            _ => {}
        },
    )
    .expect("fixture compiles");
    Fixture {
        erased: erased.unwrap(),
        cont: cont.unwrap(),
        wasm,
        foreigns,
    }
}

fn tail_erased(fixture: &Fixture) {
    let module = &fixture.erased;
    let function = module
        .function_ids()
        .find(|&id| module.function(id).unwrap().debug_name.as_deref() == Some("/walk"))
        .expect("runtime input keeps the walk live");
    let mut blocks = vec![module.function(function).unwrap().body];
    let mut seen = BTreeSet::new();
    let mut calls = 0;
    while let Some(id) = blocks.pop() {
        if !seen.insert(id) {
            continue;
        }
        let block = module.block(id).unwrap();
        for (i, &id) in block.statements.iter().enumerate() {
            let Some(curios_ersd::Statement::Let { result, rhs }) = module.statement(id) else {
                continue;
            };
            blocks.extend(rhs.sub_blocks());
            if matches!(rhs, curios_ersd::Rhs::Apply { callee: curios_ersd::Atom::Function(callee), .. } if *callee == function)
            {
                calls += 1;
                assert_eq!(
                    i + 1,
                    block.statements.len(),
                    "nothing reconstructs after recursion: {module}"
                );
                assert!(
                    matches!(block.terminator, curios_ersd::Terminator::Return(curios_ersd::Atom::Value(value)) if value == *result),
                    "the block immediately returns its recursive call: {module}"
                );
            }
        }
    }
    assert!(calls > 0, "the fixture retains a recursive transfer");
}

fn cont_nodes(
    module: &curios_cont::Module,
    root: curios_cont::NodeId,
) -> BTreeSet<curios_cont::NodeId> {
    let mut work = vec![root];
    let mut seen = BTreeSet::new();
    while let Some(id) = work.pop() {
        if !seen.insert(id) {
            continue;
        }
        match module.node(id).unwrap() {
            curios_cont::Node::LetValue { next, .. }
            | curios_cont::Node::LetIntrinsic { next, .. } => work.push(*next),
            curios_cont::Node::LetFun { body, .. } => work.push(*body),
            curios_cont::Node::LetCont {
                continuations,
                body,
            } => {
                work.push(*body);
                work.extend(
                    continuations
                        .iter()
                        .map(|&id| module.continuation(id).unwrap().body),
                );
            }
            _ => {}
        }
    }
    seen
}

fn loop_cont(fixture: &Fixture) {
    let module = &fixture.cont;
    assert!(
        !module
            .live_functions()
            .any(|(_, f)| f.debug_name.as_deref() == Some("/walk")),
        "the one-caller walk is contified: {module}"
    );
    let target = module
        .continuation_ids()
        .find(|&id| module.continuation(id).unwrap().debug_name.as_deref() == Some("/walk"))
        .expect("the contified walk retains its loop");
    let nodes = cont_nodes(module, module.continuation(target).unwrap().body);
    assert!(nodes.iter().any(|&id| matches!(module.node(id), Some(curios_cont::Node::ApplyCont(edge)) if edge.target == target)), "the loop has a backedge: {module}");
    let wat = fixture.wasm.to_string();
    let functions = functions(&wat);
    let kernels = functions
        .iter()
        .filter(|f| f.body.contains("16777213"))
        .collect::<Vec<_>>();
    assert!(!kernels.is_empty());
    assert!(
        kernels.iter().any(|f| f.body.contains("loop ")),
        "the raw Wasm contains the recovered loop"
    );
}

fn run_both(fixture: &Fixture, cases: &[(&[&str], &str)]) {
    let raw = to_bytes(&fixture.wasm);
    for bytes in [raw.clone(), curios_binaryen::optimize(raw, false, |_| {})] {
        let code = curios_cranelift::compile(&bytes).expect("raw and optimized modules validate");
        let compiled = Compiled::of(code, fixture.foreigns.clone());
        for &(args, expected) in cases {
            let (host, io) = MockHost::builder().args(args.iter().copied()).build();
            assert_eq!(compiled.run(host, ForeignBindings::empty()), Ok(0));
            assert_eq!(io.output(), expected.as_bytes());
            assert!(io.errors().is_empty());
        }
    }
}

#[test]
fn exact_products_recur_as_loops_before_downstream_optimization() {
    let fixture = compile(&fill!(
        "${HEADER}${EXACT} let main() -> Io({}) = let n = 100000 + List/len(/std/Process/args()!); let r = walk(n, 7); /std/print(Nat/to_str(r.0 + r.1)); main()",
        HEADER,
        EXACT
    ));
    tail_erased(&fixture);
    loop_cont(&fixture);
    run_both(&fixture, &[(&[], "200027"), (&["program"], "200029")]);
}

#[test]
fn retained_proof_slots_do_not_cost_a_frame_per_step() {
    let fixture = compile(&fill!(
        "${HEADER}${PROOF} let main() -> Io({}) = let n = 100000 + List/len(/std/Process/args()!); let r = walk(n, 7); /std/print(Nat/to_str(r.at)); main()",
        HEADER,
        PROOF
    ));
    tail_erased(&fixture);
    loop_cont(&fixture);
    run_both(&fixture, &[(&[], "100007"), (&["program"], "100008")]);
}

#[test]
fn nested_proof_reconstruction_preserves_both_constructor_outcomes() {
    let fixture = compile(&fill!(
        r#"${HEADER}${OPTIONAL}
        let main() -> Io({}) =
        let n = 100000 + List/len(/std/Process/args()!);
        match walk(n, 7)
        | some(hit) => let _ = /std/print(Nat/to_str(hit.at))!; let _ = /std/print("/")!; /std/print(Nat/to_str(hit.seen))
        | none() => /std/print("none")
        end;
        main()
        "#,
        HEADER,
        OPTIONAL
    ));
    tail_erased(&fixture);
    loop_cont(&fixture);
    run_both(&fixture, &[(&[], "100007/100020"), (&["program"], "none")]);
}

#[test]
fn an_escaping_walk_with_multiple_callers_keeps_a_tail_recursive_function() {
    let fixture = compile(&fill!(
        r#"${HEADER}${PROOF}
        let main() -> Io({}) =
        let holder = Cell/new()!;
        let _ = Cell/fill(holder, walk)!;
        let indirect = Option/unwrap_or(Cell/poll(holder)!, walk);
        let n = List/len(/std/Process/args()!);
        let a = walk(n, 7);
        let b = walk(n + 1, 11);
        let c = indirect(n + 2, 19);
        /std/print(Nat/to_str(a.at + b.at + c.at));
        main()
        "#,
        HEADER,
        PROOF
    ));
    tail_erased(&fixture);
    let module = &fixture.cont;
    let (function, definition) = module
        .live_functions()
        .find(|(_, f)| f.debug_name.as_deref() == Some("/walk"))
        .expect("storing the walk keeps its function boundary");
    let nodes = cont_nodes(module, definition.body);
    let mut calls = 0;
    for id in nodes {
        if let Some(curios_cont::Node::ApplyFun {
            callee: curios_cont::Callee::Known(callee),
            return_to,
            ..
        }) = module.node(id)
            && *callee == function
        {
            calls += 1;
            assert_eq!(*return_to, definition.return_cont);
        }
    }
    assert!(calls > 0);
    let wat = fixture.wasm.to_string();
    let functions = functions(&wat);
    let recursive = functions
        .iter()
        .filter(|f| f.body.contains("16777213") && f.self_calls() > 0)
        .collect::<Vec<_>>();
    assert!(!recursive.is_empty(), "raw Wasm retains direct recursion");
    for f in recursive {
        assert_eq!(
            f.self_calls(),
            f.body.matches(&format!("return_call {}", f.name)).count(),
            "every self call is tail: {}",
            f.body
        );
    }
    run_both(&fixture, &[(&[], "40"), (&["program"], "43")]);
}

#[test]
fn overwriting_an_ordinary_numeric_field_changes_the_answer() {
    let fixture = compile(&fill!(
        r#"${HEADER}
        let walk(n: Nat) -> {at: Nat, real: Nat} = match n
            | 0 => (at = 7, real = 9)
            | k + 1 => let r = walk(k); (at = r.at, real = 0)
            end;
        let entry() -> /std/Io({}) =
            let n = List/len(/std/Process/args()!);
            let r = walk(n);
            /std/print(Nat/to_str(r.real));
        entry()
        "#,
        HEADER
    ));
    run_both(
        &fixture,
        &[
            (&[], "9"),
            (&["program"], "0"),
            (&["program", "argument"], "0"),
        ],
    );
}
