//! Independent constructor, worker, closure and SCC rewrites preserve runtime answers through both Wasm paths.

mod test_support;
use test_support::*;

use {
    super::compile_raw_program,
    curios_runtime::{Compiled, ForeignBindings, MockHost},
    curios_wasm::to_bytes,
};

fn answers(source: &str, expected: [&str; 4]) {
    let (module, foreigns) = compile_raw_program(source);
    let raw = to_bytes(&module);
    for bytes in [raw.clone(), curios_binaryen::optimize(raw, false, |_| {})] {
        let code = curios_cranelift::compile(&bytes).expect("the module validates and compiles");
        let compiled = Compiled::of(code, foreigns.clone());
        for (n, expected) in [0, 1, 4, 8].into_iter().zip(expected) {
            let (host, io) = MockHost::builder().args(vec!["argument"; n]).build();
            assert_eq!(compiled.run(host, ForeignBindings::empty()), Ok(0));
            assert_eq!(io.output(), expected.as_bytes(), "{n} host arguments");
            assert!(io.errors().is_empty());
        }
    }
}

#[test]
fn independent_constructor_joins_preserve_each_arm() {
    answers(JUMPS, ["0", "3", "23", "231"]);
}

#[test]
fn independent_tuple_workers_keep_their_recursion_and_arguments() {
    answers(WORKERS, ["3", "10", "82", "859"]);
}

#[test]
fn independent_returned_closures_keep_their_captures_and_applications() {
    answers(UNCURRY, ["37", "6421", "39126", "38087"]);
}

#[test]
fn independent_constructor_callees_keep_their_generic_escaping_uses() {
    answers(CALLS, ["80463", "285070", "216097", "89579"]);
}

#[test]
fn recursive_callback_contexts_keep_their_distinct_operations() {
    answers(SCCS, ["1", "10", "985", "52750"]);
}

#[test]
fn a_returned_closure_and_aggregate_survive_a_suspension_between_calls() {
    answers(
        r#"
        use /std/{Nat, List, proc, Io};
        induct Side: Type | left(Nat) | right(Nat) end
        let combine(n: Nat, pair: {Nat, Nat}) -> (Side) -> Nat =
            match n
            | 0 => (side) => match side | left(x) => pair.0 + x | right(x) => pair.1 + x end
            | k + 1 => (side) => combine(k, (pair.1, pair.0 + 1))(side) + 1
            end;
        let entry() -> Io({}) =
            let n = List/len(proc/args()!);
            let f = combine(n, (n, 1));
            let _ = Io/yield()!;
            let side = match n % 2 | 0 => Side/left(n) | _ => Side/right(n) end;
            /std/print(Nat/to_str(f(side)));
        entry()
        "#,
        ["0", "4", "14", "28"],
    );
}
