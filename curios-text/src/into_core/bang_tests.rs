//! Sequencing lowers at every lexical depth, with each region's monad left to elaboration.

use super::test_support::*;

#[test]
fn bangs_lower_in_value_regions_at_every_lexical_depth() {
    for source in [
        "action!",
        "let x: M = action!; x",
        "let x = action!; x",
        "match flag | true => action! | false => fallback end",
        "choose | flag => action! | _ => fallback end",
        "let x: C = C { field = action! }; x",
        "test t = action!; tail",
        "concept C(A: Type): pub Type { field: M } satisfy (@A: Type) => C(A) { field = action! } tail",
    ] {
        let _ = run(source);
    }
}

#[test]
fn function_bodies_keep_their_sequencing_regions() {
    for source in [
        "() => action!",
        "let f() -> M = action!; f()",
        "let f: () -> M = () => action!; f()",
        "let f() -> M = let x: A = action!; finish(x); f()",
        "let f() -> M = let x = action!; finish(x); f()",
        "let f() -> M = let g() -> N = action!; finish(g()); f()",
        "let c: C = C { field() = action! }; c",
        "(field() = action!)",
        "concept C(A: Type): pub Type { field() -> M } satisfy (@A: Type) => C(A) { field() = action! } tail",
    ] {
        let _ = run(source);
    }
}

#[test]
fn sibling_values_lower_their_own_sequencing_regions() {
    let _ = run("let f() -> M = first!;\nsecond!");
}

#[test]
fn functions_inside_type_expressions_use_the_same_sequencing_lowering() {
    for source in [
        "let x: action! = body; x",
        "let x: (let chosen: M = action!; project(chosen)) = body; x",
        "let x: (let f() -> M = action!; project(f())) = body; x",
        "let x: (let f: () -> M = () => action!; project(f())) = body; x",
        "let x: F(() => action!) = body; x",
    ] {
        let _ = run(source);
    }
}
