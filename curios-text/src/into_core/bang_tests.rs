//! The source functions that permit sequencing, the generated abstractions that do not, and functions written inside type expressions.

use super::test_support::*;

#[test]
fn a_value_requires_a_source_function_at_every_lexical_depth() {
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
        assert!(
            run_err(source).contains("requires an enclosing function"),
            "{source}"
        );
    }
}

#[test]
fn written_functions_permit_bangs_in_their_bodies() {
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
fn function_permission_does_not_escape_into_a_sibling_value() {
    let report = run_err_report("let f() -> M = good!;\nbad!");
    assert!(
        report.contains("requires an enclosing function"),
        "{report}"
    );
    assert!(report.contains("bad!"), "{report}");
    assert!(!report.contains("good!"), "{report}");
}

#[test]
fn functions_inside_type_expressions_use_the_same_sequencing_lowering() {
    for source in [
        "let x: (let f() -> M = action!; project(f())) = body; x",
        "let x: (let f: () -> M = () => action!; project(f())) = body; x",
        "let x: F(() => action!) = body; x",
    ] {
        let _ = run(source);
    }
}
