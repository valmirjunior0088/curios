//! Sequencing lowers at every lexical depth, with each region's monad left to elaboration.

use crate::into_core::test_support::*;

#[test]
fn bangs_lower_in_value_regions_at_every_lexical_depth() {
    for source in [
        "action!",
        "let x: M = action!; x",
        "let x = action!; x",
        "match flag | true => action! | false => fallback end",
        "match | flag => action! | _ => fallback end",
        "let x: C = C { field = action! }; x",
        "test t => action!; tail",
        "concept C(A: Type): pub Type { field: M } satisfy (@A: Type) -> C(A) { field = action! } tail",
    ] {
        let _ = run(source);
    }
}

#[test]
fn function_bodies_keep_their_sequencing_regions() {
    for source in [
        "() => action!",
        "let f() -> M => action!; f()",
        "let f: () -> M = () => action!; f()",
        "let f() -> M => let x: A = action!; finish(x); f()",
        "let f() -> M => let x = action!; finish(x); f()",
        "let f() -> M => let g() -> N => action!; finish(g()); f()",
        "let c: C = C { field() => action! }; c",
        "(field() => action!)",
        "concept C(A: Type): pub Type { field() -> M } satisfy (@A: Type) -> C(A) { field() => action! } tail",
    ] {
        let _ = run(source);
    }
}

#[test]
fn sibling_values_lower_their_own_sequencing_regions() {
    let _ = run("let f() -> M => first!;\nsecond!");
}

#[test]
fn functions_inside_type_expressions_use_the_same_sequencing_lowering() {
    for source in [
        "let x: action! = body; x",
        "let x: (() => let chosen: M = action!; project(chosen))() = body; x",
        "let x: (() => let f() -> M => action!; project(f()))() = body; x",
        "let x: (() => let f: () -> M = () => action!; project(f()))() = body; x",
        "let x: F(() => action!) = body; x",
    ] {
        let _ = run(source);
    }
}

/// A guard decides which arm is taken and holds no action: a `!` in one, under a match with a head or with none, is refused in those words with the explicit form shown, while one inside a lambda the guard writes belongs to that lambda's body.
#[test]
fn a_guards_action_is_refused_naming_the_explicit_form() {
    for source in [
        "match | flag! => yes | _ => no end",
        "match o | some(x), check(x)! => yes | _ => no end",
    ] {
        let refused = run_err_report(source);
        assert!(
            refused.contains("action in a guard") && refused.contains("let held = action!"),
            "{source}: {refused}"
        );
    }
    let _ = run("match | all((x) => check(x)!) => yes | _ => no end");
}
