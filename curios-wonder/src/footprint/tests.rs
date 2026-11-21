use {
    super::*,
    crate::{test_support::*, *},
    curios_elab::DEFAULT_BUDGET,
    curios_module::Overlay,
    curios_surface::WrittenIn,
};

/// `text` as a loose subject, the way the one-shot transport admits one from standard input: a program, or a module of its own.
fn loose(text: &str) -> Subject {
    stdin_subject(text, WrittenIn::Program)
}

/// What the program `text` holds of the declaration at `path`, asked the way the one-shot transport asks.
fn rows_of(text: &str, path: &str) -> Option<Vec<Row>> {
    footprint(
        DEFAULT_BUDGET,
        loose(text),
        &Qualifier::from([path]),
        &Overlay::default(),
        None,
        &mut |_| {},
    )
    .expect("the program compiles")
    .map(|(_, rows)| rows)
}

#[test]
fn a_module_has_no_footprint() {
    let refused = footprint(
        DEFAULT_BUDGET,
        loose("let helper(x : /std/Nat) -> /std/Nat = x + 1;\n"),
        &Qualifier::from(["helper"]),
        &Overlay::default(),
        None,
        &mut |_| {},
    );

    assert!(matches!(refused, Err(Unfooted::Unit)), "{refused:?}");
}

/// A declaration nothing the program runs reaches is not linked, in one row labelled by its path: the program holds none of its functions to tell apart.
#[test]
fn a_declaration_the_program_never_reaches_is_not_linked() {
    let rows = rows_of(
        "let helper(x : /std/Nat) -> /std/Nat = x + 1;\n/std/print(\"hello\\n\")",
        "helper",
    );

    assert_eq!(
        rows,
        Some(vec![Row {
            label: Some("/helper".to_string()),
            fate: Fate::NotLinked,
            bytes: None,
            rebuilds: None,
        }])
    );
}

#[test]
fn a_path_naming_nothing_in_scope_has_no_footprint() {
    assert_eq!(rows_of("/std/print(\"hello\\n\")", "absent"), None);
}

/// A program reading `n` from its arguments, so nothing folds away: `count` a tail recursion adding `sum_squares` of each number below `n`, `sum_squares` folding `square` over a range, and `fib` a recursion of two calls, which no pass makes a loop of.
const RECURSIONS: &str = "use /std/{Str, Nat, List, Option, Result, Try, Io, Proc, Fmt};

let square(n: Nat) -> Nat = n * n;

let sum_squares(n: Nat) -> Nat = List/fold(List/range(0, n), 0, (k, acc) => acc + square(k));

let count(k: Nat, acc: Nat) -> Nat =
    match k | 0 => acc | p + 1 => count(p, acc + sum_squares(p)) end;

let fib(n: Nat) -> Nat =
    match n | 0 => 0 | p + 1 => match p | 0 => 1 | q + 1 => fib(p) + fib(q) end end;

let execute() -> Try(Io, Str, {}) =
    let arguments = Proc/args()!;
    let bytes = Option/to_result(List/try_get(arguments, 1), \"no input\")!;
    let s = Result/map_failure(Str/of_bytes(bytes), (_) => \"invalid utf-8\")!;
    let n = Option/to_result(Nat/of_str(Str/trim(s)), \"bad input\")!;
    let () = Fmt/print(\"%\\n\")(count(n, 0))!;
    let () = Fmt/print(\"%\\n\")(fib(n))!;
    Try/pure(());

execute()
";

/// A recursion that keeps its frames is kept, weighed by its body, and the heap twin the frame bound made of it stands under it, weighed with what it lifted out of itself for its frames to wait in.
#[test]
fn a_recursion_kept_is_weighed_with_its_heap_twin_under_it() {
    let rows = rows_of(RECURSIONS, "fib").expect("`/fib` is declared");

    let [kept, twin] = rows.as_slice() else {
        panic!("{rows:?}");
    };
    assert_eq!(kept.label.as_deref(), Some("/fib"));
    assert_eq!(kept.fate, Fate::Kept);
    assert!(kept.bytes.is_some_and(|bytes| bytes > 0), "{rows:?}");
    assert_eq!(twin.fate, Fate::HeapTwin);
    assert!(twin.bytes.is_some_and(|bytes| bytes > 0), "{rows:?}");
}

/// A tail recursion contified into its caller is no function of its own: the loop it became stands in the caller, which the row names.
#[test]
fn a_loop_contified_into_its_caller_is_inlined_into_it() {
    let rows = rows_of(RECURSIONS, "count").expect("`/count` is declared");

    let [
        Row {
            fate: Fate::InlinedInto(callers),
            bytes: None,
            ..
        },
    ] = rows.as_slice()
    else {
        panic!("{rows:?}");
    };
    assert!(!callers.is_empty());
}

/// A function inlined whole as straight-line code leaves nothing of its own, and costs nothing of its own.
#[test]
fn a_function_inlined_whole_is_absorbed() {
    let rows = rows_of(RECURSIONS, "square").expect("`/square` is declared");

    assert_eq!(
        rows,
        [Row {
            label: Some("/square".to_string()),
            fate: Fate::Absorbed,
            bytes: None,
            rebuilds: None,
        }]
    );
}
