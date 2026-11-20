//! A loop contification made of a total recursion goes where nothing reads its answer and its body observes nothing, and stays where either fails.
use {
    curios_cps::Module,
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_pipeline::{Stage, compile_with_prelude},
    curios_surface::Entrypoint,
};

/// The module `source` is emitted from.
fn optimized(source: &str) -> Module {
    let entrypoint = source.parse::<Entrypoint>().expect("the fixture parses");
    let mut captured = None;
    compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |stage| {
        if let Stage::CpsOptm(module) = stage {
            captured = Some(module.clone());
        }
    })
    .expect("the fixture compiles");
    captured.expect("the pipeline observes cps-optm")
}

/// Whether anything `module` holds, a function or a continuation, is named `name`.
fn holds(module: &Module, name: &str) -> bool {
    module
        .live_functions()
        .filter_map(|(_, function)| function.debug_name.as_deref())
        .chain(
            module
                .live_continuations()
                .filter_map(|(_, continuation)| continuation.debug_name.as_deref()),
        )
        .any(|debug| debug == name)
}

/// A walk that applies what it is handed, its answer bound by a `let` nothing reads, through a field of a record and through a parameter. Each is called from one place and becomes a loop there.
const WALKS: &str = r#"
    use /std/{Nat, List, Proc, Fmt};
    pub struct Ops: pub Type {
        f: (Nat) -> Nat,
    }
    let through_field(ops: Ops, l: List(Nat)) -> Nat =
        match l
        | [] => 0
        | [h, ..r] => match h | 0 => through_field(ops, r) | _ => ops.f(h) end
        end;
    let through_param(f: (Nat) -> Nat, l: List(Nat)) -> Nat =
        match l
        | [] => 0
        | [h, ..r] => match h | 0 => through_param(f, r) | _ => f(h) end
        end;
    let dead_field(ops: Ops, l: List(Nat), n: Nat) -> Nat =
        let a = through_field(ops, l);
        n + 1;
    let dead_param(f: (Nat) -> Nat, l: List(Nat), n: Nat) -> Nat =
        let a = through_param(f, l);
        n + 2;
    let entry() -> /std/Io({}) =
        let arguments = Proc/args()!;
        let n = List/len(arguments);
        let l = [n, 0, n + 1];
        let ops = Ops { f = (x) => x * 3 };
        Fmt/print("% %\n")(dead_field(ops, l, n))(dead_param((x) => x * 5, l, n));
    entry()
    "#;

/// A total walk whose answer nothing reads leaves no loop, whether what it applies is a field of what it is handed or a parameter: the loop carries its function's termination, and its body, once the applied function is known, observes nothing. Mutation-checked: with the loop's termination left unset, both walks stay.
#[test]
fn a_total_walk_nothing_reads_leaves_no_loop() {
    let module = optimized(WALKS);
    for walk in ["/through_field", "/through_param"] {
        assert!(!holds(&module, walk), "{walk} stayed:\n{module}");
    }
}

/// A total walk handed a partial function stays, its answer unread, where the function's loop enters a total one each round and resumes at its exit: `g` is contified into the walk's loop and `h`'s into `g`'s, so `g`'s cycle passes through `h`'s marked loop, and closes by `g`'s own back edge, which no termination covers. Run, the program never ends.
///
/// Mutation-checked: with every marked continuation taken out of the cycle check, the walk goes.
#[test]
fn a_total_walk_whose_partial_step_cycles_through_a_total_loop_stays() {
    let module = optimized(
        r#"
        use /std/{Nat, Proc, List};
        let f(k: (Nat) -> Nat, n: Nat) -> Nat =
            let go(m: Nat, acc: Nat) -> Nat = match m | 0 => acc | p + 1 => go(p, k(acc)) end;
            go(n, 1);
        let h(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => h(k) + 1 end;
        let g(n: Nat) -> Nat = match h(n) == 0 | true => 0 | false => g(n + 1) end;
        let entry() -> /std/Io({}) =
            let arguments = Proc/args()!;
            let _ = f(g, List/len(arguments) + 3);
            /std/print("done");
        entry()
        "#,
    );
    assert!(holds(&module, "/g"), "{module}");
}

/// A walk no checker proves terminating stays, its answer unread: the loop carries no termination to drop it by.
#[test]
fn a_walk_not_known_to_terminate_stays() {
    let module = optimized(
        r#"
        use /std/{Nat, List, Proc};
        let settle(n: Nat) -> Nat = match n | 0 => 0 | 1 => 1 | _ => settle(match n % 2 | 0 => n / 2 | _ => 3 * n + 1 end) end;
        let entry() -> /std/Io({}) =
            let arguments = Proc/args()!;
            let a = settle(List/len(arguments) + 7);
            /std/print("done");
        entry()
        "#,
    );
    assert!(holds(&module, "/settle"), "{module}");
}
