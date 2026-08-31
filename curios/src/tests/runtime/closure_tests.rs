//! Closures and local recursion surviving erasure and codegen, including the shapes a knot builds.

use {
    crate::tests::run,
    curios_pipeline::Stage,
    curios_pipeline::compile_with_prelude,
    curios_text::{Entrypoint, RootSource},
};

#[test]
fn nullary_closure_survives_erasure_and_codegen() {
    // A nullary closure stored in an inductive field and called indirectly via a `call_ref` — the erasure+codegen path that needed `clsr_arities`. Zero-arity closures survive it, which is what lets the suspension/continuation thunks drop their dummy unit argument (`() -> T` rather than `({}) -> T`). The suspension now carries a *description* rather than performing on the way through: `force` walks the `later` closure to the `now` payload, and the write happens where that payload is forced, so the output still proves the closure was reached and called.
    assert_eq!(
        run(r#"
        use /std/{Handle, Str};
        induct Susp(A : Type) : Type
        | now(A)
        | later(() -> Susp(A))
        end
        let force(@A : Type, s : Susp(A)) -> A =
            match s : (_) => A
            | now(a) => a
            | later(k) => force(k())
            end;
        let prog : Susp(/std/Io({})) =
            Susp/later(() => Susp/now(/std/print("ok")));
        let r = force(prog);
        let _ = r!;
        let _ = /std/print("!")!;
        /std/Io/pure(())
        "#),
        b"ok!"
    );
}

#[test]
fn end_to_end() {
    let source = r#"
        induct Pair : Type
        | left(std/Int)
        | right(std/Flt)
        end
        let pair : Pair = Pair/left(+42);
        let score : (_ : Pair) -> std/Int = (p) =>
            match p : (_) => std/Int
            | left(_) => +42
            | right(_) => +7
            end;
        let _ = std/Handle/write(std/Handle/stdout, /std/Str/to_bytes(std/Int/to_str(score(pair))))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), b"+42");
}

// Local binders shadow like-named *module* bindings, and a local name never leaks past its lexical scope. Inside `mod Foo` the module binding is `Foo/go`: an inner `let go` must shadow it (so `shadowed` is 3, not the captured 7), while a `go` that is a sibling of an inner `let go = 3` — reached only after that scope closes — must resolve back to `Foo/go` (so `sibling` is 7, not a leaked, unbound bare `go`). Encoded as 3*10 + 7 = 37, so the unlawful-capture regression reads 77 and a scope leak fails to compile.
#[test]
fn local_binders_shadow_module_bindings_without_leaking() {
    let source = r#"
        use /std/{Nat, Handle, Str};
        mod Foo
            pub let go : /std/Nat = 7;
            pub let shadowed : /std/Nat =
                let go : /std/Nat = 3;
                go;
            pub let sibling : /std/Nat =
                let probe : /std/Nat = (let go : /std/Nat = 3; go);
                go;
        end
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(Nat/add(Nat/mul(Foo/shadowed, 10), Foo/sibling))))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), b"37");
}

// Named fields end to end: a dependent record (the vector's length indexes its type) constructed with written names, consumed through `.label` and `.index` access on the same value — both resolve to the same positional projection.
#[test]
fn triangular_sum() {
    let source = r#"
        let result : std/Nat =
            match 5 : (_) => std/Nat
            | 0 => 0
            | pred + 1; ih => std/Nat/add(ih, pred)
            end;
        let _ = std/Handle/write(std/Handle/stdout, /std/Str/to_bytes(std/Nat/to_str(result)))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), b"10");
}

#[test]
fn multi_arg_function() {
    let source = r#"
        let add : (std/Int, std/Int) -> std/Int = (x, y) => std/Int/add(x, y);
        let _ = std/Handle/write(std/Handle/stdout, /std/Str/to_bytes(std/Int/to_str(add(+3, +4))))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), b"+7");
}

#[test]
fn curried_function() {
    let source = r#"
        let add : (std/Int) -> (std/Int) -> std/Int = (x) => (y) => std/Int/add(x, y);
        let _ = std/Handle/write(std/Handle/stdout, /std/Str/to_bytes(std/Int/to_str(add(+3)(+4))))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), b"+7");
}

#[test]
fn folds_constant_arg_through_let_function() {
    // `let f(x) = Nat/add(x, 1); f(3)` must fold end-to-end to a literal `4`. The observation point is the host call that consumes it rather than main's return continuation: a program's tail is now a description yielding unit, so no user value reaches that continuation at all. `proc/exit` is the shortest host operation taking a `Nat`, and its operand is erased at the construction site, so a surviving `NatAdd` would mean the fold did not happen.
    let source = r#"
        use /std/{Nat};
        let f(x : Nat) -> Nat = Nat/add(x, 1);
        /std/proc/exit(f(3))
        "#;

    let entrypoint = source.parse::<Entrypoint>().unwrap();

    let mut optimized = None;
    compile_with_prelude(
        curios_pipeline::DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| {
            if let Stage::ContOptm(module) = stage {
                optimized = Some(module.clone());
            }
        },
    )
    .expect("compile succeeded");

    let optimized = optimized.expect("Stage::ContOptm observed");
    let text = format!("{optimized}");
    assert!(
        !text.contains("NatAdd"),
        "the addition must fold at compile time, got:\n{text}",
    );
    assert!(
        text.contains("exit Some(Literal(Nat(4)))"),
        "expected the folded 4 to reach the exit, got:\n{text}",
    );
}

// A local `rec` nested inside another local `rec`'s body: `go` (inner) is an ordinary term-level construct here — never lambda-lifted, never spliced anywhere — so it just works, elaborated and erased in place exactly where written. Runtime-tainted so codegen cannot const-fold it away.
#[test]
fn nested_local_rec_runs_correctly() {
    assert_eq!(
        run(r#"
        use /std/{Handle, Nat, Str, Bytes};
        let f(n : Nat) -> Nat =
            (let go(i : Nat) -> Nat =
                match i
                | 0 => 0
                | k + 1; ih => go(k) + 1
                end;
             go(n));
        /std/print(Nat/to_str(f(Bytes/len(/std/rand/bytes(4)!))))
        "#),
        b"4"
    );
}

// A local `rec` nested inside a top-level `rec` member, calling that enclosing member by name: since nothing gets lambda-lifted or spliced as a separate item, there is no forward-reference to worry about — `go` just resolves `f` through ordinary lexical/context scoping, exactly where it's written.
#[test]
fn local_rec_calls_enclosing_rec_member() {
    assert_eq!(
        run(r#"
        use /std/{Handle, Nat, Str, Bytes};
        let f(n : Nat) -> Nat =
            (let go(i : Nat) -> Nat =
                match i
                | 0 => 0
                | k + 1; ih => f(k) + go(k)
                end;
             go(n));
        /std/print(Nat/to_str(f(Bytes/len(/std/rand/bytes(3)!))))
        "#),
        b"0"
    );
}

// A non-capturing, self-referential value `rec` (`loop : Nat = loop`) that the program never calls: this is exactly the shape that silently miscompiled under lambda-lifting (a self-aliased value slot dropped by the optimizer's copy-propagation) — here it stays a term-level `Rec`, erased in place, and its mere existence has no effect on the rest of the program.
#[test]
fn self_referential_value_rec_never_forced_compiles_and_runs() {
    assert_eq!(
        run(r#"
        use /std/{Handle, Nat, Str, Bytes};
        let make(n : Nat) -> Nat =
            let loop : Nat = loop;
            n;
        /std/print(Nat/to_str(make(Bytes/len(/std/rand/bytes(5)!))))
        "#),
        b"5"
    );
}

// A sibling signature may demand the result shape of a recursive type family while the group is still being checked. Protected slots prevent conversion from solving the knot, but shape-demanding reduction can still unfold a filled slot productively: `val : T(2)` reaches `Nat`. Indexed inductive families lower to this same shape, so the prelude depends on the distinction.
#[test]
fn recursive_group_signature_reduces_concrete_type_family() {
    assert_eq!(
        run(r#"
        use /std/{Handle, Nat, Str, Bytes};
        let taint = Bytes/len(/std/rand/bytes(3)!);
        let T(n : Nat) -> Type =
            match n
            | 0 => Nat
            | k + 1; ih => T(k)
            end
        and val : T(2) =
            taint;
        /std/print(Nat/to_str(val))
        "#),
        b"3"
    );
}
