//! Structural acceptance fixtures. Each test compiles a small `.crs` fixture to the raw, pre-Binaryen wasm module and asserts a structural property of the emitted code — a clean natural loop for a hot kernel, direct recursion, the closure ABI only where a call is genuinely unknown — and that the raw module validates and executes without Binaryen repairing control flow.
//!
//! Emitted function names are `$func/<N>` ids — a module-wide monotonic index over every reachable function, prelude included — optionally suffixed with the source hint as `$func/<N>$hint`. The index carries identity; the hint is only origin annotation. Hot kernels are still located by a distinctive literal constant baked into their arithmetic (`65537` for LCG, `1000003` for trees) or by name-independent structure (self-recursion, the shared `$func/<N>`/`$clsr/<N>` index of a function used both directly and as a closure), never by a source name. A genuine irreducible-cycle dispatcher is the `loop $$dispatch/<anchor>` the emitter names in `into_wasm::expr_emitter`; an ordinary constructor-tag `switch` lowers to a `br_table` over `$case$N`/`$tail` labels and is not a dispatcher.

use {
    crate::tests::cont_optm,
    curios_pipeline::compile_with_prelude,
    curios_runtime::{ForeignBindings, MockHost, run_bytes, shared_engine},
    curios_text::{Entrypoint, RootSource},
    curios_wasm::{Module, to_bytes},
    std::collections::BTreeSet,
};

// -- fixtures ---------------------------------------------------------------
//
// Every fixture takes a runtime taint (`List/len(proc/args!)`) so its result is not constant-folded away, and prints through `/std/print(Nat/to_str(...))` to keep the kernel live.
//
// It arrives through two bindings rather than one annotated `let`, because an annotated top-level `let` is a module *item* and an item's value body is its own sequencing region — a `!` written there could not reach the program's. The unannotated binding opens the final term instead, and the annotation rides on a local `let` inside it.

const LCG: &str = r#"
    use /std/{Handle, Nat, List, proc};
    rec loop(k : Nat, x : Nat) -> Nat =
        match k : (_) => Nat
        | 0 => x
        | kp + 1; ih => loop(kp, 75 * x % 65537)
        end;
    let taint = List/len(proc/args!);
    let n : Nat = taint;
    /std/print(Nat/to_str(loop(n, 1)))
    "#;

const TREES: &str = r#"
    use /std/{Handle, Nat, List, proc};
    induct Tree : Type
    | leaf(Nat)
    | node(Nat, Tree, Tree)
    end
    rec build(d : Nat, v : Nat) -> Tree =
        match d : (_) => Tree
        | 0 => Tree/leaf(v)
        | dp + 1; ih => Tree/node(v, build(dp, v * 2), build(dp, v * 2 + 1))
        end;
    rec sum(t : Tree) -> Nat =
        match t : (_) => Nat
        | leaf(n) => n % 1000003
        | node(n, l, r) => (n + sum(l) + sum(r)) % 1000003
        end;
    let taint = List/len(proc/args!);
    let d : Nat = taint;
    /std/print(Nat/to_str(sum(build(d, 1))))
    "#;

const HIGHER_ORDER: &str = r#"
    use /std/{Handle, Nat, Bool, List, proc};
    let pick(b : Bool) -> (Nat) -> Nat =
        match b : (_) => (Nat) -> Nat
        | true => (y) => y + 1
        | false => (y) => y * 2
        end;
    let taint = List/len(proc/args!);
    let n : Nat = taint;
    let f = pick(n <= 0);
    /std/print(Nat/to_str(f(n)))
    "#;

const DIRECT_ESCAPING: &str = r#"
    use /std/{Handle, Nat, Bool, List, proc};
    let inc(x : Nat) -> Nat = x + 1;
    let apply(g : (Nat) -> Nat, x : Nat) -> Nat = g(x);
    let select(b : Bool, g : (Nat) -> Nat) -> (Nat) -> Nat =
        match b : (_) => (Nat) -> Nat
        | true => g
        | false => (y) => y
        end;
    let taint = List/len(proc/args!);
    let n : Nat = taint;
    let escaped = select(n <= 0, inc);
    /std/print(Nat/to_str(inc(n) + apply(escaped, n)))
    "#;

const FUNCTION_ONLY: &str = r#"
    use /std/{Handle, Nat, List, proc};
    rec down(n : Nat, acc : Nat) -> Nat =
        match n : (_) => Nat
        | 0 => acc
        | p + 1; ih => down(p, acc + 1)
        end;
    let taint = List/len(proc/args!);
    let n : Nat = taint;
    /std/print(Nat/to_str(down(n, 0)))
    "#;

/// Two mutually recursive functions entered from two arms of a runtime match — the closest surface shape to an irreducible cycle. Curios has no unstructured jump, so the structurizer lays this out reducibly (no dispatcher); see [`mutual_recursion_stays_reducible`].
const MUTUAL_RECURSION: &str = r#"
    use /std/{Handle, Nat, List, proc};
    rec ping(n : Nat) -> Nat =
        match n : (_) => Nat | 0 => 0 | p + 1; ih => pong(p) end
    and pong(n : Nat) -> Nat =
        match n : (_) => Nat | 0 => 1 | p + 1; ih => ping(p) end;
    let taint = List/len(proc/args!);
    let n : Nat = taint;
    let start = n <= 0;
    /std/print(Nat/to_str(match start : (_) => Nat | true => ping(n) | false => pong(n) end))
    "#;

// -- helpers ----------------------------------------------------------------

/// Compile `source` (no external modules) to the raw, pre-Binaryen wasm module. The returned `.0` of `compile_entrypoint` is the module `into_wasm` produces; Binaryen only runs later, in `crate::to_cwasm`.
fn compile_raw(source: &str) -> Module {
    let entrypoint = source.parse::<Entrypoint>().expect("fixture parses");

    let (module, _foreigns) = compile_with_prelude(
        curios_pipeline::DEFAULT_STEP_BUDGET,
        &entrypoint,
        RootSource::none(),
        |_| {},
    )
    .expect("fixture compiles");

    module
}

/// The raw module's WAT text. Not digit-normalized: literal constants and exact token counts are load-bearing here.
fn wat(source: &str) -> String {
    compile_raw(source).to_string()
}

/// The lines of `wat` mentioning `needle` that are not the `Io` carrier's own.
///
/// Every program's tail is a description, and a description erases to a zero-argument closure — so an effect boundary allocates a closure and forces it through an indirect call no matter how the *user's* code is written. Those carry the `$io/…` hint their thunk was minted with (`io/pure`, `io/bind`, `io/write`, …), which is what lets a claim about user code stay a claim about user code. A test that dropped the distinction would either fail on every program or assert nothing.
fn user_lines<'a>(wat: &'a str, needle: &str) -> Vec<&'a str> {
    wat.lines()
        .map(str::trim)
        .filter(|line| line.contains(needle) && !line.contains("$io/"))
        .collect()
}

/// One emitted function: its `$name` and full text.
struct Function<'a> {
    name: &'a str,
    body: &'a str,
}

impl Function<'_> {
    /// How many times this function calls itself directly. `call $name` is a substring of `return_call $name`, so this counts tail and non-tail self calls alike.
    fn self_calls(&self) -> usize {
        self.body.matches(&format!("call {}", self.name)).count()
    }
}

/// Split the module WAT into its emitted functions. Each starts at a `    (func ` line (module indent); its text runs to the next one. Module items after the last function (exports, `$start`) never open a `    (func ` line, so they ride along on the final entry without introducing calls or refs of their own.
fn functions(wat: &str) -> Vec<Function<'_>> {
    const MARKER: &str = "\n    (func ";
    let mut starts = Vec::new();
    let mut cursor = 0;
    while let Some(offset) = wat[cursor..].find(MARKER) {
        starts.push(cursor + offset + 1); // point at the `  (func`, past the newline
        cursor += offset + MARKER.len();
    }

    starts
        .iter()
        .enumerate()
        .map(|(index, &start)| {
            let end = starts.get(index + 1).copied().unwrap_or(wat.len());
            let body = &wat[start..end];
            let name = body
                .trim_start()
                .strip_prefix("(func ")
                .and_then(|rest| rest.split_whitespace().next())
                .unwrap_or("");
            Function { name, body }
        })
        .collect()
}

/// The single emitted function whose body contains `needle` (asserting exactly one does).
fn function_with<'a>(functions: &'a [Function<'a>], needle: &str) -> &'a Function<'a> {
    let mut hits = functions.iter().filter(|f| f.body.contains(needle));
    let found = hits.next().expect("some function contains the needle");
    assert!(
        hits.next().is_none(),
        "`{needle}` must identify exactly one function",
    );
    found
}

/// The innermost `loop … end` enclosing the (unique) `needle`: walk backward from the needle with block/loop/if-vs-`end` nesting to the enclosing `loop ` opener, then balance forward to its matching `end`. `block`/`loop`/`if` are the only `end`-terminated openers the emitter produces.
fn loop_containing<'a>(wat: &'a str, needle: &str) -> &'a str {
    assert_eq!(
        wat.matches(needle).count(),
        1,
        "`{needle}` must be unique to slice its loop",
    );
    let needle_offset = wat.find(needle).unwrap();

    let lines: Vec<(usize, &str)> = {
        let mut offset = 0;
        wat.lines()
            .map(|line| {
                let start = offset;
                offset += line.len() + 1;
                (start, line.trim())
            })
            .collect()
    };
    let is_opener =
        |t: &str| t.starts_with("loop ") || t.starts_with("block ") || t.starts_with("if ");
    let is_closer = |t: &str| t == "end";

    let needle_line = lines
        .iter()
        .rposition(|&(start, _)| start <= needle_offset)
        .unwrap();

    // Backward to the enclosing `loop ` opener.
    let mut depth = 0usize;
    let mut loop_line = None;
    for index in (0..needle_line).rev() {
        let text = lines[index].1;
        if is_closer(text) {
            depth += 1;
        } else if is_opener(text) {
            if depth == 0 {
                if text.starts_with("loop ") {
                    loop_line = Some(index);
                    break;
                }
            } else {
                depth -= 1;
            }
        }
    }
    let loop_line = loop_line.expect("needle sits inside a loop");

    // Forward to the matching `end`.
    let mut depth = 0usize;
    let mut end_line = None;
    for (index, &(_, text)) in lines.iter().enumerate().skip(loop_line) {
        if is_opener(text) {
            depth += 1;
        } else if is_closer(text) {
            depth -= 1;
            if depth == 0 {
                end_line = Some(index);
                break;
            }
        }
    }
    let end_line = end_line.expect("the loop is balanced");

    let start = lines[loop_line].0;
    let end = lines.get(end_line + 1).map_or(wat.len(), |&(next, _)| next);
    &wat[start..end]
}

/// The `<N>` indices following every occurrence of `prefix` (e.g. `"call $func/"` for directly-called functions, `"ref.func $clsr/"` for materialized closures); the digit run stops at the optional `$hint` suffix.
fn indices(wat: &str, prefix: &str) -> BTreeSet<u32> {
    let mut set = BTreeSet::new();
    let mut cursor = 0;
    while let Some(offset) = wat[cursor..].find(prefix) {
        let after = &wat[cursor + offset + prefix.len()..];
        let digits: String = after.chars().take_while(char::is_ascii_digit).collect();
        if let Ok(index) = digits.parse::<u32>() {
            set.insert(index);
        }
        cursor += offset + prefix.len();
    }
    set
}

/// Run the raw (Binaryen-free) module: Cranelift-precompile the raw bytes directly — validation, including control-flow well-formedness, happens here, so a module Binaryen would have had to repair fails — then execute it and return captured stdout. `args` seeds `proc/args!`, which drives the taint.
fn run_raw(source: &str, args: &[&str]) -> Vec<u8> {
    let module = compile_raw(source);
    let cwasm = shared_engine()
        .precompile_module(&to_bytes(&module))
        .expect("raw module validates and Cranelift-compiles without Binaryen");

    let (system, io) = MockHost::builder().args(args).build();
    run_bytes(&cwasm, system, ForeignBindings::empty()).expect("raw module executes");
    io.output()
}

/// Run the module through the ordinary Binaryen + Cranelift path, for the same input — the reference the raw path must agree with.
fn run_binaryen(source: &str, args: &[&str]) -> Vec<u8> {
    let module = compile_raw(source);
    let cwasm = crate::to_cwasm(&module).expect("binaryen path precompiles");

    let (system, io) = MockHost::builder().args(args).build();
    run_bytes(&cwasm, system, ForeignBindings::empty()).expect("optimized module executes");
    io.output()
}

// -- LCG --------------------------------------------------------------------

/// L1: the LCG kernel reaches closure conversion as a single-entry recursive continuation. Proxy: the user `loop` is contified — the optimized high-CPS module keeps only `main`, prelude helpers, the `io/…` description thunks every effect boundary erases to, and the lambdas those lift, so the recursive kernel survives as a local continuation (a recursive `cont` with a single external entry and its own backedge), not a function. The contification mechanism is owned by `curios-cont`'s `contify_calls` tests; this pins the end-to-end result.
#[test]
fn lcg_kernel_is_single_entry_recursive_continuation() {
    let cont = cont_optm(LCG);
    assert!(
        cont.contains("NatRem") && cont.contains("65537"),
        "the kernel arithmetic must survive into the optimized cont module",
    );

    for line in cont
        .lines()
        .map(str::trim_start)
        .filter(|l| l.starts_with("fun ~f"))
    {
        // A named function prints `fun ~fN$hint(...)`: the source hint is the run after the first `$` and before the parameter list.
        let provenance = line
            .split_once('$')
            .and_then(|(_, rest)| rest.split_once('('))
            .map(|(name, _)| name)
            .unwrap_or_default();
        // An allowlist rather than a check on the loop's own name, which is only possible because every emitted function carries a hint: a prelude helper its `/std/` path, the description machinery its `io/` tag, and a lifted lambda its owner's name qualified — `/std/Handle/write/1`. A leaked `loop` matches none of them.
        assert!(
            provenance == "main"
                || provenance.starts_with("/std/")
                || provenance.starts_with("io/"),
            "the recursive loop must be contified, not left a top-level function: {line}",
        );
    }
}

/// L2/L3: the hot kernel is exactly one natural loop with a clean backedge — no nested loop, and no `$dispatch/` selector driving the iteration.
#[test]
fn lcg_hot_kernel_is_one_natural_loop() {
    let wat = wat(LCG);
    let kernel = loop_containing(&wat, "65537");

    assert_eq!(
        kernel.matches("loop ").count(),
        1,
        "the kernel must be a single natural loop, not nested loops",
    );
    assert!(
        !kernel.contains("$dispatch/"),
        "the backedge must be an ordinary loop branch, not a dispatcher selector",
    );
}

/// L4: the loop body is direct scalar arithmetic — Nat multiply (`i64.mul`, widened for its overflow check) and unsigned remainder (`i32.rem_u`) — with no closure allocation and no indirect (`call_ref`) dispatch.
#[test]
fn lcg_loop_is_scalar_no_closure_no_indirect() {
    let wat = wat(LCG);
    let kernel = loop_containing(&wat, "65537");

    assert!(
        kernel.contains("i64.mul"),
        "Nat multiply is direct scalar arithmetic"
    );
    assert!(
        kernel.contains("i32.rem_u"),
        "the modulo is direct scalar arithmetic"
    );
    assert!(
        !kernel.contains("call_ref"),
        "no indirect call in the hot loop"
    );
    assert!(
        !kernel.contains("struct.new $clsr/"),
        "no closure allocation in the hot loop"
    );
    assert!(
        !kernel.contains("struct.new $envr/"),
        "no environment allocation in the hot loop"
    );
    assert!(
        !kernel.contains("struct.new_default"),
        "no closure shell in the hot loop"
    );
}

/// L5: the loop carries its scalars in registers, so a back edge moves a register to a register. `ref.as_non_null` is the tell: every edge argument used to be loaded with it, and a parameter the representation analysis holds raw is loaded at its carrier instead — a bare `local.get`. Zero of them in the kernel is the loop-carried decision the `cps::represent` fixpoint exists to produce, and it is the one count that went to zero.
///
/// The casts do *not* go to zero and asserting that they do would be wrong: 4 `ref.cast`/`i31.get_u` pairs survive on values the loop reads from outside itself, where the coercion is correct and is the cheaper side of the trade. Nor does the `i64` widening go away — see `i64.mul` in [`lcg_loop_is_scalar_no_closure_no_indirect`] — because a `Nat` product leaving the i31 envelope must trap and `i32.mul` wraps rather than trapping, which no storage decision changes.
#[test]
fn lcg_loop_carries_its_scalars_in_registers() {
    let wat = wat(LCG);
    let kernel = loop_containing(&wat, "65537");

    assert!(
        !kernel.contains("ref.as_non_null"),
        "no edge argument in the hot loop is reboxed to cross it: {kernel}"
    );
}

// -- trees ------------------------------------------------------------------

/// T1: build and sum retain direct recursive code. `sum` is the function carrying the `1000003` modulus; `build` is the other user function with two direct self calls (the recursive `to_str` prelude helper has one). Both recurse through direct `call`/`return_call`, and — since the whole module emits no `call_ref` (see [`trees_hot_arithmetic_has_no_indirect_calls`]) — that recursion is direct.
#[test]
fn trees_build_and_sum_stay_direct_recursive() {
    let wat = wat(TREES);
    let functions = functions(&wat);

    let sum = function_with(&functions, "1000003");
    assert!(sum.self_calls() >= 1, "sum must recurse directly");

    let build = functions
        .iter()
        .filter(|f| f.name.starts_with("$func/") && !f.body.contains("1000003"))
        .find(|f| f.self_calls() >= 2)
        .expect("build recurses directly on both subtrees");
    assert!(
        build.name != sum.name,
        "build and sum are distinct direct-recursive functions",
    );
}

/// T2: the recursive arithmetic is folded to bare intrinsic instructions rather than dispatched through a witness — the invariant `Nat` operation implementations propagate through the recursive SCC and collapse to `i32` instructions, with no `call_ref` witness projection left behind. The SCC known-argument propagation that enables this is owned by `curios-cont`'s specialization tests; this pins its emitted consequence.
#[test]
fn trees_invariant_arithmetic_propagates_through_scc() {
    let wat = wat(TREES);
    let functions = functions(&wat);
    let sum = function_with(&functions, "1000003");

    assert!(
        sum.body.contains("i32.rem_u"),
        "the modulus folded to a bare instruction"
    );
    assert!(
        sum.body.contains("i32.add"),
        "the summation folded to bare instructions"
    );
    assert!(
        !sum.body.contains("call_ref"),
        "no witness dispatch survives in the recursive arithmetic",
    );
}

/// T3: the hot recursive code performs no indirect calls. Every call in the trees module is direct except at the effect boundary, where forcing a description *is* an indirect call — `main` forces the program's own description, and `io/bind` forces each of the two it sequences. The tree recursion is not among them.
///
/// Stated as "the module contains no `call_ref`" this held only while programs were direct-style; a program is a description now, so two forces are structural. Pinning `main`'s count keeps that from being a licence: an indirect call anywhere in user code, or a second one in `main`, still fails.
#[test]
fn trees_hot_arithmetic_has_no_indirect_calls() {
    let wat = wat(TREES);
    let functions = functions(&wat);

    let stray = functions
        .iter()
        .filter(|function| function.body.contains("call_ref"))
        .filter(|function| !function.name.contains("$io/") && function.name != "$func/main")
        .map(|function| function.name)
        .collect::<Vec<_>>();
    assert!(
        stray.is_empty(),
        "trees calls indirectly outside the effect boundary: {stray:?}"
    );

    let main = functions
        .iter()
        .find(|function| function.name == "$func/main")
        .expect("the module has an entry");
    assert_eq!(
        main.body.matches("call_ref").count(),
        1,
        "main forces the program's description once and calls nothing else indirectly",
    );
}

/// T4: ordinary recursive functions create no shells or mutable closure fields. The trees module allocates only data tuples (`$tpl/…` for the `Tree` nodes) — no closure (`$clsr/`) or environment (`$envr/`) structs, and no `struct.new_default` shell.
#[test]
fn trees_ordinary_recursion_has_no_shells() {
    let wat = wat(TREES);
    let closures = user_lines(&wat, "struct.new $clsr/");
    assert!(closures.is_empty(), "no closure allocation: {closures:?}");
    let envs = user_lines(&wat, "struct.new $envr/");
    assert!(envs.is_empty(), "no environment allocation: {envs:?}");
    let shells = user_lines(&wat, "struct.new_default");
    assert!(shells.is_empty(), "no closure shell: {shells:?}");
}

/// T5: constructor payloads are untouched, which is what makes the locals-only scope *observable* rather than merely intended. A `Tree/node` carries its `Nat` in a `$tpl/…` field, and every such field stays `(ref null any)` — the representation analysis reaches locals and block parameters, never a heap layout, because a field is a contract between an allocation site and every reader of it rather than one function's private decision. Widening this is the successor's subject; until then a scalar field appearing here means the scope leaked.
#[test]
fn trees_constructor_payloads_stay_boxed() {
    let wat = wat(TREES);

    let scalar_fields = wat
        .lines()
        .filter(|line| line.contains("(type $tpl/") || line.trim().starts_with("(field "))
        .filter(|line| line.contains("(field $") && !line.contains("(ref null any)"))
        .collect::<Vec<_>>();

    assert!(
        scalar_fields.is_empty(),
        "tuple payloads are uniformly boxed: {scalar_fields:?}"
    );
}

// -- general corpus ---------------------------------------------------------

/// G1: a genuinely unknown higher-order call retains the closure ABI and emits `call_ref`. `f` is selected at runtime, so it cannot be devirtualized: the module declares `$clsr/…` closure types, materializes the branches with `ref.func`, and dispatches through `call_ref`.
#[test]
fn unknown_higher_order_call_uses_closure_abi_and_call_ref() {
    let wat = wat(HIGHER_ORDER);
    assert!(
        wat.contains("call_ref"),
        "the unknown call dispatches through call_ref"
    );
    assert!(wat.contains("$clsr/"), "the closure ABI is retained");
}

/// G2: direct and escaping uses of the same function coexist. A function used both directly and as a first-class value is emitted once as `$func/<N>` (the direct callee) and once as `$clsr/<N>` (the escaping wrapper) under the same index, so the set of directly-called `$func/<N>` indices and the set of `ref.func`'d `$clsr/<N>` indices overlap.
#[test]
fn direct_and_escaping_uses_coexist() {
    let wat = wat(DIRECT_ESCAPING);
    let called_directly = indices(&wat, "call $func/");
    let escaped = indices(&wat, "ref.func $clsr/");

    assert!(
        called_directly.intersection(&escaped).next().is_some(),
        "some function must be both directly called and materialized as a closure\n\
         direct: {called_directly:?}  escaping: {escaped:?}",
    );
}

/// G3: function-only recursion produces no fallback shells. `down` is a plain recursive function; the module allocates no closure (`$clsr/`) and no `struct.new_default` shell for it.
#[test]
fn function_only_recursion_has_no_fallback_shells() {
    let wat = wat(FUNCTION_ONLY);
    // Allocation, not mention: a module that forces a description at all declares the closure *type* for the arity it forces at, and names it in the `call_ref`. What `down` must not do is allocate one.
    let closures = user_lines(&wat, "struct.new $clsr/");
    assert!(
        closures.is_empty(),
        "function-only recursion needs no closures: {closures:?}"
    );
    let envs = user_lines(&wat, "struct.new $envr/");
    assert!(
        envs.is_empty(),
        "function-only recursion needs no environments: {envs:?}"
    );
    let shells = user_lines(&wat, "struct.new_default");
    assert!(
        shells.is_empty(),
        "function-only recursion needs no shells: {shells:?}"
    );
}

/// G4: ordinary corpus cases use no irreducible fallback. None of the ordinary fixtures — including mutual recursion — emit a `loop $$dispatch/` localized dispatcher; their constructor-tag matches lower to ordinary `br_table` data switches over `$case$N` labels, which are not dispatchers.
#[test]
fn ordinary_corpus_uses_no_irreducible_fallback() {
    for (label, source) in [
        ("lcg", LCG),
        ("trees", TREES),
        ("function-only", FUNCTION_ONLY),
        ("higher-order", HIGHER_ORDER),
        ("direct/escaping", DIRECT_ESCAPING),
    ] {
        assert!(
            !wat(source).contains("$dispatch/"),
            "{label} must not need an irreducible dispatcher",
        );
    }
}

/// G5: the one-localized-dispatcher guarantee. Curios surface syntax has no unstructured jump, so even mutual recursion entered from two arms is structured reducibly (no `$dispatch/`) — there is no `.crs` program that produces a genuine irreducible cycle. The dispatcher path (exactly one `loop $$dispatch/` per irreducible component) is therefore owned and asserted at the backend-unit level by `curios-cont`'s `an_irreducible_component_uses_exactly_one_localized_dispatcher` in `into_wasm::emit_tests`; this test pins the surface-level fact that motivates that ownership boundary.
#[test]
fn mutual_recursion_stays_reducible() {
    assert!(
        !wat(MUTUAL_RECURSION).contains("$dispatch/"),
        "mutual recursion must structure reducibly, without a localized dispatcher",
    );
}

/// G6: the raw, pre-Binaryen wasm validates and executes without Binaryen repairing control flow. `run_raw` Cranelift-compiles the raw bytes directly (validation, including control-flow well-formedness, happens there) and runs them; its output must match the ordinary Binaryen path for the same input.
#[test]
fn raw_wasm_validates_and_executes_without_binaryen() {
    for (label, source) in [
        ("lcg", LCG),
        ("trees", TREES),
        ("higher-order", HIGHER_ORDER),
        ("direct/escaping", DIRECT_ESCAPING),
        ("function-only", FUNCTION_ONLY),
        ("mutual-recursion", MUTUAL_RECURSION),
    ] {
        let args = ["prog", "a", "b", "c"];
        let raw = run_raw(source, &args);
        assert!(!raw.is_empty(), "{label} raw module produced output");
        assert_eq!(
            raw,
            run_binaryen(source, &args),
            "{label} raw output must match the Binaryen-optimized output",
        );
    }
}
