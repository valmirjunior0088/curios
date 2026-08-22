//! Structural acceptance fixtures. Each test compiles a small `.crs` fixture to the raw, pre-Binaryen wasm module and asserts a structural property of the emitted code — a clean natural loop for a hot kernel, direct recursion, the closure ABI only where a call is genuinely unknown — and that the raw module validates and executes without Binaryen repairing control flow.
//!
//! Emitted function names are `$func/<N>` ids — a module-wide monotonic index over every reachable function, prelude included — optionally suffixed with the source hint as `$func/<N>$hint`. The index carries identity; the hint is only origin annotation. Hot kernels are still located by a distinctive literal constant baked into their arithmetic (`65537` for LCG, `1000003` for trees) or by name-independent structure (self-recursion, the shared `$func/<N>`/`$clsr/<N>` index of a function used both directly and as a closure), never by a source name. A genuine irreducible-cycle dispatcher is the `loop $$dispatch/<anchor>` the emitter names in `into_wasm::expr_emitter`; an ordinary constructor-tag `switch` is not a dispatcher whatever shape it takes — a `br_table` over `$case$N`/`$tail` labels for three or more cases, a plain `if` for the two-way and one-way shapes.

use {
    crate::tests::{census_settles, cont_optm, run},
    curios_pipeline::compile_with_prelude,
    curios_runtime::{ForeignBindings, MockHost, precompile, run_bytes},
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

/// A callee returning two distinct constructors to two different callers — the intersection no existing pass reaches. Two external call sites is one too many for contification, whose whole admissibility rests on there being a single return context; the body is past the multi-site inline budget; and neither caller can see the construction, since it is built inside `advance` and arrives as an opaque continuation parameter. `more` carries three fields and `done` two, so the class is four slots wide and the shorter constructor exercises the filler. Neither constructor is unary on purpose: the immediate encoding hands a lone `done(Nat)` edge back as a bare payload rather than a construction, which would quietly retire this fixture's premise — exactly the decay the test's premise check exists to catch, and how it caught the encoding when it landed.
///
/// **The arithmetic in `advance` is sized, not decorative.** The middle premise is a claim about `MULTI_SITE_INLINE_LIMIT`, and the fixture stopped holding it once when that constant moved — so the body carries enough operations to stay well clear rather than to just clear the value of the day. Each of the three functions also owns a distinct modulus, which is what lets [`a_returned_constructor_is_delivered_as_its_fields`] state that the callee is a different function from its callers instead of merely that some function exists.
const SPLIT_RETURN: &str = r#"
    use /std/{Handle, Nat, List, proc};
    induct Step : Type
    | more(Nat, Nat, Nat)
    | done(Nat, Nat)
    end
    let advance(x : Nat) -> Step =
        match x % 7 : (_) => Step
        | 0 => Step/done((x * 3 + 1) * 13 % 20011, (x * 7 + 3) % 20011)
        | k + 1 =>
            let u = (x * 5 + 7) % 20011;
            let v = (u * 3 + 2) % 20011;
            let w = (k * 2 + 11) % 20011;
            Step/more((v * 9 + 4) % 20011, (w * 6 + 5) % 20011, (u * 2 + 9) % 20011)
        end;
    let first(a : Nat) -> Nat =
        match advance(a) : (_) => Nat
        | more(p, q, t) => (p + q + t) % 30011
        | done(r, s) => (r + s) % 30011
        end;
    let second(b : Nat) -> Nat =
        match advance(b + 1) : (_) => Nat
        | more(p, q, t) => (p * 2 + q + t) % 40009
        | done(r, s) => (r * 3 + s) % 40009
        end;
    let taint = List/len(proc/args!);
    let n : Nat = taint;
    /std/print(Nat/to_str(first(n) + second(n)))
    "#;

/// A recursive callee returning a closure that every caller immediately applies — the shape a monadic carrier produces, where an action *is* a function so each step allocates one.
///
/// It also returns a *different* closure per branch, which is the property separating absorption from defunctionalization: the rewrite never learns which of the two comes back, because both are applied to the same argument.
///
/// Nothing existing reaches it. The two lambdas leave as return values, so the recursion `walk → (s) => walk(m)(…) → walk` closes through an application of a value and no known-callee analysis sees a cycle at all; the closure is genuinely built and genuinely called through.
///
/// Two details of its shape are load-bearing rather than decoration, and a smaller fixture measures nothing. The arithmetic puts `walk`'s extent past the multi-site inline budget — without it the inliner *peels* the recursion into itself, since the same invisible cycle that hides the loop from this transform also leaves `walk` unmarked as recursive, and no known call site survives to rewrite. And the applied argument is bound to `c` ahead of the call rather than written into the call, because an argument computed inside the continuation that receives the closure cannot move above it. See [`a_returned_closure_every_caller_applies_is_absorbed`].
const UNCURRY: &str = r#"
    use /std/{Handle, Nat, List, proc};
    rec walk(n : Nat) -> (Nat) -> Nat =
        match n : (_) => (Nat) -> Nat
        | 0 => (s) => (s * 7 + 13) % 30011
        | m + 1 => (s) =>
            let a = (s * 5 + 3) % 30011;
            let b = (a * 11 + 17) % 30011;
            let c = (b * 3 + 19) % 30011;
            walk(m)(c)
        end;
    let taint = List/len(proc/args!);
    let n : Nat = taint;
    /std/print(Nat/to_str(walk(n)(1)))
    "#;

/// A capture-free closure selected and applied inside a loop — the constant-closure interning shape. Each iteration picks one of two lambdas by a runtime condition, so the call stays genuinely unknown (the parameter joins two closures, exactly [`HIGHER_ORDER`]'s conflict), but neither lambda captures anything: with the code field an ordinary `i32`, both are constant aggregates, so the loop must reference two module consts rather than construct an environment per iteration.
const LOOPED_PICK: &str = r#"
    use /std/{Handle, Nat, Bool, List, proc};
    rec spin(k : Nat, acc : Nat) -> Nat =
        match k : (_) => Nat
        | 0 => acc
        | p + 1; ih =>
            let f = match acc % 2 <= 0 : (_) => (Nat) -> Nat
                | true => (y) => (y * 7 + 1) % 30011
                | false => (y) => (y * 3 + 5) % 30011
                end;
            spin(p, f(acc) % 40009)
        end;
    let taint = List/len(proc/args!);
    let n : Nat = taint;
    /std/print(Nat/to_str(spin(n, 1)))
    "#;

/// A variant carried round a loop whose payload the analysis holds in a register — the shape that puts a variant-width filler in a *raw* slot.
///
/// [`SPLIT_RETURN`] exercises the filler too, but only through the return protocol, whose slots are boxed by construction; that is why it never caught this. Here the variant reaches a continuation parameter instead, and the parameter's uses demand an unboxed `Flt`, so the edges carry `f32` and the `none` edge has no value to carry. The payload must be a `Flt` rather than a `Nat`: a `Nat` filler and its slot share the `i31` carrier, so the wrong-carrier constant is indistinguishable from a right one.
///
/// **The `none` edge has to be taken, and taken while other iterations take `some`.** A loop is what buys that: `o` is genuinely joined from both constructors, and the payload is read only under the tag. `p % 3` alternates the two so neither edge is dead, and the final iteration lands on `some`, so a run that reads the filler is reading something the program never stored.
const VARIANT_FILLER: &str = r#"
    use /std/{Handle, Nat, Flt, Option, List, proc};
    rec spin(k : Nat, o : Option(Flt)) -> Flt =
        match k : (_) => Flt
        | 0 => Option/unwrap_or(o, +0.25)
        | p + 1; ih =>
            let next =
                match Nat/eql(p % 3, 0) : (_) => Option(Flt)
                | true => Option/some(Nat/to_flt(p) * +1.5 + +2.0)
                | false => Option/none()
                end;
            spin(p, next)
        end;
    let taint = List/len(proc/args!);
    let n : Nat = taint * 4 + 4;
    /std/print(Flt/to_str(spin(n, Option/none())))
    "#;

/// An idiomatic string walk: `/std/Str/fold` over a string the program derives at runtime.
///
/// The string comes from `Nat/to_str` rather than a literal or `Str/of_bytes`, and both choices are load-bearing. A literal would let partial evaluation unroll the walk over known bytes, so the fixture would assert nothing about a loop; `of_bytes` would drag in `/std/Str/utf8/check`, whose own encoding still returns a function per byte and whose allocations would swamp the claim. What is left is the walk itself.
const STRING_WALK: &str = r#"
    use /std/{Handle, Nat, Str, Char, List, proc};
    let taint = List/len(proc/args!);
    let n : Nat = taint;
    let text : Str = Nat/to_str(n);
    /std/print(Nat/to_str(Str/fold(text, 0, (codepoint, acc) => acc + Char/to_nat(codepoint))))
    "#;

// -- helpers ----------------------------------------------------------------

/// Compile `source` (no external modules) to the raw, pre-Binaryen wasm module. The returned `.0` of `compile_entrypoint` is the module `into_wasm` produces; Binaryen only runs later, in `crate::to_cwasm`.
pub(super) fn compile_raw(source: &str) -> Module {
    let entrypoint = source.parse::<Entrypoint>().expect("fixture parses");

    let (module, _foreigns) = compile_with_prelude(
        curios_pipeline::DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |_| {},
    )
    .expect("fixture compiles");

    module
}

/// The raw module's WAT text. Not digit-normalized: literal constants and exact token counts are load-bearing here.
pub(super) fn wat(source: &str) -> String {
    compile_raw(source).to_string()
}

/// The lines allocating `needle` that are not the `Io` carrier's own.
///
/// Every program's tail is a description, and a description erases to a zero-argument closure — so an effect boundary allocates a closure and forces it through an indirect call no matter how the *user's* code is written. Those carry the `$io/…` hint their thunk was minted with (`io/pure`, `io/bind`, `io/write`, …), which is what lets a claim about user code stay a claim about user code. A test that dropped the distinction would either fail on every program or assert nothing.
///
/// **The separation is by what the line names, and that is why this is for allocations and nothing else.** An allocation names its own definition, so `struct.new $clsr/451$io/bind` carries the hint even though the description is built *inside* user code — which is exactly where it is built, so filtering by enclosing function would discard the distinction this exists to make. An indirect call names the arity's table and type instead: `call_indirect $clsr/0 $clsr/0` identifies no callee, and a needle like that would silently keep every `Io` force while looking like it had excluded them. Those claims belong to [`user_functions_with`], where the enclosing function is the only thing left to separate on. The assertion below is what keeps the choice from being made by accident.
pub(super) fn user_allocations<'a>(wat: &'a str, needle: &str) -> Vec<&'a str> {
    assert!(
        needle.starts_with("struct.new") || needle.starts_with("array.new"),
        "`{needle}` names no definition of its own, so the `$io/` hint cannot separate carrier from user code — use `user_functions_with`",
    );
    wat.lines()
        .map(str::trim)
        .filter(|line| line.contains(needle) && !line.contains("$io/"))
        .collect()
}

/// The names of the user's own emitted functions whose bodies contain `needle`.
///
/// The counterpart to [`user_allocations`] for everything an instruction does not name itself. Two functions are the effect boundary rather than the user's code: one carrying the `$io/…` hint of the thunk it was minted for, and `$func/main`, which forces the program's own description exactly once because a program *is* a description.
fn user_functions_with<'a>(functions: &'a [Function<'a>], needle: &str) -> Vec<&'a str> {
    functions
        .iter()
        .filter(|function| function.body.contains(needle))
        .filter(|function| !function.name.contains("$io/") && function.name != "$func/main")
        .map(|function| function.name)
        .collect()
}

/// One emitted function: its `$name` and full text.
pub(super) struct Function<'a> {
    pub(super) name: &'a str,
    pub(super) body: &'a str,
}

impl Function<'_> {
    /// How many times this function calls itself directly. `call $name` is a substring of `return_call $name`, so this counts tail and non-tail self calls alike.
    fn self_calls(&self) -> usize {
        self.body.matches(&format!("call {}", self.name)).count()
    }
}

/// Split the module WAT into its emitted functions. Each starts at a `    (func ` line (module indent); its text runs to the next one. Module items after the last function (exports, `$start`) never open a `    (func ` line, so they ride along on the final entry without introducing calls or refs of their own.
pub(super) fn functions(wat: &str) -> Vec<Function<'_>> {
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

/// The single emitted function whose body contains `needle`, asserting it is a callee that *survived* rather than whatever the needle was inlined into.
///
/// "Exactly one function mentions it" is not that fact, and the gap is what a moved budget walks straight into. Inlining a callee into two callers puts the needle in both, and the count catches it — but inlining those callers into a common ancestor merges the mentions back down to one, and the count passes while handing back the ancestor. That is not hypothetical: raising `MULTI_SITE_INLINE_LIMIT` folded `SPLIT_RETURN`'s `first` and `second` into `main` and then `advance` into both, and the test failed claiming `main` should return three result slots — pointing at the wrong function entirely, and one lucky substring away from passing while asserting nothing.
///
/// Excluding the entry closes that case and reports it in the terms the reader needs, which is what the fixture must change rather than what the assertion saw. A fixture wanting more than this — that the callee is distinct from a *particular* caller rather than merely from the entry — gives each participant its own constant and compares the names, as [`a_returned_constructor_is_delivered_as_its_fields`] does.
fn function_with<'a>(functions: &'a [Function<'a>], needle: &str) -> &'a Function<'a> {
    let hits = functions
        .iter()
        .filter(|function| function.body.contains(needle))
        .collect::<Vec<_>>();
    let names = hits
        .iter()
        .map(|function| function.name)
        .collect::<Vec<_>>();
    assert_eq!(
        hits.len(),
        1,
        "`{needle}` must identify exactly one function, and identifies {names:?} — the callee it names was inlined into its callers, so grow it past MULTI_SITE_INLINE_LIMIT to restore the fixture's premise",
    );
    let found = hits[0];
    assert_ne!(
        found.name, "$func/main",
        "`{needle}` survives only inside the entry, so the callee it names was inlined away along with its callers — grow it past MULTI_SITE_INLINE_LIMIT to restore the fixture's premise",
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

/// The `<N>` indices following every occurrence of `prefix` (e.g. `"call $func/"` for directly-called functions, `"struct.new $envr/"` for materialized closures); the digit run stops at the optional `$hint` suffix.
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
    let cwasm = precompile(&to_bytes(&module))
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
        !kernel.contains("call_indirect"),
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
        !sum.body.contains("call_indirect"),
        "no witness dispatch survives in the recursive arithmetic",
    );
}

/// T3: the hot recursive code performs no indirect calls. Every call in the trees module is direct except at the effect boundary, where forcing a description *is* an indirect call — `main` forces the program's own description, and `io/bind` forces each of the two it sequences. The tree recursion is not among them.
///
/// Stated as "the module contains no indirect dispatch" this held only while programs were direct-style; a program is a description now, so two forces are structural. Pinning `main`'s count keeps that from being a licence: an indirect call anywhere in user code, or a second one in `main`, still fails.
#[test]
fn trees_hot_arithmetic_has_no_indirect_calls() {
    let wat = wat(TREES);
    let functions = functions(&wat);

    let stray = user_functions_with(&functions, "call_indirect");
    assert!(
        stray.is_empty(),
        "trees calls indirectly outside the effect boundary: {stray:?}"
    );

    let main = functions
        .iter()
        .find(|function| function.name == "$func/main")
        .expect("the module has an entry");
    assert_eq!(
        main.body.matches("call_indirect").count(),
        1,
        "main forces the program's description once and calls nothing else indirectly",
    );
}

/// T4: ordinary recursive functions allocate no closures. The trees module allocates only data tuples (`$tuple/…` for the `Tree` nodes) — no closure (`$clsr/`) or environment (`$envr/`) structs.
#[test]
fn trees_ordinary_recursion_allocates_no_closures() {
    let wat = wat(TREES);
    let closures = user_allocations(&wat, "struct.new $clsr/");
    assert!(closures.is_empty(), "no closure allocation: {closures:?}");
    let envs = user_allocations(&wat, "struct.new $envr/");
    assert!(envs.is_empty(), "no environment allocation: {envs:?}");
}

/// A string walk allocates nothing per character.
///
/// `/std/Str/fold` used to be an induction over the bytes whose motive was a *function* of the scan state and the accumulator, because a right fold cannot carry a value leftwards any other way. Every step therefore returned a closure: the walk built `step₀ ∘ … ∘ base` and applied it once, so N characters cost N environment allocations and N indirect calls before any of the user's own work ran. It is now a `rec` whose parameters carry the scan state and the accumulator, and whose tail call advances them.
///
/// **What this asserts is the property, not the spelling.** Any encoding that captures per character reintroduces an environment allocation here, whatever it is named — which is what makes this survive the next person to reach for the induction form.
///
/// Measured when it landed, at N = 1 000 000 on `programs/parse_digits.crs` and `programs/parse_bindless.crs`: 2.31 s to 1.07 s and 2.23 s to 1.01 s, with the emitted `$envr/…$/std/Str/fold/…` sites going from two to none. The figures live beside the probe that reproduces them, in [`super::ladder`].
#[test]
fn a_string_walk_allocates_no_closure_per_character() {
    let wat = wat(STRING_WALK);
    let envs = user_allocations(&wat, "struct.new $envr/");
    assert!(
        envs.is_empty(),
        "the walk carries its state in parameters, so nothing is captured per character: {envs:?}"
    );
}

/// T5: constructor payloads are untouched, which is what makes the locals-only scope *observable* rather than merely intended. A `Tree/node` carries its `Nat` in a `$tuple/…` field, and every such field stays `(ref null any)` — the representation analysis reaches locals and block parameters, never a heap layout, because a field is a contract between an allocation site and every reader of it rather than one function's private decision. Widening this is the successor's subject; until then a scalar field appearing here means the scope leaked.
#[test]
fn trees_constructor_payloads_stay_boxed() {
    let wat = wat(TREES);

    let scalar_fields = wat
        .lines()
        .filter(|line| line.contains("(type $tuple/") || line.trim().starts_with("(field "))
        .filter(|line| line.contains("(field $") && !line.contains("(ref null any)"))
        .collect::<Vec<_>>();

    assert!(
        scalar_fields.is_empty(),
        "tuple payloads are uniformly boxed: {scalar_fields:?}"
    );
}

/// T6: the leaf constructor rides its payload — the immediate encoding. `build`'s leaf arm returns the payload with no allocation, so its body holds exactly one construction (the node's), and `sum` discriminates leaf from node with `ref.test (ref i31)` in place of a tag read — with only one boxed constructor the tag is never read, so no `$tuple/1` cast survives in `sum`.
///
/// # What the encoding was worth, and how to retake it
///
/// Native binaries, the ladder's protocol — `cargo run --package curios -- compile programs/trees/trees.crs -o /tmp/trees`, then `echo 21 | /usr/bin/time -v /tmp/trees`, five runs, `user` seconds and max RSS. Taken **2026-08-17** on x86-64 Linux, the before row the same day at the commit before the encoding:
///
/// | Encoding | `user` | Max RSS |
/// | --- | --- | --- |
/// | tagged leaves | 0.47–0.53 s | 266 MB |
/// | leaves ride their payloads | 0.25–0.27 s | 134 MB |
///
/// Leaves are half of a perfect tree's 2^(D+1)−1 objects, and under the all-live semi-space collector halving the live bytes also halves what every collection copies — which is why the time falls with the memory rather than by the allocation count alone. `lcg` is unmoved (no variants). Both programs printed their anchors (`trees(10) = 96122`, `trees(21) = 536864`, `lcg(8) = 9345`) before either figure was read.
#[test]
fn trees_leaf_rides_its_payload() {
    let wat = wat(TREES);
    let functions = functions(&wat);

    let sum = function_with(&functions, "1000003");
    assert!(
        sum.body.contains("ref.test (ref i31)"),
        "sum dispatches on the value's kind: {}",
        sum.body
    );
    assert!(
        !sum.body.contains("$tuple/1"),
        "no tag read survives in sum: {}",
        sum.body
    );

    let build = functions
        .iter()
        .filter(|f| f.name.starts_with("$func/") && !f.body.contains("1000003"))
        .find(|f| f.self_calls() >= 2)
        .expect("build recurses directly on both subtrees");
    assert_eq!(
        build.body.matches("struct.new").count(),
        1,
        "build allocates the node and nothing else: {}",
        build.body
    );
}

// -- general corpus ---------------------------------------------------------

/// G1: a genuinely unknown higher-order call retains the closure ABI and dispatches through its arity's typed table. `f` is selected at runtime, so it cannot be devirtualized: the module declares `$clsr/…` closure types, materializes the branches as environments carrying their body's `i32` table index, and dispatches through `call_indirect`.
#[test]
fn unknown_higher_order_call_uses_closure_abi_and_call_indirect() {
    let wat = wat(HIGHER_ORDER);
    assert!(
        wat.contains("call_indirect $clsr/"),
        "the unknown call dispatches through its arity's closure table"
    );
    assert!(wat.contains("$clsr/"), "the closure ABI is retained");
}

/// The closure ABI's code field is an `i32` table index, and nothing in *function code* touches a funcref. Construction writes `i32.const`, dispatch reads the field into `call_indirect`, and one typed table per arity — `(ref null $clsr/N)`, filled by an active element segment at offset 1, slot 0 null so a zeroed code field traps — carries that arity's bodies. The element type is what satisfies the `call_indirect` signature check statically, so the per-dispatch runtime subtype check has no site left to fire on; the only `ref.func`s in a module are the segments' items, and `call_ref` is absent everywhere.
#[test]
fn closures_carry_their_code_as_a_table_index() {
    for (label, source) in [
        ("lcg", LCG),
        ("trees", TREES),
        ("higher-order", HIGHER_ORDER),
        ("direct/escaping", DIRECT_ESCAPING),
        ("function-only", FUNCTION_ONLY),
        ("mutual-recursion", MUTUAL_RECURSION),
        ("split-return", SPLIT_RETURN),
        ("uncurry", UNCURRY),
        ("string-walk", STRING_WALK),
        ("looped-pick", LOOPED_PICK),
    ] {
        let wat = wat(source);
        assert_eq!(
            wat.matches("ref.func").count(),
            wat.matches("(item ref.func").count(),
            "{label}: every funcref materialization is an element-segment item",
        );
        assert!(
            !wat.contains("call_ref"),
            "{label}: no dispatch reads a funcref back",
        );
    }

    let wat = wat(HIGHER_ORDER);
    assert!(
        wat.contains("(field $! i32)"),
        "the environment's code field is an ordinary i32",
    );
    assert!(
        wat.contains("(table $clsr/") && wat.contains("(ref null $clsr/"),
        "each arity's typed table holds its closure bodies",
    );
    assert!(
        wat.contains("(offset i32.const 1) (ref $clsr/"),
        "an active typed segment fills each table from slot 1, leaving slot 0 null",
    );
}

/// A capture-free closure constructed in a loop pins as a module const: the constant hoister interns it like any constant aggregate — the swap made its code field an `i32`, dissolving the exclusion that kept closures inline to keep `ref.func` out of the start function — so the loop's arms reference globals and no per-iteration environment construction survives in function code. The environments are built exactly once, in `$start`.
#[test]
fn a_capture_free_closure_in_a_loop_interns_as_a_const() {
    let wat = wat(LOOPED_PICK);

    // `spin` owns the 40009 modulus and is contified into the entry, so the claim is made of the loop itself; the two lambdas own 30011 and are lifted to their own functions.
    let kernel = loop_containing(&wat, "40009");
    assert!(
        !kernel.contains("struct.new $envr/"),
        "the loop constructs no environment per iteration: {kernel}",
    );
    assert!(
        kernel.contains("global.get $const/"),
        "the arms reference the interned consts instead: {kernel}",
    );

    let functions = functions(&wat);
    let start = functions
        .iter()
        .find(|function| function.name == "$start")
        .expect("the module has a start function");
    assert!(
        start.body.matches("struct.new $envr/").count() >= 2,
        "both lambdas materialize once, at instantiation: {}",
        start.body,
    );
}

/// G2: direct and escaping uses of the same function coexist. A function used both directly and as a first-class value is emitted once as `$func/<N>` (the direct callee) and once as `$clsr/<N>` (the escaping wrapper) under the same index, so the set of directly-called `$func/<N>` indices and the set of allocated `$envr/<N>` environments overlap — the environment carries its wrapper's index, and its allocation is what materializing the closure is now.
#[test]
fn direct_and_escaping_uses_coexist() {
    let wat = wat(DIRECT_ESCAPING);
    let called_directly = indices(&wat, "call $func/");
    let escaped = indices(&wat, "struct.new $envr/");

    assert!(
        called_directly.intersection(&escaped).next().is_some(),
        "some function must be both directly called and materialized as a closure\n\
         direct: {called_directly:?}  escaping: {escaped:?}",
    );
}

/// G3: function-only recursion allocates no closure. `down` is a plain recursive function; the module allocates no closure (`$clsr/`) or environment (`$envr/`) for it.
#[test]
fn function_only_recursion_allocates_no_closures() {
    let wat = wat(FUNCTION_ONLY);
    // Allocation, not mention: a module that forces a description at all declares the closure *type* for the arity it forces at, and names it in the `call_ref`. What `down` must not do is allocate one.
    let closures = user_allocations(&wat, "struct.new $clsr/");
    assert!(
        closures.is_empty(),
        "function-only recursion needs no closures: {closures:?}"
    );
    let envs = user_allocations(&wat, "struct.new $envr/");
    assert!(
        envs.is_empty(),
        "function-only recursion needs no environments: {envs:?}"
    );
}

/// G4: ordinary corpus cases use no irreducible fallback. None of the ordinary fixtures — including mutual recursion — emit a `loop $$dispatch/` localized dispatcher; their constructor-tag matches lower to ordinary data switches — a `br_table` over `$case$N` labels where the family is wide enough to want one, an `if` where it is not — and neither is a dispatcher.
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

/// A returned constructor is handed back as its fields rather than as a heap tuple, so nothing allocates it and nothing takes it apart.
///
/// The fixture is the intersection the return protocol exists for: too many call sites to contify, too large to inline, and a construction no caller can see. Before the protocol every one of those exclusions held and the tuple survived; the assertion is that the callee both declares several results and allocates nothing to fill them.
///
/// The premise is checked before the claim, because it is the half that decays: "too large to inline" is a statement about a constant that may move, and the two exclusions around it are structural. A fixture that has quietly lost its premise asserts nothing while still passing, so the distinctness check earns its place ahead of the test's actual subject.
#[test]
fn a_returned_constructor_is_delivered_as_its_fields() {
    let wat = wat(SPLIT_RETURN);
    let functions = functions(&wat);
    // Located by their own arithmetic rather than by name, which is not stable across the passes this exercises. The callers are expected to fold into the entry, being single-site; what must not happen is the callee folding in with them.
    let advance = function_with(&functions, "20011");
    for caller in ["30011", "40009"] {
        let home = functions
            .iter()
            .find(|function| function.body.contains(caller))
            .unwrap_or_else(|| panic!("a caller keeps its own modulus somewhere: {caller}"));
        assert_ne!(
            advance.name, home.name,
            "the callee must stay a function distinct from its callers, or there is no returned construction to deliver",
        );
    }

    assert!(
        advance
            .body
            .contains("(result (ref any) (ref any) (ref any) (ref any))"),
        "the callee must return the class's four slots: {}",
        advance.name,
    );
    assert!(
        !advance.body.contains("struct.new") && !advance.body.contains("array.new"),
        "nothing may be allocated to carry a result that is now handed back in registers: {}",
        advance.name,
    );
}

/// A variant-width filler is built at the carrier its destination slot is held at, not at the filler's own.
///
/// `fields.rs` justified the filler by unreadness — "a read at that index is reachable only where the discriminant says a wider constructor travelled" — and that was false of the emitted code, because `Context::jump_instrs` coerces *every* edge argument to the destination parameter's carrier before the tag is examined. A literal `Nat(0)` standing in a raw `Flt` slot therefore reached a `ref.cast (ref $flt)` over an `i31` and trapped, on the `none` edge, for a value nothing would have read.
///
/// The premise is asserted before the claim, and it is the half that decays: a pass that stopped raising this parameter to a register, or stopped splitting the variant at all, would leave a fixture that passes while measuring nothing. `f32.const 0` in the loop is the filler — the fixture's own float constants are `0.25`, `1.5` and `2`, none of them zero — so its presence says both that the split fired and that the slot is raw.
///
/// **Positive control, run 2026-08-18.** Restoring `CpsAtom::Literal(CpsLiteral::Nat(0))` at the `split_parameters` filler site and rebuilding makes this fixture fail as `execution failed: error while executing at wasm backtrace: 0: 0x70b - <wasm function 4>`. Reproduce by reverting that one line.
#[test]
fn a_variant_filler_is_built_at_its_destination_carrier() {
    let wat = wat(VARIANT_FILLER);
    assert!(
        wat.contains("f32.const 0\n"),
        "the premise: a filler must reach a register-held `Flt` slot, or this fixture measures nothing",
    );

    let args = ["prog", "a", "b", "c"];
    assert_eq!(
        run_raw(VARIANT_FILLER, &args),
        b"+2".to_vec(),
        "the `none` edge must carry a filler the destination can hold, rather than one it casts and traps on",
    );
}

/// A returned closure that every caller applies is absorbed into the callee, so nothing allocates it and nothing calls through it.
///
/// All three are asserted because each alone is satisfiable the wrong way. A module that allocated nothing but still dispatched indirectly would have moved the cost rather than removed it; one that dispatched directly while still allocating would pay for a closure nothing reaches; and both hold vacuously of a module where the recursion was simply peeled away, which is what a fixture inside the inline budget produces.
///
/// The `call_indirect` exemption is `main`'s and the `$io/` thunks', following [`trees_hot_arithmetic_has_no_indirect_calls`]: a program *is* a description now, so forcing one is structurally an indirect call. It goes through [`user_functions_with`] rather than [`user_allocations`] because the instruction names the table and the closure *type* it calls through and never the callee, leaving the enclosing function as the only thing that says whose call it is.
///
/// **The environment goes with the closure, and that is lowering's doing rather than this transform's.** A free value reaches a directly-called function as a lifted parameter and an escaping one as an environment field — one decision, taken in `machine::lower` — so absorbing the application moves `walk`'s captured `n` from the second case to the first for free. The emitted pair takes it as a parameter and allocates nothing.
#[test]
fn a_returned_closure_every_caller_applies_is_absorbed() {
    let wat = wat(UNCURRY);
    let functions = functions(&wat);

    let closures = user_allocations(&wat, "struct.new $clsr/");
    assert!(
        closures.is_empty(),
        "an absorbed closure is never built: {closures:?}"
    );

    let indirect = user_functions_with(&functions, "call_indirect");
    assert!(
        indirect.is_empty(),
        "nor does the application it received stay indirect: {indirect:?}"
    );

    // Located by the walk's own arithmetic, since neither surviving name is load-bearing: the absorbed step becomes a parameter of the function that used to return it, so the pair is identified by what it computes.
    let stepping = functions
        .iter()
        .filter(|function| function.body.contains("30011"))
        .collect::<Vec<_>>();
    assert_eq!(
        stepping.len(),
        2,
        "the walk survives as the two functions it was written as, rather than being peeled",
    );
    for function in stepping {
        assert!(
            function.body.contains("return_call $func/"),
            "and each hands on directly and in tail position, keeping the loop flat: {}",
            function.name,
        );
    }
}

/// What the closure table index is worth at product level, and where the profile's attribution had already expired.
///
/// Run it with:
///
/// ```sh
/// cargo test --package curios --lib -- --ignored --nocapture closure_index_dispatch_measurements
/// ```
///
/// It asserts nothing. The structural claim is [`closures_carry_their_code_as_a_table_index`]'s to make; this prints the static shape of the swap over the corpus — table slots, dispatch sites, environment allocations — so the timings below stay pinned to the modules that produced them.
///
/// # The native timings, taken 2026-08-17
///
/// Native binaries, debug-profile compiler, x86-64 Linux: `target/debug/curios compile <program> -o <path>` at the swap commit (after) and at its parent `1e079440` (before), then `echo <N> | /usr/bin/time -v <bin>`, five runs per arm, `user` seconds, arms interleaved run-by-run to keep thermal drift out of the comparison (a first non-interleaved pass showed parse_digits's before arm drifting 1.14 → 0.80 across five runs; the interleaved figures below are the stable ones). Every pair printed identical output before any figure was read. Max RSS was flat on every pair (~7 MB; trees 134 MB both arms).
///
/// | Program | Input | before | after |
/// | --- | --- | --- | --- |
/// | `monad_io` | 10 000 000 | 1.26 1.26 1.26 1.26 1.29 | 0.22 0.21 0.21 0.22 0.22 |
/// | `parse_digits` | 1 000 000 | 0.80 0.80 0.79 0.78 0.80 | 0.59 0.58 0.59 0.58 0.58 |
/// | `parse_multibyte` | 300 000 | 0.63 0.60 0.60 0.60 0.61 | 0.55 0.54 0.53 0.55 0.54 |
/// | `rng_state` | 10 000 000 | 0.04–0.06 | 0.04–0.06 |
/// | `rng_manual` | 10 000 000 | 0.03 | 0.03 |
/// | `state_monad` | 1 000 000 | 0.00 | 0.00 |
/// | `lcg` | 100 000 000 | 0.31 | 0.31 |
/// | `trees` | 21 | 0.32–0.38 | 0.27–0.36 |
///
/// **The monadic loop the mechanism was priced for moved 5.9×** — `monad_io` binds a description per step, so each iteration built one closure and forced it, and 1.26 s of that was the funcref machinery. The string walks moved too (parse_digits −27%, parse_multibyte −11%): two `call_indirect` per character replaced two funcref constructions' interns. The controls behaved — `lcg` and `rng_manual` build no closures and are flat.
///
/// # The typed tables, 2026-08-20
///
/// Per-arity tables typed `(ref null $clsr/N)` with bodies declared at the arity type, deleting both per-dispatch engine checks: the `call_indirect` signature check compiles away entirely (the table's element type equals the expected type — Wasmtime's `StaticMatch`), and the former named-final-subtype mismatch that took the `is_subtype` libcall on every call has no types left to mismatch. Release-profile compiler this time, native binaries, x86-64 Linux, whole-process wall, min of 5, before = the commit under `5b837023`, outputs identical across arms and every harness anchor reproduced:
///
/// | Program | Input | before | after |
/// | --- | --- | --- | --- |
/// | `monad_io` | 20 000 000 | 317 ms | 238 ms (**−24.9%**) |
/// | `parse_digits` | 1 000 000 | 442 ms | 374 ms (**−15.4%**) |
/// | `state_monad` | 100 000 000 | 185 ms | 180 ms |
/// | `chain` | 1600 | 118 ms | 118 ms |
/// | `spines` | 75 000 | 82 ms | 82 ms |
/// | `lcg` | 100 000 000 | 321 ms | 326 ms |
/// | `trees` | 21 | 352 ms | 346 ms |
/// | `churn` | 75 000 000 | 344 ms | 345 ms |
///
/// The split is the prediction: the two programs whose hot loop dispatches an unknown callee moved, and the five whose loops carry no indirect call — plus `state_monad`, whose binds the specializer absorbs — sat inside ±1.7%. What remains per dispatch is the one `ref.cast (ref $envr/N)` to the non-final environment supertype — deleted since, where the closure arrives from a heap field declared at that type, and standing where it arrives from a parameter.
///
/// # Family keying, 2026-08-20
///
/// A variant family is one final struct at its own width — `CpsValueExpr::Variant`/`CpsIntrinsic::VariantGet`, minted by the Ersd door and padded to the family's width — so a family read is one exact cast where it was a `ref.test` cascade over the arity roster. Same method as above, but **interleaved run-by-run** (before, after, before, after) rather than arm-by-arm, min of 7 each, taken at a one-minute load average under 1.0. Two independent passes, reported together because they agree:
///
/// | Program | before | after | pass 1 | pass 2 |
/// | --- | ---: | ---: | ---: | ---: |
/// | `parse_digits` | 369 ms | 329 ms | **-10.8%** | **-9.8%** |
/// | `chain` | 118 ms | 107 ms | **-9.3%** | **-10.8%** |
/// | `trees` | 329 ms | 305 ms | **-7.3%** | **-7.6%** |
/// | `spines` | 70 ms | 65 ms | **-7.1%** | **-7.5%** |
/// | `churn` | 343 ms | 338 ms | -1.5% | -0.6% |
/// | `monad_io` | 232 ms | 232 ms | +0.0% | +0.0% |
/// | `state_monad` | 181 ms | 182 ms | +0.6% | +0.5% |
/// | `lcg` | 320 ms | 322 ms | +0.6% | +0.3% |
///
/// Statically, over optimized `spines`: the cascade's `ref.test (ref $tuple/N)` falls **58 to 6**, `ref.cast (ref $tuple/N)` **86 to 17**, and the arity roster **5 types to 3**.
///
/// The four movers are exactly the programs whose hot loop walks a heap variant family, and `lcg` — which declares no variant — is flat, so the split is a class rather than a coincidence. **The controls earned their place twice here.** A first pass showed `state_monad` 5.8x *slower* (185 to 1070 ms): the specializer's profitability gates asked whether a body contained a `TupleGet` on its parameter, and once family reads became `VariantGet` they answered no for every variant, so constructor specialization declined silently and `State/bind`'s chain survived as a per-step closure allocation. Teaching both gates the second vocabulary restored it exactly. That is the cost of a second vocabulary, and the reason it is worth paying is the same measurement: a *rebuild* in the wrong vocabulary would have trapped at the next exact cast instead of merely running slower, and the verifier now refuses one.
///
/// One finding for the successor: the per-family type identity does **not** survive Binaryen while every slot is `anyref`. `GlobalTypeOptimization` drops a slot nothing reads, leaving a struct structurally identical to a same-width `$tuple/N`, which closed-world type merging then folds together — the map's fork reads ship as `struct.get $tuple/3`. Exactness is unaffected (the merged type is still final, so the cast is still one compare), but `TypeRefining` still has nothing to refine. Typed slots are what make family structs structurally distinct, which is the next step's subject.
///
/// # Typed slots, 2026-08-20
///
/// Each family slot is now declared at the carrier its recorded shape names rather than uniformly `anyref` — the tag as a packed `i8` read through `struct.get_u`, unsigned and signed immediates as raw `i32`, an `Flt` inline as `f32`, a list at its rope base, a product at its arity's type. Slots are grouped by carrier rather than by field position, so constructors agreeing on a carrier share its slots and only a disagreement costs width; `shapes.rs`'s `slot_layout_probe` is that choice's figure. Same method as family keying above — interleaved run-by-run, min of 7, two passes, load 0.82 at the start and 1.15 at the end — outputs identical across arms and every harness anchor reproduced:
///
/// | Program | before | after | pass 1 | pass 2 |
/// | --- | ---: | ---: | ---: | ---: |
/// | `spines` | 65 ms | 60 ms | **−7.7%** | **−7.7%** |
/// | `trees` | 306 ms | 284 ms | **−7.2%** | **−4.9%** |
/// | `chain` | 110 ms | 107 ms | **−2.7%** | **−2.8%** |
/// | `monad_io` | 232 ms | 234 ms | +0.9% | +0.4% |
/// | `churn` | 340 ms | 343 ms | +0.9% | +0.6% |
/// | `lcg` | 320 ms | 320 ms | +0.0% | −0.3% |
/// | `state_monad` | 182 ms | 182 ms | +0.0% | +0.0% |
/// | `parse_digits` | 332 ms | 339 ms | +1.8% | +2.4% |
///
/// **The split is payload typing, and `parse_digits` is the experiment that shows it.** Its only families are `Option` and `Result`, whose payloads are polymorphic and stay `anyref`, so it is the one program where the tag is typed and nothing else is — and it is the one program that loses. The three that win are exactly the three whose hot loop reads a *typed payload*: `Map/Node`'s `crit`, `Chain/link`'s `Nat`, `trees`' node. So the tag's packing is a small charge and the payload's carrier is what pays for it, which is worth stating because the two arrived in one commit and a single aggregate would have hidden it. The regression was left standing deliberately: it is a fifth the size of what the same mechanism returns elsewhere, and isolating it further would have cost more machine time than the finding is worth.
///
/// Statically the emitted code is uniformly *better* in the losing program too — all 26 of `parse_digits`'s functions shrank or held, `ref.i31` fell 307 to 297 and `ref.cast (ref $tuple/N)` 16 to 10 — so its 2% lives in the engine's object handling rather than in the instruction stream. Over optimized `spines`: `ref.i31` **410 to 363**, `ref.cast (ref $tuple/N)` **17 to 10**, total instructions **8962 to 8758**.
///
/// **The finding family keying left open is closed.** With every slot `anyref` a family struct was structurally identical to a same-width `$tuple/N`, and Binaryen's closed-world type merging folded them together — under family keying alone `spines` shipped exactly one surviving `$row/` type (`/syn/Str/Scan`) and `parse_digits` shipped none, with `/std/Map/Node`'s reads going out as tuple gets. Typed slots make the structs distinct, so the types survive (`spines` 1 to 3, every other program 0–1 to 2–3) and `TypeRefining` has something to work with at last: the emitted module now names `(ref (exact $row/5$/std/Map/Node))` in a signature. The descent loop reads its `crit` straight into `i32.div_u`, with no cast and no unbox between.
///
/// **One negative result, worth as much as the positive ones.** Typing `List` slots at the rope base was meant to bite into the `is_subtype` libcall class, and it did nothing: `ref.cast (ref $rope/N)` is 51 in five programs and 72 in `spines` in *both* arms. Reading where those casts sit says why, and retires the census's framing of them as an opportunity.
///
/// Take the counts before Binaryen, which merges the two rope bases into one and hides the split: **61 casts target `$rope/bin`, 5 target `$rope/list`**, and the remaining 39 target `(sub final …)` subtypes, which short-circuit to inline compares and were never the expensive kind. Every one of `/std/Map/insert1`'s sixteen is a `$rope/bin` — the `Bytes` key path.
///
/// That is the whole answer, and it is a *boundary rather than a backlog*. `Repr::Bin` is sometimes-immediate: since the map wall put a small `Bytes` on the i31 — worth 6.1× on the insert — no local, field, or slot can be declared at `$rope/bin`, because half the population is not a rope at all. The residue that a declared carrier *could* reach is the 5 `$rope/list` sites. So this class is the standing price of the packed immediate, already paid for deliberately and already priced, and not work waiting to be scheduled. What would move it is making the packed carrier always-a-rope, which is the 6.1× being handed back.
///
/// **Where the profile's attribution had expired:** the 2026-08-10 profile named `rng_state` at ~75% interning, but the inline-budget raise recorded in `cps/optimize.rs` has since absorbed that program's `State/bind` chain entirely — its loop is scalar now, closures survive only at its few effect boundaries, and both arms time identically. `state_monad`'s trivial-bind loop specializes the same way. The population the swap re-prices is what the specializers *cannot* reach: the per-step `Io` description and the genuinely unknown per-character step closure — which is exactly the spec's "only re-prices the calls that stay unknown".
///
/// # The two scale questions, answered by the corpus
///
/// The design record owed two measurements: `call_indirect` against many distinct final subtypes in one table, and instantiation of a table at hundreds of entries. The optimized corpus never builds either shape — the largest table any measured program emits is 22 slots (printed below), because dead-code elimination keeps only reachable closure bodies. At that size the answers are subsumed by the product rows above: `monad_io`'s 5.9× is measured *through* a table whose every entry is its own final subtype, and `rng_state`'s startup-dominated 0.04 s is unchanged with the table present, so neither the per-call check nor instantiation is an attributable share at the sizes this compiler emits. A future program with hundreds of live closures re-opens the question; nothing in the corpus can.
///
/// # The constant-closure annex's admission, 2026-08-17
///
/// The final column counts environments materialized once in `$start` — closures whose captures are all interned constants, which the hoister interns now that the code field is an `i32`. Its gate was frequency, and the population is everywhere: at least one per corpus fixture, and 9 of the 19–21 environment constructions in each stdin-driven program (the `/std` description machinery's capture-free thunks are most of them). The rewrite stays.
#[test]
#[ignore = "measurement: records what the closure table costs and saves rather than asserting"]
fn closure_index_dispatch_measurements() {
    const MONAD_IO: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../programs/monad_io.crs"
    ));
    const PARSE_DIGITS: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../programs/parse_digits.crs"
    ));
    const RNG_STATE: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../programs/rng_state.crs"
    ));

    for (label, source) in [
        ("lcg", LCG),
        ("trees", TREES),
        ("higher-order", HIGHER_ORDER),
        ("uncurry", UNCURRY),
        ("string-walk", STRING_WALK),
        ("monad_io", MONAD_IO),
        ("parse_digits", PARSE_DIGITS),
        ("rng_state", RNG_STATE),
    ] {
        let wat = wat(source);
        let slots = wat
            .lines()
            .filter_map(|line| line.trim().strip_prefix("(table $clsr/"))
            .filter_map(|rest| rest.split_whitespace().nth(2))
            .filter_map(|min| min.parse::<usize>().ok())
            .sum::<usize>();
        let dispatches = wat.matches("call_indirect $clsr/").count();
        let environments = wat.matches("struct.new $envr/").count();
        // The constant-closure annex's admission census: environments materialized once at instantiation, each a construction the swap moved out of function code.
        let interned = functions(&wat)
            .iter()
            .find(|function| function.name == "$start")
            .map_or(0, |start| start.body.matches("struct.new $envr/").count());
        println!(
            "{label}: {slots} table slots, {dispatches} dispatch sites, {environments} environment constructions, {interned} interned as consts"
        );
    }
}

/// What the return protocol removes from the corpus, and what that is worth.
///
/// Run it with:
///
/// ```sh
/// cargo test --package curios --lib -- --ignored --nocapture split_return_measurements
/// ```
///
/// It asserts nothing. The structural claim is [`a_returned_constructor_is_delivered_as_its_fields`]'s to make and it fails when it stops holding; this only reports how much of the corpus the protocol reaches, which is a question with no right answer to assert against.
///
/// # What it last printed
///
/// Taken **2026-08-12**, **debug**, on the commit that introduced the pass.
///
/// | Fixture | Multi-result types | Allocation sites |
/// | --- | --- | --- |
/// | lcg | 0 | 79 |
/// | trees | 0 | 81 |
/// | higher-order | 0 | 81 |
/// | direct/escaping | 0 | 81 |
/// | function-only | 0 | 79 |
/// | mutual-recursion | 0 | 79 |
/// | split-return | 1 | 79 |
///
/// **The zeroes are not a null result, they are the wrong corpus for the question.** These fixtures take their runtime taint from `proc/args!` and never read stdin, so none of them reaches the UTF-8 decode path where the protocol actually fires. What they do establish is that the pass is inert everywhere it has no candidate — which is most places.
///
/// Across `programs/`, which does read stdin, exactly one function is selected and it is the same one every time: `/syn/Str/classify`. Five return edges, all visible constructions, tagged at index zero, demanded five slots wide — and one of its two call sites sits inside `/std/Str/fold`'s per-character walk. Its emitted body goes from three allocations to none, and `programs/parse_digits.crs` as a whole from 145 to 142.
///
/// **The runtime figure is the one worth reading, and it is small.** Timing `programs/parse_digits.crs` with the pass toggled and nothing else changed, `user` time over repeated runs:
///
/// | Input | Pass off | Pass on |
/// | --- | --- | --- |
/// | 300000 | 0.27, 0.26 | 0.26, 0.25, 0.25 |
/// | 1000000 | 0.95, 0.95 | 0.94, 0.93 |
///
/// Roughly **one to two percent** — consistent in direction across all five pairs and too small to be worth more precision than that. At about 135 ns per character the loop is not allocation-bound on the tuple this removes: it is dominated by the per-character closure call and the transient `Option` that `/std/Nat/of_str`'s lifted fold step allocates, neither of which a return protocol reaches, and both of which belong to the higher-order specialization that succeeds this work.
///
/// **Two things this does not separate.** The allocation counts are taken pre-Binaryen, so some of what the pass now removes earlier, Binaryen may have been removing later — the runtime figure is the only one that accounts for that, and it is the small one. And both binaries come from a debug-profile compiler; whether the gap widens under release is unmeasured.
#[test]
#[ignore = "measurement: reports what the return protocol reaches rather than asserting"]
fn split_return_measurements() {
    for (label, source) in [
        ("lcg", LCG),
        ("trees", TREES),
        ("higher-order", HIGHER_ORDER),
        ("direct/escaping", DIRECT_ESCAPING),
        ("function-only", FUNCTION_ONLY),
        ("mutual-recursion", MUTUAL_RECURSION),
        ("split-return", SPLIT_RETURN),
    ] {
        let wat = wat(source);
        // A multi-result type is spelled `func/{parameters}/{results}`; the single-result shape keeps the bare `func/{parameters}` and is what every function had before this. Counted off the type *name* in a declaration rather than off slashes in the line, because a function definition names its type too and carries a source hint that is itself full of slashes.
        let split = wat
            .lines()
            .map(str::trim)
            .filter(|line| line.starts_with("(type $func/"))
            .filter(|line| {
                line.split_whitespace()
                    .nth(1)
                    .is_some_and(|name| name.matches('/').count() == 2)
            })
            .count();
        let allocations = wat.matches("struct.new").count() + wat.matches("array.new").count();
        println!("{label}: {split} multi-result types, {allocations} allocation sites");
    }
}

/// What copying more costs, in the two units that can see it.
///
/// Run it with:
///
/// ```sh
/// cargo test --package curios --lib -- --ignored --nocapture copy_growth_measurements
/// ```
///
/// It asserts nothing. Lifting the nested-definition refusal lets the inliner and both specializers copy bodies they used to decline, and copying is the one thing that trades size for speed in both directions at once — so the baseline is taken before the change rather than reconstructed after it.
///
/// # Which instrument sees what
///
/// **Peak memory cannot see a transient allocation, and it is not a shortcoming of the measurement.** The return protocol removes roughly one five-field object per character; running `programs/parse_digits.crs` at 1000000 with that pass toggled and nothing else changed gives a maximum resident set of 5 734 400 bytes without it and 5 767 168 bytes with it — flat, and if anything slightly up from the code that replaced it. Transient garbage never accumulates, so its cost is allocation *work* rather than footprint, and that lands on the clock. Retention is the opposite: `trees` holds what it builds, and its resident set moves from 5.77 MB at depth 18 to 271.68 MB at depth 21 on nothing but what it keeps.
///
/// So: **time for a change to transient allocation, resident set for a change to retention, emitted size for a change that copies.** Reaching for the wrong one reports a confident null.
///
/// # The baseline, taken at `82cb8ef7`
///
/// Native binaries built with `cargo run --package curios -- compile <program> -o <path>`, timed with `/usr/bin/time -l`. The binary embeds the runtime launcher, so its absolute size is mostly launcher and only the *difference* between two builds is compiled code.
///
/// | Program | Input | `user` | Max RSS | Binary |
/// | --- | --- | --- | --- | --- |
/// | `parse_digits` | 1000000 | 0.92 s | 5 767 168 B | 3 786 408 B |
/// | `trees` | 21 | 0.23 s | 271 679 488 B | 3 786 504 B |
///
/// What this test itself prints is the third unit — the raw pre-Binaryen module size for each structural fixture, which is where code growth shows up first and without a runtime at all. At the same revision: `lcg` 6708, `trees` 7706, `higher-order` 7160, `direct/escaping` 7174, `function-only` 6632, `mutual-recursion` 6834, `split-return` 8367 bytes.
#[test]
#[ignore = "measurement: reports emitted size rather than asserting"]
fn copy_growth_measurements() {
    for (label, source) in [
        ("lcg", LCG),
        ("trees", TREES),
        ("higher-order", HIGHER_ORDER),
        ("direct/escaping", DIRECT_ESCAPING),
        ("function-only", FUNCTION_ONLY),
        ("mutual-recursion", MUTUAL_RECURSION),
        ("split-return", SPLIT_RETURN),
    ] {
        let module = compile_raw(source);
        let bytes = to_bytes(&module).len();
        println!("{label}: {bytes} bytes");
    }
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
        ("split-return", SPLIT_RETURN),
        ("looped-pick", LOOPED_PICK),
        ("variant-filler", VARIANT_FILLER),
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

/// A packed literal's non-constant atoms fuse into one flat chunk build (`fuse_append_chains` in `curios-cont`): the byte literal's two runtime atoms become a `BinChunk(X, 2)` and the bit literal's one a `BinChunk(B, 1)`, in place of the append-per-atom chains the lowering honestly writes, and the program still prints what the chains would. The taint keeps every atom out of constant folding, so the chunks survive to emission and the equality runs over runtime-built values.
#[test]
fn tainted_packed_literals_fuse_to_flat_chunks() {
    let source = r#"
        use /std/{Nat, List, Byte, Bytes, Bits, Bool, Str, Handle, proc};
        let taint = List/len(proc/args!);
        let a: Byte = match taint | 0 => 7 | _ => 9 end;
        let y: Byte = match taint | 0 => 8 | _ => 10 end;
        let bytes = x[a, y, 0x21];
        let t: Bool = taint == 0;
        let bits = b[t, 1];
        match bytes == x[7, 8, 0x21] && bits == b[1, 1]
        | true => /std/print("ok\n")
        | false => /std/print("bad\n")
        end
        "#;

    let dump = cont_optm(source);
    assert!(
        dump.contains("BinChunk(X, 2)"),
        "the byte atoms fuse into one chunk: {dump}"
    );
    assert!(
        dump.contains("BinChunk(B, 1)"),
        "the bit atom fuses into one chunk: {dump}"
    );

    assert_eq!(run(source), b"ok\n");
}

/// A list stored into a field the Ersd census marks indexed-only settles at the store, and the settle over the literal's unshared construction tree fuses into one exact flat build (`flatten_indexed_lists` in `curios-cont`): the spliced rebuild becomes a `ListFlat` in place of the concat-of-rope-nodes the lowering honestly writes, and the read answers the same element. The reads reach the census through `/std/List/try_get`, so the fixture also pins the census's deferral through saturated known calls — the shape every real read has at the arena.
#[test]
fn indexed_field_store_fuses_to_flat_build() {
    let source = r#"
        use /std/{Nat, List, Str, Handle, Option, proc};

        induct Box: Type
        | pack(items: List(Nat))
        end

        let taint = List/len(proc/args!);
        let pre = [taint + 1, taint + 2];
        let post = [taint + 3, taint + 4];
        let boxed = Box/pack([..pre, taint + 9, ..post]);
        match boxed
        | pack(items) =>
            match List/try_get(items, 2)
            | some(v) => /std/print(Str/concat(Nat/to_str(v), "\n"))
            | none() => /std/print("none\n")
            end
        end
        "#;

    // The named verdict first — the assertion surface — then the fused op it becomes, then the behavior.
    assert!(
        census_settles(source, "/Box", "pack", "items"),
        "the census marks the stored field indexed-only"
    );
    let dump = cont_optm(source);
    assert!(
        dump.contains("ListFlat(3)"),
        "the spliced store flattens to one exact build: {dump}"
    );

    assert_eq!(run(source), b"9\n");
}

/// The builder idiom is protected: a field whose values are re-grown — the accumulator spelling `[..items, x]` — is poisoned by the census, so no settle is inserted and no flat build fires, and the loop keeps its O(1) append steps.
#[test]
fn regrown_field_store_stays_lazy() {
    let source = r#"
        use /std/{Nat, List, Str, Handle, proc};

        induct Acc: Type
        | keep(items: List(Nat))
        end

        let taint = List/len(proc/args!);
        rec grow(n: Nat, acc: Acc) -> Acc =
            match n: (_) => Acc
            | 0 => acc
            | m + 1 =>
                match acc
                | keep(items) => grow(m, Acc/keep([..items, m + taint]))
                end
            end;
        let result = grow(3, Acc/keep([]));
        match result
        | keep(items) => /std/print(Str/concat(Nat/to_str(List/len(items)), "\n"))
        end
        "#;

    assert!(
        !census_settles(source, "/Acc", "keep", "items"),
        "a re-grown field is never marked"
    );
    let dump = cont_optm(source);
    assert!(
        !dump.contains("ListSettle") && !dump.contains("ListFlat"),
        "a re-grown field is never settled: {dump}"
    );

    assert_eq!(run(source), b"3\n");
}

/// The demand rule stands alone: a local concatenation whose every use is indexing-shaped — here through `try_get`'s parameter, so the fact is interprocedural — builds flat with no field store involved.
#[test]
fn indexed_local_concat_fuses_to_flat_build() {
    let source = r#"
        use /std/{Nat, List, Str, Handle, Option, proc};
        let taint = List/len(proc/args!);
        let a = [taint + 1, taint + 2];
        let b = [taint + 3, taint + 4];
        let joined = [..a, ..b];
        match List/try_get(joined, 3)
        | some(v) => /std/print(Str/concat(Nat/to_str(v), "\n"))
        | none() => /std/print("none\n")
        end
        "#;

    let dump = cont_optm(source);
    assert!(
        dump.contains("ListFlat(2)"),
        "the indexed concat flattens: {dump}"
    );

    assert_eq!(run(source), b"4\n");
}

/// The `Immediate` family encoding's admission is pinned against the packed immediates: a family whose unary constructor holds a `Nat` — *always* an i31 — keeps the bare encoding and its `IsImmediate` dispatch, while the same family over `Bytes` stays `Tagged`, because a packed value is only *sometimes* immediate and a bare small `Bytes` beside boxed siblings would collide with the discrimination exactly when it boxes. `FieldShape::Opaque` for packed carriers is the invariant; this is its consequence, stated where a future widening would trip it.
#[test]
fn packed_unary_payload_declines_the_immediate_encoding() {
    let nat_family = r#"
        use /std/{Nat, List, Str, Handle, proc};

        induct Gauge: Type
        | tiny(Nat)
        | pair(Nat, Nat)
        end

        let taint = List/len(proc/args!);
        let g = match taint | 0 => Gauge/tiny(7) | _ => Gauge/pair(taint, taint) end;
        match g
        | tiny(n) => /std/print(Str/concat(Nat/to_str(n), "\n"))
        | pair(a, b) => /std/print(Str/concat(Nat/to_str(a + b), "\n"))
        end
        "#;
    let bytes_family = r#"
        use /std/{Nat, List, Bytes, Str, Handle, proc};

        induct Gauge: Type
        | tiny(Bytes)
        | pair(Nat, Nat)
        end

        let taint = List/len(proc/args!);
        let g = match taint | 0 => Gauge/tiny(x[7]) | _ => Gauge/pair(taint, taint) end;
        match g
        | tiny(inner) => /std/print(Str/concat(Nat/to_str(Bytes/len(inner)), "\n"))
        | pair(a, b) => /std/print(Str/concat(Nat/to_str(a + b), "\n"))
        end
        "#;

    assert!(
        cont_optm(nat_family).contains("IsImmediate"),
        "an always-immediate unary payload keeps the bare encoding"
    );
    let dump = cont_optm(bytes_family);
    assert!(
        !dump.contains("IsImmediate"),
        "a sometimes-immediate packed payload declines it: {dump}"
    );

    // The bit grain declines identically: a `Bits` payload is sometimes-immediate too.
    let bits_family = r#"
        use /std/{Nat, List, Bits, Str, Handle, proc};

        induct Gauge: Type
        | tiny(Bits)
        | pair(Nat, Nat)
        end

        let taint = List/len(proc/args!);
        let g = match taint | 0 => Gauge/tiny(b[1]) | _ => Gauge/pair(taint, taint) end;
        match g
        | tiny(inner) => /std/print(Str/concat(Nat/to_str(Bits/len(inner)), "\n"))
        | pair(a, b) => /std/print(Str/concat(Nat/to_str(a + b), "\n"))
        end
        "#;
    assert!(
        !cont_optm(bits_family).contains("IsImmediate"),
        "a sometimes-immediate bit payload declines it too"
    );

    assert_eq!(run(nat_family), b"7\n");
    assert_eq!(run(bytes_family), b"1\n");
    assert_eq!(run(bits_family), b"1\n");
}

/// The bit grain rides the i31 exactly as the byte grain does: small literals and appends stay immediate — the equality against a compile-time literal is the canonicity check — an append past the 26-bit envelope boxes into the rope world, and two separately grown ropes still compare by content. The taint keeps every value out of constant folding, and it also rides the recursion depth: a *literal* depth sends elaboration into a runaway reduction of the open append spine — a pre-existing pathology recorded in the map-wall follow-ups, independent of the immediate representation.
#[test]
fn small_bits_ride_the_immediate_and_overflow_boxes() {
    let source = r#"
        use /std/{Nat, List, Bits, Bool, Str, Handle, proc};
        let taint = List/len(proc/args!);
        let t: Bool = taint == 0;
        let small = b[t, 1, 0, t];
        let grown = b[..small, 1];
        rec widen(n: Nat, acc: Bits) -> Bits =
            match n | 0 => acc | _ => widen(n - 1, b[..acc, t]) end;
        let wide = widen(30 + taint, grown);
        let wide2 = widen(30 + taint, grown);
        match (grown == b[1, 1, 0, 1, 1]) && (wide == wide2) && (Bits/len(wide) == 35 + taint)
        | true => /std/print("ok\n")
        | false => /std/print("bad\n")
        end
        "#;

    assert_eq!(run(source), b"ok\n");
}

/// A two-way dispatch is a conditional branch, not a jump table.
///
/// Cases `{0, 1}` with nothing else reachable — every `Bool` match, and every exhaustive two-constructor tag — decide in one compare. A `br_table` is a bounds check, a `csel`, a dependent load and an indirect branch on the ISA lowerings in this pipeline's chain, and nothing below the emitter narrows it: Cranelift's aarch64 rule builds a `JTSequence` whatever the table's size, and Binaryen leaves a two-entry table alone (measured 2026-08-20 on `spines`: 60 tables emitted before this shape landed, 57 of them surviving `-O2`; 9 emitted after).
///
/// Asserted on the fixture's own kernel rather than the module, since the prelude rides along and its wider families table legitimately. Both dispatches stay runtime-tainted so neither folds — a closed condition is decided at compile time and emits no dispatch at all.
#[test]
fn a_two_way_dispatch_is_a_branch_not_a_table() {
    let source = r#"
        use /std/{Nat, Bool, List, Str, Handle, proc};

        induct Pair : Type
        | left(Nat)
        | right(Nat)
        end

        let taint = List/len(proc/args!);
        let flag : Bool = taint == 0;
        rec decide(n : Nat, acc : Nat) -> Nat =
            match n : (_) => Nat
            | 0 => acc
            | m + 1; ih =>
                let chosen : Pair = match flag | true => Pair/left(m) | false => Pair/right(m) end;
                let folded = match chosen | left(x) => x + 65521 | right(x) => x + 65519 end;
                decide(m, acc + folded)
            end;
        /std/print(Nat/to_str(decide(taint + 3, 0) + decide(taint + 4, 1)))
        "#;

    let wat = wat(source);
    let functions = functions(&wat);
    let kernel = function_with(&functions, "65521");

    assert!(
        !kernel.body.contains("br_table"),
        "a two-way dispatch must branch rather than table: {}",
        kernel.body
    );
}

/// An aggregate is read at its own final type — a variant family through its own type, a structural tuple through the roster.
///
/// The `$tuple/N` family used to be a prefix subtype chain — `$tuple/4 <: $tuple/3 <: … <: $tuple/1` — so one `ref.cast (ref $tuple/1)` could read field 0 of any tuple whatever its arity. That is what made the cast a *host call*: wasmtime's `is_subtype` short-circuits only when the target is final, so every cast to a prefix of a wider object took the `is_subtype` libcall, and every real node is wider than the prefix it was read through.
///
/// Final types delete the short-circuit's precondition, and the reader finds the object's exact type by exhausting the roster instead. Correctness does not rest on an object's arity being its constructor's, which `cps/fields.rs` makes false whenever a narrow constructor materialises at its region's width.
///
/// Family keying then removed the cascade from the case it was built for. A variant family is one final struct at its own width, so a family read is a single exact cast and the roster search survives only for structural tuples — this fixture pins the family half, and the `$tuple/` finality check below still pins the other.
///
/// **Measured 2026-08-20, x86-64 Linux, release, whole-process, min of 5, anchors checked on every run.** `chain` 339.6 → 131.1 ms (**−61.4%**), `spines` 100.5 → 78.8 ms (**−21.6%**), against `lcg` +0.8%, `trees` +0.8% and `churn` +0.1% — all three inside noise, and each for a stated reason: `lcg` declares no variant at all, `trees`' leaf rides the i31 so its family never reads a tag and its one boxed constructor casts exactly, and `churn`'s hot loop is not a variant walk. The two that moved are exactly the two whose hot loop reads a multi-constructor heap family, which is what makes the figure a class rather than a coincidence.
#[test]
fn a_tuple_is_read_at_its_own_final_type() {
    let source = r#"
        use /std/{Nat, List, Str, Handle, proc};

        induct Chain : Type
        | stop()
        | link(Nat, Chain)
        end

        let taint = List/len(proc/args!);
        rec build(n : Nat, acc : Chain) -> Chain =
            match n : (_) => Chain
            | 0 => acc
            | m + 1; ih => build(m, Chain/link(m, acc))
            end;
        rec total(c : Chain, acc : Nat) -> Nat =
            match c
            | stop() => acc
            | link(v, rest) => total(rest, (acc + v) % 999983)
            end;
        /std/print(Nat/to_str(total(build(taint + 3, Chain/stop()), 0) + total(build(taint + 4, Chain/stop()), 1)))
        "#;

    let wat = wat(source);

    // No tuple type is a subtype of anything: the printer renders a final, supertype-less struct
    // without a `sub` wrapper, so any `sub` on one of these lines is the chain coming back.
    for line in wat.lines().filter(|line| line.contains("(type $tuple/")) {
        assert!(
            !line.contains("sub"),
            "tuple types must be final and unrelated: {line}"
        );
    }

    // Since family keying, `Chain` is one final `$row/N$/Chain` at the family's width rather than a
    // `$tuple/3` beside a `$tuple/1`, so the walk needs no cascade at all: the object's type is a
    // fact of the family, and the read is one exact cast followed by the field.
    let functions = functions(&wat);
    let kernel = function_with(&functions, "999983");
    assert!(
        kernel.body.contains("ref.cast (ref $row/") && kernel.body.contains("struct.get $row/"),
        "the walk reads the family at its own exact type: {}",
        kernel.body
    );
    assert!(
        !kernel.body.contains("ref.test"),
        "and needs no roster cascade to find it: {}",
        kernel.body
    );

    // The family types are final and unrelated too — the same property, for the types that
    // replaced the tuples on this path.
    for line in wat.lines().filter(|line| line.contains("(type $row/")) {
        assert!(
            !line.contains("sub"),
            "family types must be final and unrelated: {line}"
        );
    }
}

/// A family slot is declared at the carrier its recorded shape names, not at `anyref`.
///
/// `Chain` is `stop() | link(Nat, Chain)`, so its slots are the tag, one unsigned immediate, and one uniform reference — and the emitted struct says exactly that: `i8`, `i32`, `anyref`. Two costs die with the declaration. The tag reads back through `struct.get_u` out of a packed byte, where a uniform slot cast an `i31` and unboxed it; and the `Nat` payload arrives in a register, where the same slot boxed at every store and unboxed at every read. That pair is the largest static population in every corpus program and prices at 17% of a dispatch-heavy fold's per-element budget (`shapes.rs`'s `boxed_field_read_measurements`).
///
/// The slots are grouped by carrier rather than by field position, which is what keeps this from widening the family: a carrier's range is as wide as the constructor holding the most fields of it, so constructors agreeing on a carrier share slots. `shapes.rs`'s `slot_layout_probe` is that decision's figure — over the standard library the rule types 22 slots against positional assignment's 11, and only three families widen at all, none of them on a hot allocation path.
#[test]
fn a_monomorphic_slot_carries_its_own_type() {
    let source = r#"
        use /std/{Nat, List, Str, Handle, proc};

        induct Chain : Type
        | stop()
        | link(Nat, Chain)
        end

        let taint = List/len(proc/args!);
        rec build(n : Nat, acc : Chain) -> Chain =
            match n : (_) => Chain
            | 0 => acc
            | m + 1; ih => build(m, Chain/link(m, acc))
            end;
        rec total(c : Chain, acc : Nat) -> Nat =
            match c
            | stop() => acc
            | link(v, rest) => total(rest, (acc + v) % 999983)
            end;
        /std/print(Nat/to_str(total(build(taint + 3, Chain/stop()), 0) + total(build(taint + 4, Chain/stop()), 1)))
        "#;

    let wat = wat(source);

    let declaration = wat
        .lines()
        .find(|line| line.contains("(type $row/") && line.contains("Chain"))
        .expect("the Chain family declares a type");
    assert!(
        declaration.contains("i8") && declaration.contains("i32"),
        "the tag packs into a byte and the `Nat` payload is a raw scalar: {declaration}"
    );

    let functions = functions(&wat);
    let kernel = function_with(&functions, "999983");
    assert!(
        kernel.body.contains("struct.get_u $row/"),
        "the tag is read out of its packed byte, with nothing to unbox: {}",
        kernel.body
    );
    // Every read of a slot lands in a local of the slot's own carrier, so none of them is followed
    // by an unbox. The casts that remain in this walk are the function's own `anyref` parameter and
    // a hoisted literal — neither is a field, and both are somebody else's campaign.
    let lines: Vec<&str> = kernel.body.lines().collect();
    for (index, line) in lines.iter().enumerate() {
        if !line.contains("struct.get") || !line.contains("$row/") {
            continue;
        }
        assert!(
            !lines[index + 1].contains("ref.cast"),
            "a slot read is followed by an unbox: {line} then {}",
            lines[index + 1]
        );
    }
}
