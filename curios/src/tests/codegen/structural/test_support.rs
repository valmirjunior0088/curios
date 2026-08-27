//! Compiling a fixture to raw pre-Binaryen wasm and asserting a structural property of the module.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

//! Structural acceptance fixtures. Each test compiles a small `.crs` fixture to the raw, pre-Binaryen wasm module and asserts a structural property of the emitted code — a clean natural loop for a hot kernel, direct recursion, the closure ABI only where a call is genuinely unknown — and that the raw module validates and executes without Binaryen repairing control flow.
//!
//! Emitted function names are `$func/<N>` ids — a module-wide monotonic index over every reachable function, prelude included — optionally suffixed with the source hint as `$func/<N>$hint`. The index carries identity; the hint is only origin annotation. Hot kernels are still located by a distinctive literal constant baked into their arithmetic (`65537` for LCG, `1000003` for trees) or by name-independent structure (self-recursion, the shared `$func/<N>`/`$clsr/<N>` index of a function used both directly and as a closure), never by a source name. A genuine irreducible-cycle dispatcher is the `loop $$dispatch/<anchor>` the emitter names in `into_wasm::expr_emitter`; an ordinary constructor-tag `switch` is not a dispatcher whatever shape it takes — a `br_table` over `$case$N`/`$tail` labels for three or more cases, a plain `if` for the two-way and one-way shapes.

use {
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

pub(super) const LCG: &str = r#"
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

pub(super) const TREES: &str = r#"
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

pub(super) const HIGHER_ORDER: &str = r#"
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

pub(super) const DIRECT_ESCAPING: &str = r#"
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

pub(super) const FUNCTION_ONLY: &str = r#"
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
pub(super) const MUTUAL_RECURSION: &str = r#"
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
pub(super) const SPLIT_RETURN: &str = r#"
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
pub(super) const UNCURRY: &str = r#"
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
pub(super) const LOOPED_PICK: &str = r#"
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
pub(super) const VARIANT_FILLER: &str = r#"
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
pub(super) const STRING_WALK: &str = r#"
    use /std/{Handle, Nat, Str, Char, List, proc};
    let taint = List/len(proc/args!);
    let n : Nat = taint;
    let text : Str = Nat/to_str(n);
    /std/print(Nat/to_str(Str/fold(text, 0, (codepoint, acc) => acc + Char/to_nat(codepoint))))
    "#;

// -- helpers ----------------------------------------------------------------

/// Compile `source` (no external modules) to the raw, pre-Binaryen wasm module. The returned `.0` of `compile_entrypoint` is the module `into_wasm` produces; Binaryen only runs later, in `crate::to_cwasm`.
pub(in crate::tests::codegen) fn compile_raw(source: &str) -> Module {
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
pub(in crate::tests::codegen) fn wat(source: &str) -> String {
    compile_raw(source).to_string()
}

/// The lines allocating `needle` that are not the `Io` carrier's own.
///
/// Every program's tail is a description, and a description erases to a zero-argument closure — so an effect boundary allocates a closure and forces it through an indirect call no matter how the *user's* code is written. Those carry the `$io/…` hint their thunk was minted with (`io/pure`, `io/bind`, `io/write`, …), which is what lets a claim about user code stay a claim about user code. A test that dropped the distinction would either fail on every program or assert nothing.
///
/// **The separation is by what the line names, and that is why this is for allocations and nothing else.** An allocation names its own definition, so `struct.new $clsr/451$io/bind` carries the hint even though the description is built *inside* user code — which is exactly where it is built, so filtering by enclosing function would discard the distinction this exists to make. An indirect call names the arity's table and type instead: `call_indirect $clsr/0 $clsr/0` identifies no callee, and a needle like that would silently keep every `Io` force while looking like it had excluded them. Those claims belong to [`user_functions_with`], where the enclosing function is the only thing left to separate on. The assertion below is what keeps the choice from being made by accident.
pub(in crate::tests::codegen) fn user_allocations<'a>(wat: &'a str, needle: &str) -> Vec<&'a str> {
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
pub(super) fn user_functions_with<'a>(functions: &'a [Function<'a>], needle: &str) -> Vec<&'a str> {
    functions
        .iter()
        .filter(|function| function.body.contains(needle))
        .filter(|function| !function.name.contains("$io/") && function.name != "$func/main")
        .map(|function| function.name)
        .collect()
}

/// One emitted function: its `$name` and full text.
pub(in crate::tests::codegen) struct Function<'a> {
    pub(in crate::tests::codegen) name: &'a str,
    pub(in crate::tests::codegen) body: &'a str,
}

impl Function<'_> {
    /// How many times this function calls itself directly. `call $name` is a substring of `return_call $name`, so this counts tail and non-tail self calls alike.
    pub(in crate::tests::codegen) fn self_calls(&self) -> usize {
        self.body.matches(&format!("call {}", self.name)).count()
    }
}

/// Split the module WAT into its emitted functions. Each starts at a `    (func ` line (module indent); its text runs to the next one. Module items after the last function (exports, `$start`) never open a `    (func ` line, so they ride along on the final entry without introducing calls or refs of their own.
pub(in crate::tests::codegen) fn functions(wat: &str) -> Vec<Function<'_>> {
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
pub(super) fn function_with<'a>(functions: &'a [Function<'a>], needle: &str) -> &'a Function<'a> {
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
pub(super) fn loop_containing<'a>(wat: &'a str, needle: &str) -> &'a str {
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
pub(super) fn indices(wat: &str, prefix: &str) -> BTreeSet<u32> {
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
pub(super) fn run_raw(source: &str, args: &[&str]) -> Vec<u8> {
    let module = compile_raw(source);
    let cwasm = precompile(&to_bytes(&module))
        .expect("raw module validates and Cranelift-compiles without Binaryen");

    let (system, io) = MockHost::builder().args(args).build();
    run_bytes(&cwasm, system, ForeignBindings::empty()).expect("raw module executes");
    io.output()
}

/// Run the module through the ordinary Binaryen + Cranelift path, for the same input — the reference the raw path must agree with.
pub(super) fn run_binaryen(source: &str, args: &[&str]) -> Vec<u8> {
    let module = compile_raw(source);
    let cwasm = crate::to_cwasm(&module).expect("binaryen path precompiles");

    let (system, io) = MockHost::builder().args(args).build();
    run_bytes(&cwasm, system, ForeignBindings::empty()).expect("optimized module executes");
    io.output()
}
