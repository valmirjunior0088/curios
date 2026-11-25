//! The combinator webs the unfolding suites compile, the spellings of one grammar and the doors a web's value is consumed through, and what a compile of one is counted by, which the bench's measurements of unfolding read too.

use {
    curios_elab::DEFAULT_BUDGET, curios_module::RootSource, curios_pipeline::compile_with_prelude,
    curios_surface::Entrypoint, std::fmt::Write,
};

/// Where a rule's inner combinator applications sit. The grammar is identical in all four; only where the applications are written moves.
#[derive(Clone, Copy)]
pub enum Inner {
    /// No combinator is applied inside a continuation at all.
    None,
    /// `Parse/many0(prev)` and `Parse/sep_by0(prev, eq)` written where they are used — inside a `!` continuation, which is a block.
    InBlock,
    /// The same two applications, named as top-level items and referenced from the continuation.
    Hoisted,
}

/// A grammar of `rules` rules in the `/std/Json/decode` idiom: each rule is a top-level `Parse` description built from explicit binds over the two rules before it. The value graph is intentional: this fixture tests sharing of descriptions.
pub fn grammar(rules: usize, inner: Inner) -> String {
    let mut source = String::from(
        "use /std/{Str, Nat, Byte, Bytes, Bool, List, Option, Result, Parse, Io};\n\n\
         let ws: Parse(Bytes, Bytes) = Parse/bytes/take_while((b) => b == 0x20);\n\
         let dig: Parse(Bytes, Bytes) = Parse/bytes/take_while((b) => 0x30 <= Byte/to_nat(b));\n\
         let eq: Parse(Bytes, Bytes) = Parse/map(Parse/bytes/literal(\"=\"), (_) => Str/to_bytes(\"=\"));\n\n",
    );

    for rule in 0..rules {
        let previous = if rule >= 1 {
            format!("r{}", rule - 1)
        } else {
            "dig".to_string()
        };
        let older = if rule >= 2 {
            format!("r{}", rule - 2)
        } else {
            "ws".to_string()
        };

        let (many, separated) = match inner {
            Inner::None => (None, None),
            Inner::InBlock => (
                Some(format!("Parse/many0({previous})")),
                Some(format!("Parse/sep_by0({previous}, eq)")),
            ),
            Inner::Hoisted => {
                let _ = writeln!(
                    source,
                    "let m{rule}: Parse(Bytes, List(Bytes)) = Parse/many0({previous});"
                );
                let _ = writeln!(
                    source,
                    "let s{rule}: Parse(Bytes, List(Bytes)) = Parse/sep_by0({previous}, eq);"
                );
                (Some(format!("m{rule}")), Some(format!("s{rule}")))
            }
        };

        let _ = writeln!(source, "let r{rule}: Parse(Bytes, Bytes) =");
        let mut bindings = vec![("x0", previous.clone()), ("x1", older), ("x2", previous)];
        if let (Some(many), Some(separated)) = (many, separated) {
            bindings.extend([("m", many), ("s", separated)]);
        }
        bindings.push(("e", "eq".to_string()));
        for (name, action) in &bindings {
            let _ = writeln!(source, "    Parse/bind({action}, ({name}) =>");
        }
        let _ = writeln!(source, "    Parse/pure(x0){};", ")".repeat(bindings.len()));
    }

    let _ = writeln!(source, "\nlet top: Parse(Bytes, Bytes) = r{};\n", rules - 1);
    source.push_str(ENTRY);
    source
}

/// The entry every grammar shares: a runtime-tainted input, so nothing folds the parse away.
pub const ENTRY: &str = r#"let entry() -> /std/Io({}) =
let standard_input = /std/Io/Reader/new(/std/Proc/stdin)!;
let input = /std/Result/unwrap_or(/std/Try/run(/std/Io/Reader/line(standard_input))!, /std/Option/none());
match input
| some(bytes) =>
    match Parse/run(top, bytes)
    | success(out) =>
        match Str/of_bytes(out)
        | success(s) => /std/print(s)
        | failure(_) => /std/print("invalid utf-8\n")
        end
    | failure(e) => /std/print(e.message)
    end
| none() => /std/print("no input\n")
end;
entry()
"#;

/// The tail every measured program here shares.
///
/// No `!`, unlike [`ENTRY`]: nothing measured here needs the runtime taint, since both checkers walk every declaration whether the entry reaches it or not.
pub const TAIL: &str = "/std/print(\"ok\")\n";

/// What a `!` in a `Parse` region comes to once its witness is resolved: `/std/Parse`'s `Monad` witness binds by `chain`, so a copy of the sequencing a grammar writes is a copy of this function, whatever `Parse/bind` a program may also call by name.
pub const SEQUENCED: &str = "/std/Parse/chain";

/// How many emitted functions carry `needle` in their debug name — one copy of a source function per hit.
pub fn copies(module: &curios_anf::Module, needle: &str) -> usize {
    module
        .functions()
        .iter()
        .flatten()
        .filter(|function| {
            function
                .debug_name
                .as_deref()
                .is_some_and(|name| name.contains(needle))
        })
        .count()
}

/// Every emitted function, live slots only.
pub fn emitted(module: &curios_anf::Module) -> usize {
    module.functions().iter().flatten().count()
}

/// How a web's value is consumed — the axis that decides whether the certifier ever demands it.
#[derive(Clone, Copy)]
pub enum Consumed {
    /// Applied to the declaration's own binder and returned. Nothing demands the value, so nothing reduces the web.
    Applied,
    /// Scrutinized by a `match` at that binder, so the case equation's subject mentions a local.
    Scrutinized,
    /// Scrutinized by a `match` at a literal. The equation's subject is local-free, which is the control separating *a scrutinee* from *a scrutinee mentioning a binder* — the local-free path has the evaluation memos and the closed machine, and the local-bearing one has neither.
    ScrutinizedClosed,
    /// Named in the index of an `Eq` the declaration takes a proof of, and eliminated. No case equation is registered for the web at all — the scrutinee is the proof, a bare variable — and the web is reduced anyway, by `solve_indices` unifying `(top(n), true)` against `refl`'s `(z, z)`: the second position, read through the first, puts `top(n)` against `true`, and the walk forces it.
    ///
    /// The second door onto the same reduction, and the one no refinement key reaches, in both checkers; what keeps it flat is weak-head reduction's, the closed machine's and the decision diagram's, not the inverter's, as the scrutinee-refinement measurement states. [`numerics`] is the same door over `Nat`, whose fold laws read both operands.
    Proved,
    /// Scrutinized by a `match` at that binder whose arm compares forms of the web at two other binders: stuck forms naming binders the case equation's subject does not, which the filter in front of the equation puts to none (`curios_analysis::could_answer`).
    Compared,
}

/// The declaration that consumes a web named `top{suffix}`, which is the only thing that differs between the arms of both measurements below.
pub fn consumer(suffix: &str, consumed: Consumed) -> String {
    match consumed {
        Consumed::Applied => format!("let probe{suffix}(n: Nat) -> Bool = top{suffix}(n);\n\n"),
        Consumed::Scrutinized => format!(
            "let probe{suffix}(n: Nat) -> Str =\n    match top{suffix}(n) | true => \"y\" | false => \"n\" end;\n\n"
        ),
        Consumed::ScrutinizedClosed => format!(
            "let probe{suffix}(n: Nat) -> Str =\n    match top{suffix}(7) | true => \"y\" | false => \"n\" end;\n\n"
        ),
        Consumed::Proved => format!(
            "let probe{suffix}(n: Nat, e: Eq()(top{suffix}(n), true)) -> Str =\n    match e | refl(@z) => \"y\" end;\n\n"
        ),
        Consumed::Compared => format!(
            "let probe{suffix}(n: Nat, m: Nat, k: Nat) -> Str =
    match top{suffix}(n)
    | true =>
        let _: Eq()(top{suffix}(m) || top{suffix}(k), top{suffix}(m) || top{suffix}(k)) = Eq/refl();
        let _: Eq()(top{suffix}(m + k), top{suffix}(k + m)) = Eq/refl();
        let _: Eq()(top{suffix}(m * 2) && top{suffix}(k), top{suffix}(m * 2) && top{suffix}(k)) = Eq/refl();
        let _: Bool/Holds(top{suffix}(n) || top{suffix}(m)) = Bool/True/qed();
        \"y\"
    | false => \"n\"
    end;\n\n"
        ),
    }
}

/// A web of `rules` predicate definitions consumed as `consumed` says, with each definition naming the one before it once or twice.
pub fn predicates(rules: usize, consumed: Consumed, twice: bool) -> String {
    let mut source = String::from(
        "use /std/{Str, Nat, Bool, Eq};\n\n\
         let Pred: Type = (x: Nat) -> Bool;\n\
         let both(p: Pred, q: Pred) -> Pred = (x) => p(x) && q(x);\n\
         let anyof(p: Pred, q: Pred) -> Pred = (x) => p(x) || q(x);\n\
         let base: Pred = (x) => x % 2 == 0;\n\
         let other: Pred = (x) => x % 3 == 0;\n\n",
    );

    for rule in 0..rules {
        let previous = if rule >= 1 {
            format!("r{}", rule - 1)
        } else {
            "base".to_string()
        };
        let older = if rule >= 2 {
            format!("r{}", rule - 2)
        } else {
            "other".to_string()
        };
        let second = match twice {
            true => previous.clone(),
            false => "base".to_string(),
        };
        let _ = writeln!(
            source,
            "let r{rule}: Pred = both({previous}, anyof({older}, {second}));"
        );
    }

    let _ = writeln!(source, "\nlet top: Pred = r{};\n", rules - 1);

    source.push_str(&consumer("", consumed));

    source.push_str(TAIL);
    source
}

/// Compile for its verdict alone.
pub fn compile_only(source: &str) -> Result<(), String> {
    let entrypoint = source.parse::<Entrypoint>().expect("probe parses");

    compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |_| {})
        .map(|_| ())
        .map_err(|error| error.to_string())
}

/// The [`predicates`] web carried over `Nat` — `+` for `both`, `*` for `anyof`, remainders for the leaves — and proved at `Eq()(top(n), 0)`. Each rule names the one before it twice, as the `twice` arm of [`predicates`] does.
pub fn numerics(rules: usize) -> String {
    let mut source = numeric_web(rules, "use /std/{Str, Nat, Eq};\n\n");
    source.push_str(
        "let probe(n: Nat, e: Eq()(top(n), 0)) -> Str =\n    match e | refl(@z) => \"y\" end;\n\n",
    );
    source.push_str(TAIL);
    source
}

/// The web's definitions through `top`, under `imports`.
pub fn numeric_web(rules: usize, imports: &str) -> String {
    let mut source = String::from(imports);
    source.push_str(
        "let Fn: Type = (x: Nat) -> Nat;\n\
         let both(p: Fn, q: Fn) -> Fn = (x) => p(x) + q(x);\n\
         let anyof(p: Fn, q: Fn) -> Fn = (x) => p(x) * q(x);\n\
         let base: Fn = (x) => x % 2;\n\
         let other: Fn = (x) => x % 3;\n\n",
    );

    for rule in 0..rules {
        let previous = if rule >= 1 {
            format!("r{}", rule - 1)
        } else {
            "base".to_string()
        };
        let older = if rule >= 2 {
            format!("r{}", rule - 2)
        } else {
            "other".to_string()
        };
        let _ = writeln!(
            source,
            "let r{rule}: Fn = both({previous}, anyof({older}, {previous}));"
        );
    }

    let _ = writeln!(source, "\nlet top: Fn = r{};\n", rules - 1);
    source
}
