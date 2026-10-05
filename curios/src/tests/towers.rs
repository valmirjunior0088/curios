//! What a tower costs to compile.
//!
//! A tower is a chain of `let`s in which each line names the line before it twice: `n` lines are a graph of `n` nodes whose tree has `2ⁿ`. A stage that walks the graph pays a line for a line; one that walks it once per path doubles with every line, and at sixty lines does not answer. Every tower here is an ordinary program that reduces nothing, so what it costs is what the stages other than reduction cost — the half of `documentation/design/soundness/a-reduction-step-costs-what-it-builds.md`'s premise a budget cannot hold, and the subject of `documentation/design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md`.
//!
//! A shape that compiles at sixty lines is a fixture here; `tower_measurements` counts what every shape costs each checker at two heights. What a tower's type costs to *show* is held here too: its print is its tree, and both a rung and a report end.

#[cfg(feature = "profile")]
mod measurement_tests;

use {
    super::core_elab,
    curios_pipeline::{DEFAULT_STEP_BUDGET, compile_with_prelude},
    curios_text::{Entrypoint, RootSource},
    std::fmt::Write,
};

/// Where a tower of values stands.
#[derive(Clone, Copy)]
enum Stands {
    /// In the body of a function that does not recurse.
    Function,
    /// In the successor arm of a function that calls itself, where each call is recorded where it is typed.
    Member,
}

/// What each line of a tower is, in the line before it, `x`. A tower of values says where it stands; an alias and a claim stand at the top level.
#[derive(Clone, Copy)]
enum Shape {
    /// `g(x)`, naming the line before it once: no tower, and what every tower is read against.
    Chain(Stands),
    /// `g(x, x)` over a function the tower is handed, so every line's type is `Nat` and the term alone is a graph.
    Calls(Stands),
    /// `(x, x)`, whose type doubles with it.
    Pairs(Stands),
    /// `[x, x]`, whose type nests and shares nothing.
    Lists(Stands),
    /// `match b | true => x | false => x end`, each arm the line before.
    Arms(Stands),
    /// `match g(b) | true => x | false => x end`, each arm the line before under a scrutinee that is no variable: an arm records its case equation, and what is typed under one answers under no other.
    Guards(Stands),
    /// `g(x, x) + tower(g, p)`, in a recursive member's body: every line calls the member, so no line's type is remembered and each is typed where it stands.
    Recursions,
    /// `{T, T}`, a top-level alias of the alias before it.
    Aliases,
    /// `(T) -> T`, a top-level alias of the alias before it.
    Arrows,
    /// `(x, x)` twice, as two top-level chains built apart and claimed equal.
    TwoTowers,
    /// `g(x, x)` twice in one function's body, the second chain from a spelling that only reduces to the first's, and the two claimed equal: one spelling is not the other, so equality cannot answer and conversion compares them.
    Apart,
}

/// What a tower of values is written from, as the body of `tower`.
struct Body {
    /// What the program imports from `/std`.
    imports: &'static str,
    /// The function's parameters, its `Nat` last.
    parameters: &'static str,
    /// The arguments a member hands on before its predecessor.
    forwarded: &'static str,
    /// The first line, in the `Nat` the tower stands on.
    first: fn(&str) -> String,
    /// A line, in the line before it.
    line: fn(&str) -> String,
    /// The body's result, in the last line and — in a member — the call it makes to itself.
    result: fn(&str, Option<&str>) -> String,
}

/// A tower of `lines` lines over its first, as a whole program.
fn tower(shape: Shape, lines: usize) -> String {
    let mut source = String::new();

    match shape {
        Shape::Chain(stands) => function(
            &mut source,
            stands,
            lines,
            &Body {
                imports: "Nat",
                parameters: "g: (Nat) -> Nat, n: Nat",
                forwarded: "g, ",
                first: |n| format!("g({n})"),
                line: |x| format!("g({x})"),
                result: |last, again| match again {
                    Some(again) => format!("g({last}) + {again}"),
                    None => last.to_string(),
                },
            },
        ),
        Shape::Calls(stands) => function(
            &mut source,
            stands,
            lines,
            &Body {
                imports: "Nat",
                parameters: "g: (Nat, Nat) -> Nat, n: Nat",
                forwarded: "g, ",
                first: |n| format!("g({n}, {n})"),
                line: |x| format!("g({x}, {x})"),
                result: |last, again| match again {
                    Some(again) => format!("g({last}, {again})"),
                    None => last.to_string(),
                },
            },
        ),
        Shape::Pairs(stands) => function(
            &mut source,
            stands,
            lines,
            &Body {
                imports: "Nat",
                parameters: "n: Nat",
                forwarded: "",
                first: |n| format!("({n}, {n})"),
                line: |x| format!("({x}, {x})"),
                result: |last, again| format!("match {last} | _ => {} end", again.unwrap_or("n")),
            },
        ),
        Shape::Lists(stands) => function(
            &mut source,
            stands,
            lines,
            &Body {
                imports: "Nat, List",
                parameters: "n: Nat",
                forwarded: "",
                first: |n| format!("[{n}, {n}]"),
                line: |x| format!("[{x}, {x}]"),
                result: |last, again| format!("match {last} | _ => {} end", again.unwrap_or("n")),
            },
        ),
        Shape::Arms(stands) => function(
            &mut source,
            stands,
            lines,
            &Body {
                imports: "Nat, Bool",
                parameters: "b: Bool, n: Nat",
                forwarded: "b, ",
                first: |n| n.to_string(),
                line: |x| format!("match b | true => {x} | false => {x} end"),
                result: |last, again| match again {
                    Some(again) => format!("{last} + {again}"),
                    None => last.to_string(),
                },
            },
        ),
        Shape::Guards(stands) => function(
            &mut source,
            stands,
            lines,
            &Body {
                imports: "Nat, Bool",
                parameters: "g: (Bool) -> Bool, b: Bool, n: Nat",
                forwarded: "g, b, ",
                first: |n| n.to_string(),
                line: |x| format!("match g(b) | true => {x} | false => {x} end"),
                result: |last, again| match again {
                    Some(again) => format!("{last} + {again}"),
                    None => last.to_string(),
                },
            },
        ),
        Shape::Recursions => function(
            &mut source,
            Stands::Member,
            lines,
            &Body {
                imports: "Nat",
                parameters: "g: (Nat, Nat) -> Nat, n: Nat",
                forwarded: "g, ",
                first: |n| format!("g({n}, {n})"),
                line: |x| format!("g({x}, {x}) + tower(g, p)"),
                result: |last, again| match again {
                    Some(again) => format!("g({last}, {again})"),
                    None => last.to_string(),
                },
            },
        ),
        Shape::Aliases => {
            source.push_str("use /std/{Nat};\n\n");
            chain(&mut source, "", "T", lines, "Nat", |t| {
                format!("{{{t}, {t}}}")
            });
            let _ = writeln!(source, "\nlet keep(x: T{lines}) -> T{lines} = x;");
        }
        Shape::Arrows => {
            source.push_str("use /std/{Nat};\n\n");
            chain(&mut source, "", "T", lines, "Nat", |t| {
                format!("({t}) -> {t}")
            });
            let _ = writeln!(source, "\nlet keep(x: T{lines}) -> T{lines} = x;");
        }
        Shape::TwoTowers => {
            source.push_str("use /std/{Nat, Eq};\n\n");
            chain(&mut source, "", "x", lines, "(1, 2)", |x| {
                format!("({x}, {x})")
            });
            chain(&mut source, "", "y", lines, "(1, 2)", |y| {
                format!("({y}, {y})")
            });
            let _ = writeln!(
                source,
                "\nlet _same: Eq()(x{lines}, y{lines}) =\n    Eq/refl();"
            );
        }
        Shape::Apart => {
            source.push_str("use /std/{Nat, Eq};\n\n");
            source.push_str("let tower(g: (Nat, Nat) -> Nat, n: Nat) -> Nat =\n");
            chain(&mut source, "    ", "x", lines, "g(n, n)", |x| {
                format!("g({x}, {x})")
            });
            chain(&mut source, "    ", "y", lines, "g(n, n + 0)", |y| {
                format!("g({y}, {y})")
            });
            let _ = writeln!(
                source,
                "    let _same: Eq()(x{lines}, y{lines}) = Eq/refl();\n    n;"
            );
        }
    }

    source.push_str("\n/std/print(\"ok\\n\")\n");
    source
}

/// The `let`s of one chain: `{name}0` at `first`, and each of the `lines` after it at `line` of the one before.
fn chain(
    source: &mut String,
    indent: &str,
    name: &str,
    lines: usize,
    first: &str,
    line: fn(&str) -> String,
) {
    let _ = writeln!(source, "{indent}let {name}0 = {first};");
    for index in 1..=lines {
        let before = format!("{name}{}", index - 1);
        let _ = writeln!(source, "{indent}let {name}{index} = {};", line(&before));
    }
}

/// A tower of values as the body of `tower`: its lines from the function's `Nat`, and in a member's body from the predecessor the successor arm binds, where the member calls itself.
fn function(source: &mut String, stands: Stands, lines: usize, body: &Body) {
    let last = format!("x{lines}");
    let _ = writeln!(source, "use /std/{{{}}};\n", body.imports);
    let _ = writeln!(source, "let tower({}) -> Nat =", body.parameters);

    match stands {
        Stands::Function => {
            chain(source, "    ", "x", lines, &(body.first)("n"), body.line);
            let _ = writeln!(source, "    {};", (body.result)(&last, None));
        }
        Stands::Member => {
            let again = format!("tower({}p)", body.forwarded);
            let _ = writeln!(source, "    match n\n    | 0 => 0\n    | p + 1; _ =>");
            chain(
                source,
                "        ",
                "x",
                lines,
                &(body.first)("p"),
                body.line,
            );
            let _ = writeln!(
                source,
                "        {}\n    end;",
                (body.result)(&last, Some(&again))
            );
        }
    }
}

/// Whether `source` compiles, with what it is refused for where it does not.
fn compiles(source: &str) -> Result<(), String> {
    let entrypoint = source.parse::<Entrypoint>().expect("a tower parses");

    compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |_| {},
    )
    .map(|_| ())
    .map_err(|error| error.to_string())
}

/// The chain every tower is read against: sixty lines each naming the one before it once, in a function's body and in a recursive member's.
#[test]
fn a_sixty_line_chain_compiles() {
    for stands in [Stands::Function, Stands::Member] {
        assert_eq!(compiles(&tower(Shape::Chain(stands), 60)), Ok(()));
    }
}

/// The towers both checkers walk in their size, at sixty lines: calls, pairs, lists, arms on a variable and arms on an expression in a function's body and in a recursive member's, lines that each call their member, both aliases, the two towers claimed equal, and the two chains of calls built apart.
#[test]
fn a_sixty_line_tower_compiles() {
    for shape in [
        Shape::Calls(Stands::Function),
        Shape::Calls(Stands::Member),
        Shape::Pairs(Stands::Function),
        Shape::Pairs(Stands::Member),
        Shape::Lists(Stands::Function),
        Shape::Lists(Stands::Member),
        Shape::Arms(Stands::Function),
        Shape::Arms(Stands::Member),
        Shape::Guards(Stands::Function),
        Shape::Guards(Stands::Member),
        Shape::Recursions,
        Shape::Aliases,
        Shape::Arrows,
        Shape::TwoTowers,
        Shape::Apart,
    ] {
        assert_eq!(compiles(&tower(shape, 60)), Ok(()));
    }
}

/// A tower's `core-elab` rung prints. The type each `let` was elaborated at is a graph the rung prints as its tree — sixty lines of pairs are a type of 2⁶⁰ leaves — and a print bounded by its term's graph shows its beginning and elides the rest.
#[test]
fn a_towers_elaborated_rung_prints() {
    let printed = core_elab(&tower(Shape::Pairs(Stands::Function), 60));

    assert!(printed.contains('…'));
}

/// A mismatch over a tower reports: the type a sixty-line tower of pairs has is shown elided, where its print whole is one no report survives writing.
#[test]
fn a_mismatch_over_a_tower_reports() {
    let mut source = String::from("use /std/{Nat};\n\nlet tower(n: Nat) -> Nat =\n");
    chain(&mut source, "    ", "x", 60, "(n, n)", |before| {
        format!("({before}, {before})")
    });
    source.push_str("    x60;\n\n/std/print(\"ok\\n\")\n");

    let report = compiles(&source).expect_err("a pair is no `Nat`");

    assert!(report.contains('…'), "{report}");
}
