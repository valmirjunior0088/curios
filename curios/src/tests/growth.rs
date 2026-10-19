//! What a program costs to compile as it grows.
//!
//! A growth is an ordinary program written along one axis an author grows: longer, wider, deeper, under more binders, or holding a larger literal. Doubling one should at most double what each stage does, and change no verdict, which is `documentation/design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md`'s rule; that its depth costs only memory is `documentation/design/compilation/depth-is-bought-with-stack-not-with-hand-rolled-frames.md`'s, and that its module validates at any size is `curios/src/compile.rs`'s.
//!
//! A growth that compiles past the size a stage once refused or never answered at is a fixture here; `growth_slopes`, among the instrumented tests, counts what each stage does at three sizes and holds the growths that are linear today to it.

use {
    super::compile,
    curios_pipeline::{DEFAULT_STEP_BUDGET, typecheck_with_prelude},
    curios_text::{Entrypoint, RootSource},
    curios_utilities::fill,
    std::{fmt::Write, iter},
};

/// One axis a program grows along. Each writes a whole program whose result reads the process's argument count, so no stage can compute it ahead of the run.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Growth {
    /// `let x{i} = x{i-1} + 1;`, each operator minting the metavariables of its witness, in one function's body.
    LetPlus,
    /// `let x{i} = Nat/add(x{i-1}, 1);`, the same chain written as calls, which mint nothing.
    LetCall,
    /// `let _ = /std/print("x")!;` in one `Io` block: each statement binds its continuation's parameter.
    Statements,
    /// Top-level definitions, each one more than the last.
    Definitions,
    /// Top-level functions, each calling the last.
    Functions,
    /// Top-level string definitions.
    Strings,
    /// A list literal of numerals.
    List,
    /// A list literal of elements computed from the argument count.
    ListOpen,
    /// A match of numeral arms.
    Arms,
    /// A function of so many parameters, adding its first and its last.
    Parameters,
    /// A function of so many parameters, adding them all: every operator stands under every parameter.
    ParametersSummed,
    /// An anonymous tuple of so many members.
    Tuple,
    /// A structure of so many fields, built and read.
    Fields,
    /// An inductive type of so many constructors, matched once.
    Constructors,
    /// A concept of so many methods, each a wrapper of its own, satisfied once and called once.
    Methods,
    /// Calls of an identity function, each the argument of the next.
    Calls,
    /// Parentheses, each around the next.
    Parentheses,
    /// `1 + (1 + (… + a))`, nested to the right.
    RightPlus,
    /// `a + 1 + … + 1`, one left-leaning chain.
    LeftPlus,
    /// Matches, each the last arm of the one around it.
    Matches,
    /// Matches, each on the option the `let` before it binds and binding the next.
    ChainedMatches,
    /// Lambdas, each the body of the one around it, applied in turn.
    Lambdas,
    /// Singleton list literals, each the element of the next.
    Lists,
    /// Parenthesized `let`s, each the tail of the one around it.
    Lets,
    /// One curried arrow type of so many parameters.
    Arrow,
    /// One string literal of so many characters.
    StringLiteral,
}

impl Growth {
    /// Every growth, in the order a table lists them.
    pub(super) const ALL: [Growth; 26] = [
        Growth::LetPlus,
        Growth::LetCall,
        Growth::Statements,
        Growth::Definitions,
        Growth::Functions,
        Growth::Strings,
        Growth::List,
        Growth::ListOpen,
        Growth::Arms,
        Growth::Parameters,
        Growth::ParametersSummed,
        Growth::Tuple,
        Growth::Fields,
        Growth::Constructors,
        Growth::Methods,
        Growth::Calls,
        Growth::Parentheses,
        Growth::RightPlus,
        Growth::LeftPlus,
        Growth::Matches,
        Growth::ChainedMatches,
        Growth::Lambdas,
        Growth::Lists,
        Growth::Lets,
        Growth::Arrow,
        Growth::StringLiteral,
    ];
}

/// The program `growth` writes at `size`.
pub(super) fn grown(growth: Growth, size: usize) -> String {
    let last = size - 1;
    match growth {
        Growth::LetPlus => chain(size, |previous| format!("{previous} + 1")),
        Growth::LetCall => chain(size, |previous| format!("Nat/add({previous}, 1)")),
        Growth::Statements => {
            let statements = "    let _ = /std/print(\"x\")!;\n".repeat(size);
            fill!(
                "let entry() -> /std/Io({}) =\n${statements}    /std/print(\"\\n\");\n\nentry()\n",
                statements,
            )
        }
        Growth::Definitions => {
            let mut definitions = "let c0: Nat = 0;\n".to_string();
            for index in 1..size {
                writeln!(definitions, "let c{index}: Nat = c{} + 1;", index - 1)
                    .expect("a string takes a line");
            }
            program(&definitions, &format!("c{last} + a"))
        }
        Growth::Functions => {
            let mut definitions = "let g0(x: Nat) -> Nat = x;\n".to_string();
            for index in 1..size {
                writeln!(
                    definitions,
                    "let g{index}(x: Nat) -> Nat = g{}(x) + 1;",
                    index - 1
                )
                .expect("a string takes a line");
            }
            program(&definitions, &format!("g{last}(a)"))
        }
        Growth::Strings => {
            let definitions = (0..size)
                .map(|index| format!("let s{index}: Str = \"s{index}\";\n"))
                .collect::<String>();
            program(&definitions, &format!("Str/len(s{last}) + a"))
        }
        Growth::List => {
            let elements = joined((0..size).map(|index| index.to_string()), ", ");
            program(
                &format!("let xs: /std/List(Nat) = [{elements}];\n"),
                "/std/List/len(xs) + a",
            )
        }
        Growth::ListOpen => {
            let elements = joined(
                (0..size).map(|index| match index % 2 {
                    0 => "a".to_string(),
                    _ => format!("a + {index}"),
                }),
                ", ",
            );
            program(
                &format!("let f(a: Nat) -> /std/List(Nat) = [{elements}];\n"),
                "/std/List/len(f(a))",
            )
        }
        Growth::Arms => {
            let arms = (0..size)
                .map(|index| format!("    | {index} => {}\n", index + 1))
                .collect::<String>();
            program(
                &format!(
                    "let f(k: Nat) -> Nat =\n    match k: (_) => Nat\n{arms}    | _ => 0\n    end;\n"
                ),
                "f(a)",
            )
        }
        Growth::Parameters => program(
            &format!("let f({}) -> Nat = x0 + x{last};\n", parameters(size)),
            &format!("f({})", repeated("a", size, ", ")),
        ),
        Growth::ParametersSummed => program(
            &format!(
                "let f({}) -> Nat = {};\n",
                parameters(size),
                joined((0..size).map(|index| format!("x{index}")), " + ")
            ),
            &format!("f({})", repeated("a", size, ", ")),
        ),
        Growth::Tuple => program(
            &format!(
                "let f(a: Nat) -> Nat =\n    let t = ({});\n    a;\n",
                repeated("a", size, ", ")
            ),
            "f(a)",
        ),
        Growth::Fields => {
            let fields = joined((0..size).map(|index| format!("f{index}: Nat")), ", ");
            let values = joined((0..size).map(|index| format!("f{index} = a")), ", ");
            program(
                &fill!(
                    "struct S: Type { ${fields} }\n\nlet f(a: Nat) -> Nat =\n    let s = S { ${values} };\n    s.f${last};\n",
                    fields,
                    values,
                    last,
                ),
                "f(a)",
            )
        }
        Growth::Constructors => {
            let constructors = (0..size)
                .map(|index| format!("| k{index}()\n"))
                .collect::<String>();
            let arms = (0..size)
                .map(|index| format!("    | k{index}() => {index}\n"))
                .collect::<String>();
            program(
                &format!(
                    "induct K: Type\n{constructors}end\n\nlet f(k: K) -> Nat =\n    match k: (_) => Nat\n{arms}    end;\n"
                ),
                &format!("f(K/k{last}()) + a"),
            )
        }
        Growth::Methods => {
            let methods = (0..size)
                .map(|index| format!("    m{index}(A) -> A,\n"))
                .collect::<String>();
            let fields = (0..size)
                .map(|index| format!("    m{index}(x) = x,\n"))
                .collect::<String>();
            program(
                &fill!(
                    "concept C(A: Type): pub Type {\n${methods}}\n\nsatisfy C(Nat) {\n${fields}}\n",
                    methods,
                    fields,
                ),
                &format!("C/m{last}(a)"),
            )
        }
        Growth::Calls => program(
            "let f(n: Nat) -> Nat = n;\n",
            &format!("{}a{}", "f(".repeat(size), ")".repeat(size)),
        ),
        Growth::Parentheses => program("", &format!("{}a{}", "(".repeat(size), ")".repeat(size))),
        Growth::RightPlus => program(
            "",
            &format!("{}a{}", "1 + (".repeat(size), ")".repeat(size)),
        ),
        Growth::LeftPlus => program("", &format!("a{}", " + 1".repeat(size))),
        Growth::Matches => {
            let term = (0..size).fold("a".to_string(), |inner, index| {
                format!("match a: (_) => Nat | 0 => {index} | _ => {inner} end")
            });
            program(&format!("let f(a: Nat) -> Nat = {term};\n"), "f(a)")
        }
        Growth::ChainedMatches => {
            let mut body =
                "let f(a: Nat) -> Nat =\n    let o0: /std/Option(Nat) = /std/Option/some(a);\n"
                    .to_string();
            for index in 1..size {
                writeln!(
                    body,
                    "    let o{index}: /std/Option(Nat) = match o{} | some(x) => /std/Option/some(x + 1) | none() => /std/Option/none() end;",
                    index - 1
                )
                .expect("a string takes a line");
            }
            writeln!(body, "    match o{last} | some(x) => x | none() => 0 end;")
                .expect("a string takes a line");
            program(&body, "f(a)")
        }
        Growth::Lambdas => {
            let lambdas = (0..size)
                .map(|index| format!("(x{index}) => "))
                .collect::<String>();
            program(
                &format!("let g: {}Nat = {lambdas}x0;\n", "(Nat) -> ".repeat(size)),
                &format!("g{}", "(a)".repeat(size)),
            )
        }
        Growth::Lists => {
            let type_ = (0..size).fold("Nat".to_string(), |inner, _| format!("/std/List({inner})"));
            program(
                &format!(
                    "let f(a: Nat) -> {type_} = {}a{};\n",
                    "[".repeat(size),
                    "]".repeat(size)
                ),
                "/std/List/len(f(a))",
            )
        }
        Growth::Lets => {
            let term = (0..size).rev().fold(format!("x{last}"), |tail, index| {
                let value = match index {
                    0 => "a".to_string(),
                    _ => format!("x{}", index - 1),
                };
                format!("(let x{index} = {value}; {tail})")
            });
            program(&format!("let f(a: Nat) -> Nat = {term};\n"), "f(a)")
        }
        Growth::Arrow => program(
            &format!("let T: Type = {}Nat;\n", "(Nat) -> ".repeat(size)),
            "a",
        ),
        Growth::StringLiteral => program(
            &format!("let s: Str = \"{}\";\n", "ab".repeat(size / 2)),
            "Str/len(s) + a",
        ),
    }
}

/// A whole program: `definitions`, then an entry printing `result` over `a`, the argument count.
fn program(definitions: &str, result: &str) -> String {
    fill!(
        "use /std/{Nat, Str};\n\n${definitions}\nlet entry() -> /std/Io({}) =\n    let a = /std/List/len(/std/Process/args()!);\n    /std/print(Str/concat(Nat/to_str(${result}), \"\\n\"));\n\nentry()\n",
        definitions,
        result,
    )
}

/// `f(n)`, a chain of `size` `let`s in its body, each `line` of the one before.
fn chain(size: usize, line: impl Fn(&str) -> String) -> String {
    let mut body = "let f(n: Nat) -> Nat =\n    let x0 = n;\n".to_string();
    for index in 1..=size {
        writeln!(
            body,
            "    let x{index} = {};",
            line(&format!("x{}", index - 1))
        )
        .expect("a string takes a line");
    }
    writeln!(body, "    x{size};").expect("a string takes a line");
    program(&body, "f(a)")
}

/// `x0: Nat, …`, `size` of them.
fn parameters(size: usize) -> String {
    joined((0..size).map(|index| format!("x{index}: Nat")), ", ")
}

fn repeated(item: &str, size: usize, separator: &str) -> String {
    joined(iter::repeat_n(item.to_string(), size), separator)
}

fn joined(items: impl Iterator<Item = String>, separator: &str) -> String {
    items.collect::<Vec<_>>().join(separator)
}

/// Whether `source` compiles through every stage — Binaryen and Cranelift included — with what it is refused for where it does not.
fn compiles(source: &str) -> Result<(), String> {
    compile(source).map(|_| ())
}

/// Whether `source` is lowered and elaborated, its erasure obligations met, with what it is refused for where it is not: what a fixture holding elaboration's own cost asks, the stages after it being other fixtures'.
fn elaborates(source: &str) -> Result<(), String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())?;
    let checked = typecheck_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none())
        .map_err(|refused| format!("{refused:?}"))?;
    match checked.obligations.is_empty() {
        true => Ok(()),
        false => Err(format!("{:?}", checked.obligations)),
    }
}

/// Elaboration takes a deep expression in its size: a left-leaning and a right-nested chain of ten thousand operators, whose recorded terms are each other's subterms and are zonked in one pass, and a thousand nested list literals, whose levels are a chain of solutions zonked once along it.
#[test]
fn deep_expressions_elaborate_in_their_size() {
    for (growth, size) in [
        (Growth::LeftPlus, 10_000),
        (Growth::RightPlus, 10_000),
        (Growth::Lists, 1_000),
    ] {
        assert_eq!(elaborates(&grown(growth, size)), Ok(()), "{growth:?}");
    }
}

/// Elaboration takes a scope in its size: a `let` chain of six thousand bindings, each operator minting its metavariables under every binding before it; an `Io` block of two thousand statements, each a frame over the last; a function summing a thousand parameters; and two thousand nested lambdas. A judgment reads its scope through the frames' persistent maps, and a metavariable is born over its scope's telescope and identity spine, shared, so none pays the binders above it; and a close or a materialization passes over what it cannot change, so none pays the statements or the lambdas within it.
#[test]
fn a_wide_scope_elaborates_in_its_size() {
    for (growth, size) in [
        (Growth::LetPlus, 6_000),
        (Growth::Statements, 2_000),
        (Growth::ParametersSummed, 1_000),
        (Growth::Lambdas, 2_000),
    ] {
        assert_eq!(elaborates(&grown(growth, size)), Ok(()), "{growth:?}");
    }
}

/// A family of four thousand constructors compiles in its size: each constructor is an item of its own, and each imports the one constructor it reads of the family's row, at its position, rather than the row.
#[test]
fn a_family_of_many_constructors_compiles_in_its_size() {
    assert_eq!(compiles(&grown(Growth::Constructors, 4_000)), Ok(()));
}

/// A concept of two thousand methods compiles in its size: each method's wrapper reads its one field of the concept's record, its parameter's shape off the record's row, and the record type's sort alone, where each once read every field.
#[test]
fn a_concept_of_many_methods_compiles_in_its_size() {
    assert_eq!(compiles(&grown(Growth::Methods, 2_000)), Ok(()));
}

/// Two thousand lambdas applied in turn compile: each is moved into its one call as the one around it is, within one pass of the continuation optimizer's shrinker, where a round apiece would run the optimizer past its backstop.
#[test]
fn a_nest_of_lambdas_applied_in_turn_compiles_in_its_size() {
    assert_eq!(compiles(&grown(Growth::Lambdas, 2_000)), Ok(()));
}

/// Nesting ten thousand deep compiles on a test thread's stack: parentheses, each a nonterminal the parser re-enters, and an arrow, each parameter a term the lowering lowers inside the one before. The depth is bought with stack where it is spent, so no stage's acceptance follows the thread it runs on.
#[test]
fn nesting_ten_thousand_deep_compiles_on_a_test_thread() {
    for growth in [Growth::Parentheses, Growth::Arrow] {
        assert_eq!(compiles(&grown(growth, 10_000)), Ok(()), "{growth:?}");
    }
}

/// Every growth compiles and runs at a small size: what `growth_slopes` measures is a program, and one a stage refuses measures nothing.
#[test]
fn every_growth_is_a_program_at_its_smallest() {
    for growth in Growth::ALL {
        assert_eq!(compiles(&grown(growth, 3)), Ok(()), "{growth:?}");
    }
}
