//! A growth: an ordinary program written along one axis an author grows — longer, wider, deeper, under more binders, or holding a larger literal — which the growth suites compile and the bench's measurement of growth counts.

use {
    curios_common::fill,
    std::{fmt::Write, iter},
};

/// One axis a program grows along. Each writes a whole program whose result reads the process's argument count, so no stage can compute it ahead of the run.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Growth {
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
    /// A function of so many parameters whose codomain claims their sum equal to itself: every operator's metavariables are born under every parameter, sharing one spine, and closing the parameters over the codomain captures it.
    CodomainSummed,
    /// `let x{i} = n + i;` in one function's body, then a lambda adding them all, applied once: the lambda's body names every binding, and every operator in it stands under the lambda's binder.
    LetsSummed,
    /// A function of so many parameters, each the summand of a call nested over the one before, claiming by `Eq/refl()` that the spine's product with the same spine over one more parameter commutes: two factors of one shape, parted at their innermost leaf, which the fold orders by rank.
    ProductCommuted,
    /// An anonymous tuple of so many members.
    Tuple,
    /// A structure of so many fields, built and read.
    Fields,
    /// A structure of so many fields and so many functions, each reading the last field of the one it takes.
    StructReaders,
    /// An inductive type of so many constructors, matched once.
    Constructors,
    /// An inductive type of so many constructors and so many top-level values, each one of its constructors: each value is evaluated where its unit is erased.
    Values,
    /// An inductive type of so many constructors and so many functions, each matching it with one arm and a catch-all.
    DefaultedMatches,
    /// A parameterized inductive type of so many constructors and so many proofs, each that two constructions of its last constructor are one.
    ComparedConstructions,
    /// A closed tuple alias of so many fields and so many functions, each projecting the first field of the parameter it types.
    AliasReaders,
    /// A closed function-type alias of so many domains and so many functions, each taking a parameter it types and leaving it unread.
    ArrowAliasReaders,
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
    /// Self-tail loops, each called once from the entry: each a continuation jumps enter once contified into it.
    Loops,
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
    pub const ALL: [Growth; 36] = [
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
        Growth::CodomainSummed,
        Growth::LetsSummed,
        Growth::ProductCommuted,
        Growth::Tuple,
        Growth::Fields,
        Growth::StructReaders,
        Growth::Constructors,
        Growth::Values,
        Growth::DefaultedMatches,
        Growth::ComparedConstructions,
        Growth::AliasReaders,
        Growth::ArrowAliasReaders,
        Growth::Methods,
        Growth::Calls,
        Growth::Parentheses,
        Growth::RightPlus,
        Growth::LeftPlus,
        Growth::Matches,
        Growth::ChainedMatches,
        Growth::Loops,
        Growth::Lambdas,
        Growth::Lists,
        Growth::Lets,
        Growth::Arrow,
        Growth::StringLiteral,
    ];
}

/// The program `growth` writes at `size`.
pub fn grown(growth: Growth, size: usize) -> String {
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
            program(&definitions, &format!("Str/length(s{last}) + a"))
        }
        Growth::List => {
            let elements = joined((0..size).map(|index| index.to_string()), ", ");
            program(
                &format!("let xs: /std/List(Nat) = [{elements}];\n"),
                "/std/List/length(xs) + a",
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
                "/std/List/length(f(a))",
            )
        }
        Growth::Arms => {
            let arms = (0..size)
                .map(|index| format!("    | {index} => {}\n", index + 1))
                .collect::<String>();
            program(
                &format!("let f(k: Nat) -> Nat =\n    match k\n{arms}    | _ => 0\n    end;\n"),
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
        Growth::CodomainSummed => {
            let sum = joined((0..size).map(|index| format!("x{index}")), " + ");
            program(
                &format!(
                    "let _claim({}) -> /std/Eq()({sum}, {sum}) = /std/Eq/refl();\n",
                    parameters(size)
                ),
                "a",
            )
        }
        Growth::LetsSummed => {
            let mut body = "let f(n: Nat) -> Nat =\n".to_string();
            for index in 0..size {
                writeln!(body, "    let x{index} = n + {index};").expect("a string takes a line");
            }
            writeln!(
                body,
                "    let g = (m: Nat) => m + {};\n    g(1);",
                joined((0..size).map(|index| format!("x{index}")), " + ")
            )
            .expect("a string takes a line");
            program(&body, "f(a)")
        }
        Growth::ProductCommuted => {
            // `g(x{size} + g(… g(x1 + leaf)))`: over `x0` and over `u`, two factors of one shape.
            let spine = |leaf: &str| {
                (1..=size).fold(leaf.to_string(), |inner, index| {
                    format!("g(x{index} + {inner})")
                })
            };
            let (over_x0, over_u) = (spine("x0"), spine("u"));
            program(
                &format!(
                    "let f(g: (Nat) -> Nat, u: Nat, {}) -> Nat =\n    let _commuted: /std/Eq()({over_x0} * {over_u}, {over_u} * {over_x0}) = /std/Eq/refl();\n    x0;\n",
                    parameters(size + 1)
                ),
                &format!("f((v: Nat) => v, a, {})", repeated("a", size + 1, ", ")),
            )
        }
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
        Growth::StructReaders => {
            let fields = joined((0..size).map(|index| format!("f{index}: Nat")), ", ");
            let readers = (0..size)
                .map(|index| format!("let g{index}(s: S) -> Nat = s.f{last};\n"))
                .collect::<String>();
            program(
                &fill!(
                    "struct S: Type { ${fields} }\n\n${readers}",
                    fields,
                    readers
                ),
                "a",
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
                    "induct K: Type\n{constructors}end\n\nlet f(k: K) -> Nat =\n    match k\n{arms}    end;\n"
                ),
                &format!("f(K/k{last}()) + a"),
            )
        }
        Growth::Values => {
            let constructors = (0..size)
                .map(|index| format!("| k{index}()\n"))
                .collect::<String>();
            let values = (0..size)
                .map(|index| format!("let v{index}: K = K/k{index}();\n"))
                .collect::<String>();
            program(
                &format!("induct K: Type\n{constructors}end\n\n{values}"),
                &format!("match v{last} | k0() => 0 | _ => a end"),
            )
        }
        Growth::DefaultedMatches => {
            let constructors = (0..size)
                .map(|index| format!("| k{index}()\n"))
                .collect::<String>();
            let matches = (0..size)
                .map(|index| {
                    format!(
                        "let g{index}(k: K) -> Nat = match k | k{last}() => {index} | _ => 0 end;\n"
                    )
                })
                .collect::<String>();
            program(
                &format!("induct K: Type\n{constructors}end\n\n{matches}"),
                "a",
            )
        }
        Growth::ComparedConstructions => {
            let constructors = (0..size)
                .map(|index| format!("| k{index}(A)\n"))
                .collect::<String>();
            let proofs = (0..size)
                .map(|index| {
                    format!(
                        "let e{index}: /std/Eq()(W/k{last}(Nat/add({index}, 1)), W/k{last}({index} + 1)) = /std/Eq/refl();\n"
                    )
                })
                .collect::<String>();
            program(
                &format!("induct W(A: Type): Type\n{constructors}end\n\n{proofs}"),
                "a",
            )
        }
        Growth::AliasReaders => {
            let readers = (0..size)
                .map(|index| format!("let f{index}(x: T) -> Nat = x.0;\n"))
                .collect::<String>();
            program(
                &format!(
                    "let T: Type = {{{}}};\n{readers}",
                    repeated("Nat", size, ", ")
                ),
                "a",
            )
        }
        Growth::ArrowAliasReaders => {
            let readers = (0..size)
                .map(|index| format!("let g{index}(_f: F) -> Nat = {index};\n"))
                .collect::<String>();
            program(
                &format!(
                    "let F: Type = ({}) -> Nat;\n{readers}",
                    repeated("Nat", size, ", ")
                ),
                "a",
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
                format!("match a | 0 => {index} | _ => {inner} end")
            });
            program(&format!("let f(a: Nat) -> Nat = {term};\n"), "f(a)")
        }
        Growth::Loops => {
            let mut definitions = String::new();
            for index in 0..size {
                writeln!(
                    definitions,
                    "let loop{index}(n: Nat, acc: Nat) -> Nat = match n | 0 => acc | p + 1 => loop{index}(p, acc + {}) end;",
                    index + 1
                )
                .expect("a string takes a line");
            }
            let result = joined((0..size).map(|index| format!("loop{index}(a, 0)")), " + ");
            program(&definitions, &result)
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
                "/std/List/length(f(a))",
            )
        }
        Growth::Lets => {
            let term = (0..size).rev().fold(format!("x{last}"), |tail, index| {
                let value = match index {
                    0 => "a".to_string(),
                    _ => format!("x{}", index - 1),
                };
                format!("(() => let x{index} = {value}; {tail})()")
            });
            program(&format!("let f(a: Nat) -> Nat = {term};\n"), "f(a)")
        }
        Growth::Arrow => program(
            &format!("let T: Type = {}Nat;\n", "(Nat) -> ".repeat(size)),
            "a",
        ),
        Growth::StringLiteral => program(
            &format!("let s: Str = \"{}\";\n", "ab".repeat(size / 2)),
            "Str/length(s) + a",
        ),
    }
}

/// A whole program: `definitions`, then an entry printing `result` over `a`, the argument count.
pub fn program(definitions: &str, result: &str) -> String {
    fill!(
        "use /std/{Nat, Str};\n\n${definitions}\nlet entry() -> /std/Io({}) =\n    let a = /std/List/length(/std/Proc/args()!);\n    /std/print(Str/concat(Nat/to_str(${result}), \"\\n\"));\n\nentry()\n",
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

pub fn repeated(item: &str, size: usize, separator: &str) -> String {
    joined(iter::repeat_n(item.to_string(), size), separator)
}

pub fn joined(items: impl Iterator<Item = String>, separator: &str) -> String {
    items.collect::<Vec<_>>().join(separator)
}
