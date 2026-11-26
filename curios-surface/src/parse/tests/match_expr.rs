//! Inductive and matrix matches, and patterns.

use {crate::*, curios_common::Plicity, curios_core::Numeral, curios_num::Natural};

#[test]
fn inductive_match_nullary_and_unary() {
    assert_eq!(
        "match v\n| null() => \"null\"\n| bool_(b) => b\nend"
            .parse::<Term>()
            .unwrap(),
        Subterm::Match(Match {
            head: Subterm::Name(Name::from(["v".to_string()])).into(),
            arms: vec![
                MatrixArm::of(
                    MatchPattern::Variant {
                        tag: "null".into(),
                        args: vec![],
                    },
                    Subterm::ProofLiteral(ProofLiteral::Str(StrLit::line("null"))).into()
                ),
                MatrixArm::of(
                    MatchPattern::Variant {
                        tag: "bool_".into(),
                        args: vec![(Plicity::Explicit, MatchPattern::Binder("b".into()))],
                    },
                    Subterm::Name(Name::from(["b".to_string()])).into()
                ),
            ],
        })
        .into()
    );
}

#[test]
fn inductive_match_multi_binder() {
    assert_eq!(
        "match v\n| lit(a, b) => a\nend".parse::<Term>().unwrap(),
        Subterm::Match(Match {
            head: Subterm::Name(Name::from(["v".to_string()])).into(),
            arms: vec![MatrixArm::of(
                MatchPattern::Variant {
                    tag: "lit".into(),
                    args: vec![
                        (Plicity::Explicit, MatchPattern::Binder("a".into())),
                        (Plicity::Explicit, MatchPattern::Binder("b".into())),
                    ],
                },
                Subterm::Name(Name::from(["a".to_string()])).into()
            )],
        })
        .into()
    );
}

#[test]
fn pattern_binders_round_trip() {
    // Tuple/struct destructuring patterns at `let`, lambda-parameter, and function-definition-sugar-parameter position: plain names still round-trip unchanged, and compound patterns (nested, field-punned, or mixed with plain-name parameters) survive print → re-parse.
    for source in [
        "let x = pair; x",                        // plain name, unchanged
        "let (x, y) = pair; x",                   // tuple pattern
        "let Point { x, y } = p; x",              // struct pattern, field-punned
        "let Point { x, w = ww } = p; x",         // punned + explicit label mixed
        "let Point { loc = (x, y) } = p; x",      // tuple nested in struct
        "let (Point { x, y }, z) = pair; x",      // struct nested in tuple
        "((x, y) : Point) => x",                  // tuple-pattern lambda param
        "(Point { x, y } : Point) => x",          // struct-pattern lambda param
        "((x, y) : Point, z : Nat) => x",         // mixed pattern/plain-name params
        "let f((x, y) : Point) -> Nat = x; f(p)", // tuple-pattern func-sugar param
        // Function-definition-sugar parameters always require an explicit `: T` annotation (unlike lambda parameters); a struct pattern's head name is descriptive only, never load-bearing as a type.
        "let f((x, y) : Point, Point { z, w = ww } : Point) -> Nat = x + y + z + ww; f(p, q)",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.printed().to_string().parse::<Term>().unwrap(),
            term,
            "term round-trip failed for {source:?}"
        );
    }
}

/// A `let`'s struct pattern left open is reported as the pattern it is: once `Name {` follows `let`, nothing but a pattern can stand there, so the refusal is not handed to the signature a plain binder `Name` would have opened. Where a term may hold a struct literal under the same prefix, the pattern still backtracks.
#[test]
fn an_unclosed_struct_pattern_after_let_is_reported_as_one() {
    let report = "let Pt { x, y = p; x".parse::<Term>().unwrap_err().format();
    assert!(
        report.contains("a struct pattern closes with `}`"),
        "reported {report}"
    );

    for source in [
        "(Pt { x = 1, y = 2 })",
        "f(Pt { x = 1, y = 2 })",
        "(base = 3, bump(x) = Pt { x = x, y = 2 })",
    ] {
        assert!(source.parse::<Term>().is_ok(), "{source}");
    }
}

#[test]
fn inductive_match_round_trips() {
    // Constructor-arm rows survive print → re-parse: distinct tags, a nullary `nil()`, and a wildcard payload binder.
    for source in [
        "match xs | cons(x, xs) => x | nil() => y end",
        "match xs | cons(x, _) => x | nil() => y end",
        "match xs | cons(a, b) => a | nil() => y end",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.printed().to_string().parse::<Term>().unwrap(),
            term,
            "match arm round-trip failed for {source:?}"
        );
    }
}

#[test]
fn matrix_match_round_trips() {
    // Nested/tuple/struct match-arm patterns — the matrix pattern compiler's grammar — survive print → re-parse, a single tupled head included.
    for source in [
        // A constructor nested inside another constructor's payload.
        "match x | some(some(y)) => y | some(none()) => y | none() => y end",
        // A tuple sub-pattern nested inside a constructor's payload.
        "match x | some((a, b)) => a | none() => a end",
        // A struct sub-pattern nested inside a constructor's payload, including field-punning.
        "match x | some(Point { a, b }) => a | none() => a end",
        // A mixed row: one argument concrete, the other a plain binder.
        "match x | pair(some(a), b) => a | pair(none(), b) => b end",
        // A tuple value as the match target directly (no constructor tag at all), and a struct value likewise — the "structs/tuples as match targets" feature.
        "match p | (a, b) => a end",
        "match p | Point { a, b } => a end",
        // The spec's own motivating example: a single tupled head, four fully-enumerated rows over two independent `Option`-shaped columns.
        "match p\n\
         | (some(x), some(y)) => f(x, y)\n\
         | (some(x), none()) => g(x)\n\
         | (none(), some(y)) => h(y)\n\
         | (none(), none()) => d\n\
         end",
        // Nat literal leaves nested inside a constructor payload.
        "match o | some(0) => y | some(n + 1) => n | none() => y end",
        // List literal leaves nested inside a tuple field.
        "match p | (x, []) => x | (x, [h, ..t]) => h end",
        // Bytes literal leaves nested inside a constructor payload.
        "match o | some(0x[]) => y | some(0x[h, ..t]) => y | none() => y end",
        // Bool literal leaves nested inside a constructor payload.
        "match p | pair(true, y) => y | pair(false, y) => y end",
        // The four hardcoded carriers as *headed* matches — no separate surface variants, just matrices over that carrier's own leaves. Each must survive print → re-parse identically to prove the collapse preserves their surface syntax.
        "match b | false => x | true => y end",
        "match n | 0 => a | m + 1 => b end",
        // Nat literal dispatch: literal cases and the mandatory `| _ =>` default.
        "match d | 0 => a | 5 => b | _ => c end",
        "match a | [] => b | [h, ..t] => c end",
        "match a | 0x[] => b | 0x[h, ..t] => c end",
        // Runs: closed and open, their elements patterns of their own, under both brackets.
        "match a | [x] => x | [none(), y, ..rest] => y | [_, _] => z | _ => w end",
        "match a | 0x[h, _, ..t] => h | 0x[p, q] => p | _ => c end",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.printed().to_string().parse::<Term>().unwrap(),
            term,
            "matrix match round-trip failed for {source:?}"
        );
    }
}

// A nested `Nat` succ pattern requires a space on each side of `+` (mirrors `infix_requires_spaces_and_disambiguates_signs`'s own operator spacing rule). A glued `n+1` is not recognized as `MatchPattern::Successor` at all — it falls through to a plain `Binder("n")`, leaving `+1` as trailing garbage the arm grammar rejects, a parse error rather than a silent reinterpretation.
#[test]
fn matrix_match_nat_succ_pattern_requires_spaces_around_plus() {
    assert!(
        "match o | some(n + 1) => n | some(0) => n | none() => n end"
            .parse::<Term>()
            .is_ok()
    );
    // Glued on either side, the refusal names the rule rather than the `+1` the arm grammar did not expect, with the caret on the `+`.
    for source in [
        "match o | some(n+1) => n | some(0) => n | none() => n end",
        "match n | 0 => 0 | p +1 => p end",
    ] {
        let report = source.parse::<Term>().unwrap_err().format();
        assert!(
            report.contains("a successor pattern takes whitespace on both sides of its `+`"),
            "{source:?} reported {report}"
        );
        assert!(
            !report.contains("expected: `=>`"),
            "{source:?} reported {report}"
        );
    }
}

// A spaced successor pattern takes a step of one or more, read whole, so `10` is a step of ten, and printed back in the radix it was written in.
#[test]
fn a_successor_pattern_takes_a_step_of_one_or_more() {
    for source in [
        "match n | 0 => 10 | 1 => 11 | k + 2 => k end",
        "match n | 0 => 0 | p + 10 => p end",
        "match n | 0 => 0 | p + 0x10 => p end",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.printed().to_string().parse::<Term>().unwrap(),
            term,
            "{source:?} printed back as {}",
            term.printed()
        );
    }
}

// A step of none, which is the binder itself, and a step that is no numeral are refused at the step, and not as a spacing they have.
#[test]
fn a_successor_pattern_of_no_step_is_refused_at_its_step() {
    for (source, refusal) in [
        (
            "match n | 0 => 0 | p + 0 => p end",
            "`p + 0` is the binder `p`",
        ),
        (
            "match n | 0 => 0 | p + q => p end",
            "a successor pattern's step is a numeral",
        ),
    ] {
        let report = source.parse::<Term>().unwrap_err().format();
        assert!(report.contains(refusal), "{source:?} reported {report}");
        assert!(
            !report.contains("whitespace"),
            "{source:?} reported {report}"
        );
    }
}

/// A constructor pattern names its constructor bare, and the refusal says so rather than blaming the token the fall-through reached.
///
/// The tag is resolved against the scrutinee's type, so the namespace is never spelled. Written anyway, it is refused for that, rather than the head parsing as a `Binder` and the arm reporting `expected: `=>`` against the `/` — with a `=>` plainly written further along the same line.
#[test]
fn a_qualified_constructor_pattern_is_refused_by_name() {
    for source in [
        "match o | Option/some(n) => n | none() => 0 end",
        "match o | some(Option/some(n)) => n | _ => 0 end",
        "match o | /std/Option/some(n) => n | _ => 0 end",
        "match o | Option/none => 0 | _ => 1 end",
    ] {
        let report = source.parse::<Term>().unwrap_err().format();
        assert!(
            report.contains("names its constructor bare"),
            "{source:?} reported {report}"
        );
    }
}

/// A numeral is a pattern with its sign and its radix, as the term it spells is a numeral, at the top of an arm and inside a payload, and each prints back as written; what takes it apart is its column's to say.
#[test]
fn a_numeral_pattern_keeps_its_sign_and_radix() {
    for source in [
        "match i | -1 => 0 | +1 => 2 | 3 => 4 | _ => 1 end",
        "match o | some(-0x10) => 0 | some(+0) => 2 | _ => 1 end",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.printed().to_string().parse::<Term>().unwrap(),
            term,
            "{source:?} printed back as {}",
            term.printed()
        );
    }
}

/// A floating-point literal takes nothing apart, and is refused by what it is, where it starts, with the guard that tests it — rather than as the binder the fall-through fails to read at its first character, the natural `1.5` opens with, or the constructor a payload written as one is read as the binder of.
#[test]
fn a_floating_point_pattern_is_refused_by_name_where_it_starts() {
    for (source, literal) in [
        ("match x | 1.5 => 0 | _ => 1 end", "1.5"),
        ("match x | -inf.0 => 0 | _ => 1 end", "-inf.0"),
        ("match o | some(+2.5) => 0 | _ => 1 end", "+2.5"),
    ] {
        let error = source.parse::<Term>().unwrap_err();
        let report = error.format();
        assert!(
            report.contains("a floating-point literal is no pattern"),
            "{source:?} reported {report}"
        );
        assert!(
            report.contains("bind the value and test it in a guard, by the comparison you mean"),
            "{source:?} reported {report}"
        );
        let column = source.find(literal).expect("the literal is written") + 1;
        assert_eq!(
            error.report().span().expect("located").line_column(),
            (1, column),
            "{source:?} reported {report}"
        );
    }
}

/// A string literal is a pattern, at the top of an arm and inside a constructor's payload, in a run's element and in either spelling, and each prints back as written.
#[test]
fn a_string_literal_is_a_pattern_printed_as_written() {
    for source in [
        r#"match name | "source" => 0 | "" => 1 | _ => 2 end"#,
        r#"match o | some("x") => 0 | _ => 1 end"#,
        r#"match args | ["--help", ..rest] => 0 | _ => 1 end"#,
        r#"match s | "a\tb\"c" => 0 | _ => 1 end"#,
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.printed().to_string(),
            source,
            "{source:?} printed back"
        );
    }

    let block = "match s\n| \"\"\"\n    two\n    lines\n    \"\"\" => 0\n| _ => 1\nend";
    let term = block.parse::<Term>().unwrap();
    let Subterm::Match(matched) = &*term else {
        panic!("a match");
    };
    assert_eq!(
        matched.arms[0].alternatives[0].pattern,
        MatchPattern::Str(StrLit::block("two\nlines"))
    );
}

/// The one match pattern that may carry a path keeps it: a struct head is kept as written, for the lowering to resolve.
#[test]
fn a_struct_match_pattern_keeps_its_qualified_head() {
    assert!(
        "match p | Whatever/P { x, y } => x | _ => y end"
            .parse::<Term>()
            .is_ok()
    );
}

/// A run's rest is a binder and stands last, after an element, so a pattern in its place and a rest before an element each fail where they are written: the run commits at its bracket, and its rest at its `..`.
#[test]
fn a_rest_that_is_no_binder_or_leads_its_run_fails_where_it_is_written() {
    for (source, place) in [
        ("match v | [head, ..some(t)] => 0 | _ => 1 end", "1:24"),
        ("match v | [a, ..[]] => 0 | _ => 1 end", "1:17"),
        ("match v | 0x[h, ..0x[]] => 0 | _ => 1 end", "1:21"),
        ("match v | [..t] => 0 | _ => 1 end", "1:12"),
        ("match v | [..t, a] => 0 | _ => 1 end", "1:12"),
    ] {
        let report = source.parse::<Term>().unwrap_err().format();
        assert!(
            report.contains(&format!("[error] {place}")),
            "{source:?} reported {report}"
        );
    }
}

/// Every run parses: the empty ones, closed and open runs of binders, of `_` and of patterns, under both brackets, with one trailing comma.
#[test]
fn every_run_parses() {
    for source in [
        "match v | [] => 0 | [head, ..tail] => 1 end",
        "match v | 0x[] => 0 | 0x[head, ..tail] => 1 end",
        "match v | [] => 0 | [_, .._] => 1 end",
        "match v | 0x[] => 0 | 0x[ head , ..tail ] => 1 end",
        "match v | some([head, ..tail]) => 1 | _ => 0 end",
        "match v | [a] => 0 | [a, b, ..t] => 1 | _ => 2 end",
        "match v | some([some(x), ..tail]) => 0 | _ => 1 end",
        "match v | [5, ..tail] => 0 | _ => 1 end",
        "match v | 0x[0x01, ..tail] => 0 | _ => 1 end",
        "match v | [a, b,] => 0 | [a, ..t,] => 1 | _ => 2 end",
    ] {
        if let Err(error) = source.parse::<Term>() {
            panic!("{source}\n{}", error.format());
        }
    }
}

/// A scrutinee that is a structure literal holds terms in its braces, so one beginning with a qualified call is no pattern that went wrong: the probe for a `match` written with braces keeps a pattern's refusal to itself.
#[test]
fn a_structure_literal_scrutinee_still_calls_a_qualified_name() {
    if let Err(error) = "match Wrap { Option/some(7) } | w => w end".parse::<Term>() {
        panic!("{}", error.format());
    }
}

/// A dispatch case keeps its numeral whole, however wide it is: the width the erased carrier has is chosen at the erase boundary, not here.
///
/// Narrowed to `u32` in the parser, a wide case would not be refused as one: the failure backtracks past every `Nat` leaf, and the arm is reported as holding no pattern.
#[test]
fn a_dispatch_case_keeps_a_numeral_wider_than_the_erased_carrier() {
    let term = "match n | 4294967296 => 1 | _ => 0 end"
        .parse::<Term>()
        .expect("a wide dispatch case parses");
    let Subterm::Match(matched) = term.as_subterm() else {
        panic!("expected a match, got {}", term.printed());
    };
    let [first, ..] = matched.arms.as_slice() else {
        panic!("expected arms");
    };
    assert_eq!(
        first.alternatives[0].pattern,
        MatchPattern::Numeral(NumLit {
            value: Numeral::Natural(
                Natural::parse_bytes(b"4294967296", 10).expect("a decimal numeral")
            ),
            radix: Radix::Dec(10),
        })
    );
}

/// A match-arm binder is spanned over its word, inside a constructor pattern as at the top of an arm.
#[test]
fn a_match_binder_spans_its_word_alone() {
    let term = "match v | some( x ) => x | none() => 0 end"
        .parse::<Term>()
        .unwrap();
    let Subterm::Match(matched) = &*term else {
        panic!("a match");
    };
    let MatchPattern::Variant { args, .. } = &matched.arms[0].alternatives[0].pattern else {
        panic!("a constructor pattern");
    };
    let MatchPattern::Binder(x) = &args[0].1 else {
        panic!("a binder argument");
    };
    assert_eq!(crate::parse::test_support::spelled(x), "x");
}

/// `end` closes a `match`, headed or not, so either is an atom: an operator, a call, a projection and a `!` take one bare, where a `let` or a lambda, which extend to the end of the enclosing term, still need parentheses.
#[test]
fn a_match_takes_operators_and_suffixes_bare() {
    let sum = "1 + match b | true => 1 | false => 0 end"
        .parse::<Term>()
        .expect("a match is an operand");
    let Subterm::Infix(infix) = sum.as_subterm() else {
        panic!("expected a sum, got {}", sum.printed());
    };
    assert!(matches!(infix.right.as_subterm(), Subterm::Match(_)));

    let call = "match b | true => f | false => g end(x)"
        .parse::<Term>()
        .expect("a match is a callee");
    let Subterm::Apply(apply) = call.as_subterm() else {
        panic!("expected a call, got {}", call.printed());
    };
    assert!(matches!(apply.head.as_subterm(), Subterm::Match(_)));

    let performed = "match | c => a | _ => b end!"
        .parse::<Term>()
        .expect("a match with no head is an action");
    let Subterm::Bang(action) = performed.as_subterm() else {
        panic!("expected a bang, got {}", performed.printed());
    };
    assert!(matches!(action.as_subterm(), Subterm::HeadlessMatch(_)));
}

/// An arm writes its alternatives each after a bar, up to the one arrow they share; a term has no bare `|`, so past a pattern a bar is the next alternative.
#[test]
fn an_arm_writes_each_alternative_after_a_bar() {
    let term = "match c | 'a' | some(x) | (y, true) => 1 | _ => 0 end"
        .parse::<Term>()
        .unwrap();
    let Subterm::Match(matched) = term.as_subterm() else {
        panic!("expected a match, got {}", term.printed());
    };
    let patterns = matched.arms[0]
        .alternatives
        .iter()
        .map(|alternative| alternative.pattern.clone())
        .collect::<Vec<_>>();

    assert_eq!(
        patterns,
        [
            MatchPattern::Char('a'.into()),
            MatchPattern::Variant {
                tag: "some".into(),
                args: vec![(Plicity::Explicit, MatchPattern::Binder("x".into()))],
            },
            MatchPattern::Tuple(vec![
                MatchPatternField {
                    label: None,
                    value: MatchPattern::Binder("y".into()),
                },
                MatchPatternField {
                    label: None,
                    value: MatchPattern::Bool(true),
                },
            ]),
        ]
    );
    assert_eq!(matched.arms[1].alternatives.len(), 1);
}

/// A pattern holds no alternatives: a bar inside one is refused by name where it stands, in each place a pattern nests, rather than reported as the token the enclosing form did not expect.
#[test]
fn an_alternative_inside_a_pattern_is_refused_by_name() {
    for source in [
        "match o | some(0 | 1) => 1 | _ => 0 end",
        "match p | (0 | 1, x) => x | _ => 0 end",
        "match p | Point { x = 0 | 1, y } => y | _ => 0 end",
        "match o | (some(0) | none()) => 1 | _ => 0 end",
    ] {
        let report = source.parse::<Term>().unwrap_err().format();
        assert!(
            report.contains("alternatives stand at the top of an arm"),
            "{source:?} reported {report}"
        );
    }
}

#[test]
fn an_arm_of_alternatives_round_trips() {
    for source in [
        "match c | ' ' | '\\t' => true | _ => false end",
        "match o | some(x) | other(_, x) => x | _ => 0 end",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(term.printed().to_string(), source);
    }
}
