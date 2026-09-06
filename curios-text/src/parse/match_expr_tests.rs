//! Inductive and matrix matches, motives, patterns, and `choose`.

use {crate::*, curios_num::Natural, curios_utilities::Plicity};

use super::test_support::*;
use curios_utilities::Sign;

#[test]
fn inductive_match_nullary_and_unary() {
    assert_eq!(
        "match v : Bin\n| null() => \"null\"\n| bool_(b) => b\nend"
            .parse::<Term>()
            .unwrap(),
        Subterm::Match(Match {
            head: Subterm::Name(Name::from(["v".to_string()])).into(),
            motive: Some(Subterm::Name(Name::from(["Bin".to_string()])).into()),
            arms: vec![
                MatrixArm {
                    pattern: MatchPattern::Variant {
                        tag: "null".to_string(),
                        args: vec![],
                    },
                    body: Subterm::Syn(Syn::Str(StrLit::line("null"))).into(),
                },
                MatrixArm {
                    pattern: MatchPattern::Variant {
                        tag: "bool_".to_string(),
                        args: vec![(Plicity::Explicit, MatchPattern::Binder("b".into()))],
                    },
                    body: Subterm::Name(Name::from(["b".to_string()])).into(),
                },
            ],
        })
        .into()
    );
}

#[test]
fn inductive_match_multi_binder() {
    assert_eq!(
        "match v : T\n| lit(a, b) => a\nend"
            .parse::<Term>()
            .unwrap(),
        Subterm::Match(Match {
            head: Subterm::Name(Name::from(["v".to_string()])).into(),
            motive: Some(Subterm::Name(Name::from(["T".to_string()])).into()),
            arms: vec![MatrixArm {
                pattern: MatchPattern::Variant {
                    tag: "lit".to_string(),
                    args: vec![
                        (Plicity::Explicit, MatchPattern::Binder("a".into())),
                        (Plicity::Explicit, MatchPattern::Binder("b".into())),
                    ],
                },
                body: Subterm::Name(Name::from(["a".to_string()])).into(),
            }],
        })
        .into()
    );
}

#[test]
fn match_omitted_motive() {
    // Dropping the `: motive` clause entirely yields `motive: None`; the elaborator later lowers it to a fresh metavariable (sugar for `: _`).
    assert_eq!(
        "match x | foo(y) => y end".parse::<Term>().unwrap(),
        Subterm::Match(Match {
            head: Subterm::Name(Name::from(["x".to_string()])).into(),
            motive: None,
            arms: vec![MatrixArm {
                pattern: MatchPattern::Variant {
                    tag: "foo".to_string(),
                    args: vec![(Plicity::Explicit, MatchPattern::Binder("y".into()))],
                },
                body: Subterm::Name(Name::from(["y".to_string()])).into(),
            }],
        })
        .into()
    );
}

#[test]
fn omitted_motive_round_trips() {
    let term = "match x | foo(y) => y end".parse::<Term>().unwrap();
    let printed = term.to_string();
    // An omitted motive prints back without the `: …` clause …
    assert!(!printed.contains(" : "));
    // … and re-parses to the same tree.
    assert_eq!(printed.parse::<Term>().unwrap(), term);
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
            term.to_string().parse::<Term>().unwrap(),
            term,
            "term round-trip failed for {source:?}"
        );
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
            term.to_string().parse::<Term>().unwrap(),
            term,
            "match arm round-trip failed for {source:?}"
        );
    }
}

#[test]
fn matrix_match_round_trips() {
    // Nested/tuple/struct match-arm patterns — the matrix pattern compiler's grammar — survive print → re-parse, including the spec's own motivating example (a single tupled head).
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
        "match p : R\n\
         | (some(x), some(y)) => f(x, y)\n\
         | (some(x), none()) => g(x)\n\
         | (none(), some(y)) => h(y)\n\
         | (none(), none()) => d\n\
         end",
        // Nat literal leaves nested inside a constructor payload, with and without the optional induction hypothesis.
        "match o | some(0) => y | some(n + 1; ih) => y | none() => y end",
        "match o | some(0) => y | some(n + 1) => n | none() => y end",
        // List literal leaves nested inside a tuple field, with and without the optional induction hypothesis.
        "match p | (x, []) => x | (x, [h, ..t]) => h end",
        "match p | (x, [h, ..t]; ih) => h | (x, []) => x end",
        // Bits and Bytes literal leaves nested inside a constructor payload.
        "match o | some(x[]) => y | some(x[h, ..t]) => y | none() => y end",
        "match o | some(b[h, ..t]; ih) => y | some(b[]) => y | none() => y end",
        // Bool literal leaves nested inside a constructor payload.
        "match p | pair(true, y) => y | pair(false, y) => y end",
        // The four hardcoded carriers as *headed* matches — no longer separate surface variants, just matrices over that carrier's own leaves. Each must survive print → re-parse identically to prove the collapse preserves their surface syntax.
        "match b | false => x | true => y end",
        "match n | 0 => a | m + 1; ih => b end",
        "match n | 0 => a | m + 1 => b end",
        // Nat literal dispatch (the old `NatMatch::Dispatch`): literal cases and the mandatory `| _ =>` default.
        "match d | 0 => a | 5 => b | _ => c end",
        "match a | [] => b | [h, ..t]; ih => c end",
        "match a | x[] => b | x[h, ..t]; ih => c end",
        "match a | b[] => b | b[h, ..t]; ih => c end",
        // The `;` fold-hypothesis position takes any irrefutable pattern — a destructuring binds the fold result's fields directly, plain names round-tripping as before.
        "match n | 0 => a | m + 1; (count, live) => count end",
        "match a | [] => b | [h, ..t]; (x, (y, z)) => x end",
        "match a | x[] => b | x[h, ..t]; (seen = s, rest = r) => s end",
        "match a | b[] => b | b[h, ..t]; (x, y) => x end",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.to_string().parse::<Term>().unwrap(),
            term,
            "matrix match round-trip failed for {source:?}"
        );
    }
}

// The motive is an ordinary term, so every spelling it can take must survive print → re-parse with arms following it. There is no motive grammar and no backtracking: a motive term always terminates at the first `|`, because `|` is not an infix operator.
#[test]
fn match_motive_spellings_round_trip() {
    for source in [
        // Binders only, one per index then the scrutinee.
        "match p : (s, t, q) => Eq(t, s)\n| refl(z) => e\nend",
        // Written binder annotations, including one naming earlier binders.
        "match p : (s : A, t : A, q : Eq(s, t)) => Eq(t, s)\n| refl(z) => e\nend",
        // A constant motive: a lambda whose binders are all `_`.
        "match v : (_, _) => Nat\n| nil() => a\n| cons(m, x, xs) => b\nend",
        // A motive naming a top-level family, eta-expanded by elaboration.
        "match p : discriminates_eq\n| refl(z) => e\nend",
        // A motive whose body is itself a Π type — the shape that made the old constant rung undecidable by shape.
        "match b : (_) => (Nat) -> Nat\n| true => f\n| false => g\nend",
        // A motive on each hardcoded carrier, whose arity is 1 throughout.
        "match n : (m) => P(m)\n| 0 => a\n| p + 1; ih => b\nend",
        "match xs : (l) => P(l)\n| [] => a\n| [h, ..t]; ih => b\nend",
        // Omitted entirely: no `:` at all, faithfully recorded as `None`.
        "match v\n| nil() => a\n| cons(m, x, xs) => b\nend",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.to_string().parse::<Term>().unwrap(),
            term,
            "motive round-trip failed for {source:?}"
        );
    }
}

// `choose`: no head term, `Bool` condition arms, and a mandatory `| _ =>` default. A bare `_` condition parses as the default (not a `Name` condition arm) — the `flat_map` guard in `parse_cond_arm` sees it and lets `many0` stop.
#[test]
fn parse_choose() {
    assert_eq!(
        "choose\n| p => 1\n| q => 2\n| _ => 3\nend"
            .parse::<Term>()
            .unwrap(),
        Subterm::Choose(Choose {
            arms: vec![
                cond_arm(name("p"), num_lit(1, Sign::Unmarked)),
                cond_arm(name("q"), num_lit(2, Sign::Unmarked)),
            ],
            default: num_lit(3, Sign::Unmarked),
        })
        .into()
    );
}

// An arm-free choose is legal: `choose | _ => d end` is just its default.
#[test]
fn a_choose_of_only_a_default_parses() {
    assert_eq!(
        "choose\n| _ => 0\nend".parse::<Term>().unwrap(),
        Subterm::Choose(Choose {
            arms: vec![],
            default: num_lit(0, Sign::Unmarked),
        })
        .into()
    );
}

// A condition whose head merely *begins* with `_` (`_ready`) is an ordinary condition, not the default — the guard rejects only a lone `_`.
#[test]
fn choose_leading_underscore_condition() {
    assert_eq!(
        "choose\n| _ready => 1\n| _ => 2\nend"
            .parse::<Term>()
            .unwrap(),
        Subterm::Choose(Choose {
            arms: vec![cond_arm(name("_ready"), num_lit(1, Sign::Unmarked))],
            default: num_lit(2, Sign::Unmarked),
        })
        .into()
    );
}

// A bind arm `| pattern = value =>` parses as a `ChooseTest::Bind`; a condition arm sharing a `|` with it stays a `Cond`.
#[test]
fn choose_bind_arm() {
    assert_eq!(
        "choose\n| some(x) = o => x\n| ready => 1\n| _ => 2\nend"
            .parse::<Term>()
            .unwrap(),
        Subterm::Choose(Choose {
            arms: vec![
                ChooseArm {
                    test: ChooseTest::Bind {
                        pattern: MatchPattern::Variant {
                            tag: "some".to_string(),
                            args: vec![(Plicity::Explicit, MatchPattern::Binder("x".into()))],
                        },
                        value: name("o"),
                    },
                    body: name("x"),
                },
                cond_arm(name("ready"), num_lit(1, Sign::Unmarked)),
            ],
            default: num_lit(2, Sign::Unmarked),
        })
        .into()
    );
}

// `choose` survives print → re-parse, including the arm-free form.
#[test]
fn choose_round_trips() {
    for source in [
        "choose | p => 1 | q => 2 | _ => 3 end",
        "choose | _ => 0 end",
        "choose | a <= b => x | _ => y end",
        // Bind arms, mixed with a condition arm.
        "choose | some(x) = o => x | _ => y end",
        "choose | cons(h, t) = xs => h | ready => 1 | _ => 0 end",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.to_string().parse::<Term>().unwrap(),
            term,
            "choose round-trip failed for {source:?}"
        );
    }
}

// A nested `Nat` succ pattern requires a space on each side of `+` (mirrors `infix_requires_spaces_and_disambiguates_signs`'s own operator spacing rule). A glued `n+1` is not recognized as `NatPattern::Succ` at all — it falls through to a plain `Binder("n")`, leaving `+1; ih` as trailing garbage the arm grammar rejects, a parse error rather than a silent reinterpretation.
#[test]
fn matrix_match_nat_succ_pattern_requires_spaces_around_plus() {
    assert!(
        "match o | some(n + 1; ih) => n | some(0) => n | none() => n end"
            .parse::<Term>()
            .is_ok()
    );
    // Glued on either side, the refusal names the rule rather than the `+1` the arm grammar did not expect, with the caret on the `+`.
    for source in [
        "match o | some(n+1; ih) => n | some(0) => n | none() => n end",
        "match n | 0 => 0 | p +1 => p end",
    ] {
        let report = source.parse::<Term>().unwrap_err().format();
        assert!(
            report.contains("a successor pattern takes whitespace on both sides of its `+`"),
            "{source:?} reported {report}"
        );
        assert!(
            !report.contains("Expected '=>'"),
            "{source:?} reported {report}"
        );
    }
}

/// A constructor pattern names its constructor bare, and the refusal says so rather than blaming the token the fall-through reached.
///
/// The tag is resolved against the scrutinee's type, so the namespace is never spelled. Written anyway, the head used to parse as a `Binder` and the arm reported `Expected '=>'` against the `/` — with a `=>` plainly written further along the same line.
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

/// The one match pattern that may carry a path keeps it: a struct head is documentary, resolved by nothing.
#[test]
fn a_struct_match_pattern_keeps_its_qualified_head() {
    assert!(
        "match p | Whatever/P { x, y } => x | _ => y end"
            .parse::<Term>()
            .is_ok()
    );
}

/// A `choose` condition arm is a term, so one beginning with a qualified call is not a pattern that went wrong.
#[test]
fn a_choose_condition_arm_still_calls_a_qualified_name() {
    assert!(
        "choose | Option/is_some(o) => 1 | _ => 0 end"
            .parse::<Term>()
            .is_ok()
    );
}

/// A dispatch case keeps its numeral whole, however wide it is: the width the erased carrier has is chosen at the erase boundary, not here.
///
/// Narrowing to `u32` in the parser did not merely refuse a wide case early — the failure backtracked, and a digit run *is* an identifier, so the arm fell past every `Nat` leaf to a plain `Binder`. The match then dispatched on nothing and took its one arm for every input.
#[test]
fn a_dispatch_case_keeps_a_numeral_wider_than_the_erased_carrier() {
    let term = "match n | 4294967296 => 1 | _ => 0 end"
        .parse::<Term>()
        .expect("a wide dispatch case parses");
    let Subterm::Match(matched) = term.as_subterm() else {
        panic!("expected a match, got {term}");
    };
    let [first, ..] = matched.arms.as_slice() else {
        panic!("expected arms");
    };
    assert_eq!(
        first.pattern,
        MatchPattern::Nat(NatPattern::Lit(
            Natural::parse_bytes(b"4294967296", 10).expect("a decimal numeral")
        ))
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
    let MatchPattern::Variant { args, .. } = &matched.arms[0].pattern else {
        panic!("a constructor pattern");
    };
    let MatchPattern::Binder(x) = &args[0].1 else {
        panic!("a binder argument");
    };
    assert_eq!(super::test_support::spelled(x), "x");
}
