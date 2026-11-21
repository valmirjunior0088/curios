//! Guards after an arm's patterns, and a match with no head.

use {
    crate::parse::test_support::*,
    crate::*,
    curios_common::{Plicity, Sign},
};

// An arm writes its guards after its alternatives, each after a comma: a condition, or `pattern = value`.
#[test]
fn an_arm_writes_guards_after_its_patterns() {
    let term = "match o | some(n), n > 1, some(m) = f(n) => m | _ => 0 end"
        .parse::<Term>()
        .unwrap();
    let Subterm::Match(matched) = term.as_subterm() else {
        panic!("expected a match, got {term}");
    };
    let [Guard::Holds(condition), Guard::Binds { pattern, value }] =
        matched.arms[0].guards.as_slice()
    else {
        panic!("expected a condition and a bind, got {term}");
    };

    assert_eq!(condition.to_string(), "n > 1");
    assert_eq!(
        pattern.pattern,
        MatchPattern::Variant {
            tag: "some".into(),
            args: vec![(Plicity::Explicit, MatchPattern::Binder("m".into()))],
        }
    );
    assert_eq!(value.to_string(), "f(n)");
    assert!(matched.arms[1].guards.is_empty());
}

// A bar right after `match` is what tells a match with no head from a headed one: its arms are guards alone, and a default closes them.
#[test]
fn a_match_with_no_head_is_its_guards_and_its_default() {
    let term = "match | p => 1 | some(x) = o, x > 0 => x | _ => 3 end"
        .parse::<Term>()
        .unwrap();
    let Subterm::HeadlessMatch(headless) = term.as_subterm() else {
        panic!("expected a match with no head, got {term}");
    };

    assert_eq!(headless.arms.len(), 2);
    assert_eq!(headless.arms[0].guards, vec![Guard::Holds(name("p"))]);
    assert_eq!(headless.arms[1].guards.len(), 2);
    assert_eq!(headless.default, num_lit(3, Sign::Unmarked));
}

// A bind guard's pattern may hold what no term does, `_`, and its value is an operand, so a tuple of names standing before the arm's `=>` is a tuple and no lambda's parameters.
#[test]
fn a_bind_guard_reads_a_pattern_no_term_spells_and_a_tuple_value() {
    for source in [
        "match | some(_) = o => 0 | _ => 1 end",
        "match | (some(x), some(y)) = (o, p) => x + y | _ => 1 end",
        "match | (some(x), _) = ((o), p) => x | _ => 1 end",
        "match | _ready => 1 | _ => 2 end",
    ] {
        assert!(source.parse::<Term>().is_ok(), "{source:?}");
    }
}

// A condition may begin with a qualified name, which the bind reading would refuse as a constructor pattern: the condition is read first, and no `=` follows it.
#[test]
fn a_condition_may_begin_with_a_qualified_name() {
    for source in [
        "match\n| /std/Nat/lt(a, b) => 1\n| _ => 0\nend",
        "match | Option/is_some(o) => 1 | _ => 0 end",
        "match o | some(n), Nat/lt(n, 2) => 1 | _ => 0 end",
    ] {
        assert!(source.parse::<Term>().is_ok(), "{source:?}");
    }
}

// A match with no head may be its default alone.
#[test]
fn a_match_with_no_head_may_be_its_default_alone() {
    assert_eq!(
        "match\n| _ => 0\nend".parse::<Term>().unwrap(),
        Subterm::HeadlessMatch(HeadlessMatch {
            arms: Vec::new(),
            default: num_lit(0, Sign::Unmarked),
        })
        .into()
    );
}

// A match with no head ends with its default, and one that closes without it is refused by that rule, at the `end` standing in its place.
#[test]
fn a_match_with_no_head_and_no_default_is_refused_by_name() {
    for source in ["match | n > 2 => 1 end", "match | some(x) = o => x end"] {
        let report = source.parse::<Term>().unwrap_err().format();
        assert!(
            report.contains("a `match` with no head ends with a default arm")
                && report.contains("write `| _ => …` before `end`"),
            "{source:?} reported {report}"
        );
        assert!(report.contains(" ^^^\n"), "{source:?} reported {report}");
    }
}

// A malformed guard reports the guard, not a default the reader never wrote: a term followed by another is a missing `=>`, and a bind's pattern the grammar refuses by name is the diagnosis, as a match arm's is.
#[test]
fn a_malformed_guard_reports_the_guard() {
    for (source, expected) in [
        (
            "match\n| some(v) = key v\n| _ => 0\nend",
            "obtained: `v`\n│  expected: `=>`",
        ),
        (
            "match\n| true 1\n| _ => 0\nend",
            "obtained: `1`\n│  expected: `=>`",
        ),
        (
            "match\n| Option/some(v) = key => v\n| _ => 0\nend",
            "a constructor pattern names its constructor bare",
        ),
        (
            "match o\n| some(v), Option/some(w) = v => w\n| _ => 0\nend",
            "a constructor pattern names its constructor bare",
        ),
    ] {
        let report = source.parse::<Term>().unwrap_err().format();
        assert!(report.contains(expected), "{source:?} reported {report}");
    }
}

// A guard takes no alternatives: the grammar writes no bar inside one, so a bar after a guard is refused where it stands, as the arm's missing `=>`.
#[test]
fn a_guard_written_with_alternatives_is_refused_at_its_bar() {
    for source in [
        "match\n| some(0) | some(1) = o => 1\n| _ => 0\nend",
        "match\n| ready, some(0) | some(1) = o => 1\n| _ => 0\nend",
    ] {
        let report = source.parse::<Term>().unwrap_err().format();
        assert!(
            report.contains("obtained: `|`\n│  expected: `=>`"),
            "{source:?} reported {report}"
        );
    }
}

// Guards and a match with no head print back as they are written.
#[test]
fn guards_round_trip() {
    for source in [
        "match o | some(n), n > 1 => n | _ => 0 end",
        "match | p => 1 | some(x) = o, x > 0 => x | _ => 3 end",
        "match | (some(x), some(y)) = (o, p) => x + y | _ => 1 end",
        "match | _ => 0 end",
        "match | a <= b => x | _ => y end",
        "match | cons(h, t) = xs => h | ready => 1 | _ => 0 end",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(term.to_string(), source);
    }
}
