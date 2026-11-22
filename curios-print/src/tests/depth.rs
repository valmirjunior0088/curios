//! A document of any depth printed, measured, walked and freed without recursing.

use {super::test_support::*, crate::*, std::fmt};

/// A document nests as deep as the term it prints, and both walks over it — printing and freeing — must survive that. Depth is not steps, so no reduction budget bounds either one; only an explicit stack does.
fn nested(depth: usize) -> Printer {
    let mut document = pure("x");
    for _ in 0..depth {
        document = indent(flat([pure("("), document, pure(")")]));
    }
    document
}

/// The layout variants must survive the same depth as the rest of the algebra, in all three walks: drop, unbounded print, and the measuring print. No `indent` here — a broken 100k-deep document would otherwise write quadratically many indentation spaces, and depth is what this exercises, not volume.
fn nested_groups(depth: usize) -> Printer {
    let mut document = pure("x");
    for _ in 0..depth {
        document = group(flat([pure("("), soft_line(), document, pure(")")]));
    }
    document
}

#[test]
fn a_deep_document_is_freed_without_recursing() {
    drop(nested(100_000));
}

#[test]
fn a_deep_document_is_printed_without_recursing() {
    struct Deep;
    impl fmt::Display for Deep {
        fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            run_printer(nested(100_000), formatter, 2)
        }
    }

    let printed = Deep.to_string();

    assert_eq!(printed.matches('(').count(), 100_000);
    assert_eq!(printed.matches(')').count(), 100_000);
}

#[test]
fn a_deep_grouped_document_is_freed_without_recursing() {
    drop(nested_groups(100_000));
}

#[test]
fn a_deep_grouped_document_is_printed_and_measured_without_recursing() {
    let printed = render(nested_groups(100_000), Some(30));
    assert_eq!(printed.matches('(').count(), 100_000);
}

#[test]
fn a_deep_flow_is_walked_and_freed_without_recursing() {
    let nested = || {
        let mut document = named("x");
        for _ in 0..100_000 {
            document = flow([FillItem {
                printer: document,
                compact: false,
            }]);
        }
        document
    };
    drop(nested());
    let mut document = nested();
    each_named(&mut document, |text, _| *text = "y".to_string());
    assert_eq!(render(document, None), "y");
}
