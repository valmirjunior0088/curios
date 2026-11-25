use super::*;

/// What a measurement prints is what the runner reads back, a name with a space in it and a figure taken at no size among them.
#[test]
fn a_line_reads_back_as_the_reading_it_printed() {
    for line in [
        Line {
            name: "cps_optimize".to_string(),
            unit: Unit::Microseconds,
            size: None,
            value: 1_909_334,
        },
        Line {
            name: "LetPlus/looks/erase at n".to_string(),
            unit: Unit::Count,
            size: Some(25),
            value: 2,
        },
    ] {
        assert_eq!(Line::read(&line.printed()), Some(Ok(line)));
    }
}

/// A test prints other lines, the harness's own among them, and the runner reads only the record's.
#[test]
fn a_line_without_the_prefix_is_no_reading() {
    assert_eq!(Line::read("running 1 test"), None);
    assert_eq!(Line::read("test tests::fixpoint ... ok"), None);
}

/// A line that opens with the prefix and states no reading refuses, so a misprint fails the sitting rather than dropping a reading unseen.
#[test]
fn a_line_with_the_prefix_and_no_reading_refuses() {
    for text in [
        "xbench: furlongs - 1 span",
        "xbench: microseconds - many span",
        "xbench: microseconds - 1",
        "xbench: microseconds",
    ] {
        assert!(matches!(Line::read(text), Some(Err(_))), "{text}");
    }
}
