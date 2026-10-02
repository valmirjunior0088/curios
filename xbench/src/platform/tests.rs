use super::*;

const CPUINFO: &str = "\
processor\t: 0
vendor_id\t: AuthenticAMD
model name\t: AMD Ryzen 7 5800X3D 8-Core Processor
microcode\t: 0xa201213
processor\t: 1
model name\t: AMD Ryzen 7 5800X3D 8-Core Processor
";

#[test]
fn a_field_is_read_from_the_first_processor_that_states_it() {
    assert_eq!(
        field(CPUINFO, "model name"),
        "AMD Ryzen 7 5800X3D 8-Core Processor"
    );
    assert_eq!(field(CPUINFO, "microcode"), "0xa201213");
}

#[test]
fn a_field_the_host_does_not_state_is_spelled_rather_than_dropped() {
    assert_eq!(field(CPUINFO, "flags"), UNKNOWN);
    assert_eq!(field("", "model name"), UNKNOWN);
}

#[test]
fn a_field_name_matches_whole_and_not_as_a_prefix() {
    assert_eq!(field(CPUINFO, "model"), UNKNOWN);
}

#[test]
fn total_memory_is_read_as_gibibytes() {
    assert_eq!(
        gibibytes("MemTotal:       32793108 kB\nMemFree:  1 kB\n"),
        "31.3 GiB"
    );
}

#[test]
fn memory_in_a_unit_the_parser_does_not_know_is_not_guessed_at() {
    assert_eq!(gibibytes("MemTotal:       32793108 MB\n"), UNKNOWN);
    assert_eq!(gibibytes(""), UNKNOWN);
}

/// The two drivers spell boosting oppositely, and a reading that recorded either raw would compare two machines by a number meaning the reverse on each.
#[test]
fn both_drivers_are_read_as_the_same_two_words() {
    assert_eq!(boosting(Some("1\n"), None), "boost on");
    assert_eq!(boosting(Some("0\n"), None), "boost off");
    assert_eq!(boosting(None, Some("0\n")), "boost on");
    assert_eq!(boosting(None, Some("1\n")), "boost off");
}

#[test]
fn the_cpufreq_driver_answers_where_a_host_exposes_both() {
    assert_eq!(boosting(Some("0\n"), Some("0\n")), "boost off");
}

#[test]
fn a_host_with_neither_driver_says_so() {
    assert_eq!(boosting(None, None), "boost unknown");
}

/// Every reading of one machine must capture one `machine` and one `state`, or the comparability gate would refuse a reading against itself.
#[test]
fn capturing_twice_answers_the_same_lines() {
    assert_eq!(capture(), capture());
}

#[test]
fn every_captured_line_says_something() {
    let captured = capture();

    assert!(!captured.machine.is_empty());
    assert!(!captured.state.is_empty());
    assert!(!captured.software.is_empty());
}
