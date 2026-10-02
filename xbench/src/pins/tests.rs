use super::*;

const TOOLCHAIN: &str = "\
[toolchain]
channel = \"1.95.0\"
components = [\"rustfmt\", \"clippy\"]
targets = [\"wasm32-unknown-unknown\", \"wasm32-wasip2\"]
";

const LOCK: &str = "\
[[package]]
name = \"wasmtime-environ\"
version = \"46.0.0\"

[[package]]
name = \"wasmtime\"
version = \"47.0.3\"
source = \"registry+https://github.com/rust-lang/crates.io-index\"
";

#[test]
fn the_channel_is_read_from_the_file_that_pins_it() {
    assert_eq!(channel(TOOLCHAIN).as_deref(), Some("1.95.0"));
}

/// A key the channel's name merely starts would resolve the pin to whatever that key holds.
#[test]
fn a_key_the_channel_only_prefixes_does_not_answer_for_it() {
    assert_eq!(channel("[toolchain]\nchannel-override = \"9.9.9\"\n"), None);
    assert_eq!(channel("[toolchain]\ncomponents = []\n"), None);
}

/// A prefix match would resolve `wasmtime` to whichever of its satellite crates the lock file listed first.
#[test]
fn a_locked_package_is_found_by_its_whole_name() {
    assert_eq!(locked(LOCK, "wasmtime").as_deref(), Some("47.0.3"));
    assert_eq!(locked(LOCK, "wasmtime-environ").as_deref(), Some("46.0.0"));
}

#[test]
fn a_version_is_the_first_word_of_a_report_that_reads_like_one() {
    assert_eq!(version("rustc 1.95.0 (abc123 2026-01-01)"), "1.95.0");
    assert_eq!(version("wasmtime-cli 47.0.3"), "47.0.3");
    assert_eq!(version("v22.14.0\n"), "v22.14.0");
}

#[test]
fn a_report_stating_no_version_answers_nothing() {
    assert_eq!(version("no version here"), "");
    assert_eq!(version(""), "");
}
