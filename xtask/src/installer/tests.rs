//! What a rendered installer holds, and which arguments the recipe refuses to bake in.

use super::*;

#[test]
fn the_version_is_baked_in_and_the_template_guard_cannot_fire() {
    let script = Installer { version: "0.12.1" }.render().unwrap();

    assert!(script.contains("\nVERSION=\"0.12.1\"\n"), "{script}");
    assert!(!script.contains("{{"), "{script}");
    assert!(script.contains("*\"{\"*) die"), "{script}");
}

#[test]
fn a_whole_tag_an_empty_argument_and_a_quoted_one_are_not_versions() {
    for refused in [
        "",
        "release/0.12.1",
        "0.12.1 ",
        "\"0.12.1\"",
        "{{ version }}",
    ] {
        assert!(validated(refused).is_err(), "{refused:?}");
    }

    assert_eq!(validated("0.12.1"), Ok("0.12.1"));
    assert_eq!(validated("1.0.0-rc.1+build.7"), Ok("1.0.0-rc.1+build.7"));
}
