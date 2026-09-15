//! Which test paths `--filter` selects.

use super::under;

#[test]
fn a_filter_selects_the_path_it_names_and_every_test_under_it() {
    assert!(under("/app/Map", "/app/Map"));
    assert!(under("/app/Map/holds", "/app/Map"));
    assert!(under("/app/Map/Tree/holds", "/app/Map"));
}

#[test]
fn a_filter_never_selects_a_neighbour_its_last_segment_begins() {
    assert!(!under("/app/MapBuilder/holds", "/app/Map"));
    assert!(!under("/app/addition_passes", "/app/addition"));
    assert!(!under("/app", "/app/Map"));
}

#[test]
fn a_trailing_slash_selects_what_the_path_without_it_does() {
    assert!(under("/app/Map/holds", "/app/Map/"));
    assert!(!under("/app/MapBuilder/holds", "/app/Map/"));
    assert!(under("/app/Map/holds", "/"));
}
