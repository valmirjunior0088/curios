//! Which prefix a `--filter` names.

use {super::prefix, curios_utilities::Qualifier};

#[test]
fn a_filter_names_its_paths_segments() {
    let app_map = Qualifier::from(["app", "Map"]);

    assert_eq!(prefix("/app/Map"), app_map);
    assert_eq!(prefix("app/Map"), app_map);
}

#[test]
fn a_trailing_slash_names_what_the_path_without_it_does() {
    assert_eq!(prefix("/app/Map/"), Qualifier::from(["app", "Map"]));
    assert_eq!(prefix("/"), Qualifier::empty());
}
