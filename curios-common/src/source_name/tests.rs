use {super::*, std::path::Path};

#[test]
fn a_file_below_a_root_is_named_by_its_package_and_its_path() {
    let name = SourceName::of("std", Path::new("Nat/le.crs")).expect("a plain path");

    assert_eq!(name.as_str(), "/std/Nat/le.crs");
}

#[test]
fn a_path_holding_anything_but_plain_names_names_nothing() {
    for path in ["", "/Nat/le.crs", "./le.crs", "Nat/../le.crs"] {
        assert_eq!(SourceName::of("std", Path::new(path)), None, "{path:?}");
    }
}

#[test]
fn a_package_name_that_is_no_identifier_names_nothing() {
    assert_eq!(SourceName::of("std/Nat", Path::new("le.crs")), None);
    assert_eq!(SourceName::of("", Path::new("le.crs")), None);
}
