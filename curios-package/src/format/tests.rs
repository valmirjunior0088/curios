//! `[format]` as a manifest states it, the owner membership chooses for it, the one source a file's width is read from, and the one spelling each setting has as a key and as a flag.

use {
    super::*,
    crate::*,
    clap::Command,
    curios_common::test_support::*,
    serde::de::{self, Visitor},
    std::{fmt, fs},
};

fn write(root: &Path, path: &str, source: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, source).unwrap();
}

fn width(columns: usize) -> Width {
    Width::new(columns).expect("the width is admitted")
}

fn flags(columns: usize) -> FormatSettings {
    FormatSettings {
        width: Some(width(columns)),
    }
}

#[test]
fn a_package_and_an_umbrella_each_state_a_width_under_format() {
    let Ok(Manifest::Package(package)) =
        "name = \"app\"\ncompiler = \"0.0.0\"\n\n[format]\nwidth = 72\n".parse()
    else {
        panic!("a package's manifest");
    };
    assert_eq!(package.format, Some(flags(72)));

    let Ok(Manifest::Umbrella(umbrella)) =
        "compiler = \"0.0.0\"\nmembers = []\n\n[format]\nwidth = 80\n".parse()
    else {
        panic!("an umbrella's manifest");
    };
    assert_eq!(umbrella.format, Some(flags(80)));
}

#[test]
fn a_key_format_does_not_take_is_refused_where_the_manifest_is_read() {
    let refusal = "name = \"app\"\n\n[format]\nwidht = 80\n"
        .parse::<Manifest>()
        .unwrap_err();
    assert!(refusal.contains("unknown field `widht`"), "{refusal}");
    assert!(refusal.contains("`width`"), "{refusal}");
}

#[test]
fn a_width_the_formatter_does_not_admit_is_refused_where_the_manifest_is_read() {
    for (stated, refused) in [
        ("30", "a width is"),
        ("101", "a width is"),
        ("-1", "invalid value"),
        ("\"80\"", "invalid type"),
    ] {
        let refusal = format!("name = \"app\"\n\n[format]\nwidth = {stated}\n")
            .parse::<Manifest>()
            .unwrap_err();
        assert!(refusal.contains(refused), "{stated}: {refusal}");
    }
}

#[test]
fn a_package_formats_at_the_width_it_states_and_at_the_line_width_where_it_states_none() {
    let root = Temporary::new("format", "standalone");
    write(
        &root,
        "stated/curios.toml",
        "name = \"stated\"\ncompiler = \"0.0.0\"\n\n[format]\nwidth = 72\n",
    );
    write(
        &root,
        "silent/curios.toml",
        "name = \"silent\"\ncompiler = \"0.0.0\"\n",
    );

    let stated = Governing::of(&root.join("stated")).expect("a package");
    let silent = Governing::of(&root.join("silent")).expect("a package");

    let none = FormatSettings::default();
    assert_eq!(none.width_under(Some(&stated)), Ok(width(72)));
    assert_eq!(none.width_under(Some(&silent)), Ok(Width::default()));
}

#[test]
fn a_member_formats_at_its_umbrellas_width_and_states_none_of_its_own() {
    let root = Temporary::new("format", "umbrella");
    write(
        &root,
        MANIFEST,
        "compiler = \"0.0.0\"\nmembers = [\"app\", \"own\"]\n\n[format]\nwidth = 60\n",
    );
    write(&root, "app/curios.toml", "name = \"app\"\n");
    write(
        &root,
        "own/curios.toml",
        "name = \"own\"\n\n[format]\nwidth = 90\n",
    );

    let member = Governing::of(&root.join("app")).expect("a member");
    assert_eq!(
        FormatSettings::default().width_under(Some(&member)),
        Ok(width(60))
    );

    let refusal = Governing::of(&root.join("own"))
        .err()
        .expect("a member restates `[format]`");
    assert!(
        refusal.contains("inherits `[format]` from the umbrella at"),
        "{refusal}"
    );
    assert!(
        refusal.contains(&root.join(MANIFEST).display().to_string()),
        "{refusal}"
    );
}

#[test]
fn a_flag_sets_the_width_of_what_no_package_governs_and_is_refused_where_one_does() {
    let root = Temporary::new("format", "flags");
    write(
        &root,
        "app/curios.toml",
        "name = \"app\"\ncompiler = \"0.0.0\"\n",
    );
    write(&root, "app/lib.crs", "");
    write(
        &root,
        "work/curios.toml",
        "compiler = \"0.0.0\"\nmembers = []\n\n[format]\nwidth = 80\n",
    );
    write(&root, "work/scratch.crs", "");
    write(&root, "loose.crs", "");

    let narrow = flags(60);
    assert_eq!(narrow.width_under(None), Ok(width(60)));
    assert_eq!(
        narrow.width_of_file(&root.join("loose.crs"), None),
        Ok(width(60))
    );
    // An umbrella governs only the packages it enumerates, so a file in its tree and in none of them is no package's.
    assert_eq!(
        narrow.width_of_file(&root.join("work/scratch.crs"), None),
        Ok(width(60))
    );

    let refusal = narrow
        .width_of_file(&root.join("app/lib.crs"), None)
        .unwrap_err();
    assert!(
        refusal.contains("the package `app` takes its format from"),
        "{refusal}"
    );
    assert!(
        refusal.contains(&root.join("app/curios.toml").display().to_string()),
        "{refusal}"
    );
    assert_eq!(
        FormatSettings::default().width_of_file(&root.join("app/lib.crs"), None),
        Ok(Width::default())
    );
}

#[test]
fn a_file_outside_the_package_manifest_names_is_refused() {
    let root = Temporary::new("format", "outside");
    write(
        &root,
        "app/curios.toml",
        "name = \"app\"\ncompiler = \"0.0.0\"\n",
    );
    write(&root, "loose.crs", "");

    let refusal = FormatSettings::default()
        .width_of_file(&root.join("loose.crs"), Some(&root.join("app/curios.toml")))
        .unwrap_err();
    assert!(
        refusal.contains("`--manifest` cannot place it"),
        "{refusal}"
    );
}

/// The keys serde reads `FormatSettings` by, recorded where its derive hands them to the deserializer.
fn keys() -> Vec<&'static str> {
    struct Recorder<'a>(&'a mut Vec<&'static str>);

    #[derive(Debug)]
    struct Recorded;

    impl fmt::Display for Recorded {
        fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            formatter.write_str("the keys were recorded")
        }
    }

    impl std::error::Error for Recorded {}

    impl de::Error for Recorded {
        fn custom<T: fmt::Display>(_: T) -> Self {
            Recorded
        }
    }

    impl<'de> Deserializer<'de> for Recorder<'_> {
        type Error = Recorded;

        fn deserialize_any<V: Visitor<'de>>(self, _: V) -> Result<V::Value, Recorded> {
            Err(Recorded)
        }

        fn deserialize_struct<V: Visitor<'de>>(
            self,
            _: &'static str,
            fields: &'static [&'static str],
            _: V,
        ) -> Result<V::Value, Recorded> {
            self.0.extend_from_slice(fields);
            Err(Recorded)
        }

        serde::forward_to_deserialize_any! {
            bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string bytes byte_buf
            option unit unit_struct newtype_struct seq tuple tuple_struct map enum identifier
            ignored_any
        }
    }

    let mut keys = Vec::new();
    let _ = FormatSettings::deserialize(Recorder(&mut keys));
    keys
}

/// **Every setting is a key and a flag, spelled alike**: one field is both, and this holds the two derives' spellings of it equal, so a setting cannot be stated in a manifest that the command line cannot state, or the other way round.
#[test]
fn every_format_setting_is_a_key_and_a_flag_spelled_alike() {
    let command = FormatSettings::augment_args(Command::new("format"));
    let mut flags = command
        .get_arguments()
        .map(|argument| {
            argument
                .get_long()
                .unwrap_or_else(|| panic!("{} is a long flag", argument.get_id()))
        })
        .collect::<Vec<_>>();
    let mut keys = keys();

    flags.sort_unstable();
    keys.sort_unstable();
    assert!(!keys.is_empty());
    assert_eq!(flags, keys);
}
