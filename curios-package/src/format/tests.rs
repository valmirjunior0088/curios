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
        line_width: Some(width(columns)),
        ..FormatSettings::default()
    }
}

#[test]
fn a_package_and_an_umbrella_each_state_a_width_under_format() {
    let Ok(Manifest::Package(package)) = concat!(
        "name = \"app\"\ncompiler = \"",
        env!("CARGO_PKG_VERSION"),
        "\"\n\n[format]\nline-width = 80\n"
    )
    .parse() else {
        panic!("a package's manifest");
    };
    assert_eq!(package.format, Some(flags(80)));

    let Ok(Manifest::Umbrella(umbrella)) = concat!(
        "compiler = \"",
        env!("CARGO_PKG_VERSION"),
        "\"\nmembers = []\n\n[format]\nline-width = 80\n"
    )
    .parse() else {
        panic!("an umbrella's manifest");
    };
    assert_eq!(umbrella.format, Some(flags(80)));
}

#[test]
fn a_key_format_does_not_take_is_refused_where_the_manifest_is_read() {
    let refusal = "name = \"app\"\n\n[format]\nwidht = 80\n"
        .parse::<Manifest>()
        .unwrap_err()
        .said
        .to_string();
    assert!(
        refusal.contains("`widht` is no field of `[format]`"),
        "{refusal}"
    );
    assert!(refusal.contains("`line-width`"), "{refusal}");
}

#[test]
fn a_width_the_formatter_does_not_admit_is_refused_where_the_manifest_is_read() {
    for (stated, refused) in [
        ("30", "a line width is"),
        (
            "60",
            "a line width is `50`, `80` or `100` columns, and `60` is not",
        ),
        ("101", "a line width is"),
        ("-1", "`line-width` is a whole number"),
        ("\"80\"", "`line-width` is a whole number"),
    ] {
        let refusal = format!("name = \"app\"\n\n[format]\nline-width = {stated}\n")
            .parse::<Manifest>()
            .unwrap_err()
            .said
            .to_string();
        assert!(refusal.contains(refused), "{stated}: {refusal}");
    }
}

/// An indentation is stated under `[format]` and as a flag, the default's where neither states one, and one the formatter does not admit is refused where the manifest is read.
#[test]
fn an_indentation_is_stated_as_a_width_is() {
    let two = Indent::new(2).expect("the indentation is admitted");
    let Ok(Manifest::Package(package)) = "name = \"app\"\n\n[format]\nindent-width = 2\n".parse()
    else {
        panic!("a package's manifest");
    };
    assert_eq!(
        package.format.map(|settings| settings.indent_width),
        Some(Some(two))
    );
    let flag = FormatSettings {
        indent_width: Some(two),
        ..FormatSettings::default()
    };
    assert_eq!(
        flag.format_under(None).map(|format| format.indent_width),
        Ok(two)
    );
    assert_eq!(
        FormatSettings::default()
            .format_under(None)
            .map(|format| format.indent_width),
        Ok(Indent::default())
    );

    for (stated, refused) in [
        ("3", "an indentation is `2` or `4` columns, and `3` is not"),
        ("8", "and `8` is not"),
        ("-1", "`indent-width` is a whole number"),
    ] {
        let refusal = format!("name = \"app\"\n\n[format]\nindent-width = {stated}\n")
            .parse::<Manifest>()
            .unwrap_err()
            .said
            .to_string();
        assert!(refusal.contains(refused), "{stated}: {refusal}");
    }
}

/// A flat match is stated beside the line width, and refused where it says what no line could show or keeps a match off every line by its arms, which the span alone says: where the manifest is read, at the key, and as a flag, by the flag.
#[test]
fn a_flat_match_past_its_lines_or_of_no_arms_is_refused() {
    for (stated, refused) in [
        (
            "line-width = 50\nflat-match-width = 70\n",
            "a flat match's width is at most the line width, and `70` is past `50`",
        ),
        ("flat-match-width = 101\n", "`101` is past `100`"),
        ("flat-match-arms = 0\n", "a flat match keeps at least 1 arm"),
        (
            "flat-match-arms = 11\n",
            "a flat match keeps at most 10 arms",
        ),
        (
            "flat-match-arms = -1\n",
            "`flat-match-arms` is a whole number",
        ),
    ] {
        let refusal = format!("name = \"app\"\n\n[format]\n{stated}")
            .parse::<Manifest>()
            .unwrap_err()
            .said
            .to_string();
        assert!(refusal.contains(refused), "{stated}: {refusal}");
    }

    let past = FormatSettings {
        line_width: Some(width(50)),
        flat_match_width: Some(70),
        ..FormatSettings::default()
    };
    let refusal = past.format_under(None).unwrap_err().render();
    assert!(refusal.contains("--flat-match-width"), "{refusal}");
    let none = FormatSettings {
        flat_match_arms: Some(0),
        ..FormatSettings::default()
    };
    let refusal = none.format_under(None).unwrap_err().render();
    assert!(refusal.contains("--flat-match-arms"), "{refusal}");
}

/// A flat match unstated keeps the defaults, narrowed to the line where the line is narrower, and one stated is kept as stated, from no span up to the line and from the fewest arms through the most.
#[test]
fn a_flat_match_is_its_defaults_where_nothing_states_it() {
    let flat_match = |settings: FormatSettings| {
        settings
            .format_under(None)
            .map(|format| (format.flat_match.width(), format.flat_match.arms()))
    };
    assert_eq!(
        flat_match(FormatSettings::default()),
        Ok((DEFAULT_FLAT_MATCH_WIDTH, DEFAULT_FLAT_MATCH_ARMS))
    );
    assert_eq!(flat_match(flags(50)), Ok((50, DEFAULT_FLAT_MATCH_ARMS)));
    assert_eq!(
        flat_match(FormatSettings {
            line_width: Some(width(50)),
            flat_match_width: Some(0),
            flat_match_arms: Some(MAX_FLAT_MATCH_ARMS),
            ..FormatSettings::default()
        }),
        Ok((0, MAX_FLAT_MATCH_ARMS))
    );
}

#[test]
fn a_package_formats_at_the_width_it_states_and_at_the_line_width_where_it_states_none() {
    let root = Temporary::new("format", "standalone");
    write(
        &root,
        "stated/curios.toml",
        concat!(
            "name = \"stated\"\ncompiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\n\n[format]\nline-width = 80\n"
        ),
    );
    write(
        &root,
        "silent/curios.toml",
        concat!(
            "name = \"silent\"\ncompiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\n"
        ),
    );

    let stated = Governing::of(&root.join("stated"), None).expect("a package");
    let silent = Governing::of(&root.join("silent"), None).expect("a package");

    let none = FormatSettings::default();
    assert_eq!(
        none.format_under(Some(&stated))
            .map(|format| format.line_width),
        Ok(width(80))
    );
    assert_eq!(
        none.format_under(Some(&silent))
            .map(|format| format.line_width),
        Ok(Width::default())
    );
}

#[test]
fn a_member_formats_at_its_umbrellas_width_and_states_none_of_its_own() {
    let root = Temporary::new("format", "umbrella");
    write(
        &root,
        MANIFEST,
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nmembers = [\"app\", \"own\"]\n\n[format]\nline-width = 50\n"
        ),
    );
    write(&root, "app/curios.toml", "name = \"app\"\n");
    write(
        &root,
        "own/curios.toml",
        "name = \"own\"\n\n[format]\nline-width = 80\n",
    );

    let member = Governing::of(&root.join("app"), None).expect("a member");
    assert_eq!(
        FormatSettings::default()
            .format_under(Some(&member))
            .map(|format| format.line_width),
        Ok(width(50))
    );

    let refusal = Governing::of(&root.join("own"), None)
        .err()
        .expect("a member restates `[format]`")
        .render();
    assert!(
        refusal.contains("`[format]` is inherited from the umbrella at"),
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
        concat!(
            "name = \"app\"\ncompiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\n"
        ),
    );
    write(&root, "app/lib.crs", "");
    write(
        &root,
        "work/curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nmembers = []\n\n[format]\nline-width = 80\n"
        ),
    );
    write(&root, "work/scratch.crs", "");
    write(&root, "loose.crs", "");

    let narrow = flags(50);
    assert_eq!(
        narrow.format_under(None).map(|format| format.line_width),
        Ok(width(50))
    );
    assert_eq!(
        narrow
            .format_of_file(&root.join("loose.crs"), None, None)
            .map(|format| format.line_width),
        Ok(width(50))
    );
    // An umbrella governs only the packages it enumerates, so a file in its tree and in none of them is no package's.
    assert_eq!(
        narrow
            .format_of_file(&root.join("work/scratch.crs"), None, None)
            .map(|format| format.line_width),
        Ok(width(50))
    );

    let refusal = narrow
        .format_of_file(&root.join("app/lib.crs"), None, None)
        .unwrap_err()
        .render();
    assert!(
        refusal.contains("the package `app` takes its format from"),
        "{refusal}"
    );
    assert!(
        refusal.contains(&root.join("app/curios.toml").display().to_string()),
        "{refusal}"
    );
    assert_eq!(
        FormatSettings::default()
            .format_of_file(&root.join("app/lib.crs"), None, None)
            .map(|format| format.line_width),
        Ok(Width::default())
    );
}

#[test]
fn a_file_outside_the_package_manifest_names_is_refused() {
    let root = Temporary::new("format", "outside");
    write(
        &root,
        "app/curios.toml",
        concat!(
            "name = \"app\"\ncompiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\n"
        ),
    );
    write(&root, "loose.crs", "");

    let refusal = FormatSettings::default()
        .format_of_file(
            &root.join("loose.crs"),
            Some(&root.join("app/curios.toml")),
            None,
        )
        .unwrap_err()
        .render();
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
