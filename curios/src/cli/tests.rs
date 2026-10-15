//! The boundary between compiler options, the optional target and opaque program arguments.

use {super::*, curios_pipeline::DEFAULT_STEP_BUDGET, std::iter};

fn run(words: &[&str]) -> (Option<String>, Vec<OsString>, Elaboration) {
    let cli = Cli::try_parse_from(iter::once("curios").chain(words.iter().copied())).unwrap();
    let Mode::Run {
        target,
        args,
        elaboration,
    } = cli.mode
    else {
        panic!("the fixture names run");
    };
    (target, args, elaboration)
}

#[test]
fn the_target_and_program_arguments_follow_one_boundary() {
    for (words, target, arguments) in [
        (vec!["run"], None, vec![]),
        (vec!["run", "--"], None, vec![]),
        (vec!["run", "serve"], Some("serve"), vec![]),
        (
            vec!["run", "scratch.crs", "--port", "8080"],
            Some("scratch.crs"),
            vec!["--port", "8080"],
        ),
        (vec!["run", "--", "mock"], None, vec!["mock"]),
        (vec!["run", "--", "--help"], None, vec!["--help"]),
        (vec!["run", "--", "--", "mock"], None, vec!["--", "mock"]),
        (vec!["run", "-", "--", "mock"], Some("-"), vec!["mock"]),
        (vec!["run", "serve", "--"], Some("serve"), vec![]),
        (
            vec!["run", "serve", "--", "--help"],
            Some("serve"),
            vec!["--help"],
        ),
        (
            vec!["run", "serve", "--", "--", "mock"],
            Some("serve"),
            vec!["--", "mock"],
        ),
        (
            vec!["run", "serve", "arg", "--", "later"],
            Some("serve"),
            vec!["arg", "--", "later"],
        ),
        (
            vec![
                "run",
                "serve",
                "--help",
                "--version",
                "--budget",
                "0",
                "--manifest",
                "elsewhere",
            ],
            Some("serve"),
            vec![
                "--help",
                "--version",
                "--budget",
                "0",
                "--manifest",
                "elsewhere",
            ],
        ),
        (vec!["run", "serve", ""], Some("serve"), vec![""]),
    ] {
        let (actual_target, args, elaboration) = run(&words);
        assert_eq!(actual_target.as_deref(), target, "{words:?}");
        assert_eq!(
            args,
            arguments.iter().map(OsString::from).collect::<Vec<_>>(),
            "{words:?}"
        );
        assert_eq!(elaboration.budget, DEFAULT_STEP_BUDGET);
        assert!(elaboration.manifest.manifest.is_none());
    }
}

#[test]
fn compiler_options_are_read_only_before_the_program_tail() {
    let (target, args, elaboration) = run(&[
        "run",
        "--budget",
        "5",
        "--manifest",
        "app/curios.toml",
        "serve",
        "--budget",
        "0",
    ]);
    assert_eq!(target.as_deref(), Some("serve"));
    assert_eq!(args, ["--budget", "0"]);
    assert_eq!(elaboration.budget, 5);
    assert_eq!(
        elaboration.manifest.manifest,
        Some(PathBuf::from("app/curios.toml"))
    );

    let (target, args, elaboration) = run(&["run", "--budget=5", "--", "--budget", "0"]);
    assert!(target.is_none());
    assert_eq!(args, ["--budget", "0"]);
    assert_eq!(elaboration.budget, 5);

    let (target, args, _) = run(&["run", "--manifest=--", "serve"]);
    assert_eq!(target.as_deref(), Some("serve"));
    assert!(args.is_empty());
}

#[test]
fn compiler_failures_and_help_still_belong_to_the_prefix() {
    for (words, kind) in [
        (
            vec!["curios", "run", "--budgte", "5"],
            ErrorKind::UnknownArgument,
        ),
        (vec!["curios", "run", "--mock"], ErrorKind::UnknownArgument),
        (vec!["curios", "run", "--budget"], ErrorKind::InvalidValue),
        (
            vec!["curios", "run", "--budget", "many"],
            ErrorKind::ValueValidation,
        ),
        (vec!["curios", "run", "--manifest"], ErrorKind::InvalidValue),
        (vec!["curios", "run", "--help"], ErrorKind::DisplayHelp),
        (vec!["curios", "--help"], ErrorKind::DisplayHelp),
        (vec!["curios", "--version"], ErrorKind::DisplayVersion),
    ] {
        assert_eq!(
            Cli::try_parse_from(words.clone()).unwrap_err().kind(),
            kind,
            "{words:?}"
        );
    }
}

#[test]
fn help_describes_the_optional_target_and_program_tail() {
    let error = Cli::try_parse_from(["curios", "run", "--help"]).unwrap_err();
    let help = error.to_string();
    assert!(help.contains("curios run [OPTIONS] [TARGET] [ARGS]..."));
    assert!(help.contains(&RUN.target_help()));
    assert!(help.contains("begin with -- to omit TARGET"));
    Cli::command().debug_assert();
}

#[cfg(unix)]
#[test]
fn program_arguments_remain_opaque_while_targets_require_utf8() {
    use std::os::unix::ffi::OsStringExt;

    let raw = OsString::from_vec(vec![0xff, 0xfe]);
    for prefix in [
        vec!["curios", "run", "--"],
        vec!["curios", "run", "serve"],
        vec!["curios", "run", "serve", "--"],
    ] {
        let words = prefix
            .iter()
            .map(OsString::from)
            .chain(iter::once(raw.clone()));
        let cli = Cli::try_parse_from(words).unwrap();
        let Mode::Run { args, .. } = cli.mode else {
            unreachable!()
        };
        assert_eq!(args.as_slice(), std::slice::from_ref(&raw));
    }
    let error = Cli::try_parse_from(["curios".into(), "run".into(), raw]).unwrap_err();
    assert_eq!(error.kind(), ErrorKind::InvalidUtf8);
}

#[cfg(feature = "profile")]
#[test]
fn profiling_is_global_until_the_program_tail_starts() {
    for words in [
        vec![
            "curios",
            "--profile",
            "trace",
            "run",
            "serve",
            "--profile",
            "guest",
        ],
        vec![
            "curios",
            "run",
            "--profile",
            "trace",
            "--",
            "--profile",
            "guest",
        ],
    ] {
        let cli = Cli::try_parse_from(words).unwrap();
        assert_eq!(cli.profile, Some(PathBuf::from("trace")));
        let Mode::Run { args, .. } = cli.mode else {
            unreachable!()
        };
        assert_eq!(args, ["--profile", "guest"]);
    }
}
