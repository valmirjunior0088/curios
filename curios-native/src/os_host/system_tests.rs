//! What the system answers: randomness, an environment variable, the clock, and a file's modification time.

use {super::*, curios_utilities::test_support::Temporary};

/// Randomness answers exactly the bytes asked for, and a request no memory can hold refuses the call rather than abort the process.
#[test]
fn randomness_answers_exactly_what_was_asked_or_refuses() {
    let host = OsHost::with_args(vec![]);

    assert_eq!(host.rand_bytes(32).map(|bytes| bytes.len()), Ok(32));
    assert!(host.rand_bytes(u64::MAX).is_err());
}

/// A name that is empty or holds `=` or NUL names no variable, so it is absent without the environment being asked how the platform reads such a name.
#[test]
fn a_name_no_variable_can_have_is_absent() {
    let host = OsHost::with_args(vec![]);

    for name in [&b""[..], b"PATH=", b"PA\0TH"] {
        assert_eq!(host.proc_env(name.to_vec()), None);
    }
    assert!(host.proc_env(b"PATH".to_vec()).is_some());
}

/// Both clocks answer whole seconds beside the nanoseconds within one.
#[test]
fn a_clock_answers_the_nanoseconds_within_a_second() {
    let host = OsHost::with_args(vec![]);

    assert!(host.clock_wall().unwrap().nanos < 1_000_000_000);
    assert!(host.clock_mono().unwrap().nanos < 1_000_000_000);
}

/// A file dated before 1970 crosses as itself: its seconds below zero and its nanoseconds counting forward, where it once crossed as the epoch.
#[test]
fn a_modification_time_before_the_epoch_crosses_as_itself() {
    let host = OsHost::with_args(vec![]);
    let dir = Temporary::new("runtime-stat", "before-the-epoch");
    fs::create_dir(&dir).expect("a fresh temporary directory");
    let path = dir.join("old");
    let file = fs::File::create(&path).expect("a fresh file");
    let before = std::time::UNIX_EPOCH - std::time::Duration::from_millis(1_500);
    file.set_modified(before)
        .expect("a modification time before 1970");

    let stat = host.file_stat(path.into_os_string().into_vec()).unwrap();

    assert_eq!((stat.mtime_secs, stat.mtime_nanos), (-2, 500_000_000));
}
