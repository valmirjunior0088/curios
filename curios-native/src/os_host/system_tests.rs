//! What the system answers: randomness, an environment variable, the clock, a file's modification time, an open that creates, what a private file or directory may be read by, and a symbolic link read as itself.

use {
    super::*,
    curios_common::test_support::*,
    std::os::unix::{self, fs::PermissionsExt},
};

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

/// An open to create takes a path nothing is at, and a second open at it is `AlreadyExists`, the operating system answering both: `O_CREAT | O_EXCL`.
#[test]
fn an_open_to_create_is_refused_where_the_file_exists() {
    let host = OsHost::with_args(vec![]);
    let dir = Temporary::new("runtime-open", "create");
    fs::create_dir(&dir).expect("a fresh temporary directory");
    let path = dir.join("lock").into_os_string().into_vec();

    assert!(host.file_open(path.clone(), Mode::Create).is_ok());
    assert!(matches!(
        host.file_open(path, Mode::Create),
        Err(Failure::AlreadyExists)
    ));
}

/// A private create and a private directory are their owner's alone, whatever the umask lets the others have, where a plain create and a plain directory keep what the umask leaves of every bit; the umask is set to `0` for the test, so what is created is what was asked.
#[test]
fn a_private_file_and_directory_are_their_owners_alone() {
    let host = OsHost::with_args(vec![]);
    let dir = Temporary::new("runtime-open", "private");
    fs::create_dir(&dir).expect("a fresh temporary directory");
    let bytes = |name: &str| dir.join(name).into_os_string().into_vec();
    let permissions = |name: &str| {
        fs::metadata(dir.join(name))
            .expect("what the host created")
            .permissions()
            .mode()
            & 0o777
    };

    let was = rustix::process::umask(rustix::fs::Mode::empty());
    let created = (
        host.file_open(bytes("private"), Mode::CreatePrivate)
            .is_ok(),
        host.file_open(bytes("plain"), Mode::Create).is_ok(),
        host.dir_create(bytes("held"), true),
        host.dir_create(bytes("shared"), false),
    );
    rustix::process::umask(was);

    assert_eq!(created, (true, true, Ok(()), Ok(())));
    assert_eq!(
        [
            permissions("private"),
            permissions("plain"),
            permissions("held"),
            permissions("shared")
        ],
        [0o600, 0o666, 0o700, 0o777]
    );
}

/// `file_link_stat` reads a symbolic link as itself, where `file_stat` reads what it names: a link to a directory is a directory to one and a link to the other, and the directory itself is a directory to both.
#[test]
fn a_link_is_read_as_itself_where_stat_follows_it() {
    let host = OsHost::with_args(vec![]);
    let dir = Temporary::new("runtime-stat", "link");
    fs::create_dir_all(dir.join("target")).expect("a fresh temporary directory");
    unix::fs::symlink(dir.join("target"), dir.join("link")).expect("a link");
    let bytes = |name: &str| dir.join(name).into_os_string().into_vec();
    let kind = |stat: Result<FileStat, Failure>| stat.map(|stat| stat.kind);

    assert_eq!(kind(host.file_stat(bytes("link"))), Ok(FileKind::Directory));
    assert_eq!(
        kind(host.file_link_stat(bytes("link"))),
        Ok(FileKind::Symlink)
    );
    assert_eq!(
        kind(host.file_link_stat(bytes("target"))),
        Ok(FileKind::Directory)
    );
}
