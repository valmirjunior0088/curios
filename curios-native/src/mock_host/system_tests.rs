//! The rest of the scripted host: a handle missed loudly after close, the file system's root and directories, replies and children, and the timeline a wait and a deadline share.

use {
    super::{
        EBUSY, Looked, MockHost,
        test_support::{come, spawned, watched},
    },
    crate::{ChildExit, Failure, FileKind, FileStat, Mode, Settled, blocking},
    curios_abi::{HostOps, event},
    std::{num::NonZeroU32, sync::mpsc},
};

#[test]
fn use_after_close_on_a_handle_is_a_loud_miss_not_an_alias() {
    let (host, _io) = MockHost::builder().build();

    // Open a write-mode file, write to it, then close it.
    let handle = host.file_open(b"f".to_vec(), Mode::Write).unwrap();
    assert_eq!(host.handle_write(handle.clone(), b"x".to_vec()), Ok(1));
    host.handle_close(handle.clone());

    // Write after close is a loud `NotFound`, never a silent success...
    assert_eq!(
        host.handle_write(handle.clone(), b"y".to_vec()),
        Err(Failure::NotFound)
    );
    // ...read after close is the same loud miss, never a quiet end-of-stream drain...
    assert_eq!(host.handle_read(handle.clone(), 8), Err(Failure::NotFound));
    // ...and a double close is a no-op, not a panic.
    host.handle_close(handle.clone());

    // A second open never reuses the closed token, and the stale handle keeps missing rather than aliasing the freshly opened file.
    let fresh = host.file_open(b"g".to_vec(), Mode::Write).unwrap();
    assert_ne!(handle.bytes(), fresh.bytes());
    assert_eq!(
        host.handle_write(handle, b"z".to_vec()),
        Err(Failure::NotFound)
    );
    assert_eq!(host.handle_write(fresh, b"ok".to_vec()), Ok(2));
}

/// A writing `file_open` under a directory that is not there answers `NotFound`, as the OS does, rather than filing a file whose parent `file_stat` would then deny; once the directory is made, the same open succeeds and the parent stats as a directory.
#[test]
fn a_writing_open_under_a_missing_directory_is_refused_until_the_directory_exists() {
    let (host, _io) = MockHost::builder().build();

    for mode in [Mode::Write, Mode::Append] {
        assert!(matches!(
            host.file_open(b"a/b.txt".to_vec(), mode),
            Err(Failure::NotFound)
        ));
    }
    assert_eq!(host.file_stat(b"a".to_vec()), Err(Failure::NotFound));

    assert_eq!(host.dir_create(b"a".to_vec()), Ok(()));
    let handle = host.file_open(b"a/b.txt".to_vec(), Mode::Write).unwrap();
    assert_eq!(host.handle_write(handle.clone(), b"x".to_vec()), Ok(1));
    host.handle_close(handle);
    assert!(matches!(
        host.file_stat(b"a".to_vec()),
        Ok(FileStat {
            kind: FileKind::Directory,
            ..
        })
    ));
    assert!(host.file_open(b"a/b.txt".to_vec(), Mode::Append).is_ok());
}

/// The root exists without being seeded: a directory is made and a file written under `/`, the root stats and lists as a directory holding them, remaking it is `AlreadyExists`, and removing it is refused — `NotEmpty` while it holds anything, `EBUSY` once it is bare, as `rmdir(2)` answers.
#[test]
fn the_root_directory_exists_holds_absolute_paths_and_cannot_be_removed() {
    let (host, _io) = MockHost::builder().build();

    assert_eq!(host.dir_create(b"/x".to_vec()), Ok(()));
    let handle = host.file_open(b"/x/f".to_vec(), Mode::Write).unwrap();
    host.handle_close(handle);

    assert!(matches!(
        host.file_stat(b"/".to_vec()),
        Ok(FileStat {
            kind: FileKind::Directory,
            ..
        })
    ));
    assert_eq!(host.dir_list(b"/".to_vec()), Ok(vec![b"x".to_vec()]));
    assert_eq!(host.dir_list(b"/x".to_vec()), Ok(vec![b"f".to_vec()]));
    assert_eq!(host.dir_create(b"/".to_vec()), Err(Failure::AlreadyExists));
    assert_eq!(host.file_remove(b"/".to_vec()), Err(Failure::IsDirectory));
    assert_eq!(host.dir_remove(b"/".to_vec()), Err(Failure::NotEmpty));

    assert_eq!(host.file_remove(b"/x/f".to_vec()), Ok(()));
    assert_eq!(host.dir_remove(b"/x".to_vec()), Ok(()));
    assert_eq!(host.dir_remove(b"/".to_vec()), Err(Failure::Other(EBUSY)));
}

// A reply the script gives later is pending for one look and there on the next, so a call answered later suspends once, as a native one that takes its time does; the token is read back once and names nothing after.
#[test]
fn a_scripted_reply_comes_on_its_second_look() {
    let (host, _io) = MockHost::builder().build();
    let reply = Looked::later(7u64);
    let token = host
        .later(Box::pin(async move { Box::new(reply.await) as Settled }))
        .expect_err("the first look finds it pending");

    assert_eq!(host.handle_ready(vec![token.clone()]), [event::READY]);
    let settled = host.settled(token.clone()).unwrap().expect("it has come");
    assert_eq!(*settled.downcast::<u64>().unwrap(), 7);
    assert!(host.settled(token).is_err());
}

// A reply that comes from work run elsewhere wakes a wait that holds out for it alone, which sleeps until it does rather than refusing the run.
#[test]
fn a_wait_on_a_reply_to_come_sleeps_until_it_wakes() {
    let (host, _io) = MockHost::builder().build();
    let (signal, signalled) = mpsc::channel::<()>();
    let work = blocking(move || {
        signalled.recv().unwrap();

        3u64
    });

    let token = host
        .later(Box::pin(async move { Box::new(work.await) as Settled }))
        .expect_err("the work waits for its signal");
    signal.send(()).unwrap();
    host.wait(vec![token.clone()]).unwrap();

    let settled = host.settled(token).unwrap().expect("it has come");
    assert_eq!(*settled.downcast::<u64>().unwrap(), 3);
}

/// A running child is not ready and cannot be waited for until a kill ends it by `SIGKILL`, which is recorded; a child that has ended is not signaled again, and nothing is recorded — the mirror of a native host that never signals a pid it has reaped.
#[test]
fn a_running_child_ends_by_its_kill_and_an_ended_one_is_not_signaled() {
    let (host, io) = MockHost::builder()
        .running_children(["sleepy"])
        .children([("done", "", "", ChildExit::Code(0))])
        .build();

    let sleepy = spawned(&host, b"sleepy").child;
    assert_eq!(host.proc_wait(sleepy.clone()), Err(Failure::WouldBlock));
    assert!(!watched(&host, &sleepy, true, false));

    assert_eq!(host.proc_kill(sleepy.clone()), Ok(()));
    assert!(watched(&host, &sleepy, true, false));
    assert_eq!(
        host.proc_wait(sleepy.clone()),
        Ok(ChildExit::Signal(NonZeroU32::new(9).unwrap()))
    );
    assert_eq!(host.proc_wait(sleepy), Err(Failure::NotFound));

    let done = spawned(&host, b"done").child;
    assert_eq!(host.proc_kill(done.clone()), Ok(()));
    assert_eq!(host.proc_wait(done), Ok(ChildExit::Code(0)));
    assert_eq!(io.kills(), [b"sleepy".to_vec()]);
}

/// Waiting consumes only a child: a handle of any other kind answers `not_found` and stays filed.
#[test]
fn waiting_on_what_is_not_a_child_leaves_it_filed() {
    let (host, _io) = MockHost::builder().build();
    let file = host.file_open(b"f".to_vec(), Mode::Write).unwrap();

    assert_eq!(host.proc_wait(file.clone()), Err(Failure::NotFound));
    assert_eq!(host.handle_write(file, b"x".to_vec()), Ok(1));
}

/// A spawn answers the parent's end of each stream it piped and nothing for the rest, and a piped output reads back what the script wrote.
#[test]
fn a_spawn_answers_exactly_the_streams_it_piped() {
    let (host, _io) = MockHost::builder()
        .children([("greet", "hello", "", ChildExit::Code(0))])
        .build();
    let spawned = spawned(&host, b"greet");

    assert_eq!((&spawned.stdin, &spawned.stderr), (&None, &None));
    assert_eq!(
        host.handle_read(spawned.stdout.expect("stdout was piped"), 8),
        Ok(Some(b"hello".to_vec()))
    );
}

/// A wait that readies nothing sleeps until its soonest deadline, after which that deadline is ready and a later one is not; a wait on watches alone that readies nothing is refused, since nothing would end it.
#[test]
fn a_wait_sleeps_to_its_soonest_deadline_and_one_nothing_can_end_is_refused() {
    let (host, _io) = MockHost::builder().inbound_open([vec!["ping"]]).build();
    let listener = host.socket_listen(b"0.0.0.0:8080".to_vec()).unwrap();
    let conn = host.socket_accept(listener).unwrap();
    assert_eq!(
        host.handle_read(conn.clone(), 8),
        Ok(Some(b"ping".to_vec()))
    );

    let watch = host.handle_watch(conn, true, false);
    let (soon, late) = (host.clock_deadline(1, 0), host.clock_deadline(2, 0));
    assert_eq!(host.handle_ready(vec![soon.clone(), late.clone()]), [0, 0]);
    assert!(
        host.wait(vec![watch.clone(), late.clone(), soon.clone()])
            .is_ok()
    );
    assert_eq!(host.handle_ready(vec![soon, late]), [event::READY, 0]);
    assert!(host.wait(vec![watch]).is_err());
}

/// The scripted host refuses what the native host refuses before asking anything: a lookup of a host that is not UTF-8 or of a port past sixteen bits, and a variable whose name no variable can have.
#[test]
fn a_lookup_and_a_variable_are_refused_as_the_native_host_refuses_them() {
    let (host, _io) = MockHost::builder().env([("A", "1")]).build();

    assert_eq!(
        come(&host, host.dns_lookup(b"\xff".to_vec(), 80)),
        Err(Failure::NotFound)
    );
    assert_eq!(
        come(&host, host.dns_lookup(b"example.com".to_vec(), 65_536)),
        Err(Failure::Other(22))
    );
    assert!(come(&host, host.dns_lookup(b"example.com".to_vec(), 80)).is_ok());

    assert_eq!(host.proc_env(b"A".to_vec()), Some(b"1".to_vec()));
    assert_eq!(host.proc_env(b"A=".to_vec()), None);
    assert_eq!(host.proc_env(vec![]), None);
}

/// Randomness answers exactly the bytes asked for.
#[test]
fn randomness_answers_exactly_what_was_asked() {
    let (host, _io) = MockHost::builder().build();

    assert_eq!(host.rand_bytes(0).map(|bytes| bytes.len()), Ok(0));
    assert_eq!(host.rand_bytes(32).map(|bytes| bytes.len()), Ok(32));
}

/// Both clocks answer whole seconds beside the nanoseconds within one.
#[test]
fn a_clock_answers_the_nanoseconds_within_a_second() {
    let (host, _io) = MockHost::builder().build();

    assert!(host.clock_wall().unwrap().nanos < 1_000_000_000);
    assert!(host.clock_mono().unwrap().nanos < 1_000_000_000);
}

/// The mock keeps one timeline: a scripted reading moves it forward, a deadline it has passed is ready at once, and a reading after a wait is never before the deadline the wait slept to.
#[test]
fn a_reading_and_a_deadline_share_one_timeline() {
    let (host, _io) = MockHost::builder().mono([(5, 0), (1, 0)]).build();

    assert_eq!(host.clock_mono().unwrap().secs, 5);
    let passed = host.clock_deadline(3, 0);
    assert_eq!(host.handle_ready(vec![passed]), [event::READY]);

    let ahead = host.clock_deadline(9, 0);
    assert!(host.wait(vec![ahead]).is_ok());
    assert_eq!(host.clock_mono().unwrap().secs, 9);
}
