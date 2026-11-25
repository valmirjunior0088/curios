//! Children: spawned or refused, their piped streams filed non-blocking, reaped through their handle, and killed.

use {
    super::test_support::*,
    super::*,
    std::{
        num::NonZeroU32,
        thread,
        time::{Duration, Instant},
    },
};

/// A piped child stream is filed non-blocking by `proc_spawn`: before the child writes, a read answers `WouldBlock` instead of blocking the caller; bytes it echoes come back through `handle_read` once a watch of it is ready; closing its stdin ends it, its stdout reads to its end, and `proc_wait` reaps it. These streams are what let a drain suspend instead of blocking the run.
#[test]
fn a_piped_child_stream_is_filed_non_blocking() {
    let host = OsHost::with_args(vec![]);
    let ChildHandles {
        child,
        stdin: Some(stdin),
        stdout: Some(stdout),
        stderr: None,
    } = host
        .proc_spawn(
            vec![b"/bin/cat".to_vec()],
            vec![],
            vec![],
            StdioMode::Pipe,
            StdioMode::Pipe,
            StdioMode::Null,
        )
        .unwrap()
    else {
        panic!("the piped streams, and only they, are answered");
    };

    assert_eq!(
        host.handle_read(stdout.clone(), 8),
        Err(Failure::WouldBlock)
    );
    assert_eq!(host.handle_write(stdin.clone(), b"abc".to_vec()), Ok(()));
    assert!(awaited(&host, &stdout, true, false));
    assert_eq!(
        host.handle_read(stdout.clone(), 8),
        Ok(Some(b"abc".to_vec()))
    );

    // Another test's child may have been forked while this write end was open and hold it until its `exec`, so the end of the stream may arrive a few reads late as `WouldBlock`.
    host.handle_close(stdin);
    let mut outcome = host.handle_read(stdout.clone(), 8);
    for _ in 0..100 {
        if outcome != Err(Failure::WouldBlock) {
            break;
        }
        thread::sleep(Duration::from_millis(10));
        outcome = host.handle_read(stdout.clone(), 8);
    }
    assert_eq!(outcome, Ok(None));

    assert!(awaited(&host, &child, true, false));
    assert_eq!(host.proc_wait(child), Ok(ChildExit::Code(0)));

    host.handle_close(stdout);
}

/// A write takes the whole of its message and holds what the pipe has no room for, answering `WouldBlock` for what it still holds, which each flush hands on as the reader makes room: `cat` echoes the message whole, and a flush answers `WouldBlock` until nothing is held. A mebibyte is past what the two pipes and `cat` hold between them.
#[test]
fn a_write_holds_what_its_pipe_has_no_room_for_until_flushes_hand_it_on() {
    let host = OsHost::with_args(vec![]);
    let ChildHandles {
        child,
        stdin: Some(stdin),
        stdout: Some(stdout),
        stderr: None,
    } = host
        .proc_spawn(
            vec![b"/bin/cat".to_vec()],
            vec![],
            vec![],
            StdioMode::Pipe,
            StdioMode::Pipe,
            StdioMode::Null,
        )
        .unwrap()
    else {
        panic!("the piped streams, and only they, are answered");
    };
    let message: Vec<u8> = (0..1 << 20).map(|i: u32| (i % 251) as u8).collect();

    assert_eq!(
        host.handle_write(stdin.clone(), message.clone()),
        Err(Failure::WouldBlock)
    );
    assert_eq!(host.handle_flush(stdin.clone()), Err(Failure::WouldBlock));

    let given_up = Instant::now() + Duration::from_secs(10);
    let mut echoed = Vec::new();
    while echoed.len() < message.len() {
        assert!(
            Instant::now() < given_up,
            "the echo stalled at {}",
            echoed.len()
        );
        let _ = host.handle_flush(stdin.clone());
        match host.handle_read(stdout.clone(), 65536) {
            Ok(Some(bytes)) => echoed.extend(bytes),
            Err(Failure::WouldBlock) => thread::sleep(Duration::from_millis(1)),
            other => panic!("the echo ended early: {other:?}"),
        }
    }
    assert!(
        echoed == message,
        "the message is echoed whole and in order"
    );
    assert_eq!(host.handle_flush(stdin.clone()), Ok(()));

    host.handle_close(stdin);
    assert!(awaited(&host, &child, true, false));
    assert_eq!(host.proc_wait(child), Ok(ChildExit::Code(0)));
    host.handle_close(stdout);
}

/// A flush that meets a reader gone answers the broken pipe once and drops what was held, so the flush after it answers that nothing is held.
#[test]
fn a_drain_that_fails_is_answered_once_and_drops_what_was_held() {
    let host = OsHost::with_args(vec![]);
    let ChildHandles {
        child,
        stdin: Some(stdin),
        stdout: None,
        stderr: None,
    } = host
        .proc_spawn(
            vec![b"/bin/sleep".to_vec(), b"0.2".to_vec()],
            vec![],
            vec![],
            StdioMode::Pipe,
            StdioMode::Null,
            StdioMode::Null,
        )
        .unwrap()
    else {
        panic!("the piped stream, and only it, is answered");
    };

    assert_eq!(
        host.handle_write(stdin.clone(), vec![b'x'; 1 << 20]),
        Err(Failure::WouldBlock)
    );
    assert_eq!(host.handle_flush(stdin.clone()), Err(Failure::WouldBlock));
    assert!(awaited(&host, &child, true, false));
    assert_eq!(host.proc_wait(child), Ok(ChildExit::Code(0)));

    // Another test's child may have been forked while the read end was open and hold it until its `exec`, so the broken pipe may arrive a few flushes late as `WouldBlock`.
    let mut flushed = host.handle_flush(stdin.clone());
    for _ in 0..100 {
        if flushed != Err(Failure::WouldBlock) {
            break;
        }
        thread::sleep(Duration::from_millis(10));
        flushed = host.handle_flush(stdin.clone());
    }
    assert_eq!(flushed, Err(Failure::BrokenPipe));
    assert_eq!(host.handle_flush(stdin.clone()), Ok(()));

    host.handle_close(stdin);
}

/// A real child end to end: `echo` is spawned with its output piped, its handle becomes readable once the reaper has recorded the exit, `proc_wait` reports a clean zero, and the piped output is what it wrote. The inherited stdin and the null stderr have no handle.
#[test]
fn a_child_is_reaped_through_its_handle_and_its_piped_output_read() {
    let host = OsHost::with_args(vec![]);
    let ChildHandles {
        child,
        stdin: None,
        stdout: Some(stdout),
        stderr: None,
    } = host
        .proc_spawn(
            vec![b"/bin/echo".to_vec(), b"hi".to_vec()],
            vec![],
            vec![],
            StdioMode::Inherit,
            StdioMode::Pipe,
            StdioMode::Null,
        )
        .unwrap()
    else {
        panic!("the piped stream, and only it, is answered");
    };

    assert!(awaited(&host, &child, true, false));
    assert_eq!(host.proc_wait(child), Ok(ChildExit::Code(0)));
    assert_eq!(
        host.handle_read(stdout.clone(), 64),
        Ok(Some(b"hi\n".to_vec()))
    );

    host.handle_close(stdout);
}

/// Spawn `argv` with every stream on the null device, and the child's handle.
fn quiet(host: &OsHost, argv: &[&[u8]]) -> Result<Handle, Failure> {
    host.proc_spawn(
        argv.iter().map(|arg| arg.to_vec()).collect(),
        vec![],
        vec![],
        StdioMode::Null,
        StdioMode::Null,
        StdioMode::Null,
    )
    .map(|spawned| spawned.child)
}

/// Wait up to five seconds for `child`'s handle to report that its end is recorded.
fn ended(host: &OsHost, child: &Handle) {
    assert!(awaited(host, child, true, false), "the child ended");
}

/// Once the reaper has recorded an exit it has also reaped the pid, which the OS may hand to another process, so a kill answers `ok` without signaling anything — and the exit still reads back. A kill that signaled the freed pid would answer `ESRCH` here, or end whatever process had inherited it.
#[test]
fn killing_a_child_that_has_ended_signals_nothing() {
    let host = OsHost::with_args(vec![]);
    let child = quiet(&host, &[b"/bin/sh", b"-c", b"exit 0"]).unwrap();

    ended(&host, &child);
    assert_eq!(host.proc_kill(child.clone()), Ok(()));
    assert_eq!(host.proc_wait(child), Ok(ChildExit::Code(0)));
}

#[test]
fn killing_a_running_child_ends_it_by_its_signal() {
    let host = OsHost::with_args(vec![]);
    let child = quiet(&host, &[b"/bin/sleep", b"60"]).unwrap();

    assert_eq!(host.proc_wait(child.clone()), Err(Failure::WouldBlock));
    assert_eq!(host.proc_kill(child.clone()), Ok(()));
    ended(&host, &child);
    assert_eq!(
        host.proc_wait(child),
        Ok(ChildExit::Signal(NonZeroU32::new(9).unwrap()))
    );
}

/// An empty `argv` names no program, a NUL fits in no C string, and an environment entry needs a name before its `=`: each is `EINVAL`, as `execve` and `setenv` answer, and none starts anything.
#[test]
fn spawning_refuses_what_no_program_can_be() {
    const EINVAL: u32 = 22;

    let host = OsHost::with_args(vec![]);
    let spawn = |argv: Vec<Vec<u8>>, env: Vec<Vec<u8>>| {
        host.proc_spawn(
            argv,
            vec![],
            env,
            StdioMode::Null,
            StdioMode::Null,
            StdioMode::Null,
        )
    };
    let refused = Err(Failure::Other(EINVAL));

    assert_eq!(spawn(vec![], vec![]), refused);
    assert_eq!(spawn(vec![b"/bin/e\0cho".to_vec()], vec![]), refused);
    assert_eq!(
        spawn(vec![b"/bin/echo".to_vec()], vec![b"=x".to_vec()]),
        refused
    );
    assert_eq!(spawn(vec![b"/bin/echo".to_vec()], vec![vec![]]), refused);
}
