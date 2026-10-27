//! Streams and their readiness: pipes, FIFOs, the null device and the standard streams, a read and a write within what was asked, and a reply to come.

use {
    super::test_support::{awaited, loopback_pair, watched},
    super::*,
    crate::{Failure, Handle, Mode},
    curios_abi::{errno, event},
    curios_utilities::test_support::Temporary,
    rustix::fs::OFlags,
    std::{path::Path, thread, time::Duration},
};

/// The stdin gate, exercised on a pipe of its own: an empty pipe is not readable, a written byte makes it readable, and so does the writer's close, since a read then answers the end of the stream at once.
#[test]
fn readable_now_follows_a_pipe_end_as_its_writer_fills_and_closes_it() {
    let (mut reader, mut writer) = std::io::pipe().expect("a pipe");

    assert!(!readable_now(reader.as_fd()));
    writer.write_all(b"x").expect("the pipe takes a byte");
    assert!(readable_now(reader.as_fd()));

    let mut byte = [0u8; 1];
    reader.read_exact(&mut byte).expect("the byte is there");
    assert!(!readable_now(reader.as_fd()));

    // A child another test spawns while this pipe's close-on-exec flag is being set — macOS has no `pipe2`, so `std` sets it after the pipe exists — holds the write end until it exits or is killed, so the end of the stream may arrive late rather than at once.
    drop(writer);
    let closed = (0..100).any(|_| {
        let readable = readable_now(reader.as_fd());

        if !readable {
            thread::sleep(Duration::from_millis(10));
        }

        readable
    });
    assert!(closed);
}

/// The gates tell a descriptor of every kind ready as it is, the null device included, which macOS's poll cannot watch, so its gates wait on it with `select`: the null device reads its end and takes a write at once, so it is ready both ways.
#[test]
fn the_null_device_is_ready_to_read_and_to_write() {
    let reader = File::open("/dev/null").expect("the null device opens to read");
    let writer = OpenOptions::new()
        .write(true)
        .open("/dev/null")
        .expect("the null device opens to write");

    assert!(readable_now(reader.as_fd()));
    assert!(writable_now(writer.as_fd()));
}

/// A wait made with `select` finds what one made with a poll finds, so the descriptors a poll cannot watch are waited on alike: a pipe's write end is ready to write, and its read end ready to read once a byte is there and not before.
#[test]
fn a_select_finds_a_pipe_ready_as_a_poll_does() {
    let (reader, writer) = std::io::pipe().expect("a pipe");
    let watched = [
        Watched {
            fd: reader.as_fd(),
            asked: PollFlags::IN,
        },
        Watched {
            fd: writer.as_fd(),
            asked: PollFlags::OUT,
        },
    ];
    let now = || Some(Instant::now());

    assert_eq!(selected(&watched, now()), Ok(vec![1]));
    assert_eq!(ready(&watched, now()), Ok(vec![1]));

    rustix::io::write(&writer, b"x").expect("the pipe takes a byte");
    assert_eq!(selected(&watched, now()), Ok(vec![0, 1]));
    assert_eq!(ready(&watched, now()), Ok(vec![0, 1]));
}

/// The standard output gate, exercised on a pipe of its own: a pipe with room takes a write, one its writer has filled does not until its reader drains it, and a pipe's writes are bounded where a device's are not.
#[test]
fn writable_now_follows_a_pipe_end_as_its_reader_drains_it() {
    let (mut reader, writer) = std::io::pipe().expect("a pipe");

    assert!(writable_now(writer.as_fd()));
    assert!(pipe_or_socket(writer.as_fd()));
    let device = std::fs::File::open("/dev/null").expect("the null device");
    assert!(!pipe_or_socket(device.as_fd()));

    // Filled through a write end switched non-blocking, so the fill stops where the pipe is full instead of waiting there.
    rustix::fs::fcntl_setfl(&writer, OFlags::NONBLOCK).expect("the write end switched");
    let chunk = [0u8; 4096];
    while rustix::io::write(&writer, &chunk).is_ok() {}
    assert!(!writable_now(writer.as_fd()));

    let mut drained = vec![0u8; 1 << 20];
    let read = reader.read(&mut drained).expect("the pipe drains");
    assert!(read > 0);
    assert!(writable_now(writer.as_fd()));
}

/// A reply filed for later is waited on as any handle is: it is not ready while the work it waits for runs elsewhere, the wait sleeps until that work wakes it, and it is read back once, naming nothing after.
#[test]
fn a_reply_to_come_wakes_the_wait_once_it_has_come() {
    let host = OsHost::with_args(vec![]);
    let (signal, signalled) = std::sync::mpsc::channel::<()>();
    let work = blocking(move || {
        signalled.recv().unwrap();

        7u64
    });

    let token = host
        .later(Box::pin(async move { Box::new(work.await) as Settled }))
        .expect_err("the work waits for its signal");
    assert_eq!(host.handle_ready(vec![token.clone()]), [0]);
    assert!(host.settled(token.clone()).unwrap().is_none());

    signal.send(()).unwrap();
    host.wait(vec![token.clone()]).unwrap();
    assert_eq!(host.handle_ready(vec![token.clone()]), [event::READY]);

    let settled = host.settled(token.clone()).unwrap().expect("it has come");
    assert_eq!(*settled.downcast::<u64>().unwrap(), 7);
    assert!(host.settled(token).is_err());
}

/// Closing a reply still to come drops its future, which is how a call cancelled while it waits lets go of what it waited on.
#[test]
fn closing_a_reply_to_come_drops_its_future() {
    struct Held(Arc<AtomicBool>);

    impl Drop for Held {
        fn drop(&mut self) {
            self.0.store(true, Ordering::Release);
        }
    }

    let host = OsHost::with_args(vec![]);
    let dropped = Arc::new(AtomicBool::new(false));
    let held = Held(Arc::clone(&dropped));

    let token = host
        .later(Box::pin(async move {
            let _held = held;
            future::pending::<()>().await;

            Box::new(()) as Settled
        }))
        .expect_err("a reply that never comes is filed");
    assert!(!dropped.load(Ordering::Acquire));

    host.handle_close(token);
    assert!(dropped.load(Ordering::Acquire));
}

/// The standard streams are open one way each, and using one the other way is `EBADF` — the operating system's own, which is the one the hosts without one answer in its place — as it is for any descriptor opened one way: never an invented end of stream, and never a write into a terminal the program only reads.
#[test]
fn a_standard_stream_is_open_one_way() {
    let host = OsHost::with_args(vec![]);

    assert_eq!(
        host.handle_read(Handle::Stdout, 8),
        Err(Failure::Other(errno::EBADF))
    );
    assert_eq!(
        host.handle_read(Handle::Stderr, 0),
        Err(Failure::Other(errno::EBADF))
    );
    assert_eq!(
        host.handle_write(Handle::Stdin, b"x".to_vec()),
        Err(Failure::Other(errno::EBADF))
    );
    assert_eq!(
        host.handle_write(Handle::Stdin, vec![]),
        Err(Failure::Other(errno::EBADF))
    );
}

/// A request for nothing moves nothing: a read of zero bytes answers empty bytes, never the end of a stream it did not look at, and an empty write answers `0` — each only after checking the handle is a stream at all.
#[test]
fn a_request_for_nothing_checks_the_handle_and_moves_nothing() {
    let host = OsHost::with_args(vec![]);
    let null = host.file_open(b"/dev/null".to_vec(), Mode::Read).unwrap();
    let unknown = Handle::Other(vec![0xff, 0xff]);

    assert_eq!(host.handle_read(null.clone(), 0), Ok(Some(vec![])));
    assert_eq!(host.handle_read(null.clone(), 8), Ok(None));
    assert_eq!(host.handle_read(unknown.clone(), 0), Err(Failure::NotFound));
    assert_eq!(host.handle_write(Handle::Stdout, vec![]), Ok(0));
    assert_eq!(host.handle_write(unknown, vec![]), Err(Failure::NotFound));

    host.handle_close(null);
}

/// One read allocates for at most its cap, however much it is asked for, and answers the prefix that fits — which the row allows, since a read may answer fewer bytes than requested.
#[test]
fn a_read_takes_at_most_its_cap() {
    let host = OsHost::with_args(vec![]);
    let dir = Temporary::new("runtime-read", "cap");
    fs::create_dir(&dir).expect("a fresh temporary directory");
    let path = dir.join("large");
    fs::write(&path, vec![b'x'; 100 * 1024]).expect("a file past the cap");

    let file = host
        .file_open(path.into_os_string().into_vec(), Mode::Read)
        .unwrap();
    let read = host.handle_read(file.clone(), u64::MAX).unwrap().unwrap();

    assert_eq!(read.len() as u64, READ_MAX);
    host.handle_close(file);
}

/// A watch is ready for what it asks and no other: a socket with a byte to read is ready to read and to write, and its peer, sent nothing, is ready to write and not to read.
#[test]
fn a_watch_is_ready_for_what_it_asks() {
    let host = OsHost::with_args(vec![]);
    let (listener, client, server) = loopback_pair(&host);

    assert_eq!(host.handle_write(client.clone(), b"x".to_vec()), Ok(1));

    // The byte is waited for first, since it may not have crossed the loopback yet.
    assert!(awaited(&host, &server, true, false));
    assert!(watched(&host, &server, false, true));
    assert!(
        !watched(&host, &client, true, false),
        "nothing was sent to the client"
    );
    assert!(watched(&host, &client, false, true));

    host.handle_close(server);
    host.handle_close(client);
    host.handle_close(listener);
}

/// A write accepts no more than it was offered, and at least a byte of a buffer that holds one.
#[test]
fn a_write_accepts_within_what_it_was_offered() {
    let host = OsHost::with_args(vec![]);
    let dir = Temporary::new("runtime-write", "accepted");
    fs::create_dir(&dir).expect("a fresh temporary directory");

    let file = host
        .file_open(dir.join("out").into_os_string().into_vec(), Mode::Write)
        .unwrap();
    let accepted = host.handle_write(file.clone(), vec![b'x'; 4096]).unwrap();

    assert!((1..=4096).contains(&accepted));
    host.handle_close(file);
}

/// A look answers a byte for each handle, in their order: a deadline at the clock's origin is ready and one an hour on is not, and a handle that is neither a watch nor a deadline, which nothing would hold, is ready at once.
#[test]
fn a_look_answers_each_handle_in_its_order() {
    let host = OsHost::with_args(vec![]);
    let now = host.clock_mono().unwrap();
    let (passed, later) = (
        host.clock_deadline(0, 0),
        host.clock_deadline(now.secs + 3_600, 0),
    );

    assert_eq!(
        host.handle_ready(vec![later.clone(), passed.clone(), Handle::Stderr]),
        [0, event::READY, event::READY]
    );

    host.handle_close(passed);
    host.handle_close(later);
}

/// A FIFO at a fresh path under `dir`, made by the system's own tool, which every platform the host runs on has.
fn fifo(dir: &Path) -> Vec<u8> {
    let path = dir.join("fifo");
    let made = std::process::Command::new("mkfifo")
        .arg(&path)
        .status()
        .expect("mkfifo runs");
    assert!(made.success(), "mkfifo made {}", path.display());

    path.into_os_string().into_vec()
}

/// A FIFO opened to read opens with no writer, and reads nothing, rather than its end, until a writer has opened it: a watch of it is ready once the writer's bytes are there, and its end is read once the writer has closed.
#[test]
fn a_fifo_opened_to_read_waits_for_a_writer() {
    let host = OsHost::with_args(vec![]);
    let dir = Temporary::new("runtime-fifo", "read");
    fs::create_dir(&dir).expect("a fresh temporary directory");
    let path = fifo(&dir);

    let reader = host.file_open(path.clone(), Mode::Read).unwrap();
    assert_eq!(
        host.handle_read(reader.clone(), 8),
        Err(Failure::WouldBlock)
    );
    assert!(!watched(&host, &reader, true, false));

    let mut writer = fs::OpenOptions::new()
        .write(true)
        .open(OsStr::from_bytes(&path))
        .expect("the reader holds the FIFO open");
    writer.write_all(b"ok").expect("the FIFO takes two bytes");
    assert!(awaited(&host, &reader, true, false));
    assert_eq!(
        host.handle_read(reader.clone(), 8),
        Ok(Some(b"ok".to_vec()))
    );

    drop(writer);
    assert!(awaited(&host, &reader, true, false));
    assert_eq!(host.handle_read(reader.clone(), 8), Ok(None));

    host.handle_close(reader);
}

/// A FIFO opened to write with no reader is refused at once with the system's `ENXIO`, rather than waiting for a reader to open it.
#[test]
fn a_fifo_opened_to_write_with_no_reader_is_refused() {
    const ENXIO: u32 = 6;

    let host = OsHost::with_args(vec![]);
    let dir = Temporary::new("runtime-fifo", "write");
    fs::create_dir(&dir).expect("a fresh temporary directory");

    assert_eq!(
        host.file_open(fifo(&dir), Mode::Write),
        Err(Failure::Other(ENXIO))
    );
}

/// A device opened by path is served as a stream: the null device reads its end at once and takes every write whole.
#[test]
fn the_null_device_reads_its_end_and_takes_every_write() {
    let host = OsHost::with_args(vec![]);
    let reader = host.file_open(b"/dev/null".to_vec(), Mode::Read).unwrap();
    let writer = host.file_open(b"/dev/null".to_vec(), Mode::Write).unwrap();

    assert_eq!(host.handle_read(reader.clone(), 8), Ok(None));
    assert_eq!(host.handle_write(writer.clone(), b"gone".to_vec()), Ok(4));

    host.handle_close(reader);
    host.handle_close(writer);
}

/// A standard stream the program closed names nothing, as a closed handle does: a read answers `NotFound`, and a watch of it is ready at once, with no descriptor left for a poll to watch. Only standard input is closed here, since closing standard output or error would close the test runner's own; the end its reader reads is held through a program of its own.
#[test]
fn a_closed_standard_input_names_nothing() {
    let host = OsHost::with_args(vec![]);

    host.handle_close(Handle::Stdin);

    assert_eq!(host.handle_read(Handle::Stdin, 8), Err(Failure::NotFound));
    let watch = host.handle_watch(Handle::Stdin, true, false);
    assert_eq!(host.handle_ready(vec![watch]), [event::READY]);
}
