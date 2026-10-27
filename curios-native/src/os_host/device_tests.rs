//! Terminals and serial ports: a terminal taken and given back with its settings, and a port opened raw, with its speed, frame and input modes, and its drain.

use {
    super::{
        test_support::{awaited, pseudo_terminal},
        *,
    },
    crate::{Failure, Handle, Mode, SerialFlow, SerialOp, SerialParity},
    curios_abi::errno,
    curios_utilities::test_support::Temporary,
    rustix::{
        fs::OFlags,
        termios::{ControlModes, InputModes, LocalModes},
    },
    std::{path::Path, thread, time::Duration},
};

/// A descriptor that is not a terminal refuses a take and a size through the errno lane, which is how a program learns it has none, and one never taken has nothing to give back. Which errno is the platform's own — Linux answers `ENOTTY`, macOS `ENODEV` — so what is under test is the lane rather than the code. `/dev/null` rather than a standard stream, because under an interactive `cargo test` stdin *is* a terminal and a passing `tty_take` would leave it in raw mode.
#[test]
fn the_tty_rows_on_a_descriptor_that_is_not_a_terminal_refuse_through_the_errno_lane() {
    #[cfg(target_os = "linux")]
    const NOT_A_TERMINAL: u32 = 25;
    #[cfg(not(target_os = "linux"))]
    const NOT_A_TERMINAL: u32 = 19;

    let host = OsHost::with_args(vec![]);
    let handle = host.file_open(b"/dev/null".to_vec(), Mode::Read).unwrap();

    assert_eq!(
        host.tty_take(handle.clone(), b"leave".to_vec()),
        Err(Failure::Other(NOT_A_TERMINAL))
    );
    assert_eq!(
        host.tty_size(handle.clone()),
        Err(Failure::Other(NOT_A_TERMINAL))
    );
    assert_eq!(host.tty_give(handle.clone()), Ok(()));

    host.handle_close(handle);
}

/// Whether the terminal behind `fd` reads lines and echoes them, as a terminal does before a program takes it.
fn cooked(fd: BorrowedFd<'_>) -> bool {
    tcgetattr(fd)
        .expect("its settings")
        .local_modes
        .contains(LocalModes::ICANON | LocalModes::ECHO)
}

/// Everything the near end of a pseudo-terminal has to read now, its far end having written it already.
fn written_to(near: &OwnedFd) -> Vec<u8> {
    let mut buffer = vec![0; 256];
    let read = rustix::io::read(near, &mut buffer).expect("the far end wrote");
    buffer.truncate(read);

    buffer
}

/// A terminal opened read and write by path is taken raw, and given back writes the leave bytes its take recorded before restoring the settings it had: the take's own bytes, and the second take's, which replaced them.
#[test]
fn a_taken_terminal_is_given_back_with_its_leave_bytes_and_its_settings() {
    let (near, name) = pseudo_terminal();
    let host = OsHost::with_args(vec![]);
    let far = host.file_open(name, Mode::ReadWrite).unwrap();
    let is_cooked = |host: &OsHost| host.with_fd(&far, cooked).expect("a filed descriptor");

    assert!(is_cooked(&host));
    assert_eq!(host.tty_take(far.clone(), b"first".to_vec()), Ok(()));
    assert!(!is_cooked(&host));
    assert_eq!(host.tty_take(far.clone(), b"\x1b[?1049l".to_vec()), Ok(()));
    assert_eq!(host.tty_give(far.clone()), Ok(()));
    assert!(is_cooked(&host));
    assert_eq!(written_to(&near), b"\x1b[?1049l");
    assert_eq!(host.tty_give(far.clone()), Ok(()));

    host.handle_close(far);
}

/// The pseudo-terminal the child half of [`a_terminating_signal_gives_back_every_terminal_taken`] takes, named by the variable its parent sets.
const SIGNALLED_TERMINAL: &str = "CURIOS_TEST_SIGNALLED_TERMINAL";

/// Bytes the near end of a pseudo-terminal reads within ten seconds, until they end with `marker`; a child that never writes it, or a far end that ends first, fails the test rather than holding it.
fn read_until(near: &OwnedFd, marker: &[u8]) -> Vec<u8> {
    let deadline = Instant::now() + Duration::from_secs(10);
    let mut seen = Vec::new();

    while !seen.ends_with(marker) {
        let watched = [Watched {
            fd: near.as_fd(),
            asked: PollFlags::IN,
        }];
        let ready = ready(&watched, Some(deadline)).expect("the near end is waited on");
        assert!(!ready.is_empty(), "nothing arrived in time; read {seen:?}");
        let read = written_to(near);
        assert!(!read.is_empty(), "the far end ended first; read {seen:?}");
        seen.extend(read);
    }

    seen
}

/// The far end of the pseudo-terminal named `name`, opened apart from any host. On macOS a terminal's last close discards what its near end has not read, so a test holds one, as a shell holds a program's terminal, to read what a host leaves.
fn far_end(name: &[u8]) -> fs::File {
    fs::OpenOptions::new()
        .read(true)
        .write(true)
        .custom_flags(OFlags::NOCTTY.bits().cast_signed())
        .open(OsStr::from_bytes(name))
        .expect("the far end opens")
}

/// A process ended by a terminating signal gives back every terminal it took first, then ends by that signal. A test cannot outlive the signal it sends itself, so its child is this test's own binary run again: given a pseudo-terminal by the variable above, it takes the terminal and waits, and the parent sends it SIGTERM and finds the terminal as the child found it, its leave bytes written.
#[test]
fn a_terminating_signal_gives_back_every_terminal_taken() {
    if let Some(name) = env::var_os(SIGNALLED_TERMINAL) {
        let host = OsHost::with_args(vec![]);
        let far = host.file_open(name.into_vec(), Mode::ReadWrite).unwrap();
        host.tty_take(far.clone(), b"\x1b[?1049l".to_vec()).unwrap();
        host.handle_write(far, b"taken".to_vec()).unwrap();
        loop {
            thread::sleep(Duration::from_secs(60));
        }
    }

    let (near, name) = pseudo_terminal();
    let held = far_end(&name);
    let mut child = std::process::Command::new(env::current_exe().unwrap())
        .args([
            "--exact",
            "os_host::device_tests::a_terminating_signal_gives_back_every_terminal_taken",
            "--nocapture",
        ])
        .env(SIGNALLED_TERMINAL, OsStr::from_bytes(&name))
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .spawn()
        .expect("the test runs again as a child");

    read_until(&near, b"taken");
    rustix::process::kill_process(
        rustix::process::Pid::from_child(&child),
        rustix::process::Signal::TERM,
    )
    .expect("the child is signalled");
    let status = child.wait().expect("the child ends");

    assert_eq!(
        std::os::unix::process::ExitStatusExt::signal(&status),
        Some(SIGTERM)
    );
    assert_eq!(read_until(&near, b"\x1b[?1049l"), b"\x1b[?1049l");
    assert!(cooked(held.as_fd()));
}

/// A host dropped with a terminal still taken gives it back, as a trap or an `exit` drops it: the leave bytes reach the terminal and its settings are the ones it had.
#[test]
fn a_dropped_host_gives_back_every_terminal_still_taken() {
    let (near, name) = pseudo_terminal();
    let held = far_end(&name);
    let host = OsHost::with_args(vec![]);
    let far = host.file_open(name, Mode::ReadWrite).unwrap();

    assert_eq!(host.tty_take(far, b"\x1b[?25h".to_vec()), Ok(()));
    drop(host);

    assert_eq!(read_until(&near, b"\x1b[?25h"), b"\x1b[?25h");
    assert!(cooked(held.as_fd()));
}

/// A path is the bytes the host holds, never decoded on the way: the name `dir_list` hands back opens as the file it names, exactly as `file_stat` finds it. The file is made with the standard library on the raw bytes, so the row under test is the only one asked to read them back. The name is the hardest one the filesystem takes — bytes that are not UTF-8 at all where they are allowed, and a non-ASCII name where they are not, since APFS refuses an undecodable one with `EILSEQ`.
#[test]
fn open_takes_a_listed_name_back_as_the_bytes_it_was_given() {
    #[cfg(target_os = "linux")]
    const NAME: &[u8] = b"\xffname.txt";
    #[cfg(not(target_os = "linux"))]
    const NAME: &[u8] = "café.txt".as_bytes();

    let host = OsHost::with_args(vec![]);
    let dir = Temporary::new("runtime-open", "listed-name");
    fs::create_dir(&dir).expect("a fresh temporary directory");
    let name = NAME;
    fs::write(dir.join(OsStr::from_bytes(name)), b"x").expect("a file under the raw name");

    let names = host.dir_list(dir.as_os_str().as_bytes().to_vec()).unwrap();
    assert_eq!(names, vec![name.to_vec()]);

    let path = dir.join(OsStr::from_bytes(&names[0]));
    let handle = host
        .file_open(path.into_os_string().into_vec(), Mode::Read)
        .unwrap();
    assert_eq!(host.handle_read(handle.clone(), 8), Ok(Some(b"x".to_vec())));

    host.handle_close(handle);
}

/// A serial port opened on the far end of a pseudo-terminal: a frame outside the row's ranges opens nothing, the open takes the raw frame, a byte the near end writes arrives through `handle_read` once a watch of it is ready, the input discard is served, and a closed port misses loudly. A pty forces its own character size and keeps no modem lines, so what is under test is the open's shape rather than a wire: Linux refuses the lines through the errno lane.
#[test]
fn a_serial_port_opens_raw_on_a_pseudo_terminal() {
    const EINVAL: u32 = 22;

    let (near, name) = pseudo_terminal();

    let host = OsHost::with_args(vec![]);
    let open = |data_bits| {
        host.serial_open(
            name.clone(),
            115_200,
            data_bits,
            SerialParity::None,
            1,
            SerialFlow::None,
        )
    };

    assert!(matches!(open(5), Err(Failure::Other(EINVAL))));

    let port = open(8).unwrap();

    let termios = host
        .with_fd(&port, |fd| tcgetattr(fd))
        .expect("a filed descriptor")
        .expect("its settings");
    assert!(
        termios
            .control_modes
            .contains(ControlModes::CS8 | ControlModes::CLOCAL | ControlModes::CREAD)
    );
    assert!(
        !termios
            .local_modes
            .intersects(LocalModes::ICANON | LocalModes::ECHO)
    );

    assert_eq!(host.handle_read(port.clone(), 8), Err(Failure::WouldBlock));
    rustix::io::write(&near, b"ok").expect("the near end writes");
    assert!(awaited(&host, &port, true, false));
    assert_eq!(host.handle_read(port.clone(), 8), Ok(Some(b"ok".to_vec())));

    assert_eq!(
        host.serial_control(port.clone(), SerialOp::DiscardInput, false),
        Ok(())
    );
    #[cfg(target_os = "linux")]
    assert_eq!(
        host.serial_control(port.clone(), SerialOp::Dtr, true),
        Err(Failure::Other(25))
    );

    host.handle_close(port.clone());
    assert_eq!(
        host.serial_control(port, SerialOp::DiscardInput, false),
        Err(Failure::NotFound)
    );
}

/// The listing names devices an open could be handed: in byte order, each a path that is there. What a machine has is its own, so the list itself is not pinned.
#[test]
fn the_serial_devices_listed_are_sorted_paths_that_exist() {
    let host = OsHost::with_args(vec![]);
    let devices = host.serial_list().unwrap();

    assert!(devices.is_sorted());

    for device in devices {
        let path = Path::new(OsStr::from_bytes(&device));

        assert!(path.exists(), "{} is listed and absent", path.display());
    }
}

/// A speed no setting holds opens nothing, as a frame outside the row's ranges opens nothing: zero is how a terminal hangs the line up, and a setting is 32 bits wide.
#[test]
fn a_serial_open_refuses_a_speed_no_setting_holds() {
    let (_near, name) = pseudo_terminal();
    let host = OsHost::with_args(vec![]);

    for baud in [0, u64::from(u32::MAX) + 1] {
        assert_eq!(
            host.serial_open(
                name.clone(),
                baud,
                8,
                SerialParity::None,
                1,
                SerialFlow::None
            ),
            Err(Failure::Other(errno::EINVAL))
        );
    }
}

/// What the port's last user left in its input modes is gone after an open: software flow control and parity checking set beforehand are cleared, so what the device sends arrives as it was sent whichever program held the port before. The far end is held open across the open, since a pseudo-terminal keeps its settings only while it is.
#[test]
fn a_serial_open_clears_the_input_modes_it_finds() {
    let (_near, name) = pseudo_terminal();
    let far = rustix::fs::open(
        name.as_slice(),
        OFlags::RDWR | OFlags::NOCTTY,
        rustix::fs::Mode::empty(),
    )
    .expect("the far end");

    let mut left = tcgetattr(&far).expect("its settings");
    left.input_modes |= InputModes::IXOFF | InputModes::INPCK;
    tcsetattr(&far, OptionalActions::Now, &left).expect("its settings set");

    let host = OsHost::with_args(vec![]);
    let port = host
        .serial_open(name, 115_200, 8, SerialParity::None, 1, SerialFlow::None)
        .unwrap();

    let termios = host
        .with_fd(&port, |fd| tcgetattr(fd))
        .expect("a filed descriptor")
        .expect("its settings");
    assert!(
        !termios
            .input_modes
            .intersects(InputModes::IXOFF | InputModes::INPCK)
    );
}

/// Only a serial port has lines to drive: a file and a standard stream are `NotFound` by the kind the host filed, whatever an `ioctl` on their descriptors would answer.
#[test]
fn a_control_on_a_handle_that_is_not_a_port_is_not_found() {
    let host = OsHost::with_args(vec![]);
    let file = host.file_open(b"/dev/null".to_vec(), Mode::Read).unwrap();

    for handle in [file, Handle::Stdin] {
        for op in [SerialOp::Dtr, SerialOp::Rts, SerialOp::DiscardInput] {
            assert_eq!(
                host.serial_control(handle.clone(), op, true),
                Err(Failure::NotFound)
            );
        }
    }
}
