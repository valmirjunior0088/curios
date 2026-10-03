//! Serial ports of the native host: the devices the system has, and one opened raw at its speed and frame, its modem lines driven, its unread input dropped, and its output waited for on a thread of its own.
//!
//! **A port is filed as a resource of its own**, not as the bare descriptor a pipe to a child is, although the stream rows serve both alike. What only a port can do is then decided by the kind the table holds: a file, a pipe or a socket handed to `serial_control` is `NotFound`, as it is on every host, where asking the kernel would answer whichever errno that descriptor's driver reports.
//!
//! **An open sets every mode it means and checks what the device took.** A serial device keeps its settings from one open to the next on Linux, and what "raw" clears differs between the two release targets, so the input modes are set outright rather than edited: none of them, which leaves no software flow control, no parity checking and no break handling behind, on either target. A terminal's settings call succeeds when any part of it applied, so the frame and the speed are read back, and a device that took another is refused rather than opened at settings the program did not ask for.
//!
//! **Closing a port discards what it has not sent.** Left to itself, Linux holds the last close of a serial device until its output has drained — thirty seconds by default when the device takes none — whatever the descriptor's flags, and macOS discards the output of a non-blocking one instead; neither is a close that never waits and loses nothing. So the port drops its unsent output as it closes, which bounds the kernel's wait to what the hardware already holds, and the wait a program wants is `serial_drain`'s: `tcdrain` on a thread of its own, signalling a pipe the scheduler polls, as `os_child` reaps a child. One thread per drain under way is the native host's cost for a wait neither system offers without blocking.

#[cfg(target_os = "linux")]
use std::{collections::HashMap, io::ErrorKind, path::PathBuf};
#[cfg(not(target_os = "linux"))]
use std::{ffi::c_ulong, os::unix::ffi::OsStrExt};
use {
    super::{Failure, SerialOp, failure_from_error},
    rustix::{
        fs::{OFlags, fcntl_getfl, fcntl_setfl},
        io::{Errno, retry_on_intr},
        ioctl::{Opcode, Setter, ioctl},
        termios::{
            ControlModes, InputModes, OptionalActions, QueueSelector, Termios, tcdrain, tcflush,
            tcgetattr, tcsetattr,
        },
    },
    std::{
        ffi::c_int,
        fs,
        io::{Error, pipe},
        os::{
            fd::{AsFd, BorrowedFd, OwnedFd},
            unix::ffi::OsStringExt,
        },
        path::Path,
        thread,
    },
};

// The modem-line ioctls rustix does not wrap. Linux numbers its generic tty ioctls by hand, and both release architectures use these two; macOS keeps the BSD family's encoded ones, spelled as its header defines them, `_IOW('t', 108, int)` and `_IOW('t', 107, int)`. The line bits agree across all of them.
#[cfg(target_os = "linux")]
const TIOCMBIS: Opcode = 0x5416;
#[cfg(target_os = "linux")]
const TIOCMBIC: Opcode = 0x5417;
#[cfg(not(target_os = "linux"))]
const TIOCMBIS: Opcode = rustix::ioctl::opcode::write::<c_int>(b't', 108);
#[cfg(not(target_os = "linux"))]
const TIOCMBIC: Opcode = rustix::ioctl::opcode::write::<c_int>(b't', 107);
const TIOCM_DTR: c_int = 0x002;
const TIOCM_RTS: c_int = 0x004;

/// The control modes a frame is written in: what an open clears before setting its own, and what it reads back.
const FRAME: ControlModes = ControlModes::CSIZE
    .union(ControlModes::PARENB)
    .union(ControlModes::PARODD)
    .union(ControlModes::CSTOPB)
    .union(ControlModes::CRTSCTS);

/// Set `termios` on `fd` at `speed`. Linux takes any speed through the settings themselves.
#[cfg(target_os = "linux")]
fn apply(fd: &OwnedFd, termios: &mut Termios, speed: u32) -> rustix::io::Result<()> {
    termios.set_speed(speed)?;

    tcsetattr(fd, OptionalActions::Now, termios)
}

/// Set `termios` on `fd` at `speed`. Apple's serial driver takes through the settings only the speeds in its own table and answers `EINVAL` for any other, so a refusal is retried at a speed the table holds and the speed then set by `IOSSIOSPEED`, `_IOW('T', 2, speed_t)`, which takes any. A refusal that was the frame's is refused again by the retry.
#[cfg(not(target_os = "linux"))]
fn apply(fd: &OwnedFd, termios: &mut Termios, speed: u32) -> rustix::io::Result<()> {
    const IOSSIOSPEED: Opcode = rustix::ioctl::opcode::write::<c_ulong>(b'T', 2);

    termios.set_speed(speed)?;

    match tcsetattr(fd, OptionalActions::Now, termios) {
        Err(Errno::INVAL) => {
            termios.set_speed(9600)?;
            tcsetattr(fd, OptionalActions::Now, termios)?;

            // SAFETY: the opcode takes a pointer to a `speed_t`, an `unsigned long`, and writes nothing back, which is the opcode and input type `Setter` is built for.
            unsafe {
                ioctl(
                    fd,
                    Setter::<IOSSIOSPEED, c_ulong>::new(c_ulong::from(speed)),
                )
            }
        }
        outcome => outcome,
    }
}

/// The serial devices the system has, each by a path an open takes, sorted.
///
/// A terminal class entry is a serial device when a device stands behind it, which a console, a virtual terminal and a pseudo-terminal have none of, and when its driver found the port: the 8250 driver registers its legacy ports whether or not a chip answers, and reports one that did not as type `0`. A device udev named under `/dev/serial/by-id` is listed by that name, the first in byte order where it has several, and any other by its node.
#[cfg(target_os = "linux")]
pub(crate) fn serial_devices() -> std::io::Result<Vec<Vec<u8>>> {
    // Filed last to first, so the name left under a node is the first in byte order.
    let mut links = fs::read_dir("/dev/serial/by-id")
        .map(|links| links.flatten().map(|link| link.path()).collect::<Vec<_>>())
        .unwrap_or_default();
    links.sort();

    let mut named: HashMap<PathBuf, PathBuf> = HashMap::new();

    for link in links.into_iter().rev() {
        if let Ok(node) = fs::canonicalize(&link) {
            named.insert(node, link);
        }
    }

    let classes = match fs::read_dir("/sys/class/tty") {
        Ok(classes) => classes,
        Err(error) if error.kind() == ErrorKind::NotFound => return Ok(Vec::new()),
        Err(error) => return Err(error),
    };

    let mut devices = Vec::new();

    for class in classes {
        let class = class?;
        let entry = class.path();
        let node = Path::new("/dev").join(class.file_name());
        let unanswered = fs::read(entry.join("type")).is_ok_and(|kind| kind.trim_ascii() == b"0");

        if entry.join("device").exists() && !unanswered && node.exists() {
            let name = named.remove(&node).unwrap_or(node);

            devices.push(name.into_os_string().into_vec());
        }
    }

    devices.sort();

    Ok(devices)
}

/// The serial devices the system has, each by a path an open takes, sorted: the callout devices, `/dev/cu.*`, which open without waiting for carrier as their dial-in twins `/dev/tty.*` do not promise.
#[cfg(not(target_os = "linux"))]
pub(crate) fn serial_devices() -> std::io::Result<Vec<Vec<u8>>> {
    let mut devices = Vec::new();

    for entry in fs::read_dir("/dev")? {
        let name = entry?.file_name();

        if name.as_bytes().starts_with(b"cu.") {
            devices.push(Path::new("/dev").join(name).into_os_string().into_vec());
        }
    }

    devices.sort();

    Ok(devices)
}

/// An open serial port: the descriptor `serial_open` configured, non-blocking from the open itself, so a fiber draining it yields on `WouldBlock` as it does on a pipe to a child.
pub(crate) struct SerialPort(OwnedFd);

impl SerialPort {
    /// Open the device at `path` and set it raw at `speed`, with the control modes `frame` names; `EINVAL` when the device took another frame or speed than those.
    pub(crate) fn open(path: &[u8], speed: u32, frame: ControlModes) -> rustix::io::Result<Self> {
        // Non-blocking from the open rather than switched after it, because opening a port whose carrier line is down waits for carrier until `CLOCAL` is set, and `CLOCAL` is set on a descriptor already open. `NOCTTY` keeps the port from becoming this process's controlling terminal, and `CLOEXEC` keeps a spawned child from holding it past the program's own close. No exclusive hold is taken: whether `TIOCEXCL` refuses a second open differs by kernel, by device and by privilege, so the row promises only what every kernel does.
        let fd = rustix::fs::open(
            path,
            OFlags::RDWR | OFlags::NOCTTY | OFlags::NONBLOCK | OFlags::CLOEXEC,
            rustix::fs::Mode::empty(),
        )?;

        let mut termios = tcgetattr(&fd)?;

        termios.make_raw();
        termios.input_modes = InputModes::empty();
        termios.control_modes -= FRAME;
        termios.control_modes |= frame | ControlModes::CLOCAL | ControlModes::CREAD;

        apply(&fd, &mut termios, speed)?;

        let applied = tcgetattr(&fd)?;

        if applied.control_modes & FRAME != frame || applied.output_speed() != speed {
            return Err(Errno::INVAL);
        }

        Ok(Self(fd))
    }

    /// Apply `op`: a modem line set to the level `on`, or what the device sent and the program has not read dropped.
    pub(crate) fn control(&self, op: SerialOp, on: bool) -> rustix::io::Result<()> {
        match op {
            SerialOp::Dtr => self.set_modem_lines(TIOCM_DTR, on),
            SerialOp::Rts => self.set_modem_lines(TIOCM_RTS, on),
            SerialOp::DiscardInput => tcflush(&self.0, QueueSelector::IFlush),
        }
    }

    /// Start waiting for the port to send what it has accepted, and hand back the read end of a pipe that reaches its end when the wait is over — non-blocking, so a fiber parks on it. The thread waits on a duplicate of the descriptor, so the port may close under it: the close discards the unsent output, which is what the wait was for, and the thread ends.
    pub(crate) fn drain(&self) -> Result<OwnedFd, Failure> {
        let errno_failure = |errno: Errno| failure_from_error(Error::from(errno));

        let port = self.0.try_clone().map_err(failure_from_error)?;
        let (done, signal) = pipe().map_err(failure_from_error)?;
        let done = OwnedFd::from(done);

        let flags = fcntl_getfl(&done).map_err(errno_failure)?;
        fcntl_setfl(&done, flags | OFlags::NONBLOCK).map_err(errno_failure)?;

        // The wait's own outcome is not reported: a port that can no longer send says so to the write or the control that meets it next, and the pipe's end means only that there is nothing left to wait for.
        thread::Builder::new()
            .name("curios-drain".to_string())
            .spawn(move || {
                let _ = retry_on_intr(|| tcdrain(&port));

                drop(signal);
            })
            .map_err(failure_from_error)?;

        Ok(done)
    }

    /// Raise (`on`) or lower the modem lines `mask` names. `TIOCMBIS` and `TIOCMBIC` touch only those lines, where a `TIOCMGET` read back and a `TIOCMSET` of the lot would race whatever else drives the port between the two.
    fn set_modem_lines(&self, mask: c_int, on: bool) -> rustix::io::Result<()> {
        // SAFETY: both opcodes take a pointer to an `int` holding the line mask and write nothing back, which is the opcode and input type `Setter` is built for.
        unsafe {
            match on {
                true => ioctl(&self.0, Setter::<TIOCMBIS, c_int>::new(mask)),
                false => ioctl(&self.0, Setter::<TIOCMBIC, c_int>::new(mask)),
            }
        }
    }
}

/// Discard what the port accepted and has not sent, so the close that follows never waits on the wire.
impl Drop for SerialPort {
    fn drop(&mut self) {
        let _ = tcflush(&self.0, QueueSelector::OFlush);
    }
}

impl AsFd for SerialPort {
    fn as_fd(&self) -> BorrowedFd<'_> {
        self.0.as_fd()
    }
}
