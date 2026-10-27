//! The scripted terminal and serial ports: sizes and keys as scripted, the devices listed, and a port's speed, discard and drain.

use {
    super::{
        CONTROLLING_TERMINAL, ENOTTY, ENXIO, MockHost,
        test_support::{come, watched},
    },
    crate::{Failure, Handle, Mode, SerialFlow, SerialOp, SerialParity, TtySize},
    curios_abi::{HostOps, errno},
};

/// The controlling terminal a scripted host answers at `/dev/tty`.
fn terminal(host: &MockHost) -> Handle {
    host.file_open(CONTROLLING_TERMINAL.to_vec(), Mode::ReadWrite)
        .expect("a scripted terminal")
}

#[test]
fn terminal_sizes_advance_on_queries_and_repeat_the_last() {
    let (host, io) = MockHost::builder().tty_sizes([(12, 5), (16, 6)]).build();
    let tty = terminal(&host);

    assert_eq!(host.tty_take(tty.clone(), b"leave".to_vec()), Ok(()));
    assert_eq!(
        host.tty_size(tty.clone()),
        Ok(TtySize { cols: 12, rows: 5 })
    );
    assert_eq!(host.tty_give(tty.clone()), Ok(()));
    for _ in 0..3 {
        assert_eq!(
            host.tty_size(tty.clone()),
            Ok(TtySize { cols: 16, rows: 6 })
        );
    }
    assert_eq!(io.raw_modes(), [true, false]);
    assert_eq!(io.output(), b"leave");
}

#[test]
fn a_fixed_terminal_size_repeats_and_an_empty_script_has_no_terminal() {
    let (host, _) = MockHost::builder().tty_size(20, 5).build();
    let tty = terminal(&host);
    for _ in 0..3 {
        assert_eq!(
            host.tty_size(tty.clone()),
            Ok(TtySize { cols: 20, rows: 5 })
        );
    }

    let (host, io) = MockHost::builder().tty_size(20, 5).tty_sizes([]).build();
    assert_eq!(
        host.file_open(CONTROLLING_TERMINAL.to_vec(), Mode::ReadWrite),
        Err(Failure::Other(ENXIO))
    );
    assert_eq!(host.tty_size(Handle::Stdin), Err(Failure::Other(ENOTTY)));
    assert_eq!(
        host.tty_take(Handle::Stdin, Vec::new()),
        Err(Failure::Other(ENOTTY))
    );
    assert!(io.raw_modes().is_empty());
}

/// The scripted terminal is the device behind both standard streams: it reads the scripted keys and writes where standard output does, and a host dropped with it still taken gives it back, writing its leave bytes, as a trap or an `exit` drops the native host.
#[test]
fn the_terminal_reads_the_scripted_keys_and_a_dropped_host_gives_it_back() {
    let (host, io) = MockHost::builder()
        .tty_size(20, 5)
        .stdin_chunks(vec![b"q".to_vec()])
        .build();
    let tty = terminal(&host);

    assert_eq!(host.handle_read(tty.clone(), 8), Ok(Some(b"q".to_vec())));
    assert_eq!(host.handle_write(tty.clone(), b"frame".to_vec()), Ok(5));
    assert_eq!(host.tty_take(tty, b"/leave".to_vec()), Ok(()));
    drop(host);

    assert_eq!(io.output(), b"frame/leave");
    assert_eq!(io.raw_modes(), [true, false]);
}

// An input discard on a scripted serial port drops the chunk that has arrived, while the one still to come survives it.
#[test]
fn a_serial_discard_drops_only_what_arrived() {
    let (host, _io) = MockHost::builder()
        .serial([("/dev/ttyUSB0", vec!["banner", "ready"])])
        .build();

    let port = host
        .serial_open(
            b"/dev/ttyUSB0".to_vec(),
            9600,
            8,
            SerialParity::None,
            1,
            SerialFlow::None,
        )
        .unwrap();

    assert_eq!(
        host.serial_control(port.clone(), SerialOp::DiscardInput, false),
        Ok(())
    );
    assert_eq!(host.handle_read(port.clone(), 16), Err(Failure::WouldBlock));
    assert!(watched(&host, &port, true, false));
    assert_eq!(host.handle_read(port, 16), Ok(Some(b"ready".to_vec())));
}

// The scripted devices are what the host has: listed in byte order, whatever order they were scripted in, and a host scripted with none lists none.
#[test]
fn the_serial_devices_listed_are_the_scripted_ones_in_byte_order() {
    let (host, _io) = MockHost::builder()
        .serial([
            ("/dev/ttyUSB1", Vec::<&str>::new()),
            ("/dev/ttyS0", Vec::<&str>::new()),
        ])
        .build();

    assert_eq!(
        host.serial_list(),
        Ok(vec![b"/dev/ttyS0".to_vec(), b"/dev/ttyUSB1".to_vec()])
    );

    let (bare, _io) = MockHost::builder().build();

    assert_eq!(bare.serial_list(), Ok(vec![]));
}

// A speed no setting holds is refused before the device is looked for, as the native host refuses it.
#[test]
fn a_serial_open_refuses_a_speed_no_setting_holds() {
    let (host, io) = MockHost::builder()
        .serial([("/dev/ttyUSB0", Vec::<&str>::new())])
        .build();

    for baud in [0, u64::from(u32::MAX) + 1] {
        assert_eq!(
            host.serial_open(
                b"/dev/ttyUSB0".to_vec(),
                baud,
                8,
                SerialParity::None,
                1,
                SerialFlow::None
            ),
            Err(Failure::Other(errno::EINVAL))
        );
    }

    assert_eq!(io.serial_opens(), []);
}

// A scripted port takes every write whole, so its drain is over once looked at, and the port is recorded as drained. A handle that is not a port has nothing to drain.
#[test]
fn a_serial_drain_is_over_once_looked_at() {
    let (host, io) = MockHost::builder()
        .serial([("/dev/ttyUSB0", Vec::<&str>::new())])
        .build();

    let port = host
        .serial_open(
            b"/dev/ttyUSB0".to_vec(),
            9600,
            8,
            SerialParity::None,
            1,
            SerialFlow::None,
        )
        .unwrap();

    assert_eq!(come(&host, host.serial_drain(port)), Ok(()));
    assert_eq!(io.serial_drains(), [b"/dev/ttyUSB0".to_vec()]);

    let file = host.file_open(b"f".to_vec(), Mode::Write).unwrap();
    assert_eq!(come(&host, host.serial_drain(file)), Err(Failure::NotFound));
    assert_eq!(
        come(&host, host.serial_drain(Handle::Stdin)),
        Err(Failure::NotFound)
    );
}

// A file and a standard stream have no lines to drive, and are `NotFound` as the native host answers them.
#[test]
fn a_control_on_a_handle_that_is_not_a_port_is_not_found() {
    let (host, io) = MockHost::builder().build();
    let file = host.file_open(b"f".to_vec(), Mode::Write).unwrap();

    for handle in [file, Handle::Stdin] {
        assert_eq!(
            host.serial_control(handle, SerialOp::Dtr, true),
            Err(Failure::NotFound)
        );
    }

    assert_eq!(io.serial_controls(), []);
}
