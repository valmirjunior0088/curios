//! What a Linux pseudo-terminal alone answers for: a frame the device did not take refused at the open, and a drain answered once the output has left. A macOS pseudo-terminal keeps no frame, and its `tcdrain` can wait for good.

use {
    super::{
        test_support::{come, pseudo_terminal},
        *,
    },
    crate::{Failure, Handle, SerialFlow, SerialParity},
    curios_abi::errno,
    rustix::termios::ControlModes,
};

/// The open reads back what the device took. A Linux pseudo-terminal keeps two stop bits, hardware flow control and its speed, and forces eight data bits with no parity, so the first frame opens with its modes set and the second is refused as one the device did not take.
#[test]
fn a_serial_open_is_refused_a_frame_the_device_did_not_take() {
    let (_near, name) = pseudo_terminal();
    let host = OsHost::with_args(vec![]);

    let port = host
        .serial_open(
            name.clone(),
            9600,
            8,
            SerialParity::None,
            2,
            SerialFlow::Hardware,
        )
        .unwrap();

    let termios = host
        .with_fd(&port, |fd| tcgetattr(fd))
        .expect("a filed descriptor")
        .expect("its settings");
    assert!(
        termios
            .control_modes
            .contains(ControlModes::CSTOPB | ControlModes::CRTSCTS)
    );
    assert_eq!(termios.output_speed(), 9600);
    host.handle_close(port);

    assert_eq!(
        host.serial_open(name, 9600, 7, SerialParity::Even, 1, SerialFlow::None),
        Err(Failure::Other(errno::EINVAL))
    );
}

/// A drain answers once the port has sent what it accepted — at once on a pseudo-terminal, whose far end has no wire to wait on — with the output arrived rather than discarded. Only a port has output to wait for. Linux alone, since `tcdrain` on a macOS pseudo-terminal can wait for good.
#[test]
fn a_serial_drain_answers_once_the_output_has_left() {
    let (near, name) = pseudo_terminal();

    let host = OsHost::with_args(vec![]);
    let port = host
        .serial_open(name, 115_200, 8, SerialParity::None, 1, SerialFlow::None)
        .unwrap();

    assert_eq!(host.handle_write(port.clone(), b"ok".to_vec()), Ok(2));
    assert_eq!(come(&host, host.serial_drain(port.clone())), Ok(()));

    let mut arrived = [0; 8];
    assert_eq!(rustix::io::read(&near, &mut arrived), Ok(2));
    assert_eq!(&arrived[..2], b"ok");

    host.handle_close(port.clone());
    assert_eq!(come(&host, host.serial_drain(port)), Err(Failure::NotFound));
    assert_eq!(
        come(&host, host.serial_drain(Handle::Stdin)),
        Err(Failure::NotFound)
    );
}
