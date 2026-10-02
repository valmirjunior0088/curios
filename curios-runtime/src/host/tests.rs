//! What a serial open takes before any device is touched: the control modes a frame is written in, and the speeds a setting holds.

use {super::*, rustix::termios::ControlModes};

#[test]
fn a_serial_frame_is_the_control_modes_that_set_it() {
    assert_eq!(
        serial_frame(8, SerialParity::None, 1, SerialFlow::None),
        Some(ControlModes::CS8)
    );
    assert_eq!(
        serial_frame(8, SerialParity::Odd, 1, SerialFlow::None),
        Some(ControlModes::CS8 | ControlModes::PARENB | ControlModes::PARODD)
    );
    assert_eq!(
        serial_frame(7, SerialParity::Even, 2, SerialFlow::Hardware),
        Some(
            ControlModes::CS7 | ControlModes::PARENB | ControlModes::CSTOPB | ControlModes::CRTSCTS
        )
    );
}

#[test]
fn a_serial_frame_outside_the_row_s_ranges_is_refused() {
    for data_bits in [0, 5, 6, 9] {
        assert_eq!(
            serial_frame(data_bits, SerialParity::None, 1, SerialFlow::None),
            None
        );
    }

    for stop_bits in [0, 3] {
        assert_eq!(
            serial_frame(8, SerialParity::None, stop_bits, SerialFlow::None),
            None
        );
    }
}

// Zero is a terminal's word for hanging the line up, and a speed setting is 32 bits wide.
#[test]
fn a_serial_speed_is_neither_zero_nor_past_a_speed_setting() {
    assert_eq!(serial_speed(0), None);
    assert_eq!(serial_speed(1), Some(1));
    assert_eq!(serial_speed(115_200), Some(115_200));
    assert_eq!(serial_speed(u64::from(u32::MAX)), Some(u32::MAX));
    assert_eq!(serial_speed(u64::from(u32::MAX) + 1), None);
}
