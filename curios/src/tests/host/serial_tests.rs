//! A serial port through `/std/Serial`, against scripted devices: the frame an open asks for, a command written and a line read under the bracket, the lines and the input discard, the refusals, and the devices the host lists.
//!
//! The host's tags are spelled as numbers, since this crate names no `curios-abi` constant: parity none is 0 and even 1, flow control none is 0 and hardware 1, and the controls DTR, RTS and the input discard are 0, 1 and 2.

use {crate::tests::run_text, curios_runtime::MockHost};

// `config` asks for the frame nearly every device speaks, and a written `Config` reaches the host setting for setting.
#[test]
fn an_open_asks_the_host_for_the_configured_speed_and_frame() {
    let source = r#"
        use /std/{Show, Try, Io, Path, Serial};
        let port: Path = Path/of_str("/dev/ttyUSB0");
        let custom: Serial/Config =
            Serial/Config {
                baud = 9600,
                data_bits = 7,
                parity = Serial/Parity/even(),
                stop_bits = 2,
                flow = Serial/Flow/hardware(),
            };
        let open_close(c: Serial/Config) -> Try(Io, Io/Error, {}) =
            let s = Serial/open(port, c)!;
            let _ = Serial/close(s)!;
            Try/pure(());
        let program: Try(Io, Io/Error, {}) =
            let _ = open_close(Serial/config(115200))!;
            open_close(custom);
        match Try/run(program)!
        | success(_) => /std/print("closed")
        | failure(e) => /std/print(Show/show(e))
        end
        "#;

    let (system, io) = MockHost::builder()
        .serial([("/dev/ttyUSB0", Vec::<&str>::new())])
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"closed");
    assert_eq!(
        io.serial_opens(),
        vec![
            (b"/dev/ttyUSB0".to_vec(), [115_200, 8, 0, 1, 0]),
            (b"/dev/ttyUSB0".to_vec(), [9600, 7, 1, 2, 1]),
        ]
    );
}

// Under `with`, a command lands on the device whole and the reply is read as one line across two arrivals: the fiber parks between the carriage return and the newline, `poll` brings the second, and the line ending is dropped whole.
#[test]
fn with_writes_a_command_and_reads_its_reply_line_across_arrivals() {
    let source = r#"
        use /std/{Str, Bytes, Option, Show, Try, Async, Io, Path, Serial};
        let exchange(s: Serial) -> Try(Async, Io/Error, Bytes) =
            let _ = Try/attempt(Serial/write(s, Str/to_bytes("AT\r\n")))!;
            let line = Try/attempt(Async/read_line(s))!;
            Try/pure(Option/unwrap_or(line, x[]));
        let fiber: Async({}) =
            let r = Try/run(Serial/with(Path/of_str("/dev/ttyUSB0"), Serial/config(115200), exchange))!;
            match r
            | success(line) => Io/write(Io/stdout, line)
            | failure(e) => /std/print(Show/show(e))
            end;
        Async/run(fiber)
        "#;

    let (system, io) = MockHost::builder()
        .serial([("/dev/ttyUSB0", vec!["OK\r", "\n"])])
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"OK");
    assert_eq!(io.serial_written(b"/dev/ttyUSB0"), b"AT\r\n");
}

// The lines are driven in the order the program asks, and a discard once the board has booted drops its banner, so the first line read is the one that arrives after it.
#[test]
fn the_lines_are_driven_in_order_and_a_discard_drops_the_boot_banner() {
    let source = r#"
        use /std/{Bytes, Option, Show, Try, Async, Io, Path, Serial};
        let reset(s: Serial) -> Try(Async, Io/Error, Bytes) =
            let _ = Serial/set_dtr(s, false)!;
            let _ = Serial/set_rts(s, true)!;
            let _ = Serial/set_dtr(s, true)!;
            let _ = Serial/discard_input(s)!;
            let line = Try/attempt(Async/read_line(s))!;
            Try/pure(Option/unwrap_or(line, x[]));
        let fiber: Async({}) =
            let r = Try/run(Serial/with(Path/of_str("/dev/ttyUSB0"), Serial/config(115200), reset))!;
            match r
            | success(line) => Io/write(Io/stdout, line)
            | failure(e) => /std/print(Show/show(e))
            end;
        Async/run(fiber)
        "#;

    let (system, io) = MockHost::builder()
        .serial([("/dev/ttyUSB0", vec!["boot banner\r\n", "ready\r\n"])])
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"ready");
    assert_eq!(
        io.serial_controls(),
        vec![(0, false), (1, true), (0, true), (2, false)]
    );
}

// An unscripted device is `not_found`, and a frame outside the row's ranges is `other(22)` before any device is looked for.
#[test]
fn opens_are_refused_by_name() {
    let source = r#"
        use /std/{Str, Result, Show, Try, Async, Io, Path, Serial};
        let elsewhere: Path = Path/of_str("/dev/ttyACM9");
        let shown(@A: Type, r: Result(Io/Error, A)) -> Str =
            match r | success(_) => "ok" | failure(e) => Show/show(e) end;
        let fiber: Async({}) =
            let missing = Try/run(Serial/open(elsewhere, Serial/config(9600)))!;
            let framed = Try/run(Serial/open(elsewhere, Serial/Config { ..Serial/config(9600), data_bits = 5 }))!;
            /std/print(Str/join(" ", [shown(missing), shown(framed)]));
        Async/run(fiber)
        "#;

    let (system, io) = MockHost::builder()
        .serial([("/dev/ttyUSB0", Vec::<&str>::new())])
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"not_found other(22)");
}

// `list` names every entry of `/dev/serial/by-id` under the directory, in byte order, and a host with no such directory has no devices rather than a failure.
#[test]
fn list_names_the_devices_by_id_and_is_empty_without_the_directory() {
    let source = r#"
        use /std/{Str, List, Show, Try, Io, Serial};
        match Try/run(Serial/list)!
        | success(ports) => /std/print(Str/join(",", List/map(ports, (p) => Show/show(p))))
        | failure(e) => /std/print(Show/show(e))
        end
        "#;

    let (system, io) = MockHost::builder()
        .files([
            ("/dev/serial/by-id/usb-FTDI_FT232R_A5-if00-port0", ""),
            ("/dev/serial/by-id/usb-Arduino_Uno_85-if00", ""),
        ])
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"/dev/serial/by-id/usb-Arduino_Uno_85-if00,/dev/serial/by-id/usb-FTDI_FT232R_A5-if00-port0"
    );

    let (system, io) = MockHost::builder().build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"");
}
