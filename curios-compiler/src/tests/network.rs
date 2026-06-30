use {curios_runtime::MockHost, std::time::Duration};

#[test]
fn net_call_round_trips_a_scripted_endpoint() {
    let source = r#"
        use /std/{Tcp, Io, Str, Task};
        match Task/block_on(Tcp/call(Tcp/default, "example.com", 80, Str/to_bin("GET /\r\n\r\n")))
        | success(response) => Io/write(Io/stdout, response)
        | failure(_) => Io/write(Io/stdout, Str/to_bin("error"))
        end
        "#;

    let (system, io) = MockHost::builder()
        .net([("example.com:80", "HTTP/1.0 200 OK\r\n\r\nhello")])
        .build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"HTTP/1.0 200 OK\r\n\r\nhello");
}

// Connecting to an endpoint that was never scripted is refused, and the status
// decodes to `Tcp/refused`.
#[test]
fn net_call_to_an_unscripted_endpoint_is_refused() {
    let source = r#"
        use /std/{Tcp, Io, Task};
        match Task/block_on(Tcp/call(Tcp/default, "example.com", 80, /std/Str/to_bin("ping")))
        | success(_) => Io/print("connected")
        | failure(e) =>
            match e : {}
            | refused() => Io/print("refused")
            | tls() => Io/print("tls")
            | not_found() => Io/print("not found")
            | permission_denied() => Io/print("denied")
            | exists() => Io/print("exists")
            | would_block() => Io/print("would block")
            | other(_) => Io/print("other")
            end
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"refused");
}

// A custom `Config` with an optional `Duration` timeout flows through the
// bracket; `Tcp/read` pulls bytes from the socket the body is handed.
#[test]
fn net_with_custom_timeout_config_reads_response() {
    let source = r#"
        use /std/{Tcp, Io, Str, Bin, Option, Time, Task};
        let settings = Tcp/Settings {
            connect_timeout = Option/some(Time/of_millis(500)),
            read_timeout = Option/none(),
            write_timeout = Option/none(),
            tls = false
        };
        match Task/block_on(Tcp/with(settings, "db.internal", 5432, (s) =>
            Task/bind(Tcp/read(s, 64), (r) =>
                match r : Task(Bin)
                | chunk(b) => Task/pure(b)
                | eof() => Task/pure(\\)
                | error(_) => Task/pure(\\)
                end)))
        | success(bytes) => Io/write(Io/stdout, bytes)
        | failure(_) => Io/write(Io/stdout, Str/to_bin("error"))
        end
        "#;

    let (system, io) = MockHost::builder()
        .net([("db.internal:5432", "PONG")])
        .build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"PONG");
}

// Server network IO (Stage A): `serve` binds a listener, pulls the scripted
// inbound connection, and runs the handler per connection — which reads the
// request off the socket and writes a response the host captures. The exhausted
// inbound queue then fails the next `accept`, ending the loop and closing the
// bracketed listener.
#[test]
fn net_serve_handles_a_scripted_inbound_connection() {
    let source = r#"
        use /std/{Tcp, Io, Str, Bin, Task};
        match Task/block_on(Tcp/serve("0.0.0.0", 8080, (c) =>
            Task/bind(Tcp/read(c, 64), (r) =>
                match r : Task({})
                | chunk(bytes) =>
                    Task/bind(Tcp/write(c, Bin/concat(Str/to_bin("echo: "), bytes)), (wrote) => Task/pure(()))
                | eof() => Task/pure(())
                | error(_) => Task/pure(())
                end))) : {}
        | success(u) => ()
        | failure(_) => Io/print("listen failed")
        end
        "#;

    let (system, io) = MockHost::builder().inbound(["ping"]).build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.captures(), vec![b"echo: ping".to_vec()]);
}

// TLS client (Stage A): `Tcp/with` with `tls = true` upgrades the connected
// socket via `start_tls` before the body runs. The mock host serves the
// scripted endpoint cleartext (no real handshake under test), so the upgrade is
// a no-op identity and the round-trip still succeeds — exercising the wiring,
// types, and prim threading end to end through codegen.
#[test]
fn net_with_tls_upgrades_and_reads() {
    let source = r#"
        use /std/{Tcp, Io, Str, Bin, Option, Task};
        let settings = Tcp/Settings {
            connect_timeout = Option/none(),
            read_timeout = Option/none(),
            write_timeout = Option/none(),
            tls = true
        };
        match Task/block_on(Tcp/with(settings, "secure.example", 443, (s) =>
            Task/bind(Tcp/read(s, 64), (r) =>
                match r : Task(Bin)
                | chunk(b) => Task/pure(b)
                | eof() => Task/pure(\\)
                | error(_) => Task/pure(\\)
                end)))
        | success(bytes) => Io/write(Io/stdout, bytes)
        | failure(_) => Io/write(Io/stdout, Str/to_bin("error"))
        end
        "#;

    let (system, io) = MockHost::builder()
        .net([("secure.example:443", "SECURE-PONG")])
        .build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"SECURE-PONG");
}

// Server TLS termination (Stage A): `serve_tls` builds a config token, then
// upgrades each accepted connection via `start_tls_server` before the handler
// runs. The mock host runs cleartext, so the upgrade is a no-op identity and
// the handler echoes the scripted request the host captures.
#[test]
fn net_serve_tls_handles_a_scripted_inbound_connection() {
    let source = r#"
        use /std/{Tcp, Io, Str, Bin, Task};
        match Task/block_on(Tcp/serve_tls("0.0.0.0", 8443, Str/to_bin("CERT"), Str/to_bin("KEY"), (c) =>
            Task/bind(Tcp/read(c, 64), (r) =>
                match r : Task({})
                | chunk(bytes) =>
                    Task/bind(Tcp/write(c, Bin/concat(Str/to_bin("tls: "), bytes)), (wrote) => Task/pure(()))
                | eof() => Task/pure(())
                | error(_) => Task/pure(())
                end))) : {}
        | success(u) => ()
        | failure(_) => Io/print("serve failed")
        end
        "#;

    let (system, io) = MockHost::builder().inbound(["ping"]).build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.captures(), vec![b"tls: ping".to_vec()]);
}

// HTTP client (Phase B): `Http/perform` renders a `Request`, sends it through
// `/std/Tcp`, and runs the `/std/Parse`-based response parser over the reply —
// exercising the byte-scanning parser end to end through codegen.
#[test]
fn http_perform_parses_a_scripted_response() {
    let source = r#"
        use /std/{Http, Io, Str, Nat, Task};
        match Task/block_on(Http/perform(Http/get("example.com", 80, "/"))) : {}
        | success(response) =>
            let ct = match Http/header(response, "Content-Type") : Str
                | some(value) => value
                | none() => "none"
                end;
            match Str/of_bin(response.body) : {}
            | some(body) =>
                let _ = Io/write(Io/stdout, Str/to_bin(Str/flatten([
                    Nat/to_str(response.status.code), " ", ct, " ", body
                ]))); ()
            | none() => let _ = Io/write(Io/stdout, Str/to_bin("bad body")); ()
            end
        | failure(_) => let _ = Io/write(Io/stdout, Str/to_bin("error")); ()
        end
        "#;

    // The trailing bytes past `Content-Length: 5` must be dropped by the body
    // framing, leaving the body exactly "hello".
    let (system, io) = MockHost::builder()
        .net([(
            "example.com:80",
            "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 5\r\n\r\nhello AND MORE",
        )])
        .build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"200 text/plain hello");
}
