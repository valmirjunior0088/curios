use {curios_runtime::MockHost, std::time::Duration};

#[test]
fn net_call_round_trips_a_scripted_endpoint() {
    let source = r#"
        use /std/{Handle, Str, Async};
        use /std/tcp/{Settings, Socket};
        match Async/block_on(Socket/call(Settings/default, "example.com", 80, Str/to_bytes("GET /\r\n\r\n")))
        | failure(_) => Handle/write(Handle/stdout, /std/Str/to_bytes("deadlock"))
        | success(outcome) =>
            match outcome
            | success(response) => Handle/write(Handle/stdout, response)
            | failure(_) => Handle/write(Handle/stdout, Str/to_bytes("error"))
            end
        end
        "#;

    let (system, io) = MockHost::builder()
        .net([("example.com:80", "HTTP/1.0 200 OK\r\n\r\nhello")])
        .build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"HTTP/1.0 200 OK\r\n\r\nhello");
}

// Connecting to an endpoint that was never scripted is refused, and the status
// decodes to `refused()`.
#[test]
fn net_call_to_an_unscripted_endpoint_is_refused() {
    let source = r#"
        use /std/{Handle, Async};
        use /std/tcp/{Settings, Socket};
        match Async/block_on(Socket/call(Settings/default, "example.com", 80, /std/Str/to_bytes("ping")))
        | failure(_) => /std/print("deadlock")
        | success(outcome) =>
            match outcome
            | success(_) => /std/print("connected")
            | failure(e) =>
                match e : {}
                | refused() => /std/print("refused")
                | tls() => /std/print("tls")
                | not_found() => /std/print("not found")
                | permission_denied() => /std/print("denied")
                | exists() => /std/print("exists")
                | would_block() => /std/print("would block")
                | other(_) => /std/print("other")
                end
            end
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"refused");
}

// A custom `Config` with an optional `Duration` timeout flows through the
// bracket; `Socket/read` pulls bytes from the socket the body is handed.
#[test]
fn net_with_custom_timeout_config_reads_response() {
    let source = r#"
        use /std/{Handle, Str, Bytes, Option, Async};
        use /std/tcp/{Settings, Socket};
        use /std/time/{Duration};
        let settings = Settings {
            connect_timeout = Option/some(Duration/of_millis(500)),
            read_timeout = Option/none(),
            write_timeout = Option/none(),
            tls = false
        };
        match Async/block_on(Socket/with(settings, "db.internal", 5432, (s) =>
            Async/bind(Socket/read(s, 64), (r) =>
                match r : Async(Bytes)
                | chunk(b) => Async/pure(b)
                | eof() => Async/pure(x\)
                | error(_) => Async/pure(x\)
                end)))
        | failure(_) => Handle/write(Handle/stdout, /std/Str/to_bytes("deadlock"))
        | success(outcome) =>
            match outcome
            | success(bytes) => Handle/write(Handle/stdout, bytes)
            | failure(_) => Handle/write(Handle/stdout, Str/to_bytes("error"))
            end
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
        use /std/{Handle, Str, Bytes, Async};
        use /std/tcp/{Listener, Socket};
        match Async/block_on(Listener/serve("0.0.0.0", 8080, (c) =>
            Async/bind(Socket/read(c, 64), (r) =>
                match r : Async({})
                | chunk(bytes) =>
                    Async/bind(Socket/write(c, Bytes/concat(Str/to_bytes("echo: "), bytes)), (wrote) => Async/pure(()))
                | eof() => Async/pure(())
                | error(_) => Async/pure(())
                end)))
        | failure(_) => /std/print("deadlock")
        | success(outcome) =>
            match outcome : {}
            | success(u) => ()
            | failure(_) => /std/print("listen failed")
            end
        end
        "#;

    let (system, io) = MockHost::builder().inbound(["ping"]).build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.captures(), vec![b"echo: ping".to_vec()]);
}

// TLS client (Stage A): `Socket/with` with `tls = true` upgrades the connected
// socket via `start_tls` before the body runs. The mock host serves the
// scripted endpoint cleartext (no real handshake under test), so the upgrade is
// a no-op identity and the round-trip still succeeds — exercising the wiring,
// types, and prim threading end to end through codegen.
#[test]
fn net_with_tls_upgrades_and_reads() {
    let source = r#"
        use /std/{Handle, Str, Bytes, Option, Async};
        use /std/tcp/{Settings, Socket};
        let settings = Settings {
            connect_timeout = Option/none(),
            read_timeout = Option/none(),
            write_timeout = Option/none(),
            tls = true
        };
        match Async/block_on(Socket/with(settings, "secure.example", 443, (s) =>
            Async/bind(Socket/read(s, 64), (r) =>
                match r : Async(Bytes)
                | chunk(b) => Async/pure(b)
                | eof() => Async/pure(x\)
                | error(_) => Async/pure(x\)
                end)))
        | failure(_) => Handle/write(Handle/stdout, /std/Str/to_bytes("deadlock"))
        | success(outcome) =>
            match outcome
            | success(bytes) => Handle/write(Handle/stdout, bytes)
            | failure(_) => Handle/write(Handle/stdout, Str/to_bytes("error"))
            end
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
        use /std/{Handle, Str, Bytes, Async};
        use /std/tcp/{Listener, Socket};
        match Async/block_on(Listener/serve_tls("0.0.0.0", 8443, Str/to_bytes("CERT"), Str/to_bytes("KEY"), (c) =>
            Async/bind(Socket/read(c, 64), (r) =>
                match r : Async({})
                | chunk(bytes) =>
                    Async/bind(Socket/write(c, Bytes/concat(Str/to_bytes("tls: "), bytes)), (wrote) => Async/pure(()))
                | eof() => Async/pure(())
                | error(_) => Async/pure(())
                end)))
        | failure(_) => /std/print("deadlock")
        | success(outcome) =>
            match outcome : {}
            | success(u) => ()
            | failure(_) => /std/print("serve failed")
            end
        end
        "#;

    let (system, io) = MockHost::builder().inbound(["ping"]).build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.captures(), vec![b"tls: ping".to_vec()]);
}

// HTTP client (Phase B): `http/perform` renders a `Request`, sends it through
// `/std/tcp`, and runs the `/std/Parse`-based response parser over the reply —
// exercising the byte-scanning parser end to end through codegen.
#[test]
fn http_perform_parses_a_scripted_response() {
    let source = r#"
        use /std/{Handle, Str, Nat, Async, http};
        match Async/block_on(http/perform(http/get("example.com", 80, "/")))
        | failure(_) => /std/print("deadlock")
        | success(outcome) =>
            match outcome : {}
            | success(response) =>
                let ct = match http/header(response, "Content-Type") : Str
                    | some(value) => value
                    | none() => "none"
                    end;
                match Str/of_bytes(response.body) : {}
                | some(body) =>
                    let _ = Handle/write(Handle/stdout, Str/to_bytes(Str/flatten([
                        Nat/to_str(response.status.code), " ", ct, " ", body
                    ]))); ()
                | none() => let _ = Handle/write(Handle/stdout, Str/to_bytes("bad body")); ()
                end
            | failure(_) => let _ = Handle/write(Handle/stdout, Str/to_bytes("error")); ()
            end
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
