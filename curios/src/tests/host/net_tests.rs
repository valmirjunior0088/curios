//! Calling and serving over the network, TLS, and the foreign declaration that reaches the host through supplied bindings.

use {
    crate::tests::{run, run_text},
    curios_pipeline::compile_with_prelude,
    curios_runtime::{ForeignBindings, MockHost},
    curios_text::{Entrypoint, RootSource},
};

#[test]
fn call_round_trips_a_scripted_endpoint() {
    let source = r#"
        use /std/{Handle, Str, Async, Io};
        use /std/tcp/{Settings, Socket};
        let _ = (match Async/block_on(Socket/call(Settings/default, "example.com", 80, Str/to_bytes("GET /\r\n\r\n")))!
        | failure(_) => Io/write(Io/stdout, /std/Str/to_bytes("deadlock"))
        | success(outcome) =>
            match outcome
            | success(response) => Io/write(Io/stdout, response)
            | failure(_) => Io/write(Io/stdout, Str/to_bytes("error"))
            end
        end)!;
        /std/Io/pure(())
        "#;

    let (system, io) = MockHost::builder()
        .net([("example.com:80", "HTTP/1.0 200 OK\r\n\r\nhello")])
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"HTTP/1.0 200 OK\r\n\r\nhello");
}

// Connecting to an endpoint that was never scripted is refused, and the status decodes to `refused()`.
#[test]
fn call_to_an_unscripted_endpoint_is_refused() {
    let source = r#"
        use /std/{Handle, Async, Io};
        use /std/tcp/{Settings, Socket};
        match Async/block_on(Socket/call(Settings/default, "example.com", 80, /std/Str/to_bytes("ping")))!
        | failure(_) => /std/print("deadlock")
        | success(outcome) =>
            match outcome
            | success(_) => /std/print("connected")
            | failure(e) =>
                match e : (_) => /std/Io({})
                | refused() => /std/print("refused")
                | tls() => /std/print("tls")
                | not_found() => /std/print("not found")
                | permission_denied() => /std/print("denied")
                | exists() => /std/print("exists")
                | would_block() => /std/print("would block")
                | not_empty() => /std/print("not empty")
                | is_directory() => /std/print("is a directory")
                | not_directory() => /std/print("not a directory")
                | other(_) => /std/print("other")
                end
            end
        end
        "#;

    assert_eq!(run(source), b"refused");
}

// A connect to a remote peer goes pending: the fiber parks on the socket's writability, `finish_connect` reads the outcome once `poll` has reported it, and only then is the request sent. A refusal arrives the same way, deferred to the settle, and decodes to `refused()` as a synchronous one does.
#[test]
fn a_pending_connect_is_awaited_before_the_request_is_sent() {
    let source = r#"
        use /std/{Handle, Str, Async, Io};
        use /std/tcp/{Settings, Socket};
        let _ = (match Async/block_on(Socket/call(Settings/default, "example.com", 80, Str/to_bytes("GET /\r\n\r\n")))!
        | failure(_) => Io/write(Io/stdout, /std/Str/to_bytes("deadlock"))
        | success(outcome) =>
            match outcome
            | success(response) => Io/write(Io/stdout, response)
            | failure(e) =>
                match e : (_) => /std/Io({})
                | refused() => Io/write(Io/stdout, Str/to_bytes("refused"))
                | _ => Io/write(Io/stdout, Str/to_bytes("error"))
                end
            end
        end)!;
        /std/Io/pure(())
        "#;

    let (system, io) = MockHost::builder()
        .net([("example.com:80", "pong")])
        .connect_pending()
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"pong");

    let (system, io) = MockHost::builder().connect_pending().build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"refused");
}

// The one case that needs the network: an HTTPS request against a public host, driven end to end by the scheduler over the real host — the resolution, the pending connect and the handshake each park on `poll` between the fiber's reads and writes. Ignored by default; run by hand on a networked machine with `cargo test -p curios --lib -- --ignored https_perform`. The real host's output cannot be captured, so the program reports through a file.
#[test]
#[ignore = "needs the network"]
fn https_perform_reaches_a_public_host_over_the_real_host() {
    let report = std::env::temp_dir().join(format!("curios-https-{}", std::process::id()));
    let path = report.to_str().expect("a UTF-8 temporary path");
    let source = format!(
        r#"
        use /std/{{Str, Nat, Result, Try, Async, Io, Path, File, http}};
        let program: Async({{}}) =
            let r = http/perform(http/get_tls("example.com", 443, "/"))!;
            let line = match r | success(resp) => Nat/to_str(resp.status.code) | failure(_) => "failed" end;
            let _ = Try/run(File/write_all(Path/of_str("{path}"), Str/to_bytes(line)))!;
            Async/pure(());
        match Async/block_on(program)!
        | failure(_) => /std/print("deadlock")
        | success(_) => Io/pure(())
        end
        "#
    );

    run_text(&source, curios_runtime::OsHost::with_args(vec![])).expect("expected result");

    let written = std::fs::read(&report).expect("the program wrote its report");
    std::fs::remove_file(&report).expect("the report is removable");
    assert_eq!(written, b"200");
}

// A custom `Config` with an optional `Duration` timeout flows through the bracket; `Socket/read` pulls bytes from the socket the body is handed.
#[test]
fn net_with_custom_timeout_config_reads_response() {
    let source = r#"
        use /std/{Handle, Str, Bytes, Option, Async, Io};
        use /std/tcp/{Settings, Socket};
        use /std/time/{Duration};
        let settings = Settings {
            connect_timeout = Option/some(Duration/of_millis(500)),
            read_timeout = Option/none(),
            write_timeout = Option/none(),
            tls = false
        };
        let _ = (match Async/block_on(Socket/with(settings, "db.internal", 5432, (s) =>
            Async/bind(Socket/read(s, 64), (r) =>
                match r : (_) => Async(Bytes)
                | chunk(b) => Async/pure(b)
                | eof() => Async/pure(x[])
                | error(_) => Async/pure(x[])
                end)))!
        | failure(_) => Io/write(Io/stdout, /std/Str/to_bytes("deadlock"))
        | success(outcome) =>
            match outcome
            | success(bytes) => Io/write(Io/stdout, bytes)
            | failure(_) => Io/write(Io/stdout, Str/to_bytes("error"))
            end
        end)!;
        /std/Io/pure(())
        "#;

    let (system, io) = MockHost::builder()
        .net([("db.internal:5432", "PONG")])
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"PONG");
}

// Server network IO (Stage A): `serve` binds a listener, pulls the scripted inbound connection, and runs the handler per connection — which reads the request off the socket and writes a response the host captures. The exhausted inbound queue then fails the next `accept`, ending the loop and closing the bracketed listener.
#[test]
fn serve_handles_a_scripted_inbound_connection() {
    let source = r#"
        use /std/{Handle, Str, Bytes, Async, Io};
        use /std/tcp/{Listener, Socket};
        match Async/block_on(Listener/serve("0.0.0.0", 8080, (c) =>
            Async/bind(Socket/read(c, 64), (r) =>
                match r : (_) => Async({})
                | chunk(bytes) =>
                    Async/bind(Socket/write(c, x[..Str/to_bytes("echo: "), ..bytes]), (wrote) => Async/pure(()))
                | eof() => Async/pure(())
                | error(_) => Async/pure(())
                end)))!
        | failure(_) => /std/print("deadlock")
        | success(outcome) =>
            match outcome : (_) => /std/Io({})
            | success(u) => /std/Io/pure(())
            | failure(_) => /std/print("listen failed")
            end
        end
        "#;

    let (system, io) = MockHost::builder().inbound(["ping"]).build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.captures(), vec![b"echo: ping".to_vec()]);
}

// TLS client (Stage A): `Socket/with` with `tls = true` upgrades the connected socket via `start_tls` before the body runs. The mock host serves the scripted endpoint cleartext (no real handshake under test), so the upgrade is a no-op identity and the round-trip still succeeds — exercising the wiring, types, and intrinsic threading end to end through codegen.
#[test]
fn net_with_tls_upgrades_and_reads() {
    let source = r#"
        use /std/{Handle, Str, Bytes, Option, Async, Io};
        use /std/tcp/{Settings, Socket};
        let settings = Settings {
            connect_timeout = Option/none(),
            read_timeout = Option/none(),
            write_timeout = Option/none(),
            tls = true
        };
        let _ = (match Async/block_on(Socket/with(settings, "secure.example", 443, (s) =>
            Async/bind(Socket/read(s, 64), (r) =>
                match r : (_) => Async(Bytes)
                | chunk(b) => Async/pure(b)
                | eof() => Async/pure(x[])
                | error(_) => Async/pure(x[])
                end)))!
        | failure(_) => Io/write(Io/stdout, /std/Str/to_bytes("deadlock"))
        | success(outcome) =>
            match outcome
            | success(bytes) => Io/write(Io/stdout, bytes)
            | failure(_) => Io/write(Io/stdout, Str/to_bytes("error"))
            end
        end)!;
        /std/Io/pure(())
        "#;

    let (system, io) = MockHost::builder()
        .net([("secure.example:443", "SECURE-PONG")])
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"SECURE-PONG");
}

// Server TLS termination (Stage A): `serve_tls` builds a config token, then upgrades each accepted connection via `start_tls_server` before the handler runs. The mock host runs cleartext, so the upgrade is a no-op identity and the handler echoes the scripted request the host captures.
#[test]
fn serve_tls_handles_a_scripted_inbound_connection() {
    let source = r#"
        use /std/{Handle, Str, Bytes, Async, Io};
        use /std/tcp/{Listener, Socket};
        match Async/block_on(Listener/serve_tls("0.0.0.0", 8443, Str/to_bytes("CERT"), Str/to_bytes("KEY"), (c) =>
            Async/bind(Socket/read(c, 64), (r) =>
                match r : (_) => Async({})
                | chunk(bytes) =>
                    Async/bind(Socket/write(c, x[..Str/to_bytes("tls: "), ..bytes]), (wrote) => Async/pure(()))
                | eof() => Async/pure(())
                | error(_) => Async/pure(())
                end)))!
        | failure(_) => /std/print("deadlock")
        | success(outcome) =>
            match outcome : (_) => /std/Io({})
            | success(u) => /std/Io/pure(())
            | failure(_) => /std/print("serve failed")
            end
        end
        "#;

    let (system, io) = MockHost::builder().inbound(["ping"]).build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.captures(), vec![b"tls: ping".to_vec()]);
}

// HTTP client (Phase B): `http/perform` renders a `Request`, sends it through `/std/tcp`, and runs the `/std/Parse`-based response parser over the reply — exercising the byte-scanning parser end to end through codegen.
#[test]
fn http_perform_parses_a_scripted_response() {
    let source = r#"
        use /std/{Handle, Str, Nat, Async, http, Io};
        match Async/block_on(http/perform(http/get("example.com", 80, "/")))!
        | failure(_) => /std/print("deadlock")
        | success(outcome) =>
            match outcome : (_) => /std/Io({})
            | success(response) =>
                let ct = match http/header(response, "Content-Type") : (_) => Str
                    | some(value) => value
                    | none() => "none"
                    end;
                match Str/of_bytes(response.body) : (_) => /std/Io({})
                | some(body) =>
                    let _ = Io/write(Io/stdout, Str/to_bytes(Str/flatten([
                        Nat/to_str(response.status.code), " ", ct, " ", body
                    ])))!; /std/Io/pure(())
                | none() => let _ = Io/write(Io/stdout, Str/to_bytes("bad body"))!; /std/Io/pure(())
                end
            | failure(_) => let _ = Io/write(Io/stdout, Str/to_bytes("error"))!; /std/Io/pure(())
            end
        end
        "#;

    // The trailing bytes past `Content-Length: 5` must be dropped by the body framing, leaving the body exactly "hello".
    let (system, io) = MockHost::builder()
        .net([(
            "example.com:80",
            "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 5\r\n\r\nhello AND MORE",
        )])
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"200 text/plain hello");
}

#[test]
fn foreign_declaration_runs_through_supplied_bindings() {
    // `double` has no host meaning at all — it's purely an embedder-supplied function, wired up via the store `compile_entrypoint` hands back and exercised end to end through `run_wasm`, not the compiler's own `sys` implementations.
    let source = r#"
        foreign double : (Nat) -> Nat;
        let _ = /std/proc/exit(double(21)!)!;
        let _ = std/Io/write(std/Io/stdout, /std/Str/to_bytes("unreachable"))!;
        /std/Io/pure(())
        "#
    .parse::<Entrypoint>()
    .expect("failed to parse source");

    let (module, foreigns) = compile_with_prelude(
        curios_pipeline::DEFAULT_STEP_BUDGET,
        &source,
        &RootSource::none(),
        |_| {},
    )
    .expect("compile succeeded");

    let mut bindings = ForeignBindings::new(foreigns);
    bindings.define("/double", |x: u32| x * 2);

    let (system, io) = MockHost::builder().build();
    let code = crate::run_wasm(&module, system, bindings).expect("execution succeeded");

    assert_eq!(code, 42);
    assert!(io.output().is_empty());
}
