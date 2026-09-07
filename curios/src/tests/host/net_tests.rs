//! Calling and serving over the network, TLS, and the foreign declaration that reaches the host through supplied bindings.

use {
    crate::tests::{run, run_text},
    curios_pipeline::compile_with_prelude,
    curios_runtime::{ForeignBindings, MockHost},
    curios_text::{Entrypoint, RootSource},
};

/// One `call` to `example.com:80`, its reply written out or its failure printed by name.
const CALL: &str = r#"
    use /std/{Str, Show, Try, Async, Io};
    use /std/tcp/{Socket};
    let fiber: Async({}) =
        let r = Try/run(Socket/call(Socket/connect("example.com", 80), Str/to_bytes("GET /\r\n\r\n")))!;
        match r
        | success(response) => Io/write(Io/stdout, response)
        | failure(e) => /std/print(Show/show(e))
        end;
    Async/run(fiber)
    "#;

#[test]
fn call_round_trips_a_scripted_endpoint() {
    let (system, io) = MockHost::builder()
        .net([("example.com:80", "HTTP/1.0 200 OK\r\n\r\nhello")])
        .build();
    run_text(CALL, system).expect("expected result");
    assert_eq!(io.output(), b"HTTP/1.0 200 OK\r\n\r\nhello");
}

// Connecting to an endpoint that was never scripted is refused, and the status decodes to `refused()`.
#[test]
fn call_to_an_unscripted_endpoint_is_refused() {
    assert_eq!(run(CALL), b"refused");
}

// A connect to a remote peer goes pending: the fiber parks on the socket's writability, `finish_connect` reads the outcome once `poll` has reported it, and only then is the request sent. A refusal arrives the same way, deferred to the settle, and decodes to `refused()` as a synchronous one does.
#[test]
fn a_pending_connect_is_awaited_before_the_request_is_sent() {
    let (system, io) = MockHost::builder()
        .net([("example.com:80", "pong")])
        .connect_pending()
        .build();
    run_text(CALL, system).expect("expected result");
    assert_eq!(io.output(), b"pong");

    let (system, io) = MockHost::builder().connect_pending().build();
    run_text(CALL, system).expect("expected result");
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
            let r = Try/run(http/perform(http/get(http/Url/lit("https://example.com/"))))!;
            let line = match r | success(resp) => Nat/to_str(resp.status.code) | failure(_) => "failed" end;
            let _ = Try/run(File/write_all(Path/of_str("{path}"), Str/to_bytes(line)))!;
            Async/pure(());
        Async/run(program)
        "#
    );

    run_text(&source, curios_runtime::OsHost::with_args(vec![])).expect("expected result");

    let written = std::fs::read(&report).expect("the program wrote its report");
    std::fs::remove_file(&report).expect("the report is removable");
    assert_eq!(written, b"200");
}

/// `with` over the connector `open`, the body reading one chunk off the socket it is handed and answering it.
fn read_one_chunk(open: &str) -> String {
    format!(
        r#"
        use /std/{{Str, Bytes, Show, Try, Async, Io}};
        use /std/tcp/{{Socket}};
        let fiber: Async({{}}) =
            let r = Try/run(Socket/with({open}, (s) =>
                let c = Socket/read(s, 64)!;
                Try/pure(match c | chunk(b) => b | _ => x[] end)))!;
            match r
            | success(bytes) => Io/write(Io/stdout, bytes)
            | failure(e) => /std/print(Show/show(e))
            end;
        Async/run(fiber)
        "#
    )
}

// `with` hands the body the socket its connector opened and closes it after; `Socket/read` pulls the scripted bytes off it inside the bracket.
#[test]
fn with_hands_the_body_a_connected_socket() {
    let source = read_one_chunk(r#"Socket/connect("db.internal", 5432)"#);

    let (system, io) = MockHost::builder()
        .net([("db.internal:5432", "PONG")])
        .build();
    run_text(&source, system).expect("expected result");
    assert_eq!(io.output(), b"PONG");
}

// TLS client: `connect_tls` files the session on the connected socket before the body runs. The mock host serves the scripted endpoint cleartext (no real handshake under test), so the upgrade is a no-op identity and the round-trip still succeeds — exercising the wiring, types, and intrinsic threading end to end through codegen.
#[test]
fn connect_tls_upgrades_and_reads() {
    let source = read_one_chunk(r#"Socket/connect_tls("secure.example", 443)"#);

    let (system, io) = MockHost::builder()
        .net([("secure.example:443", "SECURE-PONG")])
        .build();
    run_text(&source, system).expect("expected result");
    assert_eq!(io.output(), b"SECURE-PONG");
}

/// A server over `serve`, the handler echoing each request under `prefix`, with `listener` the `serve` or `serve_tls` call to make.
fn echo_server(listener: &str, prefix: &str) -> String {
    format!(
        r#"
        use /std/{{Str, Bytes, Show, Try, Async, Io}};
        use /std/tcp/{{Listener, Socket}};
        let handler(c: Socket) -> Async({{}}) =
            let r = Socket/read(c, 64)!;
            match r
            | chunk(bytes) =>
                let _ = Socket/write(c, x[..Str/to_bytes("{prefix}"), ..bytes])!;
                Async/pure(())
            | _ => Async/pure(())
            end;
        let fiber: Async({{}}) =
            let r = Try/run({listener})!;
            match r
            | success(_) => Io/pure(())
            | failure(e) => /std/print(Show/show(e))
            end;
        Async/run(fiber)
        "#
    )
}

// Server network IO: `serve` binds a listener, pulls the scripted inbound connection, and runs the handler per connection — which reads the request off the socket and writes a response the host captures. The exhausted inbound queue then fails the next `accept`, ending the loop and closing the bracketed listener.
#[test]
fn serve_handles_a_scripted_inbound_connection() {
    let source = echo_server(r#"Listener/serve("0.0.0.0", 8080, handler)"#, "echo: ");

    let (system, io) = MockHost::builder().inbound(["ping"]).build();
    run_text(&source, system).expect("expected result");
    assert_eq!(io.captures(), vec![b"echo: ping".to_vec()]);
}

// Server TLS termination: `serve_tls` builds a config token, then files the server session on each accepted connection before the handler runs. The mock host runs cleartext, so the upgrade is a no-op identity and the handler echoes the scripted request the host captures.
#[test]
fn serve_tls_handles_a_scripted_inbound_connection() {
    let source = echo_server(
        r#"Listener/serve_tls("0.0.0.0", 8443, Str/to_bytes("CERT"), Str/to_bytes("KEY"), handler)"#,
        "tls: ",
    );

    let (system, io) = MockHost::builder().inbound(["ping"]).build();
    run_text(&source, system).expect("expected result");
    assert_eq!(io.captures(), vec![b"tls: ping".to_vec()]);
}

// The loop written by hand: `listen`, one `accept`, the exchange over the accepted socket, and both closed — the pieces `serve` is made of, reachable on their own.
#[test]
fn listen_and_accept_serve_one_connection_by_hand() {
    let source = r#"
        use /std/{Str, Bytes, Result, Show, Try, Async, Io};
        use /std/tcp/{Listener, Socket};
        let session: Try(Async, Io/Error, {}) =
            let l = Listener/listen("0.0.0.0", 8080)!;
            let c = Listener/accept(l)!;
            let r = Socket/read(c, 64)!;
            let reply =
                match r
                | chunk(bytes) => Socket/write(c, x[..Str/to_bytes("one: "), ..bytes])
                | _ => Async/pure(Result/success(()))
                end;
            let _ = Try/attempt(reply)!;
            let _ = Socket/close(c)!;
            Listener/close(l);
        let fiber: Async({}) =
            let r = Try/run(session)!;
            match r
            | success(_) => Io/pure(())
            | failure(e) => /std/print(Show/show(e))
            end;
        Async/run(fiber)
        "#;

    let (system, io) = MockHost::builder().inbound(["ping"]).build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.captures(), vec![b"one: ping".to_vec()]);
}

/// `http/perform` against `example.com:80`, the status, content type and body printed, or the failure's arm.
const HTTP_GET: &str = r#"
    use /std/{Str, Nat, Option, Try, Async, http, Io};
    let fiber: Async({}) =
        let r = Try/run(http/perform(http/get(http/Url/lit("http://example.com/"))))!;
        match r
        | success(response) =>
            let ct = Option/unwrap_or(http/Response/header(response, "Content-Type"), "none");
            let body = Option/unwrap_or(Str/of_bytes(response.body), "bad body");
            /std/print(Str/flatten([Nat/to_str(response.status.code), " ", ct, " ", body]))
        | failure(e) =>
            match e
            | net(_) => /std/print("net")
            | malformed(_) => /std/print("malformed")
            | timeout() => /std/print("timeout")
            | redirected(_) => /std/print("redirected")
            end
        end;
    Async/run(fiber)
    "#;

// HTTP client: `http/perform` renders a `Request`, sends it through `/std/tcp`, and runs the `/std/Parse`-based response parser over the reply — exercising the byte-scanning parser end to end through codegen.
#[test]
fn http_perform_parses_a_scripted_response() {
    // The trailing bytes past `Content-Length: 5` must be dropped by the body framing, leaving the body exactly "hello".
    let (system, io) = MockHost::builder()
        .net([(
            "example.com:80",
            "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 5\r\n\r\nhello AND MORE",
        )])
        .build();
    run_text(HTTP_GET, system).expect("expected result");
    assert_eq!(io.output(), b"200 text/plain hello");
}

// The two arms of `http/Error`: a reply that is not HTTP is `malformed`, and a peer that is not there is `net`.
#[test]
fn http_perform_tells_a_malformed_reply_from_a_network_failure() {
    let (system, io) = MockHost::builder()
        .net([("example.com:80", "not http at all")])
        .build();
    run_text(HTTP_GET, system).expect("expected result");
    assert_eq!(io.output(), b"malformed");

    assert_eq!(run(HTTP_GET), b"net");
}

#[test]
fn foreign_declaration_runs_through_supplied_bindings() {
    // `double` has no host meaning at all — it's purely an embedder-supplied function, wired up via the store `compile_entrypoint` hands back and exercised end to end through `run_wasm`, not the compiler's own `sys` implementations.
    let source = r#"
        foreign double : (Nat) -> Nat;
        let _ = /std/proc/exit(@{}, double(21)!)!;
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
