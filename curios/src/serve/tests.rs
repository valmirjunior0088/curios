use {
    super::*,
    std::io::Read,
    tungstenite::{Error, stream::MaybeTlsStream},
};

/// A page holding its document, its program and one file under a directory, served on a port of its own until the test's process ends.
fn serving() -> u16 {
    let listener = bound(0).expect("a port of loopback's own");
    let port = listener.local_addr().unwrap().port();
    let served = Served {
        files: BTreeMap::from([
            (INDEX_HTML.to_string(), b"<!doctype html>".to_vec()),
            ("index.wasm".to_string(), b"\0asm".to_vec()),
            ("web/a b.js".to_string(), b"export {};".to_vec()),
        ]),
    };

    thread::spawn(move || serve_page(&listener, served));

    port
}

/// What the server at `port` answers `request`, written as a client writes it, as its status line, its headers and its body.
fn asked(port: u16, request: &str) -> (String, Vec<String>, Vec<u8>) {
    let mut stream = TcpStream::connect((Ipv4Addr::LOCALHOST, port)).unwrap();
    stream.write_all(request.as_bytes()).unwrap();

    let mut reply = Vec::new();
    stream.read_to_end(&mut reply).unwrap();

    let split = reply
        .windows(4)
        .position(|window| window == b"\r\n\r\n")
        .expect("a reply's head ends in a blank line");
    let head = String::from_utf8(reply[..split].to_vec()).unwrap();
    let mut lines = head.split("\r\n").map(str::to_string);

    (
        lines.next().unwrap(),
        lines.collect(),
        reply[split + 4..].to_vec(),
    )
}

/// `GET path` from the server at `port`, by its own name.
fn get(port: u16, path: &str) -> (String, Vec<String>, Vec<u8>) {
    asked(
        port,
        &format!("GET {path} HTTP/1.1\r\nHost: 127.0.0.1:{port}\r\n\r\n"),
    )
}

#[test]
fn the_root_answers_the_page_and_a_path_its_file() {
    let port = serving();

    let (status, headers, body) = get(port, "/");
    assert_eq!(status, "HTTP/1.1 200 OK");
    assert!(
        headers.contains(&"Content-Type: text/html; charset=utf-8".to_string()),
        "{headers:?}"
    );
    assert_eq!(body, b"<!doctype html>");

    let (status, _, body) = get(port, "/web/a%20b.js?v=1");
    assert_eq!(status, "HTTP/1.1 200 OK");
    assert_eq!(body, b"export {};");
}

/// `index.html` is the page's own name for its document, so a request by that name is a request for the root.
#[test]
fn the_root_and_the_pages_own_name_for_its_document_are_one_resource() {
    let port = serving();

    assert_eq!(get(port, "/index.html"), get(port, "/"));
}

#[test]
fn the_program_is_served_as_webassembly() {
    let port = serving();

    let (_, headers, _) = get(port, "/index.wasm");

    assert!(
        headers.contains(&"Content-Type: application/wasm".to_string()),
        "{headers:?}"
    );
}

#[test]
fn every_reply_says_no_store_and_closes() {
    let port = serving();

    for path in ["/", "/missing", "/index.wasm"] {
        let (_, headers, _) = get(port, path);

        assert!(
            headers.contains(&"Cache-Control: no-store".to_string()),
            "{path}: {headers:?}"
        );
        assert!(
            headers.contains(&"Connection: close".to_string()),
            "{path}: {headers:?}"
        );
    }
}

#[test]
fn a_path_the_page_holds_no_file_at_is_not_found() {
    let port = serving();

    for path in ["/missing.js", "/app/", "/web", "/index.html/"] {
        assert_eq!(get(port, path).0, "HTTP/1.1 404 Not Found", "{path}");
    }
}

#[test]
fn a_method_other_than_get_is_refused_naming_get() {
    let port = serving();

    let (status, headers, _) = asked(
        port,
        &format!("POST / HTTP/1.1\r\nHost: 127.0.0.1:{port}\r\nContent-Length: 0\r\n\r\n"),
    );

    assert_eq!(status, "HTTP/1.1 405 Method Not Allowed");
    assert!(headers.contains(&"Allow: GET".to_string()), "{headers:?}");
}

/// A name that resolves to loopback is how a page some other site loads would reach this one.
#[test]
fn a_host_other_than_loopbacks_own_is_refused() {
    let port = serving();

    assert_eq!(
        asked(
            port,
            &format!("GET / HTTP/1.1\r\nHost: localhost:{port}\r\n\r\n")
        )
        .0,
        "HTTP/1.1 200 OK"
    );
    for host in [
        format!("attacker.example:{port}"),
        "127.0.0.1".to_string(),
        String::new(),
    ] {
        assert_eq!(
            asked(port, &format!("GET / HTTP/1.1\r\nHost: {host}\r\n\r\n")).0,
            "HTTP/1.1 421 Misdirected Request",
            "{host:?}"
        );
    }
}

/// A head is read to its end before it is answered, so one past the bound is refused however it arrives, and what parses as no request is refused as such.
#[test]
fn a_head_past_the_bound_or_that_is_no_request_is_refused() {
    let port = serving();
    let past = format!(
        "GET / HTTP/1.1\r\nHost: 127.0.0.1:{port}\r\nX-Long: {}\r\n\r\n",
        "a".repeat(HEAD)
    );

    assert_eq!(
        asked(port, &past).0,
        "HTTP/1.1 431 Request Header Fields Too Large"
    );
    assert_eq!(
        asked(port, "NOT HTTP AT ALL\r\n\r\n").0,
        "HTTP/1.1 400 Bad Request"
    );
}

#[test]
fn a_port_another_holds_is_refused_naming_the_flag_that_picks_another() {
    let taken = bound(0).unwrap();
    let port = taken.local_addr().unwrap().port();

    let refusal = bound(port).unwrap_err();

    assert!(
        refusal.starts_with(&format!("cannot serve on 127.0.0.1:{port}:"))
            && refusal.ends_with("`--port` serves on another"),
        "{refusal}"
    );
}

/// The query is the loader's to decode, so every byte but the unreserved ones crosses percent-encoded, a separator of the query's own among them.
#[test]
fn a_url_names_the_root_and_carries_each_argument_percent_encoded() {
    assert_eq!(url(8000, &[]), "http://127.0.0.1:8000/");
    assert_eq!(
        url(8001, &["one".into(), "a&b=c+d".into(), "é".into()]),
        "http://127.0.0.1:8001/?arg=one&arg=a%26b%3Dc%2Bd&arg=%C3%A9"
    );
}

#[test]
fn a_path_is_decoded_before_it_is_looked_for_and_a_malformed_escape_is_refused() {
    let port = serving();

    assert_eq!(get(port, "/%77eb/a%20b.js").0, "HTTP/1.1 200 OK");
    for path in ["/web/a%2", "/web/a%+1.js", "/%FF"] {
        assert_eq!(get(port, path).0, "HTTP/1.1 400 Bad Request", "{path}");
    }
}

/// A page of a run: its document and its program.
fn page(document: &str, program: &[u8]) -> Served {
    Served {
        files: BTreeMap::from([
            (INDEX_HTML.to_string(), document.as_bytes().to_vec()),
            ("index.wasm".to_string(), program.to_vec()),
        ]),
    }
}

/// A run of two pages, the first scheduling two tests and the second one, served on a port of its own: an outcome taken is answered with the guest's stdout and a line naming its step on stdout, and the posted trap, where there is one, on stderr; the thread serving the run ends once every step is taken.
fn running() -> (u16, thread::JoinHandle<Result<(), String>>) {
    let listener = bound(0).expect("a port of loopback's own");
    let port = listener.local_addr().unwrap().port();
    let served = thread::spawn(move || {
        let run = Run::new(
            vec![
                (page("<!doctype html>first", b"\0asm1"), 2),
                (page("<!doctype html>second", b"\0asm2"), 1),
            ],
            |step, guest, posted| Streams {
                stdout: format!(
                    "{}step {}/{}\n",
                    String::from_utf8_lossy(&guest.stdout),
                    step.subject,
                    step.test
                )
                .into_bytes(),
                stderr: posted.trap.clone().unwrap_or_default().into_bytes(),
            },
        );

        serve_run(&listener, run)
    });

    (port, served)
}

/// A run's socket, as a client holds it.
type Socket = WebSocket<MaybeTlsStream<TcpStream>>;

/// The run's socket at `port`, opened as a runner opens it.
fn socket(port: u16) -> Result<Socket, Error> {
    tungstenite::connect(format!("ws://127.0.0.1:{port}/")).map(|(socket, _)| socket)
}

/// An outcome as a runner sends one: `stdout` in base64, and `trap` where there is one.
fn outcome(stdout: &str, trap: Option<&str>) -> Message {
    let trap = trap.map_or("null".to_string(), |trap| format!("{trap:?}"));

    Message::text(format!(
        "{{\"stdout\":\"{stdout}\",\"stderr\":\"\",\"exitCode\":0,\"trap\":{trap},\"held\":true}}"
    ))
}

/// The next reply `socket` is sent: what was written on each stream, and the next step.
fn reply(socket: &mut Socket) -> (String, String, Option<Step>) {
    let Message::Text(text) = socket.read().expect("a reply") else {
        panic!("a reply is a text message");
    };
    let reply = Reply::parse(text.as_bytes()).expect("a reply");
    let (stdout, stderr) = reply.streams().expect("base64");

    (
        String::from_utf8(stdout).unwrap(),
        String::from_utf8(stderr).unwrap(),
        reply.next,
    )
}

/// The status a handshake for the run's socket at `port` is answered with, stating `headers` beside the ones that ask for it.
fn handshake(port: u16, headers: &str) -> String {
    asked(
        port,
        &format!(
            "GET / HTTP/1.1\r\nHost: 127.0.0.1:{port}\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n{headers}\r\n"
        ),
    )
    .0
}

#[test]
fn a_run_is_the_runner_at_the_root_and_the_page_in_hand_beside_it() {
    let (port, _served) = running();

    let (status, headers, runner) = get(port, "/");
    assert_eq!(status, "HTTP/1.1 200 OK");
    assert!(
        headers.contains(&"Content-Type: text/html; charset=utf-8".to_string()),
        "{headers:?}"
    );
    let runner = String::from_utf8(runner).unwrap();
    assert!(runner.contains("{\"argument\":\"arg\"}"), "{runner}");
    assert_eq!(get(port, "/index.html").2, b"<!doctype html>first");
    assert_eq!(get(port, "/index.wasm").2, b"\0asm1");
    assert_eq!(get(port, "/missing").0, "HTTP/1.1 404 Not Found");
    let (status, headers, _) = asked(
        port,
        &format!("POST / HTTP/1.1\r\nHost: 127.0.0.1:{port}\r\nContent-Length: 0\r\n\r\n"),
    );
    assert_eq!(status, "HTTP/1.1 405 Method Not Allowed");
    assert!(headers.contains(&"Allow: GET".to_string()), "{headers:?}");
}

/// A browser states the origin of every page that asks for a socket, so one stating another site's is no runner this run serves.
#[test]
fn a_socket_is_refused_from_another_origin_at_another_version_or_without_a_key() {
    let (port, _served) = running();
    let key = "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n";

    assert_eq!(
        handshake(
            port,
            &format!("{key}Sec-WebSocket-Version: 13\r\nOrigin: http://attacker.example\r\n")
        ),
        "HTTP/1.1 403 Forbidden"
    );
    assert_eq!(
        handshake(
            port,
            &format!(
                "{key}Sec-WebSocket-Version: 13\r\nOrigin: http://127.0.0.1:{}\r\n",
                port + 1
            )
        ),
        "HTTP/1.1 403 Forbidden"
    );
    assert_eq!(
        handshake(port, &format!("{key}Sec-WebSocket-Version: 8\r\n")),
        "HTTP/1.1 400 Bad Request"
    );
    assert_eq!(
        handshake(port, "Sec-WebSocket-Version: 13\r\n"),
        "HTTP/1.1 400 Bad Request"
    );
    // Nothing above held the run: a runner's socket still opens, from the run's own origin.
    let mut socket = tungstenite::connect(
        tungstenite::ClientRequestBuilder::new(format!("ws://127.0.0.1:{port}/").parse().unwrap())
            .with_header("Origin", format!("http://localhost:{port}")),
    )
    .expect("the runner's own origin opens the socket")
    .0;
    assert_eq!(
        reply(&mut socket).2,
        Some(Step {
            subject: 0,
            test: 0
        })
    );
}

#[test]
fn the_socket_is_sent_the_step_in_hand_and_drives_the_run_to_its_end() {
    let (port, served) = running();
    let mut socket = socket(port).expect("the run's socket opens");

    // Opened, it is sent the step in hand with nothing written.
    assert_eq!(
        reply(&mut socket),
        (
            String::new(),
            String::new(),
            Some(Step {
                subject: 0,
                test: 0
            })
        )
    );
    // The first step, its guest having written `hi`: what was written, and the next step.
    socket.send(outcome("aGk=", None)).unwrap();
    assert_eq!(
        reply(&mut socket),
        (
            "histep 0/0\n".to_string(),
            String::new(),
            Some(Step {
                subject: 0,
                test: 1
            })
        )
    );
    // The first page's last step switches the page in hand.
    socket.send(outcome("", None)).unwrap();
    assert_eq!(
        reply(&mut socket).2,
        Some(Step {
            subject: 1,
            test: 0
        })
    );
    assert_eq!(get(port, "/index.html").2, b"<!doctype html>second");
    assert_eq!(get(port, "/index.wasm").2, b"\0asm2");
    // The last step, trapped: its trap on stderr, no next step, and the serve ends, its port closed.
    socket.send(outcome("", Some("boom"))).unwrap();
    assert_eq!(
        reply(&mut socket),
        ("step 1/0\n".to_string(), "boom".to_string(), None)
    );
    served.join().unwrap().unwrap();
    assert!(TcpStream::connect((Ipv4Addr::LOCALHOST, port)).is_err());
}

#[test]
fn a_second_socket_is_refused_while_one_drives_and_the_next_resumes_at_the_step_in_hand() {
    let (port, _served) = running();
    let mut driving = socket(port).expect("the run's socket opens");
    reply(&mut driving);
    driving.send(outcome("", None)).unwrap();
    reply(&mut driving);

    match socket(port) {
        Err(Error::Http(response)) => assert_eq!(response.status(), 409),
        other => panic!("a second socket is refused, not {:?}", other.map(|_| ())),
    }

    // Once the driving socket has closed, the run is let go, and the next to open resumes it where it stands.
    driving.close(None).unwrap();
    while driving.read().is_ok() {}
    let mut resumed = socket(port).expect("a socket opens once none drives the run");
    assert_eq!(
        reply(&mut resumed).2,
        Some(Step {
            subject: 0,
            test: 1
        })
    );
}
