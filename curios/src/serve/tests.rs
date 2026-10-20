use {super::*, std::io::Read};

/// A page named `app` holding a page, its program and one file under a directory, served on a port of its own until the test's process ends.
fn serving() -> u16 {
    let listener = bound(0).expect("a port of loopback's own");
    let port = listener.local_addr().unwrap().port();
    let served = Served {
        name: "app".to_string(),
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
fn the_pages_directory_answers_its_page_and_a_path_inside_it_its_file() {
    let port = serving();

    let (status, headers, body) = get(port, "/app/");
    assert_eq!(status, "HTTP/1.1 200 OK");
    assert!(
        headers.contains(&"Content-Type: text/html; charset=utf-8".to_string()),
        "{headers:?}"
    );
    assert_eq!(body, b"<!doctype html>");

    let (status, _, body) = get(port, "/app/web/a%20b.js?v=1");
    assert_eq!(status, "HTTP/1.1 200 OK");
    assert_eq!(body, b"export {};");
}

#[test]
fn the_program_is_served_as_webassembly() {
    let port = serving();

    let (_, headers, _) = get(port, "/app/index.wasm");

    assert!(
        headers.contains(&"Content-Type: application/wasm".to_string()),
        "{headers:?}"
    );
}

#[test]
fn every_reply_says_no_store_and_closes() {
    let port = serving();

    for path in ["/app/", "/app/missing", "/"] {
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
fn the_root_and_the_pages_name_are_redirected_into_the_page() {
    let port = serving();

    for path in ["/", "/app"] {
        let (status, headers, _) = get(port, path);

        assert_eq!(status, "HTTP/1.1 308 Permanent Redirect", "{path}");
        assert!(
            headers.contains(&"Location: /app/".to_string()),
            "{path}: {headers:?}"
        );
    }
}

#[test]
fn a_path_the_page_holds_no_file_at_is_not_found() {
    let port = serving();

    for path in ["/app/missing.js", "/other/", "/app/web"] {
        assert_eq!(get(port, path).0, "HTTP/1.1 404 Not Found", "{path}");
    }
}

#[test]
fn a_method_other_than_get_is_refused_naming_get() {
    let port = serving();

    let (status, headers, _) = asked(
        port,
        &format!("POST /app/ HTTP/1.1\r\nHost: 127.0.0.1:{port}\r\nContent-Length: 0\r\n\r\n"),
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
            &format!("GET /app/ HTTP/1.1\r\nHost: localhost:{port}\r\n\r\n")
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
            asked(port, &format!("GET /app/ HTTP/1.1\r\nHost: {host}\r\n\r\n")).0,
            "HTTP/1.1 421 Misdirected Request",
            "{host:?}"
        );
    }
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
fn a_url_names_the_page_and_carries_each_argument_percent_encoded() {
    let served = Served {
        name: "my app".to_string(),
        files: BTreeMap::new(),
    };

    assert_eq!(served.url(8000, &[]), "http://127.0.0.1:8000/my%20app/");
    assert_eq!(
        served.url(8001, &["one".into(), "a&b=c+d".into(), "é".into()]),
        "http://127.0.0.1:8001/my%20app/?arg=one&arg=a%26b%3Dc%2Bd&arg=%C3%A9"
    );
}

#[test]
fn a_path_is_decoded_before_it_is_looked_for_and_a_malformed_escape_is_refused() {
    let port = serving();

    assert_eq!(get(port, "/%61pp/web/a%20b.js").0, "HTTP/1.1 200 OK");
    for path in ["/app/web/a%2", "/app/web/a%+1.js", "/app/%FF"] {
        assert_eq!(get(port, path).0, "HTTP/1.1 400 Bad Request", "{path}");
    }
}
