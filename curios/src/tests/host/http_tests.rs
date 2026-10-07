//! HTTP: the messages a program is refused where it writes them, a server's connection from the request it reads to the close, and a client's reply read as its framing delimits it.

use {
    crate::tests::{Compiled, compile, typecheck},
    curios_runtime::{MockHost, MockHostBuilder, SocketEvent},
};

/// A program that makes one message and performs nothing.
fn making(message: &str) -> String {
    format!(
        "use /std/{{Str, http}};\nuse /std/http/{{Url, Request, Response}};\nlet _ = {message};\n/std/print(\"\")"
    )
}

/// The report `message` is refused with.
fn refusal(message: &str) -> String {
    typecheck(&making(message)).expect_err("the message is refused")
}

// A message no wire could carry is refused where it is written, by the proposition its literal fails: a field name that is no token, a value that holds a line's end, a status code that is not three digits, and content under a status that carries none. The same makers accept what a wire carries.
#[test]
fn a_message_no_wire_carries_is_refused_where_it_is_written() {
    let get = r#"Request/get(Url/lit("http://h/"))"#;

    for accepted in [
        format!(r#"Request/with_header({get}, "X-A", "one")"#),
        r#"Response/of_text(200, "hello")"#.to_string(),
        r#"Response/of(204, "text/plain", x[])"#.to_string(),
        "Response/empty(304)".to_string(),
    ] {
        assert_eq!(typecheck(&making(&accepted)), Ok(()), "{accepted}");
    }

    let name = refusal(&format!(r#"Request/with_header({get}, "X (A)", "one")"#));
    assert!(name.contains("nothing discharged"), "{name}");
    assert!(name.contains(r#"is_token("X (A)")"#), "{name}");

    for refused in [
        format!(r#"Request/with_header({get}, "X-A", "one\r\nX-Injected: two")"#),
        r#"Response/of(204, "text/plain", Str/to_bytes("oops"))"#.to_string(),
        r#"Response/of_text(204, "oops")"#.to_string(),
        r#"Response/of_text(1000, "oops")"#.to_string(),
        "Response/empty(99)".to_string(),
    ] {
        let report = refusal(&refused);
        assert!(report.contains("nothing discharged"), "{refused}: {report}");
    }
}

/// A server over one connection: it accepts it, answers it as `http/Server/answer` does under the default bounds, and closes it. Its handler answers the method, the path and how many octets of content arrived, and fails where the path is `/fail`. `Listener/serve` is not what runs it: the scripted host's accept fails once its connections are spent, which ends that loop under a connection still parked.
const SERVER: &str = r#"
    use /std/{Str, Bytes, Nat, Show, Try, Async, Io};
    use /std/tcp/{Listener, Socket};
    use /std/http/{Server, Request, Response};
    use /std/http/Url/{Scheme};
    let handler(r: Request) -> Try(Async, Io/Error, Response) =
        match r.url.path == "/fail"
        | true => Try/raise(Io/Error/refused())
        | false =>
            Try/pure(
                Response/of_text(
                    200,
                    Str/flatten([Show/show(r.method), " ", r.url.path, " ", Nat/to_str(Bytes/len(r.body))])))
        end;
    let session: Try(Async, Io/Error, {}) =
        let l = Listener/listen("0.0.0.0", 8080)!;
        let c = Listener/accept(l)!;
        let _ = Server/answer(Server/default, Scheme/http(), handler)(c)!;
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

/// What the server wrote to its one connection under `host`, and that connection's events in order.
fn served(server: &Compiled, host: MockHostBuilder) -> (String, Vec<SocketEvent>) {
    let (system, io) = host.build();

    server.run(system).expect("the server runs to its end");
    assert_eq!(io.output(), b"", "the session fails nowhere");

    let written = io.captures().concat();

    (
        String::from_utf8(written).expect("a reply in ASCII"),
        io.socket_events(),
    )
}

/// The reply to `request`, sent whole by a peer that then waits for it.
fn reply(server: &Compiled, request: &str) -> String {
    served(server, MockHost::builder().inbound_waiting([vec![request]])).0
}

/// A reply's status line.
fn status(reply: &str) -> &str {
    reply.split("\r\n").next().expect("a first line")
}

/// A reply's body: what follows its head.
fn body(reply: &str) -> &str {
    reply.split_once("\r\n\r\n").expect("a head's end").1
}

// A server answers one connection as HTTP asks, from the request it reads to the close. The server is compiled once, and each claim below runs it under the scripted peers that show it.
#[test]
fn a_server_answers_a_connection_as_http_asks() {
    let server = compile(SERVER).expect("the server compiles");

    a_request_is_read_to_where_its_framing_ends(&server);
    a_chunked_request_is_read_whole_across_its_segments(&server);
    a_refused_request_is_answered_with_its_status(&server);
    a_request_that_stops_arriving_is_answered_408(&server);
    an_expectation_is_answered_from_the_head(&server);
    a_reply_to_head_is_its_head_alone(&server);
    a_reply_is_written_finished_and_read_past_before_the_close(&server);
}

// A request is read to where its framing ends and no further: the peer keeps the connection open and says no more, so a server that read past the request would be waiting on it. The handler sees the content its length states and the content its chunks hold.
fn a_request_is_read_to_where_its_framing_ends(server: &Compiled) {
    for (request, answer) in [
        ("GET /x HTTP/1.1\r\nHost: h\r\n\r\n", "GET /x 0"),
        (
            "POST /x HTTP/1.1\r\nHost: h\r\nContent-Length: 5\r\n\r\nhello",
            "POST /x 5",
        ),
        (
            "POST /x HTTP/1.1\r\nHost: h\r\nTransfer-Encoding: chunked\r\n\r\n4\r\nWiki\r\n5\r\npedia\r\n0\r\n\r\n",
            "POST /x 9",
        ),
        ("GET /x HTTP/1.0\r\nHost: h\r\n\r\n", "GET /x 0"),
    ] {
        let reply = reply(server, request);

        assert_eq!(status(&reply), "HTTP/1.1 200 OK", "{request}");
        assert_eq!(body(&reply), answer, "{request}");
    }
}

// A chunked request arrives in segments cut inside a size line, inside a chunk and inside the trailer section, and is read whole across each.
fn a_chunked_request_is_read_whole_across_its_segments(server: &Compiled) {
    let segments = vec![
        "POST /x HTTP/1.1\r\nHost: h\r\nTransfer-",
        "Encoding: chunked\r\n\r\n4",
        "\r\nWi",
        "ki\r\n5\r\npedia\r\n0\r\nExpires: ",
        "never\r\n\r\n",
    ];

    let (reply, _) = served(server, MockHost::builder().inbound_waiting([segments]));

    assert_eq!(status(&reply), "HTTP/1.1 200 OK");
    assert_eq!(body(&reply), "POST /x 9");
}

// Each refusal is answered with the status it names, and a handler that fails with 500: a request that is no request, one whose peer closes before it is whole, a request line, a head and content past their bounds, a method and a coding the server does not implement, and a version it does not serve.
fn a_refused_request_is_answered_with_its_status(server: &Compiled) {
    let long_line = format!("GET /{} HTTP/1.1\r\nHost: h\r\n\r\n", "a".repeat(9000));
    let large_head = format!(
        "GET / HTTP/1.1\r\nHost: h\r\n{}\r\n",
        format!("X-A: {}\r\n", "a".repeat(1000)).repeat(70)
    );

    for (request, expected) in [
        ("GET /\r\n\r\n", "HTTP/1.1 400 Bad Request"),
        ("GET / HTTP/1.1\r\n\r\n", "HTTP/1.1 400 Bad Request"),
        (
            "POST / HTTP/1.1\r\nHost: h\r\nContent-Length: 4194305\r\n\r\n",
            "HTTP/1.1 413 Content Too Large",
        ),
        (long_line.as_str(), "HTTP/1.1 414 URI Too Long"),
        (
            large_head.as_str(),
            "HTTP/1.1 431 Request Header Fields Too Large",
        ),
        (
            "GET /fail HTTP/1.1\r\nHost: h\r\n\r\n",
            "HTTP/1.1 500 Internal Server Error",
        ),
        (
            "BREW / HTTP/1.1\r\nHost: h\r\n\r\n",
            "HTTP/1.1 501 Not Implemented",
        ),
        (
            "POST / HTTP/1.1\r\nHost: h\r\nTransfer-Encoding: gzip, chunked\r\n\r\n",
            "HTTP/1.1 501 Not Implemented",
        ),
        (
            "GET / HTTP/2.0\r\nHost: h\r\n\r\n",
            "HTTP/1.1 505 HTTP Version Not Supported",
        ),
    ] {
        assert_eq!(status(&reply(server, request)), expected, "{request:.40}");
    }

    // A peer that closes inside its request is answered too, where one that connects and says nothing is told nothing.
    for (request, expected) in [
        (
            "POST / HTTP/1.1\r\nHost: h\r\nContent-Length: 5\r\n\r\nhel",
            "HTTP/1.1 400 Bad Request",
        ),
        ("GET / HTT", "HTTP/1.1 400 Bad Request"),
        ("", ""),
    ] {
        let (reply, _) = served(server, MockHost::builder().inbound([request]));

        assert_eq!(status(&reply), expected, "{request}");
    }
}

// A peer that stops sending and stays is answered 408 once the request's deadline has passed: the clock climbs a second a reading, and nothing else moves.
fn a_request_that_stops_arriving_is_answered_408(server: &Compiled) {
    let host = MockHost::builder()
        .inbound_waiting([vec![
            "POST / HTTP/1.1\r\nHost: h\r\nContent-Length: 5\r\n\r\nhel",
        ]])
        .mono((0..200u64).map(|s| (s, 0)));

    let (reply, _) = served(server, host);

    assert_eq!(status(&reply), "HTTP/1.1 408 Request Timeout");
}

// A request that expects `100 Continue` is told to continue once its head is read and before its content is: the content is a segment of its own, which the peer would not send unasked. A request whose content came with its head is not told, and one its head already refuses is refused at once.
fn an_expectation_is_answered_from_the_head(server: &Compiled) {
    let head = "POST /x HTTP/1.1\r\nHost: h\r\nExpect: 100-continue\r\nContent-Length: 5\r\n\r\n";

    let (invited, events) = served(
        server,
        MockHost::builder().inbound_waiting([vec![head, "hello"]]),
    );
    let (continued, answered) = invited
        .split_once("\r\n\r\n")
        .expect("an interim reply ahead of the final one");

    assert_eq!(continued, "HTTP/1.1 100 Continue");
    assert_eq!(status(answered), "HTTP/1.1 200 OK");
    assert_eq!(body(answered), "POST /x 5");
    assert_eq!(events.first(), Some(&SocketEvent::Wrote(25)));

    let whole = reply(server, &format!("{head}hello"));

    assert_eq!(status(&whole), "HTTP/1.1 200 OK");

    let refused = reply(
        server,
        "POST /x HTTP/1.1\r\nHost: h\r\nExpect: 100-continue\r\nContent-Length: 4194305\r\n\r\n",
    );

    assert_eq!(status(&refused), "HTTP/1.1 413 Content Too Large");
}

// A reply to `HEAD` is its head alone, stating the length of the body the handler made.
fn a_reply_to_head_is_its_head_alone(server: &Compiled) {
    let reply = reply(server, "HEAD /x HTTP/1.1\r\nHost: h\r\n\r\n");

    assert_eq!(status(&reply), "HTTP/1.1 200 OK");
    assert!(reply.contains("\r\nContent-Length: 9\r\n"), "{reply}");
    assert_eq!(body(&reply), "");
}

// A connection is ended as the protocol asks: the reply written, the sending half finished, what the peer still sends read until the peer closes, and only then the close. A refusal sent before the request's content is read reaches a peer that is still sending it; and a peer that neither sends nor closes is closed on once it has been quiet.
fn a_reply_is_written_finished_and_read_past_before_the_close(server: &Compiled) {
    let large = "POST / HTTP/1.1\r\nHost: h\r\nContent-Length: 4194305\r\n\r\n";

    let (reply, events) = served(
        server,
        MockHost::builder().inbound_chunks([vec![large, "aaaa", "bbbb"]]),
    );

    assert_eq!(status(&reply), "HTTP/1.1 413 Content Too Large");
    assert_eq!(
        events,
        [
            SocketEvent::Wrote(reply.len()),
            SocketEvent::Finished,
            SocketEvent::Ended,
            SocketEvent::Closed,
        ]
    );

    let (reply, events) = served(
        server,
        MockHost::builder()
            .inbound_open([vec!["GET /x HTTP/1.1\r\nHost: h\r\n\r\n"]])
            .mono((0..200u64).map(|s| (s, 0))),
    );

    assert_eq!(status(&reply), "HTTP/1.1 200 OK");
    assert_eq!(
        events,
        [
            SocketEvent::Wrote(reply.len()),
            SocketEvent::Finished,
            SocketEvent::Closed,
        ]
    );
}

/// A client that asks `example.com` with `GET` and `head.example` with `HEAD`, and says of each what came of it on a line of its own: the status and the content, or the failure with what it carries.
const CLIENT: &str = r#"
    use /std/{Str, Bytes, Nat, Option, Show, Try, Async, http};
    use /std/http/{Method, Url, Request};
    let text(b: Bytes) -> Str =
        Option/unwrap_or(Str/of_bytes(b), "not text");
    let told(method: Method, url: Url) -> Async(Str) =
        let r =
            Try/run(http/perform(Request { method = method, url = url, headers = [], body = x[] }))!;
        Async/pure(
            match r
            | success(response) =>
                Str/flatten([Nat/to_str(response.status.code), " ", text(response.body)])
            | failure(e) =>
                match e
                | net(why) => Str/concat("net ", Show/show(why))
                | refused(why) => Str/concat("refused ", Show/show(why))
                | incomplete(arrived) =>
                    match arrived
                    | none() => "incomplete"
                    | some((head, content)) =>
                        Str/flatten(["incomplete ", Nat/to_str(head.status.code), " ", text(content)])
                    end
                | timeout() => "timeout"
                | redirected(_) => "redirected"
                end
            end);
    let fiber: Async({}) =
        let got = told(Method/get(), Url/lit("http://example.com/"))!;
        let headed = told(Method/head(), Url/lit("http://head.example/"))!;
        /std/print(Str/flatten([got, "\n", headed]));
    Async/run(fiber)
    "#;

/// What the client says of its `GET` to `example.com` under `host`.
fn got(client: &Compiled, host: MockHostBuilder) -> String {
    told(client, host).0
}

/// What the client says of its `GET` and of its `HEAD` under `host`.
fn told(client: &Compiled, host: MockHostBuilder) -> (String, String) {
    let (system, io) = host.build();

    client.run(system).expect("the client runs to its end");

    let said = String::from_utf8(io.output()).expect("lines of text");
    let (got, headed) = said.split_once('\n').expect("a line for each request");

    (got.to_string(), headed.to_string())
}

// A client reads a reply as its framing delimits it and says how one that is not whole fell short. The client is compiled once, and each claim below runs it under the scripted peers that show it.
#[test]
fn a_client_reads_a_reply_as_its_framing_delimits_it() {
    let client = compile(CLIENT).expect("the client compiles");

    a_reply_is_read_to_where_its_framing_ends(&client);
    a_reply_cut_short_is_incomplete_with_what_arrived(&client);
    a_reply_that_is_not_read_says_why(&client);
}

// A reply is read to where its framing ends, and no further: the peer answers and keeps the connection open, so a client that read to the close would wait out its deadline. A reply to `HEAD` that states a length has no body to wait for, and a `100 Continue` ahead of the final reply is passed over.
fn a_reply_is_read_to_where_its_framing_ends(client: &Compiled) {
    for (reply, said) in [
        (
            "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nhello",
            "200 hello",
        ),
        (
            "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n4\r\nWiki\r\n5\r\npedia\r\n0\r\n\r\n",
            "200 Wikipedia",
        ),
        ("HTTP/1.1 204 No Content\r\n\r\n", "204 "),
        (
            "HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nhi",
            "200 hi",
        ),
    ] {
        let host = MockHost::builder().net_open([("example.com:80", vec![reply])]);

        assert_eq!(got(client, host), said, "{reply}");
    }

    let host = MockHost::builder().net_open([(
        "head.example:80",
        vec!["HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\n"],
    )]);

    assert_eq!(told(client, host).1, "200 ");
}

// A reply the peer closes on before it is whole is `incomplete`, and what arrived comes with it: the head and the content received where the head was whole — the octets of a stated length, the whole chunks of a chunked body — and nothing where the head itself was cut short. A reply framed by the close is whole when the peer closes.
fn a_reply_cut_short_is_incomplete_with_what_arrived(client: &Compiled) {
    for (reply, said) in [
        (
            "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\nhello",
            "incomplete 200 hello",
        ),
        (
            "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n4\r\nWiki\r\n5\r\npe",
            "incomplete 200 Wiki",
        ),
        ("HTTP/1.1 200 OK\r\nContent-Le", "incomplete"),
        ("", "incomplete"),
        ("HTTP/1.1 200 OK\r\n\r\nhello", "200 hello"),
    ] {
        let host = MockHost::builder().net([("example.com:80", reply)]);

        assert_eq!(got(client, host), said, "{reply}");
    }
}

// A reply that is no reply, or one past the client's bounds, is refused with the reason; a peer that resets is a failure of the wire, by its name; and one that is not there is refused its connection.
fn a_reply_that_is_not_read_says_why(client: &Compiled) {
    for (reply, said) in [
        ("not http at all\r\n\r\n", "refused malformed: "),
        (
            "HTTP/1.1 200 OK\r\nContent-Length: 67108865\r\n\r\n",
            "refused the content is too large",
        ),
    ] {
        let host = MockHost::builder().net([("example.com:80", reply)]);
        let got = got(client, host);

        assert!(got.starts_with(said), "{reply}: {got}");
    }

    let reset = MockHost::builder().net_reset([(
        "example.com:80",
        vec!["HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\nhel"],
    )]);

    assert_eq!(got(client, reset), "net reset");
    assert_eq!(got(client, MockHost::builder()), "net refused");
}
