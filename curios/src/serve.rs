//! Serving a browser on this machine: the files a browser build is, or a run of tests, held in memory and answered over loopback — a page until the process is stopped, a run until every test has reported.
//!
//! **Only what a page needs.** A request is a `GET` of a path, answered with the file at that page path, or with `index.html`, the page's own name for its document, for `/`; the page stands at the root, so its relative paths resolve there. Every reply says `Cache-Control: no-store`, since a page changes by running the command again, and `Connection: close`, so no connection outlives its one request but a run's socket.
//!
//! **A run is the runner at the root, its socket, and one subject's page beside them.** `GET /` answers the runner, and the same request asking to upgrade opens the socket the runner drives the run over. The run is driven over one socket at a time: it is sent the step in hand — the first unreported test in plan order — and each outcome it sends back is taken as that step's, answered with what the command wrote for it and the next step. A socket asked for while another drives the run is refused, and one that closes, however it closes, lets the run go, so the next to open resumes it at the step in hand. `index.html` and every other path is the page of the subject in hand, which switches to the next subject with its last report; the run is complete when every step is taken.
//!
//! **Loopback, and only by its own name.** The server binds `127.0.0.1` and refuses a request whose `Host` is not that address or `localhost` at its port, which keeps a page some other site loads from reaching it through a name that resolves here, and a socket asked for under an `Origin` other than those, which a browser states for every page and which no page can forge, so only the runner a run serves drives it.

#[cfg(test)]
mod tests;

use {
    crate::Streams,
    curios_browser::{ARGUMENT, INDEX_HTML, Posted, Reply, Step, content_type, runner},
    std::{
        collections::BTreeMap,
        ffi::OsString,
        io::{self, BufRead, BufReader, Write},
        net::{Ipv4Addr, TcpListener, TcpStream},
        sync::{Mutex, PoisonError},
        thread,
    },
    tungstenite::{
        Message, WebSocket,
        handshake::derive_accept_key,
        protocol::{CloseFrame, Role, WebSocketConfig, frame::coding::CloseCode},
    },
};

/// The port a page is served on where `--port` names none.
pub(crate) const PORT: u16 = 8000;

/// The most a request's head may hold before it is refused: far past any a browser sends for a page.
const HEAD: usize = 16 * 1024;

/// The most an outcome a runner sends may hold before its socket is refused: past any output a test writes.
const OUTCOME: usize = 16 * 1024 * 1024;

/// The WebSocket version the run's socket speaks, the one every browser asks for.
const VERSION: &str = "13";

/// A page held to serve: every file of it by its page path.
pub(crate) struct Served {
    pub(crate) files: BTreeMap<String, Vec<u8>>,
}

/// What a serve answers: one page, as `run` serves it, or a run of tests.
pub(crate) enum Site<'a> {
    Page(Served),
    Run(Run<'a>),
}

impl Site<'_> {
    /// Whether the serve is done: never for a page, and for a run once every step is taken.
    fn complete(&self) -> bool {
        match self {
            Site::Page(_) => false,
            Site::Run(run) => run.in_hand().is_none(),
        }
    }
}

/// A run of tests served: the runner document, each subject's page in plan order with how many tests it schedules, and where the run is.
pub(crate) struct Run<'a> {
    runner: Vec<u8>,
    pages: Vec<(Served, usize)>,
    state: Mutex<State<'a>>,
}

/// What the command does with the step in hand's outcome — prints and tallies — answering what it wrote on each stream.
type Reporting<'a> = Box<dyn FnMut(Step, Streams, &Posted) -> Streams + Send + 'a>;

/// Where a run is, whether a socket drives it, and what takes its outcomes.
struct State<'a> {
    /// The page in hand, past the last once the run is complete, and the test in hand on it.
    page: usize,
    test: usize,
    driven: bool,
    report: Reporting<'a>,
}

/// The step in hand's outcome is read only over a socket that has been sent that step, which a run past its last never sends.
const DRIVEN_WHILE_IN_HAND: &str = "an outcome is read only while a step is in hand";

impl<'a> Run<'a> {
    /// A run over `pages`, each with how many tests it schedules, `report` taking each step's outcome.
    pub(crate) fn new(
        pages: Vec<(Served, usize)>,
        report: impl FnMut(Step, Streams, &Posted) -> Streams + Send + 'a,
    ) -> Self {
        Self {
            runner: runner(),
            pages,
            state: Mutex::new(State {
                page: 0,
                test: 0,
                driven: false,
                report: Box::new(report),
            }),
        }
    }

    fn state(&self) -> std::sync::MutexGuard<'_, State<'a>> {
        self.state.lock().unwrap_or_else(PoisonError::into_inner)
    }

    /// The step in hand, or none once the run is complete.
    fn in_hand(&self) -> Option<Step> {
        let state = self.state();

        step_of(&state, self.pages.len())
    }

    /// The page in hand's files, where a page is in hand.
    fn current(&self) -> Option<&Served> {
        let state = self.state();

        self.pages.get(state.page).map(|(page, _)| page)
    }

    /// The run held for a socket opening now, or none where another socket drives it.
    fn held(&self) -> Option<Held<'_, 'a>> {
        let mut state = self.state();
        if state.driven {
            return None;
        }
        state.driven = true;

        Some(Held(self))
    }

    /// The step in hand's outcome taken, `guest` being the streams its page kept: what the command wrote for it, and the step in hand after it.
    fn reported(&self, outcome: &Posted, guest: Streams) -> (Streams, Option<Step>) {
        let mut state = self.state();
        let step = step_of(&state, self.pages.len()).expect(DRIVEN_WHILE_IN_HAND);

        let written = (state.report)(step, guest, outcome);
        state.test += 1;
        if state.test == self.pages[state.page].1 {
            state.page += 1;
            state.test = 0;
        }

        (written, step_of(&state, self.pages.len()))
    }
}

/// A run while one socket drives it, let go however the socket ends.
struct Held<'r, 'a>(&'r Run<'a>);

impl Drop for Held<'_, '_> {
    fn drop(&mut self) {
        self.0.state().driven = false;
    }
}

/// The step `state` is at, or none past the last of `pages` pages.
fn step_of(state: &State<'_>, pages: usize) -> Option<Step> {
    (state.page < pages).then_some(Step {
        subject: state.page,
        test: state.test,
    })
}

/// Where a browser opens the page served on `port`, its program handed `args`: the root, each argument a pair of the query under the key the page claims, every byte but the unreserved ones percent-encoded, as the loader decodes them.
pub(crate) fn url(port: u16, args: &[OsString]) -> String {
    let query = args
        .iter()
        .map(|arg| {
            let mut pair = format!("{ARGUMENT}=");
            for &byte in arg.as_encoded_bytes() {
                match byte {
                    b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'.' | b'_' | b'~' => {
                        pair.push(char::from(byte));
                    }
                    _ => pair.push_str(&format!("%{byte:02X}")),
                }
            }

            pair
        })
        .collect::<Vec<_>>()
        .join("&");

    match query.is_empty() {
        true => format!("http://127.0.0.1:{port}/"),
        false => format!("http://127.0.0.1:{port}/?{query}"),
    }
}

/// The listener a page is served on, at `port` of loopback, or why there is none: a port another process holds is refused by its number and by the flag that picks another.
pub(crate) fn bound(port: u16) -> Result<TcpListener, String> {
    TcpListener::bind((Ipv4Addr::LOCALHOST, port)).map_err(|error| {
        format!("cannot serve on 127.0.0.1:{port}: {error}; `--port` serves on another")
    })
}

/// Answer every connection `listener` accepts with `served`, each on a thread of its own, until the process is stopped.
pub(crate) fn serve_page(listener: &TcpListener, served: Served) -> Result<(), String> {
    serve(listener, &Site::Page(served))
}

/// Answer every connection `listener` accepts with `run`, each on a thread of its own, until every step of the run is taken and its last reply sent.
pub(crate) fn serve_run(listener: &TcpListener, run: Run<'_>) -> Result<(), String> {
    serve(listener, &Site::Run(run))
}

/// Answer every connection `listener` accepts with `site`, each on a thread of its own, until the site is complete: the socket that takes a run's last step connects back once its reply is sent, so the accept ends.
fn serve(listener: &TcpListener, site: &Site<'_>) -> Result<(), String> {
    let port = listener
        .local_addr()
        .map_err(|error| format!("cannot read the address served on: {error}"))?
        .port();

    thread::scope(|scope| {
        // A connection that fails before it is accepted is its client's, and ends nothing else.
        for stream in listener.incoming().flatten() {
            if site.complete() {
                break;
            }

            scope.spawn(move || {
                // A client that goes away mid-reply is no failure of the server's.
                let _ = answer(stream, site, port);
            });
        }
    });

    Ok(())
}

/// One response: its status line, the headers past the ones every response carries, and its body.
struct Response {
    status: &'static str,
    headers: Vec<(&'static str, String)>,
    body: Vec<u8>,
}

impl Response {
    fn text(status: &'static str, body: &str) -> Self {
        Self {
            status,
            headers: vec![("Content-Type", "text/plain; charset=utf-8".to_string())],
            body: format!("{body}\n").into_bytes(),
        }
    }

    fn file(path: &str, bytes: &[u8]) -> Self {
        Self {
            status: "200 OK",
            headers: vec![("Content-Type", content_type(path).to_string())],
            body: bytes.to_vec(),
        }
    }
}

/// What a request is answered with: a response, or the switch to a run's socket under the accept key its handshake asked for, the run held for it.
enum Answer<'r, 'a> {
    Response(Response),
    Socket { accept: String, held: Held<'r, 'a> },
}

impl From<Response> for Answer<'_, '_> {
    fn from(response: Response) -> Self {
        Answer::Response(response)
    }
}

/// Read one request from `stream` and answer it: write its response, or switch to the run's socket and drive the run over it.
fn answer(stream: TcpStream, site: &Site<'_>, port: u16) -> io::Result<()> {
    let mut reader = BufReader::new(&stream);
    let answered = match head(&mut reader)? {
        Some(head) => replied(&head, site, port),
        None => Response::text(
            "431 Request Header Fields Too Large",
            "the request's head is too large",
        )
        .into(),
    };
    // What the client sent past the head is the socket's, where one opens.
    let past = reader.buffer().to_vec();

    let mut stream = &stream;
    match answered {
        Answer::Response(response) => {
            write!(stream, "HTTP/1.1 {}\r\n", response.status)?;
            for (name, value) in &response.headers {
                write!(stream, "{name}: {value}\r\n")?;
            }
            write!(
                stream,
                "Content-Length: {}\r\nCache-Control: no-store\r\nConnection: close\r\n\r\n",
                response.body.len()
            )?;
            stream.write_all(&response.body)?;
            stream.flush()
        }
        Answer::Socket { accept, held } => {
            write!(
                stream,
                "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: {accept}\r\n\r\n"
            )?;
            stream.flush()?;
            let socket = WebSocket::from_partially_read(
                stream,
                past,
                Role::Server,
                Some(WebSocketConfig::default().max_message_size(Some(OUTCOME))),
            );

            drive(socket, held, port).map_err(io::Error::other)
        }
    }
}

/// Drive the run `held` holds over `socket`: the step in hand sent at once, then each outcome the runner sends taken as that step's and answered with what the command wrote and the next step, until the last, after which the socket is closed and the accept, listening on `port`, woken. A message that is no outcome closes the socket saying so.
fn drive(
    mut socket: WebSocket<&TcpStream>,
    held: Held<'_, '_>,
    port: u16,
) -> tungstenite::Result<()> {
    let run = held.0;
    let mut next = run.in_hand();
    socket.send(said(&Reply::new(&[], &[], next)))?;

    while next.is_some() {
        let read = match socket.read()? {
            Message::Text(text) => {
                Posted::parse(text.as_bytes()).and_then(|outcome| Ok((outcome.streams()?, outcome)))
            }
            Message::Binary(_) => Err("an outcome is a text message".to_string()),
            Message::Close(_) => return Ok(()),
            Message::Ping(_) | Message::Pong(_) | Message::Frame(_) => continue,
        };
        let Ok(((stdout, stderr), outcome)) = read else {
            return socket.close(Some(CloseFrame {
                code: CloseCode::Invalid,
                reason: "the message is no outcome".into(),
            }));
        };

        let (written, after) = run.reported(&outcome, Streams { stdout, stderr });
        next = after;
        socket.send(said(&Reply::new(&written.stdout, &written.stderr, next)))?;
    }

    socket.close(None)?;
    // The accept is blocked until a connection arrives: this one is it, and the loop ends on it without answering it.
    let _ = TcpStream::connect((Ipv4Addr::LOCALHOST, port));

    Ok(())
}

/// `reply` as the socket carries it.
fn said(reply: &Reply) -> Message {
    Message::text(String::from_utf8(reply.render()).expect("a reply renders as JSON"))
}

/// The request's head, its lines up to the blank one, or `None` where it runs past [`HEAD`].
fn head(reader: &mut impl BufRead) -> io::Result<Option<Vec<String>>> {
    let mut lines = Vec::new();
    let mut read = 0;

    loop {
        let mut line = String::new();
        let taken = reader.read_line(&mut line)?;
        read += taken;

        if read > HEAD {
            return Ok(None);
        }

        let line = line.trim_end_matches(['\r', '\n']);
        if taken == 0 || line.is_empty() {
            return Ok(Some(lines));
        }

        lines.push(line.to_string());
    }
}

/// The header `name` of `head`, trimmed, where the request carries it.
fn header<'h>(head: &'h [String], name: &str) -> Option<&'h str> {
    head.iter().skip(1).find_map(|line| {
        let (key, value) = line.split_once(':')?;

        key.eq_ignore_ascii_case(name).then(|| value.trim())
    })
}

/// What answers the request whose head is `head`, served on `port`.
fn replied<'r, 'a>(head: &[String], site: &'r Site<'a>, port: u16) -> Answer<'r, 'a> {
    let mut words = head.first().map_or("", String::as_str).split(' ');
    let (method, target) = (words.next().unwrap_or(""), words.next().unwrap_or(""));

    if !header(head, "host").is_some_and(|host| own(host, port)) {
        return Response::text(
            "421 Misdirected Request",
            "this server answers 127.0.0.1 and localhost at its own port alone",
        )
        .into();
    }

    let Some(path) = decoded(target.split(['?', '#']).next().unwrap_or("")) else {
        return Response::text("400 Bad Request", "the path is not percent-encoded UTF-8").into();
    };
    let upgrade =
        header(head, "upgrade").is_some_and(|upgrade| upgrade.eq_ignore_ascii_case("websocket"));

    match (method, site) {
        ("GET", Site::Page(served)) => file(served, &path).into(),
        // The root is the run, and its socket; the page in hand stands beside them, by its own name for its document and its files' page paths.
        ("GET", Site::Run(run)) if path == "/" && upgrade => socket(head, run, port),
        ("GET", Site::Run(run)) if path == "/" => Response::file(INDEX_HTML, &run.runner).into(),
        ("GET", Site::Run(run)) => match run.current() {
            Some(page) => file(page, &path),
            None => Response::text("404 Not Found", "the run is complete"),
        }
        .into(),
        (_, Site::Page(_)) => refused("GET", "a page is read with GET alone").into(),
        (_, Site::Run(_)) => refused(
            "GET",
            "a run is read with GET, and driven over the socket its runner opens at /",
        )
        .into(),
    }
}

/// Whether `authority` names this server: `127.0.0.1` or `localhost` at `port`.
fn own(authority: &str, port: u16) -> bool {
    authority == format!("127.0.0.1:{port}") || authority == format!("localhost:{port}")
}

/// The socket of `run` the handshake whose head is `head` asks for: refused where it states an `Origin` other than this server's, where it speaks another version or states no key, and where another socket drives the run.
fn socket<'r, 'a>(head: &[String], run: &'r Run<'a>, port: u16) -> Answer<'r, 'a> {
    let foreign = header(head, "origin").is_some_and(|origin| {
        !origin
            .strip_prefix("http://")
            .is_some_and(|authority| own(authority, port))
    });
    if foreign {
        return Response::text(
            "403 Forbidden",
            "a run's socket is opened by the runner it serves alone",
        )
        .into();
    }
    if header(head, "sec-websocket-version") != Some(VERSION) {
        let mut response = Response::text(
            "426 Upgrade Required",
            &format!("a run's socket speaks WebSocket version {VERSION}"),
        );
        response
            .headers
            .push(("Sec-WebSocket-Version", VERSION.to_string()));

        return response.into();
    }
    let Some(key) = header(head, "sec-websocket-key") else {
        return Response::text("400 Bad Request", "a socket's handshake states its key").into();
    };

    match run.held() {
        Some(held) => Answer::Socket {
            accept: derive_accept_key(key.as_bytes()),
            held,
        },
        None => Response::text("409 Conflict", "the run is driven by another page").into(),
    }
}

/// The response refusing a method, naming the ones `allow` takes.
fn refused(allow: &str, why: &str) -> Response {
    let mut response = Response::text("405 Method Not Allowed", why);
    response.headers.push(("Allow", allow.to_string()));

    response
}

/// The file of `served` at `path`: `index.html`, the page's own name for its document, for `/`, and otherwise the file at the page path the rest of `path` spells.
fn file(served: &Served, path: &str) -> Response {
    let file = match path {
        "/" => INDEX_HTML,
        path => match path.strip_prefix('/') {
            Some(within) if !within.is_empty() => within,
            _ => return Response::text("404 Not Found", "the page holds no file there"),
        },
    };

    match served.files.get(file) {
        Some(bytes) => Response::file(file, bytes),
        None => Response::text("404 Not Found", "the page holds no file there"),
    }
}

/// `path` percent-decoded, where what it decodes to is UTF-8: the page path a URL the page wrote names.
fn decoded(path: &str) -> Option<String> {
    let bytes = path.as_bytes();
    let mut decoded = Vec::with_capacity(bytes.len());
    let mut i = 0;

    while i < bytes.len() {
        if bytes[i] == b'%' {
            let pair = bytes.get(i + 1..i + 3)?;
            // Two hex digits exactly: `from_str_radix` would take a sign in their place.
            if !pair.iter().all(u8::is_ascii_hexdigit) {
                return None;
            }
            decoded.push(u8::from_str_radix(std::str::from_utf8(pair).ok()?, 16).ok()?);
            i += 3;
        } else {
            decoded.push(bytes[i]);
            i += 1;
        }
    }

    String::from_utf8(decoded).ok()
}
