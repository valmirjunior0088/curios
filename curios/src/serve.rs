//! Serving a browser on this machine: the files a browser build is, or a run of tests, held in memory and answered over loopback — a page until the process is stopped, a run until every test has reported.
//!
//! **Only what a page needs.** A request is a `GET` of a path, answered with the file at that page path, or with `index.html`, the page's own name for its document, for `/`; the page stands at the root, so its relative paths resolve there. Every reply says `Cache-Control: no-store`, since a page changes by running the command again, and `Connection: close`, so no connection outlives its one request.
//!
//! **A run is the runner at the root and one subject's page beside it.** `/` answers the runner, `index.html` and every other path the page of the subject in hand, and `POST /` takes a report of the step in hand — the first unreported test in plan order — under the run's token, answering what the command wrote for it and the next step; a report of any other step is answered with nothing and ignored. The page switches to the next subject with its last report, and the run is complete when every step is taken.
//!
//! **Loopback, and only by its own name.** The server binds `127.0.0.1` and refuses a request whose `Host` is not that address or `localhost` at its port, which keeps a page some other site loads from reaching it through a name that resolves here.

#[cfg(test)]
mod tests;

use {
    crate::Streams,
    curios_browser::{ARGUMENT, INDEX_HTML, Posted, Reply, Report, Step, content_type, runner},
    std::{
        collections::BTreeMap,
        ffi::OsString,
        io::{self, BufRead, BufReader, Read, Write},
        net::{Ipv4Addr, TcpListener, TcpStream},
        sync::{Mutex, PoisonError},
        thread,
    },
};

/// The port a page is served on where `--port` names none.
pub(crate) const PORT: u16 = 8000;

/// The most a request's head may hold before it is refused: far past any a browser sends for a page.
const HEAD: usize = 16 * 1024;

/// The most a report's body may hold before it is refused: past any output a test writes.
const BODY: usize = 16 * 1024 * 1024;

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

/// A run of tests served: the runner document, each subject's page in plan order with how many tests it schedules, the run's token, and where the run is.
pub(crate) struct Run<'a> {
    runner: Vec<u8>,
    pages: Vec<(Served, usize)>,
    token: String,
    state: Mutex<State<'a>>,
}

/// What the command does with the step in hand's report — prints and tallies — answering what it wrote on each stream.
type Reporting<'a> = Box<dyn FnMut(Step, Streams, &Posted) -> Streams + Send + 'a>;

/// Where a run is, and what takes its reports.
struct State<'a> {
    /// The page in hand, past the last once the run is complete, and the test in hand on it.
    page: usize,
    test: usize,
    report: Reporting<'a>,
}

impl<'a> Run<'a> {
    /// A run over `pages`, each with how many tests it schedules, under `token`, `report` taking each step's report.
    pub(crate) fn new(
        token: String,
        pages: Vec<(Served, usize)>,
        report: impl FnMut(Step, Streams, &Posted) -> Streams + Send + 'a,
    ) -> Self {
        let first = pages.first().map(|_| Step {
            subject: 0,
            test: 0,
        });

        Self {
            runner: runner(&token, first),
            pages,
            token,
            state: Mutex::new(State {
                page: 0,
                test: 0,
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

/// Answer every connection `listener` accepts with `run`, each on a thread of its own, until every step of the run is taken and its last reply written.
pub(crate) fn serve_run(listener: &TcpListener, run: Run<'_>) -> Result<(), String> {
    serve(listener, &Site::Run(run))
}

/// Answer every connection `listener` accepts with `site`, each on a thread of its own, until the site is complete: the connection that takes a run's last step connects back once its reply is written, so the accept ends.
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

/// One response: its status line, the headers past the ones every response carries, its body, and whether it completes a run, which wakes the accept once it is written.
struct Response {
    status: &'static str,
    headers: Vec<(&'static str, String)>,
    body: Vec<u8>,
    last: bool,
}

impl Response {
    fn text(status: &'static str, body: &str) -> Self {
        Self {
            status,
            headers: vec![("Content-Type", "text/plain; charset=utf-8".to_string())],
            body: format!("{body}\n").into_bytes(),
            last: false,
        }
    }

    fn file(path: &str, bytes: &[u8]) -> Self {
        Self {
            status: "200 OK",
            headers: vec![("Content-Type", content_type(path).to_string())],
            body: bytes.to_vec(),
            last: false,
        }
    }

    fn json(reply: &Reply, last: bool) -> Self {
        Self {
            status: "200 OK",
            headers: vec![("Content-Type", "application/json".to_string())],
            body: reply.render(),
            last,
        }
    }
}

/// Read one request from `stream` and write its response.
fn answer(stream: TcpStream, site: &Site<'_>, port: u16) -> io::Result<()> {
    let mut reader = BufReader::new(&stream);
    let response = match head(&mut reader)? {
        Some(head) => match body(&head, &mut reader)? {
            Ok(body) => replied(&head, &body, site, port),
            Err(refused) => refused,
        },
        None => Response::text(
            "431 Request Header Fields Too Large",
            "the request's head is too large",
        ),
    };

    let mut stream = &stream;
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
    stream.flush()?;

    if response.last {
        // The accept is blocked until a connection arrives: this one is it, and the loop ends on it without answering it.
        let _ = TcpStream::connect((Ipv4Addr::LOCALHOST, port));
    }

    Ok(())
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

/// The request's body, where its method carries one: a `POST`'s, read under its `Content-Length` up to [`BODY`], or the response refusing it — none stated, or more than the bound.
fn body(head: &[String], reader: &mut impl Read) -> io::Result<Result<Vec<u8>, Response>> {
    if !head.first().is_some_and(|line| line.starts_with("POST ")) {
        return Ok(Ok(Vec::new()));
    }

    let Some(length) =
        header(head, "content-length").and_then(|length| length.parse::<usize>().ok())
    else {
        return Ok(Err(Response::text(
            "411 Length Required",
            "a report states its Content-Length",
        )));
    };
    if length > BODY {
        return Ok(Err(Response::text(
            "413 Content Too Large",
            &format!("a report is at most {BODY} bytes"),
        )));
    }

    let mut body = vec![0; length];
    reader.read_exact(&mut body)?;

    Ok(Ok(body))
}

/// The response to the request whose head is `head` and body `body`, served on `port`.
fn replied(head: &[String], body: &[u8], site: &Site<'_>, port: u16) -> Response {
    let mut words = head.first().map_or("", String::as_str).split(' ');
    let (method, target) = (words.next().unwrap_or(""), words.next().unwrap_or(""));

    if !header(head, "host").is_some_and(|host| {
        host == format!("127.0.0.1:{port}") || host == format!("localhost:{port}")
    }) {
        return Response::text(
            "421 Misdirected Request",
            "this server answers 127.0.0.1 and localhost at its own port alone",
        );
    }

    let Some(path) = decoded(target.split(['?', '#']).next().unwrap_or("")) else {
        return Response::text("400 Bad Request", "the path is not percent-encoded UTF-8");
    };

    match (method, site) {
        ("GET", Site::Page(served)) => file(served, &path),
        // The root is the run; the page in hand stands beside it, by its own name for its document and its files' page paths.
        ("GET", Site::Run(run)) if path == "/" => Response::file(INDEX_HTML, &run.runner),
        ("GET", Site::Run(run)) => match run.current() {
            Some(page) => file(page, &path),
            None => Response::text("404 Not Found", "the run is complete"),
        },
        ("POST", Site::Run(run)) if path == "/" => match header(head, "content-type") {
            Some(content_type) if content_type.starts_with("application/json") => {
                reported(run, body)
            }
            _ => Response::text(
                "415 Unsupported Media Type",
                "a report is JSON, under `Content-Type: application/json`",
            ),
        },
        ("POST", Site::Run(_)) => Response::text("404 Not Found", "a report is posted to / alone"),
        (_, Site::Page(_)) => refused("GET", "a page is read with GET alone"),
        (_, Site::Run(_)) => refused(
            "GET, POST",
            "a run is read with GET and reported to with POST",
        ),
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

/// The response to a report `body` spells of `run`: taken where it is the step in hand's, under the run's token, with what the command wrote for it and the next step; ignored otherwise.
fn reported(run: &Run<'_>, body: &[u8]) -> Response {
    let report = match Report::parse(body) {
        Ok(report) => report,
        Err(why) => return Response::text("400 Bad Request", &why),
    };
    if report.run != run.token {
        return Response::text("403 Forbidden", "the report names no run served here");
    }
    let (stdout, stderr) = match report.outcome.streams() {
        Ok(streams) => streams,
        Err(why) => return Response::text("400 Bad Request", &why),
    };

    let mut state = run.state();
    if step_of(&state, run.pages.len()) != Some(report.step()) {
        return Response::json(&Reply::ignored(), false);
    }

    let written = (state.report)(report.step(), Streams { stdout, stderr }, &report.outcome);
    state.test += 1;
    if state.test == run.pages[state.page].1 {
        state.page += 1;
        state.test = 0;
    }
    let next = step_of(&state, run.pages.len());

    Response::json(
        &Reply::taken(&written.stdout, &written.stderr, next),
        next.is_none(),
    )
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
