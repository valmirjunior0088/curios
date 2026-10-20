//! Serving a page to a browser on this machine: the files a browser build is, held in memory and answered over loopback until the process is stopped.
//!
//! **Only what a page needs.** A request is a `GET` of a path under `/<name>/`, answered with the file at that page path, or `index.html` for the directory itself; `/` and `/<name>` are redirected there, so the page's relative paths resolve inside it. Every reply says `Cache-Control: no-store`, since a page changes by running the command again, and `Connection: close`, so no connection outlives its one request.
//!
//! **Loopback, and only by its own name.** The server binds `127.0.0.1` and refuses a request whose `Host` is not that address or `localhost` at its port, which keeps a page some other site loads from reaching it through a name that resolves here.

#[cfg(test)]
mod tests;

use {
    curios_browser::{INDEX_HTML, content_type, encoded},
    std::{
        collections::BTreeMap,
        ffi::OsString,
        io::{self, BufRead, BufReader, Write},
        net::{Ipv4Addr, TcpListener, TcpStream},
        sync::Arc,
        thread,
    },
};

/// The port a page is served on where `--port` names none.
pub(crate) const PORT: u16 = 8000;

/// The most a request's head may hold before it is refused: far past any a browser sends for a page.
const HEAD: usize = 16 * 1024;

/// A page held to serve: the name it is served under, and every file of it by its page path.
pub(crate) struct Served {
    pub(crate) name: String,
    pub(crate) files: BTreeMap<String, Vec<u8>>,
}

impl Served {
    /// Where a browser opens the page served on `port`, its program handed `args`: each an `arg=` of the query, every byte but the unreserved ones percent-encoded, as the loader decodes them.
    pub(crate) fn url(&self, port: u16, args: &[OsString]) -> String {
        let query = args
            .iter()
            .map(|arg| {
                let mut pair = String::from("arg=");
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
            true => format!("http://127.0.0.1:{port}/{}/", encoded(&self.name)),
            false => format!("http://127.0.0.1:{port}/{}/?{query}", encoded(&self.name)),
        }
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
    let port = listener
        .local_addr()
        .map_err(|error| format!("cannot read the address served on: {error}"))?
        .port();
    let served = Arc::new(served);

    // A connection that fails before it is accepted is its client's, and ends nothing else.
    for stream in listener.incoming().flatten() {
        let served = Arc::clone(&served);

        thread::spawn(move || {
            // A client that goes away mid-reply is no failure of the server's.
            let _ = answer(stream, &served, port);
        });
    }

    Ok(())
}

/// One reply: its status line, the headers past the ones every reply carries, and its body.
struct Reply {
    status: &'static str,
    headers: Vec<(&'static str, String)>,
    body: Vec<u8>,
}

impl Reply {
    fn text(status: &'static str, body: &str) -> Self {
        Self {
            status,
            headers: vec![("Content-Type", "text/plain; charset=utf-8".to_string())],
            body: format!("{body}\n").into_bytes(),
        }
    }

    fn redirect(to: String) -> Self {
        Self {
            status: "308 Permanent Redirect",
            headers: vec![("Location", to)],
            body: Vec::new(),
        }
    }
}

/// Read one request from `stream` and write its reply.
fn answer(stream: TcpStream, served: &Served, port: u16) -> io::Result<()> {
    let reply = match head(&mut BufReader::new(&stream))? {
        Some(head) => replied(&head, served, port),
        None => Reply::text(
            "431 Request Header Fields Too Large",
            "the request's head is too large",
        ),
    };

    let mut stream = &stream;
    write!(stream, "HTTP/1.1 {}\r\n", reply.status)?;
    for (name, value) in &reply.headers {
        write!(stream, "{name}: {value}\r\n")?;
    }
    write!(
        stream,
        "Content-Length: {}\r\nCache-Control: no-store\r\nConnection: close\r\n\r\n",
        reply.body.len()
    )?;
    stream.write_all(&reply.body)?;
    stream.flush()
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

/// The reply to the request whose head is `head`, served on `port`.
fn replied(head: &[String], served: &Served, port: u16) -> Reply {
    let mut words = head.first().map_or("", String::as_str).split(' ');
    let (method, target) = (words.next().unwrap_or(""), words.next().unwrap_or(""));

    let host = head.iter().skip(1).find_map(|line| {
        let (name, value) = line.split_once(':')?;

        name.eq_ignore_ascii_case("host").then(|| value.trim())
    });
    if !host.is_some_and(|host| {
        host == format!("127.0.0.1:{port}") || host == format!("localhost:{port}")
    }) {
        return Reply::text(
            "421 Misdirected Request",
            "this server answers 127.0.0.1 and localhost at its own port alone",
        );
    }

    if method != "GET" {
        let mut reply = Reply::text("405 Method Not Allowed", "a page is read with GET alone");
        reply.headers.push(("Allow", "GET".to_string()));

        return reply;
    }

    let Some(path) = decoded(target.split(['?', '#']).next().unwrap_or("")) else {
        return Reply::text("400 Bad Request", "the path is not percent-encoded UTF-8");
    };
    let page = format!("/{}/", encoded(&served.name));

    if path == "/" || path == format!("/{}", served.name) {
        return Reply::redirect(page);
    }

    let file = match path.strip_prefix(&format!("/{}/", served.name)) {
        Some("") => INDEX_HTML,
        Some(within) => within,
        None => return Reply::text("404 Not Found", "no page is served there"),
    };

    match served.files.get(file) {
        Some(bytes) => Reply {
            status: "200 OK",
            headers: vec![("Content-Type", content_type(file).to_string())],
            body: bytes.clone(),
        },
        None => Reply::text("404 Not Found", "the page holds no file there"),
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
