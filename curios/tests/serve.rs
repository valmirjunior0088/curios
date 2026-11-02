//! `run --target browser` end to end: the compiler started on a port of loopback's own, the port read off its `Serving` line, and its page asked for over loopback as a browser asks for it — with no engine running the page here, which the page suite under Node does.

use {
    curios_utilities::{digest, test_support::Temporary},
    std::{
        fs,
        io::{BufRead, BufReader, Read, Write},
        net::{Ipv4Addr, TcpStream},
        path::Path,
        process::{Child, Command, Stdio},
        sync::mpsc,
        thread::{self, JoinHandle},
    },
};

const GATEWAY: &[u8] = b"export const double = (n) => n * 2n;\n";

/// A package `clock` whose program prints what its one row's gateway answers, so its page holds the gateway beside the three files of its own.
fn package(root: &Path) {
    fs::create_dir_all(root.join("web")).unwrap();
    fs::write(root.join("web/double.js"), GATEWAY).unwrap();
    fs::write(
        root.join("curios.toml"),
        format!(
            "compiler = \"0.0.0\"\nname = \"clock\"\n\n[[foreign]]\nname = \"page\"\n\n[foreign.browser]\npath = \"web/double.js\"\nhash = \"f1:{}\"\n\n[foreign.answers]\ndouble = \"double\"\n",
            digest(GATEWAY)
        ),
    )
    .unwrap();
    fs::write(root.join("lib.crs"), "pub foreign double(Nat) -> Nat;\n").unwrap();
    fs::write(
        root.join("exe.crs"),
        "let main() -> /std/Io({}) = let doubled = /clock/double(3)!;\n/std/print(/std/Nat/to_str(doubled));\nmain()\n",
    )
    .unwrap();
}

/// The port a `Serving` line names, where `line` is one.
fn port_of(line: &str) -> Option<u16> {
    let (_, after) = line.split_once("http://127.0.0.1:")?;

    after.split('/').next()?.parse().ok()
}

/// The compiler started in `root` on `arguments` and `--port 0`, serving on a port of loopback's own: the port read off its `Serving` line, and both of its streams drained to their ends on threads of their own, so a line it writes while serving never blocks it.
struct Serving {
    child: Child,
    port: u16,
    stdout: JoinHandle<Vec<u8>>,
    stderr: JoinHandle<Vec<u8>>,
}

impl Serving {
    fn start(root: &Path, arguments: &[&str]) -> Self {
        let mut child = Command::new(env!("CARGO_BIN_EXE_curios"))
            .current_dir(root)
            .args(arguments)
            .args(["--port", "0"])
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap();
        let out = child.stdout.take().unwrap();
        let err = child.stderr.take().unwrap();
        let stdout = thread::spawn(move || {
            let mut bytes = Vec::new();
            BufReader::new(out).read_to_end(&mut bytes).unwrap();

            bytes
        });
        let (announced, port) = mpsc::channel();
        let stderr = thread::spawn(move || {
            let mut reader = BufReader::new(err);
            let mut bytes = Vec::new();
            let mut line = String::new();

            while reader.read_line(&mut line).unwrap() > 0 {
                bytes.extend_from_slice(line.as_bytes());
                if let Some(port) = port_of(&line) {
                    let _ = announced.send(port);
                }
                line.clear();
            }

            bytes
        });
        let port = port.recv().expect("the command prints where it serves");

        Self {
            child,
            port,
            stdout,
            stderr,
        }
    }

    /// What the server answers `request`, written as a client writes it, as its status line, its headers and its body.
    fn asked(&self, request: &[u8]) -> (String, Vec<String>, Vec<u8>) {
        let mut stream = TcpStream::connect((Ipv4Addr::LOCALHOST, self.port)).unwrap();
        stream.write_all(request).unwrap();

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

    fn get(&self, path: &str) -> (String, Vec<String>, Vec<u8>) {
        self.asked(
            format!(
                "GET {path} HTTP/1.1\r\nHost: 127.0.0.1:{}\r\n\r\n",
                self.port
            )
            .as_bytes(),
        )
    }

    /// `POST path` with `body`, under `content_type` where one is given.
    fn post(
        &self,
        path: &str,
        content_type: Option<&str>,
        body: &[u8],
    ) -> (String, Vec<String>, Vec<u8>) {
        let typed = content_type.map_or(String::new(), |content_type| {
            format!("Content-Type: {content_type}\r\n")
        });
        let mut request = format!(
            "POST {path} HTTP/1.1\r\nHost: 127.0.0.1:{}\r\n{typed}Content-Length: {}\r\n\r\n",
            self.port,
            body.len()
        )
        .into_bytes();
        request.extend_from_slice(body);

        self.asked(&request)
    }

    /// Stop a command that serves until stopped, and take what it wrote.
    fn stopped(mut self) -> (Vec<u8>, Vec<u8>) {
        self.child.kill().unwrap();
        self.child.wait().unwrap();

        (self.stdout.join().unwrap(), self.stderr.join().unwrap())
    }
}

/// `run --target browser` serves the page at the root — `/` and `/index.html` one resource, its program and its gateway at their page paths — answers `GET` alone, and serves until it is stopped.
#[test]
fn run_serves_the_page_at_the_root_until_stopped() {
    let root = Temporary::new("cli-serve", "run");
    package(&root);
    let serving = Serving::start(&root, &["run", "--target", "browser"]);
    let port = serving.port;

    let (status, headers, index) = serving.get("/");
    assert_eq!(status, "HTTP/1.1 200 OK");
    assert!(
        headers.contains(&"Content-Type: text/html; charset=utf-8".to_string()),
        "{headers:?}"
    );
    assert!(index.starts_with(b"<!doctype html>"));
    assert_eq!(serving.get("/index.html").2, index);
    let (status, headers, program) = serving.get("/index.wasm");
    assert_eq!(status, "HTTP/1.1 200 OK");
    assert!(
        headers.contains(&"Content-Type: application/wasm".to_string()),
        "{headers:?}"
    );
    assert!(program.starts_with(b"\0asm"));
    let (status, _, gateway) = serving.get("/web/double.js");
    assert_eq!(status, "HTTP/1.1 200 OK");
    assert_eq!(gateway, GATEWAY);
    assert_eq!(serving.get("/clock/").0, "HTTP/1.1 404 Not Found");
    let (status, headers, _) = serving.post("/", Some("application/json"), b"{}");
    assert_eq!(status, "HTTP/1.1 405 Method Not Allowed");
    assert!(headers.contains(&"Allow: GET".to_string()), "{headers:?}");

    let (stdout, stderr) = serving.stopped();
    let stderr = String::from_utf8_lossy(&stderr);
    assert!(stdout.is_empty(), "{stdout:?}");
    assert!(
        stderr.contains(&format!("↳ Serving      http://127.0.0.1:{port}/\n")),
        "{stderr}"
    );
}
