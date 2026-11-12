//! `run --target browser` and `test --target browser` end to end: the compiler started on a port of loopback's own, the port read off its `Serving` line, and its page or its run asked for over loopback as a browser asks — the runner's part played here with canned outcomes over the run's socket, since no engine runs a page in this suite, which the page suite under Node does.

use {
    curios_browser::{Reply, Step},
    curios_common::{digest, test_support::*},
    std::{
        fs,
        io::{BufRead, BufReader, Read, Write},
        net::{Ipv4Addr, TcpStream},
        path::Path,
        process::{Child, Command, Stdio},
        sync::mpsc,
        thread::{self, JoinHandle},
    },
    tungstenite::{Message, WebSocket, stream::MaybeTlsStream},
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

    /// The run's socket, opened as a runner opens it.
    fn socket(&self) -> WebSocket<MaybeTlsStream<TcpStream>> {
        tungstenite::connect(format!("ws://127.0.0.1:{}/", self.port))
            .expect("the run's socket opens")
            .0
    }

    /// Stop a command that serves until stopped, and take what it wrote.
    fn stopped(mut self) -> (Vec<u8>, Vec<u8>) {
        self.child.kill().unwrap();
        self.child.wait().unwrap();

        (self.stdout.join().unwrap(), self.stderr.join().unwrap())
    }

    /// Wait for a command that serves until its run is complete, and take its exit code and what it wrote.
    fn ended(mut self) -> (Option<i32>, Vec<u8>, Vec<u8>) {
        let status = self.child.wait().unwrap();

        (
            status.code(),
            self.stdout.join().unwrap(),
            self.stderr.join().unwrap(),
        )
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
        stderr.contains(&format!("╰─ serve; http://127.0.0.1:{port}/\n")),
        "{stderr}"
    );
}

/// A package `app` with a library of three outcomes, an executable `app` with a test of its own, and an executable `quiet` with none.
fn tested(root: &Path) {
    fs::create_dir_all(root).unwrap();
    fs::write(
        root.join("curios.toml"),
        "compiler = \"0.0.0\"\nname = \"app\"\n\n[[executables]]\nname = \"app\"\n\n[[executables]]\nname = \"quiet\"\n",
    )
    .unwrap();
    fs::write(
        root.join("lib.crs"),
        "use /std/{Io, Test};\n\ntest holds =\n    Test/assert(true);\n\ntest fails =\n    Test/equal(1, 2);\n\ntest exits =\n    Test/perform(() => let _ = /std/Proc/exit(@{}, 7)!; Io/pure(Test/assert(true)));\n",
    )
    .unwrap();
    fs::write(
        root.join("app.crs"),
        "use /std/{Test};\n\ntest own =\n    Test/assert(true);\n\n/std/print(\"ran\\n\")\n",
    )
    .unwrap();
    fs::write(root.join("quiet.crs"), "/std/print(\"quiet\\n\")\n").unwrap();
}

/// An outcome as a runner sends one over the run's socket: the streams in base64 — `stdout` given encoded — and the rest as the harness resolves it.
fn posted(stdout: &str, exit_code: &str, trap: &str, held: &str) -> Message {
    Message::text(format!(
        "{{\"stdout\":\"{stdout}\",\"stderr\":\"\",\"exitCode\":{exit_code},\"trap\":{trap},\"held\":{held}}}"
    ))
}

/// The next reply `socket` is sent: what the command wrote, and the next step.
fn reply(socket: &mut WebSocket<MaybeTlsStream<TcpStream>>) -> (String, Option<Step>) {
    let Message::Text(text) = socket.read().expect("a reply") else {
        panic!("a reply is a text message");
    };
    let reply = Reply::parse(text.as_bytes()).expect("a reply");

    (
        String::from_utf8(reply.decoded().expect("base64")).unwrap(),
        reply.next,
    )
}

/// The step `subject`/`test`.
fn step(subject: usize, test: usize) -> Option<Step> {
    Some(Step { subject, test })
}

/// Each needle's first occurrence sits after the previous one's.
fn in_order(haystack: &str, needles: &[&str]) {
    let mut from = 0;
    for needle in needles {
        match haystack[from..].find(needle) {
            Some(at) => from += at + needle.len(),
            None => panic!("missing {needle:?} (after byte {from}) in:\n{haystack}"),
        }
    }
}

/// `test --target browser` builds every subject, serves the runner at the root and the first subject's page beside it, drives the run over the socket the runner opens there — sending each step in plan order and taking each outcome as its step's — switches the page with a subject's last outcome, prints what native prints and sends the runner the same report, and exits with the tri-state once every step is taken.
#[test]
fn test_serves_the_run_at_the_root_and_ends_with_the_tally_once_every_step_is_taken() {
    let root = Temporary::new("cli-serve", "test");
    tested(&root);
    let serving = Serving::start(&root, &["test", "--target", "browser"]);

    let (status, headers, runner) = serving.get("/");
    assert_eq!(status, "HTTP/1.1 200 OK");
    assert!(
        headers.contains(&"Content-Type: text/html; charset=utf-8".to_string()),
        "{headers:?}"
    );
    let (status, _, page) = serving.get("/index.html");
    assert_eq!(status, "HTTP/1.1 200 OK");
    assert!(page.starts_with(b"<!doctype html>") && page != runner);
    let (status, _, library) = serving.get("/index.wasm");
    assert_eq!(status, "HTTP/1.1 200 OK");

    // The base64 of `/app/holds: passed\n`, `/app/fails: failed\n  expected 2 but got 1\n` and nothing.
    let held = posted("L2FwcC9ob2xkczogcGFzc2VkCg==", "0", "null", "true");
    let failed = posted(
        "L2FwcC9mYWlsczogZmFpbGVkCiAgZXhwZWN0ZWQgMiBidXQgZ290IDEK",
        "0",
        "null",
        "false",
    );
    let exited = posted("", "7", "null", "null");
    let own = posted("L293bjogcGFzc2VkCg==", "0", "null", "true");

    let mut socket = serving.socket();
    let (mut shown, first) = reply(&mut socket);
    assert_eq!(first, step(0, 0));
    for (outcome, next) in [
        (held, step(0, 1)),
        (failed, step(0, 2)),
        (exited, step(1, 0)),
    ] {
        socket.send(outcome).unwrap();
        let (written, after) = reply(&mut socket);
        shown.push_str(&written);
        assert_eq!(after, next);
    }
    // The library's last outcome switched the page in hand to the executable's.
    let (status, _, executable) = serving.get("/index.wasm");
    assert_eq!(status, "HTTP/1.1 200 OK");
    assert_ne!(executable, library);
    socket.send(own).unwrap();
    let (written, after) = reply(&mut socket);
    shown.push_str(&written);
    assert_eq!(after, None);
    // The runner is sent the report whole, from the first subject's opening, which is written before any socket opens.
    assert_eq!(
        shown,
        concat!(
            "↳ Testing      /app\n",
            "/app/holds: passed\n",
            "/app/fails: failed\n  expected 2 but got 1\n    Test/equal(1, 2)\n",
            "/app/exits: exited 7\n    Test/perform(() => let _ = /std/Proc/exit(@{}, 7)!; Io/pure(Test/assert(true)))\n",
            "↳ Tested       /app; 1 passed, 1 failed, 1 exited\n",
            "↳ Testing      /\n",
            "/own: passed\n",
            "↳ Tested       /; 1 passed, 0 failed\n",
            "Finished       2 passed, 1 failed, 1 exited\n",
        )
    );

    let (code, stdout, stderr) = serving.ended();
    assert_eq!(code, Some(1));
    assert_eq!(
        String::from_utf8_lossy(&stdout),
        "/app/holds: passed\n/app/fails: failed\n  expected 2 but got 1\n    Test/equal(1, 2)\n/app/exits: exited 7\n    Test/perform(() => let _ = /std/Proc/exit(@{}, 7)!; Io/pure(Test/assert(true)))\n/own: passed\n"
    );
    let stderr = String::from_utf8_lossy(&stderr);
    in_order(
        &stderr,
        &[
            "/app\n",
            "├─ emit; ",
            "app\n",
            "├─ emit; ",
            "quiet\n",
            "├─ check; ",
            "↳ Serving      http://127.0.0.1:",
            "↳ Testing      /app\n",
            "↳ Tested       /app; 1 passed, 1 failed, 1 exited\n",
            "↳ Testing      /\n",
            "↳ Tested       /; 1 passed, 0 failed\n",
            "Finished       2 passed, 1 failed, 1 exited\n",
        ],
    );
    assert_eq!(stderr.matches("↳ Testing").count(), 2, "{stderr}");
}

/// A run every test of which passes exits 0, and a subject with nothing selected is built and never served, its modules reported skipped as it is built — on the terminal, and in the report the runner is sent as its socket opens.
#[test]
fn test_exits_zero_once_every_selected_test_has_passed() {
    let root = Temporary::new("cli-serve", "test-green");
    tested(&root);
    let serving = Serving::start(&root, &["test", "--target", "browser", "--filter", "/own"]);

    let mut socket = serving.socket();
    assert_eq!(
        reply(&mut socket),
        (
            concat!(
                "↳ Testing      /app\n",
                "↳ Tested       /app; 0 passed, 0 failed, 3 skipped\n",
                "↳ Testing      /\n",
            )
            .to_string(),
            step(0, 0)
        )
    );
    socket
        .send(posted("L293bjogcGFzc2VkCg==", "0", "null", "true"))
        .unwrap();
    assert_eq!(
        reply(&mut socket),
        (
            concat!(
                "/own: passed\n",
                "↳ Tested       /; 1 passed, 0 failed\n",
                "Finished       1 passed, 0 failed, 3 skipped\n",
            )
            .to_string(),
            None
        )
    );

    let (code, stdout, stderr) = serving.ended();
    assert_eq!(code, Some(0), "{}", String::from_utf8_lossy(&stderr));
    assert_eq!(String::from_utf8_lossy(&stdout), "/own: passed\n");
    in_order(
        &String::from_utf8_lossy(&stderr),
        &[
            "↳ Testing      /app\n",
            "↳ Tested       /app; 0 passed, 0 failed, 3 skipped\n",
            "↳ Serving      http://127.0.0.1:",
            "↳ Testing      /\n",
            "↳ Tested       /; 1 passed, 0 failed\n",
            "Finished       1 passed, 0 failed, 3 skipped\n",
        ],
    );
}

/// `--port` beside the native target is refused, as `run` refuses it.
#[test]
fn a_port_beside_the_native_target_is_refused() {
    let root = Temporary::new("cli-serve", "port-native");
    tested(&root);

    let output = Command::new(env!("CARGO_BIN_EXE_curios"))
        .current_dir(&root)
        .args(["test", "--port", "1"])
        .output()
        .unwrap();

    assert_eq!(output.status.code(), Some(1));
    assert!(
        String::from_utf8_lossy(&output.stderr).contains(
            "`--port` is where a page is served, and `--target native` runs the tests here: `--target browser` serves their pages"
        ),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}
