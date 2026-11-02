//! `run --target browser` and `test --target browser` end to end: the compiler started on a port of loopback's own, the port read off its `Serving` line, and its page or its run asked for over loopback as a browser asks — the runner's part played here with canned reports, since no engine runs a page in this suite, which the page suite under Node does.

use {
    curios_utilities::{digest, test_support::*},
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
        stderr.contains(&format!("↳ Serving      http://127.0.0.1:{port}/\n")),
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

/// The token the runner document `runner` carries.
fn token_of(runner: &[u8]) -> String {
    let document = String::from_utf8_lossy(runner);
    let (_, after) = document
        .split_once("\"token\":\"")
        .expect("the runner carries the run's token");

    after.split('"').next().unwrap().to_string()
}

/// A report of `step`'s `outcome`, as the runner posts one, under `token`.
fn report(token: &str, step: (usize, usize), outcome: &str) -> String {
    format!(
        "{{\"run\":\"{token}\",\"subject\":{},\"test\":{},\"outcome\":{outcome}}}",
        step.0, step.1
    )
}

/// An outcome as a page posts one: the streams in base64 — `stdout` given encoded — and the rest as the harness resolves it.
fn posted(stdout: &str, exit_code: &str, trap: &str, held: &str) -> String {
    format!(
        "{{\"stdout\":\"{stdout}\",\"stderr\":\"\",\"exitCode\":{exit_code},\"trap\":{trap},\"held\":{held}}}"
    )
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

/// `test --target browser` builds every subject, serves the runner at the root and the first subject's page beside it, takes each step's report in plan order — refusing one without the token or the type and ignoring one of a step not in hand — switches the page with a subject's last report, prints what native prints, and exits with the tri-state once every step is taken.
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
    let token = token_of(&runner);
    assert!(
        String::from_utf8_lossy(&runner).contains("\"next\":{\"subject\":0,\"test\":0}"),
        "{}",
        String::from_utf8_lossy(&runner)
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

    let json = Some("application/json");
    assert_eq!(
        serving
            .post("/", json, report("nope", (0, 0), &held).as_bytes())
            .0,
        "HTTP/1.1 403 Forbidden"
    );
    assert_eq!(
        serving
            .post(
                "/",
                Some("text/plain"),
                report(&token, (0, 0), &held).as_bytes()
            )
            .0,
        "HTTP/1.1 415 Unsupported Media Type"
    );
    assert_eq!(serving.post("/", json, b"{").0, "HTTP/1.1 400 Bad Request");
    let (status, _, body) = serving.post("/", json, report(&token, (0, 1), &failed).as_bytes());
    assert_eq!(status, "HTTP/1.1 200 OK");
    assert_eq!(
        body,
        b"{\"taken\":false,\"stdout\":\"\",\"stderr\":\"\",\"next\":null}"
    );

    for (step, outcome, next) in [
        ((0, 0), &held, "{\"subject\":0,\"test\":1}"),
        ((0, 1), &failed, "{\"subject\":0,\"test\":2}"),
        ((0, 2), &exited, "{\"subject\":1,\"test\":0}"),
    ] {
        let (status, _, body) = serving.post("/", json, report(&token, step, outcome).as_bytes());
        let body = String::from_utf8(body).unwrap();
        assert_eq!(status, "HTTP/1.1 200 OK", "{step:?}");
        assert!(
            body.starts_with("{\"taken\":true,") && body.ends_with(&format!(",\"next\":{next}}}")),
            "{step:?}: {body}"
        );
    }
    // A duplicate is ignored, and the page in hand is now the executable's.
    let (_, _, body) = serving.post("/", json, report(&token, (0, 2), &exited).as_bytes());
    assert!(String::from_utf8_lossy(&body).starts_with("{\"taken\":false,"));
    let (status, _, executable) = serving.get("/index.wasm");
    assert_eq!(status, "HTTP/1.1 200 OK");
    assert_ne!(executable, library);

    let (status, _, body) = serving.post("/", json, report(&token, (1, 0), &own).as_bytes());
    assert_eq!(status, "HTTP/1.1 200 OK");
    assert!(
        String::from_utf8_lossy(&body).ends_with(",\"next\":null}"),
        "{}",
        String::from_utf8_lossy(&body)
    );

    let (code, stdout, stderr) = serving.ended();
    assert_eq!(code, Some(1));
    assert_eq!(
        String::from_utf8_lossy(&stdout),
        "/app/holds: passed\n/app/fails: failed\n  expected 2 but got 1\n    Test/equal(1, 2)\n/app/exits: exited 7\n    Test/perform(() => let _ = /std/Proc/exit(@{}, 7)!; Io/pure(Test/assert(true)))\n/own: passed\n2 passed, 1 failed, 1 exited\n"
    );
    let stderr = String::from_utf8_lossy(&stderr);
    in_order(
        &stderr,
        &[
            "Processing     /app\n",
            "↳ Linking      /app; ",
            "Processing     app\n",
            "↳ Linking      app; ",
            "Processing     quiet\n",
            "↳ Linking      quiet; ",
            "↳ Serving      http://127.0.0.1:",
            "↳ Testing      /app\n",
            "↳ Tested       /app; 1 passed, 1 failed, 1 exited\n",
            "↳ Testing      app\n",
            "↳ Tested       app; 1 passed, 0 failed\n",
        ],
    );
    assert!(!stderr.contains("Testing      quiet"), "{stderr}");
}

/// A run every test of which passes exits 0, and a subject with nothing selected is built and never served.
#[test]
fn test_exits_zero_once_every_selected_test_has_passed() {
    let root = Temporary::new("cli-serve", "test-green");
    tested(&root);
    let serving = Serving::start(&root, &["test", "--target", "browser", "--filter", "/own"]);

    let token = token_of(&serving.get("/").2);
    let (status, _, body) = serving.post(
        "/",
        Some("application/json"),
        report(
            &token,
            (0, 0),
            &posted("L293bjogcGFzc2VkCg==", "0", "null", "true"),
        )
        .as_bytes(),
    );
    assert_eq!(status, "HTTP/1.1 200 OK");
    assert!(String::from_utf8_lossy(&body).ends_with(",\"next\":null}"));

    let (code, stdout, stderr) = serving.ended();
    assert_eq!(code, Some(0), "{}", String::from_utf8_lossy(&stderr));
    assert_eq!(
        String::from_utf8_lossy(&stdout),
        "/own: passed\n1 passed, 0 failed\n"
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
