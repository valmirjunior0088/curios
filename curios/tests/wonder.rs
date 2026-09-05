//! What `curios wonder` does at the command line and over the wire — the two transports, exercised as their consumers reach them: a process with arguments and standard input, and a process spoken to in the language server protocol.
//!
//! The engine's own behaviour — which records a program yields — is covered beside it in `wonder/tests.rs`; these decide what the transports do with them: that an answer is stdout and exit 0 whatever it says, that a file is placed in its unit, and that the server publishes the same records where the editor is looking and clears them when they go.

use std::{
    env, fs,
    io::{BufRead, BufReader, Read, Write},
    path::{Path, PathBuf},
    process::{self, Child, ChildStdout, Command, Output, Stdio},
    time::{SystemTime, UNIX_EPOCH},
};

/// A directory of its own, shared with no other test.
fn temporary(name: &str) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();

    // Canonical, because the server publishes the root the governance walk canonicalizes, and macOS's temporary directory sits behind a symlink (`/var` → `/private/var`): a URI computed from the raw path would never match the one published.
    env::temp_dir().canonicalize().unwrap().join(format!(
        "curios-cli-wonder-{name}-{}-{millis}",
        process::id()
    ))
}

fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}

/// A package whose library spreads over two files and whose executable uses it.
fn project(name: &str) -> PathBuf {
    let root = temporary(name);
    write(
        &root,
        "curios.toml",
        "name = \"app\"\n\n[[executables]]\nname = \"app\"\n",
    );
    write(&root, "lib.crs", "pub mod util;\n");
    write(&root, "util.crs", "pub let word : /std/Str = \"placed\";\n");
    write(&root, "app.crs", "/std/print(/app/util/word)\n");
    root
}

/// Run the compiler in `root`, with the arguments given and `stdin` on its standard input.
fn curios(root: &Path, arguments: &[&str], stdin: &str) -> Output {
    let mut child = Command::new(env!("CARGO_BIN_EXE_curios"))
        .current_dir(root)
        .args(arguments)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("run the compiler");
    child
        .stdin
        .take()
        .unwrap()
        .write_all(stdin.as_bytes())
        .unwrap();

    child.wait_with_output().expect("the compiler exits")
}

fn stdout(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

/// A goal on standard input is an answer: located, rendered as `run` reports it, on stdout, exit 0.
#[test]
fn a_goal_is_answered_on_stdout_with_exit_zero() {
    let root = temporary("stdin");
    fs::create_dir_all(&root).unwrap();

    let answered = curios(
        &root,
        &["wonder", "diagnostics", "-"],
        "let m : /std/Nat = ?;\n/std/print(\"\")\n",
    );

    assert!(
        answered.status.success(),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    let text = stdout(&answered);
    assert!(text.starts_with("goal `?`"), "{text}");
    assert!(text.contains("--> <stdin>:1:20"), "{text}");
    assert!(answered.stderr.is_empty(), "nothing is narrated");

    fs::remove_dir_all(root).unwrap();
}

/// A file is placed in the unit that declares it: the executable compiles against its library, so `/app/util/word` resolves — which it would not standalone — and a module of the library is checked as the library, reporting at the module's own path.
#[test]
fn a_file_is_placed_in_its_unit() {
    let root = project("placed");

    let executable = curios(&root, &["wonder", "diagnostics", "app.crs"], "");
    assert!(executable.status.success());
    assert_eq!(stdout(&executable), "", "the executable sees its library");

    write(&root, "util.crs", "pub let word : /std/Str = 1;\n");
    let module = curios(&root, &["wonder", "diagnostics", "util.crs"], "");
    assert!(module.status.success(), "an error is still an answer");
    let text = stdout(&module);
    assert!(text.contains("type mismatch"), "{text}");
    assert!(text.contains("util.crs:1:27"), "{text}");

    fs::remove_dir_all(root).unwrap();
}

/// A stage that the program does not reach is not an answer: diagnostics on stderr, exit 1, stdout empty.
#[test]
fn an_unreached_stage_leaves_stdout_empty() {
    let root = temporary("unreached");
    fs::create_dir_all(&root).unwrap();

    let refused = curios(
        &root,
        &["wonder", "stage", "wasm", "-"],
        "let m : /std/Nat = true;\n/std/print(\"\")\n",
    );

    assert_eq!(refused.status.code(), Some(1));
    assert!(refused.stdout.is_empty());
    assert!(String::from_utf8_lossy(&refused.stderr).contains("type mismatch"));

    fs::remove_dir_all(root).unwrap();
}

/// One side of the wire: frame a JSON-RPC message, and read one back.
struct Editor {
    child: Child,
    reader: BufReader<ChildStdout>,
}

impl Editor {
    fn launch(root: &Path) -> Self {
        let mut child = Command::new(env!("CARGO_BIN_EXE_curios"))
            .current_dir(root)
            .args(["wonder", "server"])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("launch the server");
        let reader = BufReader::new(child.stdout.take().unwrap());

        Self { child, reader }
    }

    fn send(&mut self, body: &str) {
        let stdin = self.child.stdin.as_mut().unwrap();
        write!(stdin, "Content-Length: {}\r\n\r\n{body}", body.len()).unwrap();
        stdin.flush().unwrap();
    }

    fn receive(&mut self) -> String {
        let mut length = None;
        loop {
            let mut line = String::new();
            self.reader.read_line(&mut line).unwrap();
            let line = line.trim_end();
            if line.is_empty() {
                break;
            }
            if let Some(value) = line.strip_prefix("Content-Length:") {
                length = Some(value.trim().parse::<usize>().unwrap());
            }
        }
        let mut body = vec![0; length.expect("a Content-Length header")];
        self.reader.read_exact(&mut body).unwrap();

        String::from_utf8(body).unwrap()
    }

    fn finish(mut self) -> Output {
        self.send(r#"{"jsonrpc":"2.0","id":9,"method":"shutdown","params":null}"#);
        assert!(self.receive().contains(r#""id":9"#));
        self.send(r#"{"jsonrpc":"2.0","method":"exit","params":null}"#);
        drop(self.reader);

        self.child.wait_with_output().unwrap()
    }
}

/// The server publishes the record the query would have rendered — the goal at its own occurrence, as information — from the buffer the editor holds, and clears it once the edit removes it.
#[test]
fn the_server_publishes_from_the_buffer_and_clears() {
    let root = temporary("server");
    fs::create_dir_all(&root).unwrap();
    let path = root.join("scratch.crs");
    write(&root, "scratch.crs", "/std/print(\"\")\n");
    let uri = format!("file://{}", path.display());

    let mut editor = Editor::launch(&root);
    editor.send(r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#);
    assert!(editor.receive().contains("textDocumentSync"));
    editor.send(r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#);

    // What the editor holds, not what the disk does: the file on disk compiles, the buffer has a goal.
    editor.send(&format!(
        r#"{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{uri}","languageId":"curios","version":1,"text":"let m : /std/Nat = ?;\n/std/print(\"\")\n"}}}}}}"#
    ));
    let published = editor.receive();
    assert!(
        published.contains("textDocument/publishDiagnostics"),
        "{published}"
    );
    assert!(
        published.contains(r#""message":"goal `?`\n  ? : Nat""#),
        "{published}"
    );
    assert!(published.contains(r#""severity":3"#), "{published}");
    assert!(
        published.contains(r#""start":{"character":19,"line":0}"#),
        "{published}"
    );

    editor.send(&format!(
        r#"{{"jsonrpc":"2.0","method":"textDocument/didChange","params":{{"textDocument":{{"uri":"{uri}","version":2}},"contentChanges":[{{"text":"/std/print(\"\")\n"}}]}}}}"#
    ));
    let cleared = editor.receive();
    assert!(cleared.contains(r#""diagnostics":[]"#), "{cleared}");

    let output = editor.finish();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    fs::remove_dir_all(root).unwrap();
}

/// A lint reaches the editor as a warning, at the word it is about: the severity the protocol has for a finding that stops nothing.
#[test]
fn a_lint_is_published_as_a_warning() {
    let root = temporary("lint");
    fs::create_dir_all(&root).unwrap();
    let path = root.join("scratch.crs");
    write(&root, "scratch.crs", "/std/print(\"\")\n");
    let uri = format!("file://{}", path.display());

    let mut editor = Editor::launch(&root);
    editor.send(r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#);
    editor.receive();
    editor.send(r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#);

    editor.send(&format!(
        r#"{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{uri}","languageId":"curios","version":1,"text":"use /std/{{Bool}};\n/std/print(\"\")\n"}}}}}}"#
    ));
    let published = editor.receive();
    assert!(
        published.contains(r#""message":"unused import `Bool`; delete it""#),
        "{published}"
    );
    assert!(published.contains(r#""severity":2"#), "{published}");
    assert!(
        published.contains(r#""start":{"character":10,"line":0}"#),
        "{published}"
    );

    let output = editor.finish();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    fs::remove_dir_all(root).unwrap();
}

/// A store a build already filled answers about the disk, so it does not answer here: the library's unit is in the store, the editor holds a module of it that does not type-check, and the record is published anyway. A stored unit is believed on a re-read of the files it was compiled from, which still hold what was built — so a hit taken here would report on the file rather than on the document that was asked about.
///
/// The second half is that the record goes when the buffer stops disagreeing, with the store warm throughout. It does not witness a surviving hit: the document is still open, so the overlay still reaches its unit either way, and no progress event reaches this transport to say which happened.
#[test]
fn a_warm_store_does_not_answer_for_the_buffer() {
    let root = project("warm");

    // Filled as a build fills it — `wonder` itself never writes a store.
    let built = curios(&root, &["run", "app"], "");
    assert!(
        built.status.success(),
        "{}",
        String::from_utf8_lossy(&built.stderr)
    );
    assert!(
        root.join(".curios/unit").is_dir(),
        "a build files the library's unit"
    );

    let uri = format!("file://{}", root.join("util.crs").display());
    let mut editor = Editor::launch(&root);
    editor.send(r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#);
    editor.receive();
    editor.send(r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#);

    editor.send(&format!(
        r#"{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{uri}","languageId":"curios","version":1,"text":"pub let word : /std/Str = 1;\n"}}}}}}"#
    ));
    let published = editor.receive();
    assert!(published.contains("type mismatch"), "{published}");

    editor.send(&format!(
        r#"{{"jsonrpc":"2.0","method":"textDocument/didChange","params":{{"textDocument":{{"uri":"{uri}","version":2}},"contentChanges":[{{"text":"pub let word : /std/Str = \"placed\";\n"}}]}}}}"#
    ));
    let cleared = editor.receive();
    assert!(cleared.contains(r#""diagnostics":[]"#), "{cleared}");

    let output = editor.finish();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    fs::remove_dir_all(root).unwrap();
}

/// A library header that does not parse is a located record like any other: published on the header, underlining the item head the parser refused, with a message that holds no snippet — the editor draws the location, and a caret drawn in text cannot line up in a proportional font.
#[test]
fn a_header_that_does_not_parse_is_located_without_a_snippet() {
    let root = project("header");
    write(&root, "lib.crs", "pub mod util;\na\n");
    let uri = format!("file://{}", root.join("lib.crs").display());

    let mut editor = Editor::launch(&root);
    editor.send(r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#);
    editor.receive();
    editor.send(r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#);
    editor.send(&format!(
        r#"{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{uri}","languageId":"curios","version":1,"text":"pub mod util;\na\n"}}}}}}"#
    ));

    let published = editor.receive();
    assert!(
        published.contains(&format!(r#""uri":"{uri}""#)),
        "{published}"
    );
    assert!(
        published.contains(
            r#""range":{"end":{"character":1,"line":1},"start":{"character":0,"line":1}}"#
        ),
        "{published}"
    );
    assert!(
        published.contains("Expected a top-level item"),
        "{published}"
    );
    assert!(
        !published.contains("-->"),
        "no snippet in a message: {published}"
    );

    let output = editor.finish();
    assert!(output.status.success());

    fs::remove_dir_all(root).unwrap();
}

/// Formatting is `curios format` over what the editor holds: one whole-document edit to the canonical form, and none once it is canonical.
#[test]
fn formatting_answers_with_the_canonical_text() {
    let root = temporary("format");
    fs::create_dir_all(&root).unwrap();
    write(&root, "scratch.crs", "/std/print(\"\")\n");
    let uri = format!("file://{}", root.join("scratch.crs").display());

    let mut editor = Editor::launch(&root);
    editor.send(r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#);
    assert!(
        editor
            .receive()
            .contains(r#""documentFormattingProvider":true"#)
    );
    editor.send(r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#);
    editor.send(&format!(
        r#"{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{uri}","languageId":"curios","version":1,"text":"let   x : /std/Nat =   1;\n/std/print(\"\")\n"}}}}}}"#
    ));
    editor.receive();

    editor.send(&format!(
        r#"{{"jsonrpc":"2.0","id":2,"method":"textDocument/formatting","params":{{"textDocument":{{"uri":"{uri}"}},"options":{{"tabSize":4,"insertSpaces":true}}}}}}"#
    ));
    let formatted = editor.receive();
    assert!(
        formatted.contains(r#""newText":"let x: /std/Nat =\n    1;\n\n/std/print(\"\")\n""#),
        "{formatted}"
    );
    assert!(
        formatted.contains(r#""start":{"character":0,"line":0}"#),
        "{formatted}"
    );
    assert!(
        formatted.contains(r#""end":{"character":0,"line":2}"#),
        "{formatted}"
    );

    let output = editor.finish();
    assert!(output.status.success());

    fs::remove_dir_all(root).unwrap();
}

/// A request is never behind a compile: a formatting request sent right after an edit is answered before the edit's diagnostics are published, because the protocol thread answers it while the analyst is still checking.
#[test]
fn formatting_is_answered_while_a_check_is_running() {
    let root = temporary("interleaved");
    fs::create_dir_all(&root).unwrap();
    write(&root, "scratch.crs", "/std/print(\"\")\n");
    let uri = format!("file://{}", root.join("scratch.crs").display());

    let mut editor = Editor::launch(&root);
    editor.send(r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#);
    editor.receive();
    editor.send(r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#);

    // The edit starts a check; the request follows it immediately.
    editor.send(&format!(
        r#"{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{uri}","languageId":"curios","version":1,"text":"let   m : /std/Nat = ?;\n/std/print(\"\")\n"}}}}}}"#
    ));
    editor.send(&format!(
        r#"{{"jsonrpc":"2.0","id":2,"method":"textDocument/formatting","params":{{"textDocument":{{"uri":"{uri}"}},"options":{{"tabSize":4,"insertSpaces":true}}}}}}"#
    ));

    let first = editor.receive();
    let second = editor.receive();
    assert!(
        first.contains(r#""id":2"#),
        "the answer comes first: {first}"
    );
    assert!(first.contains(r#""newText""#), "{first}");
    assert!(
        second.contains("publishDiagnostics"),
        "then the check: {second}"
    );
    assert!(second.contains("goal `?`"), "{second}");

    let output = editor.finish();
    assert!(output.status.success());

    fs::remove_dir_all(root).unwrap();
}
