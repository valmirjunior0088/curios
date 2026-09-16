//! What `curios wonder` does at the command line and over the wire — the two transports, exercised as their consumers reach them: a process with arguments and standard input, and a process spoken to in the language server protocol.
//!
//! The engine's own behaviour — which records a program yields — is covered beside it in `wonder/tests.rs`; these decide what the transports do with them: that an answer is stdout and exit 0 whatever it says, that a file is placed in its unit, and that the server publishes the same records where the editor is looking and clears them when they go.

use {
    curios_utilities::test_support::Temporary,
    curios_wonder::SETTLE,
    std::{
        fs,
        io::{BufRead, BufReader, Read, Write},
        path::Path,
        process::{Child, ChildStdout, Command, Output, Stdio},
        thread,
        time::Duration,
    },
};

/// Long enough that two notifications sent around one of these reach the analyst as two checks rather than one coalesced pair — which is what a test of the skip needs, since a coalesced pair hides what the analyst did with the second.
///
/// Derived from the analyst's own settle rather than restated, so raising that figure cannot leave this one quietly too short. The multiple is slack for a loaded machine, not a second opinion about the cadence.
const QUIET: Duration = SETTLE.saturating_mul(4);

/// A directory of its own, shared with no other test and gone with it — canonical, which matters here because the server publishes the root the governance walk canonicalizes, and a URI computed from any other spelling would never match the one published.
fn temporary(name: &str) -> Temporary {
    Temporary::new("cli-wonder", name)
}

fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}

/// A package whose library spreads over two files and whose executable uses it.
fn project(name: &str) -> Temporary {
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
}

/// `--manifest` takes the file as it is spelled, and a bare `curios.toml` names the one in the working directory: its parent is the empty path, which is no directory to resolve on its own.
#[test]
fn a_bare_manifest_override_names_the_working_directory() {
    let root = project("manifest-override");

    let answered = curios(
        &root,
        &["wonder", "diagnostics", "--manifest", "curios.toml"],
        "",
    );

    assert!(
        answered.status.success(),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    assert!(stdout(&answered).is_empty(), "{}", stdout(&answered));
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
}

/// A file under a package that no `mod` reaches is in no unit, so it is checked on its own — written as a module, as a unit of its own — behind a note saying so and what would declare it, on a cold store and on a warm one alike, since placement walks the headers and not the record of a compile. Declaring it puts it in the library, and the note goes.
#[test]
fn a_file_no_mod_declares_is_checked_on_its_own_behind_a_note() {
    let root = project("undeclared");
    write(&root, "stray.crs", "pub let w : /std/Str = 1;\n");

    let cold = curios(&root, &["wonder", "diagnostics", "stray.crs"], "");
    assert!(cold.status.success());
    let text = stdout(&cold);
    assert!(
        text.starts_with("note: stray.crs is in no unit of `/app`"),
        "{text}"
    );
    assert!(
        text.contains("declare it with `mod stray;` in lib.crs"),
        "{text}"
    );
    assert!(
        !text.contains("stray.crs:1:1"),
        "a note is its message alone: {text}"
    );
    assert!(text.contains("type mismatch"), "checked on its own: {text}");

    // Warm: the library is served from the store, and placement still walks the headers.
    let built = curios(&root, &["run", "app"], "");
    assert!(
        built.status.success(),
        "{}",
        String::from_utf8_lossy(&built.stderr)
    );
    let warm = curios(&root, &["wonder", "diagnostics", "stray.crs"], "");
    assert!(
        stdout(&warm).contains("is in no unit of `/app`"),
        "{}",
        stdout(&warm)
    );

    write(&root, "lib.crs", "pub mod util;\npub mod stray;\n");
    let declared = curios(&root, &["wonder", "diagnostics", "stray.crs"], "");
    let text = stdout(&declared);
    assert!(!text.contains("is in no unit"), "{text}");
    assert!(
        text.contains("stray.crs:1:24"),
        "its own error, read as the library's: {text}"
    );
}

/// Text on standard input written as a module is checked as one, as a loose file is — a unit of its own, mounted at `/stdin` since it has no stem — so its items are answered one by one; a stage needs a program, and refuses it as written as a module.
#[test]
fn a_module_on_standard_input_is_checked_as_a_unit_of_its_own() {
    let root = temporary("stdin-module");
    fs::create_dir_all(&root).unwrap();
    let module = "pub let w : /std/Str = 1;\n";

    let answered = curios(&root, &["wonder", "diagnostics", "-"], module);
    assert!(answered.status.success());
    let text = stdout(&answered);
    assert!(text.contains("while elaborating /stdin/w"), "{text}");
    assert!(text.contains("--> <stdin>:1:24"), "{text}");

    let staged = curios(&root, &["wonder", "stage", "core", "-"], module);
    let refusal = String::from_utf8_lossy(&staged.stderr);
    assert_eq!(staged.status.code(), Some(1), "{refusal}");
    assert!(staged.stdout.is_empty());
    assert!(
        refusal.contains(
            "<stdin> is written as a module, with no final term to compile a program from"
        ),
        "{refusal}"
    );
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
}

/// A question stopped before it could answer exits as a build would, so written goals alone are the incomplete state that exits 2 — for `stage`, `cost` and `tests` alike — while `diagnostics`, whose answer the goals are, exits 0.
#[test]
fn a_question_stopped_by_goals_alone_exits_two() {
    let root = temporary("goals");
    fs::create_dir_all(&root).unwrap();
    let program = "let n : /std/Nat = ?;\n/std/print(\"\")\n";

    for query in [
        &["wonder", "stage", "wasm", "-"][..],
        &["wonder", "cost", "-"],
        &["wonder", "tests", "-"],
    ] {
        let stopped = curios(&root, query, program);
        assert_eq!(
            stopped.status.code(),
            Some(2),
            "{}: {}",
            query.join(" "),
            String::from_utf8_lossy(&stopped.stderr)
        );
        assert!(stopped.stdout.is_empty(), "{}", query.join(" "));
    }

    let answered = curios(&root, &["wonder", "diagnostics", "-"], program);
    assert_eq!(answered.status.code(), Some(0));
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
        // Looked for rather than assumed to be next: the analyst answers on its own thread, so a publish a test had no reason to read can still be ahead of the response.
        while !self.receive().contains(r#""id":9"#) {}
        self.send(r#"{"jsonrpc":"2.0","method":"exit","params":null}"#);
        // Read to the end rather than closing the pipe under the writer. A publish arriving after the read end is gone is a broken pipe, which the server reports as the session having failed — so dropping the reader here would make every test that left one in flight fail on the way out, for a reason that is the test's and not the server's.
        let mut rest = String::new();
        self.reader.read_to_string(&mut rest).ok();

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
}

/// A save carries no text, so the overlay it arrives with is the one the last keystroke was already checked against, and the analyst publishes nothing for it. What proves the skip is which publish comes back next: the edit's, where a save that had run would have put its own identical answer in front of it.
#[test]
fn a_save_of_text_already_checked_publishes_nothing() {
    let root = temporary("resaved");
    fs::create_dir_all(&root).unwrap();
    let path = root.join("scratch.crs");
    write(&root, "scratch.crs", "/std/print(\"\")\n");
    let uri = format!("file://{}", path.display());

    let mut editor = Editor::launch(&root);
    editor.send(r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#);
    editor.receive();
    editor.send(r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#);

    editor.send(&format!(
        r#"{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{uri}","languageId":"curios","version":1,"text":"let m : /std/Nat = ?;\n/std/print(\"\")\n"}}}}}}"#
    ));
    let opened = editor.receive();
    assert!(opened.contains(r#""severity":3"#), "{opened}");

    editor.send(&format!(
        r#"{{"jsonrpc":"2.0","method":"textDocument/didSave","params":{{"textDocument":{{"uri":"{uri}"}}}}}}"#
    ));
    thread::sleep(QUIET);

    editor.send(&format!(
        r#"{{"jsonrpc":"2.0","method":"textDocument/didChange","params":{{"textDocument":{{"uri":"{uri}","version":2}},"contentChanges":[{{"text":"/std/print(\"\")\n"}}]}}}}"#
    ));
    let published = editor.receive();
    assert!(published.contains(r#""diagnostics":[]"#), "{published}");

    let output = editor.finish();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

/// An editor discards what it held for a document it closed, so a reopen is answered even though the overlay it arrives with is exactly the one the last check read — the one case the skip above must not cover.
#[test]
fn a_reopened_document_is_published_again() {
    let root = temporary("reopened");
    fs::create_dir_all(&root).unwrap();
    let path = root.join("scratch.crs");
    write(&root, "scratch.crs", "/std/print(\"\")\n");
    let uri = format!("file://{}", path.display());
    let opening = format!(
        r#"{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{uri}","languageId":"curios","version":1,"text":"let m : /std/Nat = ?;\n/std/print(\"\")\n"}}}}}}"#
    );

    let mut editor = Editor::launch(&root);
    editor.send(r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#);
    editor.receive();
    editor.send(r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#);

    editor.send(&opening);
    assert!(editor.receive().contains(r#""severity":3"#));

    editor.send(&format!(
        r#"{{"jsonrpc":"2.0","method":"textDocument/didClose","params":{{"textDocument":{{"uri":"{uri}"}}}}}}"#
    ));
    thread::sleep(QUIET);

    editor.send(&opening);
    thread::sleep(QUIET);

    // An edit behind the reopen, so a reopen that was wrongly skipped is this edit's answer arriving first rather than silence — a test whose failure is nothing published is one that hangs instead of failing.
    editor.send(&format!(
        r#"{{"jsonrpc":"2.0","method":"textDocument/didChange","params":{{"textDocument":{{"uri":"{uri}","version":2}},"contentChanges":[{{"text":"let mm : /std/Nat = ?;\n/std/print(\"\")\n"}}]}}}}"#
    ));

    let republished = editor.receive();
    assert!(
        republished.contains(r#""start":{"character":19,"line":0}"#),
        "{republished}"
    );

    // And the edit behind it is answered in its turn, which is what says the reopen's publish was the reopen's.
    let edited = editor.receive();
    assert!(
        edited.contains(r#""start":{"character":20,"line":0}"#),
        "{edited}"
    );

    let output = editor.finish();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

/// A burst settles before it is compiled, so the first thing the editor is told is about the text the burst ended on. Without the wait the analyst would start on the first keystroke's text — it is idle when that one arrives — and publish an answer about a buffer two keystrokes out of date.
#[test]
fn a_burst_of_edits_is_answered_from_the_text_it_ended_on() {
    let root = temporary("burst");
    fs::create_dir_all(&root).unwrap();
    let path = root.join("scratch.crs");
    write(&root, "scratch.crs", "/std/print(\"\")\n");
    let uri = format!("file://{}", path.display());

    let mut editor = Editor::launch(&root);
    editor.send(r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#);
    editor.receive();
    editor.send(r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#);

    editor.send(&format!(
        r#"{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{uri}","languageId":"curios","version":1,"text":"/std/print(\"\")\n"}}}}}}"#
    ));
    assert!(editor.receive().contains(r#""diagnostics":[]"#));

    // One keystroke per name, each moving the goal one column right, sent as fast as the wire carries them.
    for (version, name) in ["m", "mm", "mmm"].iter().enumerate() {
        editor.send(&format!(
            r#"{{"jsonrpc":"2.0","method":"textDocument/didChange","params":{{"textDocument":{{"uri":"{uri}","version":{}}},"contentChanges":[{{"text":"let {name} : /std/Nat = ?;\n/std/print(\"\")\n"}}]}}}}"#,
            version + 2
        ));
    }

    let published = editor.receive();
    assert!(
        published.contains(r#""start":{"character":21,"line":0}"#),
        "{published}"
    );

    let output = editor.finish();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

/// The warming check is run for what it leaves on the analyst's thread and never for what it says. The workspace's library has a goal in it and the editor is never told: the first thing it hears about is the document it opened.
///
/// The wait before the document is what makes this a test of warming at all. A document sent straight after `initialize` is a document waiting, and the analyst drops the warming check rather than making it queue — so without the wait this would pass on a server that never warms.
#[test]
fn a_warming_check_never_publishes() {
    let root = temporary("warmed");
    write(&root, "curios.toml", "name = \"app\"\n");
    write(&root, "lib.crs", "pub mod util;\n");
    write(&root, "util.crs", "pub let word : /std/Str = ?;\n");
    write(&root, "scratch.crs", "/std/print(\"\")\n");
    let path = root.join("scratch.crs");
    let uri = format!("file://{}", path.display());
    let folder = format!("file://{}", root.display());

    let mut editor = Editor::launch(&root);
    editor.send(&format!(
        r#"{{"jsonrpc":"2.0","id":1,"method":"initialize","params":{{"capabilities":{{}},"workspaceFolders":[{{"uri":"{folder}","name":"app"}}]}}}}"#
    ));
    editor.receive();
    editor.send(r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#);
    thread::sleep(QUIET);

    editor.send(&format!(
        r#"{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{uri}","languageId":"curios","version":1,"text":"/std/print(\"\")\n"}}}}}}"#
    ));

    let published = editor.receive();
    assert!(published.contains("scratch.crs"), "{published}");
    assert!(!published.contains("util.crs"), "{published}");

    let output = editor.finish();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
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
        root.join(".curios/verdicts").is_dir(),
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
}

/// A package named `std` is the standard library, so a question about one of its modules is answered rather than refused as a collision: the fold withholds the archived root and compiles the package over the archived unit as a baseline. This checkout's tree is the archive's own, so the diff is empty and everything is reused.
///
/// The costliest test in this file: it lowers the whole standard library once and erases it whole.
#[test]
fn a_module_of_the_standard_library_is_answered_against_the_prelude_it_is_part_of() {
    let module = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../curios-prelude-archive/std/List.crs")
        .canonicalize()
        .expect("the standard library's tree");

    let answered = curios(
        module.parent().unwrap(),
        &["wonder", "diagnostics", module.to_str().unwrap()],
        "",
    );

    let stderr = String::from_utf8_lossy(&answered.stderr);
    assert!(answered.status.success(), "{stderr}");
    assert_eq!(stdout(&answered), "", "the standard library is clean");
    assert!(!stderr.contains("collid"), "{stderr}");
}

/// A file with two refusals answers with both: the compiler recovers past the first and reports the second beside it, and a declaration reaching a refused one is not in the answer.
#[test]
fn diagnostics_on_a_broken_file_lists_every_refusal() {
    let root = temporary("broken");
    fs::create_dir_all(&root).unwrap();

    let answered = curios(
        &root,
        &["wonder", "diagnostics", "-"],
        "let _a : /std/Nat = true;\nlet _b : /std/Nat = _a;\nlet _c : /std/Nat = false;\n/std/print(\"\")\n",
    );

    assert!(
        answered.status.success(),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    let text = stdout(&answered);
    assert!(text.contains("--> <stdin>:1:21"), "{text}");
    assert!(text.contains("--> <stdin>:3:21"), "{text}");
    assert!(!text.contains("_b"), "{text}");
}
