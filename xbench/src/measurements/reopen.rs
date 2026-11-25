//! What asking again about a document costs the language server, through the protocol an editor speaks: the reopen protocol of `curios-wonder`'s README, a fresh server a run over a case laid out afresh.

use {
    crate::{phase, read_span, turn},
    curios::CABINET,
    curios_common::{fill, test_support::Temporary},
    curios_wonder::SETTLE,
    std::{
        cell::Cell,
        env, fs,
        io::{BufRead, BufReader, Read, Write},
        path::{Path, PathBuf},
        process::{Child, ChildStdout, Command, Output, Stdio},
        thread,
        time::Duration,
    },
};

/// Long enough that two notifications sent around one of these reach the analyst as two checks rather than one coalesced pair: four times its settle, slack for a loaded machine.
const QUIET: Duration = SETTLE.saturating_mul(4);

/// The declaration a change adds to a module, a keystroke's worth of a definition.
const PROBE: &str = "\npub let _probe(a: Nat) -> Nat =\n    a;\n";

/// The editor's side of the wire: a server of its own, framed JSON-RPC written to it and read back.
struct Editor {
    child: Child,
    reader: BufReader<ChildStdout>,
}

impl Editor {
    /// A server over `root`: the release `curios` the runner built beside this bench, which ships, with no cabinet but the project's own.
    fn launch(root: &Path) -> Self {
        let curios = env::current_exe()
            .expect("the bench names its own executable")
            .with_file_name("curios");
        let mut child = Command::new(curios)
            .env_remove(CABINET)
            .current_dir(root)
            .args(["wonder", "--server"])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("the server launches");
        let reader = BufReader::new(child.stdout.take().expect("the server's output is piped"));

        Self { child, reader }
    }

    fn send(&mut self, body: &str) {
        let stdin = self
            .child
            .stdin
            .as_mut()
            .expect("the server's input is piped");
        write!(stdin, "Content-Length: {}\r\n\r\n{body}", body.len()).expect("the server reads");
        stdin.flush().expect("the server reads");
    }

    fn receive(&mut self) -> String {
        let mut length = None;
        loop {
            let mut line = String::new();
            self.reader.read_line(&mut line).expect("the server writes");
            let line = line.trim_end();
            if line.is_empty() {
                break;
            }
            if let Some(value) = line.strip_prefix("Content-Length:") {
                length = Some(value.trim().parse::<usize>().expect("a length"));
            }
        }
        let mut body = vec![0; length.expect("a Content-Length header")];
        self.reader
            .read_exact(&mut body)
            .expect("the server writes");

        String::from_utf8(body).expect("a message is text")
    }

    /// Send `notification` as a phase, read on to the diagnostics published for `uri` behind it, and read the phase's time as the figure `name`: from the notification's write to the publish. Its instructions are this process's waiting, not the server's work, so only the phase's span is read.
    fn answered(&mut self, notification: &str, uri: &str, name: &str) {
        let published = format!(r#""uri":"{uri}""#);
        let step = phase(&[], || {
            self.send(notification);
            while !self.receive().contains(&published) {}
        });
        read_span(name, step.span(), None);
    }

    /// Close the server's input and wait for it to end.
    fn finish(mut self) -> Output {
        drop(self.child.stdin.take());
        self.child.wait_with_output().expect("the server ends")
    }
}

/// `contents` written at `path` under `root`, its directories made.
fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().expect("a file has a directory"))
        .expect("a directory is made");
    fs::write(path, contents).expect("a file is written");
}

/// The tree at `from` copied into `to`, the cabinet it keeps left out, so a copy of a package is checked as one no question has filed.
fn copied(from: &Path, to: &Path) {
    fs::create_dir_all(to).expect("a directory is made");
    for entry in fs::read_dir(from).expect("a directory is read") {
        let entry = entry.expect("an entry is read");
        let name = entry.file_name();
        if name == ".curios" {
            continue;
        }
        match entry.file_type().expect("an entry has a type").is_dir() {
            true => copied(&entry.path(), &to.join(&name)),
            false => {
                fs::copy(entry.path(), to.join(&name)).expect("a file is copied");
            }
        }
    }
}

/// A document opened at `uri` holding `text`, at `version`.
fn opening(uri: &str, text: &str, version: u32) -> String {
    let text = serde_json::to_string(text).expect("a text encodes");

    fill!(
        r#"{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"${uri}","languageId":"curios","version":${version},"text":${text}}}}"#,
        uri,
        version,
        text
    )
}

/// The document at `uri` edited to hold `text`, at `version`.
fn changing(uri: &str, text: &str, version: u32) -> String {
    let text = serde_json::to_string(text).expect("a text encodes");

    fill!(
        r#"{"jsonrpc":"2.0","method":"textDocument/didChange","params":{"textDocument":{"uri":"${uri}","version":${version}},"contentChanges":[{"text":${text}}]}}"#,
        uri,
        version,
        text
    )
}

/// What [`reopen_measurements`] reopens, each laid out afresh for every run: a loose program reading one module, a package of twelve modules, and a copy of the standard library's package.
#[derive(Clone, Copy)]
enum Case {
    Loose,
    Package,
    Library,
}

/// One case laid out: the document opened, the text it holds, and the text a change leaves it holding, one declaration more.
struct Laid {
    pub document: PathBuf,
    pub text: String,
    pub changed: String,
}

impl Case {
    fn name(self) -> &'static str {
        match self {
            Self::Loose => "loose",
            Self::Package => "package",
            Self::Library => "library",
        }
    }

    /// Lay the case out in `root`.
    fn laid(self, root: &Path) -> Laid {
        match self {
            Self::Loose => {
                write(root, "Part.crs", "pub let value: /std/Nat = 1;\n");
                let text = "use /std/{Nat};\n\nmod Part;\n\n/std/print(Nat/to_str(Part/value))\n";
                write(root, "main.crs", text);

                Laid {
                    document: root.join("main.crs"),
                    text: text.to_string(),
                    // A program ends in its tail, so its declaration is written ahead of it.
                    changed: text.replace("\n/std/print", "\nlet _probe: Nat = 1;\n\n/std/print"),
                }
            }
            Self::Package => {
                write(
                    root,
                    "curios.toml",
                    concat!(
                        "compiler = \"",
                        env!("CARGO_PKG_VERSION"),
                        "\"\nname = \"app\"\n"
                    ),
                );
                write(
                    root,
                    "lib.crs",
                    &(0..12)
                        .map(|module| format!("pub mod M{module};\n"))
                        .collect::<String>(),
                );
                // Declarations spread evenly, cycling a sum, an induction and a proof by computation.
                for module in 0..12 {
                    let declarations = (0..4)
                        .map(|declaration| match declaration % 3 {
                            0 => format!("\npub let sum_{declaration}(n: Nat) -> Nat = n + {declaration};\n"),
                            1 => format!("\npub let count_{declaration}(n: Nat) -> Nat =\n    match n\n    | 0 => 0\n    | m + 1 => count_{declaration}(m) + 1\n    end;\n"),
                            _ => format!("\npub let zero_{declaration}(n: Nat) -> Eq()(n + 0, n) = Eq/refl();\n"),
                        })
                        .collect::<String>();
                    write(
                        root,
                        &format!("M{module}.crs"),
                        &fill!("use /std/{Nat, Eq};\n${declarations}", declarations),
                    );
                }
                let document = root.join("M0.crs");
                let text = fs::read_to_string(&document).expect("a module is read");

                Laid {
                    document,
                    changed: format!("{text}{PROBE}"),
                    text,
                }
            }
            Self::Library => {
                copied(
                    &Path::new(env!("CARGO_MANIFEST_DIR")).join("../curios-prelude-build/std"),
                    root,
                );
                let document = root.join("Nat.crs");
                let text = fs::read_to_string(&document).expect("a module is read");

                Laid {
                    document,
                    changed: format!("{text}{PROBE}"),
                    text,
                }
            }
        }
    }
}

/// One run of `case`, the `run`th: a fresh server over the case laid out afresh, each step read as a figure of the case — `<case>/open` the document opened, `<case>/change` a change adding one declaration, which is then written to disk, and `<case>/reopen` and `<case>/reopen again` the document closed and opened again with that text, twice.
fn reopened(case: Case, run: usize) {
    let root = Temporary::new("reopen", &format!("{}-{run}", case.name()));
    let Laid {
        document,
        text,
        changed,
    } = case.laid(&root);
    let uri = format!("file://{}", document.display());
    let name = case.name();

    let mut editor = Editor::launch(&root);
    editor.send(r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#);
    editor.receive();
    editor.send(r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#);

    thread::sleep(QUIET);
    editor.answered(&opening(&uri, &text, 1), &uri, &format!("{name}/open"));
    editor.answered(
        &changing(&uri, &changed, 2),
        &uri,
        &format!("{name}/change"),
    );
    fs::write(&document, &changed).expect("the change is written");
    for (version, step) in [(3, "reopen"), (4, "reopen again")] {
        editor.send(&fill!(
            r#"{"jsonrpc":"2.0","method":"textDocument/didClose","params":{"textDocument":{"uri":"${uri}"}}}"#,
            uri
        ));
        thread::sleep(QUIET);
        editor.answered(
            &opening(&uri, &changed, version),
            &uri,
            &format!("{name}/{step}"),
        );
    }

    let output = editor.finish();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

/// What asking again about a document costs where nothing moved under it: [`curios-wonder`'s README](../../../curios-wonder/README.md#a-change-on-disk-is-the-editors-to-watch-and-it-asks-again-about-every-open-document) reads it off the second reopen, `<case>/reopen again`, a reopen of a document whose text the disk already holds.
///
/// `cargo xbench measure reopen_measurements` takes it: the three cases turned round-robin seven times after an untimed round, each run a fresh server initialized with no workspace folders over its case laid out afresh ([`reopened`]), a pause of four times the analyst's settle before each open, and each step a phase from its notification's write to the diagnostics published for the document. The server is the release `curios` the runner builds beside the bench. Wall clock, the analyst's settle wait included.
pub fn reopen_measurements() {
    let runs = Cell::new(0);
    let run = |case| {
        reopened(case, runs.get());
        runs.set(runs.get() + 1);
    };
    let (mut loose, mut package, mut library) = (
        || run(Case::Loose),
        || run(Case::Package),
        || run(Case::Library),
    );

    turn(7, &mut [&mut loose, &mut package, &mut library]);
}
