//! The server transport: the same questions, asked by an editor over the language server protocol.
//!
//! **Two threads, and the compiler is on the one that never reads the protocol.** The compiler is single-threaded by construction — `Rc` spans, a thread-local prelude, a `RootSource` that is deliberately not `Send` — so exactly one thread, the *analyst*, owns it; the protocol thread reads messages, keeps the overlay, answers what needs no compilation, and hands the analyst check jobs over a channel. What this buys is that a request is never behind a compile: a formatting request arriving during a two-second check is answered by the protocol thread at once, where one thread doing both would have answered it after the check, past the editor's timeout. This was first written as one thread on the reading that `lsp-server`'s synchronous loop was the whole design, and the timeout is what that reading cost. Beyond two the channel is indifferent — more analysts would be a loop around `thread::spawn`, each with its own prelude — but a second compile of the same document is what coalescing makes unnecessary, and edits to two documents at once are rare enough that the second waits one check.
//!
//! **The editor's documents are the overlay.** Every open document's text is consulted before the disk by every unit the check assembles (`RootSource::with_overlay`), so a diagnostic reflects the buffer rather than the file, and an unsaved new module is still found by the `mod` that declares it. Placement is `curios_package::Selection`'s, exactly as the one-shot query's, and it walks the overlay too — the unit whose `mod` chain declares the document, so an unsaved `mod` line counts, or no unit at all.
//!
//! **Edits coalesce on the analyst, and a burst settles before it is compiled.** A job carries the whole overlay as it stood when the edit arrived; before compiling, the analyst drains every job queued behind it and keeps the latest overlay and the union of the documents to check, so a burst of keystrokes during one check costs one more check from the newest text rather than one per keystroke. Draining what is already queued is not enough on its own, because the analyst is *idle* when the first keystroke of a burst arrives: it would start a check on text the next keystroke replaces, and then publish an answer about a buffer nobody is looking at any more. So the drain waits for each further job — `SETTLE_SHARE` of what the last check cost, capped at `SETTLE` — which is the one figure this cadence needs and the reason it needs no second one per project. Incrementality inside a unit is not here either, because it is the fold's: a unit the store holds from an earlier text is a baseline the edited unit is compiled over (`curios-pipeline`), so a check after a keystroke re-elaborates the closure of the edit rather than the unit, and this module only hands the fold the overlay.
//!
//! **Text already checked is not checked again.** The analyst keeps the overlay its last check read and the documents it answered from it; an edit or a save carrying an equal overlay publishes nothing, because the editor is already holding that answer. Equality is over *every* open document, since a question reads every file of the unit it is about and two overlays differing anywhere may differ in what it says. This is what makes a save free: a client resolves full-text sync to a `didSave` carrying no text, so the overlay a save arrives with is by construction the one the last keystroke was checked against, and re-running the check would compute the answer the editor already has. An open is never skipped — an editor discards what it held for a document it closed, so a reopen has to be answered whatever the overlay says.
//!
//! **The analyst warms when it has nothing else to do.** On `initialize` it is handed a check of the package governing the workspace root, whose answer is thrown away: nothing is open yet, so there is no document to publish about, and what the check leaves behind on this thread is the point — the restored archive, the compiler's identity settled against the store, and every module the package declares in the parse memo. It is an ordinary check rather than a list of steps so that it cannot drift from what a real one warms.
//!
//! **And it is dropped the moment a document is waiting.** The compiler is on one thread, so a document that arrived while the warming check was queued would wait the whole of it — measured at 6.7 s against 4.3 s for the first check of `/std`, which is the warming *and* the document rather than either. Its own check warms this thread for everything after it, so the work is not lost, only unqueued. What this leaves standing is the case warming was for: an editor that opens a folder and no file, where the first document opened later finds the thread warm.
//!
//! **Formatting is `curios format` over the overlay, on the protocol thread.** The same `Formatted` the CLI runs, on the text the editor holds, answered as one whole-document edit — the formatter is pure and cheap, parse and print with no prelude, and its output is verified by reparse before it is handed back, so nothing here can hand an editor text the compiler would read differently.
//!
//! **UTF-16 exists only here.** The engine's coordinates are bytes; a `Position` is derived from the span's own text at the boundary, in both directions, and nothing below this file knows the protocol's unit.

use {
    crate::{Asked, Diagnostic as Record, Severity},
    curios_package::{Selection, Spelling},
    curios_text::{Formatted, Overlay},
    curios_utilities::{Report, Source, Span},
    lsp_server::{Connection, Message, Notification, Request, RequestId, Response},
    lsp_types::{
        Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
        DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams,
        InitializeParams, OneOf, Position, PublishDiagnosticsParams, Range, ServerCapabilities,
        TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Uri,
        notification::{
            DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, DidSaveTextDocument,
            Notification as NotificationTrait, PublishDiagnostics,
        },
        request::{Formatting, Request as RequestTrait},
    },
    std::{
        collections::{BTreeMap, BTreeSet},
        path::{Path, PathBuf},
        str::FromStr,
        sync::mpsc,
        thread,
        time::{Duration, Instant},
    },
};

/// The longest the analyst will wait for another job before compiling the ones it has.
///
/// A cap, and the wait under it is `SETTLE_SHARE` of what the last check cost. A fixed wait is the wrong shape: it buys a package whose check costs two seconds a great deal and charges a package whose check costs a quarter of one the same, where there is almost nothing to save. Measured on `/std`, a fixed 150 ms was 7% of a keystroke; on a 120-declaration package the same figure would have been most of one.
///
/// **Public because outwaiting it is the only way to send two notifications as two checks.** A driver that means to observe what the analyst did with the second — a test of the skip above all — has to leave a gap wider than this, and a gap written as its own figure somewhere else is one that stops being wider the day this one moves.
pub const SETTLE: Duration = Duration::from_millis(150);

/// What fraction of the last check's cost the analyst will spend waiting to avoid repeating it.
///
/// The invariant this states is the whole design: **never wait more than a tenth of what the work it might save would cost.** A burst then costs at most a tenth of a check to coalesce, and an isolated edit is delayed by at most that — so the wait is worth having where a check is expensive and disappears where it is not, with no figure to tune per project.
const SETTLE_SHARE: u32 = 10;

/// Serve until the editor says shutdown.
pub fn serve(budget: u64, manifest: Option<&Path>) -> Result<(), String> {
    let (connection, io) = Connection::stdio();

    let capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        document_formatting_provider: Some(OneOf::Left(true)),
        ..ServerCapabilities::default()
    };
    let initialization = connection
        .initialize(serde_json::to_value(capabilities).expect("capabilities serialize"))
        .map_err(|error| error.to_string())?;
    let params: InitializeParams =
        serde_json::from_value(initialization).map_err(|error| error.to_string())?;

    let (jobs, inbox) = mpsc::channel();
    let analyst = {
        let mut analyst = Analyst {
            budget,
            manifest: manifest.map(Path::to_path_buf),
            published: BTreeMap::new(),
            spent: None,
            checked: None,
            // A closure rather than the channel's own type, so this file names no channel crate: `lsp-server` re-exports none, and what the analyst needs is only a way to send.
            sender: {
                let sender = connection.sender.clone();
                Box::new(move |message| sender.send(message).map_err(|error| error.to_string()))
            },
        };
        thread::spawn(move || analyst.run(&inbox))
    };

    let mut server = Server {
        documents: BTreeMap::new(),
        jobs,
    };

    // Before the loop, so the warming check is under way while the editor is still sending what it opened. A workspace the editor did not name is one nothing can be warmed for, and the first document pays what it pays today.
    if let Some(root) = workspace_root(&params) {
        server.check(root, Raised::Warming)?;
    }

    let served = (|| {
        for message in &connection.receiver {
            if shutting_down(&connection, &message)? {
                return Ok(());
            }
            server.handle(&connection, message)?;
        }
        Ok::<(), String>(())
    })();

    // The analyst ends when its channel does; the writer thread ends when the last sender is dropped, and both the connection and the analyst hold one — so everything is dropped before anything is joined, or the join waits forever.
    drop(server);
    let analysed = analyst
        .join()
        .unwrap_or_else(|_| Err("the analyst panicked".to_string()));
    drop(connection);
    // The session's own error first: dropping the connection closes the reader's channel, so the reader fails on whatever the editor had already written, and reporting that would name a channel where the session ended on something else.
    let joined = io.join().map_err(|error| error.to_string());

    served.and(analysed).and(joined)
}

/// Whether `message` is the shutdown request — answered here, and followed by waiting for the `exit` that ends the session.
fn shutting_down(connection: &Connection, message: &Message) -> Result<bool, String> {
    match message {
        Message::Request(request) => connection
            .handle_shutdown(request)
            .map_err(|error| error.to_string()),
        _ => Ok(false),
    }
}

/// What the protocol thread asks of the analyst: check `document` against the overlay as it stood.
struct Job {
    document: PathBuf,
    raised: Raised,
    /// Every open document's text at the moment of the edit — `Overlay` itself holds an `Rc`, so it is built on the analyst's side.
    documents: BTreeMap<PathBuf, String>,
}

/// Why a job was raised, which is what decides whether the analyst may skip it and whether its records reach the editor.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Raised {
    /// A document was opened. Never skipped: an editor discards what it held for a document it closed, so a reopen is owed an answer even when nothing changed since the last check.
    Opened,
    /// A document was edited or saved. Skipped when the overlay it carries is the one the last check already read.
    Edited,
    /// The workspace root, checked once at `initialize`. `document` is a directory rather than a file, and the records go nowhere — see the module documentation.
    Warming,
}

/// The protocol thread's state: the overlay, and the channel to the analyst.
struct Server {
    /// Every open document's current text, by path — the overlay.
    documents: BTreeMap<PathBuf, String>,
    jobs: mpsc::Sender<Job>,
}

impl Server {
    fn handle(&mut self, connection: &Connection, message: Message) -> Result<(), String> {
        match message {
            Message::Notification(notification) => self.notified(notification),
            Message::Request(request) => {
                let Request { id, method, params } = request;
                match method.as_str() {
                    Formatting::METHOD => {
                        // One request the editor got wrong is that request failing, with the protocol's own code for it — never the session ending, which costs every open document its diagnostics until the editor restarts the server.
                        let response =
                            match serde_json::from_value::<DocumentFormattingParams>(params) {
                                Err(error) => Response::new_err(
                                    id,
                                    lsp_server::ErrorCode::InvalidParams as i32,
                                    error.to_string(),
                                ),
                                Ok(params) => match self.format(&params.text_document.uri) {
                                    Ok(edits) => Response::new_ok(id, edits),
                                    Err(message) => Response::new_err(
                                        id,
                                        lsp_server::ErrorCode::RequestFailed as i32,
                                        message,
                                    ),
                                },
                            };
                        connection
                            .sender
                            .send(Message::Response(response))
                            .map_err(|error| error.to_string())
                    }
                    // Every other request is declined in the capabilities, so one that arrives anyway gets the protocol's "not found" rather than silence.
                    _ => reply_unhandled(connection, id, &method),
                }
            }
            Message::Response(_) => Ok(()),
        }
    }

    /// Hand the analyst a check of `document` against the overlay as it stands now.
    fn check(&self, document: PathBuf, raised: Raised) -> Result<(), String> {
        self.jobs
            .send(Job {
                document,
                raised,
                documents: self.documents.clone(),
            })
            .map_err(|_| "the analyst is gone".to_string())
    }

    /// A notification whose params do not deserialize is dropped, with a line on stderr for the editor's log: it has no reply to fail, and the session is not what is wrong.
    fn notified(&mut self, notification: Notification) -> Result<(), String> {
        let Notification { method, params } = notification;
        let dropped = |error: &serde_json::Error| eprintln!("{method}: {error}; ignored");
        match method.as_str() {
            DidOpenTextDocument::METHOD => {
                let Ok(params) = serde_json::from_value::<DidOpenTextDocumentParams>(params)
                    .inspect_err(dropped)
                else {
                    return Ok(());
                };
                if let Some(path) = path_of(&params.text_document.uri) {
                    self.documents
                        .insert(path.clone(), params.text_document.text);
                    self.check(path, Raised::Opened)?;
                }
            }
            DidChangeTextDocument::METHOD => {
                let Ok(params) = serde_json::from_value::<DidChangeTextDocumentParams>(params)
                    .inspect_err(dropped)
                else {
                    return Ok(());
                };
                if let Some(path) = path_of(&params.text_document.uri)
                    && let Some(change) = params.content_changes.into_iter().last()
                {
                    self.documents.insert(path.clone(), change.text);
                    self.check(path, Raised::Edited)?;
                }
            }
            DidSaveTextDocument::METHOD => {
                let Ok(params) = serde_json::from_value::<DidSaveTextDocumentParams>(params)
                    .inspect_err(dropped)
                else {
                    return Ok(());
                };
                if let Some(path) = path_of(&params.text_document.uri) {
                    if let Some(text) = params.text {
                        self.documents.insert(path.clone(), text);
                    }
                    self.check(path, Raised::Edited)?;
                }
            }
            DidCloseTextDocument::METHOD => {
                let Ok(params) = serde_json::from_value::<DidCloseTextDocumentParams>(params)
                    .inspect_err(dropped)
                else {
                    return Ok(());
                };
                if let Some(path) = path_of(&params.text_document.uri) {
                    self.documents.remove(&path);
                }
            }
            _ => {}
        }

        Ok(())
    }

    /// `curios format`'s machinery over the document the editor holds: one edit replacing the whole text when the canonical form differs, none when it does not. A document that cannot be formatted — it does not parse, or the formatter refused its own output — is the request failing with the formatter's message, since silently editing nothing would read as "already canonical".
    fn format(&self, uri: &Uri) -> Result<Vec<TextEdit>, String> {
        let path = path_of(uri).ok_or_else(|| format!("{} is not a file", uri.as_str()))?;
        let text = match self.documents.get(&path) {
            Some(text) => text.clone(),
            None => std::fs::read_to_string(&path).map_err(|error| error.to_string())?,
        };

        match Formatted::from_source(&Source::held(&path, text.as_str()))? {
            Formatted::Unchanged(_) => Ok(Vec::new()),
            Formatted::Changed(formatted) => Ok(vec![TextEdit {
                range: Range {
                    start: Position::default(),
                    end: position_of(&text, text.len()),
                },
                new_text: formatted,
            }]),
        }
    }
}

/// The analysis thread's state: the compiler's inputs, what it last published, and the way back to the editor.
struct Analyst {
    budget: u64,
    manifest: Option<PathBuf>,
    /// For each document checked, every path it last published diagnostics to — so a diagnostic that moved or vanished is cleared where it was.
    published: BTreeMap<PathBuf, BTreeSet<PathBuf>>,
    /// How long the last batch of checks took — what the settle before the next burst is a fraction of. `None` until one has run.
    spent: Option<Duration>,
    /// The overlay the last check read, and the documents already answered from it — what lets a save of text already checked publish nothing. Cleared to a fresh set the moment the overlay moves, since an answer about one text says nothing about another.
    checked: Option<(BTreeMap<PathBuf, String>, BTreeSet<PathBuf>)>,
    sender: Box<dyn Fn(Message) -> Result<(), String> + Send>,
}

impl Analyst {
    /// Check until the channel closes, which is the protocol thread finishing.
    fn run(&mut self, inbox: &mpsc::Receiver<Job>) -> Result<(), String> {
        while let Ok(first) = inbox.recv() {
            // Coalesce and settle: everything queued behind the first job is newer, so the last overlay wins and every document any of them named is checked once against it, and the wait is what makes a burst arriving at an idle analyst one check rather than two. A closed channel ends this loop exactly as a timeout does, and the outer `recv` is what reports it.
            let mut documents = first.documents;
            let mut dirty = BTreeMap::new();
            raise(&mut dirty, first.document, first.raised);
            while let Ok(job) = inbox.recv_timeout(self.settle(&dirty)) {
                documents = job.documents;
                raise(&mut dirty, job.document, job.raised);
            }

            // Warming is worth running on an idle analyst and never in front of a document, which is why it is dropped rather than reordered: the compiler is on one thread, so a document waiting behind a warming check waits the whole of it, and its own check warms this thread for everything after it anyway. An editor that opens a folder and no file is the case this leaves standing.
            if dirty.values().any(|raised| *raised != Raised::Warming) {
                dirty.retain(|_, raised| *raised != Raised::Warming);
            }

            // Taken before the overlay is built, which consumes the map, and compared before anything is checked against it: what the editor holds is what the last answer was about, or it is not.
            let repeated = self
                .checked
                .as_ref()
                .is_some_and(|(read, _)| read == &documents);
            if !repeated {
                self.checked = Some((documents.clone(), BTreeSet::new()));
            }

            let overlay = Overlay::of(documents);
            let started = Instant::now();
            let mut ran = false;
            for (document, raised) in dirty {
                if raised == Raised::Edited && self.answered(&document) {
                    continue;
                }
                self.check(&document, raised, &overlay)?;
                ran = true;
                if let Some((_, answered)) = &mut self.checked {
                    answered.insert(document);
                }
            }

            // What the next burst is weighed against. A batch that checked nothing measures nothing, so a run of skipped saves leaves the figure where the last real check put it.
            if ran {
                self.spent = Some(started.elapsed());
            }
        }

        Ok(())
    }

    /// How long to wait for another job before compiling what is in hand.
    ///
    /// A fraction of what the last check cost, capped — so the wait is worth having where a check is expensive and vanishes where it is not, and the first check of a session, with nothing to weigh against, is never delayed at all. The exception is a batch holding nothing but a warming check: that one is worth the whole cap, because it exists to use an idle analyst and a document arriving is precisely the reason to drop it.
    ///
    /// Recomputed as the batch grows rather than taken once, since a warming batch stops being one the moment a document joins it.
    fn settle(&self, dirty: &BTreeMap<PathBuf, Raised>) -> Duration {
        if dirty.values().all(|raised| *raised == Raised::Warming) {
            return SETTLE;
        }

        self.spent
            .map_or(Duration::ZERO, |spent| (spent / SETTLE_SHARE).min(SETTLE))
    }

    /// Whether the last check already answered about `document` from the overlay now in hand — which is only ever true when that overlay is the one this check was handed, since a move replaces the set.
    fn answered(&self, document: &Path) -> bool {
        self.checked
            .as_ref()
            .is_some_and(|(_, answered)| answered.contains(document))
    }

    /// Check `document` from `overlay`, and publish what it reported.
    fn check(&mut self, document: &Path, raised: Raised, overlay: &Overlay) -> Result<(), String> {
        // The document's own directory stands in for a working directory, which a file's placement never reads — so a server started somewhere since deleted still answers. A warming job names the workspace root rather than a file, and asks about the package governing it, which is the same question with no target.
        let (spelling, directory) = match raised {
            Raised::Warming => (Spelling::Nothing, document),
            Raised::Opened | Raised::Edited => (
                Spelling::File(document.to_path_buf()),
                document.parent().unwrap_or(document),
            ),
        };
        let asked = Selection::of(spelling, self.manifest.as_deref(), directory, overlay)
            .and_then(Asked::every);
        let records = match asked {
            Ok(asked) => asked
                .into_iter()
                .flat_map(|asked| asked.diagnostics(self.budget, overlay))
                .collect(),
            // A scope that cannot be assembled is an answer about the document, not a server failure: the manifest is what is wrong, and the document is where the editor is looking.
            Err(message) => vec![Record {
                severity: Severity::Error,
                report: Report::unlocated(message),
            }],
        };

        // A warming check is run for what it leaves on this thread, never for what it says: it names a directory, so every record it holds is about a file no editor has opened, and a manifest it cannot find is not a fault to report to anybody.
        if raised == Raised::Warming {
            return Ok(());
        }

        let mut by_path: BTreeMap<PathBuf, Vec<Diagnostic>> = BTreeMap::new();
        by_path.entry(document.to_path_buf()).or_default();
        for record in records {
            let (path, diagnostic) = adapt(document, &record);
            by_path.entry(path).or_default().push(diagnostic);
        }

        // Clear wherever this document last put something that is not being replaced.
        let previous = self.published.remove(document).unwrap_or_default();
        for stale in previous.difference(&by_path.keys().cloned().collect()) {
            publish(self.sender.as_ref(), stale, Vec::new())?;
        }

        let mut placed = BTreeSet::new();
        for (path, diagnostics) in by_path {
            placed.insert(path.clone());
            publish(self.sender.as_ref(), &path, diagnostics)?;
        }
        self.published.insert(document.to_path_buf(), placed);

        Ok(())
    }
}

/// Record why `document` is dirty, keeping the reason that cannot be skipped: a document opened and then edited within one burst is checked as an open, since the editor is holding nothing for it.
fn raise(dirty: &mut BTreeMap<PathBuf, Raised>, document: PathBuf, raised: Raised) {
    let held = dirty.entry(document).or_insert(raised);
    if raised == Raised::Opened {
        *held = Raised::Opened;
    }
}

/// The directory the editor opened, for the warming check: its first workspace folder.
///
/// **`rootUri` is deliberately not read.** The protocol deprecated it in favour of workspace folders in 3.6, and a client old enough to send only that one predates every editor this language is served in. Reading it anyway would cost a `deprecated` suppression for a client nobody has, and warming is an optimization that degrades into today's behaviour: a session with no folder named here simply pays the first check what it already pays.
fn workspace_root(params: &InitializeParams) -> Option<PathBuf> {
    let folder = params.workspace_folders.as_ref()?.first()?;

    path_of(&folder.uri)
}

/// One record as the protocol's diagnostic, and the path it belongs to — the span's source when it has one, and the checked document itself, at its first position, when it has none.
fn adapt(document: &Path, record: &Record) -> (PathBuf, Diagnostic) {
    let severity = match record.severity {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Goal => DiagnosticSeverity::INFORMATION,
        Severity::Lint => DiagnosticSeverity::WARNING,
        Severity::Note => DiagnosticSeverity::HINT,
    };

    let (path, range) = match &record.report.span {
        Some(span) => (
            span.source
                .path
                .as_deref()
                .map(curios_text::identity)
                .unwrap_or_else(|| document.to_path_buf()),
            range_of(span),
        ),
        None => (document.to_path_buf(), Range::default()),
    };

    (
        path,
        Diagnostic {
            range,
            severity: Some(severity),
            // No `source`: an editor appends it to the message's last line (Zed writes `expected: Str (curios)`, which reads as one clause), it trims a trailing newline that would have moved it, and with one server for the language it identifies nothing.
            message: record.report.message.clone(),
            ..Diagnostic::default()
        },
    )
}

/// The span's byte range as protocol positions: line, and the UTF-16 unit count from the line's start — the one place the protocol's unit is spoken.
fn range_of(span: &Span) -> Range {
    let text = &span.source.text;
    // An empty span still has to be visible, so it covers the character it points at.
    let end = match span.end > span.start {
        true => span.end,
        false => text[span.start..]
            .chars()
            .next()
            .map_or(span.start, |c| span.start + c.len_utf8()),
    };

    Range {
        start: position_of(text, span.start),
        end: position_of(text, end),
    }
}

fn position_of(text: &str, offset: usize) -> Position {
    let offset = offset.min(text.len());
    let line_start = text[..offset].rfind('\n').map_or(0, |index| index + 1);
    let line = text[..line_start].matches('\n').count();
    let character = text[line_start..offset]
        .chars()
        .map(char::len_utf16)
        .sum::<usize>();

    Position {
        line: line as u32,
        character: character as u32,
    }
}

fn publish(
    sender: &dyn Fn(Message) -> Result<(), String>,
    path: &Path,
    diagnostics: Vec<Diagnostic>,
) -> Result<(), String> {
    let Some(uri) = uri_of(path) else {
        return Ok(());
    };
    let params = PublishDiagnosticsParams {
        uri,
        diagnostics,
        version: None,
    };

    sender(Message::Notification(Notification::new(
        PublishDiagnostics::METHOD.to_string(),
        params,
    )))
}

fn reply_unhandled(connection: &Connection, id: RequestId, method: &str) -> Result<(), String> {
    connection
        .sender
        .send(Message::Response(Response::new_err(
            id,
            lsp_server::ErrorCode::MethodNotFound as i32,
            format!("{method} is not served"),
        )))
        .map_err(|error| error.to_string())
}

/// The local path a `file:` URI names, percent-decoded, or `None` for any other scheme — a document this server cannot read is one it does not check.
fn path_of(uri: &Uri) -> Option<PathBuf> {
    let text = uri.as_str();
    let rest = text.strip_prefix("file://")?;
    // `file:///home/...` has an empty authority; `file://host/...` names one this server is not.
    let path = match rest.strip_prefix('/') {
        Some(_) => rest,
        None => return None,
    };

    let mut bytes = Vec::with_capacity(path.len());
    let raw = path.as_bytes();
    let mut index = 0;
    while index < raw.len() {
        match raw[index] {
            b'%' if index + 2 < raw.len() => {
                let hex = &path[index + 1..index + 3];
                match u8::from_str_radix(hex, 16) {
                    Ok(byte) => {
                        bytes.push(byte);
                        index += 3;
                    }
                    Err(_) => {
                        bytes.push(b'%');
                        index += 1;
                    }
                }
            }
            byte => {
                bytes.push(byte);
                index += 1;
            }
        }
    }

    Some(curios_text::identity(Path::new(
        &String::from_utf8_lossy(&bytes).into_owned(),
    )))
}

/// `path` as a `file:` URI, percent-encoding what the scheme reserves.
fn uri_of(path: &Path) -> Option<Uri> {
    let mut encoded = String::from("file://");
    for byte in path.to_string_lossy().bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'.' | b'_' | b'~' | b'/' => {
                encoded.push(byte as char)
            }
            _ => encoded.push_str(&format!("%{byte:02X}")),
        }
    }

    Uri::from_str(&encoded).ok()
}
