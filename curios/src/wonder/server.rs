//! The server transport: the same questions, asked by an editor over the language server protocol.
//!
//! **Two threads, and the compiler is on the one that never reads the protocol.** The compiler is single-threaded by construction — `Rc` spans, a thread-local prelude, a `RootSource` that is deliberately not `Send` — so exactly one thread, the *analyst*, owns it; the protocol thread reads messages, keeps the overlay, answers what needs no compilation, and hands the analyst check jobs over a channel. What this buys is that a request is never behind a compile: a formatting request arriving during a two-second check is answered by the protocol thread at once, where one thread doing both would have answered it after the check, past the editor's timeout. This was first written as one thread on the reading that `lsp-server`'s synchronous loop was the whole design, and the timeout is what that reading cost. Beyond two the channel is indifferent — more analysts would be a loop around `thread::spawn`, each with its own prelude — but a second compile of the same document is what coalescing makes unnecessary, and edits to two documents at once are rare enough that the second waits one check.
//!
//! **The editor's documents are the overlay.** Every open document's text is consulted before the disk by every unit the check assembles (`RootSource::with_overlay`), so a diagnostic reflects the buffer rather than the file, and an unsaved new module is still found by the `mod` that declares it. Membership is `curios_package::Membership`'s rule, unchanged from the one-shot query — the library whose directory holds the document, the executable whose entry or stem tree it is, or no unit at all.
//!
//! **Edits coalesce on the analyst.** A job carries the whole overlay as it stood when the edit arrived; before compiling, the analyst drains every job queued behind it and keeps the latest overlay and the union of the documents to check, so a burst of keystrokes during one check costs one more check from the newest text rather than one per keystroke. What is not here is incrementality inside a unit — a front-end pass is an order of magnitude below a compile, and the figure that says it must be bought has not been taken.
//!
//! **Formatting is `curios format` over the overlay, on the protocol thread.** The same `Formatted` the CLI runs, on the text the editor holds, answered as one whole-document edit — the formatter is pure and cheap, parse and print with no prelude, and its output is verified by reparse before it is handed back, so nothing here can hand an editor text the compiler would read differently.
//!
//! **UTF-16 exists only here.** The engine's coordinates are bytes; a `Position` is derived from the span's own text at the boundary, in both directions, and nothing below this file knows the protocol's unit.

use {
    crate::{Asked, Diagnostic as Record, Severity},
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
    },
};

/// Serve until the editor says shutdown.
pub fn serve(budget: u64, mounted: &[PathBuf], manifest: Option<&Path>) -> Result<(), String> {
    let (connection, io) = Connection::stdio();

    let capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        document_formatting_provider: Some(OneOf::Left(true)),
        ..ServerCapabilities::default()
    };
    let initialization = connection
        .initialize(serde_json::to_value(capabilities).expect("capabilities serialize"))
        .map_err(|error| error.to_string())?;
    let _params: InitializeParams =
        serde_json::from_value(initialization).map_err(|error| error.to_string())?;

    let (jobs, inbox) = mpsc::channel();
    let analyst = {
        let mut analyst = Analyst {
            budget,
            mounted: mounted.to_vec(),
            manifest: manifest.map(Path::to_path_buf),
            published: BTreeMap::new(),
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
    io.join().map_err(|error| error.to_string())?;

    served.and(analysed)
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
    /// Every open document's text at the moment of the edit — `Overlay` itself holds an `Rc`, so it is built on the analyst's side.
    documents: BTreeMap<PathBuf, String>,
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
                        let params: DocumentFormattingParams =
                            serde_json::from_value(params).map_err(|error| error.to_string())?;
                        let response = match self.format(&params.text_document.uri) {
                            Ok(edits) => Response::new_ok(id, edits),
                            Err(message) => Response::new_err(
                                id,
                                lsp_server::ErrorCode::RequestFailed as i32,
                                message,
                            ),
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
    fn check(&self, document: PathBuf) -> Result<(), String> {
        self.jobs
            .send(Job {
                document,
                documents: self.documents.clone(),
            })
            .map_err(|_| "the analyst is gone".to_string())
    }

    fn notified(&mut self, notification: Notification) -> Result<(), String> {
        match notification.method.as_str() {
            DidOpenTextDocument::METHOD => {
                let params: DidOpenTextDocumentParams = serde_json::from_value(notification.params)
                    .map_err(|error| error.to_string())?;
                if let Some(path) = path_of(&params.text_document.uri) {
                    self.documents
                        .insert(path.clone(), params.text_document.text);
                    self.check(path)?;
                }
            }
            DidChangeTextDocument::METHOD => {
                let params: DidChangeTextDocumentParams =
                    serde_json::from_value(notification.params)
                        .map_err(|error| error.to_string())?;
                if let Some(path) = path_of(&params.text_document.uri)
                    && let Some(change) = params.content_changes.into_iter().last()
                {
                    self.documents.insert(path.clone(), change.text);
                    self.check(path)?;
                }
            }
            DidSaveTextDocument::METHOD => {
                let params: DidSaveTextDocumentParams = serde_json::from_value(notification.params)
                    .map_err(|error| error.to_string())?;
                if let Some(path) = path_of(&params.text_document.uri) {
                    if let Some(text) = params.text {
                        self.documents.insert(path.clone(), text);
                    }
                    self.check(path)?;
                }
            }
            DidCloseTextDocument::METHOD => {
                let params: DidCloseTextDocumentParams =
                    serde_json::from_value(notification.params)
                        .map_err(|error| error.to_string())?;
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
    mounted: Vec<PathBuf>,
    manifest: Option<PathBuf>,
    /// For each document checked, every path it last published diagnostics to — so a diagnostic that moved or vanished is cleared where it was.
    published: BTreeMap<PathBuf, BTreeSet<PathBuf>>,
    sender: Box<dyn Fn(Message) -> Result<(), String> + Send>,
}

impl Analyst {
    /// Check until the channel closes, which is the protocol thread finishing.
    fn run(&mut self, inbox: &mpsc::Receiver<Job>) -> Result<(), String> {
        while let Ok(first) = inbox.recv() {
            // Coalesce: everything queued behind the first job is newer, so the last overlay wins and every document any of them named is checked once against it.
            let mut documents = first.documents;
            let mut dirty = BTreeSet::from([first.document]);
            while let Ok(job) = inbox.try_recv() {
                documents = job.documents;
                dirty.insert(job.document);
            }

            let overlay = Overlay::of(documents);
            for document in dirty {
                self.check(&document, &overlay)?;
            }
        }

        Ok(())
    }

    /// Check `document` from `overlay`, and publish what it reported.
    fn check(&mut self, document: &Path, overlay: &Overlay) -> Result<(), String> {
        let records = match Asked::about_file(document, &self.mounted, self.manifest.as_deref()) {
            Ok(asked) => asked.diagnostics(self.budget, overlay),
            // A scope that cannot be assembled is an answer about the document, not a server failure: the manifest is what is wrong, and the document is where the editor is looking.
            Err(message) => vec![Record {
                severity: Severity::Error,
                report: Report::unlocated(message),
            }],
        };

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

/// One record as the protocol's diagnostic, and the path it belongs to — the span's source when it has one, and the checked document itself, at its first position, when it has none.
fn adapt(document: &Path, record: &Record) -> (PathBuf, Diagnostic) {
    let severity = match record.severity {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Goal => DiagnosticSeverity::INFORMATION,
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
