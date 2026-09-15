//! The records a query answers with: plain data over the compiler's coordinates, deliberately distinct from any stage's own error type so a compiler refactor is never a protocol change by accident.

use {curios_text::Lint, curios_utilities::Report};

/// How a diagnostic classifies. The first two are the compile path's own split — a goal batch exits 2 where a hard error exits 1 — carried per record because a transport renders them apart: a goal is something the author wrote and asked about, not something wrong. A lint is neither: an exact finding the lowering reports and nothing stops on, which `curios lint` alone turns into an exit code. A note is less than a lint: what a question says about how it was asked, which no exit code counts.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    /// The program does not compile: a parse failure, a refused type, a kernel refusal, a dependency that could not be assembled.
    Error,
    /// A written `?`, reported with what elaboration determined there.
    Goal,
    /// An unused import, binder or declaration — see `curios_text::Lint`.
    Lint,
    /// How the question was answered rather than anything wrong with the program: a file a package holds and none of its units declares, checked on its own.
    Note,
}

/// One thing the compiler said about a program, where it said it.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    /// The message and its span — `None` for a refusal about the program as a whole or something outside it, such as a manifest or a store.
    pub report: Report,
}

impl Diagnostic {
    pub fn lint(lint: Lint) -> Self {
        Self {
            severity: Severity::Lint,
            report: lint.report,
        }
    }

    /// The diagnostic as `curios run` would have printed it: the report rendered, message then snippet. A note is the one record `run` never prints, and renders as its message alone after `note: ` — its span is only the start of the file its message already names, which is where an editor places it, and a caret there would read as a refusal of the file's first line.
    pub fn render(&self) -> String {
        match self.severity {
            Severity::Note => format!("note: {}", self.report.message),
            Severity::Error | Severity::Goal | Severity::Lint => self.report.render(),
        }
    }
}

/// One declared test: the path that names, filters and reports it. Deliberately without a rung — a rung is a constructor the body builds at run time, and this record is read from a compilation that executes nothing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclaredTest {
    pub path: String,
}
