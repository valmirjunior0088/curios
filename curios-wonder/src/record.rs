//! The records a query answers with: plain data over the compiler's coordinates, deliberately distinct from any stage's own error type so a compiler refactor is never a protocol change by accident.

use curios_common::{Kind, Report, Style};

/// How a diagnostic classifies. The first two are the compile path's own split — a goal batch exits 2 where a hard error exits 1 — carried per record because a transport renders them apart: a goal is something the author wrote and asked about, not something wrong. A lint is neither: an exact finding the lowering reports and nothing stops on, which `curios lint` alone turns into an exit code. A note is less than a lint: what a question says about how it was asked, which no exit code counts.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    /// The program does not compile: a parse failure, a refused type, a certifier refusal, a dependency that could not be assembled.
    Error,
    /// A written `?`, reported with what elaboration determined there.
    Goal,
    /// An unused import, binder or declaration — see `curios_module::Lint`.
    Lint,
}

impl Severity {
    /// What a report of this severity is, which is the word it closes on.
    pub fn kind(self) -> Kind {
        match self {
            Severity::Error => Kind::Error,
            Severity::Goal => Kind::Goal,
            Severity::Lint => Kind::Lint,
        }
    }
}

/// One thing the compiler said about a program, where it said it.
#[derive(Debug, Clone)]
pub struct Diagnosis {
    pub severity: Severity,
    /// The message and its span — `None` for a refusal about the program as a whole or something outside it, such as a manifest or a store.
    pub report: Report,
}

impl Diagnosis {
    /// A lint, as its unit's texts place it ([`curios_pipeline::Findings::reports`]).
    pub fn lint(report: Report) -> Self {
        Self {
            severity: Severity::Lint,
            report,
        }
    }

    /// The diagnostic as a reader of `style` sees it: its report, a block closed by the word of what it is.
    pub fn rendered(&self, style: &Style) -> String {
        self.report.rendered(self.severity.kind(), style)
    }
}

/// One declared test: the path that names, filters and reports it. Deliberately without a rung — a rung is a constructor the body builds at run time, and this record is read from a compilation that executes nothing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclaredTest {
    pub path: String,
}
