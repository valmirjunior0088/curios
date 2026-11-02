//! The run a test serve answers at its root: the runner document, which opens one page per test and reports each to the command, and the wire between the two — a report posted to `/` under the run's token, and the reply that carries what the command wrote for it and the step to open next. Each is one type rendered and read here, so the runner's script, the command and the suites meet on one spelling.

#[cfg(test)]
mod tests;

use {
    crate::{ARGUMENT, json},
    askama::Template,
    base64::{Engine, engine::general_purpose::STANDARD},
    serde::{Deserialize, Serialize},
};

/// The runner's script, which the document carries whole.
const RUNNER: &str = include_str!("runner.mjs");

/// The runner document's title.
const TITLE: &str = "curios test";

/// One test of the plan, by the index of its subject among the subjects with a test selected and the index of the test among the subject's records — the argument its page is opened with.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Step {
    pub subject: usize,
    pub test: usize,
}

/// What a page posted to the runner when its program ended, as the runner reports it: the streams the loader kept, in base64, and the harness's `exitCode`, `trap` and `held` as the page posted them.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Posted {
    pub stdout: String,
    pub stderr: String,
    pub exit_code: Option<u8>,
    pub trap: Option<String>,
    pub held: Option<bool>,
}

impl Posted {
    /// The guest's two streams, decoded.
    pub fn streams(&self) -> Result<(Vec<u8>, Vec<u8>), String> {
        let decoded = |stream: &str, name: &str| {
            STANDARD
                .decode(stream)
                .map_err(|error| format!("the report's {name} is not base64: {error}"))
        };

        Ok((
            decoded(&self.stdout, "stdout")?,
            decoded(&self.stderr, "stderr")?,
        ))
    }
}

/// One test's report, as `POST /` takes it: the run's token, the step, and what its page posted.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Report {
    pub run: String,
    pub subject: usize,
    pub test: usize,
    pub outcome: Posted,
}

impl Report {
    /// The report `body` spells, or why it spells none.
    pub fn parse(body: &[u8]) -> Result<Self, String> {
        serde_json::from_slice(body).map_err(|error| format!("the report is not one: {error}"))
    }

    /// The step the report is of.
    pub fn step(&self) -> Step {
        Step {
            subject: self.subject,
            test: self.test,
        }
    }
}

/// What `POST /` answers a report with: whether the report was the step in hand's and taken, what the command wrote for it on each stream, in base64, and the step to open next — none when the run is complete, or when the report was not taken.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Reply {
    pub taken: bool,
    pub stdout: String,
    pub stderr: String,
    pub next: Option<Step>,
}

impl Reply {
    /// The reply to a report taken: what the command wrote, and `next`.
    pub fn taken(stdout: &[u8], stderr: &[u8], next: Option<Step>) -> Self {
        Self {
            taken: true,
            stdout: STANDARD.encode(stdout),
            stderr: STANDARD.encode(stderr),
            next,
        }
    }

    /// The reply to a report of a step not in hand: nothing written, nothing next.
    pub fn ignored() -> Self {
        Self {
            taken: false,
            stdout: String::new(),
            stderr: String::new(),
            next: None,
        }
    }

    /// The reply as JSON.
    pub fn render(&self) -> Vec<u8> {
        json(self).into_bytes()
    }

    /// What the command wrote on each stream, decoded.
    pub fn streams(&self) -> Result<(Vec<u8>, Vec<u8>), String> {
        let decoded = |stream: &str, name: &str| {
            STANDARD
                .decode(stream)
                .map_err(|error| format!("the reply's {name} is not base64: {error}"))
        };

        Ok((
            decoded(&self.stdout, "stdout")?,
            decoded(&self.stderr, "stderr")?,
        ))
    }

    /// The reply `body` spells, or why it spells none.
    pub fn parse(body: &[u8]) -> Result<Self, String> {
        serde_json::from_slice(body).map_err(|error| format!("the reply is not one: {error}"))
    }
}

/// What the document hands the runner.
#[derive(Serialize)]
struct Run<'a> {
    token: &'a str,
    next: Option<Step>,
    argument: &'static str,
}

#[derive(Template)]
#[template(path = "runner.html")]
struct Document {
    title: &'static str,
    run: String,
    runner: &'static str,
}

/// The runner document for a run under `token` whose first step is `first`.
pub fn runner(token: &str, first: Option<Step>) -> Vec<u8> {
    Document {
        title: TITLE,
        run: json(&Run {
            token,
            next: first,
            argument: ARGUMENT,
        }),
        runner: RUNNER,
    }
    .render()
    .expect("the runner renders")
    .into_bytes()
}
