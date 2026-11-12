//! The run a test serve answers at its root: the runner document, which opens one page per test and reports each to the command, and the wire between the two over the socket the runner opens at `/` — each test's outcome as its page posted it, and the reply that carries what the command has written that the socket has not been sent and the step to open next. Each is one type rendered and read here, so the runner's script, the command and the suites meet on one spelling.

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

/// What a page posted to the runner when its program ended, as the runner sends it over the run's socket, the outcome of the step in hand: the streams the loader kept, in base64, and the harness's `exitCode`, `trap` and `held` as the page posted them.
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
    /// The outcome `message` spells, or why it spells none.
    pub fn parse(message: &[u8]) -> Result<Self, String> {
        serde_json::from_slice(message).map_err(|error| format!("the outcome is not one: {error}"))
    }

    /// The guest's two streams, decoded.
    pub fn streams(&self) -> Result<(Vec<u8>, Vec<u8>), String> {
        let decoded = |stream: &str, name: &str| {
            STANDARD
                .decode(stream)
                .map_err(|error| format!("the outcome's {name} is not base64: {error}"))
        };

        Ok((
            decoded(&self.stdout, "stdout")?,
            decoded(&self.stderr, "stderr")?,
        ))
    }
}

/// What the command sends over the run's socket: what it has written of the run that the socket has not been sent, in base64 and in the order written — all of it as the socket opens — and the step to open next, none once the run is complete.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Reply {
    pub written: String,
    pub next: Option<Step>,
}

impl Reply {
    /// The reply carrying `written`, and `next`.
    pub fn new(written: &[u8], next: Option<Step>) -> Self {
        Self {
            written: STANDARD.encode(written),
            next,
        }
    }

    /// The reply as JSON.
    pub fn render(&self) -> Vec<u8> {
        json(self).into_bytes()
    }

    /// What the command wrote, decoded.
    pub fn decoded(&self) -> Result<Vec<u8>, String> {
        STANDARD
            .decode(&self.written)
            .map_err(|error| format!("what the reply carries is not base64: {error}"))
    }

    /// The reply `message` spells, or why it spells none.
    pub fn parse(message: &[u8]) -> Result<Self, String> {
        serde_json::from_slice(message).map_err(|error| format!("the reply is not one: {error}"))
    }
}

/// What the document hands the runner: the key a page spells its arguments under. The steps come over the socket.
#[derive(Serialize)]
struct Run {
    argument: &'static str,
}

#[derive(Template)]
#[template(path = "runner.html")]
struct Document {
    title: &'static str,
    run: String,
    runner: &'static str,
}

/// The runner document.
pub fn runner() -> Vec<u8> {
    Document {
        title: TITLE,
        run: json(&Run { argument: ARGUMENT }),
        runner: RUNNER,
    }
    .render()
    .expect("the runner renders")
    .into_bytes()
}
