//! The one authored table of the repository's steps.
//!
//! **A step is an action on a subject, and everything said of one is said here.** [`Step::row`] is an exhaustive match, so no step exists without a row: the subject and the action a person types to reach it, the steps run first whenever it is asked for, the tools it needs beyond cargo, the paths whose change is its only reason to run, and what it does, worded so that what only it catches is plain. The command line and its help, a bare subject's sequence and what the check workflow must run are read off these rows, so none of them is a second statement of one.
//!
//! **Format and lint follow a tree; build, test and install follow what is built from one.** A tree is a manifest root — the Rust workspace, the two npm packages under `editors/`, the Zed extension's own workspace — and a tracked file belongs to the tree of its nearest one, so a file added anywhere is already some tree's. `compiler`, `launcher`, `bundle`, `docs` and `instrumented` are builds of the workspace's tree, each in the configuration it ships or is measured in.
//!
//! **Day to day, one configuration: the one that ships.** `workspace`'s lint and tests build with default features, so a change is checked as the compiler a user runs and the prelude's images are built once for both. What only every feature compiles — the instrumentation `profile` turns on — is `instrumented`'s to lint, with `docs`, which documents every item there is.
//!
//! **The suite is compiled once.** `workspace test` is the one step that builds the compiler's tests, so a test of the compiler is in that run or in no step: a second run in another configuration, for the tests a feature adds or for one package alone, compiles the whole suite again for a handful of tests. The recorder's tests are the one second run, because theirs compiles none of it: `curios-profile` depends on nothing of the compiler, so its `enabled` feature is a build of that crate alone.
//!
//! **A bare subject is its steps in the table's order, one after another.** The order of the variants is the one order there is: a step that reads what another of its subject writes stands after it, as a test stands after its install and the Binaryen cache check, which deletes a build the compiler links, after every step of the workspace that builds one. No command runs every subject: what runs together is for whoever runs them to decide, and what orders two subjects is said where the subjects are listed, `.claude/commands/run-full-gate.md`.
//!
//! **The table says nothing of a machine.** A step runs wherever the checkout is; which machines run which subject is the check workflow's to say.

use {crate::Tool, clap::ValueEnum};

#[cfg(test)]
mod tests;

/// What a step is about: a tree, or something built from the workspace's.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, ValueEnum)]
pub(crate) enum Subject {
    Workspace,
    Instrumented,
    Compiler,
    Launcher,
    Bundle,
    Docs,
    Grammar,
    Vscode,
    Zed,
}

impl Subject {
    /// What the subject is: the opening of its help line.
    pub(crate) fn is(self) -> &'static str {
        match self {
            Subject::Workspace => "The Rust workspace, as it ships",
            Subject::Instrumented => "The workspace with every feature on",
            Subject::Compiler => "The compiler as it is released",
            Subject::Launcher => "The slim launcher the compiler embeds",
            Subject::Bundle => "The browser bundle",
            Subject::Docs => "The workspace's Rust documentation",
            Subject::Grammar => "The tree-sitter grammar",
            Subject::Vscode => "The VS Code extension",
            Subject::Zed => "The Zed extension",
        }
    }
}

/// What a step does to its subject.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, ValueEnum)]
pub(crate) enum Action {
    Install,
    Format,
    Lint,
    Build,
    Test,
}

/// How the command line and a run's report spell a subject, an action or a step.
pub(crate) fn spelled(value: impl ValueEnum) -> String {
    value
        .to_possible_value()
        .expect("no value of the table is skipped")
        .get_name()
        .to_string()
}

/// A step of the table. The order of the variants is the table's, and the order a subject's steps and an action's are run in.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, ValueEnum)]
pub(crate) enum Step {
    LauncherBuild,
    WorkspaceFormat,
    WorkspaceFormatWeb,
    WorkspaceLint,
    WorkspaceLintWeb,
    WorkspaceTest,
    WorkspaceRecorderTest,
    WorkspaceBinaryenCache,
    InstrumentedLint,
    DocsBuild,
    CompilerBuild,
    BundleBuild,
    BundleTest,
    GrammarInstall,
    GrammarFormat,
    GrammarLint,
    GrammarTest,
    VscodeInstall,
    VscodeFormat,
    VscodeLint,
    VscodeTest,
    VscodeBuild,
    ZedFormat,
    ZedLint,
    ZedBuild,
    ZedTest,
}

/// One step as the table states it.
pub(crate) struct Row {
    pub(crate) subject: Subject,
    pub(crate) action: Action,
    /// The steps run first whenever this one is asked for, because it reads what they file.
    pub(crate) ensures: &'static [Step],
    /// The tools it needs beyond cargo.
    pub(crate) needs: &'static [Tool],
    /// The paths whose change since the base is its only reason to run; `None` runs it always.
    pub(crate) when: Option<&'static [&'static str]>,
    /// What the step does, worded so that what only it catches is plain: its help line.
    pub(crate) does: &'static str,
}

impl Step {
    /// The step's row.
    pub(crate) fn row(self) -> Row {
        let always = |subject, action, ensures, needs, does| Row {
            subject,
            action,
            ensures,
            needs,
            when: None,
            does,
        };

        match self {
            Step::LauncherBuild => always(
                Subject::Launcher,
                Action::Build,
                &[],
                &[],
                "Build the slim launcher in a cargo invocation of its own and file it where the compiler embeds it: the one build of the launcher's crate as it ships",
            ),
            Step::WorkspaceFormat => always(
                Subject::Workspace,
                Action::Format,
                &[],
                &[],
                "Format the workspace's Rust; as a check, alone fails on formatting drift",
            ),
            Step::WorkspaceFormatWeb => always(
                Subject::Workspace,
                Action::Format,
                &[],
                &[Tool::Npm],
                "Format the JavaScript, JSON and CSS the workspace holds; as a check, alone fails on formatting drift in them",
            ),
            Step::WorkspaceLint => always(
                Subject::Workspace,
                Action::Lint,
                &[Step::LauncherBuild],
                &[],
                "Lint every target of the workspace as it ships, which elaborates, erases and certifies the whole standard library; alone refuses a loop over a hash's order",
            ),
            Step::WorkspaceLintWeb => always(
                Subject::Workspace,
                Action::Lint,
                &[],
                &[Tool::Npm],
                "Lint the JavaScript, JSON and CSS the workspace holds: the browser harness, the documentation pages' script and stylesheet",
            ),
            Step::WorkspaceTest => always(
                Subject::Workspace,
                Action::Test,
                &[Step::LauncherBuild],
                &[Tool::Nextest],
                "Run the workspace's tests as it ships, the one compilation of the suite; alone reaches the back end, through the cross-stage corpus, renders the standard library's pages end to end and runs an executable the compiler wrote",
            ),
            Step::WorkspaceRecorderTest => always(
                Subject::Workspace,
                Action::Test,
                &[],
                &[Tool::Nextest],
                "Run the recorder's tests, which `curios-profile` compiles under its `enabled` feature alone: a build of that one crate, which depends on nothing of the compiler",
            ),
            Step::WorkspaceBinaryenCache => Row {
                subject: Subject::Workspace,
                action: Action::Test,
                ensures: &[],
                needs: &[],
                when: Some(&["curios-binaryen/build.rs"]),
                does: "Build Binaryen from its source and hold that a second cargo mode reuses the build, where its build script changed",
            },
            Step::InstrumentedLint => always(
                Subject::Instrumented,
                Action::Lint,
                &[Step::LauncherBuild],
                &[],
                "Lint the workspace with every feature on; alone compiles the instrumentation a feature turns on",
            ),
            Step::DocsBuild => always(
                Subject::Docs,
                Action::Build,
                &[Step::LauncherBuild],
                &[],
                "Build the workspace's Rust documentation, over private items and every feature; alone runs rustdoc, where a broken intra-doc link fails",
            ),
            Step::CompilerBuild => always(
                Subject::Compiler,
                Action::Build,
                &[Step::LauncherBuild],
                &[],
                "Build the compiler as it is released",
            ),
            Step::BundleBuild => always(
                Subject::Bundle,
                Action::Build,
                &[],
                &[],
                "Build the browser bundle for wasm32-unknown-unknown and generate its bindings",
            ),
            Step::BundleTest => always(
                Subject::Bundle,
                Action::Test,
                &[Step::BundleBuild],
                &[Tool::Node],
                "Run a compiled program under a JavaScript engine, against the browser harness's own host",
            ),
            Step::GrammarInstall => always(
                Subject::Grammar,
                Action::Install,
                &[],
                &[Tool::Npm],
                "Install the grammar's tools from its lock file",
            ),
            Step::GrammarFormat => always(
                Subject::Grammar,
                Action::Format,
                &[],
                &[Tool::Npm],
                "Format the grammar's JavaScript and JSON; as a check, alone fails on formatting drift in them",
            ),
            Step::GrammarLint => always(
                Subject::Grammar,
                Action::Lint,
                &[],
                &[Tool::Npm],
                "Lint the grammar's JavaScript and JSON",
            ),
            Step::GrammarTest => always(
                Subject::Grammar,
                Action::Test,
                &[],
                &[Tool::Npm],
                "Regenerate the parser and refuse drift from the committed one, then parse the standard library and the corpus",
            ),
            Step::VscodeInstall => always(
                Subject::Vscode,
                Action::Install,
                &[],
                &[Tool::Npm],
                "Install the VS Code extension's tools from its lock file",
            ),
            Step::VscodeFormat => always(
                Subject::Vscode,
                Action::Format,
                &[],
                &[Tool::Npm],
                "Format the VS Code extension's JavaScript and JSON; as a check, alone fails on formatting drift in them",
            ),
            Step::VscodeLint => always(
                Subject::Vscode,
                Action::Lint,
                &[],
                &[Tool::Npm],
                "Lint the VS Code extension's JavaScript and JSON",
            ),
            Step::VscodeTest => always(
                Subject::Vscode,
                Action::Test,
                &[],
                &[Tool::Npm],
                "Hold the TextMate grammar to its snapshots",
            ),
            Step::VscodeBuild => always(
                Subject::Vscode,
                Action::Build,
                &[],
                &[Tool::Npm],
                "Bundle the VS Code extension and write its package as it ships, which the snapshots never load",
            ),
            Step::ZedFormat => always(
                Subject::Zed,
                Action::Format,
                &[],
                &[],
                "Format the Zed extension; as a check, alone fails on formatting drift in it",
            ),
            Step::ZedLint => always(
                Subject::Zed,
                Action::Lint,
                &[],
                &[],
                "Lint every target of the Zed extension, for the target Zed loads",
            ),
            Step::ZedBuild => always(
                Subject::Zed,
                Action::Build,
                &[],
                &[],
                "Link the Zed extension into the wasm32-wasip2 component Zed loads",
            ),
            Step::ZedTest => always(
                Subject::Zed,
                Action::Test,
                &[],
                &[],
                "Hold the Zed extension to the tree around it: the grammar revision it pins publishes this grammar, and its lints are the workspace's",
            ),
        }
    }

    /// The steps of `action` on `subject`, in the table's order.
    pub(crate) fn of(subject: Subject, action: Action) -> Vec<Step> {
        Step::value_variants()
            .iter()
            .copied()
            .filter(|step| {
                let row = step.row();

                row.subject == subject && row.action == action
            })
            .collect()
    }

    /// Every step of `subject`, in the table's order: what a bare subject runs.
    pub(crate) fn about(subject: Subject) -> Vec<Step> {
        Step::value_variants()
            .iter()
            .copied()
            .filter(|step| step.row().subject == subject)
            .collect()
    }
}
