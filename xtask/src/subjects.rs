//! The command line's subjects, each with the actions it has.
//!
//! **An action a subject lacks cannot be written.** Each subject is a subcommand whose actions are an enum of its own, so `bundle lint` is the parser's own error rather than a refusal written here, and `cargo xtask <subject> --help` is the list of what the subject has. Which steps an action runs is not said here: it is every step of the table whose row names the pair ([`Step::of`](crate::Step::of)), and `every_action_has_a_step_and_every_step_an_action` holds the two sets equal.
//!
//! **A bare subject is its whole sequence**, every step of the table about it, formatting as a check.

use {
    crate::{Action, Request},
    clap::Subcommand,
};

/// What the command line asked of a subject: the action, and what the person narrowed it by.
pub(crate) trait Asks {
    fn asked(self) -> (Action, Request);
}

#[derive(Debug, Subcommand)]
pub(crate) enum WorkspaceAction {
    Format {
        #[arg(long, help = "Write nothing; fail where a file would change")]
        check: bool,
    },
    Lint {
        #[arg(
            value_name = "PACKAGE",
            help = "The package to lint; the whole workspace when omitted"
        )]
        package: Option<String>,
    },
    Test {
        #[arg(
            value_name = "PACKAGE",
            help = "The package whose tests to run; the whole workspace when omitted"
        )]
        package: Option<String>,

        #[arg(
            long,
            short,
            value_name = "FILTER",
            help = "Run only the tests whose path contains this; every test when omitted"
        )]
        filter: Option<String>,
    },
}

impl Asks for WorkspaceAction {
    fn asked(self) -> (Action, Request) {
        match self {
            Self::Format { check } => (Action::Format, Request::formatting(check)),
            Self::Lint { package } => (Action::Lint, Request::narrowed(package, None)),
            Self::Test { package, filter } => (Action::Test, Request::narrowed(package, filter)),
        }
    }
}

#[derive(Debug, Subcommand)]
pub(crate) enum InstrumentedAction {
    Lint,
}

impl Asks for InstrumentedAction {
    fn asked(self) -> (Action, Request) {
        match self {
            Self::Lint => (Action::Lint, Request::whole()),
        }
    }
}

#[derive(Debug, Subcommand)]
pub(crate) enum CompilerAction {
    Build,
}

impl Asks for CompilerAction {
    fn asked(self) -> (Action, Request) {
        match self {
            Self::Build => (Action::Build, Request::whole()),
        }
    }
}

#[derive(Debug, Subcommand)]
pub(crate) enum LauncherAction {
    Build,
}

impl Asks for LauncherAction {
    fn asked(self) -> (Action, Request) {
        match self {
            Self::Build => (Action::Build, Request::whole()),
        }
    }
}

#[derive(Debug, Subcommand)]
pub(crate) enum BundleAction {
    Build,
    Test,
}

impl Asks for BundleAction {
    fn asked(self) -> (Action, Request) {
        match self {
            Self::Build => (Action::Build, Request::whole()),
            Self::Test => (Action::Test, Request::whole()),
        }
    }
}

#[derive(Debug, Subcommand)]
pub(crate) enum DocsAction {
    Build,
}

impl Asks for DocsAction {
    fn asked(self) -> (Action, Request) {
        match self {
            Self::Build => (Action::Build, Request::whole()),
        }
    }
}

#[derive(Debug, Subcommand)]
pub(crate) enum GrammarAction {
    Install,
    Format {
        #[arg(long, help = "Write nothing; fail where a file would change")]
        check: bool,
    },
    Lint,
    Test,
}

impl Asks for GrammarAction {
    fn asked(self) -> (Action, Request) {
        match self {
            Self::Install => (Action::Install, Request::whole()),
            Self::Format { check } => (Action::Format, Request::formatting(check)),
            Self::Lint => (Action::Lint, Request::whole()),
            Self::Test => (Action::Test, Request::whole()),
        }
    }
}

#[derive(Debug, Subcommand)]
pub(crate) enum VscodeAction {
    Install,
    Format {
        #[arg(long, help = "Write nothing; fail where a file would change")]
        check: bool,
    },
    Lint,
    Test,
    Build,
}

impl Asks for VscodeAction {
    fn asked(self) -> (Action, Request) {
        match self {
            Self::Install => (Action::Install, Request::whole()),
            Self::Format { check } => (Action::Format, Request::formatting(check)),
            Self::Lint => (Action::Lint, Request::whole()),
            Self::Test => (Action::Test, Request::whole()),
            Self::Build => (Action::Build, Request::whole()),
        }
    }
}

#[derive(Debug, Subcommand)]
pub(crate) enum ZedAction {
    Format {
        #[arg(long, help = "Write nothing; fail where a file would change")]
        check: bool,
    },
    Lint,
    Build,
    Test,
}

impl Asks for ZedAction {
    fn asked(self) -> (Action, Request) {
        match self {
            Self::Format { check } => (Action::Format, Request::formatting(check)),
            Self::Lint => (Action::Lint, Request::whole()),
            Self::Build => (Action::Build, Request::whole()),
            Self::Test => (Action::Test, Request::whole()),
        }
    }
}
