//! The command-line surface: clap reads compiler options and a verbatim `run` tail, then `Cli` separates that tail into its subject and program arguments before dispatch or admission can read it. A subject's help comes from its command's contract.

#[cfg(test)]
mod tests;

use {
    crate::{
        CHECK, CLASSES, COMPILE, DOCUMENT, Drawing, FALSE, FOOTPRINT, FORMAT, NAME, PORT, RUN,
        STAGE, TEST, TESTS, WITNESSES, WONDER,
    },
    clap::{
        Arg, ArgAction, ArgGroup, ArgMatches, Args, Command, CommandFactory, Error, FromArgMatches,
        Id, Parser, Subcommand, ValueEnum,
        builder::{PossibleValue, PossibleValuesParser},
        error::{ContextKind, ContextValue, ErrorKind},
        value_parser,
    },
    curios_common::{Code, Qualifier, Report, nearest},
    curios_elab::DEFAULT_BUDGET,
    curios_package::{FormatSettings, Refused, Target},
    curios_pipeline::Stage,
    curios_wonder::Place,
    std::{env, error::Error as _, ffi::OsString, mem, path::PathBuf, sync::LazyLock},
};

/// A target as `--target` takes it: the manifest's own spelling of each, read off [`Target::ALL`] and [`Target::spelled`], so the flag and the `[foreign.<handle>.<target>]` tables cannot spell one target two ways.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TargetValue(pub Target);

impl ValueEnum for TargetValue {
    fn value_variants<'a>() -> &'a [Self] {
        static VARIANTS: LazyLock<Vec<TargetValue>> =
            LazyLock::new(|| Target::ALL.into_iter().map(TargetValue).collect());

        &VARIANTS
    }

    fn to_possible_value(&self) -> Option<PossibleValue> {
        Some(PossibleValue::new(self.0.spelled()))
    }
}

/// A drawing as `--progress` takes it: its one spelling, read off [`Drawing::ALL`] and [`Drawing::spelled`], so the flag and `CURIOS_PROGRESS` cannot spell one drawing two ways.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DrawingValue(pub Drawing);

impl ValueEnum for DrawingValue {
    fn value_variants<'a>() -> &'a [Self] {
        static VARIANTS: LazyLock<Vec<DrawingValue>> =
            LazyLock::new(|| Drawing::ALL.into_iter().map(DrawingValue).collect());

        &VARIANTS
    }

    fn to_possible_value(&self) -> Option<PossibleValue> {
        Some(PossibleValue::new(self.0.spelled()))
    }
}

/// When what a command reports is painted, as `--color` takes it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum Color {
    /// Where standard error is a terminal and `NO_COLOR` is not set.
    Auto,
    Always,
    Never,
}

/// [`curios_pipeline::Stage::NAMES`], each quoted and joined with `, `, computed once on first use — `wonder --stage`'s help text.
static NAMES: LazyLock<String> = LazyLock::new(|| {
    Stage::NAMES
        .iter()
        .map(|name| Code(name).to_string())
        .collect::<Vec<_>>()
        .join(", ")
});

/// Which manifest governs, for a command that resolves against a package.
#[derive(Debug, Args)]
pub struct ManifestFlag {
    /// The explicit override for scripting. It reaches only the governing package's manifest, never the umbrella question — see `documentation/usage.md`'s Which manifest governs.
    #[arg(
        long = "manifest",
        value_name = "PATH",
        help = "Use this `curios.toml` as the governing package's, instead of the nearest one"
    )]
    pub manifest: Option<PathBuf>,
}

/// What a command that elaborates reads: the work a declaration may spend, and which manifest governs.
#[derive(Debug, Args)]
pub struct Elaboration {
    #[arg(
        long,
        default_value_t = DEFAULT_BUDGET,
        hide_default_value = true,
        value_name = "UNITS",
        help = format!("Units of reduction work each declaration may spend while type checking (default: {})", Code(DEFAULT_BUDGET))
    )]
    pub budget: u64,

    #[command(flatten)]
    pub manifest: ManifestFlag,
}

#[derive(Debug, Subcommand)]
pub enum Mode {
    /// What the four forms mean is `documentation/usage.md`'s Running and compiling. The dispatch is lexical and probes no disk: the four spaces cannot overlap, so nothing here needs to look before deciding.
    #[command(
        about = "Execute an executable, a `.crs` file, or standard input, or serve its page to a browser",
        override_usage = "curios run [OPTIONS] [SUBJECT] [ARGS]..."
    )]
    Run {
        // Filled only by `Cli`: clap must stop recognizing options at the first word of the tail, including the subject itself.
        #[arg(skip)]
        subject: Option<String>,

        #[arg(
            long,
            value_enum,
            value_name = "TARGET",
            default_value_t = TargetValue(Target::Native),
            hide_default_value = true,
            hide_possible_values = true,
            help = "What the program runs on: `native` runs it here, `browser` serves its page on loopback until stopped (default: `native`)"
        )]
        target: TargetValue,

        #[arg(
            long,
            value_name = "PORT",
            help = format!("The loopback port a page is served on, beside `--target browser` (default: {})", Code(PORT))
        )]
        port: Option<u16>,

        #[command(flatten)]
        elaboration: Elaboration,

        #[arg(
            trailing_var_arg = true,
            value_names = ["SUBJECT", "ARGS"],
            help = format!("{}. Following words are program arguments (read via `/std/Proc/args`); begin with `--` to omit SUBJECT", RUN.subject_help())
        )]
        args: Vec<OsString>,
    },

    /// Dispatched through the same code as `run`, so the two cannot drift apart. A declared executable is filed under the package that declares it, under the target it is built for; a program no package declares — a file no unit declares, or standard input — has nowhere to be filed, and is built where `--output` says.
    #[command(about = "Compile a program to a native executable or a browser page")]
    Compile {
        #[arg(value_name = "SUBJECT", help = COMPILE.subject_help())]
        subject: Option<String>,

        #[arg(
            long,
            value_enum,
            value_name = "TARGET",
            default_value_t = TargetValue(Target::Native),
            hide_default_value = true,
            hide_possible_values = true,
            help = "What the program is built for: `native` writes an executable, `browser` a directory holding a page (default: `native`)"
        )]
        target: TargetValue,

        #[arg(
            short = 'o',
            long = "output",
            value_name = "PATH",
            help = "Write the executable, or the page's directory, to PATH (default: under `.curios/`, beside the governing manifest; required for a program no package declares)"
        )]
        output_path: Option<PathBuf>,

        #[command(flatten)]
        elaboration: Elaboration,
    },

    /// A library is the one thing with an interface, so the subject names one — none for the governing package's, or a file its library declares — and `--std` names the standard library this compiler was built with, which has no package to be compiled from and so no cabinet to file its pages under: the flag takes the directory they go in. Where the pages go is `documentation/usage.md`'s Documenting.
    #[command(about = "Write a library's interface as pages")]
    Document {
        #[arg(value_name = "SUBJECT", help = DOCUMENT.subject_help())]
        subject: Option<String>,

        #[arg(
            long,
            value_name = "DIR",
            conflicts_with_all = ["subject", "output_path"],
            help = "Document the standard library this compiler was built with, writing the pages under DIR"
        )]
        std: Option<PathBuf>,

        #[arg(
            short = 'o',
            long = "output",
            value_name = "DIR",
            help = "Write the pages under DIR (default: under `.curios/`, beside the governing manifest)"
        )]
        output_path: Option<PathBuf>,

        #[command(flatten)]
        elaboration: Elaboration,
    },

    /// The subject is placed as every command places one, and narrows the run to what it selects; the reasoning is `documentation/usage.md`'s Testing.
    #[command(about = "Run the declared tests of a package, a program, a library or a file")]
    Test {
        #[arg(value_name = "SUBJECT", help = TEST.subject_help())]
        subject: Option<String>,

        #[arg(
            long,
            value_enum,
            value_name = "TARGET",
            default_value_t = TargetValue(Target::Native),
            hide_default_value = true,
            hide_possible_values = true,
            help = "What the tests run on: `native` runs them here, `browser` serves their pages on loopback until every selected test has reported (default: `native`)"
        )]
        target: TargetValue,

        #[arg(
            long,
            value_name = "PORT",
            help = format!("The loopback port the pages are served on, beside `--target browser` (default: {})", Code(PORT))
        )]
        port: Option<u16>,

        #[arg(
            long,
            value_name = "PREFIX",
            help = "Run only the tests at or under the path PREFIX, e.g. `/app/Map`; repeated, a test any of them selects runs"
        )]
        filter: Vec<String>,

        #[command(flatten)]
        elaboration: Elaboration,
    },

    /// The cabinet's tool, and with `pin` one of the two commands that reach the network, both through the one fetcher `curios-package`'s `curate` holds. Acceptance is by hash, so what transport delivered the bytes does not matter — which is exactly why fetching can live in one place rather than being a capability the compiler carries.
    #[command(about = "Materialize what the manifests reference")]
    Curate {
        #[command(flatten)]
        manifest: ManifestFlag,
    },

    /// The one command that writes a manifest, and the only thing in this toolchain that derives a pin rather than checking one. What each form takes is `documentation/usage.md`'s Pinning.
    #[command(about = "Write or re-pin a manifest row, deriving its hash from the delivery")]
    Pin {
        #[command(subcommand)]
        row: Pinned,
    },

    /// Last of the machinery rather than first: it writes what everything else reads, so it can only be right once there is something for it to be right about.
    #[command(about = "Start a package in DIR, named after it")]
    New {
        #[arg(
            value_name = "DIR",
            help = "The directory to create; its name is the package's"
        )]
        directory: PathBuf,
    },

    /// The one command that checks: every diagnostic, goal and lint of each subject, and — for the package entire — the dependencies nothing reached, with the exit code turning on them. Each subject is admitted alone, as `format`'s are, and what they report is one report. The reasoning is `documentation/usage.md`'s Linting.
    #[command(
        about = "Report every error, goal and lint of each subject; exit 1 on an error or a lint, 2 on goals alone"
    )]
    Check {
        #[arg(value_name = "SUBJECT", help = CHECK.subject_help())]
        subjects: Vec<String>,

        #[command(flatten)]
        core: CoreFlag,

        #[command(flatten)]
        elaboration: Elaboration,
    },

    /// Each subject is admitted alone, and what they name is rewritten once however many of them name it; the reasoning is `documentation/usage.md`'s Formatting.
    #[command(about = "Format `.crs` and `.crsc` files canonically, in place")]
    Format {
        #[arg(value_name = "SUBJECT", help = FORMAT.subject_help())]
        subjects: Vec<String>,

        #[arg(
            long,
            help = "Write nothing; exit nonzero when any file would change (for CI)"
        )]
        check: bool,

        #[command(flatten)]
        settings: FormatSettings,

        #[command(flatten)]
        core: CoreFlag,

        #[command(flatten)]
        manifest: ManifestFlag,
    },

    /// Questions about a program, answered by the compilation that would build it. The subject is the one argument, read by the rule every command's is, and the query an option, so no word is ever both: an executable may be named as a query is, and a query added later takes no executable's name. A query is always asked, since checking is `check`'s; with `--server`, the questions are asked by an editor over a protocol, of the documents it hands over, so with no subject.
    #[command(about = "Ask what the compiler knows about a program")]
    Wonder {
        #[command(flatten)]
        query: Query,

        #[arg(value_name = "SUBJECT", help = WONDER.subject_help())]
        subject: Option<String>,

        #[command(flatten)]
        core: CoreFlag,

        #[command(flatten)]
        elaboration: Elaboration,
    },

    /// The reader of what `--profile` files, in the build that files it: the two halves of one format ship together, so neither can drift from the other.
    ///
    /// Not the last step of a profiled run, deliberately. A stream is worth reading exactly when the run did not finish — a hang, a kill, a trap — and such a run never reaches its own last step, so folding there could serve every case but the one profiling exists for. It is a separate pass over a file the compiler has already closed.
    ///
    /// PATH is the command's own argument rather than a SUBJECT: the argument resolver reads a path-bearing word as a `.crs` file and would refuse a stream as no program, so this takes its subject as a path the resolver never sees.
    #[command(about = "Summarize a profile stream filed by --profile")]
    Profile {
        #[arg(
            value_name = "PATH",
            help = "The stream to read, as it was spelled to --profile"
        )]
        stream: PathBuf,
    },
}

/// Which table a row is written into, which decides what its name means and so what may be checked about it.
///
/// The name is a positional on both, and mandatory on both, so `--rev` and `--refresh` have a row to name without a flag to carry it. What differs is what the name *is*: a `[foreign]` key is a handle no program refers to, and a `[dependencies]` key is the package's own declared name — which is why only the second is checked against the delivery, and why neither is ever discovered rather than stated.
#[derive(Debug, Subcommand)]
pub enum Pinned {
    // Required, so a row is never written pointing nowhere; multiple, because `--git` and `--rev` are one delivery spelled in two words.
    #[command(
        about = "What a target links for this package's `foreign` declarations: a WebAssembly module natively, a JavaScript gateway in a browser",
        group = ArgGroup::new("delivery").required(true).multiple(true)
    )]
    Foreign {
        #[arg(
            value_name = "NAME",
            help = "The row's name: a handle for this command"
        )]
        name: String,

        #[arg(
            long,
            value_name = "TARGET",
            hide_possible_values = true,
            help = "The target whose table of the row is written: `native` for a WebAssembly module, `browser` for a JavaScript gateway"
        )]
        target: TargetValue,

        #[arg(
            long,
            value_name = "PATH",
            group = "delivery",
            conflicts_with_all = ["url", "refresh"],
            help = "A file this package carries, by its path from the manifest"
        )]
        path: Option<PathBuf>,

        #[arg(
            long,
            value_name = "URL",
            group = "delivery",
            conflicts_with = "refresh",
            help = "A file fetched once and filed in the cabinet"
        )]
        url: Option<String>,

        #[arg(
            long,
            group = "delivery",
            help = "Deliver again whatever this row already names, and pin what arrives"
        )]
        refresh: bool,

        #[command(flatten)]
        check: CheckFlag,

        #[command(flatten)]
        manifest: ManifestFlag,
    },

    #[command(
        about = "A package this one reaches, keyed by the name that package declares",
        group = ArgGroup::new("delivery").required(true).multiple(true)
    )]
    Dependency {
        #[arg(
            value_name = "NAME",
            help = "The row's name, which must be the name the package declares for itself"
        )]
        name: String,

        #[arg(
            long,
            value_name = "PATH",
            group = "delivery",
            conflicts_with_all = ["git", "rev", "refresh"],
            help = "A live sibling on disk, by its path from the manifest"
        )]
        path: Option<PathBuf>,

        #[arg(
            long,
            value_name = "URL",
            group = "delivery",
            requires = "rev",
            conflicts_with = "refresh",
            help = "A repository, pinned to the revision --rev names"
        )]
        git: Option<String>,

        #[arg(
            long,
            value_name = "REV",
            group = "delivery",
            conflicts_with = "refresh",
            help = "The revision to pin; alone, it re-pins this row's existing URL to a new one"
        )]
        rev: Option<String>,

        #[arg(
            long,
            group = "delivery",
            help = "Deliver this row's revision again, and pin the tree that arrives"
        )]
        refresh: bool,

        #[command(flatten)]
        check: CheckFlag,

        #[command(flatten)]
        manifest: ManifestFlag,
    },
}

/// What standard input is written in: a file says it by its name, and standard input, which has none, says it here — as gcc's `-x` names a language, never guessed from the text, where a closing term alone reads in both.
#[derive(Debug, Args)]
pub struct CoreFlag {
    #[arg(
        long,
        help = "Read standard input as Core text, as a `.crsc` file is read"
    )]
    pub core: bool,
}

/// Whether the manifest is written, or only asked what it would say.
#[derive(Debug, Args)]
pub struct CheckFlag {
    /// The delivery still happens under it — what a row *should* say cannot be known without fetching — and only the write is withheld. `format --check` is the precedent for the spelling and for the exit code.
    #[arg(
        long,
        help = "Write nothing; exit nonzero if the manifest would change (for CI)"
    )]
    pub check: bool,
}

/// What `wonder` is asked of its subject: the check where no query is named, and otherwise the one its option names. At most one: the options form one group, so two are refused where the line is parsed, and this is the one value the parse leaves. A subject takes the four forms `run` takes, with the one difference `documentation/usage.md`'s Asking about a program states; the placement itself is `curios_package::Selection`'s.
#[derive(Debug, Clone)]
pub enum Query {
    Tests,
    /// A rung of the pipeline, by its name ([`Stage::NAMES`]).
    Stage(String),
    /// The place a report closed on, whose file is the subject: what stands there, a name with the declaration it names or the term written there, answered off the check of the subject, whose lowering answers whatever the check concludes, so a file that does not check answers as one that does.
    At(Place),
    /// Every span of the file the subject is asked through that the compiler classes, a row a line, answered off the check of the subject as a place is.
    Classes,
    /// A declaration by the absolute path a program outside every module writes it by, answered off the unit that declares it, or, under a view, what the view answers of what the path names.
    Name(Qualifier, Option<View>),
    /// Whether the checkers admit the subject's closing term at `False`: the soundness board's question, asked of a program or a text of Core text by the put the board asks it through.
    False,
    /// Every query, asked by an editor over the language server protocol of the documents it hands over: no subject, which the option refuses beside it where the line is parsed.
    Server,
}

impl Query {
    /// The group the query options form, which holds them to one.
    const GROUP: &str = "query";

    /// The query options, the group's members.
    const QUERIES: [&str; 6] = ["tests", "at", "classes", "name", "false", "server"];

    /// The group the views of a selection form, which holds them to one.
    const VIEWS: &str = "view";

    /// The group of every option that asks a question, `--stage` alone among them, of which a run names one at least: checking is `check`'s, so `wonder` always asks. A view stands in it too, so a view without `--name` is refused for wanting the name rather than a query.
    const ASKED: &str = "asked";

    /// `argument` as a view of the selection `--name` makes: it requires the selection, and conflicts with every other query by name, since clap waives a requirement whose target conflicts with an argument present, which the group makes `--name` do with each of them.
    fn view(argument: Arg) -> Arg {
        Self::beside_name(argument.requires("name"))
    }

    /// `argument` standing beside `--name` and no other query.
    fn beside_name(argument: Arg) -> Arg {
        argument.conflicts_with_all(Self::QUERIES.into_iter().filter(|query| *query != "name"))
    }
}

/// What `--name` is asked beside its path, in place of the declaration's own text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum View {
    /// The declaration's part of a rung of the pipeline, by the rung's name ([`Stage::NAMES`]).
    Stage(String),
    /// What the program holds of each function of the declaration's text, and what each weighs.
    Footprint,
    /// The witnesses in scope the declaration the path names takes part in.
    Witnesses,
}

/// A declaration's path as `--name` takes it: absolute, each segment written.
fn declaration_path(written: &str) -> Result<Qualifier, String> {
    written
        .strip_prefix('/')
        .map(|path| path.split('/').collect::<Vec<_>>())
        .filter(|segments| segments.iter().all(|segment| !segment.is_empty()))
        .map(Qualifier::from)
        .ok_or_else(|| {
            "a declaration is named by its absolute path, as in `/std/Bool/not`".to_string()
        })
}

impl Args for Query {
    fn group_id() -> Option<Id> {
        Some(Id::from(Self::GROUP))
    }

    fn augment_args(command: Command) -> Command {
        let flag = |name: &'static str, about: String| {
            Arg::new(name)
                .long(name)
                .action(ArgAction::SetTrue)
                .help(about)
        };

        command
            .next_help_heading("Queries")
            .arg(flag(
                "tests",
                TESTS.query_help(
                    "Every test the subject declares, one path per line; nothing executes",
                ),
            ))
            // The names are clap's to refuse, so an unknown one is a command line that does not parse; the help already lists them, so the parser's own listing is hidden. Alone it is a query, and beside `--name` a view, so it stands in the views' group, which holds a view to one, and conflicts with every other query.
            .arg(Self::beside_name(
                Arg::new("stage")
                    .long("stage")
                    .value_name("STAGE")
                    .value_parser(PossibleValuesParser::new(Stage::NAMES))
                    .hide_possible_values(true)
                    .help(STAGE.query_help(&format!(
                        "The program's representation at one rung of the pipeline, reprinted, and beside `--name` the declaration's part of a rung a unit has; one of: {}",
                        *NAMES
                    ))),
            ))
            // The place names its file, so a subject beside it would state the file twice.
            .arg(
                Arg::new("at")
                    .long("at")
                    .value_name("FILE:LINE:COLUMN")
                    .value_parser(value_parser!(Place))
                    .conflicts_with("subject")
                    .help("What stands at a place, written as a report closes on one: the declaration a name names, with what it is declared as, its type and where, or the term written there; takes no subject"),
            )
            .arg(flag(
                "classes",
                CLASSES.query_help(
                    "Every span of the file the subject is asked through that the compiler classes, one row each: its place, its length, its class, and `declaration` where what it names is declared there",
                ),
            ))
            .arg(
                Arg::new("name")
                    .long("name")
                    .value_name("PATH")
                    .value_parser(declaration_path)
                    .help(NAME.query_help(
                        "What an absolute path names in the subject's scope: a declaration, by what it is declared as, its type, its members and how many witnesses it takes part in, where, and its text; a module, by what it exposes; both where a path names both",
                    )),
            )
            .arg(flag(
                "false",
                FALSE.query_help(
                    "Whether the checkers admit the closing term at `False`: a block of the verdict, what each checker made of it, and who refused it, then every refusal; exit 0 either way",
                ),
            ))
            // A view stands beside `--name` and in no query's group, which would refuse it there.
            .arg(Self::view(flag(
                "witnesses",
                WITNESSES.query_help(
                    "Beside `--name`, the witnesses in scope the declaration takes part in, one row each: where it is written, and what it is keyed on or the concept it satisfies",
                ),
            )))
            .arg(Self::view(flag(
                "footprint",
                FOOTPRINT.query_help(
                    "Beside `--name`, what the program holds of each function of the declaration's text, one row each: its hint, what became of it, the bytes its bodies weigh in the emitted module and the values they rebuild in place",
                ),
            )))
            .arg(
                flag(
                    "server",
                    "Answer an editor over the language server protocol on standard input and output, of the documents it hands over; takes no subject".to_string(),
                )
                .conflicts_with("subject"),
            )
            .group(
                ArgGroup::new(Self::GROUP)
                    .args(Self::QUERIES)
                    .multiple(false),
            )
            .group(
                ArgGroup::new(Self::VIEWS)
                    .args(["witnesses", "footprint", "stage"])
                    .multiple(false),
            )
            .group(
                ArgGroup::new(Self::ASKED)
                    .args(
                        Self::QUERIES
                            .into_iter()
                            .chain(["stage", "witnesses", "footprint"]),
                    )
                    .multiple(true)
                    .required(true),
            )
            // The heading ends with the queries, so the flags flattened after them stand under the command's own.
            .next_help_heading(None::<&str>)
    }

    fn augment_args_for_update(command: Command) -> Command {
        Self::augment_args(command)
    }
}

impl FromArgMatches for Query {
    fn from_arg_matches(matches: &ArgMatches) -> Result<Self, Error> {
        let stage = matches.get_one::<String>("stage");

        Ok(if matches.get_flag("tests") {
            Query::Tests
        } else if let Some(place) = matches.get_one::<Place>("at") {
            Query::At(place.clone())
        } else if matches.get_flag("classes") {
            Query::Classes
        } else if let Some(path) = matches.get_one::<Qualifier>("name") {
            let view = if matches.get_flag("witnesses") {
                Some(View::Witnesses)
            } else if matches.get_flag("footprint") {
                Some(View::Footprint)
            } else {
                stage.map(|name| View::Stage(name.clone()))
            };

            Query::Name(*path, view)
        } else if let Some(name) = stage {
            Query::Stage(name.clone())
        } else if matches.get_flag("false") {
            Query::False
        } else {
            Query::Server
        })
    }

    fn update_from_arg_matches(&mut self, matches: &ArgMatches) -> Result<(), Error> {
        *self = Self::from_arg_matches(matches)?;

        Ok(())
    }
}

#[derive(Debug, Parser)]
// The stock template omits the version, so `--help` and `--version` answer different questions and a bug report quoting help output says nothing about which build produced it. Naming it here puts it on the one page anybody reads first.
#[command(
    version,
    about = "Compile, run and test Curios programs, manage their packages, and ask the compiler about them",
    help_template = "\
{name} {version}
{about-with-newline}
{usage-heading} {usage}

{all-args}{after-help}"
)]
struct ParsedCli {
    // Inert until asked for: every build carries the instrumentation, and this decides whether anything listens to it. Without that, every invocation would record — every one the integration suite spawns among them, all onto one path.
    //
    // It takes the destination rather than defaulting to one. Global, because what it measures is the invocation rather than one command, so it may stand on either side of the command, before `run` hands its tail to the program.
    //
    // A plain comment rather than documentation, because clap prints a field's documentation of more than one paragraph as the flag's long help, and a global flag's help is printed under every command.
    #[arg(
        long = "profile",
        value_name = "PATH",
        global = true,
        help = "Write one record per span and event to PATH"
    )]
    pub profile: Option<PathBuf>,

    // Global for the reason `--profile` is: it is about the invocation, whichever command it runs.
    #[arg(
        long = "color",
        value_name = "WHEN",
        value_enum,
        default_value_t = Color::Auto,
        hide_default_value = true,
        hide_possible_values = true,
        global = true,
        help = "Paint what the command reports on standard error: `auto` where standard error is a terminal and `NO_COLOR` is not set, `always` or `never` (default: `auto`)"
    )]
    pub color: Color,

    // Global for the reason `--color` is, and decided as colour is: asked here, else named in the environment, else read off where standard error goes.
    #[arg(
        long = "progress",
        value_name = "DRAWING",
        value_enum,
        hide_possible_values = true,
        global = true,
        help = "Show what the command does on standard error as `plain` lines or a `live` tree; unless `CURIOS_PROGRESS` names one, `live` where standard error is a terminal, `TERM` is not `dumb` and `CI` is unset, and `plain` elsewhere"
    )]
    pub progress: Option<DrawingValue>,

    #[command(subcommand)]
    pub mode: Mode,
}

/// A command line whose `run` subject and program arguments have been separated. The private clap input cannot reach dispatch without this normalization.
#[derive(Debug)]
pub struct Cli {
    pub profile: Option<PathBuf>,
    pub color: Color,
    pub progress: Option<DrawingValue>,
    pub mode: Mode,
}

impl CommandFactory for Cli {
    fn command() -> Command {
        ParsedCli::command()
    }

    fn command_for_update() -> Command {
        ParsedCli::command_for_update()
    }
}

/// What a line that does not parse is refused for, as a report: the tag of the refusal, what was written and what would have been taken, read off what clap kept of its own refusal, and the command whose help says what it takes. `words` are the line's, which is where the command it names is read from.
///
/// **Read off the refusal's parts, never its text.** Clap keeps what a refusal is of as data — the argument, the value — and words it for a reader of its own; the parts are what a report is built from, so a release that rewords its message changes nothing here. A near miss is offered by the rule every report offering a name follows, `nearest`, over the names the command takes, rather than by clap's own measure, which offers `format` for `frobnicate`.
pub fn refused(error: &Error, words: &[OsString]) -> Box<Report> {
    // The command the line names, as far down as its words name one: `curios pin dependency`. Built, so each command holds what clap adds to it, `--help` and the global flags among them, which a near miss may have meant.
    let mut root = Cli::command();
    root.build();
    let (named, command) = words.iter().skip(1).filter_map(|word| word.to_str()).fold(
        ("curios".to_string(), &root),
        |(named, command), word| match command.find_subcommand(word) {
            Some(below) => (format!("{named} {word}"), below),
            None => (named, command),
        },
    );
    let texts = |kind| match error.get(kind) {
        Some(ContextValue::String(text)) => vec![text.clone()],
        Some(ContextValue::Strings(texts)) => texts.clone(),
        Some(ContextValue::StyledStr(text)) => vec![text.to_string()],
        _ => Vec::new(),
    };
    let text = |kind| texts(kind).into_iter().next().unwrap_or_default();
    let quoted = |texts: Vec<String>| {
        texts
            .iter()
            .map(|text| format!("`{text}`"))
            .collect::<Vec<_>>()
            .join(", ")
    };
    // An argument as clap names it carries the placeholder of its value, `--output <PATH>`: the flag is its first word.
    let flag = |argument: &str| argument.split(' ').next().unwrap_or_default().to_string();
    // The name the house's rule finds nearest to `written` among `candidates`, the one every report suggesting a name follows, rather than clap's own.
    let suggesting = |refused: Refused, written: &str, candidates: Vec<String>| match nearest(
        written,
        candidates.iter().map(String::as_str),
    ) {
        Some(near) => refused.stating(format_args!("the nearest: {}", Code(near))),
        None => refused,
    };

    let argument = text(ContextKind::InvalidArg);
    let (refused, about) = match error.kind() {
        ErrorKind::InvalidSubcommand => {
            let word = text(ContextKind::InvalidSubcommand);

            (
                suggesting(
                    Refused::of("unknown command")
                        .stating(format_args!("`{word}` is no command of `{named}`")),
                    &word,
                    command
                        .get_subcommands()
                        .map(|below| below.get_name().to_string())
                        .collect(),
                ),
                word,
            )
        }
        ErrorKind::UnknownArgument if argument.starts_with('-') => (
            suggesting(
                Refused::of("unknown flag")
                    .stating(format_args!("`{argument}` is no flag of `{named}`")),
                &argument,
                command
                    .get_arguments()
                    .filter_map(|known| Some(format!("--{}", known.get_long()?)))
                    .collect(),
            ),
            argument,
        ),
        ErrorKind::UnknownArgument => (
            Refused::of("surplus argument")
                .stating(format_args!("`{argument}` is no argument `{named}` takes")),
            argument,
        ),
        ErrorKind::MissingRequiredArgument => (
            Refused::of("missing argument").stating(format_args!(
                "`{named}` takes {}",
                quoted(texts(ContextKind::InvalidArg))
            )),
            named.clone(),
        ),
        ErrorKind::MissingSubcommand => (
            Refused::of("missing argument").stating(format_args!("`{named}` takes a command")),
            named.clone(),
        ),
        ErrorKind::InvalidValue | ErrorKind::ValueValidation => {
            let written = text(ContextKind::InvalidValue);
            let valid = texts(ContextKind::ValidValue);
            let refused = match written.is_empty() {
                true => Refused::of("missing argument")
                    .stating(format_args!("`{}` takes a value", flag(&argument))),
                false => Refused::of("invalid value").stating(format_args!("written: `{written}`")),
            };
            let refused = match valid.is_empty() {
                true => refused,
                false => refused.stating(format_args!(
                    "`{}` takes one of: {}",
                    flag(&argument),
                    quoted(valid)
                )),
            };
            // What a value's own reader says of it, where the value was read by one.
            let refused = match error.source() {
                Some(source) => refused.stating(source),
                None => refused,
            };

            (refused, flag(&argument))
        }
        ErrorKind::ArgumentConflict => (
            Refused::of("conflicting flags").stating(format_args!(
                "`{}` and {} cannot stand together",
                flag(&argument),
                quoted(
                    texts(ContextKind::PriorArg)
                        .iter()
                        .map(|prior| flag(prior))
                        .collect()
                )
            )),
            flag(&argument),
        ),
        kind => (
            Refused::of("invalid command line").stating(
                kind.as_str()
                    .unwrap_or("the line is no command this compiler takes"),
            ),
            named.clone(),
        ),
    };

    refused.trying(format!("{named} --help")).about_named(about)
}

impl Cli {
    /// Read the process's arguments: the command they name, or what clap answered instead — help and the version, which are answers, or a line that does not parse. The caller prints it, and the process exits from `main` alone.
    pub fn parse() -> Result<Self, Error> {
        Self::try_parse_from(env::args_os())
    }

    /// Parse once with clap, preserving the original OS words so a consumed leading separator can be distinguished from a subject.
    pub fn try_parse_from(
        words: impl IntoIterator<Item = impl Into<OsString>>,
    ) -> Result<Self, Error> {
        let words = words.into_iter().map(Into::into).collect::<Vec<_>>();
        let mut parsed = ParsedCli::try_parse_from(&words)?;

        if let Mode::Run { subject, args, .. } = &mut parsed.mode {
            // This argument has no delimiter, defaults or transformations: trailing collection is exactly a suffix, including every option-looking word after its first word. Only clap's leading `--` is consumed before that suffix starts.
            let start = words.len() - args.len();
            debug_assert_eq!(&words[start..], args.as_slice());
            let omitted = words[..start].last().is_some_and(|word| word == "--");
            if !omitted && !args.is_empty() {
                let mut tail = mem::take(args).into_iter().peekable();
                *subject = Some(tail.next().unwrap().into_string().map_err(|_| {
                    let mut command = Self::command();
                    Error::new(ErrorKind::InvalidUtf8)
                        .with_cmd(command.find_subcommand_mut("run").unwrap())
                })?);
                if tail.peek().is_some_and(|word| word == "--") {
                    tail.next();
                }
                *args = tail.collect();
            }
        }

        Ok(Self {
            profile: parsed.profile,
            color: parsed.color,
            progress: parsed.progress,
            mode: parsed.mode,
        })
    }
}
