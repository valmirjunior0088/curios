//! The one-shot transport: ask about what the command line admitted, and render the answer for a reader.
//!
//! **The answer goes to stdout, and nothing else does** ([`answer`]). A query executes no program, so stdout is free to be the answer — which is what lets `curios wonder --stage wasm app > app.wat` mean what it says. Status lines stay on stderr as everywhere else, told to whoever narrates the question ([`Telling`]): that the store took nothing of what it compiled, handed to the caller, which has the terminal to say it on.
//!
//! **Exit 0 means the question was answered, including when the answer is a list of errors.** Non-zero means it could not be asked: no such subject, no such stage, a scope that cannot be assembled. `stage`, `footprint` and `tests` are where the two meet — a program that stops before the answer has not answered the question, so what stopped it goes to stderr, stdout stays empty rather than holding text nothing downstream expected, and the exit is a build's: 2 when written goals alone stopped it, and 1 when anything was refused.

use {
    crate::{
        Answer, Declaration, DeclaredTest, Described, Diagnosed, Diagnosis, Exported, Fact, Origin,
        Refusal, Rendering, Row, Severity, Stands, Subject, Unfooted, Unput, checked, declared_as,
        declared_tests, described, diagnostics, exported, falsified, footprint, named_stage,
        placed, stage, unit_stage,
    },
    curios_common::{
        About, Kind, MAX_LINE_WIDTH, OsError, Qualifier, Report, STDIN_NAME, Said, Source,
        SourceName, Style,
    },
    curios_module::{LoadError, Overlay},
    curios_package::{Entry, Home, Library, Program, Refused, Selection, Spelling},
    curios_pipeline::{CompileError, Progress, Put, Stage},
    curios_surface::{Format, Formatted, WrittenIn},
    curios_verdict::{Session, Verdicts},
    std::{
        collections::BTreeSet,
        fmt::Display,
        fs,
        io::{self, Write},
        mem,
        path::{Path, PathBuf},
        process,
        str::FromStr,
    },
};

/// The code a command ends with where the reader of its answer has gone, which is the one a program whose reader has gone ends with (`/std`'s `print`): what a shell reports of a process a broken pipe ended.
const READER_GONE: i32 = 141;

/// Write `text`, part of an answer, on standard output.
///
/// **A reader that has gone ends the command, with nothing said.** An answer is written for whoever reads it, and `curios wonder --stage wasm | head` is a reader that leaves early: once it has, no line more has anyone to read it, a sentence about the pipe least of all, so the command ends there as a filter does. Any other failure to write is said, and fails the command.
pub fn answer(text: impl Display) {
    let written = write!(io::stdout(), "{text}");

    match written {
        Ok(()) => {}
        Err(error) if error.kind() == io::ErrorKind::BrokenPipe => process::exit(READER_GONE),
        Err(error) => {
            eprintln!(
                "{}",
                Refused::of("unwritable output")
                    .stating("standard output could not be written")
                    .stating(OsError(&error))
                    .about_named("<stdout>")
                    .render()
            );
            process::exit(1);
        }
    }
}

/// Whoever a question tells of its work as it goes, beside the answer it writes on standard output: each subject as it is taken on, each step of the subject's compile, how that compile ended, that an answer is about to be written, and why the store took nothing where it could not. The command line narrates with it; the language server asks in silence (`Silent`).
pub trait Telling {
    /// A subject is about to be compiled: what a report names it, and the manifest that declares it where one does.
    fn asked(&mut self, named: &curios_package::Named, manifest: Option<&Path>);

    /// A step of the compile in hand, beside the store it files into, which says for itself whether it took what it was handed.
    fn step(&mut self, progress: Progress<'_>, store: Option<&Verdicts>);

    /// The compile in hand is over.
    fn ended(&mut self, ended: Ended);

    /// An answer is about to be written on standard output.
    fn answering(&mut self);

    /// Whoever reads the answer on standard output: where they stand and whether they see colour, which is what a diagnostic of the answer is written for.
    fn reader(&self) -> Style;

    /// Whoever reads what is said beside the answer, on standard error.
    fn bystander(&self) -> Style;
}

/// A question asked in silence.
pub struct Silent;

impl Telling for Silent {
    fn asked(&mut self, _: &curios_package::Named, _: Option<&Path>) {}

    fn step(&mut self, _: Progress<'_>, _: Option<&Verdicts>) {}

    fn ended(&mut self, _: Ended) {}

    fn answering(&mut self) {}

    fn reader(&self) -> Style {
        Style::plain()
    }

    fn bystander(&self) -> Style {
        Style::plain()
    }
}

/// How the compile of a question's subject ended, read off what it reported: a lint stops nothing, so a subject that reported only lints ended as one that reported nothing did.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ended {
    /// It reached its end.
    Done,
    /// Written goals alone stopped it, this many.
    Goals(usize),
    /// It was refused.
    Refused,
}

impl Ended {
    /// How a compile that reported `diagnostics` ended.
    pub fn of(diagnostics: &[Diagnosis]) -> Self {
        let count = |severity| {
            diagnostics
                .iter()
                .filter(|diagnostic| diagnostic.severity == severity)
                .count()
        };

        match (count(Severity::Error), count(Severity::Goal)) {
            (0, 0) => Ended::Done,
            (0, goals) => Ended::Goals(goals),
            _ => Ended::Refused,
        }
    }

    /// How a compile that came to `compiled` ended.
    fn of_compile<T>(compiled: &Result<T, CompileError>) -> Self {
        match compiled {
            Ok(_) => Ended::Done,
            Err(CompileError::Incomplete(goals)) => Ended::Goals(goals.len()),
            Err(_) => Ended::Refused,
        }
    }
}

/// One thing a question can be about, resolved: the subject, what a report names it with the manifest that declares it, and the store it reads and files into.
pub struct Asked {
    pub subject: Subject,
    /// What a report names the subject — none for the prelude's scope alone, which has nothing of its own to compile or narrate.
    pub named: Option<curios_package::Named>,
    pub manifest: Option<PathBuf>,
    pub store: Option<Verdicts>,
}

impl Asked {
    /// Every subject `selection` is asked about: one for a program or a library, and for the governing package entire its library and then every program it declares, each a subject of its own.
    pub fn every(selection: Selection) -> Result<Vec<Self>, Box<Report>> {
        Ok(match selection {
            Selection::Program(program) => vec![Self::about_program(program)?],
            Selection::Library(library) => vec![Self::about_library(library)],
            Selection::Entire(entire) => {
                let mut asked = Vec::new();
                if let Some(library) = entire.library()? {
                    asked.push(Self::about_library(library));
                }
                for program in entire.programs()? {
                    asked.push(Self::about_program(program)?);
                }
                asked
            }
        })
    }

    /// A program, asked about as its entry — the one place the transport drains standard input on the engine's behalf.
    fn about_program(program: Program) -> Result<Self, Box<Report>> {
        let origin = match program.entry() {
            Entry::Stdin(written) => Origin::Text {
                text: read_stdin()?,
                written: *written,
            },
            Entry::File(path) => Origin::File(path.clone()),
        };
        let store = program.home().map(|home| {
            Verdicts::at(home.cabinet.clone()).executing(&home.package, &home.executable)
        });
        let declares = program.declares();
        let package = program.home().map(Home::package_root);
        let named = program.named();
        let manifest = program.home().map(|home| home.manifest.clone());

        Ok(Self {
            subject: Subject::Entry {
                units: program.into_units(),
                origin,
                declares,
                package,
            },
            named: Some(named),
            manifest,
            store,
        })
    }

    /// A library, asked about as the unit entire.
    fn about_library(library: Library) -> Self {
        Self {
            named: Some(library.named()),
            manifest: Some(library.manifest),
            subject: Subject::Unit {
                units: library.units,
            },
            store: Some(Verdicts::at(library.cabinet)),
        }
    }

    /// The prelude's scope alone ([`Subject::prelude`]), with no manifest and no store.
    fn about_the_prelude() -> Self {
        Self {
            subject: Subject::prelude(),
            named: None,
            manifest: None,
            store: None,
        }
    }

    /// Compile over what `session` already compiled — see [`Session`]. A question with no store, about a file no package declares, files nothing and keeps what it compiles in the session all the same (`Verdicts::kept_by`): the next keystroke compiles over it.
    pub fn reusing(mut self, session: &Session) -> Self {
        match &mut self.store {
            Some(store) => store.reuse(session.clone()),
            None => self.store = Some(Verdicts::kept_by(session.clone())),
        }

        self
    }

    /// The subject and its store, `telling` told that the subject is taken on.
    fn taken(self, telling: &mut dyn Telling) -> (Subject, Option<Verdicts>) {
        if let Some(named) = &self.named {
            telling.asked(named, self.manifest.as_deref());
        }

        (self.subject, self.store)
    }

    /// Every diagnostic, goal and lint the subject reports, with what it reached, `telling` told of the compile as it goes.
    pub fn diagnosed(self, budget: u64, overlay: &Overlay, telling: &mut dyn Telling) -> Diagnosed {
        let (subject, store) = self.taken(telling);
        let store = store.as_ref();
        let diagnosed = diagnostics(budget, subject, overlay, store, &mut |progress| {
            telling.step(progress, store);
        });
        telling.ended(Ended::of(&diagnosed.diagnostics));

        diagnosed
    }

    /// [`Asked::diagnosed`], with what the same check leaves of `file` ([`Diagnosed::placed`]).
    pub fn checked(
        self,
        budget: u64,
        overlay: &Overlay,
        file: &Path,
        telling: &mut dyn Telling,
    ) -> Diagnosed {
        let (subject, store) = self.taken(telling);
        let store = store.as_ref();
        let diagnosed = checked(budget, subject, overlay, store, file, &mut |progress| {
            telling.step(progress, store);
        });
        telling.ended(Ended::of(&diagnosed.diagnostics));

        diagnosed
    }

    /// Every test the subject declares, in declaration order.
    pub fn tests(
        self,
        budget: u64,
        overlay: &Overlay,
        telling: &mut dyn Telling,
    ) -> Result<Vec<DeclaredTest>, CompileError> {
        let (subject, store) = self.taken(telling);
        let store = store.as_ref();
        let declared = declared_tests(budget, subject, overlay, store, &mut |progress| {
            telling.step(progress, store);
        });
        telling.ended(Ended::of_compile(&declared));

        declared
    }

    /// The subject's rung `name`, handed to `shown` where the driver emits it.
    pub fn staged(
        self,
        budget: u64,
        overlay: &Overlay,
        name: &str,
        shown: impl FnMut(&str),
        telling: &mut dyn Telling,
    ) -> Result<Rendering, Refusal> {
        let (subject, store) = self.taken(telling);
        let cache = store.as_ref();
        let reached = stage(
            budget,
            subject,
            overlay,
            cache,
            name,
            shown,
            &mut |progress| telling.step(progress, cache),
        );
        telling.ended(match &reached {
            Ok(rendering) => Ended::of(&rendering.diagnostics),
            Err(Refusal::Diagnostics(diagnostics)) => Ended::of(diagnostics),
            _ => Ended::Done,
        });

        reached
    }

    /// What `path` names in the subject's scope, and its part at the rung `name` ([`named_stage`]).
    pub fn named_staged(
        self,
        budget: u64,
        overlay: &Overlay,
        path: &Qualifier,
        name: &str,
        telling: &mut dyn Telling,
    ) -> Result<Option<(Described, String)>, Refusal> {
        let (subject, store) = self.taken(telling);
        let cache = store.as_ref();
        let staged = named_stage(
            budget,
            subject,
            path,
            name,
            overlay,
            cache,
            &mut |progress| {
                telling.step(progress, cache);
            },
        );
        telling.ended(match &staged {
            Err(Refusal::Diagnostics(diagnostics)) => Ended::of(diagnostics),
            _ => Ended::Done,
        });

        staged
    }

    /// What `path` names in the program's scope, and what the program holds of each function of its text ([`footprint()`]).
    pub fn footprinted(
        self,
        budget: u64,
        overlay: &Overlay,
        path: &Qualifier,
        telling: &mut dyn Telling,
    ) -> Result<Option<(Described, Vec<Row>)>, Unfooted> {
        let (subject, store) = self.taken(telling);
        let cache = store.as_ref();
        let footprint = footprint(budget, subject, path, overlay, cache, &mut |progress| {
            telling.step(progress, cache);
        });
        telling.ended(match &footprint {
            Err(Unfooted::Diagnostics(diagnostics)) => Ended::of(diagnostics),
            _ => Ended::Done,
        });

        footprint
    }

    /// What the checkers say of the subject's closing term put at `False` ([`falsified`](falsified())), `telling` told of the compile as it goes.
    pub fn falsified(
        self,
        budget: u64,
        overlay: &Overlay,
        telling: &mut dyn Telling,
    ) -> Result<Put, Unput> {
        let (subject, store) = self.taken(telling);
        let cache = store.as_ref();
        let put = falsified(budget, subject, overlay, cache, &mut |progress| {
            telling.step(progress, cache);
        });
        telling.ended(match &put {
            Err(Unput::Diagnostics(diagnostics)) => Ended::of(diagnostics),
            _ => Ended::Done,
        });

        put
    }

    /// What stands at `place` of `file` of the subject ([`placed`]).
    pub fn placed(
        self,
        budget: u64,
        overlay: &Overlay,
        file: &Path,
        place: &Place,
        telling: &mut dyn Telling,
    ) -> Result<Option<Answer>, Vec<Diagnosis>> {
        let (subject, store) = self.taken(telling);
        let cache = store.as_ref();
        let placed = placed(
            budget,
            subject,
            file,
            (place.line, place.column),
            overlay,
            cache,
            &mut |progress| telling.step(progress, cache),
        );
        telling.ended(match &placed {
            Err(diagnostics) => Ended::of(diagnostics),
            Ok(_) => Ended::Done,
        });

        placed
    }

    /// What `path` names in the subject's scope ([`described`]).
    pub fn described(
        self,
        budget: u64,
        overlay: &Overlay,
        path: &Qualifier,
        telling: &mut dyn Telling,
    ) -> Result<Option<Described>, Vec<Diagnosis>> {
        let (subject, store) = self.taken(telling);
        let cache = store.as_ref();
        let described = described(budget, subject, path, overlay, cache, &mut |progress| {
            telling.step(progress, cache);
        });
        telling.ended(match &described {
            Err(diagnostics) => Ended::of(diagnostics),
            Ok(_) => Ended::Done,
        });

        described
    }

    /// What a consumer reaches through the module `path` names ([`exported`]), `telling` told of the compile as it goes.
    pub fn exported(
        self,
        budget: u64,
        overlay: &Overlay,
        path: &Qualifier,
        telling: &mut dyn Telling,
    ) -> Result<Option<Exported>, CompileError> {
        let (subject, store) = self.taken(telling);
        let cache = store.as_ref();
        let exported = exported(budget, subject, path, overlay, cache, &mut |progress| {
            telling.step(progress, cache);
        });
        telling.ended(match &exported {
            Err(_) => Ended::Refused,
            Ok(_) => Ended::Done,
        });

        exported
    }
}

/// `wonder [SUBJECT]`: render every diagnostic of what `selection` selects to stdout, and after them each text of it the formatter would write otherwise, `files` and standard input read within `width`, a blank line between each, `telling` told of each subject's compile as it goes.
pub fn wonder_diagnostics(
    budget: u64,
    selection: Selection,
    files: &[PathBuf],
    format: Format,
    telling: &mut dyn Telling,
) -> Result<(), Box<Report>> {
    let overlay = Overlay::default();

    let answers = Asked::every(selection)?;
    let reader = telling.reader();
    let unformatted = uncanonical(&answers, files, format, &reader);

    let mut reports = rendered(answers, budget, &overlay, telling);
    reports.extend(
        unformatted
            .iter()
            .map(|report| report.rendered(Kind::Format, &reader)),
    );
    if !reports.is_empty() {
        telling.answering();
        answer(format_args!("{}\n", reports.join("\n\n")));
    }
    Ok(())
}

/// `wonder --tests [SUBJECT]`: every test `selection` declares, one path per line, in declaration order — the library's, then each executable's, when it is the governing package entire. Nothing executes, and a package with no tests answers with nothing and exit 0. `telling` is told as [`wonder_diagnostics`]' is.
pub fn wonder_tests(
    budget: u64,
    selection: Selection,
    telling: &mut dyn Telling,
) -> Result<(), CompileError> {
    let overlay = Overlay::default();

    let mut listed = Ok(());
    for asked in Asked::every(selection)? {
        let records = asked.tests(budget, &overlay, telling);
        match records {
            Ok(records) => {
                if !records.is_empty() {
                    telling.answering();
                }
                records
                    .iter()
                    .for_each(|record| answer(format_args!("{}\n", record.path)));
            }
            Err(error) => {
                listed = Err(error);
                break;
            }
        }
    }
    listed
}

/// Each text of a subject the formatter would write otherwise, a report a text: each of `files`, once, and the text a subject of `answers` was read from standard input as. A text the formatter refuses is none of them, since why it refuses is the compilation's to say.
///
/// **One report a text, never one a place**: what would move is the formatter's to show, by writing it, and the way out is one command however much moves.
fn uncanonical(
    answers: &[Asked],
    files: &[PathBuf],
    format: Format,
    reader: &Style,
) -> Vec<Report> {
    let moved = |formatted| matches!(formatted, Ok(Formatted::Changed(_)));
    let report = |about, what: &str, subject: &str| {
        let mut said = Said::line("not canonical");
        said.stating(format_args!("the formatter would rewrite this {what}"));

        Report::about(about, said).trying(format!("curios format {subject}"))
    };
    let mut seen = BTreeSet::new();
    let written = files
        .iter()
        .filter(|path| seen.insert(*path))
        .filter(|path| moved(Formatted::from_path(path, format)))
        .map(|path| report(About::Path(path.clone()), "file", &reader.spelled(path)));
    let read = answers
        .iter()
        .filter_map(|asked| match &asked.subject {
            Subject::Entry {
                origin: Origin::Text { text, written },
                ..
            } => Some((text, *written)),
            _ => None,
        })
        .filter(|(text, written)| {
            moved(Formatted::from_source(
                &Source::named(SourceName::stdin(), text.as_str()),
                *written,
                format,
            ))
        })
        .map(|_| {
            report(
                About::Named(STDIN_NAME.to_string()),
                "text",
                Spelling::STDIN,
            )
        });

    written.chain(read).collect()
}

/// Every answer's diagnostics, rendered, each distinct fact once.
///
/// **The subjects of a whole package overlap, and one fact is still one fact.** Every executable is compiled against the library, so a diagnostic in the library is reached by the library's own subject and again by each executable's — one unbound variable printed three times in a package declaring two programs, which is what an agent's one-error-at-a-time loop then walks through. Collapsing is safe because a rendering carries the source, the line and the column beneath the message: two that compare equal say the same thing about the same place, and two about different places never compare equal.
///
/// The subjects themselves are still compiled apart, which is what keeps this a report about a package rather than about one compilation of it. What that costs — the library folded once per subject, answered from the store when there is one and recompiled when there is not — is the price of the same independence.
pub fn rendered(
    answers: Vec<Asked>,
    budget: u64,
    overlay: &Overlay,
    telling: &mut dyn Telling,
) -> Vec<String> {
    let mut seen = BTreeSet::new();
    let reader = telling.reader();

    answers
        .into_iter()
        .flat_map(|asked| asked.diagnosed(budget, overlay, telling).diagnostics)
        .map(|diagnostic| diagnostic.rendered(&reader))
        .filter(|rendered| seen.insert(rendered.clone()))
        .collect()
}

/// What `wonder --stage` reprints a rung of: a program, at any rung, or a library through one of its files, at the rungs a unit has.
pub enum Staged {
    Program(Program),
    Library(Library),
}

/// What `wonder --name` and its views ask in: a program's scope, a library's, or the prelude's alone, which a question with no subject asks in outside every package.
pub enum Scope {
    Program(Program),
    Library(Library),
    Prelude,
}

impl Staged {
    /// This subject, a program on standard input read as written in `written` (`Program::read_stdin_as`).
    pub fn read_stdin_as(self, written: WrittenIn) -> Self {
        match self {
            Self::Program(program) => Self::Program(program.read_stdin_as(written)),
            library => library,
        }
    }
}

impl Scope {
    /// This scope, a program on standard input read as written in `written` (`Program::read_stdin_as`).
    pub fn read_stdin_as(self, written: WrittenIn) -> Self {
        match self {
            Self::Program(program) => Self::Program(program.read_stdin_as(written)),
            scope => scope,
        }
    }

    /// The question asked in this scope.
    fn asked(self) -> Result<Asked, Box<Report>> {
        Ok(match self {
            Self::Program(program) => Asked::about_program(program)?,
            Self::Library(library) => Asked::about_library(library),
            Self::Prelude => Asked::about_the_prelude(),
        })
    }
}

/// `wonder --stage STAGE [SUBJECT]`: the rung of what `staged` is, reprinted by the driver's own rendering, to stdout. `telling` is told as [`wonder_diagnostics`]' is.
pub fn wonder_stage(
    budget: u64,
    name: &str,
    staged: Staged,
    telling: &mut dyn Telling,
) -> Result<(), CompileError> {
    let overlay = Overlay::default();

    let program = match staged {
        Staged::Program(program) => program,
        Staged::Library(library) => {
            return library_stage(budget, name, library, &overlay, telling);
        }
    };

    let refusal = not_a_program(program.entry());
    let asked = Asked::about_program(program)?;

    // The rung is the answer and goes to stdout, where the driver emits it: once every unit the program stands on is compiled, and before anything downstream of the rung is.
    let shown = |text: &str| answer(format_args!("{text}\n"));
    let reached = asked.staged(budget, &overlay, name, shown, telling);
    match reached {
        Ok(rendering) => {
            // What stopped the compilation after the rung is context and goes to stderr, so a pipeline reading the rendering is unaffected by it.
            let bystander = telling.bystander();
            for diagnostic in &rendering.diagnostics {
                eprintln!("{}", diagnostic.rendered(&bystander));
            }
            Ok(())
        }
        Err(Refusal::NoSuchStage { asked }) => Err(no_such_stage(&asked)),
        Err(Refusal::Unit) => Err(refusal.into()),
        Err(Refusal::Diagnostics(diagnostics)) => Err(stopped(diagnostics)),
        Err(Refusal::Lowered) => Err(no_lowering(name)),
        Err(Refusal::Kept { .. }) => {
            unreachable!("a whole program's rung is asked of no declaration")
        }
    }
}

/// The refusal of a rung a lowering makes, `text` or `core`, asked of Core text, which is lowered by nothing.
fn no_lowering(name: &str) -> CompileError {
    Refused::of("no such rung")
        .stating(format_args!(
            "`{name}` is a lowering's rung, and Core text is lowered by nothing"
        ))
        .stating("its rungs begin at `core-elab`")
        .about_named(name)
        .into()
}

/// `wonder --name PATH --stage STAGE [SUBJECT]`: the declaration `path` names in `scope`, at a rung a unit has, to stdout — the block `--name` answers with, then, a blank line before, its part of the rung, printed as the rung prints its whole (`named_stage`). A path that names nothing in scope is refused, as `core` is of a unit in scope's declaration, with the command that asks it of the file that holds it. `telling` is told as [`wonder_diagnostics`]' is.
pub fn wonder_named_stage(
    budget: u64,
    scope: Scope,
    path: &Qualifier,
    name: &str,
    telling: &mut dyn Telling,
) -> Result<(), CompileError> {
    let reader = telling.reader();
    let refusal = match &scope {
        Scope::Program(program) => Some(not_a_program(program.entry())),
        Scope::Library(_) | Scope::Prelude => None,
    };
    let staged = scope
        .asked()?
        .named_staged(budget, &Overlay::default(), path, name, telling);

    match staged {
        Ok(Some((described, part))) => {
            telling.answering();
            answer(format_args!(
                "{}\n",
                block(&described, None).rendered(Kind::Declaration, &reader)
            ));
            // A rung's printer may close its text on a line of its own or not; the answer closes each part on one.
            let part = part.trim_end_matches('\n');
            if !part.is_empty() {
                answer(format_args!("\n{part}\n"));
            }
            Ok(())
        }
        Ok(None) => Err(unknown_declaration(path)),
        Err(Refusal::NoSuchStage { asked }) => Err(no_such_stage(&asked)),
        Err(Refusal::Diagnostics(diagnostics)) => Err(stopped(diagnostics)),
        Err(Refusal::Lowered) => Err(no_lowering(name)),
        Err(Refusal::Kept { file }) => {
            let refused = Refused::of("not the subject's own")
                .stating("`core` is the subject's own lowering, and a unit in scope keeps none")
                .about_named(path.join());
            Err(match file {
                Some(file) => Box::new(refused.trying(format!(
                    "curios wonder --name {} --stage core {}",
                    path.join(),
                    file.display()
                ))),
                None => refused,
            }
            .into())
        }
        // Whoever admits a scope at a rung past a unit's admits a program alone.
        Err(Refusal::Unit) => Err(refusal
            .expect("a rung past a unit's is asked of a program")
            .into()),
    }
}

/// [`wonder_stage`] for a library: its unit's rung, reprinted, to stdout. The file the library was selected through is the one whose text the `text` rung reprints, and its header where it was selected through none.
fn library_stage(
    budget: u64,
    name: &str,
    library: Library,
    overlay: &Overlay,
    telling: &mut dyn Telling,
) -> Result<(), CompileError> {
    let file = asked_through(&library)?;
    let store = Verdicts::at(library.cabinet.clone());

    telling.asked(&library.named(), Some(&library.manifest));
    let reached = unit_stage(
        budget,
        library.units,
        &file,
        overlay,
        Some(&store),
        name,
        &mut |progress| telling.step(progress, Some(&store)),
    );
    telling.ended(match &reached {
        Err(Refusal::Diagnostics(diagnostics)) => Ended::of(diagnostics),
        _ => Ended::Done,
    });
    if let Ok(rendered) = &reached {
        telling.answering();
        answer(format_args!("{rendered}\n"));
    }
    match reached {
        Ok(_) => Ok(()),
        Err(Refusal::NoSuchStage { asked }) => Err(no_such_stage(&asked)),
        // Whoever admits a library admits it at a rung a unit has, and refuses it at any other in its own words.
        Err(Refusal::Unit) => unreachable!("a library was admitted at a program's rung"),
        Err(Refusal::Diagnostics(diagnostics)) => Err(stopped(diagnostics)),
        Err(Refusal::Lowered) => unreachable!("a library is lowered"),
        Err(Refusal::Kept { .. }) => {
            unreachable!("a whole library's rung is asked of no declaration")
        }
    }
}

/// The file of `library` a question about one file is about: the one it was selected through, or its header, which leads the files a library is written in.
fn asked_through(library: &Library) -> Result<PathBuf, CompileError> {
    match &library.through {
        Some(file) => Ok(file.clone()),
        None => Ok(library
            .declared_files()?
            .into_iter()
            .next()
            .unwrap_or_default()),
    }
}

/// A place as a report closes on one and a question is asked of one: the file, spelled from where the reader stands, or [`STDIN_NAME`] for the text on standard input; then a line and a column, each counted from one, the column in scalar values.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Place {
    pub file: String,
    pub line: usize,
    pub column: usize,
}

impl Place {
    /// The subject the place's file is, as a command line writes one: `-` for standard input.
    pub fn subject(&self) -> &str {
        match self.file == STDIN_NAME {
            true => "-",
            false => &self.file,
        }
    }
}

/// Read from the right, so a file whose path holds a colon reads too.
impl FromStr for Place {
    type Err = String;

    fn from_str(written: &str) -> Result<Self, String> {
        let counted = |number: &str| number.parse::<usize>().ok().filter(|number| *number > 0);
        let mut parts = written.rsplitn(3, ':');
        let (column, line, file) = (parts.next(), parts.next(), parts.next());

        line.zip(column)
            .zip(file.filter(|file| !file.is_empty()))
            .and_then(|((line, column), file)| {
                Some(Place {
                    file: file.to_string(),
                    line: counted(line)?,
                    column: counted(column)?,
                })
            })
            .ok_or_else(|| {
                "a place is a file, a line and a column, each from one, as a report closes on one, as in `app.crs:12:5`".to_string()
            })
    }
}

/// `wonder --at FILE:LINE:COLUMN`: what stands at `place`, `staged` being what its file is admitted as, to stdout ([`placed`]) — for a name, the block a declaration is answered with, opened on the path it is exposed at, then its prose and head as its author wrote them, where its text is held; for a term no name stands at, a block of what is written there, quoted at its place — and nothing where nothing stands there. The block names the command that lists a declaration's witnesses where it has any, asked in the scope of the place's file. `telling` is told as [`wonder_diagnostics`]' is.
pub fn wonder_at(
    budget: u64,
    staged: Staged,
    place: &Place,
    telling: &mut dyn Telling,
) -> Result<(), CompileError> {
    let overlay = Overlay::default();
    let reader = telling.reader();
    let (asked, file) = asked_about(staged)?;

    let placed = asked.placed(budget, &overlay, &file, place, telling);
    let answered = match placed {
        Ok(answered) => {
            telling.answering();
            answered
        }
        Err(diagnostics) => return Err(stopped(diagnostics)),
    };
    match answered {
        None => {}
        Some(Answer {
            stands: Stands::Declaration { described, .. },
            ..
        }) => {
            answer(format_args!(
                "{}\n",
                block(
                    &described,
                    Some(listing(&described.asked, Some(place.subject())))
                )
                .rendered(Kind::Declaration, &reader)
            ));
            if let Some(heading) = described
                .declaration
                .as_ref()
                .and_then(Declaration::heading)
            {
                answer("\n");
                for line in &heading.prose {
                    match line.is_empty() {
                        true => answer("---\n"),
                        false => answer(format_args!("--- {line}\n")),
                    }
                }
                answer(format_args!("{}\n", heading.head));
            }
        }
        Some(Answer {
            stands: Stands::Module { path, place: at },
            ..
        }) => {
            let about = match at {
                Some(at) => About::Place(at),
                None => About::Named(path.clone()),
            };
            let report = Report::about(about, Said::line(&path))
                .stating("module")
                .trying(exports_listing(&path, Some(place.subject())));
            answer(format_args!("{}\n", report.rendered(Kind::Module, &reader)));
        }
        Some(answered) => {
            let (about, said, kind) = match &answered.stands {
                Stands::Local { name, bound, .. } => (
                    bound.clone().unwrap_or_else(|| answered.range.clone()),
                    name.clone(),
                    Kind::Local,
                ),
                Stands::Field { label, bound, .. } => (
                    bound.clone().unwrap_or_else(|| answered.range.clone()),
                    label.clone(),
                    Kind::Field,
                ),
                Stands::Term { written, .. } => {
                    (answered.range.clone(), opening(written), Kind::Term)
                }
                Stands::Declaration { .. } | Stands::Module { .. } => {
                    unreachable!("a declaration and a module are answered above")
                }
            };
            let report = answered.facts().into_iter().fold(
                Report::about(About::Place(about), Said::line(said)),
                |report, fact| report.stating(stated(fact)),
            );
            answer(format_args!("{}\n", report.rendered(kind, &reader)));
        }
    }

    Ok(())
}

/// `wonder --name PATH [SUBJECT]`: the declaration `path` names in `scope`, to stdout — a block, then its whole text as its author wrote it, where its unit read one. The block names the command that lists its witnesses where it has any, `subject` as the command line wrote it. A path that names nothing in scope is refused. `telling` is told as [`wonder_diagnostics`]' is.
pub fn wonder_name(
    budget: u64,
    scope: Scope,
    path: &Qualifier,
    subject: Option<&str>,
    telling: &mut dyn Telling,
) -> Result<(), CompileError> {
    let reader = telling.reader();
    let described = declaration(budget, scope, path, telling)?;
    answer(format_args!(
        "{}\n",
        block(&described, Some(listing(&path.join(), subject)))
            .rendered(Kind::Declaration, &reader)
    ));
    if let Some(declaration) = &described.declaration {
        answer(format_args!("\n{}\n", declaration.written()));
    }

    Ok(())
}

/// `wonder --name PATH --witnesses [SUBJECT]`: the witnesses in scope the declaration `path` names takes part in, to stdout — the block `--name` answers with, then a row for each, its place, a tab, then what it says of the declaration. A path that names nothing in scope is refused. `telling` is told as [`wonder_diagnostics`]' is.
pub fn wonder_witnesses(
    budget: u64,
    scope: Scope,
    path: &Qualifier,
    telling: &mut dyn Telling,
) -> Result<(), CompileError> {
    let reader = telling.reader();
    let described = declaration(budget, scope, path, telling)?;

    answer(format_args!(
        "{}\n",
        block(&described, None).rendered(Kind::Declaration, &reader)
    ));
    let witnesses = described.witnesses.iter().flatten().collect::<Vec<_>>();
    if !witnesses.is_empty() {
        answer("\n");
    }
    for witnessed in witnesses {
        let place = match &witnessed.place {
            Some(place) => place.place(&reader),
            None => witnessed.module.clone(),
        };

        answer(format_args!("{place}\t{}\n", witnessed.keyed));
    }

    Ok(())
}

/// `wonder --name PATH --exports [SUBJECT]`: what a consumer reaches through the module `path` names, to stdout — a block naming the module, then a row for each name it exposes, its own declarations and each a `pub use` exposes in the order it writes them, the name, what it is declared as in `--name`'s words and its head on one line, then a row for each public child module, with the command that lists its own. Its names and heads are read off the documentation record ([`curios_document::ModuleDocumentation`]), so a page and the listing list one set, and what each is declared as off the lowering, where `--name` reads it (`Exported`). A path that names no module in scope is refused. `telling` is told as [`wonder_diagnostics`]' is.
pub fn wonder_exports(
    budget: u64,
    scope: Scope,
    path: &Qualifier,
    subject: Option<&str>,
    telling: &mut dyn Telling,
) -> Result<(), CompileError> {
    let reader = telling.reader();
    let Some(Exported { module, declared }) =
        scope
            .asked()?
            .exported(budget, &Overlay::default(), path, telling)?
    else {
        return Err(Refused::of("unknown module")
            .stating("nothing in scope is a module at this path")
            .about_named(path.join())
            .into());
    };
    telling.answering();

    let report =
        Report::about(About::Named(path.join()), Said::line(path.join())).stating(format!(
            "module, exposing {} and {}",
            counted(module.declarations.len(), "name"),
            counted(module.children.len(), "module")
        ));
    answer(format_args!("{}\n", report.rendered(Kind::Module, &reader)));
    if !module.declarations.is_empty() || !module.children.is_empty() {
        answer("\n");
    }
    for (declaration, declared) in module.declarations.iter().zip(declared) {
        let head = declaration
            .signature
            .text
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ");
        answer(format_args!(
            "{}\t{}\t{head}\n",
            declaration.name,
            declared_as(declared)
        ));
    }
    for child in &module.children {
        answer(format_args!(
            "{}\tmodule\t{}\n",
            child.join(),
            exports_listing(&child.join(), subject)
        ));
    }

    Ok(())
}

/// `count` of what `noun` names, the noun plural past one.
fn counted(count: usize, noun: &str) -> String {
    match count {
        1 => format!("1 {noun}"),
        count => format!("{count} {noun}s"),
    }
}

/// The command that lists what the module at `path` exposes, asked in the scope of `subject` as the command line wrote it.
fn exports_listing(path: &str, subject: Option<&str>) -> String {
    match subject {
        Some(subject) => format!("curios wonder --name {path} --exports {subject}"),
        None => format!("curios wonder --name {path} --exports"),
    }
}

/// The command that lists the witnesses of the declaration at `path`, asked in the scope of `subject` as the command line wrote it.
fn listing(path: &str, subject: Option<&str>) -> String {
    match subject {
        Some(subject) => format!("curios wonder --name {path} --witnesses {subject}"),
        None => format!("curios wonder --name {path} --witnesses"),
    }
}

/// The declaration `path` names in `scope` ([`described`]), an answer about to be written; refused where the path names nothing in scope, and where the subject stops before the answer, as a build stops.
fn declaration(
    budget: u64,
    scope: Scope,
    path: &Qualifier,
    telling: &mut dyn Telling,
) -> Result<Described, CompileError> {
    let overlay = Overlay::default();

    let described = scope.asked()?.described(budget, &overlay, path, telling);
    match described {
        Ok(Some(described)) => {
            telling.answering();
            Ok(described)
        }
        Ok(None) => Err(unknown_declaration(path)),
        Err(diagnostics) => Err(stopped(diagnostics)),
    }
}

/// The refusal of a path that names nothing in scope.
fn unknown_declaration(path: &Qualifier) -> CompileError {
    Refused::of("unknown declaration")
        .stating("nothing in scope is declared or exported at this path")
        .about_named(path.join())
        .into()
}

/// What `wonder --name`, its `--witnesses` view and `wonder --at` say of `described` first, a block: the path it is answered by, then each fact stated of it ([`Described::facts`]), a fact of several values laid over as many lines as keep each within a block's width, then `listing`, the command that lists its witnesses, where there are any to list, closed on where its name is written, or on the path it is declared at where no file holds it.
fn block(described: &Described, listing: Option<String>) -> Report {
    let about = match described.declaration.as_ref().and_then(Declaration::place) {
        Some(place) => About::Place(place.clone()),
        None => About::Named(described.path.clone()),
    };
    let mut report = Report::about(about, Said::line(&described.asked));

    for fact in described.facts() {
        report = report.stating(stated(fact));
    }
    if let Some(listing) = listing.filter(|_| {
        described
            .witnesses
            .as_ref()
            .is_some_and(|witnesses| !witnesses.is_empty())
    }) {
        report = report.trying(listing);
    }

    report
}

/// A fact as a block states it: its lead, then its values, several laid over as many lines as keep each within a block's width.
fn stated(Fact { lead, values }: Fact) -> String {
    match values.as_slice() {
        [] => lead,
        [value] => format!("{lead} {value}"),
        _ => listed(&lead, &values),
    }
}

/// What a block about a term opens on: its first line as written, and an ellipsis where more follow.
fn opening(written: &str) -> String {
    let mut lines = written.lines();
    let first = lines.next().unwrap_or_default().trim_end();

    match lines.next() {
        Some(_) => format!("{first} …"),
        None => first.to_string(),
    }
}

/// `items` after `lead`, a comma between each, run over as many lines as keep each within what a fact's line holds beside the trunk, each line after the first indented under the first.
fn listed(lead: &str, items: &[String]) -> String {
    let width = MAX_LINE_WIDTH - "│  ".chars().count();
    let mut lines = Vec::new();
    let mut line = lead.to_string();
    for (position, item) in items.iter().enumerate() {
        let item = match position + 1 < items.len() {
            true => format!("{item},"),
            false => item.clone(),
        };
        if !line.trim().is_empty() && line.chars().count() + 1 + item.chars().count() > width {
            lines.push(mem::replace(&mut line, " ".to_string()));
        }
        line.push(' ');
        line.push_str(&item);
    }
    lines.push(line);

    lines.join("\n")
}

/// The question `staged` is asked as, beside the file a question about one file of it is about: the one it was selected through, a program's own entry, or a library's header.
fn asked_about(staged: Staged) -> Result<(Asked, PathBuf), CompileError> {
    match staged {
        Staged::Program(program) => {
            let file = match (program.through(), program.entry()) {
                (Some(module), _) => module.to_path_buf(),
                (None, Entry::File(entry)) => entry.clone(),
                (None, Entry::Stdin(_)) => PathBuf::from(STDIN_NAME),
            };

            Ok((Asked::about_program(program)?, file))
        }
        Staged::Library(library) => {
            let file = asked_through(&library)?;

            Ok((Asked::about_library(library), file))
        }
    }
}

/// What asking for a rung the pipeline has none of is refused with.
fn no_such_stage(asked: &str) -> CompileError {
    Refused::of("unknown stage")
        .stating(format_args!("no stage is named `{asked}`"))
        .stating(format_args!(
            "the stages: {}",
            Stage::NAMES.map(|name| format!("`{name}`")).join(", ")
        ))
        .about_named("--stage")
        .into()
}

/// `wonder --name PATH --footprint [SUBJECT]`: what the program holds of each function of the text of the declaration `path` names, to stdout — the block `--name` answers with, then a row for each function in the order the declaration writes them, each heap twin under its function: its hint (`-` for none), what became of it, the bytes its bodies weigh in the emitted module and the values they rebuild in place, `-` for a function with no body of its own ([`Row`]). Four tab-separated fields a row, so `awk -F'\t' '$2 == "absorbed"'` is a whole question and a diff of two runs a diff of two files. A path that names nothing in scope is refused, as a subject written as a module is, which has no program. `telling` is told as [`wonder_diagnostics`]' is.
pub fn wonder_footprint(
    budget: u64,
    program: Program,
    path: &Qualifier,
    telling: &mut dyn Telling,
) -> Result<(), CompileError> {
    let reader = telling.reader();
    let refusal = not_a_program(program.entry());
    let asked = Asked::about_program(program)?;

    match asked.footprinted(budget, &Overlay::default(), path, telling) {
        Ok(Some((described, rows))) => {
            telling.answering();
            answer(format_args!(
                "{}\n\n",
                block(&described, None).rendered(Kind::Declaration, &reader)
            ));
            let field = |field: Option<usize>| field.map_or("-".to_string(), |n| n.to_string());
            for row in rows {
                answer(format_args!(
                    "{}\t{}\t{}\t{}\n",
                    row.label.as_deref().unwrap_or("-"),
                    row.fate,
                    field(row.bytes),
                    field(row.rebuilds)
                ));
            }
            Ok(())
        }
        Ok(None) => Err(unknown_declaration(path)),
        Err(Unfooted::Unit) => Err(refusal.into()),
        Err(Unfooted::Diagnostics(diagnostics)) => Err(stopped(diagnostics)),
    }
}

/// `wonder --false [SUBJECT]`: whether the checkers admit the program's closing term at `False`, to stdout — the verdict as the first word, `ADMITTED` or `REFUSED` and by whom, what each checker the subject meets made of it beneath, then every refusal as its checker reports it. Exit 0 once the question is answered, whichever way it went; a subject that does not lower or read was put to no checker, and stops the question as it stops a build.
pub fn wonder_false(
    budget: u64,
    program: Program,
    telling: &mut dyn Telling,
) -> Result<(), CompileError> {
    let refusal = not_a_program(program.entry());
    let asked = Asked::about_program(program)?;

    match asked.falsified(budget, &Overlay::default(), telling) {
        Ok(put) => {
            telling.answering();
            answer(put);
            Ok(())
        }
        Err(Unput::Unit) => Err(refusal.into()),
        Err(Unput::Diagnostics(diagnostics)) => Err(stopped(diagnostics)),
    }
}

/// What stopped a question before it could answer, classified as the compile path classifies a build's failure — so goals alone are the incomplete state that exits 2, and anything refused beside them the failure that exits 1. The refusals come first, which is how a mixed failure says which reports are which; it renders as the reports did, a blank line between each.
fn stopped(diagnostics: Vec<Diagnosis>) -> CompileError {
    let (refused, goals): (Vec<_>, Vec<_>) = diagnostics
        .into_iter()
        .partition(|diagnostic| diagnostic.severity != Severity::Goal);
    let failures = refused.len();
    let reports = refused
        .into_iter()
        .chain(goals)
        .map(|diagnostic| diagnostic.report)
        .collect::<Vec<_>>();

    match failures {
        0 => CompileError::Incomplete(reports),
        failures if failures == reports.len() => CompileError::Failure(reports),
        failures => CompileError::Mixed { reports, failures },
    }
}

/// A file the question can be about: one the disk holds. A path that cannot be read is "no such subject" — the question could not be asked, and the exit says so — refused by the command line before placement reads it, in the words the read would have failed with. The engine would otherwise answer it as one diagnostic and exit 0. The server never comes through here: a document an editor holds may not be on disk yet, which is why the check is the command line's and not `Asked`'s.
pub fn file_subject(path: PathBuf) -> Result<PathBuf, Box<Report>> {
    let readable = fs::metadata(&path).and_then(|metadata| match metadata.is_dir() {
        true => Err(io::Error::from(io::ErrorKind::IsADirectory)),
        false => Ok(()),
    });

    match readable {
        Ok(()) => Ok(path),
        Err(error) => Err(Box::new(LoadError::Read { path, error }.report())),
    }
}

/// What a command that needs a program says of `entry` written as a module: it has no final term, so there is no program to compile.
pub fn not_a_program(entry: &Entry) -> Box<Report> {
    let refused = |text: &str| {
        Refused::of("not a program")
            .stating(format_args!("this {text} is written as a module"))
            .stating("it has no final term to compile a program from")
    };

    match entry {
        Entry::File(path) => refused("file").about_path(path),
        Entry::Stdin(_) => refused("text").about_named(STDIN_NAME),
    }
}

/// Standard input, drained to end.
fn read_stdin() -> Result<String, Box<Report>> {
    io::read_to_string(io::stdin()).map_err(|error| {
        Refused::of("unreadable input")
            .stating("standard input could not be read")
            .stating(OsError(&error))
            .about_named(STDIN_NAME)
    })
}
