//! The one-shot transport: ask about what the command line admitted, and render the answer for a reader.
//!
//! **The answer goes to stdout, and nothing else does** ([`answer`]). A query executes no program, so stdout is free to be the answer — which is what lets `curios wonder --stage wasm app > app.wat` mean what it says. Status lines stay on stderr as everywhere else, told to whoever narrates the question ([`Telling`]): that the store took nothing of what it compiled, handed to the caller, which has the terminal to say it on.
//!
//! **Exit 0 means the question was answered, including when the answer is a list of errors.** Non-zero means it could not be asked: no such subject, no such stage, a scope that cannot be assembled. `stage`, `cost` and `tests` are where the two meet — a program that stops before the answer has not answered the question, so what stopped it goes to stderr, stdout stays empty rather than holding text nothing downstream expected, and the exit is a build's: 2 when written goals alone stopped it, and 1 when anything was refused.

use {
    crate::{
        Declaration, DeclaredTest, Diagnosed, Diagnosis, Named, Origin, Reached, Refusal,
        STDIN_LABEL, Severity, Subject, Uncosted, Written, checked, cost, declared_tests,
        diagnostics, stage, unit_stage, written,
    },
    curios_common::Span,
    curios_core::Declared,
    curios_cps::{Fate, Outcome},
    curios_module::{LoadError, Overlay},
    curios_package::{Entry, Library, Program, Selection},
    curios_pipeline::{CompileError, Progress},
    curios_verdict::{Session, Verdicts},
    std::{
        collections::BTreeSet,
        fmt::Display,
        fs,
        io::{self, Write},
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
            eprintln!("failed to write to standard output: {error}");
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

    /// The store took nothing of what the question compiled, for the reason given.
    fn unfiled(&mut self, refusal: String);
}

/// A question asked in silence.
pub struct Silent;

impl Telling for Silent {
    fn asked(&mut self, _: &curios_package::Named, _: Option<&Path>) {}

    fn step(&mut self, _: Progress<'_>, _: Option<&Verdicts>) {}

    fn ended(&mut self, _: Ended) {}

    fn answering(&mut self) {}

    fn unfiled(&mut self, _: String) {}
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
    pub named: curios_package::Named,
    pub manifest: Option<PathBuf>,
    pub store: Option<Verdicts>,
}

impl Asked {
    /// Every subject `selection` is asked about: one for a program or a library, and for the governing package entire its library and then every program it declares, each a subject of its own.
    pub fn every(selection: Selection) -> Result<Vec<Self>, String> {
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
    fn about_program(program: Program) -> Result<Self, String> {
        let origin = match program.entry() {
            Entry::Stdin => Origin::Text {
                label: STDIN_LABEL.to_string(),
                text: read_stdin()?,
            },
            Entry::File(path) => Origin::File(path.clone()),
        };
        let store = program
            .home()
            .map(|home| Verdicts::at(home.root.clone()).executing(&home.package, &home.executable));
        let declares = program.declares();
        let unlinked = program.unlinked().cloned();
        let named = program.named();
        let manifest = program.home().map(|home| home.manifest.clone());

        Ok(Self {
            subject: Subject::Entry {
                units: program.into_units(),
                origin,
                declares,
                unlinked,
            },
            named,
            manifest,
            store,
        })
    }

    /// A library, asked about as the unit entire.
    fn about_library(library: Library) -> Self {
        Self {
            named: library.named(),
            manifest: Some(library.manifest),
            subject: Subject::Unit {
                units: library.units,
            },
            store: Some(Verdicts::at(library.root)),
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
        telling.asked(&self.named, self.manifest.as_deref());

        (self.subject, self.store)
    }

    /// Every diagnostic, goal and lint the subject reports, with what it reached and why its store took nothing, where it could not, `telling` told of the compile as it goes.
    pub fn diagnosed(self, budget: u64, overlay: &Overlay, telling: &mut dyn Telling) -> Diagnosed {
        let (subject, store) = self.taken(telling);
        let store = store.as_ref();
        let diagnosed = diagnostics(budget, subject, overlay, store, &mut |progress| {
            telling.step(progress, store);
        });
        telling.ended(Ended::of(&diagnosed.diagnostics));

        diagnosed
    }

    /// [`Asked::diagnosed`], with what `file` writes read off the same fold's lowering of the subject ([`Diagnosed::written`]).
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

    /// Every test the subject declares, in declaration order, beside why its store took nothing, where it could not.
    pub fn tests(
        self,
        budget: u64,
        overlay: &Overlay,
        telling: &mut dyn Telling,
    ) -> (Result<Vec<DeclaredTest>, CompileError>, Option<String>) {
        let (subject, store) = self.taken(telling);
        let store = store.as_ref();
        let declared = declared_tests(budget, subject, overlay, store, &mut |progress| {
            telling.step(progress, store);
        });
        telling.ended(Ended::of_compile(&declared));

        (declared, refused(store))
    }

    /// The subject's rung `name`, handed to `shown` where the driver emits it, beside why its store took nothing, where it could not.
    pub fn staged(
        self,
        budget: u64,
        overlay: &Overlay,
        name: &str,
        shown: impl FnMut(&str),
        telling: &mut dyn Telling,
    ) -> (Result<Reached, Refusal>, Option<String>) {
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
            Ok(Reached::Rendered(rendering)) => Ended::of(&rendering.diagnostics),
            Err(Refusal::Diagnostics(diagnostics)) => Ended::of(diagnostics),
            _ => Ended::Done,
        });

        (reached, refused(cache))
    }

    /// What the optimizer did to each of the subject's declarations, beside why its store took nothing, where it could not.
    pub fn costed(
        self,
        budget: u64,
        overlay: &Overlay,
        telling: &mut dyn Telling,
    ) -> (Result<Vec<Fate>, Uncosted>, Option<String>) {
        let (subject, store) = self.taken(telling);
        let cache = store.as_ref();
        let fates = cost(budget, subject, overlay, cache, &mut |progress| {
            telling.step(progress, cache);
        });
        telling.ended(match &fates {
            Err(Uncosted::Diagnostics(diagnostics)) => Ended::of(diagnostics),
            _ => Ended::Done,
        });

        (fates, refused(cache))
    }

    /// What `file` of the subject writes, text by text, beside why its store took nothing, where it could not.
    pub fn written(
        self,
        budget: u64,
        overlay: &Overlay,
        file: &Path,
        telling: &mut dyn Telling,
    ) -> (Result<Vec<Written>, Vec<Diagnosis>>, Option<String>) {
        let (subject, store) = self.taken(telling);
        let cache = store.as_ref();
        let texts = written(budget, subject, file, overlay, cache, &mut |progress| {
            telling.step(progress, cache);
        });
        telling.ended(match &texts {
            Err(diagnostics) => Ended::of(diagnostics),
            Ok(_) => Ended::Done,
        });

        (texts, refused(cache))
    }
}

/// `wonder [SUBJECT]`: render every diagnostic of what `selection` selects to stdout, a blank line between each, `telling` told of each subject's compile as it goes and why, where a store took nothing of what the question compiled.
pub fn wonder_diagnostics(
    budget: u64,
    selection: Selection,
    telling: &mut dyn Telling,
) -> Result<(), String> {
    let overlay = Overlay::default();

    let answers = Asked::every(selection)?;

    let (reports, refusal) = rendered(answers, budget, &overlay, telling);
    if !reports.is_empty() {
        telling.answering();
        answer(format_args!("{}\n", reports.join("\n\n")));
    }
    if let Some(refusal) = refusal {
        telling.unfiled(refusal);
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

    let mut refusal = None;
    let mut listed = Ok(());
    for asked in Asked::every(selection).map_err(CompileError::failure)? {
        // The answer is the list on standard output, so what is said of how the file was taken goes beside it.
        if let Some(said) = asked.subject.said_unlinked(&overlay, "was checked") {
            eprintln!("note: {said}");
        }
        let (records, unstored) = asked.tests(budget, &overlay, telling);
        refusal = refusal.or(unstored);
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
    if let Some(refusal) = refusal {
        telling.unfiled(refusal);
    }

    listed
}

/// Why `store` took nothing of what a question compiled, where it could not be written: asked once the question's fold is over, since that is when the store has been handed anything, and whether or not the question was answered, since a store nobody can write is true either way.
fn refused(store: Option<&Verdicts>) -> Option<String> {
    store.and_then(Verdicts::refused)
}

/// Every answer's diagnostics, rendered, each distinct fact once.
///
/// **The subjects of a whole package overlap, and one fact is still one fact.** Every executable is compiled against the library, so a diagnostic in the library is reached by the library's own subject and again by each executable's — one unbound variable printed three times in a package declaring two programs, which is what an agent's one-error-at-a-time loop then walks through. Collapsing is safe because a rendering carries the source, the line and the column beneath the message: two that compare equal say the same thing about the same place, and two about different places never compare equal.
///
/// The subjects themselves are still compiled apart, which is what keeps this a report about a package rather than about one compilation of it. What that costs — the library folded once per subject, answered from the store when there is one and recompiled when there is not — is the price of the same independence.
///
/// Beside them, the first reason a store gave for taking nothing of what an answer compiled: one store refuses every unit for one reason, and the subjects of a package share it.
pub fn rendered(
    answers: Vec<Asked>,
    budget: u64,
    overlay: &Overlay,
    telling: &mut dyn Telling,
) -> (Vec<String>, Option<String>) {
    let mut seen = BTreeSet::new();
    let mut unfiled = None;

    let reports = answers
        .into_iter()
        .flat_map(|asked| {
            let Diagnosed {
                diagnostics,
                unfiled: refusal,
                ..
            } = asked.diagnosed(budget, overlay, telling);
            unfiled = unfiled.take().or(refusal);

            diagnostics
        })
        .map(|diagnostic| diagnostic.render())
        .filter(|rendered| seen.insert(rendered.clone()))
        .collect();

    (reports, unfiled)
}

/// What `wonder --stage` reprints a rung of: a program, at any rung, or a library through one of its files, at the rungs a unit has.
pub enum Staged {
    Program(Program),
    Library(Library),
}

/// `wonder --stage STAGE [SUBJECT]`: the rung of what `staged` is, reprinted, to stdout.
///
/// `finish` renders the one rung the driver cannot: `wasm-optm` is the module after Binaryen, which this crate does not link, so the engine hands the emitted module back and the product that owns Binaryen prints it. Every other rung is printed here, from the driver's own rendering. `telling` is told as [`wonder_diagnostics`]' is.
pub fn wonder_stage(
    budget: u64,
    name: &str,
    staged: Staged,
    finish: impl FnOnce(Box<curios_wasm::Module>),
    telling: &mut dyn Telling,
) -> Result<(), CompileError> {
    let overlay = Overlay::default();

    let program = match staged {
        Staged::Program(program) => program,
        Staged::Library(library) => {
            return library_stage(budget, name, library, &overlay, telling);
        }
    };

    let refusal = written_as_a_module(label_of(program.entry()));
    let asked = Asked::about_program(program).map_err(CompileError::failure)?;
    if let Some(said) = asked.subject.said_unlinked(&overlay, "is compiled") {
        eprintln!("note: {said}");
    }

    // The rung is the answer and goes to stdout, where the driver emits it: once every unit the program stands on is compiled, and before anything downstream of the rung is.
    let shown = |text: &str| answer(format_args!("{text}\n"));
    let (reached, unstored) = asked.staged(budget, &overlay, name, shown, telling);
    let answered = match reached {
        Ok(Reached::Rendered(rendering)) => {
            // What stopped the compilation after the rung is context and goes to stderr, so a pipeline reading the rendering is unaffected by it.
            for diagnostic in &rendering.diagnostics {
                eprintln!("{}", diagnostic.render());
            }
            Ok(())
        }
        Ok(Reached::Wasm(module)) => {
            finish(module);
            Ok(())
        }
        Err(Refusal::NoSuchStage { asked }) => Err(no_such_stage(&asked)),
        Err(Refusal::Unit) => Err(CompileError::failure(refusal)),
        Err(Refusal::Diagnostics(diagnostics)) => Err(stopped(diagnostics)),
    };
    if let Some(unstored) = unstored {
        telling.unfiled(unstored);
    }

    answered
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
    let store = Verdicts::at(library.root.clone());

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
    let answered = match reached {
        Ok(_) => Ok(()),
        Err(Refusal::NoSuchStage { asked }) => Err(no_such_stage(&asked)),
        // Whoever admits a library admits it at a rung a unit has, and refuses it at any other in its own words.
        Err(Refusal::Unit) => unreachable!("a library was admitted at a program's rung"),
        Err(Refusal::Diagnostics(diagnostics)) => Err(stopped(diagnostics)),
    };
    if let Some(unstored) = refused(Some(&store)) {
        telling.unfiled(unstored);
    }

    answered
}

/// The file of `library` a question about one file is about: the one it was selected through, or its header, which leads the files a library is written in.
fn asked_through(library: &Library) -> Result<PathBuf, CompileError> {
    match &library.through {
        Some(file) => Ok(file.clone()),
        None => Ok(library
            .declared_files()
            .map_err(CompileError::failure)?
            .into_iter()
            .next()
            .unwrap_or_default()),
    }
}

/// A place in a file as a diagnostic prints one and a question is asked of one: a line and a column, each counted from one, the column in scalar values.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Place {
    pub line: usize,
    pub column: usize,
}

impl FromStr for Place {
    type Err = String;

    fn from_str(written: &str) -> Result<Self, String> {
        let counted = |number: &str| number.parse::<usize>().ok().filter(|number| *number > 0);

        written
            .split_once(':')
            .and_then(|(line, column)| {
                Some(Place {
                    line: counted(line)?,
                    column: counted(column)?,
                })
            })
            .ok_or_else(|| {
                format!("{written:?} is no place: write a line and a column, each from one, as in `12:5`")
            })
    }
}

/// `wonder --at LINE:COLUMN [SUBJECT]`: the name written at a place of the file `staged` is asked through, to stdout — one row, then its declaration's head and prose where its text is held — and nothing where no name stands there. `telling` is told as [`wonder_diagnostics`]' is.
pub fn wonder_at(
    budget: u64,
    staged: Staged,
    place: Place,
    telling: &mut dyn Telling,
) -> Result<(), CompileError> {
    for_names(budget, staged, telling, |texts| {
        let names = texts.iter().flat_map(|text| &text.names);
        let offset = texts
            .first()
            .and_then(|text| text.text.source.offset_of(place.line, place.column));

        if let Some(named) = offset.and_then(|offset| Named::at(names, offset)) {
            answer(format_args!("{}\n", row(named)));
            // Beneath the row, a blank line before each: the head as its author wrote it, then the prose above it.
            if let Some(heading) = named.declaration.as_ref().and_then(Declaration::heading) {
                answer(format_args!("\n{}\n", heading.head));
                if !heading.prose.is_empty() {
                    answer(format_args!("\n{}\n", heading.prose.join("\n")));
                }
            }
        }
    })
}

/// `wonder --declarations [SUBJECT]`: every definition the file `staged` is asked through declares, one row per line to stdout, in the order they are written. `telling` is told as [`wonder_diagnostics`]' is.
pub fn wonder_declarations(
    budget: u64,
    staged: Staged,
    telling: &mut dyn Telling,
) -> Result<(), CompileError> {
    for_names(budget, staged, telling, |texts| {
        for named in texts
            .iter()
            .flat_map(|text| &text.names)
            .filter(|named| named.declares)
        {
            answer(format_args!("{}\n", row(named)));
        }
    })
}

/// What the file `staged` is asked through writes, handed to `answer`: the one it was selected through, a program's own entry, or a library's header. A subject with nothing to read stops as a build does, what stopped it going to stderr.
fn for_names(
    budget: u64,
    staged: Staged,
    telling: &mut dyn Telling,
    answer: impl FnOnce(&[Written]),
) -> Result<(), CompileError> {
    let overlay = Overlay::default();

    let (asked, file) = match staged {
        Staged::Program(program) => {
            let file = match (program.through(), program.entry()) {
                (Some(module), _) => module.to_path_buf(),
                (None, Entry::File(entry)) => entry.clone(),
                (None, Entry::Stdin) => PathBuf::from(STDIN_LABEL),
            };

            (
                Asked::about_program(program).map_err(CompileError::failure)?,
                file,
            )
        }
        Staged::Library(library) => {
            let file = asked_through(&library)?;

            (Asked::about_library(library), file)
        }
    };
    if let Some(said) = asked.subject.said_unlinked(&overlay, "is read") {
        eprintln!("note: {said}");
    }

    let (texts, unstored) = asked.written(budget, &overlay, &file, telling);
    let answered = match texts {
        Ok(texts) => {
            telling.answering();
            answer(&texts);
            Ok(())
        }
        Err(diagnostics) => Err(stopped(diagnostics)),
    };
    if let Some(unstored) = unstored {
        telling.unfiled(unstored);
    }

    answered
}

/// One name as a row: the declaration's path, what it is declared as, and where its name is written, a tab between each — the place left out where no file holds it.
fn row(named: &Named) -> String {
    let declared = match named.declared {
        Declared::Authored => "definition",
        Declared::Inductive => "inductive",
        Declared::Struct => "structure",
        Declared::Concept => "concept",
        Declared::Constructor => "constructor",
        Declared::Method => "method",
        Declared::Witness => "witness",
        Declared::Test => "test",
    };

    match named.declaration.as_ref().and_then(Declaration::place) {
        Some(place) => format!("{}\t{declared}\t{}", named.name, placed(place)),
        None => format!("{}\t{declared}", named.name),
    }
}

/// Where `span` begins, as a diagnostic's header names it: the file, the line and the column.
fn placed(span: &Span) -> String {
    let (line, column) = span.line_column();

    match span.source.path() {
        Some(path) => format!("{}:{line}:{column}", path.display()),
        None => format!("{line}:{column}"),
    }
}

/// What asking for a rung the pipeline has none of is refused with.
fn no_such_stage(asked: &str) -> CompileError {
    CompileError::failure(format!(
        "no stage named {asked:?}; the stages are {}",
        curios_pipeline::Stage::NAMES.join(", ")
    ))
}

/// What the optimizer did to each of `program`'s declarations, one tab-separated row per line.
///
/// Two columns, because the analysis should not need this crate: `awk -F'\t' '$2 == "absorbed"'` is a whole question, and a diff of two runs is a diff of two files. The rows are ordered by name for the same reason — a report that reproduces is what makes a regression something to read rather than something to judge.
///
/// `telling` is told as [`wonder_diagnostics`]' is.
pub fn wonder_cost(
    budget: u64,
    program: Program,
    telling: &mut dyn Telling,
) -> Result<(), CompileError> {
    let overlay = Overlay::default();

    let refusal = written_as_a_module(label_of(program.entry()));
    let asked = Asked::about_program(program).map_err(CompileError::failure)?;
    if let Some(said) = asked.subject.said_unlinked(&overlay, "is compiled") {
        eprintln!("note: {said}");
    }

    let (fates, unstored) = asked.costed(budget, &overlay, telling);
    let answered = match fates {
        Ok(fates) => {
            telling.answering();
            for fate in fates {
                // The outcome is one token and its count, so a column stays a column: `specialized 3` reads as one answer and splits as one field.
                let outcome = match fate.outcome {
                    Outcome::Survived => "survived".to_string(),
                    Outcome::Specialized { copies } => format!("specialized {copies}"),
                    Outcome::Absorbed => "absorbed".to_string(),
                };

                answer(format_args!("{}\t{outcome}\n", fate.name));
            }
            Ok(())
        }
        Err(Uncosted::Unit) => Err(CompileError::failure(refusal)),
        Err(Uncosted::Diagnostics(diagnostics)) => Err(stopped(diagnostics)),
    };
    if let Some(unstored) = unstored {
        telling.unfiled(unstored);
    }

    answered
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
pub fn file_subject(path: PathBuf) -> Result<PathBuf, String> {
    let readable = fs::metadata(&path).and_then(|metadata| match metadata.is_dir() {
        true => Err(io::Error::from(io::ErrorKind::IsADirectory)),
        false => Ok(()),
    });

    match readable {
        Ok(()) => Ok(path),
        Err(error) => Err(LoadError::Read { path, error }.format()),
    }
}

/// What a command that needs a program says of an entry written as a module, named `label`: it has no final term, so there is no program to compile.
pub fn written_as_a_module(label: impl Display) -> String {
    format!("{label} is written as a module, with no final term to compile a program from")
}

/// How a report names `entry`: its path, or standard input's label.
fn label_of(entry: &Entry) -> String {
    match entry {
        Entry::File(path) => path.display().to_string(),
        Entry::Stdin => STDIN_LABEL.to_string(),
    }
}

/// Standard input, drained to end.
fn read_stdin() -> Result<String, String> {
    io::read_to_string(io::stdin())
        .map_err(|error| format!("failed to read standard input: {error}"))
}
