//! The one-shot transport: ask about what the command line admitted, and render the answer for a reader.
//!
//! **The answer goes to stdout, and nothing else does** ([`answer`]). A query executes no program, so stdout is free to be the answer — which is what lets `curios wonder --stage wasm app > app.wat` mean what it says. Status lines stay on stderr as everywhere else, told to whoever narrates the question ([`Telling`]): that the cabinet took nothing of what it compiled, handed to the caller, which has the terminal to say it on.
//!
//! **Exit 0 means the question was answered, including when the answer is a list of errors.** Non-zero means it could not be asked: no such subject, no such stage, a scope that cannot be assembled. `stage`, `footprint` and `tests` are where the two meet — a program that stops before the answer has not answered the question, so what stopped it goes to stderr, stdout stays empty rather than holding text nothing downstream expected, and the exit is a build's: 2 when written goals alone stopped it, and 1 when anything was refused.

use {
    crate::{
        Answer, Classed, Declaration, DeclaredTest, Described, Diagnosed, Diagnosis, Exported,
        Fact, Origin, Referent, Refusal, Rendering, Row, Severity, Stands, Subject, Unfooted,
        Unput, checked, classed, declared_tests, described, diagnostics, exposing, falsified,
        footprint, named_stage, placed, referent, stage, unit_stage,
    },
    curios_common::{
        About, Kind, MAX_LINE_WIDTH, OsError, Qualifier, Report, STDIN_NAME, Said, Style,
    },
    curios_module::{LoadError, Overlay},
    curios_package::{Entry, Home, Library, Program, Refused, Selection},
    curios_pipeline::{CompileError, Progress, Put, Stage},
    curios_surface::WrittenIn,
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

/// Whoever a question tells of its work as it goes, beside the answer it writes on standard output: each subject as it is taken on, each step of the subject's compile, how that compile ended, that an answer is about to be written, and why the cabinet took nothing where it could not. The command line narrates with it; the language server asks in silence (`Silent`).
pub trait Telling {
    /// A subject is about to be compiled: what a report names it, and the manifest that declares it where one does.
    fn asked(&mut self, named: &curios_package::Named, manifest: Option<&Path>);

    /// A step of the compile in hand, beside the cabinet it files into, which says for itself whether it took what it was handed.
    fn step(&mut self, progress: Progress<'_>, verdicts: Option<&Verdicts>);

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

/// One thing a question can be about, resolved: the subject, what a report names it with the manifest that declares it, and the cabinet it reads and files into.
pub struct Asked {
    pub subject: Subject,
    /// What a report names the subject — none for the prelude's scope alone, which has nothing of its own to compile or narrate.
    pub named: Option<curios_package::Named>,
    pub manifest: Option<PathBuf>,
    pub verdicts: Option<Verdicts>,
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
        let verdicts = program.home().map(|home| {
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
            verdicts,
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
            verdicts: Some(Verdicts::at(library.cabinet)),
        }
    }

    /// The prelude's scope alone ([`Subject::prelude`]), with no manifest and no cabinet.
    fn about_the_prelude() -> Self {
        Self {
            subject: Subject::prelude(),
            named: None,
            manifest: None,
            verdicts: None,
        }
    }

    /// Compile over what `session` already compiled — see [`Session`]. A question with no cabinet, about a file no package declares, files nothing and keeps what it compiles in the session all the same (`Verdicts::kept_by`): the next keystroke compiles over it.
    pub fn reusing(mut self, session: &Session) -> Self {
        match &mut self.verdicts {
            Some(verdicts) => verdicts.reuse(session.clone()),
            None => self.verdicts = Some(Verdicts::kept_by(session.clone())),
        }

        self
    }

    /// The subject and its cabinet, `telling` told that the subject is taken on.
    fn taken(self, telling: &mut dyn Telling) -> (Subject, Option<Verdicts>) {
        if let Some(named) = &self.named {
            telling.asked(named, self.manifest.as_deref());
        }

        (self.subject, self.verdicts)
    }

    /// Every diagnostic, goal and lint the subject reports, with what it reached, `telling` told of the compile as it goes.
    pub fn diagnosed(self, budget: u64, overlay: &Overlay, telling: &mut dyn Telling) -> Diagnosed {
        let (subject, verdicts) = self.taken(telling);
        let verdicts = verdicts.as_ref();
        let diagnosed = diagnostics(budget, subject, overlay, verdicts, &mut |progress| {
            telling.step(progress, verdicts);
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
        let (subject, verdicts) = self.taken(telling);
        let verdicts = verdicts.as_ref();
        let diagnosed = checked(budget, subject, overlay, verdicts, file, &mut |progress| {
            telling.step(progress, verdicts);
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
        let (subject, verdicts) = self.taken(telling);
        let verdicts = verdicts.as_ref();
        let declared = declared_tests(budget, subject, overlay, verdicts, &mut |progress| {
            telling.step(progress, verdicts);
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
        let (subject, verdicts) = self.taken(telling);
        let cache = verdicts.as_ref();
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
        let (subject, verdicts) = self.taken(telling);
        let cache = verdicts.as_ref();
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
        let (subject, verdicts) = self.taken(telling);
        let cache = verdicts.as_ref();
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
        let (subject, verdicts) = self.taken(telling);
        let cache = verdicts.as_ref();
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
        let (subject, verdicts) = self.taken(telling);
        let cache = verdicts.as_ref();
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

    /// Every span of `file` of the subject the compiler classes ([`classed`]).
    pub fn classed(
        self,
        budget: u64,
        overlay: &Overlay,
        file: &Path,
        telling: &mut dyn Telling,
    ) -> Result<Vec<Classed>, Vec<Diagnosis>> {
        let (subject, verdicts) = self.taken(telling);
        let cache = verdicts.as_ref();
        let classed = classed(budget, subject, file, overlay, cache, &mut |progress| {
            telling.step(progress, cache)
        });
        telling.ended(match &classed {
            Err(diagnostics) => Ended::of(diagnostics),
            Ok(_) => Ended::Done,
        });

        classed
    }

    /// What `path` names in the subject's scope ([`described`]).
    pub fn described(
        self,
        budget: u64,
        overlay: &Overlay,
        path: &Qualifier,
        telling: &mut dyn Telling,
    ) -> Result<Option<Described>, Vec<Diagnosis>> {
        let (subject, verdicts) = self.taken(telling);
        let cache = verdicts.as_ref();
        let described = described(budget, subject, path, overlay, cache, &mut |progress| {
            telling.step(progress, cache);
        });
        telling.ended(match &described {
            Err(diagnostics) => Ended::of(diagnostics),
            Ok(_) => Ended::Done,
        });

        described
    }

    /// What `path` names in the subject's scope, a declaration, a module or both ([`referent`]), `telling` told of the compile as it goes.
    pub fn referent(
        self,
        budget: u64,
        overlay: &Overlay,
        path: &Qualifier,
        telling: &mut dyn Telling,
    ) -> Result<Option<Referent>, Vec<Diagnosis>> {
        let (subject, verdicts) = self.taken(telling);
        let cache = verdicts.as_ref();
        let referent = referent(budget, subject, path, overlay, cache, &mut |progress| {
            telling.step(progress, cache);
        });
        telling.ended(match &referent {
            Err(diagnostics) => Ended::of(diagnostics),
            Ok(_) => Ended::Done,
        });

        referent
    }
}

/// `wonder [SUBJECT]`: render every diagnostic of what `selection` selects to stdout, a blank line between each, `telling` told of each subject's compile as it goes.
pub fn wonder_diagnostics(
    budget: u64,
    selection: Selection,
    telling: &mut dyn Telling,
) -> Result<(), Box<Report>> {
    let overlay = Overlay::default();

    let answers = Asked::every(selection)?;
    let reports = rendered(answers, budget, &overlay, telling);
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

/// Every answer's diagnostics, rendered, each distinct fact once.
///
/// **The subjects of a whole package overlap, and one fact is still one fact.** Every executable is compiled against the library, so a diagnostic in the library is reached by the library's own subject and again by each executable's — one unbound variable printed three times in a package declaring two programs, which is what an agent's one-error-at-a-time loop then walks through. Collapsing is safe because a rendering carries the source, the line and the column beneath the message: two that compare equal say the same thing about the same place, and two about different places never compare equal.
///
/// The subjects themselves are still compiled apart, which is what keeps this a report about a package rather than about one compilation of it. What that costs — the library folded once per subject, answered from the cabinet when there is one and recompiled when there is not — is the price of the same independence.
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

/// `wonder --name PATH --stage STAGE [SUBJECT]`: the declaration `path` names in `scope`, at a rung a unit has, to stdout — the block `--name` answers with, without the source it holds there, then, a blank line before, its part of the rung, printed as the rung prints its whole (`named_stage`). A path that names nothing in scope is refused, as `core` is of a unit in scope's declaration, with the command that asks it of the file that holds it. `telling` is told as [`wonder_diagnostics`]' is.
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
    let verdicts = Verdicts::at(library.cabinet.clone());

    telling.asked(&library.named(), Some(&library.manifest));
    let reached = unit_stage(
        budget,
        library.units,
        &file,
        overlay,
        Some(&verdicts),
        name,
        &mut |progress| telling.step(progress, Some(&verdicts)),
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

/// `wonder --at FILE:LINE:COLUMN`: what stands at `place`, `staged` being what its file is admitted as, to stdout ([`placed`]) — for a name, the block a declaration is answered with, its source the one entry on it, opened on the path it is exposed at; for a term no name stands at, a block of what is written there, quoted at its place — and nothing where nothing stands there. The block names the command that lists a declaration's witnesses where it has any, asked in the scope of the place's file. `telling` is told as [`wonder_diagnostics`]' is.
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
                drawn(
                    &described,
                    Some(listing(&described.asked, Some(place.subject()))),
                    &reader
                )
            ));
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
                .trying(naming(&path, Some(place.subject())));
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

/// `wonder --classes [SUBJECT]`: every span of the file the subject is asked through that the compiler classes, `staged` being what its file is admitted as, to stdout, a row a line in the order they are written ([`classed`]), four fields joined by tabs: its place as a report closes on one, its length in scalar values as a column counts them, its class by its name, and its modifiers, a space between them — `declaration` where the span is where what it names is declared, and `default-library` where what it names lies in the library the compiler was built with — or `-` where it has none, so every row splits alike. What no row covers is left to a grammar. `telling` is told as [`wonder_diagnostics`]' is.
pub fn wonder_classes(
    budget: u64,
    staged: Staged,
    telling: &mut dyn Telling,
) -> Result<(), CompileError> {
    let overlay = Overlay::default();
    let reader = telling.reader();
    let (asked, file) = asked_about(staged)?;

    let classed = match asked.classed(budget, &overlay, &file, telling) {
        Ok(classed) => {
            telling.answering();
            classed
        }
        Err(diagnostics) => return Err(stopped(diagnostics)),
    };
    for Classed {
        span,
        class,
        declaration,
        default_library,
    } in classed
    {
        let length = span.source.text()[span.start..span.end].chars().count();
        let modifiers = [
            (declaration, "declaration"),
            (default_library, "default-library"),
        ]
        .into_iter()
        .filter_map(|(holds, modifier)| holds.then_some(modifier))
        .collect::<Vec<_>>();
        let modifiers = match modifiers.is_empty() {
            true => "-".to_string(),
            false => modifiers.join(" "),
        };
        answer(format_args!(
            "{}\t{length}\t{}\t{modifiers}\n",
            span.place(&reader),
            class.name()
        ));
    }

    Ok(())
}

/// `wonder --name PATH [SUBJECT]`: what `path` names in `scope`, to stdout — a declaration by a block holding its source as one entry, the block naming the command that lists its witnesses where it has any, `subject` as the command line wrote it, and one whose source no block shows, Core text's, by its block and then its whole text; a module by a block holding an entry for each thing it exposes ([`Report::listing`]); and a facade's path, which names both, by the declaration and then the module, a blank line between, the module's block listing what it exposes beside that declaration. A path that names nothing in scope is refused. `telling` is told as [`wonder_diagnostics`]' is.
pub fn wonder_name(
    budget: u64,
    scope: Scope,
    path: &Qualifier,
    subject: Option<&str>,
    telling: &mut dyn Telling,
) -> Result<(), CompileError> {
    let reader = telling.reader();
    let named = scope
        .asked()?
        .referent(budget, &Overlay::default(), path, telling);
    let Referent {
        declaration,
        module,
    } = match named {
        Ok(Some(named)) => {
            telling.answering();
            named
        }
        Ok(None) => return Err(unknown_declaration(path)),
        Err(diagnostics) => return Err(stopped(diagnostics)),
    };

    if let Some(described) = &declaration {
        answer(format_args!(
            "{}\n",
            drawn(described, Some(listing(&path.join(), subject)), &reader)
        ));
        if let (None, Some(declaration)) = (&described.stated, &described.declaration) {
            answer(format_args!("\n{}\n", declaration.written()));
        }
    }
    if let Some(module) = &module {
        // A facade's path names both, and the module stands after the declaration, a blank line between.
        if declaration.is_some() {
            answer("\n");
        }
        answer(format_args!(
            "{}\n",
            module_block(path, module, declaration.is_some()).listing(
                Kind::Module,
                &reader,
                &module.entries
            )
        ));
    }

    Ok(())
}

/// `wonder --name PATH --witnesses [SUBJECT]`: the witnesses in scope the declaration `path` names takes part in, to stdout — the block `--name` answers with, an entry on it for each witness ([`Report::listing`]): its head as its author wrote it, closed as a block closes a further place, behind `;`, by where it is written, or by the module that declares it where no file holds it. A path that names nothing in scope is refused. `telling` is told as [`wonder_diagnostics`]' is.
pub fn wonder_witnesses(
    budget: u64,
    scope: Scope,
    path: &Qualifier,
    telling: &mut dyn Telling,
) -> Result<(), CompileError> {
    let reader = telling.reader();
    let described = declaration(budget, scope, path, telling)?;

    let entries = described
        .witnesses
        .iter()
        .flatten()
        .map(|witnessed| {
            let place = match witnessed.place() {
                Some(place) => place.place(&reader),
                None => witnessed.module.clone(),
            };

            format!("{}; {place}", witnessed.head())
        })
        .collect::<Vec<_>>();
    answer(format_args!(
        "{}\n",
        block(&described, None).listing(Kind::Declaration, &reader, &entries)
    ));

    Ok(())
}

/// What a module is answered with, a block: its path, whether its `mod` line is written `pub` — a root, which no line declares, is a module and no more — and how many names and modules it exposes ([`exposing`]), `more` saying a declaration answered before it stands at its path, closed on where its `mod` line names it, or on its path where the line is no place to open.
fn module_block(path: &Qualifier, exported: &Exported, more: bool) -> Report {
    let about = match &exported.place {
        Some(place) => About::Place(place.clone()),
        None => About::Named(path.join()),
    };
    let module = match exported.public {
        Some(true) => "public module",
        Some(false) => "private module",
        None => "module",
    };

    Report::about(about, Said::line(path.join())).stating(
        match exposing(exported.names, exported.modules, more) {
            Some(exposed) => format!("{module}, {exposed}"),
            None => module.to_string(),
        },
    )
}

/// The command that answers what `path` names, asked in the scope of `subject` as the command line wrote it.
fn naming(path: &str, subject: Option<&str>) -> String {
    match subject {
        Some(subject) => format!("curios wonder --name {path} {subject}"),
        None => format!("curios wonder --name {path}"),
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

/// What `wonder --name`, its views and `wonder --at` say of `described` first, a block: the path it is answered by, then each fact stated of it ([`Described::facts`]), a fact of several values laid over as many lines as keep each within a block's width, then `listing`, the command that lists its witnesses, where there are any to list, closed on where its name is written, or where no file holds it on the path it is declared at — and on the path it was asked by where a root no reader names declares it.
fn block(described: &Described, listing: Option<String>) -> Report {
    let about = match described.declaration.as_ref().and_then(Declaration::place) {
        Some(place) => About::Place(place.clone()),
        None if described.adopted() => About::Named(described.asked.clone()),
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

/// The block of `described` ([`block`]) as `wonder --name` and `wonder --at` write it for `reader`: its source, where a block shows one, the one entry on it ([`Stated`](crate::Stated)), so the answer is what its source does not show and then its source's own words.
fn drawn(described: &Described, listing: Option<String>, reader: &Style) -> String {
    let entries = described
        .stated
        .iter()
        .map(|stated| stated.entry.clone())
        .collect::<Vec<_>>();

    block(described, listing).listing(Kind::Declaration, reader, &entries)
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

/// `wonder --name PATH --footprint [SUBJECT]`: what the program holds of each function of the text of the declaration `path` names, to stdout — the block `--name` answers with, without the source it holds there, then a row for each function in the order the declaration writes them, each heap twin under its function: its hint (`-` for none), what became of it, the bytes its bodies weigh in the emitted module and the values they rebuild in place, `-` for a function with no body of its own ([`Row`]). Four tab-separated fields a row, so `awk -F'\t' '$2 == "absorbed"'` is a whole question and a diff of two runs a diff of two files. A path that names nothing in scope is refused, as a subject written as a module is, which has no program. `telling` is told as [`wonder_diagnostics`]' is.
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
