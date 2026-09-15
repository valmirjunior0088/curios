//! The one-shot transport: ask about what the command line admitted, and render the answer for a reader.
//!
//! **The answer goes to stdout, and nothing else does.** A query executes no program, so stdout is free to be the answer — which is what lets `curios wonder stage wasm app > app.wat` mean what it says. Status lines stay on stderr as everywhere else, and here there are none: a question is not a build.
//!
//! **Exit 0 means the question was answered, including when the answer is a list of errors.** Non-zero means it could not be asked: no such target, no such stage, a scope that cannot be assembled. `stage` is the one place the two meet — a program that stops before the rung has not answered the question, so its diagnostics go to stderr and the exit is 1, leaving stdout empty rather than holding text nothing downstream expected.

use {
    crate::{
        Diagnosed, Diagnostic, FileAsked, Origin, Reached, Refusal, STDIN_LABEL, Subject, cost,
        declared_tests, diagnosed, diagnostics, stage,
    },
    curios_cont::Outcome,
    curios_package::{Entry, Library, Program, Selection},
    curios_text::{LoadError, Overlay},
    curios_verdicts::Verdicts,
    std::{collections::BTreeSet, fs, io, path::PathBuf},
};

/// One thing a question can be about, resolved: the subject, and the store it may read.
pub struct Asked {
    pub subject: Subject,
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
        let store = program.home().map(|home| Verdicts::at(home.root.clone()));
        let declares = program.declares();

        Ok(Self {
            subject: Subject::Entry {
                units: program.into_units(),
                origin,
                declares,
            },
            store,
        })
    }

    /// A library, asked about as the unit entire — through the file it was selected by, when it was, so a file the unit never reads is said to be one.
    fn about_library(library: Library) -> Self {
        let prefix = library
            .units
            .last()
            .and_then(|unit| unit.mounts().into_iter().next())
            .map(|mount| mount.prefix)
            .unwrap_or_default();

        Self {
            subject: Subject::Unit {
                file: library.through.map(|path| FileAsked {
                    path,
                    prefix,
                    module: library.module,
                }),
                units: library.units,
            },
            store: Some(Verdicts::at(library.root)),
        }
    }

    /// Every diagnostic, goal and lint the subject reports.
    pub fn diagnostics(self, budget: u64, overlay: &Overlay) -> Vec<Diagnostic> {
        diagnostics(budget, self.subject, overlay, self.store.as_ref())
    }

    /// [`Self::diagnostics`], with what the subject reached beside them.
    pub fn diagnosed(self, budget: u64, overlay: &Overlay) -> Diagnosed {
        diagnosed(budget, self.subject, overlay, self.store.as_ref())
    }
}

/// `wonder diagnostics [TARGET]`: render every diagnostic of what `selection` selects to stdout, a blank line between each.
pub fn wonder_diagnostics(budget: u64, selection: Selection) -> Result<(), String> {
    let overlay = Overlay::default();

    let answers = Asked::every(selection)?;

    let reports = rendered(answers, budget, &overlay);
    if !reports.is_empty() {
        println!("{}", reports.join("\n\n"));
    }

    Ok(())
}

/// `wonder tests [TARGET]`: every test `selection` declares, one path per line, in declaration order — the library's, then each executable's, when it is the governing package entire. Nothing executes, and a package with no tests answers with nothing and exit 0.
pub fn wonder_tests(budget: u64, selection: Selection) -> Result<(), String> {
    let overlay = Overlay::default();

    for asked in Asked::every(selection)? {
        let records = declared_tests(budget, asked.subject, &overlay, asked.store.as_ref())
            .map_err(|error| error.to_string())?;
        for record in records {
            println!("{}", record.path);
        }
    }

    Ok(())
}

/// Every answer's diagnostics, rendered, each distinct fact once.
///
/// **The subjects of a whole package overlap, and one fact is still one fact.** Every executable is compiled against the library, so a diagnostic in the library is reached by the library's own subject and again by each executable's — one unbound variable printed three times in a package declaring two programs, which is what an agent's one-error-at-a-time loop then walks through. Collapsing is safe because a rendering carries the source, the line and the column beneath the message: two that compare equal say the same thing about the same place, and two about different places never compare equal.
///
/// The subjects themselves are still compiled apart, which is what keeps this a report about a package rather than about one compilation of it. What that costs — the library folded once per subject, answered from the store when there is one and recompiled when there is not — is the price of the same independence.
pub(crate) fn rendered(answers: Vec<Asked>, budget: u64, overlay: &Overlay) -> Vec<String> {
    let mut seen = BTreeSet::new();

    answers
        .into_iter()
        .flat_map(|asked| asked.diagnostics(budget, overlay))
        .map(|diagnostic| diagnostic.render())
        .filter(|rendered| seen.insert(rendered.clone()))
        .collect()
}

/// `wonder stage STAGE [TARGET]`: `program`'s rung, reprinted, to stdout.
///
/// `finish` renders the one rung the driver cannot: `wasm-optm` is the module after Binaryen, which this crate does not link, so the engine hands the emitted module back and the product that owns Binaryen prints it. Every other rung is printed here, from the driver's own rendering.
pub fn wonder_stage(
    budget: u64,
    name: &str,
    program: Program,
    finish: impl FnOnce(Box<curios_wasm::Module>),
) -> Result<(), String> {
    let overlay = Overlay::default();

    let Asked {
        subject:
            Subject::Entry {
                units,
                origin,
                declares,
            },
        store,
    } = Asked::about_program(program)?
    else {
        unreachable!("a program is asked about as its entry");
    };
    let cache = store.as_ref();

    match stage(budget, units, origin, declares, &overlay, cache, name) {
        Ok(Reached::Rendered(rendering)) => {
            println!("{}", rendering.text);
            // The rung is the answer and goes to stdout; what stopped the compilation afterwards is context and goes to stderr, so a pipeline reading the rendering is unaffected by it.
            for diagnostic in &rendering.diagnostics {
                eprintln!("{}", diagnostic.render());
            }
        }
        Ok(Reached::Wasm(module)) => finish(module),
        Err(Refusal::NoSuchStage { asked }) => {
            return Err(format!(
                "no stage named {asked:?}; the stages are {}",
                curios_pipeline::Stage::NAMES.join(", ")
            ));
        }
        Err(Refusal::Diagnostics(diagnostics)) => {
            let rendered = diagnostics
                .iter()
                .map(Diagnostic::render)
                .collect::<Vec<_>>();
            return Err(rendered.join("\n\n"));
        }
    }

    Ok(())
}

/// What the optimizer did to each of `program`'s declarations, one tab-separated row per line.
///
/// Two columns, because the analysis should not need this crate: `awk -F'\t' '$2 == "absorbed"'` is a whole question, and a diff of two runs is a diff of two files. The rows are ordered by name for the same reason — a report that reproduces is what makes a regression something to read rather than something to judge.
pub fn wonder_cost(budget: u64, program: Program) -> Result<(), String> {
    let overlay = Overlay::default();

    let Asked {
        subject:
            Subject::Entry {
                units,
                origin,
                declares,
            },
        store,
    } = Asked::about_program(program)?
    else {
        unreachable!("a program is asked about as its entry");
    };
    let cache = store.as_ref();

    match cost(budget, units, origin, declares, &overlay, cache) {
        Ok(fates) => {
            for fate in fates {
                // The outcome is one token and its count, so a column stays a column: `specialized 3` reads as one answer and splits as one field.
                let outcome = match fate.outcome {
                    Outcome::Survived => "survived".to_string(),
                    Outcome::Specialized { copies } => format!("specialized {copies}"),
                    Outcome::Absorbed => "absorbed".to_string(),
                };

                println!("{}\t{outcome}", fate.name);
            }
        }
        Err(diagnostics) => {
            let rendered = diagnostics
                .iter()
                .map(Diagnostic::render)
                .collect::<Vec<_>>();
            return Err(rendered.join("\n\n"));
        }
    }

    Ok(())
}

/// A file the question can be about: one the disk holds. A path that cannot be read is "no such target" — the question could not be asked, and the exit says so — refused by the command line before membership places it, in the words `run` uses for the same fault. The engine would otherwise answer it as one diagnostic and exit 0, and under a package directory would place the missing file as a library module and answer about the library. The server never comes through here: a document an editor holds may not be on disk yet, which is why the check is the command line's and not `Asked`'s.
pub fn file_target(path: PathBuf) -> Result<PathBuf, String> {
    let readable = fs::metadata(&path).and_then(|metadata| match metadata.is_dir() {
        true => Err(io::Error::from(io::ErrorKind::IsADirectory)),
        false => Ok(()),
    });

    match readable {
        Ok(()) => Ok(path),
        Err(error) => Err(LoadError::Read { path, error }.format()),
    }
}

/// Standard input, drained to end.
fn read_stdin() -> Result<String, String> {
    io::read_to_string(io::stdin())
        .map_err(|error| format!("failed to read standard input: {error}"))
}
