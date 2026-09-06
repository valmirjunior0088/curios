//! The one-shot transport: resolve what was asked about, ask, render the answer for a reader.
//!
//! **The answer goes to stdout, and nothing else does.** A query executes no program, so stdout is free to be the answer — which is what lets `curios wonder stage wasm app > app.wat` mean what it says. Status lines stay on stderr as everywhere else, and here there are none: a question is not a build.
//!
//! **Exit 0 means the question was answered, including when the answer is a list of errors.** Non-zero means it could not be asked: no such target, no such stage, a scope that cannot be assembled. `stage` is the one place the two meet — a program that stops before the rung has not answered the question, so its diagnostics go to stderr and the exit is 1, leaving stdout empty rather than holding text nothing downstream expected.

use {
    crate::{
        Diagnosed, Diagnostic, Origin, Reached, Refusal, STDIN_LABEL, Subject, declared_tests,
        diagnosed, diagnostics, stage,
    },
    curios_package::{Form, Governing, LIBRARY, Membership, Target, mounted, order},
    curios_text::{LoadError, Overlay, RootSource},
    curios_verdicts::Verdicts,
    std::{
        collections::BTreeSet,
        fs, io,
        path::{Path, PathBuf},
    },
};

/// One thing a question can be about, resolved: the subject, and the store it may read.
pub struct Asked {
    pub subject: Subject,
    pub store: Option<Verdicts>,
}

impl Asked {
    /// What `file` is part of, placed by [`Membership`], with the `--unit` scope in front.
    pub fn about_file(
        file: &Path,
        mounted: &[PathBuf],
        manifest: Option<&Path>,
    ) -> Result<Self, String> {
        let mut units = mounted_units(mounted)?;

        Ok(match Membership::of(file, manifest)? {
            Membership::Standalone => Self {
                subject: Subject::Entry {
                    units,
                    origin: Origin::File(file.to_path_buf()),
                },
                store: None,
            },
            Membership::Library { root, units: scope } => {
                units.extend(scope);
                Self {
                    subject: Subject::Unit { units },
                    store: Some(Verdicts::at(root)),
                }
            }
            Membership::Executable {
                entry,
                root,
                units: scope,
                ..
            } => {
                units.extend(scope);
                Self {
                    subject: Subject::Entry {
                        units,
                        origin: Origin::File(entry),
                    },
                    store: Some(Verdicts::at(root)),
                }
            }
        })
    }

    /// The declared executable `target` names, or the sole one.
    fn about_executable(
        target: Option<&str>,
        mounted: &[PathBuf],
        manifest: Option<&Path>,
    ) -> Result<Self, String> {
        let mut units = mounted_units(mounted)?;
        let Target::Executable {
            entry,
            root,
            units: scope,
            ..
        } = Target::here(target, manifest)?
        else {
            unreachable!("neither `-` nor a path reaches here");
        };
        units.extend(scope);

        Ok(Self {
            subject: Subject::Entry {
                units,
                origin: Origin::File(entry),
            },
            store: Some(Verdicts::at(root)),
        })
    }

    /// The program on standard input, drained.
    fn about_stdin(mounted: &[PathBuf], text: String) -> Result<Self, String> {
        Ok(Self {
            subject: Subject::Entry {
                units: mounted_units(mounted)?,
                origin: Origin::Text {
                    label: STDIN_LABEL.to_string(),
                    text,
                },
            },
            store: None,
        })
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

/// `wonder diagnostics [TARGET]`: render every diagnostic to stdout, a blank line between each.
pub fn wonder_diagnostics(
    budget: u64,
    mounted: &[PathBuf],
    manifest: Option<&Path>,
    target: Option<&str>,
) -> Result<(), String> {
    let overlay = Overlay::default();

    let answers = resolve(mounted, manifest, target)?;

    let reports = rendered(answers, budget, &overlay);
    if !reports.is_empty() {
        println!("{}", reports.join("\n\n"));
    }

    Ok(())
}

/// The subjects `target` names, resolved the way `diagnostics`, `tests` and `curios lint` share it: one for a file, an executable or standard input, and the governing package entire — its library, then every executable it declares, each a subject of its own — for none.
pub(crate) fn resolve(
    mounted: &[PathBuf],
    manifest: Option<&Path>,
    target: Option<&str>,
) -> Result<Vec<Asked>, String> {
    Ok(match Form::of(target) {
        Form::Stdin => vec![Asked::about_stdin(mounted, read_stdin()?)?],
        Form::File(path) => vec![Asked::about_file(&file_target(path)?, mounted, manifest)?],
        Form::Named(Some(name)) => {
            vec![Asked::about_executable(Some(&name), mounted, manifest)?]
        }
        Form::Named(None) => {
            let governing = Governing::here(manifest)?;
            let mut asked = Vec::new();
            if governing.directory.join(LIBRARY).is_file() {
                let mut units = mounted_units(mounted)?;
                units.extend(order(&governing)?);
                asked.push(Asked {
                    subject: Subject::Unit { units },
                    store: Some(Verdicts::at(governing.root.clone())),
                });
            }
            for executable in &governing.package.executables {
                asked.push(Asked::about_executable(
                    Some(&executable.name),
                    mounted,
                    manifest,
                )?);
            }
            asked
        }
    })
}

/// `wonder tests [TARGET]`: every test the target declares, one path per line, in declaration order — the library's, then each executable's, when the target is the governing package entire. Nothing executes, and a package with no tests answers with nothing and exit 0.
pub fn wonder_tests(
    budget: u64,
    mounted: &[PathBuf],
    manifest: Option<&Path>,
    target: Option<&str>,
) -> Result<(), String> {
    let overlay = Overlay::default();

    for asked in resolve(mounted, manifest, target)? {
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

/// `wonder stage STAGE [TARGET]`: the rung, reprinted, to stdout.
///
/// `finish` renders the one rung the driver cannot: `wasm-optm` is the module after Binaryen, which this crate does not link, so the engine hands the emitted module back and the product that owns Binaryen prints it. Every other rung is printed here, from the driver's own rendering.
pub fn wonder_stage(
    budget: u64,
    mounted: &[PathBuf],
    manifest: Option<&Path>,
    name: &str,
    target: Option<&str>,
    finish: impl FnOnce(Box<curios_wasm::Module>),
) -> Result<(), String> {
    let overlay = Overlay::default();

    let asked = match Form::of(target) {
        Form::Stdin => Asked::about_stdin(mounted, read_stdin()?)?,
        Form::File(path) => Asked::about_file(&file_target(path)?, mounted, manifest)?,
        Form::Named(name) => Asked::about_executable(name.as_deref(), mounted, manifest)?,
    };

    let Subject::Entry { units, origin } = asked.subject else {
        return Err(
            "a library has no stages of its own — name an executable or a program file".to_string(),
        );
    };
    let cache = asked.store.as_ref();

    match stage(budget, units, origin, &overlay, cache, name) {
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

/// A file the question can be about: one the disk holds. A path that cannot be read is "no such target" — the question could not be asked, and the exit says so — refused here, before membership places it, in the words `run` uses for the same fault. The engine would otherwise answer it as one diagnostic and exit 0, and under a package directory would place the missing file as a library module and answer about the library. The server never comes through here: a document an editor holds may not be on disk yet, which is why the check is this transport's and not `Asked`'s.
pub(crate) fn file_target(path: PathBuf) -> Result<PathBuf, String> {
    let readable = fs::metadata(&path).and_then(|metadata| match metadata.is_dir() {
        true => Err(io::Error::from(io::ErrorKind::IsADirectory)),
        false => Ok(()),
    });

    match readable {
        Ok(()) => Ok(path),
        Err(error) => Err(LoadError::Read { path, error }.format()),
    }
}

/// Every `--unit DIR`'s library, in the order written — which is the order they are compiled in.
fn mounted_units(directories: &[PathBuf]) -> Result<Vec<RootSource>, String> {
    mounted(directories)
}

/// Standard input, drained to end.
fn read_stdin() -> Result<String, String> {
    io::read_to_string(io::stdin())
        .map_err(|error| format!("failed to read standard input: {error}"))
}
