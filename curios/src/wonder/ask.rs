//! The one-shot transport: resolve what was asked about, ask, render the answer for a reader.
//!
//! **The answer goes to stdout, and nothing else does.** A query executes no program, so stdout is free to be the answer — which is what lets `curios wonder stage wasm app > app.wat` mean what it says. Status lines stay on stderr as everywhere else, and here there are none: a question is not a build.
//!
//! **Exit 0 means the question was answered, including when the answer is a list of errors.** Non-zero means it could not be asked: no such target, no such stage, a scope that cannot be assembled. `stage` is the one place the two meet — a program that stops before the rung has not answered the question, so its diagnostics go to stderr and the exit is 1, leaving stdout empty rather than holding text nothing downstream expected.

use {
    crate::{
        Diagnostic, Origin, Reached, Refusal, STDIN_LABEL, Subject, Verdicts, diagnostics, stage,
        to_cwasm_dumped,
    },
    curios_package::{Governing, LIBRARY, Membership, Target, mounted, names_a_file, order},
    curios_pipeline::Cache,
    curios_text::{Overlay, RootSource},
    std::{
        io,
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

    /// Every diagnostic and goal the subject reports.
    pub fn diagnostics(self, budget: u64, overlay: &Overlay) -> Vec<Diagnostic> {
        diagnostics(
            budget,
            self.subject,
            overlay,
            self.store.as_ref().map(|store| store as &dyn Cache),
        )
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

    let answers = match target {
        Some(Target::STDIN) => vec![Asked::about_stdin(mounted, read_stdin()?)?],
        Some(argument) if names_a_file(argument) => {
            vec![Asked::about_file(Path::new(argument), mounted, manifest)?]
        }
        Some(name) => vec![Asked::about_executable(Some(name), mounted, manifest)?],
        // The governing package entire: its library, then every executable it declares, each a subject of its own.
        None => {
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
    };

    let rendered = answers
        .into_iter()
        .flat_map(|asked| asked.diagnostics(budget, &overlay))
        .map(|diagnostic| diagnostic.render())
        .collect::<Vec<_>>();
    if !rendered.is_empty() {
        println!("{}", rendered.join("\n\n"));
    }

    Ok(())
}

/// `wonder stage STAGE [TARGET]`: the rung, reprinted, to stdout.
pub fn wonder_stage(
    budget: u64,
    mounted: &[PathBuf],
    manifest: Option<&Path>,
    name: &str,
    target: Option<&str>,
) -> Result<(), String> {
    let overlay = Overlay::default();

    let asked = match target {
        Some(Target::STDIN) => Asked::about_stdin(mounted, read_stdin()?)?,
        Some(argument) if names_a_file(argument) => {
            Asked::about_file(Path::new(argument), mounted, manifest)?
        }
        name => Asked::about_executable(name, mounted, manifest)?,
    };

    let Subject::Entry { units, origin } = asked.subject else {
        return Err(
            "a library has no stages of its own — name an executable or a program file".to_string(),
        );
    };
    let cache = asked.store.as_ref().map(|store| store as &dyn Cache);

    match stage(budget, units, origin, &overlay, cache, name) {
        Ok(Reached::Rendered(rendering)) => println!("{}", rendering.text),
        Ok(Reached::Wasm(module)) => {
            to_cwasm_dumped(&module, |stage| println!("{stage}"))?;
        }
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

/// Every `--unit DIR`'s library, in the order written — which is the order they are compiled in.
fn mounted_units(directories: &[PathBuf]) -> Result<Vec<RootSource>, String> {
    mounted(directories)
}

/// Standard input, drained to end.
fn read_stdin() -> Result<String, String> {
    io::read_to_string(io::stdin())
        .map_err(|error| format!("failed to read standard input: {error}"))
}
