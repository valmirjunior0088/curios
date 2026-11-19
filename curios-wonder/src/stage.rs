//! The `stage` query: the program's representation at one rung of the pipeline, reprinted, and a library's at the rungs a unit has; beside `--name`, the part of either that one declaration is.
//!
//! The rungs are [`Stage::NAMES`], each observed exactly where the driver emits it.

use {
    crate::{
        Described, Diagnosis, Overlaid, Severity, Subject, declared_where, described_entry,
        described_unit, of_error, open, overlaid, tested_last,
    },
    curios_common::Qualifier,
    curios_core::{Anchors, Definitions, Global, Item},
    curios_module::{Overlay, PreparedText, RootSource, SYNTAX, read_module},
    curios_pipeline::{Cache, EntryTail, Fold, Progress, Stage},
    curios_surface::{Entrypoint, FormScope, Module, Spellable, Spelling},
    curios_unit::Unit,
    curios_verdict::Verdicts,
    std::{
        collections::BTreeSet,
        path::{Path, PathBuf},
        rc::Rc,
    },
};

/// One rung, reprinted and handed on where the driver emitted it.
#[derive(Debug)]
pub struct Rendering {
    /// What stopped the compilation *after* this rung was reached, if anything. A rung the driver already emitted is an answer, and a later failure does not unmake it — the transport prints these beside the rendering rather than in place of it.
    pub diagnostics: Vec<Diagnosis>,
}

/// Why a rung could not be shown.
#[derive(Debug)]
pub enum Refusal {
    /// The subject is a unit rather than a program: it is written as a module, with no final term to compile a program from, and only a program has rungs.
    Unit,
    /// No rung of that name — the question could not be asked.
    NoSuchStage { asked: String },
    /// The program did not compile *as far as* the rung: what stopped it. A failure past the rung is not a refusal — see [`Rendering::diagnostics`].
    Diagnostics(Vec<Diagnosis>),
    /// A declaration's part at `core` is asked of a unit in scope, which keeps no lowering: asked of `file`, the one that holds it where one does, it is the subject's own.
    Kept { file: Option<PathBuf> },
    /// A declaration's part is asked at a rung past a unit's, which the view does not reach yet.
    PastCore,
}

/// `subject`'s representation at the rung `name`, compiled against its units and the prelude, handed to `shown` where the driver emits it: nothing a later rung does unmakes a rung already handed on, a panic on an invariant of its own among it.
pub fn stage(
    budget: u64,
    subject: Subject,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
    name: &str,
    mut shown: impl FnMut(&str),
    progress: &mut dyn FnMut(Progress<'_>),
) -> Result<Rendering, Refusal> {
    let Subject::Entry {
        units,
        origin,
        declares,
        package,
    } = subject.formed(overlay)
    else {
        return Err(Refusal::Unit);
    };
    let Some(name) = Stage::NAMES.into_iter().find(|known| *known == name) else {
        return Err(Refusal::NoSuchStage {
            asked: name.to_string(),
        });
    };

    let (entrypoint, loader) =
        open(origin, declares, package.as_ref(), overlay).map_err(Refusal::Diagnostics)?;
    let units = overlaid(units, overlay);
    let reached = cache.map(|cache| Overlaid::over(cache, overlay));
    let cache = reached.as_ref().map(|cache| cache as &dyn Cache);

    let mut reached = false;
    let compiled = Fold::new(budget, &units, cache).compile(
        &entrypoint,
        &loader,
        |stage| {
            if stage.name() == name {
                shown(&stage.to_string());
                reached = true;
            }
        },
        &mut *progress,
    );

    // A rung the driver emitted has been answered, whatever happens downstream of it. Asking for `core` on a program that fails to elaborate is asking what the lowering produced — which is most of why one asks — and discarding it because a later stage refused answers a question nobody put. What the transport's contract refuses is a program that stopped *before* the rung, which is exactly the case where nothing was observed.
    match (reached, compiled) {
        (true, Ok(_)) => Ok(Rendering {
            diagnostics: Vec::new(),
        }),
        (true, Err(error)) => Ok(Rendering {
            diagnostics: of_error(error),
        }),
        (false, Ok(_)) => unreachable!("a compile that ends emits every rung"),
        (false, Err(error)) => Err(Refusal::Diagnostics(of_error(error))),
    }
}

/// The rung `name` of a library — the last of `units`, compiled against the ones before it and the prelude — asked through `file`, as its rendering. A unit has the rungs [`Stage::of_a_unit`] names and no other: every rung from `anf` on is rooted at a program's entry, and is [`Refusal::Unit`] here.
///
/// **Each rung reprints what the unit holds of it, and none prints an entry.** `text` is the parse of the file asked through, since a unit is several files and the rung's printer reprints one. `core` is the unit's lowering, made over the units before it as a question about a place makes one, and the unit checked nowhere (`Fold::lowered_unit`). `core-elab` is its elaborated module as it stands, compiled over `cache` as a build compiles it.
pub fn unit_stage(
    budget: u64,
    units: Vec<RootSource>,
    file: &Path,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
    name: &str,
    progress: &mut dyn FnMut(Progress<'_>),
) -> Result<String, Refusal> {
    let Some(name) = Stage::NAMES.into_iter().find(|known| *known == name) else {
        return Err(Refusal::NoSuchStage {
            asked: name.to_string(),
        });
    };
    if !Stage::of_a_unit(name) {
        return Err(Refusal::Unit);
    }

    let units = overlaid(units, overlay);
    let reached = cache.map(|cache| Overlaid::over(cache, overlay));
    let through = reached.as_ref().map(|cache| cache as &dyn Cache);
    let rendered = match name {
        "text" => {
            let parsed = match overlay.get(file) {
                Some(text) => Module::overlaid(file, text).map_err(|error| error.report()),
                None => read_module(file).map_err(|error| error.report()),
            };

            parsed.map(|module| module.to_string()).map_err(|report| {
                vec![Diagnosis {
                    severity: Severity::Error,
                    report,
                }]
            })
        }
        "core" => Fold::new(budget, &units, through)
            .lowered_unit(&mut *progress, |scope, lowered| {
                lowered
                    .core()
                    .spelled(&dumped_over(scope.iter().map(|unit| unit.core())))
                    .to_string()
            })
            .map_err(of_error),
        _ => last_unit(budget, &units, through, progress, |unit, spelling| {
            unit.core().spelled(spelling).to_string()
        }),
    };

    rendered.map_err(Refusal::Diagnostics)
}

/// What `read` makes of the last of `units` once the fold has compiled it over `cache`, under the spelling a dump of its Core is written in — a test's and a foreign row's forms made against the units before it, the prelude's roots first — or what stopped the fold.
fn last_unit(
    budget: u64,
    units: &[RootSource],
    cache: Option<&dyn Cache>,
    progress: &mut dyn FnMut(Progress<'_>),
    read: impl FnOnce(&Unit, &Rc<Spelling>) -> String,
) -> Result<String, Vec<Diagnosis>> {
    Fold::new(budget, units, cache)
        .units(&mut *progress, |prelude, produced| {
            Ok(produced
                .split_last()
                .map(|(last, before)| {
                    let before = prelude
                        .iter()
                        .map(|unit| unit.core())
                        .chain(before.iter().map(Unit::core));

                    read(last, &dumped_over(before))
                })
                .unwrap_or_default())
        })
        .map_err(of_error)
}

/// What `wonder --name PATH --stage RUNG` reprints: the declaration `asked` names in `subject`'s scope, as `--name` describes it, and its part at the rung `name`, one a unit has ([`Stage::of_a_unit`]). `None` where nothing in scope is declared at `asked`.
///
/// **A declaration's part is what its text holds at the rung**, the items a type's constructors and methods lower to among them. `text` is that text parsed and reprinted. `core` is the items it lowered to in the subject's own lowering, which only the subject keeps: a unit in scope keeps none, so a declaration of one is refused with the file that holds it ([`Refusal::Kept`]). `core-elab` is its items in the elaborated module of the unit that declares it, the subject's own compiled as the subject is. A part is printed by the printer that prints a whole module, narrowed to its items ([`Spelling::narrowed_to`]), so each reads as it reads in the whole.
pub fn named_stage(
    budget: u64,
    subject: Subject,
    asked: &Qualifier,
    name: &str,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
    progress: &mut dyn FnMut(Progress<'_>),
) -> Result<Option<(Described, String)>, Refusal> {
    let Some(name) = Stage::NAMES.into_iter().find(|known| *known == name) else {
        return Err(Refusal::NoSuchStage {
            asked: name.to_string(),
        });
    };
    if !Stage::of_a_unit(name) {
        return Err(Refusal::PastCore);
    }
    let reached = cache.map(|cache| Overlaid::over(cache, overlay));
    let through = reached.as_ref().map(|cache| cache as &dyn Cache);

    match subject.formed(overlay) {
        Subject::Unit { units } => {
            let units = tested_last(overlaid(units, overlay));
            let Some(described) = described_unit(budget, &units, asked, through, &mut *progress)
                .map_err(Refusal::Diagnostics)?
            else {
                return Ok(None);
            };
            let part = match name {
                "text" => text_part(&described)?,
                "core" => Fold::new(budget, &units, through)
                    .lowered_unit(&mut *progress, |scope, own| lowered_part(asked, scope, own))
                    .map_err(of_error)
                    .map_err(Refusal::Diagnostics)??,
                _ => Fold::new(budget, &units, through)
                    .units(&mut *progress, |prelude, produced| {
                        let Some((last, before)) = produced.split_last() else {
                            return Ok(String::new());
                        };
                        let scope = prelude.iter().copied().chain(before).collect::<Vec<_>>();
                        let own = last.text().expect("a library is lowered from its text");

                        Ok(elaborated_part(asked, &scope, own, Some(last.core())))
                    })
                    .map_err(of_error)
                    .map_err(Refusal::Diagnostics)?,
            };

            Ok(Some((described, part)))
        }
        Subject::Entry {
            units,
            origin,
            declares,
            package,
        } => {
            let (entrypoint, loader) =
                open(origin, declares, package.as_ref(), overlay).map_err(Refusal::Diagnostics)?;
            let units = overlaid(units, overlay);
            let loader = loader.tested();
            let Some(described) = described_entry(
                budget,
                &units,
                &entrypoint,
                &loader,
                asked,
                through,
                &mut *progress,
            )
            .map_err(Refusal::Diagnostics)?
            else {
                return Ok(None);
            };
            let lowered = Lowered {
                budget,
                units: &units,
                through,
                entrypoint: &entrypoint,
                loader: &loader,
            };
            let part = match name {
                "text" => text_part(&described)?,
                "core" => {
                    lowered.read(&mut *progress, |scope, own| lowered_part(asked, scope, own))?
                }
                // The program's own declaration is read off the program as elaborated, which its lowering does not hold.
                _ => match lowered.read(&mut *progress, |scope, own| {
                    Ok(elaborated_part(asked, scope, own, None))
                })? {
                    part if !part.is_empty() => part,
                    _ => {
                        let checked = Fold::new(budget, &units, through)
                            .check(&entrypoint, &loader, EntryTail::Program, |step| {
                                progress(step)
                            })
                            .and_then(|checked| checked.verdict)
                            .map_err(of_error)
                            .map_err(Refusal::Diagnostics)?;

                        lowered.read(&mut *progress, |scope, own| {
                            Ok(elaborated_part(
                                asked,
                                scope,
                                own,
                                Some(&checked.program.definitions),
                            ))
                        })?
                    }
                },
            };

            Ok(Some((described, part)))
        }
    }
}

/// A program's lowering against the units it stands on, which a declaration's part is read off.
struct Lowered<'a> {
    budget: u64,
    units: &'a [RootSource],
    through: Option<&'a dyn Cache>,
    entrypoint: &'a Entrypoint,
    loader: &'a RootSource,
}

impl Lowered<'_> {
    /// What `read` makes of the program's own lowering and the units in its scope.
    fn read(
        &self,
        progress: &mut dyn FnMut(Progress<'_>),
        read: impl FnOnce(&[&Unit], &PreparedText) -> Result<String, Refusal>,
    ) -> Result<String, Refusal> {
        Fold::new(self.budget, self.units, self.through)
            .lowered(self.entrypoint, self.loader, progress, |scope, own, _| {
                read(scope, own)
            })
            .map_err(of_error)
            .map_err(Refusal::Diagnostics)?
    }
}

/// The declaration's text as its author wrote it, parsed and reprinted.
fn text_part(described: &Described) -> Result<String, Refusal> {
    let Some(declaration) = &described.declaration else {
        return Ok(String::new());
    };
    let path = Path::new(&described.path);
    let file = declaration.text.source.file().unwrap_or(path);

    Module::overlaid(file, declaration.written())
        .map(|module| module.to_string())
        .map_err(|error| {
            Refusal::Diagnostics(vec![Diagnosis {
                severity: Severity::Error,
                report: error.report(),
            }])
        })
}

/// The items the declaration `asked` names lowered to in the subject's own lowering `own`, over the units in `scope`; refused for a unit in scope's declaration, which keeps no lowering, with the file that holds it.
fn lowered_part(asked: &Qualifier, scope: &[&Unit], own: &PreparedText) -> Result<String, Refusal> {
    let Some((name, lowered, unit)) = declared_where(scope, own, asked) else {
        return Ok(String::new());
    };
    if unit.is_some() {
        let file = lowered
            .anchors()
            .written(&name)
            .and_then(|(text, _)| text.source.file().map(Path::to_path_buf));

        return Err(Refusal::Kept { file });
    }
    let part = part_of(own.core(), own.anchors(), &name);

    Ok(own
        .core()
        .spelled(&narrowed(
            dumped_over(scope.iter().map(|unit| unit.core())),
            part,
        ))
        .to_string())
}

/// The items the declaration `asked` names, in the elaborated module of the unit in `scope` that declares it, or in `elaborated`, the subject's own as elaborated, where the subject declares it: empty where neither holds it, as the subject's own does before it is handed.
fn elaborated_part(
    asked: &Qualifier,
    scope: &[&Unit],
    own: &PreparedText,
    elaborated: Option<&Definitions>,
) -> String {
    let Some((name, lowered, unit)) = declared_where(scope, own, asked) else {
        return String::new();
    };
    let Some(definitions) = unit.map(Unit::core).or(elaborated) else {
        return String::new();
    };
    let part = part_of(lowered.core(), lowered.anchors(), &name);

    definitions
        .spelled(&narrowed(
            dumped_over(scope.iter().map(|unit| unit.core())),
            part,
        ))
        .to_string()
}

/// Every name the items of `definitions` written in the text `name` was written in declare, by `anchors`: a declaration's part of its module, a type's constructors and methods with it.
fn part_of(definitions: &Definitions, anchors: &Anchors, name: &Global) -> BTreeSet<Global> {
    let text = anchors.text_key(name);

    definitions
        .items
        .iter()
        .map(Item::declared_names)
        .filter(|names| {
            names.contains(name)
                || names
                    .iter()
                    .any(|named| text.is_some() && anchors.text_key(named) == text)
        })
        .flatten()
        .collect()
}

/// `spelling`, writing of a module the items declaring one of `part` and no other.
fn narrowed(spelling: Rc<Spelling>, part: BTreeSet<Global>) -> Rc<Spelling> {
    Rc::new(Rc::unwrap_or_clone(spelling).narrowed_to(part))
}

/// The spelling a dump of a unit's Core is written in: Core text proper, a test's and a foreign row's forms made against `before`, the elaborated modules of the units before it, the prelude's roots first.
fn dumped_over<'a>(before: impl IntoIterator<Item = &'a Definitions>) -> Rc<Spelling> {
    let scope = FormScope::over(SYNTAX, before);

    Rc::new(
        Spelling::core_text()
            .with_form_scope(Rc::new(scope))
            .written_as_text(),
    )
}
