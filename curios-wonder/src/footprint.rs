//! The `--footprint` view: what the compiled program holds of each function of one declaration's text, and what it weighs there.
//!
//! It answers *which cliff am I on* — whether an optimization fired on the code a reader wrote — and needs no execution. It is read off the compilation the way a lint is read off name resolution: the driver already observes each rung, and every function and continuation carries its [`Lineage`], so reading what each rung holds of the declaration's lineages is the whole measurement, and no pass is instrumented to produce it. The erased program says what of the declaration was linked, the CPS graphs either side of the optimizer what the optimizer did to each function (`curios-cps`'s survey), and the emitted module what each weighs: its bodies, found by the names the emitter gives a function and never by reading one, as the writer measures them.
//!
//! **Nothing here judges.** A row states what became of a function and stops; whether an absorbed helper or a threefold copy is good news is the author's to decide, and a rule that decided it for them would be the heuristic the lint decision spent a paragraph refusing.
//!
//! **What a row leaves out.** Sizes after Binaryen, whose rewrite is below what the compiler controls, so the view answers what Curios made of the declaration; a frame's weight, which is the frame bound's stand-in for bytes and stands at a guarded call naming no callee, where the twin's row already says the declaration's frames may move to the heap; and why a pass inlined or copied a function, which a pass would have to record as it decides. Nor is it a field of `--name`'s block, which would compile a program to answer a question asked off the unit, and no program-wide listing stands beside it, almost every row of which would be the library's.

#[cfg(test)]
mod tests;

use {
    crate::{
        Described, Diagnosis, Origin, Overlaid, Subject, described_core_text, described_entry,
        of_error, open, overlaid,
    },
    curios_common::{Lineage, Qualifier},
    curios_cps::{held, rebuilds},
    curios_emit::emitted_functions,
    curios_module::Overlay,
    curios_pipeline::{Cache, Fold, Progress, Stage},
    curios_verdict::Verdicts,
    curios_wasm::{FuncName, code_sizes},
    std::{
        collections::{BTreeMap, HashMap},
        fmt,
    },
};

/// Why a subject has no footprint to report.
#[derive(Debug)]
pub enum Unfooted {
    /// The subject is a unit rather than a program: it is written as a module, with no final term to compile a program from, and what the compiler makes of a declaration is a program's.
    Unit,
    /// The program did not compile as far as Wasm: what stopped it.
    Diagnostics(Vec<Diagnosis>),
}

/// One function of the declaration's text, or one heap twin of it, and what became of it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Row {
    /// The hint the function bore in the CPS graph the optimizer was handed, or in the erased program where it never reached one: none for a function with none, such as the stand-in erasure makes of a type. A row the program holds nothing of is labelled by the declaration's path.
    pub label: Option<String>,
    pub fate: Fate,
    /// What its bodies weigh in the emitted module, where it has bodies of its own.
    pub bytes: Option<usize>,
    /// The values its own bodies build in the memory of one they hold dead, where it has bodies of its own.
    pub rebuilds: Option<usize>,
}

/// What became of a function of the declaration's text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Fate {
    /// The program holds nothing of the declaration: it is not linked, since nothing the program runs reaches it.
    NotLinked,
    /// The erased program held it, and the erased optimizer left nothing of it for the CPS graph: inlined or dropped before.
    PrunedBeforeCps,
    /// As many functions are of its lineage after the optimizer as before it.
    Kept,
    /// More functions are of its lineage than before, because a pass copied it — specialization on a known argument, on a call pattern, or on a branch: how many stand where it did.
    Copied(usize),
    /// No function is of its lineage any more, and continuations of it stand in these functions, by their hints: a pass put its code in their bodies.
    InlinedInto(Vec<Option<String>>),
    /// Nothing of its lineage stands: inlined whole as straight-line code, or pruned once something else was. It costs nothing of its own.
    Absorbed,
    /// The heap twin the frame bound made of the function on the row above, which runs it on the heap past the stack's bound, weighed with the functions it lifted out of the twin for its frames to wait in.
    HeapTwin,
}

impl fmt::Display for Fate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NotLinked => write!(f, "not linked"),
            Self::PrunedBeforeCps => write!(f, "pruned before CPS"),
            Self::Kept => write!(f, "kept"),
            Self::Copied(copies) => write!(f, "copied {copies}"),
            Self::InlinedInto(callers) => {
                let callers = callers
                    .iter()
                    .map(|caller| caller.as_deref().unwrap_or("-"))
                    .collect::<Vec<_>>();
                write!(f, "inlined into {}", callers.join(", "))
            }
            Self::Absorbed => write!(f, "absorbed"),
            Self::HeapTwin => write!(f, "heap twin"),
        }
    }
}

/// What `asked` names in the scope of the program `subject`, and, where that is a declaration of a path, what the program compiled against its units and the prelude holds of each function of its text: a row for each, in the order the declaration writes them, each heap twin under its function, or one row where the program holds nothing of it. `None` where nothing in scope is declared at `asked`.
///
/// The rungs read are the ones `wonder --stage` renders, taken in one compilation: the difference between them is the answer, and none alone is. A program that does not compile as far as Wasm has no footprint, so this refuses with what stopped it.
pub fn footprint(
    budget: u64,
    subject: Subject,
    asked: &Qualifier,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
    progress: &mut dyn FnMut(Progress<'_>),
) -> Result<Option<(Described, Vec<Row>)>, Unfooted> {
    let reached = cache.map(|cache| Overlaid::over(cache, overlay));
    let through = reached.as_ref().map(|cache| cache as &dyn Cache);
    let (units, origin, declares, package) = match subject {
        Subject::Entry {
            units,
            origin,
            declares,
            package,
        } => (units, origin, declares, package),
        Subject::Unit { .. } => return Err(Unfooted::Unit),
        Subject::CoreText { origin } => {
            return core_text_footprint(
                budget,
                &origin,
                asked,
                overlay,
                (cache, through),
                progress,
            );
        }
    };
    let (entrypoint, loader) =
        open(origin, declares, package.as_ref(), overlay).map_err(Unfooted::Diagnostics)?;
    let units = overlaid(units, overlay);

    // Asked as a question reads the program, with its tests, then compiled as a build compiles it.
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
    .map_err(Unfooted::Diagnostics)?
    else {
        return Ok(None);
    };
    let loader = loader.built();
    let mut seen = Seen::of(&described.path);
    Fold::new(budget, &units, through)
        .compile(
            &entrypoint,
            &loader,
            |stage| seen.observe(stage),
            &mut *progress,
        )
        .map_err(|error| Unfooted::Diagnostics(of_error(error)))?;
    let rows = seen.rows();

    Ok(Some((described, rows)))
}

/// [`footprint`] of a program read from Curios/Core: the declaration as `--name` answers it of the text, and the rows the program the text compiles to holds of its functions, read off the lineage erasure gives each, as a program's are.
fn core_text_footprint(
    budget: u64,
    origin: &Origin,
    asked: &Qualifier,
    overlay: &Overlay,
    (cache, through): (Option<&Verdicts>, Option<&dyn Cache>),
    progress: &mut dyn FnMut(Progress<'_>),
) -> Result<Option<(Described, Vec<Row>)>, Unfooted> {
    let Some(described) = described_core_text(
        budget,
        origin,
        asked,
        overlay,
        (cache, through),
        &mut *progress,
    )
    .map_err(Unfooted::Diagnostics)?
    else {
        return Ok(None);
    };
    let (source, module) = origin
        .read(overlay)
        .map_err(|refusal| Unfooted::Diagnostics(vec![*refusal]))?;
    let mut seen = Seen::of(&described.path);
    let compiled = Fold::new(budget, &[], through)
        .compile_core_text(&source, module, |stage| seen.observe(stage), &mut *progress)
        .map_err(|error| Unfooted::Diagnostics(of_error(error)))?;
    if compiled.is_none() {
        return Err(Unfooted::Unit);
    }

    Ok(Some((described, seen.rows())))
}

/// What each rung of one compilation held of one declaration's functions, by each function's place among the declaration's.
#[derive(Default)]
struct Seen {
    declaration: String,
    /// At `anf`: the hint of the first function of each lineage.
    erased: BTreeMap<u32, Option<String>>,
    /// At `cps`: the hint of the first function of each lineage, and how many there are.
    lowered: BTreeMap<u32, (Option<String>, usize)>,
    /// At `cps-optm`: what the graph the emitter is handed holds of each lineage.
    optimized: BTreeMap<u32, Optimized>,
    /// At `wasm`: what each function's body weighs.
    sizes: HashMap<FuncName, usize>,
}

/// What the optimized graph holds of one lineage.
#[derive(Default)]
struct Optimized {
    functions: Vec<Emitted>,
    /// Each heap twin, then the functions it binds that the frame bound lifted out of it.
    twins: Vec<Vec<Emitted>>,
    standing_in: Vec<Option<String>>,
}

/// One function of the optimized graph, by what the emitter makes of it.
struct Emitted {
    hint: Option<String>,
    names: [FuncName; 3],
    rebuilds: usize,
}

impl Seen {
    fn of(declaration: &str) -> Self {
        Self {
            declaration: declaration.to_string(),
            ..Self::default()
        }
    }

    /// The function `lineage` names among the declaration's, where it is one of them.
    fn own(&self, lineage: Lineage) -> Option<u32> {
        (lineage.declaration.join() == self.declaration).then_some(lineage.function)
    }

    fn observe(&mut self, stage: Stage<'_>) {
        match stage {
            Stage::Anf(module) => {
                for id in module.function_ids() {
                    let Some(function) = module.function(id) else {
                        continue;
                    };
                    if let Some(own) = function.lineage.and_then(|lineage| self.own(lineage)) {
                        self.erased
                            .entry(own)
                            .or_insert_with(|| function.debug_name.clone());
                    }
                }
            }
            Stage::Cps(module) => {
                let hint = |id| module.function(id)?.debug_name.clone();
                for (lineage, held) in held(module) {
                    let (Some(own), Some(&first)) = (self.own(lineage), held.functions.first())
                    else {
                        continue;
                    };
                    self.lowered
                        .insert(own, (hint(first), held.functions.len()));
                }
            }
            Stage::CpsOptm(module) => {
                let hint = |id| module.function(id)?.debug_name.clone();
                let emitted = |id| Emitted {
                    hint: hint(id),
                    names: emitted_functions(module, id),
                    rebuilds: rebuilds(module, id),
                };
                for (lineage, held) in held(module) {
                    let Some(own) = self.own(lineage) else {
                        continue;
                    };
                    let mut standing_in = held
                        .standing_in
                        .iter()
                        .map(|&id| hint(id))
                        .collect::<Vec<_>>();
                    standing_in.sort();
                    standing_in.dedup();
                    self.optimized.insert(
                        own,
                        Optimized {
                            functions: held.functions.iter().map(|&id| emitted(id)).collect(),
                            twins: held
                                .twins
                                .iter()
                                .map(|twin| {
                                    let lifted = twin.lifted.iter().map(|&id| emitted(id));
                                    std::iter::once(emitted(twin.function))
                                        .chain(lifted)
                                        .collect()
                                })
                                .collect(),
                            standing_in,
                        },
                    );
                }
            }
            Stage::Wasm(module) => self.sizes = code_sizes(module),
            _ => {}
        }
    }

    /// A row for each function of the declaration's text the erased program held, each heap twin under its function, or one row where it held none.
    fn rows(&self) -> Vec<Row> {
        if self.erased.is_empty() {
            return vec![Row {
                label: Some(self.declaration.clone()),
                fate: Fate::NotLinked,
                bytes: None,
                rebuilds: None,
            }];
        }
        let mut rows = Vec::new();
        for (own, erased) in &self.erased {
            let Some((lowered, count)) = self.lowered.get(own) else {
                rows.push(Row {
                    label: erased.clone(),
                    fate: Fate::PrunedBeforeCps,
                    bytes: None,
                    rebuilds: None,
                });
                continue;
            };
            let label = lowered.clone().or_else(|| erased.clone());
            let optimized = self.optimized.get(own);
            match optimized {
                Some(optimized) if !optimized.functions.is_empty() => {
                    let copies = optimized.functions.len();
                    rows.push(self.weighed(
                        label,
                        match copies > *count {
                            true => Fate::Copied(copies),
                            false => Fate::Kept,
                        },
                        &optimized.functions,
                    ));
                    for twin in &optimized.twins {
                        let hint = twin.first().and_then(|twin| twin.hint.clone());
                        rows.push(self.weighed(hint, Fate::HeapTwin, twin));
                    }
                }
                Some(optimized) if !optimized.standing_in.is_empty() => rows.push(Row {
                    label,
                    fate: Fate::InlinedInto(optimized.standing_in.clone()),
                    bytes: None,
                    rebuilds: None,
                }),
                _ => rows.push(Row {
                    label,
                    fate: Fate::Absorbed,
                    bytes: None,
                    rebuilds: None,
                }),
            }
        }

        rows
    }

    /// A row for functions with bodies of their own: what the emitted module holds under the names the emitter gives each, and what each rebuilds.
    fn weighed(&self, label: Option<String>, fate: Fate, functions: &[Emitted]) -> Row {
        let bytes = functions
            .iter()
            .flat_map(|function| &function.names)
            .filter_map(|name| self.sizes.get(name))
            .sum();
        let rebuilds = functions.iter().map(|function| function.rebuilds).sum();

        Row {
            label,
            fate,
            bytes: Some(bytes),
            rebuilds: Some(rebuilds),
        }
    }
}
