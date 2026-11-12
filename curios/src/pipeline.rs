//! Driving the compile pipeline for the CLI, from an admitted program to the payload a target consumes — natively the `.cwasm` both subcommands run or bundle, and for a browser the module Binaryen wrote — including the store consultation that can skip the whole thing. Observing a stage is not here: that is a question about a program, and questions are `wonder`'s.
//!
//! **The payload, not the wasm module, is what this hands back**, and that is what lets one stored artifact serve `run` and `compile` alike: `run` executes it in-process exactly as it executes a fresh one, and `compile` appends it to the embedded launcher or writes it beside a page. Optimization and precompilation therefore happen here rather than in `main`, which is left dispatching.

use {
    crate::{Subject, Tree, note},
    curios::{engine, opened, optimize, supplied, to_cwasm},
    curios_abi::ForeignStore,
    curios_common::Source,
    curios_document::Documentation,
    curios_module::{RootSource, UnitSource},
    curios_package::{Entry, Program, Runs, Target},
    curios_pipeline::{Cache, CompileError, Fold, Progress, Stage},
    curios_surface::{Entrypoint, Form},
    curios_verdict::Verdicts,
    curios_wasm::Module,
    std::{cell::RefCell, fs, io, path::Path, sync::Arc},
};

/// Say, ahead of everything a command reports of `program`, that its file is one its package's directory holds and no unit declares, where it is one: the command takes it on its own, which `taken` words, and nothing after would say what it was not compiled against, so a name of the package refused in it reads as a typo. What would declare it follows the form it is written in ([`Unlinked::note`](curios_package::Unlinked::note)), read off the disk: a file that cannot be read is a program's to report.
pub fn note_unlinked(program: &Program, taken: &str) {
    if let Some(unlinked) = program.unlinked() {
        let form = fs::read_to_string(&unlinked.file)
            .map_or(Form::Program, |text| Form::of(&unlinked.file, &text));

        note(unlinked.note(taken, form));
    }
}

/// The payload `program` is built into for `target`, taken from `cache` when nothing it was made from has changed, and compiled — and filed there — otherwise: natively the precompiled `.cwasm`, filed under the engine that will run it, and for a browser the module Binaryen wrote, which any browser runs. `cache` is the store its command opened for it to file into, and `None` compiles everything and files nothing.
///
/// The scope is the program's own dependency graph and nothing else: a manifest is the only thing that says what a unit is compiled against. The error keeps the incomplete/failure split so `main` can map a goal batch to its own exit code.
///
/// `then` is what the command goes on to write in the subject's tree, which comes back with the payload for it to do so: a payload the store held narrates nothing, and its tree is the subject's with no node yet.
pub fn payload_of(
    budget: u64,
    program: Program,
    cache: Option<Verdicts>,
    target: Target,
    then: Then,
) -> Result<Built, CompileError> {
    let subject = subject_of(&program);
    let runs = match target {
        Target::Native => Runs::Native { engine: engine() },
        Target::Browser => Runs::Browser,
    };

    note_unlinked(&program, "is compiled");

    // A loose program has no project, so it has no store to consult: what a compilation may reuse is a fact about the project it is in, and a loose program is in none.
    let entry = match program.entry() {
        Entry::Stdin => None,
        Entry::File(path) => Some(path.clone()),
    };
    let declared = program
        .home()
        .map(|home| (home.package.clone(), home.executable.clone()));
    let declares = program.declares();
    let manifest = program.home().map(|home| home.manifest.clone());
    let units = program.into_units();

    // Opened before the store is consulted, because the entry's own text is half of what a stored payload is verified against — and it has to be the text that was *parsed*, not a re-read taken afterwards.
    let (entrypoint, loader, source) = open(entry.as_deref())?;
    // What the manifest declared, onto the resolver the entry's own names go through. A loose program has no manifest, so it declares nothing and sees every open prefix — which is the whole scope it was given, since nothing mounted anything beside the prelude.
    let loader = match declares {
        Some(declares) => loader.declaring(declares),
        None => loader,
    };

    // A payload is filed only where all three exist: a store to put it in, a declared name to file it under, and an entry file to verify it against. Standard input has none of them, and reaches this as the `None` that skips both the get and the put.
    let filed = cache
        .as_ref()
        .zip(declared.as_ref())
        .zip(entry.as_deref())
        .map(|((cache, name), path)| {
            (
                cache,
                curios_verdict::Program {
                    package: &name.0,
                    executable: &name.1,
                    entry: path,
                    text: source.text(),
                    loader: &loader,
                    tests: None,
                },
            )
        });

    // Built once and handed to both halves of the payload family, which is what keeps them agreeing about which units the compilation holds: the probe refuses a unit the store may not file, and the write must refuse the same one.
    let sources = units.iter().map(UnitSource::mounted).collect::<Vec<_>>();

    if let Some((cache, program)) = &filed
        && let Some((payload, foreigns)) = cache.payload_get(program, &sources, runs)
    {
        return Ok(Built {
            payload,
            foreigns,
            tree: Tree::of(&subject, manifest.as_deref()),
        });
    }

    let compiled = compile_entry(
        Fold::new(
            budget,
            &units,
            cache.as_ref().map(|cache| cache as &dyn Cache),
        ),
        &entrypoint,
        &loader,
        Steps::program(&subject, manifest.as_deref(), cache.as_ref(), then),
        |module| match target {
            Target::Native => to_cwasm(module).map_err(CompileError::failure),
            Target::Browser => Ok(optimize(module, false, |_| {})),
        },
    );

    if let (Ok((payload, foreigns, _)), Some((cache, program))) = (&compiled, &filed) {
        cache.payload_put(program, &sources, payload, foreigns, runs);
    }

    unstored(cache.as_ref());

    compiled.map(|(payload, foreigns, tree)| Built {
        payload,
        foreigns,
        tree,
    })
}

/// A program built: the payload its target consumes, the rows it imports, and its subject's tree, for the command to go on writing where it does more for the subject.
pub struct Built {
    pub payload: Vec<u8>,
    pub foreigns: ForeignStore,
    pub tree: Tree,
}

/// Say why `store` took nothing of what a build compiled, where it took nothing. After the fold rather than during it: one unwritable store refuses everything for the same reason, so the reason is one line however many units went past it, each of whose `store` node says only that it was refused. Said even when the compilation failed, because a store nobody can write is true either way and the next run pays for it either way.
pub fn unstored(store: Option<&Verdicts>) {
    if let Some(refusal) = store.and_then(Verdicts::refused) {
        note_unstored(&refusal);
    }
}

/// Say that a store took nothing of what a build compiled, for the reason it gave.
pub fn note_unstored(refusal: &str) {
    note(format!("what this built is not stored; {refusal}"));
}

/// The interface of the last of `units` — a package's library, compiled against everything before it — made from the compilation that builds it, whose units are filed into `store` when there is one.
///
/// **A build, where the `wonder` engine's reading of the same record is a question.** A library documented is a library compiled, and what was compiled is worth what `run` and `test` keep of theirs; the engine's `documentation` files the units a build would have and writes no pages, which is as far as a question goes. The compilation runs to completion first, the certifier included, so a library that does not check is not documented and reports what stopped it exactly as `run` would.
///
/// The library is `subject`, whose tree comes back with the record for the command to close with where it wrote the pages.
pub fn documentation_of(
    budget: u64,
    units: &[RootSource],
    store: Option<&Verdicts>,
    subject: &Subject,
) -> Result<(Documentation, Tree), CompileError> {
    let steps = RefCell::new(Steps::library(subject, store));
    let documented = Fold::new(budget, units, store.map(|store| store as &dyn Cache))
        .documentation(|progress| steps.borrow_mut().told(progress));
    let mut steps = steps.into_inner();

    steps.ended(&documented);
    unstored(store);

    documented.map(|documentation| (documentation, steps.into_tree()))
}

/// What a program is reported as — the name that was asked for, never the file it resolved to.
///
/// A declared executable resolves to an absolute path somewhere under the governing root, and echoing that back fills a status line with what the reader already knew. A loose file *is* what was asked for, so it reports as written. Standard input was asked for as `-`, which reports as nothing a reader can act on, so it is the one subject named rather than echoed.
pub fn subject_of(program: &Program) -> Subject {
    match (program.entry(), program.home()) {
        (_, Some(home)) => Subject::Executable(home.executable.clone()),
        (Entry::File(path), None) => Subject::File(path.clone()),
        (Entry::Stdin, None) => Subject::Stdin,
    }
}

/// The entry program, what its own modules resolve against, and the text it was parsed from: a file when there is one, and otherwise standard input, drained to end.
///
/// Draining is why this is worth naming rather than inlining. The program's own standard input is gone once the compiler has read the source out of it, so a read of it reports the end of the input — unavoidable when both want one descriptor, and the reason a program that reads its input belongs in a file.
pub fn open(entry: Option<&Path>) -> Result<(Entrypoint, RootSource, Arc<Source>), CompileError> {
    match entry {
        Some(path) => opened(path),
        None => supplied(&drained()?),
    }
}

/// Standard input, drained to end — once, since a second read finds nothing.
pub fn drained() -> Result<String, CompileError> {
    io::read_to_string(io::stdin())
        .map_err(|error| CompileError::failure(format!("failed to read standard input: {error}")))
}

/// Compile `entrypoint` against `units` in the order given, and make the payload `finish` makes of the module, narrated by `steps`, whose subject's tree comes back with the payload.
///
/// The program's last build node stays open until `finish` returns, so what it reports is what the payload took: the optimized program emitted and compiled to what a run executes.
pub fn compile_entry<P>(
    fold: Fold<'_>,
    entrypoint: &Entrypoint,
    loader: &RootSource,
    steps: Steps<'_>,
    finish: impl FnOnce(&Module) -> Result<P, CompileError>,
) -> Result<(P, ForeignStore, Tree), CompileError> {
    // Two callbacks write one narration, the stages a program reaches and the units the fold goes through, so it is lent to each in turn.
    let steps = RefCell::new(steps);
    let compiled = fold
        .compile(
            entrypoint,
            loader,
            |stage| steps.borrow_mut().reached(&stage),
            |progress| steps.borrow_mut().told(progress),
        )
        .and_then(|(module, foreigns)| finish(&module).map(|payload| (payload, foreigns)));
    let mut steps = steps.into_inner();

    steps.ended(&compiled);

    compiled.map(|(payload, foreigns)| (payload, foreigns, steps.into_tree()))
}

/// What follows a program's build in its subject's tree, which is what the connector of its last build node says.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Then {
    /// Nothing: the program's own output, or the end of the command.
    Nothing,
    /// More nodes of the same tree: a `write`, a `serve`, the subject's tests.
    More,
}

/// Whose tree a narration is handed back as, once the fold is done.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Closing {
    /// The entry program's, compiled after every unit.
    Program(Then),
    /// The last unit's, where the subject is a library and no program is compiled.
    Library,
}

/// A compile narrated: a tree for each unit the fold compiles, with a `check` node and, where there is a store, a `store` node; and the subject's own tree, whose `check`, `build` and `emit` nodes open and close as the program reaches each stage. A unit the store held narrates nothing.
///
/// The nodes outlive each event, which is why they are kept here rather than written whole: `check` and the `; 1.4s` that closes it are two writes to one line, with the work between them.
pub struct Steps<'a> {
    subject: &'a Subject,
    manifest: Option<&'a Path>,
    /// The store the fold files into, asked once a unit's filing ends whether it took the unit.
    store: Option<&'a Verdicts>,
    closing: Closing,
    /// The unit the fold last compiled and its tree, kept past its last node since a library's is the subject's.
    unit: Option<(Subject, Tree)>,
    program: Option<Tree>,
    /// Whether the program's open node is `emit`, the one a failure ends rather than a refusal.
    emitting: bool,
}

impl<'a> Steps<'a> {
    /// The narration of a program's compile: `subject` is the entry, named beside `manifest` where the invocation stands elsewhere, and `then` what follows its build.
    pub fn program(
        subject: &'a Subject,
        manifest: Option<&'a Path>,
        store: Option<&'a Verdicts>,
        then: Then,
    ) -> Self {
        Self {
            subject,
            manifest,
            store,
            closing: Closing::Program(then),
            unit: None,
            program: None,
            emitting: false,
        }
    }

    /// The narration of a library's compile, where no program is: `subject` is the last unit, and its tree goes on past the fold.
    pub fn library(subject: &'a Subject, store: Option<&'a Verdicts>) -> Self {
        Self {
            subject,
            manifest: None,
            store,
            closing: Closing::Library,
            unit: None,
            program: None,
            emitting: false,
        }
    }

    /// Whether `unit`'s tree goes on past the fold: the library's own does, for what its command writes of it.
    fn goes_on(&self, unit: &Subject) -> bool {
        self.closing == Closing::Library && unit == self.subject
    }

    /// Fold one [`Progress`] event onto the trees, opening and closing nodes as units and the entry begin and end.
    pub fn told(&mut self, progress: Progress<'_>) {
        match progress {
            // A recompile over a baseline reads as a compile: what the node reports is that the unit is being checked and how long it took.
            Progress::Compiling(prefix) | Progress::Recompiling(prefix) => {
                let subject = Subject::Mounted(*prefix);
                let mut tree = Tree::of(&subject, None);

                tree.open("check", self.store.is_none() && !self.goes_on(&subject));
                self.unit = Some((subject, tree));
            }
            // The entry program is the subject, and its compile a tree of its own after every unit's.
            Progress::Entry => {
                let mut tree = Tree::of(self.subject, self.manifest);

                tree.open("check", false);
                self.program = Some(tree);
            }
            // What the store held is not narrated.
            Progress::Reused(_) => {}
            // A unit's check ends with the fold's step; the entry's has ended already, where its lowering began.
            Progress::Compiled => {
                if let Some((_, tree)) = &mut self.unit
                    && tree.is_open()
                {
                    tree.done();
                }
            }
            Progress::Filing => {
                let last = !self
                    .unit
                    .as_ref()
                    .is_some_and(|(subject, _)| self.goes_on(subject));

                if let Some((_, tree)) = &mut self.unit {
                    tree.open("store", last);
                }
            }
            // A store that took nothing says so itself, and why is said once after the fold ([`unstored`]).
            Progress::Filed => {
                let refused = self.store.is_some_and(|store| store.refused().is_some());

                if let Some((_, tree)) = &mut self.unit {
                    match refused {
                        true => tree.verdict("refused"),
                        false => tree.done(),
                    }
                }
            }
        }
    }

    /// Fold a stage the entry program reached onto its tree: its erased form ends `check` and begins `build`, and its optimized form ends `build` and begins `emit`, which lasts until the payload is made.
    pub fn reached(&mut self, stage: &Stage<'_>) {
        let Some(tree) = &mut self.program else {
            return;
        };

        match stage {
            Stage::Anf(_) => {
                tree.done();
                tree.open("build", false);
            }
            Stage::CpsOptm(_) => {
                tree.done();
                tree.open("emit", self.closing == Closing::Program(Then::Nothing));
                self.emitting = true;
            }
            _ => {}
        }
    }

    /// Close the node the compile ended in, with what it took where the compile succeeded, and with how it was stopped otherwise: a count of the goals where they alone stopped it, `failed` where the payload could not be made, `refused` in every other case. A blank line then separates the tree from the reports.
    pub fn ended<T>(&mut self, compiled: &Result<T, CompileError>) {
        let emitting = self.emitting;
        let open = self
            .unit
            .as_mut()
            .map(|(_, tree)| tree)
            .into_iter()
            .chain(self.program.as_mut())
            .find(|tree| tree.is_open());
        let Some(tree) = open else {
            return;
        };

        match compiled {
            Ok(_) => tree.done(),
            Err(error) => {
                match error {
                    CompileError::Incomplete(goals) if goals.len() == 1 => tree.verdict("1 goal"),
                    CompileError::Incomplete(goals) => {
                        tree.verdict(&format!("{} goals", goals.len()));
                    }
                    _ if emitting => tree.verdict("failed"),
                    _ => tree.verdict("refused"),
                }
                eprintln!();
            }
        }
    }

    /// The subject's tree, for the command to go on writing: the one the compile opened, or the subject's with no node yet where nothing of it had to be compiled.
    pub fn into_tree(self) -> Tree {
        let opened = match self.closing {
            Closing::Program(_) => self.program,
            Closing::Library => self
                .unit
                .filter(|(subject, _)| subject == self.subject)
                .map(|(_, tree)| tree),
        };

        opened.unwrap_or_else(|| Tree::of(self.subject, self.manifest))
    }
}
