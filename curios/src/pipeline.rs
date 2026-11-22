//! Driving the compile pipeline for the CLI, from an admitted program to the payload a target consumes — natively the `.cwasm` both subcommands run or bundle, and for a browser the module Binaryen wrote — including the cabinet consultation that can skip the whole thing. Observing a stage is not here: that is a question about a program, and questions are `wonder`'s.
//!
//! **The payload, not the wasm module, is what this hands back**, and that is what lets one stored artifact serve `run` and `compile` alike: `run` executes it in-process exactly as it executes a fresh one, and `compile` appends it to the embedded launcher or writes it beside a page. Optimization and precompilation therefore happen here rather than in `main`, which is left dispatching.

use {
    crate::{Piece, Step, Subject, Tree, apart, planned, settle, settled},
    curios::{engine, opened, optimize, supplied, to_cwasm},
    curios_abi::ForeignStore,
    curios_common::{OsError, Qualifier, Report, STDIN_NAME, Source, Style},
    curios_document::Documentation,
    curios_module::{Compiled, PackageRoot, RootSource},
    curios_package::{Entry, Home, Named, Program, Refused, Runs, Target},
    curios_pipeline::{Cache, CompileError, Fold, Progress, Stage, fold_sources},
    curios_surface::Entrypoint,
    curios_verdict::Verdicts,
    curios_wasm::Module,
    curios_wonder::{Ended, Telling},
    std::{
        cell::RefCell,
        io, mem,
        path::{Path, PathBuf},
        sync::{
            Arc,
            atomic::{AtomicBool, Ordering},
        },
    },
};

/// The payload `program` is built into for `target`, taken from `cache` when nothing it was made from has changed, and compiled — and filed there — otherwise: natively the precompiled `.cwasm`, filed under the engine that will run it, and for a browser the module Binaryen wrote, which any browser runs. `cache` is the cabinet its command opened for it to file into, and `None` compiles everything and files nothing.
///
/// The scope is the program's own dependency graph and nothing else: a manifest is the only thing that says what a unit is compiled against. The error keeps the incomplete/failure split so `main` can map a goal batch to its own exit code.
///
/// `then` is what the command goes on to write in the subject's tree, which comes back with the payload for it to do so: a payload the cabinet held narrates nothing, and its tree is the subject's with no node yet.
pub fn payload_of(
    budget: u64,
    program: Program,
    cache: Option<Verdicts>,
    target: Target,
    then: Then,
) -> Result<Built, CompileError> {
    let subject = Subject::of(&program.named());
    let runs = match target {
        Target::Native => Runs::Native { engine: engine() },
        Target::Browser => Runs::Browser,
    };

    let entry = program.entry().clone();
    let declared = program
        .home()
        .map(|home| (home.package.clone(), home.executable.clone()));
    let declares = program.declares();
    let manifest = program.home().map(|home| home.manifest.clone());
    let package = program.home().map(Home::package_root);
    let units = program.into_units();

    // Opened before the cabinet is consulted, because the entry's own text is half of what a stored payload is verified against — and it has to be the text that was *parsed*, not a re-read taken afterwards.
    let (entrypoint, loader, source) = open(&entry, package.as_ref())?;
    // What the manifest declared, onto the resolver the entry's own names go through. A loose program has no manifest, so it declares nothing and sees every open prefix — which is the whole scope it was given, since nothing mounted anything beside the prelude.
    let loader = match declares {
        Some(declares) => loader.declaring(declares),
        None => loader,
    };

    // A payload is filed only where all three exist: a cabinet to put it in, a declared name to file it under, and an entry its package names to verify it against. Standard input has none of them, and reaches this as the `None` that skips both the get and the put.
    let filed = cache
        .as_ref()
        .zip(declared.as_ref())
        .zip(source.name())
        .map(|((cache, name), entry)| {
            (
                cache,
                curios_verdict::Program {
                    package: &name.0,
                    executable: &name.1,
                    entry,
                    text: source.text(),
                    loader: &loader,
                    tests: None,
                },
            )
        });

    // The sources the fold compiles, handed to both halves of the payload family, which is what keeps the three agreeing about which units the compilation holds and where each is filed: the probe refuses a unit the cabinet may not file, and the write must refuse the same one.
    let sources = fold_sources(&units);

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
        Steps::program(&subject, manifest.as_deref(), then),
        cache.as_ref(),
        |module| match target {
            Target::Native => to_cwasm(module).map_err(CompileError::from),
            Target::Browser => Ok(optimize(module)),
        },
    );

    if let (Ok((payload, foreigns, _)), Some((cache, program))) = (&compiled, &filed) {
        cache.payload_put(program, &sources, payload, foreigns, runs);
    }

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

/// The interface of the last of `units` — a package's library, compiled against everything before it — made from the compilation that builds it, whose units are filed into `verdicts` when there is one.
///
/// **A build, where the `wonder` engine's reading of the same record is a question.** A library documented is a library compiled, and what was compiled is worth what `run` and `test` keep of theirs; the engine's `documentation` files the units a build would have and writes no pages, which is as far as a question goes. The compilation runs to completion first, the certifier included, so a library that does not check is not documented and reports what stopped it exactly as `run` would.
///
/// The library is `subject`, whose tree comes back with the record for the command to close with where it wrote the pages.
pub fn documentation_of(
    budget: u64,
    units: &[RootSource],
    verdicts: Option<&Verdicts>,
    subject: &Subject,
) -> Result<(Documentation, Tree), CompileError> {
    let mut steps = Steps::library(subject);
    let documented = Fold::new(
        budget,
        units,
        verdicts.map(|verdicts| verdicts as &dyn Cache),
    )
    .documentation(|progress| steps.told(progress, verdicts));

    steps.ended(&documented);

    documented.map(|documentation| (documentation, steps.into_tree()))
}

/// The entry program, what its own modules resolve against, and the text it was parsed from: a file, read for `package` where a package declares it, or what standard input held where its subject was admitted.
pub fn open(
    entry: &Entry,
    package: Option<&PackageRoot>,
) -> Result<(Entrypoint, RootSource, Arc<Source>), CompileError> {
    match entry {
        Entry::File(path) => opened(path, package),
        Entry::Stdin(stdin) => supplied(&stdin.text),
    }
}

/// Standard input, drained to end — once, by admission, since a second read finds nothing. The program's own standard input is gone once the compiler has read the source out of it, so a read of it reports the end of the input: unavoidable when both want one descriptor, and the reason a program that reads its input belongs in a file.
pub fn drained() -> Result<String, Box<Report>> {
    io::read_to_string(io::stdin()).map_err(|error| {
        Refused::of("unreadable input")
            .stating("standard input could not be read")
            .stating(OsError(&error))
            .about_named(STDIN_NAME)
    })
}

/// Compile `entrypoint` against `units` in the order given, and make the payload `finish` makes of the module, narrated by `steps` beside `verdicts`, the one the fold files into, whose subject's tree comes back with the payload.
///
/// The program's last build node stays open until `finish` returns, so what it reports is what the payload took: the optimized program emitted and compiled to what a run executes.
pub fn compile_entry<P>(
    fold: Fold<'_>,
    entrypoint: &Entrypoint,
    loader: &RootSource,
    steps: Steps,
    verdicts: Option<&Verdicts>,
    finish: impl FnOnce(&Module) -> Result<P, CompileError>,
) -> Result<(P, ForeignStore, Tree), CompileError> {
    // Two callbacks write one narration, the stages a program reaches and the units the fold goes through, so it is lent to each in turn.
    let steps = RefCell::new(steps);
    let compiled = fold
        .compile(
            entrypoint,
            loader,
            |stage| steps.borrow_mut().reached(&stage),
            |progress| steps.borrow_mut().told(progress, verdicts),
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

/// What a narration makes of its subject beyond the units it stands on.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Closing {
    /// The entry program, compiled after every unit and built: its `check`, `build` and `emit`, and what follows them.
    Program(Then),
    /// The entry program, checked and built no further: its `check` is the last node of its tree.
    Checked,
    /// Nothing: only the units are narrated, where a question reprints the program or reads it rather than checks it.
    Units,
    /// The last unit, where the subject is a library and no program is compiled: its tree goes on past the fold.
    Library,
}

/// How a narrated compile ended.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum How {
    /// It reached its end.
    Done,
    /// Written goals alone stopped it, this many.
    Goals(usize),
    /// It was refused, or, where what could not be done was the payload, failed.
    Stopped,
}

impl How {
    /// How a compile that came to `compiled` ended.
    fn of<T>(compiled: &Result<T, CompileError>) -> Self {
        match compiled {
            Ok(_) => How::Done,
            Err(CompileError::Incomplete(goals)) => How::Goals(goals.len()),
            Err(_) => How::Stopped,
        }
    }
}

/// Whether a command has said why its cabinet took nothing: said once, beneath the first `store` node the cabinet refused.
static WHY_UNSTORED: AtomicBool = AtomicBool::new(false);

/// A compile narrated: a tree for each unit the fold compiles, with a `check` node and, where there is a cabinet, a `store` node; and the subject's own tree, whose `check`, `build` and `emit` nodes open and close as the program reaches each stage. A unit the cabinet held narrates nothing.
///
/// The nodes outlive each event, which is why they are kept here rather than written whole: `check` and the `; 1.4s` that closes it are two writes to one line, with the work between them.
pub struct Steps {
    subject: Subject,
    manifest: Option<PathBuf>,
    closing: Closing,
    /// The unit the fold last compiled and its tree, kept past its last node since a library's is the subject's.
    unit: Option<(Subject, Tree)>,
    program: Option<Tree>,
    /// Whether the program's open node is `emit`, the one a failure ends rather than a refusal.
    emitting: bool,
}

impl Steps {
    fn new(subject: &Subject, manifest: Option<&Path>, closing: Closing) -> Self {
        Self {
            subject: subject.clone(),
            manifest: manifest.map(Path::to_path_buf),
            closing,
            unit: None,
            program: None,
            emitting: false,
        }
    }

    /// The narration of a program's compile: `subject` is the entry, named beside `manifest` where the invocation stands elsewhere, and `then` what follows its build.
    pub fn program(subject: &Subject, manifest: Option<&Path>, then: Then) -> Self {
        Self::new(subject, manifest, Closing::Program(then))
    }

    /// The narration of a library's compile, where no program is: `subject` is the last unit, and its tree goes on past the fold.
    pub fn library(subject: &Subject) -> Self {
        Self::new(subject, None, Closing::Library)
    }

    /// Whether `unit`'s tree goes on past the fold: the library's own does, for what its command writes of it.
    fn goes_on(&self, unit: &Subject) -> bool {
        self.closing == Closing::Library && *unit == self.subject
    }

    /// The tree of `unit`, where it is the unit the fold took on: an event names the unit it is of, so it is drawn on that unit's tree and on no other.
    fn tree_of(&mut self, unit: &Qualifier) -> Option<&mut Tree> {
        self.unit
            .as_mut()
            .filter(|(subject, _)| *subject == Subject::Mounted(*unit))
            .map(|(_, tree)| tree)
    }

    /// Fold one [`Progress`] event onto the trees, opening and closing nodes as units and the entry begin and end. `verdicts` is the one the fold files into, which says whether a unit has a `store` node to come and, once its filing ends, whether the cabinet took it.
    pub fn told(&mut self, progress: Progress<'_>, verdicts: Option<&Verdicts>) {
        match progress {
            Progress::Planned { units } => planned(units),
            // A recompile over a baseline reads as a compile: what the node reports is that the unit is being checked and how long it took.
            Progress::Compiling {
                unit: prefix,
                compiled,
            }
            | Progress::Recompiling {
                unit: prefix,
                compiled,
            } => {
                let subject = Subject::Mounted(*prefix);
                let mut tree = Tree::of(&subject, None);
                // A question compiles its subject with its tests and then again as what stands on it is built: the first is checked as that, which tells the two trees of one unit apart.
                let check = Step::Check {
                    tested: matches!(compiled, Compiled::Tested),
                };

                tree.open(check, verdicts.is_none() && !self.goes_on(&subject));
                self.unit = Some((subject, tree));
            }
            // The entry program is the subject, and its compile a tree of its own after every unit's. A build checks it whether or not the cabinet held its declarations, since it is erased and linked either way; a question that only checks it has nothing to narrate of one the cabinet held.
            Progress::Entry { held } => {
                let last = match (self.closing, held) {
                    (Closing::Program(_), _) => false,
                    (Closing::Checked, false) => true,
                    (Closing::Checked, true) | (Closing::Units | Closing::Library, _) => return,
                };
                let mut tree = Tree::of(&self.subject, self.manifest.as_deref());

                tree.open(Step::Check { tested: false }, last);
                self.program = Some(tree);
            }
            // What the cabinet held is not narrated, and is settled.
            Progress::Reused(_) => settled(),
            // How far a check has come is the open check's own: a mounted unit's, or the program's, whose declarations the entry's empty prefix tells of.
            Progress::Walked { unit, done, of, on } => {
                let tree = match unit.is_root() {
                    true => self.program.as_mut(),
                    false => self.tree_of(unit),
                };
                if let Some(tree) = tree
                    && tree.is_open()
                {
                    tree.walked(done, of, on.map(|name| name.faithful().to_string()));
                }
            }
            // A unit's check ends with the fold's step, which settles it; the entry's has ended already, where its lowering began.
            Progress::Compiled { unit } => {
                if let Some(tree) = self.tree_of(unit)
                    && tree.is_open()
                {
                    tree.done();
                    settled();
                }
            }
            Progress::Filing { unit } => {
                let last = !self.goes_on(&Subject::Mounted(*unit));

                if let Some(tree) = self.tree_of(unit) {
                    tree.open(Step::Store, last);
                }
            }
            // A cabinet that took nothing says so at its node, and why beneath the first node it refused: a cabinet nobody can write refuses every unit for one reason, so the reason is one line however many units went past it.
            Progress::Filed { unit } => {
                let refusal = verdicts.and_then(Verdicts::refused);

                if let Some(tree) = self.tree_of(unit) {
                    match refusal {
                        Some(reason) => {
                            let why = (!WHY_UNSTORED.swap(true, Ordering::Relaxed))
                                .then(|| Piece::from(reason));
                            tree.ending_beneath("refused", why.into_iter().collect());
                        }
                        None => tree.done(),
                    }
                }
            }
        }
    }

    /// Fold a stage the entry program reached onto its tree: its erased form ends `check` and begins `build`, and its optimized form ends `build` and begins `emit`, which lasts until the payload is made. A program that is only checked has neither.
    pub fn reached(&mut self, stage: &Stage<'_>) {
        let (Closing::Program(then), Some(tree)) = (self.closing, &mut self.program) else {
            return;
        };

        match stage {
            Stage::Anf(_) => {
                tree.done();
                tree.open(Step::Build, false);
            }
            Stage::CpsOptm(_) => {
                tree.done();
                tree.open(Step::Emit, then == Then::Nothing);
                self.emitting = true;
            }
            _ => {}
        }
    }

    /// Close the node the compile ended in, where one is open, and answer whether one was: with what it took where the compile reached its end, with the count of the goals where they alone stopped it, as `failed` where the payload could not be made, and as `refused` in every other case.
    pub fn closed(&mut self, how: How) -> bool {
        let emitting = self.emitting;
        let open = self
            .unit
            .as_mut()
            .map(|(_, tree)| tree)
            .into_iter()
            .chain(self.program.as_mut())
            .find(|tree| tree.is_open());
        let Some(tree) = open else {
            return false;
        };

        match how {
            How::Done => tree.done(),
            How::Goals(1) => tree.ending("1 goal"),
            How::Goals(goals) => tree.ending(&format!("{goals} goals")),
            How::Stopped if emitting => tree.ending("failed"),
            How::Stopped => tree.ending("refused"),
        }

        true
    }

    /// [`Steps::closed`] by what a build's compile came to, a blank line then parting the tree from the reports of what stopped it.
    pub fn ended<T>(&mut self, compiled: &Result<T, CompileError>) {
        if self.closed(How::of(compiled)) && compiled.is_err() {
            apart();
        }
    }

    /// The subject's tree, for the command to go on writing: the one the compile opened, or the subject's with no node yet where nothing of it had to be compiled.
    pub fn into_tree(self) -> Tree {
        let opened = match self.closing {
            Closing::Program(_) | Closing::Checked => self.program,
            Closing::Units => None,
            Closing::Library => self
                .unit
                .filter(|(subject, _)| *subject == self.subject)
                .map(|(_, tree)| tree),
        };

        opened.unwrap_or_else(|| Tree::of(&self.subject, self.manifest.as_deref()))
    }
}

/// A question narrated, as [`Telling`] tells it: each subject's compile as a build's is, the units it had to compile and, where the question checks a program, the program's own `check`; a blank line ahead of the answer where anything was narrated; and, after the answer, why the cabinet took nothing where it could not.
pub struct Asking {
    /// Whether the question checks a program, and so narrates its check. A rung or a cost reprints the program, and a question about its names reads it: those narrate what it stands on and never the program.
    checks: bool,
    /// The narration of the subject in hand.
    steps: Option<Steps>,
    /// Whether anything was narrated that no blank line has yet parted from what follows.
    narrated: bool,
    /// Whoever reads the answer, on standard output.
    answered: Style,
}

impl Asking {
    /// The narration of a question that checks what it is asked about, its answer written for `answered`.
    pub fn checking(answered: Style) -> Self {
        Self {
            checks: true,
            steps: None,
            narrated: false,
            answered,
        }
    }

    /// The narration of a question that reprints or reads the program it is asked about, and so narrates only the units the program stands on.
    pub fn reading(answered: Style) -> Self {
        Self {
            checks: false,
            ..Self::checking(answered)
        }
    }

    /// Part what was narrated from what follows it, once, the live region cleared before the answer is written past it.
    fn parted(&mut self) {
        if mem::take(&mut self.narrated) {
            apart();
        }
        settle();
    }
}

impl Telling for Asking {
    fn asked(&mut self, named: &Named, manifest: Option<&Path>) {
        let closing = match (self.checks, named) {
            // A library is the last of its units, narrated as each of them is.
            (_, Named::Library(_)) | (false, _) => Closing::Units,
            (true, _) => Closing::Checked,
        };

        self.steps = Some(Steps::new(&Subject::of(named), manifest, closing));
    }

    fn step(&mut self, progress: Progress<'_>, verdicts: Option<&Verdicts>) {
        let Some(steps) = &mut self.steps else {
            return;
        };

        match progress {
            Progress::Compiling { .. } | Progress::Recompiling { .. } => self.narrated = true,
            // A program that is not narrated begins once every unit it stands on is compiled, and what a question prints of it follows from there.
            Progress::Entry { .. } if steps.closing == Closing::Units => {
                if mem::take(&mut self.narrated) {
                    apart();
                }
                settle();
            }
            Progress::Entry { held: false } => self.narrated = true,
            _ => {}
        }
        steps.told(progress, verdicts);
    }

    fn ended(&mut self, ended: Ended) {
        if let Some(mut steps) = self.steps.take() {
            steps.closed(match ended {
                Ended::Done => How::Done,
                Ended::Goals(goals) => How::Goals(goals),
                Ended::Refused => How::Stopped,
            });
        }
    }

    fn answering(&mut self) {
        self.parted();
    }

    fn reader(&self) -> Style {
        self.answered.clone()
    }
}
