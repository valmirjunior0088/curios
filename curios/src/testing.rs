//! What `curios test` compiles and runs, with nothing it shows: the subjects a selection declares tests in ([`test_subjects`]), each one's test program under the prefixes its tests are selected at or under, for the target it runs on — taken from a cabinet where nothing it was made from moved, and compiled and filed there otherwise ([`TestSubject::compile`]) — and each of its tests run under a host its caller hands in ([`TestRun::run`]), or the page a browser runs them in ([`Tested::page`]). The command line narrates around these pieces; a caller running a package's tests in process asserts on what they answer.
//!
//! **A subject is one test program.** For the governing package entire it is its library and then each executable, and for a library, a program or a loose file its own. The one unit compiled tested among a subject's is its own ([`RootSource::tested`]), so its program schedules its own tests and nothing it stands on.
//!
//! **The cabinet is consulted exactly as `run` consults it**: one payload per declared subject and target, filed under a reserved executable name no identifier can spell (it contains `/`), holding every test the subject declares, module by module, beside the target's payload, so a warm run recompiles nothing and still reports everything. The prefixes a payload's tests were selected under are part of what it is verified against, so a run under other prefixes replaces the subject's one payload. A loose subject has no cabinet, so its tests are compiled every time.

mod outcome;
pub use outcome::{FailedCheck, Outcome};

use {
    crate::{bindings_of, engine, fault, linked_of, optimize, to_cwasm},
    curios_abi::{ForeignStore, HostOps, TEST_ANSWER},
    curios_browser::page,
    curios_common::{Qualifier, Report, Source, SourceName},
    curios_module::{PackageRoot, RootSource, open_entrypoint, supply_entrypoint},
    curios_native::{Compiled, Stop, TestEnd},
    curios_package::{
        Cabinet, Entry, LIBRARY, Named, Runs, Selection, Spelling, Target, order, tests_filed_as,
    },
    curios_pipeline::{Cache, CompileError, Fold, Progress, Stage, TestModule, fold_sources},
    curios_surface::Entrypoint,
    curios_verdict::{Program, Verdicts},
    std::{
        collections::BTreeMap,
        path::{Path, PathBuf},
        sync::Arc,
    },
};

/// What stands in for a library's entry text when the payload is keyed. A library is compiled through [`Entrypoint::trivial`], which is built rather than parsed and so has no text of its own; the key has to be *something* constant, and naming it here says which constant and why. The library's own content reaches the address through the unit chain, so nothing depends on this being the program.
const LIBRARY_KEY: &str = "<library>";

/// The name a subject's page runs its program by, which is its first argument and the page's title: the package's name for its library, an executable's name, a loose file's stem, and `stdin` for standard input, which a URL could not spell as `-`.
pub fn page_name(named: &Named) -> String {
    match named {
        Named::Library(name) | Named::Executable(name) => name.clone(),
        Named::File(path) => path.file_stem().map_or_else(
            || "program".to_string(),
            |stem| stem.to_string_lossy().into_owned(),
        ),
        Named::Stdin => "stdin".to_string(),
    }
}

/// Where a declared subject's test payload is filed: the cabinet it is filed in, the package it is filed under and the executable its own declarations are filed as, the name reserved in the package for the payload, and the manifest that declares it.
struct Filing {
    cabinet: Cabinet,
    package: String,
    /// The entry, by the name its package gives it: what the payload is verified against.
    entry: SourceName,
    executable: Option<String>,
    reserved: String,
    manifest: PathBuf,
}

/// One subject's test program as it is compiled: the units it stands on, its entry with the loader that entry's `mod` lines resolve through, the text the entry was parsed from and the file it is, and where it is filed, for a subject a package declares.
pub struct TestSubject {
    named: Named,
    units: Vec<RootSource>,
    entrypoint: Entrypoint,
    loader: RootSource,
    text: String,
    entry: PathBuf,
    filing: Option<Filing>,
}

/// Every subject `selection` declares tests in, in the order a run takes them: for the governing package entire its library, where it has one, then each executable in declaration order. A loose module is a unit of its own, and a loose program its entry.
pub fn test_subjects(selection: Selection) -> Result<Vec<TestSubject>, CompileError> {
    // argv[0] is the entry as `run` passes it, which for standard input is the `-` that asked for it.
    let invoked = |entry: &Entry| match entry {
        Entry::File(path) => path.clone(),
        Entry::Stdin(_) => PathBuf::from(Spelling::STDIN),
    };

    match selection {
        Selection::Entire(entire) => {
            let governing = &entire.governing;
            let mut subjects = Vec::new();
            if governing.directory.join(LIBRARY).is_file() {
                // The library is tested in its own subject and built in every executable's, each over the dependency graph with the governing package's own library last — the order `wonder` walks and `run` compiles.
                subjects.push(TestSubject::library(
                    order(governing)?,
                    &governing.package.name,
                    &governing.cabinet,
                    &governing.manifest,
                ));
            }
            for program in entire.programs()? {
                subjects.push(TestSubject::declared(program)?);
            }

            Ok(subjects)
        }
        Selection::Library(library) => Ok(vec![TestSubject::library(
            library.units,
            &library.package,
            &library.cabinet,
            &library.manifest,
        )]),
        Selection::Program(program) => match program.home() {
            Some(_) => Ok(vec![TestSubject::declared(program)?]),
            None => {
                let opened = match program.entry() {
                    Entry::File(path) => opened(path, None)?,
                    Entry::Stdin(stdin) => supplied(&stdin.text)?,
                };

                Ok(vec![TestSubject::loose_program(
                    opened,
                    &invoked(program.entry()),
                    program.named(),
                )])
            }
        },
        Selection::Module(module) => Ok(vec![TestSubject::loose_module(
            module.unit()?,
            &invoked(module.entry()),
            module.named(),
        )]),
    }
}

impl TestSubject {
    /// A library's tests. It has no written entry, so it is compiled through the trivial one: the subject is the scope's last unit, compiled tested, and the tail replacing that entry schedules its tests. [`LIBRARY_KEY`] stands in for the entry text the payload is keyed on.
    fn library(
        mut units: Vec<RootSource>,
        package: &str,
        cabinet: &Cabinet,
        manifest: &Path,
    ) -> Self {
        if let Some(library) = units.pop() {
            units.push(library.tested());
        }

        Self {
            named: Named::Library(package.to_string()),
            units,
            entrypoint: Entrypoint::trivial(),
            loader: RootSource::none(),
            text: LIBRARY_KEY.to_string(),
            entry: manifest.with_file_name(LIBRARY),
            filing: Some(Filing {
                cabinet: cabinet.clone(),
                package: package.to_string(),
                entry: SourceName::of(package, Path::new(LIBRARY))
                    .expect("a package's name and its header's are plain names"),
                executable: None,
                reserved: tests_filed_as(None),
                manifest: manifest.to_path_buf(),
            }),
        }
    }

    /// An executable's tests: its entry, opened with the prefixes its manifest declares as a run opens it ([`curios_package::Program::declares`]), its own declarations compiled tested, with the tail scheduling their tests.
    fn declared(program: curios_package::Program) -> Result<Self, CompileError> {
        let home = program.home().expect("a declared program has a home");
        let name = home.executable.clone();
        let declares = program.declares();
        let Entry::File(entry) = program.entry().clone() else {
            unreachable!("a declared program is written in a file");
        };
        let (entrypoint, loader, source) = opened(&entry, Some(&home.package_root()))?;
        let filing = Filing {
            cabinet: home.cabinet.clone(),
            package: home.package.clone(),
            entry: source
                .name()
                .cloned()
                .expect("a declared program's entry is named by its package"),
            executable: Some(home.executable.clone()),
            reserved: tests_filed_as(Some(&home.executable)),
            manifest: home.manifest.clone(),
        };
        let loader = match declares {
            Some(declares) => loader.declaring(declares),
            None => loader,
        };

        Ok(Self {
            named: Named::Executable(name),
            units: program.into_units(),
            entrypoint,
            loader: loader.tested(),
            text: source.text().to_owned(),
            entry,
            filing: Some(filing),
        })
    }

    /// A loose module's tests: a file, or standard input, mounted as a unit of its own and compiled as a library is, against the prelude alone. `invoked` is the entry as `run` would pass it.
    fn loose_module(unit: RootSource, invoked: &Path, named: Named) -> Self {
        Self {
            named,
            units: vec![unit.tested()],
            entrypoint: Entrypoint::trivial(),
            loader: RootSource::none(),
            text: LIBRARY_KEY.to_string(),
            entry: invoked.to_path_buf(),
            filing: None,
        }
    }

    /// A loose program's tests: its entry, opened from a file or supplied from standard input, compiled against the prelude alone. `invoked` is the entry as `run` passes it.
    fn loose_program(
        (entrypoint, loader, source): (Entrypoint, RootSource, Arc<Source>),
        invoked: &Path,
        named: Named,
    ) -> Self {
        Self {
            named,
            units: Vec::new(),
            entrypoint,
            loader: loader.tested(),
            text: source.text().to_owned(),
            entry: invoked.to_path_buf(),
            filing: None,
        }
    }

    /// Which subject this is, as a report names it.
    pub fn named(&self) -> &Named {
        &self.named
    }

    /// The manifest that declares the subject, for one a package declares: what a report names and its rows are linked from.
    pub fn manifest(&self) -> Option<&Path> {
        self.filing.as_ref().map(|filing| filing.manifest.as_path())
    }

    /// The directory of the cabinet the subject is filed in, for one a package declares: what its package's graph is read out of where its rows are linked.
    fn cabinet(&self) -> Option<&Path> {
        self.filing
            .as_ref()
            .map(|filing| filing.cabinet.directory())
    }

    /// The cabinet the subject is filed in, opened by `open` at the governing root, and told the executable its own declarations are filed as where it is one. `None` for a loose subject, which has no project to keep anything for.
    ///
    /// One handle per subject, as `run` holds one per invocation: a handle's placed chain is one compilation's, and a second fold on the same handle would carry the first's placements into the chain the second payload is filed against — one entry too long, which the cabinet withholds without a word.
    pub fn filed(&self, open: impl FnOnce(&Cabinet) -> Option<Verdicts>) -> Option<Verdicts> {
        let filing = self.filing.as_ref()?;
        let verdicts = open(&filing.cabinet)?;

        Some(match &filing.executable {
            Some(executable) => verdicts.executing(&filing.package, executable),
            None => verdicts,
        })
    }

    /// The subject's test program for `target`, scheduling its tests at or under any of `under` — from `verdicts` when nothing it was made from has changed and the prefixes are the ones it was filed under, and compiled and filed there otherwise, `progress` told each step of the compile. Without a cabinet, compiled and filed nowhere. A selection meeting none of the subject's tests is filed too, with no payload, so a second run under it reuses every subject.
    pub fn compile(
        &self,
        budget: u64,
        verdicts: Option<&Verdicts>,
        under: &[Qualifier],
        target: Target,
        observe: impl FnMut(Stage<'_>),
        progress: impl FnMut(Progress<'_>),
    ) -> Result<Tested, CompileError> {
        let sources = fold_sources(&self.units);
        let program = self.filing.as_ref().map(|filing| Program {
            package: &filing.package,
            executable: &filing.reserved,
            entry: &filing.entry,
            text: &self.text,
            loader: &self.loader,
            tests: Some(under),
        });
        let runs = match target {
            Target::Native => Runs::Native { engine: engine() },
            Target::Browser => Runs::Browser,
        };

        if let Some((verdicts, program)) = verdicts.zip(program.as_ref())
            && let Some((bytes, foreigns)) = verdicts.payload_get(program, &sources, runs)
            && let Some(tested) = decode(&bytes, foreigns, target)
        {
            return Ok(tested);
        }

        let compiled = Fold::new(
            budget,
            &self.units,
            verdicts.map(|verdicts| verdicts as &dyn Cache),
        )
        .tests(
            &self.entrypoint,
            &self.loader,
            under.to_vec(),
            observe,
            progress,
        )?;
        // What the target consumes, as `payload_of` makes it: machine code for the native runtime, and for a browser the module Binaryen wrote.
        let built = compiled
            .compiled
            .map(|(module, foreigns)| match target {
                Target::Native => to_cwasm(&module).map(|cwasm| (Payload::Cwasm(cwasm), foreigns)),
                Target::Browser => Ok((Payload::Module(optimize(&module)), foreigns)),
            })
            .transpose()?;
        let tested = Tested {
            modules: compiled.modules,
            unmatched: compiled.unmatched,
            built,
            reused: false,
        };

        if let Some((verdicts, program)) = verdicts.zip(program.as_ref())
            && let Some(bytes) = encode(&tested)
        {
            let foreigns = tested
                .built
                .as_ref()
                .map(|(_, foreigns)| foreigns.clone())
                .unwrap_or_default();
            verdicts.payload_put(program, &sources, bytes.as_ref(), &foreigns, runs);
        }

        Ok(tested)
    }
}

/// A test program as the target it was compiled for consumes it.
pub enum Payload {
    /// Machine code the native runtime deserializes.
    Cwasm(Vec<u8>),
    /// The module Binaryen wrote, which any browser runs.
    Module(Vec<u8>),
}

impl Payload {
    /// The payload's bytes, whichever it is.
    fn bytes(&self) -> &[u8] {
        match self {
            Payload::Cwasm(bytes) | Payload::Module(bytes) => bytes,
        }
    }

    /// `bytes` as the payload `target` consumes.
    fn of(target: Target, bytes: Vec<u8>) -> Self {
        match target {
            Target::Native => Payload::Cwasm(bytes),
            Target::Browser => Payload::Module(bytes),
        }
    }
}

/// One subject compiled as a test program for one target: every test it declares, module by module, scheduled or skipped, and its payload with the rows it imports — `None` where it schedules none — made by [`TestSubject::compile`] alone, in this process or read back from the project's own cabinet, which is what lets [`Tested::loaded`] load its machine code.
pub struct Tested {
    modules: Vec<TestModule>,
    unmatched: Vec<Qualifier>,
    built: Option<(Payload, ForeignStore)>,
    reused: bool,
}

impl Tested {
    /// Every test the subject declares, module by module, in the order its program runs them.
    pub fn modules(&self) -> &[TestModule] {
        &self.modules
    }

    /// The path of each scheduled test, in schedule order: a test's index among them is the one it is run at.
    pub fn scheduled(&self) -> impl Iterator<Item = &Qualifier> {
        self.modules.iter().flat_map(|module| &module.scheduled)
    }

    /// Each prefix the program was selected under that selects none of the subject's tests.
    pub fn unmatched(&self) -> &[Qualifier] {
        &self.unmatched
    }

    /// Every test the subject declares, scheduled or not, by its path: the paths a selection meeting none of them is answered with the nearest of.
    pub fn declared(&self) -> impl Iterator<Item = &Qualifier> {
        self.modules
            .iter()
            .flat_map(|module| module.scheduled.iter().chain(&module.skipped))
    }

    /// Whether the program was taken from the cabinet rather than compiled.
    pub fn reused(&self) -> bool {
        self.reused
    }

    /// The program, compiled for the native target, loaded to run the tests of `subject` it schedules, with what answers each `foreign` row it reaches: `None` where it schedules none, and so was built no further than its judgment.
    pub fn loaded(&self, subject: &TestSubject) -> Result<Option<TestRun>, Box<Report>> {
        let Some((payload, foreigns)) = &self.built else {
            return Ok(None);
        };
        let Payload::Cwasm(cwasm) = payload else {
            unreachable!("a test program is loaded for the target it was compiled for");
        };
        // SAFETY: a `Tested` is made by `TestSubject::compile` alone, from machine code this process precompiled or read back from the project's own cabinet, where a compilation of this compiler filed it.
        let compiled = unsafe { Compiled::deserialize(cwasm, foreigns.clone()) }
            .map_err(|error| fault("the program's machine code could not be read back", error))?;
        // Resolved once for the subject: its manifests are walked and its modules compiled here, and each test is handed the resolution. A run instantiates a module in its own store, so every test still meets an instance of its own, which keeps one test's marks in a module's memory out of the next one's. A loose subject has no package, so nothing declares what would answer a `foreign` of its own, and `bindings_of` refuses one it reaches, saying so.
        let bindings = bindings_of(subject.manifest(), foreigns, subject.cabinet())?;

        Ok(Some(TestRun {
            compiled,
            bindings,
            // argv[0] is the entry as `run` passes it: the path's bytes, since `/std/Proc/args` promises opaque byte strings.
            argv0: subject.entry.as_os_str().as_encoded_bytes().to_vec(),
        }))
    }

    /// The program, compiled for a browser, laid out as the page that runs the tests of `subject` it schedules, every file by its page path, its rows linked to the gateways `subject`'s packages declare: `None` where it schedules none. A reached row no gateway answers is refused as `compile` refuses it, naming the row and the table.
    pub fn page(
        &self,
        subject: &TestSubject,
    ) -> Result<Option<BTreeMap<String, Vec<u8>>>, Box<Report>> {
        let Some((payload, foreigns)) = &self.built else {
            return Ok(None);
        };
        let Payload::Module(module) = payload else {
            unreachable!("a test program is laid out for the target it was compiled for");
        };
        let linked = linked_of(subject.manifest(), foreigns, subject.cabinet())?;

        page(
            curios_browser::Program {
                name: &page_name(&subject.named),
                package: subject
                    .filing
                    .as_ref()
                    .map(|filing| filing.package.as_str()),
                module: module.clone(),
                foreigns,
                answer: Some(&TEST_ANSWER),
            },
            linked,
        )
        .map(Some)
    }
}

/// A subject's test program, loaded and bound, each of whose tests runs in an instantiation of its own.
pub struct TestRun {
    compiled: Compiled,
    bindings: curios_native::ForeignBindings,
    argv0: Vec<u8>,
}

impl TestRun {
    /// Run the test at `index` in the schedule under the host `host` makes of the program's arguments, `[argv0, index]`, which is how `Test/main` is told which to run. A test answers what it found, which [`Outcome::read`] reads; a trap is the error.
    pub fn run<H: HostOps + Send + Sync + 'static>(
        &self,
        index: usize,
        host: impl FnOnce(Vec<Vec<u8>>) -> H,
    ) -> Result<TestEnd, Stop> {
        let arguments = vec![self.argv0.clone(), index.to_string().into_bytes()];

        self.compiled
            .run_test(host(arguments), self.bindings.clone())
    }
}

/// The entry program written in `path`, what its own modules resolve against, and the text it was parsed from — refused, where the program grammar does not read it, in the grammar's words: a text admitted as a program is one by its declaration or by the term that closes it, so one that does not parse is a program written wrong.
pub fn opened(
    path: &Path,
    package: Option<&PackageRoot>,
) -> Result<(Entrypoint, RootSource, Arc<Source>), CompileError> {
    open_entrypoint(path, package).map_err(|error| CompileError::Failure(vec![error.report()]))
}

/// The program `text`, arrived on standard input, parsed — and refused as [`opened`] refuses a file.
pub fn supplied(text: &str) -> Result<(Entrypoint, RootSource, Arc<Source>), CompileError> {
    supply_entrypoint(text).map_err(|error| CompileError::Failure(vec![error.report()]))
}

/// What the cabinet files for one test program: its modules and each prefix it was selected under that selects nothing, by their segments, beside the target's payload — none where it schedules no test — one archive as every other artifact in the cabinet is, under the target's own payload directory, which says what the bytes are. A corrupt or foreign payload fails to decode and is a miss, never an error.
#[curios_archive::archived]
struct TestPayload {
    modules: Vec<ModulePayload>,
    unmatched: Vec<Vec<String>>,
    payload: Option<Vec<u8>>,
}

/// One module's tests as the cabinet files them: the segments of its path, of each scheduled test's path and of each skipped test's.
#[curios_archive::archived]
struct ModulePayload {
    path: Vec<String>,
    scheduled: Vec<Vec<String>>,
    skipped: Vec<Vec<String>>,
}

/// `tested` as the bytes the cabinet files — `None` when they will not serialize, on which the record is withheld exactly as a unit's is.
fn encode(tested: &Tested) -> Option<curios_archive::Serialized> {
    let payload = TestPayload {
        modules: tested
            .modules
            .iter()
            .map(|module| ModulePayload {
                path: module.path.segments().to_vec(),
                scheduled: module
                    .scheduled
                    .iter()
                    .map(|test| test.segments().to_vec())
                    .collect(),
                skipped: module
                    .skipped
                    .iter()
                    .map(|test| test.segments().to_vec())
                    .collect(),
            })
            .collect(),
        unmatched: tested
            .unmatched
            .iter()
            .map(|prefix| prefix.segments().to_vec())
            .collect(),
        payload: tested
            .built
            .as_ref()
            .map(|(payload, _)| payload.bytes().to_vec()),
    };

    curios_archive::to_bytes(&payload).ok()
}

/// The inverse of [`encode`], with the rows the cabinet filed beside the bytes and the target whose directory they were filed in, `None` on any malformation.
fn decode(bytes: &[u8], foreigns: ForeignStore, target: Target) -> Option<Tested> {
    let payload = curios_archive::from_bytes::<TestPayload>(bytes).ok()?;
    let modules = payload
        .modules
        .into_iter()
        .map(|module| TestModule {
            path: Qualifier::from(module.path),
            scheduled: module.scheduled.into_iter().map(Qualifier::from).collect(),
            skipped: module.skipped.into_iter().map(Qualifier::from).collect(),
        })
        .collect();

    Some(Tested {
        modules,
        unmatched: payload.unmatched.into_iter().map(Qualifier::from).collect(),
        built: payload
            .payload
            .map(|bytes| (Payload::of(target, bytes), foreigns)),
        reused: true,
    })
}
