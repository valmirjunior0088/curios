//! What `curios test` compiles and runs, with nothing it shows: the subjects a selection declares tests in ([`test_subjects`]), each one's test program under the prefixes its tests are selected at or under — taken from a store where nothing it was made from moved, and compiled and filed there otherwise ([`TestSubject::compile`]) — and each of its tests run under a host its caller hands in ([`TestRun::run`]). The command line narrates around these pieces; a caller running a package's tests in process asserts on what they answer.
//!
//! **A subject is one test program.** For the governing package entire it is its library and then each executable, and for a library, a program or a loose file its own. The one unit compiled tested among a subject's is its own ([`RootSource::tested`]), so its program schedules its own tests and nothing it stands on.
//!
//! **The store is consulted exactly as `run` consults it**: one payload per declared subject, filed under a reserved executable name no identifier can spell (it contains `/`), holding the records and every test the subject declares beside the machine code, so a warm run recompiles nothing and still reports everything. The prefixes a payload's tests were selected under are part of what it is verified against, so a run under other prefixes replaces the subject's one payload. A loose subject has no store, so its tests are compiled every time.

use {
    crate::{bindings_of, engine, to_cwasm},
    curios_abi::ForeignStore,
    curios_native::{Compiled, HostOps, TestEnd},
    curios_package::{Entry, LIBRARY, Runs, Selection, Spelling, order},
    curios_pipeline::{Cache, CompileError, Fold, Progress, TestRecord},
    curios_text::{Entrypoint, Form, Overlay, RootSource, UnitSource},
    curios_utilities::{Qualifier, Source},
    curios_verdicts::{Program, Verdicts},
    curios_wonder::{STDIN_LABEL, STDIN_MOUNT, written_as_a_module},
    std::{
        fs,
        path::{Path, PathBuf},
        sync::Arc,
    },
};

/// What stands in for a library's entry text when the payload is keyed. A library is compiled through [`Entrypoint::trivial`], which is built rather than parsed and so has no text of its own; the key has to be *something* constant, and naming it here says which constant and why. The library's own content reaches the address through the unit chain, so nothing depends on this being the program.
const LIBRARY_KEY: &str = "<library>";

/// Which subject a test program is compiled for, as a report names it.
pub enum Named {
    /// A package's library, by the package's name.
    Library(String),
    /// A package's executable, by its name.
    Executable(String),
    /// A file no unit declares, as it was given.
    File(PathBuf),
    /// The program on standard input.
    Stdin,
}

/// Where a declared subject's test payload is filed: the governing root its store sits beside, the package it is filed under and the executable its own declarations are filed as, the name reserved in the package for the payload, and the manifest that declares it.
struct Filing {
    root: PathBuf,
    package: String,
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

/// Every subject `selection` declares tests in, in the order a run takes them: for the governing package entire its library, where it has one, then each executable in declaration order. A loose program is a subject in the form it is written in — a module as a unit of its own, and a program as its entry — and `stdin` is drained once, before its form is known, where the program is standard input.
pub fn test_subjects(
    selection: Selection,
    stdin: impl FnOnce() -> Result<String, CompileError>,
) -> Result<Vec<TestSubject>, CompileError> {
    match selection {
        Selection::Entire(entire) => {
            let governing = &entire.governing;
            let mut subjects = Vec::new();
            if governing.directory.join(LIBRARY).is_file() {
                // The library is tested in its own subject and built in every executable's, each over the dependency graph with the governing package's own library last — the order `wonder` walks and `run` compiles.
                subjects.push(TestSubject::library(
                    order(governing).map_err(CompileError::failure)?,
                    &governing.package.name,
                    &governing.root,
                    &governing.manifest,
                ));
            }
            for program in entire.programs().map_err(CompileError::failure)? {
                subjects.push(TestSubject::declared(program)?);
            }

            Ok(subjects)
        }
        Selection::Library(library) => Ok(vec![TestSubject::library(
            library.units,
            &library.package,
            &library.root,
            &library.manifest,
        )]),
        Selection::Program(program) => match program.home() {
            Some(_) => Ok(vec![TestSubject::declared(program)?]),
            None => match program.entry() {
                Entry::File(path) => {
                    Ok(vec![
                        match RootSource::loose_module(path, &Overlay::default()) {
                            Some(unit) => {
                                TestSubject::loose_module(unit, path, Named::File(path.clone()))
                            }
                            None => TestSubject::loose_program(
                                opened(path)?,
                                path,
                                Named::File(path.clone()),
                            ),
                        },
                    ])
                }
                Entry::Stdin => {
                    let text = stdin()?;
                    // argv[0] is the entry as `run` passes it, which for standard input is the `-` that asked for it.
                    let invoked = Path::new(Spelling::STDIN);
                    Ok(vec![
                        match RootSource::labelled_module(STDIN_MOUNT, STDIN_LABEL, &text) {
                            Some(unit) => TestSubject::loose_module(unit, invoked, Named::Stdin),
                            None => {
                                TestSubject::loose_program(supplied(&text)?, invoked, Named::Stdin)
                            }
                        },
                    ])
                }
            },
        },
    }
}

impl TestSubject {
    /// A library's tests. It has no written entry, so it is compiled through the trivial one: the subject is the scope's last unit, compiled tested, and the tail replacing that entry schedules its tests. [`LIBRARY_KEY`] stands in for the entry text the payload is keyed on.
    fn library(mut units: Vec<RootSource>, package: &str, root: &Path, manifest: &Path) -> Self {
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
                root: root.to_path_buf(),
                package: package.to_string(),
                executable: None,
                reserved: "tests/".to_string(),
                manifest: manifest.to_path_buf(),
            }),
        }
    }

    /// An executable's tests: its entry, opened with the prefixes its manifest declares as a run opens it ([`curios_package::Program::declares`]), its own declarations compiled tested, with the tail scheduling their tests.
    fn declared(program: curios_package::Program) -> Result<Self, CompileError> {
        let home = program.home().expect("a declared program has a home");
        let name = home.executable.clone();
        let filing = Filing {
            root: home.root.clone(),
            package: home.package.clone(),
            executable: Some(home.executable.clone()),
            reserved: format!("tests/{}", home.executable),
            manifest: home.manifest.clone(),
        };
        let declares = program.declares();
        let Entry::File(entry) = program.entry().clone() else {
            unreachable!("a declared program is written in a file");
        };
        let (entrypoint, loader, source) = opened(&entry)?;
        let loader = match declares {
            Some(declares) => loader.declaring(declares),
            None => loader,
        };

        Ok(Self {
            named: Named::Executable(name),
            units: program.into_units(),
            entrypoint,
            loader: loader.tested(),
            text: source.text.clone(),
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
            text: source.text.clone(),
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

    /// The store the subject is filed in, opened by `open` at the governing root, and told the executable its own declarations are filed as where it is one. `None` for a loose subject, which has no project to keep anything for.
    ///
    /// One handle per subject, as `run` holds one per invocation: a handle's placed chain is one compilation's, and a second fold on the same handle would carry the first's placements into the chain the second payload is filed against — one entry too long, which the store withholds without a word.
    pub fn filed(&self, open: impl FnOnce(&Path) -> Option<Verdicts>) -> Option<Verdicts> {
        let filing = self.filing.as_ref()?;
        let store = open(&filing.root)?;

        Some(match &filing.executable {
            Some(executable) => store.executing(&filing.package, executable),
            None => store,
        })
    }

    /// The subject's test program, scheduling its tests at or under any of `under` — from `store` when nothing it was made from has changed and the prefixes are the ones it was filed under, and compiled and filed there otherwise, `progress` told each step of the compile. Without a store, compiled and filed nowhere. A selection meeting none of the subject's tests is filed too, with no machine code, so a second run under it reuses every subject.
    pub fn compile(
        &self,
        budget: u64,
        store: Option<&Verdicts>,
        under: &[Qualifier],
        progress: impl FnMut(Progress<'_>),
    ) -> Result<Tested, CompileError> {
        let sources = self
            .units
            .iter()
            .map(UnitSource::mounted)
            .collect::<Vec<_>>();
        let program = self.filing.as_ref().map(|filing| Program {
            package: &filing.package,
            executable: &filing.reserved,
            entry: &self.entry,
            text: &self.text,
            loader: &self.loader,
            tests: Some(under),
        });
        let runs = Runs::Native { engine: engine() };

        if let Some((store, program)) = store.zip(program.as_ref())
            && let Some((bytes, foreigns)) = store.payload_get(program, &sources, runs)
            && let Some(tested) = decode(&bytes, foreigns)
        {
            return Ok(tested);
        }

        let compiled = Fold::new(budget, &self.units, store.map(|store| store as &dyn Cache))
            .tests(
                &self.entrypoint,
                &self.loader,
                under.to_vec(),
                |_| {},
                progress,
            )?;
        let built = compiled
            .compiled
            .map(|(module, foreigns)| to_cwasm(&module).map(|cwasm| (cwasm, foreigns)))
            .transpose()
            .map_err(CompileError::failure)?;
        let tested = Tested {
            records: compiled.records,
            declared: compiled.declared,
            built,
            reused: false,
        };

        if let Some((store, program)) = store.zip(program.as_ref())
            && let Some(bytes) = encode(&tested)
        {
            let foreigns = tested
                .built
                .as_ref()
                .map(|(_, foreigns)| foreigns.clone())
                .unwrap_or_default();
            store.payload_put(program, &sources, bytes.as_ref(), &foreigns, runs);
        }

        Ok(tested)
    }
}

/// One subject compiled as a test program: the records of the tests it schedules, every test it declares, and the machine code with the rows it imports — `None` where it schedules none — made by [`TestSubject::compile`] alone, in this process or read back from the project's own store, which is what lets [`Tested::loaded`] load its machine code.
pub struct Tested {
    records: Vec<TestRecord>,
    declared: Vec<Qualifier>,
    built: Option<(Vec<u8>, ForeignStore)>,
    reused: bool,
}

impl Tested {
    /// One record per scheduled test, in schedule order: a test's index among them is the one it is run at.
    pub fn records(&self) -> &[TestRecord] {
        &self.records
    }

    /// Every test the subject declares, scheduled or not, by its path: what a selection meeting none of them is answered with.
    pub fn declared(&self) -> &[Qualifier] {
        &self.declared
    }

    /// Whether the program was taken from the store rather than compiled.
    pub fn reused(&self) -> bool {
        self.reused
    }

    /// The program, loaded to run the tests of `subject` it schedules, with what answers each `foreign` row it reaches: `None` where it schedules none, and so was built no further than its judgment.
    pub fn loaded(&self, subject: &TestSubject) -> Result<Option<TestRun>, String> {
        let Some((cwasm, foreigns)) = &self.built else {
            return Ok(None);
        };
        // SAFETY: a `Tested` is made by `TestSubject::compile` alone, from machine code this process precompiled or read back from the project's own store, where a compilation of this compiler filed it.
        let compiled = unsafe { Compiled::deserialize(cwasm, foreigns.clone()) }?;
        // Resolved once for the subject: its manifests are walked and its modules compiled here, and each test is handed the resolution. A run instantiates a module in its own store, so every test still meets an instance of its own, which keeps one test's marks in a module's memory out of the next one's. A loose subject has no package, so nothing declares what would answer a `foreign` of its own, and `bindings_of` refuses one it reaches, saying so.
        let bindings = bindings_of(subject.manifest(), foreigns)?;

        Ok(Some(TestRun {
            compiled,
            bindings,
            // argv[0] is the entry as `run` passes it: the path's bytes, since `/std/Process/args` promises opaque byte strings.
            argv0: subject.entry.as_os_str().as_encoded_bytes().to_vec(),
        }))
    }
}

/// A subject's test program, loaded and bound, each of whose tests runs in an instantiation of its own.
pub struct TestRun {
    compiled: Compiled,
    bindings: curios_native::ForeignBindings,
    argv0: Vec<u8>,
}

impl TestRun {
    /// Run the test at `index` in the schedule under the host `host` makes of the program's arguments, `[argv0, index]`, which is how `Test/main` is told which to run. The guest prints the outcome line of a test it survives; a trap is the error.
    pub fn run<H: HostOps + Send + Sync + 'static>(
        &self,
        index: usize,
        host: impl FnOnce(Vec<Vec<u8>>) -> H,
    ) -> Result<TestEnd, String> {
        let arguments = vec![self.argv0.clone(), index.to_string().into_bytes()];

        self.compiled
            .run_test(host(arguments), self.bindings.clone())
    }
}

/// The entry program written in `path`, what its own modules resolve against, and the text it was parsed from — refused as written as a module where its text is one, rather than as missing the term a program ends in.
pub fn opened(path: &Path) -> Result<(Entrypoint, RootSource, Arc<Source>), CompileError> {
    Entrypoint::opened(path).map_err(|error| match fs::read_to_string(path) {
        // A text written as a module fails the program grammar at its end, where it was never meant to hold a term: say what it is rather than what the grammar expected there.
        Ok(text) if Form::of(path, &text) == Form::Module => {
            CompileError::failure(written_as_a_module(path.display()))
        }
        _ => CompileError::Failure(vec![error.report()]),
    })
}

/// The program `text`, arrived on standard input, parsed — and refused as [`opened`] refuses a file when it is written as a module.
pub fn supplied(text: &str) -> Result<(Entrypoint, RootSource, Arc<Source>), CompileError> {
    Entrypoint::supplied(STDIN_LABEL, text).map_err(|error| {
        match Form::of(Path::new(STDIN_LABEL), text) {
            Form::Module => CompileError::failure(written_as_a_module(STDIN_LABEL)),
            Form::Program => CompileError::Failure(vec![error.report()]),
        }
    })
}

/// What the store files for one test program: its records as `(path, body)` pairs and every test it declares by its path's segments, beside the machine code — none where it schedules no test — one archive as every other artifact in the store is. A corrupt or foreign payload fails to decode and is a miss, never an error.
#[curios_archive::archived]
struct TestPayload {
    records: Vec<(String, String)>,
    declared: Vec<Vec<String>>,
    cwasm: Option<Vec<u8>>,
}

/// `tested` as the bytes the store files — `None` when they will not serialize, on which the record is withheld exactly as a unit's is.
fn encode(tested: &Tested) -> Option<curios_archive::Serialized> {
    let payload = TestPayload {
        records: tested
            .records
            .iter()
            .map(|record| (record.path.clone(), record.body.clone()))
            .collect(),
        declared: tested
            .declared
            .iter()
            .map(|test| test.segments().to_vec())
            .collect(),
        cwasm: tested.built.as_ref().map(|(cwasm, _)| cwasm.clone()),
    };

    curios_archive::to_bytes(&payload).ok()
}

/// The inverse of [`encode`], with the rows the store filed beside the bytes, `None` on any malformation.
fn decode(bytes: &[u8], foreigns: ForeignStore) -> Option<Tested> {
    let payload = curios_archive::from_bytes::<TestPayload>(bytes).ok()?;
    let records = payload
        .records
        .into_iter()
        .map(|(path, body)| TestRecord { path, body })
        .collect();
    let declared = payload.declared.into_iter().map(Qualifier::from).collect();

    Some(Tested {
        records,
        declared,
        built: payload.cwasm.map(|cwasm| (cwasm, foreigns)),
        reused: true,
    })
}
