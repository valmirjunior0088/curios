//! `curios test`: every declared test of what the target selects — for the governing package entire its library's and then each executable's, and for a library, a program or a loose file its own — each unit compiled as a test program, the synthesized `Test/main` tail in place of the authored one, and every registered test run in an instantiation of its own, `[argv0, index]` as the program's arguments. The guest prints its own outcome line; what only the compiler knows — the failing declaration's body as written, the count line — is printed here from the records the compile hands back.
//!
//! The store is consulted exactly as `run` consults it: one payload per declared target, filed under a reserved executable name no identifier can spell (it contains `/`), holding the records beside the machine code so a warm run recompiles nothing and still reports everything. A loose program has no store, so its tests are compiled every time.

#[cfg(test)]
mod tests;

use {
    crate::{
        Access, Heading, Line, Subject, drained, fact, open, processing, report, step, supplied,
    },
    curios::{engine, to_cwasm},
    curios_abi::ForeignStore,
    curios_package::{Entry, LIBRARY, Selection, Spelling, order},
    curios_pipeline::{Cache, CompileError, EntryTail, Fold, TestRecord},
    curios_runtime::{OsHost, run_bytes},
    curios_text::{Entrypoint, Overlay, RootSource, UnitSource},
    curios_utilities::Source,
    curios_verdicts::{Program, Verdicts},
    curios_wonder::{STDIN_LABEL, STDIN_MOUNT},
    std::{path::Path, rc::Rc},
};

/// What stands in for a library's entry text when the payload is keyed. A library is compiled through [`Entrypoint::trivial`], which is built rather than parsed and so has no text of its own; the key has to be *something* constant, and naming it here says which constant and why. The library's own content reaches the address through the unit chain, so nothing depends on this being the program.
const LIBRARY_KEY: &str = "<library>";

/// The tally a `↳ Tested` step and the count line report, in the order they spell it.
#[derive(Default)]
struct Totals {
    passed: usize,
    failed: usize,
    trapped: usize,
    exited: usize,
}

impl Totals {
    fn all_green(&self) -> bool {
        self.failed == 0 && self.trapped == 0 && self.exited == 0
    }

    fn add(&mut self, other: &Self) {
        self.passed += other.passed;
        self.failed += other.failed;
        self.trapped += other.trapped;
        self.exited += other.exited;
    }

    /// The tally: `passed` and `failed` always, since that pair is what the exit code turns on, and the rarer `trapped` and `exited` only when they happened.
    fn line(&self) -> String {
        let mut parts = vec![
            format!("{} passed", self.passed),
            format!("{} failed", self.failed),
        ];
        for (count, word) in [(self.trapped, "trapped"), (self.exited, "exited")] {
            if count > 0 {
                parts.push(format!("{count} {word}"));
            }
        }

        parts.join(", ")
    }
}

/// Run the tests `selection` declares, optionally narrowed to those at or under the path `filter`, each declared target filing what it compiled into a store `access` opens. `Ok(true)` when every selected test passed or proved.
pub(crate) fn run_tests(
    budget: u64,
    selection: Selection,
    access: Access,
    filter: Option<&str>,
) -> Result<bool, CompileError> {
    let mut run = Run {
        budget,
        access,
        filter,
        totals: Totals::default(),
        matched_any: false,
        refusal: None,
    };

    match selection {
        Selection::Entire(entire) => {
            let governing = entire.governing;
            let declared = Declared {
                package: &governing.package.name,
                root: &governing.root,
                manifest: &governing.manifest,
            };
            // The same scope for every target: the dependency graph, with the governing package's own library last — the order `wonder` walks and `run` compiles.
            let units = order(&governing).map_err(CompileError::failure)?;

            // The library first, when there is one, then every executable in declaration order — each a test program of its own, scheduling only its own unit's tests.
            if governing.directory.join(LIBRARY).is_file() {
                run.library(&units, &declared)?;
            }
            for executable in &governing.package.executables {
                let entry = governing.directory.join(&executable.path);
                run.executable(&units, &entry, &executable.name, &declared)?;
            }
        }
        Selection::Library(library) => {
            let declared = Declared {
                package: &library.package,
                root: &library.root,
                manifest: &library.manifest,
            };
            run.library(&library.units, &declared)?;
        }
        Selection::Program(program) => match program.home() {
            Some(home) => {
                let package = home.package.clone();
                let root = home.root.clone();
                let manifest = home.manifest.clone();
                let executable = home.executable.clone();
                let Entry::File(entry) = program.entry().clone() else {
                    unreachable!("a declared program is written in a file");
                };
                let declared = Declared {
                    package: &package,
                    root: &root,
                    manifest: &manifest,
                };
                run.executable(&program.into_units(), &entry, &executable, &declared)?;
            }
            // A loose program is tested in the form it is written in: a module as a unit of its own, and a program as its entry. Standard input is drained once, before its form is known, since either reading needs the whole text.
            None => match program.entry() {
                Entry::File(path) => {
                    let subject = Subject::File(path.clone());
                    match RootSource::loose_module(path, &Overlay::default()) {
                        Some(unit) => run.loose_module(unit, path, &subject)?,
                        None => run.loose_program(open(Some(path))?, path, &subject)?,
                    }
                }
                Entry::Stdin => {
                    let text = drained()?;
                    // argv[0] is the entry as `run` passes it, which for standard input is the `-` that asked for it.
                    let invoked = Path::new(Spelling::STDIN);
                    match RootSource::labelled_module(STDIN_MOUNT, STDIN_LABEL, &text) {
                        Some(unit) => run.loose_module(unit, invoked, &Subject::Stdin)?,
                        None => run.loose_program(supplied(&text)?, invoked, &Subject::Stdin)?,
                    }
                }
            },
        },
    }

    run.finish()
}

/// Where a declared target's test payload is filed and whom it is reported under: the package it is filed under, the governing root its store sits beside, and the manifest a report names.
struct Declared<'a> {
    package: &'a str,
    root: &'a Path,
    manifest: &'a Path,
}

/// One `curios test` invocation as it goes: how each target is compiled, which of its tests run, and what has been tallied.
struct Run<'a> {
    budget: u64,
    access: Access,
    filter: Option<&'a str>,
    totals: Totals,
    matched_any: bool,
    /// The first reason a store refused filing, since a store nobody can write refuses every target for one reason.
    refusal: Option<String>,
}

impl Run<'_> {
    /// A library's tests. It has no written entry, so it is compiled through the trivial one: the subject is the scope's final unit, and `EntryTail::LastUnitTests` replaces that entry with the tail scheduling the unit's tests. `LIBRARY_KEY` stands in for the entry text the payload is keyed on — a built entry has none, and the library's own content rides in through the unit chain regardless.
    fn library(
        &mut self,
        units: &[RootSource],
        declared: &Declared<'_>,
    ) -> Result<(), CompileError> {
        let library = declared.manifest.with_file_name(LIBRARY);
        let subject = Subject::package(declared.package);
        // One store handle per target, as `run` holds one per invocation: a handle's placed chain is one compilation's, and a second fold on the same handle would carry the first's placements into the chain the second payload is filed against — one entry too long, which the store withholds without a word.
        let store = self.access.filed(declared.root);
        let (records, cwasm, foreigns) = tests_payload(
            self.budget,
            units,
            &Entrypoint::trivial(),
            &RootSource::none(),
            LIBRARY_KEY,
            &library,
            store.as_ref(),
            declared.package,
            "tests/",
            EntryTail::LastUnitTests,
            &subject,
            Some(declared.manifest),
        )?;
        self.refused(store.as_ref());

        self.selected(
            &records,
            &cwasm,
            &library,
            &subject,
            Some(declared.root),
            &foreigns,
        )
    }

    /// An executable's tests: its entry, compiled with the tail scheduling its own unit's tests.
    fn executable(
        &mut self,
        units: &[RootSource],
        entry: &Path,
        name: &str,
        declared: &Declared<'_>,
    ) -> Result<(), CompileError> {
        let subject = Subject::Executable(name.to_string());
        let store = self.access.filed(declared.root);
        let (entrypoint, loader, source) = open(Some(entry))?;
        let (records, cwasm, foreigns) = tests_payload(
            self.budget,
            units,
            &entrypoint,
            &loader,
            &source.text,
            entry,
            store.as_ref(),
            declared.package,
            &format!("tests/{name}"),
            EntryTail::Tests,
            &subject,
            Some(declared.manifest),
        )?;
        self.refused(store.as_ref());

        self.selected(
            &records,
            &cwasm,
            entry,
            &subject,
            Some(declared.root),
            &foreigns,
        )
    }

    /// A loose module's tests: a file, or standard input, mounted as a unit of its own and compiled as a library is, against the prelude alone, filed nowhere. `invoked` is the entry as `run` would pass it.
    fn loose_module(
        &mut self,
        unit: RootSource,
        invoked: &Path,
        subject: &Subject,
    ) -> Result<(), CompileError> {
        let (records, cwasm, foreigns) = tests_payload(
            self.budget,
            &[unit],
            &Entrypoint::trivial(),
            &RootSource::none(),
            LIBRARY_KEY,
            invoked,
            None,
            "",
            "",
            EntryTail::LastUnitTests,
            subject,
            None,
        )?;

        // A loose target has no package, so nothing declares what would implement a `foreign` it holds; `bindings_of` refuses it saying so.
        self.selected(&records, &cwasm, invoked, subject, None, &foreigns)
    }

    /// A loose program's tests: its entry, opened from a file or supplied from standard input, compiled against the prelude alone, filed nowhere. `invoked` is the entry as `run` passes it.
    fn loose_program(
        &mut self,
        (entrypoint, loader, source): (Entrypoint, RootSource, Rc<Source>),
        invoked: &Path,
        subject: &Subject,
    ) -> Result<(), CompileError> {
        let (records, cwasm, foreigns) = tests_payload(
            self.budget,
            &[],
            &entrypoint,
            &loader,
            &source.text,
            invoked,
            None,
            "",
            "",
            EntryTail::Tests,
            subject,
            None,
        )?;

        // A loose target has no package, so nothing declares what would implement a `foreign` it holds; `bindings_of` refuses it saying so.
        self.selected(&records, &cwasm, invoked, subject, None, &foreigns)
    }

    /// Keep the first reason a store refused filing.
    fn refused(&mut self, store: Option<&Verdicts>) {
        self.refusal = self
            .refusal
            .take()
            .or_else(|| store.and_then(Verdicts::refused));
    }

    /// Run the tests of one compiled target the filter selects.
    fn selected(
        &mut self,
        records: &[TestRecord],
        cwasm: &[u8],
        entry: &Path,
        subject: &Subject,
        root: Option<&Path>,
        foreigns: &ForeignStore,
    ) -> Result<(), CompileError> {
        run_selected(
            records,
            cwasm,
            entry,
            subject,
            self.filter,
            &mut self.totals,
            &mut self.matched_any,
            root,
            foreigns,
        )
    }

    /// The end of the run: a store's refusal said once, a filter that matched nothing refused by name, and the count line.
    fn finish(self) -> Result<bool, CompileError> {
        if let Some(refusal) = self.refusal {
            fact(
                Heading::Skipped,
                format!("storing what this built; {refusal}"),
            );
        }

        if let Some(filter) = self.filter
            && !self.matched_any
        {
            return Err(CompileError::failure(format!("no test matches '{filter}'")));
        }

        println!("{}", self.totals.line());

        Ok(self.totals.all_green())
    }
}

/// The records and machine code of one target compiled as a test program — from `store` when nothing it was made from has changed, and compiled and filed there otherwise. Without a store, compiled and filed nowhere.
#[allow(clippy::too_many_arguments)]
fn tests_payload(
    budget: u64,
    units: &[RootSource],
    entrypoint: &Entrypoint,
    loader: &RootSource,
    text: &str,
    entry: &Path,
    store: Option<&Verdicts>,
    package: &str,
    reserved: &str,
    tail: EntryTail,
    subject: &Subject,
    manifest: Option<&Path>,
) -> Result<(Vec<TestRecord>, Vec<u8>, ForeignStore), CompileError> {
    let sources = units.iter().map(UnitSource::mounted).collect::<Vec<_>>();
    let program = Program {
        package,
        executable: reserved,
        entry,
        text,
        loader,
    };

    if let Some((bytes, foreigns)) =
        store.and_then(|store| store.payload_get(&program, &sources, engine()))
        && let Some((records, cwasm)) = decode(&bytes)
    {
        processing(subject, manifest);
        let mut line = Line::nested(Heading::Compiling, subject);
        line.outcome("reused");
        eprintln!();

        return Ok((records, cwasm, foreigns));
    }

    processing(subject, manifest);
    let mut line: Option<Line> = None;
    let compiled = Fold::new(budget, units, store.map(|store| store as &dyn Cache)).tests(
        entrypoint,
        loader,
        tail,
        |_| {},
        |progress| report(&mut line, subject, !units.is_empty(), progress),
    );
    if compiled.is_err() && line.is_some() {
        eprintln!();
    }
    let (module, foreigns, records) = compiled?;
    let cwasm = to_cwasm(&module).map_err(CompileError::failure)?;

    if let Some(store) = store
        && let Some(bytes) = encode(&records, &cwasm)
    {
        store.payload_put(&program, &sources, bytes.as_ref(), &foreigns, engine());
    }

    Ok((records, cwasm, foreigns))
}

/// Whether the test at `path` is at or under `prefix`, compared a whole segment at a time: `/app/Map` selects `/app/Map` and everything under it, and never `/app/MapBuilder`, since a path names modules and one module's name is no part of its neighbour's. A trailing `/` says nothing more, so `/` alone selects every test.
fn under(path: &str, prefix: &str) -> bool {
    let prefix = prefix.strip_suffix('/').unwrap_or(prefix);

    path.strip_prefix(prefix)
        .is_some_and(|rest| rest.is_empty() || rest.starts_with('/'))
}

/// Run every record `filter` selects, one instantiation each, between a `↳ Testing` step and a `↳ Tested` step carrying the unit's tally, which is folded into `totals`. The guest prints the outcome line for what it survives; a trap or a stray exit never reaches the printing, so those lines are written here.
#[allow(clippy::too_many_arguments)]
fn run_selected(
    records: &[TestRecord],
    cwasm: &[u8],
    entry: &Path,
    subject: &Subject,
    filter: Option<&str>,
    totals: &mut Totals,
    matched_any: &mut bool,
    root: Option<&Path>,
    foreigns: &ForeignStore,
) -> Result<(), CompileError> {
    let selected = records
        .iter()
        .enumerate()
        .filter(|(_, record)| filter.is_none_or(|prefix| under(&record.path, prefix)))
        .collect::<Vec<_>>();

    // A unit with nothing to run reports its compile and nothing more: a tally of zeros would only restate the absence of lines above it.
    if selected.is_empty() {
        return Ok(());
    }
    *matched_any = true;

    step(Heading::Testing, subject);
    // argv[0] is the entry as `run` passes it: the path's bytes, since `/std/proc/args` promises opaque byte strings.
    let argv0 = entry.as_os_str().as_encoded_bytes().to_vec();
    let mut unit = Totals::default();

    for (index, record) in selected {
        let arguments = vec![argv0.clone(), index.to_string().into_bytes()];
        // SAFETY: the payload was precompiled in this process, or read back from the project's own store where a compilation of this compiler filed it.
        // Built per test rather than once: `run_bytes` takes the registry by value, and a fresh set is a fresh plugin instance — which is what keeps one test's marks in a plugin's memory out of the next one's, the isolation a test run is entitled to.
        let bindings = crate::bindings_of(root, foreigns.clone()).map_err(CompileError::failure)?;

        let outcome = unsafe { run_bytes(cwasm, OsHost::with_args(arguments), bindings) };
        match outcome {
            // The guest printed `path: proved` or `path: passed` and returned.
            Ok(0) => unit.passed += 1,
            // The guest printed `path: failed` and its report, then exited 1; the body as written is what only the records know.
            Ok(1) => {
                unit.failed += 1;
                body(record);
            }
            // The test exited on its own, before the scheduler could report — its line is written here.
            Ok(code) => {
                unit.exited += 1;
                println!("{}: exited {code}", record.path);
                body(record);
            }
            Err(trap) => {
                unit.trapped += 1;
                println!("{}: trapped", record.path);
                for line in trap.lines() {
                    println!("  {line}");
                }
                body(record);
            }
        }
    }

    let mut line = Line::nested(Heading::Tested, subject);
    line.outcome(&unit.line());
    eprintln!();
    totals.add(&unit);

    Ok(())
}

/// The failing declaration's body as written, indented beneath the report — absent when no span survived to slice it from.
fn body(record: &TestRecord) {
    for line in record.body.lines() {
        println!("    {line}");
    }
}

/// What the store files for one test program: its records as `(path, body)` pairs beside the machine code, one archive as every other artifact in the store is. A corrupt or foreign payload fails to decode and is a miss, never an error.
// `always`: a product that reads and writes archives unconditionally has no `archive` feature for a `cfg_attr` to gate on.
#[curios_archive::archived(always)]
struct TestPayload {
    records: Vec<(String, String)>,
    cwasm: Vec<u8>,
}

/// `records` and `cwasm` as the bytes the store files — `None` when they will not serialize, on which the record is withheld exactly as a unit's is.
fn encode(records: &[TestRecord], cwasm: &[u8]) -> Option<curios_archive::Serialized> {
    let payload = TestPayload {
        records: records
            .iter()
            .map(|record| (record.path.clone(), record.body.clone()))
            .collect(),
        cwasm: cwasm.to_vec(),
    };

    curios_archive::to_bytes(&payload).ok()
}

/// The inverse of [`encode`], `None` on any malformation.
fn decode(bytes: &[u8]) -> Option<(Vec<TestRecord>, Vec<u8>)> {
    let payload = curios_archive::from_bytes::<TestPayload>(bytes).ok()?;
    let records = payload
        .records
        .into_iter()
        .map(|(path, body)| TestRecord { path, body })
        .collect();

    Some((records, payload.cwasm))
}
