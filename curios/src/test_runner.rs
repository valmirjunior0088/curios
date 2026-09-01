//! `curios test`: the governing package's library and each of its executables compiled as test programs — the synthesized `Test/main` tail in place of the authored one — and every registered test run in an instantiation of its own, `[argv0, index]` as the program's arguments. The guest prints its own outcome line; what only the compiler knows — the failing declaration's body as written, the count line — is printed here from the records the compile hands back.
//!
//! The store is consulted exactly as `run` consults it: one payload per target, filed under a reserved executable name no identifier can spell (it contains `/`), holding the records beside the machine code so a warm run recompiles nothing and still reports everything.

use {
    crate::{Heading, Line, Subject, fact, load_units, report},
    curios::{Program, Verdicts, to_cwasm},
    curios_package::{Governing, LIBRARY, order},
    curios_pipeline::{Cache, CompileError, EntryTail, TestRecord, compile_tests_with_units},
    curios_runtime::{ForeignBindings, OsHost, run_bytes},
    curios_text::{Entrypoint, RootSource, UnitSource},
    std::path::{Path, PathBuf},
};

/// The tally the count line reports, in the order it spells them.
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

    /// The count line: every nonzero category in fixed order, `no tests` when nothing ran.
    fn line(&self) -> String {
        let mut parts = Vec::new();
        for (count, word) in [
            (self.passed, "passed"),
            (self.failed, "failed"),
            (self.trapped, "trapped"),
            (self.exited, "exited"),
        ] {
            if count > 0 {
                parts.push(format!("{count} {word}"));
            }
        }

        match parts.is_empty() {
            true => "no tests".to_string(),
            false => parts.join(", "),
        }
    }
}

/// Run the governing package's tests, optionally narrowed to paths starting with `filter`. `Ok(true)` when every selected test passed or proved.
pub(crate) fn run_tests(
    budget: u64,
    mounted_dirs: &[PathBuf],
    manifest: Option<&Path>,
    filter: Option<&str>,
) -> Result<bool, CompileError> {
    let governing = Governing::here(manifest).map_err(CompileError::failure)?;
    let store = Verdicts::at(governing.root.clone());

    // The same scope for every target: the `--unit` mounts in front, then the dependency graph with the governing package's own library last — the order `wonder` walks and `run` compiles.
    let mut units = load_units(mounted_dirs)?;
    units.extend(order(&governing).map_err(CompileError::failure)?);

    let mut totals = Totals::default();
    let mut matched_any = false;

    // The library first, when there is one, then every executable in declaration order — each a test program of its own, scheduling only its own unit's tests.
    let library = governing.directory.join(LIBRARY);
    if library.is_file() {
        let subject = Subject::package(&governing.package.name);
        // The entry is a dummy program — `()` is the smallest text an entrypoint parses, and the tests tail replaces it before anything checks it: the subject is the scope's final unit, and `EntryTail::LastUnitTests` schedules that unit's tests. The constant text is also what keys the payload — the library's own content rides in through the unit chain.
        let (entrypoint, loader, source) = Entrypoint::supplied(LIBRARY, "()")
            .map_err(|error| CompileError::Failure(vec![error.report()]))?;
        let (records, cwasm) = tests_payload(
            budget,
            &units,
            &entrypoint,
            &loader,
            &source.text,
            &library,
            &store,
            &governing.package.name,
            "tests/",
            EntryTail::LastUnitTests,
            &subject,
        )?;
        run_selected(
            &records,
            &cwasm,
            &library,
            filter,
            &mut totals,
            &mut matched_any,
        )?;
    }

    for executable in &governing.package.executables {
        let subject = Subject::Executable(executable.name.clone());
        let entry = governing.directory.join(&executable.path);
        let (entrypoint, loader, source) = Entrypoint::opened(&entry)
            .map_err(|error| CompileError::Failure(vec![error.report()]))?;
        let (records, cwasm) = tests_payload(
            budget,
            &units,
            &entrypoint,
            &loader,
            &source.text,
            &entry,
            &store,
            &governing.package.name,
            &format!("tests/{}", executable.name),
            EntryTail::Tests,
            &subject,
        )?;
        run_selected(
            &records,
            &cwasm,
            &entry,
            filter,
            &mut totals,
            &mut matched_any,
        )?;
    }

    if let Some(refusal) = store.refused() {
        fact(
            Heading::Skipped,
            format!("storing what this built; {refusal}"),
        );
    }

    if let Some(filter) = filter
        && !matched_any
    {
        return Err(CompileError::failure(format!("no test matches '{filter}'")));
    }

    println!("{}", totals.line());

    Ok(totals.all_green())
}

/// The records and machine code of one target compiled as a test program — from the store when nothing it was made from has changed, and compiled and filed otherwise.
#[allow(clippy::too_many_arguments)]
fn tests_payload(
    budget: u64,
    units: &[RootSource],
    entrypoint: &Entrypoint,
    loader: &RootSource,
    text: &str,
    entry: &Path,
    store: &Verdicts,
    package: &str,
    reserved: &str,
    tail: EntryTail,
    subject: &Subject,
) -> Result<(Vec<TestRecord>, Vec<u8>), CompileError> {
    let sources = units.iter().map(UnitSource::mounted).collect::<Vec<_>>();
    let program = Program {
        package,
        executable: reserved,
        entry,
        text,
        loader,
    };

    if let Some(bytes) = store.payload_get(&program, &sources)
        && let Some(decoded) = decode(&bytes)
    {
        let mut line = Line::open(Heading::Building, subject);
        line.outcome("reused");
        eprintln!();

        return Ok(decoded);
    }

    fact(Heading::Building, subject);
    let mut line: Option<Line> = None;
    let compiled = compile_tests_with_units(
        budget,
        units,
        entrypoint,
        loader,
        Some(store as &dyn Cache),
        tail,
        |_| {},
        |progress| report(&mut line, subject, true, progress),
    );
    if compiled.is_err() && line.is_some() {
        eprintln!();
    }
    let (module, _foreigns, records) = compiled?;
    let cwasm = to_cwasm(&module).map_err(CompileError::failure)?;

    store.payload_put(&program, &sources, &encode(&records, &cwasm));

    Ok((records, cwasm))
}

/// Run every record `filter` selects, one instantiation each, folding outcomes into `totals`. The guest prints the outcome line for what it survives; a trap or a stray exit never reaches the printing, so those lines are written here.
fn run_selected(
    records: &[TestRecord],
    cwasm: &[u8],
    entry: &Path,
    filter: Option<&str>,
    totals: &mut Totals,
    matched_any: &mut bool,
) -> Result<(), CompileError> {
    let argv0 = entry.to_string_lossy().into_owned();

    for (index, record) in records.iter().enumerate() {
        if let Some(prefix) = filter
            && !record.path.starts_with(prefix)
        {
            continue;
        }
        *matched_any = true;

        let arguments = vec![argv0.clone().into_bytes(), index.to_string().into_bytes()];
        match run_bytes(
            cwasm,
            OsHost::with_args(arguments),
            ForeignBindings::empty(),
        ) {
            // The guest printed `path: proved` or `path: passed` and returned.
            Ok(0) => totals.passed += 1,
            // The guest printed `path: failed` and its report, then exited 1; the body as written is what only the records know.
            Ok(1) => {
                totals.failed += 1;
                body(record);
            }
            // The test exited on its own, before the scheduler could report — its line is written here.
            Ok(code) => {
                totals.exited += 1;
                println!("{}: exited {code}", record.path);
                body(record);
            }
            Err(trap) => {
                totals.trapped += 1;
                println!("{}: trapped", record.path);
                for line in trap.lines() {
                    println!("  {line}");
                }
                body(record);
            }
        }
    }

    Ok(())
}

/// The failing declaration's body as written, indented beneath the report — absent when no span survived to slice it from.
fn body(record: &TestRecord) {
    for line in record.body.lines() {
        println!("    {line}");
    }
}

/// `records ++ cwasm` as one stored payload: a count, then each record length-prefixed, then the machine code — all little-endian `u32` lengths, an internal format scoped to this compiler build exactly as the store's other artifacts are.
fn encode(records: &[TestRecord], cwasm: &[u8]) -> Vec<u8> {
    let mut bytes = Vec::new();
    bytes.extend(
        u32::try_from(records.len())
            .expect("a test count fits")
            .to_le_bytes(),
    );
    for record in records {
        for field in [&record.path, &record.body] {
            bytes.extend(
                u32::try_from(field.len())
                    .expect("a record field fits")
                    .to_le_bytes(),
            );
            bytes.extend(field.as_bytes());
        }
    }
    bytes.extend(cwasm);

    bytes
}

/// The inverse of [`encode`], `None` on any malformation — a corrupt or foreign payload is a store miss, not an error.
fn decode(bytes: &[u8]) -> Option<(Vec<TestRecord>, Vec<u8>)> {
    fn take<'a>(rest: &mut &'a [u8], n: usize) -> Option<&'a [u8]> {
        let (head, tail) = rest.split_at_checked(n)?;
        *rest = tail;

        Some(head)
    }

    fn field(rest: &mut &[u8]) -> Option<String> {
        let length = u32::from_le_bytes(take(rest, 4)?.try_into().expect("four bytes")) as usize;

        String::from_utf8(take(rest, length)?.to_vec()).ok()
    }

    let mut rest = bytes;
    let count = u32::from_le_bytes(take(&mut rest, 4)?.try_into().expect("four bytes")) as usize;
    let mut records = Vec::with_capacity(count);
    for _ in 0..count {
        let path = field(&mut rest)?;
        let body = field(&mut rest)?;
        records.push(TestRecord { path, body });
    }

    Some((records, rest.to_vec()))
}
