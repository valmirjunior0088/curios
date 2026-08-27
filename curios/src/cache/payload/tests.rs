//! What the payload family does across two invocations of one target.
//!
//! Every test here compiles for real and asks the question a caller asks — was this invocation served from the store — rather than reading the store's internals. Asserting on a slot name would pass just as happily with verification broken.
//!
//! The address parts and their separation are `curios-package`'s to test, and are: this file is about the *record*, which is what stands between an address and a stale answer.

use {
    super::*,
    crate::to_cwasm,
    curios_pipeline::{Cache, DEFAULT_STEP_BUDGET, compile_with_units},
    curios_text::{Entrypoint, Module},
    curios_utilities::RootKind,
    std::{
        path::PathBuf,
        time::{SystemTime, UNIX_EPOCH},
    },
};

/// What one invocation did.
struct Invocation {
    /// Whether the store answered instead of the compiler.
    reused: bool,
    /// The payload, however it was obtained.
    payload: Vec<u8>,
    /// Why nothing could be filed, if nothing could.
    refused: Option<String>,
}

/// One invocation of `target`, standing in `directory`, with `mounted` in front of whatever the manifest declares.
///
/// The same sequence `payload_of` runs — consult, compile what must be compiled, file what was made — minus the terminal it reports to. `mounted` is the `--unit` flag, which is how a package outside the project enters the compilation.
fn invoke(directory: &Path, target: Option<&str>, mounted: &[PathBuf]) -> Invocation {
    invoke_over(
        directory,
        target,
        curios_package::mounted(mounted).expect("mountable packages"),
    )
}

/// [`invoke`] over a scope whose front is already built, for the one unit a directory cannot supply.
fn invoke_over(directory: &Path, target: Option<&str>, mut scope: Vec<RootSource>) -> Invocation {
    let (entry, package, name, root) =
        match curios_package::Target::of(target, None, directory).expect("a governed package") {
            curios_package::Target::Executable {
                entry,
                units,
                package,
                name,
                root,
                ..
            } => {
                scope.extend(units);

                (entry, package, name, root)
            }
            curios_package::Target::File(_) | curios_package::Target::Stdin => {
                panic!("these fixtures declare their executables")
            }
        };

    let verdicts = Verdicts::at(root);
    let (entrypoint, loader, source) = Entrypoint::opened(&entry).expect("the entry parses");
    let program = Program {
        package: &package,
        executable: &name,
        entry: &entry,
        text: &source.text,
        loader: &loader,
    };

    let sources = scope.iter().map(UnitSource::mounted).collect::<Vec<_>>();
    if let Some(payload) = verdicts.payload_get(&program, &sources) {
        return Invocation {
            reused: true,
            payload,
            refused: verdicts.refused(),
        };
    }

    let (module, _foreigns) = compile_with_units(
        DEFAULT_STEP_BUDGET,
        &scope,
        &entrypoint,
        &loader,
        Some(&verdicts as &dyn Cache),
        |_| {},
        |_| {},
    )
    .expect("the package compiles");

    let payload = to_cwasm(&module).expect("the module precompiles");
    verdicts.payload_put(&program, &sources, &payload);

    Invocation {
        reused: false,
        payload,
        refused: verdicts.refused(),
    }
}

/// [`invoke`] over the governing package's sole executable, with nothing mounted by hand.
fn run(root: &Path) -> Invocation {
    invoke(root, None, &[])
}

/// A directory of its own, shared with no other test.
fn temporary(name: &str) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();

    std::env::temp_dir().join(format!(
        "curios-payload-{name}-{}-{millis}",
        std::process::id()
    ))
}

fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}

/// A package declaring one executable, whose entry reads its library.
///
/// The entry's own modules are the other half of what a payload's record covers, and they mount at the *empty* prefix: `mod greeting` in `app.crs` reads `app/greeting.crs` and is spelled `/greeting`. [`ENTRY_WITH_MODULE`] is the form that has one.
fn project(name: &str) -> PathBuf {
    let root = temporary(name);

    write(
        &root,
        "curios.toml",
        "name = \"app\"\n\n[[executables]]\nname = \"app\"\n",
    );
    write(&root, "lib.crs", &library("library"));
    write(&root, "app.crs", "/std/print(/app/word)\n");

    root
}

/// An entry that declares a module of its own, so its loader has something to record.
const ENTRY_WITH_MODULE: &str = "mod greeting;\n\n/std/print(/greeting/word)\n";

/// The package's library, saying `word`.
fn library(word: &str) -> String {
    format!("pub let word : /std/Str =\n    \"{word}\";\n")
}

/// A module of the entry's own, saying `word`.
fn greeting(word: &str) -> String {
    format!("pub let word : /std/Str =\n    \"{word}\";\n")
}

/// The payload slots at `root`, counting a store that was never created as the none it holds — which is what a refusal to file leaves behind.
fn filed(root: &Path) -> usize {
    fs::read_dir(root.join(".curios").join("payload")).map_or(0, Iterator::count)
}

/// The slots the payload family holds for the project at `root`.
fn slots(root: &Path) -> usize {
    fs::read_dir(root.join(".curios").join("payload"))
        .expect("a store with payloads in it")
        .count()
}

/// The one payload slot's directory, for a test that has to damage what is in it.
fn slot_of(root: &Path) -> PathBuf {
    let mut slots = fs::read_dir(root.join(".curios").join("payload"))
        .expect("a store with payloads in it")
        .map(|slot| slot.unwrap().path())
        .collect::<Vec<_>>();

    assert_eq!(slots.len(), 1, "these fixtures build one executable");

    slots.pop().unwrap()
}

/// The point of the thing: a program nothing has changed is not compiled again, and what comes back is what the compile produced.
#[test]
fn an_unchanged_program_is_reused() {
    let root = project("unchanged");

    let first = run(&root);
    assert!(!first.reused, "nothing is stored for the first invocation");

    let second = run(&root);
    assert!(second.reused, "and the second finds what the first filed");
    assert_eq!(
        first.payload, second.payload,
        "and it is the payload the compile produced, byte for byte — which is what lets `run` execute it and `compile` bundle it without either knowing where it came from"
    );

    fs::remove_dir_all(root).unwrap();
}

/// The address carries no file contents, so it is the *record* that has to notice an edit — and the entry's own header is the one file no loader ever reads, so nothing but the record's separate entry half can catch this.
#[test]
fn an_edited_entry_is_not_reused() {
    let root = project("edited-entry");

    run(&root);
    assert!(run(&root).reused, "filed by the first invocation");

    write(&root, "app.crs", "/std/print(\"rewritten\")\n");
    assert!(!run(&root).reused, "and refused once the entry differs");
    assert!(
        run(&root).reused,
        "the recompile refiles, so the next invocation hits again"
    );

    fs::remove_dir_all(root).unwrap();
}

/// A module the entry declares joins the compilation through the loader, and is recorded there. It cannot join without the entry's own text changing — that is the read-set closure — but a *changed* one has to be caught here.
#[test]
fn an_edited_module_of_the_entry_is_not_reused() {
    let root = project("edited-module");

    write(&root, "app.crs", ENTRY_WITH_MODULE);
    write(&root, "app/greeting.crs", &greeting("hello"));
    run(&root);
    assert!(run(&root).reused, "filed by the first invocation");

    write(&root, "app/greeting.crs", &greeting("goodbye"));
    assert!(
        !run(&root).reused,
        "and refused once a file its loader read differs"
    );
    assert!(run(&root).reused, "the recompile refiles");

    fs::remove_dir_all(root).unwrap();
}

/// A dependency's edit never reaches the payload's own record: it makes the unit stale, the chain undecidable, and the payload a miss by construction.
#[test]
fn an_edited_dependency_is_not_reused() {
    let root = project("edited-dependency");

    run(&root);
    assert!(run(&root).reused, "filed by the first invocation");

    write(&root, "lib.crs", &library("rewritten"));
    assert!(
        !run(&root).reused,
        "a stale unit is a stale payload, whatever the payload's own record says"
    );
    assert!(run(&root).reused, "the recompile refiles");

    fs::remove_dir_all(root).unwrap();
}

/// Changing what a program is compiled *against* moves the address rather than invalidating a record — which is what lets the payload for the old configuration survive and answer again when it comes back.
#[test]
fn a_changed_dependency_set_moves_the_address() {
    let shape = temporary("addressed-shape");
    write(&shape, "curios.toml", "name = \"shape\"\n");
    write(&shape, "lib.crs", "pub let side : /std/Nat = 4;\n");

    let root = project("addressed");
    write(&root, "app.crs", "/std/print(/app/word)\n");

    run(&root);
    assert!(run(&root).reused, "one slot, filed and found");
    assert_eq!(slots(&root), 1);

    let mounted = [shape.clone()];
    assert!(
        !invoke(&root, None, &mounted).reused,
        "mounting another unit is a different compilation, so it is a different address"
    );
    assert_eq!(slots(&root), 2, "and the first slot is still there");

    assert!(
        run(&root).reused,
        "which is why dropping the dependency again finds the original payload rather than a mismatched record"
    );
    assert!(
        invoke(&root, None, &mounted).reused,
        "and so does keeping it"
    );
    assert_eq!(slots(&root), 2, "two configurations, two slots, forever");

    fs::remove_dir_all(root).unwrap();
    fs::remove_dir_all(shape).unwrap();
}

/// Two executables of one package are two programs, so they occupy two slots and neither answers for the other.
#[test]
fn two_executables_occupy_two_slots() {
    let root = temporary("two-executables");

    write(
        &root,
        "curios.toml",
        "name = \"app\"\n\n[[executables]]\nname = \"one\"\n\n[[executables]]\nname = \"two\"\n",
    );
    write(&root, "lib.crs", &library("shared"));
    write(&root, "one.crs", "/std/print(/app/word)\n");
    write(&root, "two.crs", "/std/print(\"different\")\n");

    let one = invoke(&root, Some("one"), &[]);
    let two = invoke(&root, Some("two"), &[]);
    assert!(!one.reused && !two.reused, "neither is stored yet");
    assert_ne!(one.payload, two.payload, "and they are different programs");

    assert_eq!(slots(&root), 2, "one slot each");
    assert!(invoke(&root, Some("one"), &[]).reused);
    assert!(invoke(&root, Some("two"), &[]).reused);

    fs::remove_dir_all(root).unwrap();
}

/// The payload's own digest is in its record, so a slot whose artifact has been damaged is a miss here rather than a failure to deserialize at run time.
#[test]
fn a_corrupted_payload_is_not_reused() {
    let root = project("corrupted");

    run(&root);
    assert!(run(&root).reused, "filed by the first invocation");

    let stored = slot_of(&root).join(STORED);
    let mut bytes = fs::read(&stored).unwrap();
    bytes[16] ^= 0xff;
    fs::write(&stored, &bytes).unwrap();

    assert!(!run(&root).reused, "damaged bytes are not the bytes filed");
    assert!(run(&root).reused, "and the recompile repairs the slot");

    fs::remove_dir_all(root).unwrap();
}

/// The write ordering exists so that every state an interrupted run can leave behind reads as a miss. These are those states, enumerated.
///
/// The record goes last, so an interruption can leave a slot with neither file, with the artifact alone, or with a record still being written — never with a record vouching for an artifact it was not made from. Each is checked by producing it.
#[test]
fn every_half_written_slot_state_reads_as_a_miss() {
    let root = project("half-written");

    run(&root);
    assert!(run(&root).reused, "filed by the first invocation");

    let slot = slot_of(&root);
    let (stored, record) = (slot.join(STORED), slot.join(RECORD));
    let (payload, recorded) = (fs::read(&stored).unwrap(), fs::read(&record).unwrap());

    let restore = || {
        fs::write(&stored, &payload).unwrap();
        fs::write(&record, &recorded).unwrap();
    };

    // The state an interrupted run leaves after removing the old record and before writing the artifact.
    fs::remove_file(&record).unwrap();
    assert!(
        !run(&root).reused,
        "an artifact with no record vouches for nothing"
    );
    restore();

    // The state a store that lost its artifact leaves — and the state after the record is removed but before anything replaces it.
    fs::remove_file(&stored).unwrap();
    assert!(
        !run(&root).reused,
        "a record with no artifact answers for nothing"
    );
    restore();

    // A record interrupted mid-write.
    fs::write(&record, &recorded[..recorded.len() / 2]).unwrap();
    assert!(
        !run(&root).reused,
        "a record that will not read back is a store to ignore"
    );
    restore();

    // And an artifact interrupted mid-write, which the record's own digest catches.
    fs::write(&stored, &payload[..payload.len() / 2]).unwrap();
    assert!(
        !run(&root).reused,
        "a truncated artifact is not the artifact filed"
    );
    restore();

    assert!(run(&root).reused, "and the intact slot still answers");

    fs::remove_dir_all(root).unwrap();
}

/// A slot filed by one project must not answer for another's, even when both address it identically — which two projects declaring a package and an executable of one name, compiled by one compiler after one chain, always do.
///
/// The store is shared whenever `CURIOS_CACHE` names one, so this is reachable. The dependency sits outside both projects and is read from that one path by each, standing in for the `src/` tree `curate` materializes — so the *units* legitimately cross and only the payload's own record can refuse this.
#[test]
fn a_slot_does_not_answer_for_another_projects_program() {
    let shape = temporary("aliasing-shape");
    write(&shape, "curios.toml", "name = \"shape\"\n");
    write(&shape, "lib.crs", "pub let word : /std/Str = \"shared\";\n");

    let manifest = "name = \"tool\"\n\n[[executables]]\nname = \"tool\"\n";
    let (mine, theirs) = (temporary("aliasing-mine"), temporary("aliasing-theirs"));
    write(&mine, "curios.toml", manifest);
    write(&theirs, "curios.toml", manifest);
    write(&mine, "tool.crs", "/std/print(/shape/word)\n");
    write(&theirs, "tool.crs", "/std/print(\"theirs\")\n");

    let mounted = [shape.clone()];
    let filed = invoke(&theirs, None, &mounted);
    assert!(!filed.reused);

    // Copied rather than shared through `CURIOS_CACHE`, since the store's hermeticity rests on no test ever setting it — and what is under test is the verification, which cannot tell how a foreign slot arrived.
    stage(&theirs, &mine);

    let asked = invoke(&mine, None, &mounted);
    assert!(
        !asked.reused,
        "their record names their entry, which is not mine to have compiled"
    );
    assert_ne!(
        asked.payload, filed.payload,
        "and what came back is my program rather than theirs"
    );

    fs::remove_dir_all(mine).unwrap();
    fs::remove_dir_all(theirs).unwrap();
    fs::remove_dir_all(shape).unwrap();
}

/// Copy every unit and payload slot `from`'s store holds into `into`'s, replacing whatever was there.
fn stage(from: &Path, into: &Path) {
    for family in ["unit", "payload"] {
        let (from, into) = (
            from.join(".curios").join(family),
            into.join(".curios").join(family),
        );

        let _ = fs::remove_dir_all(&into);
        fs::create_dir_all(&into).unwrap();

        for slot in fs::read_dir(&from).expect("a store with slots in it") {
            let slot = slot.unwrap().path();
            let target = into.join(slot.file_name().unwrap());
            fs::create_dir_all(&target).unwrap();

            for file in fs::read_dir(&slot).unwrap() {
                let file = file.unwrap().path();
                fs::copy(&file, target.join(file.file_name().unwrap())).unwrap();
            }
        }
    }
}

/// A store nobody can write costs the reuse and never the verdict or the run: the compilation proceeds, a payload comes back, and the reason is kept for a caller with somewhere to say it.
///
/// The store is made unwritable by putting a *file* where its directory belongs, which fails for every user rather than only for one who is not root.
#[test]
fn an_unwritable_store_refuses_once_and_stops_nothing() {
    let root = project("unwritable");
    fs::write(root.join(".curios"), "not a directory").unwrap();

    let first = run(&root);
    assert!(!first.reused);
    assert!(
        !first.payload.is_empty(),
        "the compilation is unaffected by a store it cannot write"
    );
    assert!(
        first.refused.is_some(),
        "and the reason is recorded rather than raised"
    );

    let second = run(&root);
    assert!(
        !second.reused,
        "nothing was filed, so nothing is found — which costs the work and nothing else"
    );
    assert_eq!(
        first.payload, second.payload,
        "and both invocations produce the same program"
    );

    fs::remove_dir_all(root).unwrap();
}

/// A payload is filed only over a chain a later look-up can derive — never over one with a gap in it.
///
/// **A unit with nothing on disk is the gap, and `RootSource::supplied` is what builds one.** It holds no directory, so `Verdicts::slot` declines to address it: the fold compiles it every time and places nothing for it, which leaves `Verdicts::placed` one entry shorter than the scope it folded. [`Verdicts::payload_get`] derives its own chain through `Verdicts::chain`, which refuses that unit outright — so a payload filed against the short chain is addressed under a prefix the probe never computes. Nothing stale comes back; a slot is simply written that nothing will ever read, on every invocation, forever.
///
/// Nothing in this product folds such a unit — the CLI's scope is disk-backed throughout — but the constructor is public API and the two halves must not be able to disagree about what the chain is. The assertion is on the store's own shape rather than on a reuse verdict, because the defect never produced a wrong answer to assert against: it produced a write.
#[test]
fn a_payload_is_not_filed_over_a_chain_with_a_gap() {
    let root = project("gapped");
    write(&root, "supplied.crs", &library("supplied"));

    let mut supplied = RootSource::supplied();
    supplied.insert_root(
        "supplied",
        RootKind::Ordinary,
        Module::from_path(root.join("supplied.crs")).expect("the supplied module parses"),
    );

    let gapped = invoke_over(&root, None, vec![supplied]);
    assert!(!gapped.reused, "nothing is stored for the first invocation");
    assert_eq!(
        filed(&root),
        0,
        "the fold placed nothing for the supplied unit, so there is no chain to file a payload under"
    );

    // The guard refuses a gap rather than refusing everything.
    let plain = run(&root);
    assert!(
        !plain.reused,
        "and the gapped invocation left nothing to reuse"
    );
    assert_eq!(filed(&root), 1, "a chain with no gap in it still files");

    fs::remove_dir_all(root).unwrap();
}
