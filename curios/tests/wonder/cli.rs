//! `curios wonder` at the command line: a question answered on standard output, a place or a path asked of a checked unit, and the exit code each outcome takes.

use {
    super::test_support::*,
    curios::CABINET,
    std::{
        fs,
        io::Write,
        path::Path,
        process::{Command, Stdio},
    },
};

/// A program of a kept structure, a sealed concept whose method is noted, a family and a concept whose representations are public, a definition not known to terminate, a proof, and a witness the compiler writes.
const FACTS: &str = "use /std/{Nat, Eq};

--- A shade, kept.
struct Shade: Type {
    depth: Nat,
}

--- A size.
concept Sized(T: Type): Type {
    --- How big.
    size(T) -> Nat,
}

induct Tone: pub Type
| high()
| low()
end

concept Named(T: Type): pub Type {
    name(T) -> Nat,
}

--- Never returns.
let spin(n: Nat) -> Nat =
    spin(n);

--- One is one.
let one_is_one: Eq()(1, 1) =
    Eq/refl();

struct Dot: pub Type {
    x: Nat,
}

satisfy /std/Spell(Dot) { .. }

/std/print(\"\")
";

/// An answer is written for whoever reads it, so a reader that leaves early — `curios wonder --stage wasm | head` — ends the command as a filter ends, with the code a broken pipe gives and nothing said: a panic over the pipe says what no answer does.
#[test]
fn a_reader_that_leaves_early_ends_the_question_with_nothing_said() {
    let root = temporary("reader-gone");
    fs::create_dir_all(&root).unwrap();
    let mut child = Command::new(env!("CARGO_BIN_EXE_curios"))
        .env_remove(CABINET)
        .current_dir(&root)
        .args(["wonder", "--stage", "wasm", "-"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("run the compiler");
    // Gone before the program is so much as read, so the rung's first line already has no reader.
    drop(child.stdout.take());
    child
        .stdin
        .take()
        .unwrap()
        .write_all(b"/std/print(\"\")\n")
        .unwrap();

    let ended = child.wait_with_output().expect("the compiler exits");
    let said = String::from_utf8_lossy(&ended.stderr);
    assert_eq!(ended.status.code(), Some(141), "{said}");
    assert!(!said.contains("panicked"), "{said}");
}

/// A goal on standard input is reported: located, rendered as `run` reports it, on stdout, and the incomplete state, exit 2.
#[test]
fn a_goal_is_reported_on_stdout_with_exit_two() {
    let root = temporary("stdin");
    fs::create_dir_all(&root).unwrap();

    let answered = curios(
        &root,
        &["check", "-"],
        "let _m : /std/Nat = ?;\n/std/print(\"\")\n",
    );

    assert_eq!(
        answered.status.code(),
        Some(2),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    let text = stdout(&answered);
    assert!(text.contains("└─ [goal] <stdin>:1:21"), "{text}");
    // The check is narrated beside the answer, closed by what stopped it.
    let narrated = String::from_utf8_lossy(&answered.stderr);
    assert!(
        narrated.starts_with("Checking `<stdin>`\nChecked `<stdin>`: 1 goal in "),
        "{narrated}"
    );
}

/// A text's layout is the formatter's question, never the check's: a text that compiles clean answers nothing however it is laid out, read from standard input or from a package's files.
#[test]
fn the_check_says_nothing_of_a_texts_layout() {
    let root = project("layout");
    write(&root, "lib.crs", "pub   mod util;\n");
    write(&root, "app.crs", "/std/print(  /app/util/word)\n");

    let entire = curios(&root, &["check"], "");
    assert!(entire.status.success());
    assert_eq!(stdout(&entire), "");

    let read = curios(&root, &["check", "-"], "/std/print(  \"\")\n");
    assert!(read.status.success());
    assert_eq!(stdout(&read), "");
}

/// `--manifest` takes the file as it is spelled, and a bare `curios.toml` names the one in the working directory: its parent is the empty path, which is no directory to resolve on its own.
#[test]
fn a_bare_manifest_override_names_the_working_directory() {
    let root = project("manifest-override");

    let answered = curios(&root, &["check", "--manifest", "curios.toml"], "");

    assert!(
        answered.status.success(),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    assert!(stdout(&answered).is_empty(), "{}", stdout(&answered));
}

/// A place answers the name written there on stdout as `--name` answers a declaration, a block — its path, what it is declared as, the type and totality the library's check established of it, and where its name is written — then the prose and head its author wrote; and a segment of the path, the module it reaches.
#[test]
fn a_place_answers_the_name_written_there() {
    let root = project("at");
    write(
        &root,
        "util.crs",
        "--- The word a run prints.\npub let word : /std/Str = \"placed\";\n",
    );

    let answered = curios(&root, &["wonder", "--at", "app.crs:1:22"], "");

    assert!(
        answered.status.success(),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    let answer = stdout(&answered);
    assert_eq!(
        answer,
        concat!(
            "/app/util/word\n",
            "│  total\n",
            "├─ --- The word a run prints.\n",
            "│  pub let word: /std/Str\n",
            "└─ [declaration] util.crs:2:9\n",
        )
    );

    // Asked again, of the library the first question filed: a unit restored from the cabinet states its declarations as one compiled now does.
    let again = curios(&root, &["wonder", "--at", "app.crs:1:22"], "");
    assert_eq!(stdout(&again), answer);

    // A segment of the path answers the module it reaches, closed where the library's `mod` line declares it, with the command that lists what it exposes.
    let module = curios(&root, &["wonder", "--at", "app.crs:1:18"], "");
    assert_eq!(
        stdout(&module),
        concat!(
            "/app/util\n",
            "│  module\n",
            "├─ try; curios wonder --name /app/util app.crs\n",
            "└─ [module] lib.crs:1:9\n",
        )
    );
}

/// A place in a library's file answers what is written there whatever the cabinet holds of the library: here the check of the package entire files the library, beside an executable, before the place is asked.
#[test]
fn a_place_answers_over_a_library_a_check_filed() {
    let root = project("at-filed");
    let checked = curios(&root, &["check"], "");
    assert!(
        checked.status.success(),
        "{}",
        String::from_utf8_lossy(&checked.stderr)
    );

    let answered = curios(&root, &["wonder", "--at", "util.crs:1:9"], "");

    assert!(
        answered.status.success(),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    let answer = stdout(&answered);
    assert!(answer.starts_with("/app/util/word\n"), "{answer:?}");
}

/// A place in a file of the standard library's package answers what is written there: the package is lowered over the library the compiler carries, and what it keeps of that library's files lies in the files the package holds. Asked of a copy, so the tree's own package keeps no cabinet.
#[test]
fn a_place_in_the_standard_librarys_package_answers() {
    fn copied(from: &Path, to: &Path) {
        fs::create_dir_all(to).unwrap();
        for entry in fs::read_dir(from).unwrap() {
            let entry = entry.unwrap();
            let name = entry.file_name();
            if name == ".curios" {
                continue;
            }
            match entry.file_type().unwrap().is_dir() {
                true => copied(&entry.path(), &to.join(&name)),
                false => {
                    fs::copy(entry.path(), to.join(&name)).unwrap();
                }
            }
        }
    }

    let root = temporary("at-std");
    copied(
        &Path::new(env!("CARGO_MANIFEST_DIR")).join("../curios-prelude-build/std"),
        &root,
    );
    let text = fs::read_to_string(root.join("Bool.crs")).unwrap();
    let line = 1 + text
        .lines()
        .position(|line| line.starts_with("pub let not("))
        .expect("the library declares `not`");

    let answered = curios(
        &root,
        &["wonder", "--at", &format!("Bool.crs:{line}:9")],
        "",
    );

    assert!(
        answered.status.success(),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    let answer = stdout(&answered);
    assert!(answer.starts_with("/std/Bool/not\n"), "{answer:?}");
}

/// The place a refusal closes on is asked of `--at` as it was printed, from where it was printed: here a nested module's file, its path spelled from the package's root.
#[test]
fn a_refusals_place_is_asked_as_it_was_printed() {
    let root = temporary("pasted");
    write(
        &root,
        "curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"app\"\n"
        ),
    );
    write(&root, "lib.crs", "pub mod a;\n");
    write(&root, "a.crs", "pub mod b;\n");
    write(
        &root,
        "a/b.crs",
        "use /std/{Nat, Str};\n\npub let count: Nat = 1;\n\npub let shown: Str = count;\n",
    );

    let checked = stdout(&curios(&root, &["check"], ""));
    let place = checked
        .lines()
        .find_map(|line| line.strip_prefix("└─ [error] "))
        .unwrap_or_else(|| panic!("a refusal closes on its place: {checked}"));
    assert_eq!(place, "a/b.crs:5:22");

    let answered = curios(&root, &["wonder", "--at", place], "");

    assert!(
        answered.status.success(),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    let answer = stdout(&answered);
    assert!(answer.starts_with("/app/a/b/count\n"), "{answer:?}");
}

/// A block about a type with witnesses in scope points to the view that lists them, asked of the subject the block was asked of: through `--name`, and through `--at` of its place's file.
#[test]
fn a_block_points_to_the_view_listing_its_witnesses() {
    let root = project("pointer");

    let named = stdout(&curios(
        &root,
        &["wonder", "--name", "/std/Show", "app.crs"],
        "",
    ));
    assert!(
        named.contains("├─ try; curios wonder --name /std/Show --witnesses app.crs\n"),
        "{named}"
    );

    let placed = stdout(&curios(&root, &["wonder", "--at", "util.crs:1:20"], ""));
    assert!(placed.contains(" --witnesses util.crs\n"), "{placed}");

    let listed = stdout(&curios(
        &root,
        &["wonder", "--name", "/std/Show", "--witnesses", "app.crs"],
        "",
    ));
    // Each witness is an entry on the block: its name, which `--name` takes, its head as its author wrote it, closed by where it is written, or by the module that declares it where no file holds it.
    assert!(
        listed.contains(
            "\n├─ /std/Bool/witness~/std/Show/Show(Bool): satisfy Show(Bool); /std/Bool\n"
        ),
        "{listed}"
    );
    assert!(!listed.contains('\t'), "{listed}");
    assert!(
        listed
            .trim_end()
            .ends_with("└─ [declaration] /std/Show/Show"),
        "{listed}"
    );
}

/// The name each witness a listing holds leads its entry with is the one `--name` answers it by, whatever its key's heads are: a family's, an intrinsic type's, a tuple type's shape, and a concept of several arguments'.
#[test]
fn every_listed_witness_is_answered_by_the_name_it_leads_with() {
    let root = temporary("witness-names");
    fs::create_dir_all(&root).unwrap();

    for concept in ["/std/Ord", "/std/Monad/Lift"] {
        let listed = stdout(&curios(
            &root,
            &["wonder", "--name", concept, "--witnesses"],
            "",
        ));
        let entries = listed
            .lines()
            .filter_map(|line| line.strip_prefix("├─ "))
            .filter_map(|entry| entry.split_once(": satisfy"))
            .collect::<Vec<_>>();
        assert!(entries.len() > 2, "{listed}");

        for (name, head) in entries {
            // An entry closes on where its witness is written, past its head; the block's entry is its source, its prose above its head.
            let head = head.split_once("; ").map_or(head, |(head, _)| head);
            let answered = curios(&root, &["wonder", "--name", name], "");
            let block = stdout(&answered);
            assert_eq!(answered.status.code(), Some(0), "{name}: {block}");
            assert!(block.contains(&format!("satisfy{head}")), "{name}: {block}");
            // No file holds the standard library's text, so its block closes on the name it was asked by.
            assert!(
                block
                    .trim_end()
                    .ends_with(&format!("└─ [declaration] {name}")),
                "{name}: {block}"
            );
        }
    }
}

/// A program deriving a witness, and the name its listing leads it with.
const DERIVED: &str = "struct Dot: pub Type {\n    x: /std/Nat,\n}\n\nsatisfy /std/Spell(Dot) { .. }\n\n/std/print(\"\")\n";

/// The name a witness of [`DERIVED`] is listed under.
const DERIVED_NAME: &str = "/witness~/std/Spell/Spell(/Dot)";

/// A witness asked by its name is answered by what it witnesses as its author wrote it, its source the head alone, and a derived one at `core-elab` by the body its derivation wrote, which its source does not hold.
#[test]
fn a_derived_witness_is_answered_with_its_body_at_core_elab() {
    let root = temporary("derived-witness");
    fs::create_dir_all(&root).unwrap();

    let listed = stdout(&curios(
        &root,
        &["wonder", "--name", "/std/Spell", "--witnesses", "-"],
        DERIVED,
    ));
    assert!(
        listed.contains(&format!(
            "├─ {DERIVED_NAME}: satisfy /std/Spell(Dot); <stdin>:5:9\n"
        )),
        "{listed}"
    );

    let named = stdout(&curios(
        &root,
        &["wonder", "--name", DERIVED_NAME, "-"],
        DERIVED,
    ));
    assert_eq!(
        named,
        "/std/Spell(Dot)\n│  derived\n├─ satisfy /std/Spell(Dot)\n└─ [declaration] <stdin>:5:9\n"
    );

    let staged = stdout(&curios(
        &root,
        &[
            "wonder",
            "--name",
            DERIVED_NAME,
            "--stage",
            "core-elab",
            "-",
        ],
        DERIVED,
    ));
    let (_, part) = staged
        .split_once("\n\n")
        .expect("a blank line parts the block from the part");
    assert!(part.contains(&format!("let {DERIVED_NAME}:")), "{part}");
    assert!(part.contains("/std/Spell/record("), "{part}");
}

/// A witness has no name a program writes, so the word that opens it is where a place answers it, as `--name` answers it; and the word stays the keyword it is.
#[test]
fn a_witness_is_answered_at_the_word_that_opens_it() {
    let root = temporary("witness-word");
    fs::create_dir_all(&root).unwrap();
    write(&root, "derived.crs", DERIVED);

    for column in [1, 7] {
        let place = format!("derived.crs:5:{column}");
        assert_eq!(
            stdout(&curios(&root, &["wonder", "--at", &place], "")),
            "/std/Spell(Dot)\n│  derived\n├─ satisfy /std/Spell(Dot)\n└─ [declaration] derived.crs:5:9\n",
            "{place}"
        );
    }
    let classes = stdout(&curios(&root, &["wonder", "--classes", "derived.crs"], ""));
    assert!(
        classes.contains("derived.crs:5:1\t7\tkeyword\t-\n"),
        "{classes}"
    );
}

/// A witness's name no witness in scope is declared under is refused by kind, pointing to the listing of its concept's witnesses; and a witness, which has no path, is refused at every view that finds a part of the program by one.
#[test]
fn a_witness_is_refused_where_nothing_answers_its_name() {
    let root = temporary("refused-witness");
    fs::create_dir_all(&root).unwrap();
    let refused = |arguments: &[&str]| {
        let output = curios(&root, arguments, DERIVED);
        assert_eq!(output.status.code(), Some(1), "{arguments:?}");

        String::from_utf8_lossy(&output.stderr).into_owned()
    };

    assert_eq!(
        refused(&["wonder", "--name", "/witness~/std/Spell/Spell(/Nope)", "-"]),
        "unknown witness\n│  no witness in scope is declared under this name\n│  `--name /std/Spell/Spell --witnesses` lists those that are\n└─ [error] /witness~/std/Spell/Spell(/Nope)\n"
    );
    for view in [&["--stage", "anf"][..], &["--footprint"][..]] {
        let mut arguments = vec!["wonder", "--name", DERIVED_NAME];
        arguments.extend_from_slice(view);
        arguments.push("-");
        let said = refused(&arguments);
        assert!(said.starts_with("no path\n"), "{said}");
        assert!(
            said.contains("│  a witness's rungs end at `core-elab`\n"),
            "{said}"
        );
    }
}

/// A module is answered by a block: whether its `mod` line is written `pub`, how many names and modules it exposes, an entry behind a `├─` for each thing it exposes, and its `mod` line's place as the close. One no documentation page shows, a private one, is answered as one a page shows is, with nothing it keeps.
#[test]
fn a_module_is_answered_by_a_block_whether_or_not_a_page_shows_it() {
    let root = project("exports");
    write(&root, "lib.crs", "pub mod util;\nmod kept;\n");
    write(
        &root,
        "kept.crs",
        "pub let word: /std/Str =\n    \"kept\";\nlet hidden: /std/Str =\n    \"\";\npub mod deeper;\n",
    );
    write(
        &root,
        "kept/deeper.crs",
        "pub let one: /std/Nat =\n    1;\n",
    );

    let shown = curios(&root, &["wonder", "--name", "/app/util", "app.crs"], "");
    assert!(
        shown.status.success(),
        "{}",
        String::from_utf8_lossy(&shown.stderr)
    );
    assert_eq!(
        stdout(&shown),
        concat!(
            "/app/util\n",
            "│  public module, exposing 1 name\n",
            "├─ pub let word: /std/Str\n",
            "└─ [module] lib.crs:1:9\n",
        )
    );

    let kept = curios(&root, &["wonder", "--name", "/app/kept", "app.crs"], "");
    assert!(
        kept.status.success(),
        "{}",
        String::from_utf8_lossy(&kept.stderr)
    );
    assert_eq!(
        stdout(&kept),
        concat!(
            "/app/kept\n",
            "│  private module, exposing 1 name and 1 module\n",
            "├─ pub let word: /std/Str\n",
            "├─ pub mod deeper, exposing 1 name\n",
            "└─ [module] lib.crs:2:5\n",
        )
    );
}

/// A module no documentation page shows is read off the lowering, so it answers where its library does not check, and a module a page shows is refused there as a build is.
#[test]
fn a_private_module_answers_where_its_library_does_not_check() {
    let root = project("exports-unchecked");
    write(&root, "lib.crs", "pub mod util;\nmod kept;\n");
    write(&root, "util.crs", "pub let word: /std/Str =\n    true;\n");
    write(
        &root,
        "kept.crs",
        "pub let word: /std/Str =\n    \"kept\";\n",
    );

    let kept = curios(&root, &["wonder", "--name", "/app/kept", "app.crs"], "");
    assert!(
        kept.status.success(),
        "{}",
        String::from_utf8_lossy(&kept.stderr)
    );
    assert_eq!(
        stdout(&kept),
        concat!(
            "/app/kept\n",
            "│  private module, exposing 1 name\n",
            "├─ pub let word: /std/Str\n",
            "└─ [module] lib.crs:2:5\n",
        )
    );

    let shown = curios(&root, &["wonder", "--name", "/app/util", "app.crs"], "");
    let refused = String::from_utf8_lossy(&shown.stderr);
    assert!(!shown.status.success());
    assert!(refused.contains("[error] util.crs:2:5"), "{refused}");
}

/// `--name` answers everything a path names: a module by its block, and a facade's path, which names a declaration and a module, by the declaration and then the module, a blank line between.
#[test]
fn a_path_naming_a_module_answers_the_module_beside_its_declaration() {
    let root = project("named-module");
    write(
        &root,
        "lib.crs",
        "pub mod util;\npub mod word;\npub use word/{let word};\n",
    );
    write(
        &root,
        "word.crs",
        "pub let word: /std/Str =\n    \"w\";\npub let other: /std/Str =\n    \"o\";\n",
    );

    let module = curios(&root, &["wonder", "--name", "/app/util", "app.crs"], "");
    assert!(
        module.status.success(),
        "{}",
        String::from_utf8_lossy(&module.stderr)
    );
    assert_eq!(
        stdout(&module),
        concat!(
            "/app/util\n",
            "│  public module, exposing 1 name\n",
            "├─ pub let word: /std/Str\n",
            "└─ [module] lib.crs:1:9\n",
        )
    );

    let facade = curios(&root, &["wonder", "--name", "/app/word", "app.crs"], "");
    assert!(
        facade.status.success(),
        "{}",
        String::from_utf8_lossy(&facade.stderr)
    );
    assert_eq!(
        stdout(&facade),
        concat!(
            "/app/word\n",
            "│  total\n",
            "│  declared at `/app/word/word`\n",
            "├─ pub let word: /std/Str\n",
            "└─ [declaration] word.crs:1:9\n",
            "\n",
            "/app/word\n",
            "│  public module, exposing 1 more name\n",
            "├─ pub let other: /std/Str\n",
            "└─ [module] lib.crs:2:9\n",
        )
    );
}

/// A declaration's block states what its source does not show, in the words its documentation page says the same facts in — how its representation stands, that it is a proof, whether it is proven total — and holds its source as one entry: its prose above its head, its members with it where its representation is public and its head alone where it is kept, as its page shows it, and its body left out. What the source line shows, its keyword and its own `pub`, is no fact, and neither is a type the head states.
#[test]
fn a_block_states_what_a_source_does_not_show_and_holds_the_source() {
    let root = temporary("named-facts");
    write(&root, "facts.crs", FACTS);
    let named = |path: &str| {
        let answered = curios(&root, &["wonder", "--name", path, "facts.crs"], "");
        assert!(
            answered.status.success(),
            "{}",
            String::from_utf8_lossy(&answered.stderr)
        );

        stdout(&answered)
    };

    assert_eq!(
        named("/Shade"),
        concat!(
            "/Shade\n",
            "│  opaque\n",
            "├─ --- A shade, kept.\n",
            "│  struct Shade: Type\n",
            "└─ [declaration] facts.crs:4:8\n",
        )
    );
    assert_eq!(
        named("/Sized"),
        concat!(
            "/Sized\n",
            "│  sealed\n",
            "├─ --- A size.\n",
            "│  concept Sized(T: Type): Type {\n",
            "│      size(T) -> Nat,\n",
            "│  }\n",
            "└─ [declaration] facts.crs:9:9\n",
        )
    );
    assert_eq!(
        named("/Tone"),
        concat!(
            "/Tone\n",
            "│  transparent\n",
            "├─ induct Tone: pub Type\n",
            "│  | high()\n",
            "│  | low()\n",
            "│  end\n",
            "└─ [declaration] facts.crs:14:8\n",
        )
    );
    assert!(named("/Named").starts_with("/Named\n│  unsealed\n"));
    assert_eq!(
        named("/spin"),
        concat!(
            "/spin\n",
            "│  unfounded\n",
            "├─ --- Never returns.\n",
            "│  let spin(n: Nat) -> Nat\n",
            "└─ [declaration] facts.crs:24:5\n",
        )
    );
    assert_eq!(
        named("/one_is_one"),
        concat!(
            "/one_is_one\n",
            "│  proof, total\n",
            "├─ --- One is one.\n",
            "│  let one_is_one: Eq()(1, 1)\n",
            "└─ [declaration] facts.crs:28:5\n",
        )
    );
    // A member is answered within its owner's head and closing word, its own prose above its own line and its siblings left out.
    assert_eq!(
        named("/Sized/size"),
        concat!(
            "/Sized/size\n",
            "├─ concept Sized(T: Type): Type {\n",
            "│      --- How big.\n",
            "│      size(T) -> Nat,\n",
            "│  }\n",
            "└─ [declaration] facts.crs:11:5\n",
        )
    );
    assert_eq!(
        named("/Tone/low"),
        concat!(
            "/Tone/low\n",
            "├─ induct Tone: pub Type\n",
            "│  | low()\n",
            "│  end\n",
            "└─ [declaration] facts.crs:16:3\n",
        )
    );

    // A place answers the name written there by the same block: a proof of the file's own, once the file checks, and a witness, which has no path and is answered by its head, the compiler writing its body.
    let at = |place: &str| stdout(&curios(&root, &["wonder", "--at", place], ""));
    assert_eq!(at("facts.crs:28:5"), named("/one_is_one"));
    assert_eq!(
        at("facts.crs:35:24"),
        concat!(
            "/std/Spell(Dot)\n",
            "│  derived\n",
            "├─ satisfy /std/Spell(Dot)\n",
            "└─ [declaration] facts.crs:35:9\n",
        )
    );
}

/// What the compiler provides is answered as any other declaration: by the word that says the compiler provides it, by its source as the library's documentation holds it, and on the path asked, since no reader writes the root that declares it. A facade's module then lists what it exposes beside that declaration.
#[test]
fn what_the_compiler_provides_is_answered_on_the_path_asked() {
    let root = temporary("named-intrinsic");
    fs::create_dir_all(&root).unwrap();
    let named = |path: &str| stdout(&curios(&root, &["wonder", "--name", path], ""));

    let option = named("/std/Option");
    assert!(
        option.starts_with("/std/Option\n│  transparent, intrinsic\n│  witnesses: "),
        "{option}"
    );
    assert!(
        option.contains(concat!(
            "│  pub induct Option(A: Sort): pub Type\n",
            "│  | none()\n",
            "│  | some(A)\n",
            "│  end\n",
            "├─ try; curios wonder --name /std/Option --witnesses\n",
            "└─ [declaration] /std/Option\n",
            "\n",
            "/std/Option\n",
        )),
        "{option}"
    );
    assert!(
        option.contains("more names\n├─ pub use Option/{none, some}\n"),
        "the module lists what it exposes beside the declaration: {option}"
    );
    assert!(!option.contains("/sys"), "{option}");

    assert_eq!(
        named("/std/Option/some"),
        concat!(
            "/std/Option/some\n",
            "│  intrinsic\n",
            "├─ pub induct Option(A: Sort): pub Type\n",
            "│  --- The value is there.\n",
            "│  | some(A)\n",
            "│  end\n",
            "└─ [declaration] /std/Option/some\n",
        )
    );

    // A place that names what the compiler provides answers the same: its source off the library's documentation, on the path a reader writes.
    let placed = stdout(&curios(
        &root,
        &["wonder", "--at", "<stdin>:1:13"],
        "let n: /std/Nat = 1;\n/std/print(\"\")\n",
    ));
    assert!(
        placed.starts_with("/std/Nat\n│  intrinsic, total\n│  witnesses: "),
        "{placed}"
    );
    assert!(
        placed.contains("│  pub let Nat: Type\n├─ try; curios wonder --name /std/Nat --witnesses -\n└─ [declaration] /std/Nat\n"),
        "{placed}"
    );
}

/// A block says the path a declaration is declared at where its last line does not: a declaration of a library read from no file closes on that path, and one a file holds on its place, the path above it.
#[test]
fn a_block_says_where_a_declaration_is_declared_once() {
    let root = temporary("named-declared-at");
    fs::create_dir_all(&root).unwrap();

    let show = stdout(&curios(&root, &["wonder", "--name", "/std/Show"], ""));
    assert!(
        show.starts_with("/std/Show\n│  unsealed\n│  witnesses: "),
        "{show}"
    );
    assert!(!show.contains("declared at"), "{show}");
    assert!(
        show.contains("\n└─ [declaration] /std/Show/Show\n"),
        "{show}"
    );

    // A name of a unit in scope that is a proof says so where it is written, as the unit's check established it.
    let proof = stdout(&curios(
        &root,
        &["wonder", "--at", "<stdin>:1:28"],
        "let q = /std/Option/some_inj;\n/std/print(\"\")\n",
    ));
    assert!(
        proof.starts_with("/std/Option/some_inj\n│  proof, total\n├─ --- "),
        "{proof}"
    );
}

/// `--classes` lists a row a span, four fields joined by tabs in every row — its place, its length, its class and its modifiers — and `-` where a span has no modifier, so a script splits every row alike.
#[test]
fn the_classes_are_rows_of_four_fields_joined_by_tabs() {
    let root = project("classes-rows");

    let classed = curios(&root, &["wonder", "--classes", "app.crs"], "");
    assert!(
        classed.status.success(),
        "{}",
        String::from_utf8_lossy(&classed.stderr)
    );
    assert_eq!(
        stdout(&classed),
        concat!(
            "app.crs:1:2\t3\tmodule\tdefault-library\n",
            "app.crs:1:6\t5\tfunction\tdefault-library\n",
            "app.crs:1:13\t3\tmodule\t-\n",
            "app.crs:1:17\t4\tmodule\t-\n",
            "app.crs:1:22\t4\tvalue\t-\n",
        )
    );
}

/// A place where nothing is named is refused, as a path that names nothing is: no answer is written, and the refusal says why.
#[test]
fn a_place_naming_nothing_is_refused() {
    let root = project("nothing-named");

    // The `pub` a declaration opens with names nothing.
    let nothing = curios(&root, &["wonder", "--at", "util.crs:1:1"], "");
    let said = String::from_utf8_lossy(&nothing.stderr);
    assert_eq!(nothing.status.code(), Some(1), "{said}");
    assert_eq!(stdout(&nothing), "");
    assert!(
        said.contains("nothing here\n│  no name or term stands at this place"),
        "{said}"
    );
}

/// A path that names nothing is refused as a program that wrote it is told: the step that failed, and the nearest name a reader may write there.
#[test]
fn a_path_naming_nothing_offers_the_nearest_name() {
    let root = temporary("nearest-named");
    fs::create_dir_all(&root).unwrap();

    let refused = curios(&root, &["wonder", "--name", "/std/Nat/ad"], "");
    let said = String::from_utf8_lossy(&refused.stderr);
    assert_eq!(refused.status.code(), Some(1), "{said}");
    assert!(
        said.contains("has no `ad`") && said.contains("did you mean `add`?"),
        "{said}"
    );
}

/// A written name is resolved where it is lowered, so a program whose first declaration is a type mismatch still answers where a name is declared.
#[test]
fn a_program_that_does_not_check_answers_where_a_name_is_declared() {
    let root = temporary("at-refused");
    fs::create_dir_all(&root).unwrap();

    let answered = curios(
        &root,
        &["wonder", "--at", "<stdin>:2:21"],
        "let one: /std/Nat = true;\nlet two: /std/Nat = one;\n/std/print(\"\")\n",
    );

    assert!(
        answered.status.success(),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    assert_eq!(
        stdout(&answered),
        concat!(
            "/one\n",
            "├─ let one: /std/Nat\n",
            "└─ [declaration] <stdin>:1:5\n",
        )
    );
}

/// A stage that the program does not reach is not an answer: diagnostics on stderr, exit 1, stdout empty.
#[test]
fn an_unreached_stage_leaves_stdout_empty() {
    let root = temporary("unreached");
    fs::create_dir_all(&root).unwrap();

    let refused = curios(
        &root,
        &["wonder", "--stage", "wasm", "-"],
        "let m : /std/Nat = true;\n/std/print(\"\")\n",
    );

    assert_eq!(refused.status.code(), Some(1));
    assert!(refused.stdout.is_empty());
    assert!(String::from_utf8_lossy(&refused.stderr).contains("type mismatch"));
}

/// A question stopped before it could answer exits as a build would, so written goals alone are the incomplete state that exits 2 — for `--stage`, `--footprint` and `--tests` alike — and so does `check`, which reports them.
#[test]
fn a_question_stopped_by_goals_alone_exits_two() {
    let root = temporary("goals");
    fs::create_dir_all(&root).unwrap();
    let program = "let _n : /std/Nat = ?;\n/std/print(\"\")\n";

    for query in [
        &["wonder", "--stage", "wasm", "-"][..],
        &["wonder", "--name", "/_n", "--footprint", "-"],
        &["wonder", "--tests", "-"],
    ] {
        let stopped = curios(&root, query, program);
        assert_eq!(
            stopped.status.code(),
            Some(2),
            "{}: {}",
            query.join(" "),
            String::from_utf8_lossy(&stopped.stderr)
        );
        assert!(stopped.stdout.is_empty(), "{}", query.join(" "));
    }

    let answered = curios(&root, &["check", "-"], program);
    assert_eq!(answered.status.code(), Some(2));
}

/// A check of a package compiles its library twice where an executable stands on it, with its tests, which the check reads, and as the executable is built over it: the transcript says which the first is, so the two compiles of one unit are told apart.
#[test]
fn a_library_checked_with_its_tests_is_narrated_as_that() {
    let root = project("tested-narrated");

    let checked = curios(&root, &["check"], "");

    let narrated = String::from_utf8_lossy(&checked.stderr);
    assert!(checked.status.success(), "{narrated}");
    // Each line up to what its step took, which no two runs share.
    let steps = narrated
        .lines()
        .map(|line| line.split(" in ").next().unwrap_or(line))
        .collect::<Vec<_>>();
    assert_eq!(
        steps,
        [
            "Checking `/app` · tests",
            "Checked `/app` · tests",
            "Storing `/app` · tests",
            "Stored `/app` · tests",
            "Checking `/app`",
            "Checked `/app`",
            "Storing `/app`",
            "Stored `/app`",
            "Checking `app`",
            "Checked `app`",
            "0 lints",
        ],
        "{narrated}"
    );
}

/// `--footprint` answers the block `--name` answers with, without the source it holds there, then, a blank line before, a row a function of four tab-separated fields: here the one row of a declaration the program never reaches.
#[test]
fn a_footprint_is_the_block_then_a_row_of_four_fields_a_function() {
    let root = temporary("footprint");
    fs::create_dir_all(&root).unwrap();
    let program = "let helper(x : /std/Nat) -> /std/Nat = x + 1;\n/std/print(\"hello\\n\")\n";

    let answered = curios(
        &root,
        &["wonder", "--name", "/helper", "--footprint", "-"],
        program,
    );
    assert_eq!(
        answered.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    let stdout = String::from_utf8_lossy(&answered.stdout);
    let (block, rows) = stdout
        .split_once("\n\n")
        .expect("a blank line parts the block from the rows");
    assert_eq!(
        block, "/helper\n│  total\n└─ [declaration] <stdin>:1:5",
        "{stdout}"
    );
    assert_eq!(rows, "/helper\tnot linked\t-\t-\n");
}

/// `--stage` beside `--name` answers the block, without the source it holds for `--name` alone, then, a blank line before, the declaration's part of the rung, closed on a line of its own: at `text`, its text reprinted, and nothing of its neighbours.
#[test]
fn a_stage_beside_a_name_is_the_block_then_the_declarations_part() {
    let root = temporary("named-stage");
    fs::create_dir_all(&root).unwrap();
    let program = "let helper(x : /std/Nat) -> /std/Nat = x + 1;\n\nlet other(x : /std/Nat) -> /std/Nat = x;\n\n/std/print(\"hello\\n\")\n";

    let answered = curios(
        &root,
        &["wonder", "--name", "/helper", "--stage", "text", "-"],
        program,
    );
    assert_eq!(
        answered.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    let stdout = String::from_utf8_lossy(&answered.stdout);
    let (block, part) = stdout
        .split_once("\n\n")
        .expect("a blank line parts the block from the part");
    assert_eq!(
        block, "/helper\n│  total\n└─ [declaration] <stdin>:1:5",
        "{stdout}"
    );
    assert_eq!(part, "let helper(x: /std/Nat) -> /std/Nat =\n    x + 1;\n");
}

/// A place's answer at a bare name a `use` line brought into scope states that line as it is written, an entry closed by where the line writes what brought the name, each name by its own line; an answer at a name the file declares, or at the line itself, states none.
#[test]
fn a_bare_name_is_answered_with_the_line_that_brought_it() {
    let root = temporary("named-brought");
    write(
        &root,
        "brought.crs",
        "use /std/{Nat};\nuse /std/{Bool, print};\n\npub let same(n: Nat) -> Bool =\n    n == n;\n\npub let twice(n: Nat) -> Bool =\n    same(n);\n\nprint(\"\")\n",
    );
    let at = |place: &str| stdout(&curios(&root, &["wonder", "--at", place], ""));

    for (place, line) in [
        ("brought.crs:4:17", "use /std/{Nat}; brought.crs:1:11"),
        (
            "brought.crs:4:25",
            "use /std/{Bool, print}; brought.crs:2:11",
        ),
        (
            "brought.crs:10:1",
            "use /std/{Bool, print}; brought.crs:2:17",
        ),
    ] {
        let answered = at(place);
        assert!(
            answered.contains(&format!("\n├─ {line}\n")),
            "{place}: {answered}"
        );
    }
    for place in ["brought.crs:8:5", "brought.crs:1:11"] {
        let answered = at(place);
        assert!(!answered.contains("├─ use "), "{place}: {answered}");
    }
}
