//! What `curios format` does at the command line: which files a target names — every file a package declares for none, a file itself, standard input for `-` — the width each is formatted within, and that `--check` writes nothing.

use {
    curios::CABINET,
    curios_common::test_support::*,
    curios_surface::MIN_LINE_WIDTH,
    std::{
        fs,
        io::Write,
        path::Path,
        process::{Command, Output, Stdio},
    },
};

/// A directory of its own, shared with no other test and gone with it.
fn temporary(name: &str) -> Temporary {
    Temporary::new("cli-format", name)
}

fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}

/// Run the compiler in `root`, with the arguments given and `stdin` on its standard input.
fn curios(root: &Path, arguments: &[&str], stdin: &str) -> Output {
    let mut child = Command::new(env!("CARGO_BIN_EXE_curios"))
        .env_remove(CABINET)
        .current_dir(root)
        .args(arguments)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("run the compiler");
    child
        .stdin
        .take()
        .unwrap()
        .write_all(stdin.as_bytes())
        .unwrap();
    child.wait_with_output().expect("the compiler exits")
}

/// A declaration the formatter moves: indented where the canonical form puts it at the margin.
const UNFORMATTED: &str = "    pub let word: /std/Str = \"formatted\";\n";

/// No target is every file the governing package declares, so a file beside them that nothing declares is left as it is — and `--check` names what would change without writing it.
#[test]
fn no_target_formats_what_the_package_declares_and_nothing_else() {
    let root = temporary("package");
    write(
        &root,
        "curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"app\"\n"
        ),
    );
    write(&root, "lib.crs", "pub mod util;\n");
    write(&root, "util.crs", UNFORMATTED);
    write(&root, "stray.crs", UNFORMATTED);

    let checked = curios(&root, &["format", "--check"], "");
    let stderr = String::from_utf8_lossy(&checked.stderr);
    assert!(!checked.status.success(), "{stderr}");
    assert!(stderr.contains("util.crs"), "{stderr}");
    assert!(!stderr.contains("stray.crs"), "{stderr}");
    assert_eq!(
        fs::read_to_string(root.join("util.crs")).unwrap(),
        UNFORMATTED,
        "--check writes nothing"
    );

    let formatted = curios(&root, &["format"], "");
    assert!(
        formatted.status.success(),
        "{}",
        String::from_utf8_lossy(&formatted.stderr)
    );
    assert_ne!(
        fs::read_to_string(root.join("util.crs")).unwrap(),
        UNFORMATTED
    );
    assert_eq!(
        fs::read_to_string(root.join("stray.crs")).unwrap(),
        UNFORMATTED,
        "a file nothing declares is formatted only when named"
    );
}

/// `-` formats standard input onto standard output, exactly as the file form rewrites the same text.
#[test]
fn a_dash_formats_standard_input_onto_standard_output() {
    let root = temporary("stdin");
    write(&root, "scratch.crs", UNFORMATTED);

    let piped = curios(&root, &["format", "-"], UNFORMATTED);
    assert!(
        piped.status.success(),
        "{}",
        String::from_utf8_lossy(&piped.stderr)
    );

    let rewritten = curios(&root, &["format", "scratch.crs"], "");
    assert!(
        rewritten.status.success(),
        "{}",
        String::from_utf8_lossy(&rewritten.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&piped.stdout),
        fs::read_to_string(root.join("scratch.crs")).unwrap()
    );
}

/// Core text the formatter moves: a definition whose body the canonical form writes on the line of its head.
const UNFORMATTED_CORE: &str = "let /one : #NatType =\n    1;\n";

/// `--core` reads standard input as Core text, written onto standard output exactly as the file form rewrites the same text of a `.crsc`; without it the text is read as the program it is not, and beside a file it is refused, the file's name saying what it is written in, with nothing written.
#[test]
fn core_formats_standard_input_as_a_crsc_is_formatted() {
    let root = temporary("core");
    write(&root, "proof.crsc", UNFORMATTED_CORE);

    let piped = curios(&root, &["format", "--core", "-"], UNFORMATTED_CORE);
    assert!(
        piped.status.success(),
        "{}",
        String::from_utf8_lossy(&piped.stderr)
    );

    let rewritten = curios(&root, &["format", "proof.crsc"], "");
    assert!(
        rewritten.status.success(),
        "{}",
        String::from_utf8_lossy(&rewritten.stderr)
    );
    let canonical = fs::read_to_string(root.join("proof.crsc")).unwrap();
    assert_ne!(canonical, UNFORMATTED_CORE);
    assert_eq!(String::from_utf8_lossy(&piped.stdout), canonical);

    let as_a_program = curios(&root, &["format", "-"], UNFORMATTED_CORE);
    assert!(!as_a_program.status.success());

    write(&root, "proof.crsc", UNFORMATTED_CORE);
    let beside_a_file = curios(&root, &["format", "--core", "proof.crsc"], "");
    let stderr = String::from_utf8_lossy(&beside_a_file.stderr);
    assert!(!beside_a_file.status.success(), "{stderr}");
    assert!(stderr.contains("surplus flag"), "{stderr}");
    assert_eq!(
        fs::read_to_string(root.join("proof.crsc")).unwrap(),
        UNFORMATTED_CORE
    );
}

/// A declaration the default width keeps on one line and the narrowest breaks.
const WIDE: &str =
    "let total: Nat =\n    combine(first_argument, second_argument, third_argument);\n";

/// The narrowest width a file may be formatted within, as the flag spells it.
fn narrowest() -> String {
    MIN_LINE_WIDTH.to_string()
}

/// Whether every line of `text` is within the narrowest width.
fn narrow(text: &str) -> bool {
    text.lines()
        .all(|line| line.chars().count() <= MIN_LINE_WIDTH)
}

/// `--line-width` formats standard input and a file no package governs within it, the two alike, where the default leaves the text as it is.
#[test]
fn a_width_formats_standard_input_and_a_file_no_package_governs_within_it() {
    let root = temporary("width-loose");
    write(&root, "loose.crs", WIDE);

    let piped = curios(&root, &["format", "--line-width", &narrowest(), "-"], WIDE);
    let piped_text = String::from_utf8_lossy(&piped.stdout).into_owned();
    assert!(
        piped.status.success(),
        "{}",
        String::from_utf8_lossy(&piped.stderr)
    );
    assert_ne!(piped_text, WIDE);
    assert!(narrow(&piped_text), "{piped_text}");

    assert!(
        curios(&root, &["format", "--check", "loose.crs"], "")
            .status
            .success()
    );
    let rewritten = curios(
        &root,
        &["format", "--line-width", &narrowest(), "loose.crs"],
        "",
    );
    assert!(
        rewritten.status.success(),
        "{}",
        String::from_utf8_lossy(&rewritten.stderr)
    );
    assert_eq!(
        fs::read_to_string(root.join("loose.crs")).unwrap(),
        piped_text
    );
}

/// A package's files are formatted within the width its `[format]` states, whether the package is formatted whole or a file of it is named.
#[test]
fn a_package_formats_within_the_width_its_manifest_states() {
    let root = temporary("width-package");
    write(
        &root,
        "curios.toml",
        &format!(
            "compiler = \"{compiler}\"\nname = \"app\"\n\n[format]\nline-width = {MIN_LINE_WIDTH}\n",
            compiler = env!("CARGO_PKG_VERSION")
        ),
    );
    write(&root, "lib.crs", WIDE);

    let checked = curios(&root, &["format", "--check", "lib.crs"], "");
    assert!(
        !checked.status.success(),
        "the file is wider than its package states"
    );

    let formatted = curios(&root, &["format"], "");
    assert!(
        formatted.status.success(),
        "{}",
        String::from_utf8_lossy(&formatted.stderr)
    );
    let written = fs::read_to_string(root.join("lib.crs")).unwrap();
    assert_ne!(written, WIDE);
    assert!(narrow(&written), "{written}");
    assert!(
        curios(&root, &["format", "--check", "lib.crs"], "")
            .status
            .success()
    );
}

/// `--line-width` reaches no file a package governs, since the package states its own: the command is refused before any subject is written, the one no package governs included.
#[test]
fn a_width_is_refused_for_a_file_a_package_governs_before_anything_is_written() {
    let root = temporary("width-governed");
    write(
        &root,
        "app/curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"app\"\n"
        ),
    );
    write(&root, "app/lib.crs", UNFORMATTED);
    write(&root, "loose.crs", UNFORMATTED);

    let refused = curios(
        &root,
        &[
            "format",
            "--line-width",
            &narrowest(),
            "loose.crs",
            "app/lib.crs",
        ],
        "",
    );
    let stderr = String::from_utf8_lossy(&refused.stderr);
    assert!(!refused.status.success(), "{stderr}");
    assert!(
        stderr.contains("the package `app` takes its format from"),
        "{stderr}"
    );
    for file in ["loose.crs", "app/lib.crs"] {
        assert_eq!(
            fs::read_to_string(root.join(file)).unwrap(),
            UNFORMATTED,
            "{file}"
        );
    }
}

/// `text` with every attribute code taken out.
fn stripped(text: &str) -> String {
    let mut stripped = String::new();
    let mut rest = text;

    while let Some((before, coded)) = rest.split_once("\x1b[") {
        stripped.push_str(before);
        rest = coded.split_once('m').map_or("", |(_, after)| after);
    }
    stripped.push_str(rest);

    stripped
}

/// Colour is a layer over what a command reports: asked for, it paints a subject bold and what went wrong red, over bytes that are the plain report's once the codes are taken out; and a pipe, which is no terminal, is written none unless it is asked for.
#[test]
fn a_painted_report_stripped_of_its_codes_is_the_plain_one() {
    let root = temporary("painted");
    write(&root, "loose.crs", "let x:/std/Nat=1;\nx\n");
    write(&root, "broken.crs", "let = ;\n");
    let reported = |color: &[&str]| {
        let arguments = [color, &["format", "--check", "loose.crs", "broken.crs"]].concat();
        let output = curios(&root, &arguments, "");
        assert!(!output.status.success());

        String::from_utf8(output.stderr).unwrap()
    };

    let plain = reported(&["--color", "never"]);
    assert!(
        plain.starts_with(
            "Formatted `loose.crs`: would change\nFormatted `broken.crs`: refused\n\n"
        ),
        "{plain}"
    );
    assert_eq!(reported(&[]), plain, "a pipe is written no code");

    let painted = reported(&["--color", "always"]);
    assert!(
        painted.starts_with(
            "Formatted \x1b[1m`loose.crs`\x1b[0m: would change\nFormatted \x1b[1m`broken.crs`\x1b[0m: \x1b[31mrefused\x1b[0m\n\n"
        ),
        "{painted:?}"
    );
    assert_eq!(stripped(&painted), plain);
}

/// A match the default keeps on one line, of two arms.
const DECIDED: &str = "let f(n: Nat) -> Nat =\n    match n | 0 => 1 | p + 1 => p end;\n";

/// `--flat-match-arms` and `--flat-match-width` hold a match on standard input to what they state, and a manifest's `[format]` holds a package's file to the same: a match of more arms than stated, or wider than the span, is broken. A flat match wider than its lines is refused, by the flag on the command line and by the key in the manifest.
#[test]
fn a_flat_match_is_stated_by_a_flag_and_by_a_manifest_alike() {
    let root = temporary("flat-match");
    write(&root, "loose.crs", DECIDED);
    let kept = |text: &str| {
        text.lines()
            .any(|line| line.trim_start().starts_with("match n |"))
    };

    let unchanged = curios(&root, &["format", "-"], DECIDED);
    assert!(kept(&String::from_utf8_lossy(&unchanged.stdout)));
    for flags in [["--flat-match-arms", "1"], ["--flat-match-width", "0"]] {
        let piped = curios(&root, &["format", flags[0], flags[1], "-"], DECIDED);
        let text = String::from_utf8_lossy(&piped.stdout);
        assert!(
            piped.status.success(),
            "{}",
            String::from_utf8_lossy(&piped.stderr)
        );
        assert!(!kept(&text), "{flags:?}: {text}");
    }

    let past = curios(
        &root,
        &[
            "format",
            "--line-width",
            &narrowest(),
            "--flat-match-width",
            "70",
            "-",
        ],
        DECIDED,
    );
    let stderr = String::from_utf8_lossy(&past.stderr);
    assert!(!past.status.success(), "{stderr}");
    assert!(stderr.contains("--flat-match-width"), "{stderr}");

    let compiler = env!("CARGO_PKG_VERSION");
    write(
        &root,
        "app/curios.toml",
        &format!("compiler = \"{compiler}\"\nname = \"app\"\n\n[format]\nflat-match-arms = 1\n"),
    );
    write(&root, "app/lib.crs", DECIDED);
    let rewritten = curios(&root.join("app"), &["format", "lib.crs"], "");
    assert!(
        rewritten.status.success(),
        "{}",
        String::from_utf8_lossy(&rewritten.stderr)
    );
    assert!(!kept(
        &fs::read_to_string(root.join("app/lib.crs")).unwrap()
    ));

    write(
        &root,
        "app/curios.toml",
        &format!(
            "compiler = \"{compiler}\"\nname = \"app\"\n\n[format]\nline-width = 60\nflat-match-width = 70\n"
        ),
    );
    let refused = curios(&root.join("app"), &["format", "--check"], "");
    let stderr = String::from_utf8_lossy(&refused.stderr);
    assert!(!refused.status.success(), "{stderr}");
    assert!(stderr.contains("`70` is past `60`"), "{stderr}");
}
