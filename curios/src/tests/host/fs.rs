//! The filesystem through `/std/fs`: what is at a path, listing, making, moving and removing, and the whole-file twins, against a scripted tree — every call a `Try` over `Io`, so a failure the test does not expect ends the program with its name.

use {crate::tests::test_support::*, curios_common::fill, curios_native::test_support::*};

/// A scripted tree: two files under `data`, one of them a level down, so `data` and `data/sub` exist without being named.
fn seeded() -> (MockHost, MockIo) {
    MockHost::builder()
        .files([("data/a.txt", "aaa"), ("data/sub/b.txt", "b")])
        .build()
}

/// The `Str` a program's `Try` region computes, printed once at the tail; a failure that escapes the region prints as its name instead.
fn program(body: &str) -> String {
    fill!(
        r#"
        use /std/{Str, Bytes, Nat, Bool, List, Option, Result, Show, Try, Io, Path, File, fs, Proc};
        let show_unit(r: Result(Io/Error, {})) -> Str =
            match r | success(_) => "ok" | failure(e) => Show/show(e) end;
        let text(b: Bytes) -> Str =
            Result/unwrap_or(Str/of_bytes(b), "?");
        let program() -> Try(Io, Io/Error, Str) =
            ${body};
        let entry() -> /std/Io({}) =
            match Try/run(program())!
            | failure(e) => /std/print(Show/show(e))
            | success(s) => /std/print(s)
            end;
        entry()
        "#,
        body
    )
}

// `exists`, `is_dir` and `is_file` read `not_found` as `false`; `list` hands back the names as paths in byte order; removing a directory that has entries is `not_empty`, caught at its own `run`; a file's size comes back through the envelope as `some(3)`.
#[test]
fn a_seeded_tree_is_inspected_listed_and_refused_removal_while_full() {
    let source = program(
        r#"
            let a = fs/exists(Path/of_str("data"))!;
            let b = fs/is_dir(Path/of_str("data"))!;
            let c = fs/is_file(Path/of_str("data/a.txt"))!;
            let d = fs/exists(Path/of_str("nope"))!;
            let names = fs/list(Path/of_str("data"))!;
            let listed = Str/join(",", List/map(names, (n: Path) => Show/show(n)));
            let rm = Try/run(fs/remove_dir(Path/of_str("data")))!;
            let m = fs/stat(Path/of_str("data/a.txt"))!;
            let size = Str/flatten([Show/show(m.kind), " ", Show/show(m.size)]);
            Try/pure(Str/join(" ", [Bool/to_str(a), Bool/to_str(b), Bool/to_str(c), Bool/to_str(d), listed, show_unit(rm), size]))
        "#,
    );

    let (host, io) = seeded();
    run_text(&source, host).expect("expected result");
    assert_eq!(
        io.output(),
        b"true true true false a.txt,sub not_empty file 3"
    );
}

// The rewriting surface end to end: `create_dir_all` makes every prefix, `write_all`/`read_all` round-trip a file inside it, `rename` moves the directory with its contents, `remove_all` takes the tree down, and `cwd` is the host's.
#[test]
fn directories_are_made_moved_and_removed_whole() {
    let source = program(
        r#"
            let _ = fs/create_dir_all(Path/of_str("x/y/z"))!;
            let made = fs/is_dir(Path/of_str("x/y/z"))!;
            let _ = File/write_all(Path/of_str("x/y/z/f.txt"), Str/to_bytes("hi"))!;
            let back = File/read_all(Path/of_str("x/y/z/f.txt"))!;
            let _ = fs/rename(Path/of_str("x/y"), Path/of_str("x/w"))!;
            let moved = fs/is_file(Path/of_str("x/w/z/f.txt"))!;
            let _ = fs/remove_all(Path/of_str("x"))!;
            let gone = fs/exists(Path/of_str("x"))!;
            let here = Proc/cwd()!;
            Try/pure(Str/join(" ", [Bool/to_str(made), text(back), Bool/to_str(moved), Bool/to_str(gone), Show/show(here)]))
        "#,
    );

    let (host, io) = MockHost::builder().cwd("/work").build();
    run_text(&source, host).expect("expected result");
    assert_eq!(io.output(), b"true hi true false /work");
    assert_eq!(io.file(b"x/w/z/f.txt"), None);
}

// `create_dir_all` under a prefix that already exists leaves it as it is and makes the rest, and made twice it is `ok` twice.
#[test]
fn create_dir_all_keeps_an_existing_prefix_and_is_idempotent() {
    let source = program(
        r#"
            let _ = fs/create_dir_all(Path/of_str("data/sub/new/deep"))!;
            let again = Try/run(fs/create_dir_all(Path/of_str("data/sub/new/deep")))!;
            let made = fs/is_dir(Path/of_str("data/sub/new/deep"))!;
            let kept = fs/is_file(Path/of_str("data/sub/b.txt"))!;
            Try/pure(Str/join(" ", [show_unit(again), Bool/to_str(made), Bool/to_str(kept)]))
        "#,
    );

    let (host, io) = seeded();
    run_text(&source, host).expect("expected result");
    assert_eq!(io.output(), b"ok true true");
}

// Three status codes, each told apart by `Show(Io/Error)` at its own `run`: a file operation on a directory, a directory operation on a file, and a listing of nothing.
#[test]
fn the_named_filesystem_failures_show_by_name() {
    let source = program(
        r#"
            let a = Try/run(fs/remove_file(Path/of_str("data")))!;
            let b = Try/run(fs/remove_dir(Path/of_str("data/a.txt")))!;
            let c = Try/run(fs/list(Path/of_str("nope")))!;
            let listed = match c | success(_) => "listed" | failure(e) => Show/show(e) end;
            Try/pure(Str/join(" ", [show_unit(a), show_unit(b), listed]))
        "#,
    );

    let (host, io) = seeded();
    run_text(&source, host).expect("expected result");
    assert_eq!(io.output(), b"is_directory not_directory not_found");
}

// A failure the region does not catch ends the program with its name: a `stat` of nothing is `not_found` at the tail, and nothing after it runs.
#[test]
fn an_uncaught_failure_ends_the_region_with_its_name() {
    let source = program(
        r#"
            let m = fs/stat(Path/of_str("nope"))!;
            Try/pure(Show/show(m.kind))
        "#,
    );

    let (host, io) = seeded();
    run_text(&source, host).expect("expected result");
    assert_eq!(io.output(), b"not_found");
}

/// The temporaries a program makes in a scope under a `/scratch` that `TMPDIR` names: a file written through the handle it answers and read back by its path, and a directory holding a file, each printed by its path, then whether each is still there once the scope has ended.
fn temporaries(prefix: &str) -> String {
    program(&fill!(
        r#"
            let made =
                Try/scope((s) =>
                    let t = File/temp(s, "${prefix}")!;
                    let () = Io/write(t.file, Str/to_bytes("held"))!;
                    let held = File/read_all(t.path)!;
                    let d = fs/temp_dir(s, "work-")!;
                    let () = File/write_all(Path/join(d, Path/of_str("inner")), Str/to_bytes("x"))!;
                    Try/pure((t.path, d, text(held))))!;
            let file_kept = fs/exists(made.0)!;
            let dir_kept = fs/exists(made.1)!;
            Try/pure(Str/join(" ", [Show/show(made.0), Show/show(made.1), made.2, Bool/to_str(file_kept), Bool/to_str(dir_kept)]))
        "#,
        prefix
    ))
}

/// A temporary file and a temporary directory are made under the directory `TMPDIR` names, each named by its prefix and sixteen hexadecimal digits, the file written through the handle `File/temp` answers beside its path, and both removed when their scope ends, the directory with what it holds.
#[test]
fn a_temporary_file_and_directory_are_made_under_tmpdir_and_removed_with_their_scope() {
    let (host, io) = MockHost::builder()
        .files([("/scratch/keep", "")])
        .env([("TMPDIR", "/scratch")])
        .build();
    run_text(&temporaries("run-"), host).expect("expected result");
    let output = String::from_utf8(io.output()).expect("text");
    let fields = output.split(' ').collect::<Vec<_>>();

    let named = |path: &str, prefix: &str| {
        path.strip_prefix(prefix).is_some_and(|digits| {
            digits.len() == 16 && digits.chars().all(|digit| digit.is_ascii_hexdigit())
        })
    };
    assert_eq!(fields.len(), 5, "{output}");
    assert!(named(fields[0], "/scratch/run-"), "{output}");
    assert!(named(fields[1], "/scratch/work-"), "{output}");
    assert_eq!(fields[2..], ["held", "false", "false"], "{output}");
}

/// Where `TMPDIR` names nothing, or is empty, the temporaries are made under `/tmp`.
#[test]
fn a_temporary_is_made_under_tmp_where_tmpdir_names_nothing() {
    for env in [vec![], vec![("TMPDIR", "")]] {
        let (host, io) = MockHost::builder()
            .files([("/tmp/keep", "")])
            .env(env)
            .build();
        run_text(&temporaries("run-"), host).expect("expected result");
        let output = String::from_utf8(io.output()).expect("text");

        assert!(output.starts_with("/tmp/run-"), "{output}");
        assert!(output.ends_with(" held false false"), "{output}");
    }
}

/// The `field`th path `temporaries` prints is drawn again where a file holds the name it took: the mock host's randomness draws the same bytes in every run, so the file a first run's temporary was named, there before a second run, sends that run's temporary to another name, and is left as it was.
fn drawn_again(field: usize) {
    let (host, io) = MockHost::builder()
        .files([("/scratch/keep", "")])
        .env([("TMPDIR", "/scratch")])
        .build();
    run_text(&temporaries("run-"), host).expect("expected result");
    let first = String::from_utf8(io.output()).expect("text");
    let taken = first.split(' ').nth(field).expect("a path").to_string();

    let (host, io) = MockHost::builder()
        .files([(taken.as_str(), "someone's")])
        .env([("TMPDIR", "/scratch")])
        .build();
    run_text(&temporaries("run-"), host).expect("expected result");
    let second = String::from_utf8(io.output()).expect("text");
    let drawn = second.split(' ').nth(field).expect("a path");

    assert_ne!(drawn, taken, "{second}");
    assert!(drawn.starts_with(&taken[..taken.len() - 16]), "{second}");
    assert_eq!(io.file(taken.as_bytes()), Some(b"someone's".to_vec()));
}

/// A temporary file's name another file holds is drawn again.
#[test]
fn a_temporary_file_name_a_file_holds_is_drawn_again() {
    drawn_again(0);
}

/// A temporary directory's name a file holds is drawn again.
#[test]
fn a_temporary_directory_name_a_file_holds_is_drawn_again() {
    drawn_again(1);
}

/// A prefix holding a `/` would name another directory, so it is refused where it is written: the bound it fails reduces to `Bool/False`, and the program never runs.
#[test]
fn a_temporary_prefix_holding_a_slash_is_refused_where_it_is_written() {
    let refusal = typecheck(&temporaries("../run-")).expect_err("the prefix is refused");

    assert!(refusal.contains("undischarged bound"), "{refusal}");
    assert!(refusal.contains("`plain`"), "{refusal}");
}
