//! The filesystem through `/std/fs`: what is at a path, listing, making, moving and removing, and the whole-file twins, against a scripted tree — every call a `Try` over `Io`, so a failure the test does not expect ends the program with its name.

use {
    crate::tests::run_text,
    curios_runtime::{MockHost, MockIo},
};

/// A scripted tree: two files under `data`, one of them a level down, so `data` and `data/sub` exist without being named.
fn seeded() -> (MockHost, MockIo) {
    MockHost::builder()
        .files([("data/a.txt", "aaa"), ("data/sub/b.txt", "b")])
        .build()
}

/// The `Str` a program's `Try` region computes, printed once at the tail; a failure that escapes the region prints as its name instead.
fn program(body: &str) -> String {
    format!(
        r#"
        use /std/{{Str, Bytes, Nat, Bool, List, Option, Result, Show, Try, Io, Path, File, fs}};
        let show_unit(r: Result(Io/Error, {{}})) -> Str =
            match r | success(_) => "ok" | failure(e) => Show/show(e) end;
        let text(b: Bytes) -> Str =
            Option/unwrap_or(Str/of_bytes(b), "?");
        let program: Try(Io, Io/Error, Str) =
            {body};
        match Try/run(program)!
        | failure(e) => /std/print(Show/show(e))
        | success(s) => /std/print(s)
        end
        "#
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
            let size = Str/flatten([Show/show(m.kind), " ", Show/show(fs/size_nat(m))]);
            Try/pure(Str/join(" ", [Bool/to_str(a), Bool/to_str(b), Bool/to_str(c), Bool/to_str(d), listed, show_unit(rm), size]))
        "#,
    );

    let (host, io) = seeded();
    run_text(&source, host).expect("expected result");
    assert_eq!(
        io.output(),
        b"true true true false a.txt,sub not_empty file some(3)"
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
            let here = fs/cwd!;
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

// The three status codes this campaign named, each told apart by `Show(Io/Error)` at its own `run`: a file operation on a directory, a directory operation on a file, and a listing of nothing.
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
