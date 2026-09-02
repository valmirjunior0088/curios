//! The filesystem through `/std/fs`: what is at a path, listing, making, moving and removing, the whole-file twins, and the path functions, against a scripted tree.

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

/// The `Str` a program's `Async` computes, printed once at the tail.
fn program(body: &str) -> String {
    format!(
        r#"
        use /std/{{Str, Bytes, Nat, Bool, List, Option, Result, Show, Async, Io, Handle, File, fs}};
        let show_bool(r: Result(Handle/Error, Bool)) -> Str =
            match r | success(b) => Bool/to_str(b) | failure(e) => Show/show(e) end;
        let show_unit(r: Result(Handle/Error, {{}})) -> Str =
            match r | success(_) => "ok" | failure(e) => Show/show(e) end;
        let text(r: Result(Handle/Error, Bytes)) -> Str =
            match r | success(b) => Option/unwrap_or(Str/of_bytes(b), "?") | failure(e) => Show/show(e) end;
        let program: Async(Str) =
            {body};
        match Async/block_on(program)!
        | failure(_) => /std/print("deadlock")
        | success(s) => /std/print(s)
        end
        "#
    )
}

// `exists`, `is_dir` and `is_file` read `not_found` as `false`; `list` hands back the names as bytes in byte order; removing a directory that has entries is `not_empty`; a file's size comes back through the envelope as `some(3)`.
#[test]
fn a_seeded_tree_is_inspected_listed_and_refused_removal_while_full() {
    let source = program(
        r#"
            let a = fs/exists("data")!;
            let b = fs/is_dir("data")!;
            let c = fs/is_file("data/a.txt")!;
            let d = fs/exists("nope")!;
            let names = fs/list("data")!;
            let listed =
                match names
                | success(ns) => Str/join(",", List/map(ns, (n: Bytes) => Option/unwrap_or(Str/of_bytes(n), "?")))
                | failure(e) => Show/show(e)
                end;
            let rm = fs/remove_dir("data")!;
            let st = fs/stat("data/a.txt")!;
            let size =
                match st
                | success(m) => Str/flatten([Show/show(m.kind), " ", Show/show(fs/size_nat(m))])
                | failure(e) => Show/show(e)
                end;
            Async/pure(Str/join(" ", [show_bool(a), show_bool(b), show_bool(c), show_bool(d), listed, show_unit(rm), size]))
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
            let made_all = fs/create_dir_all("x/y/z")!;
            let made = fs/is_dir("x/y/z")!;
            let written = File/write_all("x/y/z/f.txt", Str/to_bytes("hi"))!;
            let back = File/read_all("x/y/z/f.txt")!;
            let renamed = fs/rename("x/y", "x/w")!;
            let moved = fs/is_file("x/w/z/f.txt")!;
            let removed = fs/remove_all("x")!;
            let gone = fs/exists("x")!;
            let here = fs/cwd!;
            Async/pure(Str/join(" ", [show_unit(made_all), show_bool(made), show_unit(written), text(back), show_unit(renamed), show_bool(moved), show_unit(removed), show_bool(gone), text(here)]))
        "#,
    );

    let (host, io) = MockHost::builder().cwd("/work").build();
    run_text(&source, host).expect("expected result");
    assert_eq!(io.output(), b"ok true ok hi ok true ok false /work");
    assert_eq!(io.file(b"x/w/z/f.txt"), None);
}

// The three status codes this campaign named, each told apart by `Show(Handle/Error)`: a file operation on a directory, a directory operation on a file, and a listing of nothing.
#[test]
fn the_named_filesystem_failures_show_by_name() {
    let source = program(
        r#"
            let a = fs/remove_file("data")!;
            let b = fs/remove_dir("data/a.txt")!;
            let c = fs/list("nope")!;
            let listed = match c | success(_) => "listed" | failure(e) => Show/show(e) end;
            Async/pure(Str/join(" ", [show_unit(a), show_unit(b), listed]))
        "#,
    );

    let (host, io) = seeded();
    run_text(&source, host).expect("expected result");
    assert_eq!(io.output(), b"is_directory not_directory not_found");
}

// The four pure path functions, with `/` as the one separator: a join never doubles it, a name is the last component, a parent of a root child is `/`, and an extension is what follows the last `.` of a name that does not start with it.
#[test]
fn the_path_functions_split_and_join_on_the_separator() {
    let source = program(
        r#"
            Async/pure(Str/join(" ", [
                fs/join("a/", "b"), fs/join("a", "b"), fs/join("", "b"),
                fs/name("a/b/c.txt"),
                Show/show(fs/parent("a/b/c")), Show/show(fs/parent("c")), Show/show(fs/parent("/c")),
                Show/show(fs/extension("archive.tar.gz")), Show/show(fs/extension(".bashrc")), Show/show(fs/extension("noext"))
            ]))
        "#,
    );

    let (host, io) = seeded();
    run_text(&source, host).expect("expected result");
    assert_eq!(
        io.output(),
        b"a/b a/b b c.txt some(a/b) none() some(/) some(gz) none() none()"
    );
}
