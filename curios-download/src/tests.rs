//! What a pinned file is read from, what is kept, and what refuses it: a copy a hand placed, a download served by a listener on loopback, a digest either fails, what a download holds kept unpacked or as an executable, and a directory of an archive pinned by the digest of its files.

use {
    super::*,
    flate2::{Compression, write::GzEncoder},
    std::{
        collections::BTreeSet,
        env,
        io::{BufRead, BufReader, Write as _},
        net::TcpListener,
        thread,
    },
    tar::{Builder, Header},
};

const CONTENT: &[u8] = b"a pinned file\n";

/// A directory of this test's own under the system's temporary one, empty: nextest runs each test in a process of its own, so the process's id and the test's name keep two apart.
fn directory(name: &str) -> PathBuf {
    let directory = env::temp_dir().join(format!("curios-download-{}-{name}", process::id()));
    let _ = fs::remove_dir_all(&directory);
    fs::create_dir_all(&directory).unwrap();
    directory
}

/// An address on loopback that answers one request with `body`, and nothing after it.
fn served(body: &'static [u8]) -> String {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let address = listener.local_addr().unwrap();
    thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut request = BufReader::new(&stream);
        let mut line = String::new();
        while request.read_line(&mut line).unwrap() > 2 {
            line.clear();
        }
        let mut stream = &stream;
        write!(
            stream,
            "HTTP/1.1 200 OK\r\ncontent-length: {}\r\nconnection: close\r\n\r\n",
            body.len()
        )
        .unwrap();
        stream.write_all(body).unwrap();
    });
    format!("http://{address}/pinned")
}

/// An address on loopback nothing answers at, for a run that must not download.
fn unanswered() -> String {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let address = listener.local_addr().unwrap();
    drop(listener);
    format!("http://{address}/pinned")
}

/// A gzipped tarball of `entries`, each a path, its bytes and its mode.
fn tarball(entries: &[(&str, &[u8], u32)]) -> Vec<u8> {
    let mut builder = Builder::new(GzEncoder::new(Vec::new(), Compression::fast()));
    for (path, bytes, mode) in entries {
        let mut header = Header::new_gnu();
        header.set_size(bytes.len() as u64);
        header.set_mode(*mode);
        header.set_cksum();
        builder.append_data(&mut header, path, *bytes).unwrap();
    }

    builder.into_inner().unwrap().finish().unwrap()
}

fn mode(path: &Path) -> u32 {
    unix::fs::PermissionsExt::mode(&fs::metadata(path).unwrap().permissions()) & 0o777
}

// A copy placed by hand is what an offline run reads: no download is tried where it stands and its digest holds.
#[test]
fn a_copy_at_the_place_is_read_without_a_download() {
    let directory = directory("placed");
    let place = directory.join("pinned.txt");
    fs::write(&place, CONTENT).unwrap();
    let url = unanswered();

    let read = Download {
        what: "a test's file",
        url: &url,
        sha256: &sha256_hex(CONTENT),
        place: &place,
    }
    .bytes()
    .unwrap();

    assert_eq!(read, CONTENT);
    let _ = fs::remove_dir_all(&directory);
}

// A copy whose digest is not the pin's is refused, naming the mismatch and where the right one goes.
#[test]
fn a_copy_whose_digest_differs_refuses_with_where_to_place_one() {
    let directory = directory("differs");
    let place = directory.join("pinned.txt");
    fs::write(&place, b"another file\n").unwrap();
    let url = unanswered();

    let refused = Download {
        what: "a test's file",
        url: &url,
        sha256: &sha256_hex(CONTENT),
        place: &place,
    }
    .bytes()
    .expect_err("a differing copy is refused")
    .to_string();

    assert!(refused.contains("sha256 mismatch"), "{refused}");
    assert!(refused.contains(&place.display().to_string()), "{refused}");
    let _ = fs::remove_dir_all(&directory);
}

// A download that checks is kept at the place, and the next run reads it there with nothing to answer it.
#[test]
fn a_download_that_checks_is_kept_for_the_next_build() {
    let directory = directory("kept");
    let place = directory.join("nested").join("pinned.txt");
    let pinned = sha256_hex(CONTENT);

    let url = served(CONTENT);
    let downloaded = Download {
        what: "a test's file",
        url: &url,
        sha256: &pinned,
        place: &place,
    }
    .kept()
    .unwrap();
    assert_eq!(downloaded, CONTENT);
    assert_eq!(fs::read(&place).unwrap(), CONTENT);

    let url = unanswered();
    let read = Download {
        what: "a test's file",
        url: &url,
        sha256: &pinned,
        place: &place,
    }
    .kept()
    .unwrap();
    assert_eq!(read, CONTENT);
    let _ = fs::remove_dir_all(&directory);
}

// A download whose digest is not the pin's is refused and kept nowhere, so the next run downloads again rather than trusting it.
#[test]
fn a_download_whose_digest_differs_is_refused_and_not_kept() {
    let directory = directory("refused");
    let place = directory.join("pinned.txt");
    let url = served(b"another file\n");

    let refused = Download {
        what: "a test's file",
        url: &url,
        sha256: &sha256_hex(CONTENT),
        place: &place,
    }
    .kept()
    .expect_err("a differing download is refused")
    .to_string();

    assert!(refused.contains("sha256 mismatch"), "{refused}");
    assert!(!place.exists(), "a refused download was kept");
    let _ = fs::remove_dir_all(&directory);
}

// A release's tarball holds one top directory named for its version; unpacked, its contents stand in the directory asked for, each entry with the permissions it was archived with.
#[test]
fn a_tarball_s_sole_top_directory_is_unpacked_as_the_contents() {
    let directory = directory("sole");
    let into = directory.join("tool-1.0");
    let archive = tarball(&[
        ("tool-v1.0-linux/bin/tool", b"#!/bin/sh\n", 0o755),
        ("tool-v1.0-linux/README", b"read me\n", 0o644),
    ]);

    unpack(&archive, &into).unwrap();

    assert_eq!(fs::read(into.join("bin/tool")).unwrap(), b"#!/bin/sh\n");
    assert_eq!(mode(&into.join("bin/tool")), 0o755);
    assert_eq!(mode(&into.join("README")), 0o644);
    assert_eq!(
        fs::read_dir(&directory).unwrap().count(),
        1,
        "nothing is left beside it"
    );
    let _ = fs::remove_dir_all(&directory);
}

// An archive whose entries stand side by side at its top is kept as it is.
#[test]
fn a_tarball_of_several_tops_is_unpacked_as_it_stands() {
    let directory = directory("several");
    let into = directory.join("tool-1.0");
    let archive = tarball(&[("tool", b"binary\n", 0o755), ("LICENSE", b"terms\n", 0o644)]);

    unpack(&archive, &into).unwrap();

    assert_eq!(fs::read(into.join("tool")).unwrap(), b"binary\n");
    assert_eq!(fs::read(into.join("LICENSE")).unwrap(), b"terms\n");
    let _ = fs::remove_dir_all(&directory);
}

// An archive that breaks midway leaves nothing where it was to be kept, so no later run reads a part of it as the whole.
#[test]
fn a_tarball_that_breaks_midway_leaves_nothing_at_its_place() {
    let directory = directory("broken");
    let into = directory.join("tool-1.0");
    let archive = tarball(&[("tool-v1.0/bin/tool", &[0; 4096], 0o755)]);

    unpack(&archive[..archive.len() / 2], &into).expect_err("a cut archive is refused");

    assert!(!into.exists(), "a part was kept as the whole");
    assert_eq!(
        fs::read_dir(&directory).unwrap().count(),
        0,
        "a part was left beside it"
    );
    let _ = fs::remove_dir_all(&directory);
}

// Two runs keeping one tool at once: the one that finds the place taken keeps the copy that stands.
#[test]
fn a_place_another_run_kept_first_stands() {
    let directory = directory("raced");
    let into = directory.join("tool-1.0");
    fs::create_dir_all(&into).unwrap();
    fs::write(into.join("tool"), b"kept first\n").unwrap();

    unpack(&tarball(&[("tool", b"kept second\n", 0o755)]), &into).unwrap();

    assert_eq!(fs::read(into.join("tool")).unwrap(), b"kept first\n");
    assert_eq!(fs::read_dir(&directory).unwrap().count(), 1);
    let _ = fs::remove_dir_all(&directory);
}

// A bare download is kept as the executable a step launches.
#[test]
fn a_kept_executable_is_marked_so() {
    let directory = directory("executable");
    let at = directory.join("tool-1.0");

    keep_executable(b"binary\n", &at).unwrap();

    assert_eq!(fs::read(&at).unwrap(), b"binary\n");
    assert_eq!(mode(&at), 0o755);
    let _ = fs::remove_dir_all(&directory);
}

#[test]
fn a_gzipped_file_decodes_to_what_was_compressed() {
    let mut encoder = GzEncoder::new(Vec::new(), Compression::fast());
    encoder.write_all(CONTENT).unwrap();

    assert_eq!(gunzipped(&encoder.finish().unwrap()).unwrap(), CONTENT);
}

/// Write each file of `files`, a path and its text, under `root`.
fn files(root: &Path, files: &[(&str, &str)]) {
    for (path, text) in files {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, text).unwrap();
    }
}

// The digest of a directory is what coreutils print over its files, sorted by their paths' bytes, `B.txt` before `a.txt`: `find . -type f -printf '%P\0' | LC_ALL=C sort -z | xargs -0 sha256sum | sha256sum` printed this over these four.
#[test]
fn a_directory_s_digest_is_what_sha256sum_prints_over_its_files_sorted() {
    let directory = directory("digest");
    files(
        &directory,
        &[
            ("a.txt", "one\n"),
            ("nested/b.txt", "two\n"),
            ("nested/deeper/c", "three\n"),
            ("B.txt", "zero\n"),
        ],
    );

    assert_eq!(
        tree_sha256_hex(&directory).unwrap(),
        "dcb61b3fb37e05cd458ac9a2e66e7cbdba5610ca1771e336792c76f2fdf31213"
    );
    let _ = fs::remove_dir_all(&directory);
}

/// An archive a forge generates, its sole top directory named for the commit, holding the cases a corpus keeps and what it does not.
fn generated() -> Vec<u8> {
    tarball(&[
        ("repo-abc/cases/one.json", b"[1]", 0o644),
        ("repo-abc/cases/deeper/two.json", b"[2]", 0o644),
        ("repo-abc/parsers/huge.bin", b"not kept", 0o644),
    ])
}

/// The digest of the cases `generated` holds, taken over the same files written out.
fn cases_digest(directory: &Path) -> String {
    let written = directory.join("written");
    files(&written, &[("one.json", "[1]"), ("deeper/two.json", "[2]")]);
    let digest = tree_sha256_hex(&written).unwrap();
    let _ = fs::remove_dir_all(&written);
    digest
}

// An archive placed by hand is read without a download, and of it only the directory is kept, once its files check.
#[test]
fn an_archive_s_directory_whose_files_check_is_kept_alone() {
    let directory = directory("unpacked");
    let placed = directory.join("abc.tar.gz");
    fs::write(&placed, generated()).unwrap();
    let place = directory.join("kept");
    let url = unanswered();

    Unpacked {
        what: "a test's cases",
        url: &url,
        directory: "cases",
        digest: &cases_digest(&directory),
        placed: &placed,
        place: &place,
    }
    .kept()
    .unwrap();

    assert_eq!(fs::read(place.join("deeper/two.json")).unwrap(), b"[2]");
    assert!(!place.join("parsers").exists());
    let leftovers = fs::read_dir(&directory)
        .unwrap()
        .map(|entry| entry.unwrap().file_name().to_string_lossy().into_owned())
        .collect::<BTreeSet<_>>();
    assert_eq!(
        leftovers,
        BTreeSet::from(["abc.tar.gz".to_string(), "kept".to_string()])
    );
    let _ = fs::remove_dir_all(&directory);
}

// A directory whose files are not the digest's is refused, naming the mismatch and where the archive goes, and nothing is kept.
#[test]
fn an_archive_s_directory_whose_files_differ_is_refused_and_not_kept() {
    let directory = directory("unpacked-differs");
    let placed = directory.join("abc.tar.gz");
    fs::write(&placed, generated()).unwrap();
    let place = directory.join("kept");
    let url = unanswered();

    let refused = Unpacked {
        what: "a test's cases",
        url: &url,
        directory: "cases",
        digest: &sha256_hex(b"another tree"),
        placed: &placed,
        place: &place,
    }
    .kept()
    .expect_err("a directory that does not check is refused")
    .to_string();

    assert!(refused.contains("digest mismatch over cases"), "{refused}");
    assert!(refused.contains(&placed.display().to_string()), "{refused}");
    assert!(!place.exists());
    assert_eq!(fs::read_dir(&directory).unwrap().count(), 1);
    let _ = fs::remove_dir_all(&directory);
}

// Files published one by one are gathered into one directory, a file a hand placed read where it stands and another downloaded, and kept once they check together.
#[test]
fn files_published_one_by_one_are_gathered_placed_or_downloaded_and_kept_once_they_check() {
    let directory = directory("gathered");
    let placed = directory.join("placed");
    files(&placed, &[("deeper/two.txt", "two")]);
    let written = directory.join("written");
    files(&written, &[("one.txt", "one"), ("deeper/two.txt", "two")]);
    let digest = tree_sha256_hex(&written).unwrap();
    fs::remove_dir_all(&written).unwrap();
    let place = directory.join("kept");
    // The listener answers whatever path it is asked for, once: the file no hand placed.
    let url = served(b"one");

    Gathered {
        what: "a test's files",
        url: &url,
        files: &["one.txt".to_string(), "deeper/two.txt".to_string()],
        digest: &digest,
        placed: &placed,
        place: &place,
    }
    .kept()
    .unwrap();

    assert_eq!(fs::read(place.join("one.txt")).unwrap(), b"one");
    assert_eq!(fs::read(place.join("deeper/two.txt")).unwrap(), b"two");
    let leftovers = fs::read_dir(&directory)
        .unwrap()
        .map(|entry| entry.unwrap().file_name().to_string_lossy().into_owned())
        .collect::<BTreeSet<_>>();
    assert_eq!(
        leftovers,
        BTreeSet::from(["kept".to_string(), "placed".to_string()])
    );
    let _ = fs::remove_dir_all(&directory);
}

// Files that are not the digest's together are refused, naming the mismatch, each file's address and where a hand places them, and nothing is kept.
#[test]
fn gathered_files_that_differ_are_refused_and_not_kept() {
    let directory = directory("gathered-differs");
    let placed = directory.join("placed");
    files(
        &placed,
        &[("one.txt", "one"), ("deeper/two.txt", "another")],
    );
    let place = directory.join("kept");
    let url = unanswered();

    let refused = Gathered {
        what: "a test's files",
        url: &url,
        files: &["one.txt".to_string(), "deeper/two.txt".to_string()],
        digest: &sha256_hex(b"another tree"),
        placed: &placed,
        place: &place,
    }
    .kept()
    .expect_err("files that do not check are refused")
    .to_string();

    assert!(
        refused.contains("digest mismatch over the files"),
        "{refused}"
    );
    assert!(
        refused.contains(&format!("{url}/deeper/two.txt")),
        "{refused}"
    );
    assert!(refused.contains(&placed.display().to_string()), "{refused}");
    assert!(!place.exists());
    assert_eq!(fs::read_dir(&directory).unwrap().count(), 1);
    let _ = fs::remove_dir_all(&directory);
}

// An archive that names no directory is kept whole, its top directory taken as the contents, once all its files check.
#[test]
fn an_archive_kept_whole_holds_all_its_files_once_they_check() {
    let directory = directory("whole");
    let placed = directory.join("abc.tar.gz");
    fs::write(&placed, generated()).unwrap();
    let written = directory.join("written");
    files(
        &written,
        &[
            ("cases/one.json", "[1]"),
            ("cases/deeper/two.json", "[2]"),
            ("parsers/huge.bin", "not kept"),
        ],
    );
    let digest = tree_sha256_hex(&written).unwrap();
    fs::remove_dir_all(&written).unwrap();
    let place = directory.join("kept");
    let url = unanswered();

    Unpacked {
        what: "a test's source",
        url: &url,
        directory: "",
        digest: &digest,
        placed: &placed,
        place: &place,
    }
    .kept()
    .unwrap();

    assert_eq!(
        fs::read(place.join("parsers/huge.bin")).unwrap(),
        b"not kept"
    );
    assert_eq!(
        fs::read(place.join("cases/deeper/two.json")).unwrap(),
        b"[2]"
    );
    let _ = fs::remove_dir_all(&directory);
}
