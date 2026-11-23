//! What a pinned file is read from, what is kept, and what refuses it: a copy a hand placed, a download served by a listener on loopback, a digest either fails, and what a download holds kept unpacked or as an executable.

use {
    super::*,
    flate2::{Compression, write::GzEncoder},
    std::{
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
