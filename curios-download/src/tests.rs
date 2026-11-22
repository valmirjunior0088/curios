//! What a pinned file is read from, what is kept, and what refuses a build: a copy a hand placed, a download served by a listener on loopback, and a digest either fails.

use {
    super::*,
    std::{
        any::Any,
        env,
        io::{BufRead, BufReader, Write as _},
        net::TcpListener,
        panic::{self, AssertUnwindSafe},
        path::PathBuf,
        process, thread,
    },
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

/// An address on loopback nothing answers at, for a build that must not download.
fn unanswered() -> String {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let address = listener.local_addr().unwrap();
    drop(listener);
    format!("http://{address}/pinned")
}

/// What a refusal said, from the payload a panic carries.
fn said(payload: Box<dyn Any + Send>) -> String {
    payload
        .downcast::<String>()
        .map(|said| *said)
        .unwrap_or_default()
}

// A copy placed by hand is what an offline build reads: no download is tried where it stands and its digest holds.
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
    .bytes();

    assert_eq!(read, CONTENT);
    let _ = fs::remove_dir_all(&directory);
}

// A copy whose digest is not the pin's refuses the build, naming the mismatch and where the right one goes.
#[test]
fn a_copy_whose_digest_differs_refuses_with_where_to_place_one() {
    let directory = directory("differs");
    let place = directory.join("pinned.txt");
    fs::write(&place, b"another file\n").unwrap();
    let url = unanswered();

    let refused = panic::catch_unwind(AssertUnwindSafe(|| {
        Download {
            what: "a test's file",
            url: &url,
            sha256: &sha256_hex(CONTENT),
            place: &place,
        }
        .bytes()
    }))
    .map_err(said)
    .expect_err("a differing copy refuses the build");

    assert!(refused.contains("sha256 mismatch"), "{refused}");
    assert!(refused.contains(&place.display().to_string()), "{refused}");
    let _ = fs::remove_dir_all(&directory);
}

// A download that checks is kept at the place, and the next build reads it there with nothing to answer it.
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
    .kept();
    assert_eq!(downloaded, CONTENT);
    assert_eq!(fs::read(&place).unwrap(), CONTENT);

    let url = unanswered();
    let read = Download {
        what: "a test's file",
        url: &url,
        sha256: &pinned,
        place: &place,
    }
    .kept();
    assert_eq!(read, CONTENT);
    let _ = fs::remove_dir_all(&directory);
}

// A download whose digest is not the pin's refuses the build and is kept nowhere, so the next build downloads again rather than trusting it.
#[test]
fn a_download_whose_digest_differs_is_refused_and_not_kept() {
    let directory = directory("refused");
    let place = directory.join("pinned.txt");
    let url = served(b"another file\n");

    let refused = panic::catch_unwind(AssertUnwindSafe(|| {
        Download {
            what: "a test's file",
            url: &url,
            sha256: &sha256_hex(CONTENT),
            place: &place,
        }
        .kept()
    }))
    .map_err(said)
    .expect_err("a differing download refuses the build");

    assert!(refused.contains("sha256 mismatch"), "{refused}");
    assert!(!place.exists(), "a refused download was kept");
    let _ = fs::remove_dir_all(&directory);
}
