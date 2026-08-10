use {
    super::*,
    std::{
        fs,
        time::{SystemTime, UNIX_EPOCH},
    },
};

/// A tree of `(relative path, contents)` pairs, rooted at a fresh directory nothing else is using.
fn tree(name: &str, files: &[(&str, &str)]) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();
    let root = std::env::temp_dir().join(format!("curios-{name}-{}-{millis}", std::process::id()));

    for (path, contents) in files {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, contents).unwrap();
    }

    fs::create_dir_all(&root).unwrap();

    root
}

/// A well-formed `c1:` digest, round-tripping through the spelling it was written as.
#[test]
fn a_c1_hash_parses() {
    let spelling = format!("c1:{}", "0123456789abcdef".repeat(4));
    let hash = TreeHash::parse(&spelling).expect("a well-formed c1 hash");

    assert_eq!(hash.to_string(), spelling);
}

#[test]
fn an_unknown_scheme_is_refused() {
    let refusal = TreeHash::parse(&format!("c2:{}", "a".repeat(64)))
        .expect_err("a scheme this compiler does not know");

    assert!(refusal.contains("names no hash scheme"), "{refusal}");
}

/// A bare digest with no scheme is the same refusal: the prefix is what lets two schemes verify during a transition, so it is never optional.
#[test]
fn a_scheme_less_digest_is_refused() {
    let refusal = TreeHash::parse(&"a".repeat(64)).expect_err("a digest with no scheme");

    assert!(refusal.contains("names no hash scheme"), "{refusal}");
}

#[test]
fn a_short_digest_is_refused() {
    let refusal = TreeHash::parse("c1:abc").expect_err("a digest of the wrong length");

    assert!(refusal.contains("64 lowercase hex digits"), "{refusal}");
}

/// Uppercase is refused rather than folded, because the hash is a store key and two spellings of one digest would be two directories.
#[test]
fn an_uppercase_digest_is_refused() {
    let refusal =
        TreeHash::parse(&format!("c1:{}", "A".repeat(64))).expect_err("an uppercase digest");

    assert!(refusal.contains("64 lowercase hex digits"), "{refusal}");
}

/// **The hash.** A delivered tree verifies under `c1:`, and the same contents hash the same wherever they sit.
#[test]
fn one_tree_hashes_one_way() {
    let files = &[
        ("curios.toml", "name = \"json\"\n"),
        ("lib.crs", "pub mod parse;"),
        ("parse/lexer.crs", "pub let token : Type = Type;"),
    ];

    let left = tree("hash-left", files);
    let right = tree("hash-right", files);

    let hash = TreeHash::of(&left).expect("a tree of regular files");

    assert_eq!(hash, TreeHash::of(&right).expect("the same tree elsewhere"));
    assert_eq!(TreeHash::parse(&hash.to_string()), Ok(hash));

    fs::remove_dir_all(left).unwrap();
    fs::remove_dir_all(right).unwrap();
}

/// A tampered byte is a different tree, which is the whole point of accepting by hash.
#[test]
fn a_tampered_byte_is_a_different_hash() {
    let before = tree("hash-before", &[("lib.crs", "pub let x : Type = Type;")]);
    let after = tree("hash-after", &[("lib.crs", "pub let x : Type = Typo;")]);

    assert_ne!(
        TreeHash::of(&before).unwrap(),
        TreeHash::of(&after).unwrap()
    );

    fs::remove_dir_all(before).unwrap();
    fs::remove_dir_all(after).unwrap();
}

/// Both halves are length-framed, so moving a boundary between a path and its contents is a different tree.
///
/// Unframed, `ab` holding `c` and `a` holding `bc` feed the digest the identical byte sequence — two trees, one store key.
#[test]
fn a_moved_boundary_is_a_different_hash() {
    let left = tree("hash-frame-left", &[("ab", "c")]);
    let right = tree("hash-frame-right", &[("a", "bc")]);

    assert_ne!(TreeHash::of(&left).unwrap(), TreeHash::of(&right).unwrap());

    fs::remove_dir_all(left).unwrap();
    fs::remove_dir_all(right).unwrap();
}

/// A tree is its files: an empty directory leaves no trace, because nothing about it could be delivered.
#[test]
fn an_empty_directory_leaves_no_trace() {
    let bare = tree("hash-bare", &[("lib.crs", "")]);
    let padded = tree("hash-padded", &[("lib.crs", "")]);
    fs::create_dir_all(padded.join("empty/deeper")).unwrap();

    assert_eq!(TreeHash::of(&bare).unwrap(), TreeHash::of(&padded).unwrap());

    fs::remove_dir_all(bare).unwrap();
    fs::remove_dir_all(padded).unwrap();
}

/// A symlink in a delivered tree is refused: followed it reaches outside the tree, recorded it hashes a path whose meaning depends on where it is unpacked.
#[test]
#[cfg(unix)]
fn a_symlink_in_a_delivered_tree_is_refused() {
    let root = tree("hash-symlink", &[("lib.crs", "")]);
    std::os::unix::fs::symlink("lib.crs", root.join("alias.crs")).unwrap();

    let refusal = TreeHash::of(&root)
        .map(|_| ())
        .expect_err("a delivered tree may hold no symlink");
    assert!(refusal.contains("is a symlink"), "{refusal}");

    fs::remove_dir_all(root).unwrap();
}
