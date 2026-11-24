//! Where a tool or data is kept and how a run gets it, from task files of the test's own whose tools are fetched for the host the test runs on, each under a name of this process's own in `xtask/.artifacts`.

use {
    super::*,
    crate::*,
    curios_common::fill,
    curios_download::{sha256_hex, tree_sha256_hex},
    std::{
        net::TcpListener,
        os::unix,
        process::{self, Command},
    },
};

/// The tree whose root declares `tools` and whose file `a` holds one step running `run`.
fn tree_of(tools: &str, run: &str) -> Tree {
    Tree::parse(&[
        (
            "xtask/tasks.toml",
            &format!("description = \"The top\"\n{tools}"),
        ),
        (
            "xtask/tasks/a.toml",
            &format!("description = \"A\"\n[s]\ndescription = \"S\"\nrun = [{run}]\n"),
        ),
    ])
    .unwrap()
}

/// The tool `name` at version 1.0, fetched on this host from `url` at `digest`.
fn fetched(name: &str, url: &str, digest: &str) -> String {
    format!(
        "[tools.{name}]\nversion = \"1.0\"\nurl.{HOST_TRIPLE} = \"{url}\"\nsha256.{HOST_TRIPLE} = \"{digest}\"\n"
    )
}

/// A name no other test's process gives a tool.
fn unique(label: &str) -> String {
    format!("{label}-{}", process::id())
}

/// An address on loopback nothing answers at, for a run that must not download.
fn unanswered(file: &str) -> String {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let address = listener.local_addr().unwrap();
    drop(listener);
    format!("http://{address}/{file}")
}

fn tool(name: &str) -> Name {
    vec!["tools".to_string(), name.to_string()]
}

/// A tool is kept by its name and version: a download kept as one executable is that file, a tarball's executable sits at its path inside, a tool another ships is found in that one's directory, and a bumped version is kept beside the old copy.
#[test]
fn a_tool_is_kept_by_its_name_and_version() {
    let digest = "0".repeat(64);
    let tools = [
        fetched("bare", "https://example.org/bare-linux", &digest),
        fetched("packed", "https://example.org/packed.tar.gz", &digest) + "path = \"bin/packed\"\n",
        "[tools.shipped]\narchive = \".tools.packed\"\npath = \"bin/shipped\"\n".to_string(),
    ]
    .concat();
    let tree = tree_of(&tools, "[\"{.tools.bare}\", \"{.tools.shipped}\"]");

    assert_eq!(
        executable(&tree, &tool("bare")),
        artifacts().join("tools/bare-1.0")
    );
    assert_eq!(
        executable(&tree, &tool("packed")),
        artifacts().join("tools/packed-1.0/bin/packed")
    );
    assert_eq!(
        executable(&tree, &tool("shipped")),
        artifacts().join("tools/packed-1.0/bin/shipped")
    );
    let bumped = tree_of(
        &fetched("bare", "https://example.org/bare-linux", &digest).replace("1.0", "1.1"),
        "[\"{.tools.bare}\"]",
    );
    assert_eq!(
        executable(&bumped, &tool("bare")),
        artifacts().join("tools/bare-1.1")
    );

    let Instruction::Command(words) = &tree.steps[0].run[0] else {
        panic!("the step runs a command");
    };
    assert_eq!(
        Values::new(&tree, &Given::new()).command(words),
        Ok(Some(vec![
            artifacts()
                .join("tools/bare-1.0")
                .to_string_lossy()
                .into_owned(),
            artifacts()
                .join("tools/packed-1.0/bin/shipped")
                .to_string_lossy()
                .into_owned(),
        ]))
    );
}

/// A step finds first on its `PATH` the directory of each tool it names that is kept as a directory, so a launch inside it finds what ships beside it; a tool kept as one executable has nothing beside it to find.
#[test]
fn a_step_finds_the_directory_of_each_tool_kept_as_one_first() {
    let digest = "0".repeat(64);
    let tools = [
        fetched("engine", "https://example.org/engine.tar.gz", &digest) + "path = \"bin/engine\"\n",
        "[tools.manager]\narchive = \".tools.engine\"\npath = \"bin/manager\"\n".to_string(),
        fetched("lone", "https://example.org/lone", &digest),
    ]
    .concat();
    let tree = tree_of(
        &tools,
        "[\"{.tools.manager}\", \"install\"], [\"{.tools.lone}\"], [\"{.tools.engine}\"]",
    );

    assert_eq!(
        directories(&tree, 0),
        vec![tools_place().join("engine-1.0/bin")]
    );
}

/// A run on a host a tool has no download for runs nothing, naming the tool and the host.
#[test]
fn a_tool_without_a_download_for_the_host_refuses_the_run() {
    let tools = format!(
        "[tools.elsewhere]\nversion = \"1.0\"\nurl.no-such-host = \"https://example.org/elsewhere\"\nsha256.no-such-host = \"{}\"\n",
        "0".repeat(64)
    );
    let tree = tree_of(&tools, "[\"{.tools.elsewhere}\"]");

    let refused = fetch(&tree, &Values::new(&tree, &Given::new()), &[0]).unwrap_err();

    assert!(
        refused.contains(&format!(
            "`.tools.elsewhere` has no download for {HOST_TRIPLE}"
        )),
        "{refused}"
    );
    assert!(refused.ends_with("nothing run"), "{refused}");
}

/// A tool kept already is read as it stands, with nothing answering where it is published.
#[test]
fn a_tool_kept_already_is_not_fetched_again() {
    let name = unique("kept");
    let kept = tools_place().join(format!("{name}-1.0"));
    fs::create_dir_all(tools_place()).unwrap();
    fs::write(&kept, b"kept\n").unwrap();
    let tree = tree_of(
        &fetched(&name, &unanswered(&name), &"0".repeat(64)),
        &fill!("[\"{.tools.${name}}\"]", name),
    );

    let outcome = fetch(&tree, &Values::new(&tree, &Given::new()), &[0]);

    fs::remove_file(&kept).unwrap();
    assert_eq!(outcome, Ok(()));
}

/// A download placed by hand beside the kept tools is read before the network, checked, and kept as the executable a step runs.
#[test]
fn a_placed_download_is_checked_and_kept_as_an_executable() {
    let name = unique("placed");
    let file = format!("{name}-download");
    let placed = tools_place().join(&file);
    fs::create_dir_all(tools_place()).unwrap();
    fs::write(&placed, b"#!/bin/sh\n").unwrap();
    let tree = tree_of(
        &fetched(&name, &unanswered(&file), &sha256_hex(b"#!/bin/sh\n")),
        &fill!("[\"{.tools.${name}}\"]", name),
    );

    let outcome = fetch(&tree, &Values::new(&tree, &Given::new()), &[0]);

    let kept = executable(&tree, &tool(&name));
    let held = (fs::read(&kept).ok(), fs::metadata(&kept).ok());
    let _ = fs::remove_file(&placed);
    let _ = fs::remove_file(&kept);
    assert_eq!(outcome, Ok(()));
    let (bytes, metadata) = held;
    assert_eq!(bytes.as_deref(), Some(&b"#!/bin/sh\n"[..]));
    assert_eq!(
        unix::fs::PermissionsExt::mode(&metadata.unwrap().permissions()) & 0o777,
        0o755
    );
}

/// A tarball is unpacked into the tool's directory, its top directory taken as the contents, so its path holds whatever the archive's top is called.
#[test]
fn a_placed_tarball_is_unpacked_into_the_tool_s_directory() {
    let name = unique("packed");
    let file = format!("{name}.tar.gz");
    let placed = tools_place().join(&file);
    let staging = tools_place().join(format!("{name}-staging"));
    fs::create_dir_all(staging.join("packed-v1.0-linux/bin")).unwrap();
    fs::write(staging.join("packed-v1.0-linux/bin/packed"), b"binary\n").unwrap();
    let archived = Command::new("tar")
        .arg("-czf")
        .arg(&placed)
        .arg("-C")
        .arg(&staging)
        .arg("packed-v1.0-linux")
        .status()
        .unwrap();
    assert!(archived.success());
    let tree = tree_of(
        &(fetched(
            &name,
            &unanswered(&file),
            &sha256_hex(&fs::read(&placed).unwrap()),
        ) + "path = \"bin/packed\"\n"),
        &fill!("[\"{.tools.${name}}\"]", name),
    );

    let outcome = fetch(&tree, &Values::new(&tree, &Given::new()), &[0]);

    let kept = tools_place().join(format!("{name}-1.0"));
    let held = fs::read(executable(&tree, &tool(&name))).ok();
    let _ = fs::remove_file(&placed);
    let _ = fs::remove_dir_all(&staging);
    let _ = fs::remove_dir_all(&kept);
    assert_eq!(outcome, Ok(()));
    assert_eq!(held.as_deref(), Some(&b"binary\n"[..]));
}

/// A placed download whose digest is not the files' is refused, and nothing is kept for a step to run.
#[test]
fn a_placed_download_whose_digest_differs_is_refused() {
    let name = unique("differs");
    let file = format!("{name}-download");
    let placed = tools_place().join(&file);
    fs::create_dir_all(tools_place()).unwrap();
    fs::write(&placed, b"another\n").unwrap();
    let tree = tree_of(
        &fetched(&name, &unanswered(&file), &sha256_hex(b"pinned\n")),
        &fill!("[\"{.tools.${name}}\"]", name),
    );

    let outcome = fetch(&tree, &Values::new(&tree, &Given::new()), &[0]);

    let kept = executable(&tree, &tool(&name)).exists();
    let _ = fs::remove_file(&placed);
    let refused = outcome.unwrap_err();
    assert!(refused.contains("sha256 mismatch"), "{refused}");
    assert!(!kept, "a refused download was kept");
}

/// A placed archive whose top holds `cases/one.json` and `other/two.json` under one directory, as a forge generates one at a commit, and the digest of its `cases`.
fn archived(name: &str) -> (PathBuf, String) {
    fs::create_dir_all(data_place()).unwrap();
    let placed = data_place().join(format!("{name}.tar.gz"));
    let staging = data_place().join(format!("{name}-staging"));
    fs::create_dir_all(staging.join("repo-abc/cases")).unwrap();
    fs::create_dir_all(staging.join("repo-abc/other")).unwrap();
    fs::write(staging.join("repo-abc/cases/one.json"), b"[1]").unwrap();
    fs::write(staging.join("repo-abc/other/two.json"), b"[2]").unwrap();
    let archived = Command::new("tar")
        .arg("-czf")
        .arg(&placed)
        .arg("-C")
        .arg(&staging)
        .arg("repo-abc")
        .status()
        .unwrap();
    assert!(archived.success());
    let digest = tree_sha256_hex(&staging.join("repo-abc/cases")).unwrap();
    let _ = fs::remove_dir_all(&staging);

    (placed, digest)
}

/// The root declaring the data `name` at commit `abc`, its archive at `url`, its directory `cases` at `digest`.
fn data_of(name: &str, url: &str, digest: &str) -> String {
    format!(
        "[data.{name}]\nversion = \"abc\"\nurl = \"{url}\"\npath = \"cases\"\nsha256 = \"{digest}\"\n"
    )
}

fn data(name: &str) -> Name {
    vec!["data".to_string(), name.to_string()]
}

/// Data a scheduled step names is kept as its one directory, with the runner's own and apart from the tools, read here from a placed archive with nothing to answer the network.
#[test]
fn data_is_kept_as_its_one_directory_of_a_placed_archive() {
    let name = unique("cases");
    let (placed, digest) = archived(&name);
    let tree = tree_of(
        &data_of(&name, &unanswered(&format!("{name}.tar.gz")), &digest),
        &fill!("[\"true\", \"{.data.${name}}\"]", name),
    );

    let outcome = fetch(&tree, &Values::new(&tree, &Given::new()), &[0]);

    let kept = data_kept(&tree, &data(&name));
    let one = fs::read(kept.join("one.json")).ok();
    let other = kept.join("other").exists();
    let _ = fs::remove_file(&placed);
    let _ = fs::remove_dir_all(&kept);
    assert_eq!(outcome, Ok(()));
    assert_eq!(one.as_deref(), Some(&b"[1]"[..]));
    assert_eq!(kept, artifacts().join(format!("data/{name}-abc")));
    assert!(!other, "what no step reads of the archive was kept");
}

/// An archive whose directory's files are not the digest's is refused, and nothing is kept for a step to read.
#[test]
fn data_whose_files_differ_from_its_digest_is_refused() {
    let name = unique("differing");
    let (placed, _) = archived(&name);
    let tree = tree_of(
        &data_of(
            &name,
            &unanswered(&format!("{name}.tar.gz")),
            &sha256_hex(b"another tree"),
        ),
        &fill!("[\"true\", \"{.data.${name}}\"]", name),
    );

    let outcome = fetch(&tree, &Values::new(&tree, &Given::new()), &[0]);

    let kept = data_kept(&tree, &data(&name)).exists();
    let _ = fs::remove_file(&placed);
    let refused = outcome.unwrap_err();
    assert!(refused.contains("digest mismatch over cases"), "{refused}");
    assert!(!kept, "refused data was kept");
}

/// Data a crate's build reads is kept beside that crate, under its name and version, for a run none of whose steps names it: its files, published one by one, are each read here where a hand placed it.
#[test]
fn data_a_crate_s_build_reads_is_gathered_beside_the_crate_whatever_the_run_s_steps_name() {
    let name = unique("tables");
    let kept = artifacts().join(format!("{name}-7.0"));
    let placed = artifacts().join(format!("{name}-7.0.placed"));
    fs::create_dir_all(placed.join("deeper")).unwrap();
    fs::write(placed.join("one.txt"), b"one").unwrap();
    fs::write(placed.join("deeper/two.txt"), b"two").unwrap();
    let digest = tree_sha256_hex(&placed).unwrap();
    // This crate is the one beside which the test keeps the data: a crate at the repository's root, with a directory of kept things.
    let tree = tree_of(
        &fill!(
            "[data.${name}]\nversion = \"7.0\"\nurl = \"${url}\"\nfiles = [\"one.txt\", \"deeper/two.txt\"]\nsha256 = \"${digest}\"\nbeside = \"xtask\"\n",
            name,
            url = unanswered("published"),
            digest
        ),
        "[\"true\"]",
    );

    let outcome = fetch(&tree, &Values::new(&tree, &Given::new()), &[0]);

    let stated = data_kept(&tree, &data(&name));
    let two = fs::read(kept.join("deeper/two.txt")).ok();
    let _ = fs::remove_dir_all(&placed);
    let _ = fs::remove_dir_all(&kept);
    assert_eq!(outcome, Ok(()));
    assert_eq!(stated, kept);
    assert_eq!(two.as_deref(), Some(&b"two"[..]));
}

/// Data kept beside a directory that is no crate is refused before anything is fetched.
#[test]
fn data_beside_what_is_no_crate_is_refused() {
    let name = unique("stray");
    let tree = tree_of(
        &fill!(
            "[data.${name}]\nversion = \"7.0\"\nurl = \"${url}\"\nfiles = [\"one.txt\"]\nsha256 = \"${digest}\"\nbeside = \"no-such-crate\"\n",
            name,
            url = unanswered("published"),
            digest = "0".repeat(64)
        ),
        "[\"true\"]",
    );

    let refused = fetch(&tree, &Values::new(&tree, &Given::new()), &[0]).unwrap_err();

    assert!(
        refused.contains("is no crate at the repository's root"),
        "{refused}"
    );
    assert!(!root().join("no-such-crate").exists());
}
