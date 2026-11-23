//! Where a tool or a corpus a step names is kept, and how a run gets it before its first step.
//!
//! **A tool is kept by its name and version**, `xtask/.artifacts/biome-2.6.2`: a gzipped tarball unpacked into a directory, and any other download kept as the executable itself, a gzipped one decoded first, as [`Shape`] reads it off the URL. A kept copy is trusted by its presence, since [`curios_download`] renames one into place only once its digest has checked, so a run never reads part of one as the whole; and a bumped version is kept beside the old copy and never reads it. A download placed by hand under the name its URL ends in, beside the kept copies, is read before the network, as a build script reads its own.
//!
//! **A corpus is kept as its one directory**, `xtask/.artifacts/json-<commit>`: the archive a forge generates at its commit, placed by hand or downloaded, unpacked, the directory's files checked against the digest the corpus states, and the directory kept, as [`curios_download::Unpacked`] keeps one, the rest of the archive and the archive itself not. A kept corpus is trusted by its presence, as a kept tool is.
//!
//! **Naming a tool is the edge.** Every tool and corpus the scheduled steps name, `{.tools.npm}` or `{.corpora.json}`, is fetched before the first step, once for each download, and a run any of them cannot be had for runs nothing. A step runs with the directory holding each named tool's executable first on its `PATH`, where the tool is kept as a directory, so a launch nested inside it finds what ships beside it: npm's `#!/usr/bin/env node` finds the Node it came with.

use {
    crate::{Fetched, Fill, HOST_TRIPLE, Name, Shape, Tool, Tree, Values, artifacts, from_top},
    curios_download::{Download, Unpacked, gunzipped, keep_executable, unpack},
    std::{fs, path::PathBuf},
};

#[cfg(test)]
mod tests;

/// The tool whose download holds `name`'s executable, and the path inside it: the tool itself, or the one it names as its archive.
fn owner<'a>(tree: &'a Tree, name: &'a Name) -> (&'a Name, &'a Fetched, &'a str) {
    match &tree.tools[name] {
        Tool::Fetched(fetched) => (name, fetched, &fetched.path),
        Tool::Shipped { archive, path } => match &tree.tools[archive] {
            Tool::Fetched(fetched) => (archive, fetched, path),
            Tool::Shipped { .. } => {
                unreachable!("the reader holds an archive to a tool fetched itself")
            }
        },
    }
}

/// Where the fetched tool `name` is kept: under its name and version.
fn kept(name: &Name, fetched: &Fetched) -> PathBuf {
    let name = name.last().expect("a tool is named");

    artifacts().join(format!("{name}-{}", fetched.version))
}

/// How this host keeps the fetched tool, which a run has refused before its first step where it has no download.
fn shape(fetched: &Fetched) -> Option<Shape> {
    fetched
        .downloads
        .get(HOST_TRIPLE)
        .map(|published| published.shape)
}

/// The executable the tool `name` is on this host.
pub fn executable(tree: &Tree, name: &Name) -> PathBuf {
    let (owner, fetched, path) = owner(tree, name);
    let kept = kept(owner, fetched);

    match shape(fetched) {
        Some(Shape::Tarball) => kept.join(path),
        Some(Shape::Gzipped | Shape::Bare) | None => kept,
    }
}

/// Where the corpus `name` is kept: its one directory, under its name and commit.
pub fn corpus(tree: &Tree, name: &Name) -> PathBuf {
    let version = &tree.corpora[name].version;
    let name = name.last().expect("a corpus is named");

    artifacts().join(format!("{name}-{version}"))
}

/// What some steps name of what a run fetches, each once, in the order their words name it.
struct Named {
    tools: Vec<Name>,
    corpora: Vec<Name>,
}

/// The tools and the corpora the `steps` name.
fn named(tree: &Tree, steps: &[usize]) -> Named {
    let (mut tools, mut corpora) = (Vec::new(), Vec::new());
    for &step in steps {
        for fill in tree.steps[step]
            .run
            .iter()
            .flat_map(|instruction| instruction.words())
            .flat_map(|word| word.fills())
        {
            let (named, name) = match fill {
                Fill::Tool(name) => (&mut tools, name),
                Fill::Corpus(name) => (&mut corpora, name),
                _ => continue,
            };
            if !named.contains(name) {
                named.push(name.clone());
            }
        }
    }

    Named { tools, corpora }
}

/// The directories `step`'s commands find first on their `PATH`: each holding a named tool's executable where the tool is kept as a directory.
pub fn directories(tree: &Tree, step: usize) -> Vec<PathBuf> {
    let mut directories = Vec::new();
    for name in named(tree, &[step]).tools {
        let (_, fetched, _) = owner(tree, &name);
        if shape(fetched) != Some(Shape::Tarball) {
            continue;
        }
        let executable = executable(tree, &name);
        let directory = executable
            .parent()
            .expect("an executable stands in a directory")
            .to_path_buf();
        if !directories.contains(&directory) {
            directories.push(directory);
        }
    }

    directories
}

/// Keep every tool and corpus the `scheduled` steps name that is not kept yet, once for each download, refusing the run with every one this host cannot have.
pub fn fetch(tree: &Tree, values: &Values<'_>, scheduled: &[usize]) -> Result<(), String> {
    let Named { tools, corpora } = named(tree, scheduled);
    let mut owners = Vec::<Name>::new();
    for name in tools {
        let (owner, _, _) = owner(tree, &name);
        if !owners.contains(owner) {
            owners.push(owner.clone());
        }
    }

    let refused = owners
        .iter()
        .filter_map(|owner| obtained(tree, values, owner).err())
        .chain(
            corpora
                .iter()
                .filter_map(|name| unpacked(tree, values, name).err()),
        )
        .collect::<Vec<_>>();
    match refused.is_empty() {
        true => Ok(()),
        false => Err(format!("{}\nnothing run", refused.join("\n"))),
    }
}

/// Keep the corpus `name`, where it is not kept already.
fn unpacked(tree: &Tree, values: &Values<'_>, name: &Name) -> Result<(), String> {
    let kept = corpus(tree, name);
    if kept.exists() {
        return Ok(());
    }
    let read = &tree.corpora[name];
    let url = values.text(&read.url)?;
    let placed = artifacts().join(url.rsplit('/').next().unwrap_or(&url));
    if !placed.exists() {
        eprintln!("fetch {url}");
    }

    Unpacked {
        what: &format!("`{}` at {}", from_top(name), read.version),
        url: &url,
        directory: &read.path,
        digest: &read.sha256,
        placed: &placed,
        place: &kept,
    }
    .kept()
    .map_err(|unobtained| unobtained.to_string())
}

/// Keep the fetched tool `name` for this host, where it is not kept already.
fn obtained(tree: &Tree, values: &Values<'_>, name: &Name) -> Result<(), String> {
    let Tool::Fetched(fetched) = &tree.tools[name] else {
        unreachable!("a tool's owner is fetched itself");
    };
    let kept = kept(name, fetched);
    if kept.exists() {
        return Ok(());
    }
    let published = fetched.downloads.get(HOST_TRIPLE).ok_or_else(|| {
        format!(
            "`{}` has no download for {HOST_TRIPLE}, the host this run is on",
            from_top(name)
        )
    })?;

    let url = values.text(&published.url)?;
    let placed = artifacts().join(url.rsplit('/').next().unwrap_or(&url));
    if !placed.exists() {
        eprintln!("fetch {url}");
    }
    let bytes = Download {
        what: &format!("`{}` at {}", from_top(name), fetched.version),
        url: &url,
        sha256: &published.sha256,
        place: &placed,
    }
    .bytes()
    .map_err(|unobtained| unobtained.to_string())?;

    let failed = |error: std::io::Error| format!("keep {}: {error}", kept.display());
    fs::create_dir_all(artifacts()).map_err(failed)?;
    match published.shape {
        Shape::Tarball => unpack(&bytes, &kept),
        Shape::Gzipped => gunzipped(&bytes).and_then(|decoded| keep_executable(&decoded, &kept)),
        Shape::Bare => keep_executable(&bytes, &kept),
    }
    .map_err(failed)
}
