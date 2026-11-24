//! Where a tool or data a step names is kept, and how a run gets it before its first step.
//!
//! **A tool is kept by its name and version**, `xtask/.artifacts/tools/biome-2.6.2`: a gzipped tarball unpacked into a directory, and any other download kept as the executable itself, a gzipped one decoded first, as [`Shape`] reads it off the URL. A kept copy is trusted by its presence, since [`curios_download`] renames one into place only once its digest has checked, so a run never reads part of one as the whole; and a bumped version is kept beside the old copy and never reads it. A download placed by hand under the name its URL ends in, beside the kept copies, is read before the network, as a build script reads its own.
//!
//! **Data is kept as one directory, beside what reads it.** What a step's command reads is kept with the runner's own, `xtask/.artifacts/data/json-<commit>`, beside the tools and apart from them, so a tool and data of one name are two places; what a crate's build reads is kept beside the crate its table names, `curios-unicode/.artifacts/unicode-<version>`, where the build finds it through its own manifest as it finds everything it keeps, and no crate learns where the runner keeps its own. Either is one directory of the archive a forge generates at a commit, placed by hand or downloaded and unpacked, or the files a site publishes one by one, each placed by hand or downloaded; the directory's files checked against the digest the data states, and the directory kept, as [`curios_download::Unpacked`] and [`curios_download::Gathered`] keep one, an archive and the rest of it not. Kept data is trusted by its presence, as a kept tool is.
//!
//! **Naming a tool is the edge.** Every tool and all the data the scheduled steps name, `{.tools.npm}` or `{.data.json}`, is fetched before the first step; data a crate's build reads is fetched before any run's, since no step names it and which steps build the crate is Cargo's to know, once for each download, and a run any of them cannot be had for runs nothing. A step runs with the directory holding each named tool's executable first on its `PATH`, where the tool is kept as a directory, so a launch nested inside it finds what ships beside it: npm's `#!/usr/bin/env node` finds the Node it came with.

use {
    crate::{
        Fetched, Fill, Form, HOST_TRIPLE, Name, Shape, Tool, Tree, Values, artifacts, from_top,
        root,
    },
    curios_download::{Download, Gathered, Unpacked, gunzipped, keep_executable, unpack},
    std::{
        fs,
        path::{Path, PathBuf},
    },
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

/// Where the tools are kept, and a download of one a hand placed is read.
fn tools_place() -> PathBuf {
    artifacts().join("tools")
}

/// Where data a step's command reads is kept, and an archive of any data a hand placed is read: beside the tools and apart from them, so a tool and data of one name are two places.
fn data_place() -> PathBuf {
    artifacts().join("data")
}

/// Where the fetched tool `name` is kept: under its name and version.
fn kept(name: &Name, fetched: &Fetched) -> PathBuf {
    let name = name.last().expect("a tool is named");

    tools_place().join(format!("{name}-{}", fetched.version))
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

/// Where the data `name` is kept: its one directory, under its name and version, beside the crate whose build reads it where its table names one, and with the runner's own otherwise.
pub fn data_kept(tree: &Tree, name: &Name) -> PathBuf {
    let data = &tree.data[name];
    let name = name.last().expect("data is named");
    let kept = format!("{name}-{}", data.version);

    match &data.beside {
        Some(reader) => root().join(reader).join(".artifacts").join(kept),
        None => data_place().join(kept),
    }
}

/// What some steps name of what a run fetches, each once, in the order their words name it.
struct Named {
    tools: Vec<Name>,
    data: Vec<Name>,
}

/// The tools and the data the `steps` name.
fn named(tree: &Tree, steps: &[usize]) -> Named {
    let (mut tools, mut data) = (Vec::new(), Vec::new());
    for &step in steps {
        for fill in tree.steps[step]
            .run
            .iter()
            .flat_map(|instruction| instruction.words())
            .flat_map(|word| word.fills())
        {
            let (named, name) = match fill {
                Fill::Tool(name) => (&mut tools, name),
                Fill::Data(name) => (&mut data, name),
                _ => continue,
            };
            if !named.contains(name) {
                named.push(name.clone());
            }
        }
    }

    Named { tools, data }
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

/// Keep every tool and all the data the `scheduled` steps name, and all the data a crate's build reads, whatever the steps: each that is not kept yet, once for each download, refusing the run with every one this host cannot have.
pub fn fetch(tree: &Tree, values: &Values<'_>, scheduled: &[usize]) -> Result<(), String> {
    let Named { tools, mut data } = named(tree, scheduled);
    // What a crate's build reads no step names: which steps build the crate is Cargo's to know, so the data is kept before any of them runs.
    for (name, read) in &tree.data {
        if read.beside.is_some() && !data.contains(name) {
            data.push(name.clone());
        }
    }
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
            data.iter()
                .filter_map(|name| gathered(tree, values, name).err()),
        )
        .collect::<Vec<_>>();
    match refused.is_empty() {
        true => Ok(()),
        false => Err(format!("{}\nnothing run", refused.join("\n"))),
    }
}

/// Where a download from `url` a hand placed stands, under `kept`: the name the address ends in.
fn placed(kept: &Path, url: &str) -> PathBuf {
    kept.join(url.rsplit('/').next().unwrap_or(url))
}

/// Keep the data `name`, where it is not kept already: one directory of its archive, or its files gathered.
fn gathered(tree: &Tree, values: &Values<'_>, name: &Name) -> Result<(), String> {
    let kept = data_kept(tree, name);
    if kept.exists() {
        return Ok(());
    }
    let read = &tree.data[name];
    if let Some(reader) = &read.beside
        && !root().join(reader).join("Cargo.toml").is_file()
    {
        return Err(format!(
            "`{}` is kept beside `{reader}`, which is no crate at the repository's root",
            from_top(name)
        ));
    }
    let url = values.text(&read.url)?;
    let what = format!("`{}` at {}", from_top(name), read.version);

    match &read.form {
        Form::Directory(directory) => {
            let placed = placed(&data_place(), &url);
            if !placed.exists() {
                eprintln!("fetch {url}");
            }

            Unpacked {
                what: &what,
                url: &url,
                directory,
                digest: &read.sha256,
                placed: &placed,
                place: &kept,
            }
            .kept()
        }
        Form::Files(files) => {
            // Files placed by hand stand beside where the directory is kept, each at its path, under the directory's name with `.placed` after it.
            let mut placed = kept.clone().into_os_string();
            placed.push(".placed");
            let placed = PathBuf::from(placed);
            for file in files {
                if !placed.join(file).exists() {
                    eprintln!("fetch {url}/{file}");
                }
            }

            Gathered {
                what: &what,
                url: &url,
                files,
                digest: &read.sha256,
                placed: &placed,
                place: &kept,
            }
            .kept()
        }
    }
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
    let placed = placed(&tools_place(), &url);
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
    fs::create_dir_all(tools_place()).map_err(failed)?;
    match published.shape {
        Shape::Tarball => unpack(&bytes, &kept),
        Shape::Gzipped => gunzipped(&bytes).and_then(|decoded| keep_executable(&decoded, &kept)),
        Shape::Bare => keep_executable(&bytes, &kept),
    }
    .map_err(failed)
}
