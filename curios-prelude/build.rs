//! The fixed prelude's build, as Cargo runs it: the work is `curios-prelude-build`'s `compile`, filed where the library that embeds the images reads them, and done once a state of the sources the build links, a run under any other configuration of the same state copying them from the images the checkout keeps (`curios_prelude_build::Images`).
//!
//! The compiled crate is the verdict: the build panics on the first refusal of any stage, the certifier's among them, and images are filed only once the work is done, so the crate compiles only if the certifier accepted the whole prelude, and nothing can reach the prelude except through a crate that compiled.

use {
    curios_package::MANIFEST,
    curios_prelude_build::{Images, Settled, compile, key, std_directory},
    std::{
        env,
        path::{Path, PathBuf},
        process::Command,
    },
};

/// What a run is asked to do with the images the checkout keeps, `reuse` or `compare`, which the step holding them asks of two cargo modes; unset, a run takes the kept images where they are its state's and makes them where they are not.
///
/// One variable with two answers, so no run is asked both. Declared to Cargo, so a step that sets it runs this script whatever ran before it; a unit that never sets it is never run again for it.
const ASKED: &str = "CURIOS_PRELUDE_IMAGES";

/// What a run is asked to do with the images the checkout keeps: what [`ASKED`] says.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Asked {
    /// Unset: the kept images where they are this state's, made and filed where they are not.
    Either,
    /// `reuse`: the kept images, and a refusal where making them is the run's only way to have them.
    Reuse,
    /// `compare`: the images made again, and a refusal where the kept images are others for this state.
    Compare,
}

impl Asked {
    /// What [`ASKED`] says, refusing a word it does not take.
    fn read() -> Self {
        match env::var(ASKED).as_deref() {
            Err(_) => Self::Either,
            Ok("reuse") => Self::Reuse,
            Ok("compare") => Self::Compare,
            Ok(other) => panic!("{ASKED} is `{other}`, where it is `reuse`, `compare` or unset"),
        }
    }
}

fn main() {
    // `curios-prelude-build`, the work, is a build-dependency, so an edit of it runs this script again as an edit of any crate it links does.
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-env-changed={ASKED}");

    // Filed under `OUT_DIR`, where Cargo keeps a build script's outputs, because the library that includes them is their one reader: nothing outside the build opens an image.
    let out = PathBuf::from(
        env::var_os("OUT_DIR").expect("Cargo runs a build script with `OUT_DIR` set"),
    );
    let prelude = PathBuf::from(
        env::var_os("CARGO_MANIFEST_DIR")
            .expect("Cargo runs a build script with `CARGO_MANIFEST_DIR` set"),
    );
    let workspace = prelude
        .parent()
        .expect("the prelude's crate sits at the workspace's root");
    let images = prelude.join(".artifacts").join("images");
    // Held until the images are in `out` and filed: a unit of another profile may run beside this one, and it waits for these images rather than making its own.
    let kept = Images::open(images.clone());
    let rustc = rustc_identity();
    let state = key(workspace, &prelude, &rustc);

    // Every configuration takes the kept images where they are its state's, whatever its profile, since the same sources make the same bytes in each; a run asked to compare makes them again, which is how the step holding them catches a key that leaves out an input its build reads.
    let asked = Asked::read();
    let reads = match (asked != Asked::Compare)
        .then(|| kept.reuse(&state, &out))
        .flatten()
    {
        Some(reads) => {
            println!("the prelude's images reused from {}", images.display());
            reads
        }
        None => {
            if asked == Asked::Reuse {
                panic!(
                    "{ASKED} is `reuse` and {} holds no images of this state of the sources: this cargo mode would make them again rather than reuse what another filed",
                    images.display()
                );
            }
            let reads = made(&out);
            let again = key(workspace, &prelude, &rustc);
            match kept
                .settle(&state, &again, &out, &reads, asked == Asked::Compare)
                .unwrap_or_else(|difference| panic!("{difference}"))
            {
                Settled::Same => println!(
                    "the prelude's images made again, the same as {} holds",
                    images.display()
                ),
                Settled::Filed => {
                    println!("the prelude's images filed in {}", images.display())
                }
                Settled::Moved => println!(
                    "cargo:warning=a source the prelude's build links changed while it ran, so its images are neither compared nor filed"
                ),
            }
            reads
        }
    };

    // Each file the lowering read, rather than the directory: the read set is closed — a module joins `/std` only through a `mod` in a header that is itself a read, so the edit that adds a file changes a file already watched, and the rerun then records the new one. Watching the directory would also rerun this build for anything else written under it, and a question asked about a `/std` module writes its cabinet's memo there. The kept images name each under `std/`, so images taken from another checkout watch this one's.
    let std = std_directory();
    for read in &reads {
        println!("cargo:rerun-if-changed={}", std.join(read).display());
    }

    // The library's manifest is read beside its files, as a package's is: what it names, describes and says its modules answer is part of what the unit is made from.
    println!("cargo:rerun-if-changed={}", std.join(MANIFEST).display());
}

/// Make the prelude's images in `out`, answering the files the lowering read, named under `std/`.
fn made(out: &Path) -> Vec<PathBuf> {
    let std = std_directory();
    compile(out)
        .into_iter()
        .map(|path| {
            path.strip_prefix(&std)
                .unwrap_or_else(|_| panic!("{} is not under {}", path.display(), std.display()))
                .to_path_buf()
        })
        .collect()
}

/// What the compiler Cargo builds with says of itself, `rustc -vV`: its release, its commit and its host. Where the probe fails, the compiler's name, so an unidentified compiler reads a key of its own rather than another's.
fn rustc_identity() -> String {
    let rustc = env::var("RUSTC").unwrap_or_else(|_| "rustc".to_owned());

    Command::new(&rustc)
        .arg("-vV")
        .output()
        .ok()
        .filter(|probe| probe.status.success())
        .and_then(|probe| String::from_utf8(probe.stdout).ok())
        .unwrap_or_else(|| format!("unidentified {rustc}"))
}
