//! Where everything a build generates or fetches is kept.
//!
//! `.curios/` sits beside the governing root — the umbrella's manifest when one governs the invocation, the package's otherwise — and it is **the only generated directory in the tree**: member directories hold user files and nothing else.
//!
//! It holds what a command emits, a family for each target and one for documentation, beside the [cabinet](crate::Cabinet), which holds whole what is kept for reuse, each family in its own subtree rather than sharing one namespace:
//!
//! ```text
//! .curios/
//!   native/           json/serve        what `curios compile` emits for the native target
//!   browser/          json/serve/       what it emits for a browser: the page's directory
//!   documentation/    json/             what `curios document` emits
//!   cabinet/
//!     sources/        c1/<digest>/      materialized source trees, keyed by their manifest hash
//!     foreign/        f1/<digest>       fetched foreign modules, keyed by the hash they were accepted against
//!     objects/        <compiler>/       what one compiler filed, an object an address: a judged unit's
//!                       <address>/      `unit` file and the one it replaced, with the packs of the blobs
//!                                       they name; or an executable's precompiled payloads, a file a target
//!     compiler                          this machine's memo of the digest of the compiler running now
//! ```
//!
//! One transient entry passes through the cabinet beside its families: `pinning.<pid>`, where `curios pin` stages a delivery whose digest it has yet to compute, before renaming it into the family it belongs to. See [`Cabinet::staging`](crate::Cabinet::staging).
//!
//! Separated because the alternative re-invites a collision that nesting otherwise removes: a hash has to be transformed to sit in a directory name at all — `c1:<digest>` most naturally becoming `c1/<digest>` — and a package legitimately named `c1` would then land on top of it.
//!
//! That tree is what a project gets when nothing points elsewhere. A cabinet named for every project a run sees (`CURIOS_CABINET`, which the command line reads) stands in for `cabinet/` whole — never for `native/`, `browser/` or `documentation/`, which belong to the package that declared them — so two projects pinning one revision materialize and compile it once, and a project shares, moves or clears what it keeps for reuse as one directory.
//!
//! A product's family is named for the target it is built for, as the manifest's `[foreign.<handle>.<target>]` tables name it, and every family of the cabinet is the plural of what it holds: sources, foreign modules and objects. A rename orphans entries under an old name rather than misreading them, since an address holds its family's schema tag and nothing looks in the old directory, so an existing tree rebuilds once; and a filing in the project's own cabinet takes away what no family of the running compiler holds, the orphans with it (`curios-verdict`'s `collect_project`).

#[cfg(test)]
mod tests;

use {crate::Target, std::path::PathBuf};

/// The generated directory itself.
pub const PRODUCTS: &str = ".curios";

/// The family of every package's documentation.
const DOCUMENTATION: &str = "documentation";

/// What a project's commands emit: a build of each executable for each target and each package's documentation, beside the project.
///
/// Named by the package that declared them, they belong to the project that built them and stay in its `.curios/` whatever cabinet it files into.
pub struct Products {
    /// The governing root — the umbrella's directory when one governs, the package's otherwise.
    project: PathBuf,
}

impl Products {
    /// The products of the project at `project`.
    pub fn at(project: PathBuf) -> Self {
        Self { project }
    }

    /// Where a build of an executable for `target` is written: a native executable's file, or a browser page's directory, nested under the package that declares it, beside the project.
    ///
    /// A package's name is the one identity in a compilation that cannot collide (law 2), so nesting by it removes the collision by construction — two members of one umbrella may both declare `serve` and nothing has to refuse it. Nesting also fixes the path *inside* `.curios/`: `<target>/<package>/<executable>` does not depend on what encloses the package, so joining an umbrella re-roots a build without renaming it.
    pub fn built(&self, target: Target, package: &str, executable: &str) -> PathBuf {
        self.directory()
            .join(target.spelled())
            .join(package)
            .join(executable)
    }

    /// Where a package's generated documentation is written: nested under the package, beside the project, for the reason an executable is.
    pub fn documentation(&self, package: &str) -> PathBuf {
        self.directory().join(DOCUMENTATION).join(package)
    }

    /// `.curios/` itself.
    pub fn directory(&self) -> PathBuf {
        self.project.join(PRODUCTS)
    }

    /// Every family of what a command emits, each where it stands: a build's for each target, and the documentation's. Beside them `.curios/` holds the project's own cabinet, and nothing else of its own.
    pub fn families(&self) -> Vec<PathBuf> {
        Target::ALL
            .into_iter()
            .map(|target| self.directory().join(target.spelled()))
            .chain([self.directory().join(DOCUMENTATION)])
            .collect()
    }
}
