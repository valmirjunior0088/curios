//! What a plan reads of the checkout, asked once a run: the paths git tracks, and which packages a selection builds.
//!
//! **A plan is computed before anything runs, so planning reads and never writes.** Every step's plan is made from the request and the checkout as it stands, and no plan reads what another step writes, so a run's whole plan, with every edge between its steps, exists before its first task. A question planning puts to git or cargo is asked here, once, and every plan of the run reads the one answer.
//!
//! **Cargo answers for its own graph.** Which packages a selection builds is cargo's to say, so it is asked, through `cargo metadata`, the output cargo versions, rather than stated beside the manifests: a dependency added to any of them moves the answer with nothing else to edit.

use {
    crate::{Selection, TOOLS, Tool, ask},
    serde_json::Value,
    std::{
        cell::OnceCell,
        collections::{BTreeMap, BTreeSet},
        process::Command,
    },
};

#[cfg(test)]
mod tests;

/// The checkout as a run's plans read it.
#[derive(Default)]
pub(crate) struct Checkout {
    tracked: OnceCell<Vec<String>>,
    graph: OnceCell<Graph>,
}

impl Checkout {
    /// Every path git tracks, from the repository's root.
    pub(crate) fn tracked(&self) -> Result<&[String], String> {
        if let Some(tracked) = self.tracked.get() {
            return Ok(tracked);
        }

        let mut git = Command::new("git");
        git.arg("ls-files");
        let listing = ask(git, &[])?;

        Ok(self
            .tracked
            .get_or_init(|| listing.lines().map(str::to_string).collect()))
    }

    /// Whether a build of `selection` builds the workspace's package `package`.
    pub(crate) fn builds(&self, selection: &Selection, package: &str) -> Result<bool, String> {
        Ok(self.graph()?.builds(selection, package))
    }

    /// The workspace's dependency graph, as cargo resolves it.
    fn graph(&self) -> Result<&Graph, String> {
        if let Some(graph) = self.graph.get() {
            return Ok(graph);
        }

        let metadata = ask(
            Tool::Cargo.command(),
            &["metadata", "--format-version", "1"],
        )?;
        let graph = Graph::read(&metadata)?;

        Ok(self.graph.get_or_init(|| graph))
    }
}

/// The workspace's members and what each package depends on, by the ids `cargo metadata` gives them.
#[derive(Debug, Default)]
struct Graph {
    /// Each member's name, by its id.
    members: BTreeMap<String, String>,
    /// What each package depends on, and whether only as a dev-dependency.
    dependencies: BTreeMap<String, Vec<(String, bool)>>,
}

impl Graph {
    /// The graph `cargo metadata`'s JSON states.
    fn read(metadata: &str) -> Result<Self, String> {
        let unread = |what: &str| format!("cargo metadata gave no {what}");
        let metadata = serde_json::from_str::<Value>(metadata)
            .map_err(|error| format!("cargo metadata's output: {error}"))?;

        let names = metadata["packages"]
            .as_array()
            .ok_or_else(|| unread("packages"))?
            .iter()
            .filter_map(|package| Some((package["id"].as_str()?, package["name"].as_str()?)))
            .collect::<BTreeMap<_, _>>();
        let members = metadata["workspace_members"]
            .as_array()
            .ok_or_else(|| unread("workspace members"))?
            .iter()
            .filter_map(Value::as_str)
            .filter_map(|id| Some((id.to_string(), names.get(id)?.to_string())))
            .collect();
        let dependencies = metadata["resolve"]["nodes"]
            .as_array()
            .ok_or_else(|| unread("resolved graph"))?
            .iter()
            .filter_map(|node| {
                let edges = node["deps"]
                    .as_array()?
                    .iter()
                    .filter_map(|dependency| {
                        let kinds = dependency["dep_kinds"].as_array()?;
                        let dev = kinds.iter().all(|kind| kind["kind"] == "dev");

                        Some((dependency["pkg"].as_str()?.to_string(), dev))
                    })
                    .collect();

                Some((node["id"].as_str()?.to_string(), edges))
            })
            .collect();

        Ok(Graph {
            members,
            dependencies,
        })
    }

    /// Whether a build of `selection` builds the member `package`. A selected package's dev-dependencies are built with it, since the steps build every target, and no other package's are.
    fn builds(&self, selection: &Selection, package: &str) -> bool {
        let roots = self
            .members
            .iter()
            .filter(|(_, name)| match selection {
                Selection::Shipped => !TOOLS.contains(&name.as_str()),
                Selection::Every => true,
                Selection::Packages(packages) => packages.contains(name),
            })
            .map(|(id, _)| id.as_str())
            .collect::<BTreeSet<_>>();

        let mut reached = roots.clone();
        let mut pending = roots.iter().copied().collect::<Vec<_>>();
        while let Some(id) = pending.pop() {
            if self.members.get(id).is_some_and(|name| name == package) {
                return true;
            }

            let edges = self.dependencies.get(id).into_iter().flatten();
            for (dependency, dev) in edges {
                if (!dev || roots.contains(id)) && reached.insert(dependency.as_str()) {
                    pending.push(dependency.as_str());
                }
            }
        }

        false
    }
}
