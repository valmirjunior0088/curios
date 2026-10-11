//! The four trees, and which of the repository's paths each holds.
//!
//! **A tree is a manifest root, and a tracked file belongs to the nearest one above it.** The Rust workspace's root is the repository's, and three more stand inside it: the two npm packages and the Zed extension's own workspace under `editors/`. So the workspace's tree is everything outside those three, and each of theirs is its own directory. Nothing lists the files: [`Tree::paths`] reads what git tracks, so a file added anywhere is already some tree's, and exactly one's.

use {crate::ask, std::process::Command};

#[cfg(test)]
mod tests;

/// A manifest root.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Tree {
    Workspace,
    Grammar,
    Vscode,
    Zed,
}

/// The trees that stand inside the workspace's.
const INNER: [Tree; 3] = [Tree::Grammar, Tree::Vscode, Tree::Zed];

impl Tree {
    /// Where the tree's manifest stands, from the repository's root, which is the empty path.
    pub(crate) fn root(self) -> &'static str {
        match self {
            Tree::Workspace => "",
            Tree::Grammar => "editors/grammar",
            Tree::Vscode => "editors/vscode",
            Tree::Zed => "editors/zed",
        }
    }

    /// The fewest paths that hold every tracked file of the tree and no other's: a tool handed them walks the tree and nothing else.
    pub(crate) fn paths(self) -> Result<Vec<String>, String> {
        let mut git = Command::new("git");
        git.arg("ls-files");

        Ok(self.holding(ask(git, &[])?.lines()))
    }

    /// [`Tree::paths`] over `tracked`, the paths git names.
    fn holding<'a>(self, tracked: impl Iterator<Item = &'a str>) -> Vec<String> {
        if self != Tree::Workspace {
            return vec![self.root().to_string()];
        }

        let mut paths = Vec::<String>::new();
        for file in tracked {
            if INNER.iter().any(|inner| within(file, inner.root())) {
                continue;
            }

            // The shortest leading part of the path under which no other tree stands.
            let mut held = String::new();
            for part in file.split('/') {
                if !held.is_empty() {
                    held.push('/');
                }
                held.push_str(part);
                if !INNER.iter().any(|inner| within(inner.root(), &held)) {
                    break;
                }
            }
            if !paths.contains(&held) {
                paths.push(held);
            }
        }

        paths
    }
}

/// Whether `path` is `directory` or lies under it.
fn within(path: &str, directory: &str) -> bool {
    path.strip_prefix(directory)
        .is_some_and(|rest| rest.is_empty() || rest.starts_with('/'))
}
