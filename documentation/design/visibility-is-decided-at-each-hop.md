# Visibility is decided at each hop

**Decision.** In the workspace's Rust a name — a function, a type, a constant, a module — is private or `pub`, and every module is declared private, so who reaches a `pub` name is decided by the modules it stands in and the facades above them. A facade is one cluster per child module, a blank line apart: `mod x;`, then `use x::*;` where this module's subtree reads `x`'s surface, then `pub use x::{…};` naming what passes one hop up, or `pub use x::*;` where all of it does. At a crate root `pub use` names the crate's API and is never a glob, so nothing leaves a crate that its root does not name. A struct is a record or an owner: a record's fields are all `pub`, data with no invariant beyond their types; an owner's are all private, and what is done with it is its methods, in `impl` blocks within its own module subtree. A member reaches as far as its type; one meant for fewer readers carries the scoped visibility of its audience, `pub(crate)` or `pub(super)`, the one place a scoped modifier is written. No crate re-exports another crate's item. Clippy's `partial_pub_fields` and `field_scoped_visibility_modifiers` hold the two kinds of struct.

**Rationale.**

- **It is the language's own rule.** [Privacy is scoped to a subtree](surface/privacy-is-scoped-to-a-subtree.md): a declaration without `pub` is visible within its module's subtree and a `pub` one wherever its module is, a representation is sealed or transparent as a whole, and siblings share through a private module, so `pub` keeps one meaning. The compiler is written the way the language it compiles is.
- **An audience is stated once, at the boundary it crosses.** A scoped modifier on every item states its audience beside it and again in the facades it passes, and moving a module rewrites both. A facade is where a subtree decides its surface, and a root's named list is the crate's API in one place.
- **Unused code stays visible.** What a root does not export is checked by rustc's dead-code analysis however it is spelled; it is a root's `pub use x::*` that hides an unused name, never `pub`.
- **A member has no facade.** Rust carries a method's reach with its type, so a method narrower than its type is the case scoped visibility exists for. A field never is: a type whose fields some readers may touch and others may not is an owner, whose fields none touch.
- **Records and owners are the two kinds of type** ([C-STRUCT-PRIVATE](https://rust-lang.github.io/api-guidelines/future-proofing.html#c-struct-private)): a field is public where any value keeps the invariants, and private behind a method where one does not.

**Rejected.**

- **A scoped modifier on each item, held by `unreachable_pub`**, the convention rustc's own crates keep: three spellings the language does not have, fields some readers may touch and others may not, and every audience stated twice.
- **Getters on a record**, a method per field for data that holds no invariant; **setters**, which leave an owner's invariant to its callers.
- **A glob in a crate root's `pub use`**, which exports every name and hides the unused ones from the compiler.
- **Re-exporting another crate's item**, a second path to a name its [authority](one-crate-is-the-authority-for-one-external-concern.md) owns.
