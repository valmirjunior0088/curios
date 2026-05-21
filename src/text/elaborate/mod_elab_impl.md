# Module elaboration implementation

## Context

The elaborator threads a context through the fold with two distinct behaviors:

```rust
struct Context<'a> {
    prefix: Name,
    table: &'a mut HashMap<Name, ModuleInfo>,
    scope: HashMap<String, Name>,
}
```

`table` is shared mutable state accumulated across the entire fold, held as `&'a mut`
following the pattern established by `Lowerer<'a>` in the codebase. As each module is
fully processed, it writes its entry into `table`. All subsequent modules — including
siblings and ancestors — can read from it for absolute `use` resolution.

`prefix` and `scope` are local to each module. `prefix` is passed by value; descending
into `mod Foo` produces a new `Name` via `prefix.with("Foo")`. `scope` is a fresh
`HashMap` per module, built up as items are processed in order: each declared child
automatically adds its label, and each `use` declaration adds its last segment after
being resolved and checked for conflicts.

Entering a child module means calling the recursive function with a context produced
by `context.nested("Foo")`, which reborrows `table` to produce a shorter-lived child:

```rust
fn nested(&mut self, label: &str) -> Context<'_> {
    Context {
        prefix: self.prefix.with(label),
        table: &mut *self.table,
        scope: HashMap::new(),
    }
}
```

Leaving is just the call returning — the child's context is dropped, the parent's
context is unchanged. The only thing that survives the descent is what the child wrote
into `table`.

`ModuleInfo` records which direct children and bindings are `pub`, providing enough
information to enforce visibility when resolving paths:

```rust
struct ModuleInfo {
    children: HashMap<String, bool>,  // label → is_pub
    bindings: HashMap<String, bool>,  // label → is_pub
}
```

`children` covers submodules, `bindings` covers `let` and `rec` entries. No term
content lives here — `ModuleInfo` is only consulted during path traversal to check
existence and visibility at each segment.

After all of a module's items have been processed (children recursed into, `use`
declarations resolved, bindings elaborated), the elaborator constructs `ModuleInfo`
from the `is_pub` flags observed during the fold and inserts it at the current `prefix`
into `table`. This makes the module visible to its siblings and ancestors for subsequent
absolute `use` resolution.

## `use` resolution

Given `TopUse { is_abs, name }`:

**Relative** (`!is_abs`, e.g. `use Foo/Bar`): the first segment must already be in
`scope` as a declared child (enforced by the sequential rule). Look up its full path
from `scope`. For each subsequent segment, look up the previous segment's full path in
`table`, check the next segment exists in `ModuleInfo.children`, and verify it is `pub`
(the current module is not its direct parent). The resolved full path is built by
appending each segment in turn.

**Absolute** (`is_abs`, e.g. `use /Foo/Bar`): the full path is the `name` itself. Walk
each segment through `table` to validate it exists. The first segment's `is_pub` is not
checked — it is a direct child of the root, which is always accessible. Every subsequent
segment must be `pub` in its parent's `ModuleInfo.children`.

**After resolution** (both cases): verify that the resolved full path exists in `table`
as a module — `use` may not target individual bindings. The qualifier is the last
segment of `name`. Check it does not already exist in `scope` — if it does, that is a
conflict error regardless of whether the collision is with another `use` or a declared
child. On success, insert `qualifier → resolved full path` into `scope`.

## Name resolution in terms

`use` may only target modules — the last segment of any `use` path must resolve to a
module in `table`, never to a binding. This means a qualifier in `scope` always names
a module, and single-segment names in terms are unambiguously local variables.

A `Term::Name { path }` is resolved as follows:

**Single segment** (`f`): a local variable (lambda- or let-bound). No scope lookup —
passed through as-is to `core::Var::free("f")`.

**Multi-segment** (`Foo/f`, `Foo/Bar/g`): resolved through the current scope:

1. Look up the first segment in `scope` to get the fully-qualified module path.
2. For each middle segment (all but first and last), look up in the previous module's
   `ModuleInfo.children`. Each must exist and be `pub`.
3. Look up the last segment in the final module's `ModuleInfo.bindings`. It must exist
   and be `pub`.
4. The resolved name is the module path from step 1 extended with all remaining segments.

The result is a fully-qualified `Name` (e.g. `["Foo", "Bar", "g"]`), which at lowering
time becomes `core::Var::free("Foo/Bar/g")` — consistent with what `elaborate.rs`
already produces for `Term::Name`.

## Flat item

The flat list produced by the fold has the shape `Vec<FlatItem>`:

```rust
struct FlatLet {
    name: Name,
    type_: core::Term,
    body: core::Term,
}

enum FlatItem {
    Let(FlatLet),
    Rec(Vec<FlatLet>),
}
```

`FlatLet` is the common unit — a single binding with its fully-qualified `Name`, type,
and body. `FlatItem` is either a single `FlatLet` or a mutually recursive group of
them. The flat list is `Vec<FlatItem>`. Names are joined to `String` only at the
lowering boundary when producing `core::Term`.

## Lowering

By the time lowering begins, the flat list holds fully-qualified `Name`s and
already-elaborated `core::Term` values — name resolution and term elaboration both
happen during the fold. Lowering is a right-fold over the flat list with
`elaborate(entrypoint.tail)` as the base case:

- `Let(FlatLet { name, type_, body })` → `core::Let::new(name.path.join("/"), type_, body, acc)`
- `Rec(items)` → `core::Rec::new(items.map(|it| (it.name.path.join("/"), it.type_, it.body)), acc)`
