# Module elaboration specification

## Pipeline

```
Entrypoint  →  resolve use + enforce opacity  →  flat representation  →  core::Term
```

The elaborator receives a textual `Entrypoint` exactly as the parser produces it. It is
responsible for all semantic work before lowering to `core::Term`. Nothing is pre-resolved
by the bundler; the elaborator handles the full tree.

## Module graph

Modules form a DAG — there are no circular dependencies. A module may `use` another only if
that module does not (transitively) `use` it back.

Declaration order is enforced: a module or binding may only be referenced after it has been
declared. The elaborator processes items sequentially and rejects forward references.

## Scoping

Modules are fully isolated. No context is inherited between modules.

A module's declared children (`mod Foo { ... }`) are automatically in scope within that
module — their full transitive public surface is accessible via qualified paths (`Foo/f`,
`Foo/Bar/g`, etc.) without any `use` declaration.

`use` is needed in two cases:

1. To bring a non-child module into scope (always via an absolute path).
2. To get a short-name shorthand for a deeper descendant: `use Foo/Bar` makes `Bar/...`
   writable in terms instead of `Foo/Bar/...`.

`use` does not inline or import bindings — it only authorizes the use of a qualified name.
The content of the module remains in the module table; the elaborator looks it up when
resolving a reference.

`TopUse` carries an `is_abs` flag:

- `use Foo/Bar` — relative: follows edges downward from the current module (`Foo` is a
  child, `Bar` is a child of `Foo`). Authorizes `Bar/...` in terms.
- `use /Foo/Bar` — absolute: resolved from the root. Also authorizes `Bar/...` in terms.

The last segment of a `use` path is the qualifier in terms. The preceding segments are only
for resolution. Relative paths never search upward or sideways. `use` declarations are
independent of each other.

`is_abs` lives only on `TopUse`. Name references inside terms (`Foo/f`) are always resolved
through the established scope; they carry no absolute annotation of their own.

### Conflicts

If two `use` declarations produce the same last segment (e.g. `use Foo/Baz` and
`use Bar/Baz`), it is a conflict error. The fix is to drop both and refer to the targets by
their full subpaths (`Foo/Baz/f`, `Bar/Baz/f`), which are already accessible since `Foo`
and `Bar` are declared children.

The same rule applies when a `use` declaration's last segment matches a declared child
module. A child named `Foo` and a `use /External/Foo` both introduce `Foo/...` — that is
an error.

## Visibility

`is_pub` on `TopLet` and `TopMod` controls visibility _above the declaring module_. The
direct parent always has access to its own children regardless of `is_pub` — it declared
them. `is_pub` is only meaningful to ancestors further up and to unrelated modules.

Visibility cascades: each segment in a qualified path must be `pub` for the path to be
reachable. A private intermediate module blocks the entire subtree below it.

For bindings: a module that accesses `Bar/f` requires `f` to be `pub` in `Bar`.

For modules: in a `Foo/Bar/Baz` hierarchy where `Baz` is not `pub`, `Bar` can still access
`Baz/...` (it is `Baz`'s direct parent), but `Foo` cannot reach `Baz` and neither can any
outside module via an absolute path. This lets `Bar` use private sub-modules as
implementation details invisible to `Foo` and beyond.

`use` declarations are never re-exported — there is no transitive visibility.

## Flat representation

After resolving `use` and flattening nested `Mod`s, every binding is assigned a fully
qualified name. The flat representation has the shape:

```
Vec<Item>
```

where `Item` is either `Let(FlatLet)` or `Rec(Vec<FlatLet>)`, and `FlatLet` is a
single binding with its fully-qualified `Name`, type, and body (e.g. `Name` is
`["Bar", "helper"]` for a `pub let helper` inside `mod Bar`). `Rec` groups mutually
recursive bindings, each as a `FlatLet` with its own fully-qualified `Name`.

The `Entrypoint.tail` is not part of the flat list — it is lowered separately and becomes
the tail of the final `core::Term`.

At the text level, `TopLet.label` and `TopMod.label` are `String` (local single-segment
names). The qualified `Name` is only computed during flattening.

## Forbidden constructs

The elaborator rejects:

- `pub` on a top-level `Entrypoint` item — there is no outside to export to.
- A single-segment relative `use Foo` — `Foo` is already in scope as a declared child
  and re-declaring it as a `use` qualifier is an error.

## Future work

**Opacity** controls whether the _definition_ of a `pub` binding is visible to consumers,
as distinct from whether the name is accessible. A per-binding annotation on `TopLet` is
needed. The precise form is not yet decided.
