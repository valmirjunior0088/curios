# A package's documentation is generated from the compilation that builds it

## Status

Refined; nothing landed. `curios document` writes a package's interface as static pages, read off the same compilation that builds it. The visual design of the pages and where they are hosted are deliberately not specified here.

## Why it exists

A package can be declared, pinned, fetched, compiled, cached and depended on, and a consumer still cannot read its interface without reading its source. This is the missing half of the ecosystem story, and every fact it needs is already computed: what a module exports, which representation is transparent to whom, and how a declaration is printed.

## The organizing rule

**A library is documented for its consumers; a program for its author.** What a page shows is what its audience may see. For a library that is the export view the text lowering already builds — public names pointing at canonical declaration sites — so a private declaration, a private constructor and a test are absent rather than hidden. A program has no consumer, so documenting one means the author audience, in which everything renders. The first version fixes the audience at the library's consumers and refuses programs by name; making the audience a parameter is how programs arrive.

## What it reads and writes

`curios document` takes no target, as `compile` takes at most an executable's name: it documents the governing package's library, and `--manifest` overrides as everywhere. It reads the manifest for the package name and its optional `description`; the scope from the store in dependency order, read and never written, `/std` from the archive; the library's surface modules through the compiler's own resolver, whose headers give the module tree; and the export view of each module. Elaboration must succeed first, so a package that does not check is not documented and its diagnostics are reported as `run` reports them, with nothing written and a non-zero exit.

It writes `.curios/documentation/<name>/`, or `--output <DIR>`: `index.html`, the landing page, showing the package name, its description and the module tree; one page per module at its source path, so `/json/parse/lexer` is `parse/lexer.html`; and one stylesheet under `static/`. No script, no search, nothing fetched from outside; the pages read from `file://`. Files are overwritten by name and nothing else in the directory is touched. Success prints nothing.

## Decisions

- **Only a package is compiled or documented; only `run` and `wonder` take a loose file or standard input.** `compile` takes the two forms a package gives it — nothing, for the default executable, or an executable's name — and `document` takes nothing. A product written to disk needs an owner to be filed under and a name to be filed as, and the manifest is the only thing that supplies either; a loose file has neither, which is why `compile -` had to demand `-o` and `compile file.crs` had to invent a name from a stem. `run` keeps all four forms because trying a theory is what a heredoc is for and it leaves no product behind, and `wonder` keeps them because a question executes nothing. With standard input gone from `compile`, `-o` is only ever an override.
- **The store's families are named for what they hold, in the plural.** `.curios/` gains `documentation/<name>/` as its fifth family, filed beside `executables/` because both are products of the package that declared them and stay local when `CURIOS_CACHE` moves the content-addressed families elsewhere. The existing four are renamed with it — `bin` to `executables`, `src` to `sources`, `unit` to `verdicts`, `payload` to `payloads` — so every family is the plural of its contents and the words match the ones the manifest, the cache type and the soundness entry already use: `[[executables]]`, `Verdicts`, "Cached verdicts". A rename orphans entries under the old names rather than misreading them, since the slot schema keys live inside slot names, so an existing store rebuilds once.
- **Signatures are printed from the surface tree, by the printer `curios format` uses.** Every declaration states its signature — a top-level `let` requires its annotation, and `induct`, `struct`, `concept`, `foreign` and `satisfy` declare theirs — so the elaborated term is never the source of a page. The elaborated print spells `Nat` as `/sys/Nat/Nat` and carries universe variables, which no reader wants. A `satisfy` shows its head and telescope as written, with a note when its body is derived.
- **A signature carries its referents.** Every name in a printed signature is recorded with the declaration it resolves to and the mount owning it. A referent inside the unit renders as a link to its anchor; one outside renders as its qualified name in plain text, because a dependency mounts at a declared name pinned by revision and hash, and there is no registry whose address a link could name. Recording the identity now is what lets a mapping arrive later without changing the record.
- **The record is plain data in the `wonder` engine, and the subcommand renders it.** A query answers on stdout and writes nothing, which a bundle of files cannot honor, so `document` is a subcommand beside `test` and `format`, with its computation in `wonder/document.rs` as `declared_tests` is beside `curios test`. Producing a unit's record belongs in `curios-text`, which owns both the surface tree and the export view. A `wonder document` query printing the record is a transport for later.
- **A documentation comment is syntax.** `-- ` with the space is the only plain comment opener and `-- | ` opens a documentation comment; both are also valid with the line ending immediately after them, since a trailing space is invisible. Consecutive `-- |` lines form one block, an empty `-- |` line is a paragraph break, and the parser attaches a block to the immediately following `let`, `and` member, `induct`, `struct`, `concept`, `satisfy`, `foreign` or `mod`, or to a constructor, field or concept method inside one. Whitespace and plain comments between are insignificant. A block before anything else, a second block before the same item, or a `-- |` after code on a line is an error. Plain comments stay outside the tree, so the formatter's identity contract is untouched, and no existing source changes meaning: none lacks the space and none begins with a pipe.
- **A module's prose is the `-- |` on its `mod` declaration; a package's is its manifest `description`.** One rule, no positional tie-break for a block at the top of a file, and no second marker. The library root is declared by no `mod`, and what describes a package as a whole is a manifest fact.
- **A re-export is a link, never a copy.** A declaration has exactly one page and one anchor, its name within its module.
- **`/std` is documented by the same engine, in the tests.** It is the largest interface in the tree — opaque types, derived witnesses, concepts, proof-carrying structures — and it is reachable from the archive without a checkout. A tool that cannot document it documents nothing worth reading.

## Sequence

1. `compile` narrowed to a package: the two target forms, `-o` optional, the CLI's help text, `usage.md`, `README.md`'s example and the bundle test.
2. The store's families renamed and `documentation/` added: the constants and header in `curios-package/src/store.rs`, the "four families" comment in `curios/src/cache.rs`, "Where things go" in `usage.md`.
3. The comment grammar: the required space, `-- |`, its attachment, `syntax.md`, parser tests and the formatter's round trip over every source in the tree.
4. The engine and the record, with a test documenting `/std` and one documenting a fixture package.
5. `curios document`: the landing page, the module pages, the stylesheet, the `usage.md` row and the CLI entry.
6. `/std`'s prose above declarations moved from `-- ` to `-- |` where it describes the declaration.

## Later, and out of this specification's scope

Programs, under the author audience, which also gives a standalone file and `-` a meaning, and with them the landing page listing the package's executables. A file target placed in its library by membership, as `wonder` places one. A `wonder document` query emitting the record as JSON for an editor and the browser playground. Code spans in prose resolved against the declaring scope, and tests rendered as the examples of the declarations they mention. Pages for the dependency closure, so every referent links. A mapping from a mount to an external address once anything hosts documentation.
