# curios-archive

Zero-copy archiving for the workspace: the one crate that names rkyv ([One crate is the authority for one external concern](../documentation/design/one-crate-is-the-authority-for-one-external-concern.md)), the `archived` attribute every stored type carries, the `Proxy`/`Via` adapter for a type rkyv cannot archive directly, and the four entry points — `to_bytes`, `from_bytes`, `access`, `deserialize` — with the error type fixed. rkyv is reached through this crate's re-export by `curios-archive-derive`'s expansion and nowhere else, so grepping the workspace for `rkyv` finds only prose and file names. How to annotate a type and what each entry point returns belong to the crate rustdoc; the macro's own decisions are `curios-archive-derive/README.md`'s.

## Design

### Every build archives

**Decision.** Archiving is no Cargo feature. rkyv is an ordinary dependency of this crate, `#[archived]` derives in every build, and a crate whose types are stored says so by depending on this one.

**Rationale.** The compiler, the launcher and the browser bundle each read an archive, so a build without one is a configuration no product is and no check compiles: what only it compiles goes stale unseen, and a package built alone reaches it through a dev-dependency that names no feature. With one configuration, what a focused check compiles is what ships. Products are told apart by crate boundaries, never by features.

**Rejected.** An `archive` feature on each crate that stores a type, gating the derive's expansion and every function that reads or writes stored bytes: it keeps a second body beside each read that restores, for a build nothing runs. A check that builds the workspace with archiving off, which holds that build to compile and nothing to need it.

### A type rkyv cannot archive is described by a stand-in, once

**Decision.** `Proxy<Value>` states one conversion — this value converts to an archivable one, and back — and `Via<P>` supplies the three rkyv adapter impls (`ArchiveWith`, `SerializeWith`, `DeserializeWith`) from it. A bignum is its little-endian bytes, a hash map its sorted entries, and an interned path or spelling the segments or text the process table already holds.

**Rationale.** A hand-written field adapter is that three-impl shape around one idea; written once here, a crate declaring a proxy names no rkyv trait. The stand-in is handed over borrowed rather than by value because the fixed prelude holds on the order of a hundred thousand qualifier occurrences over a couple of thousand distinct paths, and a by-value signature would clone once per occurrence on the way out.

### The entry points fix the error type

**Decision.** `to_bytes`, `from_bytes`, `access` and `deserialize` fix rkyv's error type to `rancor::Error`, hand back a `String`, and take rkyv's serializer, validator and deserializer bounds on themselves. `to_bytes` returns rkyv's aligned buffer behind a newtype rather than a `Vec<u8>`.

**Rationale.** Every call site instantiates that generality identically, so it buys nothing and costs each caller a `curios_archive::rkyv::` path. The aligned buffer stays because converting it to a `Vec` would copy every byte of an image that can run to megabytes; the workspace builds rkyv `unaligned`, so an archive is read wherever it lands.

### A value some readers never ask for is stored apart and read when asked

**Decision.** `Deferred<T>` holds a value in hand or the bytes it was archived as, and restores it the first time it is read; its holder skips the field in its own archive, stores `Deferred::bytes` where it stores itself, and hands them back through `Deferred::stored` where it is restored. The bytes are a `Bytes`: a run of the buffer they were read from, shared with every other part read from it, or a constant of the executable, borrowed. `joined` lays several parts out behind their lengths and `parted` reads them back as runs of the one buffer, refusing a layout cut short. A value restored from bytes it never changed is stored again as those bytes (`Deferred::as_stored`). `Pooled` restores the parts of one archive through one pool kept for as long as the archive is read, so a node two parts share is restored once.

**Rationale.** A unit is read by more readers than any one compilation is, and each takes a different part: a holder that decodes everything pays for all of them at every opening. Bytes held as a run of their buffer cost a count to keep and nothing to copy, and a part never read is never validated. The pool is what keeps one node for one structure across parts restored at different times, which a term's comparison leans on: its first test is a pointer.

**Rejected.** A part of a shared archive checked where it is read: the library's validator walks from a root and narrows what each pointer may reach to what the pointers before it left, so a node first written for an earlier part is out of range from a later one, and an archive whose parts share is checked whole where it is opened. A checked view held beside the bytes it borrows, for an archive that is no constant: it takes `unsafe`, or an unchecked access after the checked one, which is the same word.
