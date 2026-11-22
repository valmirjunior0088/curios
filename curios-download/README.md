# curios-download

A file a build script pins by its SHA-256, obtained one way for every build script that takes something from the network: read where a hand placed it or an earlier build kept it, else downloaded, checked against its digest either way, and the build failing with how to fetch and place it by hand where it cannot be had. `curios-unicode`'s database, `curios-binaryen`'s source archive and `curios-profile`'s trace schema are fetched through it. What a `Download` takes and what each of its two readings does belong to the crate rustdoc.

## Design

### A crate of its own, taken by build scripts alone

**Decision.** Fetching a pinned file is this crate's, a build-dependency of each build script that fetches one and a dependency of nothing. It is the one crate that names `ureq`, and states its version itself.

**Rationale.** The protocol — a placed copy before the network, the digest checked either way, a download kept by a rename so a stopped build leaves no partial file, and the instructions an offline build is given — was written in each build script that fetched, alike. `curios-common`, where shared pieces live, cannot hold it: `curios-unicode`'s build script is one of those that fetch, and `curios-common` takes `curios-unicode`, which Cargo refuses as a cycle; and `curios-common` is a dependency of every product crate, the certifier and the browser bundle among them, which would each then carry an HTTP client that only build scripts use.

**Rejected.** The protocol written again in each build script that fetches. The helper in `curios-common`, for the cycle and the client above.

### Keeping a download is the caller's choice

**Decision.** `Download::kept` keeps what it downloads where a placed copy would stand, for a file a build reads every time it runs, as `curios-unicode` reads the database and `curios-profile` the schema; `Download::bytes` keeps nothing, for a file a build consumes into a cache of its own, as `curios-binaryen` keeps the library it builds and not the archive it built it from.

**Rationale.** A file read at every build costs a download at every build unless it is kept, and one consumed once into a cache is dead weight beside it once the cache is filled.
