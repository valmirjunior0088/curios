# curios-download

A file a build pins by its SHA-256, obtained one way for everything that takes something from the network: read where a hand placed it or an earlier run kept it, else downloaded, checked against its digest either way, and a failure handed back with how to fetch and place it by hand. `curios-unicode`'s database, `curios-binaryen`'s source archive, `curios-profile`'s trace schema and the tools `xtask`'s steps run are fetched through it, and what a download holds is kept by it too, a tarball unpacked or a binary kept as an executable, whole or not at all. What a `Download` takes, what each of its readings does and what each keeping piece promises belong to the crate rustdoc.

## Design

### A crate of its own, taken by the build scripts and `xtask`

**Decision.** Fetching a pinned file is this crate's, a build-dependency of each build script that fetches one, a dependency of `xtask`, and a dependency of no product. It is the one crate that names `ureq`, `tar` and `flate2`, and states their versions itself. A failure is a value it hands back, which a build script panics with and `xtask` refuses a run with, in the same words.

**Rationale.** The protocol — a placed copy before the network, the digest checked either way, a download kept by a rename so a stopped run leaves no partial file, and the instructions an offline build is given — was written in each build script that fetched, alike, and `xtask` fetches the tools its steps run by the same protocol rather than a second. `curios-common`, where shared pieces live, cannot hold it: `curios-unicode`'s build script is one of those that fetch, and `curios-common` takes `curios-unicode`, which Cargo refuses as a cycle; and `curios-common` is a dependency of every product crate, the certifier and the browser bundle among them, which would each then carry an HTTP client that only build scripts and the jig use. A pinned archive is unpacked here for the same reason it is fetched here: `curios-binaryen` unpacked its source with `tar` and `flate2` of its own, which `xtask` would have restated for the tools.

**Rejected.** The protocol written again in each taker. The helper in `curios-common`, for the cycle and the client above. A fetcher in `xtask` beside this one, a second protocol for one job. A failure that panics here, which a build script reads as its own and `xtask` would report as a crash.

### Keeping a download is the caller's choice

**Decision.** `Download::kept` keeps what it downloads where a placed copy would stand, for a file a build reads every time it runs, as `curios-unicode` reads the database and `curios-profile` the schema; `Download::bytes` keeps nothing, for a file a run consumes into something of its own, as `curios-binaryen` keeps the library it builds and not the archive it built it from, and `xtask` keeps a tool unpacked and not the download it came in.

**Rationale.** A file read at every build costs a download at every build unless it is kept, and one consumed once is dead weight beside what it was consumed into.
