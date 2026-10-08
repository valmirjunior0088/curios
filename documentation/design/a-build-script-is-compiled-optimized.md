# A build script is compiled optimized

**Decision.** The workspace sets `opt-level = 3` under `[profile.dev.build-override]` and `[profile.release.build-override]`: a build script, and every crate one depends on, is compiled optimized under both profiles.

**Rationale.** Two build scripts are the compiler run over the standard library. `curios-prelude-archive`'s parses, lowers, elaborates and erases `/sys` and `/std`, and `curios-prelude`'s certifies what that produced, so every build that reaches the prelude runs the compiler's front half over the largest program the workspace holds. Cargo compiles a build script and its dependencies [unoptimized under every profile](https://doc.rust-lang.org/cargo/reference/profiles.html#build-dependencies) unless told otherwise, which runs that program on the slowest compiler the workspace can make. Optimizing it costs one optimized compile of the front half and saves the run: after an edit to `curios-core`, the lint recipe takes 82 s where it took 221 s, and the profiling release build 143 s where it took 232 s.

Timed at `6f0186b01` on eight jobs, each figure the median of three readings, which span 82 to 82 s and 220 to 221 s for the lint and 143 to 147 s and 231 to 232 s for the build. To retake it, take the manifest with the two sections and without them, and for each in turn touch `curios-core/src/lib.rs` and time `cargo xtask clippy`, then `cargo xtask profile programs/hello_world.crs --profile release`: one round unrecorded, so that a reading pays for the edit and not for a profile's first build, then three rounds with the variants interleaved.

**Rejected.**

- **The release override at the release profile's own settings**, `codegen-units = 16` beside the level, so that Cargo would build the front half once for the scripts and for the binary. It builds `curios-core` twice all the same, as it does without, and takes as long: 143 to 146 s over the three readings.
- **The debug profile left as it was.** A debug build compiles the front half unoptimized for its own targets either way, and the scripts' copy is the one that runs the library; the lint recipe is the check made between the steps of a change, which is where the 221 s were.
