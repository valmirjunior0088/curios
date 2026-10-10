# A test is compiled optimized, with its checks on

**Decision.** The workspace sets `opt-level = 3` and `debug = "line-tables-only"` under `[profile.test]`: every test run — the `test` and `doctest` recipes, the gate's, the check workflow's and a contributor's own `cargo test` — compiles the workspace and its dependencies optimized. The profile inherits everything else from `dev`, so debug assertions and overflow checks stay on. The level is the one `dev` states too, [for its build scripts' sake](a-build-script-is-compiled-optimized.md), and is written here so that a test run is optimized by this profile's own word; what the profile changes of `dev` is the debug information.

**Rationale.**

- **A test is a program compiled by the compiler under test.** Nearly every test of `curios` and of `curios-std` restores the prelude and then lowers, optimizes and emits what its program links of `/std`, so the suite's time is the compiler's speed at exactly that, and an unoptimized compiler is the slowest one the workspace can make.
- **The checks are settings of their own.** What Cargo's release profile gives up beside the optimization it adds is debug assertions and overflow checks, which that profile switches off and this one does not: a `debug_assert!`, a verification under `cfg(debug_assertions)` and an overflow panic fire in a test as they do in a `dev` build.
- **In the profile, not in a flag.** No spelling of a test run is the slow one: the recipe, the gate and a bare `cargo test -p <crate> <name>` build one set of artifacts and reuse it.
- **Line tables keep a failure placed.** A panic names its file and line whatever the debug information, and a backtrace keeps its lines. What is given up is the variables a debugger reads, and the build a debugger is pointed at is `cargo build`'s, which this profile does not touch.
- **The frames a walk is tested on are the shipped compiler's.** A walk over data-shaped depth is held to the default test-thread stack, and an optimized frame is smaller than an unoptimized one: the suite's claim is then about the frames a release has, and not about larger ones no profile of the workspace compiles.

**Rejected.**

- **The release profile for a test run.** It drops the debug assertions and the overflow checks, which a test run exists to meet.
- **A named profile that inherits `release` and switches the checks on**, passed by the recipe: a bare `cargo test` stays `dev`'s, and two spellings of a test run build two sets of artifacts.
- **Dependencies optimized alone.** The time is in the workspace's own crates — closed-term evaluation, the continuation optimizer, the prelude's restoration — and generic code is compiled at its user's level.
- **Full debug information**, as `dev` has: an optimized build made slower and larger for variables no test run reads.
