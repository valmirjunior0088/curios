# Full-conformance `std/Toml` upgrade

Not refined yet. This placeholder reserves the specification location for upgrading the landed native-width `std/Toml` codec to full TOML conformance.

## Known constraints

- Follows the landed native-width codec (`curios-prelude-archive/std/Toml.crs`, which documents its contract and product limits) and preserves the `Toml` module's ownership and public API shape.
- Exact integer storage and exact decimal handling depend on the general numeric work: `/std/BigInt` and the general rational `BigFlt` sequence in [`big-flt-general`](big-flt-general/01-big-nat-euclidean-spec.md).
- The native-width codec keeps its numeric limitations explicit in its API, tests, and the roadmap precisely so this upgrade can land without changing module ownership.
