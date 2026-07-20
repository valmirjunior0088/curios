# Full-conformance `std/Toml` upgrade

Not refined yet. This placeholder reserves the specification location for upgrading the landed native-width `std/Toml` codec to full TOML conformance.

## Known constraints

- Follows the landed [native-width codec](02_TOML_NATIVE_WIDTH_SPEC.md) and preserves the `Toml` module's ownership and public API shape.
- Exact integer storage and exact decimal handling depend on the general numeric work: `/std/BigInt` and the general rational `BigFlt` sequence in [`big_flt_general`](../big_flt_general/01_BIG_NAT_EUCLIDEAN_SPEC.md).
- The native-width codec keeps its numeric limitations explicit in its API, tests, and the roadmap precisely so this upgrade can land without changing module ownership.
