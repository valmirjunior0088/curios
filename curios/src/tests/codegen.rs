//! End-to-end source-to-Wasm codegen parity tests.

mod big_nat_limb;
mod census;
#[cfg(feature = "profile")]
pub(super) use census::TOML_DRIVER;
// The churn probe hears the engine's collection announcements through `curios-profile`'s log bridge, which exists only under its `enabled` feature — reached here through this crate's `profile` feature, which the probe's recorded command enables via `--all-features`.
#[cfg(feature = "profile")]
mod churn;
mod ladder;
mod map_wall;
mod parity;
mod shapes;
mod structural;
