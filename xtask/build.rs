//! Hands the tool the triple it was built for, which is the host every recipe defaults to — read from cargo's `HOST` here, since a running binary has no constant for it and the alternative is parsing `rustc -vV` at every invocation.

use std::env;

fn main() {
    println!(
        "cargo:rustc-env=CURIOS_HOST_TRIPLE={}",
        env::var("HOST").expect("cargo sets HOST for a build script")
    );
}
