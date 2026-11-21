//! The addresses the cabinet files a unit and a payload under: what each is, and nothing of what it was compiled from, which the record filed beside it is verified against.

#[cfg(test)]
mod tests;

use {
    curios_common::{Fingerprint, Mount, Qualifier},
    curios_module::Compiled,
    curios_package::Runs,
};

/// What version of the unit family's layout an address names.
///
/// In the address rather than in a file beside it, so an entry written by an older layout is not found rather than found and misread. Bump it whenever what an object holds, or what a hit is verified against, changes.
const OBJECT_SCHEMA: &str = "u116";

/// The same, for the payload family — its own tag, because the two families version independently and neither should invalidate the other by moving.
const PAYLOAD_SCHEMA: &str = "p13";

/// The address a unit compiled by `compiler`, claiming `mounts`, declaring `declared`, `compiled` built or tested, and in a fold whose first unit stands in for the archived root claiming `stand_in` where one does, is filed under.
///
/// **No file contents here, deliberately.** This names a *place*, not a version of what lives in it: "the unit for these mounts, declaring these dependencies, by this compiler". What was compiled is checked when the object is opened, against the record of what was read, so a source edit changes the verification and not the address. That is what bounds the cabinet — a project has as many objects as it has units, forever, rather than one per compile, as an address hashed over the unit's source directory, which contains its `.curios/`, would give.
///
/// **Five parts, and each is load-bearing.** The compiler is who judged it — see [`compiler`](curios_package::compiler()), and note that a key naming no compiler would be believed on behalf of any. The mounts are how the unit's names are spelled, which the lowering depends on and no read of its files reveals. The third is what the unit declared it could see: a declared dependency answers "may these names reach that root" per unit, and it is the answer the lowering consults — the same source declaring different prefixes resolves different names and is a different unit. `None` is its own value, not an empty list: it means the caller declared nothing, which is every open prefix in scope, where an empty list means a unit that asked for none of them. The fourth is what the unit is compiled for: a tested unit holds its tests and a built one none, so the same source is two units. The fifth is the archived root a unit of its fold stands in for, as a package named `std` does: what a unit reads of the compiler's own is named by the compiler alone and recorded nowhere, so a unit that read a stand-in in its place is another unit, and none is named where no unit stands in, so every other address is the one it was.
///
/// **No unit before it.** What a unit read of the units before it is verified, as its own source is: its record names what each unit it could have read contained, and an object whose record no longer agrees is the baseline its unit is compiled over. So a unit two folds reach after different others is filed once, and an edit to a unit it cannot read moves neither its address nor its record.
pub fn unit_address(
    compiler: &str,
    mounts: &[Mount],
    declared: Option<&[Qualifier]>,
    compiled: Compiled,
    stand_in: Option<&[Qualifier]>,
) -> String {
    let mut fingerprint = Fingerprint::new();

    fingerprint.feed(OBJECT_SCHEMA);
    fingerprint.feed(compiler);
    fingerprint.feed(mounts.len().to_string());
    for mount in mounts {
        fingerprint.feed(mount.prefix.join());
    }
    feed_declared(&mut fingerprint, declared);
    feed_compiled(&mut fingerprint, compiled);
    feed_stand_in(&mut fingerprint, stand_in);

    fingerprint.hex()
}

/// The address the unit of an entry's own declarations is filed under: `package`'s `executable`, compiled by `compiler`, declaring `declared`, `compiled` built or tested, and over the stand-in `stand_in` names where its fold has one.
///
/// **An entry claims the empty prefix, as every entry does**, so [`unit_address`] would file every program of a project in one place, each overwriting the last. What tells two entries apart is what tells their payloads apart ([`payload_address`]): the package's name and the executable's, the one identity in a compilation that cannot collide. A program no package declares has none, and its unit is filed nowhere.
///
/// What was compiled is verified when the object is opened, as a unit's is, the entry's own file among what it read.
pub fn entry_address(
    compiler: &str,
    package: &str,
    executable: &str,
    declared: Option<&[Qualifier]>,
    compiled: Compiled,
    stand_in: Option<&[Qualifier]>,
) -> String {
    let mut fingerprint = Fingerprint::new();

    fingerprint.feed(OBJECT_SCHEMA);
    fingerprint.feed(compiler);
    // Where a unit's address states how many prefixes it claims, which no word spells.
    fingerprint.feed("executable");
    fingerprint.feed(package);
    fingerprint.feed(executable);
    feed_declared(&mut fingerprint, declared);
    feed_compiled(&mut fingerprint, compiled);
    feed_stand_in(&mut fingerprint, stand_in);

    fingerprint.hex()
}

/// The archived root a unit of the fold stands in for, as an address states it: nothing where none does, so an address over the archived root is the one it was before a stand-in had a word.
fn feed_stand_in(fingerprint: &mut Fingerprint, stand_in: Option<&[Qualifier]>) {
    if let Some(prefixes) = stand_in {
        fingerprint.feed("stand-in");
        fingerprint.feed(prefixes.len().to_string());
        for prefix in prefixes {
            fingerprint.feed(prefix.join());
        }
    }
}

/// What a unit is compiled for, as an address states it: one word either way, so neither address is a prefix of the other's.
fn feed_compiled(fingerprint: &mut Fingerprint, compiled: Compiled) {
    fingerprint.feed(match compiled {
        Compiled::Built => "built",
        Compiled::Tested => "tested",
    });
}

/// What a unit declared it could see, as an address states it.
fn feed_declared(fingerprint: &mut Fingerprint, declared: Option<&[Qualifier]>) {
    match declared {
        // Two distinguishable shapes rather than one, so "declared nothing" and "declared none" cannot collide: a length-prefixed list can never spell the sentinel.
        None => fingerprint.feed("*"),
        Some(declared) => {
            fingerprint.feed(declared.len().to_string());
            for prefix in declared {
                fingerprint.feed(prefix.join());
            }
        }
    }
}

/// The address the precompiled payload of `package`'s `executable`, built by `compiler` and run by the engine `engine` fingerprints, is filed under.
///
/// **The same address-and-record split [`unit_address`] states, one level up.** This names a place — "the payload for this executable, by this compiler, for this engine" — and carries no file contents and no unit, so an edit changes what the payload is verified against rather than where it is looked for: one payload per executable per compiler per engine, overwritten in place, bounded exactly as the unit family is. What every unit of the compilation was filed as is in the payload's record.
///
/// **The executable's identity is its package's name and its own**, never a path. That pair is the one identity in a compilation that cannot collide (law 2), which is why two executables of one package have two addresses and why nothing has to refuse an umbrella whose members both declare `serve`. A path here would key the payload on where a project sits rather than on what it builds.
///
/// **The engine fingerprint is the part [`unit_address`] has no counterpart for.** A unit is a judgment and travels anywhere the compiler binary does; a payload is machine code, and Cranelift compiled it for the host's ISA — the one input neither the compiler digest nor any recorded source file covers. It rides here as an opaque string because deciding what makes two engines compatible is `curios-native`'s to answer, this crate's only job being to keep the answer in the address.
pub fn payload_address(compiler: &str, package: &str, executable: &str, runs: Runs<'_>) -> String {
    let mut fingerprint = Fingerprint::new();

    fingerprint.feed(PAYLOAD_SCHEMA);
    fingerprint.feed(compiler);
    fingerprint.feed(package);
    fingerprint.feed(executable);
    if let Runs::Native { engine } = runs {
        fingerprint.feed(engine);
    }

    fingerprint.hex()
}
