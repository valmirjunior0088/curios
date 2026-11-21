use {
    super::*,
    curios_common::{Qualifier, RootKind, digest},
};

/// A unit's address is a function of each of its parts, and of nothing else: no unit before its own is in it, so what those hold is verified where the object is opened.
#[test]
fn a_unit_moves_with_each_of_its_parts() {
    let mounts = [Mount::new(Qualifier::from(["json"]), RootKind::Ordinary)];
    let other = [Mount::new(Qualifier::from(["yaml"]), RootKind::Ordinary)];

    let address = unit_address("compiler", &mounts, None, Compiled::Built, None);

    assert_eq!(
        address,
        unit_address("compiler", &mounts, None, Compiled::Built, None),
        "and it is a function"
    );
    assert_ne!(
        address,
        unit_address("another", &mounts, None, Compiled::Built, None),
        "the compiler is in it"
    );
    assert_ne!(
        address,
        unit_address("compiler", &other, None, Compiled::Built, None),
        "the mounts are in it"
    );
    assert_ne!(
        address,
        unit_address("compiler", &mounts, None, Compiled::Tested, None),
        "what the unit is compiled for is in it"
    );
    assert_ne!(
        address,
        unit_address(
            "compiler",
            &mounts,
            None,
            Compiled::Built,
            Some(&[Qualifier::from(["std"])])
        ),
        "and the root a unit of its fold stands in for"
    );
}

/// What a unit declared decides which names resolve in it, so one source over one scope declaring different prefixes is two units rather than one. Three shapes, because "declared nothing" is not "declared none": the first sees every open prefix in scope and the second sees only its own.
#[test]
fn a_units_declared_dependencies_move_it() {
    let mounts = [Mount::new(Qualifier::from(["app"]), RootKind::Ordinary)];
    let json = [Qualifier::from(["json"])];

    let undeclared = unit_address("compiler", &mounts, None, Compiled::Built, None);
    let none = unit_address("compiler", &mounts, Some(&[]), Compiled::Built, None);
    let one = unit_address("compiler", &mounts, Some(&json), Compiled::Built, None);

    assert_ne!(undeclared, none);
    assert_ne!(none, one);
    assert_ne!(undeclared, one);
}

/// What a unit was compiled *from* is verified rather than addressed, so editing its source must leave the address alone — that is what keeps a project's objects as many as its units instead of growing by one per compile, as an address hashed over the unit's tree would.
#[test]
fn a_unit_stays_where_its_source_changes() {
    let mounts = [Mount::new(Qualifier::from(["json"]), RootKind::Ordinary)];

    assert_eq!(
        unit_address("compiler", &mounts, None, Compiled::Built, None),
        unit_address("compiler", &mounts, None, Compiled::Built, None)
    );
    assert_ne!(
        digest(b"one"),
        digest(b"two"),
        "and the digest that does the verifying still separates them"
    );
}

/// Framing, for the reason the tree hash frames its own: without it, moving a boundary between two parts feeds the digest identical bytes.
#[test]
fn a_moved_boundary_is_another_place() {
    let mounts = [Mount::new(Qualifier::from(["c"]), RootKind::Ordinary)];

    assert_ne!(
        unit_address("1", &mounts, None, Compiled::Built, None),
        unit_address("", &mounts, Some(&[]), Compiled::Built, None),
        "a length ahead of each part, so no part's bytes are read as another's"
    );
    assert_ne!(
        payload_address("c", "ab", "c", Runs::Native { engine: "e" }),
        payload_address("c", "a", "bc", Runs::Native { engine: "e" }),
        "and the payload address frames its own parts for the same reason"
    );
}

/// A payload's address is a function of every part — the same argument [`unit_address`]'s makes, with the two this one adds in place of the mounts.
#[test]
fn a_payload_moves_with_each_of_its_parts() {
    let address = payload_address(
        "compiler",
        "app",
        "serve",
        Runs::Native { engine: "engine" },
    );

    assert_eq!(
        address,
        payload_address(
            "compiler",
            "app",
            "serve",
            Runs::Native { engine: "engine" }
        ),
        "and it is a function"
    );
    assert_ne!(
        address,
        payload_address("another", "app", "serve", Runs::Native { engine: "engine" }),
        "the compiler is in it"
    );
}

/// The executable's identity is the pair, so neither half alone decides an address: two executables of one package are two, and one name declared by two packages is two.
#[test]
fn an_executables_identity_is_its_package_and_its_name() {
    let address = payload_address(
        "compiler",
        "app",
        "serve",
        Runs::Native { engine: "engine" },
    );

    assert_ne!(
        address,
        payload_address(
            "compiler",
            "app",
            "worker",
            Runs::Native { engine: "engine" }
        ),
        "two executables of one package have two addresses"
    );
    assert_ne!(
        address,
        payload_address(
            "compiler",
            "other",
            "serve",
            Runs::Native { engine: "engine" }
        ),
        "and two packages declaring one name do too"
    );
}

/// The part the unit family has no counterpart for. A unit is a judgment and travels wherever the compiler binary does; a payload is machine code compiled for one host, so two engines that cannot read each other's artifacts must not look in one place for them.
#[test]
fn two_engines_are_two_places() {
    assert_ne!(
        payload_address("compiler", "app", "serve", Runs::Native { engine: "one" }),
        payload_address(
            "compiler",
            "app",
            "serve",
            Runs::Native { engine: "another" }
        )
    );
}

/// What a payload was compiled *from* is verified rather than addressed, exactly as a unit's source is — which is what bounds the family at one payload per executable rather than one per compile.
#[test]
fn a_payload_stays_where_its_source_changes() {
    assert_eq!(
        payload_address(
            "compiler",
            "app",
            "serve",
            Runs::Native { engine: "engine" }
        ),
        payload_address(
            "compiler",
            "app",
            "serve",
            Runs::Native { engine: "engine" }
        )
    );
}

/// The two families version independently, so an entry written by one layout is never found by the other — which is the whole reason each carries its own schema tag.
#[test]
fn the_two_families_are_tagged_apart() {
    assert_ne!(OBJECT_SCHEMA, PAYLOAD_SCHEMA);
}
