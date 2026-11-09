//! What the roster's suites lower against: the real `/sys`, and a `/std` of stubs deep enough for every name `/sys` reaches through the registry.

use {
    curios_common::{Qualifier, RootKind},
    curios_core::test_support::*,
    curios_module::{RootSource, SYNTAX},
    curios_surface::Entrypoint,
};

/// The `/std` of [`prelude_fixture`].
const FIXTURE_STD: &str = r#"
    pub mod Str
        pub let Valid : Type = Type;
    end
    pub mod Nat
        pub let Nat : Type = Type;
        pub let add : Type = Type;
    end
    pub use Nat/{let Nat};
    pub mod Bool
        pub let Holds : Type = Type;
        pub use /sys/Bool/{True};
    end
    pub mod Shape
        pub concept Shape(A : Type) : pub Type {
            area(A, A) -> Type,
        }
    end
    mod Hidden
        pub use /sys/{Handle};
    end
"#;

/// [`prelude_fixture`], its `/std` closing with `extra`.
fn prelude_with(extra: &str) -> RootSource {
    let mut modules = RootSource::supplied();
    modules.insert_root("sys", RootKind::Internal, super::sys_module(&SYNTAX));
    modules.insert_root(
        "std",
        RootKind::Ordinary,
        [FIXTURE_STD, extra].concat().parse().unwrap(),
    );
    // `/sys` states each decided precondition as `Holds` over one of its own comparisons, and names `Holds` through the registry — so the scope has to hold whatever this fixture's registry points it at. Stubs, not definitions: these tests lower and never elaborate, so a name that resolves is the whole requirement.
    modules
}

/// The prelude these tests lower against: the real `/sys` roster, and a `/std` of stubs deep enough for every name `/sys` reaches through the registry — with one concept beside them, for the order a unit takes around a row it registers into a concept declared before it. `Nat` is a module and a declaration at its root, as the library's own is, so a fixture that imports it writes the type bare.
fn prelude_fixture() -> RootSource {
    prelude_with("")
}

// Lower against the real prelude (so `sys` and `std` are served and rooted), returning only success/error — the lens for the internal-root gate. A name the lowering filed as resolving to nothing is an error here as one it raised is.
pub fn lower_with_prelude(src: &str) -> Result<(), String> {
    let modules = prelude_fixture();
    let prepared = curios_module::prepare_prelude(&modules, &[], &SYNTAX)
        .map_err(|error| error.to_string())?;
    let lowered = curios_module::into_core_with_prelude(
        &src.parse::<Entrypoint>().unwrap(),
        &RootSource::none().tested(),
        std::slice::from_ref(&&prepared),
        &SYNTAX,
    )
    .map_err(|error| error.to_string())?;

    unresolved(&lowered.minted)
}

/// [`lower_with_prelude`], with the entry seeing only `declared` of what the scope mounts — the lens for the undeclared-dependency refusal.
pub fn lower_declaring(declared: &[&str], src: &str) -> Result<(), String> {
    let modules = prelude_fixture();
    let prepared = curios_module::prepare_prelude(&modules, &[], &SYNTAX)
        .map_err(|error| error.to_string())?;
    let entrypoint = src.parse::<Entrypoint>().unwrap();
    let loader = RootSource::none().tested();

    let unit = curios_module::into_core_unit(
        &curios_module::UnitSource::entry(&entrypoint, &loader).seeing(
            declared
                .iter()
                .map(|prefix| Qualifier::from([*prefix]))
                .collect(),
        ),
        std::slice::from_ref(&&prepared),
        &SYNTAX,
    )
    .map_err(|error| error.to_string())?;

    unresolved(unit.minted())
}

/// The fixture's prelude with `extra` closing its `/std`, lowered as the one unit it is: the lens for what the prelude's own lowering refuses.
pub fn prepare_with(extra: &str) -> Result<(), String> {
    curios_module::prepare_prelude(&prelude_with(extra), &[], &SYNTAX)
        .map(|_| ())
        .map_err(|error| error.to_string())
}
