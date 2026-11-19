//! The lint erasure decides: `startup-value`, a value every program its unit is in scope of runs as it starts.
//!
//! **What it reports is the fact a program is linked by.** An item whose initialization is observable ([`ErasedForms::observable`](crate::ErasedForms)) is a computation not known to end that its unit's compile could not finish, which every program runs before its entry whether or not it names the item; one that is not is left out by every program that does not read it, and is not reported. The decision is `documentation/design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md`.
//!
//! **It stands at the declaration's name**, which the unit's anchors place, as erasure's refusals are placed: a witness's value carries no extent of its own, and a name was written for every declaration.

use curios_common::{Qualifier, Said};

/// What `startup-value` says of the item declaring `name`, a witness where `witness`, placed `at`.
pub fn startup_value(
    name: &curios_core::Global,
    witness: bool,
    at: Option<curios_core::Extent>,
) -> curios_core::Lint {
    let mut said = Said::line("value computed at every start");
    let total = "give what it computes a definition the certifier judges total, which a program leaves out where nothing reads it";
    match witness {
        true => {
            said.stating("this witness is computed as each program in its unit's scope starts, whether or not the program reads it: what it computes is not known to end");
            said.stating(total);
        }
        false => {
            said.stating(format_args!(
                "`{}` is computed as each program in its unit's scope starts, whether or not the program reads it: what it computes is not known to end",
                name.qualifier().map_or("", Qualifier::last)
            ));
            said.stating(format_args!(
                "make it a function of none, which runs where it is called, or {total}"
            ));
        }
    }

    curios_core::Lint {
        kind: curios_core::LintKind::StartupValue,
        said,
        at,
    }
}
