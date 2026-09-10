//! Where a compilation's names come from: the prefixes units mount, and what each prefix may reach.
//!
//! A compilation is a set of units, and each unit claims one or more *prefixes*. The fixed prelude is two units claiming one each — `/sys` and `/std` — a package claims one, and the entry program claims the empty prefix, which is what makes it the entry. [`Mount`] pairs a prefix with whether it is a root only the compiler supplies.
//!
//! The name *is* the identity: which mount owns a declaration is [`Mount::owning`] over the name against the table of what is mounted, and the only thing carried is the mount list itself, one per module rather than one per declaration. Why a prefix and not an identity beside it is `README.md`'s decision.

#[cfg(test)]
mod tests;

use crate::Qualifier;

/// One prefix a unit claims, and whether it is a root only the compiler supplies.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Mount {
    /// The prefix every name this mount owns lies within. The empty qualifier is the entry program's, and at most one mount may claim it.
    pub prefix: Qualifier,
    pub kind: RootKind,
}

impl Mount {
    pub fn new(prefix: Qualifier, kind: RootKind) -> Self {
        Self { prefix, kind }
    }

    /// The mount owning `name`: the most specific prefix `name` lies within, or `None` when nothing mounted claims it.
    ///
    /// *Most specific* is load-bearing rather than a tie-break. The entry program mounts the empty prefix and every qualifier lies within that, so a `/std/Option` answered by the first match would come back ordinary. Mounts are pairwise disjoint, so no two of equal depth can both contain one name and there is no tie left to break.
    ///
    /// `None` is a real answer, not a missing one: while the fixed prelude is prepared, only `/sys` and `/std` are mounted and the empty qualifier is the synthetic compilation root, owned by no unit.
    pub fn owning<'a>(mounts: &'a [Mount], name: &Qualifier) -> Option<&'a Mount> {
        mounts
            .iter()
            .filter(|mount| name.is_within(&mount.prefix))
            .max_by_key(|mount| mount.prefix.segments().len())
    }
}

/// Whether a root is one the compiler supplies rather than one a manifest can name.
///
/// **This was a privilege tier and is not one any more.** Three variants once answered "may this root reference that one", with `/std` ranked above `/sys` so the standard library could reach the intrinsics. That question is a unit's declared dependencies now — `/std` reaches `/sys` because it is the one unit that declares it — so what survives is the single fact a dependency list cannot state: a root with no path is one no manifest can name, so it is in no unit's *default* set and a reader who writes it is told to use the facade rather than told to declare it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum RootKind {
    /// A root the compiler supplies — `sys` alone today. Mounted by every compilation and named by no manifest: discoverable, so the unit that declares it resolves it by absolute path, and refused to every unit that did not.
    ///
    /// **The reason is interface stability, not safety.** `/sys` is the compiler's own vocabulary rather than anybody's interface: it is generated from `curios-abi`'s foreign store, so its roster, its argument order, and the shape of every host row move when the ABI moves, and the intrinsic carriers beside them move with `Intrinsic::signature`. A consumer that reached past the `/std` facade would be pinned to a surface with no compatibility promise. What the surface forms desugar *into* is no longer behind this gate: those concepts are ordinary `/std` declarations the compiler reaches through the registry as already-resolved identities, which never pass it. That is the whole of what this tier buys, and it is worth buying.
    ///
    /// It is worth stating because the tier was long assumed to be a soundness mechanism, and it never was one. It grants trust to whole *roots*, so `/std/Map` and `/std/Bytes` are indistinguishable to it — which is why the bypasses that motivated giving `/sys`'s operations their preconditions were all *inside* the roots this tier authorizes, one of them inside the very module any conceivable reach rule would have allowed. Nothing behind the gate is a hazard now that those operations carry their domains in their types, and nothing behind it was a hazard the gate itself was catching. It does not constrain what `/sys` *exports* either, so the one premise that does depend on `/sys`'s surface — that `/sys/Io` offers no eliminator — is asserted where that roster is built and not here.
    Internal,
    /// A root a manifest names — the standard library, the entry program, and every package.
    Ordinary,
}
