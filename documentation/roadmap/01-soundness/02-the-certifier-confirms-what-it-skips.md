# The certifier confirms what it skips

**Not refined yet.** This specification reserves moving a premise the certifier relies on into the trusted base. It is not an implementation plan.

## What is missing

The certifier judges only what the environment does not already declare, deciding by name ([Judging only what is not in scope](../../design/soundness/admission/judging-only-what-is-not-in-scope.md)). An item arriving under a name already in scope is not judged at all, and what keeps that from happening is the mount discipline: mount sets are pairwise disjoint, checked in `curios-text`'s `into_core` before discovery, and the elaborator's registries refuse a duplicate. Both checks sit outside the trusted base, and nothing in `curios-cert` confirms them.

## Refinement

Whether the kernel refuses a name the environment already declares — a unit whose item collides is then a refusal rather than a skip — or checks the mounts' disjointness itself, and what either costs a compile that meets no collision.
