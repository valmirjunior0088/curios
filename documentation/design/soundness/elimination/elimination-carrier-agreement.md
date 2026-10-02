# Elimination carrier agreement

**Assumes.** A `Cases` form eliminates the carrier its scrutinee's type names.

**Evidence.** Probed at all four forms. A nominal elimination needs its declaration to read at all, the free-monoid rule states the agreement for its three carriers, and the boolean and dispatch forms check it: arms are typed at the case values while the elimination's type is the motive at the scrutinee, so a mismatched form would type its arms at one carrier and run them at another. `kernel::infer::intrinsic_tests::a_boolean_elimination_requires_a_boolean_scrutinee` and `a_dispatch_requires_a_natural_scrutinee` refuse, with `each_intrinsic_elimination_at_its_own_carrier_is_still_accepted` as the control. No surface program reaches it, since the elaborator refuses both spellings; and no derivation of `False` is known through it, because every eliminator goes stuck on a carrier it does not match, leaving motive instances nothing constructs.
