# CLI grammars carry their soundness

`/std/Cli` currently describes a flat dependent record. It cannot directly produce a sum representing a path-or-flag choice, and its separate help prescan mistakes argument data for built-in requests. The replacement has one grammar, one ownership scanner and one typed interpreter.

## Settled contracts

- `Grammar(A)` is a transparent struct containing a public typed tree and erased structural soundness evidence about that actual tree. Its supporting types and propositions are public. Soundness covers name uniqueness, valid positional composition, distinguishable alternatives and repetition progress.
- Standard combinators construct preservation proofs. Fixed declarations discharge decidable compatibility premises by reduction; dynamic construction returns either a specification error or the same certified grammar. No axiom or opaque field substitutes for a proof.
- Prepared routing data carries correspondence evidence tied to the grammar. A fixed typed interpreter constructs the result; metadata and a decoder are never independently trusted.
- Mapping, products, exclusive choices, required flags, switches, optional/default values, repeated named leaves and pure validation compose grammars. `Grammar` has no Monad witness. Existing value readers remain useful.
- Support ordered required positionals, fixed positional blocks in distinguishable choices, and one nullable terminal positional suffix (an optional/defaulted fixed block or a rest collector). Positional choices have at most one branch without a required named selector. Nested choices are admitted when flattening preserves this form. Refuse competing nullable suffixes, optional/repeated prefixes and repeated composite groups.
- Explicit occurrences and positional allocation select branches. Defaults and reader success do not. Partial occurrence commits to its group. Reject repeated single-value options by default; an explicit last-value policy and the record adapter retain last-value behavior. Switches are idempotent.
- Scan token ownership once, respecting `--`, attached values and clusters. A value-taking option consumes its next token even when it starts with `-`. Only owned help/version flags are control requests; help precedes version and ordinary input errors, while invalid specifications remain errors.
- Validate the entire command tree before parsing or help, including unselected siblings and active built-in collisions. Specification errors exit 1; invocation errors retain exit 2; help/version succeed with exit 0. Preserve nested dispatch and UTF-8 policy.
- Leaves accept a grammar and a handler over its result type. Checked and fallible record adapters produce `Grammar(Values(args))`. Migrate callers and remove the old scanner and prescan; retain one parsing engine.

## Stages and checks

1. Implement ownership, structural selection, decoding, rendering and multiplicity. Check both/neither/exactly-one inputs, a shared destination, partial groups, invalid selected values, defaults, repetition, clusters, attached values and help used as data.
2. Migrate command trees and callers, remove the old engine, and check deep dispatch, error streams/statuses, UTF-8 rejection, invalid siblings and typed record lookup.
3. Update authoritative documentation, run default-feature workspace checks and retire this specification after redirecting references. No measurement is retaken.
