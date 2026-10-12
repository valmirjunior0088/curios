# A literal is realized by its expected type

**Decision.** A literal is an ordinary term the elaborator realizes against its expected type, and every value of a carrier has one literal spelling.

- **A numeral** realizes at `Nat`, `Int`, `Byte`, `Flt` or `Bool` under its range rules, `Byte` and `Bool` only where expected.
- **A character literal is a numeral spelled by its scalar**: `Transient::NumLit`, whose `Character` variant carries a `char` beside `Number`'s magnitude and three-valued `Sign`. It realizes as the certified `/std/Char` wherever nothing pins it and as its code point at `Nat`, `Byte` or `Int`, never at `Bool` or `Flt`. In a match it is a `Nat` dispatch case, and a `Char` scrutinee dispatches through `Char/to_nat`.
- **A string literal** is a transparent, certified `/std/Str` value; the erased carriers stay `Nat` and packed `Bytes`.
- **A `Bits` or `Bytes` literal** is bracketed, selected by a grain letter glued to the bracket: `b[1, flag, ..rest]`, `x[0x48, ..suffix]`, `b[]`, `x[]`, and the fold arm `b[head, ..tail]; ih`. Each entry is an ordinary term contributing one atom, a constant atom is a numeral realized at the grain's element type, `..` spreads a packed value, and adjacent constant atoms lower to one packed constant (`lower_bin_literal`, pinned by `constant_atoms_fold_into_the_packed_run`).
- **A literal whose type is still open waits for the term that awaits it.** A numeral checked at a type stuck on an unsolved metavariable that a term awaits — a call, a list or a structure literal, a match, an operator reading its parts — is realized where a solution gives the type, and otherwise where that term has read every part: the numerals that wait on one type take one default there, `Int` where one of them is signed, `Nat` where one is an unsigned number, and `Char` only where every one is a character. A tuple literal that waits gives the type its shape there, a product of one open type a field, at which every other literal waiting on it is checked. Where no term awaits the type the default is taken at once.
- **A float's non-finite values** are `Flt` literals with a required sign: `+inf.0` and `-inf.0`, and `+nan.0` and `-nan.0`, the default quiet NaN with its sign clear and set. Every other NaN is spelled by the call that builds it, `Flt/of_le_bytes(x[…])`, and a decimal too large for `Flt` is refused, never rounded to an infinity.

**Rationale.**

- **The kernel stays free of literal types**, and a literal arrives carrying the structure and certificates library code consumes, which erasure makes free at run time.
- **One mechanism realizes every literal**: `elaborate_num_lit`'s candidate table with a shape default, the `switch` the match compiler emits for `Nat` dispatch, and the certified-value emitter. An enum rather than a flag makes a signed, radixed or out-of-range character unrepresentable.
- **A default is no fact about the order a term is written in.** Taken where the numeral is read, `g(5, b)` defaults the numeral to `Nat` and refuses a `b` of another type, where `g(b, 5)` compiles; taken once the call has read both, the argument that states the type gives it in either order, and two numerals that must share a type are defaulted as one.
- **Every value reads back.** A report prints what its reader can write ([A diagnostic spells what its reader can write](../tools/a-diagnostic-spells-what-its-reader-can-write.md)), so a carrier's literals cover its values, and reading back is a property of the grammar rather than of library names a printer must know. The non-finite spelling is [R7RS Scheme's](https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-12.html); unlike R7RS, `-nan.0` is its own value, since term identity is bitwise and a NaN's sign is observable. An overflowing decimal is far more often a mistake than a request for an infinity.
- **Delimiters make an entry an ordinary term**, printed by the ordinary printer, and the three bracketed carriers share one grammar differing in their prefix. A radix is presentation the writer chooses per site — `0x48` where bytes read hex, `1` where bits read binary.

**Rejected.**

- **Kernel-intrinsic character and string types.**
- **A default held to the end of the declaration**, as Lean's and Rust's are: a value stands in the types after it here, and whatever reads the type before then, a match, a projection, a call, would have to wait in its turn. **The first of two literals at one type synthesized and the second held to it**, which answers a call's literals in the order of its slots.
- **A `character: bool` beside the numeral's fields**, which admits the bogus pairings.
- **Matching a `Char` scrutinee by projecting its `code` field in the match compiler**, which would teach pattern compilation a library representation and hide a projection `Char/to_nat` states in the source.
- **Character spellings for `Bool` and `Flt`**, and **equality-guard or `Str` patterns**, which `choose` with `==` already owns.
- **A tight packed spelling**, `b\1\..rest\0`: with no delimiters, operands need a glued grammar and a printer of their own, and whitespace ends the literal.
- **An escape marking constant atoms beside numerals**, two spellings of one constant whose constancy lowering already guarantees; **bare `[` for every carrier**, which leaves an empty literal's carrier unrecoverable. The price accepted is that postfix `[` is spent.
- **Named float constants**, as OCaml's `infinity` and Julia's `Inf`, which make each value two spellings and reading back rest on names; **printing a computation such as `Flt/div(+1.0, +0.0)`**, which shows a division where the reader has a value.
