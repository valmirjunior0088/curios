# Strict positivity, modulo polarity

**Decision.** Every `induct` and `struct` declaration is strictly positive. The check computes a per-parameter polarity vector for each declaration over a five-point lattice (`Unused ⊑ Strict ⊑ Pos ⊑ Mixed`, `Unused ⊑ Neg ⊑ Mixed`, with `Strict` and `Neg` incomparable), builds a relation recording at what polarity each declaration occurs inside each other, closes it under composition, and accepts exactly when no declaration reaches itself through a non-strict path. An occurrence's index arguments are walked at `Mixed`, since a family is not uniform in its indices; a declaration's own index binder types are not walked, since they describe its arity and store nothing, so `Eq(@A : Type) : (x : A, y : A)` is `Strict` in `A` from `refl(@z : A)`'s payload alone. The vectors persist on the registry entries; the analysis is `curios-analysis`'s, run post-zonk beside `validate_universes`, where telescopes are final and meta-free, with each checker supplying its own driver.

**Rationale.**

- **An `induct` claims its functor has an initial algebra**, and its eliminator is the statement that the algebra is initial. Strict positivity is the syntactic condition for that functor being polynomial, hence for the algebra existing, and it is the construction that yields the eliminator. Without it `induct Bad | c(f : (Bad) -> False) end` inhabits `False` with no recursion at all.
- **Polarity is what admits the standard library.** Most parameterized declarations are not covariant — `Show(A)` carries `(A) -> Str` — while `/std/Toml` recurses through `Map(Toml)` and `Option(Node(V))` and `/std/Json` through `List` and a tuple. A check that recognizes recursion only in immediate payloads rejects `Toml`, `Json` and `Map`. Every functor polarity admits was polynomial already; the checker only recognizes it through abstraction.
- **Merely positive occurrences stay out**, because Cantor with an impredicative `Prop` gives the Coquand–Paulin construction (`tests::positivity::refusal_tests::a_positive_but_not_strictly_positive_occurrence_is_rejected`).
- **A declaration cannot reach itself through its own index binder type**: `induct Foo : (x : Foo) -> Type` fails kinding before positivity runs, so walking those types would cost precision for nothing — `Eq` would come out `Mixed` and refuse the sound `induct Wit | base() | tied(a : Wit, b : Wit, p : Eq()(a, b)) end`.

**Rejected.**

- **Running the check during elaboration**, where it would guess what an unsolved metavariable becomes.
- **Treating `Mixed` as a stop rather than a result**, which lets an occurrence hidden in a stuck `match` arm pass unseen.
- **A provenance tag on `Mixed`** for diagnostics, which costs `Polarity` its derived equality to say what the reported site already says.
- **Surface syntax for polarity**, such as Agda's `@++`: the property is computed and the obligation inferred.
