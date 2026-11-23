---
paths:
  - "curios-prelude-build/std/**/*.crs"
  - "programs/**/*.crs"
  - "editors/**/*.crs"
---

# Writing Curios

- Read `documentation/language.md` in full before writing or editing a `.crs` file. `curios-prelude-build/std/` is the reference for idiom and for a standard-library signature.
- Syntax forms are closed: a type opts into an operator or `!` with a `satisfy` witness against the form's `/std` concept, never with syntax of its own. A syntax proposal leads with what is impossible today, not with a shorter spelling of what already works.
- A `/std` module joins the library only through a `mod` line in its parent's header — `lib.crs` for a top-level module, the parent module's own file for a submodule. `curios-module/src/registry.rs` changes only when Rust emits the new name.
- A module housing a type it re-exports is uppercase (`State`, `Flt/Env`); a namespace of functions is lowercase (`Json/encode`, `ops`). No cryptic abbreviations: a name is spelled out, but where the field already spells one way — `cwd`, `stat`, `lstat` — that spelling is kept.
- A monad-shaped `/std` type is nominal — a struct over `State`, its own `Monad` witness, and a `Lift` edge from `State` — never an alias.
- An operation a standard names that an existing function already computes keeps the existing name, with the correspondence in its doc; no second spelling.
- Where a proof needs a trivial case, a relation that holds everywhere or a bound always met, `/std` states a lemma taking the proof, the literal written at the use, not a constant standing for the case.
- `/std`'s library writes the decided-proposition vocabulary through its qualifier — `Bool/Holds(b)`, `Bool/True/qed()`, `Bool/False`, and `Bool/holds(b)` deciding one — with `Bool` imported as any root module is; only `Bool.crs`, which re-exports them, writes them bare. A module of tests or a program imports them by name, `use /std/Bool/{True, False, Holds};`, as `documentation/language.md` teaches. The elaborator's door is `/std`'s own `proved`, imported loose as `print` is and written `proved()`, its proposition pinned by the expectation or given: `proved(@Bool/False)` is the contradiction an arm the facts rule out matches on. A spelling that is itself the subject, as a test of resolution through a facade, keeps what it tests.
- `{}` is the unit type and `()` the unit value; a name's visibility is independent of its representation's.
- An effect of `/std` that takes no argument is a function of none, called `now()`, `args()`, `cwd()`; a value spelled among functions of none leaves a caller to learn which each is.
