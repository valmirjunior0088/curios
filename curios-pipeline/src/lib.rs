use {curios_abi::sys_io, std::time::Duration};

pub enum Stage<'a> {
    Text(&'a curios_text::Entrypoint),
    Core(&'a curios_core::Module),
    Ersd(&'a curios_ersd::Module),
    ErsdOptm(&'a curios_ersd::Module),
    Cont(&'a curios_cont::Module),
    ContOptm(&'a curios_cont::Module),
    Wasm(&'a curios_wasm::Module),
}

thread_local! {
    // The fixed `sys`/`syn`/`std` prelude, elaborated and zonked once per thread
    // and reused for every compile. Since the reachability prune no longer runs
    // in `curios_text::to_core`, every program lowers the *same* prelude prefix, so its
    // (expensive) type-checking is program-independent and cacheable — the whole
    // point of removing the prune. `Term` is not `Sync`, so this is thread-local
    // (like the loader's parse caches), built once per `cargo test` worker thread
    // rather than once per test. A malformed prelude is a compiler invariant, so
    // a failure here is a `panic!`.
    static PRELUDE: curios_core::Module = build_prelude();
}

/// Elaborate the bare `sys`/`syn`/`std` prelude (no user code) once, to seed
/// [`PRELUDE`]. Lowers a trivial entrypoint under the standard prelude loader —
/// the embedded prelude is identical regardless of any inner loader — then
/// elaborates and zonks it. The result is an ordinary (meta-free) `curios_core::Module`
/// whose `items` are the whole prelude; its trivial `body`/`type_` go unused.
/// A generous timeout: this is a one-time, per-thread cost amortized across
/// every compile.
fn build_prelude() -> curios_core::Module {
    let entrypoint = "0"
        .parse::<curios_text::Entrypoint>()
        .expect("the trivial prelude entrypoint parses");

    let (module, metavars) = curios_text::to_core(
        &entrypoint,
        &curios_text::prelude(&sys_io(), curios_text::NullLoader),
    )
    .unwrap_or_else(|error| panic!("the embedded prelude failed to lower: {}", error.format()));

    let mut context = curios_core::Context::new(Duration::from_secs(300));

    let (module, _) =
        curios_core::elaborate_module(&mut context, &module, metavars, curios_core::Mode::Infer)
            .unwrap_or_else(|error| {
                panic!(
                    "the embedded prelude failed to elaborate: {}",
                    error.format_with(&module)
                )
            });

    curios_core::zonk_module(&context, &module).unwrap_or_else(|error| {
        panic!(
            "the embedded prelude failed to zonk: {}",
            error.format_with(&module)
        )
    })
}

/// The type-checking prologue shared by [`compile_entrypoint`] and
/// [`typecheck_entrypoint`]: lower to core, elaborate (checking against the
/// entrypoint's type when it carries one, else synthesizing), then zonk
/// metavariable solutions in so the module is meta-free — the `elaborate → zonk`
/// half of the `elaborate → zonk → erase` data flow (§9). Elaboration is
/// authoritative: it returns a rebuilt module (lambda domains solved, binders
/// re-closed), and it is *that* module — not the lowered one — that zonk makes
/// meta-free. `zonk` is also where an unsolved hole is rejected, so a program that
/// merely *type-checks* is fully validated by the time this returns. Elaboration
/// and zonking share one context (the solutions live in its `MetaStore`); the
/// returned module is self-contained, so the caller's `erase` runs over a fresh one.
///
/// The `sys`/`syn`/`std` prelude is not re-elaborated per call: the cached
/// [`PRELUDE`] is replayed into the fresh context and only the user code is
/// type-checked on top (see [`curios_core::elaborate_and_zonk_with_prelude`]).
fn elaborate_and_zonk<O>(
    timeout: Duration,
    entrypoint: &curios_text::Entrypoint,
    loader: &dyn curios_text::Loader,
    observe: &mut O,
) -> Result<(curios_core::Module, curios_core::Term), String>
where
    O: FnMut(Stage<'_>),
{
    observe(Stage::Text(entrypoint));

    // The compilation's foreign store — today exactly the `/sys/Io` builtin
    // set. The prelude mints the `/sys/Io` declarations from it; the rows ride
    // the IR nodes from there, so no later stage needs the store itself.
    let foreigns = sys_io();

    let (lowered, metavars) =
        curios_text::to_core(entrypoint, &curios_text::prelude(&foreigns, loader))
            .map_err(|error| error.format())?;

    observe(Stage::Core(&lowered));

    let core_mode = match &lowered.type_ {
        Some(type_) => curios_core::Mode::Check(type_.clone()),
        None => curios_core::Mode::Infer,
    };

    // Reuse the cached, pre-elaborated prelude: only the user items and the
    // entrypoint body are type-checked here, not the whole `sys`/`syn`/`std`
    // prelude (the bulk of the work). See `curios_core::elaborate_and_zonk_with_prelude`.
    // The timed context is created *inside* `with` so the one-time, per-thread
    // prelude build (which `with` triggers on first access) does not count
    // against the caller's `timeout` — the deadline bounds only the user work.
    let (module, core_type) = PRELUDE
        .with(|prelude| {
            let mut context = curios_core::Context::new(timeout);

            curios_core::elaborate_and_zonk_with_prelude(
                &mut context,
                prelude,
                &lowered,
                metavars,
                core_mode,
            )
        })
        .map_err(|error| error.format_with(&lowered))?;

    Ok((module, core_type))
}

/// Type-check an entrypoint and stop — the fast path for `check`. Runs only
/// `to_core → elaborate → zonk` (so it observes `Text` and `Core` only), skipping
/// the `erase → cont → optm → wasm` lowering that a type-check does not need.
pub fn typecheck_entrypoint<O>(
    timeout: Duration,
    entrypoint: &curios_text::Entrypoint,
    loader: &dyn curios_text::Loader,
    mut observe: O,
) -> Result<(), String>
where
    O: FnMut(Stage<'_>),
{
    elaborate_and_zonk(timeout, entrypoint, loader, &mut observe)?;

    Ok(())
}

pub fn compile_entrypoint<O>(
    timeout: Duration,
    entrypoint: &curios_text::Entrypoint,
    loader: &dyn curios_text::Loader,
    mut observe: O,
) -> Result<curios_wasm::Module, String>
where
    O: FnMut(Stage<'_>),
{
    let (module, core_type) = elaborate_and_zonk(timeout, entrypoint, loader, &mut observe)?;

    // The whole elaborated module (the entire `sys`/`syn`/`std` prelude included)
    // is erased: there is no source-level prune, so std is type-checked in full on
    // every compile and its bugs surface instead of hiding behind an unreached
    // path.
    let ersd_module =
        curios_core::erase_module(&mut curios_core::Context::new(timeout), &module, &core_type)
            .map_err(|error| error.format_with(&module))?;

    observe(Stage::Ersd(&ersd_module));

    // *After* erase has type-checked everything, run the Ersd optimization
    // pipeline in place: drop the items the entrypoint cannot reach, then re-base
    // self-recursion onto accumulators and offsets (see `curios_ersd::optm`).
    let mut ersd_optm_module = ersd_module;
    curios_ersd::optm::optimize(&mut ersd_optm_module);

    observe(Stage::ErsdOptm(&ersd_optm_module));

    let cont_module = curios_ersd::to_cont(&ersd_optm_module);

    observe(Stage::Cont(&cont_module));

    // Run the Cont optimization pipeline in place (see `curios_cont::optm`).
    let mut cont_optm_module = cont_module;
    curios_cont::optm::optimize(&mut cont_optm_module);

    observe(Stage::ContOptm(&cont_optm_module));

    let wasm_module = curios_cont::to_wasm(&cont_optm_module);

    observe(Stage::Wasm(&wasm_module));

    Ok(wasm_module)
}

#[cfg(test)]
mod tests {
    use {super::*, std::time::Duration};

    #[test]
    fn entrypoint_type_is_used_as_expected_type() {
        let entrypoint = "0"
            .parse::<curios_text::Entrypoint>()
            .unwrap()
            .with_type("/std/Bln".parse().unwrap());

        let error = compile_entrypoint(
            Duration::from_secs(5),
            &entrypoint,
            &curios_text::NullLoader,
            |_| {},
        )
        .unwrap_err();

        assert!(error.contains("type mismatch"));
    }

    fn compile(source: &str, type_: Option<&str>) -> Result<curios_wasm::Module, String> {
        let entrypoint = source.parse::<curios_text::Entrypoint>().unwrap();

        let entrypoint = match type_ {
            Some(type_) => entrypoint.with_type(type_.parse().unwrap()),
            None => entrypoint,
        };

        compile_entrypoint(
            Duration::from_secs(5),
            &entrypoint,
            &curios_text::NullLoader,
            |_| {},
        )
    }

    fn compile_printed_stages(source: &str) -> Result<(String, String), String> {
        let entrypoint = source.parse::<curios_text::Entrypoint>().unwrap();
        let mut ersd = String::new();
        let mut cont = String::new();

        compile_entrypoint(
            Duration::from_secs(5),
            &entrypoint,
            &curios_text::NullLoader,
            |stage| match stage {
                Stage::Ersd(stage) => ersd = format!("{stage}"),
                Stage::Cont(stage) => cont = format!("{stage}"),
                _ => {}
            },
        )?;

        Ok((ersd, cont))
    }

    #[test]
    fn meta_free_prelude_program_compiles_without_overflow() {
        // The exact case BUG.md calls out: a meta-free entrypoint (no holes) that
        // still pulls in the whole std/std prelude. Assembling and traversing the
        // old N-deep nested term overflowed the stack during construction and in
        // every pass; the flat `curios_core::Module`/`curios_ersd::Module` representation lowers
        // it end-to-end to wasm without overflow.
        let source = r#"
            let id(A : Type, a : A) -> A = a;
            id(/std/Nat, 5)
        "#;

        assert!(compile(source, None).is_ok());
    }

    #[test]
    fn hole_in_a_type_argument_is_inferred_and_lowers() {
        // `id ? 5`: the type argument `?` is solved to `Nat` from the value `5`,
        // synthesizing the whole program end-to-end through to wasm (§14, `id ? x`).
        let source = r#"
            let id(A : Type, a : A) -> A = a;
            id(?, 5)
        "#;

        assert!(compile(source, None).is_ok());
    }

    #[test]
    fn hole_pinned_through_the_expected_type_is_solved() {
        // `id ? true` checked against `/std/Bln`: the turnaround pins the type
        // argument `?` to `Bln` through the expected type (§14, a type-level pin).
        let source = r#"
            use /std/{Bln};
            let id(A : Type, a : A) -> A = a;
            id(?, true)
        "#;

        assert!(compile(source, Some("/std/Bln")).is_ok());
    }

    #[test]
    fn unconstrained_value_hole_cannot_be_inferred() {
        // `let m : Nat = ? in m`: nothing constrains the value of `?`, so the
        // metavariable is unsolved at zonk and compilation fails (§14).
        let source = r#"
            use /std/{Nat};
            let m : Nat = ?;
            m
        "#;

        let error = compile(source, Some("/std/Nat")).unwrap_err();

        assert!(error.contains("cannot"), "unexpected error: {error}");
    }

    #[test]
    fn omitted_motive_mentioning_a_type_param_lowers() {
        // `pick` is polymorphic in `A`, and the `match c` omits its motive. The
        // motive metavar is solved to `A` — a binder local to `pick`'s telescope.
        // zonk must realign that solution to the enclosing binders when it splices
        // it back in; otherwise `A` dangles as a free var after the module is
        // re-closed and `erase` rejects it with `unbound variable`. Guards the
        // zonk binder-realignment fix.
        let source = r#"
            use /std/{Bln};
            let pick(A : Type, a : A, b : A, c : Bln) -> A =
                match c
                | false => a
                | true => b
                end;
            pick(/std/Nat, 1, 2, true)
        "#;

        assert!(compile(source, None).is_ok());
    }

    #[test]
    fn projection_through_a_stuck_inductive_payload_lowers() {
        // `Fmt/printf`'s return type is `format_type_with({}, parse(s))`, so
        // erasing `printf` evaluates `parse(s)` at compile time with a *symbolic*
        // `s`. The `Parse` combinator's result is a `Result` inductive whose
        // discriminant is therefore stuck, and the inlined `success` payload is
        // reached by a projection. `erase` must lower that projection through the
        // neutral payload `match` (every variant carries the field at the same
        // index) instead of demanding a literal `TupleType`. Guards
        // `projectable_at`; without it this panics `erase: projected a non-tuple`.
        let source = r#"
            use /std/{Fmt, Bin};
            Fmt/printf("%s is %d")
        "#;

        assert!(compile(source, None).is_ok());
    }

    #[test]
    fn checked_constructor_postpones_a_tuple_under_a_holed_type_arg() {
        // `Result/success((a, a))` checked against a known `Result(...)`. The
        // tuple is an introduction form whose parameter type is the inserted
        // implicit `?A`, so it can't be checked until `?A` is known. Elaboration postpones it,
        // unifies the result against the expected `Result` — solving `?A` (the
        // success type, which the tuple's own result witnesses) and the *phantom*
        // `?E` (the failure type, carried only by the expected type) — then re-checks
        // the tuple. Guards the result-directed argument order in `elaborate_apply`;
        // without it this fails "introduced a tuple where the expected type is not a
        // tuple type".
        let source = r#"
            use /std/{Result};
            use /std/{Nat};
            let f(a : Nat) -> Result({ Nat, Nat }, Nat) =
                Result/success((a, a));
            f(7)
        "#;

        assert!(compile(source, None).is_ok());

        // In infer position nothing pins the holes, so the postponed tuple is
        // re-checked against a still-unsolved metavar and rejected — graceful
        // degradation, no new acceptance of un-annotated constructors.
        let unpinned = r#"
            use /std/{Result};
            Result/success((1, 1))
        "#;

        assert!(compile(unpinned, None).is_err());
    }

    #[test]
    fn inductive_match_arm_arity_is_checked_statically() {
        // Each arm's binder count is checked against the
        // constructor's registry telescope at elaboration time. Under the
        // legacy tagged-tuple desugar this mismatch was silent (the extra
        // binder became an out-of-range payload projection).
        let source = r#"
            use /std/{Result};
            use /std/{Nat, Bin};
            let f(r : Result(Nat, Bin)) -> Nat =
                match r : Nat
                | success(value, extra) => value
                | failure(_) => 0
                end;
            f(Result/success(7))
        "#;

        let error = compile(source, None).unwrap_err();

        assert!(
            error.contains("constructor 'success' takes 1 argument(s) but the match arm binds 2"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn implicit_arguments_can_all_be_supplied_explicitly() {
        // Every implicit slot can be overridden positionally with a call-site
        // `@` — including an inductive constructor's parameters, which are implicit
        // by default — and the fully-supplied call compiles end-to-end.
        let source = r#"
            use /std/{Nat};
            induct Opt(A : Type) : Type
            | some(A)
            | none()
            end
            let id(@T : Type, x : T) -> T = x;
            match Opt/some(@Nat, id(@Nat, 1)) : Nat
            | some(value) => value
            | none() => 0
            end
        "#;

        compile(source, None).unwrap();
    }

    #[test]
    fn implicit_argument_is_inserted_and_inferred() {
        // The INDUCTIVES.md promise realized: an `@`-marked inductive parameter
        // makes the constructor's type argument implicit, so the call site
        // writes no holes at all.
        let source = r#"
            use /std/{Nat};
            induct Opt(A : Type) : Type
            | some(A)
            | none()
            end
            match Opt/some(1) : Nat
            | some(value) => value
            | none() => 0
            end
        "#;

        compile(source, None).unwrap();
    }

    #[test]
    fn interleaved_implicit_with_partial_override() {
        // `T` is overridden positionally with `@`, `U` (interleaved after an
        // explicit binder) is inferred from `y`.
        let source = r#"
            use /std/{Nat, Bin};
            let second(@T : Type, x : T, @U : Type, y : U) -> U = y;
            std/Bin/len(second(@Nat, 1, /std/Str/to_bin("abc")))
        "#;

        compile(source, None).unwrap();
    }

    #[test]
    fn implicit_argument_queues_are_order_insensitive() {
        // The two queues are matched independently: an `@`-argument fills the
        // first unfilled implicit binder no matter where it sits among the
        // plain arguments.
        let at_first = r#"
            use /std/{Nat, Bin};
            let second(@T : Type, x : T, @U : Type, y : U) -> U = y;
            std/Bin/len(second(@Nat, 1, /std/Str/to_bin("abc")))
        "#;
        let at_last = r#"
            use /std/{Nat, Bin};
            let second(@T : Type, x : T, @U : Type, y : U) -> U = y;
            std/Bin/len(second(1, /std/Str/to_bin("abc"), @Nat))
        "#;

        compile(at_first, None).unwrap();
        compile(at_last, None).unwrap();
    }

    #[test]
    fn trailing_implicit_is_pinned_by_the_expected_type() {
        // The proof-argument shape: the implicit trails every explicit binder
        // and is mentioned only in the result type, so nothing but the
        // result-directed turnaround can pin it.
        let source = r#"
            use /std/{Nat};
            induct Opt(A : Type) : Type
            | some(A)
            | none()
            end
            let nothing(n : Nat, @T : Type) -> Opt(T) = Opt/none(@T);
            let r : Opt(Nat) = nothing(0);
            match r : Nat
            | some(value) => value
            | none() => 9
            end
        "#;

        compile(source, None).unwrap();
    }

    #[test]
    fn all_implicit_telescope_saturates_and_retargets() {
        // The curried `bind` shape: `(@A, @B) -> (M A, A -> M B) -> M B`.
        // Applying it directly to plain arguments saturates the all-implicit
        // telescope with fresh metavariables and re-targets the arguments at
        // the next telescope — both through a direct call and the `!` sugar
        // (which sequences through the user's `Monad(Id)` witness).
        let source = r#"
            use /std/{Nat, Monad};
            induct Id(A : Type) : Type
            | wrap(A)
            end
            let bind : (@A : Type, @B : Type) -> (Id(A), (A) -> Id(B)) -> Id(B) =
                (A, B) => (m, f) =>
                    match m : Id(B)
                    | wrap(x) => f(x)
                    end;
            witness : Monad(Id) {
                pure(A, x) = Id/wrap(x),
                bind(A, B, m, f) = bind(@A, @B)(m, f)
            }
            let direct = bind(Id/wrap(1), (x) => Id/wrap(Nat/succ(x)));
            -- The lambda body is its own region root: the `!` sequences inside
            -- it instead of hoisting into the entrypoint tail (which returns a
            -- bare `Nat`, not an `Id`).
            let sugared_block = () =>
                let v = Id/wrap(3)!;
                Id/wrap(v);
            let sugared = sugared_block();
            match sugared : Nat
            | wrap(value) =>
                match direct : Nat
                | wrap(other) => Nat/add(value, other)
                end
            end
        "#;

        compile(source, None).unwrap();
    }

    #[test]
    fn uninferred_implicit_names_the_binder_and_function() {
        // Nothing mentions `T` outside the binder itself, so unification can
        // never pin it; the report must name the hole, not a bare metavar id.
        let source = r#"
            use /std/{Nat};
            let cast(x : Nat, @T : Type) -> Nat = x;
            cast(5)
        "#;

        let error = compile(source, None).unwrap_err();

        assert!(
            error.contains("implicit argument 'T' of '/cast' was not inferred"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn surplus_implicit_arguments_are_rejected() {
        let source = r#"
            use /std/{Nat};
            let id(@T : Type, x : T) -> T = x;
            id(@Nat, @Nat, 1)
        "#;

        let error = compile(source, None).unwrap_err();

        assert!(
            error.contains("2 '@' argument(s) but the function has only 1 implicit parameter(s)"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn non_pub_inductive_constructors_are_usable_in_the_declaring_module() {
        // Constructors are exactly as visible as their inductive: a non-`pub`
        // inductive is module-local but fully usable where it is declared.
        let source = r#"
            use /std/{Nat};
            induct Opt : Type
            | none()
            | some(Nat)
            end
            match Opt/some(7) : Nat
            | none() => 0
            | some(n) => n
            end
        "#;

        assert!(compile(source, None).is_ok());
    }

    #[test]
    fn non_pub_inductive_constructors_stay_private_across_modules() {
        // The same inductive declared inside a submodule is not reachable from
        // the parent: the inductive's own visibility still gates the outside.
        let source = r#"
            pub mod m
                induct Secret : Type
                | hide(/std/Nat)
                end
            end
            m/Secret/hide(7)
        "#;

        assert!(compile(source, None).is_err());
    }

    #[test]
    fn inductive_match_on_a_non_inductive_scrutinee_is_rejected_directly() {
        // With the legacy fallback gone, matching inductive
        // constructors on a non-inductive value reports the real problem instead
        // of a downstream projection error.
        let source = r#"
            use /std/{Nat};
            match 7 : Nat
            | success(value) => value
            end
        "#;

        let error = compile(source, None).unwrap_err();

        assert!(
            error.contains("matched inductive constructors on a non-inductive type"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn new_style_inductive_match_lowers_end_to_end() {
        // The same program with correct arities compiles through to wasm: the
        // `Result` declaration takes the primitive-inductive path (InductiveType /
        // Variant / InductiveMatch) and erases back to the legacy tagged-tuple
        // runtime shape.
        let source = r#"
            use /std/{Result};
            use /std/{Nat, Bin};
            let f(r : Result(Nat, Bin)) -> Nat =
                match r : Nat
                | success(value) => value
                | failure(_) => 0
                end;
            f(Result/success(7))
        "#;

        assert!(compile(source, None).is_ok());
    }

    #[test]
    fn indexed_inductive_declares_constructs_and_matches() {
        // Phase 1 of INDICES.md, end to end: an indexed `Vec` declares (head
        // index telescope, named/`@` payload binders, per-case targets),
        // constructs with `@T`/`@m` inferred — `Nat/succ(?m)` unifies against the
        // annotation's `2` — and matches under a constant motive (Rung 0:
        // arms are typed from the constructor telescopes; indices ride
        // along), lowering through to wasm.
        let source = r#"
            use /std/{Nat};
            induct Vec(T : Type) : (n : Nat) -> Type
            | nil() : (0)
            | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
            end
            rec len(@T : Type, @n : Nat, v : Vec(T, n)) -> Nat =
                match v : Nat
                | nil() => 0
                | cons(m, x, xs) => Nat/add(len(xs), 1)
                end;
            let v : Vec(Nat, 2) = Vec/cons(10, Vec/cons(20, Vec/nil()));
            len(v)
        "#;

        assert!(compile(source, None).is_ok());
    }

    #[test]
    fn indexed_inductive_without_params_and_unnamed_index_lowers() {
        // The head's index names are optional (`: (Nat)`), and an inductive can be
        // indexed without being parameterized. Targets are arbitrary index
        // expressions — here distinct literals — and conversion compares them
        // pointwise: `Tag(7)` accepts `Tag/b` and the match dispatches on the
        // tag as ever.
        let source = r#"
            use /std/{Nat, Bin};
            induct Tag : (Nat) -> Type
            | a() : (0)
            | b() : (7)
            end
            let t : Tag(7) = Tag/b();
            match t : Bin
            | a() => /std/Str/to_bin("a")
            | b() => /std/Str/to_bin("b")
            end
        "#;

        assert!(compile(source, None).is_ok());
    }

    #[test]
    fn indexed_inductive_motive_binds_the_index() {
        // Rung A: the annotated motive `(v : Vec(T, k)) => Vec(T, Nat/add(k, m))`
        // binds the length index in its natural slot; each arm checks against
        // the motive at that case's target index (`0` for nil, `Nat/succ(j)` for
        // cons), and the whole match at the scrutinee's actual index. The cons
        // arm converges via `Nat/add`'s definitional successor peeling.
        let source = r#"
            use /std/{Nat};
            induct Vec(T : Type) : (n : Nat) -> Type
            | nil() : (0)
            | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
            end
            rec append(@T : Type, @n : Nat, @m : Nat, v : Vec(T, n), w : Vec(T, m)) -> Vec(T, Nat/add(n, m)) =
                match v : (v : Vec(T, k)) => Vec(T, Nat/add(k, m))
                | nil() => w
                | cons(j, x, xs) => Vec/cons(x, append(xs, w))
                end;
            let a : Vec(Nat, 2) = Vec/cons(1, Vec/cons(2, Vec/nil()));
            let b : Vec(Nat, 1) = Vec/cons(3, Vec/nil());
            let c : Vec(Nat, 3) = append(a, b);
            0
        "#;

        assert!(compile(source, None).is_ok());
    }

    #[test]
    fn motive_pattern_slots_are_validated() {
        // The annotated motive's slots are validated positionally against the
        // registry: slot count must cover parameters then indices; an index
        // slot must bind (a fresh name or `_`); a verbatim parameter must be
        // the scrutinee's actual parameter.
        let inductive_decl = r#"
            use /std/{Nat, Bin};
            induct Vec(T : Type) : (n : Nat) -> Type
            | nil() : (0)
            | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
            end
        "#;

        let arity = format!(
            r#"{inductive_decl}
            let f(@T : Type, @n : Nat, v : Vec(T, n)) -> Nat =
                match v : (v : Vec(T)) => Nat
                | nil() => 0
                | cons(m, x, xs) => 1
                end;
            0
        "#
        );
        let error = compile(&arity, None).unwrap_err();
        assert!(
            error.contains("argument slot(s)"),
            "unexpected error: {error}"
        );

        let index_slot = format!(
            r#"{inductive_decl}
            let f(@T : Type, @n : Nat, v : Vec(T, Nat/succ(n))) -> Nat =
                match v : (v : Vec(T, Nat/succ(n))) => Nat
                | nil() => 0
                | cons(m, x, xs) => 1
                end;
            0
        "#
        );
        let error = compile(&index_slot, None).unwrap_err();
        assert!(
            error.contains("must bind a fresh name"),
            "unexpected error: {error}"
        );

        let param = format!(
            r#"{inductive_decl}
            let f(@n : Nat, v : Vec(Nat, n)) -> Nat =
                match v : (v : Vec(Bin, k)) => Nat
                | nil() => 0
                | cons(m, x, xs) => 1
                end;
            0
        "#
        );
        let error = compile(&param, None).unwrap_err();
        assert!(
            error.contains("fixes a parameter"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn index_refinement_learns_inside_the_arm() {
        // Rung B: a scrutinee index that is a stable key is refined to the
        // case's target inside the arm. Three faces of it:
        // - `subst` casts `Vec(Bin, n)` to `Vec(Bin, m)` through an
        //   `Eq(Nat, n, m)` under a *constant* motive — the equality is
        //   learned (`n := z`, `m := z`), not eliminated;
        // - `sym` is J-style elimination from the pattern motive alone;
        // - `f`'s nil arm uses a hypothesis demanding `Vec(T, 0)` — legal
        //   because the arm refines `n := 0`.
        let source = r#"
            use /std/{Nat, Bin};
            induct Vec(T : Type) : (n : Nat) -> Type
            | nil() : (0)
            | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
            end
            induct Eq(A : Type) : (x : A, y : A) -> Type
            | refl(z : A) : (z, z)
            end
            let subst(@n : Nat, @m : Nat, p : Eq(Nat, n, m), v : Vec(Bin, n)) -> Vec(Bin, m) =
                match p : Vec(Bin, m)
                | refl(z) => v
                end;
            let sym(@A : Type, @x : A, @y : A, p : Eq(A, x, y)) -> Eq(A, y, x) =
                match p : (q : Eq(A, s, t)) => Eq(A, t, s)
                | refl(z) => Eq/refl(z)
                end;
            let zonly(@T : Type, v : Vec(T, 0)) -> Nat = 9;
            let f(@T : Type, @n : Nat, v : Vec(T, n), w : Vec(T, n)) -> Nat =
                match v : Nat
                | nil() => zonly(w)
                | cons(j, x, xs) => 1
                end;
            let a : Vec(Bin, 0) = Vec/nil();
            let p : Eq(Nat, 0, 0) = Eq/refl(0);
            let b : Vec(Bin, 0) = subst(p, a);
            let q : Eq(Nat, 3, 3) = sym(Eq/refl(3));
            f(Vec/nil(@Bin), Vec/nil())
        "#;

        assert!(compile(source, None).is_ok());
    }

    #[test]
    fn empty_inductive_lowers_and_vacuous_match_eliminates_it() {
        // An inductive may declare zero cases — `False`. Its eliminator is a match
        // with zero arms: every omission is vacuously justified, so the match
        // checks at any motive and lowers through erasure and codegen.
        let source = r#"
            induct False : Type
            end
            let absurd(A : Type, v : False) -> A =
                match v : A
                end;
            5
        "#;

        assert!(compile(source, None).is_ok());
    }

    #[test]
    fn inversion_prunes_impossible_arms_and_solves_binders() {
        // Rung C: at `Vec(T, Nat/succ(n))` the nil arm's target `0` clashes
        // definitely with the successor spine, so the arm is omitted —
        // checker-verified, no `impossible` keyword — and erase fills its
        // dispatch slot with an unreachable body. In the cons arm the
        // unifier decomposes `Nat/succ(n) ~ Nat/succ(j)` and pins `j := n`, which is
        // what types `xs : Vec(T, j)` at the declared `Vec(T, n)`.
        let source = r#"
            use /std/{Nat, Bin};
            induct Vec(T : Type) : (n : Nat) -> Type
            | nil() : (0)
            | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
            end
            let first(@T : Type, @n : Nat, v : Vec(T, Nat/succ(n))) -> T =
                match v : T
                | cons(j, x, xs) => x
                end;
            let rest(@T : Type, @n : Nat, v : Vec(T, Nat/succ(n))) -> Vec(T, n) =
                match v : Vec(T, n)
                | cons(j, x, xs) => xs
                end;
            let v : Vec(Bin, 2) = Vec/cons(/std/Str/to_bin("a"), Vec/cons(/std/Str/to_bin("b"), Vec/nil()));
            let w : Vec(Bin, 1) = rest(v);
            first(w)
        "#;

        assert!(compile(source, None).is_ok());
    }

    #[test]
    fn impossible_inductive_arm_lowers_to_unreachable() {
        let source = r#"
            use /std/{Nat, Bin};
            induct Vec(T : Type) : (n : Nat) -> Type
            | nil() : (0)
            | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
            end
            let first(@T : Type, @n : Nat, v : Vec(T, Nat/succ(n))) -> T =
                match v : T
                | cons(j, x, xs) => x
                end;
            let v : Vec(Bin, 1) = Vec/cons(/std/Str/to_bin("a"), Vec/nil());
            first(v)
        "#;

        let (ersd, cont) = compile_printed_stages(source).unwrap();

        assert!(
            ersd.contains("unreachable"),
            "expected Ersd output to contain unreachable, got {ersd}",
        );
        assert!(
            cont.contains("unreachable"),
            "expected Cont output to contain unreachable, got {cont}",
        );
    }

    #[test]
    fn omission_requires_a_definite_clash() {
        // An opaque index proves nothing: omitting nil at `Vec(T, n)` is
        // rejected with the explanation as the error.
        let opaque = r#"
            use /std/{Nat};
            induct Vec(T : Type) : (n : Nat) -> Type
            | nil() : (0)
            | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
            end
            let f(@T : Type, @n : Nat, v : Vec(T, n)) -> Nat =
                match v : Nat
                | cons(j, x, xs) => 1
                end;
            0
        "#;
        let error = compile(opaque, None).unwrap_err();
        assert!(
            error.contains("not provably impossible"),
            "unexpected error: {error}"
        );

        // The non-linear refusal — no K through the back door: `same`'s
        // target `(z, z)` constrains two positions with one binder, which
        // the unifier refuses, so the arm stays mandatory even at the
        // plainly-uninhabited `Foo(3, 4)`. The flip side: `diff`'s target
        // `(0, 1)` clashes against literals `(5, 5)` and prunes.
        let nonlinear = r#"
            use /std/{Nat, Bin};
            induct Foo : (x : Nat, y : Nat) -> Type
            | same(z : Nat) : (z, z)
            | diff() : (0, 1)
            end
            let f(q : Foo(3, 4)) -> Bin =
                match q : Bin
                | diff() => /std/Str/to_bin("d")
                end;
            0
        "#;
        let error = compile(nonlinear, None).unwrap_err();
        assert!(
            error.contains("missing arm 'same'"),
            "unexpected error: {error}"
        );

        let prunes = r#"
            use /std/{Nat, Bin};
            induct Foo : (x : Nat, y : Nat) -> Type
            | same(z : Nat) : (z, z)
            | diff() : (0, 1)
            end
            let g(q : Foo(5, 5)) -> Bin =
                match q : Bin
                | same(z) => /std/Str/to_bin("s")
                end;
            g(Foo/same(5))
        "#;
        assert!(compile(prunes, None).is_ok());
    }

    #[test]
    fn indexed_inductive_index_mismatch_is_rejected() {
        // A two-element vector annotated at length 3: the per-case target
        // `Nat/succ(m)` propagates through conversion until the index clash
        // surfaces as an ordinary type mismatch.
        let source = r#"
            use /std/{Nat};
            induct Vec(T : Type) : (n : Nat) -> Type
            | nil() : (0)
            | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
            end
            let v : Vec(Nat, 3) = Vec/cons(10, Vec/cons(20, Vec/nil()));
            0
        "#;

        let error = compile(source, None).unwrap_err();

        assert!(error.contains("type mismatch"), "unexpected error: {error}");
    }

    #[test]
    fn indexed_inductive_targets_are_required_and_arity_checked() {
        // A case of an indexed inductive without its `: (...)` target is a parse
        // error, as is a target whose arity differs from the head's index
        // telescope, or a target on an unindexed inductive.
        let missing = r#"
            use /std/{Nat};
            induct Vec(T : Type) : (n : Nat) -> Type
            | nil()
            | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
            end
            0
        "#;
        let error = missing.parse::<curios_text::Entrypoint>().unwrap_err();
        assert!(
            format!("{error:?}").contains("must state its index target"),
            "unexpected error: {error:?}"
        );

        let surplus = r#"
            use /std/{Nat};
            induct Pair(A : Type) : Type
            | pair(A, A) : (0)
            end
            0
        "#;
        let error = surplus.parse::<curios_text::Entrypoint>().unwrap_err();
        assert!(
            format!("{error:?}").contains("declares no indices"),
            "unexpected error: {error:?}"
        );

        let arity = r#"
            use /std/{Nat};
            induct Vec(T : Type) : (n : Nat) -> Type
            | nil() : (0, 1)
            | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
            end
            0
        "#;
        let error = arity.parse::<curios_text::Entrypoint>().unwrap_err();
        assert!(
            format!("{error:?}").contains("but the head declares 1"),
            "unexpected error: {error:?}"
        );
    }

    #[test]
    fn lambda_argument_postpones_until_a_sibling_pins_its_domain() {
        // `Arr/map((pair) => pair.0, xs)`: the inserted implicit `?A` is the
        // lambda's domain *and* `xs`'s element type, but `xs : Arr(?A)` is checked
        // after the lambda. Elaboration must postpone the lambda (its domain is an
        // unsolved metavar, and its body projects `pair.0`) until `xs` pins `?A`,
        // then re-check it. Guards the lambda-domain arm of `blocked_on_metavar`;
        // without it this fails "projected from a non-tuple". Checked at the
        // type-check level — the inference is the point, not lowering.
        let source = r#"
            use /std/{Arr};
            use /std/{Nat};
            let first(xs : Arr({ Nat, Nat })) -> Arr(Nat) =
                Arr/map((pair) => pair.0, xs);
            first
        "#;

        assert!(typecheck(source).is_ok());
    }

    #[test]
    fn empty_array_postpones_until_a_sibling_pins_its_element_type() {
        // `pick([||], Arr/concat)`: the inserted implicit `?A` is the empty array's
        // type *and* `combine`'s domain. The empty-array literal `[||]` borrows its
        // element type from the expected (check-only intro), so against the bare
        // metavar `?A` it cannot elaborate. Elaboration must postpone it until the
        // sibling `Arr/concat` grounds `?A := Arr(?T)`, then re-check — at which
        // point the `Arr(Nat)` result pins `?T`. Exercises the array arm of
        // `blocked_on_metavar`; without it this fails "type mismatch" eagerly.
        let source = r#"
            use /std/{Arr};
            use /std/{Nat};
            let pick(@A : Type, fallback : A, combine : (A, A) -> A) -> A =
                combine(fallback, fallback);
            let go : Arr(Nat) =
                pick([||], Arr/concat);
            go
        "#;

        assert!(typecheck(source).is_ok());

        // With no sibling to ground the element type and no result type to pin it,
        // the postponed `[||]` re-checks against a bare metavar and is rejected —
        // graceful degradation, no new acceptance.
        let unpinned = r#"
            use /std/{Arr};
            let id(@A : Type, x : A) -> A = x;
            let bad = id([||]);
            bad
        "#;

        assert!(typecheck(unpinned).is_err());
    }

    #[test]
    fn continuation_postpones_until_the_result_type_pins_its_codomain() {
        // A `!` region whose tail is `Parse/pure((x, x))` — a *bare tuple*,
        // checkable only against a known tuple type. The expected type reaches
        // the tail solely through each bind's result metavar `?B`, which the turnaround
        // solves *after* the continuation is checked. Elaboration must postpone the
        // continuation lambda (its codomain `M(?B)` carries a result metavar) until
        // `expect` grounds `?B` against the concrete `Parse({ Nat, Nat })`, then re-check
        // it. Guards the codomain arm of `blocked_on_metavar`; without it the tail fails
        // "introduced a tuple where the expected type is not a tuple type".
        let source = r#"
            use /std/{Parse};
            use /std/{Nat};
            let pair : Parse({ Nat, Nat }) =
                let x = Parse/any_byte!;
                Parse/pure((x, x));
            pair
        "#;

        assert!(typecheck(source).is_ok());

        // The `expected_ground` gate: with no concrete result type to pin `?B`, the
        // codomain stays a metavar, the continuation is *not* postponed, and the bare
        // tuple is rejected — graceful degradation, no new acceptance.
        let unpinned = r#"
            use /std/{Parse};
            let x = Parse/any_byte!;
            Parse/pure((x, x))
        "#;

        assert!(typecheck(unpinned).is_err());
    }

    #[test]
    fn closure_returning_a_bare_projection_lowers() {
        // A closure whose body *is* a tuple projection (`(pair) => pair.0`), handed to
        // a higher-order function over an empty array, never constructs a tuple
        // anywhere in the module — yet lowering must still emit the arity-1 tuple type
        // the projection reads through. The wasm `Table` sizes its tuple types from the
        // max arity it sees; scanning only tuple *constructions* missed this
        // projection-only arity and panicked "`Table` lacks tuple type for arity `1`".
        // Guards folding projection (`index + 1`) and prealloc arities into that scan.
        let source = r#"
            use /std/{Arr};
            use /std/{Nat};
            Arr/map(@{ Nat, Nat }, @Nat, (pair) => pair.0, [||])
        "#;

        assert!(compile(source, None).is_ok());
    }

    #[test]
    fn bare_polymorphic_function_inserts_implicits_in_value_position() {
        // Passing a bare `Arr/concat : (@T, Arr T, Arr T) -> Arr T` where an
        // explicit `(Arr Nat, Arr Nat) -> Arr Nat` is expected: the check
        // turnaround (`insert_implicits_on_check`) inserts the implicit `@T` and
        // eta-expands over the explicit binders, so no hand-written
        // `(l, r) => concat(l, r)` wrapper is needed. Lowers end-to-end — the
        // eta-expansion is an ordinary closure over a saturated call.
        let source = r#"
            use /std/{Arr};
            use /std/{Nat};
            let pairwise(f : (Arr(Nat), Arr(Nat)) -> Arr(Nat), a : Arr(Nat)) -> Arr(Nat) =
                f(a, a);
            pairwise(Arr/concat, [|1|])
        "#;

        assert!(compile(source, None).is_ok());
    }

    #[test]
    fn polymorphic_value_assignment_keeps_its_implicit() {
        // The guard arm: when the *expected* type also leads with an implicit
        // binder, implicit-eta must not fire — the polymorphic function is assigned
        // as-is, implicit intact, and stays applicable at a chosen instance. Without
        // the expected-not-implicit gate this would wrongly eta-expand and fail to
        // convert against the implicit-leading annotation.
        let source = r#"
            use /std/{Arr};
            use /std/{Nat};
            let g : (@T : Type, Arr(T), Arr(T)) -> Arr(T) = Arr/concat;
            g(@Nat, [|1|], [|2|])
        "#;

        assert!(compile(source, None).is_ok());
    }

    #[test]
    fn typeless_let_infers_a_literal_body() {
        // A local `let` with no type annotation infers the body's type (`Nat`
        // here) and lowers end-to-end.
        let source = r#"
            let n = 5;
            n
        "#;

        assert!(compile(source, None).is_ok());
    }

    #[test]
    fn typeless_let_binds_an_annotated_closure() {
        // The composite feature: a typeless local `let` binds an annotated
        // closure. The closure's type is synthesized from its annotation
        // (Infer-mode `elaborate_func`), the let's type is inferred from it, and
        // `f(5)` checks and lowers all the way to wasm.
        let source = r#"
            use /std/{Nat};
            let f = (x : Nat) => x;
            f(5)
        "#;

        assert!(compile(source, None).is_ok());
    }

    #[test]
    fn closure_annotation_must_match_the_expected_domain() {
        // In checking position the param annotation is verified against the
        // expected function type's domain — a wrong annotation is a type mismatch.
        let source = r#"
            use /std/{Nat, Bln};
            let f : (Nat) -> Nat = (x : Bln) => x;
            f(5)
        "#;

        let error = compile(source, None).unwrap_err();

        assert!(error.contains("mismatch"), "unexpected error: {error}");
    }

    #[test]
    fn bare_typeless_let_closure_cannot_be_inferred() {
        // Without an annotation there is nothing to infer the domain from, so a
        // typeless `let` binding a bare closure is a `cannot`-infer error.
        let source = r#"
            let f = (x) => x;
            f
        "#;

        let error = compile(source, None).unwrap_err();

        assert!(error.contains("cannot"), "unexpected error: {error}");
    }

    // --- A: fast `check` (typecheck-only) ------------------------------------

    fn typecheck(source: &str) -> Result<(), String> {
        let entrypoint = source.parse::<curios_text::Entrypoint>().unwrap();
        typecheck_entrypoint(
            Duration::from_secs(5),
            &entrypoint,
            &curios_text::NullLoader,
            |_| {},
        )
    }

    #[test]
    fn typecheck_accepts_a_well_typed_program() {
        // The fast path stops after `elaborate → zonk`; a well-typed program passes
        // without running erase/cont/optm/wasm.
        assert!(
            typecheck("/std/Io/write(/std/Io/stdout, /std/Str/to_bin(/std/Nat/to_str(0)))").is_ok()
        );
    }

    #[test]
    fn typecheck_rejects_an_unsolved_hole() {
        // `zonk` is included in the fast path, so an unconstrained hole is still
        // caught — type-checking is fully validated even though lowering is skipped.
        let error = typecheck(
            r#"
            use /std/{Nat};
            let m : Nat = ?;
            m
            "#,
        )
        .unwrap_err();

        assert!(error.contains("cannot"), "unexpected error: {error}");
    }

    // --- B2: named tuple fields ----------------------------------------------

    #[test]
    fn proj_by_label_resolves_to_its_position() {
        // `.label` is elaboration-time sugar for the positional projection,
        // so both spellings typecheck identically.
        let source = r#"
            use /std/{Nat, Bin, Io};
            let r : { status : Nat, payload : Bin } = (0, /std/Str/to_bin("ok"));
            let by_label : Bin = r.payload;
            let by_index : Bin = r.1;
            Io/write(Io/stdout, by_label)
        "#;

        assert!(typecheck(source).is_ok());
    }

    #[test]
    fn proj_unknown_label_names_the_available_fields() {
        let source = r#"
            use /std/{Nat, Bin};
            let r : { status : Nat, payload : Bin } = (0, /std/Str/to_bin("ok"));
            r.body
        "#;

        let error = typecheck(source).unwrap_err();
        assert!(
            error.contains("no field named 'body'") && error.contains("status"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn duplicate_tuple_label_is_rejected() {
        let source = r#"
            use /std/{Nat};
            let r : { x : Nat, x : Nat } = (0, 1);
            r.x
        "#;

        let error = typecheck(source).unwrap_err();
        assert!(
            error.contains("duplicate field label 'x'"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn tuple_labels_are_part_of_type_identity() {
        // Same positional types, different label order: not convertible —
        // this is what makes `.label` re-indexing impossible.
        let reordered = r#"
            use /std/{Nat};
            let p : { width : Nat, height : Nat } = (640, 480);
            let q : { height : Nat, width : Nat } = p;
            q.width
        "#;
        assert!(typecheck(reordered).is_err());

        // Labeled and unlabeled spellings are distinct types too.
        let unlabeled = r#"
            use /std/{Nat};
            let p : { width : Nat, height : Nat } = (640, 480);
            let q : { Nat, Nat } = p;
            q.0
        "#;
        assert!(typecheck(unlabeled).is_err());
    }

    #[test]
    fn named_construction_checks_against_the_labels() {
        // Written names must match the expected type's labels positionally;
        // bare fields are always accepted.
        let source = r#"
            use /std/{Nat, Bin};
            let r : { status : Nat, payload : Bin } = (status = 0, payload = /std/Str/to_bin("ok"));
            let mixed : { status : Nat, payload : Bin } = (status = 0, /std/Str/to_bin("ok"));
            r.status
        "#;
        assert!(typecheck(source).is_ok());

        let wrong_name = r#"
            use /std/{Nat, Bin};
            let r : { status : Nat, payload : Bin } = (code = 0, payload = /std/Str/to_bin("ok"));
            r.status
        "#;
        let error = typecheck(wrong_name).unwrap_err();
        assert!(
            error.contains("'code'") && error.contains("'status'"),
            "unexpected error: {error}"
        );

        let unlabeled_type = r#"
            use /std/{Nat, Bin};
            let r : { Nat, Bin } = (status = 0, /std/Str/to_bin("ok"));
            r.0
        "#;
        assert!(typecheck(unlabeled_type).is_err());
    }

    #[test]
    fn dependent_record_projects_by_label() {
        // Labels bind dependently: a later field's type mentions an earlier
        // label, and label projection re-types through the dependency.
        let source = r#"
            let p : { T : Type, x : T } = (T = /std/Nat, x = 3);
            let v : p.T = p.x;
            /std/Io/write(/std/Io/stdout, /std/Str/to_bin(/std/Nat/to_str(v)))
        "#;

        assert!(typecheck(source).is_ok());
    }

    #[test]
    fn inductive_payload_relying_on_implicit_insertion_is_rebuilt() {
        // The inductive registry used to keep `to_core`'s *lowered* payload and
        // index types, so a type relying on implicit-argument insertion —
        // `Eq(0, 1)` against `Eq`'s 3-ary type constructor — survived
        // under-applied and panicked the `Telescope::open` arity assert the
        // first time reduction met the registry copy. The registry telescopes
        // are now rebuilt during `elaborate_module` (indices while the inductive
        // group's signatures are assumed, constructors once its bodies are
        // defined), so the payload elaborates like any other type.
        let payload = r#"
            induct Eq(@A : Type) : (x : A, y : A) -> Type
            | refl(z : A) : (z, z)
            end
            induct Box : Type
            | mk(p : Eq(0, 1))
            end
            0
        "#;
        assert!(typecheck(payload).is_ok());

        // Index types take the same path — and previously panicked even
        // earlier, while the type-constructor binding itself elaborated
        // (its body's `InductiveType` node checks against the index telescope).
        let index = r#"
            induct Eq(@A : Type) : (x : A, y : A) -> Type
            | refl(z : A) : (z, z)
            end
            induct Tag : (p : Eq(0, 0)) -> Type
            | mk() : (Eq/refl(0))
            end
            0
        "#;
        assert!(typecheck(index).is_ok());

        // End to end: construct and eliminate through the rebuilt registry —
        // the match arm's binder is typed from the rebuilt payload type, and
        // the whole program lowers to wasm.
        let through = r#"
            use /std/{Nat};
            induct Eq(@A : Type) : (x : A, y : A) -> Type
            | refl(z : A) : (z, z)
            end
            induct Box : Type
            | mk(p : Eq(0, 0))
            end
            let b : Box = Box/mk(Eq/refl(0));
            match b : Nat
            | mk(p) => 7
            end
        "#;
        assert!(compile(through, None).is_ok());
    }

    #[test]
    fn dead_user_definition_is_still_typechecked() {
        // A user-authored top-level binding the body never references is still
        // type-checked (every item is, before any reachability is considered), so
        // its error is reported. (`write` returns its `Nat` status, `Bin` mismatches.)
        let error = typecheck(
            r#"
            let dead : /std/Bin = /std/Io/write(/std/Io/stdout, /std/Str/to_bin("x"));
            /std/Io/write(/std/Io/stdout, /std/Str/to_bin("ok"))
            "#,
        )
        .unwrap_err();

        assert!(error.contains("mismatch"), "unexpected error: {error}");
    }
}
