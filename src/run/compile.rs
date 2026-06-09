use {
    crate::{cont, core, ersd, optm, text, wasm},
    std::time::Duration,
};

pub enum Stage<'a> {
    Text(&'a text::Entrypoint),
    Core(&'a core::Module),
    Ersd(&'a ersd::Module),
    Cont(&'a cont::Module),
    Optm(&'a cont::Module),
    Wasm(&'a wasm::Module),
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
fn elaborate_and_zonk<O>(
    timeout: Duration,
    entrypoint: &text::Entrypoint,
    loader: &dyn text::Loader,
    observe: &mut O,
) -> Result<(core::Module, core::Term), String>
where
    O: FnMut(Stage<'_>),
{
    observe(Stage::Text(entrypoint));

    let module =
        text::to_core(entrypoint, &text::prelude(loader)).map_err(|error| error.format())?;

    observe(Stage::Core(&module));

    let mut context = core::Context::new(timeout);

    let core_mode = match &module.type_ {
        Some(type_) => core::Mode::Check(type_.clone()),
        None => core::Mode::Infer,
    };

    let (module, core_type) =
        core::elaborate_module(&mut context, &module, core_mode).map_err(|error| error.format())?;

    let module = core::zonk_module(&context, &module).map_err(|error| error.format())?;
    let core_type = core::zonk(&context, &core_type).map_err(|error| error.format())?;

    Ok((module, core_type))
}

/// Type-check an entrypoint and stop — the fast path for `check`. Runs only
/// `to_core → elaborate → zonk` (so it observes `Text` and `Core` only), skipping
/// the `erase → cont → optm → wasm` lowering that a type-check does not need.
pub fn typecheck_entrypoint<O>(
    timeout: Duration,
    entrypoint: &text::Entrypoint,
    loader: &dyn text::Loader,
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
    entrypoint: &text::Entrypoint,
    loader: &dyn text::Loader,
    mut observe: O,
) -> Result<wasm::Module, String>
where
    O: FnMut(Stage<'_>),
{
    let (module, core_type) = elaborate_and_zonk(timeout, entrypoint, loader, &mut observe)?;

    let ersd_module = core::erase_module(&mut core::Context::new(timeout), &module, &core_type)
        .map_err(|error| error.format())?;

    observe(Stage::Ersd(&ersd_module));

    let cont_module = ersd::to_cont(&ersd_module);

    observe(Stage::Cont(&cont_module));

    let optm_module = optm::optimize(cont_module);

    observe(Stage::Optm(&optm_module));

    let wasm_module = cont::to_wasm(&optm_module);

    observe(Stage::Wasm(&wasm_module));

    Ok(wasm_module)
}

#[cfg(test)]
mod tests {
    use {super::*, std::time::Duration};

    #[test]
    fn entrypoint_type_is_used_as_expected_type() {
        let entrypoint = "0"
            .parse::<text::Entrypoint>()
            .unwrap()
            .with_type("/sys/Bln".parse().unwrap());

        let error = compile_entrypoint(
            Duration::from_secs(1),
            &entrypoint,
            &text::NullLoader,
            |_| {},
        )
        .unwrap_err();

        assert!(error.contains("type mismatch"));
    }

    fn compile(source: &str, type_: Option<&str>) -> Result<wasm::Module, String> {
        let entrypoint = source.parse::<text::Entrypoint>().unwrap();

        let entrypoint = match type_ {
            Some(type_) => entrypoint.with_type(type_.parse().unwrap()),
            None => entrypoint,
        };

        compile_entrypoint(
            Duration::from_secs(5),
            &entrypoint,
            &text::NullLoader,
            |_| {},
        )
    }

    #[test]
    fn meta_free_prelude_program_compiles_without_overflow() {
        // The exact case BUG.md calls out: a meta-free entrypoint (no holes) that
        // still pulls in the whole sys/std prelude. Assembling and traversing the
        // old N-deep nested term overflowed the stack during construction and in
        // every pass; the flat `core::Module`/`ersd::Module` representation lowers
        // it end-to-end to wasm without overflow.
        let source = r#"
            let id(A : Type, a : A) -> A = a;
            id(/sys/Nat, 5)
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
        // `id ? true` checked against `/sys/Bln`: the turnaround pins the type
        // argument `?` to `Bln` through the expected type (§14, a type-level pin).
        let source = r#"
            use /sys/{Bln};
            let id(A : Type, a : A) -> A = a;
            id(?, true)
        "#;

        assert!(compile(source, Some("/sys/Bln")).is_ok());
    }

    #[test]
    fn unconstrained_value_hole_cannot_be_inferred() {
        // `let m : Nat = ? in m`: nothing constrains the value of `?`, so the
        // metavariable is unsolved at zonk and compilation fails (§14).
        let source = r#"
            use /sys/{Nat};
            let m : Nat = ?;
            m
        "#;

        let error = compile(source, Some("/sys/Nat")).unwrap_err();

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
            use /sys/{Bln};
            let pick(A : Type, a : A, b : A, c : Bln) -> A =
                match c
                | false => a
                | true => b
                end;
            pick(/sys/Nat, 1, 2, true)
        "#;

        assert!(compile(source, None).is_ok());
    }

    #[test]
    fn projection_through_a_stuck_union_payload_lowers() {
        // `Fmt/printf`'s return type is `format_type_with({}, parse(s))`, so
        // erasing `printf` evaluates `parse(s)` at compile time with a *symbolic*
        // `s`. The `Parse` combinator's result is a `Result` union whose
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
            use /sys/{Nat};
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
    fn union_match_arm_arity_is_checked_statically() {
        // Each arm's binder count is checked against the
        // constructor's registry telescope at elaboration time. Under the
        // legacy tagged-tuple desugar this mismatch was silent (the extra
        // binder became an out-of-range payload projection).
        let source = r#"
            use /std/{Result};
            use /sys/{Nat, Bin};
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
        // `@` — including a union constructor's parameters, which are implicit
        // by default — and the fully-supplied call compiles end-to-end.
        let source = r#"
            use /sys/{Nat};
            union Opt(A : Type)
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
        // The INDUCTIVES.md promise realized: an `@`-marked union parameter
        // makes the constructor's type argument implicit, so the call site
        // writes no holes at all.
        let source = r#"
            use /sys/{Nat};
            union Opt(A : Type)
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
            use /sys/{Nat, Bin};
            let second(@T : Type, x : T, @U : Type, y : U) -> U = y;
            sys/Bin/len(second(@Nat, 1, "abc"))
        "#;

        compile(source, None).unwrap();
    }

    #[test]
    fn implicit_argument_queues_are_order_insensitive() {
        // The two queues are matched independently: an `@`-argument fills the
        // first unfilled implicit binder no matter where it sits among the
        // plain arguments.
        let at_first = r#"
            use /sys/{Nat, Bin};
            let second(@T : Type, x : T, @U : Type, y : U) -> U = y;
            sys/Bin/len(second(@Nat, 1, "abc"))
        "#;
        let at_last = r#"
            use /sys/{Nat, Bin};
            let second(@T : Type, x : T, @U : Type, y : U) -> U = y;
            sys/Bin/len(second(1, "abc", @Nat))
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
            use /sys/{Nat};
            union Opt(A : Type)
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
        // the next telescope — both through a direct call and the `with`
        // sugar (which desugars to exactly that call).
        let source = r#"
            use /sys/{Nat};
            union Id(A : Type)
            | wrap(A)
            end
            let bind : (@A : Type, @B : Type) -> (Id(A), (A) -> Id(B)) -> Id(B) =
                (A, B) => (m, f) =>
                    match m : Id(B)
                    | wrap(x) => f(x)
                    end;
            let direct = bind(Id/wrap(1), (x) => Id/wrap(x + 1));
            let sugared =
                with bind
                let v = Id/wrap(3)!;
                Id/wrap(v);
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
            use /sys/{Nat};
            let cast(x : Nat, @T : Type) -> Nat = x;
            cast(5)
        "#;

        let error = compile(source, None).unwrap_err();

        assert!(
            error.contains("implicit argument 'T' of 'cast' was not inferred"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn surplus_implicit_arguments_are_rejected() {
        let source = r#"
            use /sys/{Nat};
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
    fn non_pub_union_constructors_are_usable_in_the_declaring_module() {
        // Constructors are exactly as visible as their union: a non-`pub`
        // union is module-local but fully usable where it is declared.
        let source = r#"
            use /sys/{Nat};
            union Opt
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
    fn non_pub_union_constructors_stay_private_across_modules() {
        // The same union declared inside a submodule is not reachable from
        // the parent: the union's own visibility still gates the outside.
        let source = r#"
            pub mod m
                union Secret
                | hide(/sys/Nat)
                end
            end
            m/Secret/hide(7)
        "#;

        assert!(compile(source, None).is_err());
    }

    #[test]
    fn union_match_on_a_non_union_scrutinee_is_rejected_directly() {
        // With the legacy fallback gone, matching union
        // constructors on a non-union value reports the real problem instead
        // of a downstream projection error.
        let source = r#"
            use /sys/{Nat};
            match 7 : Nat
            | success(value) => value
            end
        "#;

        let error = compile(source, None).unwrap_err();

        assert!(
            error.contains("matched union constructors on a non-union type"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn new_style_union_match_lowers_end_to_end() {
        // The same program with correct arities compiles through to wasm: the
        // `Result` declaration takes the primitive-inductive path (UnionType /
        // Variant / UnionMatch) and erases back to the legacy tagged-tuple
        // runtime shape.
        let source = r#"
            use /std/{Result};
            use /sys/{Nat, Bin};
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
            use /sys/{Nat};
            let first(xs : Arr({ Nat, Nat })) -> Arr(Nat) =
                Arr/map((pair) => pair.0, xs);
            first
        "#;

        assert!(typecheck(source).is_ok());
    }

    #[test]
    fn continuation_postpones_until_the_result_type_pins_its_codomain() {
        // A `with Parse/bind` block whose tail is `Parse/pure((x, x))` — a *bare
        // tuple*, checkable only against a known tuple type. The expected type reaches
        // the tail solely through each bind's result metavar `?B`, which the turnaround
        // solves *after* the continuation is checked. Elaboration must postpone the
        // continuation lambda (its codomain `Parse(?B)` carries a result metavar) until
        // `expect` grounds `?B` against the concrete `Parse({ Nat, Nat })`, then re-check
        // it. Guards the codomain arm of `blocked_on_metavar`; without it the tail fails
        // "introduced a tuple where the expected type is not a tuple type".
        let source = r#"
            use /std/{Parse};
            use /sys/{Nat};
            let pair : Parse({ Nat, Nat }) =
                with Parse/bind
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
            with Parse/bind
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
            use /sys/{Nat};
            Arr/map(@{ Nat, Nat }, @Nat, (pair) => pair.0, [])
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
            use /sys/{Nat};
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
            use /sys/{Nat, Bln};
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
        let entrypoint = source.parse::<text::Entrypoint>().unwrap();
        typecheck_entrypoint(
            Duration::from_secs(5),
            &entrypoint,
            &text::NullLoader,
            |_| {},
        )
    }

    #[test]
    fn typecheck_accepts_a_well_typed_program() {
        // The fast path stops after `elaborate → zonk`; a well-typed program passes
        // without running erase/cont/optm/wasm.
        assert!(typecheck("/sys/Io/print(/sys/Nat/to_str(0))").is_ok());
    }

    #[test]
    fn typecheck_rejects_an_unsolved_hole() {
        // `zonk` is included in the fast path, so an unconstrained hole is still
        // caught — type-checking is fully validated even though lowering is skipped.
        let error = typecheck(
            r#"
            use /sys/{Nat};
            let m : Nat = ?;
            m
            "#,
        )
        .unwrap_err();

        assert!(error.contains("cannot"), "unexpected error: {error}");
    }

    // --- C: reachability prune of unused sys/std -----------------------------

    /// The `name`s of every top-level item in the lowered `core::Module`, captured
    /// from the `Core` stage of a full compile (which runs the real `prelude`).
    fn core_item_names(source: &str) -> Vec<String> {
        let entrypoint = source.parse::<text::Entrypoint>().unwrap();
        let mut names = Vec::new();

        compile_entrypoint(
            Duration::from_secs(5),
            &entrypoint,
            &text::NullLoader,
            |stage| {
                if let Stage::Core(module) = stage {
                    for item in &module.items {
                        match item {
                            core::Item::Let(def) => names.push(def.name.clone()),
                            core::Item::Rec(defs) => {
                                names.extend(defs.iter().map(|def| def.name.clone()))
                            }
                        }
                    }
                }
            },
        )
        .unwrap();

        names
    }

    #[test]
    fn prune_drops_unreachable_library_modules() {
        // A program touching only `Io`/`Nat` must not drag the unrelated (and
        // expensive) `std/Json`, `std/Parse`, `std/Fmt`, … into the typechecked core.
        let names = core_item_names("use /std/{Io, Nat};\n/std/Io/print(/std/Nat/to_str(0))");

        for unused in ["std/Json", "std/Parse", "std/Fmt", "std/Lst", "std/Str"] {
            assert!(
                !names.iter().any(|name| name.starts_with(unused)),
                "expected `{unused}` to be pruned, but it survived in {names:?}"
            );
        }
    }

    #[test]
    fn prune_keeps_reachable_library_and_transitive_deps() {
        // Decoding pulls `std/Json` and its transitive `std/Parse` dependency.
        let names = core_item_names(
            "use /std/{Io, Json, Parse};\n/std/Parse/run(/std/Json/decode, \"1\")",
        );

        assert!(
            names.iter().any(|name| name.starts_with("std/Json")),
            "expected `std/Json` to be retained, got {names:?}"
        );
        assert!(
            names.iter().any(|name| name.starts_with("std/Parse")),
            "expected transitive `std/Parse` to be retained, got {names:?}"
        );
    }

    #[test]
    fn prune_still_typechecks_dead_user_definitions() {
        // The prune is root-restricted: a user-authored top-level binding the body
        // never references is still type-checked, so its error is reported.
        let error = typecheck(
            r#"
            let dead : /sys/Nat = /sys/Io/print("x");
            /sys/Io/print("ok")
            "#,
        )
        .unwrap_err();

        assert!(error.contains("mismatch"), "unexpected error: {error}");
    }
}
