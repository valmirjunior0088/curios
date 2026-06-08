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
        // `Result/success(?, ?, (a, a))` checked against a known `Result(...)`. The
        // tuple is an introduction form whose parameter type is the holed type-arg
        // `?A`, so it can't be checked until `?A` is known. Elaboration postpones it,
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
                Result/success(?, ?, (a, a));
            f(7)
        "#;

        assert!(compile(source, None).is_ok());

        // In infer position nothing pins the holes, so the postponed tuple is
        // re-checked against a still-unsolved metavar and rejected — graceful
        // degradation, no new acceptance of un-annotated constructors.
        let unpinned = r#"
            use /std/{Result};
            Result/success(?, ?, (1, 1))
        "#;

        assert!(compile(unpinned, None).is_err());
    }

    #[test]
    fn lambda_argument_postpones_until_a_sibling_pins_its_domain() {
        // `Arr/map(?, ?, (pair) => pair.0, xs)`: the holed type-arg `?A` is the
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
                Arr/map(?, ?, (pair) => pair.0, xs);
            first
        "#;

        assert!(typecheck(source).is_ok());
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
        typecheck_entrypoint(Duration::from_secs(5), &entrypoint, &text::NullLoader, |_| {})
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

        compile_entrypoint(Duration::from_secs(5), &entrypoint, &text::NullLoader, |stage| {
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
        })
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
        let names =
            core_item_names("use /std/{Io, Json, Parse};\n/std/Parse/run(/std/Json/Json, /std/Json/decode, \"1\")");

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
