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

pub fn compile_entrypoint<O>(
    timeout: Duration,
    entrypoint: &text::Entrypoint,
    loader: &dyn text::Loader,
    mut observe: O,
) -> Result<wasm::Module, String>
where
    O: FnMut(Stage<'_>),
{
    observe(Stage::Text(entrypoint));

    let module =
        text::to_core(entrypoint, &text::prelude(loader)).map_err(|error| error.format())?;

    observe(Stage::Core(&module));

    // Elaborate (checking against the entrypoint's type when it carries one, else
    // synthesizing), then zonk metavariable solutions in so the module is
    // meta-free, then erase the meta-free module to `ersd` — the
    // `elaborate → zonk → erase` data flow (§9). Elaboration is authoritative: it
    // returns a rebuilt module (lambda domains solved, binders re-closed), and it
    // is *that* module — not the lowered one — that zonk makes meta-free.
    // Elaboration and zonking share one context (the solutions live in its
    // `MetaStore`); erase runs over a fresh one. Each pass iterates the flat
    // top-level items rather than recursing a nested spine, so prelude depth no
    // longer overflows the stack (BUG.md).
    let mut context = core::Context::new(timeout);

    let core_mode = match &module.type_ {
        Some(type_) => core::Mode::Check(type_.clone()),
        None => core::Mode::Infer,
    };

    let (module, core_type) =
        core::elaborate_module(&mut context, &module, core_mode).map_err(|error| error.format())?;

    let module = core::zonk_module(&context, &module).map_err(|error| error.format())?;
    let core_type = core::zonk(&context, &core_type).map_err(|error| error.format())?;

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
}
