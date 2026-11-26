//! A structural recursion is a loop: each `/std` function written as a recursion on one peel enters a worker after the ANF optimizer, which takes the levels in turn and which the CPS optimizer makes a loop, so it keeps no frame a level.

use {
    curios_anf::{BlockId, Rhs, Statement},
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_pipeline::{Stage, compile_with_prelude},
    curios_surface::Entrypoint,
};

/// A program reaching each recursion of [`RECURSIONS`] on text its first argument gives, so no stage folds them away.
const PROGRAM: &str = r#"
    use /std/{Str, Nat, List, Option, Result, Try, Io, Proc, Fmt, Bytes, Flt};
    use /std/Tui/{Style, Frame, View};
    use /std/http/{Url, Field};
    let execute() -> Try(Io, Str, {}) =
        let arguments = Proc/args()!;
        let bytes = Option/to_result(List/try_get(arguments, 1), "no input")!;
        let s = Result/map_failure(Str/of_bytes(bytes), (_) => "invalid utf-8")!;
        let n = Str/length(s);
        let _ = Fmt/print("%\n")(Bytes/to_nat(bytes) % 1000003)!;
        let _ = Fmt/print("%\n")(Flt/to_str(Flt/pow10(n)))!;
        let cropped = View/crop(View/text(Style/plain, s), n / 2, 0);
        let fitted = View/fit(cropped, View/width(cropped), View/height(cropped));
        let _ = Fmt/print("%\n")(Str/join("/", Frame/to_lines(Frame/of_view(fitted))))!;
        let escaped = Url/escaped(s);
        let _ = Fmt/print("%\n")(Option/unwrap_or(Field/Value/to_str(Field/Value/append(escaped, escaped)), ""))!;
        Try/pure(());
    execute()
"#;

/// The `/std` functions written as a recursion on one peel, by the name each is erased under: a run's bytes read as a number, a power of ten, a frame's leading tiles dropped, a field value escaped, and one value's octets put ahead of another's.
const RECURSIONS: [&str; 5] = [
    "/std/Bytes/to_nat/to_nat",
    "/std/Flt/pow10",
    "/std/Tui/Frame/drop_upto",
    "/std/http/Url/escaped_bytes",
    "/std/http/Field/Value/prepended",
];

/// The module `source` is optimized into in ANF, and the one CPS emits from.
fn optimized(source: &str) -> (curios_anf::Module, curios_cps::Module) {
    let entrypoint = source.parse::<Entrypoint>().expect("fixture parses");
    let (mut anf, mut cps) = (None, None);
    compile_with_prelude(
        DEFAULT_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| match stage {
            Stage::AnfOptm(module) => anf = Some(module.clone()),
            Stage::CpsOptm(module) => cps = Some(module.clone()),
            _ => {}
        },
    )
    .expect("fixture compiles");
    (
        anf.expect("the pipeline observes anf-optm"),
        cps.expect("the pipeline observes cps-optm"),
    )
}

/// Whether `body` is the loop an inversion makes: it ends by entering the worker the recursion became, named for it with `@loop`.
fn is_loop(module: &curios_anf::Module, body: BlockId) -> bool {
    let Some(&last) = module.block(body).and_then(|block| block.statements.last()) else {
        return false;
    };
    matches!(
        module.statement(last),
        Some(Statement::Let {
            rhs: Rhs::Apply {
                callee: curios_anf::Atom::Function(worker),
                ..
            },
            ..
        }) if module
            .function(*worker)
            .and_then(|worker| worker.debug_name.as_deref())
            .is_some_and(|name| name.ends_with("@loop"))
    )
}

#[test]
fn every_recursion_on_one_peel_std_writes_is_a_loop_and_keeps_no_twin() {
    let (anf, cps) = optimized(PROGRAM);
    for name in RECURSIONS {
        let bodies: Vec<BlockId> = anf
            .function_ids()
            .filter_map(|id| anf.function(id))
            .filter(|function| function.debug_name.as_deref() == Some(name))
            .map(|function| function.body)
            .collect();
        assert!(!bodies.is_empty(), "{name} is in the program:\n{anf}");
        assert!(
            bodies.iter().all(|&body| is_loop(&anf, body)),
            "{name} is a loop:\n{anf}"
        );
        assert!(
            cps.live_functions()
                .filter(|(_, definition)| definition.debug_name.as_deref() == Some(name))
                .all(|(function, _)| cps.twin(function).is_none()),
            "{name} keeps no twin:\n{cps}"
        );
    }
}
