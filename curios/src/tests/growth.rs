//! What a program costs to compile as it grows.
//!
//! A growth is an ordinary program written along one axis an author grows: longer, wider, deeper, under more binders, or holding a larger literal. Doubling one should at most double what each stage does, and change no verdict, which is `documentation/design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md`'s rule; that its depth costs only memory is `documentation/design/compilation/depth-is-bought-with-stack-not-with-hand-rolled-frames.md`'s, and that its module validates at any size is `curios/src/compile.rs`'s.
//!
//! A growth that compiles past the size a stage once refused or never answered at is a fixture here; the bench's `growth_slopes` (`xbench/src/measurements/growth.rs`) counts what each stage does at three sizes and holds the growths that are linear today to it.

use {
    crate::test_support::*,
    curios_common::fill,
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_native::test_support::*,
    curios_pipeline::{compile_with_prelude, test_support::*},
    curios_surface::Entrypoint,
    std::fmt::Write,
};

/// `size` constant prints at a program's top level, each of its own line, `line 1` to `line {size}`: the chain a program writes where `Growth::Statements` writes one in a function's body.
fn printed_lines(size: usize) -> String {
    let lines = (1..size)
        .map(|line| fill!("let () = print(\"line ${line}\\n\")!;\n", line))
        .collect::<String>();
    fill!(
        "use /std/{print};\n\n${lines}print(\"line ${size}\\n\")\n",
        lines,
        size,
    )
}

/// Whether `source` compiles through every stage — Binaryen and Cranelift included — with what it is refused for where it does not.
fn compiles(source: &str) -> Result<(), String> {
    compile(source).map(|_| ())
}

/// Whether `source` is lowered and elaborated, its erasure obligations met, with what it is refused for where it is not: what a fixture holding elaboration's own cost asks, the stages after it being other fixtures'.
fn elaborates(source: &str) -> Result<(), String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())?;
    let checked = typecheck_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none())
        .map_err(|refused| format!("{refused:?}"))?;
    match checked.obligations.is_empty() {
        true => Ok(()),
        false => Err(format!("{:?}", checked.obligations)),
    }
}

/// `f(n)`, `size` calls of `step` in its body, each at the answer of the one before it, ending in the sum of every answer: each stays live to the end.
fn kept_calls(size: usize) -> String {
    let mut body = "let step(n: Nat) -> Nat =\n    match n | 0 => 1 | m + 1 => (step(m) * 3 + 1) % 1000003 end;\n\nlet f(n: Nat) -> Nat =\n    let x0 = n;\n".to_string();
    for index in 1..=size {
        writeln!(body, "    let x{index} = step(x{});", index - 1).expect("a string takes a line");
    }
    let sum = joined((0..=size).map(|index| format!("x{index}")), " + ");
    writeln!(body, "    {sum};").expect("a string takes a line");
    program(&body, "f(a)")
}

/// The most locals any function of the module `source` is emitted as holds.
fn most_locals(source: &str) -> usize {
    let entrypoint = source.parse::<Entrypoint>().expect("the fixture parses");
    let (mut module, _) =
        compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |_| {})
            .expect("the fixture compiles");
    module
        .funcs_mut()
        .map(|(_, function)| function.locals.len())
        .max()
        .unwrap_or(0)
}

/// Whether `source` is emitted as a module the engine validates, with what it is refused for where it is not: what a fixture holding the emitter to the engine's limits asks.
fn emits(source: &str) -> Result<(), String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())?;
    let (module, _) =
        compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |_| {})?;
    start_test_engine();
    curios_native::validate(&curios_wasm::to_bytes(
        &module,
        &curios_wasm::Names::default(),
    ))
    .map_err(|error| error.to_string())
}

/// Elaboration takes a deep expression in its size: a left-leaning and a right-nested chain of ten thousand operators, whose recorded terms are each other's subterms and are zonked in one pass, and a thousand nested list literals, whose levels are a chain of solutions zonked once along it.
#[test]
fn deep_expressions_elaborate_in_their_size() {
    for (growth, size) in [
        (Growth::LeftPlus, 10_000),
        (Growth::RightPlus, 10_000),
        (Growth::Lists, 1_000),
    ] {
        assert_eq!(elaborates(&grown(growth, size)), Ok(()), "{growth:?}");
    }
}

/// Elaboration takes a scope in its size: a `let` chain of six thousand bindings, each operator minting its metavariables under every binding before it; an `Io` block of two thousand statements, each a frame over the last; a function summing a thousand parameters, and one claiming that sum in its codomain; two thousand nested lambdas; and a lambda summing two thousand bindings before it. A judgment reads its scope through the frames' persistent maps, and a metavariable is born over its scope's telescope and identity spine, shared, so none pays the binders above it; a close or a materialization passes over what it cannot change, so none pays the statements or the lambdas within it, a close of the summing lambda reads the spine entries that hold its binder, and a close of the codomain rewrites the spine its metavariables share once for all of them; and a node's free variables share its children's, so a sum naming one more binding a level pays for that one.
#[test]
fn a_wide_scope_elaborates_in_its_size() {
    for (growth, size) in [
        (Growth::LetPlus, 6_000),
        (Growth::Statements, 2_000),
        (Growth::ParametersSummed, 1_000),
        (Growth::CodomainSummed, 1_000),
        (Growth::Lambdas, 2_000),
        (Growth::LetsSummed, 2_000),
    ] {
        assert_eq!(elaborates(&grown(growth, size)), Ok(()), "{growth:?}");
    }
}

/// A family of four thousand constructors compiles in its size: each constructor is an item of its own, and each imports the one constructor it reads of the family's row, at its position, rather than the row.
#[test]
fn a_family_of_many_constructors_compiles_in_its_size() {
    assert_eq!(compiles(&grown(Growth::Constructors, 4_000)), Ok(()));
}

/// A chain of constant prints folds in its length, at a program's top level and in a function's body: each print's description is copied where it stands, its bytes reaching the host write as one constant, and the continuation each link names is that function rather than a copy carrying the rest of the chain into the next round, so twice the prints leave at most twice the writes in the optimized code.
#[test]
fn a_chain_of_constant_prints_folds_in_its_length() {
    let [top_level_8, top_level_16] =
        [8, 16].map(|size| host_writes(&anf_optm(&printed_lines(size))));
    let [in_body_8, in_body_16] =
        [8, 16].map(|size| host_writes(&anf_optm(&grown(Growth::Statements, size))));
    for (shape, shorter, longer) in [
        ("at the top level", &top_level_8, &top_level_16),
        ("in a function's body", &in_body_8, &in_body_16),
    ] {
        assert!(
            longer.len() <= 2 * shorter.len(),
            "{shape}, 16 prints leave {} host writes where 8 leave {}",
            longer.len(),
            shorter.len(),
        );
    }

    let constant = top_level_16.iter().flatten().collect::<Vec<_>>();
    for line in 1..=16 {
        let message = format!("line {line}\n").into_bytes();
        assert!(
            constant.contains(&&message),
            "line {line}'s bytes reach no host write as a constant"
        );
    }
}

/// A concept of two thousand methods compiles in its size: each method's wrapper reads its one field of the concept's record, its parameter's shape off the record's row, and the record type's sort alone, where each once read every field.
#[test]
fn a_concept_of_many_methods_compiles_in_its_size() {
    assert_eq!(compiles(&grown(Growth::Methods, 2_000)), Ok(()));
}

/// Two thousand lambdas applied in turn compile: each is moved into its one call as the one around it is, within one pass of the continuation optimizer's shrinker, where a round apiece would run the optimizer past its backstop.
#[test]
fn a_nest_of_lambdas_applied_in_turn_compiles_in_its_size() {
    assert_eq!(compiles(&grown(Growth::Lambdas, 2_000)), Ok(()));
}

/// A thousand matches, each on the `let` before it, compile: a local definition is read by value once, where it is made, its body stored with its solutions substituted, and a settlement is kept for as long as what it read stands, so no arm reads the chain before it again, and a probe under an arm is spelled once for its lookup, outside its metavariables' occurrences.
#[test]
fn a_chain_of_matches_each_on_the_let_before_it_compiles_in_its_size() {
    assert_eq!(compiles(&grown(Growth::ChainedMatches, 1_000)), Ok(()));
}

/// Ten thousand one hundred `+` in one function are emitted as a module the engine validates, written in one expression and as a `let` chain: each operator's value dies where the next is written, so the emitter folds their locals by liveness onto a few, where a local apiece would pass the engine's fifty thousand.
#[test]
fn ten_thousand_operators_in_one_function_fit_the_engine_s_locals() {
    for growth in [Growth::LeftPlus, Growth::LetPlus] {
        assert_eq!(emits(&grown(growth, 10_100)), Ok(()), "{growth:?}");
    }
}

/// A function's locals grow with what it binds: a chain of calls whose answers are all kept to the end is emitted with locals in its length, each value in the one local the function holds it in, where a block binding again every value live into it bound the chain's whole live set at each call.
#[test]
fn a_chain_of_calls_whose_answers_are_all_kept_is_emitted_with_locals_in_its_length() {
    let (hundred, two_hundred) = (most_locals(&kept_calls(100)), most_locals(&kept_calls(200)));

    assert!(
        two_hundred < 3 * hundred,
        "two hundred calls hold {two_hundred} locals, a hundred {hundred}"
    );
}

/// A structure of 10 001 fields and a family whose constructor carries 10 001 run, and a tuple of 10 001 elements handed to a function is emitted as a module the engine validates: a struct type past the engine's 10 000 fields keeps 9 999 and nests the rest in a trailing struct.
#[test]
fn a_struct_past_the_engine_s_fields_spills_into_a_trailing_struct() {
    let width = 10_001;
    let values = joined((0..width).map(|index| format!("a + {index}")), ", ");

    let fields = joined((0..width).map(|index| format!("x{index}: Nat")), ", ");
    let named = joined(
        (0..width).map(|index| format!("x{index} = a + {index}")),
        ", ",
    );
    let last = width - 1;
    let structure = program(
        &fill!(
            "struct Wide: Type { ${fields} }\n\nlet f(a: Nat) -> Nat =\n    let w = Wide { ${named} };\n    w.x${last};\n",
            fields,
            named,
            last,
        ),
        "f(a)",
    );
    assert_eq!(run(&structure), b"10000\n");

    let payload = repeated("Nat", width, ", ");
    let binders = joined((0..width).map(|index| format!("x{index}")), ", ");
    let constructor = program(
        &fill!(
            "induct Wide: Type\n| make(${payload})\nend\n\nlet f(a: Nat) -> Nat = match Wide/make(${values}) | make(${binders}) => x${last} end;\n",
            payload,
            values,
            binders,
            last,
        ),
        "f(a)",
    );
    assert_eq!(run(&constructor), b"10000\n");

    let wide = repeated("Nat", width, ", ");
    let tuple = program(
        &fill!(
            "let pick(n: Nat, t: {${wide}}) -> Nat = match n | 0 => t.${last} | _ => pick(n - 1, t) end;\n\nlet f(a: Nat) -> Nat =\n    let t = (${values});\n    pick(a, t) + pick(a + 1, t);\n",
            wide,
            last,
            values,
        ),
        "f(a)",
    );
    assert_eq!(emits(&tuple), Ok(()));
}

/// A closure capturing 10 001 values, called where it is not known, is emitted as a module the engine validates: its environment is a struct past the engine's fields, spilled as any other.
#[test]
fn a_closure_capturing_past_the_engine_s_fields_spills_its_environment() {
    let width = 10_001;
    let lets = (0..width)
        .map(|index| format!("    let x{index} = a + {index};\n"))
        .collect::<String>();
    let captured = joined((0..width).map(|index| format!("x{index}")), " + ");
    let source = program(
        &fill!(
            "use /std/{List};\n\nlet apply(gs: List((Nat) -> Nat), n: Nat) -> Nat = match gs | [g, .._] => g(n) | [] => n end;\n\nlet f(a: Nat) -> Nat =\n${lets}    apply([(n: Nat) => n + ${captured}, (n: Nat) => n], a);\n",
            lets,
            captured,
        ),
        "f(a)",
    );
    assert_eq!(emits(&source), Ok(()));
}

/// A closure of 1 001 parameters, called where it is not known, runs, and a function returning a tuple of 1 001 elements its callers read is emitted as a module the engine validates: a signature past the engine's 1 000 parameters or results keeps 999 and passes the rest as one struct.
#[test]
fn a_signature_past_the_engine_s_parameters_or_results_spills_into_a_struct() {
    let width = 1_001;
    let last = width - 1;
    let arrow = repeated("Nat", width, ", ");
    let typed = joined((0..width).map(|index| format!("x{index}: Nat")), ", ");
    let arguments = joined((0..width).map(|index| format!("n + {index}")), ", ");
    let parameters = program(
        &fill!(
            "use /std/{List};\n\nlet apply(gs: List((${arrow}) -> Nat), n: Nat) -> Nat = match gs | [g, .._] => g(${arguments}) | [] => n end;\n\nlet f(a: Nat) -> Nat = apply([(${typed}) => x0 + x${last}, (${typed}) => x1], a);\n",
            arrow,
            arguments,
            typed,
            last,
        ),
        "f(a)",
    );
    assert_eq!(run(&parameters), b"1000\n");

    let elements = joined((0..width).map(|index| format!("a + {index}")), ", ");
    let results = program(
        &fill!(
            "let g(n: Nat, a: Nat) -> {${arrow}} = match n | 0 => (${elements}) | _ => g(n - 1, a + 1) end;\n\nlet f(a: Nat) -> Nat = g(a, a).${last} + g(a + 1, a).0;\n",
            arrow,
            elements,
            last,
        ),
        "f(a)",
    );
    assert_eq!(emits(&results), Ok(()));
}

/// Two thousand lambdas applied in turn at two calls compile: the nest is collapsed into one function of all its parameters, which both calls pass, where absorbing a level a round would run the optimizer past its backstop.
#[test]
fn a_nest_of_lambdas_applied_in_turn_at_two_calls_compiles_in_its_size() {
    let size = 2_000;
    let lambdas = (0..size)
        .map(|index| format!("(x{index}) => "))
        .collect::<String>();
    let applied = format!("g{}", "(a)".repeat(size));
    let source = program(
        &format!("let g: {}Nat = {lambdas}x0;\n", "(Nat) -> ".repeat(size)),
        &format!("{applied} + {applied}"),
    );
    assert_eq!(compiles(&source), Ok(()));
}

/// Nesting ten thousand deep compiles on a test thread's stack: parentheses, each a nonterminal the parser re-enters, and an arrow, each parameter a term the lowering lowers inside the one before. The depth is bought with stack where it is spent, so no stage's acceptance follows the thread it runs on.
#[test]
fn nesting_ten_thousand_deep_compiles_on_a_test_thread() {
    for growth in [Growth::Parentheses, Growth::Arrow] {
        assert_eq!(compiles(&grown(growth, 10_000)), Ok(()), "{growth:?}");
    }
}

/// Every growth compiles and runs at a small size: what `growth_slopes` measures is a program, and one a stage refuses measures nothing.
#[test]
fn every_growth_is_a_program_at_its_smallest() {
    for growth in Growth::ALL {
        assert_eq!(compiles(&grown(growth, 3)), Ok(()), "{growth:?}");
    }
}
