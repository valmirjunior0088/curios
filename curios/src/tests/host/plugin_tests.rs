//! A `foreign` declaration answered by a WebAssembly module, end to end: the plugin is built here, instantiated by the runtime, and called from a compiled Curios program.
//!
//! **The plugin is assembled rather than checked in.** `curios-wasm` is the workspace's encoder and `curios-js`'s bridge builder is the precedent for standing a module up with it — which keeps the fixture readable as the thing it is, costs no `.wasm` in the tree, and means a change to the raw ABI shows up here as a compile error rather than as a binary nobody can read.

use {
    crate::tests::run_wasm,
    curios_pipeline::compile_with_prelude,
    curios_runtime::{DeclaredModule, MockHost, plugin_bindings},
    curios_text::{Entrypoint, RootSource},
    curios_wasm::{
        AddressType, CompType, Export, Expr, Func, FuncName, FuncType, Instr, Limits, MemName,
        MemType, Module, NumType, ResultType, SubType, TypeName, ValType, to_bytes,
    },
    std::collections::BTreeMap,
};

/// `i32`, the carrier a `(ptr, len)` half and the allocator's size cross as.
fn i32_type() -> ValType {
    ValType::Num(NumType::I32)
}

/// `i64`, the carrier a `Nat` or `Int` crosses to a plugin as.
fn i64_type() -> ValType {
    ValType::Num(NumType::I64)
}

/// A final function type with the given signature, which is how every export below is declared.
fn func_type(inputs: Vec<ValType>, outputs: Vec<ValType>) -> SubType {
    SubType {
        is_final: true,
        super_types: vec![],
        comp_type: CompType::Func(FuncType {
            inputs: ResultType::from(inputs),
            outputs: ResultType::from(outputs),
        }),
    }
}

/// One export: its name, its signature, and its whole body.
fn export(
    module: &mut Module,
    name: &str,
    inputs: Vec<ValType>,
    outputs: Vec<ValType>,
    body: Vec<Instr>,
) {
    let type_name = TypeName::from(name);
    let func_name = FuncName::from(name);

    module.add_type(type_name.clone(), func_type(inputs.clone(), outputs));
    module.add_func(
        func_name.clone(),
        Func {
            type_name,
            params: (0..inputs.len())
                .map(|index| curios_wasm::LocalName::from(format!("a{index}")))
                .collect(),
            locals: vec![],
            expr: Expr::from(body),
        },
    );
    module.add_export(name, Export::Func(func_name));
}

fn local(index: usize) -> Instr {
    Instr::LocalGet {
        local_name: curios_wasm::LocalName::from(format!("a{index}")),
    }
}

/// The fixture plugin: a memory, an allocator, and two functions covering both ways a value crosses.
///
/// `alloc` answers a constant offset. A bump allocator would be more honest about what a plugin does, and buys nothing here: one call is in flight at a time, so every allocation may as well land in the same place — and a test whose allocator could run out would be testing the fixture.
fn plugin() -> Vec<u8> {
    let mut module = Module::new("probe");

    let memory = MemName::from("memory");
    module.add_memory(
        memory.clone(),
        MemType {
            address_type: AddressType::I32,
            limits: Limits { min: 1, max: None },
        },
    );
    module.add_export("memory", Export::Memory(memory));

    export(
        &mut module,
        "alloc",
        vec![i32_type()],
        vec![i32_type()],
        vec![Instr::I32Const { value: 0 }],
    );

    // A scalar crossing: doubled, so the answer could not be the argument passed through.
    export(
        &mut module,
        "double",
        vec![i64_type()],
        vec![i64_type()],
        vec![local(0), Instr::I64Const { value: 2 }, Instr::I64Mul],
    );

    // A scalar answer with every bit set: `-1` as the `i64` it crosses as, which a `Nat` reads as the largest the wire carries.
    export(
        &mut module,
        "ones",
        vec![],
        vec![i64_type()],
        vec![Instr::I64Const { value: -1 }],
    );

    // A byte crossing: `255 - b`, so both ends of the range come back as the other.
    export(
        &mut module,
        "flip",
        vec![i32_type()],
        vec![i32_type()],
        vec![Instr::I32Const { value: 255 }, local(0), Instr::I32Sub],
    );

    // A byte answer the guest cannot hold: `b + 1`, which is 256 for 255.
    export(
        &mut module,
        "bump",
        vec![i32_type()],
        vec![i32_type()],
        vec![local(0), Instr::I32Const { value: 1 }, Instr::I32Add],
    );

    // A recursion with no end: the export calls itself, so only the engine's limit stops it.
    export(
        &mut module,
        "spin",
        vec![],
        vec![i64_type()],
        vec![Instr::Call {
            func_name: FuncName::from("spin"),
        }],
    );

    // An export that traps where it stands.
    export(
        &mut module,
        "boom",
        vec![],
        vec![i64_type()],
        vec![Instr::Unreachable],
    );

    // A byte-string answer past the end of the one page the module's memory holds.
    export(
        &mut module,
        "far",
        vec![],
        vec![i32_type(), i32_type()],
        vec![
            Instr::I32Const { value: 65530 },
            Instr::I32Const { value: 100 },
        ],
    );

    // A byte string crossing: the `(ptr, len)` pair handed straight back, which is what proves the write and the read back agree on where the bytes are.
    export(
        &mut module,
        "echo",
        vec![i32_type(), i32_type()],
        vec![i32_type(), i32_type()],
        vec![local(0), local(1)],
    );

    to_bytes(&module)
}

/// One module answering the declarations `exports` names, each an export beside the declaration it answers.
fn declared(exports: &[(&str, &str)]) -> Vec<DeclaredModule> {
    vec![DeclaredModule {
        package: "fixture".to_string(),
        name: "probe".to_string(),
        code: curios_cranelift::compile(&plugin()).expect("the plugin compiles"),
        answers: exports
            .iter()
            .map(|(export, declaration)| (declaration.to_string(), export.to_string()))
            .collect::<BTreeMap<_, _>>(),
    }]
}

/// Compile `source`, bind it against the fixture plugin, and run it, answering the exit code.
fn run_against(source: &str, exports: &[(&str, &str)]) -> Result<u8, String> {
    run_linked(source, declared(exports))
}

/// Compile `source`, bind it against `modules`, and run it, answering the exit code.
fn run_linked(source: &str, modules: Vec<DeclaredModule>) -> Result<u8, String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");

    let (module, foreigns) = compile_with_prelude(
        curios_pipeline::DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |_| {},
    )
    .expect("compile succeeded");

    let bindings = plugin_bindings(&foreigns, modules)?;
    let (system, _io) = MockHost::builder().build();

    run_wasm(&module, foreigns, system, bindings)
}

/// A scalar reaching a plugin and coming back, which is the whole path with no memory in it: the guest unboxes its i31, the plugin doubles a raw `i64`, and the guest boxes the answer.
#[test]
fn a_scalar_crosses_to_a_plugin_and_back() {
    let code = run_against(
        r#"
        foreign double(Nat) -> Nat;
        let _ = /std/proc/exit(@{}, /std/Nat/to_byte(double(21)! % 256))!;
        /std/Io/pure(())
        "#,
        &[("double", "/double")],
    )
    .expect("execution succeeded");

    assert_eq!(code, 42);
}

/// A `Byte` reaching a plugin as its word and coming back as one, at both ends of its range.
#[test]
fn a_byte_crosses_to_a_plugin_and_back() {
    let code = run_against(
        r#"
        foreign flip(Byte) -> Byte;
        let top = flip(0)!;
        let bottom = flip(255)!;
        let _ = /std/proc/exit(@{}, match /std/Byte/to_nat(bottom) == 0 | true => top | false => 1 end)!;
        /std/Io/pure(())
        "#,
        &[("flip", "/flip")],
    )
    .expect("execution succeeded");

    assert_eq!(code, 255);
}

/// A plugin answering a `Byte` with a word past 255 is refused rather than truncated into a different byte.
#[test]
fn a_plugin_byte_past_255_is_refused() {
    let refusal = run_against(
        r#"
        foreign bump(Byte) -> Byte;
        let _ = /std/proc/exit(@{}, bump(255)!)!;
        /std/Io/pure(())
        "#,
        &[("bump", "/bump")],
    )
    .expect_err("256 is not a byte");

    assert!(
        refusal.contains("with 256, which is not a byte"),
        "{refusal}"
    );
}

/// A byte string reaching a plugin's linear memory and coming back out of it — the crossing the whole design exists for, since this is where a GC array and a linear memory have to meet.
#[test]
fn a_byte_string_crosses_through_the_plugins_memory() {
    let code = run_against(
        r#"
        foreign echo(Bytes) -> Bytes;
        let answered = echo(/std/Str/to_bytes("curios"))!;
        let matched = /std/Bytes/eql(answered, /std/Str/to_bytes("curios"));
        let _ = /std/proc/exit(@{}, match matched | true => 9 | false => 1 end)!;
        /std/Io/pure(())
        "#,
        &[("echo", "/echo")],
    )
    .expect("execution succeeded");

    assert_eq!(code, 9, "the bytes came back as they went in");
}

/// A bit run reaching a plugin and coming back, over the export a byte string already crosses — which is the claim the wire type makes: `Bits` and `Bytes` hand the plugin the same `(ptr, len)` pair, and only the length the guest seals on the way back differs.
#[test]
fn a_bit_run_crosses_through_the_plugins_memory() {
    let code = run_against(
        r#"
        foreign echo(Bits) -> Bits;
        let sent = b[1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0];
        let answered = echo(sent)!;
        let matched = /std/Bits/eql(answered, sent);
        let _ = /std/proc/exit(@{}, match matched | true => 9 | false => 1 end)!;
        /std/Io/pure(())
        "#,
        &[("echo", "/echo")],
    )
    .expect("execution succeeded");

    assert_eq!(code, 9, "the bits came back as they went in");
}

/// A run whose length is not a multiple of eight comes back at `8n`, because the wire has no slot for a bit length — the one thing a `Bits` row does not preserve, pinned here rather than left to the prose that states it. Twelve bits cross as their two packed bytes and return as sixteen, the trailing four zero because the representation keeps a run's padding zeroed.
#[test]
fn a_run_shorter_than_its_bytes_returns_padded_to_them() {
    let code = run_against(
        r#"
        foreign echo(Bits) -> Bits;
        let answered = echo(b[1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1])!;
        let padded = b[1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0];
        let matched = /std/Bits/eql(answered, padded);
        let _ = /std/proc/exit(@{}, match matched | true => /std/Nat/to_byte(/std/Bits/len(answered) % 256) | false => 1 end)!;
        /std/Io/pure(())
        "#,
        &[("echo", "/echo")],
    )
    .expect("execution succeeded");

    assert_eq!(
        code, 16,
        "twelve bits returned as the sixteen they packed into"
    );
}

/// A declaration a program reaches that neither a module nor the host answers is refused before the program starts, naming it.
#[test]
fn a_declaration_nothing_answers_is_refused_where_a_program_reaches_it() {
    let refusal = run_against(
        r#"
        foreign double(Nat) -> Nat;
        foreign unclaimed(Nat) -> Nat;
        let _ = /std/proc/exit(@{}, /std/Nat/to_byte((double(21)! + unclaimed(1)!) % 256))!;
        /std/Io/pure(())
        "#,
        &[("double", "/double")],
    )
    .expect_err("a declaration nothing implements");

    assert!(
        refusal.contains("no implementation for `/unclaimed`"),
        "{refusal}"
    );
}

/// A program asks for what it reaches: a declaration nothing calls needs no implementation, and a claim on a row the program does not reach is a claim nothing asks about. Whether a package's claim names a declaration at all is held where its unit is compiled, which `curios-pipeline`'s `a_claim_is_held_to_a_declaration_of_its_own_unit` pins.
#[test]
fn a_declaration_nothing_reaches_asks_for_no_implementation() {
    let code = run_against(
        r#"
        foreign double(Nat) -> Nat;
        foreign unclaimed(Nat) -> Nat;
        let _ = /std/proc/exit(@{}, /std/Nat/to_byte(double(21)! % 256))!;
        /std/Io/pure(())
        "#,
        &[("double", "/double"), ("echo", "/absent")],
    )
    .expect("execution succeeded");

    assert_eq!(code, 42);
}

/// An export is held to its declaration where the two are linked, so a module that answers another type is refused before the program runs and not at the first call that reaches it.
#[test]
fn an_export_of_another_type_is_refused_before_the_program_runs() {
    let refusal = run_against(
        r#"
        foreign flip(Byte) -> Byte;
        let _ = /std/proc/exit(@{}, flip(0)!)!;
        /std/Io/pure(())
        "#,
        &[("double", "/flip")],
    )
    .expect_err("an export of another type");

    assert!(
        refusal.contains(
            "exports `double` as (i64) -> (i64), and `/flip` crosses to a module as (i32) -> (i32)"
        ),
        "{refusal}"
    );
}

/// A claim on an export the module does not have, refused listing what it does — so a typo is read off the message rather than guessed at.
#[test]
fn a_claim_on_a_missing_export_is_refused_listing_what_there_is() {
    let refusal = run_against(
        r#"
        foreign double(Nat) -> Nat;
        let _ = /std/proc/exit(@{}, /std/Nat/to_byte(double(21)! % 256))!;
        /std/Io/pure(())
        "#,
        &[("dubble", "/double")],
    )
    .expect_err("a claim on no export");

    assert!(refusal.contains("exports no such name"), "{refusal}");
    assert!(
        refusal.contains("double"),
        "the refusal lists what it exports: {refusal}"
    );
}

/// A `Nat` a plugin answers with every bit set arrives as `2⁶⁴ - 1`: the result crosses as an `i64` and the guest reads it unsigned, so no answer the integer holds arrives as another number.
#[test]
fn a_nat_answered_with_every_bit_set_is_read_unsigned() {
    let code = run_against(
        r#"
        foreign ones() -> Nat;
        let answered = ones()!;
        let _ = /std/proc/exit(@{}, match answered == 18446744073709551615 | true => 9 | false => 1 end)!;
        /std/Io/pure(())
        "#,
        &[("ones", "/ones")],
    )
    .expect("execution succeeded");

    assert_eq!(code, 9, "the answer arrived whole");
}

/// A declaration two modules are handed over for is refused where the run admits its bindings, naming it: nothing chooses between two answers. A manifest cannot state the two, its claims being keyed by declaration and held apart across its rows where it is read, so this is what an embedder of the runtime meets.
#[test]
fn a_declaration_two_modules_claim_is_refused() {
    let claim: &[(&str, &str)] = &[("double", "/double")];
    let refusal = run_linked(
        r#"
        foreign double(Nat) -> Nat;
        let _ = /std/proc/exit(@{}, /std/Nat/to_byte(double(21)! % 256))!;
        /std/Io/pure(())
        "#,
        declared(claim).into_iter().chain(declared(claim)).collect(),
    )
    .expect_err("one declaration, two answers");

    assert!(
        refusal.contains("`/double` is answered twice by the bindings of this run"),
        "{refusal}"
    );
}

/// A module's trap ends the run as a refusal that says who trapped, in which export, and what the engine said of it: a trap's own display is its frames, so the cause is carried in the sentence.
#[test]
fn a_modules_trap_names_the_module_the_export_and_the_cause() {
    let refusal = run_against(
        r#"
        foreign boom() -> Nat;
        let _ = /std/proc/exit(@{}, /std/Nat/to_byte(boom()! % 256))!;
        /std/Io/pure(())
        "#,
        &[("boom", "/boom")],
    )
    .expect_err("the export traps");

    assert!(
        refusal.contains("the module \"probe\" of fixture failed in `boom`: wasm trap: wasm `unreachable` instruction executed"),
        "{refusal}"
    );
}

/// A byte string a module answers past the end of its own memory is refused by the module's name and the export's, as a `Byte` answered past 255 is.
#[test]
fn an_answer_past_a_modules_memory_names_the_module_and_the_export() {
    let refusal = run_against(
        r#"
        foreign far() -> Bytes;
        let _ = /std/proc/exit(@{}, /std/Nat/to_byte(/std/Bytes/len(far()!) % 256))!;
        /std/Io/pure(())
        "#,
        &[("far", "/far")],
    )
    .expect_err("the answer is past the memory");

    assert!(
        refusal.contains("the module \"probe\" of fixture answered `far` with 100 bytes at 65530, past the end of its memory"),
        "{refusal}"
    );
}

/// A module's recursion with no end stops on the engine's trap wherever in the program it is called from: the module is an instance in the program's own store, so the limit set where the program was entered bounds its frames too. Here it is called from the last element of a walk two million frames deep; in a store of its own it would be handed the whole limit again below that frame, of a stack that holds it once, and the process would abort with no trap.
#[test]
fn a_modules_recursion_from_a_deep_frame_stops_on_the_engines_trap() {
    let refusal = run_against(
        r#"
        use /std/{Io, List, Nat};
        foreign spin() -> Nat;
        let numbers = List/range(0, 2000000);
        let walked = List/traverse(numbers, (n: Nat) => match n == 1999999 | true => spin() | false => Io/pure(n) end)!;
        let _ = /std/proc/exit(@{}, /std/Nat/to_byte(List/len(walked) % 256))!;
        /std/Io/pure(())
        "#,
        &[("spin", "/spin")],
    )
    .expect_err("a recursion with no end cannot answer");

    assert!(
        refusal.contains(
            "the module \"probe\" of fixture failed in `spin`: wasm trap: call stack exhausted"
        ),
        "{refusal}"
    );
}
