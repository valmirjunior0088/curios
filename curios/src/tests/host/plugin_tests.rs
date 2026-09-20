//! A `foreign` declaration answered by a WebAssembly module, end to end: the plugin is built here, instantiated by the runtime, and called from a compiled Curios program.
//!
//! **The plugin is assembled rather than checked in.** `curios-wasm` is the workspace's encoder and `curios-js`'s bridge builder is the precedent for standing a module up with it — which keeps the fixture readable as the thing it is, costs no `.wasm` in the tree, and means a change to the raw ABI shows up here as a compile error rather than as a binary nobody can read.

use {
    crate::tests::run_wasm,
    curios_pipeline::compile_with_prelude,
    curios_runtime::{DeclaredModule, MockHost, ModuleBytes, plugin_bindings},
    curios_text::{Entrypoint, RootSource},
    curios_wasm::{
        AddressType, CompType, Export, Expr, Func, FuncName, FuncType, Instr, Limits, MemName,
        MemType, Module, NumType, ResultType, SubType, TypeName, ValType, to_bytes,
    },
    std::collections::BTreeMap,
};

/// `i32`, the carrier every raw-ABI scalar and every `(ptr, len)` half crosses as.
fn i32_type() -> ValType {
    ValType::Num(NumType::I32)
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
        vec![i32_type()],
        vec![i32_type()],
        vec![local(0), Instr::I32Const { value: 2 }, Instr::I32Mul],
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

/// One module answering the declarations `exports` names.
fn declared(exports: &[(&str, &str)]) -> Vec<DeclaredModule> {
    vec![DeclaredModule {
        package: "fixture".to_string(),
        name: "probe".to_string(),
        bytes: ModuleBytes::Source(plugin()),
        exports: exports
            .iter()
            .map(|(export, declaration)| (export.to_string(), declaration.to_string()))
            .collect::<BTreeMap<_, _>>(),
    }]
}

/// Compile `source`, bind it against the fixture plugin, and run it, answering the exit code.
fn run_against(source: &str, exports: &[(&str, &str)]) -> Result<i32, String> {
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

    let bindings = plugin_bindings(foreigns, declared(exports))?;
    let (system, _io) = MockHost::builder().build();

    run_wasm(&module, system, bindings)
}

/// A scalar reaching a plugin and coming back, which is the whole path with no memory in it: the guest unboxes its i31, the plugin doubles a raw `i32`, and the host boxes the answer.
#[test]
fn a_scalar_crosses_to_a_plugin_and_back() {
    let code = run_against(
        r#"
        foreign double : (Nat) -> Nat;
        let _ = /std/proc/exit(@{}, double(21)!)!;
        /std/Io/pure(())
        "#,
        &[("double", "/double")],
    )
    .expect("execution succeeded");

    assert_eq!(code, 42);
}

/// A byte string reaching a plugin's linear memory and coming back out of it — the crossing the whole design exists for, since this is where a GC array and a linear memory have to meet.
#[test]
fn a_byte_string_crosses_through_the_plugins_memory() {
    let code = run_against(
        r#"
        foreign echo : (Bytes) -> Bytes;
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

/// A declaration nothing claims is refused naming it, because a manifest that has not caught up with a source file is the ordinary way to arrive here.
#[test]
fn a_declaration_no_export_claims_is_refused() {
    let refusal = run_against(
        r#"
        foreign double : (Nat) -> Nat;
        foreign unclaimed : (Nat) -> Nat;
        let _ = /std/proc/exit(@{}, double(21)!)!;
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

/// A claim naming a declaration the program does not hold, which is the same manifest drift seen from the other side.
#[test]
fn a_claim_naming_no_declaration_is_refused() {
    let refusal = run_against(
        r#"
        foreign double : (Nat) -> Nat;
        let _ = /std/proc/exit(@{}, double(21)!)!;
        /std/Io/pure(())
        "#,
        &[("double", "/double"), ("echo", "/absent")],
    )
    .expect_err("a claim answering nothing");

    assert!(refusal.contains("`/absent`"), "{refusal}");
}

/// A claim on an export the module does not have, refused listing what it does — so a typo is read off the message rather than guessed at.
#[test]
fn a_claim_on_a_missing_export_is_refused_listing_what_there_is() {
    let refusal = run_against(
        r#"
        foreign double : (Nat) -> Nat;
        let _ = /std/proc/exit(@{}, double(21)!)!;
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
