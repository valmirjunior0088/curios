//! A WebAssembly module supplying a program's own `foreign` declarations, and the bindings it becomes.
//!
//! **The marshalling is host-side, and that is the whole design.** A guest `Bytes` is a wasm-GC array; a plugin compiled from C, Rust or Zig holds its bytes in linear memory and knows nothing of GC. Nothing compiles those languages to wasm-GC today, so the two representations have to meet somewhere, and the host is where both are already in hand: `Lift` reads the guest's array into a `Vec<u8>`, this module writes that into the plugin's memory through the plugin's own allocator, and the result comes back the same way. Neither module learns the other's representation.
//!
//! **Nothing is supplied to a plugin, so a plugin that imports is refused** — before instantiation, naming all of its imports rather than whichever one a `LinkError` reached first. With nothing to reach, a plugin can compute and nothing else, which is a stronger guarantee than a native library could offer and the reason this is a wasm module rather than a shared object.
//!
//! **That refusal is a floor, not a ceiling, and the import list is where the ceiling would rise.** A module's imports are exactly the interface through which one payload could ask another for what it exports — which is how wasm composition works generally, and what would let several already-compiled modules share a library between them instead of each carrying a copy. Refusing by *naming what was demanded* is chosen with that in mind: the day some demands can be answered, this narrows to the ones that cannot, and no mechanism has to be invented to replace a blanket no.
//!
//! **One instance per row, not per module.** Two packages naming one module by its hash get one instance each, so a plugin holding state in its memory cannot have it observed or corrupted across a package boundary. Identical bytes cost an instantiation twice, which is cheap beside the confusion of shared mutable memory nobody declared.
//!
//! The ABI is the raw one every wasm toolchain already emits: a scalar crosses as itself, a byte string as a `(ptr, len)` pair into the plugin's memory, and a byte-string result comes back as the same pair through wasm's multi-value return. The plugin exports its `memory` and an allocator; what it is called and what it takes are [`ALLOC`] and the conformance check below.

use {
    super::{Lift, Lower, engine::ForeignBindings, shared_engine},
    curios_abi::{ForeignStore, ResultShape, WireType},
    std::{
        collections::{BTreeMap, BTreeSet},
        sync::{Arc, Mutex},
    },
    wasmtime::{Instance, Linker, Memory, Module, Store, TypedFunc, Val},
};

/// The allocator a plugin exports, by the name every raw-ABI toolchain gives it: bytes in, an offset into the plugin's own memory out.
///
/// Nothing is freed. A plugin is instantiated per row and lives as long as the program, so a call's scratch is reclaimed when the process ends — and a `dealloc` the plugin may also export is deliberately not called, because pairing allocations across a boundary whose far side may have panicked is a way to corrupt a heap rather than to save one.
const ALLOC: &str = "alloc";

/// The memory a plugin exports, through which every byte string crosses.
const MEMORY: &str = "memory";

/// One module a package declared, with what it answers.
///
/// Spelled here rather than taken from `curios-package`, which sits above this crate: what the runtime needs is bytes and a mapping, and naming the manifest type would put a package model under the launcher.
pub struct DeclaredModule {
    /// The package whose manifest declared the row, for a refusal to name.
    pub package: String,
    /// The row's own name, likewise.
    pub name: String,
    /// The module itself, already accepted against its hash by whoever read it.
    pub bytes: ModuleBytes,
    /// Which declaration each of its exports answers, as `export -> fully qualified name`.
    pub exports: BTreeMap<String, String>,
}

/// A plugin's module, and how this build is able to turn it into one.
///
/// **The split is the whole reason the launcher can link a plugin at all.** Compiling WebAssembly needs a backend; deserializing code this engine already emitted does not. `curios run` holds Cranelift and compiles the module a manifest pointed at, while a bundled executable carries what `curios compile` compiled and its launcher deserializes — so a plugin travels without a compiler travelling with it.
///
/// Gating the *variant* rather than a branch is what makes that structural: a runtime-only build has no way to spell a source module, so the case cannot be reached by a mistake rather than being reachable and refused.
pub enum ModuleBytes {
    /// A WebAssembly module, compiled here. Only a build carrying a backend can hold one.
    #[cfg(feature = "cranelift")]
    Source(Vec<u8>),
    /// Machine code this engine already emitted, deserialized rather than compiled.
    Precompiled(Vec<u8>),
}

/// An instantiated plugin: its store, its memory, the allocator every byte string goes through, and what it exports.
struct Plugin {
    store: Store<()>,
    instance: Instance,
    memory: Memory,
    alloc: TypedFunc<u32, u32>,
    /// Read once at instantiation, because reading it later would need the store this also owns — and a caller holding the lock to ask what a plugin exports would be holding it to answer itself.
    exports: BTreeSet<String>,
}

impl Plugin {
    /// Instantiate `bytes`, refusing a module that is not a plugin before it can become one.
    ///
    /// The checks are the whole of what "a plugin" means, and each is refused by name rather than left to a `LinkError` that would report one missing import and stop.
    fn instantiate(module: &DeclaredModule) -> Result<Self, String> {
        let engine = shared_engine();
        let subject = format!("the module {:?} of {}", module.name, module.package);

        let compiled = match &module.bytes {
            #[cfg(feature = "cranelift")]
            ModuleBytes::Source(bytes) => Module::new(engine, bytes).map_err(|error| {
                format!("{subject} is not a WebAssembly module this engine accepts: {error}")
            })?,
            // SAFETY: the payload was emitted by `curios compile` through `precompile`, which compiles on the very engine `shared_engine` builds here — the wasmtime pin is one manifest row, so a bundler's engine and its launcher's cannot drift — and it travels inside the executable's own image, past the point any other producer could substitute it. Wasmtime's own stamp inside the artifact stays the backstop for whatever that failed to separate.
            ModuleBytes::Precompiled(bytes) => unsafe {
                Module::deserialize(engine, bytes).map_err(|error| {
                    format!("{subject} is not machine code this engine accepts: {error}")
                })?
            },
        };

        // Before instantiation, so the refusal names every import rather than whichever one the linker reached first.
        let demanded = compiled
            .imports()
            .map(|import| format!("{}.{}", import.module(), import.name()))
            .collect::<Vec<_>>();
        if !demanded.is_empty() {
            return Err(format!(
                "{subject} imports {}, and nothing is supplied to a foreign module: it reaches no environment, which is what makes it able to compute and nothing else. Build it freestanding — `wasm32-unknown-unknown` rather than a WASI target",
                demanded.join(", ")
            ));
        }

        let mut store = Store::new(engine, ());
        let instance = Linker::new(engine)
            .instantiate(&mut store, &compiled)
            .map_err(|error| format!("{subject} could not be instantiated: {error}"))?;

        let Some(memory) = instance.get_memory(&mut store, MEMORY) else {
            return Err(format!(
                "{subject} exports no `{MEMORY}`, which is where a byte string crossing to it is written"
            ));
        };

        let alloc = instance
            .get_typed_func::<u32, u32>(&mut store, ALLOC)
            .map_err(|error| {
                format!(
                    "{subject} exports no `{ALLOC}` taking a size and answering an offset, which is how a byte string reaches its memory: {error}"
                )
            })?;

        let exports = instance
            .exports(&mut store)
            .map(|export| export.name().to_string())
            .collect();

        Ok(Self {
            store,
            instance,
            memory,
            alloc,
            exports,
        })
    }

    /// Copy `bytes` into the plugin's memory through its allocator, answering the offset they landed at.
    fn write(&mut self, bytes: &[u8]) -> Result<u32, String> {
        let length = u32::try_from(bytes.len()).map_err(|_| {
            format!(
                "a byte string of {} bytes is too long to cross",
                bytes.len()
            )
        })?;

        let offset = self
            .alloc
            .call(&mut self.store, length)
            .map_err(|error| format!("`{ALLOC}` failed: {error}"))?;

        self.memory
            .write(&mut self.store, offset as usize, bytes)
            .map_err(|error| format!("writing {length} bytes at {offset} failed: {error}"))?;

        Ok(offset)
    }

    /// Read `length` bytes back out of the plugin's memory at `offset`.
    fn read(&mut self, offset: u32, length: u32) -> Result<Vec<u8>, String> {
        let mut bytes = vec![0; length as usize];

        self.memory
            .read(&mut self.store, offset as usize, &mut bytes)
            .map_err(|error| format!("reading {length} bytes at {offset} failed: {error}"))?;

        Ok(bytes)
    }
}

/// What one wire type costs a plugin's signature, and how a value of it crosses.
///
/// A scalar is itself: a `Nat` or `Int` an `i64`, a `Bool` or a `Byte` an `i32`, an `Flt` an `f64`, as they cross to the host. A byte string is a `(ptr, len)` pair, which is why it costs two slots where a scalar costs one — the shape every raw-ABI toolchain already emits, rather than a convention invented here. `Bits` crosses as that same pair over its packed bytes, `len` counting bytes as it does for `Bytes`: the wire has no slot for a bit count, and a plugin needing one reads it in band.
///
/// `Handle` and `List` are refused. A handle is a token into the *host's* resource table and means nothing inside a plugin, which holds none; a list is a rope whose element marshalling nothing has asked for. Both are refused where a signature is read rather than mistranslated where it is called.
fn crossing(wire: WireType, subject: &str, declaration: &str) -> Result<Crossing, String> {
    match wire {
        WireType::Nat => Ok(Crossing::Nat),
        WireType::Int => Ok(Crossing::Int),
        WireType::Bool => Ok(Crossing::Bool),
        WireType::Byte => Ok(Crossing::Byte),
        WireType::Flt => Ok(Crossing::Float),
        WireType::Bytes | WireType::Bits => Ok(Crossing::Bytes),
        WireType::Handle | WireType::List(_) => Err(format!(
            "{subject} answers `{declaration}`, whose signature names a `Handle` or a `List`. A foreign module takes scalars and byte strings: a handle is a token into the host's own table and names nothing inside a plugin, and a list has no element marshalling yet"
        )),
    }
}

/// How one value crosses between the guest and a plugin's linear memory.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Crossing {
    Nat,
    Int,
    Bool,
    Byte,
    Float,
    Bytes,
}

/// Wire one declaration to the plugin export answering it.
///
/// The trampoline owns its plugin through an `Arc<Mutex<_>>`: wasmtime calls it from whichever thread is running the guest, and a plugin's memory is a single mutable thing whatever the guest is doing.
fn define_row(
    bindings: &mut ForeignBindings,
    plugin: &Arc<Mutex<Plugin>>,
    export: &str,
    row: &Arc<curios_abi::ForeignFunction>,
    subject: &str,
) -> Result<(), String> {
    let declaration = row.name.clone();
    let signature = row.signature.clone();

    let params = signature
        .params
        .iter()
        .map(|(_, wire)| crossing(*wire, subject, &declaration))
        .collect::<Result<Vec<_>, _>>()?;

    let result = match signature.results.shape() {
        ResultShape::Unit => None,
        ResultShape::Single(wire) => Some(crossing(wire, subject, &declaration)?),
        // A plugin answering several results at once would need the guest-facing tuple built from a shape the raw ABI has no spelling for. Refused rather than guessed at.
        ResultShape::Record(_) => {
            return Err(format!(
                "{subject} answers `{declaration}`, which yields several results. A foreign module answers one result or none"
            ));
        }
    };

    let plugin = Arc::clone(plugin);
    let export = export.to_string();
    let subject = subject.to_string();

    bindings.define_raw(&declaration, move |mut caller, given, answers| {
        let mut plugin = plugin.lock().map_err(|_| {
            wasmtime::Error::msg(format!("{subject} was left poisoned by an earlier call"))
        })?;

        // Guest -> plugin. A byte string is lifted out of its GC array and written through the plugin's allocator; a scalar is read straight off its slot.
        let mut arguments = Vec::new();
        for (index, crossing) in params.iter().enumerate() {
            let slot = &given[index..index + 1];

            match crossing {
                Crossing::Nat => {
                    arguments.push(Val::I64(u64::lift(&mut caller, slot)?.cast_signed()))
                }
                Crossing::Int => arguments.push(Val::I64(i64::lift(&mut caller, slot)?)),
                Crossing::Bool => {
                    arguments.push(Val::I32(u32::lift(&mut caller, slot)?.cast_signed()))
                }
                Crossing::Byte => arguments.push(Val::I32(i32::from(u8::lift(&mut caller, slot)?))),
                Crossing::Float => {
                    arguments.push(Val::F64(f64::lift(&mut caller, slot)?.to_bits()))
                }
                Crossing::Bytes => {
                    let bytes = Vec::<u8>::lift(&mut caller, slot)?;
                    let offset = plugin.write(&bytes).map_err(wasmtime::Error::msg)?;

                    arguments.push(Val::I32(offset as i32));
                    arguments.push(Val::I32(bytes.len() as i32));
                }
            }
        }

        // The instance is copied out first: `get_func` borrows the store mutably, and reaching through the plugin for both at once would borrow it twice.
        let instance = plugin.instance;
        let function = instance
            .get_func(&mut plugin.store, &export)
            .ok_or_else(|| wasmtime::Error::msg(format!("{subject} exports no `{export}`")))?;

        // A byte-string result is the `(ptr, len)` pair wasm multi-value returns; everything else is one slot, and nothing is two.
        let mut returned = vec![Val::I32(0); usize::from(result == Some(Crossing::Bytes)) + 1];
        let returned = match result {
            None => {
                function
                    .call(&mut plugin.store, &arguments, &mut [])
                    .map_err(|error| wasmtime::Error::msg(format!("`{export}`: {error}")))?;

                return Ok(());
            }
            Some(_) => {
                function
                    .call(&mut plugin.store, &arguments, &mut returned)
                    .map_err(|error| wasmtime::Error::msg(format!("`{export}`: {error}")))?;

                returned
            }
        };

        // Plugin -> guest, by the same reading in reverse.
        match result {
            Some(Crossing::Nat) => returned[0]
                .unwrap_i64()
                .cast_unsigned()
                .lower(&mut caller, answers),
            Some(Crossing::Int) => returned[0].unwrap_i64().lower(&mut caller, answers),
            Some(Crossing::Bool) => returned[0]
                .unwrap_i32()
                .cast_unsigned()
                .lower(&mut caller, answers),
            // A plugin answers a word of its own choosing, so one past 255 is refused here rather than truncated into a different byte.
            Some(Crossing::Byte) => {
                let word = returned[0].unwrap_i32();

                u8::try_from(word)
                    .map_err(|_| {
                        wasmtime::Error::msg(format!(
                            "{subject} answered `{export}` with {word}, which is not a byte"
                        ))
                    })?
                    .lower(&mut caller, answers)
            }
            Some(Crossing::Float) => returned[0].unwrap_f64().lower(&mut caller, answers),
            Some(Crossing::Bytes) => {
                let offset = returned[0].unwrap_i32() as u32;
                let length = returned[1].unwrap_i32() as u32;
                let bytes = plugin.read(offset, length).map_err(wasmtime::Error::msg)?;

                bytes.lower(&mut caller, answers)
            }
            None => Ok(()),
        }
    });

    Ok(())
}

/// Build the `ffi`-tier bindings a compilation needs from the modules its packages declared.
///
/// **The coverage rule is checked here, against the store rather than against the manifests.** The [`ForeignStore`] is what actually holds the rows the program imports, so a check over it cannot disagree with what the module demands — where a check over manifests would have to predict a declaration's qualified name from its source position, which is a second derivation of something the compilation already computed.
pub fn plugin_bindings(
    foreigns: ForeignStore,
    declared: Vec<DeclaredModule>,
) -> Result<ForeignBindings, String> {
    let rows = foreigns
        .iter()
        .map(|function| (function.name.clone(), Arc::clone(function)))
        .collect::<BTreeMap<_, _>>();

    // Which row each claim names, and who claimed it — the second half is what lets a duplicate name both claimants.
    let mut claimed: BTreeMap<String, (String, String)> = BTreeMap::new();
    let mut bindings = ForeignBindings::new(foreigns.clone());

    for module in &declared {
        let subject = format!("the module {:?} of {}", module.name, module.package);
        let instantiated = Plugin::instantiate(module)?;
        let exported = instantiated.exports.clone();
        let plugin = Arc::new(Mutex::new(instantiated));

        for (export, declaration) in &module.exports {
            let Some(row) = rows.get(declaration) else {
                return Err(format!(
                    "{subject} claims to answer `{declaration}`, which the program declares no `foreign` for"
                ));
            };

            if !exported.contains(export) {
                return Err(format!(
                    "{subject} claims `{export}` answers `{declaration}`, and exports no such name. It exports {}",
                    exported.iter().cloned().collect::<Vec<_>>().join(", ")
                ));
            }

            if let Some((package, name)) = claimed.get(declaration) {
                return Err(format!(
                    "`{declaration}` is claimed twice: by the module {name:?} of {package}, and by {subject}"
                ));
            }
            claimed.insert(
                declaration.clone(),
                (module.package.clone(), module.name.clone()),
            );

            define_row(&mut bindings, &plugin, export, row, &subject)?;
        }
    }

    // Every declaration the program holds, claimed. The refusal names the row and where to say so, since an unclaimed one is a manifest that has not caught up with a source file.
    for name in rows.keys() {
        if !claimed.contains_key(name) {
            return Err(format!(
                "no implementation for `{name}`: the program declares it `foreign`, and no `[foreign.exports]` in the package graph claims it. Add the export answering it to the `[[foreign]]` row of the package that declares it"
            ));
        }
    }

    Ok(bindings)
}
