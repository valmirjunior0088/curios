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
//! **An instance lives in the store of the program that calls it.** A module is resolved once — compiled or read back, refused if no plugin can be made of it, each export held to the declaration it answers — and instantiated where a program is run, in that program's own store, ahead of the program itself. It is still an instance and a memory of its own, which the program cannot name; what the two share is the engine's accounting, so the stack limit set where the program is entered bounds a module's frames too, and a recursion with no end inside one stops on the engine's trap wherever in the program it was called from. A store of its own would be handed the whole limit again below the frame that called it, of a stack that holds it once.
//!
//! The ABI is the raw one every wasm toolchain already emits: a scalar crosses as itself, a byte string as a `(ptr, len)` pair into the plugin's memory, and a byte-string result comes back as the same pair through wasm's multi-value return. The plugin exports its `memory` and an allocator; what it is called and what it takes are [`ALLOC`] and the conformance check below.

use {
    super::{Answers, ForeignBindings, Lift, Lower, Returned, shared_engine},
    curios_abi::{ForeignFunction, ForeignStore, WireReturn, WireType},
    std::{collections::BTreeMap, sync::Arc},
    wasmtime::{Caller, ExternType, Func, Linker, Memory, Module, Store, TypedFunc, Val},
};

/// The allocator a plugin exports, by the name every raw-ABI toolchain gives it: bytes in, an offset into the plugin's own memory out.
///
/// Nothing is freed. A plugin is instantiated per row and lives as long as the run, so a call's scratch is reclaimed when the run ends — and a `dealloc` the plugin may also export is deliberately not called, because pairing allocations across a boundary whose far side may have panicked is a way to corrupt a heap rather than to save one.
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

/// The allocator of a module that follows the canonical ABI, by the name that ABI gives it: the old offset and size, an alignment and the new size in, an offset out.
const REALLOC: &str = "cabi_realloc";

/// How a module hands out the memory a byte string crossing to it is written in, which is also which conventions its exports follow.
#[derive(Clone)]
enum Allocator {
    /// [`ALLOC`]: a size in, an offset out. Its byte-string result is the `(ptr, len)` pair of a multi-value return, and it answers one result or none.
    Own(TypedFunc<u32, u32>),
    /// [`REALLOC`]. A result that is no single scalar comes back as one pointer to an area holding each field as the canonical ABI lays it out, released through the export's post-return — the shape a bindings generator emits for a core module, so a module built from generated bindings is linked as it is.
    Canonical(TypedFunc<(u32, u32, u32, u32), u32>),
}

/// The core types of a function, as a refusal spells them: its parameters, then its results.
type Spelled = (Vec<String>, Vec<String>);

/// A module's export named `name`, if it is a function, as its core types.
fn exported(module: &Module, name: &str) -> Option<Spelled> {
    match module.get_export(name)? {
        ExternType::Func(function) => Some((
            function.params().map(|slot| slot.to_string()).collect(),
            function.results().map(|slot| slot.to_string()).collect(),
        )),
        _ => None,
    }
}

/// `slots` as owned strings, for comparing against what a module exports.
fn spelled(slots: &[&str]) -> Vec<String> {
    slots.iter().map(|slot| slot.to_string()).collect()
}

/// One declaration a module answers: the export that answers it, and how each value crosses.
struct Row {
    function: Arc<ForeignFunction>,
    export: String,
    params: Vec<Crossing>,
    returns: Returns,
}

/// A module resolved against the rows a program reaches: compiled or read back, refused if no plugin can be made of it, and each export that answers a reached row held to that row's declaration. Nothing is instantiated here: an instance is made where a program is run ([`Plugin::instantiate`]), so one resolution serves every run of the program.
pub(crate) struct Plugin {
    /// What a refusal names: the module, and the package whose manifest declared it.
    subject: String,
    compiled: Module,
    /// Whether the module follows the canonical ABI's conventions, which its allocator's name says.
    canonical: bool,
    rows: Vec<Row>,
}

impl Plugin {
    /// Compile or read back `module`, refusing one that is not a plugin before it can become one.
    ///
    /// The checks are the whole of what "a plugin" means, each made of the module's own types and refused by name rather than left to a `LinkError` that would report one missing import and stop.
    fn of(module: &DeclaredModule) -> Result<Self, String> {
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

        // Every import at once, rather than whichever one a linker would reach first.
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

        if !matches!(compiled.get_export(MEMORY), Some(ExternType::Memory(_))) {
            return Err(format!(
                "{subject} exports no `{MEMORY}`, which is where a byte string crossing to it is written"
            ));
        }

        let canonical = if exported(&compiled, REALLOC)
            == Some((spelled(&["i32"; 4]), spelled(&["i32"])))
        {
            true
        } else if exported(&compiled, ALLOC) == Some((spelled(&["i32"]), spelled(&["i32"]))) {
            false
        } else {
            return Err(format!(
                "{subject} exports neither `{REALLOC}` nor an `{ALLOC}` taking a size and answering an offset, which is how a byte string reaches its memory"
            ));
        };

        Ok(Self {
            subject,
            compiled,
            canonical,
            rows: Vec::new(),
        })
    }

    /// The names the module exports, for a refusal to list.
    fn exports(&self) -> Vec<String> {
        self.compiled
            .exports()
            .map(|export| export.name().to_string())
            .collect()
    }

    /// Instantiate the module into `store`, the store of the program about to run, and answer in `answers` each row it was resolved for.
    ///
    /// A trampoline holds the instance's handles and enters them through the caller it is called with, which is that same store: the module's frames are counted where the program's are.
    pub(crate) fn instantiate(
        self: &Arc<Self>,
        store: &mut Store<()>,
        answers: &mut Answers,
    ) -> Result<(), String> {
        let subject = &self.subject;

        // Instantiating runs the module's start function, an entry into it like any other, on the stack the program is about to run on.
        let instance = Linker::new(shared_engine())
            .instantiate(&mut *store, &self.compiled)
            .map_err(|error| {
                format!(
                    "{subject} could not be instantiated: {}",
                    error.root_cause()
                )
            })?;

        // What `of` read off the module's types is found again on the instance: a failure here is this file's own.
        let missing =
            |what: &str| format!("{subject} lost its `{what}` between its types and its instance");
        let memory = instance
            .get_memory(&mut *store, MEMORY)
            .ok_or_else(|| missing(MEMORY))?;
        let allocator = match self.canonical {
            true => Allocator::Canonical(
                instance
                    .get_typed_func(&mut *store, REALLOC)
                    .map_err(|_| missing(REALLOC))?,
            ),
            false => Allocator::Own(
                instance
                    .get_typed_func(&mut *store, ALLOC)
                    .map_err(|_| missing(ALLOC))?,
            ),
        };
        let instantiated = Instantiated { memory, allocator };

        for (index, row) in self.rows.iter().enumerate() {
            let function = instance
                .get_func(&mut *store, &row.export)
                .ok_or_else(|| missing(&row.export))?;
            let released = instance.get_func(&mut *store, &format!("cabi_post_{}", row.export));
            let plugin = Arc::clone(self);
            let instantiated = instantiated.clone();
            let signature = row.function.signature();

            answers.answer_raw(
                row.function.name().to_string(),
                signature.params.clone(),
                Returned::declared(&signature.returns),
                move |mut caller, given, answered| {
                    plugin.rows[index].call(
                        &plugin.subject,
                        &instantiated,
                        function,
                        released,
                        &mut caller,
                        given,
                        answered,
                    )
                },
            );
        }

        Ok(())
    }
}

/// A module's instance in the store of the program that calls it: its memory and the allocator every byte string goes through. Both are handles into that store, so a call reaches the instance through the caller it arrives with.
#[derive(Clone)]
struct Instantiated {
    memory: Memory,
    allocator: Allocator,
}

/// A call into the module that failed, as the refusal a run reports: who failed, in what, and the cause at the root of the engine's chain. A trap's own display is its frames, so the root is the sentence that says what happened.
fn failed(subject: &str, what: &str, error: wasmtime::Error) -> wasmtime::Error {
    wasmtime::Error::msg(format!(
        "{subject} failed in `{what}`: {}",
        error.root_cause()
    ))
}

impl Instantiated {
    /// Copy `bytes` into the module's memory through its allocator, answering the offset they landed at.
    fn write(
        &self,
        subject: &str,
        caller: &mut Caller<'_, ()>,
        bytes: &[u8],
    ) -> wasmtime::Result<u32> {
        let length = u32::try_from(bytes.len()).map_err(|_| {
            wasmtime::Error::msg(format!(
                "a byte string of {} bytes is too long to cross to {subject}",
                bytes.len()
            ))
        })?;

        // The module owns what it hands out: a call's argument is its to release, as the canonical ABI has it, so nothing is freed from this side.
        let offset = match &self.allocator {
            Allocator::Own(alloc) => alloc
                .call(&mut *caller, length)
                .map_err(|error| failed(subject, ALLOC, error))?,
            Allocator::Canonical(realloc) => realloc
                .call(&mut *caller, (0, 0, 1, length))
                .map_err(|error| failed(subject, REALLOC, error))?,
        };

        self.memory
            .write(&mut *caller, offset as usize, bytes)
            .map_err(|_| {
                wasmtime::Error::msg(format!(
                    "{subject} allocated {length} bytes at {offset}, past the end of its memory"
                ))
            })?;

        Ok(offset)
    }

    /// Read `length` bytes back out of the module's memory at `offset`, which its export `export` answered.
    fn read(
        &self,
        subject: &str,
        export: &str,
        caller: &mut Caller<'_, ()>,
        offset: u32,
        length: u32,
    ) -> wasmtime::Result<Vec<u8>> {
        let mut bytes = vec![0; length as usize];

        self.memory
            .read(&mut *caller, offset as usize, &mut bytes)
            .map_err(|_| {
                wasmtime::Error::msg(format!(
                    "{subject} answered `{export}` with {length} bytes at {offset}, past the end of its memory"
                ))
            })?;

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

impl Crossing {
    /// The core types a value of this crossing takes in a module's signature, as [`crossing`] states them.
    fn slots(self) -> &'static [&'static str] {
        match self {
            Crossing::Nat | Crossing::Int => &["i64"],
            Crossing::Bool | Crossing::Byte => &["i32"],
            Crossing::Float => &["f64"],
            Crossing::Bytes => &["i32", "i32"],
        }
    }

    /// The size and the alignment a value of this crossing has in a module's memory, as the canonical ABI lays a field out: a byte string is its offset and its length, four bytes each.
    fn layout(self) -> (u32, u32) {
        match self {
            Crossing::Nat | Crossing::Int | Crossing::Float => (8, 8),
            Crossing::Bool | Crossing::Byte => (1, 1),
            Crossing::Bytes => (8, 4),
        }
    }
}

/// How a module's export returns what it answers.
#[derive(Clone, PartialEq, Eq)]
enum Returns {
    Nothing,
    /// One scalar, as itself.
    Scalar(Crossing),
    /// A byte string as the `(ptr, len)` pair of a multi-value return: what a module with an [`ALLOC`] of its own answers.
    Pair,
    /// Whatever else a module that follows the canonical ABI answers — a byte string, or several results — as one pointer to an area holding each field in order.
    Area(Vec<Crossing>),
}

impl Returns {
    /// The core types the export's results have.
    fn slots(&self) -> &'static [&'static str] {
        match self {
            Returns::Nothing => &[],
            Returns::Scalar(crossing) => crossing.slots(),
            Returns::Pair => &["i32", "i32"],
            Returns::Area(_) => &["i32"],
        }
    }
}

/// Lower the scalar a module answered as `value` into the guest's slot, refusing a word its type does not admit.
fn lower_scalar(
    crossing: Crossing,
    value: Val,
    caller: &mut Caller<'_, ()>,
    slot: &mut [Val],
    subject: &str,
    export: &str,
) -> wasmtime::Result<()> {
    match crossing {
        Crossing::Nat => value.unwrap_i64().cast_unsigned().lower(caller, slot),
        Crossing::Int => value.unwrap_i64().lower(caller, slot),
        // A module's `Bool` is a word of its own choosing, so anything but `0` or `1` is refused rather than read as true.
        Crossing::Bool => match value.unwrap_i32() {
            0 => false.lower(caller, slot),
            1 => true.lower(caller, slot),
            word => Err(wasmtime::Error::msg(format!(
                "{subject} answered `{export}` with {word}, which is not a Bool"
            ))),
        },
        // A word past 255 is refused here rather than truncated into a different byte.
        Crossing::Byte => {
            let word = value.unwrap_i32();

            u8::try_from(word)
                .map_err(|_| {
                    wasmtime::Error::msg(format!(
                        "{subject} answered `{export}` with {word}, which is not a byte"
                    ))
                })?
                .lower(caller, slot)
        }
        Crossing::Float => value.unwrap_f64().lower(caller, slot),
        Crossing::Bytes => unreachable!("a byte string is no scalar"),
    }
}

impl Row {
    /// Hold `export` of `plugin` to the declaration `function`, where the two are resolved: a module that answers another type is refused before the program runs, and not at the first call that reaches it.
    fn of(plugin: &Plugin, export: &str, function: &Arc<ForeignFunction>) -> Result<Self, String> {
        let subject = &plugin.subject;
        let declaration = function.name();
        let signature = function.signature();

        let params = signature
            .params
            .iter()
            .map(|wire| crossing(*wire, subject, declaration))
            .collect::<Result<Vec<_>, _>>()?;

        // Ending the instance is the host's to do, or an embedder's: a module reaches no environment, and its own trap is a failure of the call.
        let WireReturn::Answers(results) = &signature.returns else {
            return Err(format!(
                "{subject} answers `{declaration}`, which does not return. A foreign module answers a value or none, and only the host ends the program"
            ));
        };

        let fields = results
            .types()
            .iter()
            .map(|wire| crossing(*wire, subject, declaration))
            .collect::<Result<Vec<_>, _>>()?;

        let returns = match (fields.as_slice(), plugin.canonical) {
            ([], _) => Returns::Nothing,
            ([Crossing::Bytes], false) => Returns::Pair,
            ([scalar], _) if *scalar != Crossing::Bytes => Returns::Scalar(*scalar),
            (fields, true) => Returns::Area(fields.to_vec()),
            // Several results at once have no spelling in a multi-value return a toolchain emits, which is what the canonical ABI's area is for.
            (_, false) => {
                return Err(format!(
                    "{subject} answers `{declaration}`, which yields several results. A module with an `{ALLOC}` of its own answers one result or none; one that follows the canonical ABI, exporting `{REALLOC}`, answers several"
                ));
            }
        };

        let found = exported(&plugin.compiled, export)
            .ok_or_else(|| format!("{subject} exports `{export}`, which is no function"))?;
        let expected = (
            params
                .iter()
                .flat_map(|crossing| crossing.slots())
                .map(|slot| slot.to_string())
                .collect::<Vec<_>>(),
            spelled(returns.slots()),
        );

        if expected != found {
            return Err(format!(
                "{subject} exports `{export}` as ({}) -> ({}), and `{declaration}` crosses to a module as ({}) -> ({})",
                found.0.join(", "),
                found.1.join(", "),
                expected.0.join(", "),
                expected.1.join(", "),
            ));
        }

        Ok(Self {
            function: Arc::clone(function),
            export: export.to_string(),
            params,
            returns,
        })
    }

    /// One call of the row: the operands marshalled out of the guest and into the module's memory, `function` entered, and what it answers marshalled back.
    #[allow(clippy::too_many_arguments)]
    fn call(
        &self,
        subject: &str,
        instantiated: &Instantiated,
        function: Func,
        released: Option<Func>,
        caller: &mut Caller<'_, ()>,
        given: &[Val],
        answers: &mut [Val],
    ) -> wasmtime::Result<()> {
        let export = self.export.as_str();

        // Guest -> plugin. A byte string is lifted out of its GC array and written through the plugin's allocator; a scalar is read straight off its slot.
        let mut arguments = Vec::new();
        for (index, crossing) in self.params.iter().enumerate() {
            let slot = &given[index..index + 1];

            match crossing {
                Crossing::Nat => arguments.push(Val::I64(u64::lift(caller, slot)?.cast_signed())),
                Crossing::Int => arguments.push(Val::I64(i64::lift(caller, slot)?)),
                Crossing::Bool => arguments.push(Val::I32(i32::from(bool::lift(caller, slot)?))),
                Crossing::Byte => arguments.push(Val::I32(i32::from(u8::lift(caller, slot)?))),
                Crossing::Float => arguments.push(Val::F64(f64::lift(caller, slot)?.to_bits())),
                Crossing::Bytes => {
                    let bytes = Vec::<u8>::lift(caller, slot)?;
                    let offset = instantiated.write(subject, caller, &bytes)?;

                    arguments.push(Val::I32(offset as i32));
                    arguments.push(Val::I32(bytes.len() as i32));
                }
            }
        }

        let mut returned = vec![Val::I32(0); self.returns.slots().len()];

        function
            .call(&mut *caller, &arguments, &mut returned)
            .map_err(|error| failed(subject, export, error))?;

        // Plugin -> guest, by the same reading in reverse.
        match &self.returns {
            Returns::Nothing => Ok(()),
            Returns::Scalar(crossing) => {
                lower_scalar(*crossing, returned[0], caller, answers, subject, export)
            }
            Returns::Pair => {
                let offset = returned[0].unwrap_i32() as u32;
                let length = returned[1].unwrap_i32() as u32;

                instantiated
                    .read(subject, export, caller, offset, length)?
                    .lower(caller, answers)
            }
            // Each field where the canonical ABI lays it: after the ones before it, at its own alignment. The module is told once every field is read, so it may release what the area holds.
            Returns::Area(fields) => {
                let area = returned[0].unwrap_i32() as u32;
                let mut offset = 0;

                for (index, field) in fields.iter().enumerate() {
                    let (size, alignment) = field.layout();
                    offset = u32::next_multiple_of(offset, alignment);

                    let held = instantiated.read(subject, export, caller, area + offset, size)?;
                    let slot = &mut answers[index..index + 1];

                    match field {
                        Crossing::Bytes => {
                            let word = |at: usize| {
                                u32::from_le_bytes(held[at..at + 4].try_into().expect("four bytes"))
                            };

                            instantiated
                                .read(subject, export, caller, word(0), word(4))?
                                .lower(caller, slot)?
                        }
                        Crossing::Nat | Crossing::Int => lower_scalar(
                            *field,
                            Val::I64(i64::from_le_bytes(
                                held[..].try_into().expect("eight bytes"),
                            )),
                            caller,
                            slot,
                            subject,
                            export,
                        )?,
                        Crossing::Float => lower_scalar(
                            *field,
                            Val::F64(u64::from_le_bytes(
                                held[..].try_into().expect("eight bytes"),
                            )),
                            caller,
                            slot,
                            subject,
                            export,
                        )?,
                        Crossing::Bool | Crossing::Byte => lower_scalar(
                            *field,
                            Val::I32(i32::from(held[0])),
                            caller,
                            slot,
                            subject,
                            export,
                        )?,
                    }

                    offset += size;
                }

                if let Some(released) = released {
                    released
                        .call(&mut *caller, &[Val::I32(area as i32)], &mut [])
                        .map_err(|error| failed(subject, &format!("cabi_post_{export}"), error))?;
                }

                Ok(())
            }
        }
    }
}

/// Resolve the modules a program's packages declared against `foreigns`, the rows the program reaches, into the bindings a run of it instantiates them from.
///
/// **The coverage rule is checked against the store rather than against the manifests.** The [`ForeignStore`] is what actually holds the rows the program imports, so a check over it cannot disagree with what the module demands — where a check over manifests would have to predict a declaration's qualified name from its source position, which is a second derivation of something the compilation already computed.
pub fn plugin_bindings(
    foreigns: &ForeignStore,
    declared: Vec<DeclaredModule>,
) -> Result<ForeignBindings, String> {
    // Which claim took each row: the export, and the module it is of — what lets a second claim name both.
    let mut claimed: BTreeMap<String, (String, String)> = BTreeMap::new();
    let mut bindings = ForeignBindings::empty();

    for module in &declared {
        let mut resolved = Plugin::of(module)?;

        for (export, declaration) in &module.exports {
            // The rows handed over are the ones the program reaches, so a claim on another is a claim nothing asks about here.
            let Some(function) = foreigns.get(declaration) else {
                continue;
            };

            let exports = resolved.exports();
            if !exports.contains(export) {
                return Err(format!(
                    "{} claims `{export}` answers `{declaration}`, and exports no such name. It exports {}",
                    resolved.subject,
                    exports.join(", ")
                ));
            }

            if let Some((first, subject)) = claimed.get(declaration) {
                return Err(format!(
                    "`{declaration}` is claimed twice: by `{first}` of {subject}, and by `{export}` of {}",
                    resolved.subject
                ));
            }
            claimed.insert(
                declaration.clone(),
                (export.clone(), resolved.subject.clone()),
            );

            let row = Row::of(&resolved, export, function)?;
            resolved.rows.push(row);
        }

        bindings.modules.push(Arc::new(resolved));
    }

    // A row no module claims is left for the link, which follows what the program imports: the host answers it there, or it is refused by name where the program reaches it. A declaration nothing reaches asks for no implementation.
    Ok(bindings)
}
