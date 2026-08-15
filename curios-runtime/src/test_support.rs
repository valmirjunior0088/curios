//! Driving a wasm module by hand, without naming wasmtime.
//!
//! Instantiating a module and poking at its exports is what a *test* of an emitted module does — `curios-js`'s bridge suite builds the JavaScript-side accessor module and round-trips bytes through it. That work needs `Store`, `Instance`, `Linker`, `Func`, `Val` and `Memory`, none of which any product code in this workspace touches, and exposing them for a test's sake would put wasmtime's vocabulary back into a crate that has no other use for it.
//!
//! So this module hands out two opaque types instead. [`GuestValue`](crate::test_support::GuestValue) carries a wasm value the caller cannot inspect except as an `i32` — which is what lets a test thread an opaque GC reference from one call into the next, the thing a scalar-only API could not express — and [`GuestInstance`](crate::test_support::GuestInstance) owns the store and the instance together, so no lifetime or borrow of wasmtime's escapes. Both are spelled from the crate root because this module's declaration carries a doc comment of its own, and a merged one resolves in the scope the outer half was written in.
//!
//! **Behind `test-support`, which implies `cranelift`:** instantiating compiles, so this cannot exist in a runtime-only build. That it is a feature rather than `#[cfg(test)]` is not a stylistic choice — `cfg(test)` is set only while *this* crate is compiled as its own test harness, so a `cfg(test)` item is invisible to another crate's tests, which is exactly the case here.
//!
//! Calls are untyped, and deliberately: they type-check dynamically against the module's declared signatures, so a call succeeding *is* the assertion that the emitted shapes are right.

use {
    super::shared_engine,
    wasmtime::{Instance, Linker, Memory, Module, Store, Val},
};

/// One wasm value crossing into or out of a guest call, opaque by construction.
///
/// A test builds these from `i32`s and reads them back the same way, but it may also hold one it cannot interpret — a GC reference the guest minted — and pass it into a later call. That is the whole reason this is a type rather than an `i32`.
#[derive(Debug, Clone, Copy)]
pub struct GuestValue(Val);

impl GuestValue {
    pub fn from_i32(value: i32) -> Self {
        Self(Val::I32(value))
    }

    /// The value as an `i32`, or `None` if the guest returned something else. Never panics: a wrong shape is a fact about the module under test, which is a thing to assert on rather than abort over.
    pub fn to_i32(self) -> Option<i32> {
        match self.0 {
            Val::I32(value) => Some(value),
            _ => None,
        }
    }
}

/// An instantiated module together with the store it lives in, so neither escapes into a caller's signature.
pub struct GuestInstance {
    store: Store<()>,
    instance: Instance,
}

impl GuestInstance {
    /// Compile and instantiate `bytes` against the shared engine — the same engine configuration a compiled program runs under, so the module is judged by the settings that will actually host it. Imports nothing: a module needing any is a mismatch this reports rather than papers over.
    pub fn instantiate(bytes: &[u8]) -> Result<Self, String> {
        let engine = shared_engine();
        let module = Module::new(engine, bytes).map_err(|error| error.to_string())?;
        let mut store = Store::new(engine, ());
        let instance = Linker::new(engine)
            .instantiate(&mut store, &module)
            .map_err(|error| error.to_string())?;

        Ok(Self { store, instance })
    }

    /// Call `export` with `args`, returning as many results as its signature declares.
    ///
    /// The call is untyped, which is the point: wasmtime checks the arguments against the declared signature at the boundary, so a mismatched shape fails here rather than silently coercing.
    pub fn call(&mut self, export: &str, args: &[GuestValue]) -> Result<Vec<GuestValue>, String> {
        let function = self
            .instance
            .get_func(&mut self.store, export)
            .ok_or_else(|| format!("missing func export `{export}`"))?;

        let arity = function.ty(&self.store).results().len();
        let args = args.iter().map(|value| value.0).collect::<Vec<_>>();
        let mut results = vec![Val::I32(0); arity];

        function
            .call(&mut self.store, &args, &mut results)
            .map_err(|error| format!("calling `{export}`: {error}"))?;

        Ok(results.into_iter().map(GuestValue).collect())
    }

    /// The size of the memory exported as `export`, in pages.
    pub fn memory_size(&mut self, export: &str) -> Result<u64, String> {
        let memory = self.memory(export)?;

        Ok(memory.size(&self.store))
    }

    /// Grow the memory exported as `export` by `pages`.
    pub fn memory_grow(&mut self, export: &str, pages: u64) -> Result<(), String> {
        let memory = self.memory(export)?;

        memory
            .grow(&mut self.store, pages)
            .map(|_| ())
            .map_err(|error| format!("growing `{export}`: {error}"))
    }

    pub fn memory_write(
        &mut self,
        export: &str,
        offset: usize,
        bytes: &[u8],
    ) -> Result<(), String> {
        let memory = self.memory(export)?;

        memory
            .write(&mut self.store, offset, bytes)
            .map_err(|error| format!("writing `{export}`: {error}"))
    }

    pub fn memory_read(
        &mut self,
        export: &str,
        offset: usize,
        into: &mut [u8],
    ) -> Result<(), String> {
        let memory = self.memory(export)?;

        memory
            .read(&self.store, offset, into)
            .map_err(|error| format!("reading `{export}`: {error}"))
    }

    fn memory(&mut self, export: &str) -> Result<Memory, String> {
        self.instance
            .get_memory(&mut self.store, export)
            .ok_or_else(|| format!("missing memory export `{export}`"))
    }
}
