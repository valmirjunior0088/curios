use {
    super::{Io, Poll, Status},
    wasmtime::{
        AnyRef, ArrayRef, ArrayRefPre, ArrayType, Caller, FieldType, HeapType, I31, Mutability,
        RefType, StorageType, Val, ValType,
    },
};

/// Encoding one host-import result list into wasmtime `Val`s — the outbound half of the FFI boundary (`Lift` is the inbound half). Each impl produces the exact GC shape the generated code expects on that wire type (i31-boxed scalars, `Bin` as an i8 array, `Lst` as an anyref-element array), so a host function returns plain Rust values and the trampoline lands them in wasm-typed result slots.
pub trait Lower {
    /// Encode `self` into the import's `results` slots, allocating any GC values through `caller`. Contract: every single-value impl fills exactly `results[0]` — the alignment the tuple impls rely on to re-slice per component.
    fn lower(self, caller: &mut Caller<'_, ()>, results: &mut [Val])
    -> Result<(), wasmtime::Error>;
}

impl Lower for () {
    fn lower(self, _: &mut Caller<'_, ()>, _: &mut [Val]) -> Result<(), wasmtime::Error> {
        Ok(())
    }
}

/// A Curios IO status lowers as its `u32` wire code (an i31).
impl Lower for Status {
    fn lower(
        self,
        caller: &mut Caller<'_, ()>,
        results: &mut [Val],
    ) -> Result<(), wasmtime::Error> {
        self.code().lower(caller, results)
    }
}

/// A descriptor lowers as its wire token bytes — a `Bin` (an i8 array), the same
/// uniform shape the runtime keys handles on.
impl Lower for Io {
    fn lower(
        self,
        caller: &mut Caller<'_, ()>,
        results: &mut [Val],
    ) -> Result<(), wasmtime::Error> {
        self.bytes().lower(caller, results)
    }
}

/// Scalar results cross the boundary pre-boxed as i31 refs so generated code
/// can land them directly in anyref block params (see `emit_sys_imports`).
impl Lower for u32 {
    fn lower(
        self,
        caller: &mut Caller<'_, ()>,
        results: &mut [Val],
    ) -> Result<(), wasmtime::Error> {
        results[0] = Val::AnyRef(Some(AnyRef::from_i31(
            &mut *caller,
            I31::wrapping_u32(self),
        )));

        Ok(())
    }
}

// Pairs lower positionally: each component fills one result slot. (Every
// single-value impl writes `results[0]`, so slicing re-aligns them.)
impl<A: Lower, B: Lower> Lower for (A, B) {
    fn lower(
        self,
        caller: &mut Caller<'_, ()>,
        results: &mut [Val],
    ) -> Result<(), wasmtime::Error> {
        let (a, b) = self;
        a.lower(caller, &mut results[0..1])?;
        b.lower(caller, &mut results[1..2])?;

        Ok(())
    }
}

impl<A: Lower, B: Lower, C: Lower> Lower for (A, B, C) {
    fn lower(
        self,
        caller: &mut Caller<'_, ()>,
        results: &mut [Val],
    ) -> Result<(), wasmtime::Error> {
        let (a, b, c) = self;
        a.lower(caller, &mut results[0..1])?;
        b.lower(caller, &mut results[1..2])?;
        c.lower(caller, &mut results[2..3])?;

        Ok(())
    }
}

impl Lower for Vec<u8> {
    fn lower(
        self,
        caller: &mut Caller<'_, ()>,
        results: &mut [Val],
    ) -> Result<(), wasmtime::Error> {
        let array_type = ArrayType::new(
            caller.engine(),
            FieldType::new(Mutability::Var, StorageType::I8),
        );

        let array_ref_pre = ArrayRefPre::new(&mut *caller, array_type);

        results[0] = Val::AnyRef(Some(
            ArrayRef::new_fixed(
                &mut *caller,
                &array_ref_pre,
                &self
                    .into_iter()
                    .map(|byte| Val::I32(byte as i32))
                    .collect::<Vec<_>>(),
            )?
            .to_anyref(),
        ));

        Ok(())
    }
}

/// `Lst(Nat)`: `poll`'s parallel `revents` masks, lowered as an array of
/// i31-boxed bits. Same uniform `Lst` shape as `Vec<Vec<u8>>` below (anyref
/// elements over the codegen's `lst_type`), only the elements are i31s rather
/// than `Bin`s — the outbound dual of `lift.rs`'s `lift_i31_array`.
impl Lower for Vec<Poll> {
    fn lower(
        self,
        caller: &mut Caller<'_, ()>,
        results: &mut [Val],
    ) -> Result<(), wasmtime::Error> {
        let outer_type = ArrayType::new(
            caller.engine(),
            FieldType::new(
                Mutability::Var,
                StorageType::ValType(ValType::Ref(RefType::new(true, HeapType::Any))),
            ),
        );

        let outer_pre = ArrayRefPre::new(&mut *caller, outer_type);

        let elements = self
            .into_iter()
            .map(|mask| {
                Val::AnyRef(Some(AnyRef::from_i31(
                    &mut *caller,
                    I31::wrapping_u32(mask.bits()),
                )))
            })
            .collect::<Vec<_>>();

        results[0] = Val::AnyRef(Some(
            ArrayRef::new_fixed(&mut *caller, &outer_pre, &elements)?.to_anyref(),
        ));

        Ok(())
    }
}

/// `Lst(Bin)`: an array of `anyref` whose elements are `Bin`s (`i8` arrays). The
/// outer element type `(mut (ref null any))` matches the codegen's uniform
/// `lst_type`, so the array's runtime type is the one downstream `ref.cast`s
/// expect.
impl Lower for Vec<Vec<u8>> {
    fn lower(
        self,
        caller: &mut Caller<'_, ()>,
        results: &mut [Val],
    ) -> Result<(), wasmtime::Error> {
        let byte_type = ArrayType::new(
            caller.engine(),
            FieldType::new(Mutability::Var, StorageType::I8),
        );

        let byte_pre = ArrayRefPre::new(&mut *caller, byte_type);

        let elements = self
            .into_iter()
            .map(|bytes| {
                Ok(Val::AnyRef(Some(
                    ArrayRef::new_fixed(
                        &mut *caller,
                        &byte_pre,
                        &bytes
                            .into_iter()
                            .map(|byte| Val::I32(byte as i32))
                            .collect::<Vec<_>>(),
                    )?
                    .to_anyref(),
                )))
            })
            .collect::<Result<Vec<_>, wasmtime::Error>>()?;

        let outer_type = ArrayType::new(
            caller.engine(),
            FieldType::new(
                Mutability::Var,
                StorageType::ValType(ValType::Ref(RefType::new(true, HeapType::Any))),
            ),
        );

        let outer_pre = ArrayRefPre::new(&mut *caller, outer_type);

        results[0] = Val::AnyRef(Some(
            ArrayRef::new_fixed(&mut *caller, &outer_pre, &elements)?.to_anyref(),
        ));

        Ok(())
    }
}
