use {
    super::{Io, Status},
    wasmtime::{
        AnyRef, ArrayRef, ArrayRefPre, ArrayType, Caller, FieldType, HeapType, I31, Mutability,
        RefType, StorageType, Val, ValType,
    },
};

pub trait Lower {
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
    fn lower(self, caller: &mut Caller<'_, ()>, results: &mut [Val]) -> Result<(), wasmtime::Error> {
        self.code().lower(caller, results)
    }
}

/// A descriptor lowers as its `u32` wire token (an i31).
impl Lower for Io {
    fn lower(self, caller: &mut Caller<'_, ()>, results: &mut [Val]) -> Result<(), wasmtime::Error> {
        self.token().lower(caller, results)
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

        b.lower(caller, &mut results[1..2])
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

        c.lower(caller, &mut results[2..3])
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

/// `Arr(Bin)`: an array of `anyref` whose elements are `Bin`s (`i8` arrays). The
/// outer element type `(mut (ref null any))` matches the codegen's uniform
/// `arr_type`, so the array's runtime type is the one downstream `ref.cast`s
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

        let mut elements = Vec::with_capacity(self.len());
        for bytes in self {
            let bin = ArrayRef::new_fixed(
                &mut *caller,
                &byte_pre,
                &bytes
                    .into_iter()
                    .map(|byte| Val::I32(byte as i32))
                    .collect::<Vec<_>>(),
            )?;
            elements.push(Val::AnyRef(Some(bin.to_anyref())));
        }

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
