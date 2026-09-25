use {
    super::{Handle, Poll, Status},
    wasmtime::{
        ArrayRef, ArrayRefPre, ArrayType, Caller, Engine, FieldType, HeapType, Mutability, RefType,
        StorageType, Val, ValType,
    },
};

/// Encoding one host-import result list into wasmtime `Val`s — the outbound half of the FFI boundary (`Lift` is the inbound half). Each impl produces the exact shape the generated code expects on that wire type (a scalar as its raw number, `Bytes` as an i8 array, `List` as an anyref-element array), so a host function returns plain Rust values and the trampoline lands them in wasm-typed result slots.
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

/// A Curios IO status lowers as its wire code, a `Nat`.
impl Lower for Status {
    fn lower(
        self,
        caller: &mut Caller<'_, ()>,
        results: &mut [Val],
    ) -> Result<(), wasmtime::Error> {
        self.code().lower(caller, results)
    }
}

/// A descriptor lowers as its wire token bytes — a `Bytes` (an i8 array), the same uniform shape the runtime keys handles on.
impl Lower for Handle {
    fn lower(
        self,
        caller: &mut Caller<'_, ()>,
        results: &mut [Val],
    ) -> Result<(), wasmtime::Error> {
        self.bytes().lower(caller, results)
    }
}

/// An `Int` result, as the raw `i64` it is: the guest boxes it after the call, into the i31 or past it into the boxed magnitude, neither of which this side needs to know.
impl Lower for i64 {
    fn lower(self, _: &mut Caller<'_, ()>, results: &mut [Val]) -> Result<(), wasmtime::Error> {
        results[0] = Val::I64(self);

        Ok(())
    }
}

/// A `Nat` result, as the raw `i64` its bits fill: the guest reads them unsigned and boxes the number, so every `u64` crosses whole.
impl Lower for u64 {
    fn lower(self, _: &mut Caller<'_, ()>, results: &mut [Val]) -> Result<(), wasmtime::Error> {
        results[0] = Val::I64(self.cast_signed());

        Ok(())
    }
}

/// A `Byte` result, as its word. The type holds it below 256, which is the range the guest refuses a reply outside of.
impl Lower for u8 {
    fn lower(self, _: &mut Caller<'_, ()>, results: &mut [Val]) -> Result<(), wasmtime::Error> {
        results[0] = Val::I32(i32::from(self));

        Ok(())
    }
}

/// A `Bool` result, as its word `0` or `1`.
impl Lower for u32 {
    fn lower(self, _: &mut Caller<'_, ()>, results: &mut [Val]) -> Result<(), wasmtime::Error> {
        results[0] = Val::I32(self.cast_signed());

        Ok(())
    }
}

/// An `Flt` result, as the raw binary64 it is: the guest wraps it in the `Flt` struct after the call, whose shape is `curios-emit`'s and nothing here should have to know.
impl Lower for f64 {
    fn lower(self, _: &mut Caller<'_, ()>, results: &mut [Val]) -> Result<(), wasmtime::Error> {
        results[0] = Val::F64(self.to_bits());

        Ok(())
    }
}

/// Tuples lower positionally: each component fills one result slot, and slicing re-aligns the single-value impls, which all write `results[0]`. Arities two through seven — past `file_stat`'s five results, the widest row, so an embedder's own record result has room.
macro_rules! lower_tuple {
    ($($name:ident $value:ident $index:tt),+) => {
        impl<$($name: Lower),+> Lower for ($($name,)+) {
            fn lower(
                self,
                caller: &mut Caller<'_, ()>,
                results: &mut [Val],
            ) -> Result<(), wasmtime::Error> {
                let ($($value,)+) = self;
                $($value.lower(caller, &mut results[$index..$index + 1])?;)+

                Ok(())
            }
        }
    };
}

lower_tuple!(A a 0, B b 1);
lower_tuple!(A a 0, B b 1, C c 2);
lower_tuple!(A a 0, B b 1, C c 2, D d 3);
lower_tuple!(A a 0, B b 1, C c 2, D d 3, E e 4);
lower_tuple!(A a 0, B b 1, C c 2, D d 3, E e 4, F f 5);
lower_tuple!(A a 0, B b 1, C c 2, D d 3, E e 4, F f 5, G g 6);

/// The GC array type an `i8` byte array (`Bytes`) allocates under — shared by every wire shape that carries raw bytes, whether directly (`Vec<u8>`) or as `List(Bytes)`'s per-element `Bytes` (`Vec<Vec<u8>>`), and by `engine.rs`'s `host_func_type`, which describes the same shape for a host import's static function type.
pub(crate) fn i8_array_type(engine: &Engine) -> ArrayType {
    ArrayType::new(engine, FieldType::new(Mutability::Var, StorageType::I8))
}

/// The GC array type a list of references allocates under — a `(mut (ref null any))` element array, shared by `Vec<Vec<u8>>`'s `Bytes` and `Vec<Handle>`'s tokens, and by `engine.rs`'s `host_func_type` for the same reason. A list of scalars crosses as [`longs_array_type`] or [`words_array_type`] instead.
pub(crate) fn anyref_array_type(engine: &Engine) -> ArrayType {
    ArrayType::new(
        engine,
        FieldType::new(
            Mutability::Var,
            StorageType::ValType(ValType::Ref(RefType::new(true, HeapType::Any))),
        ),
    )
}

/// The GC array type a list of `Nat` or `Int` crosses as, `(mut i64)` — one element per slot, the guest's `$longs` — and `engine.rs`'s `host_func_type` describes the same shape. The guest narrows each element on the way out and boxes each on the way back, as it does a scalar, so neither side reads the other's representation of a number.
pub(crate) fn longs_array_type(engine: &Engine) -> ArrayType {
    ArrayType::new(
        engine,
        FieldType::new(Mutability::Var, StorageType::ValType(ValType::I64)),
    )
}

/// The GC array type a list of `Bool` crosses as, `(mut i32)` — one word per element, the guest's `$words`.
pub(crate) fn words_array_type(engine: &Engine) -> ArrayType {
    ArrayType::new(
        engine,
        FieldType::new(Mutability::Var, StorageType::ValType(ValType::I32)),
    )
}

/// Lower `values` as a flat array of `array_type`, the guest's `$longs` or `$words`.
fn lower_scalars(
    caller: &mut Caller<'_, ()>,
    array_type: ArrayType,
    values: impl IntoIterator<Item = Val>,
    results: &mut [Val],
) -> Result<(), wasmtime::Error> {
    let array_ref_pre = ArrayRefPre::new(&mut *caller, array_type);
    let values = values.into_iter().collect::<Vec<_>>();

    results[0] = Val::AnyRef(Some(
        ArrayRef::new_fixed(&mut *caller, &array_ref_pre, &values)?.to_anyref(),
    ));

    Ok(())
}

/// `List(Nat)`: each element's bits as an `i64`, which the guest reads unsigned.
impl Lower for Vec<u64> {
    fn lower(
        self,
        caller: &mut Caller<'_, ()>,
        results: &mut [Val],
    ) -> Result<(), wasmtime::Error> {
        let values = self.into_iter().map(|value| Val::I64(value.cast_signed()));

        let array_type = longs_array_type(caller.engine());

        lower_scalars(caller, array_type, values, results)
    }
}

/// `List(Int)`: each element as the `i64` it is.
impl Lower for Vec<i64> {
    fn lower(
        self,
        caller: &mut Caller<'_, ()>,
        results: &mut [Val],
    ) -> Result<(), wasmtime::Error> {
        let values = self.into_iter().map(Val::I64);

        let array_type = longs_array_type(caller.engine());

        lower_scalars(caller, array_type, values, results)
    }
}

/// `List(Bool)`: each element as the word `0` or `1`.
impl Lower for Vec<bool> {
    fn lower(
        self,
        caller: &mut Caller<'_, ()>,
        results: &mut [Val],
    ) -> Result<(), wasmtime::Error> {
        let values = self.into_iter().map(|value| Val::I32(i32::from(value)));

        let array_type = words_array_type(caller.engine());

        lower_scalars(caller, array_type, values, results)
    }
}

impl Lower for Vec<u8> {
    fn lower(
        self,
        caller: &mut Caller<'_, ()>,
        results: &mut [Val],
    ) -> Result<(), wasmtime::Error> {
        let array_type = i8_array_type(caller.engine());
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

/// `handle_poll`'s `revents`: a `Bytes` with one readiness mask per handle.
impl Lower for Vec<Poll> {
    fn lower(
        self,
        caller: &mut Caller<'_, ()>,
        results: &mut [Val],
    ) -> Result<(), wasmtime::Error> {
        self.into_iter()
            .map(Poll::bits)
            .collect::<Vec<u8>>()
            .lower(caller, results)
    }
}

/// `List(Bytes)`: an array of `anyref` whose elements are `Bytes` (`i8` arrays). The outer element type `(mut (ref null any))` matches the codegen's uniform `list_type`, so the array's runtime type is the one downstream `ref.cast`s expect.
impl Lower for Vec<Vec<u8>> {
    fn lower(
        self,
        caller: &mut Caller<'_, ()>,
        results: &mut [Val],
    ) -> Result<(), wasmtime::Error> {
        let byte_type = i8_array_type(caller.engine());
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

        let outer_type = anyref_array_type(caller.engine());
        let outer_pre = ArrayRefPre::new(&mut *caller, outer_type);

        results[0] = Val::AnyRef(Some(
            ArrayRef::new_fixed(&mut *caller, &outer_pre, &elements)?.to_anyref(),
        ));

        Ok(())
    }
}
