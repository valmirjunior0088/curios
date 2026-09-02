use {
    super::{Handle, Poll, Status},
    wasmtime::{
        AnyRef, ArrayRef, ArrayRefPre, ArrayType, Caller, Engine, FieldType, HeapType, I31,
        Mutability, RefType, StorageType, Val, ValType,
    },
};

/// Encoding one host-import result list into wasmtime `Val`s — the outbound half of the FFI boundary (`Lift` is the inbound half). Each impl produces the exact GC shape the generated code expects on that wire type (i31-boxed scalars, `Bytes` as an i8 array, `List` as an anyref-element array), so a host function returns plain Rust values and the trampoline lands them in wasm-typed result slots.
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

/// Box `value` as the i31 ref every scalar crosses this boundary in, refusing a value the carrier cannot hold rather than wrapping one.
///
/// **The refusal is the point, and it is the guest half's rule stated on the host half.** `I31::wrapping_u32` drops bit 31 and reports nothing, while `into_wasm`'s literal materialization emits `unreachable` for exactly the values it would drop — so a wrapping host result would be the one direction across this boundary in which leaving the envelope changed a number instead of stopping. Every result that crosses today is in range, but each for a separate reason held somewhere else: `clock_wall` is split base-10⁹ so its limbs fit, `clock_mono`'s seconds would need decades of uptime, a status code is small, and `write`'s count is bounded by a buffer the guest allocated. Five facts in four files are what a check here replaces.
fn i31_ref(caller: &mut Caller<'_, ()>, value: u32) -> Result<Val, wasmtime::Error> {
    let boxed = I31::new_u32(value).ok_or_else(|| {
        wasmtime::Error::msg(format!("host result {value} leaves the i31 carrier"))
    })?;

    Ok(Val::AnyRef(Some(AnyRef::from_i31(caller, boxed))))
}

/// Scalar results cross the boundary pre-boxed as i31 refs so generated code can land them directly in anyref block params (see `emit_sys_imports`).
impl Lower for u32 {
    fn lower(
        self,
        caller: &mut Caller<'_, ()>,
        results: &mut [Val],
    ) -> Result<(), wasmtime::Error> {
        results[0] = i31_ref(caller, self)?;

        Ok(())
    }
}

/// Tuples lower positionally: each component fills one result slot, and slicing re-aligns the single-value impls, which all write `results[0]`. Arities two through seven — `stat`'s seven results are the widest row.
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

/// The GC array type the uniform `List` shape allocates under — a `(mut (ref null any))` element array, shared by every `List` lowering regardless of what its elements themselves are (`Vec<Poll>`'s i31s, `Vec<Vec<u8>>`'s `Bytes`), and by `engine.rs`'s `host_func_type` for the same reason.
pub(crate) fn anyref_array_type(engine: &Engine) -> ArrayType {
    ArrayType::new(
        engine,
        FieldType::new(
            Mutability::Var,
            StorageType::ValType(ValType::Ref(RefType::new(true, HeapType::Any))),
        ),
    )
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

/// `List(Nat)`: `poll`'s parallel `revents` masks, lowered as an array of i31-boxed bits. Same uniform `List` shape as `Vec<Vec<u8>>` below (anyref elements over the codegen's `list_type`), only the elements are i31s rather than `Bytes` — the outbound dual of `lift.rs`'s `lift_i31_array`.
impl Lower for Vec<Poll> {
    fn lower(
        self,
        caller: &mut Caller<'_, ()>,
        results: &mut [Val],
    ) -> Result<(), wasmtime::Error> {
        let outer_type = anyref_array_type(caller.engine());
        let outer_pre = ArrayRefPre::new(&mut *caller, outer_type);

        let elements = self
            .into_iter()
            .map(|mask| i31_ref(&mut *caller, mask.bits()))
            .collect::<Result<Vec<_>, wasmtime::Error>>()?;

        results[0] = Val::AnyRef(Some(
            ArrayRef::new_fixed(&mut *caller, &outer_pre, &elements)?.to_anyref(),
        ));

        Ok(())
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
