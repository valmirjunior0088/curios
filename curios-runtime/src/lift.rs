use {
    super::{Handle, Mode, Poll},
    curios_abi::open_mode,
    wasmtime::{Caller, Val},
};

/// Decoding one host-import argument list out of wasmtime `Val`s — the inbound half of the FFI boundary (`Lower` is the outbound half). `ForeignBindings::define` and the `sys`-tier glue compose each trampoline from `Li::lift`/`Lo::lower`, so a host implementation is written against plain Rust types (`u32`, `Vec<u8>`, [`Handle`], tuples) and never touches a `Val`.
pub trait Lift: Sized {
    /// Decode `Self` from the import's incoming `params`, reading any GC arrays through `caller`. Contract: every single-value impl consumes exactly `params[0]` — the alignment the tuple impls rely on to re-slice per component.
    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error>;
}

impl Lift for () {
    fn lift(_: &mut Caller<'_, ()>, _: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(())
    }
}

/// A descriptor lifts from its wire token bytes (a `Bytes`): the three stdio encodings map to the named streams, anything else is a host-minted handle.
impl Lift for Handle {
    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(Handle::from_bytes(Vec::<u8>::lift(caller, params)?))
    }
}

/// `open`'s mode lifts from its `/std/File` `Mode` tag. An out-of-range tag is a codegen bug (the inductive only marshals `0`/`1`/`2`), so it panics.
impl Lift for Mode {
    fn lift(_: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(match params[0].unwrap_i32() as u32 {
            open_mode::READ => Mode::Read,
            open_mode::WRITE => Mode::Write,
            open_mode::APPEND => Mode::Append,
            tag => panic!("invalid open mode tag: {tag}"),
        })
    }
}

impl Lift for u32 {
    fn lift(_: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(params[0].unwrap_i32() as u32)
    }
}

impl Lift for i32 {
    fn lift(_: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(params[0].unwrap_i32())
    }
}

/// Tuples lift positionally: each component consumes one param slot, and slicing re-aligns the single-value impls, which all read `params[0]`. Arities two through seven — `spawn`'s seven operands are the widest row.
macro_rules! lift_tuple {
    ($($name:ident $index:tt),+) => {
        impl<$($name: Lift),+> Lift for ($($name,)+) {
            fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
                Ok(($($name::lift(caller, &params[$index..$index + 1])?,)+))
            }
        }
    };
}

lift_tuple!(A 0, B 1);
lift_tuple!(A 0, B 1, C 2);
lift_tuple!(A 0, B 1, C 2, D 3);
lift_tuple!(A 0, B 1, C 2, D 3, E 4);
lift_tuple!(A 0, B 1, C 2, D 3, E 4, F 5);
lift_tuple!(A 0, B 1, C 2, D 3, E 4, F 5, G 6);

impl Lift for Vec<u8> {
    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        let Val::AnyRef(Some(anyref)) = &params[0] else {
            return Err(wasmtime::Error::msg("expected non-null anyref"));
        };

        let array_ref = anyref
            .as_array(&*caller)?
            .ok_or_else(|| wasmtime::Error::msg("expected array ref"))?;

        let len = array_ref.len(&*caller)?;

        (0..len)
            .map(|index| {
                array_ref
                    .get(&mut *caller, index)
                    .map(|value| value.unwrap_i32() as u8)
            })
            .collect()
    }
}

/// Read a `List(Nat)`/`List(Handle)` host-import argument: a `params[0]` anyref array whose elements are i31-boxed scalars (the module's uniform `List` shape, not `Bytes`'s packed `i8`). The inbound dual of `lower.rs`'s `Vec<u32>` lowering.
fn lift_i31_array(caller: &mut Caller<'_, ()>, param: &Val) -> Result<Vec<u32>, wasmtime::Error> {
    let Val::AnyRef(Some(anyref)) = param else {
        return Err(wasmtime::Error::msg("expected non-null anyref"));
    };

    let array_ref = anyref
        .as_array(&*caller)?
        .ok_or_else(|| wasmtime::Error::msg("expected array ref"))?;

    let len = array_ref.len(&*caller)?;

    (0..len)
        .map(|index| {
            let Val::AnyRef(Some(element)) = array_ref.get(&mut *caller, index)? else {
                return Err(wasmtime::Error::msg("expected non-null anyref element"));
            };

            Ok(element.unwrap_i31(&*caller)?.get_u32())
        })
        .collect()
}

/// `List(Nat)` lifts to the per-handle interest masks — `poll`'s `events` array.
impl Lift for Vec<Poll> {
    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(lift_i31_array(caller, &params[0])?
            .into_iter()
            .map(Poll::from_bits)
            .collect())
    }
}

/// Read a `List(Bytes)` host-import argument: a `params[0]` anyref array whose elements are themselves `Bytes` (i8 arrays). The inbound dual of `lower.rs`'s `Vec<Vec<u8>>` lowering; `List(Handle)` rides this shape now that a handle is bytes.
fn lift_bytes_array(
    caller: &mut Caller<'_, ()>,
    param: &Val,
) -> Result<Vec<Vec<u8>>, wasmtime::Error> {
    let Val::AnyRef(Some(anyref)) = param else {
        return Err(wasmtime::Error::msg("expected non-null anyref"));
    };

    let array_ref = anyref
        .as_array(&*caller)?
        .ok_or_else(|| wasmtime::Error::msg("expected array ref"))?;

    let len = array_ref.len(&*caller)?;

    (0..len)
        .map(|index| {
            let element = array_ref.get(&mut *caller, index)?;

            Vec::<u8>::lift(caller, &[element])
        })
        .collect()
}

/// `List(Bytes)` lifts each element as the `Bytes` it is — `spawn`'s argument and environment lists.
impl Lift for Vec<Vec<u8>> {
    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        lift_bytes_array(caller, &params[0])
    }
}

/// `List(Handle)` lifts each token through the same stdio/handle classification a single `Handle` does — `poll`'s `handles` array.
impl Lift for Vec<Handle> {
    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(lift_bytes_array(caller, &params[0])?
            .into_iter()
            .map(Handle::from_bytes)
            .collect())
    }
}
