use {
    super::{Handle, Mode, Poll},
    curios_abi::mode,
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

/// A descriptor lifts from its wire token bytes (a `Bin`): the three stdio encodings map to the named streams, anything else is a host-minted handle.
impl Lift for Handle {
    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(Handle::from_bytes(Vec::<u8>::lift(caller, params)?))
    }
}

/// `open`'s mode lifts from its `/std/File` `Mode` tag. An out-of-range tag is a codegen bug (the inductive only marshals `0`/`1`/`2`), so it panics.
impl Lift for Mode {
    fn lift(_: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(match params[0].unwrap_i32() as u32 {
            mode::READ => Mode::Read,
            mode::WRITE => Mode::Write,
            mode::APPEND => Mode::Append,
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

// Pairs lift positionally: each component consumes one param slot. (Every single-value impl above reads `params[0]`, so slicing re-aligns them.)
impl<A: Lift, B: Lift> Lift for (A, B) {
    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok((
            A::lift(caller, &params[0..1])?,
            B::lift(caller, &params[1..2])?,
        ))
    }
}

// Triples lift positionally too — `poll(handles, events, timeout)` is the one host import with three operands.
impl<A: Lift, B: Lift, C: Lift> Lift for (A, B, C) {
    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok((
            A::lift(caller, &params[0..1])?,
            B::lift(caller, &params[1..2])?,
            C::lift(caller, &params[2..3])?,
        ))
    }
}

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

/// Read an `Lst(Nat)`/`Lst(Handle)` host-import argument: a `params[0]` anyref array whose elements are i31-boxed scalars (the module's uniform `Lst` shape, not `Bin`'s packed `i8`). The inbound dual of `lower.rs`'s `Vec<u32>` lowering.
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

/// `Lst(Nat)` lifts to the per-handle interest masks — `poll`'s `events` array.
impl Lift for Vec<Poll> {
    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(lift_i31_array(caller, &params[0])?
            .into_iter()
            .map(Poll::from_bits)
            .collect())
    }
}

/// Read an `Lst(Bin)` host-import argument: a `params[0]` anyref array whose elements are themselves `Bin`s (i8 arrays). The inbound dual of `lower.rs`'s `Vec<Vec<u8>>` lowering; `Lst(Handle)` rides this shape now that a handle is bytes.
fn lift_bin_array(
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

/// `Lst(Handle)` lifts each token through the same stdio/handle classification a single `Handle` does — `poll`'s `handles` array.
impl Lift for Vec<Handle> {
    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(lift_bin_array(caller, &params[0])?
            .into_iter()
            .map(Handle::from_bytes)
            .collect())
    }
}
