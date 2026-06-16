use wasmtime::{Caller, Val};

pub trait Lift: Sized {
    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error>;
}

impl Lift for () {
    fn lift(_: &mut Caller<'_, ()>, _: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(())
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

impl Lift for f32 {
    fn lift(_: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(params[0].unwrap_f32())
    }
}

// Pairs lift positionally: each component consumes one param slot. (Every
// single-value impl above reads `params[0]`, so slicing re-aligns them.)
impl<A: Lift, B: Lift> Lift for (A, B) {
    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok((
            A::lift(caller, &params[0..1])?,
            B::lift(caller, &params[1..2])?,
        ))
    }
}

// A 5-tuple lifts positionally, one param slot each — the shape `io_connect`
// needs (host, port, connect_timeout, read_timeout, write_timeout).
impl<A: Lift, B: Lift, C: Lift, D: Lift, E: Lift> Lift for (A, B, C, D, E) {
    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok((
            A::lift(caller, &params[0..1])?,
            B::lift(caller, &params[1..2])?,
            C::lift(caller, &params[2..3])?,
            D::lift(caller, &params[3..4])?,
            E::lift(caller, &params[4..5])?,
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
