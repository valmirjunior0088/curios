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
