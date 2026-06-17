use {
    super::{Io, Mode},
    wasmtime::{Caller, Val},
};

pub trait Lift: Sized {
    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error>;
}

impl Lift for () {
    fn lift(_: &mut Caller<'_, ()>, _: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(())
    }
}

/// A descriptor lifts from its `u32` wire token: the three stdio numbers map to
/// the named streams, anything else is a host-minted handle.
impl Lift for Io {
    fn lift(_: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(match params[0].unwrap_i32() as u32 {
            Io::STDIN => Io::Stdin,
            Io::STDOUT => Io::Stdout,
            Io::STDERR => Io::Stderr,
            token => Io::Other(token),
        })
    }
}

/// `open`'s mode lifts from its `/std/File/Mode` tag. An out-of-range tag is a
/// codegen bug (the union only marshals `0`/`1`/`2`), so it panics.
impl Lift for Mode {
    fn lift(_: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(match params[0].unwrap_i32() {
            0 => Mode::Read,
            1 => Mode::Write,
            2 => Mode::Append,
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
