use {
    super::{ChildStream, Handle, Mode, Poll, SerialFlow, SerialOp, SerialParity, StdioMode},
    curios_abi::{ClosedCode, WireLeaf, WireType},
    wasmtime::{Caller, Val},
};

/// Decoding one host-import argument list out of wasmtime `Val`s — the inbound half of the FFI boundary (`Lower` is the outbound half). `ForeignBindings::define` and the `sys`-tier glue compose each trampoline from `Li::lift`/`Lo::lower`, so a host implementation is written against plain Rust types (`u64`, `Vec<u8>`, [`Handle`], tuples) and never touches a `Val`.
///
/// A value the guest's type does not admit — a `Bool` word past `1`, a closed code outside its table, an interest bit a guest cannot ask for — is refused rather than read as something nearby: the call fails, naming what it was handed.
pub trait Lift: Sized {
    /// The wire types this decodes, in order: what `ForeignBindings::define` holds a binding's operands to against its row.
    fn shape() -> Vec<WireType>;

    /// Decode `Self` from the import's incoming `params`, reading any GC arrays through `caller`. Contract: every single-value impl consumes exactly `params[0]` — the alignment the tuple impls rely on to re-slice per component.
    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error>;
}

impl Lift for () {
    fn shape() -> Vec<WireType> {
        vec![]
    }

    fn lift(_: &mut Caller<'_, ()>, _: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(())
    }
}

/// A descriptor lifts from its wire token bytes (a `Bytes`): the three stdio encodings map to the named streams, anything else is a host-minted handle.
impl Lift for Handle {
    fn shape() -> Vec<WireType> {
        vec![WireType::Handle]
    }

    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(Handle::from_bytes(Vec::<u8>::lift(caller, params)?))
    }
}

/// A closed code arrives as its `Nat` tag, and a tag outside `T`'s table is refused, `what` naming the code in the refusal.
fn lift_code<T: ClosedCode>(params: &[Val], what: &str) -> Result<T, wasmtime::Error> {
    let code = params[0].unwrap_i64().cast_unsigned();

    T::from_code(code).ok_or_else(|| wasmtime::Error::msg(format!("{code} is not {what}")))
}

impl Lift for Mode {
    fn shape() -> Vec<WireType> {
        vec![WireType::Nat]
    }

    fn lift(_: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        lift_code(params, "an open mode")
    }
}

impl Lift for StdioMode {
    fn shape() -> Vec<WireType> {
        vec![WireType::Nat]
    }

    fn lift(_: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        lift_code(params, "a stdio mode")
    }
}

impl Lift for ChildStream {
    fn shape() -> Vec<WireType> {
        vec![WireType::Nat]
    }

    fn lift(_: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        lift_code(params, "a standard stream")
    }
}

impl Lift for SerialParity {
    fn shape() -> Vec<WireType> {
        vec![WireType::Nat]
    }

    fn lift(_: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        lift_code(params, "a serial parity")
    }
}

impl Lift for SerialFlow {
    fn shape() -> Vec<WireType> {
        vec![WireType::Nat]
    }

    fn lift(_: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        lift_code(params, "a serial flow control")
    }
}

impl Lift for SerialOp {
    fn shape() -> Vec<WireType> {
        vec![WireType::Nat]
    }

    fn lift(_: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        lift_code(params, "a serial control")
    }
}

/// A `Byte` arrives as the word it is. The guest's byte is below `256`, so a word past it comes from a module this compiler did not emit, and is refused rather than truncated into a different byte.
impl Lift for u8 {
    fn shape() -> Vec<WireType> {
        vec![WireType::Byte]
    }

    fn lift(_: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        let word = params[0].unwrap_i32();

        u8::try_from(word).map_err(|_| wasmtime::Error::msg(format!("{word} is not a byte")))
    }
}

/// A `Bool`'s word, `0` or `1` and nothing else.
fn bool_of(word: i32) -> Result<bool, wasmtime::Error> {
    match word {
        0 => Ok(false),
        1 => Ok(true),
        word => Err(wasmtime::Error::msg(format!("{word} is not a Bool"))),
    }
}

/// A `Bool` arrives as its word.
impl Lift for bool {
    fn shape() -> Vec<WireType> {
        vec![WireType::Bool]
    }

    fn lift(_: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        bool_of(params[0].unwrap_i32())
    }
}

/// A `Nat` arrives as the `i64` the guest narrowed it to, read unsigned.
impl Lift for u64 {
    fn shape() -> Vec<WireType> {
        vec![WireType::Nat]
    }

    fn lift(_: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(params[0].unwrap_i64().cast_unsigned())
    }
}

/// An `Int` arrives as the `i64` the guest narrowed it to.
impl Lift for i64 {
    fn shape() -> Vec<WireType> {
        vec![WireType::Int]
    }

    fn lift(_: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(params[0].unwrap_i64())
    }
}

/// An `Flt` crosses as a raw binary64 in both directions, so it arrives as the number it is — the guest read it out of its box before the call, and boxes the result after.
impl Lift for f64 {
    fn shape() -> Vec<WireType> {
        vec![WireType::Flt]
    }

    fn lift(_: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(params[0].unwrap_f64())
    }
}

/// Tuples lift positionally: each component consumes one param slot, and slicing re-aligns the single-value impls, which all read `params[0]`. Arities one through seven — the one-tuple for the `sys` bindings, which lift every row's operands as a tuple whatever their count, and past the six operands of `proc_spawn` and `serial_open`, the widest rows, so an embedder's own declaration has room.
macro_rules! lift_tuple {
    ($($name:ident $index:tt),+) => {
        impl<$($name: Lift),+> Lift for ($($name,)+) {
            fn shape() -> Vec<WireType> {
                [$($name::shape()),+].concat()
            }

            fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
                Ok(($($name::lift(caller, &params[$index..$index + 1])?,)+))
            }
        }
    };
}

lift_tuple!(A 0);
lift_tuple!(A 0, B 1);
lift_tuple!(A 0, B 1, C 2);
lift_tuple!(A 0, B 1, C 2, D 3);
lift_tuple!(A 0, B 1, C 2, D 3, E 4);
lift_tuple!(A 0, B 1, C 2, D 3, E 4, F 5);
lift_tuple!(A 0, B 1, C 2, D 3, E 4, F 5, G 6);

/// Read a list of scalars: the guest's flat array, one element per slot, each already narrowed by the guest to what the wire carries — `$longs` for a `Nat` or `Int`, `$words` for a `Bool`.
fn lift_scalars(caller: &mut Caller<'_, ()>, param: &Val) -> Result<Vec<Val>, wasmtime::Error> {
    let Val::AnyRef(Some(anyref)) = param else {
        return Err(wasmtime::Error::msg("expected non-null anyref"));
    };

    let array_ref = anyref
        .as_array(&*caller)?
        .ok_or_else(|| wasmtime::Error::msg("expected array ref"))?;

    let len = array_ref.len(&*caller)?;

    (0..len)
        .map(|index| array_ref.get(&mut *caller, index))
        .collect()
}

/// `List(Nat)`: each element read unsigned, as a `Nat` argument is.
impl Lift for Vec<u64> {
    fn shape() -> Vec<WireType> {
        vec![WireType::List(WireLeaf::Nat)]
    }

    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(lift_scalars(caller, &params[0])?
            .into_iter()
            .map(|value| value.unwrap_i64().cast_unsigned())
            .collect())
    }
}

/// `List(Int)`: each element as the `Int` it is.
impl Lift for Vec<i64> {
    fn shape() -> Vec<WireType> {
        vec![WireType::List(WireLeaf::Int)]
    }

    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(lift_scalars(caller, &params[0])?
            .into_iter()
            .map(|value| value.unwrap_i64())
            .collect())
    }
}

/// `List(Bool)`: each word held to `0` or `1`, as a single `Bool` is.
impl Lift for Vec<bool> {
    fn shape() -> Vec<WireType> {
        vec![WireType::List(WireLeaf::Bool)]
    }

    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        lift_scalars(caller, &params[0])?
            .into_iter()
            .map(|value| bool_of(value.unwrap_i32()))
            .collect()
    }
}

impl Lift for Vec<u8> {
    fn shape() -> Vec<WireType> {
        vec![WireType::Bytes]
    }

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

/// `handle_poll`'s `events`: a `Bytes` with one interest mask per handle, each within the bits a guest may ask for.
impl Lift for Vec<Poll> {
    fn shape() -> Vec<WireType> {
        vec![WireType::Bytes]
    }

    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Vec::<u8>::lift(caller, params)?
            .into_iter()
            .map(|bits| {
                Poll::interest(bits).ok_or_else(|| {
                    wasmtime::Error::msg(format!(
                        "{bits:#06b} asks for readiness no guest can ask for"
                    ))
                })
            })
            .collect()
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

/// `List(Bytes)` lifts each element as the `Bytes` it is — `proc_spawn`'s argument and environment lists.
impl Lift for Vec<Vec<u8>> {
    fn shape() -> Vec<WireType> {
        vec![WireType::List(WireLeaf::Bytes)]
    }

    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        lift_bytes_array(caller, &params[0])
    }
}

/// `List(Handle)` lifts each token through the same stdio/handle classification a single `Handle` does — `handle_poll`'s `handles` array.
impl Lift for Vec<Handle> {
    fn shape() -> Vec<WireType> {
        vec![WireType::List(WireLeaf::Handle)]
    }

    fn lift(caller: &mut Caller<'_, ()>, params: &[Val]) -> Result<Self, wasmtime::Error> {
        Ok(lift_bytes_array(caller, &params[0])?
            .into_iter()
            .map(Handle::from_bytes)
            .collect())
    }
}
