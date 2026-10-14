//! A program that can suspend, run to its end: its turns advanced one after another, and the host's wait between them.
//!
//! The entry answers the first turn and [`ADVANCE`] every later one, each turn the five values [`ADVANCE`] states. What crosses from one turn to the next — the rest, and the cancel a deadlock runs — is held as an owned root, and everything else a turn hands back is read inside a root scope of its own, so a run of any length holds what its present suspension needs and nothing it has finished with.

use {
    super::ExitTrap,
    crate::{Handle, HostOps},
    curios_abi::{ADVANCE, DEADLOCK},
    wasmtime::{
        AnyRef, AsContextMut, Func, Instance, OwnedRooted, RootScope, Rooted, Store, TypedFunc,
    },
};

/// One turn as the program answers it.
type Turn = (
    i32,
    Option<Rooted<AnyRef>>,
    Option<Rooted<AnyRef>>,
    Option<Rooted<AnyRef>>,
    Option<Rooted<AnyRef>>,
);

/// The advance, taking the description it forces.
type Advance = TypedFunc<Option<Rooted<AnyRef>>, Turn>;

/// What a turn leaves to do: hand back what the program yielded, or wait on handles before advancing the rest — or the cancel, where nothing can wake it.
enum Turned {
    Yielded(Option<OwnedRooted<AnyRef>>),
    Suspended {
        on: Vec<Handle>,
        cancel: Option<OwnedRooted<AnyRef>>,
        rest: Option<OwnedRooted<AnyRef>>,
    },
}

/// Run `entry` turn by turn under `host`, answering what the program yielded.
///
/// A suspension on no handle can never be advanced, since no wait could end: its cancel is run to its end, [`DEADLOCK`] is reported on standard error, and the run ends with code `1`, as an exit does.
pub(super) fn run_turns(
    store: &mut Store<()>,
    instance: &Instance,
    entry: Func,
    host: &(dyn HostOps + Send + Sync),
) -> Result<Option<Rooted<AnyRef>>, wasmtime::Error> {
    let entry = entry.typed::<(), Turn>(&*store)?;
    let advance = instance.get_typed_func::<Option<Rooted<AnyRef>>, Turn>(&mut *store, ADVANCE)?;

    let mut turned = {
        let mut scope = RootScope::new(&mut *store);
        let turn = entry.call(&mut scope, ())?;

        took(&mut scope, turn)?
    };

    loop {
        turned = match turned {
            Turned::Yielded(value) => {
                return Ok(value.map(|value| value.to_rooted(&mut *store)));
            }
            Turned::Suspended { on, cancel, .. } if on.is_empty() => {
                finish(store, &advance, cancel, host)?;
                // The report is the host's last word on the run, and a stream that refuses it leaves nothing to say why.
                let _ = host.handle_write(Handle::Stderr, DEADLOCK.as_bytes().to_vec());

                return Err(wasmtime::Error::from(ExitTrap(1)));
            }
            Turned::Suspended { on, rest, .. } => {
                waited(host, on)?;
                step(store, &advance, rest)?
            }
        };
    }
}

/// Advance `description` by one turn.
fn step(
    store: &mut Store<()>,
    advance: &Advance,
    description: Option<OwnedRooted<AnyRef>>,
) -> Result<Turned, wasmtime::Error> {
    let mut scope = RootScope::new(store);
    let description = description.map(|description| description.to_rooted(&mut scope));
    let turn = advance.call(&mut scope, description)?;

    took(&mut scope, turn)
}

/// Run `cancel` to its end, waiting where it suspends. One that suspends on no handle in turn is left where it stands, since nothing could wake it either.
fn finish(
    store: &mut Store<()>,
    advance: &Advance,
    cancel: Option<OwnedRooted<AnyRef>>,
    host: &(dyn HostOps + Send + Sync),
) -> Result<(), wasmtime::Error> {
    let mut turned = step(store, advance, cancel)?;

    loop {
        turned = match turned {
            Turned::Suspended { on, rest, .. } if !on.is_empty() => {
                waited(host, on)?;
                step(store, advance, rest)?
            }
            Turned::Yielded(_) | Turned::Suspended { .. } => return Ok(()),
        };
    }
}

/// Wait on `on`; a system that refuses the wait refuses the run.
fn waited(host: &(dyn HostOps + Send + Sync), on: Vec<Handle>) -> Result<(), wasmtime::Error> {
    host.wait(on)
        .map_err(|refusal| wasmtime::Error::msg(refusal.0))
}

/// What `turn` leaves to do, its references owned past the scope it was read in.
fn took(
    scope: &mut RootScope<&mut Store<()>>,
    (suspended, value, on, cancel, rest): Turn,
) -> Result<Turned, wasmtime::Error> {
    let mut owned = |reference: Option<Rooted<AnyRef>>| {
        reference
            .map(|reference| reference.to_owned_rooted(&mut *scope))
            .transpose()
    };

    match suspended {
        0 => Ok(Turned::Yielded(owned(value)?)),
        _ => {
            let cancel = owned(cancel)?;
            let rest = owned(rest)?;

            Ok(Turned::Suspended {
                on: handles(&mut *scope, on)?,
                cancel,
                rest,
            })
        }
    }
}

/// The handles a suspension waits on, read off the flat payload a `List(Handle)` crosses as: an array of each handle's token bytes.
fn handles(
    mut store: impl AsContextMut,
    on: Option<Rooted<AnyRef>>,
) -> Result<Vec<Handle>, wasmtime::Error> {
    let not_a_list = || wasmtime::Error::msg("a turn's handles are not a list");
    let on = on
        .ok_or_else(not_a_list)?
        .as_array(&store)?
        .ok_or_else(not_a_list)?;
    let count = on.len(&store)?;

    (0..count)
        .map(|index| {
            let token = on
                .get(&mut store, index)?
                .unwrap_anyref()
                .and_then(|token| token.as_array(&store).transpose())
                .ok_or_else(not_a_list)??;
            let mut bytes = vec![0; token.len(&store)? as usize];
            token.copy_to_i8_slice(&mut store, &mut bytes)?;

            Ok(Handle::from_bytes(bytes))
        })
        .collect()
}
