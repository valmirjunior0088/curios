use {
    crate::printer::{Printer, flat, indent, pure, sep_flat},
    std::collections::HashMap,
    wasmtime::{AnyRef, OwnedRooted, Rooted, Store, Val},
};

#[derive(Default)]
struct Visited {
    ref_ids: HashMap<u32, usize>,
}

impl Visited {
    fn new() -> Self {
        Self::default()
    }

    fn raw_anyref(store: &mut Store<()>, reference: &Rooted<AnyRef>) -> Result<u32, String> {
        reference
            .to_raw(&mut *store)
            .map_err(|error| format!("failed to read anyref identity: {error}"))
    }

    fn backref_id(
        &mut self,
        store: &mut Store<()>,
        reference: &Rooted<AnyRef>,
    ) -> Result<Option<usize>, String> {
        let raw = Self::raw_anyref(store, reference)?;

        Ok(self.ref_ids.get(&raw).copied())
    }

    fn insert_anyref(
        &mut self,
        store: &mut Store<()>,
        reference: &Rooted<AnyRef>,
    ) -> Result<usize, String> {
        let raw = Self::raw_anyref(store, reference)?;
        let id = self.ref_ids.len();
        self.ref_ids.insert(raw, id);

        Ok(id)
    }
}

fn print_anyref(
    visited: &mut Visited,
    store: &mut Store<()>,
    reference: &Rooted<AnyRef>,
) -> Result<Printer<'static>, String> {
    if let Some(id) = visited.backref_id(store, reference)? {
        return Ok(pure(format!("ref #{id}")));
    }

    let id = visited.insert_anyref(store, reference)?;

    if let Some(value) = reference
        .as_i31(&*store)
        .map_err(|error| format!("failed to inspect i31 anyref: {error}"))?
    {
        let value = value.get_i32();
        let bits = (value as u32) & 0x7fff_ffff;

        return Ok(pure(format!(
            "#{id} = i31(bits=0x{bits:08x}, value={value})"
        )));
    }

    if let Some(struct_ref) = reference
        .as_struct(&*store)
        .map_err(|error| format!("failed to inspect struct anyref: {error}"))?
    {
        let field_count = struct_ref
            .ty(&*store)
            .map_err(|error| format!("failed to inspect struct type: {error}"))?
            .fields()
            .len();

        let fields = (0..field_count)
            .map(|index| {
                struct_ref
                    .field(&mut *store, index)
                    .map_err(|error| format!("failed to read struct field {index}: {error}"))
                    .and_then(|value| {
                        print_val(visited, store, value)
                            .map(|printer| flat([pure(format!("{index}: ")), printer]))
                    })
            })
            .collect::<Result<Vec<_>, String>>()?;

        return Ok(match fields.is_empty() {
            true => pure(format!("#{id} = struct {{}}")),
            false => flat([
                pure(format!("#{id} = struct {{\n")),
                indent(sep_flat(fields, || pure("\n"))),
                pure("\n}"),
            ]),
        });
    }

    if let Some(array_ref) = reference
        .as_array(&*store)
        .map_err(|error| format!("failed to inspect array anyref: {error}"))?
    {
        let length = array_ref
            .len(&*store)
            .map_err(|error| format!("failed to inspect array length: {error}"))?;
        let elems = (0..length)
            .map(|index| {
                array_ref
                    .get(&mut *store, index)
                    .map_err(|error| format!("failed to read array element {index}: {error}"))
                    .and_then(|value| {
                        print_val(visited, store, value)
                            .map(|printer| flat([pure(format!("{index}: ")), printer]))
                    })
            })
            .collect::<Result<Vec<_>, String>>()?;

        return Ok(match elems.is_empty() {
            true => pure(format!("#{id} = array []")),
            false => flat([
                pure(format!("#{id} = array [\n")),
                indent(sep_flat(elems, || pure("\n"))),
                pure("\n]"),
            ]),
        });
    }

    Ok(pure(format!("#{id} = anyref")))
}

fn print_val(
    visited: &mut Visited,
    store: &mut Store<()>,
    value: Val,
) -> Result<Printer<'static>, String> {
    match value {
        Val::I32(value) => Ok(pure(format!(
            "i32(bits=0x{:08x}, value={value})",
            value as u32
        ))),
        Val::I64(value) => Ok(pure(format!(
            "i64(bits=0x{:016x}, value={value})",
            value as u64
        ))),
        Val::F32(bits) => Ok(pure(format!(
            "f32(bits=0x{bits:08x}, value={})",
            f32::from_bits(bits)
        ))),
        Val::F64(bits) => Ok(pure(format!(
            "f64(bits=0x{bits:016x}, value={})",
            f64::from_bits(bits)
        ))),
        Val::V128(value) => Ok(pure(format!("v128({value:?})"))),
        Val::FuncRef(Some(_)) => Ok(pure("funcref")),
        Val::FuncRef(None) => Ok(pure("null funcref")),
        Val::ExternRef(Some(_)) => Ok(pure("externref")),
        Val::ExternRef(None) => Ok(pure("null externref")),
        Val::AnyRef(Some(reference)) => print_anyref(visited, store, &reference),
        Val::AnyRef(None) => Ok(pure("null anyref")),
        Val::ExnRef(Some(_)) => Ok(pure("exnref")),
        Val::ExnRef(None) => Ok(pure("null exnref")),
        Val::ContRef(Some(_)) => Ok(pure("contref")),
        Val::ContRef(None) => Ok(pure("null contref")),
    }
}

pub fn print_ref(store: &mut Store<()>, reference: &OwnedRooted<AnyRef>) -> Result<String, String> {
    let rooted = reference.to_rooted(&mut *store);

    Ok(print_anyref(&mut Visited::new(), store, &rooted)?.to_string())
}
