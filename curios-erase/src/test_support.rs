//! Scaffolding for tests that read what erasure made: the families a unit's items declare, as each declaring item erased them, and the digest of every part a unit's items are read by.
//!
//! Compiled for this crate's tests and behind the `test-support` feature, so no build that ships holds it. What it hands out is owned and names a family by its spelling, since a family's identity is its fragment's own and means nothing outside it.
//!
//! For this crate's tests alone, and gated to them, the fixtures its erasure suites share.

use crate::{ErasedUnit, NamedPart, Part, declared_digest, erased_digest};

#[cfg(test)]
use {
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_cert::{Globals, certify_module, certify_program},
    curios_core::Zonkable,
    curios_elab::DEFAULT_BUDGET,
    curios_utilities::Qualifier,
    std::collections::BTreeSet,
};

/// The digest of every part of every declaration `module` holds where `places` keeps it, under the record `certification` its walk filed: each signature, body and kind, how an item naming each name reads `erased`'s forms of it, and those forms whole, each beside the name and part it is of. What a unit records of a read: a unit held in hand and the same unit restored from a store are read alike by a unit compiled over either only where these agree.
pub fn part_digests(
    module: &curios_core::Module,
    places: &curios_core::Places,
    certification: &curios_core::Certification,
    erased: &ErasedUnit,
) -> Vec<(NamedPart, Result<Option<String>, String>)> {
    module
        .items
        .places()
        .flat_map(|place| module.items.names(place).into_iter().copied())
        .flat_map(|name| Part::ALL.map(|part| (name, part)))
        .map(|read| {
            let digest = match (read.1, erased.place(&read.0)) {
                (Part::Binding | Part::Forms, Some(at)) => erased_digest(erased, at, read),
                (Part::Binding | Part::Forms, None) => Ok(None),
                _ => declared_digest(module, places, certification, read),
            };

            (read, digest)
        })
        .collect()
}

/// A field of a constructor's row as its declaring item erased it: its shape, and for a field at a family, that family's name.
#[derive(Debug, Clone)]
pub struct ErasedField {
    pub shape: curios_anf::FieldShape,
    pub family: Option<String>,
}

/// A family a unit declares: its name, and each constructor's fields in the declaration's order.
#[derive(Debug, Clone)]
pub struct ErasedFamily {
    pub name: String,
    pub constructors: Vec<Vec<ErasedField>>,
}

/// The families `unit`'s items declare. Each is read off the fragment of the item that declares it, the one place its fields' shapes stand: a fragment that imports a family reads its name and each constructor's width and mask, and no shape.
pub fn declared_families(unit: &ErasedUnit) -> Vec<ErasedFamily> {
    unit.declared_families()
}

/// A declaration's name, from the path a test writes. Fixture-only.
#[cfg(test)]
pub fn nominal(path: &str) -> curios_core::Global {
    curios_core::Global::Authored(Qualifier::from([path]))
}

/// A binder no fixture spells, minted far above where the kernel mints: a process-wide counter keeps two in one test apart.
#[cfg(test)]
pub fn fresh(hint: Option<&str>) -> curios_core::Free {
    static MINTED: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1_000_000);

    curios_core::Free::local(
        MINTED.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
        hint,
    )
}

/// `unit` certified with nothing in scope and erased through the kernel that certified it, as the driver erases a unit it compiles.
#[cfg(test)]
pub fn erase_alone(unit: &curios_core::Zonked<curios_core::Module>) -> Result<ErasedUnit, Error> {
    let globals = Globals::default();
    let (rechecked, mut kernel) = certify_module(unit, DEFAULT_BUDGET, &globals, SYNTAX);
    assert!(
        rechecked.verdicts.is_empty(),
        "the fixture is certified: {:?}",
        rechecked.verdicts
    );

    erase_unit(&mut kernel, Erasures::nothing(), unit)
}

/// A top-level definition's identity, from the path a test writes — the same name [`definition`] declares it under. Fixture-only.
#[cfg(test)]
pub fn global(path: &str) -> curios_core::Free {
    curios_core::Free::global(Qualifier::from([path]))
}

#[cfg(test)]
pub fn nat_lit(n: usize) -> curios_core::Term {
    curios_core::Term::intrinsic(curios_core::Intrinsic::Nat(curios_core::Nat::new(n)))
}

#[cfg(test)]
pub fn definition(
    name: &str,
    type_: curios_core::Term,
    body: curios_core::Term,
) -> curios_core::Item {
    curios_core::Item::Let(Box::new(curios_core::Definition {
        name: curios_core::Global::Authored(Qualifier::from([name])),
        kind: curios_core::DefinitionKind::Authored,
        universe_context: curios_core::UniverseContext::empty(),
        island: Qualifier::empty(),
        totality: curios_core::Totality::default(),
        proof: false,
        variances: Vec::new(),
        field_variances: Vec::new(),
        parameter_field: None,
        value_sort: None,
        type_,
        body,
    }))
}

/// A program of `items` closing with `body`, at no stated type yet — see [`stating`].
#[cfg(test)]
pub fn module(items: Vec<curios_core::Item>, body: curios_core::Term) -> curios_core::Program {
    curios_core::Program {
        module: curios_core::Module {
            mounts: Vec::new(),
            items: items.into(),
            witnesses: BTreeSet::new(),
            tests: Vec::new(),
        },
        entry: curios_core::Entrypoint { body, type_: None },
    }
}

#[cfg(test)]
pub fn zonked<T: Zonkable>(value: &T) -> curios_core::Zonked<T> {
    curios_core::Zonked::project(value).expect("the fixture is zonked")
}

/// `program` with its entry stating `type_`, which is what the entry's body is sealed at.
#[cfg(test)]
pub fn stating(program: &curios_core::Program, type_: curios_core::Term) -> curios_core::Program {
    let mut program = program.clone();
    program.entry.type_ = Some(type_);
    program
}

/// `program` erased with nothing in scope, its entry stating `type_`.
#[cfg(test)]
pub fn try_erase(
    program: &curios_core::Program,
    type_: curios_core::Term,
) -> Result<curios_anf::Module, Error> {
    let program = zonked(&stating(program, type_));
    let globals = Globals::default();
    let (rechecked, mut kernel) = certify_program(&program, DEFAULT_BUDGET, &globals, SYNTAX);
    assert!(
        rechecked.verdicts.is_empty(),
        "the fixture is certified: {:?}",
        rechecked.verdicts
    );

    erase_program(&mut kernel, Erasures::nothing(), &program).map(ErasedProgram::into_module)
}

#[cfg(test)]
pub fn erase(program: &curios_core::Program, type_: curios_core::Term) -> curios_anf::Module {
    try_erase(program, type_).expect("the program erases")
}

/// Every `Rhs` the erased module binds, in statement order. A test whose subject is what erasure *built* asks this rather than the printout: a spelling belongs to the printer, and an anchor on one can silently match nothing, where a missing shape here is a loud failure.
#[cfg(test)]
pub fn bound(module: &curios_anf::Module) -> impl Iterator<Item = &curios_anf::Rhs> {
    module
        .statements()
        .iter()
        .flatten()
        .filter_map(|statement| match statement {
            curios_anf::Statement::Let { rhs, .. } => Some(rhs),
            _ => None,
        })
}

/// A no-parameter single-field struct declaration — the classifier fixtures' newtype link.
#[cfg(test)]
pub fn newtype_struct(field: &str, type_: curios_core::Term) -> curios_core::StructDecl {
    let f = fresh(Some(field));
    curios_core::StructDecl {
        universe_context: curios_core::UniverseContext::empty(),
        arity: curios_core::Telescope::done(curios_core::Telescope::build([(f, type_)], ())),
        result_sort: curios_core::Term::type_ground(),
        module: Qualifier::empty(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

#[cfg(test)]
pub fn struct_type_of(name: &str) -> curios_core::Term {
    curios_core::Term::from(curios_core::Subterm::StructType(curios_core::StructType {
        name: nominal(name),
        universes: curios_core::Universes::default(),
        params: Vec::new(),
    }))
}

/// The parameter count of the function `atom` is, where it is one `module` defines.
#[cfg(test)]
pub fn arity(module: &curios_anf::Module, atom: curios_anf::Atom) -> Option<usize> {
    let curios_anf::Atom::Function(function) = atom else {
        return None;
    };

    module
        .function(function)
        .map(|function| function.params.len())
}
