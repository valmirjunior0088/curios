//! Straight-line constructor wrappers as argument-to-field templates, without copying or evaluating their bodies.
//!
//! Fields are parameters or already-bound atoms. Forwarding composes those mappings, never expands an aggregate tree. A locally constructed aggregate used as a field declines this small envelope; ordinary constructors build one row from their parameters. A wrapper may contain only aliases, immutable constructions and calls to already recognized wrappers, and may read no computed recursive member. Cyclic forwarding never enters the recognized set.

use {
    super::{Construction, Shape},
    crate::{Atom, FunctionId, Module, Rebindings, Rhs, Statement, Terminator, ValueId},
    std::collections::{BTreeMap, BTreeSet},
};

#[derive(Clone)]
enum TemplateAtom {
    Parameter(usize),
    Bound(Atom),
}

pub(super) struct ConstructionTemplate {
    pub(super) shape: Shape,
    arity: usize,
    fields: Vec<TemplateAtom>,
}

impl ConstructionTemplate {
    pub(super) fn instantiate(&self, arguments: &[Atom]) -> Option<Vec<Atom>> {
        if self.arity != arguments.len() {
            return None;
        }
        self.fields
            .iter()
            .map(|field| match field {
                TemplateAtom::Parameter(index) => arguments.get(*index).copied(),
                TemplateAtom::Bound(atom) => Some(*atom),
            })
            .collect()
    }
}

pub(super) fn construction_templates(
    module: &Module,
    bindings: &Rebindings,
) -> BTreeMap<FunctionId, ConstructionTemplate> {
    let definitions = module
        .statements()
        .iter()
        .flatten()
        .filter_map(|statement| match statement {
            Statement::Let { result, rhs } => Some((*result, rhs)),
            _ => None,
        })
        .collect::<BTreeMap<_, _>>();
    let mut users = BTreeMap::<FunctionId, BTreeSet<FunctionId>>::new();
    let mut work = module.function_ids().collect::<BTreeSet<_>>();
    for function in module.function_ids() {
        let body = module
            .block(module.function(function).expect("a function is live").body)
            .expect("its body is live");
        for &id in &body.statements {
            if let Some(Statement::Let {
                rhs: Rhs::Apply { callee, .. },
                ..
            }) = module.statement(id)
                && let Atom::Function(callee) = bindings.resolve(*callee)
            {
                users.entry(callee).or_default().insert(function);
            }
        }
    }
    let mut templates = BTreeMap::new();
    while let Some(function) = work.pop_first() {
        if templates.contains_key(&function) {
            continue;
        }
        if let Some(template) = recognize(module, bindings, &templates, &definitions, function) {
            templates.insert(function, template);
            work.extend(users.get(&function).into_iter().flatten().copied());
        }
    }
    templates
}

fn recognize(
    module: &Module,
    bindings: &Rebindings,
    templates: &BTreeMap<FunctionId, ConstructionTemplate>,
    all_definitions: &BTreeMap<ValueId, &Rhs>,
    function: FunctionId,
) -> Option<ConstructionTemplate> {
    let function = module.function(function)?;
    let body = module.block(function.body)?;
    let mut definitions = BTreeMap::new();
    for &id in &body.statements {
        let Statement::Let { result, rhs } = module.statement(id)? else {
            return None;
        };
        if rhs
            .operands()
            .iter()
            .any(|&atom| bindings.is_computed(bindings.resolve(atom)))
        {
            return None;
        }
        match rhs {
            Rhs::Alias(_) | Rhs::Product { .. } | Rhs::Construct { .. } => {}
            Rhs::Apply { callee, .. } if matches!(bindings.resolve(*callee), Atom::Function(id) if templates.contains_key(&id)) =>
                {}
            _ => return None,
        }
        definitions.insert(*result, rhs);
    }
    let Terminator::Return(returned) = body.terminator else {
        return None;
    };
    let returned = bindings.resolve(returned);
    let Atom::Value(result) = returned else {
        return None;
    };
    let rhs = definitions
        .get(&result)
        .copied()
        .or_else(|| all_definitions.get(&result).copied())?;
    let Construction { shape, fields } = match rhs {
        Rhs::Product { schema, fields } => Construction {
            shape: Shape::Product(*schema),
            fields: fields.clone(),
        },
        Rhs::Construct {
            constructor,
            fields,
        } => Construction {
            shape: Shape::Constructor(*constructor),
            fields: fields.clone(),
        },
        Rhs::Apply { callee, arguments } => {
            let Atom::Function(id) = bindings.resolve(*callee) else {
                return None;
            };
            let template = templates.get(&id)?;
            Construction {
                shape: template.shape,
                fields: template.instantiate(arguments)?,
            }
        }
        _ => return None,
    };
    let fields = fields
        .into_iter()
        .map(|atom| {
            let atom = bindings.resolve(atom);
            if bindings.is_computed(atom) {
                return None;
            }
            if let Atom::Value(value) = atom {
                if let Some(index) = function.params.iter().position(|&param| param == value) {
                    return Some(TemplateAtom::Parameter(index));
                }
                if definitions.contains_key(&value) {
                    return None;
                }
            }
            Some(TemplateAtom::Bound(atom))
        })
        .collect::<Option<Vec<_>>>()?;
    Some(ConstructionTemplate {
        shape,
        arity: function.params.len(),
        fields,
    })
}
