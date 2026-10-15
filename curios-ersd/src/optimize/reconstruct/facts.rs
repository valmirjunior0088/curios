//! Immutable origin and field facts for reconstruction, conditional on normal return.
//!
//! Construction sites are shared graph nodes, not recursively expanded shapes. Known calls connect to their function's return; recognized constructor calls get a site whose fields are the actual arguments. Parameters, state reads, unknown calls and computed recursive members are opaque. Selectors register dependencies on the fields they discover, so recursion settles through a deterministic worklist. At most 32 origins reach one key before it widens to opaque; empty flow never proves equality.

use {
    super::{Construction, ConstructionTemplate, Shape},
    crate::{
        Atom, BlockId, Constant, ConstantId, ConstructorId, FunctionId, Module, ProductId,
        Rebindings, Rhs, Statement, StatementId, Terminator, ValueId,
    },
    std::collections::{BTreeMap, BTreeSet},
};

pub(super) const ORIGIN_LIMIT: usize = 32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Origin {
    Unit,
    Site(StatementId),
    Other,
}

#[derive(Clone, PartialEq, Eq)]
enum Origins {
    Known(BTreeSet<Origin>),
    Opaque,
}

impl Origins {
    fn empty() -> Self {
        Self::Known(BTreeSet::new())
    }
    fn one(origin: Origin) -> Self {
        Self::Known(BTreeSet::from([origin]))
    }
    fn join(&mut self, incoming: Self) -> bool {
        match (&mut *self, incoming) {
            (Self::Opaque, _) => false,
            (current, Self::Opaque) => {
                *current = Self::Opaque;
                true
            }
            (Self::Known(current), Self::Known(incoming)) => {
                let before = current.len();
                current.extend(incoming);
                if current.len() > ORIGIN_LIMIT {
                    *self = Self::Opaque;
                    true
                } else {
                    current.len() != before
                }
            }
        }
    }
    fn is_unit(&self) -> bool {
        matches!(self, Self::Known(origins) if origins.len() == 1 && origins.contains(&Origin::Unit))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Key {
    Value(ValueId),
    Block(BlockId),
    Return(FunctionId),
}

#[derive(Clone, Copy)]
enum Source {
    Atom(Atom),
    Key(Key),
}

impl Source {
    fn key(self) -> Option<Key> {
        match self {
            Self::Key(key) => Some(key),
            Self::Atom(Atom::Value(value)) => Some(Key::Value(value)),
            _ => None,
        }
    }
}

#[derive(Clone, Copy)]
enum Selection {
    Product(ProductId, usize),
    Payload(ConstructorId, usize),
}

struct Constraint {
    target: Key,
    source: Source,
    selection: Option<Selection>,
}

pub(super) struct ValueFacts {
    facts: BTreeMap<Key, Origins>,
    sites: BTreeMap<StatementId, Construction>,
    units: BTreeSet<ConstantId>,
}

impl ValueFacts {
    pub(super) fn analyze(
        module: &Module,
        bindings: &Rebindings,
        templates: &BTreeMap<FunctionId, ConstructionTemplate>,
    ) -> Self {
        curios_profile::profile!("reconstruction_facts");
        let mut facts = Self {
            facts: BTreeMap::new(),
            sites: BTreeMap::new(),
            units: module
                .constants()
                .iter()
                .enumerate()
                .filter_map(|(i, constant)| {
                    matches!(constant, Constant::Unit).then_some(ConstantId(i as u32))
                })
                .collect(),
        };
        let mut constraints = Vec::new();
        for function in module.function_ids() {
            let definition = module.function(function).expect("a function is live");
            for &param in &definition.params {
                facts.facts.insert(Key::Value(param), Origins::Opaque);
            }
            constraints.push(Constraint {
                target: Key::Return(function),
                source: Source::Key(Key::Block(definition.body)),
                selection: None,
            });
        }
        for group in module.rec_groups().iter().flatten() {
            for member in &group.values {
                facts
                    .facts
                    .insert(Key::Value(member.value), Origins::Opaque);
            }
        }
        for (i, block) in module.blocks().iter().enumerate() {
            let Some(block) = block else { continue };
            if let Terminator::Return(atom) = block.terminator {
                constraints.push(Constraint {
                    target: Key::Block(BlockId(i as u32)),
                    source: Source::Atom(atom),
                    selection: None,
                });
            }
        }
        for (i, statement) in module.statements().iter().enumerate() {
            let Some(Statement::Let { result, rhs }) = statement else {
                continue;
            };
            let target = Key::Value(*result);
            let site = StatementId(i as u32);
            macro_rules! copy {
                ($source:expr) => {
                    constraints.push(Constraint {
                        target,
                        source: $source,
                        selection: None,
                    })
                };
            }
            let construction = match rhs {
                Rhs::Product { schema, fields } => Some(Construction {
                    shape: Shape::Product(*schema),
                    fields: fields.clone(),
                }),
                Rhs::Construct {
                    constructor,
                    fields,
                } => Some(Construction {
                    shape: Shape::Constructor(*constructor),
                    fields: fields.clone(),
                }),
                Rhs::Apply { callee, arguments } => match bindings.resolve(*callee) {
                    Atom::Function(function) => templates.get(&function).and_then(|template| {
                        Some(Construction {
                            shape: template.shape,
                            fields: template.instantiate(arguments)?,
                        })
                    }),
                    _ => None,
                },
                _ => None,
            };
            if let Some(construction) = construction {
                facts.sites.insert(site, construction);
                facts.facts.insert(target, Origins::one(Origin::Site(site)));
                continue;
            }
            match rhs {
                Rhs::Alias(atom) => copy!(Source::Atom(*atom)),
                Rhs::Apply { callee, .. } => match bindings.resolve(*callee) {
                    Atom::Function(function) => copy!(Source::Key(Key::Return(function))),
                    _ => {
                        facts.facts.insert(target, Origins::Opaque);
                    }
                },
                Rhs::Project {
                    schema,
                    product,
                    field,
                } => constraints.push(Constraint {
                    target,
                    source: Source::Atom(*product),
                    selection: Some(Selection::Product(*schema, *field as usize)),
                }),
                Rhs::MatchVariant {
                    scrutinee,
                    arms,
                    default,
                    ..
                } => {
                    for arm in arms {
                        copy!(Source::Key(Key::Block(arm.block)));
                        for (i, &binding) in arm.bindings.iter().enumerate() {
                            constraints.push(Constraint {
                                target: Key::Value(binding),
                                source: Source::Atom(*scrutinee),
                                selection: Some(Selection::Payload(arm.constructor, i)),
                            });
                        }
                    }
                    if let Some(default) = default {
                        constraints.push(Constraint {
                            target,
                            source: Source::Key(Key::Block(*default)),
                            selection: None,
                        });
                    }
                }
                Rhs::SwitchBool {
                    if_false, if_true, ..
                } => {
                    copy!(Source::Key(Key::Block(*if_false)));
                    copy!(Source::Key(Key::Block(*if_true)));
                }
                Rhs::SwitchNat { cases, default, .. } => {
                    for case in cases {
                        copy!(Source::Key(Key::Block(case.block)));
                    }
                    copy!(Source::Key(Key::Block(*default)));
                }
                Rhs::FoldNat { zero, step, .. } => {
                    copy!(Source::Key(Key::Block(*zero)));
                    copy!(Source::Key(Key::Block(step.block)));
                    for value in [step.predecessor, step.hypothesis] {
                        facts.facts.insert(Key::Value(value), Origins::Opaque);
                    }
                }
                Rhs::FoldSequence { empty, step, .. } => {
                    copy!(Source::Key(Key::Block(*empty)));
                    copy!(Source::Key(Key::Block(step.block)));
                    for value in [step.element, step.suffix, step.accumulator] {
                        facts.facts.insert(Key::Value(value), Origins::Opaque);
                    }
                }
                Rhs::UnconsSequence { empty, cons, .. } => {
                    copy!(Source::Key(Key::Block(*empty)));
                    copy!(Source::Key(Key::Block(cons.block)));
                    for value in [cons.element, cons.suffix] {
                        facts.facts.insert(Key::Value(value), Origins::Opaque);
                    }
                }
                Rhs::Operation { .. } => {
                    facts.facts.insert(target, Origins::one(Origin::Other));
                }
                Rhs::Product { .. } | Rhs::Construct { .. } => {
                    unreachable!("construction sites were seeded above")
                }
                Rhs::Sequence { .. }
                | Rhs::Cell { .. }
                | Rhs::Channel { .. }
                | Rhs::Foreign { .. }
                | Rhs::Intrinsic { .. }
                | Rhs::Suspension { .. } => {
                    facts.facts.insert(target, Origins::Opaque);
                }
            }
        }
        facts.solve(module, constraints);
        facts
    }

    fn source(&self, source: Source) -> Origins {
        if let Some(key) = source.key() {
            return self.facts.get(&key).cloned().unwrap_or_else(Origins::empty);
        }
        match source {
            Source::Atom(Atom::Constant(id)) if self.units.contains(&id) => {
                Origins::one(Origin::Unit)
            }
            _ => Origins::one(Origin::Other),
        }
    }

    fn solve(&mut self, module: &Module, constraints: Vec<Constraint>) {
        let mut users = BTreeMap::<Key, BTreeSet<usize>>::new();
        for (i, constraint) in constraints.iter().enumerate() {
            if let Some(key) = constraint.source.key() {
                users.entry(key).or_default().insert(i);
            }
        }
        let mut work = (0..constraints.len()).collect::<BTreeSet<_>>();
        while let Some(i) = work.pop_first() {
            let constraint = &constraints[i];
            let mut incoming = self.source(constraint.source);
            if let Some(selection) = constraint.selection {
                let mut dependencies = BTreeSet::new();
                incoming = self.select(module, incoming, selection, &mut dependencies);
                for dependency in dependencies {
                    users.entry(dependency).or_default().insert(i);
                }
            }
            if self
                .facts
                .entry(constraint.target)
                .or_insert_with(Origins::empty)
                .join(incoming)
            {
                work.extend(users.get(&constraint.target).into_iter().flatten().copied());
            }
        }
    }

    fn select(
        &self,
        module: &Module,
        source: Origins,
        selection: Selection,
        dependencies: &mut BTreeSet<Key>,
    ) -> Origins {
        let Origins::Known(origins) = source else {
            return Origins::Opaque;
        };
        let mut result = Origins::empty();
        for origin in origins {
            let Origin::Site(id) = origin else {
                return Origins::Opaque;
            };
            let construction = &self.sites[&id];
            let field = match (selection, construction.shape) {
                (Selection::Product(schema, i), Shape::Product(actual)) if schema == actual => i,
                (Selection::Payload(constructor, i), Shape::Constructor(actual))
                    if constructor == actual =>
                {
                    i
                }
                (Selection::Payload(constructor, _), Shape::Constructor(actual))
                    if module.constructor(constructor).unwrap().family
                        == module.constructor(actual).unwrap().family =>
                {
                    continue;
                }
                _ => return Origins::Opaque,
            };
            let Some(&atom) = construction.fields.get(field) else {
                return Origins::Opaque;
            };
            let source = Source::Atom(atom);
            if let Some(key) = source.key() {
                dependencies.insert(key);
            }
            result.join(self.source(source));
        }
        result
    }

    pub(super) fn unit_field(&self, base: Atom, schema: ProductId, field: usize) -> bool {
        let Origins::Known(origins) = self.source(Source::Atom(base)) else {
            return false;
        };
        !origins.is_empty()
            && origins.iter().all(|origin| {
                let Origin::Site(id) = origin else {
                    return false;
                };
                let construction = &self.sites[id];
                construction.shape == Shape::Product(schema)
                    && construction
                        .fields
                        .get(field)
                        .is_some_and(|&atom| self.source(Source::Atom(atom)).is_unit())
            })
    }
}
