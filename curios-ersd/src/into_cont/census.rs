//! The sequence-usage census behind the door's settle insertion: which constructor and product fields hold values that are only ever *indexed* — read by position, length, window, fold, or map — and never re-grown or moved somewhere the census cannot see. The fact is recorded here, over the arena, because this is the last stage at which family and product identity exist: at Cont a variant is an anonymous tuple, so a per-field fact keyed there would be cross-poisoned by every unrelated tuple of the same width. The door spends the fact by settling the stores into a marked field, which is what lets the reshaped map's child arrays be flat without any library spelling; see the list-half refinement in `documentation/roadmap/map-wall-classes-spec.md`.
//!
//! The census follows exactly two shapes of indirection and no others: a computation-free alias, and an argument of a saturated known call, which fares as the receiving parameter fares — without which every read would be poisoned, since the surface reaches `ListGet` through the `/sys/List/get` wrapper and an `Apply` is what a read looks like at this level. Everything else — a closure call, a return, a store, a scrutinee — poisons, because following it would be the interprocedural demand analysis this fact deliberately is not. Recursion is read coinductively: a value revisited during classification is assumed to hold, which is the greatest fixpoint of a safety property and sound for it. A field is marked only when at least one read carries *list* evidence — a `List`-shaped use — so a field the program never reads, or one holding a packed binary, is never wrapped in a list operation it cannot carry.

use {
    super::super::{
        Atom, Block, ConstructorId, Intrinsic, Module, ProductId, Rhs, SequenceGrain, SequenceOp,
        Statement, Terminator, ValueId,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// What one occurrence of a value asks of it, from the census's point of view.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UseKind {
    /// An indexing-shaped `List` use — the evidence that both proves the value list-shaped and makes settling worthwhile.
    ListEvidence,
    /// An indexing-shaped packed-binary use: harmless, but no license to wrap the value in a list operation.
    Neutral,
    /// The value's fate is another value's: an alias result, or the parameter receiving it at a saturated known call.
    Alias(ValueId),
    /// Anything else — growth, escape, or a shape the census does not follow.
    Poison,
}

/// The census's verdicts: the constructor and product fields whose every read is indexing-shaped, with list evidence.
#[derive(Debug, Default)]
pub(super) struct SequenceFacts {
    constructors: BTreeSet<(ConstructorId, usize)>,
    products: BTreeSet<(ProductId, usize)>,
}

impl SequenceFacts {
    pub(super) fn indexed_only_constructor(
        &self,
        constructor: ConstructorId,
        field: usize,
    ) -> bool {
        self.constructors.contains(&(constructor, field))
    }

    pub(super) fn indexed_only_product(&self, product: ProductId, field: usize) -> bool {
        self.products.contains(&(product, field))
    }
}

/// Walk the whole arena once: collect every value's uses and every field read, then mark the fields whose reads all hold.
pub(super) fn sequence_census(module: &Module) -> SequenceFacts {
    let mut uses: BTreeMap<ValueId, Vec<UseKind>> = BTreeMap::new();
    let mut constructor_reads: BTreeMap<(ConstructorId, usize), Vec<ValueId>> = BTreeMap::new();
    let mut product_reads: BTreeMap<(ProductId, usize), Vec<ValueId>> = BTreeMap::new();

    let mut record = |atom: &Atom, kind: UseKind| {
        if let Atom::Value(value) = atom {
            uses.entry(*value).or_default().push(kind);
        }
    };

    for statement in module.statements().iter().flatten() {
        let Statement::Let { result, rhs } = statement else {
            continue;
        };
        match rhs {
            Rhs::Alias(atom) => record(atom, UseKind::Alias(*result)),
            // A saturated known call hands each argument to a parameter, and the argument fares as that parameter fares — the deferral that sees through the `/sys` wrappers and the library's own indexing helpers. A closure callee resolves no parameter, so its arguments poison.
            Rhs::Apply { callee, arguments } => {
                let params = match callee {
                    Atom::Function(function) => module
                        .function(*function)
                        .map(|function| function.params.as_slice()),
                    _ => None,
                };
                for (position, atom) in arguments.iter().enumerate() {
                    let kind = params
                        .and_then(|params| params.get(position))
                        .map(|&param| UseKind::Alias(param))
                        .unwrap_or(UseKind::Poison);
                    record(atom, kind);
                }
            }
            Rhs::Sequence {
                operation,
                operands,
            } => {
                for (position, atom) in operands.iter().enumerate() {
                    record(atom, sequence_use(*operation, position));
                }
            }
            Rhs::Intrinsic {
                intrinsic: Intrinsic::ListMap,
                operands,
            } => {
                for (position, atom) in operands.iter().enumerate() {
                    let kind = match position {
                        0 => UseKind::ListEvidence,
                        _ => UseKind::Poison,
                    };
                    record(atom, kind);
                }
            }
            Rhs::FoldSequence {
                grain, scrutinee, ..
            }
            | Rhs::UnconsSequence {
                grain, scrutinee, ..
            } => {
                let kind = match grain {
                    SequenceGrain::List => UseKind::ListEvidence,
                    SequenceGrain::Bin(_) => UseKind::Neutral,
                };
                record(scrutinee, kind);
            }
            Rhs::MatchVariant {
                scrutinee, arms, ..
            } => {
                record(scrutinee, UseKind::Poison);
                for arm in arms {
                    for (field, &binding) in arm.bindings.iter().enumerate() {
                        constructor_reads
                            .entry((arm.constructor, field))
                            .or_default()
                            .push(binding);
                    }
                }
            }
            Rhs::Project {
                schema,
                product,
                field,
            } => {
                record(product, UseKind::Poison);
                product_reads
                    .entry((*schema, *field as usize))
                    .or_default()
                    .push(*result);
            }
            // Every other occurrence — a construction, a scalar operation, a cell, a foreign call, a dispatch scrutinee — moves the value somewhere this census does not follow.
            rhs => {
                for atom in rhs_atoms(rhs) {
                    record(atom, UseKind::Poison);
                }
            }
        }
    }

    // A block result flows into the statement that owns the block, which the census does not connect; a returned or exited value is poisoned like any other escape.
    for block in module.blocks().iter().flatten() {
        let Block { terminator, .. } = block;
        match terminator {
            Terminator::Return(atom) | Terminator::Exit(atom) => record(atom, UseKind::Poison),
            Terminator::Unreachable => {}
        }
    }

    let mut facts = SequenceFacts::default();
    let classifier = Classifier { uses: &uses };
    for (key, reads) in constructor_reads {
        if classifier.indexed_only(&reads) {
            facts.constructors.insert(key);
        }
    }
    for (key, reads) in product_reads {
        if classifier.indexed_only(&reads) {
            facts.products.insert(key);
        }
    }
    facts
}

/// How a value fares as operand `position` of `operation`: the carrier positions of the read forms are indexing-shaped, growth and construction poison every position, and a position a list could never occupy by typing is poisoned rather than reasoned about.
fn sequence_use(operation: SequenceOp, position: usize) -> UseKind {
    match (operation, position) {
        (SequenceOp::ListLen, 0) | (SequenceOp::ListGet | SequenceOp::ListSlice, 0) => {
            UseKind::ListEvidence
        }
        (SequenceOp::BinLen(_) | SequenceOp::BinEql(_), _) => UseKind::Neutral,
        (SequenceOp::BinGet(_) | SequenceOp::BinSlice(_), 0) => UseKind::Neutral,
        _ => UseKind::Poison,
    }
}

/// Every atom of a right-hand side, for the arms classified wholesale. `Apply` keeps an arm for exhaustiveness, though the census classifies it before reaching here.
fn rhs_atoms(rhs: &Rhs) -> Vec<&Atom> {
    match rhs {
        Rhs::Alias(atom) => vec![atom],
        Rhs::Apply { callee, arguments } => {
            std::iter::once(callee).chain(arguments.iter()).collect()
        }
        Rhs::Operation { operands, .. }
        | Rhs::Sequence { operands, .. }
        | Rhs::Cell { operands, .. }
        | Rhs::Foreign { operands, .. }
        | Rhs::Intrinsic { operands, .. } => operands.iter().collect(),
        Rhs::Product { fields, .. } | Rhs::Construct { fields, .. } => fields.iter().collect(),
        Rhs::Project { product, .. } => vec![product],
        Rhs::MatchVariant { scrutinee, .. }
        | Rhs::SwitchBool { scrutinee, .. }
        | Rhs::SwitchNat { scrutinee, .. }
        | Rhs::FoldNat { scrutinee, .. }
        | Rhs::FoldSequence { scrutinee, .. }
        | Rhs::UnconsSequence { scrutinee, .. } => vec![scrutinee],
    }
}

struct Classifier<'a> {
    uses: &'a BTreeMap<ValueId, Vec<UseKind>>,
}

impl Classifier<'_> {
    /// Every read holds — no poison anywhere, aliases included — and at least one use across the reads carries list evidence.
    fn indexed_only(&self, reads: &[ValueId]) -> bool {
        let mut evidence = false;
        let mut visited = BTreeSet::new();
        for &read in reads {
            match self.value_holds(read, &mut visited) {
                None => return false,
                Some(found) => evidence |= found,
            }
        }
        evidence
    }

    /// `None` when any use poisons; otherwise whether list evidence was seen. `visited` guards the alias walk — cycles cannot occur in ANF, but a shared alias target is classified once.
    fn value_holds(&self, value: ValueId, visited: &mut BTreeSet<ValueId>) -> Option<bool> {
        if !visited.insert(value) {
            return Some(false);
        }
        let mut evidence = false;
        for kind in self.uses.get(&value).map(Vec::as_slice).unwrap_or(&[]) {
            match kind {
                UseKind::ListEvidence => evidence = true,
                UseKind::Neutral => {}
                UseKind::Alias(result) => evidence |= self.value_holds(*result, visited)?,
                UseKind::Poison => return None,
            }
        }
        Some(evidence)
    }
}
