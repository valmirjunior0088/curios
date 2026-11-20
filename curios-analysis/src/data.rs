//! Whether a value of a type holds no type: what makes a value a datum, which strict positivity reads past.
//!
//! A type is data where its weak-head form is an intrinsic carrier whose element types are data, a tuple or function type whose parts are data, or an occurrence of a family outside the declarations analysed whose every payload, read at the arguments the occurrence gives, is data — never a sort and never a variable. No part of a value of such a type is a type, and no Curios term computes on a type, so a value standing where data is expected is one value at every instance of the declaration's type parameters, which is why a type parameter inside one is no occurrence ([`crate::positivity_vectors`]).
//!
//! A family's answer is computed once an analysis, as the parameters that must be data for its payloads to be: a payload naming no parameter is data or not whatever the arguments, and one naming a parameter as a type asks that parameter's argument. A family met again while its own answer is being computed is taken to need every parameter, which can only answer no where the answer was yes: `Mix(T) | base(x: T) | step(y: Mix(Type))` holds a type at `step(base(Nat))`, and taken to need none of its parameters on the way round it would be data at `Mix(Nat)`.

use {
    crate::{Declarations, Driver, binders, forceable, labelled},
    curios_core::{
        Advance, Free, FuncType, Global, InductType, Intrinsic, StructType, Subterm, Term,
        TupleType,
    },
    std::{
        collections::{BTreeMap, BTreeSet},
        mem,
    },
};

/// What a type needs of the variables standing for a family's parameters to be data: nothing it can have, or each of a set of them to be data.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Need {
    Never,
    Given(BTreeSet<usize>),
}

impl Need {
    fn join(self, other: Need) -> Need {
        match (self, other) {
            (Need::Given(mut left), Need::Given(right)) => {
                left.extend(right);
                Need::Given(left)
            }
            _ => Need::Never,
        }
    }
}

/// The classification one analysis keeps: the declarations it analyses, none of which is data, and the answers it has computed, by family — the parameter positions that must be data for a value of it to be, or none where no argument makes it data.
pub struct Data<'d> {
    declarations: Declarations<'d>,
    families: BTreeMap<Global, Option<BTreeSet<usize>>>,
    pending: BTreeSet<Global>,
}

impl<'d> Data<'d> {
    /// The classification of an analysis of `declarations`, nothing computed yet.
    pub fn new(declarations: Declarations<'d>) -> Self {
        Self {
            declarations,
            families: BTreeMap::new(),
            pending: BTreeSet::new(),
        }
    }

    /// The declarations the analysis this classification is part of analyses.
    pub fn declarations(&self) -> Declarations<'d> {
        self.declarations
    }

    /// Whether `type_` is data. A reduction the driver refuses answers no, and is kept in `refused` for the analysis to report.
    pub fn is_data<D: Driver>(
        &mut self,
        driver: &mut D,
        type_: &Term,
        refused: &mut Option<D::Error>,
    ) -> bool {
        let mut reading = Reading {
            driver,
            data: self,
            placeholders: Vec::new(),
            refused,
        };
        reading.need(type_) == Need::Given(BTreeSet::new())
    }
}

/// One question put to the classification: the variables standing for the parameters of the family being classified, by position.
struct Reading<'a, 'd, D: Driver> {
    driver: &'a mut D,
    data: &'a mut Data<'d>,
    placeholders: Vec<Free>,
    refused: &'a mut Option<D::Error>,
}

impl<D: Driver> Reading<'_, '_, D> {
    fn need(&mut self, type_: &Term) -> Need {
        let type_ = match forceable(type_) {
            false => type_.clone(),
            true => match self.driver.force(type_) {
                Ok(reduced) => reduced,
                Err(error) => {
                    self.refused.get_or_insert(error);
                    return Need::Never;
                }
            },
        };

        match &*type_ {
            Subterm::Var(var) => match var
                .as_free()
                .and_then(|free| self.placeholders.iter().position(|held| held == free))
            {
                Some(position) => Need::Given(BTreeSet::from([position])),
                None => Need::Never,
            },
            // A carrier whose values hold no type is data whatever is asked of it ([`Intrinsic::values_carrier`]); a list holds its elements, and a handle, a cell, a channel and a description of an effect, which no operation of the algebra is over, are read here.
            Subterm::Intrinsic(intrinsic) => match (intrinsic.values_carrier(), intrinsic) {
                (Some(carrier), _) if !carrier.holds_types() => Need::Given(BTreeSet::new()),
                (_, Intrinsic::HandleType) => Need::Given(BTreeSet::new()),
                (
                    _,
                    Intrinsic::ListType(element)
                    | Intrinsic::IoType(element)
                    | Intrinsic::CellType(element)
                    | Intrinsic::ChannelType(element),
                ) => self.need(element),
                _ => Need::Never,
            },
            Subterm::FuncType(FuncType { telescope, .. }) => {
                let mut need = Need::Given(BTreeSet::new());
                let mut cursor = telescope.cursor();
                while let Some((_, domain)) = cursor.entry() {
                    need = need.join(self.need(&domain));
                    cursor.advance_fresh(|hint| self.driver.fresh(hint));
                }
                let codomain = cursor.body().expect("a cursor past every entry");
                need.join(self.need(&codomain))
            }
            Subterm::TupleType(TupleType { telescope }) => {
                let mut need = Need::Given(BTreeSet::new());
                let mut cursor = telescope.cursor();
                while let Some((_, field)) = cursor.entry() {
                    need = need.join(self.need(&field));
                    cursor.advance_fresh(|hint| self.driver.fresh(hint));
                }
                need
            }
            Subterm::InductType(InductType { name, params, .. })
            | Subterm::StructType(StructType { name, params, .. }) => {
                let Some(required) = self.family(name) else {
                    return Need::Never;
                };
                required
                    .iter()
                    .fold(
                        Need::Given(BTreeSet::new()),
                        |need, position| match params.get(*position) {
                            Some(argument) => need.join(self.need(argument)),
                            None => Need::Never,
                        },
                    )
            }
            _ => Need::Never,
        }
    }

    /// The parameter positions `name` needs data at, or none where no argument makes it data: a family of the declarations analysed is none, being what the analysis is deciding.
    fn family(&mut self, name: &Global) -> Option<BTreeSet<usize>> {
        let declarations = self.data.declarations;
        if declarations.induct(name).is_some() || declarations.struct_(name).is_some() {
            return None;
        }
        if let Some(answer) = self.data.families.get(name) {
            return answer.clone();
        }
        if self.data.pending.contains(name) {
            let count = match (self.driver.induct_decl(name), self.driver.struct_decl(name)) {
                (Some(declaration), _) => declaration.arity().len(),
                (None, Some(declaration)) => declaration.arity.len(),
                (None, None) => return None,
            };
            return Some((0..count).collect());
        }

        let (arity, payloads) = match (self.driver.induct_decl(name), self.driver.struct_decl(name))
        {
            (Some(declaration), _) => {
                let declaration = declaration.clone();
                let params = binders(self.driver, declaration.arity());
                let arguments = params.iter().map(Term::free_var).collect::<Vec<_>>();
                let mut payloads = Vec::new();
                for (_, constructor) in declaration.constructors() {
                    let payload = constructor.open(&arguments);
                    payloads.extend(
                        labelled(self.driver, &payload)
                            .into_iter()
                            .map(|(_, type_)| type_),
                    );
                }
                (params, payloads)
            }
            (None, Some(declaration)) => {
                let declaration = declaration.clone();
                let params = binders(self.driver, &declaration.arity);
                let arguments = params.iter().map(Term::free_var).collect::<Vec<_>>();
                let payloads = labelled(self.driver, &declaration.fields_at(&arguments))
                    .into_iter()
                    .map(|(_, type_)| type_)
                    .collect();
                (params, payloads)
            }
            (None, None) => return None,
        };

        self.data.pending.insert(*name);
        let outer = mem::replace(&mut self.placeholders, arity);
        let need = payloads
            .iter()
            .fold(Need::Given(BTreeSet::new()), |need, payload| {
                need.join(self.need(payload))
            });
        self.placeholders = outer;
        self.data.pending.remove(name);

        let answer = match need {
            Need::Never => None,
            Need::Given(positions) => Some(positions),
        };
        self.data.families.insert(*name, answer.clone());
        answer
    }
}
