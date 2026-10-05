//! The key a witness declaration is spelled at, read off its lowered signature before anything elaborates.
//!
//! **A witness is known by its key before a question about its concept is answered**, so the key is a fact about how the declaration is written, never about which declarations have elaborated: the concept its signature applies, and each parameter by the type its head names. Registration reads the key again off the elaborated signature ([`HeadKey::of_whnf`]) and refuses a witness the two disagree on, so this reading decides only what it is certain of, and everything else is no key at all.
//!
//! **What a head is spelled by.** A type former, applied or not; a tuple type, by its shape; a definition bound to one of these, read through, and through its parameters where it is applied to all of them; and a function to an applied former, `(A) => State(S, A)`, which names the family it partially applies. A definition that computes its result spells nothing. Neither does a function to a definition's application, unless the definition is a name for an intrinsic former: reduction stops at a binder, so what stands under one is read as it is written.

use {
    super::{HeadKey, WitnessKey},
    curios_core::{Free, Func, Global, Subterm, Telescope, Term},
    curios_utilities::Plicity,
};

/// What a name is declared as, as far as a key reads it.
pub(crate) enum Written<'a> {
    /// A type former: an inductive type, a struct or a concept.
    Former,
    /// A definition, with the body it is bound to.
    Defined(&'a Term),
    /// Anything a key is not read through.
    Opaque,
}

/// The reading of one unit's witness signatures.
pub(crate) struct Spelled<'a> {
    /// What each name is declared as: the unit's own by its lowered form, another unit's by what it published.
    pub(crate) written: &'a dyn Fn(&Global) -> Written<'a>,
    /// The marks the concept `name` declares its parameters under, where it is one.
    pub(crate) marks: &'a dyn Fn(&Global) -> Option<Vec<Plicity>>,
}

impl Spelled<'_> {
    /// The concept and the key the witness declared at `type_` is spelled at: the application its signature ends in, behind its premise telescope, read parameter by parameter. `None` where the spelling decides either of them no further than elaboration would.
    pub(crate) fn key(&self, type_: &Term) -> Option<(Global, WitnessKey)> {
        let Subterm::Apply(apply) = &**terminal(type_) else {
            return None;
        };
        let concept = self.concept(&apply.head, &mut Vec::new())?;
        // Every parameter written, each under the mark the concept declares it by: the arguments are then the parameters in order, and nothing is left for elaboration to place or to infer.
        if !apply.plicities().eq((self.marks)(&concept)?) {
            return None;
        }
        let heads = apply
            .params()
            .map(|param| self.head(param, 0, &mut Vec::new()))
            .collect::<Option<Vec<_>>>()?;

        Some((concept, WitnessKey(heads)))
    }

    /// The concept `head` names: a concept's own name, or a definition bound to one and to nothing more.
    fn concept(&self, head: &Term, seen: &mut Vec<Global>) -> Option<Global> {
        let name = named(head)?;
        match (self.written)(&name) {
            Written::Former => (self.marks)(&name).map(|_| name),
            Written::Defined(body) if !seen.contains(&name) => {
                seen.push(name);
                self.concept(body, seen)
            }
            Written::Defined(_) | Written::Opaque => None,
        }
    }

    /// The head `term` is spelled by where it stands applied to `applied` arguments.
    fn head(&self, term: &Term, applied: usize, seen: &mut Vec<Global>) -> Option<HeadKey> {
        match &**term {
            Subterm::Apply(apply) => self.head(&apply.head, applied + apply.arguments.len(), seen),
            Subterm::Var(_) | Subterm::Instance(_) => {
                let name = named(term)?;
                match (self.written)(&name) {
                    Written::Former => Some(HeadKey::Nominal(name)),
                    Written::Defined(body) if !seen.contains(&name) => {
                        seen.push(name);
                        self.head(body, applied, seen)
                    }
                    Written::Defined(_) | Written::Opaque => None,
                }
            }
            // A function applied to none of its parameters is read under its binders. Applied to every one, each written out, it is its body, read as the body stands with what is left of the arguments; applied to some of them, or past a parameter the call leaves to be inferred, it is not read.
            Subterm::Func(func) => match applied {
                0 => self.under_binders(func),
                _ if applied >= func.telescope.len()
                    && func
                        .plicities()
                        .iter()
                        .all(|plicity| matches!(plicity, Plicity::Explicit)) =>
                {
                    let rest = applied - func.telescope.len();
                    self.head(func.telescope.terminal(), rest, seen)
                }
                _ => None,
            },
            _ if applied > 0 => None,
            Subterm::InductType(induct_type) => Some(HeadKey::Nominal(induct_type.name)),
            Subterm::StructType(struct_type) => Some(HeadKey::Nominal(struct_type.name)),
            Subterm::Intrinsic(intrinsic) => HeadKey::of_intrinsic(intrinsic),
            Subterm::TupleType(tuple_type) => Some(HeadKey::of_tuple_type(tuple_type)),
            _ => None,
        }
    }

    /// The head a function's body names: a type's own node, the former it applies, or the intrinsic former a definition it applies is a name for. Reduction does not descend under a binder, so a body is read as it is written, and any other definition applied there is not read through.
    fn under_binders(&self, func: &Func) -> Option<HeadKey> {
        let body = innermost(func);
        match &**body {
            Subterm::InductType(induct_type) => Some(HeadKey::Nominal(induct_type.name)),
            Subterm::StructType(struct_type) => Some(HeadKey::Nominal(struct_type.name)),
            Subterm::Intrinsic(intrinsic) => HeadKey::of_intrinsic(intrinsic),
            Subterm::TupleType(tuple_type) => Some(HeadKey::of_tuple_type(tuple_type)),
            Subterm::Apply(_) => {
                let name = *body.head_name().and_then(Free::as_global)?;
                match (self.written)(&name) {
                    Written::Former => Some(HeadKey::Nominal(name)),
                    Written::Defined(defined) => match &**defined {
                        Subterm::Func(former) => match &**innermost(former) {
                            Subterm::Intrinsic(intrinsic) => HeadKey::of_intrinsic(intrinsic),
                            _ => None,
                        },
                        _ => None,
                    },
                    Written::Opaque => None,
                }
            }
            _ => None,
        }
    }
}

/// The body under every binder `func` and the functions it returns take.
fn innermost(func: &Func) -> &Term {
    let mut body = func.telescope.terminal();
    while let Subterm::Func(inner) = &**body {
        body = inner.telescope.terminal();
    }

    body
}

/// The application a witness's lowered type ends in, behind its premise telescope.
fn terminal(type_: &Term) -> &Term {
    match &**type_ {
        Subterm::FuncType(func_type) => {
            let mut telescope = &func_type.telescope;
            loop {
                match telescope {
                    Telescope::Done(body) => return terminal(body),
                    Telescope::Cons(_, _, scope) => telescope = scope.body(),
                }
            }
        }
        _ => type_,
    }
}

/// The top-level name `term` is, the universes it is instantiated at left aside.
fn named(term: &Term) -> Option<Global> {
    match &**term {
        Subterm::Var(_) | Subterm::Instance(_) => {
            term.head_name().and_then(Free::as_global).copied()
        }
        _ => None,
    }
}
