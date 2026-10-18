//! A term a report shows, as a program writes it where the report's reader stands: each witness resolution puts back there left as an open slot, and each method projected off one called as a program calls it.
//!
//! **What a report leaves out for elaboration to fill is settled by the procedure that fills it, where the reader stands.** A witness has no spelling, so a report leaves one out exactly where resolution, run where the reader stands, answers with a dictionary that converts with it, and declines the term elsewhere. The answer is resolution's own ([`resolve_witness`]), under the binders the term opens, the equations of the arms it enters, and the definitions as they elaborated: a model of it beside the printer would be a second statement held to the first by nothing. Why is `documentation/design/tools/a-diagnostic-spells-what-its-reader-can-write.md`.
//!
//! **The term is walked closed**, as `curios-core`'s marking walk walks one: no scope is opened and closed again, the variables standing for the binders entered are kept, and what is asked is released against them. A node nothing in changed comes back as it was, so a term keeps its sharing. A binder is entered into the context at its type, under its mark, so a `use` one joins the witness scope; an arm is entered with the equations elaboration typed it under; a local `let` is entered at its definition. A scope the walk crosses with no type for its binders leaves a question about them unanswered, and an arm whose equations it could not install leaves every witness under it as it stands.
//!
//! **A question leaves the elaborator as it found it**: it runs as an oracle, so nothing parks, and every solution it lands is rolled back. What it needs of an unfinished declaration and what it asks of the declarations around it are set aside by [`written`]'s callers ([`Context::reporting`]).

use {
    crate::{
        Binders, Context, Entailed, InductCase, Outcome, Resolution, convert_outcome, entail,
        enter_induct_case, reduce_with, refine_head, resolve_witness, synth_neutral,
        trivially_inhabited, zonk_solved_term_metas,
    },
    curios_core::{
        Apply, Argument, Arity, Atom, Bound, CalleeId, Carrier, Cases, Field, Free, Func, FuncType,
        Global, InductArm, InductType, Infix, Intrinsic, Let, LetBinding, Many, Match, MatchResult,
        Nat, Proj, Rec, Scope, Struct, StructType, Subterm, Telescope, Term, Three, Transient,
        TupleType, Var, Visit, WitnessOrigin,
    },
    curios_num::Binary,
    curios_utilities::{Plicity, recurse},
    std::{
        cell::RefCell,
        collections::{HashMap, VecDeque},
        rc::Rc,
    },
};

/// `term` as a program writes it for a reader standing in the frames the context stands in: see the module documentation. Infallible: a question that cannot be answered leaves its witness as it stands.
pub(crate) fn written(context: &mut Context, term: &Term) -> Term {
    Writing {
        entered: Vec::new(),
        layers: vec![HashMap::new()],
        unrefined: 0,
    }
    .term(context, term)
}

/// The walk's state beside the context it enters binders into.
struct Writing {
    /// The terms standing for the binders entered, outermost first: a loose index `k` names the `k`th from the end.
    entered: Vec<Term>,
    /// What a term with no loose index was written as, by node: one table for each witness scope the walk stands in, since a `use` binder or an arm's equation changes what resolution answers and nothing else the walk enters does.
    layers: Vec<HashMap<usize, Term>>,
    /// How many arms the walk stands in whose equations it could not install: a witness under one is left as it stands.
    unrefined: usize,
}

impl Writing {
    fn term(&mut self, context: &mut Context, term: &Term) -> Term {
        if matches!(&**term, Subterm::Var(_) | Subterm::Sort(..)) {
            return term.clone();
        }
        let closed = term.closed();
        if closed && let Some(written) = self.layer().get(&term.identity()) {
            return written.clone();
        }

        let written = recurse(|| self.node(context, term));
        if closed {
            self.layer_mut().insert(term.identity(), written.clone());
        }

        written
    }

    fn layer(&self) -> &HashMap<usize, Term> {
        self.layers
            .last()
            .expect("the walk stands in a witness scope")
    }

    fn layer_mut(&mut self) -> &mut HashMap<usize, Term> {
        self.layers
            .last_mut()
            .expect("the walk stands in a witness scope")
    }

    fn node(&mut self, context: &mut Context, term: &Term) -> Term {
        match &**term {
            Subterm::FuncType(FuncType { telescope }) => {
                let telescope =
                    self.telescope(context, telescope, &mut |writing, context, body| {
                        writing.term(context, body)
                    });
                rebuilt(term, Subterm::FuncType(FuncType { telescope }))
            }
            Subterm::Func(Func { telescope }) => {
                let telescope =
                    self.telescope(context, telescope, &mut |writing, context, body| {
                        writing.term(context, body)
                    });
                rebuilt(term, Subterm::Func(Func { telescope }))
            }
            Subterm::TupleType(TupleType { telescope }) => {
                let telescope = self.telescope(context, telescope, &mut |_, _, ()| ());
                rebuilt(term, Subterm::TupleType(TupleType { telescope }))
            }
            Subterm::Let(let_) => self.let_(context, term, let_),
            Subterm::Rec(rec) => self.rec(context, term, rec),
            Subterm::Match(elimination) => self.match_(context, term, elimination),
            Subterm::Apply(Apply { head, arguments }) => {
                if let Subterm::Proj(Proj {
                    head: dictionary,
                    field: Field::Struct(concept, index),
                }) = &**head
                    && let Some(called) =
                        self.method(context, dictionary, *concept, *index, Some(arguments))
                {
                    return called;
                }
                let callee = callee_of(head);
                let head = self.term(context, head);
                let arguments = arguments
                    .iter()
                    .map(|argument| Argument {
                        term: match argument.plicity {
                            Plicity::Witness => {
                                self.witness(context, &argument.term, callee.clone())
                            }
                            _ => self.term(context, &argument.term),
                        },
                        plicity: argument.plicity,
                    })
                    .collect();
                rebuilt(term, Subterm::Apply(Apply { head, arguments }))
            }
            Subterm::Proj(Proj {
                head: dictionary,
                field: Field::Struct(concept, index),
            }) => match self.method(context, dictionary, *concept, *index, None) {
                Some(called) => called,
                None => self.children(context, term),
            },
            Subterm::InductType(induct_type) => {
                let marks = context
                    .induct_decl(&induct_type.name)
                    .map(|declaration| declaration.arity.marks())
                    .unwrap_or_default();
                let callee = CalleeId::Function(Free::Global(induct_type.name));
                let mut written = induct_type.clone();
                written.params = self.marked(context, &induct_type.params, &marks, &callee);
                written.indices = induct_type
                    .indices
                    .iter()
                    .map(|index| self.term(context, index))
                    .collect();
                rebuilt(term, Subterm::InductType(written))
            }
            Subterm::StructType(struct_type) => {
                let marks = context
                    .struct_decl(&struct_type.name)
                    .map(|declaration| declaration.arity.marks())
                    .unwrap_or_default();
                let callee = CalleeId::Function(Free::Global(struct_type.name));
                let mut written = struct_type.clone();
                written.params = self.marked(context, &struct_type.params, &marks, &callee);
                rebuilt(term, Subterm::StructType(written))
            }
            Subterm::Variant(variant) => {
                // A constructor is called by its family's parameters, every one hidden, then its payload: one mark for each, in that order.
                let marks = context
                    .induct_decl(&variant.name)
                    .and_then(|declaration| {
                        declaration
                            .constructors
                            .iter()
                            .find(|(tag, _)| *tag == variant.tag)
                            .map(|(_, signature)| signature.plicities())
                    })
                    .unwrap_or_default();
                let callee = CalleeId::Function(Free::Global(variant.name));
                let (leading, payload) = marks.split_at(variant.params.len().min(marks.len()));
                let mut written = variant.clone();
                written.params = self.marked(context, &variant.params, leading, &callee);
                written.payload = self.marked(context, &variant.payload, payload, &callee);
                rebuilt(term, Subterm::Variant(written))
            }
            Subterm::Struct(literal) => {
                let (parameters, fields) = context
                    .struct_decl(&literal.name)
                    .map(|declaration| (declaration.arity.marks(), declaration.fields().marks()))
                    .unwrap_or_default();
                let callee = CalleeId::Structure(literal.name);
                let mut written = literal.clone();
                written.params = self.marked(context, &literal.params, &parameters, &callee);
                written.fields = self.marked(context, &literal.fields, &fields, &callee);
                rebuilt(term, Subterm::Struct(written))
            }
            // A match as it is written has arms whose equations are not compiled yet: what is under them is left as it stands.
            Subterm::Transient(Transient::Matrix(_)) => {
                self.unrefined(|writing| writing.children(context, term))
            }
            _ => self.children(context, term),
        }
    }

    /// `members`, each under the mark `marks` gives it, a `use` member settled ([`Writing::witness`]). A member past the marks is walked as it stands.
    fn marked(
        &mut self,
        context: &mut Context,
        members: &[Term],
        marks: &[Plicity],
        callee: &CalleeId,
    ) -> Vec<Term> {
        members
            .iter()
            .enumerate()
            .map(|(slot, member)| match marks.get(slot) {
                Some(Plicity::Witness) => self.witness(context, member, callee.clone()),
                _ => self.term(context, member),
            })
            .collect()
    }

    /// `witness`, standing at a `use` position under the binders entered, as a program writes it there: the open slot elaboration would mint for it, where resolution answers with it, and itself walked otherwise.
    fn witness(&mut self, context: &mut Context, witness: &Term, callee: CalleeId) -> Term {
        match self.restored(context, witness) {
            true => context.open_witness_slot(WitnessOrigin {
                func: callee,
                binder: String::new(),
            }),
            false => self.term(context, witness),
        }
    }

    /// Whether resolution, asked where the walk stands for the concept application `witness` is a dictionary of, answers with a dictionary that converts with it.
    fn restored(&self, context: &mut Context, witness: &Term) -> bool {
        // An open slot is one already, and nothing is known of a witness under an arm whose equations were not installed.
        if self.unrefined > 0 || matches!(&**witness, Subterm::Metavar(_)) {
            return false;
        }
        let opened = self.released(witness);
        let Some(goal) = dictionary_type(context, &opened) else {
            return false;
        };

        context.asking(|context| match resolve_witness(context, &goal, &opened) {
            Ok(Resolution::Solved(answer)) => {
                let answer = zonk_solved_term_metas(context, &answer);
                matches!(
                    convert_outcome(context, &goal, &answer, &opened),
                    Ok(Outcome::Converts)
                )
            }
            _ => false,
        })
    }

    /// The method at field `index` of `concept`, projected off `dictionary` and called with `arguments` where it heads a call, as a program calls it — its operator where the field has one and every proof the operator leaves out is proved again where the walk stands, its wrapper's call otherwise — where resolution would put the dictionary back. `None` where the field is no method or the dictionary stays, which leaves the projection as it stands.
    fn method(
        &mut self,
        context: &mut Context,
        dictionary: &Term,
        concept: Global,
        index: usize,
        arguments: Option<&[Argument]>,
    ) -> Option<Term> {
        let Global::Authored(path) = concept else {
            return None;
        };
        let label = context.concept(&concept)?.fields.get(index)?.clone();
        // A superclass edge is a dictionary, written where it stands as one.
        let fields = context.struct_decl(&concept)?.fields().marks();
        if fields.get(index) != Some(&Plicity::Explicit) || !self.restored(context, dictionary) {
            return None;
        }

        if let Some(arguments) = arguments
            && let Some(op) = context.syntax().operator.operator_for(&path, &label)
            && let [left, right, filled @ ..] = arguments
            && left.plicity == Plicity::Explicit
            && right.plicity == Plicity::Explicit
            && filled
                .iter()
                .all(|argument| argument.plicity == Plicity::Implicit)
            && self.proved_again(context, dictionary, &concept, index, arguments)
        {
            let left = self.term(context, &left.term);
            let right = self.term(context, &right.term);

            return Some(Term::from(Subterm::Transient(Transient::Infix(Infix {
                op,
                left,
                right,
            }))));
        }

        // The wrapper takes the concept's parameters, then the witness, then — where it takes the method's own in its one group — the method's.
        let opened = self.released(dictionary);
        let type_ = dictionary_type(context, &opened)?;
        let application = reduce_with(context, &type_).ok()?;
        let Subterm::StructType(StructType { params, .. }) = &*application else {
            return None;
        };
        let wrapper = Global::Authored(path.with(&label));
        let merged = context
            .instantiate_assumption(&Free::Global(wrapper), None)
            .ok()
            .flatten()
            .is_some_and(|(type_, _)| {
                matches!(&*type_, Subterm::FuncType(function) if function.plicities().len() > params.len() + 1)
            });
        let leading = params
            .iter()
            .map(|parameter| Argument {
                term: self.closed_over(parameter),
                plicity: Plicity::Implicit,
            })
            .chain(std::iter::once(Argument {
                term: context.open_witness_slot(WitnessOrigin {
                    func: CalleeId::Function(Free::Global(wrapper)),
                    binder: String::new(),
                }),
                plicity: Plicity::Witness,
            }))
            .collect::<Vec<_>>();
        let reference = Term::var(Var::free(Free::Global(wrapper)));
        let own = arguments.map(|arguments| {
            arguments
                .iter()
                .map(|argument| Argument {
                    term: match argument.plicity {
                        Plicity::Witness => self.witness(
                            context,
                            &argument.term,
                            CalleeId::Function(Free::Global(wrapper)),
                        ),
                        _ => self.term(context, &argument.term),
                    },
                    plicity: argument.plicity,
                })
                .collect::<Vec<_>>()
        });

        Some(match (merged, own) {
            (true, Some(own)) => applied(reference, leading.into_iter().chain(own).collect()),
            // The method as a value: its wrapper, which a checked position instantiates and eta-expands.
            (true, None) => reference,
            (false, own) => {
                let field = applied(reference, leading);
                match own {
                    Some(own) => applied(field, own),
                    None => field,
                }
            }
        })
    }

    /// Whether every proof the operator form of a call of the method at `index` leaves out — what stands after its two operands — is proved again where the walk stands by the procedure that proves a bound from the facts in scope.
    fn proved_again(
        &self,
        context: &mut Context,
        dictionary: &Term,
        concept: &Global,
        index: usize,
        arguments: &[Argument],
    ) -> bool {
        if arguments.len() == 2 {
            return true;
        }
        let projection = Term::from(Subterm::Proj(Proj {
            head: self.released(dictionary),
            field: Field::Struct(*concept, index),
        }));
        let opened = arguments
            .iter()
            .map(|argument| self.released(&argument.term))
            .collect::<Vec<_>>();

        context.asking(|context| {
            let Ok(Some(type_)) = synth_neutral(context, &Binders::default(), &[], &projection)
            else {
                return false;
            };
            let Ok(reduced) = reduce_with(context, &type_) else {
                return false;
            };
            let Subterm::FuncType(FuncType { telescope }) = &*reduced else {
                return false;
            };
            let mut cursor = telescope.cursor();
            for (slot, argument) in opened.iter().enumerate() {
                let Some((_, bound)) = cursor.entry() else {
                    return false;
                };
                // Filled as elaboration fills an omitted bound: by its reduct's one inhabitant, and otherwise from the facts in scope.
                if slot >= 2 {
                    let Ok((reduced, inhabitant)) = trivially_inhabited(context, &bound) else {
                        return false;
                    };
                    if inhabitant.is_none()
                        && !matches!(entail(context, &bound, &reduced), Ok(Entailed::Proved(_)))
                    {
                        return false;
                    }
                }
                cursor.advance(argument.clone());
            }

            true
        })
    }

    /// A telescope's entries entered in order, each type walked and its binder entered into the context at that type under its mark, and `payload` walked under every binder.
    fn telescope<B: Bound>(
        &mut self,
        context: &mut Context,
        telescope: &Telescope<B>,
        payload: &mut dyn FnMut(&mut Self, &mut Context, &B) -> B,
    ) -> Telescope<B> {
        match telescope {
            Telescope::Done(body) => Telescope::Done(Box::new(payload(self, context, body))),
            Telescope::Cons(mark, type_, rest) => {
                let mark = *mark;
                let walked = self.term(context, type_);
                let opened = self.released(type_);
                let rest = context.with_frame(|context| {
                    let binder = context.fresh(rest.first_hint());
                    context.enter(&binder, &opened, mark);
                    self.entered.push(Term::free_var(&binder));
                    let witness = mark == Plicity::Witness;
                    if witness {
                        self.layers.push(HashMap::new());
                    }
                    let rest = rest.map_body(|inner| self.telescope(context, inner, payload));
                    if witness {
                        self.layers.pop();
                    }
                    self.entered.pop();
                    rest
                });

                Telescope::Cons(mark, walked, rest)
            }
        }
    }

    /// A `let`: each binding walked under the bindings before it and entered at its definition, and the tail walked under all of them.
    fn let_(&mut self, context: &mut Context, term: &Term, Let { bindings, tail }: &Let) -> Term {
        let before = self.entered.len();

        context.with_frame(|context| {
            let mut binders: Vec<Term> = Vec::with_capacity(bindings.len());
            let mut walked = Vec::with_capacity(bindings.len());
            for (index, binding) in bindings.iter().enumerate() {
                self.entered.extend(binders.iter().rev().cloned());
                let type_ = self.term(context, binding.type_());
                let value = self.term(context, binding.value());
                let opened_type = self.released(binding.type_());
                let opened_value = self.released(binding.value());
                self.entered.truncate(before);

                let binder = context.fresh(tail.hint(index));
                context.define_assuming(&binder, &opened_type, &opened_value, None);
                binders.push(Term::free_var(&binder));
                walked.push(LetBinding::new(type_, value));
            }

            self.entered.extend(binders.iter().rev().cloned());
            let tail = tail.map_body(|body| self.term(context, body));
            self.entered.truncate(before);

            rebuilt(
                term,
                Subterm::Let(Let {
                    bindings: walked,
                    tail,
                }),
            )
        })
    }

    /// A recursive group: each member's type and body, and the tail, walked under the members, each standing for itself. A member names its group, which here still stands under the binders entered, so the members a question is put over are the group's as it stands there.
    fn rec(&mut self, context: &mut Context, term: &Term, Rec { group, tail }: &Rec) -> Term {
        let members = match &*self.released(term) {
            Subterm::Rec(Rec { group, .. }) => group.members(),
            _ => unreachable!("a recursive group released is one"),
        };
        let before = self.entered.len();
        self.entered.extend(members.iter().rev().cloned());
        let group = group.map_members(|member| self.term(context, member));
        let tail = tail.map_body(|body| self.term(context, body));
        self.entered.truncate(before);

        rebuilt(term, Subterm::Rec(Rec { group, tail }))
    }

    /// A match: its scrutinee and result walked, and each arm walked in a frame of its own under the equations elaboration typed it under.
    fn match_(
        &mut self,
        context: &mut Context,
        term: &Term,
        Match {
            head,
            result,
            cases,
        }: &Match,
    ) -> Term {
        let walked_head = self.term(context, head);
        let result = match result {
            // A motive's binders are the scrutinee's indices and the scrutinee, crossed at no type.
            MatchResult::Family(motive) => MatchResult::Family(self.crossed(context, motive)),
            MatchResult::Ambient(goal) => MatchResult::Ambient(self.term(context, goal)),
        };
        let opened = self.released(head);

        let cases = match cases {
            Cases::Bool {
                false_case,
                true_case,
            } => Cases::Bool {
                false_case: self.case(
                    context,
                    &opened,
                    &Term::intrinsic(Intrinsic::Bool(false)),
                    false_case,
                ),
                true_case: self.case(
                    context,
                    &opened,
                    &Term::intrinsic(Intrinsic::Bool(true)),
                    true_case,
                ),
            },
            Cases::Switch { cases, default } => Cases::Switch {
                cases: cases
                    .iter()
                    .map(|(value, body)| {
                        let literal = Term::intrinsic(Intrinsic::Nat(Nat::new(value.clone())));
                        (value.clone(), self.case(context, &opened, &literal, body))
                    })
                    .collect(),
                default: self.term(context, default),
            },
            Cases::Induct {
                cases,
                default,
                stated,
            } => Cases::Induct {
                cases: cases
                    .iter()
                    .map(|(tag, arm)| (tag.clone(), self.induct_arm(context, &opened, tag, arm)))
                    .collect(),
                default: default.as_ref().map(|default| self.term(context, default)),
                stated: stated.as_ref().map(|stated| self.term(context, stated)),
            },
            Cases::FreeMonoid { carrier } => Cases::FreeMonoid {
                carrier: self.carrier(context, &opened, result_motive(&result), carrier),
            },
        };

        rebuilt(
            term,
            Subterm::Match(Match {
                head: walked_head,
                result,
                cases,
            }),
        )
    }

    /// An arm that binds nothing, walked in a frame where `head` is `value`.
    fn case(&mut self, context: &mut Context, head: &Term, value: &Term, body: &Term) -> Term {
        context.with_frame(|context| match enter_case(context, head, value) {
            true => {
                self.layers.push(HashMap::new());
                let written = self.term(context, body);
                self.layers.pop();
                written
            }
            false => self.unrefined_term(context, body),
        })
    }

    /// `body`, under an arm whose equations were not installed.
    fn unrefined_term(&mut self, context: &mut Context, body: &Term) -> Term {
        self.unrefined(|writing| writing.term(context, body))
    }

    /// `walk` run under an arm whose equations were not installed, in a witness scope of its own: what is written there is not what is written outside it.
    fn unrefined<R>(&mut self, walk: impl FnOnce(&mut Self) -> R) -> R {
        self.unrefined += 1;
        self.layers.push(HashMap::new());
        let written = walk(self);
        self.layers.pop();
        self.unrefined -= 1;

        written
    }

    /// One constructor arm: its payload entered at the constructor's signature, the scrutinee refined to the case and the indices to their solution ([`enter_induct_case`]), and its body walked under them. Where the scrutinee's family cannot be read, the arm is crossed with its equations left out.
    fn induct_arm(
        &mut self,
        context: &mut Context,
        head: &Term,
        tag: &Atom,
        arm: &InductArm,
    ) -> InductArm {
        let plicities = arm.plicities().to_vec();
        let before = self.entered.len();

        let body = context.with_frame(|context| {
            let labels = arm
                .body
                .hint_iter()
                .map(|hint| context.fresh(hint))
                .collect::<Vec<_>>();
            let vars = labels.iter().map(Term::free_var).collect::<Vec<_>>();
            let refined = enter_case_of(context, head, tag, &labels, &vars);
            self.entered.extend(vars.iter().rev().cloned());
            let body = match refined {
                true => {
                    self.layers.push(HashMap::new());
                    let body = arm.body.map_body(|body| self.term(context, body));
                    self.layers.pop();
                    body
                }
                false => arm.body.map_body(|body| self.unrefined_term(context, body)),
            };
            self.entered.truncate(before);
            body
        });

        InductArm::new(body, plicities)
    }

    /// A free-monoid elimination's two arms. A case split's are refined to their case, as elaboration typed them; a fold's learn nothing of the scrutinee, and its hypothesis is entered at the motive where there is one.
    fn carrier(
        &mut self,
        context: &mut Context,
        head: &Term,
        motive: Option<&Scope<Many>>,
        carrier: &Carrier,
    ) -> Carrier {
        match carrier {
            Carrier::Nat {
                empty_case,
                cons_case,
            } => {
                let folds = cons_case.uses(1);
                let zero = Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)));
                let empty_case = match folds {
                    true => self.term(context, empty_case),
                    false => self.case(context, head, &zero, empty_case),
                };
                let cons_case = context.with_frame(|context| {
                    let predecessor = context.fresh(cons_case.first_hint());
                    let hypothesis = context.fresh(cons_case.second_hint());
                    let predecessor_var = Term::free_var(&predecessor);
                    context.assume(&predecessor, &Term::intrinsic(Intrinsic::NatType));
                    if folds && let Some(motive) = motive {
                        let at = self.released(&motive.open(&[&predecessor_var]));
                        context.assume(&hypothesis, &at);
                    }
                    let successor = Term::intrinsic(Intrinsic::nat_add(
                        predecessor_var.clone(),
                        Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                    ));
                    self.free_monoid_case(
                        context,
                        head,
                        (!folds).then_some(&successor),
                        &[predecessor_var, Term::free_var(&hypothesis)],
                        cons_case,
                    )
                });

                Carrier::Nat {
                    empty_case,
                    cons_case,
                }
            }
            Carrier::List {
                elem,
                empty_case,
                cons_case,
            } => {
                let element = self.released(elem);
                let sequence = Term::intrinsic(Intrinsic::ListType(element.clone()));
                let empty = Term::intrinsic(Intrinsic::List {
                    element: element.clone(),
                    items: vec![],
                });
                let (empty_case, cons_case) = self.sequence(
                    context,
                    head,
                    motive,
                    (&element, &sequence, &empty),
                    |atom, tail| {
                        Term::intrinsic(Intrinsic::ListConcat {
                            element: element.clone(),
                            operands: vec![
                                Term::intrinsic(Intrinsic::List {
                                    element: element.clone(),
                                    items: vec![atom.clone()],
                                }),
                                tail.clone(),
                            ],
                        })
                    },
                    (empty_case, cons_case),
                );

                Carrier::List {
                    elem: self.term(context, elem),
                    empty_case,
                    cons_case,
                }
            }
            Carrier::Bytes {
                empty_case,
                cons_case,
            } => {
                let atom = Term::intrinsic(Intrinsic::ByteType);
                let sequence = Term::intrinsic(Intrinsic::BytesType);
                let empty = Term::intrinsic(Intrinsic::Bytes(Binary::empty()));
                let (empty_case, cons_case) = self.sequence(
                    context,
                    head,
                    motive,
                    (&atom, &sequence, &empty),
                    |head, tail| {
                        Term::intrinsic(Intrinsic::BytesConcat {
                            operands: vec![
                                Term::intrinsic(Intrinsic::BytesAppend {
                                    bytes: Term::intrinsic(Intrinsic::Bytes(Binary::empty())),
                                    element: head.clone(),
                                }),
                                tail.clone(),
                            ],
                        })
                    },
                    (empty_case, cons_case),
                );

                Carrier::Bytes {
                    empty_case,
                    cons_case,
                }
            }
        }
    }

    /// A sequence's two arms, its cons arm binding the atom, the tail and the hypothesis: see [`Writing::carrier`].
    fn sequence(
        &mut self,
        context: &mut Context,
        head: &Term,
        motive: Option<&Scope<Many>>,
        (atom_type, sequence, empty): (&Term, &Term, &Term),
        cons: impl FnOnce(&Term, &Term) -> Term,
        (empty_case, cons_case): (&Term, &Scope<Three>),
    ) -> (Term, Scope<Three>) {
        let folds = cons_case.uses(2);
        let empty_case = match folds {
            true => self.term(context, empty_case),
            false => self.case(context, head, empty, empty_case),
        };
        let cons_case = context.with_frame(|context| {
            let atom = context.fresh(cons_case.first_hint());
            let tail = context.fresh(cons_case.second_hint());
            let hypothesis = context.fresh(cons_case.third_hint());
            let (atom_var, tail_var) = (Term::free_var(&atom), Term::free_var(&tail));
            context.assume(&atom, atom_type);
            context.assume(&tail, sequence);
            if folds && let Some(motive) = motive {
                let at = self.released(&motive.open(&[&tail_var]));
                context.assume(&hypothesis, &at);
            }
            let value = cons(&atom_var, &tail_var);
            self.free_monoid_case(
                context,
                head,
                (!folds).then_some(&value),
                &[atom_var, tail_var, Term::free_var(&hypothesis)],
                cons_case,
            )
        });

        (empty_case, cons_case)
    }

    /// A free-monoid cons arm, its binders already entered at `binders`, refined to `value` where it is a case split.
    fn free_monoid_case<A: Arity>(
        &mut self,
        context: &mut Context,
        head: &Term,
        value: Option<&Term>,
        binders: &[Term],
        arm: &Scope<A>,
    ) -> Scope<A> {
        let before = self.entered.len();
        self.entered.extend(binders.iter().rev().cloned());
        let refined = value.is_none_or(|value| enter_case(context, head, value));
        let arm = match refined {
            true => {
                self.layers.push(HashMap::new());
                let arm = arm.map_body(|body| self.term(context, body));
                self.layers.pop();
                arm
            }
            false => arm.map_body(|body| self.unrefined_term(context, body)),
        };
        self.entered.truncate(before);

        arm
    }

    /// A scope crossed with a variable of no type for each of its binders: a question about one is unanswered, and nothing it stands under is an equation.
    fn crossed<A: Arity>(&mut self, context: &mut Context, scope: &Scope<A>) -> Scope<A> {
        let before = self.entered.len();
        for _ in 0..scope.arity() {
            let binder = context.fresh(None);
            self.entered.push(Term::free_var(&binder));
        }
        let scope = scope.map_body(|body| self.term(context, body));
        self.entered.truncate(before);

        scope
    }

    /// Any other node: each child term walked where it stands, under a variable of no type for every binder between the node and it, and the node rebuilt over what came back where a child changed.
    fn children(&mut self, context: &mut Context, term: &Term) -> Term {
        let found = Rc::new(RefCell::new(Vec::new()));
        let sink = Rc::clone(&found);
        let mut collect = Visit::rewriting(
            |_, _| None,
            Box::new(move |depth, child| {
                sink.borrow_mut().push((depth, child.clone()));
                Some(child.clone())
            }),
        );
        let _: Subterm = (**term).traverse(&mut collect);
        let children = found.take();

        let before = self.entered.len();
        let mut walked = VecDeque::with_capacity(children.len());
        let mut changed = false;
        for (depth, child) in &children {
            for _ in 0..*depth {
                let binder = context.fresh(None);
                self.entered.push(Term::free_var(&binder));
            }
            let written = self.term(context, child);
            self.entered.truncate(before);

            changed |= written.identity() != child.identity();
            walked.push_back(written);
        }
        if !changed {
            return term.clone();
        }

        let walked = Rc::new(RefCell::new(walked));
        let mut rebuild = Visit::rewriting(
            |_, _| None,
            Box::new(move |_, _| walked.borrow_mut().pop_front()),
        );

        rebuilt(term, (**term).traverse(&mut rebuild))
    }

    /// `value` as it stands under the binders entered.
    fn released<B: Bound>(&self, value: &B) -> B {
        let terms = self.entered.iter().rev().collect::<Vec<_>>();

        value.release(&terms)
    }

    /// `opened`, a term as it stands under the binders entered, with each binder entered as a variable bound again: what stands where the walk is.
    fn closed_over(&self, opened: &Term) -> Term {
        let unused = Free::local(u64::MAX, None);
        let labels = self
            .entered
            .iter()
            .rev()
            .map(|entered| match &**entered {
                Subterm::Var(var) => var.as_free().unwrap_or(&unused),
                _ => &unused,
            })
            .collect::<Vec<_>>();

        opened.capture(&labels)
    }
}

/// Install the equation an arm stands under, `head` being `value` there, as elaboration installs it: whether it was installed.
fn enter_case(context: &mut Context, head: &Term, value: &Term) -> bool {
    context.open_arm(head, value);

    refine_head(context, head, value).is_ok()
}

/// Enter the arm of `tag` of a match on `head` as elaboration typed it ([`enter_induct_case`]), the family read off the scrutinee's type: whether it was entered, which it is not where that type is no inductive the walk can read.
fn enter_case_of(
    context: &mut Context,
    head: &Term,
    tag: &Atom,
    labels: &[Free],
    vars: &[Term],
) -> bool {
    let Some(type_) = synth_neutral(context, &Binders::default(), &[], head)
        .ok()
        .flatten()
    else {
        return false;
    };
    let Ok(reduced) = reduce_with(context, &type_) else {
        return false;
    };
    let Subterm::InductType(InductType {
        name,
        universes,
        params,
        indices,
    }) = &*reduced
    else {
        return false;
    };
    let Some(Ok(Some(constructor))) =
        context.induct_facet(name, universes, |at| at.constructor(tag).transpose())
    else {
        return false;
    };
    let telescope = constructor.telescope.open_params(params);
    let family = InductCase {
        name,
        universes,
        params,
        actual_indices: indices,
    };

    enter_induct_case(context, head, family, tag, (labels, vars), telescope).is_ok()
}

/// The motive a free-monoid fold's hypothesis is typed at, where its result is a family.
fn result_motive(result: &MatchResult) -> Option<&Scope<Many>> {
    match result {
        MatchResult::Family(motive) => Some(motive),
        MatchResult::Ambient(_) => None,
    }
}

/// The type of a dictionary: a literal's structure at its parameters, and a neutral's as synthesized. `None` for any other term.
fn dictionary_type(context: &mut Context, dictionary: &Term) -> Option<Term> {
    match &**dictionary {
        Subterm::Struct(Struct {
            name,
            universes,
            params,
            ..
        }) => Some(Term::from(Subterm::StructType(StructType {
            name: *name,
            universes: universes.clone(),
            params: params.clone(),
        }))),
        _ => synth_neutral(context, &Binders::default(), &[], dictionary)
            .ok()
            .flatten(),
    }
}

/// Who a call headed by `head` is a call of, as an open slot's provenance names it.
fn callee_of(head: &Term) -> CalleeId {
    match head.head_name() {
        Some(name) => CalleeId::Function(*name),
        None => CalleeId::Anonymous,
    }
}

/// `head` applied to `arguments`.
fn applied(head: Term, arguments: Vec<Argument>) -> Term {
    Term::from(Subterm::Apply(Apply { head, arguments }))
}

/// `inner` at the place `term` stood.
fn rebuilt(term: &Term, inner: Subterm) -> Term {
    match term.extent() {
        Some(extent) => Term::placed(extent, inner),
        None => Term::from(inner),
    }
}
