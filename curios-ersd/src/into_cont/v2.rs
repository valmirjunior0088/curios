//! Direct Ersd to arena-backed high-CPS lowering.

use {
    super::{Error, LowerResult},
    crate::{
        CellPrim, Func as ErsdFunction, HostPrim, Item, Match, Module as ErsdModule, NatMatch,
        Prim, PurePrim, Subterm, Term,
    },
    curios_base::{Grain, PackedBin},
    curios_cont::{
        CpsAtom, CpsCallee, CpsCellOp, CpsContId, CpsContinuation, CpsEdge, CpsFunId, CpsFunction,
        CpsIntrinsicOp, CpsLiteral, CpsModule, CpsNode, CpsNodeId, CpsPrimOp, CpsValueExpr,
        CpsValueId,
    },
    num_bigint::BigUint,
    std::{
        collections::{BTreeMap, BTreeSet},
        rc::Rc,
        sync::Arc,
    },
};

fn rec_computed_order(names: &[&str], dependencies: &[Vec<usize>]) -> LowerResult<Vec<usize>> {
    fn visit(
        node: usize,
        names: &[&str],
        dependencies: &[Vec<usize>],
        marks: &mut [u8],
        stack: &mut Vec<usize>,
        order: &mut Vec<usize>,
    ) -> LowerResult<()> {
        marks[node] = 1;
        stack.push(node);
        for &dependency in &dependencies[node] {
            match marks[dependency] {
                1 => {
                    let start = stack.iter().position(|&item| item == dependency).unwrap();
                    let cycle = stack[start..]
                        .iter()
                        .chain([&dependency])
                        .map(|&item| names[item].to_string())
                        .collect();
                    return Err(Error::CyclicRecComputed { cycle });
                }
                0 => visit(dependency, names, dependencies, marks, stack, order)?,
                _ => {}
            }
        }
        stack.pop();
        marks[node] = 2;
        order.push(node);
        Ok(())
    }

    let mut marks = vec![0; names.len()];
    let mut stack = Vec::new();
    let mut order = Vec::new();
    for node in 0..names.len() {
        if marks[node] == 0 {
            visit(
                node,
                names,
                dependencies,
                &mut marks,
                &mut stack,
                &mut order,
            )?;
        }
    }
    Ok(order)
}

#[derive(Clone, Default)]
struct Env(Rc<BTreeMap<String, CpsAtom>>);

impl Env {
    fn get(&self, name: &str) -> CpsAtom {
        self.0
            .get(name)
            .unwrap_or_else(|| panic!("high-CPS lowering lacks value `{name}`"))
            .clone()
    }

    fn insert(&mut self, name: String, atom: CpsAtom) {
        Rc::make_mut(&mut self.0).insert(name, atom);
    }
}

pub(super) fn lower(source: &ErsdModule) -> LowerResult<CpsModule> {
    let mut lowerer = Lowerer {
        module: CpsModule::new(),
    };
    let main = lowerer.module.reserve_function(Some("main".into()));
    let return_cont = lowerer.module.reserve_continuation();
    let body = lowerer.lower_items(&source.items, &source.body, Env::default(), return_cont)?;
    lowerer.module.define_function(
        main,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
        },
    );
    lowerer.module.set_entry(main);
    lowerer
        .module
        .verify()
        .map_err(|error| panic!("Ersd produced invalid high CPS: {error}"))
        .unwrap();
    Ok(lowerer.module)
}

struct Lowerer {
    module: CpsModule,
}

impl Lowerer {
    fn jump(&mut self, target: CpsContId, args: Vec<CpsAtom>) -> CpsNodeId {
        self.module
            .add_node(CpsNode::ApplyCont(CpsEdge { target, args }))
    }

    /// Sequence operands left-to-right without heap-allocated Rust callbacks.
    /// Each operand gets a local continuation parameter allocated up front; the
    /// chain is then assembled from right to left.
    fn lower_sequence(
        &mut self,
        terms: Vec<&Term>,
        env: &Env,
        finish: impl FnOnce(&mut Self, Vec<CpsAtom>) -> LowerResult<CpsNodeId>,
    ) -> LowerResult<CpsNodeId> {
        let mut dynamic = Vec::new();
        let mut atoms = Vec::with_capacity(terms.len());
        for (index, term) in terms.into_iter().enumerate() {
            if let Some(atom) = Self::immediate_atom(term, env) {
                atoms.push(atom);
            } else {
                let value = self
                    .module
                    .add_value(Some(format!("operand{index}")), false);
                atoms.push(CpsAtom::Value(value));
                dynamic.push((term, value));
            }
        }
        let mut current = finish(self, atoms)?;

        for (term, value) in dynamic.into_iter().rev() {
            let continuation = self.module.reserve_continuation();
            self.module.define_continuation(
                continuation,
                CpsContinuation {
                    debug_name: None,
                    params: vec![value],
                    body: current,
                },
            );
            let entry = self.lower_term(term, env, continuation)?;
            current = self.module.add_node(CpsNode::LetCont {
                continuations: vec![continuation],
                body: entry,
            });
        }
        Ok(current)
    }

    /// Terms that are already CPS atoms do not need a resumption merely to be
    /// used as an operand. Keeping them atomic preserves known function identity
    /// for call analysis and avoids administrative continuation chains.
    fn immediate_atom(term: &Term, env: &Env) -> Option<CpsAtom> {
        match &**term {
            Subterm::Name(name) => Some(env.get(name.as_str())),
            Subterm::Erased => Some(CpsAtom::Literal(CpsLiteral::Nat(0))),
            Subterm::Atom(atom) => Some(CpsAtom::Literal(CpsLiteral::Nat(atom.index as u32))),
            Subterm::Prim(Prim::Pure(PurePrim::Nat(value))) => {
                Some(CpsAtom::Literal(CpsLiteral::Nat(*value)))
            }
            Subterm::Prim(Prim::Pure(PurePrim::Int(value))) => {
                Some(CpsAtom::Literal(CpsLiteral::Int(*value)))
            }
            Subterm::Prim(Prim::Pure(PurePrim::Flt(value))) => {
                Some(CpsAtom::Literal(CpsLiteral::Flt(*value)))
            }
            Subterm::Prim(Prim::Pure(PurePrim::Bin(grain, value))) => {
                Some(CpsAtom::Literal(CpsLiteral::Bin(*grain, value.clone())))
            }
            Subterm::Prim(Prim::Pure(PurePrim::Io(token))) => {
                Some(CpsAtom::Literal(CpsLiteral::Bin(
                    Grain::X,
                    PackedBin::from_bytes(BigUint::from(*token).to_bytes_le()),
                )))
            }
            _ => None,
        }
    }

    fn lower_term(&mut self, term: &Term, env: &Env, target: CpsContId) -> LowerResult<CpsNodeId> {
        let mut term = term;
        let mut env = env.clone();
        let mut functions = Vec::new();

        while let Subterm::Let(let_) = &**term {
            let mut index = 0;
            while index < let_.bindings.len() {
                let (name, body) = &let_.bindings[index];
                let Some((atom, mut introduced)) =
                    self.static_atom(body, &env, Some(name.clone()))?
                else {
                    let entry =
                        self.lower_let(&let_.bindings[index..], &let_.tail, &env, target)?;
                    return Ok(self.wrap_functions(functions, entry));
                };
                env.insert(name.clone(), atom);
                functions.append(&mut introduced);
                index += 1;
            }
            term = &let_.tail;
        }

        let body = self.lower_term_unpeeled(term, &env, target)?;
        Ok(self.wrap_functions(functions, body))
    }

    fn lower_term_unpeeled(
        &mut self,
        term: &Term,
        env: &Env,
        target: CpsContId,
    ) -> LowerResult<CpsNodeId> {
        match &**term {
            Subterm::Name(name) => Ok(self.jump(target, vec![env.get(name.as_str())])),
            Subterm::Erased => Ok(self.jump(target, vec![CpsAtom::Literal(CpsLiteral::Nat(0))])),
            Subterm::Unreachable => Ok(self.module.add_node(CpsNode::Unreachable)),
            Subterm::Atom(atom) => Ok(self.jump(
                target,
                vec![CpsAtom::Literal(CpsLiteral::Nat(atom.index as u32))],
            )),
            Subterm::Func(function) => {
                let function = self.lower_function(function, env, None)?;
                let body = self.jump(target, vec![CpsAtom::Fun(function)]);
                Ok(self.module.add_node(CpsNode::LetFun {
                    functions: vec![function],
                    body,
                }))
            }
            Subterm::Apply(apply) => {
                let mut terms = Vec::with_capacity(apply.params.len() + 1);
                terms.push(&apply.head);
                terms.extend(apply.params.iter());
                self.lower_sequence(terms, env, move |lowerer, mut atoms| {
                    let head = atoms.remove(0);
                    let callee = match head {
                        CpsAtom::Fun(function) => CpsCallee::Known(function),
                        CpsAtom::Value(value) => CpsCallee::Closure(value),
                        CpsAtom::Literal(_) => panic!("literal reached Ersd application head"),
                    };
                    Ok(lowerer.module.add_node(CpsNode::ApplyFun {
                        callee,
                        args: atoms,
                        return_to: target,
                    }))
                })
            }
            Subterm::Tuple(tuple) => {
                let terms = tuple.fields.iter().collect();
                self.lower_sequence(terms, env, move |lowerer, fields| {
                    let result = lowerer.module.add_value(None, false);
                    let next = lowerer.jump(target, vec![CpsAtom::Value(result)]);
                    Ok(lowerer.module.add_node(CpsNode::LetValue {
                        result,
                        value: CpsValueExpr::Tuple(fields),
                        next,
                    }))
                })
            }
            Subterm::Proj(projection) => {
                self.lower_sequence(vec![&projection.head], env, move |lowerer, args| {
                    lowerer.finish_prim(CpsPrimOp::TplGet(projection.index), args, target)
                })
            }
            Subterm::Prim(primitive) => self.lower_primitive(primitive, env, target),
            Subterm::Match(match_) => self.lower_match(match_, env, target),
            Subterm::NatMatch(match_) => self.lower_nat_match(match_, env, target),
            Subterm::Let(let_) => self.lower_let(&let_.bindings, &let_.tail, env, target),
            Subterm::Rec(rec) => self.lower_rec(
                &rec.names,
                rec.items.iter().collect(),
                &[],
                &rec.tail,
                env,
                |lowerer, env| lowerer.lower_term(&rec.tail, env, target),
            ),
        }
    }

    fn finish_prim(
        &mut self,
        op: CpsPrimOp,
        args: Vec<CpsAtom>,
        target: CpsContId,
    ) -> LowerResult<CpsNodeId> {
        let result = self.module.add_value(None, false);
        let next = self.jump(target, vec![CpsAtom::Value(result)]);
        Ok(self.module.add_node(CpsNode::LetPrim {
            result,
            op,
            args,
            next,
        }))
    }

    fn lower_function(
        &mut self,
        function: &ErsdFunction,
        env: &Env,
        reserved: Option<(CpsFunId, String)>,
    ) -> LowerResult<CpsFunId> {
        let (id, debug_name) = reserved.unwrap_or_else(|| {
            let name = "lambda".to_owned();
            (self.module.reserve_function(Some(name.clone())), name)
        });
        let return_cont = self.module.reserve_continuation();
        let mut body_env = Env::default();
        for capture in &function.captures {
            body_env.insert(capture.name.clone(), env.get(&capture.name));
        }
        let params = function
            .params
            .iter()
            .map(|param| {
                let value = self
                    .module
                    .add_value(Some(param.name.clone()), param.candidate);
                body_env.insert(param.name.clone(), CpsAtom::Value(value));
                value
            })
            .collect::<Vec<_>>();
        let body = self.lower_term(&function.body, &body_env, return_cont)?;
        self.module.define_function(
            id,
            CpsFunction {
                debug_name: Some(debug_name),
                params,
                return_cont,
                body,
            },
        );
        Ok(id)
    }

    fn static_atom(
        &mut self,
        term: &Term,
        env: &Env,
        debug_name: Option<String>,
    ) -> LowerResult<Option<(CpsAtom, Vec<CpsFunId>)>> {
        Ok(match &**term {
            Subterm::Name(name) => Some((env.get(name.as_str()), vec![])),
            Subterm::Erased => Some((CpsAtom::Literal(CpsLiteral::Nat(0)), vec![])),
            Subterm::Atom(atom) => {
                Some((CpsAtom::Literal(CpsLiteral::Nat(atom.index as u32)), vec![]))
            }
            Subterm::Func(function) => {
                let name = debug_name.unwrap_or_else(|| "lambda".into());
                let id = self.module.reserve_function(Some(name.clone()));
                self.lower_function(function, env, Some((id, name)))?;
                Some((CpsAtom::Fun(id), vec![id]))
            }
            Subterm::Prim(Prim::Pure(PurePrim::Nat(value))) => {
                Some((CpsAtom::Literal(CpsLiteral::Nat(*value)), vec![]))
            }
            Subterm::Prim(Prim::Pure(PurePrim::Int(value))) => {
                Some((CpsAtom::Literal(CpsLiteral::Int(*value)), vec![]))
            }
            Subterm::Prim(Prim::Pure(PurePrim::Flt(value))) => {
                Some((CpsAtom::Literal(CpsLiteral::Flt(*value)), vec![]))
            }
            Subterm::Prim(Prim::Pure(PurePrim::Bin(grain, value))) => Some((
                CpsAtom::Literal(CpsLiteral::Bin(*grain, value.clone())),
                vec![],
            )),
            Subterm::Prim(Prim::Pure(PurePrim::Io(token))) => Some((
                CpsAtom::Literal(CpsLiteral::Bin(
                    Grain::X,
                    PackedBin::from_bytes(BigUint::from(*token).to_bytes_le()),
                )),
                vec![],
            )),
            _ => None,
        })
    }

    fn wrap_functions(&mut self, functions: Vec<CpsFunId>, body: CpsNodeId) -> CpsNodeId {
        if functions.is_empty() {
            body
        } else {
            self.module.add_node(CpsNode::LetFun { functions, body })
        }
    }

    fn lower_let(
        &mut self,
        bindings: &[(String, Term)],
        tail: &Term,
        env: &Env,
        target: CpsContId,
    ) -> LowerResult<CpsNodeId> {
        let mut env = env.clone();
        let mut functions = Vec::new();
        let mut index = 0;
        while index < bindings.len() {
            let (name, body) = &bindings[index];
            if let Some((atom, mut introduced)) =
                self.static_atom(body, &env, Some(name.clone()))?
            {
                env.insert(name.clone(), atom);
                functions.append(&mut introduced);
                index += 1;
                continue;
            }

            let value = self.module.add_value(Some(name.clone()), false);
            let mut next_env = env.clone();
            next_env.insert(name.clone(), CpsAtom::Value(value));
            let continuation_body =
                self.lower_let(&bindings[index + 1..], tail, &next_env, target)?;
            let continuation = self.module.add_continuation(CpsContinuation {
                debug_name: Some(format!("let/{name}")),
                params: vec![value],
                body: continuation_body,
            });
            let entry = self.lower_term(body, &env, continuation)?;
            let entry = self.module.add_node(CpsNode::LetCont {
                continuations: vec![continuation],
                body: entry,
            });
            return Ok(self.wrap_functions(functions, entry));
        }

        let body = self.lower_term(tail, &env, target)?;
        Ok(self.wrap_functions(functions, body))
    }

    fn lower_items(
        &mut self,
        items: &[Item],
        tail: &Term,
        mut env: Env,
        target: CpsContId,
    ) -> LowerResult<CpsNodeId> {
        let mut functions = Vec::new();
        let mut index = 0;
        while index < items.len() {
            match &items[index] {
                Item::Let { name, body } => {
                    if let Some((atom, mut introduced)) =
                        self.static_atom(body, &env, Some(name.clone()))?
                    {
                        env.insert(name.clone(), atom);
                        functions.append(&mut introduced);
                        index += 1;
                        continue;
                    }

                    let value = self.module.add_value(Some(name.clone()), false);
                    let mut next_env = env.clone();
                    next_env.insert(name.clone(), CpsAtom::Value(value));
                    let continuation_body =
                        self.lower_items(&items[index + 1..], tail, next_env, target)?;
                    let continuation = self.module.add_continuation(CpsContinuation {
                        debug_name: Some(format!("item/{name}")),
                        params: vec![value],
                        body: continuation_body,
                    });
                    let entry = self.lower_term(body, &env, continuation)?;
                    let entry = self.module.add_node(CpsNode::LetCont {
                        continuations: vec![continuation],
                        body: entry,
                    });
                    return Ok(self.wrap_functions(functions, entry));
                }
                Item::Rec {
                    names,
                    items: definitions,
                } if definitions
                    .iter()
                    .all(|term| matches!(&**term, Subterm::Func(_))) =>
                {
                    let ids = names
                        .iter()
                        .map(|name| self.module.reserve_function(Some(name.clone())))
                        .collect::<Vec<_>>();
                    for (name, id) in names.iter().zip(ids.iter().copied()) {
                        env.insert(name.clone(), CpsAtom::Fun(id));
                    }
                    for ((name, definition), id) in
                        names.iter().zip(definitions).zip(ids.iter().copied())
                    {
                        let Subterm::Func(function) = &**definition else {
                            unreachable!()
                        };
                        self.lower_function(function, &env, Some((id, name.clone())))?;
                    }
                    functions.extend(ids);
                    index += 1;
                }
                Item::Rec {
                    names,
                    items: definitions,
                } => {
                    let rest = &items[index + 1..];
                    let body = self.lower_rec(
                        names,
                        definitions.iter().collect(),
                        rest,
                        tail,
                        &env,
                        |lowerer, env| lowerer.lower_items(rest, tail, env.clone(), target),
                    )?;
                    return Ok(self.wrap_functions(functions, body));
                }
            }
        }
        let body = self.lower_term(tail, &env, target)?;
        Ok(self.wrap_functions(functions, body))
    }

    fn lower_rec(
        &mut self,
        names: &[String],
        items: Vec<&Term>,
        rest: &[Item],
        tail: &Term,
        env: &Env,
        finish: impl FnOnce(&mut Self, &Env) -> LowerResult<CpsNodeId>,
    ) -> LowerResult<CpsNodeId> {
        assert_eq!(names.len(), items.len());
        let mut required = tail.free_names();
        required.extend(rest.iter().flat_map(Item::free_names));
        let positions = names
            .iter()
            .enumerate()
            .map(|(index, name)| (name.as_str(), index))
            .collect::<BTreeMap<_, _>>();
        let mut live = BTreeSet::new();
        let mut work = required
            .iter()
            .filter_map(|name| positions.get(name.as_str()).copied())
            .collect::<Vec<_>>();
        while let Some(index) = work.pop() {
            if live.insert(index) {
                work.extend(
                    items[index]
                        .free_names()
                        .iter()
                        .filter_map(|name| positions.get(name.as_str()).copied()),
                );
            }
        }
        if live.is_empty() {
            return finish(self, env);
        }

        let mut inner = env.clone();
        let mut functions = Vec::<(usize, CpsFunId, &ErsdFunction)>::new();
        let mut computed = Vec::<(usize, CpsValueId, &Term)>::new();

        for (index, (name, item)) in names.iter().zip(&items).enumerate() {
            if !live.contains(&index) {
                continue;
            }
            match &***item {
                Subterm::Func(function) => {
                    let id = self.module.reserve_function(Some(name.clone()));
                    inner.insert(name.clone(), CpsAtom::Fun(id));
                    functions.push((index, id, function));
                }
                _ => {
                    let value = self.module.add_value(Some(name.clone()), false);
                    inner.insert(name.clone(), CpsAtom::Value(value));
                    computed.push((index, value, item));
                }
            }
        }

        for &(index, id, function) in &functions {
            self.lower_function(function, &inner, Some((id, names[index].clone())))?;
        }

        let computed_names = computed
            .iter()
            .map(|(index, _, _)| names[*index].as_str())
            .collect::<Vec<_>>();
        let positions = computed_names
            .iter()
            .enumerate()
            .map(|(index, name)| (*name, index))
            .collect::<BTreeMap<_, _>>();
        let dependencies = computed
            .iter()
            .map(|(_, _, term)| {
                term.free_names()
                    .iter()
                    .filter_map(|name| positions.get(name.as_str()).copied())
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        let order = rec_computed_order(&computed_names, &dependencies)?;

        let function_names = functions
            .iter()
            .map(|(index, _, _)| names[*index].as_str())
            .collect::<BTreeSet<_>>();
        let computed_name_set = computed_names.iter().copied().collect::<BTreeSet<_>>();
        let function_depends_on_computed = functions.iter().any(|(_, _, function)| {
            function
                .captures
                .iter()
                .any(|capture| computed_name_set.contains(capture.name.as_str()))
        });
        let computed_depends_on_function = computed.iter().any(|(_, _, term)| {
            term.free_names()
                .iter()
                .any(|name| function_names.contains(name.as_str()))
        });

        let function_ids = functions.iter().map(|(_, id, _)| *id).collect::<Vec<_>>();
        let computed_ids = computed.iter().map(|(_, id, _)| *id).collect::<Vec<_>>();
        let mut current = finish(self, &inner)?;
        let ready = current;

        if function_depends_on_computed && !computed_depends_on_function {
            current = self.wrap_functions(function_ids.clone(), current);
        }

        for position in order.into_iter().rev() {
            let (_, value, term) = computed[position];
            let continuation = self.module.reserve_continuation();
            self.module.define_continuation(
                continuation,
                CpsContinuation {
                    debug_name: Some(format!("rec/{}", computed_names[position])),
                    params: vec![value],
                    body: current,
                },
            );
            let entry = self.lower_term(term, &inner, continuation)?;
            current = self.module.add_node(CpsNode::LetCont {
                continuations: vec![continuation],
                body: entry,
            });
        }

        if function_depends_on_computed && computed_depends_on_function {
            Ok(self.module.add_node(CpsNode::RecInit {
                functions: function_ids,
                values: computed_ids,
                ready,
                body: current,
            }))
        } else if function_depends_on_computed {
            Ok(current)
        } else {
            Ok(self.wrap_functions(function_ids, current))
        }
    }

    fn lower_match(
        &mut self,
        match_: &Match,
        env: &Env,
        target: CpsContId,
    ) -> LowerResult<CpsNodeId> {
        let mut continuations = Vec::new();
        let mut cases = BTreeMap::new();
        for (&tag, branch) in &match_.cases {
            let continuation = self.module.reserve_continuation();
            let body = self.lower_term(branch, env, target)?;
            self.module.define_continuation(
                continuation,
                CpsContinuation {
                    debug_name: Some(format!("case/{tag}")),
                    params: vec![],
                    body,
                },
            );
            continuations.push(continuation);
            cases.insert(
                tag as u32,
                CpsEdge {
                    target: continuation,
                    args: vec![],
                },
            );
        }
        let default = if let Some(branch) = &match_.default {
            let continuation = self.module.reserve_continuation();
            let body = self.lower_term(branch, env, target)?;
            self.module.define_continuation(
                continuation,
                CpsContinuation {
                    debug_name: Some("case/default".into()),
                    params: vec![],
                    body,
                },
            );
            continuations.push(continuation);
            Some(CpsEdge {
                target: continuation,
                args: vec![],
            })
        } else {
            None
        };

        let head_value = self.module.add_value(Some("match/head".into()), false);
        let head_cont = self.module.reserve_continuation();
        let switch = self.module.add_node(CpsNode::Switch {
            scrutinee: CpsAtom::Value(head_value),
            cases,
            default,
        });
        self.module.define_continuation(
            head_cont,
            CpsContinuation {
                debug_name: Some("match/dispatch".into()),
                params: vec![head_value],
                body: switch,
            },
        );
        continuations.push(head_cont);
        let entry = self.lower_term(&match_.head, env, head_cont)?;
        Ok(self.module.add_node(CpsNode::LetCont {
            continuations,
            body: entry,
        }))
    }

    fn lower_nat_match(
        &mut self,
        match_: &NatMatch,
        env: &Env,
        target: CpsContId,
    ) -> LowerResult<CpsNodeId> {
        match match_ {
            NatMatch::Dispatch {
                head,
                cases: branches,
                default,
            } => {
                let mut continuations = Vec::new();
                let mut cases = BTreeMap::new();
                for (&tag, branch) in branches {
                    let continuation = self.module.reserve_continuation();
                    let body = self.lower_term(branch, env, target)?;
                    self.module.define_continuation(
                        continuation,
                        CpsContinuation {
                            debug_name: Some(format!("nat/{tag}")),
                            params: vec![],
                            body,
                        },
                    );
                    continuations.push(continuation);
                    cases.insert(
                        tag,
                        CpsEdge {
                            target: continuation,
                            args: vec![],
                        },
                    );
                }
                let default_cont = self.module.reserve_continuation();
                let default_body = self.lower_term(default, env, target)?;
                self.module.define_continuation(
                    default_cont,
                    CpsContinuation {
                        debug_name: Some("nat/default".into()),
                        params: vec![],
                        body: default_body,
                    },
                );
                continuations.push(default_cont);

                let head_value = self.module.add_value(Some("nat/head".into()), false);
                let head_cont = self.module.reserve_continuation();
                let switch = self.module.add_node(CpsNode::Switch {
                    scrutinee: CpsAtom::Value(head_value),
                    cases,
                    default: Some(CpsEdge {
                        target: default_cont,
                        args: vec![],
                    }),
                });
                self.module.define_continuation(
                    head_cont,
                    CpsContinuation {
                        debug_name: Some("nat/dispatch".into()),
                        params: vec![head_value],
                        body: switch,
                    },
                );
                continuations.push(head_cont);
                let entry = self.lower_term(head, env, head_cont)?;
                Ok(self.module.add_node(CpsNode::LetCont {
                    continuations,
                    body: entry,
                }))
            }
            NatMatch::Induction {
                head,
                zero_case,
                pred,
                ih,
                succ_case,
            } => self.lower_nat_induction(head, zero_case, pred, ih, succ_case, env, target),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_nat_induction(
        &mut self,
        head: &Term,
        zero_case: &Term,
        pred: &str,
        ih: &str,
        succ_case: &Term,
        env: &Env,
        target: CpsContId,
    ) -> LowerResult<CpsNodeId> {
        let head_value = self.module.add_value(Some("ind/head".into()), false);
        let zero_value = self.module.add_value(Some("ind/zero".into()), false);
        let loop_index = self.module.add_value(Some("ind/index".into()), false);
        let loop_acc = self.module.add_value(Some("ind/acc".into()), false);
        let step_index = self.module.add_value(Some(pred.into()), false);
        let step_acc = self.module.add_value(Some(ih.into()), false);
        let next_acc = self.module.add_value(Some("ind/next-acc".into()), false);
        let next_index = self.module.add_value(Some("ind/next-index".into()), false);
        let final_acc = self.module.add_value(Some("ind/result".into()), false);

        let head_cont = self.module.reserve_continuation();
        let zero_cont = self.module.reserve_continuation();
        let loop_cont = self.module.reserve_continuation();
        let step_cont = self.module.reserve_continuation();
        let step_resume = self.module.reserve_continuation();
        let exit_cont = self.module.reserve_continuation();

        let exit_body = self.jump(target, vec![CpsAtom::Value(final_acc)]);
        self.module.define_continuation(
            exit_cont,
            CpsContinuation {
                debug_name: Some("ind/exit".into()),
                params: vec![final_acc],
                body: exit_body,
            },
        );

        let loop_back = self.jump(
            loop_cont,
            vec![CpsAtom::Value(next_index), CpsAtom::Value(next_acc)],
        );
        let increment = self.module.add_node(CpsNode::LetPrim {
            result: next_index,
            op: CpsPrimOp::NatAdd,
            args: vec![
                CpsAtom::Value(step_index),
                CpsAtom::Literal(CpsLiteral::Nat(1)),
            ],
            next: loop_back,
        });
        self.module.define_continuation(
            step_resume,
            CpsContinuation {
                debug_name: Some("ind/step-resume".into()),
                params: vec![next_acc],
                body: increment,
            },
        );

        let mut step_env = env.clone();
        step_env.insert(pred.into(), CpsAtom::Value(step_index));
        step_env.insert(ih.into(), CpsAtom::Value(step_acc));
        let step_body = self.lower_term(succ_case, &step_env, step_resume)?;
        let step_body = self.module.add_node(CpsNode::LetCont {
            continuations: vec![step_resume],
            body: step_body,
        });
        self.module.define_continuation(
            step_cont,
            CpsContinuation {
                debug_name: Some("ind/step".into()),
                params: vec![step_index, step_acc],
                body: step_body,
            },
        );

        let comparison = self.module.add_value(Some("ind/done".into()), false);
        let switch = self.module.add_node(CpsNode::Switch {
            scrutinee: CpsAtom::Value(comparison),
            cases: BTreeMap::from([(
                0,
                CpsEdge {
                    target: step_cont,
                    args: vec![CpsAtom::Value(loop_index), CpsAtom::Value(loop_acc)],
                },
            )]),
            default: Some(CpsEdge {
                target: exit_cont,
                args: vec![CpsAtom::Value(loop_acc)],
            }),
        });
        let loop_body = self.module.add_node(CpsNode::LetPrim {
            result: comparison,
            op: CpsPrimOp::NatEql,
            args: vec![CpsAtom::Value(loop_index), CpsAtom::Value(head_value)],
            next: switch,
        });
        self.module.define_continuation(
            loop_cont,
            CpsContinuation {
                debug_name: Some("ind/loop".into()),
                params: vec![loop_index, loop_acc],
                body: loop_body,
            },
        );

        let zero_body = self.jump(
            loop_cont,
            vec![
                CpsAtom::Literal(CpsLiteral::Nat(0)),
                CpsAtom::Value(zero_value),
            ],
        );
        self.module.define_continuation(
            zero_cont,
            CpsContinuation {
                debug_name: Some("ind/zero-resume".into()),
                params: vec![zero_value],
                body: zero_body,
            },
        );

        let zero_entry = self.lower_term(zero_case, env, zero_cont)?;
        let inner = self.module.add_node(CpsNode::LetCont {
            continuations: vec![zero_cont, loop_cont, step_cont, exit_cont],
            body: zero_entry,
        });
        self.module.define_continuation(
            head_cont,
            CpsContinuation {
                debug_name: Some("ind/head-resume".into()),
                params: vec![head_value],
                body: inner,
            },
        );
        let entry = self.lower_term(head, env, head_cont)?;
        Ok(self.module.add_node(CpsNode::LetCont {
            continuations: vec![head_cont],
            body: entry,
        }))
    }

    fn lower_primitive(
        &mut self,
        primitive: &Prim,
        env: &Env,
        target: CpsContId,
    ) -> LowerResult<CpsNodeId> {
        match primitive {
            Prim::Pure(primitive) => self.lower_pure_primitive(primitive, env, target),
            Prim::Host(HostPrim::IoExit(code)) => {
                self.lower_sequence(vec![code], env, |lowerer, mut args| {
                    Ok(lowerer.module.add_node(CpsNode::Exit {
                        value: Some(args.remove(0)),
                    }))
                })
            }
            Prim::Host(HostPrim::Foreign(function, args)) => {
                let function = Arc::clone(function);
                self.lower_sequence(args.iter().collect(), env, move |lowerer, args| {
                    let arity = function.signature.results.len();
                    if arity == 1 {
                        return Ok(lowerer.module.add_node(CpsNode::Foreign {
                            function,
                            args,
                            return_to: target,
                        }));
                    }

                    let results = (0..arity)
                        .map(|index| {
                            lowerer
                                .module
                                .add_value(Some(format!("foreign/result{index}")), false)
                        })
                        .collect::<Vec<_>>();
                    let record = lowerer
                        .module
                        .add_value(Some("foreign/record".into()), false);
                    let next = lowerer.jump(target, vec![CpsAtom::Value(record)]);
                    let pack = lowerer.module.add_node(CpsNode::LetValue {
                        result: record,
                        value: CpsValueExpr::Tuple(
                            results.iter().copied().map(CpsAtom::Value).collect(),
                        ),
                        next,
                    });
                    let resume = lowerer.module.reserve_continuation();
                    lowerer.module.define_continuation(
                        resume,
                        CpsContinuation {
                            debug_name: Some("foreign/resume".into()),
                            params: results,
                            body: pack,
                        },
                    );
                    let call = lowerer.module.add_node(CpsNode::Foreign {
                        function,
                        args,
                        return_to: resume,
                    });
                    Ok(lowerer.module.add_node(CpsNode::LetCont {
                        continuations: vec![resume],
                        body: call,
                    }))
                })
            }
            Prim::Cell(CellPrim::New(init)) => {
                self.lower_sequence(vec![init], env, |lowerer, args| {
                    Ok(lowerer.module.add_node(CpsNode::Cell {
                        op: CpsCellOp::New,
                        args,
                        return_to: target,
                    }))
                })
            }
            Prim::Cell(CellPrim::Get(cell)) => {
                self.lower_sequence(vec![cell], env, |lowerer, args| {
                    Ok(lowerer.module.add_node(CpsNode::Cell {
                        op: CpsCellOp::Get,
                        args,
                        return_to: target,
                    }))
                })
            }
            Prim::Cell(CellPrim::Set(cell, value)) => {
                self.lower_sequence(vec![cell, value], env, |lowerer, args| {
                    let unit = lowerer.module.add_value(Some("cell/unit".into()), false);
                    let next = lowerer.jump(target, vec![CpsAtom::Value(unit)]);
                    let pack = lowerer.module.add_node(CpsNode::LetValue {
                        result: unit,
                        value: CpsValueExpr::Tuple(vec![]),
                        next,
                    });
                    let resume = lowerer.module.reserve_continuation();
                    lowerer.module.define_continuation(
                        resume,
                        CpsContinuation {
                            debug_name: Some("cell/set-resume".into()),
                            params: vec![],
                            body: pack,
                        },
                    );
                    let set = lowerer.module.add_node(CpsNode::Cell {
                        op: CpsCellOp::Set,
                        args,
                        return_to: resume,
                    });
                    Ok(lowerer.module.add_node(CpsNode::LetCont {
                        continuations: vec![resume],
                        body: set,
                    }))
                })
            }
        }
    }

    fn lower_pure_primitive(
        &mut self,
        primitive: &PurePrim,
        env: &Env,
        target: CpsContId,
    ) -> LowerResult<CpsNodeId> {
        macro_rules! unary {
            ($operand:expr, $op:expr) => {
                self.lower_sequence(vec![$operand], env, |lowerer, args| {
                    lowerer.finish_prim($op, args, target)
                })
            };
        }
        macro_rules! binary {
            ($left:expr, $right:expr, $op:expr) => {
                self.lower_sequence(vec![$left, $right], env, |lowerer, args| {
                    lowerer.finish_prim($op, args, target)
                })
            };
        }
        macro_rules! ternary {
            ($a:expr, $b:expr, $c:expr, $op:expr) => {
                self.lower_sequence(vec![$a, $b, $c], env, |lowerer, args| {
                    lowerer.finish_prim($op, args, target)
                })
            };
        }

        match primitive {
            PurePrim::Nat(value) => {
                Ok(self.jump(target, vec![CpsAtom::Literal(CpsLiteral::Nat(*value))]))
            }
            PurePrim::Int(value) => {
                Ok(self.jump(target, vec![CpsAtom::Literal(CpsLiteral::Int(*value))]))
            }
            PurePrim::Flt(value) => {
                Ok(self.jump(target, vec![CpsAtom::Literal(CpsLiteral::Flt(*value))]))
            }
            PurePrim::Bin(grain, value) => Ok(self.jump(
                target,
                vec![CpsAtom::Literal(CpsLiteral::Bin(*grain, value.clone()))],
            )),
            PurePrim::Io(token) => Ok(self.jump(
                target,
                vec![CpsAtom::Literal(CpsLiteral::Bin(
                    Grain::X,
                    PackedBin::from_bytes(BigUint::from(*token).to_bytes_le()),
                ))],
            )),
            PurePrim::NatEql(a, b) => binary!(a, b, CpsPrimOp::NatEql),
            PurePrim::NatNeq(a, b) => binary!(a, b, CpsPrimOp::NatNeq),
            PurePrim::NatAdd(a, b) => binary!(a, b, CpsPrimOp::NatAdd),
            PurePrim::NatSub(a, b) => binary!(a, b, CpsPrimOp::NatSub),
            PurePrim::NatMul(a, b) => binary!(a, b, CpsPrimOp::NatMul),
            PurePrim::NatLt(a, b) => binary!(a, b, CpsPrimOp::NatLt),
            PurePrim::NatDiv(a, b) => binary!(a, b, CpsPrimOp::NatDiv),
            PurePrim::NatRem(a, b) => binary!(a, b, CpsPrimOp::NatRem),
            PurePrim::NatGt(a, b) => binary!(a, b, CpsPrimOp::NatGt),
            PurePrim::NatLte(a, b) => binary!(a, b, CpsPrimOp::NatLte),
            PurePrim::NatGte(a, b) => binary!(a, b, CpsPrimOp::NatGte),
            PurePrim::NatAnd(a, b) => binary!(a, b, CpsPrimOp::NatAnd),
            PurePrim::NatOr(a, b) => binary!(a, b, CpsPrimOp::NatOr),
            PurePrim::NatXor(a, b) => binary!(a, b, CpsPrimOp::NatXor),
            PurePrim::NatShl(a, b) => binary!(a, b, CpsPrimOp::NatShl),
            PurePrim::NatShr(a, b) => binary!(a, b, CpsPrimOp::NatShr),
            PurePrim::NatToInt(a) => unary!(a, CpsPrimOp::NatToInt),
            PurePrim::NatToFlt(a) => unary!(a, CpsPrimOp::NatToFlt),
            PurePrim::IntEql(a, b) => binary!(a, b, CpsPrimOp::IntEql),
            PurePrim::IntNeq(a, b) => binary!(a, b, CpsPrimOp::IntNeq),
            PurePrim::IntAdd(a, b) => binary!(a, b, CpsPrimOp::IntAdd),
            PurePrim::IntSub(a, b) => binary!(a, b, CpsPrimOp::IntSub),
            PurePrim::IntMul(a, b) => binary!(a, b, CpsPrimOp::IntMul),
            PurePrim::IntDiv(a, b) => binary!(a, b, CpsPrimOp::IntDiv),
            PurePrim::IntRem(a, b) => binary!(a, b, CpsPrimOp::IntRem),
            PurePrim::IntLt(a, b) => binary!(a, b, CpsPrimOp::IntLt),
            PurePrim::IntGt(a, b) => binary!(a, b, CpsPrimOp::IntGt),
            PurePrim::IntLte(a, b) => binary!(a, b, CpsPrimOp::IntLte),
            PurePrim::IntGte(a, b) => binary!(a, b, CpsPrimOp::IntGte),
            PurePrim::IntAnd(a, b) => binary!(a, b, CpsPrimOp::IntAnd),
            PurePrim::IntOr(a, b) => binary!(a, b, CpsPrimOp::IntOr),
            PurePrim::IntXor(a, b) => binary!(a, b, CpsPrimOp::IntXor),
            PurePrim::IntShl(a, b) => binary!(a, b, CpsPrimOp::IntShl),
            PurePrim::IntShr(a, b) => binary!(a, b, CpsPrimOp::IntShr),
            PurePrim::IntToNat(a) => unary!(a, CpsPrimOp::IntToNat),
            PurePrim::IntToFlt(a) => unary!(a, CpsPrimOp::IntToFlt),
            PurePrim::FltAdd(a, b) => binary!(a, b, CpsPrimOp::FltAdd),
            PurePrim::FltSub(a, b) => binary!(a, b, CpsPrimOp::FltSub),
            PurePrim::FltMul(a, b) => binary!(a, b, CpsPrimOp::FltMul),
            PurePrim::FltDiv(a, b) => binary!(a, b, CpsPrimOp::FltDiv),
            PurePrim::FltRem(a, b) => binary!(a, b, CpsPrimOp::FltRem),
            PurePrim::FltEql(a, b) => binary!(a, b, CpsPrimOp::FltEql),
            PurePrim::FltNeq(a, b) => binary!(a, b, CpsPrimOp::FltNeq),
            PurePrim::FltLt(a, b) => binary!(a, b, CpsPrimOp::FltLt),
            PurePrim::FltGt(a, b) => binary!(a, b, CpsPrimOp::FltGt),
            PurePrim::FltLte(a, b) => binary!(a, b, CpsPrimOp::FltLte),
            PurePrim::FltGte(a, b) => binary!(a, b, CpsPrimOp::FltGte),
            PurePrim::FltMin(a, b) => binary!(a, b, CpsPrimOp::FltMin),
            PurePrim::FltMax(a, b) => binary!(a, b, CpsPrimOp::FltMax),
            PurePrim::FltNeg(a) => unary!(a, CpsPrimOp::FltNeg),
            PurePrim::FltAbs(a) => unary!(a, CpsPrimOp::FltAbs),
            PurePrim::FltSqrt(a) => unary!(a, CpsPrimOp::FltSqrt),
            PurePrim::FltFloor(a) => unary!(a, CpsPrimOp::FltFloor),
            PurePrim::FltCeil(a) => unary!(a, CpsPrimOp::FltCeil),
            PurePrim::FltTrunc(a) => unary!(a, CpsPrimOp::FltTrunc),
            PurePrim::FltNearest(a) => unary!(a, CpsPrimOp::FltNearest),
            PurePrim::FltToNat(a) => unary!(a, CpsPrimOp::FltToNat),
            PurePrim::FltToLeBytes(a) => unary!(a, CpsPrimOp::FltToLeBytes),
            PurePrim::FltOfLeBytes(a) => unary!(a, CpsPrimOp::FltOfLeBytes),
            PurePrim::FltToInt(a) => unary!(a, CpsPrimOp::FltToInt),
            PurePrim::BinLen(grain, a) => unary!(a, CpsPrimOp::BinLen(*grain)),
            PurePrim::BinEql(grain, a, b) => binary!(a, b, CpsPrimOp::BinEql(*grain)),
            PurePrim::IoEql(a, b) => binary!(a, b, CpsPrimOp::BinEql(Grain::X)),
            PurePrim::BinGet(grain, a, b) => binary!(a, b, CpsPrimOp::BinGet(*grain)),
            PurePrim::BinSlice(grain, a, b, c) => {
                ternary!(a, b, c, CpsPrimOp::BinSlice(*grain))
            }
            PurePrim::BinAppend(grain, a, b) => binary!(a, b, CpsPrimOp::BinAppend(*grain)),
            PurePrim::BinConcat(grain, operands) => {
                let arity = operands.len();
                self.lower_sequence(operands.iter().collect(), env, move |lowerer, args| {
                    lowerer.finish_prim(CpsPrimOp::BinConcat(*grain, arity), args, target)
                })
            }
            PurePrim::Lst(elements) => {
                self.lower_sequence(elements.iter().collect(), env, |lowerer, elements| {
                    let result = lowerer.module.add_value(None, false);
                    let next = lowerer.jump(target, vec![CpsAtom::Value(result)]);
                    Ok(lowerer.module.add_node(CpsNode::LetValue {
                        result,
                        value: CpsValueExpr::List(elements),
                        next,
                    }))
                })
            }
            PurePrim::LstLen(a) => unary!(a, CpsPrimOp::LstLen),
            PurePrim::LstGet(a, b) => binary!(a, b, CpsPrimOp::LstGet),
            PurePrim::LstSlice(a, b, c) => ternary!(a, b, c, CpsPrimOp::LstSlice),
            PurePrim::LstAppend(a, b) => binary!(a, b, CpsPrimOp::LstAppend),
            PurePrim::LstConcat(operands) => {
                let arity = operands.len();
                self.lower_sequence(operands.iter().collect(), env, move |lowerer, args| {
                    lowerer.finish_prim(CpsPrimOp::LstConcat(arity), args, target)
                })
            }
            PurePrim::LstMap(source, function) => {
                self.lower_sequence(vec![source, function], env, |lowerer, args| {
                    Ok(lowerer.module.add_node(CpsNode::Intrinsic {
                        op: CpsIntrinsicOp::LstMap,
                        args,
                        return_to: target,
                    }))
                })
            }
        }
    }
}
