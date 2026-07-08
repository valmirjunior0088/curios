use super::*;

/// Self-tail-recursion → loop conversion.
///
/// A function whose every direct self-call is a *tail* call — its `resume` is
/// the function's own resume sentinel, so the result flows straight out — is
/// not really recursive: the IR's blocks-with-params plus `Jump` already
/// express the loop it denotes. This pass rewrites it as one:
///
/// ```text
///   Func f params [p1..pn] resume r:        Func f params [p1@loop..pn@loop] resume r:
///     region R                                Jump(b@loop, [p1@loop..pn@loop])
///     ... f(a1..an) r ...           =>        block b@loop[p1..pn]:
///                                               region R, with each
///                                               f(a1..an) r  →  Jump(b@loop, [a1..an])
/// ```
///
/// The header block takes over the *original* parameter names, so `R` needs no
/// use-rewriting at all — only the self-call tails change, and the function's
/// outer parameters get fresh `@loop` names that exist solely to feed the entry
/// jump. Codegen already supports the backward edge: every jump compiles to a
/// `br` back into the region's dispatch loop, and the pure-call interpreter
/// dispatches jumps iteratively under a step budget.
///
/// After conversion the function has no direct self-call, so it drops out of
/// every direct-call cycle and [`inline_calls`] no longer excludes it — the
/// loop can be spliced into a caller like any other body. Self-calls become
/// *visible* as direct calls in the first place via the self-capture seed in
/// [`lift_closures`], which devirtualizes a recursive closure's indirect
/// self-call; this pass is its second half.
///
/// A function with any *non*-tail self-call is left untouched: converting only
/// the tail sites would keep the function in a cycle, paying the rewrite for
/// no structural gain. Mutual recursion is out of scope — only self-cycles.
pub(crate) fn convert_tail_recursion(module: &mut Module) {
    for (name, func) in module.funcs_mut() {
        func.convert_tail_recursion(name);
    }
}

impl Func {
    fn convert_tail_recursion(&mut self, name: &FuncName) {
        let counts = count_self_calls(&self.region, name, &self.resume);
        if counts.tail == 0 || counts.non_tail > 0 {
            return;
        }

        let header = mangle::loop_header();
        rewrite_self_calls(&mut self.region, name, &header);

        // The header block inherits the original parameter names; the function's
        // new outer parameters keep their candidate flags so specialization still
        // keys on them.
        let header_params: Vec<ValueName> = self.params.iter().map(|p| p.name.clone()).collect();
        let outer: Vec<Argument> = self
            .params
            .iter()
            .map(|param| Argument {
                name: mangle::loop_param(&param.name),
                candidate: param.candidate,
            })
            .collect();
        let entry_args: Vec<ValueName> = outer.iter().map(|a| a.name.clone()).collect();

        let body = std::mem::replace(
            &mut self.region,
            Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail: Tail::Unreachable,
            },
        );

        self.region = Region {
            preallocs: vec![],
            values: vec![],
            blocks: vec![(
                header.clone(),
                Block {
                    params: header_params,
                    region: body,
                },
            )],
            tail: Tail::Jump(JumpTarget {
                target: header,
                params: entry_args,
            }),
        };
        self.params = outer;
    }
}

#[derive(Default)]
struct SelfCalls {
    tail: usize,
    non_tail: usize,
}

fn count_self_calls(region: &Region, name: &FuncName, sentinel: &BlockName) -> SelfCalls {
    fn walk(region: &Region, name: &FuncName, sentinel: &BlockName, counts: &mut SelfCalls) {
        if let Tail::Call(CallTarget::Direct { target, resume, .. }) = &region.tail
            && target == name
        {
            if resume == sentinel {
                counts.tail += 1;
            } else {
                counts.non_tail += 1;
            }
        }

        for (_, block) in &region.blocks {
            walk(&block.region, name, sentinel, counts);
        }
    }

    let mut counts = SelfCalls::default();
    walk(region, name, sentinel, &mut counts);
    counts
}

/// Rewrite every direct self-call into a jump to the loop header. Only called
/// once the gate has established that all self-calls are tail calls.
fn rewrite_self_calls(region: &mut Region, name: &FuncName, header: &BlockName) {
    if let Tail::Call(CallTarget::Direct { target, params, .. }) = &region.tail
        && target == name
    {
        region.tail = Tail::Jump(JumpTarget {
            target: header.clone(),
            params: params.clone(),
        });
    }

    for (_, block) in &mut region.blocks {
        rewrite_self_calls(&mut block.region, name, header);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(name: &str) -> ValueName {
        ValueName::from(name)
    }

    fn b(name: &str) -> BlockName {
        BlockName::from(name)
    }

    fn direct(target: &str, args: Vec<ValueName>, resume: &str) -> Tail {
        Tail::Call(CallTarget::Direct {
            target: FuncName::from(target),
            params: args,
            resume: b(resume),
        })
    }

    fn func_named<'a>(module: &'a Module, name: &str) -> &'a Func {
        module
            .funcs()
            .iter()
            .find(|(n, _)| n.as_str() == name)
            .map(|(_, func)| func)
            .expect("func present")
    }

    #[test]
    fn converts_all_tail_self_recursion_to_a_loop() {
        // f(n) jumps into b1, which self-calls f with the sentinel resume —
        // the nested-block case, so the rewrite must reach inside blocks.
        let recurse = Block {
            params: vec![v("m")],
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail: direct("f", vec![v("m")], "r"),
            },
        };
        let f = Func {
            params: vec![Argument {
                name: v("n"),
                candidate: true,
            }],
            resume: b("r"),
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![(b("b1"), recurse)],
                tail: Tail::Jump(JumpTarget {
                    target: b("b1"),
                    params: vec![v("n")],
                }),
            },
        };

        let mut module = Module::new();
        module.add_func(FuncName::from("f"), f);

        convert_tail_recursion(&mut module);

        let f = func_named(&module, "f");

        // Outer params are freshened, candidate flags preserved.
        assert_eq!(f.params.len(), 1);
        assert_eq!(f.params[0].name, v("n@loop"));
        assert!(f.params[0].candidate);

        // The entry region just feeds the header block.
        match &f.region.tail {
            Tail::Jump(target) => {
                assert_eq!(target.target, b("b@loop"));
                assert_eq!(target.params, vec![v("n@loop")]);
            }
            other => panic!("expected entry jump to header, got {other:?}"),
        }

        // The header takes over the original param name and wraps the body.
        let (header_name, header) = &f.region.blocks[0];
        assert_eq!(header_name, &b("b@loop"));
        assert_eq!(header.params, vec![v("n")]);

        // The nested self-call became a backward jump to the header.
        let (_, inner) = &header.region.blocks[0];
        match &inner.region.tail {
            Tail::Jump(target) => {
                assert_eq!(target.target, b("b@loop"));
                assert_eq!(target.params, vec![v("m")]);
            }
            other => panic!("expected backward jump, got {other:?}"),
        }
    }

    #[test]
    fn leaves_non_tail_self_call_alone() {
        // The self-call resumes at an interior block `k`, not the sentinel —
        // its result is consumed, so this is genuine recursion, not a loop.
        let k = Block {
            params: vec![v("res")],
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail: Tail::Jump(JumpTarget {
                    target: b("r"),
                    params: vec![v("res")],
                }),
            },
        };
        let f = Func {
            params: vec![v("n").into()],
            resume: b("r"),
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![(b("k"), k)],
                tail: direct("f", vec![v("n")], "k"),
            },
        };

        let mut module = Module::new();
        module.add_func(FuncName::from("f"), f);

        convert_tail_recursion(&mut module);

        let f = func_named(&module, "f");
        assert_eq!(f.params, vec![v("n")]);
        assert!(matches!(
            f.region.tail,
            Tail::Call(CallTarget::Direct { .. })
        ));
    }

    #[test]
    fn one_non_tail_self_call_disables_the_whole_conversion() {
        // One tail self-call plus one non-tail self-call: converting only the
        // tail site would leave the function recursive anyway, so neither is
        // touched.
        let k = Block {
            params: vec![v("res")],
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail: direct("f", vec![v("res")], "r"),
            },
        };
        let f = Func {
            params: vec![v("n").into()],
            resume: b("r"),
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![(b("k"), k)],
                tail: direct("f", vec![v("n")], "k"),
            },
        };

        let mut module = Module::new();
        module.add_func(FuncName::from("f"), f);

        convert_tail_recursion(&mut module);

        let f = func_named(&module, "f");
        assert_eq!(f.params, vec![v("n")]);
        assert!(matches!(
            f.region.tail,
            Tail::Call(CallTarget::Direct { .. })
        ));
        assert!(matches!(
            f.region.blocks[0].1.region.tail,
            Tail::Call(CallTarget::Direct { .. })
        ));
    }

    #[test]
    fn leaves_non_recursive_functions_alone() {
        // f calls g, never itself — nothing to convert on either side.
        let f = Func {
            params: vec![v("n").into()],
            resume: b("r"),
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail: direct("g", vec![v("n")], "r"),
            },
        };
        let g = Func {
            params: vec![v("m").into()],
            resume: b("rg"),
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail: Tail::Jump(JumpTarget {
                    target: b("rg"),
                    params: vec![v("m")],
                }),
            },
        };

        let mut module = Module::new();
        module.add_func(FuncName::from("f"), f);
        module.add_func(FuncName::from("g"), g);

        convert_tail_recursion(&mut module);

        assert_eq!(func_named(&module, "f").params, vec![v("n")]);
        assert!(matches!(
            func_named(&module, "f").region.tail,
            Tail::Call(CallTarget::Direct { .. })
        ));
        assert_eq!(func_named(&module, "g").params, vec![v("m")]);
    }
}
