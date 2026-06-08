use {
    super::*,
    std::collections::{HashMap, HashSet},
};

/// Closure specialization — monomorphization on first-class-function arguments.
///
/// Type-directed erasure flagged every parameter whose pre-erasure type was a
/// function, a `Type`, or unit as a specialization *candidate* (`Argument::candidate`)
/// — each a compile-time-constant shape. This pass uses that flag to clone a function
/// per distinct shape passed into a candidate position, baking the shape into the
/// clone so the abstract parameter becomes a statically-known value: a known closure
/// (captures threaded through), or unit (the erasure of a `Type`/unit argument,
/// baked as the constant `{}`). The closure case below is the motivating one; unit
/// is the same machinery with nothing to thread.
///
/// ```text
///   Clsr c { fields:[e], params:[x], .. }       // captures one env value
///   Func f { params:[*p, n], .. }               //  *p is a candidate
///       ... p(n) ...  (indirect, through p)
///
///   let clo = c{env};                           // p's argument is a known closure
///   f(clo, k)   (direct)
/// ```
/// becomes
/// ```text
///   Func f@spec__0_c { params:[e, n], .. }      // candidate param dropped...
///       let p = c{e};                           //  ...rebuilt from a threaded capture,
///       ... p(n) ...                            //     now a KNOWN closure.
///   f@spec__0_c(env, k)   (direct)
/// ```
///
/// The pass does not devirtualize `p(n)` itself: it only makes `p` a known
/// `Data::Clsr`, which the later [`lift_closures`] pass then turns into a direct
/// call. Specialization's whole job is to feed closure-lifting the known closures
/// it cannot otherwise see — the cases [`inline_calls`] gives up on, namely
/// multi-call-site and recursive higher-order combinators (`map`, `fold`).
///
/// # Two sites: parameters at calls, captures at allocations
///
/// The same monomorphization runs at two sites. The first (above) keys a *function*
/// on its candidate **parameters** at a `Direct` **call**. The second keys a
/// *closure* on its candidate **captures** at its **allocation** — a closure that
/// dispatches on a captured combinator (`c{*p, ..}(..)` whose body does `p(..)`)
/// is abstract in `p` inside its own body, but its allocation `let v = c{q, ..}`
/// names the captured closure `q` right there. Cloning `c` per the shapes of its
/// known candidate captures bakes them in exactly as for parameters, so the inner
/// indirect calls become known-closure calls for [`lift_closures`] to devirtualize.
/// Allocations are plain `Data::Clsr` constructions — never hidden behind dispatch —
/// so this needs no control-flow analysis to find them. Both sites share the clone
/// builder ([`specialize_arguments`]): "drop each resolved candidate argument,
/// thread the baked closure's own captures through as leading args, rebind the
/// dropped name to its baked shape at the body top."
///
/// # Threading captures instead of baking them
///
/// A closure's captured environment lives in the *caller's* scope, so it cannot be
/// baked into the callee as a constant. Only the closure's *identity* `c{·}` is
/// baked; the captures are threaded through as ordinary (non-candidate) leading
/// parameters in place of the dropped candidate one. Within a single clone the
/// `ClsrName` is fixed, so its capture arity is fixed — call sites that pass the
/// same closure *shape* but different captured values share the clone. That is
/// monomorphization keyed on shape, and it is what makes `p` known inside the
/// clone without its captures having to be compile-time constants.
///
/// # Recursion ties its own knot
///
/// A recursive combinator passes its own candidate parameter back to itself. In
/// the original body that parameter is abstract, so its self-call does not
/// specialize. In a clone the parameter is rebound to a known closure, so the
/// *clone's* self-call now sees a known closure in that position and specializes
/// on the next round — to a name that encodes the same shape key, which already
/// exists, so the self-call is simply retargeted to the clone. The clone thus
/// recurses into itself with the closure fully devirtualized, with no special
/// handling of self-calls: the fixed-point loop plus the name-as-key memo do it.
///
/// # Termination
///
/// Each round either retargets a call to a more-specialized callee or mints a new
/// clone, and a clone's name is a pure function of `(base, shape key)`, so a given
/// specialization is built at most once. The statically-known closures in any one
/// body form a finite acyclic set (a name's binding precedes its uses), so the
/// reachable family of clones is finite and the loop converges.
pub fn specialize_calls(module: &mut Module) {
    loop {
        let candidates = candidate_positions(module);
        let capture_candidates = candidate_capture_positions(module);
        let fields = closure_fields(module);
        let func_bodies = func_bodies(module);
        let clsr_bodies = clsr_bodies(module);

        let mut spec = Specializer {
            candidates: &candidates,
            capture_candidates: &capture_candidates,
            needed_funcs: HashMap::new(),
            needed_clsrs: HashMap::new(),
            changed: false,
        };

        for (_, func) in module.funcs_mut() {
            let scope = func.params.iter().map(|a| a.name.clone()).collect();
            spec.specialize_body(&mut func.region, scope);
        }
        for (_, clsr) in module.clsrs_mut() {
            let scope = clsr
                .fields
                .iter()
                .chain(clsr.params.iter())
                .map(|a| a.name.clone())
                .collect();
            spec.specialize_body(&mut clsr.region, scope);
        }

        let Specializer {
            needed_funcs,
            needed_clsrs,
            mut changed,
            ..
        } = spec;

        for (name, plan) in needed_funcs {
            if module.funcs().iter().any(|(present, _)| present == &name) {
                continue;
            }

            let base = func_bodies.get(&plan.base).expect("base function present");
            module.add_func(name, build_specialized(base, &plan, &fields));
            changed = true;
        }

        for (name, plan) in needed_clsrs {
            if module.clsrs().iter().any(|(present, _)| present == &name) {
                continue;
            }

            let base = clsr_bodies.get(&plan.base).expect("base closure present");
            module.add_clsr(name, build_specialized_clsr(base, &plan, &fields));
            changed = true;
        }

        if !changed {
            return;
        }
    }
}

/// The driver for one fixed-point round: the read-only snapshots both rewrite sites
/// consult, plus the clones each site discovered (minted by the caller) and whether
/// anything changed this round.
struct Specializer<'a> {
    candidates: &'a HashMap<FuncName, Vec<usize>>,
    capture_candidates: &'a HashMap<ClsrName, Vec<usize>>,
    needed_funcs: HashMap<FuncName, SpecPlan>,
    needed_clsrs: HashMap<ClsrName, ClsrSpecPlan>,
    changed: bool,
}

/// The compile-time value a candidate position resolved to — the two shapes erasure
/// leaves at a candidate parameter:
///
/// - a known closure, baked by identity with its captures threaded through;
/// - unit (`{}`), the erasure of a `Type` or unit argument, baked as a constant.
#[derive(Clone, PartialEq)]
enum Shape {
    Clsr(ClsrName),
    Unit,
}

impl std::fmt::Display for Shape {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Shape::Clsr(clsr) => write!(formatter, "{clsr}"),
            Shape::Unit => write!(formatter, "unit"),
        }
    }
}

/// The plan for one specialized clone: which base function it comes from, and the
/// shape each candidate position resolved to (in ascending position order).
struct SpecPlan {
    base: FuncName,
    resolved: Vec<(usize, Shape)>,
}

/// The same plan for a specialized *closure* clone, keyed on candidate *capture*
/// positions instead of parameter positions.
struct ClsrSpecPlan {
    base: ClsrName,
    resolved: Vec<(usize, Shape)>,
}

// --- Read-only snapshots ----------------------------------------------------

/// The candidate parameter positions of every function — the only positions worth
/// keying a specialization on.
fn candidate_positions(module: &Module) -> HashMap<FuncName, Vec<usize>> {
    module
        .funcs()
        .iter()
        .map(|(name, func)| {
            let positions = func
                .params
                .iter()
                .enumerate()
                .filter(|(_, arg)| arg.candidate)
                .map(|(index, _)| index)
                .collect();

            (name.clone(), positions)
        })
        .collect()
}

/// The candidate capture positions of every closure — the field positions worth
/// keying a closure specialization on, the allocation-site mirror of
/// [`candidate_positions`].
fn candidate_capture_positions(module: &Module) -> HashMap<ClsrName, Vec<usize>> {
    module
        .clsrs()
        .iter()
        .map(|(name, clsr)| {
            let positions = clsr
                .fields
                .iter()
                .enumerate()
                .filter(|(_, arg)| arg.candidate)
                .map(|(index, _)| index)
                .collect();

            (name.clone(), positions)
        })
        .collect()
}

/// Every closure's captured fields, for building the threaded capture parameters
/// and the rebinding closure shape of each clone.
fn closure_fields(module: &Module) -> HashMap<ClsrName, Vec<Argument>> {
    module
        .clsrs()
        .iter()
        .map(|(name, clsr)| (name.clone(), clsr.fields.clone()))
        .collect()
}

/// A snapshot of every function body, so clones can be built from a base while the
/// module is borrowed mutably for the rewrite.
fn func_bodies(module: &Module) -> HashMap<FuncName, Func> {
    module
        .funcs()
        .iter()
        .map(|(name, func)| (name.clone(), func.clone()))
        .collect()
}

/// A snapshot of every closure body, the allocation-site mirror of [`func_bodies`].
fn clsr_bodies(module: &Module) -> HashMap<ClsrName, Clsr> {
    module
        .clsrs()
        .iter()
        .map(|(name, clsr)| (name.clone(), clsr.clone()))
        .collect()
}

// --- Site rewriting ---------------------------------------------------------

/// The statically-known value a name is bound to. A closure carries the captures to
/// thread at the call site; unit carries nothing.
enum KnownValue {
    Clsr(ClsrName, Vec<ValueName>),
    Unit,
}

/// Maps a value name to the compile-time value it is statically bound to.
type Known = HashMap<ValueName, KnownValue>;

impl Specializer<'_> {
    /// Rewrite both sites in one body against a single tree-wide snapshot of its
    /// statically-known values. The snapshot is owned (it clones the data it
    /// records), so the in-place rewrites below never invalidate it; transitive and
    /// nested specialization is resolved across outer fixed-point rounds, which
    /// rebuild every snapshot.
    fn specialize_body(&mut self, region: &mut Region, scope: HashSet<ValueName>) {
        let known = known_values(region);

        if known.is_empty() {
            return;
        }

        // A recursive closure is built as a `prealloc` reserving its shape, then a
        // fill binding writing its captures — and the fill can sit in a descendant
        // region of its prealloc. Both name the same value, so specializing the fill
        // means re-pointing its prealloc to the same clone, or the reserved slot and
        // the written value disagree on layout. Record every fill we respecialize
        // across the whole body, then sync the preallocs tree-wide.
        let mut respecialized: HashMap<ValueName, ClsrName> = HashMap::new();
        self.rewrite_region(region, &known, &scope, &mut respecialized);

        if !respecialized.is_empty() {
            sync_preallocs(region, &respecialized);
        }
    }

    /// Rewrite one region, tracking the value names in scope on the path from the
    /// body root. `known` is tree-wide, but a known closure's *captures* are only
    /// valid where they are in scope: a recursive closure's `prealloc` shell makes
    /// its name visible across the shell's whole subtree, while its captures are
    /// written by a fill in just one descendant. Threading those captures into a
    /// sibling of the fill would emit out-of-scope references, so each splice is
    /// gated on `scope` (see [`Self::resolve`]).
    fn rewrite_region(
        &mut self,
        region: &mut Region,
        known: &Known,
        scope: &HashSet<ValueName>,
        respecialized: &mut HashMap<ValueName, ClsrName>,
    ) {
        // A region's own preallocs and values are in scope throughout it (fills and
        // forward references included), so fold them in before rewriting.
        let mut here = scope.clone();
        here.extend(region.preallocs.iter().map(|(name, _)| name.clone()));
        here.extend(region.values.iter().map(|(name, _)| name.clone()));

        for (name, value) in &mut region.values {
            if let Some(specialized) = self.rewrite_value(value, known, &here) {
                respecialized.insert(name.clone(), specialized);
            }
        }

        self.rewrite_tail(&mut region.tail, known, &here);

        for (_, block) in &mut region.blocks {
            let mut child = here.clone();
            child.extend(block.params.iter().cloned());
            self.rewrite_region(&mut block.region, known, &child, respecialized);
        }
    }

    /// Retarget a closure *allocation* whose candidate captures carry known shapes to
    /// the matching specialized closure clone — the allocation-site mirror of
    /// [`Self::rewrite_tail`]. Each known-closure capture is expanded into its own
    /// captures and each unit capture is dropped, exactly as a call's arguments are.
    fn rewrite_value(
        &mut self,
        value: &mut Value,
        known: &Known,
        scope: &HashSet<ValueName>,
    ) -> Option<ClsrName> {
        let Value::Pure(Data::Clsr(clsr, captures)) = value else {
            return None;
        };

        let positions = self.capture_candidates.get(clsr)?;

        let resolved: Vec<(usize, Shape, Vec<ValueName>)> = positions
            .iter()
            .filter_map(|&index| resolve(known.get(captures.get(index)?)?, index, scope))
            .collect();

        if resolved.is_empty() {
            return None;
        }

        let key: Vec<(usize, Shape)> = resolved
            .iter()
            .map(|(index, shape, _)| (*index, shape.clone()))
            .collect();
        let name = specialized_clsr_name(clsr, &key);

        let new_captures = splice(captures, &resolved);

        self.needed_clsrs
            .entry(name.clone())
            .or_insert(ClsrSpecPlan {
                base: clsr.clone(),
                resolved: key,
            });

        *value = Value::Pure(Data::Clsr(name.clone(), new_captures));
        self.changed = true;

        Some(name)
    }

    /// Retarget a `Direct` call whose candidate positions carry known shapes to the
    /// matching specialized clone, expanding each known-closure argument into its
    /// captures and dropping each unit argument.
    fn rewrite_tail(&mut self, tail: &mut Tail, known: &Known, scope: &HashSet<ValueName>) {
        let Tail::Call(CallTarget::Direct {
            target,
            params,
            resume,
        }) = tail
        else {
            return;
        };

        let Some(positions) = self.candidates.get(target) else {
            return;
        };

        // Resolve the candidate positions that carry a statically-known shape, pairing
        // each with the arguments to splice in its place (a closure's captures, or
        // nothing for unit).
        let resolved: Vec<(usize, Shape, Vec<ValueName>)> = positions
            .iter()
            .filter_map(|&index| resolve(known.get(&params[index])?, index, scope))
            .collect();

        if resolved.is_empty() {
            return;
        }

        let key: Vec<(usize, Shape)> = resolved
            .iter()
            .map(|(index, shape, _)| (*index, shape.clone()))
            .collect();
        let name = specialized_func_name(target, &key);

        let args = splice(params, &resolved);

        self.needed_funcs.entry(name.clone()).or_insert(SpecPlan {
            base: target.clone(),
            resolved: key,
        });

        *tail = Tail::Call(CallTarget::Direct {
            target: name,
            params: args,
            resume: resume.clone(),
        });

        self.changed = true;
    }
}

/// Resolve a candidate position against its statically-known value, gated on scope.
///
/// A known closure's captures must be threaded into the rewritten site, so they have
/// to be in scope *there*. The closure's *name* being in scope at the site does not
/// guarantee its captures are: a recursive closure's `prealloc` shell makes the name
/// visible across the shell's whole subtree, while the fill that writes its captures
/// sits in one descendant — so a sibling of the fill sees the name but not the
/// captures. When any threaded capture is out of scope, the position is left
/// unspecialized (the site keeps referring to the closure by name). Unit threads
/// nothing, so it is always in scope.
fn resolve(
    known: &KnownValue,
    index: usize,
    scope: &HashSet<ValueName>,
) -> Option<(usize, Shape, Vec<ValueName>)> {
    match known {
        KnownValue::Clsr(clsr, captures) => captures
            .iter()
            .all(|capture| scope.contains(capture))
            .then(|| (index, Shape::Clsr(clsr.clone()), captures.clone())),
        KnownValue::Unit => Some((index, Shape::Unit, vec![])),
    }
}

/// Splice each resolved position's replacement arguments (a known closure's captures,
/// or nothing for unit) in place, keeping every other argument verbatim. Mirrors the
/// clone's argument-list construction in [`specialize_arguments`].
fn splice(args: &[ValueName], resolved: &[(usize, Shape, Vec<ValueName>)]) -> Vec<ValueName> {
    let expansions: HashMap<usize, &Vec<ValueName>> = resolved
        .iter()
        .map(|(index, _, args)| (*index, args))
        .collect();

    let mut spliced = Vec::new();
    for (index, arg) in args.iter().enumerate() {
        match expansions.get(&index) {
            Some(replacement) => spliced.extend(replacement.iter().cloned()),
            None => spliced.push(arg.clone()),
        }
    }

    spliced
}

/// Re-point every recursive-closure `prealloc` whose fill was respecialized to the
/// fill's clone, so the reserved shell and its backpatch agree on closure shape.
fn sync_preallocs(region: &mut Region, respecialized: &HashMap<ValueName, ClsrName>) {
    for (name, prealloc) in &mut region.preallocs {
        if let Prealloc::Clsr(clsr) = prealloc
            && let Some(specialized) = respecialized.get(name)
        {
            *clsr = specialized.clone();
        }
    }

    for (_, block) in &mut region.blocks {
        sync_preallocs(&mut block.region, respecialized);
    }
}

/// Collect every binding to a bakeable compile-time value — a `let v = c{captures}`
/// closure or a `let v = {}` unit — in the tree. Names are unique within a body and
/// scoping is lexical, so a single tree-wide map is sound: a call can only name a
/// value that is actually in scope at the call.
fn known_values(region: &Region) -> Known {
    let mut known = Known::new();
    collect_known(region, &mut known);
    known
}

fn collect_known(region: &Region, known: &mut Known) {
    for (name, value) in &region.values {
        match value {
            Value::Pure(Data::Clsr(clsr, captures)) => {
                known.insert(
                    name.clone(),
                    KnownValue::Clsr(clsr.clone(), captures.clone()),
                );
            }
            Value::Pure(Data::Tpl(fields)) if fields.is_empty() => {
                known.insert(name.clone(), KnownValue::Unit);
            }
            _ => {}
        }
    }

    for (_, block) in &region.blocks {
        collect_known(&block.region, known);
    }
}

/// A clone's name is a pure function of its base and shape key, so equal keys map
/// to one clone (the memo) and a self-reference's key resolves back to its own clone.
fn specialized_func_name(base: &FuncName, resolved: &[(usize, Shape)]) -> FuncName {
    FuncName::from(specialized_label(base, resolved))
}

/// The closure-clone mirror of [`specialized_func_name`].
fn specialized_clsr_name(base: &ClsrName, resolved: &[(usize, Shape)]) -> ClsrName {
    ClsrName::from(specialized_label(base, resolved))
}

fn specialized_label(base: impl std::fmt::Display, resolved: &[(usize, Shape)]) -> String {
    let mut name = format!("{base}@spec");

    for (position, shape) in resolved {
        name.push_str(&format!("__{position}_{shape}"));
    }

    name
}

// --- Clone construction -----------------------------------------------------

/// Build the specialized *function* clone: specialize on its parameters, prepend the
/// rebinds so each formerly-abstract candidate parameter is now a statically-known
/// value, and keep the resume verbatim.
fn build_specialized(
    base: &Func,
    plan: &SpecPlan,
    fields: &HashMap<ClsrName, Vec<Argument>>,
) -> Func {
    let resolved = resolved_map(&plan.resolved);
    let (params, rebinds) = specialize_arguments(&base.params, &resolved, fields);

    Func {
        params,
        resume: base.resume.clone(),
        region: rebound_region(&base.region, rebinds),
    }
}

/// Build the specialized *closure* clone — the allocation-site mirror of
/// [`build_specialized`]. Specialize on its captured `fields`, leaving `params` and
/// `resume` untouched.
fn build_specialized_clsr(
    base: &Clsr,
    plan: &ClsrSpecPlan,
    fields: &HashMap<ClsrName, Vec<Argument>>,
) -> Clsr {
    let resolved = resolved_map(&plan.resolved);
    let (new_fields, rebinds) = specialize_arguments(&base.fields, &resolved, fields);

    Clsr {
        fields: new_fields,
        params: base.params.clone(),
        resume: base.resume.clone(),
        region: rebound_region(&base.region, rebinds),
    }
}

fn resolved_map(resolved: &[(usize, Shape)]) -> HashMap<usize, &Shape> {
    resolved
        .iter()
        .map(|(index, shape)| (*index, shape))
        .collect()
}

/// The shared core of both clone builders, agnostic to function-vs-closure and
/// parameter-vs-capture: drop each resolved candidate argument and rebind its name
/// to its baked shape — a known closure threaded through its captures as leading
/// non-candidate arguments, or unit baked as the constant `{}`. Returns the rewritten
/// argument list and the rebinds to prepend to the clone's body.
fn specialize_arguments(
    base_args: &[Argument],
    resolved: &HashMap<usize, &Shape>,
    fields: &HashMap<ClsrName, Vec<Argument>>,
) -> (Vec<Argument>, Vec<(ValueName, Value)>) {
    let mut args = Vec::new();
    let mut rebinds = Vec::new();

    for (index, arg) in base_args.iter().enumerate() {
        let rebind = match resolved.get(&index) {
            None => {
                args.push(arg.clone());
                continue;
            }
            Some(Shape::Clsr(clsr)) => {
                let arity = fields.get(clsr).expect("specialized closure present").len();
                let captures: Vec<ValueName> = (0..arity)
                    .map(|field| capture_param(&arg.name, field))
                    .collect();

                // The threaded captures are plain non-candidate arguments.
                args.extend(captures.iter().cloned().map(Argument::from));
                Value::Pure(Data::Clsr((*clsr).clone(), captures))
            }
            Some(Shape::Unit) => Value::Pure(Data::Tpl(vec![])),
        };

        rebinds.push((arg.name.clone(), rebind));
    }

    (args, rebinds)
}

/// Prepend the rebinds to a clone of the base region's values, so each dropped
/// candidate name is rebound to its baked shape ahead of its first use.
fn rebound_region(base: &Region, mut rebinds: Vec<(ValueName, Value)>) -> Region {
    let mut region = base.clone();
    rebinds.extend(std::mem::take(&mut region.values));
    region.values = rebinds;
    region
}

fn capture_param(param: &ValueName, index: usize) -> ValueName {
    ValueName::from(format!("{param}@cap{index}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(name: &str) -> ValueName {
        ValueName::from(name)
    }

    fn candidate(name: &str) -> Argument {
        Argument {
            name: v(name),
            candidate: true,
        }
    }

    fn region(values: Vec<(ValueName, Value)>, tail: Tail) -> Region {
        Region {
            preallocs: vec![],
            values,
            blocks: vec![],
            tail,
        }
    }

    fn indirect(target: &str, args: Vec<ValueName>) -> Tail {
        Tail::Call(CallTarget::Indirect {
            target: v(target),
            params: args,
            resume: BlockName::from("r"),
        })
    }

    fn direct(target: &str, args: Vec<ValueName>) -> Tail {
        Tail::Call(CallTarget::Direct {
            target: FuncName::from(target),
            params: args,
            resume: BlockName::from("r"),
        })
    }

    fn func_named<'a>(module: &'a Module, name: &str) -> Option<&'a Func> {
        module
            .funcs()
            .iter()
            .find(|(n, _)| n.as_str() == name)
            .map(|(_, func)| func)
    }

    fn clsr_named<'a>(module: &'a Module, name: &str) -> Option<&'a Clsr> {
        module
            .clsrs()
            .iter()
            .find(|(n, _)| n.as_str() == name)
            .map(|(_, clsr)| clsr)
    }

    #[test]
    fn specializes_known_closure_and_threads_its_captures() {
        // f(*p, n) = p(n); main passes a known closure c{env} for p.
        let f = Func {
            params: vec![candidate("p"), v("n").into()],
            resume: BlockName::from("r"),
            region: region(vec![], indirect("p", vec![v("n")])),
        };
        let c = Clsr {
            fields: vec![v("e").into()],
            params: vec![v("x").into()],
            resume: BlockName::from("r"),
            region: region(
                vec![],
                Tail::Jump(JumpTarget {
                    target: BlockName::from("r"),
                    params: vec![v("x")],
                }),
            ),
        };
        let main = Func {
            params: vec![],
            resume: BlockName::from("r"),
            region: region(
                vec![
                    (v("env"), Value::Pure(Data::Nat(7))),
                    (v("k"), Value::Pure(Data::Nat(1))),
                    (
                        v("clo"),
                        Value::Pure(Data::Clsr(ClsrName::from("c"), vec![v("env")])),
                    ),
                ],
                direct("f", vec![v("clo"), v("k")]),
            ),
        };

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_func(FuncName::from("f"), f);
        module.add_clsr(ClsrName::from("c"), c);

        specialize_calls(&mut module);

        // The call is retargeted to the clone, with `clo` expanded into `env`.
        match &func_named(&module, "main").unwrap().region.tail {
            Tail::Call(CallTarget::Direct { target, params, .. }) => {
                assert_eq!(target.as_str(), "f@spec__0_c");
                assert_eq!(params, &vec![v("env"), v("k")]);
            }
            other => panic!("expected direct call to clone, got {other:?}"),
        }

        // The clone drops `p`, takes the threaded capture as a leading param, and
        // rebuilds the closure so `p` is a known `Data::Clsr`.
        let clone = func_named(&module, "f@spec__0_c").expect("clone present");
        assert_eq!(clone.params, vec![v("p@cap0"), v("n")]);
        assert!(matches!(
            &clone.region.values[0],
            (name, Value::Pure(Data::Clsr(c, caps)))
                if name == &v("p") && c.as_str() == "c" && caps == &vec![v("p@cap0")]
        ));
    }

    #[test]
    fn specializes_unit_argument_as_a_constant() {
        // f(*u, n) = n; main passes unit `{}` for the candidate `u`. The clone drops
        // `u` with nothing threaded and rebinds it to the unit constant.
        let f = Func {
            params: vec![candidate("u"), v("n").into()],
            resume: BlockName::from("r"),
            region: region(
                vec![],
                Tail::Jump(JumpTarget {
                    target: BlockName::from("r"),
                    params: vec![v("n")],
                }),
            ),
        };
        let main = Func {
            params: vec![],
            resume: BlockName::from("r"),
            region: region(
                vec![
                    (v("k"), Value::Pure(Data::Nat(1))),
                    (v("unit"), Value::Pure(Data::Tpl(vec![]))),
                ],
                direct("f", vec![v("unit"), v("k")]),
            ),
        };

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_func(FuncName::from("f"), f);

        specialize_calls(&mut module);

        // The call drops the unit argument entirely.
        match &func_named(&module, "main").unwrap().region.tail {
            Tail::Call(CallTarget::Direct { target, params, .. }) => {
                assert_eq!(target.as_str(), "f@spec__0_unit");
                assert_eq!(params, &vec![v("k")]);
            }
            other => panic!("expected direct call to clone, got {other:?}"),
        }

        // The clone takes only `n`, and rebinds `u` to the unit constant.
        let clone = func_named(&module, "f@spec__0_unit").expect("clone present");
        assert_eq!(clone.params, vec![v("n")]);
        assert!(matches!(
            &clone.region.values[0],
            (name, Value::Pure(Data::Tpl(fields))) if name == &v("u") && fields.is_empty()
        ));
    }

    #[test]
    fn leaves_unknown_argument_alone() {
        // main forwards its own abstract parameter `g`, not a known closure.
        let f = Func {
            params: vec![candidate("p")],
            resume: BlockName::from("r"),
            region: region(vec![], indirect("p", vec![])),
        };
        let main = Func {
            params: vec![candidate("g")],
            resume: BlockName::from("r"),
            region: region(vec![], direct("f", vec![v("g")])),
        };

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_func(FuncName::from("f"), f);

        specialize_calls(&mut module);

        assert!(func_named(&module, "f@spec__0_c").is_none());
        assert_eq!(module.funcs().len(), 2);
        assert!(matches!(
            &func_named(&module, "main").unwrap().region.tail,
            Tail::Call(CallTarget::Direct { target, .. }) if target.as_str() == "f",
        ));
    }

    #[test]
    fn specializes_recursive_self_call_through_the_clone() {
        // f(*p, n) calls itself with p; specializing on c must devirtualize the
        // recursion too — the clone's self-call retargets to the clone.
        let body = Block {
            params: vec![],
            region: region(vec![], direct("f", vec![v("p"), v("n")])),
        };
        let f = Func {
            params: vec![candidate("p"), v("n").into()],
            resume: BlockName::from("r"),
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![(BlockName::from("loop"), body)],
                tail: Tail::Jump(JumpTarget {
                    target: BlockName::from("loop"),
                    params: vec![],
                }),
            },
        };
        let c = Clsr {
            fields: vec![v("e").into()],
            params: vec![v("x").into()],
            resume: BlockName::from("r"),
            region: region(
                vec![],
                Tail::Jump(JumpTarget {
                    target: BlockName::from("r"),
                    params: vec![v("x")],
                }),
            ),
        };
        let main = Func {
            params: vec![],
            resume: BlockName::from("r"),
            region: region(
                vec![
                    (v("env"), Value::Pure(Data::Nat(0))),
                    (v("k"), Value::Pure(Data::Nat(1))),
                    (
                        v("clo"),
                        Value::Pure(Data::Clsr(ClsrName::from("c"), vec![v("env")])),
                    ),
                ],
                direct("f", vec![v("clo"), v("k")]),
            ),
        };

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_func(FuncName::from("f"), f);
        module.add_clsr(ClsrName::from("c"), c);

        specialize_calls(&mut module);

        // The clone's own recursive call targets the clone, with `p` expanded into
        // the threaded capture rather than passed as a closure.
        let clone = func_named(&module, "f@spec__0_c").expect("clone present");
        match &clone.region.blocks[0].1.region.tail {
            Tail::Call(CallTarget::Direct { target, params, .. }) => {
                assert_eq!(target.as_str(), "f@spec__0_c");
                assert_eq!(params, &vec![v("p@cap0"), v("n")]);
            }
            other => panic!("expected specialized self-call, got {other:?}"),
        }
    }

    #[test]
    fn specializes_closure_on_known_captured_closure_and_threads_nested_captures() {
        // outer{*p, q}(m) = p(q): dispatches on the captured combinator `p`. main
        // allocates it with a known closure `inner{env}` for that capture.
        let inner = Clsr {
            fields: vec![v("e").into()],
            params: vec![v("x").into()],
            resume: BlockName::from("r"),
            region: region(
                vec![],
                Tail::Jump(JumpTarget {
                    target: BlockName::from("r"),
                    params: vec![v("x")],
                }),
            ),
        };
        let outer = Clsr {
            fields: vec![candidate("p"), v("q").into()],
            params: vec![v("m").into()],
            resume: BlockName::from("r"),
            region: region(vec![], indirect("p", vec![v("q")])),
        };
        let main = Func {
            params: vec![],
            resume: BlockName::from("r"),
            region: region(
                vec![
                    (v("env"), Value::Pure(Data::Nat(7))),
                    (v("k"), Value::Pure(Data::Nat(1))),
                    (
                        v("pclo"),
                        Value::Pure(Data::Clsr(ClsrName::from("inner"), vec![v("env")])),
                    ),
                    (
                        v("oclo"),
                        Value::Pure(Data::Clsr(ClsrName::from("outer"), vec![v("pclo"), v("k")])),
                    ),
                ],
                Tail::Jump(JumpTarget {
                    target: BlockName::from("r"),
                    params: vec![v("oclo")],
                }),
            ),
        };

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_clsr(ClsrName::from("inner"), inner);
        module.add_clsr(ClsrName::from("outer"), outer);

        specialize_calls(&mut module);

        // The allocation is retargeted to the clone, with `pclo` expanded into its
        // captured `env`; `k` is kept verbatim.
        let oclo = &func_named(&module, "main").unwrap().region.values[3];
        assert!(matches!(
            oclo,
            (name, Value::Pure(Data::Clsr(c, caps)))
                if name == &v("oclo") && c.as_str() == "outer@spec__0_inner"
                    && caps == &vec![v("env"), v("k")]
        ));

        // The clone drops `p`, takes its threaded capture as a leading field, keeps
        // `q`, and rebuilds the closure so `p` is a known `Data::Clsr`.
        let clone = clsr_named(&module, "outer@spec__0_inner").expect("clone present");
        assert_eq!(clone.fields, vec![v("p@cap0"), v("q")]);
        assert_eq!(clone.params, vec![v("m")]);
        assert!(matches!(
            &clone.region.values[0],
            (name, Value::Pure(Data::Clsr(c, caps)))
                if name == &v("p") && c.as_str() == "inner" && caps == &vec![v("p@cap0")]
        ));
    }

    #[test]
    fn specializes_closure_on_unit_capture() {
        // outer{*u, q} captures a unit `{}` at a candidate position; the clone drops
        // it with nothing threaded and rebinds it to the unit constant.
        let outer = Clsr {
            fields: vec![candidate("u"), v("q").into()],
            params: vec![v("m").into()],
            resume: BlockName::from("r"),
            region: region(vec![], indirect("q", vec![v("m")])),
        };
        let main = Func {
            params: vec![],
            resume: BlockName::from("r"),
            region: region(
                vec![
                    (v("k"), Value::Pure(Data::Nat(1))),
                    (v("unit"), Value::Pure(Data::Tpl(vec![]))),
                    (
                        v("oclo"),
                        Value::Pure(Data::Clsr(ClsrName::from("outer"), vec![v("unit"), v("k")])),
                    ),
                ],
                Tail::Jump(JumpTarget {
                    target: BlockName::from("r"),
                    params: vec![v("oclo")],
                }),
            ),
        };

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_clsr(ClsrName::from("outer"), outer);

        specialize_calls(&mut module);

        // The allocation drops the unit capture entirely, keeping `k`.
        let oclo = &func_named(&module, "main").unwrap().region.values[2];
        assert!(matches!(
            oclo,
            (name, Value::Pure(Data::Clsr(c, caps)))
                if name == &v("oclo") && c.as_str() == "outer@spec__0_unit" && caps == &vec![v("k")]
        ));

        // The clone takes only `q`, and rebinds `u` to the unit constant.
        let clone = clsr_named(&module, "outer@spec__0_unit").expect("clone present");
        assert_eq!(clone.fields, vec![v("q")]);
        assert!(matches!(
            &clone.region.values[0],
            (name, Value::Pure(Data::Tpl(fields))) if name == &v("u") && fields.is_empty()
        ));
    }

    #[test]
    fn leaves_unknown_capture_alone() {
        // main allocates `outer` with its own abstract parameter `g`, not a known
        // closure, so there is nothing to bake.
        let outer = Clsr {
            fields: vec![candidate("p")],
            params: vec![],
            resume: BlockName::from("r"),
            region: region(vec![], indirect("p", vec![])),
        };
        let main = Func {
            params: vec![candidate("g")],
            resume: BlockName::from("r"),
            region: region(
                vec![(
                    v("oclo"),
                    Value::Pure(Data::Clsr(ClsrName::from("outer"), vec![v("g")])),
                )],
                Tail::Jump(JumpTarget {
                    target: BlockName::from("r"),
                    params: vec![v("oclo")],
                }),
            ),
        };

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_clsr(ClsrName::from("outer"), outer);

        specialize_calls(&mut module);

        assert_eq!(module.clsrs().len(), 1);
        assert!(matches!(
            &func_named(&module, "main").unwrap().region.values[0],
            (_, Value::Pure(Data::Clsr(c, _))) if c.as_str() == "outer",
        ));
    }

    #[test]
    fn terminates_on_self_capturing_closure() {
        // `let v = c{v}` — a recursive closure capturing itself at a candidate
        // position. The name-as-key memo builds one clone and converges.
        let c = Clsr {
            fields: vec![candidate("f")],
            params: vec![v("x").into()],
            resume: BlockName::from("r"),
            region: region(vec![], indirect("f", vec![v("x")])),
        };
        let main = Func {
            params: vec![],
            resume: BlockName::from("r"),
            region: region(
                vec![(
                    v("v"),
                    Value::Pure(Data::Clsr(ClsrName::from("c"), vec![v("v")])),
                )],
                Tail::Jump(JumpTarget {
                    target: BlockName::from("r"),
                    params: vec![v("v")],
                }),
            ),
        };

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_clsr(ClsrName::from("c"), c);

        specialize_calls(&mut module);

        // One clone, its self-capture threaded; the allocation retargets to it.
        let clone = clsr_named(&module, "c@spec__0_c").expect("clone present");
        assert_eq!(clone.fields, vec![v("f@cap0")]);
        assert!(matches!(
            &func_named(&module, "main").unwrap().region.values[0],
            (_, Value::Pure(Data::Clsr(name, _))) if name.as_str() == "c@spec__0_c",
        ));
    }

    #[test]
    fn skips_specialization_when_a_known_closures_captures_are_out_of_scope() {
        // A recursive closure `rec` is a `prealloc` shell in the root region plus a
        // fill, in one block, that captures *that block's local param* `v`. A
        // *sibling* block passes `rec` to a candidate parameter of `f`. `rec`'s name
        // is in scope there (via the shell), but its capture `v` is not — it is bound
        // in a different block. Threading `v` would emit an out-of-scope reference, so
        // the pass must leave that call unspecialized.
        let f = Func {
            params: vec![candidate("p")],
            resume: BlockName::from("r"),
            region: region(vec![], indirect("p", vec![])),
        };
        let c = Clsr {
            fields: vec![v("e").into()],
            params: vec![v("x").into()],
            resume: BlockName::from("r"),
            region: region(
                vec![],
                Tail::Jump(JumpTarget {
                    target: BlockName::from("r"),
                    params: vec![v("x")],
                }),
            ),
        };
        // The fill captures the block-local `v`; reachable from the root only here.
        let fill = Block {
            params: vec![v("v")],
            region: region(
                vec![(
                    v("rec"),
                    Value::Pure(Data::Clsr(ClsrName::from("c"), vec![v("v")])),
                )],
                Tail::Jump(JumpTarget {
                    target: BlockName::from("r"),
                    params: vec![v("rec")],
                }),
            ),
        };
        // A sibling of `fill`: `rec`'s name reaches here via the shell, but `v` does not.
        let used = Block {
            params: vec![],
            region: region(vec![], direct("f", vec![v("rec")])),
        };
        let main = Func {
            params: vec![],
            resume: BlockName::from("r"),
            region: Region {
                preallocs: vec![(v("rec"), Prealloc::Clsr(ClsrName::from("c")))],
                values: vec![(v("init"), Value::Pure(Data::Nat(0)))],
                blocks: vec![
                    (BlockName::from("fill"), fill),
                    (BlockName::from("used"), used),
                ],
                tail: Tail::Jump(JumpTarget {
                    target: BlockName::from("fill"),
                    params: vec![v("init")],
                }),
            },
        };

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_func(FuncName::from("f"), f);
        module.add_clsr(ClsrName::from("c"), c);

        specialize_calls(&mut module);

        // The sibling call is untouched: still targets `f`, still passes `rec` — no
        // out-of-scope `v` spliced in.
        let used_region = &func_named(&module, "main").unwrap().region.blocks[1]
            .1
            .region;
        match &used_region.tail {
            Tail::Call(CallTarget::Direct { target, params, .. }) => {
                assert_eq!(target.as_str(), "f");
                assert_eq!(params, &vec![v("rec")]);
            }
            other => panic!("expected an unspecialized call, got {other:?}"),
        }
        assert!(func_named(&module, "f@spec__0_c").is_none());
    }

    #[test]
    fn syncs_a_specialized_fills_prealloc_shell() {
        // A recursive closure is a `prealloc` shell plus a self-capturing fill that
        // names the same value. Specializing the fill must re-point the shell to the
        // same clone, or the reserved slot and the written value disagree on shape.
        let c = Clsr {
            fields: vec![candidate("f")],
            params: vec![v("x").into()],
            resume: BlockName::from("r"),
            region: region(vec![], indirect("f", vec![v("x")])),
        };
        let main = Func {
            params: vec![],
            resume: BlockName::from("r"),
            region: Region {
                preallocs: vec![(v("rec"), Prealloc::Clsr(ClsrName::from("c")))],
                values: vec![(
                    v("rec"),
                    Value::Pure(Data::Clsr(ClsrName::from("c"), vec![v("rec")])),
                )],
                blocks: vec![],
                tail: Tail::Jump(JumpTarget {
                    target: BlockName::from("r"),
                    params: vec![v("rec")],
                }),
            },
        };

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_clsr(ClsrName::from("c"), c);

        specialize_calls(&mut module);

        // Both the fill and its shell now name the clone.
        let region = &func_named(&module, "main").unwrap().region;
        assert!(matches!(
            &region.values[0],
            (_, Value::Pure(Data::Clsr(name, _))) if name.as_str() == "c@spec__0_c",
        ));
        assert!(matches!(
            &region.preallocs[0],
            (_, Prealloc::Clsr(name)) if name.as_str() == "c@spec__0_c",
        ));
    }
}
