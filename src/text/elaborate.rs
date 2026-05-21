use {super::*, crate::core, std::collections::HashMap};

struct FlatLet {
    name: Name,
    type_: core::Term,
    body: core::Term,
}

enum FlatItem {
    Let(FlatLet),
    Rec(Vec<FlatLet>),
}

struct ModuleInfo {
    children: HashMap<String, bool>,
    bindings: HashMap<String, bool>,
}

struct Context<'a> {
    prefix: Name,
    table: &'a mut HashMap<Name, ModuleInfo>,
    scope: HashMap<String, Name>,
}

impl<'a> Context<'a> {
    fn new(table: &'a mut HashMap<Name, ModuleInfo>) -> Context<'a> {
        Context {
            prefix: Name::new(),
            table,
            scope: HashMap::new(),
        }
    }

    fn nested(&mut self, label: &str) -> Context<'_> {
        Context {
            prefix: self.prefix.with(label),
            table: &mut *self.table,
            scope: HashMap::new(),
        }
    }
}

fn elaborate_atom(a: &Atom) -> core::Atom {
    core::Atom::from(a.string.clone())
}

fn resolve_name(
    name: &Name,
    scope: &HashMap<String, Name>,
    table: &HashMap<Name, ModuleInfo>,
) -> Name {
    let qualifier = &name.path[0];
    let base = scope
        .get(qualifier)
        .unwrap_or_else(|| panic!("unresolved qualifier: {qualifier}"))
        .clone();
    let mut current_prefix = base.clone();
    for seg in &name.path[1..name.path.len() - 1] {
        let info = table
            .get(&current_prefix)
            .unwrap_or_else(|| panic!("module not found: {}", current_prefix.path.join("/")));
        let is_pub = info
            .children
            .get(seg)
            .unwrap_or_else(|| panic!("child module not found: {seg}"));
        if !is_pub {
            panic!("private child module: {seg}");
        }
        current_prefix = current_prefix.with(seg);
    }
    let last = name.path.last().unwrap();
    let info = table
        .get(&current_prefix)
        .unwrap_or_else(|| panic!("module not found: {}", current_prefix.path.join("/")));
    let is_pub = info
        .bindings
        .get(last)
        .unwrap_or_else(|| panic!("binding not found: {last}"));
    if !is_pub {
        panic!("private binding: {last}");
    }
    Name {
        path: base
            .path
            .iter()
            .cloned()
            .chain(name.path[1..].iter().cloned())
            .collect(),
    }
}

fn resolve_use(top_use: &TopUse, context: &mut Context) {
    if !top_use.is_abs && top_use.name.path.len() == 1 {
        let seg = &top_use.name.path[0];
        panic!("single-segment relative use is forbidden: {seg}");
    }

    let qualifier = top_use.name.path.last().unwrap().clone();

    let resolved_path = if top_use.is_abs {
        let segments = &top_use.name.path;
        let mut current = Name {
            path: vec![segments[0].clone()],
        };
        if !context.table.contains_key(&current) {
            panic!("module not found: {}", segments[0]);
        }
        for seg in &segments[1..] {
            let info = context
                .table
                .get(&current)
                .unwrap_or_else(|| panic!("module not found: {}", current.path.join("/")));
            let is_pub = info
                .children
                .get(seg)
                .unwrap_or_else(|| panic!("child module not found: {seg}"));
            if !is_pub {
                panic!("private child module: {seg}");
            }
            current = current.with(seg);
            if !context.table.contains_key(&current) {
                panic!("module not found: {}", current.path.join("/"));
            }
        }
        current
    } else {
        let first = &top_use.name.path[0];
        let mut current = context
            .scope
            .get(first)
            .unwrap_or_else(|| panic!("undeclared child in relative use: {first}"))
            .clone();
        for seg in &top_use.name.path[1..] {
            let info = context
                .table
                .get(&current)
                .unwrap_or_else(|| panic!("module not found: {}", current.path.join("/")));
            let is_pub = info
                .children
                .get(seg)
                .unwrap_or_else(|| panic!("child module not found: {seg}"));
            if !is_pub {
                panic!("private child module: {seg}");
            }
            current = current.with(seg);
        }
        if !context.table.contains_key(&current) {
            panic!("module not found: {}", current.path.join("/"));
        }
        current
    };

    if context.scope.contains_key(&qualifier) {
        panic!("use qualifier conflicts with existing scope entry: {qualifier}");
    }
    context.scope.insert(qualifier, resolved_path);
}

fn elaborate_term(
    term: &Term,
    scope: &HashMap<String, Name>,
    table: &HashMap<Name, ModuleInfo>,
) -> core::Term {
    match term {
        Term::Type => core::Term::Type,

        Term::Prim(p) => core::Term::Prim(elaborate_prim(p, scope, table)),

        Term::Name(v) => {
            if v.path.len() == 1 {
                core::Var::free(v.path[0].clone()).into()
            } else {
                let resolved = resolve_name(v, scope, table);
                core::Var::free(resolved.path.join("/")).into()
            }
        }

        Term::Atom(a) => core::Term::Atom(elaborate_atom(a)),

        Term::AtomType(at) => core::AtomType::new(at.atoms.iter().map(elaborate_atom)).into(),

        Term::FuncType(ft) => core::FuncType::new(
            ft.label.clone().unwrap_or_default(),
            elaborate_term(&ft.input, scope, table),
            elaborate_term(&ft.output, scope, table),
        )
        .into(),

        Term::Func(f) => {
            core::Func::new(f.label.clone(), elaborate_term(&f.body, scope, table)).into()
        }

        Term::Apply(ap) => core::Apply::new(
            elaborate_term(&ap.head, scope, table),
            elaborate_term(&ap.param, scope, table),
        )
        .into(),

        Term::TupleType(tt) => {
            let fields = tt.fields.iter().map(|(label, t)| {
                let label = label.clone().unwrap_or_default();
                (label, elaborate_term(t, scope, table))
            });
            core::TupleType::new(fields).into()
        }

        Term::Tuple(t) => {
            core::Tuple::new(t.fields.iter().map(|f| elaborate_term(f, scope, table))).into()
        }

        Term::NatFold(nm) => core::NatFold::new(
            elaborate_term(&nm.head, scope, table),
            nm.motive_label.clone(),
            elaborate_term(&nm.motive, scope, table),
            elaborate_term(&nm.zero_case, scope, table),
            nm.pred_label.clone(),
            nm.ih_label.clone(),
            elaborate_term(&nm.succ_case, scope, table),
        )
        .into(),

        Term::NatMatch(nm) => core::NatMatch::new(
            elaborate_term(&nm.head, scope, table),
            nm.motive_label.clone(),
            elaborate_term(&nm.motive, scope, table),
            nm.cases
                .iter()
                .map(|(&n, body)| (n, elaborate_term(body, scope, table))),
            elaborate_term(&nm.default, scope, table),
        )
        .into(),

        Term::Split(sp) => core::Split::new(
            elaborate_term(&sp.head, scope, table),
            sp.motive_label.clone(),
            elaborate_term(&sp.motive, scope, table),
            sp.field_labels.iter().cloned(),
            elaborate_term(&sp.tail, scope, table),
        )
        .into(),

        Term::Match(m) => {
            let cases = m.cases.iter().map(|(a, b)| {
                (
                    core::Atom {
                        string: a.string.clone(),
                    },
                    elaborate_term(b, scope, table),
                )
            });
            core::Match::new(
                elaborate_term(&m.head, scope, table),
                m.motive_label.clone(),
                elaborate_term(&m.motive, scope, table),
                cases,
            )
            .into()
        }

        Term::Let(l) => core::Let::new(
            l.label.clone(),
            elaborate_term(&l.type_, scope, table),
            elaborate_term(&l.body, scope, table),
            elaborate_term(&l.tail, scope, table),
        )
        .into(),

        Term::Rec(r) => {
            let items = r.items.iter().map(|it| {
                (
                    it.label.clone(),
                    elaborate_term(&it.type_, scope, table),
                    elaborate_term(&it.value, scope, table),
                )
            });
            core::Rec::new(items, elaborate_term(&r.tail, scope, table)).into()
        }
    }
}

fn elaborate_prim(
    p: &Prim,
    scope: &HashMap<String, Name>,
    table: &HashMap<Name, ModuleInfo>,
) -> core::Prim {
    match p {
        Prim::NatType => core::Prim::NatType,
        Prim::Nat(nat) => core::Prim::Nat(match nat {
            Nat::Number(n) => *n,
            Nat::Char(c) => *c as u32,
        }),
        Prim::NatEql(l, r) => core::Prim::nat_eql(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::NatNeq(l, r) => core::Prim::nat_neq(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::NatAdd(l, r) => core::Prim::nat_add(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::NatSub(l, r) => core::Prim::nat_sub(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::NatMul(l, r) => core::Prim::nat_mul(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::NatLt(l, r) => core::Prim::nat_lt(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::NatDiv(l, r) => core::Prim::nat_div(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::NatRem(l, r) => core::Prim::nat_rem(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::NatGt(l, r) => core::Prim::nat_gt(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::NatLte(l, r) => core::Prim::nat_lte(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::NatGte(l, r) => core::Prim::nat_gte(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::IntType => core::Prim::IntType,
        Prim::Int(n) => core::Prim::Int(*n),
        Prim::IntEql(l, r) => core::Prim::int_eql(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::IntNeq(l, r) => core::Prim::int_neq(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::IntAdd(l, r) => core::Prim::int_add(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::IntSub(l, r) => core::Prim::int_sub(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::IntMul(l, r) => core::Prim::int_mul(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::IntDiv(l, r) => core::Prim::int_div(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::IntRem(l, r) => core::Prim::int_rem(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::IntLt(l, r) => core::Prim::int_lt(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::IntGt(l, r) => core::Prim::int_gt(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::IntLte(l, r) => core::Prim::int_lte(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::IntGte(l, r) => core::Prim::int_gte(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::FltType => core::Prim::FltType,
        Prim::Flt(n) => core::Prim::Flt(core::Flt::from_f32(*n)),
        Prim::FltAdd(l, r) => core::Prim::flt_add(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::FltSub(l, r) => core::Prim::flt_sub(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::FltMul(l, r) => core::Prim::flt_mul(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::FltDiv(l, r) => core::Prim::flt_div(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::FltEql(l, r) => core::Prim::flt_eql(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::FltNeq(l, r) => core::Prim::flt_neq(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::FltLt(l, r) => core::Prim::flt_lt(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::FltGt(l, r) => core::Prim::flt_gt(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::FltLte(l, r) => core::Prim::flt_lte(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::FltGte(l, r) => core::Prim::flt_gte(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::FltMin(l, r) => core::Prim::flt_min(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::FltMax(l, r) => core::Prim::flt_max(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::FltNeg(t) => core::Prim::flt_neg(elaborate_term(t, scope, table)),
        Prim::FltAbs(t) => core::Prim::flt_abs(elaborate_term(t, scope, table)),
        Prim::FltSqrt(t) => core::Prim::flt_sqrt(elaborate_term(t, scope, table)),
        Prim::FltFloor(t) => core::Prim::flt_floor(elaborate_term(t, scope, table)),
        Prim::FltCeil(t) => core::Prim::flt_ceil(elaborate_term(t, scope, table)),
        Prim::FltTrunc(t) => core::Prim::flt_trunc(elaborate_term(t, scope, table)),
        Prim::FltNearest(t) => core::Prim::flt_nearest(elaborate_term(t, scope, table)),
        Prim::NatToInt(t) => core::Prim::nat_to_int(elaborate_term(t, scope, table)),
        Prim::NatToFlt(t) => core::Prim::nat_to_flt(elaborate_term(t, scope, table)),
        Prim::IntToNat(t) => core::Prim::int_to_nat(elaborate_term(t, scope, table)),
        Prim::IntToFlt(t) => core::Prim::int_to_flt(elaborate_term(t, scope, table)),
        Prim::FltToNat(t) => core::Prim::flt_to_nat(elaborate_term(t, scope, table)),
        Prim::FltToInt(t) => core::Prim::flt_to_int(elaborate_term(t, scope, table)),
        Prim::BinType => core::Prim::BinType,
        Prim::Bin(bin) => core::Prim::Bin(match bin {
            Bin::Bytes(bytes) => bytes.clone(),
            Bin::String(s) => s.as_bytes().to_vec(),
        }),
        Prim::BinLen(t) => core::Prim::bin_len(elaborate_term(t, scope, table)),
        Prim::BinEql(l, r) => core::Prim::bin_eql(
            elaborate_term(l, scope, table),
            elaborate_term(r, scope, table),
        ),
        Prim::BinGet(b, i) => core::Prim::bin_get(
            elaborate_term(b, scope, table),
            elaborate_term(i, scope, table),
        ),
        Prim::BinSlice(b, s, e) => core::Prim::bin_slice(
            elaborate_term(b, scope, table),
            elaborate_term(s, scope, table),
            elaborate_term(e, scope, table),
        ),
        Prim::BinAppend(b, byte) => core::Prim::bin_append(
            elaborate_term(b, scope, table),
            elaborate_term(byte, scope, table),
        ),
        Prim::BinConcat(vs) => {
            core::Prim::bin_concat(vs.iter().map(|t| elaborate_term(t, scope, table)))
        }
        Prim::ArrType(t) => core::Prim::arr_type(elaborate_term(t, scope, table)),
        Prim::Arr(vs) => core::Prim::Arr(
            vs.iter()
                .map(|t| elaborate_term(t, scope, table).into())
                .collect(),
        ),
        Prim::ArrLen(t) => core::Prim::arr_len(elaborate_term(t, scope, table)),
        Prim::ArrGet(l, i) => core::Prim::arr_get(
            elaborate_term(l, scope, table),
            elaborate_term(i, scope, table),
        ),
        Prim::ArrSlice(l, s, e) => core::Prim::arr_slice(
            elaborate_term(l, scope, table),
            elaborate_term(s, scope, table),
            elaborate_term(e, scope, table),
        ),
        Prim::ArrAppend(l, elem) => core::Prim::arr_append(
            elaborate_term(l, scope, table),
            elaborate_term(elem, scope, table),
        ),
        Prim::ArrConcat(vs) => {
            core::Prim::arr_concat(vs.iter().map(|t| elaborate_term(t, scope, table)))
        }
    }
}

fn process_items(items: &[TopItem], context: &mut Context, flat: &mut Vec<FlatItem>) {
    let mut info = ModuleInfo {
        children: HashMap::new(),
        bindings: HashMap::new(),
    };
    for item in items {
        match item {
            TopItem::Mod(m) => {
                context
                    .scope
                    .insert(m.label.clone(), context.prefix.with(&m.label));
                info.children.insert(m.label.clone(), m.is_pub);
                {
                    let mut child = context.nested(&m.label);
                    process_items(&m.module.items, &mut child, flat);
                }
            }
            TopItem::Use(u) => {
                resolve_use(u, context);
            }
            TopItem::Let(l) => {
                let name = context.prefix.with(&l.label);
                let type_ = elaborate_term(&l.type_, &context.scope, &*context.table);
                let body = elaborate_term(&l.body, &context.scope, &*context.table);
                info.bindings.insert(l.label.clone(), l.is_pub);
                flat.push(FlatItem::Let(FlatLet { name, type_, body }));
            }
            TopItem::Rec(ls) => {
                let flat_lets: Vec<FlatLet> = ls
                    .iter()
                    .map(|l| {
                        let name = context.prefix.with(&l.label);
                        let type_ = elaborate_term(&l.type_, &context.scope, &*context.table);
                        let body = elaborate_term(&l.body, &context.scope, &*context.table);
                        info.bindings.insert(l.label.clone(), l.is_pub);
                        FlatLet { name, type_, body }
                    })
                    .collect();
                flat.push(FlatItem::Rec(flat_lets));
            }
        }
    }
    context.table.insert(context.prefix.clone(), info);
}

pub fn elaborate(entrypoint: &Entrypoint) -> core::Term {
    for item in &entrypoint.items {
        match item {
            TopItem::Mod(m) if m.is_pub => panic!("pub on top-level entrypoint item"),
            TopItem::Let(l) if l.is_pub => panic!("pub on top-level entrypoint item"),
            TopItem::Rec(ls) => {
                for l in ls {
                    if l.is_pub {
                        panic!("pub on top-level entrypoint item");
                    }
                }
            }
            _ => {}
        }
    }

    let mut table: HashMap<Name, ModuleInfo> = HashMap::new();
    let mut context = Context::new(&mut table);
    let mut flat: Vec<FlatItem> = Vec::new();

    process_items(&entrypoint.items, &mut context, &mut flat);

    let base = elaborate_term(&entrypoint.tail, &context.scope, &*context.table);

    flat.into_iter().rev().fold(base, |acc, item| match item {
        FlatItem::Let(l) => core::Let::new(l.name.path.join("/"), l.type_, l.body, acc).into(),
        FlatItem::Rec(items) => core::Rec::new(
            items
                .into_iter()
                .map(|it| (it.name.path.join("/"), it.type_, it.body)),
            acc,
        )
        .into(),
    })
}

#[cfg(test)]
mod tests {
    use crate::{core, text};

    fn run(src: &str) -> core::Term {
        super::elaborate(&src.parse::<text::Entrypoint>().unwrap())
    }

    #[test]
    fn no_items_simple_tail() {
        assert_eq!(run("Type"), core::Term::Type);
    }

    #[test]
    fn single_let_binding() {
        assert_eq!(
            run("let x : Type = Type;\nx"),
            core::Let::new("x", core::Type, core::Type, core::Var::free("x")).into(),
        );
    }

    #[test]
    fn nested_module_binding_reference() {
        assert_eq!(
            run(r#"
                mod Foo
                    pub let f : Type = Type;
                end
                Foo/f
            "#),
            core::Let::new("Foo/f", core::Type, core::Type, core::Var::free("Foo/f")).into(),
        );
    }

    #[test]
    fn use_shorthand_resolves_qualifier() {
        assert_eq!(
            run(r#"
                mod Foo
                    pub mod Bar
                        pub let f : Type = Type;
                    end
                end
                use Foo/Bar
                Bar/f
            "#),
            core::Let::new(
                "Foo/Bar/f",
                core::Type,
                core::Type,
                core::Var::free("Foo/Bar/f")
            )
            .into(),
        );
    }

    #[test]
    #[should_panic(expected = "pub on top-level entrypoint item")]
    fn rejects_pub_at_entrypoint_root() {
        run("pub let f : Type = Type;\nType");
    }

    #[test]
    #[should_panic(expected = "single-segment relative use is forbidden")]
    fn rejects_single_segment_relative_use() {
        run(r#"
            mod Foo
                let x : Type = Type;
            end
            use Foo
            Type
        "#);
    }

    #[test]
    #[should_panic(expected = "unresolved qualifier")]
    fn rejects_unresolved_qualifier_in_term() {
        run("Foo/f");
    }

    #[test]
    #[should_panic(expected = "private binding")]
    fn rejects_private_binding_access() {
        run(r#"
            mod Foo
                let f : Type = Type;
            end
            Foo/f
        "#);
    }

    #[test]
    #[should_panic(expected = "private child module")]
    fn rejects_private_module_in_path() {
        run(r#"
            mod Foo
                mod Bar
                    pub let f : Type = Type;
                end
            end
            Foo/Bar/f
        "#);
    }

    #[test]
    #[should_panic(expected = "use qualifier conflicts")]
    fn rejects_conflicting_use_qualifiers() {
        run(r#"
            mod Foo
                pub mod Baz
                    pub let f : Type = Type;
                end
            end
            mod Bar
                pub mod Baz
                    pub let g : Type = Type;
                end
            end
            use Foo/Baz
            use Bar/Baz
            Type
        "#);
    }

    #[test]
    #[should_panic(expected = "child module not found")]
    fn rejects_use_of_nonexistent_child() {
        run(r#"
            mod Foo
            end
            use Foo/Nonexistent
            Type
        "#);
    }

    #[test]
    #[should_panic(expected = "module not found")]
    fn rejects_absolute_use_of_nonexistent_module() {
        run("use /Nonexistent\nType");
    }
}
