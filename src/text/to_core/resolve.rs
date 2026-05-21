use {super::{Context, ModuleInfo}, crate::text::{Name, TopUse}, std::collections::HashMap};

pub fn resolve_name(
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

pub fn resolve_use(top_use: &TopUse, context: &mut Context) {
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
