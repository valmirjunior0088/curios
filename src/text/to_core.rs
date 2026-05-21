mod context;
use context::*;

mod resolve;
use resolve::*;

mod term;
use term::*;

use {super::*, crate::core, std::collections::HashMap};

fn process_items(items: &[TopItem], context: &mut Context, flat: &mut Vec<FlatItem>) {
    let mut info = ModuleInfo {
        children: HashMap::new(),
        bindings: HashMap::new(),
    };
    for item in items {
        match item {
            TopItem::Mod(mod_item) => {
                context
                    .scope
                    .insert(mod_item.label.clone(), context.prefix.with(&mod_item.label));
                info.children.insert(mod_item.label.clone(), mod_item.is_pub);
                let mut child = context.nested(&mod_item.label);
                process_items(&mod_item.module.items, &mut child, flat);
            }
            TopItem::Use(use_item) => {
                resolve_use(use_item, context);
            }
            TopItem::Let(let_item) => {
                let name = context.prefix.with(&let_item.label);
                let type_ = elaborate_term(&let_item.type_, &context.scope, &*context.table);
                let body = elaborate_term(&let_item.body, &context.scope, &*context.table);
                info.bindings.insert(let_item.label.clone(), let_item.is_pub);
                flat.push(FlatItem::Let(FlatLet { name, type_, body }));
            }
            TopItem::Rec(ls) => {
                flat.push(FlatItem::Rec(
                    ls.iter()
                        .map(|let_item| {
                            let name = context.prefix.with(&let_item.label);
                            let type_ = elaborate_term(&let_item.type_, &context.scope, &*context.table);
                            let body = elaborate_term(&let_item.body, &context.scope, &*context.table);
                            info.bindings.insert(let_item.label.clone(), let_item.is_pub);
                            FlatLet { name, type_, body }
                        })
                        .collect(),
                ));
            }
        }
    }
    context.table.insert(context.prefix.clone(), info);
}

pub fn to_core(entrypoint: &Entrypoint) -> core::Term {
    for item in &entrypoint.items {
        match item {
            TopItem::Mod(mod_item) if mod_item.is_pub => panic!("pub on top-level entrypoint item"),
            TopItem::Let(let_item) if let_item.is_pub => panic!("pub on top-level entrypoint item"),
            TopItem::Rec(ls) => {
                for let_item in ls {
                    if let_item.is_pub {
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
        FlatItem::Let(let_) => core::Let::new(let_.name.path.join("/"), let_.type_, let_.body, acc).into(),
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
        super::to_core(&src.parse::<text::Entrypoint>().unwrap())
    }

    #[test]
    fn no_items_simple_tail() {
        assert_eq!(run("Type"), core::Term::Type);
    }

    #[test]
    fn single_let_binding() {
        assert_eq!(
            run(r#"
                let x : Type = Type;
                x
            "#),
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
        run(r#"
            pub let f : Type = Type;
            Type
        "#);
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
        run(r#"
            use /Nonexistent
            Type
        "#);
    }
}
