use {super::*, crate::*, curios_common::Qualifier, curios_num::Natural};

/// The lineage of the `function`th function erasure minted for the declaration at `/<declaration>`.
fn of(declaration: &str, function: u32) -> Lineage {
    Lineage {
        declaration: Qualifier::empty().with(declaration),
        function,
    }
}

/// A module holding one function per lineage and hint given.
fn module_of(functions: &[(Option<Lineage>, Option<&str>)]) -> (Module, Vec<FunctionId>) {
    let mut module = Module::new();
    let mut ids = Vec::new();

    for (lineage, name) in functions {
        let function = module.reserve_function();
        let return_cont = module.reserve_continuation();
        let result = module.add_value(None);
        let return_node = module.add_node(Node::ApplyCont(Edge {
            target: return_cont,
            args: vec![Atom::Value(result)],
        }));
        let body = module.add_node(Node::LetValue {
            result,
            value: ValueExpr::Literal(Literal::Nat(Natural::from(0u32))),
            next: return_node,
        });

        module.define_function(
            function,
            Function {
                lineage: *lineage,
                debug_name: name.map(str::to_string),
                params: vec![],
                return_cont,
                body,
                droppable: Droppable::Never,
                terminates: false,
            },
        );
        ids.push(function);
    }

    (module, ids)
}

fn counted(rows: &[(Counted, &str, usize)]) -> BTreeMap<Counted, (String, usize)> {
    rows.iter()
        .map(|(counted, name, count)| (counted.clone(), ((*name).to_string(), *count)))
        .collect()
}

#[test]
fn a_lineage_is_counted_once_per_function_of_it() {
    let (module, _) = module_of(&[
        (Some(of("main", 0)), Some("/main")),
        (Some(of("loop", 0)), Some("/loop")),
        (Some(of("loop", 0)), Some("/loop")),
    ]);

    assert_eq!(
        descendants(&module),
        counted(&[
            (Counted::Lineage(of("loop", 0)), "/loop", 2),
            (Counted::Lineage(of("main", 0)), "/main", 1),
        ])
    );
}

/// What the lineage replaced: two declarations' locals bear one hint, and a count by hint made them one row.
#[test]
fn two_declarations_locals_of_one_hint_are_two_rows() {
    let (module, _) = module_of(&[
        (Some(of("twice", 1)), Some("go")),
        (Some(of("thrice", 1)), Some("go")),
    ]);

    assert_eq!(
        descendants(&module),
        counted(&[
            (Counted::Lineage(of("thrice", 1)), "go", 1),
            (Counted::Lineage(of("twice", 1)), "go", 1),
        ])
    );
}

/// No row could tell an unnamed function apart, whether it was made of a declaration's text or of none.
#[test]
fn code_of_no_declaration_is_counted_by_its_hint_and_an_unnamed_function_not_at_all() {
    let (module, _) = module_of(&[
        (Some(of("main", 0)), Some("/main")),
        (None, Some("map@heap")),
        (Some(of("main", 1)), None),
        (None, None),
    ]);

    assert_eq!(
        descendants(&module),
        counted(&[
            (Counted::Lineage(of("main", 0)), "/main", 1),
            (Counted::Hint("map@heap".into()), "map@heap", 1),
        ])
    );
}

/// A twin runs its original on the heap: it is the same code twice over, and no copy a pass made.
#[test]
fn a_heap_twin_is_no_copy_of_its_original() {
    let (mut module, ids) = module_of(&[
        (Some(of("walk", 0)), Some("/walk")),
        (Some(of("walk", 0)), Some("/walk")),
    ]);
    module.record_twin(ids[0], ids[1]);

    assert_eq!(
        descendants(&module),
        counted(&[(Counted::Lineage(of("walk", 0)), "/walk", 1)])
    );
}

#[test]
fn a_row_that_kept_its_count_survived() {
    let main = [(Counted::Lineage(of("main", 0)), "/main", 1)];
    let fates = fates(&counted(&main), &counted(&main));

    assert_eq!(
        fates,
        vec![Fate {
            name: "/main".into(),
            outcome: Outcome::Survived,
        }]
    );
}

#[test]
fn a_row_that_gained_functions_was_specialized() {
    let fates = fates(
        &counted(&[(Counted::Lineage(of("step", 0)), "/step", 1)]),
        &counted(&[(Counted::Lineage(of("step", 0)), "/step", 3)]),
    );

    assert_eq!(
        fates,
        vec![Fate {
            name: "/step".into(),
            outcome: Outcome::Specialized { copies: 3 },
        }]
    );
}

#[test]
fn a_row_no_function_is_of_any_more_was_absorbed() {
    let fates = fates(
        &counted(&[(Counted::Lineage(of("helper", 0)), "/helper", 1)]),
        &counted(&[]),
    );

    assert_eq!(
        fates,
        vec![Fate {
            name: "/helper".into(),
            outcome: Outcome::Absorbed,
        }]
    );
}

/// The order is the point: a profile that reproduces cannot be handed a map's iteration order, and every consumer diffs one report against another.
#[test]
fn rows_are_ordered_by_name() {
    let before = counted(&[
        (Counted::Lineage(of("b", 0)), "/b", 1),
        (Counted::Lineage(of("a", 0)), "/a", 1),
        (Counted::Lineage(of("c", 0)), "/c", 1),
    ]);
    let fates = fates(&before, &before);
    let names = fates.iter().map(|fate| fate.name.as_str());

    assert!(names.eq(["/a", "/b", "/c"]));
}
