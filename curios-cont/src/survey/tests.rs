use curios_num::Natural;

use {
    super::*,
    crate::{CpsAtom, CpsEdge, CpsLiteral, CpsNode, CpsValueExpr},
};

/// A module holding one function per name given, plus one unnamed function — which is what a compiler-minted shell looks like to [`descendants`].
fn module_naming(names: &[Option<&str>]) -> CpsModule {
    let mut module = CpsModule::new();

    for name in names {
        let function = module.reserve_function();
        let return_cont = module.reserve_continuation();
        let result = module.add_value(None);
        let return_node = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: return_cont,
            args: vec![CpsAtom::Value(result)],
        }));
        let body = module.add_node(CpsNode::LetValue {
            result,
            value: CpsValueExpr::Literal(CpsLiteral::Nat(Natural::from(0u32))),
            next: return_node,
        });

        module.define_function(
            function,
            CpsFunction {
                debug_name: name.map(str::to_string),
                params: vec![],
                return_cont,
                body,
            },
        );
    }

    module
}

fn counted(pairs: &[(&str, usize)]) -> BTreeMap<String, usize> {
    pairs
        .iter()
        .map(|(name, count)| ((*name).to_string(), *count))
        .collect()
}

#[test]
fn a_name_is_counted_once_per_function_bearing_it() {
    let module = module_naming(&[Some("/main"), Some("/loop"), Some("/loop")]);

    assert_eq!(descendants(&module), counted(&[("/loop", 2), ("/main", 1)]));
}

#[test]
fn an_unnamed_function_descends_from_no_declaration() {
    let module = module_naming(&[Some("/main"), None]);

    assert_eq!(descendants(&module), counted(&[("/main", 1)]));
}

#[test]
fn a_name_that_kept_its_count_survived() {
    let fates = fates(&counted(&[("/main", 1)]), &counted(&[("/main", 1)]));

    assert_eq!(
        fates,
        vec![Fate {
            name: "/main".into(),
            outcome: Outcome::Survived,
        }]
    );
}

#[test]
fn a_name_that_gained_functions_was_specialized() {
    let fates = fates(&counted(&[("/step", 1)]), &counted(&[("/step", 3)]));

    assert_eq!(
        fates,
        vec![Fate {
            name: "/step".into(),
            outcome: Outcome::Specialized { copies: 3 },
        }]
    );
}

#[test]
fn a_name_no_function_bears_any_more_was_absorbed() {
    let fates = fates(&counted(&[("/helper", 1)]), &counted(&[]));

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
    let before = counted(&[("/b", 1), ("/a", 1), ("/c", 1)]);
    let fates = fates(&before, &before);
    let names = fates.iter().map(|fate| fate.name.as_str());

    assert!(names.eq(["/a", "/b", "/c"]));
}
