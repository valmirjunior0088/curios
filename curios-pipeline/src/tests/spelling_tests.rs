//! A name's spelling held to resolution: every spelling a report gives a declaration of the library is resolved, by the lowering, to that declaration from where the reader stands, and no shorter one built from its paths is.
//!
//! `curios-text`'s `spell` is the reverse of resolution, written apart from the functions that resolve. This is what compares the two: the spellings are put back through the lowering's own doors, in one generated program per reader, and what each lowers to is read off the lowered module.

use {
    super::test_support::*,
    curios_core::{
        Enter, Global, IntrinsicForm, Item, Subterm, Term, intrinsic_form, intrinsic_global,
    },
    curios_prelude::with_prelude,
    curios_text::{RootSource, SYNTAX, into_core_with_prelude, spell},
    curios_unit::Predecessors,
    curios_utilities::{Qualifier, fill},
    std::{
        collections::{BTreeMap, BTreeSet},
        fmt::Write,
    },
};

/// Where a reader stands: under the `use` lines it wrote, at the root of a program or in a module nested in one.
struct Reader {
    imports: String,
    nested: bool,
}

impl Reader {
    fn at_the_root(imports: &str) -> Self {
        Self {
            imports: imports.to_string(),
            nested: false,
        }
    }

    fn nested(imports: &str) -> Self {
        Self {
            imports: imports.to_string(),
            nested: true,
        }
    }

    /// The module the reader's declarations are written in.
    fn island(&self) -> Qualifier {
        match self.nested {
            true => Qualifier::from(["Inner"]),
            false => Qualifier::empty(),
        }
    }

    /// The name the reader's declaration `label` is declared under.
    fn declared(&self, label: &str) -> Global {
        Global::Authored(self.island().with(label))
    }

    /// The reader's program over `declarations`, each a `let` of the reader's module.
    fn program(&self, declarations: &str) -> String {
        let imports = &self.imports;
        match self.nested {
            true => format!("mod Inner\n{imports}\n{declarations}end\n/std/print(\"\")\n"),
            false => format!("{imports}\n{declarations}/std/print(\"\")\n"),
        }
    }
}

/// What a differential over one reader counted: the declarations it spelled and put back, how many of them from a name in scope and not from the root, and the shorter candidates it showed to name something else.
#[derive(Debug, Default)]
struct Counted {
    spelled: usize,
    relative: usize,
    refuted: usize,
}

/// The declaration a lowered body names, where the body is a name and nothing else.
fn named(body: &Term) -> Option<Global> {
    match &**body {
        Subterm::Var(var) => var.as_free()?.as_global().copied(),
        _ => None,
    }
}

/// Hold every spelling `reader` is given to the lowering. With `under`, the declarations with no path under that module are left out: what a reader with one more import adds to the check is what stands under the module it imported.
fn held(reader: &Reader, under: Option<&Qualifier>) -> Counted {
    with_prelude(|prelude| {
        let text = Predecessors::over(prelude).text();
        let lower = |declarations: &str| {
            let source = reader.program(declarations);

            into_core_with_prelude(&entrypoint_of(&source), &RootSource::none(), &text, &SYNTAX)
                .unwrap_or_else(|error| panic!("the reader's program lowers: {}", error.format()))
        };

        // The reader's spellings, taken where one declaration stands under its imports: every later one is written under the same.
        let probe = "let probe: Type = Type;\n";
        let spellings = lower(probe).names.spellings(&text);
        let island = reader.island();
        let owner = reader.declared("probe");
        let paths = |global: &Global| {
            spellings
                .units
                .iter()
                .flat_map(|unit| unit.paths.get(global).into_iter().flatten())
                .map(|written| written.path)
                .collect::<Vec<_>>()
        };
        let spelled = spellings
            .units
            .iter()
            .flat_map(|unit| unit.paths.keys().copied())
            .collect::<BTreeSet<_>>()
            .into_iter()
            .filter(|global| {
                under.is_none_or(|module| paths(global).iter().any(|path| path.is_within(module)))
            })
            .filter_map(|global| Some((global, spell(&spellings, &island, Some(&owner), &global)?)))
            .collect::<Vec<_>>();

        // One program: each spelling as the body of a declaration, and beside it every shorter name a suffix of one of its paths spells.
        let mut declarations = String::from(probe);
        let mut shorter = Vec::new();
        for (index, (global, spelling)) in spelled.iter().enumerate() {
            writeln!(declarations, "let p{index}: Type = {spelling};").unwrap();

            let segments = spelling.trim_start_matches('/').split('/').count();
            let candidates = paths(global)
                .iter()
                .flat_map(|path| {
                    let path = path.segments();

                    (1..path.len()).map(move |from| path[from..].join("/"))
                })
                .filter(|candidate| candidate.split('/').count() < segments)
                .collect::<BTreeSet<_>>();
            for (place, candidate) in candidates.into_iter().enumerate() {
                let label = format!("c{index}_{place}");
                writeln!(declarations, "let {label}: Type = {candidate};").unwrap();
                shorter.push((label, candidate, *global));
            }
        }

        let lowered = lower(&declarations);
        let bodies = lowered
            .program
            .module
            .items
            .iter()
            .flat_map(Item::definitions)
            .map(|definition| (definition.name, named(&definition.body)))
            .collect::<BTreeMap<_, _>>();
        let body = |label: &str| bodies.get(&reader.declared(label)).copied().flatten();

        let strays = spelled
            .iter()
            .enumerate()
            .filter(|(index, (global, _))| body(&format!("p{index}")) != Some(*global))
            .map(|(_, (global, spelling))| format!("`{spelling}` for {}", global.faithful()))
            .collect::<Vec<_>>();
        assert!(
            strays.is_empty(),
            "{} of {} spellings do not resolve to what they spell, the first: {:#?}",
            strays.len(),
            spelled.len(),
            &strays[..strays.len().min(12)]
        );

        let shortcuts = shorter
            .iter()
            .filter(|(label, _, global)| body(label) == Some(*global))
            .map(|(_, candidate, global)| format!("`{candidate}` reaches {}", global.faithful()))
            .collect::<Vec<_>>();
        assert!(
            shortcuts.is_empty(),
            "{} shorter names resolve to what a longer spelling was given for, the first: {:#?}",
            shortcuts.len(),
            &shortcuts[..shortcuts.len().min(12)]
        );

        Counted {
            spelled: spelled.len(),
            relative: spelled
                .iter()
                .filter(|(_, spelling)| !spelling.starts_with('/'))
                .count(),
            refuted: shorter.len(),
        }
    })
}

/// The modules the library holds at its root, by name: every one a program can import alone.
fn library_modules() -> BTreeSet<String> {
    with_prelude(|prelude| {
        Predecessors::over(prelude)
            .text()
            .iter()
            .flat_map(|unit| {
                let spellings = unit.spellings(&[]);

                spellings
                    .units
                    .iter()
                    .flat_map(|unit| unit.paths.values().flatten())
                    .filter(|written| {
                        written
                            .audience
                            .iter()
                            .any(|root| root.segments().is_empty())
                    })
                    .map(|written| written.path)
                    .collect::<Vec<_>>()
            })
            .filter_map(|path| match path.segments() {
                [root, module, _, ..] if root == "std" => Some(module.clone()),
                _ => None,
            })
            .collect()
    })
}

// A module's own re-export binds from the `use` that makes it, as every `use` does: a declaration standing above that `use` is given the re-exported name through the module it is declared in, and one standing below it the name the `use` brought.
#[test]
fn a_re_export_of_the_readers_own_module_is_spelled_bare_only_below_its_use() {
    with_prelude(|prelude| {
        let text = Predecessors::over(prelude).text();
        let source = "mod M\n    pub mod Inner\n        pub let x: Type = Type;\n    end\n    let above: Type = Type;\n    pub use Inner/{x};\n    let below: Type = Type;\nend\n/std/print(\"\")\n";
        let lowered =
            into_core_with_prelude(&entrypoint_of(source), &RootSource::none(), &text, &SYNTAX)
                .unwrap_or_else(|error| panic!("the program lowers: {}", error.format()));
        let spellings = lowered.names.spellings(&text);
        let island = Qualifier::from(["M"]);
        let x = Global::Authored(Qualifier::from(["M", "Inner", "x"]));
        let at = |label: &str| {
            let owner = Global::Authored(island.with(label));

            spell(&spellings, &island, Some(&owner), &x)
        };

        assert_eq!(at("above").as_deref(), Some("Inner/x"));
        assert_eq!(at("below").as_deref(), Some("x"));
    });
}

/// Every declaration of `/sys` the intrinsic table writes a node by ([`intrinsic_form`]) that a reader at a program's root reaches by no name, read off the nodes `/sys`'s own declarations are built of, each as a report prints it and as it prints it keeping a concatenation's grouping. A program names `/sys` by no path, so it reaches one through what `/std` re-exports of it, and a report showing a node whose declaration it reaches by none declines its term.
fn intrinsics_unreached_from_the_root() -> Vec<String> {
    with_prelude(|prelude| {
        let system = prelude.first().expect("`/sys` is the prelude's first root");
        let mut written = BTreeSet::new();
        for item in &system.core().items {
            item.each_term(|term| {
                term.walk(
                    &mut written,
                    |written, term| {
                        if let Subterm::Intrinsic(node) = &**term {
                            for grouped in [false, true] {
                                match intrinsic_form(node.clone(), grouped) {
                                    IntrinsicForm::Named(path) => {
                                        written.insert(intrinsic_global(path));
                                    }
                                    IntrinsicForm::Call { path, .. } => {
                                        written.insert(intrinsic_global(&path));
                                    }
                                    _ => {}
                                }
                            }
                        }
                        Enter::Descend
                    },
                    |_, _, _| (),
                );
            });
        }

        let text = Predecessors::over(prelude).text();
        let reader = Reader::at_the_root("");
        let source = reader.program("let probe: Type = Type;\n");
        let lowered =
            into_core_with_prelude(&entrypoint_of(&source), &RootSource::none(), &text, &SYNTAX)
                .unwrap_or_else(|error| panic!("the reader's program lowers: {}", error.format()));
        let spellings = lowered.names.spellings(&text);
        let (island, owner) = (reader.island(), reader.declared("probe"));

        written
            .into_iter()
            .filter(|global| spell(&spellings, &island, Some(&owner), global).is_none())
            .map(|global| global.faithful().to_string())
            .collect()
    })
}

/// The declarations of `/sys` a report writes an intrinsic by and a reader at a program's root reaches by no name: `/std` calls each only inside a declaration of its own that does more than name it (`documentation/roadmap/tools/000-findings.md`), or, for `/sys/Bits` and `/sys/Bytes/to_bits`, not at all.
const UNREACHED_INTRINSICS: [&str; 55] = [
    "/sys/Bits/Bits",
    "/sys/Bits/and",
    "/sys/Bits/get",
    "/sys/Bits/len",
    "/sys/Bits/or",
    "/sys/Bits/replicate",
    "/sys/Bits/slice",
    "/sys/Bits/to_bytes",
    "/sys/Bits/xor",
    "/sys/Bytes/to_bits",
    "/sys/Channel/capacity",
    "/sys/Channel/close",
    "/sys/Channel/closed",
    "/sys/Channel/count",
    "/sys/Channel/new",
    "/sys/Channel/push",
    "/sys/Channel/take",
    "/sys/Flt/ties_to_away/add",
    "/sys/Flt/ties_to_away/div",
    "/sys/Flt/ties_to_away/fma",
    "/sys/Flt/ties_to_away/mul",
    "/sys/Flt/ties_to_away/of_int",
    "/sys/Flt/ties_to_away/of_nat",
    "/sys/Flt/ties_to_away/sqrt",
    "/sys/Flt/ties_to_away/sub",
    "/sys/Flt/toward_negative/add",
    "/sys/Flt/toward_negative/div",
    "/sys/Flt/toward_negative/fma",
    "/sys/Flt/toward_negative/mul",
    "/sys/Flt/toward_negative/of_int",
    "/sys/Flt/toward_negative/of_nat",
    "/sys/Flt/toward_negative/sqrt",
    "/sys/Flt/toward_negative/sub",
    "/sys/Flt/toward_positive/add",
    "/sys/Flt/toward_positive/div",
    "/sys/Flt/toward_positive/fma",
    "/sys/Flt/toward_positive/mul",
    "/sys/Flt/toward_positive/of_int",
    "/sys/Flt/toward_positive/of_nat",
    "/sys/Flt/toward_positive/sqrt",
    "/sys/Flt/toward_positive/sub",
    "/sys/Flt/toward_zero/add",
    "/sys/Flt/toward_zero/div",
    "/sys/Flt/toward_zero/fma",
    "/sys/Flt/toward_zero/mul",
    "/sys/Flt/toward_zero/of_int",
    "/sys/Flt/toward_zero/of_nat",
    "/sys/Flt/toward_zero/sqrt",
    "/sys/Flt/toward_zero/sub",
    "/sys/Handle/Handle",
    "/sys/Handle/stderr",
    "/sys/Handle/stdin",
    "/sys/Handle/stdout",
    "/sys/Io/advance",
    "/sys/Io/suspend",
];

#[test]
fn every_intrinsic_a_report_names_but_the_listed_is_written_from_the_root() {
    assert_eq!(intrinsics_unreached_from_the_root(), UNREACHED_INTRINSICS);
}

#[test]
fn a_reader_with_no_import_is_given_paths_that_resolve() {
    let counted = held(&Reader::at_the_root(""), None);

    // Counted, so a differential that spelled nothing does not pass for one that held.
    assert!(counted.spelled > 1000, "{counted:?}");
    assert!(counted.refuted > 1000, "{counted:?}");
}

#[test]
fn a_reader_under_a_glob_is_given_the_shortest_names_that_resolve() {
    let counted = held(&Reader::at_the_root("use /std/*;"), None);

    assert!(counted.spelled > 1000, "{counted:?}");
    assert!(counted.relative > 1000, "{counted:?}");
}

// One import at a time, for every module of the library: a name is spelled through the module from the label it was imported under, however far below it the declaration stands, and a path from the root that a shorter one through the label would replace is a shorter name that resolves.
#[test]
fn a_reader_importing_one_module_is_given_names_through_it() {
    let modules = library_modules();
    let mut through = 0;
    for module in &modules {
        let reader = Reader::at_the_root(&fill!("use /std/{${module}};", module));
        let imported = Qualifier::from(["std", module.as_str()]);

        through += held(&reader, Some(&imported)).relative;
    }

    assert!(modules.len() > 20, "{modules:?}");
    assert!(
        through > 1000,
        "{through} names through {} modules",
        modules.len()
    );
}

#[test]
fn a_reader_in_a_nested_module_is_given_names_that_resolve_there() {
    let reader = Reader::nested("    use /std/{Nat, Str, List, Option};");
    let counted = held(&reader, None);

    assert!(counted.spelled > 1000, "{counted:?}");
    assert!(counted.relative > 100, "{counted:?}");
}
