use curios_abi::{WireType, host_ops};
use curios_base::RootId;
use std::{
    fs,
    path::{Path, PathBuf},
    time::{Duration, SystemTime, UNIX_EPOCH},
};

const fn syn_name(segments: &'static [&'static str]) -> crate::SyntaxName {
    crate::SyntaxName::new(segments)
}

const SYNTAX: crate::SyntaxRegistry = crate::SyntaxRegistry::new(
    crate::MonadSyntax::new(syn_name(&["syn", "Monad", "bind"])),
    crate::CharacterSyntax::new(
        syn_name(&["syn", "Char", "Char"]),
        syn_name(&["syn", "Char", "Scalar", "below"]),
        syn_name(&["syn", "Char", "Scalar", "above"]),
    ),
    crate::StringSyntax::new(
        syn_name(&["syn", "Str", "Str"]),
        syn_name(&["syn", "Str", "Scan", "lead"]),
        syn_name(&["syn", "Str", "Utf8", "stop"]),
        syn_name(&["syn", "Str", "Utf8", "more"]),
        syn_name(&["syn", "Str", "step"]),
    ),
    crate::ProofSyntax::new(
        syn_name(&["syn", "True", "True", "qed"]),
        syn_name(&["syn", "False", "absurd"]),
    ),
);

fn syntax() -> &'static crate::SyntaxRegistry {
    &SYNTAX
}

/// A top-level definition's identity, from the path a test writes. Fixture-only
/// — production code carries the `Qualifier` from resolution instead of
/// recovering it from a spelling.
fn global(path: &str) -> curios_core::Free {
    curios_core::Free::global(curios_base::Qualifier::from(
        path.trim_start_matches('/').split('/'),
    ))
}

fn global_name(path: &str) -> curios_core::Global {
    curios_core::Global::Authored(curios_base::Qualifier::from(
        path.trim_start_matches('/').split('/'),
    ))
}

fn run(src: &str) -> curios_core::Term {
    let (module, _, _, _) = super::into_core(
        &src.parse::<crate::Entrypoint>().unwrap(),
        &crate::RootSource::none(),
        syntax(),
    )
    .unwrap();

    module.into_nested_term()
}

fn written_type(id: usize) -> curios_core::Term {
    curios_core::Term::type_at(curios_core::Level::meta(curios_core::UniverseMetaId(id)))
}

fn elaborate_source(src: &str) -> curios_core::Module {
    let (module, metavar_floor, universe_floor, _) = super::into_core(
        &src.parse::<crate::Entrypoint>().unwrap(),
        &crate::RootSource::none(),
        syntax(),
    )
    .unwrap();
    let mut context = curios_core::Context::new(Duration::from_secs(1));
    curios_core::elaborate_and_zonk_module(
        &mut context,
        &module,
        metavar_floor,
        universe_floor,
        curios_core::Mode::Infer,
    )
    .unwrap()
    .0
}

fn elaboration_paths(src: &str) -> (curios_core::Module, curios_core::Module) {
    let (lowered, metavar_floor, universe_floor, _) = super::into_core(
        &src.parse::<crate::Entrypoint>().unwrap(),
        &crate::RootSource::none(),
        syntax(),
    )
    .unwrap();
    assert!(lowered.items.len() >= 2);

    let mut lowered_prefix = lowered.clone();
    lowered_prefix.items.truncate(1);
    lowered_prefix.induct_decls.clear();
    lowered_prefix.struct_decls.clear();
    lowered_prefix.concepts.clear();
    lowered_prefix.witnesses.clear();
    lowered_prefix.type_ = None;
    lowered_prefix.body = curios_core::Term::prim(curios_core::Prim::Nat(curios_core::Nat::Zero));
    let prelude = curios_core::elaborate_and_zonk_module(
        &mut curios_core::Context::new(Duration::from_secs(1)),
        &lowered_prefix,
        metavar_floor,
        universe_floor,
        curios_core::Mode::Infer,
    )
    .unwrap()
    .0;

    let full = curios_core::elaborate_and_zonk_module(
        &mut curios_core::Context::new(Duration::from_secs(1)),
        &lowered,
        metavar_floor,
        universe_floor,
        curios_core::Mode::Infer,
    )
    .unwrap()
    .0;
    let cached = curios_core::elaborate_and_zonk_with_prelude(
        &mut curios_core::Context::new(Duration::from_secs(1)),
        &prelude,
        &lowered,
        metavar_floor,
        universe_floor,
        curios_core::Mode::Infer,
    )
    .unwrap()
    .0;
    (full, cached)
}

fn run_err(src: &str) -> String {
    super::into_core(
        &src.parse::<crate::Entrypoint>().unwrap(),
        &crate::RootSource::none(),
        syntax(),
    )
    .unwrap_err()
    .to_string()
}

// Lower against the real prelude (so `sys` and `std` are served and rooted),
// returning only success/error — the lens for the internal-root gate.
fn lower_with_prelude(src: &str) -> Result<(), String> {
    let mut modules = crate::PreludeModules::new();
    modules.insert_root("sys", RootId::Sys, crate::sys_module(&host_ops()));
    modules.insert_root(
        "std",
        RootId::Std,
        r#"
            pub mod Str
                pub let Valid : Type = Type;
            end
            pub mod Nat
                pub let Nat : Type = Type;
                pub let add : Type = Type;
            end
        "#
        .parse()
        .unwrap(),
    );
    let prepared = super::prepare_prelude(&modules, syntax()).map_err(|error| error.to_string())?;
    super::into_core_with_prelude(
        &src.parse::<crate::Entrypoint>().unwrap(),
        &crate::RootSource::none(),
        &prepared,
        syntax(),
    )
    .map(|_| ())
    .map_err(|error| error.to_string())
}

fn temp_dir(name: &str) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();
    std::env::temp_dir().join(format!("curios-{name}-{}-{millis}", std::process::id()))
}

fn write_module(base: &Path, path: &str, source: &str) {
    let path = base.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, source).unwrap();
}

#[test]
fn no_items_simple_tail() {
    assert_eq!(run("Type"), written_type(0));
}

#[test]
fn written_types_get_distinct_levels_and_lexical_roles() {
    let (module, _, universe_floor, _) = super::into_core(
        &"let id(@A : Type, x : A) -> A = x; Type"
            .parse::<crate::Entrypoint>()
            .unwrap(),
        &crate::RootSource::none(),
        syntax(),
    )
    .unwrap();

    assert_eq!(universe_floor, 2);
    assert_eq!(
        module
            .universe_seeds
            .iter()
            .map(|seed| seed.role)
            .collect::<Vec<_>>(),
        vec![
            curios_core::UniverseRole::Generalizable,
            curios_core::UniverseRole::Flexible,
        ],
    );
    assert!(
        module
            .universe_seeds
            .iter()
            .all(|seed| seed.origin.is_some())
    );
}

#[test]
fn cached_and_full_elaboration_have_identical_universe_transactions() {
    let (full, cached) = elaboration_paths(
        "let pre(@A : Type, x : A) -> A = x;\
         let user(@B : Type, x : B) -> B = pre(x);\
         user(Type)",
    );

    assert_eq!(cached, full);
}

#[test]
fn a_polymorphic_definition_instantiates_at_prop_and_type() {
    let module = elaborate_source("let id(@A : Type, x : A) -> A = x; (id(Prop), id(Type))");
    let definition = module
        .items
        .iter()
        .find_map(|item| match item {
            curios_core::Item::Let(definition) if definition.name.symbol() == "/id" => {
                Some(definition)
            }
            _ => None,
        })
        .unwrap();
    assert_eq!(definition.universe_context.parameter_count, 1);

    let curios_core::Subterm::Tuple(tuple) = &*module.body else {
        panic!("the entrypoint is a tuple");
    };
    let levels = tuple
        .fields
        .iter()
        .map(|field| {
            let curios_core::Subterm::Apply(apply) = &**field else {
                panic!("each tuple field is an id application");
            };
            let curios_core::Subterm::UniverseInst(instance) = &*apply.head else {
                panic!("each external id use is universe-instantiated");
            };
            instance.levels.clone()
        })
        .collect::<Vec<_>>();
    assert_eq!(
        levels,
        vec![
            vec![curios_core::Level::constant(1)],
            vec![curios_core::Level::constant(2)],
        ]
    );
}

#[test]
fn inductive_constructor_ownership_is_explicit() {
    let module = elaborate_source(
        r#"
        induct Result(A : Type, E : Type) : Type
        | success(A)
        | failure(E)
        end
        Type
        "#,
    );
    let schemes = module
        .items
        .iter()
        .flat_map(|item| match item {
            curios_core::Item::Let(definition) => vec![definition.clone()],
            curios_core::Item::Rec(rec) => rec.definitions(),
        })
        .map(|definition| {
            (
                definition.name,
                definition.kind,
                definition.universe_context.parameter_count,
            )
        })
        .collect::<Vec<_>>();

    assert_eq!(
        schemes,
        vec![
            (
                global_name("/Result"),
                curios_core::DefinitionKind::InductiveType,
                2,
            ),
            (
                global_name("/Result/success"),
                curios_core::DefinitionKind::InductiveConstructor {
                    owner: curios_base::Qualifier::from(["Result"]),
                    tag: curios_core::Atom::from("success"),
                },
                2,
            ),
            (
                global_name("/Result/failure"),
                curios_core::DefinitionKind::InductiveConstructor {
                    owner: curios_base::Qualifier::from(["Result"]),
                    tag: curios_core::Atom::from("failure"),
                },
                2,
            ),
        ],
    );
}

/// `id` is applied at two different levels in one block, which a local
/// universe scheme once served. Cumulativity carries it instead: `Prop : Type
/// 0` and `Type 0 : Type 1`, so a single monomorphic `A : Type 1` accepts
/// both, and the level order is linear so a sup always exists. The binding
/// therefore carries no scheme of its own — universe polymorphism belongs to
/// declarations, which are frozen into the prelude archive and re-instantiated
/// by later programs.
#[test]
fn cumulativity_admits_two_uses_of_a_monomorphic_local() {
    let module = elaborate_source(
        "let outer : {Type, Type} = let id : (@A : Type, A) -> A = (x) => x; (id(Prop), id(Type)); outer",
    );
    let definition = module
        .items
        .iter()
        .find_map(|item| match item {
            curios_core::Item::Let(definition) if definition.name.symbol() == "/outer" => {
                Some(definition)
            }
            _ => None,
        })
        .unwrap();
    let curios_core::Subterm::Let(let_) = &*definition.body else {
        panic!("outer contains the local let");
    };
    assert_eq!(let_.bindings.len(), 1);
}

/// The same, one indirection further: `alias` has no annotation at all, so its
/// type is inferred from `id` and then used at both levels.
#[test]
fn cumulativity_admits_two_uses_of_an_inferred_local_alias() {
    let module = elaborate_source(
        "let outer : {Type, Type} = let id : (@A : Type, A) -> A = (x) => x; let alias = id; (alias(Prop), alias(Type)); outer",
    );
    let definition = module
        .items
        .iter()
        .find_map(|item| match item {
            curios_core::Item::Let(definition) if definition.name.symbol() == "/outer" => {
                Some(definition)
            }
            _ => None,
        })
        .unwrap();
    let curios_core::Subterm::Let(let_) = &*definition.body else {
        panic!("outer contains the local lets");
    };
    assert_eq!(let_.bindings.len(), 2);
}

fn universe_parameters(module: &curios_core::Module, name: &str) -> usize {
    module
        .items
        .iter()
        .find_map(|item| match item {
            curios_core::Item::Let(definition) if definition.name.symbol() == name => {
                Some(definition.universe_context.parameter_count)
            }
            // An inductive and its constructors are one recursive group, so a
            // lookup restricted to `Let` would miss every one of them.
            curios_core::Item::Rec(rec) => rec
                .definitions()
                .iter()
                .find(|definition| definition.name.symbol() == name)
                .map(|definition| definition.universe_context.parameter_count),
            _ => None,
        })
        .unwrap_or_else(|| panic!("{name} is declared"))
}

/// A level a caller supplies stays a parameter: `@A : Type` puts the level in
/// an argument position, so each occurrence chooses it.
#[test]
fn a_level_in_argument_position_stays_a_parameter() {
    let module = elaborate_source("pub let pick(@A : Type, x : A) -> A = x; pick");
    assert_eq!(universe_parameters(&module, "/pick"), 1);
}

/// A level occurring *only* in the result is determined, not chosen: no
/// occurrence of `Holds` can supply it, so generalizing would mint a parameter
/// every use site has to instantiate for nothing. Minimizing it instead is what
/// keeps a literal's per-byte constructor applications from each minting fresh
/// levels — see `result_sort_only_metas`.
#[test]
fn a_level_only_in_the_result_is_minimized_away() {
    let module = elaborate_source(
        "pub induct Unit : pub Type | only() end
         pub let Holds(x : Unit) -> Type = Unit;
         Holds",
    );
    assert_eq!(universe_parameters(&module, "/Unit"), 0);
    assert_eq!(universe_parameters(&module, "/Holds"), 0);
}

/// A generated method wrapper belongs to *its concept's* universe context, not
/// to one generalized from its own signature. The wrapper's type names only the
/// levels its own field needs, yet it also carries `use w : C(…)` applied at all
/// of the concept's; a level outside the wrapper's own generalized set would
/// then have nothing to denote it.
#[test]
fn a_concept_method_wrapper_shares_its_concept_universe_context() {
    let module = elaborate_source("pub concept C(A : Type) : pub Type { f(A) -> A, } C");
    assert_eq!(universe_parameters(&module, "/C"), 1);
    assert_eq!(universe_parameters(&module, "/C/f"), 1);
}

/// The same rule where the concept's levels genuinely exceed any one wrapper's:
/// `pure` names a strict subset of `M`'s and `bind` a different subset, so
/// generalizing either alone comes out short.
#[test]
fn every_wrapper_of_a_higher_kinded_concept_shares_one_universe_context() {
    let module = elaborate_source(
        "pub concept M(F : (Type) -> Type) : pub Type {
             pure(@A : Type, value : A) -> F(A),
             bind(@A : Type, @B : Type, action : F(A), next : (A) -> F(B)) -> F(B),
         } M",
    );
    // Five, against `pure`'s own two and `bind`'s one: the point of the test is
    // lost if the concept ever stops outrunning its wrappers.
    assert_eq!(universe_parameters(&module, "/M"), 5);
    assert_eq!(universe_parameters(&module, "/M/pure"), 5);
    assert_eq!(universe_parameters(&module, "/M/bind"), 5);
}

#[test]
fn single_let_binding() {
    assert_eq!(
        run(r#"
            let x : Type = Type;
            x
        "#),
        curios_core::Term::let_(
            &global("/x"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/x")))
        ),
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
        curios_core::Term::let_(
            &global("/Foo/f"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/Foo/f")))
        ),
    );
}

#[test]
fn module_named_after_type_resolves_by_qualified_path() {
    assert_eq!(
        run(r#"
            mod Nat
                pub let double : Type = Type;
            end
            Nat/double
        "#),
        curios_core::Term::let_(
            &global("/Nat/double"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/Nat/double")))
        ),
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
            use Foo/{Bar};
            Bar/f
        "#),
        curios_core::Term::let_(
            &global("/Foo/Bar/f"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/Foo/Bar/f")))
        ),
    );
}

#[test]
fn allows_pub_on_root_items() {
    run(r#"
        pub mod Foo
            pub let f : Type = Type;
        end
        pub let g : Type = Type;
        Type
    "#);
}

#[test]
fn rejects_unresolved_qualifier_in_term() {
    assert!(run_err("Foo/f").contains("unresolved qualifier"));
}

#[test]
fn rejects_private_binding_access() {
    assert!(
        run_err(
            r#"
        mod Foo
            let f : Type = Type;
        end
        Foo/f
    "#
        )
        .contains("private binding")
    );
}

#[test]
fn rejects_private_module_in_path() {
    assert!(
        run_err(
            r#"
        mod Foo
            mod Bar
                pub let f : Type = Type;
            end
        end
        Foo/Bar/f
    "#
        )
        .contains("private child module")
    );
}

#[test]
fn rejects_conflicting_use_qualifiers() {
    assert!(
        run_err(
            r#"
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
        use Foo/{Baz};
        use Bar/{Baz};
        Type
    "#
        )
        .contains("qualifier conflicts with existing scope entry")
    );
}

#[test]
fn rejects_use_of_nonexistent_child() {
    assert!(
        run_err(
            r#"
        mod Foo
        end
        use Foo/{Nonexistent};
        Type
    "#
        )
        .contains("no module or binding named Nonexistent")
    );
}

#[test]
fn rejects_absolute_use_of_nonexistent_module() {
    assert!(
        run_err(
            r#"
        use /{Nonexistent};
        Type
    "#
        )
        .contains("no module or binding named Nonexistent")
    );
}

#[test]
fn pub_use_exposes_qualifier() {
    assert_eq!(
        run(r#"
            pub mod Foo
                pub mod Bar
                    pub let f : Type = Type;
                end
            end
            pub mod MyMod
                pub use /Foo/{Bar};
            end
            MyMod/Bar/f
        "#),
        curios_core::Term::let_(
            &global("/Foo/Bar/f"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/Foo/Bar/f")))
        ),
    );
}

#[test]
fn rejects_mod_that_overwrites_prior_use() {
    assert!(
        run_err(
            r#"
        mod Foo
            pub mod Bar
                pub let f : Type = Type;
            end
        end
        use Foo/{Bar};
        mod Bar
        end
        Type
    "#
        )
        .contains("qualifier conflicts with existing scope entry: Bar")
    );
}

#[test]
fn use_of_pub_use_path_resolves_through_alias() {
    assert_eq!(
        run(r#"
            pub mod Foo
                pub mod Bar
                    pub let f : Type = Type;
                end
            end
            pub mod MyMod
                pub use /Foo/{Bar};
            end
            use /MyMod/{Bar};
            Bar/f
        "#),
        curios_core::Term::let_(
            &global("/Foo/Bar/f"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/Foo/Bar/f")))
        ),
    );
}

#[test]
fn chained_pub_use_re_exports_transitively() {
    assert_eq!(
        run(r#"
            pub mod A
                pub mod X
                    pub let f : Type = Type;
                end
            end
            pub mod B
                pub use /A/{X};
            end
            pub mod C
                pub use /B/{X};
            end
            C/X/f
        "#),
        curios_core::Term::let_(
            &global("/A/X/f"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/A/X/f")))
        ),
    );
}

// --- Module-interface redesign (SPEC.md) acceptance cases ---

// A re-exports x from B; B re-exports x from C; C declares x. A is declared
// before its providers. The phase-3 fixed point must resolve A/x to /C/x
// regardless of declaration order.
#[test]
fn chained_re_export_resolves_out_of_order() {
    assert_eq!(
        run(r#"
            pub mod A
                pub use /B/{x};
            end
            pub mod B
                pub use /C/{x};
            end
            pub mod C
                pub let x : Type = Type;
            end
            A/x
        "#),
        curios_core::Term::let_(
            &global("/C/x"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/C/x")))
        ),
    );
}

// A direct `pub let x` and a `pub use` that also yields x are two distinct
// sources for the same export slot: a conflict, even though one of them is the
// module's own declaration.
#[test]
fn rejects_direct_and_re_export_of_same_label() {
    assert!(
        run_err(
            r#"
        pub mod B
            pub let x : Type = Type;
        end
        pub mod A
            pub let x : Type = Type;
            pub use /B/{x};
        end
        Type
    "#
        )
        .contains("export conflict")
    );
}

// Two globs each exposing x are two sources for the same slot → conflict.
#[test]
fn rejects_two_globs_exposing_same_label() {
    assert!(
        run_err(
            r#"
        pub mod B
            pub let x : Type = Type;
        end
        pub mod C
            pub let x : Type = Type;
        end
        pub mod A
            pub use /B/*;
            pub use /C/*;
        end
        Type
    "#
        )
        .contains("export conflict")
    );
}

// A re-exports module M from B; a later path /A/M/x must traverse A's
// re-exported M into /B/M and resolve x there.
#[test]
fn deep_facade_traversal_through_re_exported_module() {
    assert_eq!(
        run(r#"
            pub mod B
                pub mod M
                    pub let x : Type = Type;
                end
            end
            pub mod A
                pub use /B/{M};
            end
            use /A/M/{x};
            x
        "#),
        curios_core::Term::let_(
            &global("/B/M/x"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/B/M/x")))
        ),
    );
}

// A module may re-export names out of its own private child via a relative path:
// being inside the module, its privacy does not apply to itself. This is the
// facade-over-private-impl pattern.
#[test]
fn re_exports_from_own_private_child() {
    assert_eq!(
        run(r#"
            pub mod Facade
                mod Impl
                    pub let helper : Type = Type;
                end
                pub use Impl/{helper};
            end
            use /Facade/{helper};
            helper
        "#),
        curios_core::Term::let_(
            &global("/Facade/Impl/helper"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/Facade/Impl/helper")))
        ),
    );
}

// The relaxation is scoped to a module's *own* child: re-exporting through
// another module's private child is still forbidden.
#[test]
fn rejects_re_export_through_other_modules_private_child() {
    assert!(
        run_err(
            r#"
        pub mod Other
            mod Impl
                pub let helper : Type = Type;
            end
        end
        pub mod Facade
            pub use /Other/Impl/{helper};
        end
        Type
    "#
        )
        .contains("private child module")
    );
}

// An inductive's constructor module is a first-class interface member built in phase
// 2, so its cases re-export by name and by glob through the fixed point.
#[test]
fn re_exports_inductive_constructor_by_name() {
    let term = run(r#"
        pub mod Foo
             pub induct U : pub Type
            | A()
            | B()
            end
        end
        pub mod Bar
            pub use /Foo/U/{A};
        end
        use /Bar/{A};
        A
    "#);

    // Asserted on the printed term, not on `Debug`: a qualifier's `Debug`
    // shows its segments, deliberately, so that `/Foo/bar` and a single
    // segment spelled `Foo/bar` never look alike in a dump.
    assert!(
        format!("{term}").contains("Foo/U/A"),
        "unexpected term: {term}"
    );
}

#[test]
fn re_exports_inductive_constructors_by_glob() {
    let term = run(r#"
        pub mod Foo
             pub induct U : pub Type
            | A()
            | B()
            end
        end
        pub mod Bar
            pub use /Foo/U/*;
        end
        use /Bar/{A};
        A
    "#);

    // Asserted on the printed term, not on `Debug`: a qualifier's `Debug`
    // shows its segments, deliberately, so that `/Foo/bar` and a single
    // segment spelled `Foo/bar` never look alike in a dump.
    assert!(
        format!("{term}").contains("Foo/U/A"),
        "unexpected term: {term}"
    );
}

#[test]
fn opaque_inductive_constructor_namespace_cannot_be_re_exported() {
    let error = run_err(
        r#"
        pub mod Foo
            pub induct U : Type
            | A()
            end
        end
        pub mod Bar
            pub use /Foo/U/*;
        end
        Type
    "#,
    );

    assert!(
        error.contains("private child module"),
        "unexpected error: {error}"
    );
}

#[test]
fn rejects_named_re_export_from_own_opaque_constructor_namespace() {
    let error = run_err(
        r#"
        pub mod Foo
            pub induct U : Type
            | A()
            end
            pub use U/{A};
        end
        Type
    "#,
    );

    assert!(
        error.contains("constructors of opaque inductive '/Foo/U' cannot be re-exported")
            && error.contains("mark its representation public"),
        "unexpected error: {error}"
    );
}

#[test]
fn rejects_glob_re_export_from_own_opaque_constructor_namespace() {
    let error = run_err(
        r#"
        pub mod Foo
            pub induct U : Type
            | A()
            end
            pub use U/*;
        end
        Type
    "#,
    );

    assert!(
        error.contains("constructors of opaque inductive '/Foo/U' cannot be re-exported")
            && error.contains("mark its representation public"),
        "unexpected error: {error}"
    );
}

#[test]
fn declaring_module_can_use_own_opaque_constructors_lexically() {
    run(r#"
        pub mod Foo
            pub induct U : Type
            | A()
            end
            use U/{A};
            pub let make : U = A();
        end
        Type
    "#);
}

#[test]
fn private_inductive_with_public_representation_can_export_constructor_facade() {
    let term = run(r#"
        pub mod Foo
            induct U : pub Type
            | A()
            end
            pub use U/{A};
        end
        use /Foo/{A};
        A
    "#);

    // Asserted on the printed term, not on `Debug`: a qualifier's `Debug`
    // shows its segments, deliberately, so that `/Foo/bar` and a single
    // segment spelled `Foo/bar` never look alike in a dump.
    assert!(
        format!("{term}").contains("Foo/U/A"),
        "unexpected term: {term}"
    );
}

#[test]
fn unexposed_public_representation_may_use_private_helpers() {
    run(r#"
        mod M
            struct Hidden : Type { Type }
            struct Open : pub Type { Hidden }
        end
        Type
    "#);
}

#[test]
fn transparent_alias_exposure_audits_the_complete_representation() {
    let error = run_err(
        r#"
        mod M
            struct Hidden : Type { Type }
            struct Open : pub Type { Hidden }
            pub let Alias : Type = Open;
        end
        Type
    "#,
    );

    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

#[test]
fn parameterized_transparent_alias_preserves_representation_provenance() {
    let error = run_err(
        r#"
        mod M
            struct Hidden : Type { Type }
            struct Open(A : Type) : pub Type { hidden : Hidden, value : A }
            pub let Alias(A : Type) -> Type = Open(A);
        end
        Type
    "#,
    );

    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

#[test]
fn specialized_direct_type_alias_preserves_representation_provenance() {
    let error = run_err(
        r#"
        mod M
            struct Hidden : Type { Type }
            struct Open(A : Type) : pub Type { hidden : Hidden, value : A }
            pub let Alias : Type = Open(Type);
        end
        Type
    "#,
    );

    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

#[test]
fn reordered_direct_type_alias_preserves_representation_provenance() {
    let error = run_err(
        r#"
        mod M
            struct Hidden : Type { Type }
            struct Open(A : Type, B : Type) : pub Type { hidden : Hidden, a : A, b : B }
            pub let Alias(A : Type, B : Type) -> Type = Open(B, A);
        end
        Type
    "#,
    );

    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

#[test]
fn chained_direct_type_aliases_preserve_representation_provenance() {
    let error = run_err(
        r#"
        mod M
            struct Hidden : Type { Type }
            struct Open(A : Type) : pub Type { hidden : Hidden, value : A }
            let Middle(A : Type) -> Type = Open(A);
            pub let Alias(A : Type) -> Type = Middle(A);
        end
        Type
    "#,
    );

    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

#[test]
fn constant_direct_type_family_alias_preserves_representation_provenance() {
    let error = run_err(
        r#"
        mod M
            struct Hidden : Type { Type }
            struct Open : pub Type { hidden : Hidden }
            pub let Alias(_A : Type) -> Type = Open;
        end
        Type
    "#,
    );

    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

#[test]
fn direct_type_alias_audits_private_body_dependencies() {
    let error = run_err(
        r#"
        mod M
            struct Hidden : Type { Type }
            struct Open(A : Type) : pub Type { value : A }
            pub let Alias : Type = Open(Hidden);
        end
        Type
    "#,
    );

    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

#[test]
fn direct_type_alias_accepts_separately_exposed_body_dependencies() {
    run(r#"
        mod M
            struct Hidden : Type { Type }
            pub let PublicHidden : Type = Hidden;
            struct Open(A : Type) : pub Type { value : A }
            pub let Alias : Type = Open(Hidden);
        end
        Type
    "#);
}

#[test]
fn direct_type_alias_does_not_expose_an_opaque_nominals_fields() {
    run(r#"
        mod M
            struct Hidden : Type { Type }
            struct Opaque(A : Type) : Type { hidden : Hidden, value : A }
            pub let Alias : Type = Opaque(Type);
        end
        Type
    "#);
}

#[test]
fn local_head_is_not_a_direct_type_alias() {
    run(r#"
        mod M
            struct Hidden : Type { Type }
            struct Open(A : Type) : pub Type { value : A }
            pub let Alias(F : (Type) -> Type) -> Type = F(Hidden);
        end
        Type
    "#);
}

#[test]
fn computed_heads_are_not_direct_type_aliases() {
    run(r#"
        mod M
            struct Hidden : Type { Type }
            struct Open(A : Type) : pub Type { value : A }
            pub let LetHead : Type =
                let F : (Type) -> Type = Open;
                F(Hidden);
            pub let BetaHead : Type = ((A : Type) => Open(A))(Hidden);
            pub let ProjectionHead : Type = (Open, Open).0(Hidden);
            pub let MatchHead : Type =
                match true
                | true => Open(Hidden)
                | false => Open(Hidden)
                end;
        end
        Type
    "#);
}

#[test]
fn aliased_universe_annotation_is_not_a_direct_type_alias() {
    run(r#"
        mod M
            pub let Universe : Type = Type;
            struct Hidden : Type { Type }
            struct Open(A : Type) : pub Type { value : A }
            pub let Alias : Universe = Open(Hidden);
        end
        Type
    "#);
}

#[test]
fn cyclic_direct_type_aliases_terminate_without_nominal_exposure() {
    run(r#"
        mod M
            pub rec A(T : Type) -> Type = B(T)
            and B(T : Type) -> Type = A(T);
        end
        Type
    "#);
}

#[test]
fn non_type_identity_alias_still_preserves_representation_provenance() {
    let error = run_err(
        r#"
        mod M
            struct Hidden : Type { Type }
            struct Open : pub Type { hidden : Hidden }
            pub let Alias : {} = Open;
        end
        Type
    "#,
    );

    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

#[test]
fn standard_library_direct_type_aliases_pass_exposure_audit() {
    lower_with_prelude("use /std/Str/{Valid}; Valid").unwrap();
}

#[test]
fn exposure_audit_accepts_a_separately_exposed_dependency() {
    run(r#"
        mod M
            struct Hidden : Type { Type }
            pub let PublicHidden : Type = Hidden;
            struct Open : pub Type { Hidden }
            pub let PublicOpen : Type = Open;
        end
        Type
    "#);
}

#[test]
fn direct_representation_exposure_accepts_a_separately_exposed_dependency() {
    run(r#"
        mod M
            struct Hidden : Type { Type }
            pub let PublicHidden : Type = Hidden;
            pub struct Open : pub Type { Hidden }
        end
        Type
    "#);
}

// A re-exports x from B; B re-exports x from A; nobody declares x. Following the
// chain returns to the start without a concrete target → cyclic, not missing.
#[test]
fn rejects_cyclic_re_export_with_no_concrete_target() {
    assert!(
        run_err(
            r#"
        pub mod A
            pub use /B/{x};
        end
        pub mod B
            pub use /A/{x};
        end
        Type
    "#
        )
        .contains("cyclic re-export")
    );
}

// Two public declarations of the same label in the same namespace conflict at
// phase 2, before any elaboration.
#[test]
fn rejects_duplicate_public_declaration() {
    assert!(
        run_err(
            r#"
        pub let x : Type = Type;
        pub let x : Type = Type;
        Type
    "#
        )
        .contains("duplicate public declaration")
    );
}

// Phase 5: A.f references B.g and B.h references A.e, with e and g independent —
// no cycle, but no contiguous source order binds both references. The reorder
// must produce a valid binding order, leaving the lowered term with no free name.
#[test]
fn orders_acyclic_bidirectional_value_graph() {
    assert!(
        run(r#"
            pub mod A
                pub let e : Type = Type;
                pub let f : Type = /B/g;
            end
            pub mod B
                pub let g : Type = Type;
                pub let h : Type = /A/e;
            end
            Type
        "#)
        .free_vars()
        .is_empty()
    );
}

// A dependency through a type annotation is as much a binding-order constraint as
// one through a value: `f : T` declared before `T` must still order `T` first.
#[test]
fn orders_dependency_through_type_annotation() {
    assert!(
        run(r#"
            let f : T = Type;
            let T : Type = Type;
            f
        "#)
        .free_vars()
        .is_empty()
    );
}

// A genuine non-atomic value cycle cannot be ordered; phase 5 emits it anyway and
// leaves one reference as a free name, which core rejects as unbound. There is
// nothing to repair — cross-declaration value recursion is unexpressible.
#[test]
fn genuine_value_cycle_leaves_unbound_name() {
    assert!(
        !run(r#"
            pub mod A
                pub let f : Type = /B/g;
            end
            pub mod B
                pub let g : Type = /A/f;
            end
            Type
        "#)
        .free_vars()
        .is_empty()
    );
}

// `sys` is the trusted primitive substrate, reachable only from the standard
// library. A user entrypoint that names it — through a `use` or a bare term
// reference — is rejected at resolution; the `/std` wrappers are the door.
#[test]
fn rejects_sys_use_from_user_code() {
    let error = lower_with_prelude("use /sys/{Nat}; Nat/add(1, 2)").unwrap_err();
    assert!(
        error.contains("internal to the standard library"),
        "unexpected error: {error}"
    );
}

#[test]
fn rejects_sys_reference_in_term_from_user_code() {
    let error = lower_with_prelude("/sys/Nat/add(1, 2)").unwrap_err();
    assert!(
        error.contains("internal to the standard library"),
        "unexpected error: {error}"
    );
}

// The guard rides the *resolved* qualifier, not the spelling, so a relative
// reference is rejected exactly as the absolute one is — the leading `/` is not
// the boundary.
#[test]
fn rejects_relative_sys_reference_in_term() {
    let error = lower_with_prelude("sys/Nat/add(1, 2)").unwrap_err();
    assert!(
        error.contains("internal to the standard library"),
        "unexpected error: {error}"
    );
}

#[test]
fn rejects_relative_sys_use() {
    let error = lower_with_prelude("use sys/{Nat}; Nat/add(1, 2)").unwrap_err();
    assert!(
        error.contains("internal to the standard library"),
        "unexpected error: {error}"
    );
}

#[test]
fn rejects_relative_sys_glob() {
    let error = lower_with_prelude("use sys/*; Nat/add(1, 2)").unwrap_err();
    assert!(
        error.contains("internal to the standard library"),
        "unexpected error: {error}"
    );
}

// The interface (`pub use`) phase guards too: a user module cannot launder `sys`
// into its own public surface.
#[test]
fn rejects_sys_pub_use_reexport_from_user_code() {
    let error = lower_with_prelude("pub mod Foo\n    pub use /sys/{Nat};\nend\nType").unwrap_err();
    assert!(
        error.contains("internal to the standard library"),
        "unexpected error: {error}"
    );
}

// The same primitive reached through its `/std` wrapper resolves: `std` is
// privileged to reference `sys`, and re-exports it.
#[test]
fn allows_sys_reference_through_std_wrapper() {
    assert!(lower_with_prelude("use /std/{Nat}; Nat/add(1, 2)").is_ok());
}

// A user program cannot declare its own top-level `std`, `pub` or not — it
// would collide with the embedded standard library mounted at the same name.
#[test]
fn rejects_user_pub_mod_std_colliding_with_prelude_std() {
    let error =
        lower_with_prelude("pub mod std\n    pub let x : Type = Type;\nend\nType").unwrap_err();
    assert!(error.contains("std"), "unexpected error: {error}");
}

// The private case is the actual regression this guard closes: before
// `ModuleInfo::insert_child`'s collision check was made unconditional, a
// private redeclaration of a reserved name didn't trip the pub-only guard and
// silently overwrote the prelude's `std` registration instead of erroring.
#[test]
fn rejects_user_private_mod_std_colliding_with_prelude_std() {
    let error = lower_with_prelude("mod std\n    let x : Type = Type;\nend\nType").unwrap_err();
    assert!(error.contains("std"), "unexpected error: {error}");
}

// Without a prelude attached, `has_embedded_roots()` is false, so the fixed
// sys/syn/std machinery never runs at all — the user's own `mod std` is just
// an ordinary, unreserved entry-rooted module, not a collision.
#[test]
fn user_own_mod_std_without_prelude_is_not_a_collision() {
    run("mod std\n    pub let x : Type = Type;\nend\nuse std/{x};\nx");
}

#[test]
fn rejects_private_module_from_outside_its_declaring_subtree() {
    assert!(
        run_err(
            r#"
        pub mod Owner
            mod Foo
                pub let f : Type = Type;
            end
        end
        pub mod Bar
            use /Owner/Foo/{f};
        end
        Type
    "#
        )
        .contains("private child module")
    );
}

// A private declaration written at the root belongs to the root's subtree,
// which is the whole program — so a sibling module may name it. The boundary
// is the declaring module, and the root declares no boundary above itself.
#[test]
fn allows_a_private_root_module_from_a_sibling() {
    run(r#"
        mod Foo
            pub let f : Type = Type;
        end
        pub mod Bar
            use /{Foo};
        end
        Type
    "#);
}

#[test]
fn allows_pub_root_module_via_absolute_path() {
    run(r#"
        pub mod Foo
            pub let f : Type = Type;
        end
        pub mod Bar
            use /{Foo};
        end
        Type
    "#);
}

#[test]
fn private_use_does_not_expose_qualifier() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            pub mod Bar
                pub let f : Type = Type;
            end
        end
        pub mod MyMod
            use /Foo/{Bar};
        end
        MyMod/Bar/f
    "#
        )
        .contains("child module not found")
    );
}

#[test]
fn use_imports_binding_by_path() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        use /Foo/{x};
        x
    "#);
}

#[test]
fn rejects_use_of_private_binding() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            let x : Type = Type;
        end
        use /Foo/{x};
        x
    "#
        )
        .contains("private binding: x")
    );
}

#[test]
fn pub_use_re_exports_binding() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        pub mod Bar
            pub use /Foo/{x};
        end
        use /Bar/{x};
        x
    "#);
}

#[test]
fn pub_use_binding_aliases_to_canonical_path() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        pub mod Bar
            pub use /Foo/{x};
        end
        Bar/x
    "#);
}

#[test]
fn rejects_use_followed_by_local_let_of_same_name() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        use /Foo/{x};
        let x : Type = Type;
        x
    "#
        )
        .contains("binding conflicts with existing scope entry: x")
    );
}

#[test]
fn rejects_two_imports_of_same_name() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        pub mod Bar
            pub let x : Type = Type;
        end
        use /Foo/{x};
        use /Bar/{x};
        x
    "#
        )
        .contains("binding conflicts with existing scope entry: x")
    );
}

#[test]
fn relative_use_imports_binding() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        pub mod Bar
            use /{Foo};
            use Foo/{x};
            pub let y : Type = x;
        end
        Bar/y
    "#);
}

#[test]
fn rejects_use_of_unknown_item() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        use /Foo/{nope};
        Type
    "#
        )
        .contains("no module or binding named nope")
    );
}

#[test]
fn use_of_dual_existence_registers_both() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let y : Type = Type;
            end
            pub use X/{y};
        end
        pub mod Bar
            use /Foo/{y};
            pub let direct : Type = y;
            pub let via_path : Type = y;
        end
        Type
    "#);
}

#[test]
fn module_member_is_not_classified_as_a_generated_nominal_member() {
    let module = elaborate_source(
        r#"
        struct Foo(A : Type, B : Type) : pub Type { a : A, b : B }
        pub mod Foo
            pub let bar : Type = Type;
        end
        Type
    "#,
    );
    let bar = module
        .items
        .iter()
        .find_map(|item| match item {
            curios_core::Item::Let(definition) if definition.name.symbol() == "/Foo/bar" => {
                Some(definition)
            }
            _ => None,
        })
        .expect("module member definition");
    assert_eq!(bar.kind, curios_core::DefinitionKind::Authored);
}

#[test]
fn dual_use_lets_bare_name_resolve_to_binding() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let y : Type = Type;
            end
            pub use X/{y};
        end
        use /Foo/{y};
        y
    "#);
}

#[test]
fn dual_use_lets_path_resolve_through_module() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let y : Type = Type;
                pub let q : Type = Type;
            end
            pub use X/{y};
        end
        Foo/y
    "#);
}

#[test]
fn public_child_with_private_binding_imports_only_module() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let z : Type = Type;
            end
            use X/{z};
            let X : Type = z;
        end
        use /Foo/{X};
        X/z
    "#);
}

#[test]
fn private_child_with_public_binding_imports_only_binding() {
    run(r#"
        pub mod Foo
            mod X
                pub let z : Type = Type;
            end
            use X/{z};
            pub let X : Type = z;
        end
        use /Foo/{X};
        X
    "#);
}

#[test]
fn use_of_dual_existence_from_outside_module() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let X : Type = Type;
            end
            pub use X/{X};
        end
        use /Foo/{X};
        X
    "#);
}

#[test]
fn use_of_dual_existence_from_outside_qualifier_path() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let X : Type = Type;
                pub let q : Type = Type;
            end
            pub use X/{X};
        end
        use /Foo/{X};
        X/q
    "#);
}

#[test]
fn use_brace_group_imports_all_labels() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
            pub let y : Type = Type;
        end
        use /Foo/{x, y};
        x
    "#);
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
            pub let y : Type = Type;
        end
        use /Foo/{x, y};
        y
    "#);
}

#[test]
fn rejects_use_when_both_sides_private() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            mod X
                pub let z : Type = Type;
            end
            use X/{z};
            let X : Type = z;
        end
        use /Foo/{X};
        Type
    "#
        )
        .contains("private child module: X")
    );
}

#[test]
fn use_glob_imports_all_public_bindings() {
    assert_eq!(
        run(r#"
            pub mod Foo
                pub let x : Type = Type;
                pub let y : Type = Type;
            end
            use /Foo/*;
            x
        "#),
        curios_core::Term::let_(
            &global("/Foo/x"),
            written_type(0),
            written_type(1),
            curios_core::Term::let_(
                &global("/Foo/y"),
                written_type(2),
                written_type(3),
                curios_core::Term::var(curios_core::Var::free(global("/Foo/x")))
            )
        ),
    );
}

#[test]
fn use_glob_imports_child_modules_as_qualifiers() {
    run(r#"
        pub mod Foo
            pub mod Bar
                pub let f : Type = Type;
            end
        end
        use /Foo/*;
        Bar/f
    "#);
}

#[test]
fn use_glob_skips_private_child_modules() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            mod Bar
                pub let f : Type = Type;
            end
        end
        use /Foo/*;
        Bar/f
    "#
        )
        .contains("unresolved qualifier")
    );
}

#[test]
fn relative_use_glob_imports_from_qualifier() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        use Foo/*;
        x
    "#);
}

#[test]
fn pub_use_glob_re_exports_all_public_labels() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
            pub mod Bar
                pub let f : Type = Type;
            end
        end
        pub mod Mine
            pub use /Foo/*;
        end
        use /Mine/{Bar};
        use /Mine/{x};
        Bar/f
    "#);
}

#[test]
fn use_glob_on_dual_existence_imports_once() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let X : Type = Type;
                pub let q : Type = Type;
            end
            pub use X/{X};
        end
        use /Foo/*;
        X/q
    "#);
}

#[test]
fn file_loader_prepares_sibling_modules_before_to_core() {
    let base = temp_dir("sibling-order");
    write_module(
        &base,
        "A.crs",
        r#"
            use /B/{x};
            pub let y : Type = x;
        "#,
    );
    write_module(&base, "B.crs", "pub let x : Type = Type;");

    let entrypoint = r#"
            pub mod A;
            pub mod B;
            A/y
        "#
    .parse::<crate::Entrypoint>()
    .unwrap();
    let loader = crate::RootSource::file_system(base.clone());

    super::into_core(&entrypoint, &loader, syntax()).unwrap();

    fs::remove_dir_all(base).unwrap();
}

#[test]
fn file_backed_module_missing_from_loader_is_module_not_found() {
    let entrypoint = r#"
            pub mod A;
            Type
        "#
    .parse::<crate::Entrypoint>()
    .unwrap();

    assert!(matches!(
        super::into_core(&entrypoint, &crate::RootSource::none(), syntax()).unwrap_err(),
        crate::Error::Located { error, .. }
            if matches!(error.as_ref(), crate::Error::ModuleNotFound { path } if path == "/A")
    ));
}

#[test]
fn goal_lowers_to_marked_metavar() {
    // A written `?` lowers to the same fresh metavariable a desugared hole
    // does, but marked `MetavarOrigin::Goal` so zonk reports it.
    assert_eq!(run("?"), curios_core::Term::goal(0));
}

#[test]
fn distinct_goals_get_distinct_ids() {
    // Two goals in one program draw distinct, monotonic ids from the shared counter.
    let term = run("(?, ?)");
    assert_eq!(
        term,
        curios_core::Term::tuple([curios_core::Term::goal(0), curios_core::Term::goal(1)]),
    );
}

#[test]
fn bang_desugars_through_syn_monad_bind() {
    // Every value body is a region root: `x!` hoists to it and sequences
    // through the `/syn/Monad/bind` wrapper applied to the action and the
    // continuation over a gensym'd binder. The witness slot and implicits are
    // inserted during core elaboration.
    let expected = curios_core::Term::apply(
        curios_core::Term::var(curios_core::Var::free(global("/syn/Monad/bind"))),
        [
            // `x` resolves to nothing, so it lowers to a binder identity that
            // core will report as unbound — never to a global that a same-named
            // root-level definition could satisfy.
            curios_core::Term::var(curios_core::Var::free(curios_core::Free::local(
                0,
                Some("x"),
            ))),
            curios_core::Term::func(
                [(
                    curios_core::Free::local(1, None),
                    curios_core::Term::metavar(0),
                )],
                curios_core::Term::var(curios_core::Var::free(curios_core::Free::local(1, None))),
            ),
        ],
    );
    assert_eq!(run("x!"), expected);
}

#[test]
fn choose_lowers_to_nested_bool_matches() {
    // `choose | p => a | q => b | _ => ? end` right-folds into two nested
    // `Bool` matches: the first condition's false branch holds the second,
    // whose own false branch is the `_` default (a plain hole here).
    let term = run("choose | p => a | q => b | _ => ? end");

    let curios_core::Subterm::Match(outer) = &*term else {
        panic!("expected a Match at the top, got {term:?}");
    };
    let curios_core::Cases::Bool { false_case, .. } = &outer.cases else {
        panic!("expected the outer Cases::Bool, got {:?}", outer.cases);
    };
    let curios_core::Subterm::Match(inner) = &**false_case else {
        panic!("expected a nested Match in the outer false branch, got {false_case:?}");
    };
    let curios_core::Cases::Bool {
        false_case: inner_false,
        ..
    } = &inner.cases
    else {
        panic!("expected the inner Cases::Bool, got {:?}", inner.cases);
    };
    assert!(
        matches!(&**inner_false, curios_core::Subterm::Metavar(_)),
        "the `_` default should sit at the innermost false branch, got {inner_false:?}"
    );
}

#[test]
fn bind_arm_bare_binder_is_rejected() {
    // `| x = n =>` binds irrefutably — always fires, so the rest of the ladder
    // is dead. Rejected in favor of a `let`.
    let error = run_err("choose | x = n => x | _ => 0 end");
    assert!(
        error.contains("refutable") && error.contains("let"),
        "unexpected error: {error}"
    );
}

#[test]
fn named_catch_all_is_rejected() {
    // A named final arm among concrete constructor arms is not a catch-all.
    let error = run_err("match m | some(x) => x | rest => 0 end");
    assert!(
        error.contains("named final arm") && error.contains("_"),
        "unexpected error: {error}"
    );
}

#[test]
fn nested_underscore_mixed_with_concrete_stays_inconsistent_shape() {
    // A `_` *nested* inside a constructor payload (not a final top-level arm)
    // still mixes a binder with a concrete shape in the same column — the
    // pre-existing full-enumeration boundary, not a catch-all.
    let error = run_err("match m | some(some(x)) => x | some(_) => 0 | none() => 1 end");
    assert!(
        error.contains("disagree on shape"),
        "unexpected error: {error}"
    );
}

#[test]
fn nested_nat_literal_lowers_to_switch() {
    // A literal `5` inside a constructor payload, with a `_` fallthrough, is
    // value dispatch — it lowers through `compile_nat`'s switch mode to a
    // `Cases::Switch`, not the `Nat` eliminator. (`wrap`/`b` need not resolve:
    // lowering precedes name resolution.)
    let term = run("match b | wrap(5) => 1 | _ => 0 end");
    assert!(
        format!("{term:?}").contains("Switch"),
        "expected a Cases::Switch, got {term:?}"
    );
}

#[test]
fn nat_literal_mixed_with_succ_is_rejected() {
    // A literal case and a `n + 1; ih` successor arm in the same `Nat` column
    // select incompatible core forms (a value `switch` vs. the eliminator).
    let error = run_err("match b | wrap(5) => 1 | wrap(n + 1; ih) => n | _ => 0 end");
    assert!(
        error.contains("mixes successor-peeling"),
        "unexpected error: {error}"
    );
}

#[test]
fn bang_in_a_type_is_rejected() {
    // Types have no region to hoist an action to, so a `!` in an annotation is
    // rejected during desugaring.
    assert!(run_err("let a : e! = x; a").contains("not allowed inside a type"));
}

#[test]
fn foreign_declaration_populates_the_store() {
    // No loader/prelude needed at all: a `foreign` signature is parsed
    // directly into `WireType`s, not resolved as ordinary names.
    let (_, _, _, foreigns) = super::into_core(
        &"foreign frobnicate : (Nat, Bin) -> Nat; 0"
            .parse::<crate::Entrypoint>()
            .unwrap(),
        &crate::RootSource::none(),
        syntax(),
    )
    .unwrap();

    let function = foreigns.get("/frobnicate").expect("frobnicate registered");
    assert_eq!(
        function.signature.params,
        vec![
            ("a0".to_string(), WireType::Nat),
            ("a1".to_string(), WireType::Bin),
        ]
    );
    assert_eq!(
        function.signature.results,
        vec![("_".to_string(), WireType::Nat)]
    );
}

#[test]
fn foreign_declaration_zero_arg_populates_the_store() {
    let (_, _, _, foreigns) = super::into_core(
        &"foreign clock : Nat; 0"
            .parse::<crate::Entrypoint>()
            .unwrap(),
        &crate::RootSource::none(),
        syntax(),
    )
    .unwrap();

    let function = foreigns.get("/clock").expect("clock registered");
    assert!(function.signature.params.is_empty());
}

#[test]
fn foreign_declaration_call_lowers() {
    // Declaring and calling a foreign function lowers end to end (`run`
    // panics on failure) — the `Prim::Foreign` body `foreign_signature`
    // builds is well typed against the same wire-typed signature the call
    // site checks against.
    let _ = run(r#"
        foreign frobnicate : (Nat, Bin) -> Nat;
        frobnicate(5, x\00\01)
    "#);
}

// Caught during discovery now (`ModuleInfo::insert_binding`'s collision guard
// is unconditional, not pub-only), before `Context::insert_binding`'s later
// scope-conflict check would otherwise see it.
#[test]
fn duplicate_foreign_declaration_in_one_scope_is_rejected() {
    assert!(
        run_err("foreign frobnicate : Nat; foreign frobnicate : Nat; 0")
            .contains("duplicate public declaration")
    );
}

#[test]
fn foreign_declarations_across_modules_get_distinct_import_names() {
    // Two `foreign` declarations in different modules coexist: the wasm
    // import name is the declaration's fully qualified name, so the shared
    // label never collides on the wire — each module's row registers under
    // its own name.
    let (_, _, _, foreigns) = super::into_core(
        &r#"
        mod A
            foreign frobnicate : Nat;
        end
        mod B
            foreign frobnicate : Nat;
        end
        0
    "#
        .parse::<crate::Entrypoint>()
        .unwrap(),
        &crate::RootSource::none(),
        syntax(),
    )
    .unwrap();

    assert!(foreigns.get("/A/frobnicate").is_some());
    assert!(foreigns.get("/B/frobnicate").is_some());
}

// === Subtree visibility ======================================================

// A declaration written without `pub` in `M` is visible within `M`'s subtree,
// so a descendant may name its ancestor's private binding.
#[test]
fn descendant_reads_its_ancestors_private_binding() {
    run(r#"
        pub mod Owner
            let helper : Type = Type;
            pub mod Worker
                use /Owner/{helper};
                pub let use_it : Type = helper;
            end
        end
        Type
    "#);
}

// The relaxation is downward only: a sibling is outside the declaring module's
// subtree, so it stays shut out.
#[test]
fn sibling_cannot_read_a_siblings_private_binding() {
    assert!(
        run_err(
            r#"
        pub mod Owner
            pub mod A
                let secret : Type = Type;
            end
            pub mod B
                use /Owner/A/{secret};
                pub let use_it : Type = secret;
            end
        end
        Type
    "#
        )
        .contains("private binding")
    );
}

// Nor upward: a parent may traverse its own private child, but not read that
// child's private bindings.
#[test]
fn parent_cannot_read_its_childs_private_binding() {
    assert!(
        run_err(
            r#"
        pub mod Owner
            mod Impl
                let secret : Type = Type;
            end
            pub let use_it : Type = Impl/secret;
        end
        Type
    "#
        )
        .contains("private binding")
    );
}

// `pub` inside a private module means "wherever this module is visible", which
// is its declaring module's subtree — not the world.
#[test]
fn pub_inside_a_private_module_reaches_the_subtree_only() {
    run(r#"
        pub mod Owner
            mod Impl
                pub let helper : Type = Type;
            end
            pub mod Worker
                use /Owner/Impl/{helper};
                pub let use_it : Type = helper;
            end
        end
        Type
    "#);

    assert!(
        run_err(
            r#"
        pub mod Owner
            mod Impl
                pub let helper : Type = Type;
            end
        end
        pub mod Outsider
            use /Owner/Impl/{helper};
        end
        Type
    "#
        )
        .contains("private child module")
    );
}

// A glob imports the exported surface, never a subtree-private declaration:
// reaching one always requires naming it. The reference is left as a bare name
// for core to reject, rather than silently resolving to `/Owner/helper`.
#[test]
fn glob_does_not_import_subtree_private_bindings() {
    let term = run(r#"
        pub mod Owner
            let helper : Type = Type;
            pub mod Worker
                use /Owner/*;
                pub let use_it : Type = helper;
            end
        end
        Type
    "#);

    // The reference is left for core to reject. What it must *not* be is any
    // global: `/Owner/helper` would mean the glob leaked a private binding, and
    // a root-level `/helper` would silently capture an entry-module definition
    // of the same name. A binder identity can be neither.
    // The reference is left for core to reject. What it must *not* be is any
    // global: `/Owner/helper` would mean the glob leaked a private binding, and
    // a root-level `/helper` would silently capture an entry-module definition
    // of the same name. A binder identity can be neither.
    let dumped = format!("{term:?}");
    assert!(
        dumped.contains("Local(Mint { index: 0, hint: Some(\"helper\") })"),
        "unexpected term: {dumped}"
    );
    // `/Owner/helper` occurs exactly once — as the binder the declaration
    // introduces. A second occurrence would be the reference resolving to it.
    assert_eq!(
        dumped
            .matches("Authored(Qualifier([\"Owner\", \"helper\"]))")
            .count(),
        1,
        "the glob leaked a private binding: {dumped}"
    );
}

// === Interface audit =========================================================

// The facade pattern: a module re-exports a name out of its own private child
// and then uses it in a public signature. The audit follows the re-export, so
// the name is as visible as the facade makes it.
#[test]
fn facade_may_name_what_it_re_exports_in_a_public_signature() {
    run(r#"
        pub mod Facade
            mod Impl
                pub let Helper : Type = Type;
            end
            pub use Impl/{Helper};
            pub let build(h : Helper) -> Helper = h;
        end
        Type
    "#);
}

// Without the re-export the same signature is rejected: `Helper` reaches only
// `Facade`'s subtree, while `build` reaches the whole program.
#[test]
fn public_signature_naming_an_unexported_subtree_item_is_rejected() {
    assert!(
        run_err(
            r#"
        pub mod Facade
            mod Impl
                pub let Helper : Type = Type;
            end
            use Impl/{Helper};
            pub let build(h : Helper) -> Helper = h;
        end
        Type
    "#
        )
        .contains("exposes private item"),
    );
}

// `/std/Async/block_on` pins an unannotated local binding through a *type
// alias* — `let Slot : Type = Cell(Option(Job));` — whose bare written `Type`
// mints a generalizable level while its value sits at one fixed level.
#[test]
fn an_unannotated_local_let_is_pinned_through_a_type_alias() {
    let module = elaborate_source(
        "induct Box(A : Type) : Type | empty() | wrap(A) end \
         induct Job : Type | job() end \
         let Slot : Type = Box(Job); \
         let outer(@A : Type, x : A) -> A = \
             let slot = Box/empty(); \
             let force(b : Slot) -> {} = (); \
             let _ = force(slot); \
             x; \
         outer",
    );
    assert!(
        module
            .items
            .iter()
            .any(|item| matches!(item, curios_core::Item::Let(d) if d.name.symbol() == "/outer")),
        "outer elaborated"
    );
}
