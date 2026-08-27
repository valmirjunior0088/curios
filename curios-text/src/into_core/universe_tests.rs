//! Universe levels and roles the lowering assigns, cumulativity, and the context a concept's wrappers share.

use crate::{Entrypoint, RootSource};
use curios_utilities::Qualifier;

use super::test_support::*;

#[test]
fn no_items_simple_tail() {
    assert_eq!(run("Type"), written_type(0));
}

#[test]
fn written_types_get_distinct_levels_and_lexical_roles() {
    let (module, _, universe_floor, _) = super::into_core(
        &"let id(@A : Type, x : A) -> A = x; Type"
            .parse::<Entrypoint>()
            .unwrap(),
        &RootSource::none(),
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

    let body = module.body.as_ref().expect("the entrypoint has a body");
    let curios_core::Subterm::Tuple(tuple) = &**body else {
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
                    owner: Qualifier::from(["Result"]),
                    tag: curios_core::Atom::from("success"),
                },
                2,
            ),
            (
                global_name("/Result/failure"),
                curios_core::DefinitionKind::InductiveConstructor {
                    owner: Qualifier::from(["Result"]),
                    tag: curios_core::Atom::from("failure"),
                },
                2,
            ),
        ],
    );
}

/// `id` is applied at two different levels in one block, which a local universe scheme once served. Cumulativity carries it instead: `Prop : Type 0` and `Type 0 : Type 1`, so a single monomorphic `A : Type 1` accepts both, and the level order is linear so a sup always exists. The binding therefore carries no scheme of its own — universe polymorphism belongs to declarations, which are frozen into the prelude archive and re-instantiated by later programs.
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

/// The same, one indirection further: `alias` has no annotation at all, so its type is inferred from `id` and then used at both levels.
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

/// A level a caller supplies stays a parameter: `@A : Type` puts the level in an argument position, so each occurrence chooses it.
#[test]
fn a_level_in_argument_position_stays_a_parameter() {
    let module = elaborate_source("pub let pick(@A : Type, x : A) -> A = x; pick");
    assert_eq!(universe_parameters(&module, "/pick"), 1);
}

/// A level occurring *only* in the result is determined, not chosen: no occurrence of `Holds` can supply it, so generalizing would mint a parameter every use site has to instantiate for nothing. Minimizing it instead is what keeps a literal's per-byte constructor applications from each minting fresh levels — see `result_sort_only_metas`.
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

/// Superclass fields are anonymous positional slots: no namespace binding, no wrapper. Registering their empty labels used to make a concept's *second* superclass collide with the first as an empty-named duplicate declaration, refusing the whole module at discovery.
#[test]
fn a_concept_with_two_superclasses_lowers() {
    run("pub concept A(T : Type) : pub Type { fa(T) -> T, }
         pub concept B(T : Type) : pub Type { fb(T) -> T, }
         pub concept C(T : Type) : pub Type {
             use A(T),
             use B(T),
             fc(T) -> T,
         }
         C");
}

/// The scheduler's witness edges (`witness_dep_nodes`): `probe`'s declared type only converts by unfolding `+` within `probe`'s own item, the row satisfying it is declared *last* and referenced by no name, and only the operator's soft edge can order the row first — the deferred-witness store retries between items, which is too late for a conversion the item drain must decide. Under name edges alone `probe` elaborates first and fails exactly as `/std/BigPos/add`'s certificate once did.
#[test]
fn an_operator_in_a_dependent_type_orders_after_its_witness_row() {
    elaborate_source(
        "mod syn
             pub concept Add(A : Type) : pub Type {
                 add(A, A) -> A,
             }
         end
         use /syn/{Add};
         pub induct One : pub Type
         | point()
         end
         pub induct Box : pub Type
         | box()
         end
         pub let join(a : Box, b : Box) -> Box =
             Box/box();
         pub let Probe(b : Box) -> Type =
             match b : (_) => Type | box() => One end;
         pub let probe(a : Box, b : Box) -> Probe(a + b) =
             One/point();
         satisfy Add(Box) {
             add = join,
         }
         Type",
    );
}

/// A generated method wrapper belongs to *its concept's* universe context, not to one generalized from its own signature. The wrapper's type names only the levels its own field needs, yet it also carries `use w : C(…)` applied at all of the concept's; a level outside the wrapper's own generalized set would then have nothing to denote it.
#[test]
fn a_concept_method_wrapper_shares_its_concept_universe_context() {
    let module = elaborate_source("pub concept C(A : Type) : pub Type { f(A) -> A, } C");
    assert_eq!(universe_parameters(&module, "/C"), 1);
    assert_eq!(universe_parameters(&module, "/C/f"), 1);
}

/// The same rule where the concept's levels genuinely exceed any one wrapper's: `pure` names a strict subset of `M`'s and `bind` a different subset, so generalizing either alone comes out short.
#[test]
fn every_wrapper_of_a_higher_kinded_concept_shares_one_universe_context() {
    let module = elaborate_source(
        "pub concept M(F : (Type) -> Type) : pub Type {
             pure(@A : Type, value : A) -> F(A),
             bind(@A : Type, @B : Type, action : F(A), next : (A) -> F(B)) -> F(B),
         } M",
    );
    // Five, against `pure`'s own two and `bind`'s one: the point of the test is lost if the concept ever stops outrunning its wrappers.
    assert_eq!(universe_parameters(&module, "/M"), 5);
    assert_eq!(universe_parameters(&module, "/M/pure"), 5);
    assert_eq!(universe_parameters(&module, "/M/bind"), 5);
}

/// A concept's field telescope is dependent — the record pass binds each field's label for the fields after it — so a field type may name a preceding field. The generated method wrapper has to state that type with every such reference projected off its own witness, `Eq(w.op(w.op(x)), w.op(x))`; re-lowering the written type in the wrapper's scope instead leaves `op` bound by nothing.
#[test]
fn a_concept_field_may_reference_a_preceding_field() {
    elaborate_source(
        "pub induct Eq(@A : Type) : (A, A) -> pub Prop
         | refl(@z : A) : (z, z)
         end
         pub concept Idem(A : Type) : pub Type {
             op(A) -> A,
             law(x : A) -> Eq(op(op(x)), op(x)),
         }
         Idem",
    );
}

/// Superclass fields occupy positions in the field telescope under minted `_super{i}` labels, but generate no wrapper. A wrapper must therefore read its field type at that field's *absolute* telescope index; using its position among the non-super fields would read `law`'s type one slot early here and hand the wrapper `op`'s.
#[test]
fn a_superclass_does_not_shift_a_dependent_field_reference() {
    elaborate_source(
        "pub induct Eq(@A : Type) : (A, A) -> pub Prop
         | refl(@z : A) : (z, z)
         end
         pub concept Base(T : Type) : pub Type { base(T) -> T, }
         pub concept Idem(A : Type) : pub Type {
             use Base(A),
             op(A) -> A,
             law(x : A) -> Eq(op(op(x)), op(x)),
         }
         Idem",
    );
}
