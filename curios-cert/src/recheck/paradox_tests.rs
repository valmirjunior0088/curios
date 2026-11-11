//! The field's paradoxes put to the certifier alone, as Core text no elaborator writes: a sort at its own level, a universe context with no model, an instance its scheme does not admit, Hurkens' carrier at one level, a proposition over its own sort, an instance's sorts out of place, a proposition taken apart into data, a family reaching itself non-strictly, a level read through a family declared later, and an ordinal applied at its own universe.

use {super::test_support::*, crate::*, curios_common::fill, curios_core::Polarity};

/// Whether `verdicts` refuse the item `symbol` by an error `refused` holds of.
fn refuses(verdicts: &[Verdict], symbol: &str, refused: impl Fn(&Error) -> bool) -> bool {
    verdicts
        .iter()
        .any(|verdict| verdict.name == Some(global(symbol)) && refused(&verdict.error))
}

/// Whether `verdicts` refuse nothing under `symbol`.
fn stands(verdicts: &[Verdict], symbol: &str) -> bool {
    !verdicts
        .iter()
        .any(|verdict| verdict.name == Some(global(symbol)))
}

/// Girard's paradox needs one universe that is its own type: `Type 0` at `Type 0`, a universe at its own parameter, a `Prop` at the `Prop` of its level, and `Prop` at `Type 0`.
const SORTS_AT_THEIR_OWN_LEVEL: &str = r#"
let /tt : Type = Type;
#{universe_context = {parameter_count = 1, constraints = []}}
let /selfsort : #{level = u} Type = #{level = u} Type;
#{universe_context = {parameter_count = 1, constraints = []}}
let /propself : #{level = u} Prop = #{level = u} Prop;
let /propsort : Type = Prop;
let /raised : #{level = 1} Type = Type;
/std/Bool/False
"#;

/// Each sort is refused at the level of its own type, and `Type 0` stands at `Type 1`.
#[test]
fn no_sort_inhabits_itself_at_any_level() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${SORTS_AT_THEIR_OWN_LEVEL}",
        PROPOSITIONS,
        SORTS_AT_THEIR_OWN_LEVEL
    ));

    for symbol in ["tt", "selfsort", "propself", "propsort"] {
        assert!(
            refuses(&verdicts, symbol, |error| matches!(
                error,
                Error::Mismatch { .. }
            )),
            "{symbol}: {verdicts:?}"
        );
    }
    assert!(stands(&verdicts, "raised"), "{verdicts:?}");
}

/// Ten sets with no model in the naturals, each a loop through more than one parameter, a maximum or the zero, and three that have one, each a loop above less the constraint that closes it.
const CONTEXTS_WITH_NO_MODEL: &str = r#"
#{universe_context = {parameter_count = 2, constraints = [u + 1 <= v, v <= u]}}
let /twoloop : #NatType = 0;
#{universe_context = {parameter_count = 3, constraints = [u + 1 <= v, v + 1 <= w, w <= u]}}
let /threeloop : #NatType = 0;
#{universe_context = {parameter_count = 2, constraints = [u + 1 <= max(u, v), v <= u]}}
let /maxloop : #NatType = 0;
#{universe_context = {parameter_count = 2, constraints = [max(u + 1, v + 1) <= v]}}
let /maxlower : #NatType = 0;
#{universe_context = {parameter_count = 2, constraints = [u + 1 <= v, v <= 0]}}
let /zeroloop : #NatType = 0;
#{universe_context = {parameter_count = 2, constraints = [1 <= u, u <= 0]}}
let /constloop : #NatType = 0;
#{universe_context = {parameter_count = 1, constraints = [2 <= max(1, u), u <= 1]}}
let /constmax : #NatType = 0;
#{universe_context = {parameter_count = 1, constraints = [u + 1 <= max(u, 0)]}}
let /maxzero : #NatType = 0;
#{universe_context = {parameter_count = 2, constraints = [max(u + 1, 3) <= max(v, 2), v <= u]}}
let /mixed : #NatType = 0;
#{universe_context = {parameter_count = 2, constraints = [max(u + 1, v + 1) <= max(u, v)]}}
let /maxself : #NatType = 0;
#{universe_context = {parameter_count = 3, constraints = [u + 1 <= v, v + 1 <= w]}}
let /chain : #NatType = 0;
#{universe_context = {parameter_count = 2, constraints = [u + 1 <= max(u, v)]}}
let /escapes : #NatType = 0;
#{universe_context = {parameter_count = 2, constraints = [max(u + 1, 3) <= max(v, 2)]}}
let /lifted : #NatType = 0;
/std/Bool/False
"#;

/// A context assumed is a hypothesis set `entails` answers every level question under, so one with no model is refused before anything is judged under it, wherever its loop runs; each loop less its closing constraint stands.
#[test]
fn a_universe_context_with_no_model_through_several_parameters_is_refused() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${CONTEXTS_WITH_NO_MODEL}",
        PROPOSITIONS,
        CONTEXTS_WITH_NO_MODEL
    ));

    for symbol in [
        "twoloop",
        "threeloop",
        "maxloop",
        "maxlower",
        "zeroloop",
        "constloop",
        "constmax",
        "maxzero",
        "mixed",
        "maxself",
    ] {
        assert!(
            refuses(&verdicts, symbol, |error| matches!(
                error,
                Error::UnsatisfiableUniverses
            )),
            "{symbol}: {verdicts:?}"
        );
    }
    for symbol in ["chain", "escapes", "lifted"] {
        assert!(stands(&verdicts, symbol), "{symbol}: {verdicts:?}");
    }
}

/// `T@{u, v | u + 1 <= v} : Type v = Type u` read where its constraint fails: at `{0, 0}`, under a reader's `u <= v`, and under a reader's `u = v`; a family whose former states no constraint where its entry states `u + 1 <= v`, instantiated by its node at `{0, 0}`; and a family, a record and a definition stating no constraint each read `T@{u, v}` right after an item whose context holds the constraint, so a context left standing would discharge it.
const INSTANCES_BELOW_THEIR_SCHEME: &str = r#"
#{universe_context = {parameter_count = 2, constraints = [u + 1 <= v]}}
let /T : #{level = v} Type = #{level = u} Type;
let /collapse : Type = #{universes = {levels = [0, 0]}} /T;
#{universe_context = {parameter_count = 2, constraints = [u <= v]}}
let /weaker : #{level = v} Type = #{universes = {levels = [u, v]}} /T;
#{universe_context = {parameter_count = 2, constraints = [u <= v, v <= u]}}
let /equal : #{level = v} Type = #{universes = {levels = [u, v]}} /T;
#{
    universe_context = {parameter_count = 2, constraints = []},
    entry = {universe_context = {parameter_count = 2, constraints = [u + 1 <= v]}}}
induct /Big: pub #{level = v} Type
| mk(T: #{level = u} Type)
end
let /node : Type = #{node = InductType, universes = {levels = [0, 0]}} /Big();
#{universe_context = {parameter_count = 2, constraints = [u + 1 <= v]}}
let /before : #NatType = 0;
#{universe_context = {parameter_count = 2, constraints = []}}
induct /F: pub #{level = v + 1} Type
| f(x: #{universes = {levels = [u, v]}} /T)
end
#{universe_context = {parameter_count = 2, constraints = [u + 1 <= v]}}
let /before2 : #NatType = 0;
#{universe_context = {parameter_count = 2, constraints = []}}
struct /S: pub #{level = v + 1} Type { x: #{universes = {levels = [u, v]}} /T }
#{universe_context = {parameter_count = 2, constraints = [u + 1 <= v]}}
let /before3 : #NatType = 0;
#{universe_context = {parameter_count = 2, constraints = []}}
let /after : #{level = v + 1} Type = #{universes = {levels = [u, v]}} /T;
let /fits : #{level = 1} Type = #{universes = {levels = [0, 1]}} /T;
/std/Bool/False
"#;

/// An instance chosen to collapse two levels is refused wherever it is read, under the reader's own context and none left by an item before it, the former of a family whose entry states the constraint included; `T@{0, 1}` stands.
#[test]
fn an_instance_is_held_to_its_scheme_under_the_readers_own_context() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${INSTANCES_BELOW_THEIR_SCHEME}",
        PROPOSITIONS,
        INSTANCES_BELOW_THEIR_SCHEME
    ));

    for symbol in [
        "collapse", "weaker", "equal", "Big", "node", "F", "S", "after",
    ] {
        assert!(
            refuses(&verdicts, symbol, |error| matches!(
                error,
                Error::UniverseInstance { .. }
            )),
            "{symbol}: {verdicts:?}"
        );
    }
    assert!(stands(&verdicts, "fits"), "{verdicts:?}");
}

/// A type taken below its level: a family of `Type 1`s claimed as one of `Type 0`s, a function over `Type 0` whose results its caller sizes handed to a parameter over `Type 1`, a proposition claimed a type, and a proposition over every proposition claimed one of them.
const LOWERED: &str = r#"
let /codomain : (x: #NatType) -> Type = (x: #NatType) => Type;
let /g : (A: Type) -> Type = (A: Type) => (x: A) -> A;
let /domain : (A: #{level = 1} Type) -> Type = /g;
let /proptype : Type = /std/Bool/False;
let /propdown : Prop = (A: Prop) -> A;
let /upward : (x: #NatType) -> #{level = 1} Type = (x: #NatType) => Type;
/std/Bool/False
"#;

/// Cumulativity lifts a codomain and never lowers one, compares a domain by conversion, and takes no proposition where a type is wanted: `/domain` is the shape that would size `g(Type 0)`, a `Type 1`, at `Type 0`. A family lifted upward stands.
#[test]
fn cumulativity_takes_no_type_to_a_lower_level() {
    let verdicts = core_text_verdicts(&fill!("${PROPOSITIONS}\n${LOWERED}", PROPOSITIONS, LOWERED));

    for symbol in ["codomain", "domain", "proptype", "propdown"] {
        assert!(
            refuses(&verdicts, symbol, |error| matches!(
                error,
                Error::Mismatch { .. }
            )),
            "{symbol}: {verdicts:?}"
        );
    }
    assert!(stands(&verdicts, "upward"), "{verdicts:?}");
}

/// The four products Hurkens' paradox reads off its carrier, each claimed at the level of the types it quantifies over, as `xboard/tickets/formation/a_group_met_as_a_bare_projection_is_checked_under_the_enclosing_declarations_constraints_and_read_under_the_readers/`'s witness reads them: `(A: Type) -> A`, the universe `U` of System U−, a product over the predicates on a `Type 1` and one over a `Type 1`; then each a level up, where it stands.
const HURKENS_CARRIER: &str = r#"
let /bottom : Type = (A: Type) -> A;
let /universe : Type =
    (X: Type, f: (t: (s: (s2: X) -> Type) -> Type) -> X) -> (s3: (s4: X) -> Type) -> Type;
let /over : (S: #{level = 1} Type, F: (p: (s: S) -> Type) -> Type) -> Type =
    (S: #{level = 1} Type, F: (p: (s: S) -> Type) -> Type) => (p: (s: S) -> Type) -> F(p);
let /pi : (S: #{level = 1} Type, F: (x: S) -> Type) -> Type =
    (S: #{level = 1} Type, F: (x: S) -> Type) => (x: S) -> F(x);
let /bottomUp : #{level = 1} Type = (A: Type) -> A;
let /universeUp : #{level = 1} Type =
    (X: Type, f: (t: (s: (s2: X) -> Type) -> Type) -> X) -> (s3: (s4: X) -> Type) -> Type;
let /overUp : (S: #{level = 1} Type, F: (p: (s: S) -> Type) -> Type) -> #{level = 1} Type =
    (S: #{level = 1} Type, F: (p: (s: S) -> Type) -> Type) => (p: (s: S) -> Type) -> F(p);
let /piUp : (S: #{level = 1} Type, F: (x: S) -> Type) -> #{level = 1} Type =
    (S: #{level = 1} Type, F: (x: S) -> Type) => (x: S) -> F(x);
/std/Bool/False
"#;

/// A product stands a level above what it quantifies over, so none of the four the paradox needs is impredicative, and each stands one level up.
#[test]
fn hurkens_carrier_stands_a_level_above_the_types_it_quantifies_over() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${HURKENS_CARRIER}",
        PROPOSITIONS,
        HURKENS_CARRIER
    ));

    for symbol in ["bottom", "universe", "over", "pi"] {
        assert!(
            refuses(&verdicts, symbol, |error| matches!(
                error,
                Error::Mismatch { .. }
            )),
            "{symbol}: {verdicts:?}"
        );
    }
    for symbol in ["bottomUp", "universeUp", "overUp", "piUp"] {
        assert!(stands(&verdicts, symbol), "{symbol}: {verdicts:?}");
    }
}

/// Hurkens' impredicative pair `{P: Prop, p: P}` claimed a proposition, and the same pair as a proposition's constructor, as a constructor whose payload an index pins, and as a proposition's field quantifying over every proposition; then the constructor a level up, where it stands.
const PROPOSITIONS_OVER_THEIR_SORT: &str = r#"
let /pair : Prop = {P: Prop, p: P};
induct /I: pub Prop
| i(A: Prop, a: A)
end
induct /J: (A: Prop) -> pub Prop
| j(A: Prop): (A)
end
struct /S: pub Prop { f: (A: Prop) -> (a: A) -> A }
induct /Raised: pub #{level = 1} Prop
| i(A: Prop, a: A)
end
/std/Bool/False
"#;

/// `Prop` is predicative: a record of a proposition and its proof is a `Type 1`, and a declared proposition is sized as a type is, a payload its index pins and a proof-valued field among the domains it bounds. A large elimination out of `/I` and Abel and Coquand's cast need the declarations at `Prop 0`.
#[test]
fn a_proposition_quantifying_over_propositions_stands_a_level_above_them() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${PROPOSITIONS_OVER_THEIR_SORT}",
        PROPOSITIONS,
        PROPOSITIONS_OVER_THEIR_SORT
    ));

    assert!(
        refuses(&verdicts, "pair", |error| matches!(
            error,
            Error::Mismatch { .. }
        )),
        "{verdicts:?}"
    );
    for symbol in ["I", "J", "S"] {
        assert!(
            refuses(&verdicts, symbol, |error| matches!(
                error,
                Error::Oversized { .. }
            )),
            "{symbol}: {verdicts:?}"
        );
    }
    assert!(stands(&verdicts, "Raised"), "{verdicts:?}");
}

/// `Sel@{q0, q1}(A: Sort q0, B: Sort q1) : Sort q1 = B`, a declaration leaving two sorts to its caller, read at `{Prop, Type}` over `Nat`, where irrelevance would make `0` and `1` one, and at `{Type, Prop}` over `True`, where it does make `qed` one with itself.
const TWO_SORTS_LEFT_TO_A_CALLER: &str = r#"
#{universe_context = {parameter_count = 0, quality_count = 2, constraints = []}}
let /Sel : (A: Sort, B: #{quality = Param(1)} Sort) -> #{quality = Param(1)} Sort =
    (A: Sort, B: #{quality = Param(1)} Sort) => B;
let /swap : (P: (x: (#{universes = {qualities = [Prop, Type]}} /Sel)(/std/Bool/True, #NatType)) -> Type) -> (h: P(0)) -> P(1) =
    (P: (x: (#{universes = {qualities = [Prop, Type]}} /Sel)(/std/Bool/True, #NatType)) -> Type) => (h: P(0)) => h;
let /kept : (P: (x: (#{universes = {qualities = [Type, Prop]}} /Sel)(#NatType, /std/Bool/True)) -> Type) -> (h: P(#{node = Variant} /std/Bool/True/qed())) -> P(#{node = Variant} /std/Bool/True/qed()) =
    (P: (x: (#{universes = {qualities = [Type, Prop]}} /Sel)(#NatType, /std/Bool/True)) -> Type) => (h: P(#{node = Variant} /std/Bool/True/qed())) => h;
/std/Bool/False
"#;

/// A type known for a proposition by its declaration is read at the qualities its instance states, each in the place its declaration binds it, so `Sel@{Prop, Type}(True, Nat)` is `Nat`, no proposition, and `/swap` is refused.
#[test]
fn an_instance_states_each_sort_where_its_declaration_binds_it() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${TWO_SORTS_LEFT_TO_A_CALLER}",
        PROPOSITIONS,
        TWO_SORTS_LEFT_TO_A_CALLER
    ));

    assert!(
        refuses(&verdicts, "swap", |error| matches!(
            error,
            Error::Mismatch { .. }
        )),
        "{verdicts:?}"
    );
    for symbol in ["Sel", "kept"] {
        assert!(stands(&verdicts, symbol), "{symbol}: {verdicts:?}");
    }
}

/// Berardi's route without a classical axiom: `Two : Prop | a | b` computes a type, inhabited at `a` and empty at `b`, and the closing term crosses from one to the other on the two proofs being one.
const TWO_PROOFS_TWO_TYPES: &str = r#"
induct /E: pub Type
end
induct /U: pub Type
| u()
end
induct /Two: pub Prop
| a()
| b()
end
let /T : (p: #{node = InductType} /Two()) -> Type =
    (p: #{node = InductType} /Two()) =>
        #{result = Ambient(Type)}
        match p
        | a() => #{node = InductType} /U()
        | b() => #{node = InductType} /E()
        end;
((h: (x: /T(#{node = Variant} /Two/a())) -> /T(#{node = Variant} /Two/b())) =>
    #{result = Ambient(/std/Bool/False)}
    match h(#{node = Variant} /U/u())
    end)(
    (x: /T(#{node = Variant} /Two/a())) => x
)
"#;

/// The large-elimination guard refuses `/T`, and nothing else is refused: the closing term types at `False` once `/T` is admitted.
#[test]
fn two_constructors_of_a_proposition_compute_no_type_a_coercion_could_cross() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${TWO_PROOFS_TWO_TYPES}",
        PROPOSITIONS,
        TWO_PROOFS_TWO_TYPES
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(name), error: Error::LargeElimination(family) }]
                if *name == global("T") && *family == global("Two")
        ),
        "{verdicts:?}"
    );
}

/// A proposition holding a boolean no index pins, read out into data and through it into a type, inhabited at `true` and empty at `false`, crossed by the closing term on `mk(true)` and `mk(false)` being one proof; the same payload under a constructor in its target, which pins it no more; and a payload its index does pin, read out where it stands.
const UNPINNED_PAYLOADS: &str = r#"
induct /E: pub Type
end
induct /U: pub Type
| u()
end
induct /W: pub Type
| w(n: #BoolType)
end
induct /Bx: pub Prop
| mk(n: #BoolType)
end
induct /Under: (x: #{node = InductType} /W()) -> pub Prop
| mk(n: #BoolType): (#{node = Variant} /W/w(n))
end
induct /Pin: (n: #BoolType) -> pub Prop
| mk(n: #BoolType): (n)
end
let /T : (b: #BoolType) -> Type =
    (b: #BoolType) =>
        #{result = Ambient(Type)}
        match b
        | false => #{node = InductType} /E()
        | true => #{node = InductType} /U()
        end;
let /read : (p: #{node = InductType} /Bx()) -> #BoolType =
    (p: #{node = InductType} /Bx()) =>
        #{result = Ambient(#BoolType)}
        match p
        | mk(n) => n
        end;
let /readUnder : (x: #{node = InductType} /W(), p: #{node = InductType} /Under()(x)) -> #BoolType =
    (x: #{node = InductType} /W(), p: #{node = InductType} /Under()(x)) =>
        match p: (y, z) => #BoolType
        | mk(n) => n
        end;
let /readPin : (b: #BoolType, p: #{node = InductType} /Pin()(b)) -> #BoolType =
    (b: #BoolType, p: #{node = InductType} /Pin()(b)) =>
        match p: (y, z) => #BoolType
        | mk(n) => n
        end;
((h: (x: /T(/read(#{node = Variant} /Bx/mk(true)))) -> /T(/read(#{node = Variant} /Bx/mk(false)))) =>
    #{result = Ambient(/std/Bool/False)}
    match h(#{node = Variant} /U/u())
    end)(
    (x: /T(/read(#{node = Variant} /Bx/mk(true)))) => x
)
"#;

/// A proposition is taken apart into data only where its index targets pin every payload, a target being the payload itself and nothing built around it: `/read` and `/readUnder` are refused, `/readPin` stands, and the closing term types at `False` once `/read` is admitted.
#[test]
fn a_payload_no_index_pins_is_read_out_of_no_proposition() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${UNPINNED_PAYLOADS}",
        PROPOSITIONS,
        UNPINNED_PAYLOADS
    ));

    assert!(
        matches!(
            &verdicts[..],
            [
                Verdict { name: Some(read), error: Error::LargeElimination(bx) },
                Verdict { name: Some(under), error: Error::LargeElimination(family) },
            ] if *read == global("read")
                && *bx == global("Bx")
                && *under == global("readUnder")
                && *family == global("Under")
        ),
        "{verdicts:?}"
    );
}

/// Coquand and Paulin's family `c(((CP) -> Prop) -> Prop)` at `Type 1` and at `Prop 1`, its twin through an empty type, a parameter whose stated `Strict` its use contradicts, and an occurrence behind a stuck match, a projection of a tuple of types, a `let` and a family applied on the spot; then an infinitely branching tree, which is strictly positive.
const NOT_STRICTLY_POSITIVE: &str = r#"
induct /E: pub Type
end
induct /CP: pub #{level = 1} Type
| c(f: (g: (x: #{node = InductType} /CP()) -> Prop) -> Prop)
end
induct /CPProp: pub #{level = 1} Prop
| c(f: (g: (x: #{node = InductType} /CPProp()) -> Prop) -> Prop)
end
induct /NN: pub Type
| c(f: (g: (x: #{node = InductType} /NN()) -> #{node = InductType} /E()) -> #{node = InductType} /E())
end
#{polarities = [Strict]}
induct /W(X: Type): pub Type
| w(f: (x: X) -> #{node = InductType} /E())
end
induct /UsesW: pub Type
| c(x: #{node = InductType} /W(#{node = InductType} /UsesW()))
end
induct /StuckMatch: pub Type
| c(b: #BoolType, x: #{result = Ambient(Type)} match b | false => #{node = InductType} /E() | true => (y: #{node = InductType} /StuckMatch()) -> #{node = InductType} /E() end)
end
induct /Projected: pub Type
| c(x: ((y: #{node = InductType} /Projected()) -> #{node = InductType} /E(), #NatType).0)
end
induct /LetBound: pub Type
| c(x: (let T: Type = (y: #{node = InductType} /LetBound()) -> #{node = InductType} /E(); T))
end
induct /Applied: pub Type
| c(x: ((T: Type) => (y: T) -> #{node = InductType} /E())(#{node = InductType} /Applied()))
end
induct /Branching: pub Type
| leaf()
| node(f: (n: #NatType) -> #{node = InductType} /Branching())
end
/std/Bool/False
"#;

/// Each family is refused at the polarity it reaches itself at, a `Strict` vector its entry states being read by nothing: a merely positive occurrence, as Cantor's argument needs, at `Pos`; through `/W`'s parameter at the `Neg` the certifier computed; behind a stuck match at `Mixed`; and through what reduction leaves of a projection, a `let` and an application at `Neg`.
#[test]
fn a_family_reaching_itself_through_any_non_strict_shape_is_refused() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${NOT_STRICTLY_POSITIVE}",
        PROPOSITIONS,
        NOT_STRICTLY_POSITIVE
    ));

    for (symbol, reached) in [
        ("CP", Polarity::Pos),
        ("CPProp", Polarity::Pos),
        ("NN", Polarity::Pos),
        ("UsesW", Polarity::Neg),
        ("StuckMatch", Polarity::Mixed),
        ("Projected", Polarity::Neg),
        ("LetBound", Polarity::Neg),
        ("Applied", Polarity::Neg),
    ] {
        assert!(
            refuses(&verdicts, symbol, |error| matches!(
                error,
                Error::NotPositive { name, polarity, .. }
                    if *name == global(symbol) && *polarity == reached
            )),
            "{symbol}: {verdicts:?}"
        );
    }
    for symbol in ["W", "Branching"] {
        assert!(stands(&verdicts, symbol), "{symbol}: {verdicts:?}");
    }
}

/// `A@{u} | a(b: B@{u})` declared before `B@{u} | b(T: Type u)`, which holds its level, an `A@{1}` taken at `A@{0}`, and the `Type 0` stored in it read back out as a `Type 0`: Girard's collapse, were `A` read irrelevant for want of a vector filed for `B` when `A` was judged.
const LEVEL_THROUGH_A_LATER_FAMILY: &str = r#"
#{universe_context = {parameter_count = 1, constraints = []}}
induct /A: pub #{level = u + 1} Type
| a(b: #{node = InductType, universes = {levels = [u]}} /B())
end
#{universe_context = {parameter_count = 1, constraints = []}}
induct /B: pub #{level = u + 1} Type
| b(T: #{level = u} Type)
end
let /lower : (x: #{node = InductType, universes = {levels = [1]}} /A()) -> #{node = InductType, universes = {levels = [0]}} /A() =
    (x: #{node = InductType, universes = {levels = [1]}} /A()) => x;
let /small : Type =
    #{result = Ambient(Type)}
    match
        /lower(
            #{node = Variant, universes = {levels = [1]}}
            /A/a(
                #{node = Variant, universes = {levels = [1]}}
                /B/b(Type)
            )
        )
    | a(b) =>
        #{result = Ambient(Type)}
        match b
        | b(T) => T
        end
    end;
/std/Bool/False
"#;

/// A family no vector is filed for yet is read invariant in every level, so `A` holds the level `B` holds and the coercion is refused; `/small` is refused for nothing of its own.
#[test]
fn a_level_reaching_a_family_declared_later_is_compared_as_that_family_holds_it() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${LEVEL_THROUGH_A_LATER_FAMILY}",
        PROPOSITIONS,
        LEVEL_THROUGH_A_LATER_FAMILY
    ));

    assert!(
        refuses(&verdicts, "lower", |error| matches!(
            error,
            Error::Mismatch { .. }
        )),
        "{verdicts:?}"
    );
    assert!(stands(&verdicts, "small"), "{verdicts:?}");
}

/// An ordinal whose limit quantifies over `Type 0`, declared at `Type 0` and at `Type 1`, a proof recursing on what the limit returns when applied at the ordinal's own type, a proof recursing on a predecessor no arm rules zero out for, and the same recursion under an arm for zero; closed with the second recursion.
const ORDINALS: &str = r#"
induct /O0: pub Type
| l(f: (A: Type) -> (a: A) -> A)
end
induct /O1: pub #{level = 1} Type
| l(f: (A: Type) -> (a: A) -> A)
end
#{node = Rec}
let /down : (o: #{node = InductType} /O1()) -> /std/Bool/False =
    (o: #{node = InductType} /O1()) =>
        match o: (x) => /std/Bool/False
        | l(f) =>
            /down(f(#{node = InductType} /O1())(o))
        end;
let /pred : (n: #NatType) -> #NatType = (n: #NatType) => #NatSub(n, 1);
#{node = Rec}
let /spin : (n: #NatType) -> /std/Bool/False =
    (n: #NatType) => /spin(/pred(n));
#{node = Rec}
let /count : (n: #NatType) -> /std/Bool/True =
    (n: #NatType) =>
        #{result = Ambient(/std/Bool/True)}
        match n
        | 0 => #{node = Variant} /std/Bool/True/qed()
        | _ => /count(/pred(n))
        end;
/spin(0)
"#;

/// The limit is sized under its family, so `/O0` is refused where it is declared and `/O1`'s limit takes no `/O1`; a predecessor read through a definition grades below its argument only under an arm that rules zero out, so `/spin` does not descend and `/count` stands.
#[test]
fn an_ordinal_over_its_own_universe_is_refused_where_it_is_sized_or_applied() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${ORDINALS}",
        PROPOSITIONS,
        ORDINALS
    ));

    assert!(
        refuses(&verdicts, "O0", |error| matches!(
            error,
            Error::Oversized { .. }
        )),
        "{verdicts:?}"
    );
    assert!(
        refuses(&verdicts, "down", |error| matches!(
            error,
            Error::Mismatch { .. }
        )),
        "{verdicts:?}"
    );
    assert!(
        refuses(&verdicts, "spin", |error| matches!(
            error,
            Error::NotDescending { .. }
        )),
        "{verdicts:?}"
    );
    for symbol in ["O1", "count"] {
        assert!(stands(&verdicts, symbol), "{symbol}: {verdicts:?}");
    }
}

/// `Small@{u, v | u + 1 <= v} : Type v = Type u` handed back by `named`, a sibling naming it at the group's own instance, so `named@{0, 0}` would be `Type 0` at `Type 0`; and `shifted`, a member naming its sibling at another instance, `Smaller@{v, v}`.
const A_GROUP_READ_THROUGH_A_SIBLING: &str = r#"
#{node = Rec, universe_context = {parameter_count = 2, constraints = [u + 1 <= v]}}
let /Small : #{level = v} Type = #{level = u} Type
and /named : #{level = v} Type = #{universes = {levels = [u, v]}} /Small;
let /collapse : #{level = 0} Type = #{universes = {levels = [0, 0]}} /named;
let /fits : #{level = 1} Type = #{universes = {levels = [0, 1]}} /named;
#{node = Rec, universe_context = {parameter_count = 2, constraints = [u + 1 <= v]}}
let /Smaller : #{level = v} Type = #{level = u} Type
and /shifted : #{level = v} Type = #{universes = {levels = [v, v]}} /Smaller;
/std/Bool/False
"#;

/// A member named through its sibling is read at the instance a reader states of the declaration that holds its group, held to that declaration's constraints: `named@{0, 0}` is refused for its instance and `named@{0, 1}` stands. A group binds no universe, so inside it a member at any instance but the group's own names no declaration in scope.
///
/// Mutation-checked at `synth_neutral`'s `Instance` arm: with `check_instance` dropped, `/collapse` stands.
#[test]
fn a_member_named_through_its_sibling_is_held_to_its_items_scheme() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_GROUP_READ_THROUGH_A_SIBLING}",
        PROPOSITIONS,
        A_GROUP_READ_THROUGH_A_SIBLING
    ));

    assert!(
        refuses(&verdicts, "collapse", |error| matches!(
            error,
            Error::UniverseInstance { .. }
        )),
        "{verdicts:?}"
    );
    assert!(
        refuses(&verdicts, "Smaller", |error| matches!(
            error,
            Error::Undeclared(name) if *name == global("Smaller")
        )),
        "{verdicts:?}"
    );
    assert!(stands(&verdicts, "Small"), "{verdicts:?}");
    assert!(stands(&verdicts, "fits"), "{verdicts:?}");
}

/// `A`'s case holds a `B` at a λ that takes an `A` apart and then the `B` proof inside it, whose case pins its index to the function it carries: typing the payload types the λ, its inner arm solves `k` to the λ itself, and binding the solution types the λ again.
const A_FAMILY_WHOSE_PAYLOAD_TYPES_ITSELF_THROUGH_AN_INDEX: &str = r#"
induct /B(T: Prop): (x: T) -> pub Prop
| c(k: T): (k)
end
induct /A: pub Prop
| mk(b: #{node = InductType} /B((a: #{node = InductType} /A()) -> #{node = InductType} /std/Bool/False())(
    (a: #{node = InductType} /A()) =>
        #{result = Ambient(#{node = InductType} /std/Bool/False())}
        match a
        | mk(b2) =>
            #{result = Ambient(#{node = InductType} /std/Bool/False())}
            match b2
            | c(k) =>
                k(a)
            end
        end
))
end
/std/Bool/True/qed()
"#;

/// [`A_FAMILY_WHOSE_PAYLOAD_TYPES_ITSELF_THROUGH_AN_INDEX`] with `A` given a parameter `P` its λ answers at, so the λ names a local: the family's parameter, which stands as it stood at every turn of the regress.
const A_FAMILY_WHOSE_PAYLOAD_TYPES_ITSELF_OVER_ITS_PARAMETER: &str = r#"
induct /B(T: Prop): (x: T) -> pub Prop
| c(k: T): (k)
end
induct /A(P: Prop): pub Prop
| mk(b: #{node = InductType} /B((a: #{node = InductType} /A(P)) -> P)(
    (a: #{node = InductType} /A(P)) =>
        #{result = Ambient(P)}
        match a
        | mk(b2) =>
            #{result = Ambient(P)}
            match b2
            | c(k) =>
                k(a)
            end
        end
))
end
/std/Bool/True/qed()
"#;

/// [`A_FAMILY_WHOSE_PAYLOAD_TYPES_ITSELF_THROUGH_AN_INDEX`] with `B` declared in `A`'s group and indexed by a function of `A`, which the scan before the walk refuses as indexed by its group, and the walk judges all the same.
const A_GROUP_WHOSE_PAYLOAD_TYPES_ITSELF_THROUGH_AN_INDEX: &str = r#"
induct /A: pub Prop
| mk(b: #{node = InductType} /B()(
    (a: #{node = InductType} /A()) =>
        #{result = Ambient(#{node = InductType} /std/Bool/False())}
        match a
        | mk(b2) =>
            #{result = Ambient(#{node = InductType} /std/Bool/False())}
            match b2
            | c(k) =>
                k(a)
            end
        end
))
end
and induct /B: (f: (a: #{node = InductType} /A()) -> #{node = InductType} /std/Bool/False()) -> pub Prop
| c(k: (a: #{node = InductType} /A()) -> #{node = InductType} /std/Bool/False()): (k)
end
/std/Bool/True/qed()
"#;

// A term's typing reads the definition store and the binders its locals stand at, so one that needs itself while it is in progress, its locals standing as they stood, needs itself again at every turn: a derivation resting on its own conclusion is none, and the walk refuses it by name. Each module here types the λ inside its own typing, the last over the family's parameter, and the walk followed the regress a level a turn, its stack grown on the heap, without end.
#[test]
fn a_typing_that_needs_itself_is_refused() {
    for (module, family) in [
        (A_FAMILY_WHOSE_PAYLOAD_TYPES_ITSELF_THROUGH_AN_INDEX, "A"),
        (A_GROUP_WHOSE_PAYLOAD_TYPES_ITSELF_THROUGH_AN_INDEX, "A"),
        (A_FAMILY_WHOSE_PAYLOAD_TYPES_ITSELF_OVER_ITS_PARAMETER, "A"),
    ] {
        let verdicts =
            core_text_verdicts(&fill!("${PROPOSITIONS}\n${module}", PROPOSITIONS, module));

        assert!(
            refuses(&verdicts, family, |error| matches!(
                error,
                Error::NeedsItsOwnTyping(_)
            )),
            "{verdicts:?}"
        );
    }
}
