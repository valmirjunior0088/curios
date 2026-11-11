//! A family the walk has registered and not yet judged, read by an item stored before it: every family of a module is registered before any is judged, so an item stored ahead of a family reads its entry as data, and each fact it reads is one the family's own judgment holds the entry to, whose refusal refuses the module, or the refusing answer until that judgment files its own — no mark on a payload, invariance where no vector is filed, and a name not yet defined standing for nothing.

use {super::test_support::*, crate::*, curios_common::fill};

/// `text` after the propositions it names, put at `False`.
fn verdicts_of(text: &str) -> Vec<Verdict> {
    core_text_verdicts(&fill!("${PROPOSITIONS}\n${text}", PROPOSITIONS, text))
}

/// Whether `name` was refused for a conversion that did not hold.
fn mismatched(verdicts: &[Verdict], name: &str) -> bool {
    verdicts.iter().any(|verdict| {
        verdict.name == Some(global(name)) && matches!(verdict.error, Error::Mismatch { .. })
    })
}

/// `E : (x: F(0)) -> False`, a match with no arm, stored before `D : Nat = 0` and `induct F : (n: Nat) -> Type | c() : (D)`, and closed with `E(c())`: the case `c` is excused only where its target `D` clashes with `0`, and `c()` stands at `F(D)`, which is `F(0)` once `D` is defined.
const A_TARGET_NAMING_A_LATER_DEFINITION: &str = r#"
let /E : (x: #{node = InductType} /F()(0)) -> /std/Bool/False =
    (x: #{node = InductType} /F()(0)) =>
        #{result = Ambient(/std/Bool/False)}
        match x
        end;
let /D : #NatType = 0;
induct /F: (n: #NatType) -> pub Type
| c(): (/D)
end
/E(
    #{node = Variant}
    /F/c()
)
"#;

// Where `E` is judged `D` is not yet defined, so inversion meets a name it cannot reduce against `0` and concludes nothing: the arm is owed, and `E` is refused for it alone, the closing term typing at `False` against `E` standing at its type. With `D` defined as `1` before `E`, the arm is excused and the closing term is refused instead, at `F(D)` against `F(0)`.
//
// Mutation-check: have `curios-analysis`'s `unify_node` answer `Clash` for a name against a literal, and the module is admitted whole.
#[test]
fn a_case_whose_target_names_a_definition_stored_after_the_match_is_owed_its_arm() {
    let verdicts = verdicts_of(A_TARGET_NAMING_A_LATER_DEFINITION);

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(name), error: Error::MissingArm { family, .. } }]
                if *name == global("E") && *family == global("F")
        ),
        "{verdicts:?}"
    );
}

/// `E : (a: S, b: S, h: Holds(a.0 == 0)) -> Holds(b.0 == 0) = h`, stored before `struct S : Prop { n: Nat }` and closed with `E(S { 0 }, S { 1 }, qed)`, a proof of `Holds(1 == 0)`: `E` reads the sort `S` declares and holds any two of its values one by irrelevance, so a fact about one's field is a fact about the other's.
const IRRELEVANCE_BEFORE_A_PROP_RECORD: &str = r#"
let /E : (
    a: #{node = StructType} /S(),
    b: #{node = StructType} /S(),
    h: /std/Bool/Holds(#NatEql(#{field = Struct(/S, 0)} a.0, 0))
) -> /std/Bool/Holds(#NatEql(#{field = Struct(/S, 0)} b.0, 0)) =
    (
        a: #{node = StructType} /S(),
        b: #{node = StructType} /S(),
        h: /std/Bool/Holds(#NatEql(#{field = Struct(/S, 0)} a.0, 0))
    ) => h;
struct /S: pub Prop { n: #NatType }
/E(/S() { 0 }, /S() { 1 }, #{node = Variant} /std/Bool/True/qed())
"#;

// `E` is admitted on the sort `S` declares, and the closing term with it: what refuses the module is `S`'s own judgment, which holds a `Prop` record to fields that are proofs wherever the record is stored.
//
// Mutation-check: have `check_non_informative` read no field, and the module is admitted whole.
#[test]
fn a_prop_record_read_by_irrelevance_before_it_is_judged_is_refused_for_its_informative_field() {
    let verdicts = verdicts_of(IRRELEVANCE_BEFORE_A_PROP_RECORD);

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(name), error: Error::Informative { .. } }] if *name == global("S")
        ),
        "{verdicts:?}"
    );
}

/// `E : (p: P(5)) -> Bool = match p | mk(b) => b end`, the singleton rung taking a proposition apart into data, stored before `induct P : (n: Nat) -> Prop | mk(b: Bool) : (b)`: `mk`'s one payload is its own target, so the match reads it off the index, a `Nat`, where the payload is a `Bool`.
const A_SINGLETON_READ_BEFORE_ITS_TARGET_IS_TYPED: &str = r#"
let /E : (p: #{node = InductType} /P()(5)) -> #BoolType =
    (p: #{node = InductType} /P()(5)) =>
        #{result = Ambient(#BoolType)}
        match p
        | mk(b) => b
        end;
induct /P: (n: #NatType) -> pub Prop
| mk(b: #BoolType): (b)
end
/E
"#;

// Two refusals, each standing alone: the arm binds `b` to `5` at the solution's own type, so `E` is refused for the body it hands back at `Bool`; and `P` is refused for a target its index does not type, which its own judgment checks wherever it is stored.
#[test]
fn a_singleton_read_off_an_index_before_its_family_types_the_target_is_refused_at_both() {
    let verdicts = verdicts_of(A_SINGLETON_READ_BEFORE_ITS_TARGET_IS_TYPED);

    assert!(mismatched(&verdicts, "E"), "{verdicts:?}");
    assert!(mismatched(&verdicts, "P"), "{verdicts:?}");
}

/// `E : Holds((match c(2 + 3, qed) | c(n, h) => n end) == 5) = qed`, stored before `induct F : Type | c(n: Nat, h: True) end`, whose closed run takes `F`'s value apart while `F` states no payload a proof, and closed with `E`, a proof of `True`.
const A_RUN_BEFORE_THE_MARKS_ARE_STATED: &str = r#"
let /E : /std/Bool/Holds(
    #NatEql(
        #{result = Ambient(#NatType)}
        match
            #{node = Variant}
            /F/c(#NatAdd(2, 3), #{node = Variant} /std/Bool/True/qed())
        | c(n, h) => n
        end,
        5
    )
) =
    #{node = Variant}
    /std/Bool/True/qed();
induct /F: pub Type
| c(n: #NatType, h: #{node = InductType} /std/Bool/True())
end
/E
"#;

/// [`A_RUN_BEFORE_THE_MARKS_ARE_STATED`] with `F` stored first, so its run binds the proof `h` as written.
const A_RUN_AFTER_THE_MARKS_ARE_STATED: &str = r#"
induct /F: pub Type
| c(n: #NatType, h: #{node = InductType} /std/Bool/True())
end
let /E : /std/Bool/Holds(
    #NatEql(
        #{result = Ambient(#NatType)}
        match
            #{node = Variant}
            /F/c(#NatAdd(2, 3), #{node = Variant} /std/Bool/True/qed())
        | c(n, h) => n
        end,
        5
    )
) =
    #{node = Variant}
    /std/Bool/True/qed();
/E
"#;

// Stored before `F`, the run binds every payload reduced, `h` among them; stored after, it binds `h` as written. A mark decides no reduct, so the item is judged alike in both, and the one refusal is the closing term's, a proof of `True` put at `False`. No mutation is claimed: no mark the walk could state moves this verdict, which is what the test holds.
#[test]
fn a_run_through_a_family_before_its_marks_are_stated_is_judged_as_one_after() {
    let before = verdicts_of(A_RUN_BEFORE_THE_MARKS_ARE_STATED);
    let after = verdicts_of(A_RUN_AFTER_THE_MARKS_ARE_STATED);

    assert!(
        matches!(
            &before[..],
            [Verdict {
                name: None,
                error: Error::Mismatch { .. }
            }]
        ),
        "{before:?}"
    );
    assert_eq!(before, after);
}

/// `Cast.{u, v} : (x: Box.{v}) -> Box.{u} = (x) => x`, stored before `induct Box.{u} : Type (u + 1) | box(T: Type u) end`, which carries a vector calling its level irrelevant: admitted, `box(Type 0) : Box.{1}` is read back at `Box.{0}`, which then holds `Type 0` among the types of `Type 0`.
const A_CAST_BEFORE_THE_VECTOR_IS_FILED: &str = r#"
#{universe_context = {parameter_count = 2}}
let /Cast : (x: #{node = InductType, universes = {levels = [v]}} /Box()) -> #{node = InductType, universes = {levels = [u]}} /Box() =
    (x: #{node = InductType, universes = {levels = [v]}} /Box()) => x;
#{universe_context = {parameter_count = 1}, variances = [Irrelevant]}
induct /Box: pub
    #{level = u + 1}
    Type
| box(
    T:
        #{level = u}
        Type
)
end
(#{universes = {levels = [0, 1]}} /Cast)
"#;

// Where `Cast` is judged no walk has filed `Box`'s vector, and a family with none compares every level: the cast is refused, whatever the entry carries, and `Box` is denied the irrelevance it carries where it is declared. `variance_tests` holds the same lie with `Box` stored first, where the vector the walk filed is what refuses the cast.
//
// Mutation-check: have `Environment::variance` answer from the vector an entry carries where no walk filed one, and `Cast` is admitted.
#[test]
fn a_comparison_before_a_familys_vector_is_filed_is_invariant_whatever_its_entry_carries() {
    let verdicts = verdicts_of(A_CAST_BEFORE_THE_VECTOR_IS_FILED);

    assert!(mismatched(&verdicts, "Cast"), "{verdicts:?}");
    assert!(
        verdicts.iter().any(|verdict| verdict.error
            == Error::VarianceDenied {
                name: global("Box"),
                level: 0
            }),
        "{verdicts:?}"
    );
}

/// `E : Type 1 = F.{1, 0}`, stored before `induct F.{u, v | u + 1 <= v} : Type v | mk(A: Type u) end`: admitted, `F.{1, 0}` holds a `Type 1` and stands at `Type 0`.
const AN_INSTANCE_BEFORE_ITS_SCHEME_IS_JUDGED: &str = r#"
let /E : #{level = 1} Type = #{node = InductType, universes = {levels = [1, 0]}} /F();
#{universe_context = {parameter_count = 2, constraints = [u + 1 <= v]}}
induct /F: pub
    #{level = v}
    Type
| mk(
    A:
        #{level = u}
        Type
)
end
/E
"#;

// The instance is held to the scheme the entry states where `E` reads it, before `F`'s own judgment: `2 <= 0` does not hold, and `E` is refused for it.
//
// Mutation-check: have `Certifier::check_instance` check an instance's width alone, and `E` is admitted.
#[test]
fn an_instance_of_a_family_stored_after_it_is_held_to_the_familys_scheme() {
    let verdicts = verdicts_of(AN_INSTANCE_BEFORE_ITS_SCHEME_IS_JUDGED);

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("E"))
                && matches!(verdict.error, Error::UniverseInstance { .. })),
        "{verdicts:?}"
    );
}

/// `rec loop(x: F) : False = match x | c(f) => loop(f(F)(x)) end`, stored before `induct F : Type | c(f: (A: Type) -> (a: A) -> A) end`, and closed with `loop(c((A) => (a) => a))`: the size order reads `f(F)(x)`, an application of a payload of `x`'s case, as a child of `x`, and a payload quantifying over the sort `F` stands at may hand `x` back.
const A_DESCENT_THROUGH_A_LATER_OVERSIZED_PAYLOAD: &str = r#"
#{node = Rec}
let /loop : (x: #{node = InductType} /F()) -> /std/Bool/False =
    (x: #{node = InductType} /F()) =>
        #{result = Ambient(/std/Bool/False)}
        match x
        | c(f) => /loop(f(#{node = InductType} /F())(x))
        end;
induct /F: pub Type
| c(f: (A: Type) -> (a: A) -> A)
end
/loop(
    #{node = Variant}
    /F/c((A: Type) => (a: A) => a)
)
"#;

// `loop` descends as the size order reads `F`'s entry where it is judged, and the closing term types at `False`: what refuses the module is `F`'s own judgment, which sizes its payload against the sort it stands at wherever it is stored.
//
// Mutation-check: have `check_signature` skip its size condition, and the module is admitted whole.
#[test]
fn a_descent_through_a_family_stored_after_the_group_is_refused_at_the_familys_size() {
    let verdicts = verdicts_of(A_DESCENT_THROUGH_A_LATER_OVERSIZED_PAYLOAD);

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(name), error: Error::Oversized { .. } }] if *name == global("F")
        ),
        "{verdicts:?}"
    );
}

/// `struct A { g: (x: B) -> E }`, declared before `induct B | b(x: A) end`, closed with Curry's paradox and no recursion: `sa` takes a `B` apart to the record it holds and applies the record's function to the `B` itself.
const CURRY_THROUGH_A_RECORD_DECLARED_BEFORE: &str = r#"
induct /E: pub Type
end
struct /A: pub Type { g: (x: #{node = InductType} /B()) -> #{node = InductType} /E() }
induct /B: pub Type
| b(x: #{node = StructType} /A())
end
((sa: (x: #{node = InductType} /B()) -> #{node = InductType} /E()) =>
    #{result = Ambient(/std/Bool/False)}
    match sa(
        #{node = Variant}
        /B/b(/A() { sa })
    )
    end)(
    (x: #{node = InductType} /B()) =>
        #{result = Ambient(#{node = InductType} /E())}
        match x
        | b(y) => (#{field = Struct(/A, 0)} y.0)(x)
        end
)
"#;

// The cycle the formation ticket for two families declared apart closes, through a record: `A` reaches `B`, which no walk has judged where `A` is, so the occurrence reads as reaching `A` again at `Mixed`, and `B` reaches `A` through the record's function. Each is refused for its own part.
//
// Mutation-check: have the certifier's `Env::judged` answer yes for every family, and the module is admitted whole.
#[test]
fn a_record_and_a_family_declared_apart_that_hold_each_other_negatively_are_each_refused() {
    let verdicts = verdicts_of(CURRY_THROUGH_A_RECORD_DECLARED_BEFORE);

    assert!(
        matches!(
            &verdicts[..],
            [
                Verdict { name: Some(a), error: Error::NotPositive { .. } },
                Verdict { name: Some(b), error: Error::NotPositive { .. } },
            ] if *a == global("A") && *b == global("B")
        ),
        "{verdicts:?}"
    );
}
