//! A concept field that carries a type: chosen by a witness or an explicit dictionary, and unfolded through its projection.

use crate::tests::run;

// A `Type`-sorted field and a `Type`-returning method both spell `Type` in the field type's result spine. The method-wrapper signature lowers that span under the record's role — `input_type`'s lexical `Generalizable` — rather than at the default `Flexible` its output position would give, since one universe seed cannot take two roles; so the associated type registers, resolves through the table, and its projection unfolds definitionally: `v : alias` checks `3` against `Nat`, and `b : picked` checks `true` against `Bool`.
#[test]
fn a_concept_field_may_carry_a_type() {
    let source = r#"
        use /std/{Nat, Bool, Str};
        pub concept Sized(A : Type) : pub Type {
            Carrier : Type,
            pick() -> Type,
        }
        satisfy Sized(Nat) {
            Carrier = Nat,
            pick() = Bool,
        }
        let alias : Type = Sized/Carrier(@Nat);
        let v : alias = 3;
        let picked : Type = Sized/pick(@Nat);
        let b : picked = true;
        let _ = /std/print(Nat/to_str(v))!;
        match b
        | true => /std/print("t")
        | false => /std/print("f")
        end
        "#;

    assert_eq!(run(source), b"3t");
}

// The higher-kinded twin exercises the same wrapper machinery with the dictionary supplied explicitly. The explicit `use` is deliberate: it pins the wrapper, the projection, and the definitional unfolding without depending on table resolution for a written type-former argument.
#[test]
fn a_higher_kinded_type_field_projects_through_an_explicit_dictionary() {
    let source = r#"
        use /std/{Nat, Option, Str};
        pub concept Named(M : (Type) -> Type) : pub Type {
            Carrier : Type,
        }
        let dict : Named(Option) = Named { Carrier = Nat };
        let alias : Type = Named/Carrier(@Option, use dict);
        let v : alias = 3;
        /std/print(Nat/to_str(v))
        "#;

    assert_eq!(run(source), b"3");
}
