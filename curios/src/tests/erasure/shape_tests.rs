//! A structural type's runtime shape is its telescope's: a function takes a parameter for each binder and an anonymous tuple holds a field for each member, whatever a binder's type is read as. Each program makes a value at one reading of its type and uses it at another, a substitution or a refinement apart, where one reads a binder as a type and the other as a variable. With a binder dropped by either reading, the first three print `1`, the fourth stops the compiler in `curios-cont`'s verifier, and the tuple traps.
//!
//! A nominal type's runtime shape is its declaration's row, at a reading that erases a field the declaration keeps as at any other.

use crate::tests::run;

// The lambda is written at `(Type, Nat) -> Nat`, with `A` the caller's `Type`, and called at `(A, Nat) -> Nat`.
#[test]
fn a_function_written_at_a_universe_is_called_at_a_variable() {
    let source = r#"
        use /std/{Nat};
        let apply(@A: Type, h: (A, Nat) -> Nat, a: A) -> Nat = h(a, 41);
        /std/print(Nat/to_str(apply((t, n) => n + 1, Nat)))
        "#;
    assert_eq!(run(source), b"42");
}

// The same with a proposition where the type went, the callee leaving its variable's sort to the caller: a proof is a value of that variable to the callee, and fills the parameter a type's value would.
#[test]
fn a_function_written_at_a_proposition_is_called_at_a_variable() {
    let source = r#"
        use /std/{Nat};
        use /std/Bool/{True};
        let apply(@A: Sort, h: (A, Nat) -> Nat, a: A) -> Nat = h(a, 41);
        /std/print(Nat/to_str(apply((p, n) => n + 1, True/qed())))
        "#;
    assert_eq!(run(source), b"42");
}

// The other way about: the function is written at a variable, inside the definition that hands it back, and called at the universe the caller put there.
#[test]
fn a_function_written_at_a_variable_is_called_at_a_universe() {
    let source = r#"
        use /std/{Nat};
        let make(@A: Type, a: A) -> (A, Nat) -> Nat = (b, n) => n + 1;
        /std/print(Nat/to_str(make(Nat)(Nat, 41)))
        "#;
    assert_eq!(run(source), b"42");
}

// A domain a type-level `match` computes is a universe at one argument and stuck at a variable.
#[test]
fn a_function_typed_by_a_computed_domain_is_called_at_its_stuck_form() {
    let source = r#"
        use /std/{Nat};
        let Sel(n: Nat) -> Type =
            match n: (_) => Type | 0 => Nat | k + 1 => Type end;
        let f(x: Nat, keep: (b: Sel(x), n: Nat) -> Nat, a: Sel(x)) -> Nat = keep(a, 41);
        /std/print(Nat/to_str(f(1, (b, n) => n + 1, Nat)))
        "#;
    assert_eq!(run(source), b"42");
}

// The same domain read under an arm's refinement, which makes it a universe at the call and leaves it stuck where the function is written.
#[test]
fn a_function_written_outside_an_arm_is_called_under_its_refinement() {
    let source = r#"
        use /std/{Nat};
        let Sel(n: Nat) -> Type =
            match n: (_) => Type | 0 => Nat | k + 1 => Type end;
        let f(x: Nat, keep: (b: Sel(x), n: Nat) -> Nat, a: Sel(x)) -> Nat =
            match x | 0 => 0 | xp + 1 => keep(a, 41) end;
        let outer(x: Nat, a: Sel(x)) -> Nat = f(x, (b, n) => n + 1, a);
        /std/print(Nat/to_str(outer(1, Nat)))
        "#;
    assert_eq!(run(source), b"42");
}

// A tuple built at `{Type, Nat}` and read at `{A, Nat}`: its second field is its second field at both.
#[test]
fn a_tuple_built_at_a_universe_is_read_at_a_variable() {
    let source = r#"
        use /std/{Nat};
        let second(@A: Type, p: {A, Nat}) -> Nat = p.1;
        /std/print(Nat/to_str(second((Nat, 41))))
        "#;
    assert_eq!(run(source), b"41");
}

// A tuple holding a proof beside a number is two fields to a reader that knows the first only as a variable at a sort its caller states.
#[test]
fn a_tuple_built_at_a_proposition_is_read_at_a_variable() {
    let source = r#"
        use /std/{Nat};
        use /std/Bool/{True};
        let second(@A: Sort, p: {A, Nat}) -> Nat = p.1;
        /std/print(Nat/to_str(second((True/qed(), 41))))
        "#;
    assert_eq!(run(source), b"41");
}

// A structure's layout is its declaration's, which every site reads; a function it holds is a structural value all the same.
#[test]
fn a_function_held_in_a_structure_is_called_at_a_variable() {
    let source = r#"
        use /std/{Nat};
        struct Runner(A: Type): Type { run: (A, Nat) -> Nat }
        let call(@A: Type, runner: Runner(A), a: A) -> Nat = runner.run(a, 41);
        /std/print(Nat/to_str(call(Runner { run = (t, n) => n + 1 }, Nat)))
        "#;
    assert_eq!(run(source), b"42");
}

// `S(P)` erases `x`, `A` being the proposition `P` there, and a value of it is the two fields its declaration keeps all the same. Read off the reading instead, `S(P)` would be its one number, so the family holding one would take the encoding that tells a number from its other case, and `a` would be matched as `b`, printing `7`.
#[test]
fn a_structure_is_its_declarations_row_where_a_reading_erases_a_field() {
    let source = r#"
        use /std/{Nat, List, Process};
        struct S(A: Sort): Type {
            x: A,
            y: Nat,
        }
        induct P: Prop
        | p()
        end
        induct U: Type
        | a(s: S(P))
        | b()
        end
        let pick(u: U) -> Nat =
            match u: (_) => Nat
            | a(s) => s.y
            | b() => 7
            end;
        let entry() -> /std/Io({}) =
            let n = List/len(Process/args()!);
            /std/print(Nat/to_str(pick(U/a(S(P) { x = P/p(), y = n + 40 }))));
        entry()
        "#;
    assert_eq!(run(source), b"40");
}
