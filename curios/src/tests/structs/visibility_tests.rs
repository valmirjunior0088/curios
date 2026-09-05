//! What a representation exposes and to whom: private construction and projection, public signatures, and the plumbing user code may not reach.

use crate::tests::{error, run};

// Constructing a private-representation struct from outside its declaring module is rejected (`PrivateRepresentation`).
#[test]
fn struct_private_construction_rejected() {
    let source = r#"
        use /std/{Nat};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius : Type { Nat }
        end
        let c : Celsius/Celsius = Celsius/Celsius { 42 };
        /std/print("no")
        "#;

    let error = error(source);
    assert!(
        error.contains("representation"),
        "unexpected error: {error}"
    );
}

// Projecting a private-representation struct's field from outside its module is rejected (`PrivateField`), even when the value was obtained legitimately.
#[test]
fn struct_private_projection_rejected() {
    let source = r#"
        use /std/{Nat};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius : Type { Nat }
            pub let of_nat(n : Nat) -> Celsius = Celsius { n };
        end
        let c : Celsius/Celsius = Celsius/of_nat(42);
        /std/print(Nat/to_str(c.0))
        "#;

    let error = error(source);
    assert!(
        error.contains("field") && error.contains("private"),
        "unexpected error: {error}"
    );
}

// A `pub` item's signature may not expose a private sibling: the reference resolves through lexical scope (no publicness walk), so a dedicated interface audit closes the gap.
#[test]
fn pub_signature_exposing_private_sibling_is_rejected() {
    let source = r#"
        use /std/{Nat};
        mod M
            use /std/{Nat};
            struct Secret : Type { Nat }
            pub let f(s : Secret) -> Nat = 1;
        end
        /std/print("no")
        "#;

    let error = error(source);
    assert!(
        error.contains("exposes private item '/M/Secret'"),
        "unexpected error: {error}"
    );
}

// The other privately-resolvable path: an item inside the module's own private child (the head segment resolves lexically, so resolution never checked the child's visibility). `T` reaches only `M`'s subtree while `g` reaches the whole program, so the audit names `T` itself rather than the first private hop on the way to it.
#[test]
fn pub_signature_exposing_private_child_module_is_rejected() {
    let source = r#"
        use /std/{Nat};
        mod M
            mod Inner
                use /std/{Nat};
                pub struct T : pub Type { n : Nat }
            end
            use Inner/{T};
            pub let g(t : T) -> T = t;
        end
        /std/print("no")
        "#;

    let error = error(source);
    assert!(
        error.contains("exposes private item '/M/Inner/T'"),
        "unexpected error: {error}"
    );
}

// A transparent pub concept's field types are interface (its representation is public), superclass edges included.
#[test]
fn pub_concept_with_private_superclass_is_rejected() {
    let source = r#"
        use /std/{Nat, Bool};
        mod M
            use /std/{Bool};
            concept Hidden(A : Type) : pub Type {
                h(A) -> Bool
            }
            pub concept Loud(A : Type) : pub Type {
                use Hidden(A),
                l(A) -> Bool
            }
        end
        /std/print("no")
        "#;

    let error = error(source);
    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

// A *sealed* pub concept's fields are not interface — a private superclass is a hidden implementation obligation, discharged by resolution without the consumer ever naming it (the sealed-trait-with-private-supertrait idiom).
#[test]
fn sealed_pub_concept_with_private_superclass_is_accepted() {
    let source = r#"
        use /std/{Nat, Bool};
        mod M
            use /std/{Bool};
            concept Hidden(A : Type) : Type {
                h(A) -> Bool
            }
            pub concept Loud(A : Type) : Type {
                use Hidden(A),
                l(A) -> Bool
            }
        end
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// A pub inductive's constructors are its interface: a private payload type is rejected (the `Async`/`Pause` shape).
#[test]
fn pub_inductive_with_private_payload_type_is_rejected() {
    let source = r#"
        use /std/{Nat};
        mod M
            induct Secret : Type
            | mk()
            end
             pub induct Box : pub Type
            | wrap(Secret)
            end
        end
        /std/print("no")
        "#;

    let error = error(source);
    assert!(
        error.contains("exposes private item '/M/Secret'"),
        "unexpected error: {error}"
    );
}

// An opaque struct's field types are not interface, while an inner-`pub` struct's fields are.
#[test]
fn hidden_struct_fields_are_not_interface_but_exposed_fields_are() {
    let hidden = r#"
        use /std/{Nat};
        mod M
            use /std/{Nat};
            struct Secret : pub Type { n : Nat }
            pub struct Opaque : Type { Secret }
            pub let mk() -> Opaque = Opaque { Secret { n = 1 } };
        end
        let o = M/mk();
        /std/print("ok")
        "#;
    assert_eq!(run(hidden), b"ok");

    let exposed = r#"
        use /std/{Nat};
        mod M
            use /std/{Nat};
            struct Secret : pub Type { n : Nat }
            pub struct Open : pub Type { s : Secret }
        end
        /std/print("no")
        "#;
    let error = error(exposed);
    assert!(
        error.contains("exposes private item '/M/Secret'"),
        "unexpected error: {error}"
    );
}

// A private representation is transparent within its declaring module's subtree: a descendant may construct and project it, so an abstraction can be implemented across several files without exporting its representation.
#[test]
fn struct_private_representation_open_in_descendant() {
    let source = r#"
        use /std/{Nat};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius : Type { Nat }
            pub mod Build
                use /std/{Nat};
                use /Celsius/{Celsius};
                pub let of_nat(n : Nat) -> Celsius = Celsius { n };
                pub let to_nat(c : Celsius) -> Nat = c.0;
            end
        end
        /std/print(Nat/to_str(Celsius/Build/to_nat(Celsius/Build/of_nat(42))))
        "#;

    assert_eq!(run(source), b"42");
}

// The relaxation is downward only. A sibling subtree is outside the declaring module, so its representation stays opaque there.
#[test]
fn struct_private_representation_closed_to_siblings() {
    let source = r#"
        use /std/{Nat};
        mod Owner
            pub mod Celsius
                use /std/{Nat};
                pub struct Celsius : Type { Nat }
            end
            pub mod Other
                use /std/{Nat};
                use /Owner/Celsius/{Celsius};
                pub let of_nat(n : Nat) -> Celsius = Celsius { n };
            end
        end
        /std/print("no")
        "#;

    let error = error(source);
    assert!(
        error.contains("representation"),
        "unexpected error: {error}"
    );
}

// An opaque inductive is eliminable throughout its declaring subtree, matching the struct rule — the whole family of eliminators moves together.
#[test]
fn opaque_inductive_is_eliminable_in_a_descendant() {
    let source = r#"
        use /std/{Nat, Bool};
        mod Flag
            use /std/{Nat, Bool};
            pub induct Flag : Type
            | on()
            | off()
            end
            pub let on : Flag = Flag/on();
            pub mod Read
                use /std/{Nat};
                use /Flag/{Flag};
                pub let to_nat(f : Flag) -> Nat =
                    match f
                    | on() => 42
                    | off() => 0
                    end;
            end
        end
        /std/print(Nat/to_str(Flag/Read/to_nat(Flag/on)))
        "#;

    assert_eq!(run(source), b"42");
}

// `/std/Async` keeps `Future` and `Waker` as private child modules and re-exports only the two type names, so a program can name a `Future` but cannot reach the scheduler plumbing that drives one.
#[test]
fn async_future_plumbing_is_not_reachable_from_user_code() {
    let source = r#"
        use /std/{Nat, Async};
        let f = /std/Async/Future/new();
        /std/print("no")
        "#;

    let error = error(source);
    assert!(
        error.contains("private child module"),
        "unexpected error: {error}"
    );
}
