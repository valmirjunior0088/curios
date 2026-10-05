//! `!` sequencing through a user monad witness, including a two-parameter region.

use {
    crate::tests::run,
    curios_core::Zonked,
    curios_pipeline::{DEFAULT_STEP_BUDGET, recheck_with_prelude, typecheck_with_prelude},
    curios_text::{Entrypoint, RootSource},
};

// The List witness: bind is concat-map.
#[test]
fn prelude_monad_arr_binds() {
    let source = r#"
        use /std/{Nat, Str, List, Monad};
        let l : List(Nat) = [1, 2];
        let doubled : List(Nat) = Monad/bind(l, (x) => [x, x]);
        /std/print(Nat/to_str(List/len(doubled)))
        "#;

    assert_eq!(run(source), b"4");
}

// The monadic sugar: each `e!` desugars to `/std/Monad/bind(e, cont)`, whose `use` binder resolves the `Monad` witness from the action's type — no header, no imports needed for the dispatch itself.
#[test]
fn monadic_sugar_binds_through_the_concept() {
    let source = r#"
        use /std/{Nat, Str, Option, Monad};
        pub let chain(a : Option(Nat), b : Option(Nat)) -> Option(Nat) =
            let x = a!;
            let y = b!;
            Monad/pure(Nat/add(x, y));
        /std/print(Nat/to_str(Option/unwrap_or(chain(Option/some(20), Option/some(22)), 0)))
        "#;

    assert_eq!(run(source), b"42");
}

// Generic do-notation: `!` inside a function that is generic over the monad. Each site's `Monad(M)` goal (M a bound variable) resolves against the local `use` binder — impossible with a concrete bind function, and the payoff of dispatching `!` through the concept.
#[test]
fn bang_works_in_monad_generic_code() {
    let source = r#"
        use /std/{Monad};
        use /std/{Nat, Str, Option, List};
        pub let add_both(@M : (Type) -> Type, use Monad(M), a : M(Nat), b : M(Nat)) -> M(Nat) =
            Monad/pure(a! + b!);
        let o : Option(Nat) = add_both(Option/some(20), Option/some(22));
        let l : List(Nat) = add_both([1, 2], [10]);
        /std/print(Str/concat(
            Nat/to_str(Option/unwrap_or(o, 0)),
            Nat/to_str(List/len(l))))
        "#;

    assert_eq!(run(source), b"422");
}

// The use side of the partial family: a `!` inside a `Box(Str, Nat)` region pins the bind's monad by right-biased partial imitation (`?M := (A) => Box(Str, A)`), which the parametric witness then answers.
#[test]
fn a_bang_sequences_in_a_two_parameter_monad_region() {
    let source = r#"
        use /std/{Nat, Str, Monad};
        induct Box(S : Type, A : Type) : Type
        | wrap(A)
        end
        satisfy (@S : Type) => Monad((A : Type) => Box(S, A)) {
            pure(@A, a) = Box/wrap(a),
            bind(@A, @B, m, f) =
                match m : (_) => Box(S, B)
                | wrap(a) => f(a)
                end,
        }
        pub let prog : Box(Str, Nat) =
            let v = Monad/pure(3)!;
            Monad/pure(Nat/add(v, v));
        let out =
            match prog : (_) => Nat
            | wrap(value) => value
            end;
        /std/print(Nat/to_str(out))
        "#;

    assert_eq!(run(source), b"6");
}

/// Three ways to sequence a `Result(Str, Type)`, whose payload is a type and so sits a level above the types it names: `walk` with `!` in an arm, `flat` with `!` in a flat body, and `spelled` through `Monad/bind` itself. All three go through `Result`'s `Monad` witness, and a witness pinning the method levels at zero — `bind` taking `A, B : Type 0` only — would refuse each, the arm as "this Type would need to be strictly below itself". A concept's method level is its family's domain (`UniverseSolver::identify_bounded_choices`), which `Result`'s witness leaves to the caller. `tag` reads each outcome back, so the three are run rather than only checked.
const LARGE_PAYLOAD: &str = r#"
    use /std/{Str, Nat, List, Result, Monad};
    pub let walk(x: Result(Str, Type), n: List(Str)) -> Result(Str, Type) =
        match n
        | [] => x
        | [_, .._] =>
            let t = x!;
            Result/success(t)
        end;
    pub let flat(x: Result(Str, Type)) -> Result(Str, Type) =
        let t = x!;
        Result/success(t);
    pub let spelled(x: Result(Str, Type)) -> Result(Str, Type) =
        Monad/bind(x, (t) => Result/success(t));
    let tag(r: Result(Str, Type)) -> Str =
        match r
        | success(_) => "s"
        | failure(_) => "f"
        end;
"#;

#[test]
fn a_bang_sequences_a_large_payload() {
    let source = format!(
        r#"{LARGE_PAYLOAD}
        /std/print(Str/flatten([
            tag(walk(Result/success(Nat), ["a"])),
            tag(flat(Result/success(Str))),
            tag(spelled(Result/failure("refused"))),
        ]))
        "#
    );

    assert_eq!(run(&source), b"ssf");
}

// The control: `Result/bind` names no witness, so it sequences the same payload whatever the witness's method levels are.
#[test]
fn a_large_payload_binds_without_the_witness() {
    let source = format!(
        r#"{LARGE_PAYLOAD}
        let direct(x: Result(Str, Type)) -> Result(Str, Type) = Result/bind(x, (t) => Result/success(t));
        /std/print(Str/flatten([tag(direct(Result/success(Nat))), tag(direct(Result/failure("refused")))]))
        "#
    );

    assert_eq!(run(&source), b"sf");
}

// A monad the level matrix is put to: the `/std` names its programs import, its type at a `Nat` payload and at a `Type` one, its unit and its own sequencing function.
struct Region {
    uses: &'static str,
    small: &'static str,
    large: &'static str,
    pure: &'static str,
    bind: &'static str,
}

const RESULT: Region = Region {
    uses: "Str, Nat, Bool, Result, Monad",
    small: "Result(Str, Nat)",
    large: "Result(Str, Type)",
    pure: "Result/success",
    bind: "Result/bind",
};

const OPTION: Region = Region {
    uses: "Nat, Bool, Option, Monad",
    small: "Option(Nat)",
    large: "Option(Type)",
    pure: "Option/some",
    bind: "Option/bind",
};

const STATE: Region = Region {
    uses: "Nat, Bool, State, Monad",
    small: "State(Nat, Nat)",
    large: "State(Nat, Type)",
    pure: "State/pure",
    bind: "State/bind",
};

const IO: Region = Region {
    uses: "Nat, Bool, Io, Monad",
    small: "Io(Nat)",
    large: "Io(Type)",
    pure: "Io/pure",
    bind: "Io/bind",
};

const TRY: Region = Region {
    uses: "Str, Nat, Bool, Io, Try, Monad",
    small: "Try(Io, Str, Nat)",
    large: "Try(Io, Str, Type)",
    pure: "Try/pure",
    bind: "Try/bind",
};

// `Try` over `Io` written as a lambda: the same monad with no bare former in it.
const TRY_LAMBDA: Region = Region {
    uses: "Str, Nat, Bool, Io, Try, Monad",
    small: "Try((A: Type) => Io(A), Str, Nat)",
    large: "Try((A: Type) => Io(A), Str, Type)",
    pure: "Try/pure",
    bind: "Try/bind",
};

const REGIONS: [&Region; 5] = [&RESULT, &OPTION, &STATE, &IO, &TRY];

// How a cell sequences its action: `!`, `Monad/bind` written out, or the monad's own function, which names no witness.
#[derive(Clone, Copy)]
enum Spelling {
    Bang,
    Written,
    Own,
}

impl Region {
    fn program(&self, declarations: &str) -> String {
        let Region {
            uses,
            small,
            large,
            pure,
            ..
        } = self;

        format!(
            r#"
            use /std/{{{uses}}};
            let small(n: Nat) -> {small} = {pure}(n);
            let large(n: Nat) -> {large} = {pure}(Nat);
            {declarations}
            /std/print("bound")
            "#
        )
    }

    fn sequenced(&self, spelling: Spelling, action: &str, answer: &str) -> String {
        match spelling {
            Spelling::Bang => format!("let _ = {action}!; {answer}"),
            Spelling::Written => format!("Monad/bind({action}, (_) => {answer})"),
            Spelling::Own => format!("{}({action}, (_) => {answer})", self.bind),
        }
    }

    // A large region binding a small action.
    fn big(&self, spelling: Spelling) -> String {
        let body = self.sequenced(spelling, "small(n)", &format!("{}(Nat)", self.pure));

        self.program(&format!("pub let big(n: Nat) -> {} = {body};", self.large))
    }

    fn one_declaration(&self, spelling: Spelling) -> String {
        let body = self.sequenced(spelling, "large(n)", &format!("{}(n)", self.pure));

        format!("pub let one(n: Nat) -> {} = {body};", self.small)
    }

    // A small region binding a large action.
    fn one(&self, spelling: Spelling) -> String {
        self.program(&self.one_declaration(spelling))
    }

    // A small region binding both actions.
    fn both(&self) -> String {
        let Region { small, pure, .. } = self;

        self.program(&format!(
            "pub let both(n: Nat) -> {small} = let _ = small(n)!; let _ = large(n)!; {pure}(n);"
        ))
    }

    // A region one level above a large action. `large`'s instance is decided, where `small`'s over a bare base monad is its caller's, so this is the cell in which two decided instances meet.
    fn huge(&self, spelling: Spelling) -> String {
        let body = self.sequenced(spelling, "large(n)", &format!("{}(Type)", self.pure));

        self.program(&format!("pub let huge(n: Nat) -> {} = {body};", self.large))
    }

    // `one` set beside a region at its own level by a caller.
    fn pick(&self, spelling: Spelling) -> String {
        let one = self.one_declaration(spelling);
        let small = self.small;

        self.program(&format!(
            "{one}
            pub let pick(b: Bool, n: Nat) -> {small} =
                match b | true => one(n) | false => small(n) end;"
        ))
    }
}

const BELOW_ITSELF: &str = "strictly below itself";

// What the two checkers say of a cell, asked without erasing or emitting it: the elaborator's refusal as a compilation prints it, or what the kernel refuses of the program the elaborator built. A cell is about its levels, which both have judged by then, and compiled whole the matrix costs a minute a test.
fn judged(source: &str) -> Result<Vec<String>, String> {
    let entrypoint = source.parse::<Entrypoint>().expect("the cell parses");
    let checked = typecheck_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none())
        .map_err(|refused| refused.to_string())?;
    assert!(
        checked.obligations.is_empty(),
        "a cell raises an erasure obligation: {source}"
    );
    let program = Zonked::project(&checked.program).expect("an elaborated program is zonked");

    Ok(recheck_with_prelude(&program, DEFAULT_STEP_BUDGET)
        .iter()
        .map(|verdict| format!("{verdict:?}"))
        .collect())
}

fn accepted(source: &str) {
    match judged(source) {
        Ok(verdicts) => assert!(
            verdicts.is_empty(),
            "the kernel refuses: {source}\n{verdicts:?}"
        ),
        Err(message) => panic!("refused: {source}\n{message}"),
    }
}

fn refused(source: &str, report: &str) {
    match judged(source) {
        Ok(_) => panic!("accepted: {source}"),
        Err(message) => assert!(message.contains(report), "got: {message}\nfor: {source}"),
    }
}

// A region's monad is one nominal instance and its action's another, apart in a level that only types a payload. Both checkers call the two one type, so `!` sequences the action whatever level its payload sits at.
#[test]
fn a_bang_sequences_an_action_below_its_region() {
    for region in REGIONS {
        accepted(&region.big(Spelling::Bang));
    }
}

// The control: a monad's own function names no witness and instantiates each side apart.
#[test]
fn a_monads_own_bind_sequences_an_action_below_its_region() {
    for region in REGIONS {
        accepted(&region.big(Spelling::Own));
    }
}

// A witness's levels close with the declaration that resolves it, so `!`, which resolves its witness before the action is read, takes an action above its region.
#[test]
fn a_bang_takes_an_action_above_its_region() {
    for region in REGIONS {
        accepted(&region.one(Spelling::Bang));
    }
}

#[test]
fn a_monads_own_bind_takes_an_action_above_its_region() {
    for region in REGIONS {
        accepted(&region.one(Spelling::Own));
    }
}

// Written out, `Monad/bind` infers its monad from the action. The family it imitates is an occurrence of its own, so the monad is not held to the action's level.
#[test]
fn a_written_bind_sequences_an_action_below_its_region() {
    for region in REGIONS {
        accepted(&region.big(Spelling::Written));
    }
}

#[test]
fn a_written_bind_takes_an_action_above_its_region() {
    for region in REGIONS {
        accepted(&region.one(Spelling::Written));
    }
}

#[test]
fn a_region_binds_actions_at_two_levels() {
    for region in REGIONS {
        accepted(&region.both());
    }
}

// It needs no `!`: a family applied at two payloads takes a nominal instance at each.
#[test]
fn a_family_takes_nominal_instances_at_two_payloads() {
    let through = r#"
        use /std/{Str, Nat, Result};
        let small(n: Nat) -> Result(Str, Nat) = Result/success(n);
        let through(F: (Type) -> Type, x: F(Nat), y: F(Type)) -> Nat = 0;
        let probe(n: Nat) -> Nat = through((A) => Result(Str, A), small(n), Result/success(Nat));
        /std/print("bound")
        "#;

    accepted(through);
}

// A region accepted below its action is one type with a region at its own level, whatever level its signature carries, so a caller sets the two beside each other.
#[test]
fn a_region_bound_with_bang_below_its_action_sits_beside_one_at_its_own_level() {
    for region in REGIONS {
        accepted(&region.pick(Spelling::Bang));
    }
}

#[test]
fn a_region_bound_by_a_written_bind_below_its_action_sits_beside_one_at_its_own_level() {
    for region in REGIONS {
        accepted(&region.pick(Spelling::Written));
    }
}

// `Try/bind` takes its base monad as one argument, and a declaration that names one bare leaves its level to its callers: a larger region is accepted over `Io`, over `Option` and over `Io` eta-expanded.
#[test]
fn a_transformers_own_bind_takes_a_larger_region_over_a_bare_base_monad() {
    let over = |base: &str| {
        format!(
            r#"
            use /std/{{Str, Nat, Io, Option, Try}};
            let small(n: Nat) -> Try({base}, Str, Nat) = Try/pure(n);
            pub let big(n: Nat) -> Try({base}, Str, Type) = Try/bind(small(n), (_) => Try/pure(Nat));
            /std/print("bound")
            "#
        )
    };

    for base in ["Io", "Option", "(A: Type) => Io(A)"] {
        accepted(&over(base));
    }
}

// One level up the action's instance is decided, and so is the region's: two constants no equation could join, where `small`'s level over a bare base monad is its caller's to choose.
#[test]
fn a_bang_sequences_a_decided_action_below_its_region() {
    for region in [&RESULT, &OPTION, &STATE, &IO, &TRY_LAMBDA] {
        accepted(&region.huge(Spelling::Bang));
    }
}

// Over `Try` the base monad is an implicit that unification solves from the action. It is committed as its check rebuilt it, at the domain `Try/bind` takes it at, so the kernel is handed the same monad whichever side solved it.
#[test]
fn a_monads_own_bind_sequences_a_decided_action_below_its_region() {
    for region in [&RESULT, &OPTION, &STATE, &TRY, &TRY_LAMBDA] {
        accepted(&region.huge(Spelling::Own));
    }
}

#[test]
fn a_written_bind_sequences_a_decided_action_below_its_region() {
    for region in [&RESULT, &OPTION, &STATE, &TRY, &TRY_LAMBDA] {
        accepted(&region.huge(Spelling::Written));
    }
}

// A bare `Io` is no nominal instance: its level is compared, and the levels-only shortcut commits the region's to the action's before either is unfolded. The refusal is the one the roadmap's *A universe level settled before its evidence is in* owns, reached through `Try`; the same region bound by `Try/bind` or by `Monad/bind` written out is accepted above.
#[test]
fn a_bang_over_a_bare_base_monad_holds_its_region_at_a_decided_actions_level() {
    refused(&TRY.huge(Spelling::Bang), BELOW_ITSELF);
}
