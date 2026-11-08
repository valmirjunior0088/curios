//! What the rules' suites share: the programs more than one of them puts, the control a refusal is derived from, and what a refusal is held to.

use {
    crate::test_support::*,
    curios_cert::Verdict,
    curios_common::{Qualifier, test_support::Variants},
    curios_core::{Global, Term, WitnessKey},
    curios_module::RootSource,
    curios_pipeline::{DEFAULT_BUDGET, EntryTail, Examined, Refused, test_support::*},
    curios_surface::{Entrypoint, Spellable, Spelling},
    std::{fmt, rc::Rc},
};

/// What the checkers said of a program a rule refuses, put whole at the program contract as the soundness board puts a witness at `False` ([`examine_with_prelude`]): every error the elaborator raised or reported, each still wrapped in where it was raised, and every verdict of the kernel's where the elaborator built it a module.
pub struct Refusals {
    elaborator: Vec<curios_elab::Error>,
    kernel: Vec<Verdict>,
    /// Whether the kernel judged the program, which it does wherever the elaborator built it a module, its erasure obligations reported and not raised.
    judged: bool,
}

/// A rule's sound neighbour: a program the rule admits, run as the program it is and checked by what it prints where it is made. A refusal of the rule is derived from it by the one thing the refusal turns on ([`Control::refused`]), so a rule that came to refuse everything of a shape fails at its control, which a refusal alone would pass: the soundness board's decision places each refusal beside its control.
pub struct Control {
    source: String,
}

/// The control `source`, which runs and prints `prints`.
pub fn control(source: &str, prints: &[u8]) -> Control {
    assert_eq!(run(source), prints, "what the control prints");

    Control {
        source: source.to_string(),
    }
}

impl Control {
    /// The control with each of `edits` made in turn, put to both checkers as [`refused`] puts a program. Each edit's text stands once in the program as the edits before it left it, so an edit the control has moved away from fails rather than drifting.
    pub fn refused(&self, edits: &[(&str, &str)]) -> Refusals {
        let source = edits
            .iter()
            .fold(self.source.clone(), |source, (from, to)| {
                assert_eq!(
                    source.matches(from).count(),
                    1,
                    "`{from}` stands once in the control as the edits before it left it"
                );
                source.replacen(from, to, 1)
            });

        refused(&source)
    }
}

/// Put `source` to both checkers at the program contract: what they refused it with, which must be something. Private, so a refusal is put only as derived from its control.
fn refused(source: &str) -> Refusals {
    let entrypoint = source.parse::<Entrypoint>().expect("the program parses");
    let refusals = match examine_with_prelude(
        DEFAULT_BUDGET,
        &entrypoint,
        &RootSource::none(),
        EntryTail::Program,
    ) {
        Err(Refused {
            error: Some(error), ..
        }) => Refusals {
            elaborator: vec![*error],
            kernel: Vec::new(),
            judged: false,
        },
        Err(Refused {
            error: None,
            reported,
        }) => panic!("refused before the elaborator was asked:\n{reported}"),
        Ok(Examined {
            obligations,
            verdicts,
        }) => Refusals {
            elaborator: obligations
                .into_iter()
                .map(|obligation| obligation.error)
                .collect(),
            kernel: verdicts,
            judged: true,
        },
    };
    assert!(
        !refusals.elaborator.is_empty() || !refusals.kernel.is_empty(),
        "admitted"
    );

    refusals
}

/// The declaration an absolute path names, as an error carries it.
pub fn global(path: &str) -> Global {
    Global::Authored(Qualifier::from(
        path.strip_prefix('/').expect("an absolute path").split('/'),
    ))
}

/// `term`, a term an elaborator's error holds, as a rule's test pins it: every global by its absolute path, an intrinsic by its surface name, no universe instance and no metavariable's number, so the pin reads the term and not the order elaboration met its levels in, and on one line, each run of whitespace a space.
pub fn pinned(term: &Term) -> String {
    on_one_line(term, Spelling::default().with_erased_universes())
}

/// `key`, the key a coherence refusal holds, as a rule's test pins it.
pub fn pinned_key(key: &WitnessKey) -> String {
    key.faithful().to_string()
}

/// `term` as [`pinned`] writes it with its universe instances shown, each level metavariable a bare `?u`: for a test whose claim is the instance a term stands at.
pub fn pinned_with_instances(term: &Term) -> String {
    let spelled = on_one_line(term, Spelling::default());
    let mut pinned = String::with_capacity(spelled.len());
    let mut rest = spelled.as_str();
    while let Some(at) = rest.find("?u") {
        pinned.push_str(&rest[..at + 2]);
        rest = rest[at + 2..].trim_start_matches(|character: char| character.is_ascii_digit());
    }
    pinned.push_str(rest);

    pinned
}

/// `term` under `spelling` with every metavariable a bare `?`, on one line, each run of whitespace a space.
fn on_one_line(term: &Term, spelling: Spelling) -> String {
    term.spelled(&Rc::new(spelling.with_anonymous_metavars()))
        .to_string()
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

/// Where a refusal raised in `name` stands: the declaration it names, written as its name prints, a witness's included, or `entry` for the term a program closes with.
fn place(name: Option<&Global>) -> String {
    name.map_or("entry".to_string(), |name| name.faithful().to_string())
}

impl Refusals {
    /// Every error the elaborator refused the program with, still wrapped in where it was raised.
    fn elaborated(&self) -> impl Iterator<Item = &curios_elab::Error> {
        self.elaborator.iter().flat_map(curios_elab::Error::each)
    }

    /// The elaborator refused the program `at` by an error `pattern` holds of, read beneath what located it.
    pub fn by_elaborator_in(
        &self,
        at: &str,
        pattern: impl Fn(&curios_elab::Error) -> bool,
    ) -> &Self {
        assert!(
            self.elaborated()
                .any(|error| place(error.declared_in()) == at && pattern(error.raised())),
            "not refused so by the elaborator in `{at}`:\n{self}"
        );

        self
    }

    /// The kernel refused the program `at` by an error `pattern` holds of.
    pub fn by_kernel_in(&self, at: &str, pattern: impl Fn(&curios_cert::Error) -> bool) -> &Self {
        assert!(
            self.kernel
                .iter()
                .any(|verdict| place(verdict.name.as_ref()) == at && pattern(&verdict.error)),
            "not refused so by the kernel in `{at}`:\n{self}"
        );

        self
    }

    /// The elaborator refused the program by no error `pattern` holds of, wherever it stands.
    pub fn not_by_elaborator(&self, pattern: impl Fn(&curios_elab::Error) -> bool) {
        assert!(
            !self.elaborated().any(|error| pattern(error.raised())),
            "refused so by the elaborator:\n{self}"
        );
    }
}

/// Each refusal by its checker, its variant and where it stands, with the terms a mismatch compares as [`pinned`] writes them: what a pattern is written from.
impl fmt::Display for Refusals {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        for error in self.elaborated() {
            write!(
                formatter,
                "elaborator {} in {}",
                error.raised().variant_name(),
                place(error.declared_in())
            )?;
            match error.raised() {
                curios_elab::Error::DuplicateWitness {
                    concept,
                    key,
                    standing,
                    ..
                } => write!(
                    formatter,
                    "\n  concept: {}\n  key: {}\n  standing: {}",
                    concept.faithful(),
                    pinned_key(key),
                    standing.faithful()
                )?,
                curios_elab::Error::NotBoolType { head_type }
                | curios_elab::Error::NotNatType { head_type } => {
                    write!(formatter, "\n  head type: {}", pinned(head_type))?
                }
                curios_elab::Error::UnboundVariable { term, .. } => {
                    write!(formatter, "\n  term: {}", pinned(term))?
                }
                curios_elab::Error::OrphanWitness { concept, key, .. } => write!(
                    formatter,
                    "\n  concept: {}\n  key: {}",
                    concept.faithful(),
                    pinned_key(key)
                )?,
                _ => {}
            }
            if let curios_elab::Error::UninferredImplicit { bound, .. } = error.raised() {
                write!(formatter, "\n  bound: {}", pinned(bound))?;
            }
            if let curios_elab::Error::TypeMismatch {
                inferred, expected, ..
            } = error.raised()
            {
                for (field, term) in [("inferred", inferred), ("expected", expected)] {
                    write!(formatter, "\n  {field}: {}", pinned(term))?;
                    if pinned_with_instances(term) != pinned(term) {
                        write!(
                            formatter,
                            "\n  {field} with instances: {}",
                            pinned_with_instances(term)
                        )?;
                    }
                }
            }
            writeln!(formatter)?;
        }
        if !self.judged {
            writeln!(formatter, "kernel not asked")?;
        }
        for verdict in &self.kernel {
            writeln!(
                formatter,
                "kernel {} in {}",
                verdict.error.variant_name(),
                place(verdict.name.as_ref())
            )?;
        }

        Ok(())
    }
}

pub const AN_EMPTY_PROPOSITION_STILL_ELIMINATES_INTO_DATA: &str = r#"
    use /std/{Nat};
    use /std/Bool/{False};

    let ex_falso(f : False) -> Nat =
        match f
        end;

    /std/print(Nat/to_str(0))
    "#;

pub const A_PROPOSITION_STILL_ELIMINATES_INTO_ANOTHER_PROPOSITION: &str = r#"
    use /std/{Nat};
    use /std/Bool/{True};

    induct Two : pub Prop
    | a()
    | b()
    end

    let into_prop(t : Two) -> True =
        match t
        | a() => True/qed()
        | b() => True/qed()
        end;

    /std/print(Nat/to_str(1))
    "#;

/// `drop`'s `@A` is constrained by nothing — no argument mentions it and no result determines it — so every use below leaves one unsolved term metavariable exactly where the fixture plants it.
pub const AN_UNCONSTRAINED_IMPLICIT: &str = r#"
    use /std/{Nat};

    let drop(@A : Type, n : Nat) -> Nat = n;
"#;

/// The same argument supplied in all four positions at once, so the refusals above cannot be passing for "a declaration may not mention an implicit".
pub const A_SOLVED_METAVARIABLE_IN_EVERY_POSITION: &str = r#"
    induct Fine : pub Type
    | c(x : /std/Eq()(drop(@Nat, 0), 0))
    end

    struct Also : pub Type {
        x : /std/Eq()(drop(@Nat, 0), 0)
    }

    let f(x : /std/Eq()(drop(@Nat, 0), 0)) -> Nat = 0;

    /std/print(Nat/to_str(drop(@Nat, 0)))
    "#;

pub const A_NESTED_RELEVANT_CLASH_STILL_EXCUSES_AN_OMITTED_ARM: &str = r#"
    use /std/{Nat};

    induct Pair : pub Type
    | mk(n : Nat)
    end

    induct Ind : (x : Pair) -> pub Type
    | left()  : (Pair/mk(0))
    | right() : (Pair/mk(1))
    end

    let f(w : Ind(Pair/mk(0))) -> Nat =
        match w : (x, q) => Nat
        | left() => 0
        end;

    /std/print(Nat/to_str(f(Ind/left())))
    "#;

pub const A_CLASH_BETWEEN_TWO_FORCINGS_OF_ONE_BINDER_EXCUSES_THE_ARM: &str = r#"
    use /std/{Eq, Nat, Option};
    use /std/Bool/{False};

    induct Color : pub Type
    | red()
    | green()
    end

    induct Same(@A : Type) : (A, A) -> pub Prop
    | same(@z : A) : (z, z)
    end

    let absurd_bool(h : Eq()(false, true)) -> False =
        match h end;

    let absurd_color(h : Eq()(Color/green(), Color/red())) -> False =
        match h end;

    let absurd_nat(h : Eq()(0, 1)) -> False =
        match h end;

    let absurd_successor(n : Nat, h : Eq()(0, n + 1)) -> False =
        match h end;

    let absurd_option(h : Eq()(Option/some(1), Option/none())) -> False =
        match h end;

    let absurd_same(h : Same()(Color/green(), Color/red())) -> False =
        match h end;

    /std/print("ok")
    "#;

pub const A_VARIABLE_OF_THE_INDICES_FORCED_TWO_WAYS_EXCUSES_THE_ARM: &str = r#"
    use /std/{Bool, Eq, Nat};

    induct Two : (a : Nat, b : Nat) -> Type
    | mk() : (0, 1)
    end

    induct Tagged : (x : {Nat, Bool}) -> Type
    | mk() : ((0, false))
    end

    let neither(n : Nat, x : Two(n, n)) -> Nat =
        match x end;

    let refined(n : Nat, h : Eq()(n, 7), x : Two(n, n)) -> Eq()(n, 7) =
        match x end;

    let behind(n : Nat, x : Tagged((n, true))) -> Nat =
        match x end;

    /std/print("ok")
    "#;

pub const A_POSITIVE_SEGMENT_STILL_CLASHES_WITH_THE_EMPTY_VALUE: &str = r#"
    use /std/{Eq, Bytes};
    use /std/Bool/{False};

    let absurd(b : Bytes, h : Eq()(x[..b, 0x01], x[])) -> False =
        match h end;

    /std/print("ok")
    "#;

pub const A_MATCH_ON_A_PINNED_PROOF_IS_ITS_ARM_AT_THE_INDEX_UNDER_EVERY_CASE: &str = r#"
    use /std/{Bool};
    use /std/Bool/{True, Holds};

    induct Read: (Bool) -> Prop | mk(x: Bool): (x) end

    let direct(g: (Bool) -> Bool, b: Bool, p: Read(b),
          guard: Holds(match p: (i, q) => Bool | mk(x) => g(x) end)) -> Holds(g(b)) =
        guard;

    let split(g: (Bool) -> Bool, b: Bool, p: Read(b),
          guard: Holds(match p: (i, q) => Bool | mk(x) => g(x) end),
          chosen: Holds(b)) -> Holds(g(b)) =
        match (match p: (i, q) => Bool | mk(x) => g(x) end)
        | false => match guard end
        | true => match b | false => match chosen end | true => True/qed() end
        end;

    /std/print("ok")
    "#;

/// The witness's lemma at a *genuine* proposition, which must stay accepted: `all_equal` is sound, and a fix that closed the hole by refusing `Prop`-abstracted binders would take this with it.
pub const IRRELEVANCE_STILL_IDENTIFIES_A_PROPOSITIONS_INHABITANTS: &str = r#"
    use /std/{Nat};

    induct Same(@P: Prop): (P, P) -> pub Prop | same(@p: P): (p, p) end

    induct Two : pub Prop
    | a()
    | b()
    end

    let all_equal(@X : Prop, x : X, y : X) -> Same()(x, y) =
        Same/same();

    let same : Same()(Two/a(), Two/b()) =
        all_equal(Two/a(), Two/b());

    /std/print(Nat/to_str(1))
    "#;

pub const A_LIST_OF_EVIDENCE_IS_STILL_A_LIST: &str = r#"
    use /std/{Nat, List, Option};
    use /std/Bool/{True};

    let one : List(Option(True)) = [Option/some(True/qed())];

    /std/print(Nat/to_str(List/len(one)))
    "#;

pub const A_CATCH_ALL_IS_CHECKED_AT_ITS_SCRUTINEE: &str = r#"
    use /std/{Nat, Eq};

    induct Three : pub Type
    | a()
    | b()
    | c()
    end

    let same(t : Three) -> Eq()(t, t) =
        match t : (q) => Eq()(q, q)
        | a() => Eq/refl()
        | _ => Eq/refl()
        end;

    /std/print(Nat/to_str(1))
    "#;

pub const A_RECORD_OF_PROPOSITIONS_IS_A_PROPOSITION: &str = r#"
    use /std/{Nat, Eq};

    struct Holder : pub Prop {
        field : {Eq()(0, 0), Eq()(1, 1)}
    }

    /std/print(Nat/to_str(1))
    "#;

pub const A_FUNCTION_INTO_A_PROPOSITION_IS_A_PROPOSITION: &str = r#"
    use /std/{Nat, Eq};

    struct Holder : pub Prop {
        field : (A : Type) -> Eq()(0, 0)
    }

    /std/print(Nat/to_str(1))
    "#;

pub const ETA_CONVERTS_A_FUNCTION_AND_A_RECORD_WITH_THEIR_EXPANSIONS: &str = r#"
    use /std/{Eq, Nat, Bool};

    let function(g : (Nat) -> Nat) -> Eq()((x : Nat) => g(x), g) = Eq/refl();

    let record(p : {Nat, Bool}) -> Eq()((p.0, p.1), p) = Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const A_LITERALS_ETA_HOLDS_WHERE_NO_TYPE_DIRECTS_IT: &str = r#"
    use /std/{Eq, Nat, Bool};

    let hold(@A : Type, a : A, count : Nat) -> A =
        match count: (_) => A | 0 => a | pred + 1 => a end;

    let in_an_arm(b : Bool, p : {Nat, Nat})
        -> Eq()(match b: (_) => {Nat, Nat} | true => (p.0, p.1) | false => p end,
                match b: (_) => {Nat, Nat} | true => p | false => p end)
        = Eq/refl();

    let in_a_fold_arm(n : Nat, p : {Nat, Nat})
        -> Eq()(match n: (_) => {Nat, Nat} | 0 => (p.0, p.1) | m + 1 => p end,
                match n: (_) => {Nat, Nat} | 0 => p | m + 1 => p end)
        = Eq/refl();

    let under_a_projection(b : Bool, p : {Nat, Nat})
        -> Eq()((match b: (_) => {Nat, Nat} | true => (p.0, p.1) | false => p end).0,
                (match b: (_) => {Nat, Nat} | true => p | false => p end).0)
        = Eq/refl();

    let under_a_lambda(b : Bool, p : {Nat, Nat})
        -> Eq()(match b: (_) => (Nat) -> {Nat, Nat}
                | true => (x : Nat) => (p.0, p.1)
                | false => (x : Nat) => p
                end,
                match b: (_) => (Nat) -> {Nat, Nat}
                | true => (x : Nat) => p
                | false => (x : Nat) => p
                end)
        = Eq/refl();

    let a_function(b : Bool, g : (Nat) -> Nat)
        -> Eq()(match b: (_) => (Nat) -> Nat | true => (x : Nat) => g(x + 0) | false => g end,
                match b: (_) => (Nat) -> Nat | true => g | false => g end)
        = Eq/refl();

    let through_a_definition(p : {Nat, Nat}, count : Nat)
        -> Eq()(hold((p.0, p.1), count), hold(p, count))
        = Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const ANY_TWO_TERMS_CONVERT_AT_A_TYPE_WITH_NO_FIELD: &str = r#"
    use /std/{Eq, Nat, Bool};

    struct U: pub Type {}

    let at_the_unit(F : ({}) -> Type, r : {}, s : {}, x : F(r)) -> F(s) = x;

    let at_a_struct(F : (U) -> Type, r : U, s : U, x : F(r)) -> F(s) = x;

    let at_a_record_of_units(r : {a: {}, b: {}}, s : {a: {}, b: {}})
        -> Eq(@({a: {}, b: {}}))(r, s)
        = Eq/refl();

    let at_a_function_into_a_unit(f : (Nat) -> {}, g : (Nat) -> {})
        -> Eq(@((Nat) -> {}))(f, g)
        = Eq/refl();

    let two_applications(f : (Nat) -> {}, g : (Nat) -> {}, n : Nat)
        -> Eq(@({}))(f(n), g(n))
        = Eq/refl();

    let two_stuck_matches(b : Bool, r : {}, s : {})
        -> Eq(@({}))(match b: (_) => {} | true => r | false => r end,
                     match b: (_) => {} | true => s | false => s end)
        = Eq/refl();

    let two_stuck_matches_at_a_struct(b : Bool, r : U, s : U)
        -> Eq(@(U))(match b: (_) => U | true => r | false => r end,
                    match b: (_) => U | true => s | false => s end)
        = Eq/refl();

    let two_stuck_matches_at_a_record_of_units(b : Bool, r : {{}, {}}, s : {{}, {}})
        -> Eq(@({{}, {}}))(match b: (_) => {{}, {}} | true => r | false => r end,
                           match b: (_) => {{}, {}} | true => s | false => s end)
        = Eq/refl();

    let two_stuck_matches_at_a_function_into_a_unit(b : Bool, f : (Nat) -> {}, g : (Nat) -> {})
        -> Eq(@((Nat) -> {}))(match b: (_) => (Nat) -> {} | true => f | false => f end,
                              match b: (_) => (Nat) -> {} | true => g | false => g end)
        = Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const ANY_TWO_TERMS_CONVERT_AT_A_STRUCT_WITH_ONE_INHABITANT: &str = r#"
    use /std/{Eq, Nat, Bool};

    struct W: pub Type { u: {} }

    struct Proved: pub Type { p: Eq()(0, 0) }

    struct N: pub Type { w: W, f: (Nat) -> {}, r: {a: {}, b: Proved} }

    struct Pair(A: Type): pub Type { a: A, b: A }

    struct Other(A: Type): pub Type { held: A }

    let at_a_struct_of_a_unit(r : W, s : W) -> Eq(@(W))(r, s) = Eq/refl();

    let by_a_cast(F : (W) -> Type, r : W, s : W, x : F(r)) -> F(s) = x;

    let at_a_struct_of_a_proof(r : Proved, s : Proved) -> Eq(@(Proved))(r, s) = Eq/refl();

    let at_a_struct_of_such(r : N, s : N) -> Eq(@(N))(r, s) = Eq/refl();

    let two_applications(f : (Nat) -> W, g : (Nat) -> W, n : Nat)
        -> Eq(@(W))(f(n), g(n))
        = Eq/refl();

    let two_stuck_matches(b : Bool, r : W, s : W)
        -> Eq(@(W))(match b: (_) => W | true => r | false => r end,
                    match b: (_) => W | true => s | false => s end)
        = Eq/refl();

    let at_a_struct_nested_in_its_own_parameter(r : Pair(Pair({})), s : Pair(Pair({})))
        -> Eq(@(Pair(Pair({}))))(r, s)
        = Eq/refl();

    let at_a_struct_nested_under_another(r : Pair(Other(Pair({}))), s : Pair(Other(Pair({}))))
        -> Eq(@(Pair(Other(Pair({})))))(r, s)
        = Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const A_GOALS_TYPE_IS_READ_FORCED: &str = r#"
    use /std/{Eq, Nat, Bool};

    induct Same(@P: Prop): (P, P) -> pub Prop | same(@p: P): (p, p) end

    struct Outer: pub Prop { inner: Inner, p: Eq()(0, 0) }
    and Inner: pub Prop { p: Eq()(0, 0), outer: () -> Outer }

    struct Pair(A: Type): pub Type { a: A, b: A }

    let Tower(n : Nat) -> Type = match n: (_) => Type | 0 => {} | p + 1 => Pair(Tower(p)) end;

    let at_a_member(r : Inner, s : Inner) -> Same()(r, s) = Same/same();

    let at_a_member_that_holds_another(r : Outer, s : Outer) -> Same()(r, s) = Same/same();

    let by_a_cast(F : (Inner) -> Type, r : Inner, s : Inner, x : F(r)) -> F(s) = x;

    let at_a_struct_a_definition_computes(r : Tower(2), s : Tower(2))
        -> Eq(@(Tower(2)))(r, s)
        = Eq/refl();

    let where_a_lookup_reads_the_type(b : Bool, r : Outer, s : Outer)
        -> Eq(@(Nat))((match b: (_) => {Outer, Nat} | true => (r, 1) | false => (r, 2) end).1,
                      (match b: (_) => {Outer, Nat} | true => (s, 1) | false => (s, 2) end).1)
        = Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const TWO_NEUTRALS_CONVERT_WHERE_A_LOOKUP_GIVES_THEM_A_TYPE_WITH_ONE_INHABITANT: &str = r#"
    use /std/{Eq, Nat, Bool};

    struct W: pub Type { u: {} }

    let at_a_record_of_units(b : Bool, r : {{}, {}}, s : {{}, {}})
        -> Eq(@(Nat))((match b: (_) => {{{}, {}}, Nat} | true => (r, 1) | false => (r, 2) end).1,
                      (match b: (_) => {{{}, {}}, Nat} | true => (s, 1) | false => (s, 2) end).1)
        = Eq/refl();

    let at_a_function_into_a_unit(b : Bool, r : (Nat) -> {}, s : (Nat) -> {})
        -> Eq(@(Nat))((match b: (_) => {(Nat) -> {}, Nat} | true => (r, 1) | false => (r, 2) end).1,
                      (match b: (_) => {(Nat) -> {}, Nat} | true => (s, 1) | false => (s, 2) end).1)
        = Eq/refl();

    let at_a_struct_of_a_unit(b : Bool, r : W, s : W)
        -> Eq(@(Nat))((match b: (_) => {W, Nat} | true => (r, 1) | false => (r, 2) end).1,
                      (match b: (_) => {W, Nat} | true => (s, 1) | false => (s, 2) end).1)
        = Eq/refl();

    let at_a_proposition_whose_implicit_is_solved(b : Bool, p : Eq()(0, 0), q : Eq()(0, 0))
        -> Eq(@(Nat))((match b: (_) => {Eq()(0, 0), Nat} | true => (p, 1) | false => (p, 2) end).1,
                      (match b: (_) => {Eq()(0, 0), Nat} | true => (q, 1) | false => (p, 2) end).1)
        = Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const A_STUCK_ELIMINATION_IS_TYPED_BY_ITS_RESULT_AT_ITS_SCRUTINEE: &str = r#"
    use /std/{Eq, Nat, Bool};

    let against_a_variable(b : Bool, c : Bool, p : Eq()(0, 0), q : Eq()(0, 0))
        -> Eq(@(Nat))((match c: (_) => {Eq()(0, 0), Nat}
                       | true => (match b: (_) => Eq()(0, 0) | true => p | false => q end, 1)
                       | false => (p, 2) end).1,
                      (match c: (_) => {Eq()(0, 0), Nat} | true => (p, 1) | false => (p, 2) end).1)
        = Eq/refl();

    let of_two_scrutinees(a : Bool, b : Bool, c : Bool, p : Eq()(0, 0), q : Eq()(0, 0))
        -> Eq(@(Nat))((match c: (_) => {Eq()(0, 0), Nat}
                       | true => (match a: (_) => Eq()(0, 0) | true => p | false => q end, 1)
                       | false => (p, 2) end).1,
                      (match c: (_) => {Eq()(0, 0), Nat}
                       | true => (match b: (_) => Eq()(0, 0) | true => q | false => p end, 1)
                       | false => (p, 2) end).1)
        = Eq/refl();

    let at_a_record_of_units(b : Bool, c : Bool, r : {{}, {}}, s : {{}, {}})
        -> Eq(@(Nat))((match c: (_) => {{{}, {}}, Nat}
                       | true => (match b: (_) => {{}, {}} | true => r | false => s end, 1)
                       | false => (r, 2) end).1,
                      (match c: (_) => {{{}, {}}, Nat} | true => (r, 1) | false => (r, 2) end).1)
        = Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const A_CONSTRUCTORS_VALUE_IS_TYPED_BY_ITS_DECLARATION: &str = r#"
    use /std/{Eq, Nat, Bool};

    let a_proof_built_against_one_named(b : Bool, p : Eq()(0, 0))
        -> Eq(@(Nat))((match b: (_) => {Eq()(0, 0), Nat} | true => (p, 1) | false => (p, 2) end).1,
                      (match b: (_) => {Eq()(0, 0), Nat} | true => (Eq/refl(), 1) | false => (Eq/refl(), 2) end).1)
        = Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const AN_ARMS_BINDERS_ARE_OPENED_AT_THE_TYPES_THEIR_CONSTRUCTOR_GIVES_THEM: &str = r#"
    use /std/{Eq, Nat};

    induct Two : pub Type
    | two(p : Eq()(0, 0), q : Eq()(0, 0))
    end

    let either_proof(t : Two)
        -> Eq(@(Nat))((match t: (_) => {Eq()(0, 0), Nat} | two(p, q) => (p, 1) end).1,
                      (match t: (_) => {Eq()(0, 0), Nat} | two(p, q) => (q, 1) end).1)
        = Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const A_LITERALS_ETA_IS_TAKEN_AGAINST_A_STUCK_ELIMINATION: &str = r#"
    use /std/{Eq, Nat, Bool};

    struct Record: pub Type { a: Nat, b: Nat }

    let hold(@A: Type, a: A, count: Nat) -> A =
        match count: (_) => A | 0 => a | pred + 1 => a end;

    let a_tuple_in_an_arm(b : Bool, c : Bool, p : {Nat, Nat}, q : {Nat, Nat})
        -> Eq(@({Nat, Nat}))(
            match b: (_) => {Nat, Nat}
            | true => ((match c: (_) => {Nat, Nat} | true => p | false => q end).0,
                       (match c: (_) => {Nat, Nat} | true => p | false => q end).1)
            | false => p
            end,
            match b: (_) => {Nat, Nat}
            | true => match c: (_) => {Nat, Nat} | true => p | false => q end
            | false => p
            end)
        = Eq/refl();

    let a_tuple_against_a_definition_that_unfolds_to_one(b : Bool, n : Nat, p : {Nat, Nat})
        -> Eq(@({Nat, Nat}))(
            match b: (_) => {Nat, Nat} | true => (hold(p, n).0, hold(p, n).1) | false => p end,
            match b: (_) => {Nat, Nat} | true => hold(p, n) | false => p end)
        = Eq/refl();

    let a_lambda_in_an_arm(b : Bool, c : Bool, g : (Nat) -> Nat, k : (Nat) -> Nat)
        -> Eq(@((Nat) -> Nat))(
            match b: (_) => (Nat) -> Nat
            | true => (x : Nat) => (match c: (_) => (Nat) -> Nat | true => g | false => k end)(x + 0)
            | false => g
            end,
            match b: (_) => (Nat) -> Nat
            | true => match c: (_) => (Nat) -> Nat | true => g | false => k end
            | false => g
            end)
        = Eq/refl();

    let a_struct_at_its_own_type(c : Bool, s : Record, r : Record)
        -> Eq(@(Record))(
            Record { a = (match c: (_) => Record | true => s | false => r end).a,
                     b = (match c: (_) => Record | true => s | false => r end).b },
            match c: (_) => Record | true => s | false => r end)
        = Eq/refl();

    let a_struct_in_an_arm(b : Bool, c : Bool, s : Record, r : Record)
        -> Eq(@(Record))(
            match b: (_) => Record
            | true => Record { a = (match c: (_) => Record | true => s | false => r end).a,
                               b = (match c: (_) => Record | true => s | false => r end).b }
            | false => s
            end,
            match b: (_) => Record
            | true => match c: (_) => Record | true => s | false => r end
            | false => s
            end)
        = Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const A_FUNCTION_INTO_A_PROPOSITION_IS_DISCHARGED_BEFORE_ETA: &str = r#"
    use /std/{Eq, Nat};

    induct Same(@P: Prop): (P, P) -> pub Prop | same(@p: P): (p, p) end

    let same(g : (Nat) -> Eq()(0, 0), h : (Nat) -> Eq()(0, 0)) -> Same()(g, h) = Same/same();

    /std/print(Nat/to_str(1))
    "#;

pub const ETA_HANDS_A_RECORDS_PROOF_COMPONENT_TO_IRRELEVANCE: &str = r#"
    use /std/{Eq, Nat};

    let same(g : (Nat) -> {Nat, Eq()(0, 0)}, p : Eq()(0, 0))
        -> Eq()(g, (x : Nat) => (g(x).0, p)) = Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const A_SPINE_ARGUMENT_COMPARES_AT_THE_HEADS_DOMAIN: &str = r#"
    use /std/{Eq, Nat};

    let ground(f : (Eq()(0, 0)) -> Nat, p : Eq()(0, 0), q : Eq()(0, 0)) -> Eq()(f(p), f(q)) =
        Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const A_DEFINITION_APPLIED_TO_TWO_PROOFS_CONVERTS_BEFORE_UNFOLDING: &str = r#"
    use /std/{Eq, Nat};

    induct Z: (Nat) -> pub Prop
    | mk(): (0)
    end

    let h(n : Nat, e : Z(n)) -> Nat = match e | mk() => 1 end;

    let same(n : Nat, p : Z(n), q : Z(n), x : Eq()(h(n, p), 0)) -> Eq()(h(n, q), 0) = x;

    /std/print(Nat/to_str(1))
    "#;

pub const TWO_CALLS_OF_ONE_DEFINITION_CONVERT_BY_THEIR_SPINES_AT_ANY_TYPE: &str = r#"
    use /std/{Eq, Nat};

    induct Z: (Nat) -> pub Prop
    | mk(): (0)
    end

    let into(n : Nat, e : Z(n)) -> (Nat) -> Nat =
        match e: (_, _) => (Nat) -> Nat | mk() => (x : Nat) => x end;

    let pair(n : Nat, e : Z(n)) -> {Nat, Nat} =
        match e: (_, _) => {Nat, Nat} | mk() => (1, 2) end;

    let through(n : Nat, e : Z(n)) -> Nat = match e | mk() => 1 end;

    let at_a_function_type(n : Nat, k : (Nat) -> Nat, p : Z(n), q : Z(n),
                           x : Eq(@((Nat) -> Nat))(into(n, p), k))
        -> Eq(@((Nat) -> Nat))(into(n, q), k) = x;

    let at_a_record_type(n : Nat, k : {Nat, Nat}, p : Z(n), q : Z(n),
                         x : Eq(@({Nat, Nat}))(pair(n, p), k))
        -> Eq(@({Nat, Nat}))(pair(n, q), k) = x;

    let through_a_curried_call(n : Nat, p : Z(n), q : Z(n), x : Eq()(into(n, p)(3), 0))
        -> Eq()(into(n, q)(3), 0) = x;

    let whatever_spells_them(n : Nat, p : Z(n), q : Z(n))
        -> Eq()(through(n, p), through(n, q)) = Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const TWO_CALLS_OF_ONE_DEFINITION_CONVERT_WHEREVER_THEY_SIT: &str = r#"
    use /std/{Eq, Nat, Bool, List};

    induct Zero: (Nat) -> pub Prop
    | zero(): (0)
    end

    let through(n : Nat, e : Zero(n)) -> Nat = match e | zero() => 1 end;

    let into(n : Nat, e : Zero(n)) -> (Nat) -> Nat =
        match e: (_, _) => (Nat) -> Nat | zero() => (x : Nat) => x end;

    let pair(n : Nat, e : Zero(n)) -> {Nat, Nat} =
        match e: (_, _) => {Nat, Nat} | zero() => (1, 2) end;

    let a_curried_call(n : Nat, p : Zero(n), q : Zero(n))
        -> Eq(@(Nat))(into(n, p)(3), into(n, q)(3)) = Eq/refl();

    let under_a_lambda(n : Nat, p : Zero(n), q : Zero(n))
        -> Eq(@((Nat) -> Nat))((z : Nat) => through(n, p), (z : Nat) => through(n, q))
        = Eq/refl();

    let in_a_tuple(n : Nat, p : Zero(n), q : Zero(n))
        -> Eq(@({Nat, Nat}))((through(n, p), 0), (through(n, q), 0)) = Eq/refl();

    let in_a_let(n : Nat, p : Zero(n), q : Zero(n))
        -> Eq(@(Nat))((let z = through(n, p); z), (let z = through(n, q); z)) = Eq/refl();

    let in_a_list(n : Nat, p : Zero(n), q : Zero(n))
        -> Eq(@(List(Nat)))([through(n, p)], [through(n, q)]) = Eq/refl();

    let as_a_scrutinee(n : Nat, p : Zero(n), q : Zero(n))
        -> Eq(@(Nat))(match through(n, p): (_) => Nat | 0 => 0 | pred + 1 => 1 end,
                      match through(n, q): (_) => Nat | 0 => 0 | pred + 1 => 1 end)
        = Eq/refl();

    let as_an_operand(n : Nat, p : Zero(n), q : Zero(n))
        -> Eq(@(Nat))(through(n, p) + 1, through(n, q) + 1) = Eq/refl();

    let under_a_projection(n : Nat, p : Zero(n), q : Zero(n))
        -> Eq(@(Nat))(pair(n, p).0, pair(n, q).0) = Eq/refl();

    let past_a_stuck_head(n : Nat, b : Bool, f : (Zero(n)) -> Nat, g : (Zero(n)) -> Nat,
                          p : Zero(n), q : Zero(n),
                          x : Eq()((match b: (_) => (Zero(n)) -> Nat | true => f | false => g end)(p), 0))
        -> Eq()((match b: (_) => (Zero(n)) -> Nat | true => f | false => g end)(q), 0) = x;

    /std/print(Nat/to_str(1))
    "#;

pub const A_SPINES_ARGUMENTS_ARE_TYPED_UNDER_A_CURRIED_AND_A_PROJECTED_HEAD: &str = r#"
    use /std/{Eq, Nat};

    induct Z: (Nat) -> pub Prop
    | mk(): (0)
    end

    let past_a_curried_head(n : Nat, f : (Nat) -> (Z(n)) -> Nat, p : Z(n), q : Z(n),
                            x : Eq()(f(n)(p), 0))
        -> Eq()(f(n)(q), 0) = x;

    let past_a_projected_head(n : Nat, r : {(Z(n)) -> Nat, Nat}, p : Z(n), q : Z(n),
                              x : Eq()(r.0(p), 0))
        -> Eq()(r.0(q), 0) = x;

    /std/print(Nat/to_str(1))
    "#;

pub const AN_INTRINSIC_APPLIED_TO_TWO_PROOFS_CONVERTS_AT_THEIR_PROPOSITION: &str = r#"
    use /std/{Eq, Nat};
    use /std/Bool/{Holds};

    let halve(a : Nat, b : Nat, @p : Holds(0 < b)) -> Nat = Nat/div(a, b, @p);

    let same(a : Nat, b : Nat, p : Holds(0 < b), q : Holds(0 < b)) -> Eq()(halve(a, b, @p), Nat/div(a, b, @q)) = Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const A_POLYMORPHIC_DEFINITION_APPLIED_TO_TWO_PROOFS_CONVERTS: &str = r#"
    use /std/{Eq, Nat};

    let h(n : Nat, e : Eq()(n, n)) -> Nat = match e | refl(@_) => 1 end;

    let same(n : Nat, p : Eq()(n, n), q : Eq()(n, n), x : Eq()(h(n, p), 0)) -> Eq()(h(n, q), 0) = x;

    /std/print(Nat/to_str(1))
    "#;

pub const A_RECURSIVE_FUNCTION_CARRYING_A_PROOF_CONVERTS_WITHOUT_UNFOLDING: &str = r#"
    use /std/{Eq, Nat};

    induct T: pub Prop
    | t()
    end

    let g(n : Nat, p : T) -> Nat = match n | 0 => 0 | k + 1; _ => g(k, p) + 1 end;

    let same(n : Nat, p : T, q : T, x : Eq()(g(n, p), 0)) -> Eq()(g(n, q), 0) = x;

    /std/print(Nat/to_str(1))
    "#;

pub const A_RECURSIVE_FUNCTION_MATCHING_ITS_PROOF_CONVERTS: &str = r#"
    use /std/{Eq, Nat};

    induct T: pub Prop
    | t()
    end

    let g(n : Nat, p : T) -> Nat = match n | 0 => (match p | t() => 0 end) | k + 1; _ => g(k, p) + 1 end;

    let same(n : Nat, p : T, q : T, x : Eq()(g(n, p), 0)) -> Eq()(g(n, q), 0) = x;

    /std/print(Nat/to_str(1))
    "#;

pub const A_CAST_ALONG_A_HYPOTHESIS_THE_ALGEBRA_EQUATES_IS_ITS_ARGUMENT: &str = r#"
    use /std/{Eq, Nat};

    let along(P : (Nat) -> Type, a : Nat, b : Nat, p : Eq()(a + b, b + a), v : P(a + b))
        -> Eq()(Eq/subst(P, p, v), v) = Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const A_CAST_ALONG_A_PROOF_WRITTEN_AS_A_REDEX_IS_ITS_ARGUMENT: &str = r#"
    use /std/{Eq, Nat};

    let through_a_let(P : (Nat) -> Type, a : Nat, b : Nat, p : Eq()(a + b, b + a), v : P(a + b))
        -> Eq()(Eq/subst(P, (let q = p; q), v), v) = Eq/refl();

    let through_a_function(P : (Nat) -> Type, a : Nat, b : Nat, p : Eq()(a + b, b + a), v : P(a + b))
        -> Eq()(Eq/subst(P, ((q : Eq()(a + b, b + a)) => q)(p), v), v) = Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const A_CAST_ALONG_A_LEMMA_RUNS_NONE_OF_THE_LEMMA: &str = r#"
    use /std/{Eq, Nat};

    let slow(n : Nat) -> Eq()(n, n) =
        match n : (k) => Eq()(k, k)
        | 0 => Eq/refl()
        | k + 1; ih => Eq/cong((m : Nat) => m + 1, ih)
        end;

    let _closed : Eq()(Eq/subst((_) => Nat, slow(100000000000), 7), 7) = Eq/refl();

    let _open(n : Nat, v : Nat) -> Eq()(Eq/subst((_) => Nat, slow(n), v), v) = Eq/refl();

    let inner(@x : Nat, @y : Nat, e : Eq()(x + 1, y + 1)) -> Nat = match e | refl(@z) => z end;
    let widen(n : Nat) -> Eq()(n + 1, n + 1) = Eq/cong((m : Nat) => m + 1, slow(n));
    let _named : Eq()(inner(widen(100000000000)), 100000000001) = Eq/refl();

    /std/print(Nat/to_str(Eq/subst((_) => Nat, slow(100000000000), 7)))
    "#;

pub const AN_ARM_THAT_NAMES_A_PINNED_PAYLOAD_READS_THE_INDEX: &str = r#"
    use /std/{Eq, Nat, Option};

    let z_of(@a : Nat, @b : Nat, e : Eq()(a, b)) -> Nat = match e | refl(@z) => z + 1 end;

    let _open(a : Nat, e : Eq()(a, a)) -> Eq()(z_of(e), a + 1) = Eq/refl();
    let _closed : Eq()(z_of(Eq/refl(@Nat, @41)), 42) = Eq/refl();

    let inner(@x : Nat, @y : Nat, e : Eq()(x + 1, y + 1)) -> Nat = match e | refl(@z) => z end;

    let _left_open(x : Nat, e : Eq()(x + 1, x + 1)) -> Eq()(inner(e), x + 1) = Eq/refl();

    let boxed(@x : Nat, @y : Nat, e : Eq()(Option/some(x), Option/some(y))) -> Option(Nat) =
        match e | refl(@z) => z end;

    let _one_instance(x : Nat, e : Eq()(Option/some(x), Option/some(x)))
        -> Eq()(boxed(e), Option/some(x)) = Eq/refl();

    /std/print(Nat/to_str(z_of(Eq/refl(@Nat, @41))))
    "#;

pub const AN_INFERRED_VALUE_UNDER_A_REFINED_PROOF_KEEPS_ITS_UNIVERSE_INSTANCE: &str = r#"
    use /std/{Eq, Nat};

    induct Same(@P: Prop): (P, P) -> pub Prop | same(@p: P): (p, p) end

    let k(@x: Eq()(0, 0), _: Same()(x, x)) -> Nat = 1;

    let f(h : (A : Prop) -> A, w : Same()(h(Eq()(0, 0)), h(Eq()(0, 0)))) -> Nat =
        k(match h(Eq()(0, 0)) | refl(@_) => w end);

    /std/print(Nat/to_str(1))
    "#;

pub const A_PROOF_IS_NOT_REDUCED_TO_COMPARE_IT_WITH_ANOTHER: &str = r#"
    use /std/{Nat};

    induct Same(@P: Prop): (P, P) -> pub Prop | same(@p: P): (p, p) end

    induct T: pub Prop
    | t()
    end

    let spin(n : Nat) -> T = match n : (_) => T | 0 => T/t() | k + 1; ih => ih end;

    let within(y : T, w : Same(@T)(y, y)) -> Nat =
        let _v : Same(@T)(spin(100000000000), y) = w;
        0;

    /std/print(Nat/to_str(1))
    "#;

pub const A_STRUCTS_FUNCTION_FIELD_MEETS_A_NEUTRAL_APPLICATION: &str = r#"
    use /std/{Eq, Nat, State};

    let left(a: Nat, f: (Nat) -> State(Nat, Nat)) -> Eq()(State/bind(State/pure(a), f), f(a)) =
        Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const A_STRUCTS_FUNCTION_FIELD_MEETS_A_NEUTRAL_VARIABLE: &str = r#"
    use /std/{Eq, Nat, State};

    let right(m: State(Nat, Nat)) -> Eq()(State/bind(m, (v) => State/pure(v)), m) =
        Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const TWO_STRUCT_LITERALS_COMPARE_THEIR_FUNCTION_FIELDS: &str = r#"
    use /std/{Eq, Nat, State};

    let assoc(m: State(Nat, Nat), f: (Nat) -> State(Nat, Nat), g: (Nat) -> State(Nat, Nat))
        -> Eq()(State/bind(State/bind(m, f), g), State/bind(m, (v) => State/bind(f(v), g))) =
        Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const A_GROUP_MEMBER_IS_USED_A_LEVEL_UP_BY_ITS_SIBLING: &str = r#"
    use /std/{Nat};

    let pick(@A: Type, n: Nat, x: A) -> Nat =
        match n | 0 => 1 | k + 1 => other(k) end
    and other(n: Nat) -> Nat =
        match n | 0 => 1 | k + 1 => pick(k, Nat) end;

    /std/print(Nat/to_str(other(1)))
    "#;

pub const THE_SAME_PAIR_DECLARED_APART_CERTIFIES: &str = r#"
    use /std/{Nat};

    let pick(@A: Type, x: A) -> Nat = 0;
    let other(n: Nat) -> Nat = pick(Nat) + n;

    /std/print(Nat/to_str(other(1)))
    "#;

pub const A_PROOF_FIELD_DOES_NOT_DISTINGUISH_TWO_LITERALS: &str = r#"
    use /std/{Eq, Nat, Option, Str};

    induct P : pub Prop | mk() end
    struct S : pub Type { n : Nat, p : P }
    induct W : pub Type | wrap(Nat, P) end

    let field(n : Nat, p : P, q : P) -> Eq()(S { n = n, p = p }, S { n = n, p = q }) = Eq/refl();
    let payload(n : Nat, p : P, q : P) -> Eq()(W/wrap(n, p), W/wrap(n, q)) = Eq/refl();
    let some(p : P, q : P) -> Eq()(Option/some(p), Option/some(q)) = Eq/refl();
    let assoc(a : Str, b : Str, c : Str)
        -> Eq()(Str/concat(Str/concat(a, b), c), Str/concat(a, Str/concat(b, c))) = Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

pub const A_NOMINAL_STRUCTS_ETA_IS_NOT_FORFEITED_THERE: &str = r#"
    use /std/{Eq, Nat};

    struct Sealed : pub Type {
        one : Eq()(0, 0),
        two : Eq()(1, 1)
    }

    let same(f : (Sealed) -> Nat, b : Sealed, p : Eq()(0, 0), q : Eq()(1, 1))
        -> Eq()(f(Sealed { one = p, two = q }), f(b)) = Eq/refl();

    /std/print(Nat/to_str(1))
    "#;

// The same premise behind a stuck head. With the read an argument of the parameters `f`, `g` and `h`, weak-head reduction stops at the head without visiting it, so one spelling of the read would be refined to `true` in the outer arm and, after `Cell/fill(c, false)`, to `false` in the inner one — `h` carrying the outer arm's knowledge across, so `p` re-reads at `Eq()(false, true)` and `/std/Bool/false_neq_true` turns it into `/std/Bool/False`. Two heads rather than one because the kernel drops a nested refinement of a *single* key: `assume_case_value` reduces the inner scrutinee under the outer arm's equation, gets the literal `true` back, and `Scope::refine` skips a key with no local free. The read's type is `Io(Bool)`, which `f : (Bool) -> Bool` does not take, so the argument is refused before any arm records an equation.
// The route no search over the term could close. The scrutinee is `f(true)` for a *parameter* `f`: nothing in it names an effect, and at the moment an arm records its equation the binder has no value to inspect, so whether `f(true)` performs one is a property of the environment rather than of the term. The caller's `(b) => Cell/fill(c, true)` has type `(Bool) -> Io(Bool)` and does not inhabit `(Bool) -> Bool`, so the *caller's argument* is refused and the derivation never reaches an arm, a refinement, or an equation. What removes the class is an effect discipline on the arrow rather than a walk over the term (see `documentation/design/theory/effects-are-descriptions-and-the-carrier-has-no-eliminator.md`), and [`a_parameter_headed_scrutinee_refines_again`] is what such a walk would cost.
/// The description every fixture below takes apart, and the accepting control: `Io(False)` is built from an exit at a carrier with no constructors, so a program holding a description of a computation that yields `False` compiles and runs. What keeps it from being a proof is that nothing opens it.
pub const A_DESCRIPTION_OF_A_CONTRADICTION_STILL_COMPILES: &str = r#"
    use /std/{Io, Proc};
    use /std/Bool/{False};

    induct Empty : pub Type end

    pub let forged() -> Io(False) =
        Io/bind(Proc/exit(@Empty, 1), (e : Empty) => Io/pure(match e end));

    /std/print("ok")
    "#;

pub const A_FOLD_WHOSE_ARMS_INHABIT_A_MOTIVE_THAT_NAMES_ITS_SCRUTINEE_IS_ACCEPTED: &str = r#"
    use /std/{Nat};
    use /std/Bool/{True, Holds};

    let bounded(n : Nat, bound : Nat, h : Holds(n <= bound)) -> Holds(n <= bound) =
        match n : (_) => Holds(n <= bound)
        | 0 => h
        | k + 1; ih => ih
        end;

    let kept : Holds(3 <= 5) = bounded(3, 5, True/qed());

    /std/print("held")
    "#;

pub const A_FOLD_MOTIVE_THAT_BINDS_ITS_SCRUTINEE_STILL_FOLDS: &str = r#"
    use /std/{Nat, Eq};

    let plus_zero(n : Nat) -> Eq()(n + 0, n) =
        match n : (m) => Eq()(m + 0, m)
        | 0 => Eq/refl()
        | k + 1; ih => Eq/cong((w : Nat) => w + 1, ih)
        end;

    /std/print(Nat/to_str(3))
    "#;

pub const A_HYPOTHESIS_ABOUT_A_FOLDS_SCRUTINEE_RIDES_IN_ITS_MOTIVE: &str = r#"
    use /std/{Nat, proved};
    use /std/Bool/{True, Holds};

    let counted(n : Nat, bound : Nat, h : Holds(n <= bound)) -> Nat =
        match n : (m) => (h : Holds(m <= bound)) -> Nat
        | 0 => (_) => 0
        | k + 1; ih => (h) => ih(proved()) + 1
        end(h);

    /std/print(Nat/to_str(counted(3, 5, True/qed())))
    "#;

pub const A_PROPOSITION_HOLDING_A_POLYMORPHIC_PROOF_IS_ADMITTED: &str = r#"
    use /std/{Nat};
    use /std/Bool/{True};

    induct Fine: Prop
    | c(f: (A: Prop) -> (A) -> A)
    end

    let through(b: Fine, p: True) -> True = match b | c(f) => f(True)(p) end;

    /std/print(Nat/to_str(1))
    "#;
