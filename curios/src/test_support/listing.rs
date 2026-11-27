//! The bordered listing a terminal session draws, which the terminal suites drive and the bench's measurements of coordination and the host boundary run.

pub const LISTING_PROGRAM: &str = r#"
    use /std/{Nat, Str, Bool, List, Option, Try, Io, Show, Tui};
    use /std/Tui/{Style, Pane, Key, Event, Directive, Listing};
    let app: Tui(Listing(Str), Nat) =
        Tui {
            init = (Listing/new(["one", "two", "three"]), Directive/none()),
            update(model, event) =>
                match event
                | input(key(k)) =>
                    match k.code
                    | char(c) => match c == 'q' | true => (model, Directive/quit()) | false => (model, Directive/none()) end
                    | _ => (Listing/handle(model, k, 3), Directive/none())
                    end
                | _ => (model, Directive/none())
                end,
            view(model) => Pane/border(Style/plain, "pick", Listing/view(model, (s: Str) => s, Style/plain, Style { ..Style/plain, reverse = true })),
        };
    let program() -> Io({}) =>
        let outcome = Try/run(Tui/main(app))!;
        match outcome
        | success(model) => /std/print(Option/unwrap_or(Listing/selected(model), "none"))
        | failure(e) => /std/print(Show/show(e))
        end;
    program()
    "#;
