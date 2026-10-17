# A terminal program describes its screen

Working specification for the widgets of `/std/Tui` and a whole program built of them, over the screen description, the protocol and the loop that have landed. One rule the tree states is broken today: [`/std` is laid out by tier, subject and kind](../../../curios-std/README.md#std-is-laid-out-by-tier-subject-and-kind) gives one meaning one spelling, and a widget's reaction to a key and a terminal's size are each spelled two ways in `/std/Tui`.

## What is wrong

- **A field moves by character.** `Field/handle` steps its cursor a `Char` at a time and `Field/view` scrolls by each character's columns alone, so a cursor stops between a letter and its combining mark, which `Str/graphemes` keeps one cluster, and a sequence joined by ZWJs scrolls as the sum of its parts.
- **Spellings.** `Field/handle(f, Key)` and `Listing/move(l, Key/Code)` are two shapes for one reaction. A size is `w, h` in a view and a pane and `cols, rows` in `Tty/Size`, which `Input/resize` carries.
- **A listing holds text alone and forgets where it scrolled.** `Listing` is a list of `Str`, so a program showing its own items converts them before and finds them again after; and `Listing/view` scrolls the selection into view from the top each time, so a selection moving up within the rows shown jumps the rows.

## What this builds on

- **The terminal is one device the host gives back.** `Tty/open` opens the controlling terminal read and write, `Tty/raw` takes it with the bytes that undo what the program sets on it, and `Tty/released` hands it back around an action and takes it again (`curios-runtime/src/os_host.rs`).
- **The screen is a description.** `Tui/View(w, h)` is read as its tiles by `Frame/of_view` and as its cursor request by `View/cursor`; a `Pane` is a view at the size it is given, which a program's `view`, `Listing/view` and `Field/view` answer and `Pane/rows` and `Pane/cols` share an axis among. Text is drawn as `Line`s of `Glyph`s, printable by proof ([the screen's decision](../../../curios-std/README.md#a-screen-is-a-description-at-its-size-and-a-layout-shares-an-axis-among-panes)).
- **What the terminal sends is read apart from what it says of itself.** `Input/decode` answers the inputs — a `Key` as the kitty keyboard protocol has it, a paste, a size reported in band, a change of focus — and beside them the `Reply`s to what a session asks.
- **The loop owns its session.** `Session/open` asks the terminal what it supports and sets up what it confirmed; a session is a handle; `Tui/run` is one task racing a read, the answers of its directives and, where the terminal reports no size, a deadline for asking it; a `Directive` is data, and `quit` waits for the work its update asked for.
- **The Unicode Character Database has one authority.** `/std/Str` steps by extended grapheme cluster: `Str/At/next_grapheme`, `try_next_grapheme`, `try_prev_grapheme` and `Str/graphemes`, the boundaries of UAX #29 over the classes `/std/Char` reads from the database `curios-unicode` pins.
- **Prior art.** brick's `List` widget keeps its items of any type and a view offset beside its selection; readline's keys move by word and kill to either end of a line.

## Decisions

Settled with the user on 2026-10-17.

- **A widget with state is a type module; one without is an operation of what it answers.** `Listing(A)`, `Field` and `Focus(P)` each have `handle` and `view`; `View/text` is a view's, and `Pane/paragraph`, `Pane/border`, `Pane/window` and `Pane/wave` are a pane's.
- **What waits for a consumer.** The mouse and named regions, kitty release events and report-all keys, subscriptions, an inline mode, images, a stop and continue on `SIGTSTP`, and a terminal in the browser harness are not part of this specification.

## Stages

1. **Widgets, and a whole program.** `Listing(A)`, showing its items through a function, keeping its scroll offset, moving by page and reading a key's modifiers; `Field` over graphemes, with `insert(f, Str)` for a paste and the readline keys — Ctrl with A, E, B, F, K, U and W, and Alt with B and F; `Focus(P)`, a bounded ring over the program's labels; and a program of a field and a listing in bordered panes under `Pane/rows` and `Pane/cols`, run against the scripted terminal through a resize and a save and quit. Check: the program's frames, its cursor in the field and its answer.

## Verification

`cargo xtask workspace format` and `cargo xtask workspace lint`, with the stage's check by name; then `cargo xtask workspace test`, `cargo xtask instrumented lint` and `cargo xtask docs build`. The figures the stages may move are named, not retaken: the prelude's elaboration, the `tui` workloads of `curios/src/tests/instrumented/coordination.rs`, `host_boundary.rs` and `fixpoint.rs`, and the launcher's size.

## Completion and retirement

The contracts go to the doc comments of `/std/Tui`, and the widget rule's rationale to `curios-std/README.md`'s decision on the screen. Once no reference to this specification remains, this file is deleted.
