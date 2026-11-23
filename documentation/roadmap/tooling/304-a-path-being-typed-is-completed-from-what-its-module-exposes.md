# A path being typed is completed from what its module exposes

**Not refined yet.** A path being typed in an editor is offered the names its module exposes, as `curios wonder --name <PATH>` lists a module's at the command line ([`wonder`](../../usage.md#wonder)): today the server answers no completion, and a reader finds what a module offers by asking the command or reading its page.

## What is known

- **What a module exposes is read off the documentation record and the lowering**, the names in the order its record lists them and what each is declared as (`exported`, `curios-wonder/src/named.rs`), so a page, the listing and a completion would offer one set.
- **An item the parser cannot read costs that item** (`Module::parse`, `curios-surface/src/module.rs`), so the declarations around one being typed stay checked, and the one being typed answers nothing from its check.
- **A path's segments answer what they reach**, the walk the lowering takes through a path kept with its declaration (`curios_module::Reached`, [A place in a file is answered from the declaration that writes it](../../design/tooling/a-place-in-a-file-is-answered-from-the-declaration-that-writes-it.md)): a completion asks the module the segments before the one being typed reached.

## What it waits on

- **What a path being typed parses to**: a path ending in `/`, or in a partial segment, is no item the parser reads, so the request has no lowered term and no recorded walk at its place, and the protocol thread parses nothing of a document ([A place in a file is answered from the declaration that writes it](../../design/tooling/a-place-in-a-file-is-answered-from-the-declaration-that-writes-it.md)).

## Prior art

- rust-analyzer completes in a file that does not parse by parsing it again with a marker at the cursor: "Insert a fake ident to get a valid parse tree. We will use this file to determine context, though the original_file will be used for actual completion." ([`context.rs`](https://github.com/rust-lang/rust-analyzer/blob/master/crates/ide-completion/src/context.rs)); Lean completes from the info tree's completion nodes the elaborator leaves where an identifier was being elaborated ([`InfoTree/Types`](https://lean-lang.org/doc/api/Lean/Elab/InfoTree/Types.html)).

## Verification and retirement

Check, once refined: completion of an unfinished path through a scripted server session, the names offered equal to the entries `--name` answers its module with. Once it lands, `documentation/usage.md`'s `--server` row states it, and this file is deleted.
