# Syntax

This document defines the surface language accepted in `.crs` files. It is a reference for writing and reading Curios programs, not a description of compiler internals. An implementation disagreement is a language conformance bug: either the implementation or this document must be corrected.

Examples use declarations from `/std` and `/syn`. The authored libraries under `curios-prelude/std/` and `curios-prelude/syn/` are the main corpus of complete programs.

## Lexical structure

### Whitespace and comments

Spaces, tabs, and newlines separate tokens but otherwise have no meaning. Some operators require surrounding whitespace, as specified in [Operators](#operators).

A line comment begins with `--` and continues to the end of the line. There are no block comments.

```crs
-- A complete line comment.
let n = 1; -- A trailing comment.
n
```

Every comma-separated list — parameter and argument lists, tuple and struct fields, list literals, import groups — admits one optional trailing comma before its closing delimiter. A comma alone does not form an empty list.

### Identifiers

An identifier is a nonempty sequence of Unicode alphanumeric characters and `_`.

The following words are reserved and cannot be used as path segments:

| Declaration and expression words | Literal words |
| --- | --- |
| `let`, `match`, `choose`, `rec`, `mod`, `use`, `pub`, `end`, `induct`, `struct`, `foreign` | `true`, `false` |

`concept`, `satisfy`, and `and` are contextual words. They are recognized only in the grammatical positions that use them and remain valid identifiers and path segments elsewhere. `Type` and `Prop` denote sorts when parsed as terms, but they are not globally forbidden path segments.

### Paths

A path is one or more identifier segments separated by `/`. A leading `/` makes the path absolute; otherwise name resolution begins in the current lexical and module scope.

```crs
Nat                 -- relative name
Option/some         -- member of Option
/std/Lst            -- absolute name
/sys/Handle         -- absolute primitive declaration
```

Whitespace may appear around ordinary path separators. Packed `Bits` and `Bytes` spread operands use a separate tight grammar and must be written without whitespace; see [Packed literals](#packed-literals).

## Literals

### Numeric literals

Integer literals may be decimal, hexadecimal, or binary. An optional sign must touch the digits.

```crs
0
42
0xFF
0b1010
-7
+3
```

Elaboration chooses `Nat`, `Byte`, `Int`, or `Flt` from context. A written sign excludes `Nat` and `Byte`. `Byte` is selected only by an expected `Byte` type and accepts values from `0` through `255`. An unconstrained unsigned integer defaults to `Nat`; an unconstrained signed integer defaults to `Int`.

`-42` is one literal. `- 42` is parsed as an operator occurrence and is not a signed literal.

A floating-point literal has a decimal point followed by at least one decimal digit. It may have a sign and an `e` or `E` exponent.

```crs
5.0
-0.5
1.0e9
```

Floating-point literals have type `Flt`. `5.` is not a floating-point literal.

### Character and string literals

A character literal contains one Unicode scalar value or one supported escape and has the proof-certified type `Char`. `Char` excludes the surrogate range and values above `U+10FFFF`; use `Char/to_nat` for an explicit code-point conversion. Character literals are expressions only and are not accepted as `Nat` or `Byte` pattern spellings.

```crs
'c'
'\n'
'\''
```

Character escapes are `\n`, `\t`, `\r`, `\\`, and `\'`. An unrecognized escape in a character literal is a parse error.

A string literal has type `Str`.

```crs
"hello"
"first\nsecond"
```

String escapes are `\n`, `\t`, `\r`, `\\`, and `\"`. An unrecognized escape in a string literal is not an error: the backslash and the following character both stand for themselves, so `"\%"` is the two-character string `\%`. This is unlike a character literal, where an unrecognized escape is a parse error.

`Str` stores certified UTF-8 bytes. Its logical length, indexing, slicing, folding, and search operations count Unicode scalar values (`Char`), not bytes or grapheme clusters.

### Boolean literals

`true` and `false` have type `Bool`.

### List literals

A list literal constructs `Lst(T)`. Entries are elements or spreads; a spread inserts every element of another list at its position.

```crs
[]
[1, 2, 3]
[head, ..middle, tail]
```

A nonempty literal may infer `T` from its elements. An empty literal needs an expected list type from its position, such as a binder annotation:

```crs
let empty : Lst(Nat) = [];
```

Spreads may appear in any position and may be repeated. Every element and spread operand must agree on the same element type.

### Packed literals

A `Bits` literal begins with `b`; each atom is `\0` or `\1`. A `Bytes` literal begins with `x`; each atom is `\` followed by exactly two hexadecimal digits.

```crs
b\                 -- empty Bits
b\0\1\1
x\                 -- empty Bytes
x\48\69
```

Packed atoms are written least-significant first. The first bit written occupies the least-significant available packed bit.

`\..` spreads another packed value of the same kind:

```crs
b\1\..rest\0
x\48\..suffix\00
x\..header.bytes
x\..make_bytes(n)
x\..(pick(flag, a, b))
```

The entire packed literal is whitespace-free. An unparenthesized spread operand must be a glued name, projection, application, or postfix-`!` chain. Parentheses admit an arbitrary term. A following `\` resumes the literal. `Bits` and `Bytes` cannot be mixed.

## Sorts and types

### Sorts

`Type` is the sort of computational types. `Prop` is the sort of proof-irrelevant propositions.

Although the surface spelling is always the nullary term `Type`, each occurrence has an implicit level in a cumulative hierarchy. The compiler infers those levels and generalizes reusable declarations over them; there is no syntax for universe variables, levels, or explicit universe arguments. A type accepted at one level is also accepted where a higher level is required.

All inhabitants of the same proposition are definitionally irrelevant. Eliminating a proposition into informative data is restricted; proofs may always be eliminated to prove another proposition.

### Function types

A function type is a parenthesized dependent parameter list followed by `->` and its result.

An explicit parameter is written `name : type` or as an unlabeled type. An implicit parameter begins with `@`. A witness parameter begins with `use` and is anonymous.

```crs
(Nat) -> Nat
(x : Nat, y : Nat) -> Nat
(@A : Type, x : A) -> A
(@A : Type, use Show(A), value : A) -> Str
```

Later parameter types and the result may refer to earlier named parameters.

### Tuple types

A tuple type is a dependent field telescope enclosed in braces.

```crs
{Nat, Bool}
{fst : Nat, snd : Bool}
{value : A, proof : Valid(value)}
{}
```

Later fields may refer to earlier named fields. The empty tuple type `{}` is the unit type.

A labeled function field may use signature sugar:

```crs
{run(input : Bytes) -> Async(Nat)}
```

This is equivalent to:

```crs
{run : (input : Bytes) -> Async(Nat)}
```

## Expressions

### Unit and tuples

`()` is the unit value. It is distinct from `{}`, the unit type.

Tuple values use parentheses and comma-separated fields:

```crs
(1, true)
(left = 1, right = true)
()
```

Labeled fields may use function-definition sugar:

```crs
(base = 3, bump(x) = x + 1)
```

### Names, calls, and projections

A path refers to a value. A call supplies a parenthesized argument list:

```crs
f(x)
Map/lookup(map, key)
```

Arguments are divided into three independent queues:

- `value` supplies an explicit parameter;
- `@value` supplies an implicit parameter;
- `use value` supplies a witness parameter explicitly.

```crs
f(@Nat, x)
join(use custom_show, values)
```

Omitted implicit arguments are inferred. Omitted witness arguments are resolved as described in [Witness resolution](#witness-resolution).

A projection is positional or labeled:

```crs
pair.0
pair.fst
configuration.network.port
```

Calls, projections, and postfix `!` may be chained.

### Lambdas

A lambda is a comma-separated parameter list followed by `=>` and a body.

```crs
(x) => x
(x : Nat) => x + 1
(f, value) => f(value)
```

Lambda parameters may be plain binders or irrefutable tuple and struct patterns. An annotation may be written when the parameter type is not supplied by context.

A lambda's parameter list is a dependent telescope, exactly as a function type's is: a later parameter's annotation may name the parameters written before it, including the leaf names bound by an earlier tuple or struct pattern. An earlier parameter shadows a like-named module binding inside a later annotation, just as it does inside the body.

```crs
(s : A, t : A, q : Eq(s, t)) => proof(q)
((lo, hi), q : Eq(lo, hi)) => lo
```

A lambda parameter carries the same plicity mark as a function-type parameter: `@name` binds an implicit slot, `use name` binds a witness slot, and an unmarked binder binds an explicit slot. The mark applies to the slot the parameter occupies whatever the pattern shape. Each written binder is checked against the plicity of the slot it claims when the lambda is checked against an expected function type.

```crs
(@A, value) => value
(@A, use show, value) => Show/show(value)
```

An omitted implicit or witness binder is inserted automatically from the expected function type, so hidden binders may be left out when the body does not name them. Alignment is positional by plicity: among the parameters of the expected type, each written binder claims the next slot of its own plicity, and every skipped implicit or witness slot before it is inserted. A plain binder never silently binds a hidden slot. For the expected type `(@A : Type, use Show(A), value : A) -> Str`, every one of `(value) => …`, `(@A, value) => …`, `(use show, value) => …`, and `(@A, use show, value) => …` is accepted; `(A, show, value) => …` is not, because `A` binds the sole explicit slot and the remaining binders are surplus.

### Local `let`

A local `let` binds a value throughout the term after its terminating `;`.

```crs
let x = compute();
let y : Nat = 0;
x + y
```

Function-definition sugar introduces parameters and an optional result type:

```crs
let increment(n : Nat) -> Nat = n + 1;
increment(4)
```

The binder may be an irrefutable tuple or struct pattern:

```crs
let (x, y) = pair;
let Point { x, y } = point;
x + y
```

### Local `rec`

`rec` introduces locally scoped recursive definitions. Every recursive member requires a type. `and` joins a mutually recursive group.

```crs
rec even(n : Nat) -> Bool =
    match n
    | 0 => true
    | p + 1; _ => odd(p)
    end
and odd(n : Nat) -> Bool =
    match n
    | 0 => false
    | p + 1; _ => even(p)
    end;
even(input)
```

### Irrefutable binder patterns

The binders of `let`, lambdas, and function-definition sugar accept nested tuple and struct patterns.

```crs
let (x, (_, y)) = value;
let Point { loc = (x, y), color } = point;
body
```

These patterns are projection sugar, not runtime matches. The struct head is documentary and is not resolved or checked; fields are matched positionally. Field punning such as `Point { x, y }` binds fields with the same written names.

Refutable patterns belong only to `match`.

### Written goals

`?` is a development goal. It asks the elaborator to infer as much as possible, records the local scope and expected type, and then causes compilation to fail with a report.

```crs
let compose(@A : Type, @B : Type, @C : Type, f : (B) -> C, g : (A) -> B) -> (A) -> C =
    ?;
compose
```

A goal is never accepted in a successfully compiled program.

### Postfix `!`

`action!` is monadic sequencing. Each occurrence is equivalent to a call to `/syn/Monad/bind(action, continuation)`.

```crs
let parser : Parse(Nat) =
    let a = Parse/any_byte!;
    let b = Parse/any_byte!;
    Parse/pure(a + b);
```

Every value body is a sequencing region. Lambda bodies, match arms, and recursive member bodies begin fresh regions; the tail after a local `let` remains in the same region. There is no `let !` header or matching `end`.

Postfix `!` is not allowed in types. The token `!=` is an infix operator and is not parsed as postfix `!` followed by `=`.

### Whole-term forms and operand positions

`let`, `rec`, `match`, `choose`, lambdas, and function types are whole-term forms: a body or tail extends to the end of the enclosing term. There is no expression-level `term : type` ascription; a `:` annotation appears only in binder, signature, and motive positions.

An infix operand is an applied atom: a literal, name, tuple, structure literal, or parenthesized term, followed by any chain of calls, projections, and postfix `!`. A whole-term form is not an operand; parenthesize it to use it as one.

```crs
1 + (match flag | true => 1 | false => 0 end)
```

Positions that accept a full term need no parentheses: call arguments, list elements, field values, match scrutinees, and arm bodies.

## Operators

All infix operators require whitespace on both sides and associate to the left.

| Precedence | Operators | Concept dispatch |
| --- | --- | --- |
| 1, loosest | `\|\|` | `Or` |
| 2 | `&&` | `And` |
| 3 | `==`, `!=`, `<`, `>`, `<=`, `>=` | `Eql`, `Cmp` |
| 4 | `+`, `-` | `Add`, `Sub` |
| 5, tightest | `*`, `/`, `%` | `Mul`, `Div`, `Rem` |

Both operands of an operator have the same type. `!=` negates the result of `Eql/eql`; it is not a separate concept method.

Operator notation always uses witness resolution, including primitive operands. Standard witnesses cover the primitive types, while a `satisfy` declaration enables the same notation for a user-defined type.

## Pattern matching

### Match shell and motives

A headed match has a scrutinee, an optional motive, one or more `| pattern => body` arms unless the eliminated type is empty, and a closing `end`.

The motive states the result type as a family. It is an ordinary term, checked against the eliminator's motive type — a function of the scrutinee's indices, in declaration order, and then the scrutinee:

```text
(indices) -> Scrutinee(indices) -> Sort
```

There is no motive grammar. What follows `:` is parsed as a term and terminates at the first arm, since `|` is not an infix operator.

```crs
match b : (_) => Nat                            -- result ignores the scrutinee
match n : (m) => P(m)                           -- result depends on it
match p : (s, t, q) => Eq(t, s)                 -- an indexed family
match p : (s : A, t : A, q : Eq(s, t)) => Eq(t, s) -- with written annotations
match p : discriminates_eq                      -- a named family
match v                                         -- omitted; inferred
```

The number of binders is fixed by the eliminated type: one per index, then one for the scrutinee. A non-indexed scrutinee — every primitive carrier, and any inductive declared without an index telescope — therefore takes exactly one binder, so a result that ignores the scrutinee is written `(_) => T`. `Vec(T)` has one index and takes two binders; `Eq` has two and takes three.

Parameters are never binders. They are uniform across constructors and fixed by the scrutinee's type, so the motive body refers to them through the ambient scope, exactly as the declaration side states only index expressions in a constructor's case target.

Each arm is checked against the motive at that constructor's target indices, and the match as a whole at the scrutinee's actual indices. A `| _ =>` default binds nothing and refines no index, so it is checked at the actual indices too.

A binder may be written bare, as `_`, or annotated. An annotation is an ordinary type in an ordinary position: it is checked by conversion against the binder's expected type, obeys the usual plicity rules, and may name the binders written before it. Annotating the scrutinee binder is how a reader recovers the eliminated family on the motive line.

```crs
match p : (s, t, q : Eq(s, t)) => Eq(t, s)
```

Omitting the motive asks the elaborator to infer it. Prefer omission wherever inference succeeds; a written motive is needed where there is nothing to infer from — a type-level match whose result appears in a signature, or an elimination in inference position.

### Inductive patterns

An inductive pattern names a constructor and supplies one pattern per payload position.

```crs
match option
| some(value) => use(value)
| none() => fallback
end
```

A payload position the constructor declared implicit (`@`) must be matched with `@`; a plain payload is matched without a mark. A constructor pattern supplies one pattern per payload position, hidden ones included — omitted hidden payload patterns are not inserted (unlike lambda binders). Witness payloads are not a surface feature, so `use` is not accepted in a constructor pattern.

```crs
match vector
| cons(@length, head, tail) => head
end
```

Patterns may nest through constructors, tuples, structs, booleans, naturals, lists, `Bits`, and `Bytes`.

```crs
match value
| some([head, ..tail]) => consume(head, tail)
| none() => empty
end
```

Concrete constructor rows have no priority. Each reachable combination needed by the program must be represented by a compatible row.

### Inductive default arm

A final top-level bare `_` may follow concrete inductive arms and covers constructors not named earlier.

```crs
match option
| some(value) => use(value)
| _ => fallback
end
```

Only a bare `_` in this exact position is a default. A named binder is not a catch-all. Nested wildcard defaults are not accepted. A lone `_` with no concrete arm is an irrefutable binder match rather than an inductive default.

### Multiple scrutinees

A tuple scrutinee supports matrix matching over several values. Columns are considered left to right, grouping rows by the shape in each column.

```crs
match (left, right)
| (some(x), some(y)) => x + y
| (some(x), none()) => x
| (none(), _) => 0
end
```

A binder may occupy a later column when an earlier column has already distinguished its row. A binder and a concrete shape cannot compete in the same column of the same group.

Tuple and struct patterns over non-inductive values compile to projections rather than constructor dispatch.

### Boolean match

A `Bool` match has exactly one `true` arm and one `false` arm, in either order.

```crs
match condition
| true => yes
| false => no
end
```

### Natural-number induction

Natural-number induction has a zero arm and a successor arm. The successor arm binds the predecessor before `+ 1`; a binding after `;` receives the induction hypothesis for the predecessor, and omitting it makes the arm an ordinary case split.

```crs
match n : (m) => P(m)
| 0 => base
| predecessor + 1; hypothesis => step(predecessor, hypothesis)
end
```

### Natural-number dispatch

A natural-number dispatch has literal arms and a mandatory `_` default.

```crs
match tag
| 0 => first
| 1 => second
| _ => otherwise
end
```

Induction arms and literal-dispatch arms cannot be mixed in one match.

Natural-number pattern literals are numeric literals only. Character literals do not provide character, natural-number, or byte patterns.

### List fold and case split

A list match uses `[]` for the empty case and `[head, ..tail]` for the nonempty case. A binding after `;` receives the fold result for `tail`; omit it for an ordinary case split.

```crs
match values
| [] => base
| [head, ..tail]; hypothesis => step(head, hypothesis)
end
```

### Packed folds

`Bits` and `Bytes` use their literal prefixes to select the carrier. The nonempty arm binds the leading element and tail; an optional binding after `;` receives the fold result for the tail. A `Bits` head has type `Bool`; a `Bytes` head has type `Byte`.

```crs
match bits
| b\ => base
| b\head\..tail; hypothesis => step(head, hypothesis)
end

match bytes
| x\ => base
| x\head\..tail => inspect(head, tail)
end
```

## Choose

`choose` is an ordered guarded ladder, not a match — it consumes no scrutinee. Arms are tried from top to bottom, and a final `_` arm is mandatory.

```crs
choose
| condition => when_true
| some(value) = lookup(key) => when_found(value)
| _ => fallback
end
```

A condition arm fires when its expression evaluates to `true`. A bind arm evaluates the expression on the right of `=` and fires when it matches the refutable pattern on the left. A bare binder is not allowed as a bind-arm pattern because it cannot fail; use `let` instead.

Each selected arm receives the same definitional refinement that an equivalent nested headed match would provide.

## Declarations and modules

An entrypoint consists of zero or more top-level items followed by exactly one final term — the value the program computes. A module file consists of top-level items only and has no final term.

### Top-level definitions

Top-level `let` declarations require a type annotation. Function-definition sugar supplies the annotation as a parameter telescope and result type.

```crs
pub let zero : Nat = 0;

pub let map(@A : Type, @B : Type, value : Option(A), f : (A) -> B) -> Option(B) =
    match value
    | some(x) => Option/some(f(x))
    | none() => Option/none()
    end;
```

Top-level `rec` declarations also require types. `and` joins mutually recursive members; each member takes its own `pub` marker — before `rec` for the first member and before `and` for each later member — and one `;` terminates the whole group.

### Modules

A file-backed module ends its declaration with `;` and loads `Name.crs`. An inline module ends with `end`.

```crs
pub mod Nat;

pub mod Internal
    pub let value : Nat = 1;
end
```

### Imports and re-exports

`use` imports paths. Prefixing it with `pub` re-exports what it imports.

```crs
use /std/{Nat, Bool};
pub use Option/*;
use /syn/Str/{classify, step};
```

Inside a group, a bare name imports both a child module and a value with that name when both exist. `mod Name` imports only the module namespace; `let Name` imports only the value namespace.

```crs
pub use Lst/{let Lst};
use Package/{mod Syntax, let parse};
```

### Visibility

One rule governs every declaration, in both namespaces:

> A declaration written **without** `pub` in module `M` is visible exactly within `M`'s subtree — `M` itself and its descendants at any depth. A declaration written **with** `pub` is additionally visible wherever `M` itself is visible.

Reachability along a path is the conjunction of that rule at each hop, and the root's subtree is the whole program. So a descendant may name its ancestors' private declarations, while ancestors and siblings may not: `Owner/Worker` can reach a private binding of `Owner`, but neither `Owner` nor a sibling `Owner/Other` can reach a private binding of `Owner/Worker`. `pub` inside a private module means "wherever this module is visible", which is that module's own audience rather than the whole program — the facade pattern, where a public module re-exports selected names out of a private child.

`struct`, `induct`, and `concept` have a second, declaration-local `pub` before their result sort; this independently exposes their representation, under the same subtree rule. A private representation is transparent throughout its declaring module's subtree, so an abstraction can be implemented across several files without exporting how it is built.

Globs are the exception: `use M/*` and `pub use M/*` import the exported surface only, never a subtree-private declaration. Reaching one always requires naming it.

A public interface cannot mention an item its own consumers cannot reach. The interface includes:

- parameters, indices, and result sorts of publicly reachable nominal declarations;
- struct and concept fields when the representation is public;
- inductive constructor signatures when the inductive representation is public;
- declared types of definitions.

The check compares audiences rather than declaration paths, so a name re-exported out of a private child counts as visible wherever the re-export puts it, and an item that reaches only a subtree may freely mention other declarations of that subtree. It follows re-exports, identity aliases, and direct-headed type-family aliases whose declared result structurally ends in literal `Type` or `Prop`. Ordinary definition bodies are not part of the public interface, and neither are the signatures of members synthesized into a nested namespace — an inductive's constructors and a concept's method wrappers — so a constructor facade may hand out values of a type the consumer cannot name.

## Inductive declarations

An inductive declaration introduces a nominal family and its constructors.

```crs
pub induct Option(A : Type) : pub Type
| some(A)
| none()
end
```

Parameters follow the name. A parameter marked `@` is implicit at the type constructor; all inductive parameters are implicit at value constructors.

The required result annotation is either a sort or an index telescope followed by a sort:

```crs
pub induct Vec(T : Type) : (length : Nat) -> pub Type
| nil() : (0)
| cons(@n : Nat, head : T, tail : Vec(T, n)) : (n + 1)
end
```

Each index binder may be named or left bare — `(length : Nat)` and `(Nat)` are both well-formed — and an index never takes `@`. The name is never in scope in the constructor cases; it appears in the family's printed signature, and a later entry of the same telescope may depend on it.

Each constructor of an indexed family must state the indices it produces after `:`. A non-indexed constructor does not accept a target.

An inductive proposition is declared with `Prop`:

```crs
pub induct Eq(@A : Type) : (left : A, right : A) -> pub Prop
| refl(@value : A) : (value, value)
end
```

The outer `pub` exports the family name. The inner `pub` exports construction and every form of elimination. Without the inner marker, constructor access and pattern matching are restricted to the declaring module's subtree.

Mutually recursive inductives are separated by `and` within one block. Each member has its own outer and representation visibility markers.

## Structure declarations

A structure is a nominal dependent record.

```crs
pub struct Pair(A : Type, B : Type) : pub Type {
    fst : A,
    snd : B,
}
```

A single unlabeled field defines a newtype-like structure and is projected with `.0`.

```crs
pub struct Meters : pub Type { Nat }
```

The outer `pub` exports the type name. The inner `pub` exports construction and projection. Without the inner marker, those operations are restricted to the declaring module's subtree.

A `Prop` structure may contain only non-informative fields.

### Structure literals

A structure value names its type and supplies its fields. Parameterized heads may supply type parameters before the field block.

```crs
Pair { fst = 1, snd = true }
Pair(Nat, Bool) { fst = 1, snd = true }
Api { base = 3, bump(x) = x + 1 }
```

Fields are checked in declaration order. Function-definition sugar is equivalent to assigning a lambda.

### Structure update

A leading `..base` copies a value of the same nominal structure. Labeled entries following it replace fields.

```crs
Pair { ..pair, snd = false }
Pair(Str, Nat) { ..pair, fst = "new" }
```

The spread must be first and may occur only once. Every override must be labeled and overrides must follow declaration order. The head may choose different parameters, but every copied and replaced field is checked at that new instantiation; dependent fields must therefore remain consistent.

Tuple and string literals do not have this update form. List and packed spreads are concatenation forms governed by their literal sections.

## Concepts and witnesses

Concepts provide ad-hoc polymorphism. A concept is a record-shaped interface, a witness is a registered inhabitant of a concept application, and a `use` parameter asks the elaborator to supply such an inhabitant.

`concept` and `satisfy` are contextual words: they remain ordinary identifiers outside their declaration positions.

### Concept declarations

A concept has zero or more parameters, a required representation sort, and a field list. The representation sort follows the struct rules: `: pub Type` declares a transparent concept, and `: Type` a *sealed* one, whose representation is private to its declaring module's subtree — witness declarations, dictionary literals, structure updates, and raw field projections are then permitted only there. Resolution, `use` parameters, and the generated method wrappers work the same either way, and visibility of the concept's name remains independent of its representation.

```crs
pub concept Show(A : Type) : pub Type {
    show(A) -> Str,
}

pub concept Monad(M : (Type) -> Type) : pub Type {
    pure(@A : Type, value : A) -> M(A),
    bind(@A : Type, @B : Type, action : M(A), next : (A) -> M(B)) -> M(B),
}
```

Every ordinary field receives a wrapper in the concept's namespace, so `Show/show(value)` asks for an implicit witness of `Show(A)` and projects its `show` implementation.

A field beginning with `use` is an anonymous superclass edge. Its type must be a concept application.

```crs
pub concept Ord(A : Type) : pub Type {
    use Eql(A),
    cmp(A, A) -> Order,
}
```

A local `Ord(A)` witness can therefore satisfy an `Eql(A)` goal by superclass projection.

A sealed concept's fields are not part of its public interface: a `pub` sealed concept may reference private names in its field types — a private superclass is a hidden obligation that resolution discharges without the consumer naming it. A transparent `pub` concept's field types are interface and must be `pub` themselves.

A concept returning `Prop` (or `pub Prop`) has proof-irrelevant witnesses that erase completely.

### Witness declarations

`satisfy` registers an anonymous witness. Its terminal type is a concept application and its body supplies the concept fields.

```crs
satisfy Show(Nat) {
    show(n) = Nat/to_str(n),
}
```

A witness may quantify over implicit parameters and require other witnesses. A nonempty telescope is separated from the concept application by `=>`. It cannot declare explicit parameters because resolution has no explicit arguments to supply.

```crs
satisfy (@A : Type, use Show(A)) => Show(Lst(A)) {
    show(values) = Lst/fold(values, "", (value, result) => Str/concat(result, Show/show(value))),
}
```

Every registered witness is keyed by the concept name and the tuple of rigid heads of every concept parameter. Each head must reduce to an inductive, structure, primitive type, or supported higher-kinded type constructor. Remaining arguments below those heads are checked by unification after lookup.

A globally registered witness therefore requires a concept with at least one parameter. A parameterless concept can still be used through an ordinary value supplied in a local `use` scope.

For example, witnesses for `Into(Nat, Str)` and `Into(Nat, Bool)` have distinct keys. A call must determine both parameters from its explicit arguments, expected result, or an explicitly supplied witness before automatic lookup can proceed.

Only one witness may occupy a key across the whole program. Module visibility does not scope witness registration, but a *sealed* concept's representation does gate declaration: its witnesses may only be declared within the concept's declaring module's subtree.

To use a second dictionary for the same key on a *transparent* concept, construct an ordinary concept value and supply it explicitly (a sealed concept forbids the literal outside its module):

```crs
let reverse : Ord(Nat) = Ord { cmp(a, b) = compare_reverse(a, b) };
sort(use reverse, values)
```

### Witness premises

A witness premise must be a concept applied only to variables bound by the witness's own telescope. This regularity restriction makes recursive resolution structurally decreasing.

### Orphan rule

A witness may be declared only by the compilation root that owns its concept or at least one rigid type head in its key. This prevents independent third parties from defining the same globally coherent instance.

The coordinated `/sys`, `/syn`, and `/std` roots are exempt from the restriction against one another.

### Superclass fields in literals

A concept's superclass fields remain positional slots in concept values and witness bodies. Omitting one asks witness resolution to fill it. `use value` fills the next superclass slot explicitly.

```crs
Ord { use custom_eql, cmp(a, b) = compare(a, b) }
```

In a structure update, a spread copies superclass fields from the base. An explicit `use value` after the spread replaces the corresponding slot.

### Witness parameters and arguments

A witness parameter is written `use Concept(args)` in a function type or definition telescope. It is anonymous but joins the witness scope of the function body.

```crs
pub let join(@A : Type, use Show(A), values : Lst(A)) -> Str =
    Lst/fold(values, "", (value, result) => Str/concat(result, Show/show(value)));
```

At a call site, `use value` supplies a witness argument explicitly and overrides resolution.

```crs
join([1, 2, 3])
join(use custom_show, [1, 2, 3])
```

### Witness resolution

An omitted witness argument is resolved in this order:

1. Search local `use` parameters from innermost to outermost; the first direct match wins.
2. Search superclass projections of local witnesses breadth-first; more than one match at the same minimum depth is ambiguous.
3. Look up the concept and the rigid heads of every parameter in the global witness table.

If any concept parameter is still headed by an unsolved metavariable, resolution waits until that metavariable is solved. A selected global witness is instantiated with fresh implicit arguments, its witness premises are resolved recursively, and its full result type is unified with the goal.

Higher-kinded parameters are keyable. When conversion establishes a shape such as `M(A) = Option(Nat)`, it may infer `M` as the `Option` type constructor, allowing `Monad(Option)` lookup.

## Foreign declarations

A `foreign` declaration introduces a value implemented by the embedder. Its declared type uses the wire grammar rather than arbitrary Curios types.

```crs
foreign random : Nat;
foreign frobnicate : (Nat, Bin) -> Nat;
pub foreign log : (Bin) -> Nat;
```

The wire types are `Nat`, `Int`, `Bool`, `Bin`, `Handle`, and recursively `Lst(T)`. A wire signature is a bare wire result type for a zero-argument foreign or a parenthesized wire parameter list followed by `->` and a wire result type.

Wire `Bin` maps to object-language `Bytes`. `Byte` and `Bits` are not distinct wire types. The Wasm import uses the declaration's fully qualified name in the `ffi` namespace.

## Equality and proofs

Propositional equality `Eq` is an ordinary indexed inductive proposition from `/std/Eq`. Its proofs use the same constructors, functions, and match forms as other inductives.

```crs
pub let sym(@A : Type, @x : A, @y : A, proof : Eq(x, y)) -> Eq(y, x) =
    match proof : (left, right, p) => Eq(right, left)
    | refl(value) => Eq/refl()
    end;
```

The standard equality operations include reflexivity, symmetry, transitivity, congruence, and substitution. `Eq` is propositional equality; `Eql` is the value-level concept used by `==` and `!=`.

## Quick reference

| Form | Meaning |
| --- | --- |
| `{}` | Unit type |
| `()` | Unit value |
| `@A : Type` | Implicit binder |
| `@value` | Explicitly supplied implicit argument |
| `use C(A)` | Automatically resolved witness binder |
| `use value` | Explicitly supplied witness argument or superclass field |
| `?` | Written elaboration goal that always reports and fails compilation |
| `term!` | Monadic bind through `Monad` |
| `Name { ... }` | Structure or concept literal |
| `Name { ..base, ... }` | Structure update |
| `match term ... end` | Typed elimination or dispatch |
| `choose ... end` | Ordered guarded ladder |
| `satisfy C(args) { ... }` | Globally registered anonymous witness |
| `satisfy (@A : Type, use C(A)) => D(args) { ... }` | Parameterized globally registered anonymous witness |
