# A cookie is read and written as RFC 6265 states it

**Not refined yet.** This specification reserves `/std/http`'s reading of a `Cookie` field and its reading and writing of a `Set-Cookie` field with its attributes. It is not an implementation plan; its gate is a server or a client of `/std/http` that keeps a session.

## What is missing

- **`http` treats both fields as opaque.** A server that keeps a session reads `Cookie` and writes `Set-Cookie`, and a client that logs in sends back what it was set; `/std/http` hands both on as field values, so each program parses RFC 6265's grammar and its attributes (`Expires`, `Max-Age`, `Domain`, `Path`, `Secure`, `HttpOnly`, `SameSite`) by hand.

## What is known

- **Peers ship the grammar.** Go's `net/http` (`Cookie`, `SetCookie`, `Request.Cookies`), Python's `http.cookies` and Java's `HttpCookie` parse and render both fields.
- **The grammar has one answer and a jar does not** (decided by the user on 2026-11-23). RFC 6265 states both fields' syntax and its date and path matching, while a client's jar decides which domains a cookie reaches by the public suffix list, data that changes outside any library, so the jar is left out, as [`/std` answers the questions a program cannot avoid, where each has one correct answer](../../design/prelude/std-answers-the-questions-a-program-cannot-avoid-where-each-has-one-correct-answer.md) bounds the library. `http/Client`'s `redirected` keeps handing on the request's own fields within its origin alone.

## Open

- **The types**: a cookie as a name, a value and its attributes, and whether `Expires` reads into `/std/time`'s moment.
