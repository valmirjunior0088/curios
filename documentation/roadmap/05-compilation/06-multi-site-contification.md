# Contification of a function with several return contexts

**Not refined yet.** This specification reserves contification where one function returns to more than one caller context. It is not an implementation plan.

## What is missing

`curios-cont`'s `contify_calls` turns a function into a continuation where every call returns to one context. A function with two external return contexts stays a function, and nothing downstream contifies it: `curios-emit`'s machine lowering performs no contification.

## Refinement

Common-dominator placement for a function returned to from several contexts, which analysis owns it — the continuation IR or the machine CFG — and which programs it moves, measured.
