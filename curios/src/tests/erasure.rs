//! Erasure, end to end: the typing facts it rests on, what it deletes, that a deleted proof gives a program no behaviour, that a head is read at the type the checkers accepted it at, that an arm is erased under the refinement it was elaborated under, that a function and a tuple have one shape at every type they stand at, that a binder states the register carrier its type names, and that what it keeps computes the value the checker proved.

mod dropped;
mod evaluation;
mod head;
mod irrelevance;
mod refinement;
mod register;
mod shape;
mod value;
