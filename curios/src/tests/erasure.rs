//! Erasure, end to end: the typing facts it rests on, what it deletes, that a deleted proof gives a program no behaviour, that a head is read at the type the checkers accepted it at, that an arm is erased under the refinement it was elaborated under, that a function and a tuple have one shape at every type they stand at, and that what it keeps computes the value the checker proved.

mod dropped_tests;
mod evaluation_tests;
mod head_tests;
mod irrelevance_tests;
mod refinement_tests;
mod shape_tests;
mod value_tests;
