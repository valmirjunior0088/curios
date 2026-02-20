use super::Term;

pub fn check(term: Term, kind: Term) -> bool {
    infer(term) == kind
}

pub fn infer(term: Term) -> Term {
    term
}
