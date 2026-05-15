use {
    super::{Arity, Atom, Many, One, Prim, Two},
    std::collections::{BTreeMap, BTreeSet},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum VarType {
    Free(String),
    Bound(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    type_: VarType,
}

impl Var {
    pub fn free<A>(label: A) -> Self
    where
        A: Into<String>,
    {
        Self {
            type_: VarType::Free(label.into()),
        }
    }

    fn as_free(&self) -> Option<&str> {
        match &self.type_ {
            VarType::Free(label) => Some(label),
            VarType::Bound(_) => None,
        }
    }

    fn bound(index: usize) -> Self {
        Self {
            type_: VarType::Bound(index),
        }
    }

    fn as_bound(&self) -> Option<usize> {
        match &self.type_ {
            VarType::Free(_) => None,
            &VarType::Bound(index) => Some(index),
        }
    }

    pub fn unwrap(&self) -> &str {
        self.as_free().unwrap()
    }
}

pub type Subterm = Box<Term>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scope<A: Arity> {
    arity: A,
    body: Subterm,
}

impl<A> Scope<A>
where
    A: Arity,
{
    pub fn close<'a, B>(arity: A, labels: A::Params<'a, str>, body: B) -> Self
    where
        B: Into<Term>,
    {
        assert!(
            arity.arity() == labels.as_ref().len(),
            "scope arity mismatch in `close`: expected {}, got {}",
            arity.arity(),
            labels.as_ref().len()
        );

        Self {
            arity,
            body: body.into().capture(labels.as_ref()).into(),
        }
    }

    pub fn arity(&self) -> usize {
        self.arity.arity()
    }

    pub fn open<'a>(&self, terms: A::Params<'a, Term>) -> Term {
        assert!(
            self.arity() == terms.as_ref().len(),
            "scope arity mismatch in `open`: expected {}, got {}",
            self.arity(),
            terms.as_ref().len()
        );

        self.body.release(terms.as_ref())
    }

    pub fn free_vars(&self) -> BTreeSet<String> {
        self.body.free_vars()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncType {
    pub input: Subterm,
    pub output: Scope<One>,
}

impl FuncType {
    pub fn new<L, I, O>(label: L, input: I, output: O) -> Self
    where
        L: Into<String>,
        I: Into<Term>,
        O: Into<Term>,
    {
        let label = label.into();

        Self {
            input: input.into().into(),
            output: Scope::close(One, &[label.as_str()], output),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Func {
    pub body: Scope<One>,
}

impl Func {
    pub fn new<L, B>(label: L, body: B) -> Self
    where
        L: Into<String>,
        B: Into<Term>,
    {
        let label = label.into();

        Self {
            body: Scope::close(One, &[label.as_str()], body),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Apply {
    pub head: Subterm,
    pub param: Subterm,
}

impl Apply {
    pub fn new<H, P>(head: H, param: P) -> Self
    where
        H: Into<Term>,
        P: Into<Term>,
    {
        Self {
            head: head.into().into(),
            param: param.into().into(),
        }
    }

    pub fn many<H, I, P>(head: H, params: I) -> Term
    where
        H: Into<Term>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
    {
        params
            .into_iter()
            .fold(head.into(), |head, param| Self::new(head, param).into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PairType {
    pub input: Subterm,
    pub output: Scope<One>,
}

impl PairType {
    pub fn new<L, I, O>(label: L, input: I, output: O) -> Self
    where
        L: Into<String>,
        I: Into<Term>,
        O: Into<Term>,
    {
        let label = label.into();

        Self {
            input: input.into().into(),
            output: Scope::close(One, &[label.as_str()], output),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pair {
    pub fst: Subterm,
    pub snd: Subterm,
}

impl Pair {
    pub fn new<F, S>(fst: F, snd: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self {
            fst: fst.into().into(),
            snd: snd.into().into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Split {
    pub head: Subterm,
    pub motive: Scope<One>,
    pub tail: Scope<Two>,
}

impl Split {
    pub fn new<H, ML, M, FL, SL, T>(
        head: H,
        motive_label: ML,
        motive: M,
        fst_label: FL,
        snd_label: SL,
        tail: T,
    ) -> Self
    where
        H: Into<Term>,
        ML: Into<String>,
        M: Into<Term>,
        FL: Into<String>,
        SL: Into<String>,
        T: Into<Term>,
    {
        let motive_label = motive_label.into();
        let fst_label = fst_label.into();
        let snd_label = snd_label.into();

        Self {
            head: head.into().into(),
            motive: Scope::close(One, &[motive_label.as_str()], motive),
            tail: Scope::close(Two, &[fst_label.as_str(), snd_label.as_str()], tail),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AtomType {
    pub atoms: BTreeSet<Atom>,
}

impl AtomType {
    pub fn new<I, A>(atoms: I) -> Self
    where
        I: IntoIterator<Item = A>,
        A: Into<Atom>,
    {
        Self {
            atoms: atoms.into_iter().map(Into::into).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Case {
    pub head: Subterm,
    pub motive: Scope<One>,
    pub cases: BTreeMap<Atom, Subterm>,
}

impl Case {
    pub fn new<H, L, M, I, A, B>(head: H, motive_label: L, motive: M, cases: I) -> Self
    where
        H: Into<Term>,
        L: Into<String>,
        M: Into<Term>,
        I: IntoIterator<Item = (A, B)>,
        A: Into<Atom>,
        B: Into<Term>,
    {
        let motive_label = motive_label.into();

        Self {
            head: head.into().into(),
            motive: Scope::close(One, &[motive_label.as_str()], motive),
            cases: cases
                .into_iter()
                .map(|(atom, body)| (atom.into(), body.into().into()))
                .collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Let {
    pub type_: Subterm,
    pub body: Subterm,
    pub tail: Scope<One>,
}

impl Let {
    pub fn new<L, T, B, U>(label: L, type_: T, body: B, tail: U) -> Self
    where
        L: Into<String>,
        T: Into<Term>,
        B: Into<Term>,
        U: Into<Term>,
    {
        let label = label.into();

        Self {
            type_: type_.into().into(),
            body: body.into().into(),
            tail: Scope::close(One, &[label.as_str()], tail),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LetRec {
    pub items: Vec<(Scope<Many>, Scope<Many>)>,
    pub tail: Scope<Many>,
}

impl LetRec {
    pub fn new<I, L, T, U, V>(items: I, tail: V) -> Self
    where
        I: IntoIterator<Item = (L, T, U)>,
        L: Into<String>,
        T: Into<Term>,
        U: Into<Term>,
        V: Into<Term>,
    {
        let items = items
            .into_iter()
            .map(|(label, type_, value)| (label.into(), type_.into(), value.into()))
            .collect::<Vec<_>>();

        let labels = items
            .iter()
            .map(|(label, _, _)| label.clone())
            .collect::<Vec<_>>();

        let labels = labels
            .iter()
            .map(|label| label.as_str())
            .collect::<Vec<_>>();

        Self {
            items: items
                .into_iter()
                .map(|(_, type_, value)| {
                    (
                        Scope::close(Many(labels.len()), &labels, type_),
                        Scope::close(Many(labels.len()), &labels, value),
                    )
                })
                .collect(),
            tail: Scope::close(Many(labels.len()), &labels, tail),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    Type,
    Prim(Prim),
    FuncType(FuncType),
    Func(Func),
    Apply(Apply),
    PairType(PairType),
    Pair(Pair),
    Split(Split),
    AtomType(AtomType),
    Atom(Atom),
    Case(Case),
    Let(Let),
    LetRec(LetRec),
    Var(Var),
}

impl Term {
    pub fn free_vars(&self) -> BTreeSet<String> {
        let mut vars = BTreeSet::new();

        Visit::new(|_, var| {
            if let Some(label) = var.as_free() {
                vars.insert(label.to_string());
            }

            None
        })
        .visit_term(self);

        vars
    }

    fn shift(&self, amount: usize) -> Self {
        Visit::new(|depth, var| {
            var.as_bound()
                .filter(|&index| index >= depth)
                .map(|index| Var::bound(index + amount).into())
        })
        .visit_term(self)
    }

    fn capture(&self, labels: &[&str]) -> Self {
        Visit::new(|depth, var| {
            var.as_free()
                .and_then(|label| {
                    labels
                        .iter()
                        .position(|&candidate| label == candidate)
                        .map(|index| Var::bound(depth + index).into())
                })
                .or_else(|| {
                    var.as_bound()
                        .filter(|&index| index >= depth)
                        .map(|index| Var::bound(index + labels.len()).into())
                })
        })
        .visit_term(self)
    }

    fn release(&self, terms: &[&Term]) -> Self {
        Visit::new(|depth, var| {
            var.as_bound().and_then(|index| {
                index
                    .checked_sub(depth)
                    .map(|delta| match delta < terms.len() {
                        true => terms[delta].shift(depth),
                        false => Var::bound(index - terms.len()).into(),
                    })
            })
        })
        .visit_term(self)
    }
}

impl From<Type> for Term {
    fn from(Type: Type) -> Self {
        Self::Type
    }
}

impl From<Prim> for Term {
    fn from(value: Prim) -> Self {
        Self::Prim(value)
    }
}

impl From<FuncType> for Term {
    fn from(value: FuncType) -> Self {
        Self::FuncType(value)
    }
}

impl From<Func> for Term {
    fn from(value: Func) -> Self {
        Self::Func(value)
    }
}

impl From<Apply> for Term {
    fn from(value: Apply) -> Self {
        Self::Apply(value)
    }
}

impl From<PairType> for Term {
    fn from(value: PairType) -> Self {
        Self::PairType(value)
    }
}

impl From<Pair> for Term {
    fn from(value: Pair) -> Self {
        Self::Pair(value)
    }
}

impl From<Split> for Term {
    fn from(value: Split) -> Self {
        Self::Split(value)
    }
}

impl From<AtomType> for Term {
    fn from(value: AtomType) -> Self {
        Self::AtomType(value)
    }
}

impl From<Atom> for Term {
    fn from(value: Atom) -> Self {
        Self::Atom(value)
    }
}

impl From<Case> for Term {
    fn from(value: Case) -> Self {
        Self::Case(value)
    }
}

impl From<Let> for Term {
    fn from(value: Let) -> Self {
        Self::Let(value)
    }
}

impl From<LetRec> for Term {
    fn from(value: LetRec) -> Self {
        Self::LetRec(value)
    }
}

impl From<Var> for Term {
    fn from(value: Var) -> Self {
        Self::Var(value)
    }
}

#[derive(Debug)]
struct Visit<F> {
    depth: usize,
    visit: F,
}

impl<F> Visit<F>
where
    F: FnMut(usize, &Var) -> Option<Term>,
{
    fn new(visit: F) -> Self {
        Self { depth: 0, visit }
    }

    fn visit_subterm(&mut self, subterm: &Subterm) -> Subterm {
        self.visit_term(subterm).into()
    }

    fn visit_prim(&mut self, prim: &Prim) -> Prim {
        match prim {
            Prim::NatType => Prim::NatType,
            Prim::Nat(value) => Prim::Nat(*value),
            Prim::NatEql(left, right) => {
                Prim::NatEql(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatNeq(left, right) => {
                Prim::NatNeq(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatAdd(left, right) => {
                Prim::NatAdd(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatSub(left, right) => {
                Prim::NatSub(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatMul(left, right) => {
                Prim::NatMul(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatDiv(left, right) => {
                Prim::NatDiv(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatRem(left, right) => {
                Prim::NatRem(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatLt(left, right) => {
                Prim::NatLt(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatGt(left, right) => {
                Prim::NatGt(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatLte(left, right) => {
                Prim::NatLte(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatGte(left, right) => {
                Prim::NatGte(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntType => Prim::IntType,
            Prim::Int(value) => Prim::Int(*value),
            Prim::IntEql(left, right) => {
                Prim::IntEql(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntAdd(left, right) => {
                Prim::IntAdd(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntSub(left, right) => {
                Prim::IntSub(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntMul(left, right) => {
                Prim::IntMul(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltType => Prim::FltType,
            Prim::Flt(bits) => Prim::Flt(*bits),
            Prim::FltAdd(left, right) => {
                Prim::FltAdd(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltSub(left, right) => {
                Prim::FltSub(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltMul(left, right) => {
                Prim::FltMul(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntNeq(left, right) => {
                Prim::IntNeq(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntDiv(left, right) => {
                Prim::IntDiv(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntRem(left, right) => {
                Prim::IntRem(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntLt(left, right) => {
                Prim::IntLt(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntGt(left, right) => {
                Prim::IntGt(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntLte(left, right) => {
                Prim::IntLte(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntGte(left, right) => {
                Prim::IntGte(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltNeg(inner) => Prim::FltNeg(self.visit_subterm(inner)),
            Prim::FltAbs(inner) => Prim::FltAbs(self.visit_subterm(inner)),
            Prim::FltSqrt(inner) => Prim::FltSqrt(self.visit_subterm(inner)),
            Prim::FltFloor(inner) => Prim::FltFloor(self.visit_subterm(inner)),
            Prim::FltCeil(inner) => Prim::FltCeil(self.visit_subterm(inner)),
            Prim::FltTrunc(inner) => Prim::FltTrunc(self.visit_subterm(inner)),
            Prim::FltNearest(inner) => Prim::FltNearest(self.visit_subterm(inner)),
            Prim::FltDiv(left, right) => {
                Prim::FltDiv(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltMin(left, right) => {
                Prim::FltMin(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltMax(left, right) => {
                Prim::FltMax(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltEql(left, right) => {
                Prim::FltEql(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltNeq(left, right) => {
                Prim::FltNeq(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltLt(left, right) => {
                Prim::FltLt(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltGt(left, right) => {
                Prim::FltGt(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltLte(left, right) => {
                Prim::FltLte(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltGte(left, right) => {
                Prim::FltGte(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatToInt(inner) => Prim::NatToInt(self.visit_subterm(inner)),
            Prim::IntToNat(inner) => Prim::IntToNat(self.visit_subterm(inner)),
            Prim::IntToFlt(inner) => Prim::IntToFlt(self.visit_subterm(inner)),
            Prim::NatToFlt(inner) => Prim::NatToFlt(self.visit_subterm(inner)),
            Prim::FltToInt(inner) => Prim::FltToInt(self.visit_subterm(inner)),
            Prim::FltToNat(inner) => Prim::FltToNat(self.visit_subterm(inner)),
            Prim::BinType => Prim::BinType,
            Prim::Bin(bytes) => Prim::Bin(bytes.clone()),
            Prim::BinLen(bin) => Prim::BinLen(self.visit_subterm(bin)),
            Prim::BinGet(bin, index) => {
                Prim::BinGet(self.visit_subterm(bin), self.visit_subterm(index))
            }
            Prim::BinSlice(bin, start, end) => Prim::BinSlice(
                self.visit_subterm(bin),
                self.visit_subterm(start),
                self.visit_subterm(end),
            ),
            Prim::BinAppend(bin, byte) => {
                Prim::BinAppend(self.visit_subterm(bin), self.visit_subterm(byte))
            }
            Prim::BinConcat(operands) => {
                Prim::BinConcat(operands.iter().map(|e| self.visit_subterm(e)).collect())
            }
            Prim::ArrType(elem) => Prim::ArrType(self.visit_subterm(elem)),
            Prim::Arr(elems) => Prim::Arr(elems.iter().map(|e| self.visit_subterm(e)).collect()),
            Prim::ArrLen(list) => Prim::ArrLen(self.visit_subterm(list)),
            Prim::ArrGet(list, index) => {
                Prim::ArrGet(self.visit_subterm(list), self.visit_subterm(index))
            }
            Prim::ArrSlice(list, start, end) => Prim::ArrSlice(
                self.visit_subterm(list),
                self.visit_subterm(start),
                self.visit_subterm(end),
            ),
            Prim::ArrAppend(list, elem) => {
                Prim::ArrAppend(self.visit_subterm(list), self.visit_subterm(elem))
            }
            Prim::ArrConcat(operands) => {
                Prim::ArrConcat(operands.iter().map(|e| self.visit_subterm(e)).collect())
            }
        }
    }

    fn visit_scope<A: Arity>(&mut self, scope: &Scope<A>) -> Scope<A> {
        self.depth += scope.arity.arity();
        let body = self.visit_subterm(&scope.body);
        self.depth -= scope.arity.arity();

        Scope {
            arity: scope.arity,
            body,
        }
    }

    fn visit_func_type(&mut self, ft: &FuncType) -> FuncType {
        FuncType {
            input: self.visit_subterm(&ft.input),
            output: self.visit_scope(&ft.output),
        }
    }

    fn visit_func(&mut self, func: &Func) -> Func {
        Func {
            body: self.visit_scope(&func.body),
        }
    }

    fn visit_apply(&mut self, apply: &Apply) -> Apply {
        Apply {
            head: self.visit_subterm(&apply.head),
            param: self.visit_subterm(&apply.param),
        }
    }

    fn visit_pair_type(&mut self, pt: &PairType) -> PairType {
        PairType {
            input: self.visit_subterm(&pt.input),
            output: self.visit_scope(&pt.output),
        }
    }

    fn visit_pair(&mut self, pair: &Pair) -> Pair {
        Pair {
            fst: self.visit_subterm(&pair.fst),
            snd: self.visit_subterm(&pair.snd),
        }
    }

    fn visit_split(&mut self, split: &Split) -> Split {
        Split {
            head: self.visit_subterm(&split.head),
            motive: self.visit_scope(&split.motive),
            tail: self.visit_scope(&split.tail),
        }
    }

    fn visit_case(&mut self, case: &Case) -> Case {
        Case {
            head: self.visit_subterm(&case.head),
            motive: self.visit_scope(&case.motive),
            cases: case
                .cases
                .iter()
                .map(|(atom, body)| (atom.clone(), self.visit_subterm(body)))
                .collect(),
        }
    }

    fn visit_let(&mut self, let_: &Let) -> Let {
        Let {
            type_: self.visit_subterm(&let_.type_),
            body: self.visit_subterm(&let_.body),
            tail: self.visit_scope(&let_.tail),
        }
    }

    fn visit_letrec(&mut self, letrec: &LetRec) -> LetRec {
        LetRec {
            items: letrec
                .items
                .iter()
                .map(|(type_, value)| (self.visit_scope(type_), self.visit_scope(value)))
                .collect(),
            tail: self.visit_scope(&letrec.tail),
        }
    }

    fn visit_term(&mut self, term: &Term) -> Term {
        match term {
            Term::Type => Type.into(),
            Term::Prim(prim) => self.visit_prim(prim).into(),
            Term::FuncType(ft) => self.visit_func_type(ft).into(),
            Term::Func(func) => self.visit_func(func).into(),
            Term::Apply(apply) => self.visit_apply(apply).into(),
            Term::PairType(pt) => self.visit_pair_type(pt).into(),
            Term::Pair(pair) => self.visit_pair(pair).into(),
            Term::Split(split) => self.visit_split(split).into(),
            Term::AtomType(at) => at.clone().into(),
            Term::Atom(atom) => atom.clone().into(),
            Term::Case(case) => self.visit_case(case).into(),
            Term::Let(let_) => self.visit_let(let_).into(),
            Term::LetRec(letrec) => self.visit_letrec(letrec).into(),
            Term::Var(var) => (self.visit)(self.depth, var).unwrap_or_else(|| var.clone().into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn close_open_substitutes_label_name() {
        let term = Scope::close(One, &["x"], Var::free("x")).open(&[&Var::free("y").into()]);

        let var = match term {
            Term::Var(var) => var,
            term => panic!("unexpected `{term:?}`"),
        };

        assert_eq!(var, Var::free("y"));
    }

    #[test]
    fn close_open_preserves_nested_bind() {
        let term = Scope::close(One, &["x"], Func::new("y", Var::free("x")))
            .open(&[&Var::free("z").into()]);

        let body = match term {
            Term::Func(body) => body.body,
            term => panic!("unexpected `{term:?}`"),
        };

        let var = match body.open(&[&Var::free("w").into()]) {
            Term::Var(var) => var,
            term => panic!("unexpected `{term:?}`"),
        };

        assert_eq!(var, Var::free("z"));
    }

    #[test]
    fn collect_ignores_index_names() {
        let term = Term::from(Func::new(
            "x",
            Pair::new(
                Var::free("x"),
                LetRec::new(
                    vec![("y", Type, Var::free("z"))],
                    Pair::new(Var::free("y"), Var::free("w")),
                ),
            ),
        ));

        assert_eq!(
            term.free_vars(),
            BTreeSet::from([String::from("w"), String::from("z")])
        );
    }
}
