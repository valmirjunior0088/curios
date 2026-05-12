use crate::macros::name;

name!(ErasedName);

pub type ErasedSubterm = Box<ErasedTerm>;

#[derive(Debug)]
pub enum ErasedPrim {
    Unit,
    Nat(u32),
    NatEql(ErasedSubterm, ErasedSubterm),
    NatAdd(ErasedSubterm, ErasedSubterm),
    NatSub(ErasedSubterm, ErasedSubterm),
    NatMul(ErasedSubterm, ErasedSubterm),
    NatLt(ErasedSubterm, ErasedSubterm),
    Int(i32),
    IntEql(ErasedSubterm, ErasedSubterm),
    IntAdd(ErasedSubterm, ErasedSubterm),
    IntSub(ErasedSubterm, ErasedSubterm),
    IntMul(ErasedSubterm, ErasedSubterm),
    Flt(f32),
    FltAdd(ErasedSubterm, ErasedSubterm),
    FltSub(ErasedSubterm, ErasedSubterm),
    FltMul(ErasedSubterm, ErasedSubterm),
}

impl From<()> for ErasedPrim {
    fn from((): ()) -> Self {
        Self::Unit
    }
}

impl From<u32> for ErasedPrim {
    fn from(value: u32) -> Self {
        Self::Nat(value)
    }
}

impl From<i32> for ErasedPrim {
    fn from(value: i32) -> Self {
        Self::Int(value)
    }
}

impl From<f32> for ErasedPrim {
    fn from(value: f32) -> Self {
        Self::Flt(value)
    }
}

#[derive(Debug)]
pub struct ErasedFunc {
    pub captures: Vec<String>,
    pub param: String,
    pub body: ErasedSubterm,
}

#[derive(Debug)]
pub struct ErasedApply {
    pub head: ErasedSubterm,
    pub param: ErasedSubterm,
}

#[derive(Debug)]
pub struct ErasedPair {
    pub fst: ErasedSubterm,
    pub snd: ErasedSubterm,
}

#[derive(Debug)]
pub struct ErasedSplit {
    pub head: ErasedSubterm,
    pub fst: String,
    pub snd: String,
    pub tail: ErasedSubterm,
}

#[derive(Debug, Clone, Copy)]
pub struct ErasedAtom {
    pub index: usize,
}

#[derive(Debug)]
pub struct ErasedMatch {
    pub head: ErasedSubterm,
    pub cases: Vec<ErasedSubterm>,
}

#[derive(Debug)]
pub struct ErasedLet {
    pub name: String,
    pub body: ErasedSubterm,
    pub tail: ErasedSubterm,
}

#[derive(Debug)]
pub struct ErasedLetRec {
    pub names: Vec<String>,
    pub items: Vec<ErasedSubterm>,
    pub tail: ErasedSubterm,
}

#[derive(Debug)]
pub enum ErasedTerm {
    Prim(ErasedPrim),
    Func(ErasedFunc),
    Apply(ErasedApply),
    Pair(ErasedPair),
    Split(ErasedSplit),
    Atom(ErasedAtom),
    Match(ErasedMatch),
    Let(ErasedLet),
    LetRec(ErasedLetRec),
    Name(ErasedName),
}

impl From<ErasedPrim> for ErasedTerm {
    fn from(value: ErasedPrim) -> Self {
        Self::Prim(value)
    }
}

impl From<()> for ErasedTerm {
    fn from(value: ()) -> Self {
        ErasedPrim::from(value).into()
    }
}

impl From<u32> for ErasedTerm {
    fn from(value: u32) -> Self {
        ErasedPrim::from(value).into()
    }
}

impl From<i32> for ErasedTerm {
    fn from(value: i32) -> Self {
        ErasedPrim::from(value).into()
    }
}

impl From<f32> for ErasedTerm {
    fn from(value: f32) -> Self {
        ErasedPrim::from(value).into()
    }
}

impl From<ErasedFunc> for ErasedTerm {
    fn from(value: ErasedFunc) -> Self {
        Self::Func(value)
    }
}

impl From<ErasedApply> for ErasedTerm {
    fn from(value: ErasedApply) -> Self {
        Self::Apply(value)
    }
}

impl From<ErasedPair> for ErasedTerm {
    fn from(value: ErasedPair) -> Self {
        Self::Pair(value)
    }
}

impl From<ErasedSplit> for ErasedTerm {
    fn from(value: ErasedSplit) -> Self {
        Self::Split(value)
    }
}

impl From<ErasedAtom> for ErasedTerm {
    fn from(value: ErasedAtom) -> Self {
        Self::Atom(value)
    }
}

impl From<ErasedMatch> for ErasedTerm {
    fn from(value: ErasedMatch) -> Self {
        Self::Match(value)
    }
}

impl From<ErasedLet> for ErasedTerm {
    fn from(value: ErasedLet) -> Self {
        Self::Let(value)
    }
}

impl From<ErasedLetRec> for ErasedTerm {
    fn from(value: ErasedLetRec) -> Self {
        Self::LetRec(value)
    }
}

impl From<ErasedName> for ErasedTerm {
    fn from(value: ErasedName) -> Self {
        Self::Name(value)
    }
}
