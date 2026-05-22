use {
    super::{DefStack, ModuleInfo},
    crate::{
        core,
        text::{Bin, Name, Nat, Prim, Term},
    },
    std::collections::HashMap,
};

pub struct Elaborate<'a> {
    scope: &'a HashMap<String, Name>,
    table: &'a HashMap<Name, ModuleInfo>,
    def_stack: &'a DefStack,
}

impl<'a> Elaborate<'a> {
    pub fn new(
        scope: &'a HashMap<String, Name>,
        table: &'a HashMap<Name, ModuleInfo>,
        def_stack: &'a DefStack,
    ) -> Self {
        Self {
            scope,
            table,
            def_stack,
        }
    }

    pub fn term(&self, term: &Term) -> core::Term {
        match term {
            Term::Type => core::Term::Type,

            Term::Prim(prim) => core::Term::Prim(self.prim(prim)),

            Term::Name(name) => {
                let path = if name.is_single() {
                    let label = name.head();

                    if let Some(full) = self.scope.get(label) {
                        full.join()
                    } else if let Some(full) = self.def_stack.get(label) {
                        full.join()
                    } else {
                        label.to_string()
                    }
                } else {
                    self.resolve_name(name).join()
                };

                core::Var::free(path).into()
            }

            Term::Atom(atom) => core::Term::Atom(core::Atom::from(atom.as_str())),

            Term::AtomType(at) => core::AtomType::new(
                at.atoms
                    .iter()
                    .map(|atom| core::Atom::from(atom.as_str())),
            )
            .into(),

            Term::FuncType(ft) => core::FuncType::new(
                ft.label.clone().unwrap_or_default(),
                self.term(&ft.input),
                self.term(&ft.output),
            )
            .into(),

            Term::Func(func) => core::Func::new(func.label.clone(), self.term(&func.body)).into(),

            Term::Apply(ap) => core::Apply::new(self.term(&ap.head), self.term(&ap.param)).into(),

            Term::TupleType(tt) => {
                let fields = tt.fields.iter().map(|(label, type_)| {
                    let label = label.clone().unwrap_or_default();
                    (label, self.term(type_))
                });
                core::TupleType::new(fields).into()
            }

            Term::Tuple(tuple) => {
                core::Tuple::new(tuple.fields.iter().map(|field| self.term(field))).into()
            }

            Term::NatFold(nat_fold) => core::NatFold::new(
                self.term(&nat_fold.head),
                nat_fold.motive_label.clone(),
                self.term(&nat_fold.motive),
                self.term(&nat_fold.zero_case),
                nat_fold.pred_label.clone(),
                nat_fold.ih_label.clone(),
                self.term(&nat_fold.succ_case),
            )
            .into(),

            Term::NatMatch(nm) => core::NatMatch::new(
                self.term(&nm.head),
                nm.motive_label.clone(),
                self.term(&nm.motive),
                nm.cases.iter().map(|(&nat, body)| (nat, self.term(body))),
                self.term(&nm.default),
            )
            .into(),

            Term::Split(split) => core::Split::new(
                self.term(&split.head),
                split.motive_label.clone(),
                self.term(&split.motive),
                split.field_labels.iter().cloned(),
                self.term(&split.tail),
            )
            .into(),

            Term::Match(match_) => core::Match::new(
                self.term(&match_.head),
                match_.motive_label.clone(),
                self.term(&match_.motive),
                match_
                    .cases
                    .iter()
                    .map(|(atom, body)| (core::Atom::from(atom.as_str()), self.term(body))),
            )
            .into(),

            Term::From(from) => {
                let name = self
                    .def_stack
                    .get(&from.label)
                    .unwrap_or_else(|| panic!("coercion outside def block: {}", from.label));
                core::Seal::new(core::Var::free(name.join()), self.term(&from.body)).into()
            }

            Term::Into(into) => core::Unseal::new(
                core::Var::free(
                    self.def_stack
                        .get(&into.label)
                        .unwrap_or_else(|| panic!("coercion outside def block: {}", into.label))
                        .join(),
                ),
                self.term(&into.body),
            )
            .into(),

            Term::Let(let_) => core::Let::new(
                let_.label.clone(),
                self.term(&let_.type_),
                self.term(&let_.body),
                self.term(&let_.tail),
            )
            .into(),

            Term::Rec(rec) => core::Rec::new(
                rec.items
                    .iter()
                    .map(|it| (it.label.clone(), self.term(&it.type_), self.term(&it.value))),
                self.term(&rec.tail),
            )
            .into(),
        }
    }

    pub fn prim(&self, prim: &Prim) -> core::Prim {
        match prim {
            Prim::NatType => core::Prim::NatType,
            Prim::Nat(Nat::Number(number)) => core::Prim::Nat(*number),
            Prim::Nat(Nat::Char(character)) => core::Prim::Nat(*character as u32),
            Prim::NatEql(left, right) => core::Prim::nat_eql(self.term(left), self.term(right)),
            Prim::NatNeq(left, right) => core::Prim::nat_neq(self.term(left), self.term(right)),
            Prim::NatAdd(left, right) => core::Prim::nat_add(self.term(left), self.term(right)),
            Prim::NatSub(left, right) => core::Prim::nat_sub(self.term(left), self.term(right)),
            Prim::NatMul(left, right) => core::Prim::nat_mul(self.term(left), self.term(right)),
            Prim::NatLt(left, right) => core::Prim::nat_lt(self.term(left), self.term(right)),
            Prim::NatDiv(left, right) => core::Prim::nat_div(self.term(left), self.term(right)),
            Prim::NatRem(left, right) => core::Prim::nat_rem(self.term(left), self.term(right)),
            Prim::NatGt(left, right) => core::Prim::nat_gt(self.term(left), self.term(right)),
            Prim::NatLte(left, right) => core::Prim::nat_lte(self.term(left), self.term(right)),
            Prim::NatGte(left, right) => core::Prim::nat_gte(self.term(left), self.term(right)),
            Prim::IntType => core::Prim::IntType,
            Prim::Int(value) => core::Prim::Int(*value),
            Prim::IntEql(left, right) => core::Prim::int_eql(self.term(left), self.term(right)),
            Prim::IntNeq(left, right) => core::Prim::int_neq(self.term(left), self.term(right)),
            Prim::IntAdd(left, right) => core::Prim::int_add(self.term(left), self.term(right)),
            Prim::IntSub(left, right) => core::Prim::int_sub(self.term(left), self.term(right)),
            Prim::IntMul(left, right) => core::Prim::int_mul(self.term(left), self.term(right)),
            Prim::IntDiv(left, right) => core::Prim::int_div(self.term(left), self.term(right)),
            Prim::IntRem(left, right) => core::Prim::int_rem(self.term(left), self.term(right)),
            Prim::IntLt(left, right) => core::Prim::int_lt(self.term(left), self.term(right)),
            Prim::IntGt(left, right) => core::Prim::int_gt(self.term(left), self.term(right)),
            Prim::IntLte(left, right) => core::Prim::int_lte(self.term(left), self.term(right)),
            Prim::IntGte(left, right) => core::Prim::int_gte(self.term(left), self.term(right)),
            Prim::FltType => core::Prim::FltType,
            Prim::Flt(flt) => core::Prim::Flt(core::Flt::from_f32(*flt)),
            Prim::FltAdd(left, right) => core::Prim::flt_add(self.term(left), self.term(right)),
            Prim::FltSub(left, right) => core::Prim::flt_sub(self.term(left), self.term(right)),
            Prim::FltMul(left, right) => core::Prim::flt_mul(self.term(left), self.term(right)),
            Prim::FltDiv(left, right) => core::Prim::flt_div(self.term(left), self.term(right)),
            Prim::FltEql(left, right) => core::Prim::flt_eql(self.term(left), self.term(right)),
            Prim::FltNeq(left, right) => core::Prim::flt_neq(self.term(left), self.term(right)),
            Prim::FltLt(left, right) => core::Prim::flt_lt(self.term(left), self.term(right)),
            Prim::FltGt(left, right) => core::Prim::flt_gt(self.term(left), self.term(right)),
            Prim::FltLte(left, right) => core::Prim::flt_lte(self.term(left), self.term(right)),
            Prim::FltGte(left, right) => core::Prim::flt_gte(self.term(left), self.term(right)),
            Prim::FltMin(left, right) => core::Prim::flt_min(self.term(left), self.term(right)),
            Prim::FltMax(left, right) => core::Prim::flt_max(self.term(left), self.term(right)),
            Prim::FltNeg(inner) => core::Prim::flt_neg(self.term(inner)),
            Prim::FltAbs(inner) => core::Prim::flt_abs(self.term(inner)),
            Prim::FltSqrt(inner) => core::Prim::flt_sqrt(self.term(inner)),
            Prim::FltFloor(inner) => core::Prim::flt_floor(self.term(inner)),
            Prim::FltCeil(inner) => core::Prim::flt_ceil(self.term(inner)),
            Prim::FltTrunc(inner) => core::Prim::flt_trunc(self.term(inner)),
            Prim::FltNearest(inner) => core::Prim::flt_nearest(self.term(inner)),
            Prim::NatToInt(inner) => core::Prim::nat_to_int(self.term(inner)),
            Prim::NatToFlt(inner) => core::Prim::nat_to_flt(self.term(inner)),
            Prim::IntToNat(inner) => core::Prim::int_to_nat(self.term(inner)),
            Prim::IntToFlt(inner) => core::Prim::int_to_flt(self.term(inner)),
            Prim::FltToNat(inner) => core::Prim::flt_to_nat(self.term(inner)),
            Prim::FltToInt(inner) => core::Prim::flt_to_int(self.term(inner)),
            Prim::BinType => core::Prim::BinType,
            Prim::Bin(Bin::Bytes(bytes)) => core::Prim::Bin(bytes.clone()),
            Prim::Bin(Bin::String(string)) => core::Prim::Bin(string.as_bytes().to_vec()),
            Prim::BinLen(inner) => core::Prim::bin_len(self.term(inner)),
            Prim::BinEql(left, right) => core::Prim::bin_eql(self.term(left), self.term(right)),
            Prim::BinGet(bin, index) => core::Prim::bin_get(self.term(bin), self.term(index)),
            Prim::BinSlice(bin, start, end) => {
                core::Prim::bin_slice(self.term(bin), self.term(start), self.term(end))
            }
            Prim::BinAppend(bin, byte) => core::Prim::bin_append(self.term(bin), self.term(byte)),
            Prim::BinConcat(operands) => {
                core::Prim::bin_concat(operands.iter().map(|operand| self.term(operand)))
            }
            Prim::ArrType(inner) => core::Prim::arr_type(self.term(inner)),
            Prim::Arr(elems) => {
                core::Prim::Arr(elems.iter().map(|elem| self.term(elem).into()).collect())
            }
            Prim::ArrLen(inner) => core::Prim::arr_len(self.term(inner)),
            Prim::ArrGet(list, index) => core::Prim::arr_get(self.term(list), self.term(index)),
            Prim::ArrSlice(list, start, end) => {
                core::Prim::arr_slice(self.term(list), self.term(start), self.term(end))
            }
            Prim::ArrAppend(list, elem) => core::Prim::arr_append(self.term(list), self.term(elem)),
            Prim::ArrConcat(operands) => {
                core::Prim::arr_concat(operands.iter().map(|operand| self.term(operand)))
            }
        }
    }

    fn resolve_name(&self, name: &Name) -> Name {
        let qualifier = name.head();

        let base = self
            .scope
            .get(qualifier)
            .unwrap_or_else(|| panic!("unresolved qualifier: {qualifier}"))
            .clone();

        let mut current_prefix = base.clone();

        for segment in name.interior() {
            let info = self
                .table
                .get(&current_prefix)
                .unwrap_or_else(|| panic!("module not found: {}", current_prefix.join()));

            let is_pub = info
                .children
                .get(segment)
                .unwrap_or_else(|| panic!("child module not found: {segment}"));

            if !is_pub {
                panic!("private child module: {segment}");
            }

            current_prefix = current_prefix.with(segment);
        }

        let last = name.last();

        let info = self
            .table
            .get(&current_prefix)
            .unwrap_or_else(|| panic!("module not found: {}", current_prefix.join()));

        let is_pub = info
            .bindings
            .get(last)
            .unwrap_or_else(|| panic!("binding not found: {last}"));

        if !is_pub {
            panic!("private binding: {last}");
        }

        base.extend(name.tail())
    }
}
